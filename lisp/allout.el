;;; allout.el --- extensive outline mode for use alone and with other modes

;; Copyright (C) 1992, 1993, 1994, 2001 Free Software Foundation, Inc.

;; Author: Ken Manheimer <klm@python.org>
;; Maintainer: Ken Manheimer <klm@python.org>
;; Created: Dec 1991 - first release to usenet
;; Version: $Id: allout.el,v 1.31 2001/12/09 13:13:13 pj Exp $||
;; Keywords: outlines mode wp languages

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Allout outline mode provides extensive outline formatting and
;; and manipulation beyond standard emacs outline mode.  It provides
;; for structured editing of outlines, as well as navigation and
;; exposure.  It also provides for syntax-sensitive text like
;; programming languages.  (For an example, see the allout code
;; itself, which is organized in ;; an outline framework.)
;; 
;; In addition to outline navigation and exposure, allout includes:
;; 
;;  - topic-oriented repositioning, cut, and paste
;;  - integral outline exposure-layout
;;  - incremental search with dynamic exposure and reconcealment of hidden text
;;  - automatic topic-number maintenance
;;  - "Hot-spot" operation, for single-keystroke maneuvering and
;;    exposure control.  (See the `outline-mode' docstring.)
;; 
;; and many other features.
;; 
;; The outline menubar additions provide quick reference to many of
;; the features, and see the docstring of the variable `outline-init'
;; for instructions on priming your emacs session for automatic
;; activation of `outline-mode'.
;; 
;; See the docstring of the variables `outline-layout' and
;; `outline-auto-activation' for details on automatic activation of
;; allout `outline-mode' as a minor mode.  (It has changed since allout
;; 3.x, for those of you that depend on the old method.)
;;
;; Note - the lines beginning with `;;;_' are outline topic headers.
;;        Just `ESC-x eval-current-buffer' to give it a whirl.

;; Ken Manheimer	klm@python.org

;;; Code:

;;;_* Provide
(provide 'outline)
(provide 'allout)

;;;_* USER CUSTOMIZATION VARIABLES:
(defgroup allout nil
  "Extensive outline mode for use alone and with other modes."
  :prefix "outline-"
  :group 'outlines)

;;;_ + Layout, Mode, and Topic Header Configuration

;;;_  = outline-auto-activation
(defcustom outline-auto-activation nil
  "*Regulates auto-activation modality of allout outlines - see `outline-init'.

Setq-default by `outline-init' to regulate whether or not allout
outline mode is automatically activated when the buffer-specific
variable `outline-layout' is non-nil, and whether or not the layout
dictated by `outline-layout' should be imposed on mode activation.

With value `t', auto-mode-activation and auto-layout are enabled.
\(This also depends on `outline-find-file-hooks' being installed in
`find-file-hooks', which is also done by `outline-init'.)

With value `ask', auto-mode-activation is enabled, and endorsement for
performing auto-layout is asked of the user each time.

With value `activate', only auto-mode-activation is enabled, 
auto-layout is not.

With value `nil', neither auto-mode-activation nor auto-layout are
enabled.

See the docstring for `outline-init' for the proper interface to
this variable."
  :type '(choice (const :tag "On" t)
                (const :tag "Ask about layout" "ask")
                (const :tag "Mode only" "activate")
                (const :tag "Off" nil))
  :group 'allout)
;;;_  = outline-layout
(defvar outline-layout nil
  "*Layout specification and provisional mode trigger for allout outlines.

Buffer-specific.

A list value specifies a default layout for the current buffer, to be
applied upon activation of allout `outline-mode'.  Any non-nil value will
automatically trigger allout `outline-mode', provided `outline-init'
has been called to enable it.

See the docstring for `outline-init' for details on setting up for
auto-mode-activation, and for `outline-expose-topic' for the format of
the layout specification.

You can associate a particular outline layout with a file by setting
this var via the file's local variables.  For example, the following
lines at the bottom of an Emacs Lisp file:

;;;Local variables:
;;;outline-layout: \(0 : -1 -1 0)
;;;End:

will, modulo the above-mentioned conditions, cause the mode to be
activated when the file is visited, followed by the equivalent of
`\(outline-expose-topic 0 : -1 -1 0)'.  \(This is the layout used for
the allout.el, itself.)

Also, allout's mode-specific provisions will make topic prefixes default
to the comment-start string, if any, of the language of the file.  This
is modulo the setting of `outline-use-mode-specific-leader', which see.")
(make-variable-buffer-local 'outline-layout)
;;;_  = outline-show-bodies
(defcustom outline-show-bodies nil
  "*If non-nil, show entire body when exposing a topic, rather than
just the header."
  :type 'boolean
  :group 'allout)
(make-variable-buffer-local 'outline-show-bodies)

;;;_  = outline-header-prefix
(defcustom outline-header-prefix "."
  "*Leading string which helps distinguish topic headers.

Outline topic header lines are identified by a leading topic
header prefix, which mostly have the value of this var at their front.
\(Level 1 topics are exceptions.  They consist of only a single
character, which is typically set to the outline-primary-bullet.  Many
outlines start at level 2 to avoid this discrepancy."
  :type 'string
  :group 'allout)
(make-variable-buffer-local 'outline-header-prefix)
;;;_  = outline-primary-bullet
(defcustom outline-primary-bullet "*"
  "Bullet used for top-level outline topics.

Outline topic header lines are identified by a leading topic header
prefix, which is concluded by bullets that includes the value of this
var and the respective outline-*-bullets-string vars.

The value of an asterisk (`*') provides for backwards compatibility
with the original emacs outline mode.  See outline-plain-bullets-string
and outline-distinctive-bullets-string for the range of available
bullets."
  :type 'string
  :group 'allout)
(make-variable-buffer-local 'outline-primary-bullet)
;;;_  = outline-plain-bullets-string
(defcustom outline-plain-bullets-string ".:,;"
  "*The bullets normally used in outline topic prefixes.

See `outline-distinctive-bullets-string' for the other kind of
bullets.

DO NOT include the close-square-bracket, `]', as a bullet.

Outline mode has to be reactivated in order for changes to the value
of this var to take effect."
  :type 'string
  :group 'allout)
(make-variable-buffer-local 'outline-plain-bullets-string)
;;;_  = outline-distinctive-bullets-string
(defcustom outline-distinctive-bullets-string "*+-=>([{}&!?#%\"X@$~_\\"
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
 `\"' a quote
 `=' value settings
 `~' \"more or less\"

... just for example.  (`#' typically has a special meaning to the
software, according to the value of `outline-numbered-bullet'.)

See `outline-plain-bullets-string' for the selection of
alternating bullets.

You must run `set-outline-regexp' in order for outline mode to
reconcile to changes of this value.

DO NOT include the close-square-bracket, `]', on either of the bullet
strings."
  :type 'string
  :group 'allout)
(make-variable-buffer-local 'outline-distinctive-bullets-string)

;;;_  = outline-use-mode-specific-leader
(defcustom outline-use-mode-specific-leader t
  "*When non-nil, use mode-specific topic-header prefixes.

Allout outline mode will use the mode-specific `outline-mode-leaders'
and/or comment-start string, if any, to lead the topic prefix string,
so topic headers look like comments in the programming language.

String values are used as they stand.

Value `t' means to first check for assoc value in `outline-mode-leaders'
alist, then use comment-start string, if any, then use default \(`.').
\(See note about use of comment-start strings, below.)

Set to the symbol for either of `outline-mode-leaders' or
`comment-start' to use only one of them, respectively.

Value `nil' means to always use the default \(`.').

comment-start strings that do not end in spaces are tripled, and an
`_' underscore is tacked on the end, to distinguish them from regular
comment strings.  comment-start strings that do end in spaces are not
tripled, but an underscore is substituted for the space. [This
presumes that the space is for appearance, not comment syntax.  You
can use `outline-mode-leaders' to override this behavior, when
incorrect.]"
  :type '(choice (const t) (const nil) string 
		 (const outline-mode-leaders)
		 (const comment-start))
  :group 'allout)
;;;_  = outline-mode-leaders
(defvar outline-mode-leaders '()
  "Specific outline-prefix leading strings per major modes.

Entries will be used instead or in lieu of mode-specific
comment-start strings.  See also `outline-use-mode-specific-leader'.

If you're constructing a string that will comment-out outline
structuring so it can be included in program code, append an extra
character, like an \"_\" underscore, to distinguish the lead string
from regular comments that start at bol.")

;;;_  = outline-old-style-prefixes
(defcustom outline-old-style-prefixes nil
  "*When non-nil, use only old-and-crusty outline-mode `*' topic prefixes.

Non-nil restricts the topic creation and modification
functions to asterix-padded prefixes, so they look exactly
like the original emacs-outline style prefixes.

Whatever the setting of this variable, both old and new style prefixes
are always respected by the topic maneuvering functions."
  :type 'boolean
  :group 'allout)
(make-variable-buffer-local 'outline-old-style-prefixes)
;;;_  = outline-stylish-prefixes - alternating bullets
(defcustom outline-stylish-prefixes t
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

The setting of this var is not relevant when outline-old-style-prefixes
is non-nil."
  :type 'boolean
  :group 'allout)
(make-variable-buffer-local 'outline-stylish-prefixes)

;;;_  = outline-numbered-bullet
(defcustom outline-numbered-bullet "#"
  "*String designating bullet of topics that have auto-numbering; nil for none.

Topics having this bullet have automatic maintenance of a sibling
sequence-number tacked on, just after the bullet.  Conventionally set
to \"#\", you can set it to a bullet of your choice.  A nil value
disables numbering maintenance."
  :type '(choice (const nil) string)
  :group 'allout)
(make-variable-buffer-local 'outline-numbered-bullet)
;;;_  = outline-file-xref-bullet
(defcustom outline-file-xref-bullet "@"
  "*Bullet signifying file cross-references, for `outline-resolve-xref'.

Set this var to the bullet you want to use for file cross-references."
  :type '(choice (const nil) string)
  :group 'allout)

;;;_  = outline-presentation-padding
(defcustom outline-presentation-padding 2
  "*Presentation-format white-space padding factor, for greater indent."
  :type 'integer
  :group 'allout)

(make-variable-buffer-local 'outline-presentation-padding)

;;;_  = outline-abbreviate-flattened-numbering
(defcustom outline-abbreviate-flattened-numbering nil
  "*If non-nil, `outline-flatten-exposed-to-buffer' abbreviates topic
numbers to minimal amount with some context.  Otherwise, entire
numbers are always used."
  :type 'boolean
  :group 'allout)

;;;_ + LaTeX formatting
;;;_  - outline-number-pages
(defcustom outline-number-pages nil
  "*Non-nil turns on page numbering for LaTeX formatting of an outline."
  :type 'boolean
  :group 'allout)
;;;_  - outline-label-style
(defcustom outline-label-style "\\large\\bf"
  "*Font and size of labels for LaTeX formatting of an outline."
  :type 'string
  :group 'allout)
;;;_  - outline-head-line-style
(defcustom outline-head-line-style "\\large\\sl "
  "*Font and size of entries for LaTeX formatting of an outline."
  :type 'string
  :group 'allout)
;;;_  - outline-body-line-style
(defcustom outline-body-line-style " "
  "*Font and size of entries for LaTeX formatting of an outline."
  :type 'string
  :group 'allout)
;;;_  - outline-title-style
(defcustom outline-title-style "\\Large\\bf"
  "*Font and size of titles for LaTeX formatting of an outline."
  :type 'string
  :group 'allout)
;;;_  - outline-title
(defcustom outline-title '(or buffer-file-name (current-buffer-name))
  "*Expression to be evaluated to determine the title for LaTeX
formatted copy."
  :type 'sexp
  :group 'allout)
;;;_  - outline-line-skip
(defcustom outline-line-skip ".05cm"
  "*Space between lines for LaTeX formatting of an outline."
  :type 'string
  :group 'allout)
;;;_  - outline-indent
(defcustom outline-indent ".3cm"
  "*LaTeX formatted depth-indent spacing."
  :type 'string
  :group 'allout)

;;;_ + Miscellaneous customization

;;;_  = outline-command-prefix
(defcustom outline-command-prefix "\C-c"
  "*Key sequence to be used as prefix for outline mode command key bindings."
  :type 'string
  :group 'allout)

;;;_  = outline-keybindings-list
;;; You have to reactivate outline-mode - `(outline-mode t)' - to
;;; institute changes to this var.
(defvar outline-keybindings-list ()
  "*List of outline-mode key / function bindings, for outline-mode-map.

String or vector key will be prefaced with outline-command-prefix,
unless optional third, non-nil element is present.")
(setq outline-keybindings-list
      '(
                                        ; Motion commands:
        ("\C-n" outline-next-visible-heading)
        ("\C-p" outline-previous-visible-heading)
        ("\C-u" outline-up-current-level)
        ("\C-f" outline-forward-current-level)
        ("\C-b" outline-backward-current-level)
        ("\C-a" outline-beginning-of-current-entry)
        ("\C-e" outline-end-of-current-entry)
                                        ; Exposure commands:
        ("\C-i" outline-show-children)
        ("\C-s" outline-show-current-subtree)
        ("\C-h" outline-hide-current-subtree)
        ("\C-o" outline-show-current-entry)
        ("!" outline-show-all)
                                        ; Alteration commands:
        (" " outline-open-sibtopic)
        ("." outline-open-subtopic)
        ("," outline-open-supertopic)
        ("'" outline-shift-in)
        (">" outline-shift-in)
        ("<" outline-shift-out)
        ("\C-m" outline-rebullet-topic)
        ("*" outline-rebullet-current-heading)
        ("#" outline-number-siblings)
        ("\C-k" outline-kill-line t)
        ("\C-y" outline-yank t)
        ("\M-y" outline-yank-pop t)
        ("\C-k" outline-kill-topic)
                                        ; Miscellaneous commands:
	;([?\C-\ ] outline-mark-topic)
        ("@" outline-resolve-xref)
        ("=c" outline-copy-exposed-to-buffer)
        ("=i" outline-indented-exposed-to-buffer)
	("=t" outline-latexify-exposed)
	("=p" outline-flatten-exposed-to-buffer)))

;;;_  = outline-isearch-dynamic-expose
(defcustom outline-isearch-dynamic-expose t
  "*Non-nil enable dynamic exposure of hidden incremental-search
targets as they're encountered."
  :type 'boolean
  :group 'allout)
(make-variable-buffer-local 'outline-isearch-dynamic-expose)

;;;_  = outline-use-hanging-indents
(defcustom outline-use-hanging-indents t
  "*If non-nil, topic body text auto-indent defaults to indent of the header.
Ie, it is indented to be just past the header prefix.  This is
relevant mostly for use with indented-text-mode, or other situations
where auto-fill occurs.

\[This feature no longer depends in any way on the `filladapt.el'
lisp-archive package.\]"
  :type 'boolean
  :group 'allout)
(make-variable-buffer-local 'outline-use-hanging-indents)

;;;_  = outline-reindent-bodies
(defcustom outline-reindent-bodies (if outline-use-hanging-indents
				    'text)
  "*Non-nil enables auto-adjust of topic body hanging indent with depth shifts.

When active, topic body lines that are indented even with or beyond
their topic header are reindented to correspond with depth shifts of
the header.

A value of `t' enables reindent in non-programming-code buffers, ie
those that do not have the variable `comment-start' set.  A value of
`force' enables reindent whether or not `comment-start' is set."
  :type '(choice (const nil) (const t) (const text) (const force))
  :group 'allout)

(make-variable-buffer-local 'outline-reindent-bodies)

;;;_  = outline-inhibit-protection
(defcustom outline-inhibit-protection nil
  "*Non-nil disables warnings and confirmation-checks for concealed-text edits.

Outline mode uses emacs change-triggered functions to detect unruly
changes to concealed regions.  Set this var non-nil to disable the
protection, potentially increasing text-entry responsiveness a bit.

This var takes effect at outline-mode activation, so you may have to
deactivate and then reactivate the mode if you want to toggle the
behavior."
  :type 'boolean
  :group 'allout)

;;;_* CODE - no user customizations below.

;;;_ #1 Internal Outline Formatting and Configuration
;;;_  : Version
;;;_   = outline-version
(defvar outline-version
  (let ((rcs-rev "$Revision: 1.31 $"))
    (condition-case err
	(save-match-data
	  (string-match "Revision: \\([0-9]+\\.[0-9]+\\)" rcs-rev)
	  (substring rcs-rev (match-beginning 1) (match-end 1)))
      ('error rcs-rev)))
  "Revision number of currently loaded outline package.  \(allout.el)")
;;;_   > outline-version
(defun outline-version (&optional here)
  "Return string describing the loaded outline version."
  (interactive "P")
  (let ((msg (concat "Allout Outline Mode v " outline-version)))
    (if here (insert msg))
    (message "%s" msg)
    msg))
;;;_  : Topic header format
;;;_   = outline-regexp
(defvar outline-regexp ""
  "*Regular expression to match the beginning of a heading line.

Any line whose beginning matches this regexp is considered a
heading.  This var is set according to the user configuration vars
by set-outline-regexp.")
(make-variable-buffer-local 'outline-regexp)
;;;_   = outline-bullets-string
(defvar outline-bullets-string ""
  "A string dictating the valid set of outline topic bullets.

This var should *not* be set by the user - it is set by `set-outline-regexp',
and is produced from the elements of `outline-plain-bullets-string'
and `outline-distinctive-bullets-string'.")
(make-variable-buffer-local 'outline-bullets-string)
;;;_   = outline-bullets-string-len
(defvar outline-bullets-string-len 0
  "Length of current buffers' outline-plain-bullets-string.")
(make-variable-buffer-local 'outline-bullets-string-len)
;;;_   = outline-line-boundary-regexp
(defvar outline-line-boundary-regexp ()
  "Outline-regexp with outline-style beginning-of-line anchor.

\(Ie, C-j, *or* C-m, for prefixes of hidden topics).  This is properly
set when outline-regexp is produced by `set-outline-regexp', so
that (match-beginning 2) and (match-end 2) delimit the prefix.")
(make-variable-buffer-local 'outline-line-boundary-regexp)
;;;_   = outline-bob-regexp
(defvar outline-bob-regexp ()
  "Like outline-line-boundary-regexp, for headers at beginning of buffer.
\(match-beginning 2) and \(match-end 2) delimit the prefix.")
(make-variable-buffer-local 'outline-bob-regexp)
;;;_   = outline-header-subtraction
(defvar outline-header-subtraction (1- (length outline-header-prefix))
  "Outline-header prefix length to subtract when computing topic depth.")
(make-variable-buffer-local 'outline-header-subtraction)
;;;_   = outline-plain-bullets-string-len
(defvar outline-plain-bullets-string-len (length outline-plain-bullets-string)
  "Length of outline-plain-bullets-string, updated by set-outline-regexp.")
(make-variable-buffer-local 'outline-plain-bullets-string-len)


;;;_   X outline-reset-header-lead (header-lead)
(defun outline-reset-header-lead (header-lead)
  "*Reset the leading string used to identify topic headers."
  (interactive "sNew lead string: ")
  (setq outline-header-prefix header-lead)
  (setq outline-header-subtraction (1- (length outline-header-prefix)))
  (set-outline-regexp))
;;;_   X outline-lead-with-comment-string (header-lead)
(defun outline-lead-with-comment-string (&optional header-lead)
  "*Set the topic-header leading string to specified string.

Useful when for encapsulating outline structure in programming
language comments.  Returns the leading string."

  (interactive "P")
  (if (not (stringp header-lead))
      (setq header-lead (read-string
                         "String prefix for topic headers: ")))
  (setq outline-reindent-bodies nil)
  (outline-reset-header-lead header-lead)
  header-lead)
;;;_   > outline-infer-header-lead ()
(defun outline-infer-header-lead ()
  "Determine appropriate `outline-header-prefix'.

Works according to settings of:

       `comment-start'
       `outline-header-prefix' (default)
       `outline-use-mode-specific-leader'
and    `outline-mode-leaders'.

Apply this via \(re)activation of `outline-mode', rather than
invoking it directly."
  (let* ((use-leader (and (boundp 'outline-use-mode-specific-leader)
			  (if (or (stringp outline-use-mode-specific-leader)
				  (memq outline-use-mode-specific-leader
					'(outline-mode-leaders
					  comment-start
					  t)))
			      outline-use-mode-specific-leader
			    ;; Oops - garbled value, equate with effect of 't:
			    t)))
	 (leader
	  (cond
	   ((not use-leader) nil)
	   ;; Use the explicitly designated leader:
	   ((stringp use-leader) use-leader)
	   (t (or (and (memq use-leader '(t outline-mode-leaders))
		       ;; Get it from outline mode leaders?
		       (cdr (assq major-mode outline-mode-leaders)))
		  ;; ... didn't get from outline-mode-leaders...
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
      (if (string= leader outline-header-prefix)
	  nil				; no change, nothing to do.
	(setq outline-header-prefix leader)
	outline-header-prefix))))
;;;_   > outline-infer-body-reindent ()
(defun outline-infer-body-reindent ()
  "Determine proper setting for `outline-reindent-bodies'.

Depends on default setting of `outline-reindent-bodies' \(which see)
and presence of setting for `comment-start', to tell whether the
file is programming code."
  (if (and outline-reindent-bodies
	   comment-start
	   (not (eq 'force outline-reindent-bodies)))
      (setq outline-reindent-bodies nil)))
;;;_   > set-outline-regexp ()
(defun set-outline-regexp ()
  "Generate proper topic-header regexp form for outline functions.

Works with respect to `outline-plain-bullets-string' and
`outline-distinctive-bullets-string'."

  (interactive)
  ;; Derive outline-bullets-string from user configured components:
  (setq outline-bullets-string "")
  (let ((strings (list 'outline-plain-bullets-string
                       'outline-distinctive-bullets-string
                       'outline-primary-bullet))
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
        (setq outline-bullets-string
              (concat outline-bullets-string
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
  ;; Derive next for repeated use in outline-pending-bullet:
  (setq outline-plain-bullets-string-len (length outline-plain-bullets-string))
  (setq outline-header-subtraction (1- (length outline-header-prefix)))
  ;; Produce the new outline-regexp:
  (setq outline-regexp (concat "\\(\\"
                               outline-header-prefix
                               "[ \t]*["
                               outline-bullets-string
                               "]\\)\\|\\"
                               outline-primary-bullet
                               "+\\|\^l"))
  (setq outline-line-boundary-regexp
        (concat "\\([\n\r]\\)\\(" outline-regexp "\\)"))
  (setq outline-bob-regexp
        (concat "\\(\\`\\)\\(" outline-regexp "\\)"))
  )
;;;_  : Key bindings
;;;_   = outline-mode-map
(defvar outline-mode-map nil "Keybindings for (allout) outline minor mode.")
;;;_   > produce-outline-mode-map (keymap-alist &optional base-map)
(defun produce-outline-mode-map (keymap-list &optional base-map)
  "Produce keymap for use as outline-mode-map, from keymap-list.

Built on top of optional BASE-MAP, or empty sparse map if none specified.
See doc string for outline-keybindings-list for format of binding list."
  (let ((map (or base-map (make-sparse-keymap)))
	(pref (list outline-command-prefix)))
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
;;;_   = outline-prior-bindings - being deprecated.
(defvar outline-prior-bindings nil
  "Variable for use in V18, with outline-added-bindings, for
resurrecting, on mode deactivation, bindings that existed before
activation.  Being deprecated.")
;;;_   = outline-added-bindings - being deprecated
(defvar outline-added-bindings nil
  "Variable for use in V18, with outline-prior-bindings, for
resurrecting, on mode deactivation, bindings that existed before
activation.  Being deprecated.")
;;;_  : Menu bar
(defun produce-outline-mode-menubar-entries ()
  (require 'easymenu)
  (easy-menu-define outline-mode-exposure-menu
		    outline-mode-map
		    "Allout outline exposure menu."
		    '("Exposure"
		      ["Show Entry" outline-show-current-entry t]
		      ["Show Children" outline-show-children t]
		      ["Show Subtree" outline-show-current-subtree t]
		      ["Hide Subtree" outline-hide-current-subtree t]
		      ["Hide Leaves" outline-hide-current-leaves t]
		      "----"
		      ["Show All" outline-show-all t]))
  (easy-menu-define outline-mode-editing-menu
		    outline-mode-map
		    "Allout outline editing menu."
		    '("Headings"
		      ["Open Sibling" outline-open-sibtopic t]
		      ["Open Subtopic" outline-open-subtopic t]
		      ["Open Supertopic" outline-open-supertopic t]
		      "----"
		      ["Shift Topic In" outline-shift-in t]
		      ["Shift Topic Out" outline-shift-out t]
		      ["Rebullet Topic" outline-rebullet-topic t]
		      ["Rebullet Heading" outline-rebullet-current-heading t]
		      ["Number Siblings" outline-number-siblings t]))
  (easy-menu-define outline-mode-navigation-menu
		    outline-mode-map
		    "Allout outline navigation menu."
		    '("Navigation"
		      ["Next Visible Heading" outline-next-visible-heading t]
		      ["Previous Visible Heading"
		       outline-previous-visible-heading t]
		      "----"
		      ["Up Level" outline-up-current-level t]
		      ["Forward Current Level" outline-forward-current-level t]
		      ["Backward Current Level"
		       outline-backward-current-level t]
		      "----"
		      ["Beginning of Entry"
		       outline-beginning-of-current-entry t]
		      ["End of Entry" outline-end-of-current-entry t]
		      ["End of Subtree" outline-end-of-current-subtree t]))
  (easy-menu-define outline-mode-misc-menu
		    outline-mode-map
		    "Allout outlines miscellaneous bindings."
		    '("Misc"
		      ["Version" outline-version t]
		      "----"
		      ["Duplicate Exposed" outline-copy-exposed-to-buffer t]
		      ["Duplicate Exposed, numbered"
		       outline-flatten-exposed-to-buffer t] 
		      ["Duplicate Exposed, indented"
		       outline-indented-exposed-to-buffer t] 
		      "----"
		      ["Set Header Lead" outline-reset-header-lead t]
		      ["Set New Exposure" outline-expose-topic t])))
;;;_  : Mode-Specific Variable Maintenance Utilities
;;;_   = outline-mode-prior-settings
(defvar outline-mode-prior-settings nil
  "Internal outline mode use; settings to be resumed on mode deactivation.")
(make-variable-buffer-local 'outline-mode-prior-settings)
;;;_   > outline-resumptions (name &optional value)
(defun outline-resumptions (name &optional value)

  "Registers or resumes settings over outline-mode activation/deactivation.

First arg is NAME of variable affected.  Optional second arg is list
containing outline-mode-specific VALUE to be imposed on named
variable, and to be registered.  (It's a list so you can specify
registrations of null values.)  If no value is specified, the
registered value is returned (encapsulated in the list, so the caller
can distinguish nil vs no value), and the registration is popped
from the list."

  (let ((on-list (assq name outline-mode-prior-settings))
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
            (setq outline-mode-prior-settings
                  (cons (list name
                              (if (boundp name) (list (symbol-value name))))
                        outline-mode-prior-settings)))
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
          (while outline-mode-prior-settings
            (if (not (eq (car outline-mode-prior-settings)
                         on-list))
                (setq rebuild
                      (cons (car outline-mode-prior-settings)
                            rebuild)))
            (setq outline-mode-prior-settings
                  (cdr outline-mode-prior-settings)))
          (setq outline-mode-prior-settings rebuild)))))
  )
;;;_  : Mode-specific incidentals
;;;_   = outline-during-write-cue nil
(defvar outline-during-write-cue nil
  "Used to inhibit outline change-protection during file write.

See also `outline-post-command-business', `outline-write-file-hook',
`outline-before-change-protect', and `outline-post-command-business'
functions.")
;;;_   = outline-pre-was-isearching nil
(defvar outline-pre-was-isearching nil
  "Cue for isearch-dynamic-exposure mechanism, implemented in
outline-pre- and -post-command-hooks.")
(make-variable-buffer-local 'outline-pre-was-isearching)
;;;_   = outline-isearch-prior-pos nil
(defvar outline-isearch-prior-pos nil
  "Cue for isearch-dynamic-exposure tracking, used by outline-isearch-expose.")
(make-variable-buffer-local 'outline-isearch-prior-pos)
;;;_   = outline-isearch-did-quit
(defvar outline-isearch-did-quit nil
  "Distinguishes isearch conclusion and cancellation.

Maintained by outline-isearch-abort \(which is wrapped around the real
isearch-abort), and monitored by outline-isearch-expose for action.")
(make-variable-buffer-local 'outline-isearch-did-quit)
;;;_   = outline-override-protect nil
(defvar outline-override-protect nil
  "Used in outline-mode for regulate of concealed-text protection mechanism.

Allout outline mode regulates alteration of concealed text to protect
against inadvertent, unnoticed changes.  This is for use by specific,
native outline functions to temporarily override that protection.
It's automatically reset to nil after every buffer modification.")
(make-variable-buffer-local 'outline-override-protect)
;;;_   > outline-unprotected (expr)
(defmacro outline-unprotected (expr)
  "Evaluate EXPRESSION with `outline-override-protect' let-bound `t'."
  `(let ((outline-override-protect t))
     ,expr))
;;;_   = outline-undo-aggregation
(defvar outline-undo-aggregation 30
  "Amount of successive self-insert actions to bunch together per undo.

This is purely a kludge variable, regulating the compensation for a bug in
the way that before-change-functions and undo interact.")
(make-variable-buffer-local 'outline-undo-aggregation)
;;;_   = file-var-bug hack
(defvar outline-v18/19-file-var-hack nil
  "Horrible hack used to prevent invalid multiple triggering of outline
mode from prop-line file-var activation.  Used by outline-mode function
to track repeats.")
;;;_   > outline-write-file-hook ()
(defun outline-write-file-hook ()
  "In outline mode, run as a local-write-file-hooks activity.

Currently just sets `outline-during-write-cue', so outline-change-protection
knows to keep inactive during file write."
  (setq outline-during-write-cue t)
  nil)

;;;_ #2 Mode activation
;;;_  = outline-mode
(defvar outline-mode () "Allout outline mode minor-mode flag.")
(make-variable-buffer-local 'outline-mode)
;;;_  > outline-mode-p ()
(defmacro outline-mode-p ()
  "Return t if outline-mode is active in current buffer."
  'outline-mode)
;;;_  = outline-explicitly-deactivated
(defvar outline-explicitly-deactivated nil
  "Outline-mode was last deliberately deactivated.
So outline-post-command-business should not reactivate it...")
(make-variable-buffer-local 'outline-explicitly-deactivated)
;;;_  > outline-init (&optional mode)
(defun outline-init (&optional mode)
  "Prime outline-mode to enable/disable auto-activation, wrt `outline-layout'.

MODE is one of the following symbols:

 - nil \(or no argument) deactivate auto-activation/layout;
 - `activate', enable auto-activation only;
 - `ask', enable auto-activation, and enable auto-layout but with
   confirmation for layout operation solicited from user each time;
 - `report', just report and return the current auto-activation state;
 - anything else \(eg, t) for auto-activation and auto-layout, without
   any confirmation check.

Use this function to setup your emacs session for automatic activation
of allout outline mode, contingent to the buffer-specific setting of
the `outline-layout' variable.  (See `outline-layout' and
`outline-expose-topic' docstrings for more details on auto layout).

`outline-init' works by setting up (or removing) the outline-mode
find-file-hook, and giving `outline-auto-activation' a suitable
setting.

To prime your emacs session for full auto-outline operation, include
the following two lines in your emacs init file:

\(require 'allout)
\(outline-init t)"

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
      ((hook 'outline-find-file-hook)
       (curr-mode 'outline-auto-activation))

    (cond ((not mode)
	   (setq find-file-hooks (delq hook find-file-hooks))
	   (if (interactive-p)
	       (message "Allout outline mode auto-activation inhibited.")))
	  ((eq mode 'report)
	   (if (not (memq hook find-file-hooks))
	       (outline-init nil)
	     ;; Just punt and use the reports from each of the modes:
	     (outline-init (symbol-value curr-mode))))
	  (t (add-hook 'find-file-hooks hook)
	     (set curr-mode		; `set', not `setq'!
		  (cond ((eq mode 'activate)
			 (message
			  "Outline mode auto-activation enabled.")
			 'activate)
			((eq mode 'report)
			 ;; Return the current mode setting:
			 (outline-init mode))
			((eq mode 'ask)
			 (message
			  (concat "Outline mode auto-activation and "
				  "-layout \(upon confirmation) enabled."))
			 'ask)
			((message
			  "Outline mode auto-activation and -layout enabled.")
			 'full)))))))
		   
;;;_  > outline-setup-menubar ()
(defun outline-setup-menubar ()
  "Populate the current buffer's menubar with allout outline-mode stuff."
  (let ((menus (list outline-mode-exposure-menu
		     outline-mode-editing-menu
		     outline-mode-navigation-menu
		     outline-mode-misc-menu))
	cur)
    (while menus
      (setq cur (car menus)
	    menus (cdr menus))
      (easy-menu-add cur))))
;;;_  > outline-mode (&optional toggle)
;;;_   : Defun:
(defun outline-mode (&optional toggle)
;;;_    . Doc string:
  "Toggle minor mode for controlling exposure and editing of text outlines.

Optional arg forces mode to re-initialize iff arg is positive num or
symbol.  Allout outline mode always runs as a minor mode.

Allout outline mode provides extensive outline-oriented formatting and
manipulation.  It enables structural editing of outlines, as well as
navigation and exposure.  It also is specifically aimed at
accommodating syntax-sensitive text like programming languages.  \(For
an example, see the allout code itself, which is organized as an allout
outline.)

In addition to outline navigation and exposure, allout includes:

 - topic-oriented repositioning, cut, and paste
 - integral outline exposure-layout
 - incremental search with dynamic exposure and reconcealment of hidden text
 - automatic topic-number maintenance
 - \"Hot-spot\" operation, for single-keystroke maneuvering and
    exposure control.  \(See the outline-mode docstring.)

and many other features.

Below is a description of the bindings, and then explanation of
special outline-mode features and terminology.  See also the outline
menubar additions for quick reference to many of the features, and see
the docstring of the variable `outline-init' for instructions on
priming your emacs session for automatic activation of outline-mode.


The bindings are dictated by the `outline-keybindings-list' and
`outline-command-prefix' variables.

	Navigation:				   Exposure Control:
	----------                                 ----------------
C-c C-n outline-next-visible-heading     | C-c C-h outline-hide-current-subtree
C-c C-p outline-previous-visible-heading | C-c C-i outline-show-children
C-c C-u outline-up-current-level         | C-c C-s outline-show-current-subtree
C-c C-f outline-forward-current-level    | C-c C-o outline-show-current-entry
C-c C-b outline-backward-current-level   | ^U C-c C-s outline-show-all
C-c C-e outline-end-of-current-entry     |	   outline-hide-current-leaves
C-c C-a outline-beginning-of-current-entry, alternately, goes to hot-spot

	Topic Header Production:
	-----------------------
C-c<SP>	outline-open-sibtopic	Create a new sibling after current topic.
C-c .	outline-open-subtopic	... an offspring of current topic.
C-c ,	outline-open-supertopic	... a sibling of the current topic's parent.

	Topic Level and Prefix Adjustment:
	---------------------------------
C-c >	outline-shift-in	Shift current topic and all offspring deeper.
C-c <	outline-shift-out	... less deep.
C-c<CR>	outline-rebullet-topic	Reconcile bullets of topic and its offspring
				- distinctive bullets are not changed, others
				  alternated according to nesting depth.
C-c b	outline-rebullet-current-heading Prompt for alternate bullet for
					 current topic.
C-c #	outline-number-siblings	Number bullets of topic and siblings - the
				offspring are not affected.  With repeat
				count, revoke numbering.

	Topic-oriented Killing and Yanking:
	----------------------------------
C-c C-k	outline-kill-topic	Kill current topic, including offspring.
C-k	outline-kill-line	Like kill-line, but reconciles numbering, etc.
C-y	outline-yank		Yank, adjusting depth of yanked topic to
				depth of heading if yanking into bare topic
				heading (ie, prefix sans text).
M-y	outline-yank-pop	Is to outline-yank as yank-pop is to yank

	Misc commands:
	-------------
M-x outlineify-sticky		Activate outline mode for current buffer,
				and establish a default file-var setting
				for `outline-layout'.
C-c C-SPC outline-mark-topic
C-c = c	outline-copy-exposed-to-buffer
				Duplicate outline, sans concealed text, to
				buffer with name derived from derived from
				that of current buffer - \"*XXX exposed*\".
C-c = p	outline-flatten-exposed-to-buffer
				Like above 'copy-exposed', but convert topic
				prefixes to section.subsection... numeric
				format.
ESC ESC (outline-init t)	Setup emacs session for outline mode
				auto-activation.

		 HOT-SPOT Operation

Hot-spot operation provides a means for easy, single-keystroke outline
navigation and exposure control.

\\<outline-mode-map>
When the text cursor is positioned directly on the bullet character of
a topic, regular characters (a to z) invoke the commands of the
corresponding outline-mode keymap control chars.  For example, \"f\"
would invoke the command typically bound to \"C-c C-f\"
\(\\[outline-forward-current-level] `outline-forward-current-level').

Thus, by positioning the cursor on a topic bullet, you can execute
the outline navigation and manipulation commands with a single
keystroke.  Non-literal chars never get this special translation, so
you can use them to get away from the hot-spot, and back to normal
operation.

Note that the command `outline-beginning-of-current-entry' \(\\[outline-beginning-of-current-entry]\)
will move to the hot-spot when the cursor is already located at the
beginning of the current entry, so you can simply hit \\[outline-beginning-of-current-entry]
twice in a row to get to the hot-spot.

			    Terminology

Topic hierarchy constituents - TOPICS and SUBTOPICS:

TOPIC:	A basic, coherent component of an emacs outline.  It can
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
PREFIX: The leading text of a topic which which distinguishes it from
	normal text.  It has a strict form, which consists of a
	prefix-lead string, padding, and a bullet.  The bullet may be
	followed by a number, indicating the ordinal number of the
	topic among its siblings, a space, and then the header text.

	The relative length of the PREFIX determines the nesting depth
	of the topic.
PREFIX-LEAD:
	The string at the beginning of a topic prefix, normally a `.'.
	It can be customized by changing the setting of
	`outline-header-prefix' and then reinitializing outline-mode.

	By setting the prefix-lead to the comment-string of a
	programming language, you can embed outline-structuring in
	program code without interfering with the language processing
	of that code.  See `outline-use-mode-specific-leader'
	docstring for more detail.
PREFIX-PADDING:
	Spaces or asterisks which separate the prefix-lead and the
	bullet, according to the depth of the topic.
BULLET: A character at the end of the topic prefix, it must be one of
	the characters listed on `outline-plain-bullets-string' or
        `outline-distinctive-bullets-string'.  (See the documentation
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
		     (outline-mode-p)))
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
	 ;; outline-mode already called once during this complex command?
	 (same-complex-command (eq outline-v18/19-file-var-hack
				  (car command-history)))
	 do-layout
	 )

				       ; See comments below re v19.18,.19 bug.
    (setq outline-v18/19-file-var-hack (car command-history))

    (cond

     ;; Provision for v19.18, 19.19 bug -
     ;; Emacs v 19.18, 19.19 file-var code invokes prop-line-designated
     ;; modes twice when file is visited.  We have to avoid toggling mode
     ;; off on second invocation, so we detect it as best we can, and
     ;; skip everything.
     ((and same-complex-command		; Still in same complex command
				       ; as last time outline-mode invoked.
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
      (setq outline-explicitly-deactivated t)
      (if (string-match "^18\." emacs-version)
				       ; Revoke those keys that remain
				       ; as we set them:
	  (let ((curr-loc (current-local-map)))
	   (mapcar (function
		    (lambda (cell)
		      (if (eq (lookup-key curr-loc (car cell))
			      (car (cdr cell)))
			  (define-key curr-loc (car cell)
			    (assq (car cell) outline-prior-bindings)))))
		   outline-added-bindings)
	   (outline-resumptions 'outline-added-bindings)
	   (outline-resumptions 'outline-prior-bindings)))

      (if outline-old-style-prefixes
	  (progn
	   (outline-resumptions 'outline-primary-bullet)
	   (outline-resumptions 'outline-old-style-prefixes)))
      (outline-resumptions 'selective-display)
      (if (and (boundp 'before-change-functions) before-change-functions)
	  (outline-resumptions 'before-change-functions))
      (setq local-write-file-hooks
	   (delq 'outline-write-file-hook
		 local-write-file-hooks))
      (outline-resumptions 'paragraph-start)
      (outline-resumptions 'paragraph-separate)
      (outline-resumptions (if (string-match "^18" emacs-version)
			      'auto-fill-hook
			    'auto-fill-function))
      (outline-resumptions 'outline-former-auto-filler)
      (setq outline-mode nil))

     ;; Activation:
     ((not active)
      (setq outline-explicitly-deactivated nil)
      (if outline-old-style-prefixes
	  (progn			; Inhibit all the fancy formatting:
	   (outline-resumptions 'outline-primary-bullet '("*"))
	   (outline-resumptions 'outline-old-style-prefixes '(()))))

      (outline-infer-header-lead)
      (outline-infer-body-reindent)

      (set-outline-regexp)

				       ; Produce map from current version
				       ; of outline-keybindings-list:
      (if (boundp 'minor-mode-map-alist)

	  (progn			; V19, and maybe lucid and
				       ; epoch, minor-mode key bindings:
	   (setq outline-mode-map
		 (produce-outline-mode-map outline-keybindings-list))
	   (produce-outline-mode-menubar-entries)
	   (fset 'outline-mode-map outline-mode-map)
				       ; Include on minor-mode-map-alist,
				       ; if not already there:
	   (if (not (member '(outline-mode . outline-mode-map)
			    minor-mode-map-alist))
	       (setq minor-mode-map-alist
		     (cons '(outline-mode . outline-mode-map)
			   minor-mode-map-alist))))

				       ; V18 minor-mode key bindings:
				       ; Stash record of added bindings
				       ; for later revocation:
	(outline-resumptions 'outline-added-bindings
			    (list outline-keybindings-list))
	(outline-resumptions 'outline-prior-bindings
			    (list (current-local-map)))
				       ; and add them:
	(use-local-map (produce-outline-mode-map outline-keybindings-list
						(current-local-map)))
	)

				       ; selective-display is the
				       ; emacs conditional exposure
				       ; mechanism:
      (outline-resumptions 'selective-display '(t))
      (if outline-inhibit-protection
	  t
	(outline-resumptions 'before-change-functions
			    '(outline-before-change-protect)))
      (add-hook 'pre-command-hook 'outline-pre-command-business)
      (add-hook 'post-command-hook 'outline-post-command-business)
				       ; Temporarily set by any outline
				       ; functions that can be trusted to
				       ; deal properly with concealed text.
      (add-hook 'local-write-file-hooks 'outline-write-file-hook)
				       ; Custom auto-fill func, to support
				       ; respect for topic headline,
				       ; hanging-indents, etc:
      (let* ((fill-func-var (if (string-match "^18" emacs-version)
			       'auto-fill-hook
			     'auto-fill-function))
	    (fill-func (symbol-value fill-func-var)))
	;; Register prevailing fill func for use by outline-auto-fill:
	(outline-resumptions 'outline-former-auto-filler (list fill-func))
	;; Register outline-auto-fill to be used if filling is active:
	(outline-resumptions fill-func-var '(outline-auto-fill)))
      ;; Paragraphs are broken by topic headlines.
      (make-local-variable 'paragraph-start)
      (outline-resumptions 'paragraph-start
			  (list (concat paragraph-start "\\|^\\("
					outline-regexp "\\)")))
      (make-local-variable 'paragraph-separate)
      (outline-resumptions 'paragraph-separate
			  (list (concat paragraph-separate "\\|^\\("
					outline-regexp "\\)")))

      (or (assq 'outline-mode minor-mode-alist)
	  (setq minor-mode-alist
	       (cons '(outline-mode " Outl") minor-mode-alist)))

      (outline-setup-menubar)

      (if outline-layout
	  (setq do-layout t))

      (if (and outline-isearch-dynamic-expose
	       (not (fboundp 'outline-real-isearch-abort)))
	  (outline-enwrap-isearch))

      (run-hooks 'outline-mode-hook)
      (setq outline-mode t))

     ;; Reactivation:
     ((setq do-layout t)
      (outline-infer-body-reindent))
     )					; cond

    (if (and do-layout
	     outline-auto-activation
	     (listp outline-layout)
	     (and (not (eq outline-auto-activation 'activate))
		  (if (eq outline-auto-activation 'ask)
		      (if (y-or-n-p (format "Expose %s with layout '%s'? "
					    (buffer-name)
					    outline-layout))
			  t
			(message "Skipped %s layout." (buffer-name))
			nil)
		    t)))
	(save-excursion
	  (message "Adjusting '%s' exposure..." (buffer-name))
	  (goto-char 0)
	  (outline-this-or-next-heading)
	  (condition-case err
	      (progn
		(apply 'outline-expose-topic (list outline-layout))
		(message "Adjusting '%s' exposure... done." (buffer-name)))
	    ;; Problem applying exposure - notify user, but don't
	    ;; interrupt, eg, file visit:
	    (error (message "%s" (car (cdr err)))
		   (sit-for 1)))))
    outline-mode
    )					; let*
  )  					; defun
;;;_  > outline-minor-mode
;;; XXX released verion doesn't do this?
(defalias 'outline-minor-mode 'outline-mode)

;;;_ #3 Internal Position State-Tracking - "outline-recent-*" funcs
;;; All the basic outline functions that directly do string matches to
;;; evaluate heading prefix location set the variables
;;; `outline-recent-prefix-beginning'  and `outline-recent-prefix-end'
;;; when successful.  Functions starting with `outline-recent-' all
;;; use this state, providing the means to avoid redundant searches
;;; for just-established data.  This optimization can provide
;;; significant speed improvement, but it must be employed carefully.
;;;_  = outline-recent-prefix-beginning
(defvar outline-recent-prefix-beginning 0
  "Buffer point of the start of the last topic prefix encountered.")
(make-variable-buffer-local 'outline-recent-prefix-beginning)
;;;_  = outline-recent-prefix-end
(defvar outline-recent-prefix-end 0
  "Buffer point of the end of the last topic prefix encountered.")
(make-variable-buffer-local 'outline-recent-prefix-end)
;;;_  = outline-recent-end-of-subtree
(defvar outline-recent-end-of-subtree 0
  "Buffer point last returned by outline-end-of-current-subtree.")
(make-variable-buffer-local 'outline-recent-end-of-subtree)
;;;_  > outline-prefix-data (beg end)
(defmacro outline-prefix-data (beg end)
  "Register outline-prefix state data - BEGINNING and END of prefix.

For reference by `outline-recent' funcs.  Returns BEGINNING."
  `(setq outline-recent-prefix-end ,end
         outline-recent-prefix-beginning ,beg))
;;;_  > outline-recent-depth ()
(defmacro outline-recent-depth ()
  "Return depth of last heading encountered by an outline maneuvering function.

All outline functions which directly do string matches to assess
headings set the variables outline-recent-prefix-beginning and
outline-recent-prefix-end if successful.  This function uses those settings
to return the current depth."

  '(max 1 (- outline-recent-prefix-end
	     outline-recent-prefix-beginning
	     outline-header-subtraction)))
;;;_  > outline-recent-prefix ()
(defmacro outline-recent-prefix ()
  "Like outline-recent-depth, but returns text of last encountered prefix.

All outline functions which directly do string matches to assess
headings set the variables outline-recent-prefix-beginning and
outline-recent-prefix-end if successful.  This function uses those settings
to return the current depth."
  '(buffer-substring outline-recent-prefix-beginning
		     outline-recent-prefix-end))
;;;_  > outline-recent-bullet ()
(defmacro outline-recent-bullet ()
  "Like outline-recent-prefix, but returns bullet of last encountered prefix.

All outline functions which directly do string matches to assess
headings set the variables outline-recent-prefix-beginning and
outline-recent-prefix-end if successful.  This function uses those settings
to return the current depth of the most recently matched topic."
  '(buffer-substring (1- outline-recent-prefix-end)
		     outline-recent-prefix-end))

;;;_ #4 Navigation

;;;_  - Position Assessment
;;;_   : Location Predicates
;;;_    > outline-on-current-heading-p ()
(defun outline-on-current-heading-p ()
  "Return non-nil if point is on current visible topics' header line.

Actually, returns prefix beginning point."
  (save-excursion
    (beginning-of-line)
    (and (looking-at outline-regexp)
	 (outline-prefix-data (match-beginning 0) (match-end 0)))))
;;;_    > outline-on-heading-p ()
(defalias 'outline-on-heading-p 'outline-on-current-heading-p)
;;;_    > outline-e-o-prefix-p ()
(defun outline-e-o-prefix-p ()
  "True if point is located where current topic prefix ends, heading begins."
  (and (save-excursion (beginning-of-line)
		       (looking-at outline-regexp))
       (= (point)(save-excursion (outline-end-of-prefix)(point)))))
;;;_    > outline-hidden-p ()
(defmacro outline-hidden-p ()
  "True if point is in hidden text."
  '(save-excursion
     (and (re-search-backward "[\n\r]" () t)
	  (= ?\r (following-char)))))
;;;_    > outline-visible-p ()
(defmacro outline-visible-p ()
  "True if point is not in hidden text."
  (interactive)
  '(not (outline-hidden-p)))
;;;_   : Location attributes
;;;_    > outline-depth ()
(defsubst outline-depth ()
  "Like outline-current-depth, but respects hidden as well as visible topics."
  (save-excursion
    (if (outline-goto-prefix)
	(outline-recent-depth)
      (progn
	;; Oops, no prefix, zero prefix data:
	(outline-prefix-data (point)(point))
	;; ... and return 0:
	0))))
;;;_    > outline-current-depth ()
(defmacro outline-current-depth ()
  "Return nesting depth of visible topic most immediately containing point."
  '(save-excursion
     (if (outline-back-to-current-heading)
	 (max 1
	      (- outline-recent-prefix-end
		 outline-recent-prefix-beginning
		 outline-header-subtraction))
       0)))
;;;_    > outline-get-current-prefix ()
(defun outline-get-current-prefix ()
  "Topic prefix of the current topic."
  (save-excursion
    (if (outline-goto-prefix)
	(outline-recent-prefix))))
;;;_    > outline-get-bullet ()
(defun outline-get-bullet ()
  "Return bullet of containing topic (visible or not)."
  (save-excursion
    (and (outline-goto-prefix)
	 (outline-recent-bullet))))
;;;_    > outline-current-bullet ()
(defun outline-current-bullet ()
  "Return bullet of current (visible) topic heading, or none if none found."
  (condition-case err
      (save-excursion
	(outline-back-to-current-heading)
	(buffer-substring (- outline-recent-prefix-end 1)
			  outline-recent-prefix-end))
    ;; Quick and dirty provision, ostensibly for missing bullet:
    ('args-out-of-range nil))
  )
;;;_    > outline-get-prefix-bullet (prefix)
(defun outline-get-prefix-bullet (prefix)
  "Return the bullet of the header prefix string PREFIX."
  ;; Doesn't make sense if we're old-style prefixes, but this just
  ;; oughtn't be called then, so forget about it...
  (if (string-match outline-regexp prefix)
      (substring prefix (1- (match-end 0)) (match-end 0))))
;;;_    > outline-sibling-index (&optional depth)
(defun outline-sibling-index (&optional depth)
  "Item number of this prospective topic among its siblings.

If optional arg depth is greater than current depth, then we're
opening a new level, and return 0.

If less than this depth, ascend to that depth and count..."

  (save-excursion
    (cond ((and depth (<= depth 0) 0))
          ((or (not depth) (= depth (outline-depth)))
           (let ((index 1))
             (while (outline-previous-sibling (outline-recent-depth) nil)
	       (setq index (1+ index)))
             index))
          ((< depth (outline-recent-depth))
           (outline-ascend-to-depth depth)
           (outline-sibling-index))
          (0))))
;;;_    > outline-topic-flat-index ()
(defun outline-topic-flat-index ()
  "Return a list indicating point's numeric section.subsect.subsubsect...
Outermost is first."
  (let* ((depth (outline-depth))
	 (next-index (outline-sibling-index depth))
	 (rev-sibls nil))
    (while (> next-index 0)
      (setq rev-sibls (cons next-index rev-sibls))
      (setq depth (1- depth))
      (setq next-index (outline-sibling-index depth)))
    rev-sibls)
  )

;;;_  - Navigation macros
;;;_   > outline-next-heading ()
(defsubst outline-next-heading ()
  "Move to the heading for the topic \(possibly invisible) before this one.

Returns the location of the heading, or nil if none found."

  (if (and (bobp) (not (eobp)))
       (forward-char 1))

  (if (re-search-forward outline-line-boundary-regexp nil 0)
      (outline-prefix-data		; Got valid location state - set vars:
       (goto-char (or (match-beginning 2)
		      outline-recent-prefix-beginning))
       (or (match-end 2) outline-recent-prefix-end))))
;;;_   : outline-this-or-next-heading
(defun outline-this-or-next-heading ()
  "Position cursor on current or next heading."
  ;; A throwaway non-macro that is defined after outline-next-heading
  ;; and usable by outline-mode.
  (if (not (outline-goto-prefix)) (outline-next-heading)))
;;;_   > outline-previous-heading ()
(defmacro outline-previous-heading ()
  "Move to the prior \(possibly invisible) heading line.

Return the location of the beginning of the heading, or nil if not found."

  '(if (bobp)
       nil
     (outline-goto-prefix)
     (if
	 ;; searches are unbounded and return nil if failed:
	 (or (re-search-backward outline-line-boundary-regexp nil 0)
	     (looking-at outline-bob-regexp))
	 (progn				; Got valid location state - set vars:
	   (outline-prefix-data
	    (goto-char (or (match-beginning 2)
			   outline-recent-prefix-beginning))
	    (or (match-end 2) outline-recent-prefix-end))))))

;;;_  - Subtree Charting
;;;_   " These routines either produce or assess charts, which are
;;; nested lists of the locations of topics within a subtree.
;;;
;;; Use of charts enables efficient navigation of subtrees, by
;;; requiring only a single regexp-search based traversal, to scope
;;; out the subtopic locations.  The chart then serves as the basis
;;; for assessment or adjustment of the subtree, without redundant
;;; traversal of the structure.

;;;_   > outline-chart-subtree (&optional levels orig-depth prev-depth)
(defun outline-chart-subtree (&optional levels orig-depth prev-depth)
  "Produce a location \"chart\" of subtopics of the containing topic.

Optional argument LEVELS specifies the depth \(relative to start
depth) for the chart.  Subsequent optional args are not for public
use.

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
	(progn (setq orig-depth (outline-depth))
	       (or prev-depth (setq prev-depth (1+ orig-depth)))
	       (outline-next-heading)))

    ;; Loop over the current levels' siblings.  Besides being more
    ;; efficient than tail-recursing over a level, it avoids exceeding
    ;; the typically quite constrained emacs max-lisp-eval-depth.
    ;;
    ;; Probably would speed things up to implement loop-based stack
    ;; operation rather than recursing for lower levels.  Bah.

    (while (and (not (eobp))
					; Still within original topic?
		(< orig-depth (setq curr-depth (outline-recent-depth)))
		(cond ((= prev-depth curr-depth)
		       ;; Register this one and move on:
		       (setq chart (cons (point) chart))
		       (if (and levels (<= levels 1))
			   ;; At depth limit - skip sublevels:
			   (or (outline-next-sibling curr-depth)
			       ;; or no more siblings - proceed to
			       ;; next heading at lesser depth:
			       (while (and (<= curr-depth
					       (outline-recent-depth))
					   (outline-next-heading))))
			 (outline-next-heading)))

		      ((and (< prev-depth curr-depth)
			    (or (not levels)
				(> levels 0)))
		       ;; Recurse on deeper level of curr topic:
		       (setq chart
			     (cons (outline-chart-subtree (and levels
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
	       (setq outline-recent-end-of-subtree (point))))

    chart				; (nreverse chart) not necessary,
					; and maybe not preferable.
    ))
;;;_   > outline-chart-siblings (&optional start end)
(defun outline-chart-siblings (&optional start end)
  "Produce a list of locations of this and succeeding sibling topics.
Effectively a top-level chart of siblings.  See `outline-chart-subtree'
for an explanation of charts."
  (save-excursion
    (if (outline-goto-prefix)
	(let ((chart (list (point))))
	  (while (outline-next-sibling)
	    (setq chart (cons (point) chart)))
	  (if chart (setq chart (nreverse chart)))))))
;;;_   > outline-chart-to-reveal (chart depth)
(defun outline-chart-to-reveal (chart depth)

  "Return a flat list of hidden points in subtree CHART, up to DEPTH.

Note that point can be left at any of the points on chart, or at the
start point."

  (let (result here)
    (while (and (or (eq depth t) (> depth 0))
		chart)
      (setq here (car chart))
      (if (listp here)
	  (let ((further (outline-chart-to-reveal here (or (eq depth t)
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
;;;_   X outline-chart-spec (chart spec &optional exposing)
(defun outline-chart-spec (chart spec &optional exposing)
  "Not yet \(if ever) implemented.

Produce exposure directives given topic/subtree CHART and an exposure SPEC.

Exposure spec indicates the locations to be exposed and the prescribed
exposure status.  Optional arg EXPOSING is an integer, with 0
indicating pending concealment, anything higher indicating depth to
which subtopic headers should be exposed, and negative numbers
indicating (negative of) the depth to which subtopic headers and
bodies should be exposed.

The produced list can have two types of entries.  Bare numbers
indicate points in the buffer where topic headers that should be
exposed reside.

 - bare negative numbers indicates that the topic starting at the
   point which is the negative of the number should be opened,
   including their entries.
 - bare positive values indicate that this topic header should be
   opened.
 - Lists signify the beginning and end points of regions that should
   be flagged, and the flag to employ.  (For concealment: `\(\?r\)', and
   exposure:"
  (while spec
    (cond ((listp spec)
	   )
	  )
    (setq spec (cdr spec)))
  )

;;;_  - Within Topic
;;;_   > outline-goto-prefix ()
(defun outline-goto-prefix ()
  "Put point at beginning of immediately containing outline topic.

Goes to most immediate subsequent topic if none immediately containing.

Not sensitive to topic visibility.

Returns a the point at the beginning of the prefix, or nil if none."

  (let (done)
    (while (and (not done)
		(re-search-backward "[\n\r]" nil 1))
      (forward-char 1)
      (if (looking-at outline-regexp)
	  (setq done (outline-prefix-data (match-beginning 0)
					  (match-end 0)))
	(forward-char -1)))
    (if (bobp)
	(cond ((looking-at outline-regexp)
	       (outline-prefix-data (match-beginning 0)(match-end 0)))
	      ((outline-next-heading))
	      (done))
      done)))
;;;_   > outline-end-of-prefix ()
(defun outline-end-of-prefix (&optional ignore-decorations)
  "Position cursor at beginning of header text.

If optional IGNORE-DECORATIONS is non-nil, put just after bullet,
otherwise skip white space between bullet and ensuing text."

  (if (not (outline-goto-prefix))
      nil
    (let ((match-data (match-data)))
      (goto-char (match-end 0))
      (if ignore-decorations
	  t
	(while (looking-at "[0-9]") (forward-char 1))
	(if (and (not (eolp)) (looking-at "\\s-")) (forward-char 1)))
      (store-match-data match-data))
    ;; Reestablish where we are:
    (outline-current-depth)))
;;;_   > outline-current-bullet-pos ()
(defun outline-current-bullet-pos ()
  "Return position of current \(visible) topic's bullet."

 (if (not (outline-current-depth))
      nil
   (1- (match-end 0))))
;;;_   > outline-back-to-current-heading ()
(defun outline-back-to-current-heading ()
  "Move to heading line of current topic, or beginning if already on the line."

  (beginning-of-line)
  (prog1 (or (outline-on-current-heading-p)
             (and (re-search-backward (concat "^\\(" outline-regexp "\\)")
                                      nil
                                      'move)
                  (outline-prefix-data (match-beginning 1)(match-end 1))))
    (if (interactive-p) (outline-end-of-prefix))))
;;;_   > outline-back-to-heading ()
(defalias 'outline-back-to-heading 'outline-back-to-current-heading)
;;;_   > outline-pre-next-preface ()
(defun outline-pre-next-preface ()
  "Skip forward to just before the next heading line.

Returns that character position."

  (if (re-search-forward outline-line-boundary-regexp nil 'move)
      (prog1 (goto-char (match-beginning 0))
             (outline-prefix-data (match-beginning 2)(match-end 2)))))
;;;_   > outline-end-of-current-subtree ()
(defun outline-end-of-current-subtree ()
  "Put point at the end of the last leaf in the currently visible topic."
  (interactive)
  (outline-back-to-current-heading)
  (let ((level (outline-recent-depth)))
    (outline-next-heading)
    (while (and (not (eobp))
                (> (outline-recent-depth) level))
      (outline-next-heading))
    (and (not (eobp)) (forward-char -1))
    (and (memq (preceding-char) '(?\n ?\r))
         (memq (aref (buffer-substring (max 1 (- (point) 3)) (point)) 1)
               '(?\n ?\r))
         (forward-char -1))
    (setq outline-recent-end-of-subtree (point))))
;;;_   > outline-beginning-of-current-entry ()
(defun outline-beginning-of-current-entry ()
  "When not already there, position point at beginning of current topic's body.

If already there, move cursor to bullet for hot-spot operation.
\(See outline-mode doc string for details on hot-spot operation.)"
  (interactive)
  (let ((start-point (point)))
    (outline-end-of-prefix)
    (if (and (interactive-p)
	     (= (point) start-point))
	(goto-char (outline-current-bullet-pos)))))
;;;_   > outline-end-of-current-entry ()
(defun outline-end-of-current-entry ()
  "Position the point at the end of the current topics' entry."
  (interactive)
  (outline-show-entry)
  (prog1 (outline-pre-next-preface)
    (if (and (not (bobp))(looking-at "^$"))
        (forward-char -1))))
;;;_   > outline-end-of-current-heading ()
(defun outline-end-of-current-heading ()
  (interactive)
  (outline-beginning-of-current-entry)
  (forward-line -1)
  (end-of-line))
(defalias 'outline-end-of-heading 'outline-end-of-current-heading)

;;;_  - Depth-wise
;;;_   > outline-ascend-to-depth (depth)
(defun outline-ascend-to-depth (depth)
  "Ascend to depth DEPTH, returning depth if successful, nil if not."
  (if (and (> depth 0)(<= depth (outline-depth)))
      (let ((last-good (point)))
        (while (and (< depth (outline-depth))
                    (setq last-good (point))
                    (outline-beginning-of-level)
                    (outline-previous-heading)))
        (if (= (outline-recent-depth) depth)
            (progn (goto-char outline-recent-prefix-beginning)
                   depth)
          (goto-char last-good)
          nil))
    (if (interactive-p) (outline-end-of-prefix))))
;;;_   > outline-ascend ()
(defun outline-ascend ()
  "Ascend one level, returning t if successful, nil if not."
  (prog1
      (if (outline-beginning-of-level)
	  (outline-previous-heading))
    (if (interactive-p) (outline-end-of-prefix))))
;;;_   > outline-descend-to-depth (depth)
(defun outline-descend-to-depth (depth)
  "Descend to depth DEPTH within current topic.

Returning depth if successful, nil if not."
  (let ((start-point (point))
        (start-depth (outline-depth)))
    (while
        (and (> (outline-depth) 0)
             (not (= depth (outline-recent-depth))) ; ... not there yet
             (outline-next-heading)     ; ... go further
             (< start-depth (outline-recent-depth)))) ; ... still in topic
    (if (and (> (outline-depth) 0)
             (= (outline-recent-depth) depth))
        depth
      (goto-char start-point)
      nil))
  )
;;;_   > outline-up-current-level (arg &optional dont-complain)
(defun outline-up-current-level (arg &optional dont-complain)
  "Move out ARG levels from current visible topic.

Positions on heading line of containing topic.  Error if unable to
ascend that far, or nil if unable to ascend but optional arg
DONT-COMPLAIN is non-nil."
  (interactive "p")
  (outline-back-to-current-heading)
  (let ((present-level (outline-recent-depth))
	(last-good (point))
	failed
	return)
    ;; Loop for iterating arg:
    (while (and (> (outline-recent-depth) 1)
                (> arg 0)
                (not (bobp))
		(not failed))
      (setq last-good (point))
      ;; Loop for going back over current or greater depth:
      (while (and (not (< (outline-recent-depth) present-level))
		  (or (outline-previous-visible-heading 1)
		      (not (setq failed present-level)))))
      (setq present-level (outline-current-depth))
      (setq arg (- arg 1)))
    (if (or failed
	    (> arg 0))
	(progn (goto-char last-good)
	       (if (interactive-p) (outline-end-of-prefix))
	       (if (not dont-complain)
		   (error "Can't ascend past outermost level")
		 (if (interactive-p) (outline-end-of-prefix))
		 nil))
      (if (interactive-p) (outline-end-of-prefix))
      outline-recent-prefix-beginning)))

;;;_  - Linear
;;;_   > outline-next-sibling (&optional depth backward)
(defun outline-next-sibling (&optional depth backward)
  "Like outline-forward-current-level, but respects invisible topics.

Traverse at optional DEPTH, or current depth if none specified.

Go backward if optional arg BACKWARD is non-nil.

Return depth if successful, nil otherwise."

  (if (and backward (bobp))
      nil
    (let ((start-depth (or depth (outline-depth)))
          (start-point (point))
	  last-depth)
      (while (and (not (if backward (bobp) (eobp)))
                  (if backward (outline-previous-heading)
                    (outline-next-heading))
                  (> (setq last-depth (outline-recent-depth)) start-depth)))
      (if (and (not (eobp))
               (and (> (or last-depth (outline-depth)) 0)
                    (= (outline-recent-depth) start-depth)))
          outline-recent-prefix-beginning
        (goto-char start-point)
	(if depth (outline-depth) start-depth)
        nil))))
;;;_   > outline-previous-sibling (&optional depth backward)
(defun outline-previous-sibling (&optional depth backward)
  "Like outline-forward-current-level,but backwards & respect invisible topics.

Optional DEPTH specifies depth to traverse, default current depth.

Optional BACKWARD reverses direction.

Return depth if successful, nil otherwise."
  (outline-next-sibling depth (not backward))
  )
;;;_   > outline-snug-back ()
(defun outline-snug-back ()
  "Position cursor at end of previous topic

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
;;;_   > outline-beginning-of-level ()
(defun outline-beginning-of-level ()
  "Go back to the first sibling at this level, visible or not."
  (outline-end-of-level 'backward))
;;;_   > outline-end-of-level (&optional backward)
(defun outline-end-of-level (&optional backward)
  "Go to the last sibling at this level, visible or not."

  (let ((depth (outline-depth)))
    (while (outline-previous-sibling depth nil))
    (prog1 (outline-recent-depth)
      (if (interactive-p) (outline-end-of-prefix)))))
;;;_   > outline-next-visible-heading (arg)
(defun outline-next-visible-heading (arg)
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
		  (not (setq got (looking-at outline-regexp)))))
      ;; Register this got, it may be the last:
      (if got (setq prev got))
      (setq arg (1- arg)))
    (cond (got				; Last move was to a prefix:
	   (outline-prefix-data (match-beginning 0) (match-end 0))
	   (outline-end-of-prefix))
	  (prev				; Last move wasn't, but prev was:
	   (outline-prefix-data (match-beginning 0) (match-end 0)))
	  ((not backward) (end-of-line) nil))))
;;;_   > outline-previous-visible-heading (arg)
(defun outline-previous-visible-heading (arg)
  "Move to the previous heading line.

With argument, repeats or can move forward if negative.
A heading line is one that starts with a `*' (or that outline-regexp
matches)."
  (interactive "p")
  (outline-next-visible-heading (- arg)))
;;;_   > outline-forward-current-level (arg)
(defun outline-forward-current-level (arg)
  "Position point at the next heading of the same level.

Takes optional repeat-count, goes backward if count is negative.

Returns resulting position, else nil if none found."
  (interactive "p")
  (let ((start-depth (outline-current-depth))
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
		  (if backward (outline-previous-visible-heading 1)
		    (outline-next-visible-heading 1))
		  (> (setq last-depth (outline-recent-depth)) start-depth)))
      (if (and last-depth (= last-depth start-depth)
	       (not (if backward (bobp) (eobp))))
	  (setq last-good (point)
		arg (1- arg))
	(setq at-boundary t)))
    (if (and (not (eobp))
	     (= arg 0)
	     (and (> (or last-depth (outline-depth)) 0)
		  (= (outline-recent-depth) start-depth)))
	outline-recent-prefix-beginning
      (goto-char last-good)
      (if (not (interactive-p))
	  nil
	(outline-end-of-prefix)
	(error "Hit %s level %d topic, traversed %d of %d requested"
	       (if backward "first" "last")
	       (outline-recent-depth)
	       (- (abs start-arg) arg)
	       (abs start-arg))))))
;;;_   > outline-backward-current-level (arg)
(defun outline-backward-current-level (arg)
  "Inverse of `outline-forward-current-level'."
  (interactive "p")
  (if (interactive-p)
      (let ((current-prefix-arg (* -1 arg)))
	(call-interactively 'outline-forward-current-level))
    (outline-forward-current-level (* -1 arg))))

;;;_ #5 Alteration

;;;_  - Fundamental
;;;_   > outline-before-change-protect (beg end)
(defun outline-before-change-protect (beg end)
  "Outline before-change hook, regulates changes to concealed text.

Reveal concealed text that would be changed by current command, and
offer user choice to commit or forego the change.  Unchanged text is
reconcealed.  User has option to have changed text reconcealed.

Undo commands are specially treated - the user is not prompted for
choice, the undoes are always committed (based on presumption that the
things being undone were already subject to this regulation routine),
and undoes always leave the changed stuff exposed.

Changes to concealed regions are ignored while file is being written.
\(This is for the sake of functions that do change the file during
writes, like crypt and zip modes.)

Locally bound in outline buffers to `before-change-functions', which
in emacs 19 is run before any change to the buffer.

Any functions which set [`this-command' to `undo', or which set]
`outline-override-protect' non-nil (as does, eg, outline-flag-chars)
are exempt from this restriction."
  (if (and (outline-mode-p)
					; outline-override-protect
					; set by functions that know what
					; they're doing, eg outline internals:
	   (not outline-override-protect)
	   (not outline-during-write-cue)
	   (save-match-data		; Preserve operation position state.
					; Both beginning and end chars must
					; be exposed:
	     (save-excursion (if (memq this-command '(newline open-line))
				 ;; Compensate for stupid emacs {new,
				 ;; open-}line display optimization:
				 (setq beg (1+ beg)
				       end (1+ end)))
			     (goto-char beg)
			     (or (outline-hidden-p)
				 (and (not (= beg end))
				      (goto-char end)
				      (outline-hidden-p))))))
      (save-match-data
	(if (equal this-command 'undo)
		 ;; Allow undo without inhibition.
		 ;; - Undoing new and open-line hits stupid emacs redisplay
		 ;;   optimization (em 19 cmds.c, ~ line 200).
		 ;; - Presumably, undoing what was properly protected when
		 ;;   done.
		 ;; - Undo may be users' only recourse in protection faults.
		 ;; So, expose what getting changed:
	    (progn (message "Undo! - exposing concealed target...")
		   (if (outline-hidden-p)
		       (outline-show-children))
		   (message "Undo!"))
	  (let (response
		(rehide-completely (save-excursion (outline-goto-prefix)
						   (outline-hidden-p)))
		rehide-place)

	    (save-excursion
	      (if (condition-case err
		      ;; Condition case to catch keyboard quits during reads.
		      (progn
					; Give them a peek where
			(save-excursion
			  (if (eolp) (setq rehide-place
					   (outline-goto-prefix)))
			  (outline-show-entry))
					; Present the message, but...
					; leave the cursor at the location
					; until they respond:
					; Then interpret the response:
			(while
			    (progn
			      (message (concat "Change inside concealed"
					       " region - do it? "
					       "(n or 'y'/'r'eclose)"))
			      (setq response (read-char))
			      (not
			       (cond ((memq response '(?r ?R))
				      (setq response 'reclose))
				     ((memq response '(?y ?Y ? ))
				      (setq response t))
				     ((memq response '(?n ?N 127))
				      (setq response nil)
				      t)
				     ((eq response ??)
				      (message
				       "`r' means `yes, then reclose'")
				      nil)
				     (t (message "Please answer y, n, or r")
					(sit-for 1)
					nil)))))
			response)
		    ('quit nil))
					; Continue:
		  (if (eq response 'reclose)
		      (save-excursion
			(if rehide-place (goto-char rehide-place))
			(if rehide-completely
			    (outline-hide-current-entry-completely)
			  (outline-hide-current-entry)))
		    (if (outline-ascend-to-depth (1- (outline-recent-depth)))
			(outline-show-children)
		      (outline-show-to-offshoot)))
					; Prevent:
		(if rehide-completely
		    (save-excursion
		      (if rehide-place (goto-char rehide-place))
		      (outline-hide-current-entry-completely))
		  (outline-hide-current-entry))
		(error (concat
			"Change within concealed region prevented.")))))))
    )	; if
  )	; defun
;;;_   = outline-post-goto-bullet
(defvar outline-post-goto-bullet nil
  "Outline internal var, for `outline-pre-command-business' hot-spot operation.

When set, tells post-processing to reposition on topic bullet, and
then unset it.  Set by outline-pre-command-business when implementing
hot-spot operation, where literal characters typed over a topic bullet
are mapped to the command of the corresponding control-key on the
outline-mode-map.")
(make-variable-buffer-local 'outline-post-goto-bullet)
;;;_   > outline-post-command-business ()
(defun outline-post-command-business ()
  "Outline post-command-hook function.

- Null outline-override-protect, so it's not left open.

- Implement (and clear) outline-post-goto-bullet, for hot-spot
  outline commands.

- Massages buffer-undo-list so successive, standard character self-inserts are
  aggregated.  This kludge compensates for lack of undo bunching when
  before-change-functions is used."

					; Apply any external change func:
  (if (not (outline-mode-p))		; In outline-mode.
      nil
    (setq outline-override-protect nil)
    (if outline-isearch-dynamic-expose
	(outline-isearch-rectification))
    (if outline-during-write-cue
	;; Was used by outline-before-change-protect, done with it now:
	(setq outline-during-write-cue nil))
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
		   (> outline-undo-aggregation (- prev-to prev-from)))
	      (setq buffer-undo-list
		    (cons (cons prev-from cur-to)
			  (cdr (cdr (cdr buffer-undo-list))))))))
    ;; Implement -post-goto-bullet, if set: (must be after undo business)
    (if (and outline-post-goto-bullet
	     (outline-current-bullet-pos))
	(progn (goto-char (outline-current-bullet-pos))
	       (setq outline-post-goto-bullet nil)))
    ))
;;;_   > outline-pre-command-business ()
(defun outline-pre-command-business ()
  "Outline pre-command-hook function for outline buffers.
Implements special behavior when cursor is on bullet char.

Self-insert characters are reinterpreted control-character references
into the outline-mode-map.  The outline-mode post-command hook will
position a cursor that has moved as a result of such reinterpretation,
on the destination topic's bullet, when the cursor wound up in the

The upshot is that you can get easy, single (ie, unmodified) key
outline maneuvering operations by positioning the cursor on the bullet
char.  You stay in this mode until you use some regular
cursor-positioning command to relocate the cursor off of a bullet
char."

  (if (not (outline-mode-p))
      ;; Shouldn't be invoked if not in allout outline-mode, but just in case:
      nil
    ;; Register isearch status:
    (if (and (boundp  'isearch-mode) isearch-mode)
	(setq outline-pre-was-isearching t)
      (setq outline-pre-was-isearching nil))
    ;; Hot-spot navigation provisions:
    (if (and (eq this-command 'self-insert-command)
	     (eq (point)(outline-current-bullet-pos)))
	(let* ((this-key-num (cond
			      ((numberp last-command-char)
			       last-command-char)
			      ;; XXX Only xemacs has characterp.
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
		      (lookup-key 'outline-mode-map
				  (concat outline-command-prefix
					  (char-to-string (- this-key-num
							     64))))))
	    (if mapped-binding
		(setq outline-post-goto-bullet t
		      this-command mapped-binding)))))))
;;;_   > outline-find-file-hook ()
(defun outline-find-file-hook ()
  "Activate outline-mode when `outline-auto-activation' & `outline-layout' are non-nil.

See `outline-init' for setup instructions."
  (if (and outline-auto-activation
	   (not (outline-mode-p))
	   outline-layout)
      (outline-mode t)))
;;;_   > outline-isearch-rectification
(defun outline-isearch-rectification ()
  "Rectify outline exposure before, during, or after isearch.

Called as part of outline-post-command-business."

  (let ((isearching (and (boundp 'isearch-mode) isearch-mode)))
    (cond ((and isearching (not outline-pre-was-isearching))
	   (outline-isearch-expose 'start))
	  ((and isearching outline-pre-was-isearching)
	   (outline-isearch-expose 'continue))
	  ((and (not isearching) outline-pre-was-isearching)
	   (outline-isearch-expose 'final))
	  ;; Not and wasn't isearching:
	  (t (setq outline-isearch-prior-pos nil)
	     (setq outline-isearch-did-quit nil)))))
;;;_   = outline-isearch-was-font-lock
(defvar outline-isearch-was-font-lock
  (and (boundp 'font-lock-mode) font-lock-mode))

;;;_   > outline-flag-region (from to flag)
(defmacro outline-flag-region (from to flag)
  "Hide or show lines from FROM to TO, via emacs selective-display FLAG char.
Ie, text following flag C-m \(carriage-return) is hidden until the
next C-j (newline) char.

Returns the endpoint of the region."
  `(let ((buffer-read-only nil)
	   (outline-override-protect t))
       (subst-char-in-region ,from ,to
			     (if (= ,flag ?\n) ?\r ?\n)
			     ,flag t)))

;;;_   > outline-isearch-expose (mode)
(defun outline-isearch-expose (mode)
  "Mode is either 'clear, 'start, 'continue, or 'final."
  ;; outline-isearch-prior-pos encodes exposure status of prior pos:
  ;; (pos was-vis header-pos end-pos)
  ;; pos	- point of concern
  ;; was-vis	- t, else 'topic if entire topic was exposed, 'entry otherwise
  ;; Do reclosure or prior pos, as necessary:
  (if (eq mode 'start)
      (setq outline-isearch-was-font-lock (and (boundp 'font-lock-mode)
                                               font-lock-mode)
	    font-lock-mode nil)
    (if (eq mode 'final)
	(setq font-lock-mode outline-isearch-was-font-lock))
    (if (and outline-isearch-prior-pos
	     (listp outline-isearch-prior-pos))
	;; Conceal prior peek:
	(outline-flag-region (car (cdr outline-isearch-prior-pos))
			     (car (cdr (cdr outline-isearch-prior-pos)))
			     ?\r)))
  (if (outline-visible-p)
      (setq outline-isearch-prior-pos nil)
    (if (not (eq mode 'final))
	(setq outline-isearch-prior-pos (cons (point) (outline-show-entry)))
      (if outline-isearch-did-quit
	  nil
	(setq outline-isearch-prior-pos nil)
	(outline-show-children))))
  (setq outline-isearch-did-quit nil))
;;;_   > outline-enwrap-isearch ()
(defun outline-enwrap-isearch ()
  "Impose outline-mode isearch-abort wrapper for dynamic exposure in isearch.

The function checks to ensure that the rebinding is done only once."

  (add-hook 'isearch-mode-end-hook 'outline-isearch-rectification)
  (if (fboundp 'outline-real-isearch-abort)
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
			   (setq outline-isearch-dynamic-expose nil))))
        ;; Isearch-mode loaded, encapsulate specific entry points for
        ;; outline dynamic-exposure business:
        (progn 
	  ;; stash crucial isearch-mode funcs under known, private
	  ;; names, then register wrapper functions under the old
	  ;; names, in their stead:
          (fset 'outline-real-isearch-abort (symbol-function 'isearch-abort))
          (fset 'isearch-abort 'outline-isearch-abort)))))
;;;_   > outline-isearch-abort ()
(defun outline-isearch-abort ()
  "Wrapper for outline-real-isearch-abort \(which see), to register
actual quits."
  (interactive)
  (setq outline-isearch-did-quit nil)
  (condition-case what
      (outline-real-isearch-abort)
    ('quit (setq outline-isearch-did-quit t)
	  (signal 'quit nil))))

;;; Prevent unnecessary font-lock while isearching!
(defvar isearch-was-font-locking nil)
(defun isearch-inhibit-font-lock ()
  "Inhibit font-lock while isearching - for use on isearch-mode-hook."
  (if (and (outline-mode-p) (boundp 'font-lock-mode) font-lock-mode)
      (setq isearch-was-font-locking t
	    font-lock-mode nil)))
(add-hook 'isearch-mode-hook 'isearch-inhibit-font-lock)
(defun isearch-reenable-font-lock ()
  "Reenable font-lock after isearching - for use on isearch-mode-end-hook."
  (if (and (boundp 'font-lock-mode) font-lock-mode)
      (if (and (outline-mode-p) isearch-was-font-locking)
	  (setq isearch-was-font-locking nil
		font-lock-mode t))))
(add-hook 'isearch-mode-end-hook 'isearch-reenable-font-lock)

;;;_  - Topic Format Assessment
;;;_   > outline-solicit-alternate-bullet (depth &optional current-bullet)
(defun outline-solicit-alternate-bullet (depth &optional current-bullet)

  "Prompt for and return a bullet char as an alternative to the current one.

Offer one suitable for current depth DEPTH as default."

  (let* ((default-bullet (or (and (stringp current-bullet) current-bullet)
                             (outline-bullet-for-depth depth)))
	 (sans-escapes (regexp-sans-escapes outline-bullets-string))
	 choice)
    (save-excursion
      (goto-char (outline-current-bullet-pos))
      (setq choice (solicit-char-in-string
                    (format "Select bullet: %s ('%s' default): "
                            sans-escapes
                            default-bullet)
                    sans-escapes
                    t)))
    (message "")
    (if (string= choice "") default-bullet choice))
  )
;;;_   > outline-distinctive-bullet (bullet)
(defun outline-distinctive-bullet (bullet)
  "True if bullet is one of those on outline-distinctive-bullets-string."
  (string-match (regexp-quote bullet) outline-distinctive-bullets-string))
;;;_   > outline-numbered-type-prefix (&optional prefix)
(defun outline-numbered-type-prefix (&optional prefix)
  "True if current header prefix bullet is numbered bullet."
  (and outline-numbered-bullet
        (string= outline-numbered-bullet
                 (if prefix
                     (outline-get-prefix-bullet prefix)
                   (outline-get-bullet)))))
;;;_   > outline-bullet-for-depth (&optional depth)
(defun outline-bullet-for-depth (&optional depth)
  "Return outline topic bullet suited to optional DEPTH, or current depth."
  ;; Find bullet in plain-bullets-string modulo DEPTH.
  (if outline-stylish-prefixes
      (char-to-string (aref outline-plain-bullets-string
                            (% (max 0 (- depth 2))
                               outline-plain-bullets-string-len)))
    outline-primary-bullet)
  )

;;;_  - Topic Production
;;;_   > outline-make-topic-prefix (&optional prior-bullet
(defun outline-make-topic-prefix (&optional prior-bullet
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
notwithstanding) if it is on the outline-distinctive-bullets-string,
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

Fifth arg, NUMBER-CONTROL, matters only if `outline-numbered-bullet'
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
         (depth (or depth (outline-depth)))
         (header-lead outline-header-prefix)
         (bullet-char

          ;; Getting value for bullet char is practically the whole job:

          (cond
                                        ; Simplest situation - level 1:
           ((<= depth 1) (setq header-lead "") outline-primary-bullet)
                                        ; Simple, too: all asterisks:
           (outline-old-style-prefixes
            ;; Cheat - make body the whole thing, null out header-lead and
            ;; bullet-char:
            (setq body (make-string depth
                                    (string-to-char outline-primary-bullet)))
            (setq header-lead "")
            "")

           ;; (Neither level 1 nor old-style, so we're space padding.
           ;; Sneak it in the condition of the next case, whatever it is.)

           ;; Solicitation overrides numbering and other cases:
           ((progn (setq body (make-string (- depth 2) ?\ ))
                   ;; The actual condition:
                   solicit)
            (let* ((got (outline-solicit-alternate-bullet depth solicit)))
              ;; Gotta check whether we're numbering and got a numbered bullet:
              (setq numbering (and outline-numbered-bullet
                                   (not (and number-control (not index)))
                                   (string= got outline-numbered-bullet)))
              ;; Now return what we got, regardless:
              got))

           ;; Numbering invoked through args:
           ((and outline-numbered-bullet number-control)
            (if (setq numbering (not (setq denumbering (not index))))
                outline-numbered-bullet
              (if (and prior-bullet
                       (not (string= outline-numbered-bullet
                                     prior-bullet)))
                  prior-bullet
                (outline-bullet-for-depth depth))))

          ;;; Neither soliciting nor controlled numbering ;;;
             ;;; (may be controlled denumbering, tho) ;;;

           ;; Check wrt previous sibling:
           ((and new				  ; only check for new prefixes
                 (<= depth (outline-depth))
                 outline-numbered-bullet	      ; ... & numbering enabled
                 (not denumbering)
                 (let ((sibling-bullet
                        (save-excursion
                          ;; Locate correct sibling:
                          (or (>= depth (outline-depth))
                              (outline-ascend-to-depth depth))
                          (outline-get-bullet))))
                   (if (and sibling-bullet
                            (string= outline-numbered-bullet sibling-bullet))
                       (setq numbering sibling-bullet)))))

           ;; Distinctive prior bullet?
           ((and prior-bullet
                 (outline-distinctive-bullet prior-bullet)
                 ;; Either non-numbered:
                 (or (not (and outline-numbered-bullet
                               (string= prior-bullet outline-numbered-bullet)))
                     ;; or numbered, and not denumbering:
                     (setq numbering (not denumbering)))
                 ;; Here 'tis:
                 prior-bullet))

           ;; Else, standard bullet per depth:
           ((outline-bullet-for-depth depth)))))

    (concat header-lead
            body
            bullet-char
            (if numbering
                (format "%d" (cond ((and index (numberp index)) index)
                                   (new (1+ (outline-sibling-index depth)))
                                   ((outline-sibling-index))))))
    )
  )
;;;_   > outline-open-topic (relative-depth &optional before use_sib_bullet)
(defun outline-open-topic (relative-depth &optional before use_sib_bullet)
  "Open a new topic at depth DEPTH.

New topic is situated after current one, unless optional flag BEFORE
is non-nil, or unless current line is complete empty (not even
whitespace), in which case open is done on current line.

If USE_SIB_BULLET is true, use the bullet of the prior sibling.

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

  (let* ((depth (+ (outline-current-depth) relative-depth))
         (opening-on-blank (if (looking-at "^\$")
                               (not (setq before nil))))
         opening-numbered	; Will get while computing ref-topic, below
         ref-depth		; Will get while computing ref-topic, below
         ref-bullet		; Will get while computing ref-topic, next
         (ref-topic (save-excursion
                      (cond ((< relative-depth 0)
                             (outline-ascend-to-depth depth))
                            ((>= relative-depth 1) nil)
                            (t (outline-back-to-current-heading)))
                      (setq ref-depth (outline-recent-depth))
                      (setq ref-bullet
                            (if (> outline-recent-prefix-end 1)
                                (outline-recent-bullet)
                              ""))
                      (setq opening-numbered
                            (save-excursion
                              (and outline-numbered-bullet
                                   (or (<= relative-depth 0)
                                       (outline-descend-to-depth depth))
                                   (if (outline-numbered-type-prefix)
                                       outline-numbered-bullet))))
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
                           (outline-end-of-current-subtree)
                           (bolp)))
                    (and (= ref-depth 1)
                         (or before
                             (= depth 1)
                             (save-excursion
                               ;; Don't already have following
                               ;; vertical padding:
                               (not (outline-pre-next-preface)))))))

                                        ; Position to prior heading,
                                        ; if inserting backwards, and
					; not going outwards:
          (if (and before (>= relative-depth 0))
	      (progn (outline-back-to-current-heading)
                            (setq doing-beginning (bobp))
                            (if (not (bobp))
                                (outline-previous-heading)))
	    (if (and before (bobp))
		(outline-unprotected (open-line 1))))

          (if (<= relative-depth 0)
              ;; Not going inwards, don't snug up:
              (if doing-beginning
		  (outline-unprotected (open-line (if dbl-space 2 1)))
		(if before
		    (progn (end-of-line)
			   (outline-pre-next-preface)
			   (while (= ?\r (following-char))
                             (forward-char 1))
			   (if (not (looking-at "^$"))
			       (outline-unprotected (open-line 1))))
		  (outline-end-of-current-subtree)))
            ;; Going inwards - double-space if first offspring is,
            ;; otherwise snug up.
            (end-of-line)		; So we skip any concealed progeny.
            (outline-pre-next-preface)
            (if (bolp)
                ;; Blank lines between current header body and next
                ;; header - get to last substantive (non-white-space)
                ;; line in body:
                (re-search-backward "[^ \t\n]" nil t))
            (if (save-excursion
                  (outline-next-heading)
                  (if (> (outline-recent-depth) ref-depth)
                      ;; This is an offspring.
                      (progn (forward-line -1)
                             (looking-at "^\\s-*$"))))
                (progn (forward-line 1)
                       (outline-unprotected (open-line 1))))
            (end-of-line))
          ;;(if doing-beginning (goto-char doing-beginning))
          (if (not (bobp))
              (progn (if (and (not (> depth ref-depth))
                              (not before))
                         (outline-unprotected (open-line 1))
		       (if (> depth ref-depth)
			   (outline-unprotected (newline 1))
			 (if dbl-space
			     (outline-unprotected (open-line 1))
			   (if (not before)
			       (outline-unprotected (newline 1))))))
                     (if dbl-space
			 (outline-unprotected (newline  1)))
                     (if (and (not (eobp))
                              (not (bolp)))
                         (forward-char 1))))
          ))
    (insert (concat (outline-make-topic-prefix opening-numbered
					       t
					       depth)
		    " "))

    ;;(if doing-beginning (save-excursion (newline (if dbl-space 2 1))))


    (outline-rebullet-heading (and use_sib_bullet ref-bullet);;; solicit
                              depth			     ;;; depth
                              nil 			     ;;; number-control
                              nil			     ;;; index
                              t)     (end-of-line)
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
;;;_   > outline-open-subtopic (arg)
(defun outline-open-subtopic (arg)
  "Open new topic header at deeper level than the current one.

Negative universal arg means to open deeper, but place the new topic
prior to the current one."
  (interactive "p")
  (outline-open-topic 1 (> 0 arg)))
;;;_   > outline-open-sibtopic (arg)
(defun outline-open-sibtopic (arg)
  "Open new topic header at same level as the current one.

Positive universal arg means to use the bullet of the prior sibling.

Negative universal arg means to place the new topic prior to the current
one."
  (interactive "p")
  (outline-open-topic 0 (> 0 arg) (< 1 arg)))
;;;_   > outline-open-supertopic (arg)
(defun outline-open-supertopic (arg)
  "Open new topic header at shallower level than the current one.

Negative universal arg means to open shallower, but place the new
topic prior to the current one."

  (interactive "p")
  (outline-open-topic -1 (> 0 arg)))

;;;_  - Outline Alteration
;;;_   : Topic Modification
;;;_    = outline-former-auto-filler
(defvar outline-former-auto-filler nil
  "Name of modal fill function being wrapped by outline-auto-fill.")
;;;_    > outline-auto-fill ()
(defun outline-auto-fill ()
  "Outline-mode autofill function.

Maintains outline hanging topic indentation if
`outline-use-hanging-indents' is set."
  (let ((fill-prefix (if outline-use-hanging-indents
                         ;; Check for topic header indentation:
                         (save-excursion
                           (beginning-of-line)
                           (if (looking-at outline-regexp)
                               ;; ... construct indentation to account for
                               ;; length of topic prefix:
                               (make-string (progn (outline-end-of-prefix)
                                                   (current-column))
                                            ?\ ))))))
    (if (or outline-former-auto-filler outline-use-hanging-indents)
        (do-auto-fill))))
;;;_    > outline-reindent-body (old-depth new-depth &optional number)
(defun outline-reindent-body (old-depth new-depth &optional number)
  "Reindent body lines which were indented at old-depth to new-depth.

Optional arg NUMBER indicates numbering is being added, and it must
be accommodated.

Note that refill of indented paragraphs is not done."

  (save-excursion
    (outline-end-of-prefix)
    (let* ((new-margin (current-column))
	   excess old-indent-begin old-indent-end
	   curr-ind
	   ;; We want the column where the header-prefix text started
	   ;; *before* the prefix was changed, so we infer it relative
	   ;; to the new margin and the shift in depth:
	   (old-margin (+ old-depth (- new-margin new-depth))))

      ;; Process lines up to (but excluding) next topic header:
      (outline-unprotected
       (save-match-data
         (while
	     (and (re-search-forward "[\n\r]\\(\\s-*\\)"
				     nil
				     t)
		  ;; Register the indent data, before we reset the
		  ;; match data with a subsequent `looking-at':
		  (setq old-indent-begin (match-beginning 1)
			old-indent-end (match-end 1))
		  (not (looking-at outline-regexp)))
	   (if (> 0 (setq excess (- (current-column)
				     old-margin)))
	       ;; Text starts left of old margin - don't adjust:
	       nil
	     ;; Text was hanging at or right of old left margin -
	     ;; reindent it, preserving its existing indentation
	     ;; beyond the old margin:
	     (delete-region old-indent-begin old-indent-end)
	     (indent-to (+ new-margin excess)))))))))
;;;_    > outline-rebullet-current-heading (arg)
(defun outline-rebullet-current-heading (arg)
  "Solicit new bullet for current visible heading."
  (interactive "p")
  (let ((initial-col (current-column))
	(on-bullet (eq (point)(outline-current-bullet-pos)))
	(backwards (if (< arg 0)
		       (setq arg (* arg -1)))))
    (while (> arg 0)
      (save-excursion (outline-back-to-current-heading)
		      (outline-end-of-prefix)
		      (outline-rebullet-heading t	;;; solicit
						nil	;;; depth
						nil	;;; number-control
						nil	;;; index
						t))	;;; do-successors
      (setq arg (1- arg))
      (if (<= arg 0)
	  nil
	(setq initial-col nil)		; Override positioning back to init col
	(if (not backwards)
	    (outline-next-visible-heading 1)
	  (outline-goto-prefix)
	  (outline-next-visible-heading -1))))
    (message "Done.")
    (cond (on-bullet (goto-char (outline-current-bullet-pos)))
	  (initial-col (move-to-column initial-col)))))
;;;_    > outline-rebullet-heading (&optional solicit ...)
(defun outline-rebullet-heading (&optional solicit
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
numbered form.  It has effect only if `outline-numbered-bullet' is
non-nil and soliciting was not explicitly invoked (via first arg).
Its effect, numbering or denumbering, then depends on the setting
of the forth arg, INDEX.

If NUMBER-CONTROL is non-nil and forth arg INDEX is nil, then the
prefix of the topic is forced to be non-numbered.  Null index and
non-nil NUMBER-CONTROL forces denumbering.  Non-nil INDEX (and
non-nil NUMBER-CONTROL) forces a numbered-prefix form.  If non-nil
INDEX is a number, then that number is used for the numbered
prefix.  Non-nil and non-number means that the index for the
numbered prefix will be derived by outline-make-topic-prefix.

Fifth arg DO-SUCCESSORS t means re-resolve count on succeeding
siblings.

Cf vars `outline-stylish-prefixes', `outline-old-style-prefixes',
and `outline-numbered-bullet', which all affect the behavior of
this function."

  (let* ((current-depth (outline-depth))
         (new-depth (or new-depth current-depth))
         (mb outline-recent-prefix-beginning)
         (me outline-recent-prefix-end)
         (current-bullet (buffer-substring (- me 1) me))
         (new-prefix (outline-make-topic-prefix current-bullet
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
      (outline-unprotected (delete-region mb me))
      (goto-char mb)
					; Dispense with number if
					; numbered-bullet prefix:
      (if (and outline-numbered-bullet
               (string= outline-numbered-bullet current-bullet)
               (looking-at "[0-9]+"))
	  (outline-unprotected
	   (delete-region (match-beginning 0)(match-end 0))))

					; Put in new prefix:
      (outline-unprotected (insert new-prefix))

      ;; Reindent the body if elected and margin changed:
      (if (and outline-reindent-bodies
	       (not (= new-depth current-depth)))
	  (outline-reindent-body current-depth new-depth))

      ;; Recursively rectify successive siblings of orig topic if
      ;; caller elected for it:
      (if do-successors
	  (save-excursion
	    (while (outline-next-sibling new-depth nil)
	      (setq index
		    (cond ((numberp index) (1+ index))
			  ((not number-control)  (outline-sibling-index))))
	      (if (outline-numbered-type-prefix)
		  (outline-rebullet-heading nil		;;; solicit
					    new-depth	;;; new-depth
					    number-control;;; number-control
					    index	;;; index
					    nil)))))	;;;(dont!)do-successors
      )	; (if (and (= current-depth new-depth)...))
    ) ; let* ((current-depth (outline-depth))...)
  ) ; defun
;;;_    > outline-rebullet-topic (arg)
(defun outline-rebullet-topic (arg)
  "Like outline-rebullet-topic-grunt, but start from topic visible at point.

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
      (outline-back-to-current-heading)
      (if (<= (+ (outline-recent-depth) arg) 0)
          (error "Attempt to shift topic below level 1"))
      (outline-rebullet-topic-grunt arg)
      (if (not (zerop arg)) (message "Shifting... done.")))
    (move-to-column (max 0 (+ start-col arg)))))
;;;_     > outline-rebullet-topic-grunt (&optional relative-depth ...)
(defun outline-rebullet-topic-grunt (&optional relative-depth
                                               starting-depth
                                               starting-point
                                               index
                                               do-successors)

  "Rebullet the topic at point, visible or invisible, and all
contained subtopics.  See outline-rebullet-heading for rebulleting
behavior.

All arguments are optional.

First arg RELATIVE-DEPTH means to shift the depth of the entire
topic that amount.

The rest of the args are for internal recursive use by the function
itself.  The are STARTING-DEPTH, STARTING-POINT, and INDEX."

  (let* ((relative-depth (or relative-depth 0))
         (new-depth (outline-depth))
         (starting-depth (or starting-depth new-depth))
         (on-starting-call  (null starting-point))
         (index (or index
                    ;; Leave index null on starting call, so rebullet-heading
                    ;; calculates it at what might be new depth:
                    (and (or (zerop relative-depth)
                             (not on-starting-call))
                         (outline-sibling-index))))
         (moving-outwards (< 0 relative-depth))
         (starting-point (or starting-point (point))))

    ;; Sanity check for excessive promotion done only on starting call:
    (and on-starting-call
         moving-outwards
         (> 0 (+ starting-depth relative-depth))
         (error "Attempt to shift topic out beyond level 1"))	;;; ====>

    (cond ((= starting-depth new-depth)
           ;; We're at depth to work on this one:
           (outline-rebullet-heading nil		;;; solicit
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
           (outline-next-heading)
           (while (and (not (eobp))
                       (< starting-depth (outline-recent-depth)))
             (setq index (1+ index))
             (outline-rebullet-topic-grunt relative-depth   ;;; relative-depth
                                           (1+ starting-depth);;;starting-depth
                                           starting-point   ;;; starting-point
                                           index)))	    ;;; index

          ((< starting-depth new-depth)
           ;; Rare case - subtopic more than one level deeper than parent.
           ;; Treat this one at an even deeper level:
           (outline-rebullet-topic-grunt relative-depth   ;;; relative-depth
                                         new-depth	  ;;; starting-depth
                                         starting-point	  ;;; starting-point
                                         index)))	  ;;; index

    (if on-starting-call
        (progn
          ;; Rectify numbering of former siblings of the adjusted topic,
          ;; if topic has changed depth
          (if (or do-successors
                  (and (not (zerop relative-depth))
                       (or (= (outline-recent-depth) starting-depth)
                           (= (outline-recent-depth) (+ starting-depth
                                                        relative-depth)))))
              (outline-rebullet-heading nil nil nil nil t))
          ;; Now rectify numbering of new siblings of the adjusted topic,
          ;; if depth has been changed:
          (progn (goto-char starting-point)
                 (if (not (zerop relative-depth))
                     (outline-rebullet-heading nil nil nil nil t)))))
    )
  )
;;;_    > outline-renumber-to-depth (&optional depth)
(defun outline-renumber-to-depth (&optional depth)
  "Renumber siblings at current depth.

Affects superior topics if optional arg DEPTH is less than current depth.

Returns final depth."

  ;; Proceed by level, processing subsequent siblings on each,
  ;; ascending until we get shallower than the start depth:

  (let ((ascender (outline-depth))
	was-eobp)
    (while (and (not (eobp))
		(outline-depth)
                (>= (outline-recent-depth) depth)
                (>= ascender depth))
                                        ; Skip over all topics at
                                        ; lesser depths, which can not
                                        ; have been disturbed:
      (while (and (not (setq was-eobp (eobp)))
		  (> (outline-recent-depth) ascender))
        (outline-next-heading))
                                        ; Prime ascender for ascension:
      (setq ascender (1- (outline-recent-depth)))
      (if (>= (outline-recent-depth) depth)
          (outline-rebullet-heading nil	;;; solicit
                                    nil	;;; depth
                                    nil	;;; number-control
                                    nil	;;; index
                                    t)) ;;; do-successors
      (if was-eobp (goto-char (point-max)))))
  (outline-recent-depth))
;;;_    > outline-number-siblings (&optional denumber)
(defun outline-number-siblings (&optional denumber)
  "Assign numbered topic prefix to this topic and its siblings.

With universal argument, denumber - assign default bullet to this
topic and its siblings.

With repeated universal argument (`^U^U'), solicit bullet for each
rebulleting each topic at this level."

  (interactive "P")

  (save-excursion
    (outline-back-to-current-heading)
    (outline-beginning-of-level)
    (let ((depth (outline-recent-depth))
	  (index (if (not denumber) 1))
          (use-bullet (equal '(16) denumber))
          (more t))
      (while more
        (outline-rebullet-heading use-bullet		;;; solicit
                                  depth			;;; depth
                                  t			;;; number-control
                                  index			;;; index
                                  nil)			;;; do-successors
        (if index (setq index (1+ index)))
        (setq more (outline-next-sibling depth nil))))))
;;;_    > outline-shift-in (arg)
(defun outline-shift-in (arg)
  "Increase depth of current heading and any topics collapsed within it."
  (interactive "p")
  (outline-rebullet-topic arg))
;;;_    > outline-shift-out (arg)
(defun outline-shift-out (arg)
  "Decrease depth of current heading and any topics collapsed within it."
  (interactive "p")
  (outline-rebullet-topic (* arg -1)))
;;;_   : Surgery (kill-ring) functions with special provisions for outlines:
;;;_    > outline-kill-line (&optional arg)
(defun outline-kill-line (&optional arg)
  "Kill line, adjusting subsequent lines suitably for outline mode."

  (interactive "*P")
  (if (not (and (outline-mode-p)		; active outline mode,
		outline-numbered-bullet		; numbers may need adjustment,
		(bolp)				; may be clipping topic head,
		(looking-at outline-regexp)))	; are clipping topic head.
      ;; Above conditions do not obtain - just do a regular kill:
      (kill-line arg)
    ;; Ah, have to watch out for adjustments:
    (let* ((depth (outline-depth)))
                                        ; Do the kill:
      (kill-line arg)
                                        ; Provide some feedback:
      (sit-for 0)
      (save-excursion
                                        ; Start with the topic
                                        ; following killed line:
        (if (not (looking-at outline-regexp))
            (outline-next-heading))
        (outline-renumber-to-depth depth)))))
;;;_    > outline-kill-topic ()
(defun outline-kill-topic ()
  "Kill topic together with subtopics.

Leaves primary topic's trailing vertical whitespace, if any."

  ;; Some finagling is done to make complex topic kills appear faster
  ;; than they actually are.  A redisplay is performed immediately
  ;; after the region is disposed of, though the renumbering process
  ;; has yet to be performed.  This means that there may appear to be
  ;; a lag *after* the kill has been performed.

  (interactive)
  (let* ((beg (prog1 (outline-back-to-current-heading)(beginning-of-line)))
         (depth (outline-recent-depth)))
    (outline-end-of-current-subtree)
    (if (not (eobp))
	(if (or (not (looking-at "^$"))
		;; A blank line - cut it with this topic *unless* this
		;; is the last topic at this level, in which case
		;; we'll leave the blank line as part of the
		;; containing topic:
		(save-excursion
		  (and (outline-next-heading)
		       (>= (outline-recent-depth) depth))))
	    (forward-char 1)))

    (kill-region beg (point))
    (sit-for 0)
    (save-excursion
      (outline-renumber-to-depth depth))))
;;;_    > outline-yank-processing ()
(defun outline-yank-processing (&optional arg)

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
however, are left exactly like normal, non-outline-specific yanks."

  (interactive "*P")
					; Get to beginning, leaving
					; region around subject:
  (if (< (my-mark-marker t) (point))
      (exchange-point-and-mark))
  (let* ((subj-beg (point))
	 (subj-end (my-mark-marker t))
	 ;; 'resituate' if yanking an entire topic into topic header:
	 (resituate (and (outline-e-o-prefix-p)
			 (looking-at (concat "\\(" outline-regexp "\\)"))
			 (outline-prefix-data (match-beginning 1)
					      (match-end 1))))
	 ;; `rectify-numbering' if resituating (where several topics may
	 ;; be resituating) or yanking a topic into a topic slot (bol):
	 (rectify-numbering (or resituate
				(and (bolp) (looking-at outline-regexp)))))
    (if resituate
                                        ; The yanked stuff is a topic:
	(let* ((prefix-len (- (match-end 1) subj-beg))
	       (subj-depth (outline-recent-depth))
	       (prefix-bullet (outline-recent-bullet))
	       (adjust-to-depth
		;; Nil if adjustment unnecessary, otherwise depth to which
		;; adjustment should be made:
		(save-excursion
		  (and (goto-char subj-end)
		       (eolp)
		       (goto-char subj-beg)
		       (and (looking-at outline-regexp)
			    (progn
			      (beginning-of-line)
			      (not (= (point) subj-beg)))
			    (looking-at outline-regexp)
			    (outline-prefix-data (match-beginning 0)
						 (match-end 0)))
		       (outline-recent-depth))))
	       done
	       (more t))
	  (setq rectify-numbering outline-numbered-bullet)
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
		      (outline-unprotected (delete-char -1)))
                                        ; Work backwards, with each
                                        ; shallowest level,
                                        ; successively excluding the
                                        ; last processed topic from
                                        ; the narrow region:
		  (while more
		    (outline-back-to-current-heading)
                                        ; go as high as we can in each bunch:
		    (while (outline-ascend-to-depth (1- (outline-depth))))
		    (save-excursion
		      (outline-rebullet-topic-grunt (- adjust-to-depth
						       subj-depth))
		      (outline-depth))
		    (if (setq more (not (bobp)))
			(progn (widen)
			       (forward-char -1)
			       (narrow-to-region subj-beg (point))))))
		(message "")
		;; Preserve new bullet if it's a distinctive one, otherwise
		;; use old one:
		(if (string-match (regexp-quote prefix-bullet)
				  outline-distinctive-bullets-string)
                                        ; Delete from bullet of old to
                                        ; before bullet of new:
		    (progn
		      (beginning-of-line)
		      (delete-region (point) subj-beg)
		      (set-marker (my-mark-marker t) subj-end)
		      (goto-char subj-beg)
		      (outline-end-of-prefix))
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
	    (if (outline-goto-prefix)
		(outline-rebullet-heading nil	;;; solicit
					  (outline-depth) ;;; depth
					  nil	;;; number-control
					  nil	;;; index
					  t))
	    (message ""))))
    (if (not resituate)
      (exchange-point-and-mark))))
;;;_    > outline-yank (&optional arg)
(defun outline-yank (&optional arg)
  "Outline-mode yank, with depth and numbering adjustment of yanked topics.

Non-topic yanks work no differently than normal yanks.

If a topic is being yanked into a bare topic prefix, the depth of the
yanked topic is adjusted to the depth of the topic prefix.

  1 we're yanking in an outline-mode buffer
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

Outline-yank-pop works with outline-yank just like normal yank-pop
works with normal yank in non-outline buffers."

  (interactive "*P")
  (setq this-command 'yank)
  (yank arg)
  (if (outline-mode-p)
      (outline-yank-processing)))
;;;_    > outline-yank-pop (&optional arg)
(defun outline-yank-pop (&optional arg)
  "Yank-pop like outline-yank when popping to bare outline prefixes.

Adapts level of popped topics to level of fresh prefix.

Note - prefix changes to distinctive bullets will stick, if followed
by pops to non-distinctive yanks.  Bug..."

  (interactive "*p")
  (setq this-command 'yank)
  (yank-pop arg)
  (if (outline-mode-p)
      (outline-yank-processing)))

;;;_  - Specialty bullet functions
;;;_   : File Cross references
;;;_    > outline-resolve-xref ()
(defun outline-resolve-xref ()
  "Pop to file associated with current heading, if it has an xref bullet.

\(Works according to setting of `outline-file-xref-bullet')."
  (interactive)
  (if (not outline-file-xref-bullet)
      (error
       "outline cross references disabled - no `outline-file-xref-bullet'")
    (if (not (string= (outline-current-bullet) outline-file-xref-bullet))
        (error "current heading lacks cross-reference bullet `%s'"
               outline-file-xref-bullet)
      (let (file-name)
        (save-excursion
          (let* ((text-start outline-recent-prefix-end)
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
;;;_   > outline-flag-current-subtree (flag)
(defun outline-flag-current-subtree (flag)
  "Hide or show subtree of currently-visible topic.

See `outline-flag-region' for more details."

  (save-excursion
    (outline-back-to-current-heading)
    (outline-flag-region (point)
			 (progn (outline-end-of-current-subtree) (1- (point)))
			 flag)))

;;;_  - Topic-specific
;;;_   > outline-show-entry ()
(defun outline-show-entry ()
  "Like `outline-show-current-entry', reveals entries nested in hidden topics.

This is a way to give restricted peek at a concealed locality without the
expense of exposing its context, but can leave the outline with aberrant
exposure.  outline-hide-current-entry-completely or outline-show-offshoot
should be used after the peek to rectify the exposure."

  (interactive)
  (save-excursion
    (let ((at (point))
	  beg end)
      (outline-goto-prefix)
      (setq beg (if (= (preceding-char) ?\r) (1- (point)) (point)))
      (re-search-forward "[\n\r]" nil t)
      (setq end (1- (if (< at (point))
			;; We're on topic head line - show only it:
			(point)
		      ;; or we're in body - include it:
		      (max beg (or (outline-pre-next-preface) (point))))))
      (outline-flag-region beg end ?\n)
      (list beg end))))
;;;_   > outline-show-children (&optional level strict)
(defun outline-show-children (&optional level strict)

  "If point is visible, show all direct subheadings of this heading.

Otherwise, do outline-show-to-offshoot, and then show subheadings.

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
	     (outline-hidden-p))

	(progn (outline-show-to-offshoot) ; Point's concealed, open to
					  ; expose it.
	       ;; Then recurse, but with "strict" set so we don't
	       ;; infinite regress:
	       (setq max-pos (outline-show-children level t)))

      (save-excursion
	(save-restriction
	  (let* ((start-pt (point))
		 (chart (outline-chart-subtree (or level 1)))
		 (to-reveal (outline-chart-to-reveal chart (or level 1))))
	    (goto-char start-pt)
	    (if (and strict (= (preceding-char) ?\r))
		;; Concealed root would already have been taken care of,
		;; unless strict was set.
		(progn
		  (outline-flag-region (point) (outline-snug-back) ?\n)
		  (if outline-show-bodies
		      (progn (goto-char (car to-reveal))
			     (outline-show-current-entry)))))
	    (while to-reveal
	      (goto-char (car to-reveal))
	      (outline-flag-region (point) (outline-snug-back) ?\n)
	      (if outline-show-bodies
		  (progn (goto-char (car to-reveal))
			 (outline-show-current-entry)))
	      (setq to-reveal (cdr to-reveal)))))))))
;;;_   > outline-hide-point-reconcile ()
(defun outline-hide-reconcile ()
  "Like `outline-hide-current-entry'; hides completely if within hidden region.

Specifically intended for aberrant exposure states, like entries that were
exposed by outline-show-entry but are within otherwise concealed regions."
  (interactive)
  (save-excursion
    (outline-goto-prefix)
    (outline-flag-region (if (not (bobp)) (1- (point)) (point))
                         (progn (outline-pre-next-preface)
                                (if (= ?\r (following-char))
                                    (point)
                                  (1- (point))))
                         ?\r)))
;;;_   > outline-show-to-offshoot ()
(defun outline-show-to-offshoot ()
  "Like outline-show-entry, but reveals all concealed ancestors, as well.

As with outline-hide-current-entry-completely, useful for rectifying
aberrant exposure states produced by outline-show-entry."

  (interactive)
  (save-excursion
    (let ((orig-pt (point))
	  (orig-pref (outline-goto-prefix))
	  (last-at (point))
	  bag-it)
      (while (or bag-it (= (preceding-char) ?\r))
	(beginning-of-line)
	(if (= last-at (setq last-at (point)))
	    ;; Oops, we're not making any progress!  Show the current
	    ;; topic completely, and bag this try.
	    (progn (beginning-of-line)
		   (outline-show-current-subtree)
		   (goto-char orig-pt)
		   (setq bag-it t)
		   (beep)
		   (message "%s: %s"
			    "outline-show-to-offshoot: "
			    "Aberrant nesting encountered.")))
	(outline-show-children)
	(goto-char orig-pref))
      (goto-char orig-pt)))
  (if (outline-hidden-p)
      (outline-show-entry)))
;;;_   > outline-hide-current-entry ()
(defun outline-hide-current-entry ()
  "Hide the body directly following this heading."
  (interactive)
  (outline-back-to-current-heading)
  (save-excursion
   (outline-flag-region (point)
                        (progn (outline-end-of-current-entry) (point))
                        ?\r)))
;;;_   > outline-show-current-entry (&optional arg)
(defun outline-show-current-entry (&optional arg)

  "Show body following current heading, or hide the entry if repeat count."

  (interactive "P")
  (if arg
      (outline-hide-current-entry)
    (save-excursion
      (outline-flag-region (point)
			   (progn (outline-end-of-current-entry) (point))
			   ?\n))))
;;;_   > outline-hide-current-entry-completely ()
; ... outline-hide-current-entry-completely also for isearch dynamic exposure:
(defun outline-hide-current-entry-completely ()
  "Like outline-hide-current-entry, but conceal topic completely.

Specifically intended for aberrant exposure states, like entries that were
exposed by outline-show-entry but are within otherwise concealed regions."
  (interactive)
  (save-excursion
    (outline-goto-prefix)
    (outline-flag-region (if (not (bobp)) (1- (point)) (point))
                         (progn (outline-pre-next-preface)
                                (if (= ?\r (following-char))
                                    (point)
                                  (1- (point))))
                         ?\r)))
;;;_   > outline-show-current-subtree (&optional arg)
(defun outline-show-current-subtree (&optional arg)
  "Show everything within the current topic.  With a repeat-count,
expose this topic and its siblings."
  (interactive "P")
  (save-excursion
    (if (<= (outline-current-depth) 0)
	;; Outside any topics - try to get to the first:
	(if (not (outline-next-heading))
	    (error "No topics")
	  ;; got to first, outermost topic - set to expose it and siblings:
	  (message "Above outermost topic - exposing all.")
	  (outline-flag-region (point-min)(point-max) ?\n))
      (if (not arg)
	  (outline-flag-current-subtree ?\n)
	(outline-beginning-of-level)
	(outline-expose-topic '(* :))))))
;;;_   > outline-hide-current-subtree (&optional just-close)
(defun outline-hide-current-subtree (&optional just-close)
  "Close the current topic, or containing topic if this one is already closed.

If this topic is closed and it's a top level topic, close this topic
and its siblings.

If optional arg JUST-CLOSE is non-nil, do not treat the parent or
siblings, even if the target topic is already closed."

  (interactive)
  (let ((from (point))
	(orig-eol (progn (end-of-line)
			 (if (not (outline-goto-prefix))
			     (error "No topics found")
			   (end-of-line)(point)))))
    (outline-flag-current-subtree ?\r)
    (goto-char from)
    (if (and (= orig-eol (progn (goto-char orig-eol)
				(end-of-line)
				(point)))
	     (not just-close)
             ;; Structure didn't change - try hiding current level:
	     (goto-char from)
	     (if (outline-up-current-level 1 t)
		 t
	       (goto-char 0)
	       (let ((msg
		      "Top-level topic already closed - closing siblings..."))
		 (message msg)
		 (outline-expose-topic '(0 :))
		 (message (concat msg "  Done.")))
	       nil)
	     (/= (outline-recent-depth) 0))
	(outline-hide-current-subtree))
      (goto-char from)))
;;;_   > outline-show-current-branches ()
(defun outline-show-current-branches ()
  "Show all subheadings of this heading, but not their bodies."
  (interactive)
  (beginning-of-line)
  (outline-show-children t))
;;;_   > outline-hide-current-leaves ()
(defun outline-hide-current-leaves ()
  "Hide the bodies of the current topic and all its offspring."
  (interactive)
  (outline-back-to-current-heading)
  (outline-hide-region-body (point) (progn (outline-end-of-current-subtree)
                                           (point))))

;;;_  - Region and beyond
;;;_   > outline-show-all ()
(defun outline-show-all ()
  "Show all of the text in the buffer."
  (interactive)
  (message "Exposing entire buffer...")
  (outline-flag-region (point-min) (point-max) ?\n)
  (message "Exposing entire buffer...  Done."))
;;;_   > outline-hide-bodies ()
(defun outline-hide-bodies ()
  "Hide all of buffer except headings."
  (interactive)
  (outline-hide-region-body (point-min) (point-max)))
;;;_   > outline-hide-region-body (start end)
(defun outline-hide-region-body (start end)
  "Hide all body lines in the region, but not headings."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (not (eobp))
	(outline-flag-region (point)
                             (progn (outline-pre-next-preface) (point)) ?\r)
	(if (not (eobp))
	    (forward-char
	     (if (looking-at "[\n\r][\n\r]")
		 2 1)))))))

;;;_   > outline-expose-topic (spec)
(defun outline-expose-topic (spec)
  "Apply exposure specs to successive outline topic items.

Use the more convenient frontend, `outline-new-exposure', if you don't
need evaluation of the arguments, or even better, the `outline-layout'
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
\(outline-expose-topic '(-1 : 0))
	Close this and all following topics at current level, exposing
	only their immediate children, but close down the last topic
	at this current level completely.
\(outline-expose-topic '(-1 () : 1 0))
	Close current topic so only the immediate subtopics are shown;
	show the children in the second to last topic, and completely
	close the last one.
\(outline-expose-topic '(-2 : -1 *))
        Expose children and grandchildren of all topics at current
	level except the last two; expose children of the second to
	last and completely open the last one."

  (interactive "xExposure spec: ")
  (if (not (listp spec))
      nil
    (let ((depth (outline-depth))
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
	  (cond ((eq curr-elem '*) (outline-show-current-subtree)
		 (if (> outline-recent-end-of-subtree max-pos)
		     (setq max-pos outline-recent-end-of-subtree)))
		((eq curr-elem '+) (outline-show-current-branches)
		 (if (> outline-recent-end-of-subtree max-pos)
		     (setq max-pos outline-recent-end-of-subtree)))
		((eq curr-elem '-) (outline-show-current-entry))
		((eq curr-elem ':)
		 (setq stay t)
		 ;; Expand the `repeat' spec to an explicit version,
		 ;; w.r.t. remaining siblings:
		 (let ((residue	   ; = # of sibs not covered by remaining spec
			;; Dang - could be nice to make use of the chart, sigh:
			(- (length (outline-chart-siblings))
			   (length spec))))
		   (if (< 0 residue)
		       ;; Some residue - cover it with prev-elem:
		       (setq spec (append (make-list residue prev-elem)
					  spec)))))))
	 ((numberp curr-elem)
	  (if (and (>= 0 curr-elem) (outline-visible-p))
	      (save-excursion (outline-hide-current-subtree t)
			      (if (> 0 curr-elem)
				  nil
				(if (> outline-recent-end-of-subtree max-pos)
				    (setq max-pos
					  outline-recent-end-of-subtree)))))
	  (if (> (abs curr-elem) 0)
	      (progn (outline-show-children (abs curr-elem))
		     (if (> outline-recent-end-of-subtree max-pos)
			 (setq max-pos outline-recent-end-of-subtree)))))
	  ((listp curr-elem)
	   (if (outline-descend-to-depth (1+ depth))
	       (let ((got (outline-expose-topic curr-elem)))
		 (if (and got (> got max-pos)) (setq max-pos got))))))
	(cond (stay (setq stay nil))
	      ((listp (car spec)) nil)
	      ((> max-pos (point))
	       ;; Capitalize on max-pos state to get us nearer next sibling:
	       (progn (goto-char (min (point-max) max-pos))
		      (outline-next-heading)))
	      ((outline-next-sibling depth))))
      max-pos)))
;;;_   > outline-old-expose-topic (spec &rest followers)
(defun outline-old-expose-topic (spec &rest followers)

  "Deprecated.  Use outline-expose-topic \(with different schema
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

Optional FOLLOWER arguments dictate exposure for succeeding siblings."

  (interactive "xExposure spec: ")
  (let ((depth (outline-current-depth))
	done
	max-pos)
    (cond ((null spec) nil)
	  ((symbolp spec)
	   (if (eq spec '*) (outline-show-current-subtree))
	   (if (eq spec '+) (outline-show-current-branches))
	   (if (eq spec '-) (outline-show-current-entry)))
	  ((numberp spec)
	   (if (>= 0 spec)
	       (save-excursion (outline-hide-current-subtree t)
			       (end-of-line)
			       (if (or (not max-pos)
				       (> (point) max-pos))
				   (setq max-pos (point)))
			       (if (> 0 spec)
				   (setq spec (* -1 spec)))))
	   (if (> spec 0)
	     (outline-show-children spec)))
	  ((listp spec)
	   ;(let ((got (outline-old-expose-topic (car spec))))
	   ;  (if (and got (or (not max-pos) (> got max-pos)))
	   ;	 (setq max-pos got)))
	   (let ((new-depth  (+ (outline-current-depth) 1))
		 got)
	     (setq max-pos (outline-old-expose-topic (car spec)))
	     (setq spec (cdr spec))
	     (if (and spec
		      (outline-descend-to-depth new-depth)
		      (not (outline-hidden-p)))
		 (progn (setq got (apply 'outline-old-expose-topic spec))
			(if (and got (or (not max-pos) (> got max-pos)))
			    (setq max-pos got)))))))
    (while (and followers
		(progn (if (and max-pos (< (point) max-pos))
			   (progn (goto-char max-pos)
				  (setq max-pos nil)))
		       (end-of-line)
		       (outline-next-sibling depth)))
      (outline-old-expose-topic (car followers))
      (setq followers (cdr followers)))
    max-pos))
;;;_   > outline-new-exposure '()
(defmacro outline-new-exposure (&rest spec)
  "Literal frontend for `outline-expose-topic', doesn't evaluate arguments.
Some arguments that would need to be quoted in outline-expose-topic
need not be quoted in outline-new-exposure.

Cursor is left at start position.

Use this instead of obsolete `outline-exposure'.

Examples:
\(outline-exposure (-1 () () () 1) 0)
	Close current topic at current level so only the immediate
	subtopics are shown, except also show the children of the
	third subtopic; and close the next topic at the current level.
\(outline-exposure : -1 0)
	Close all topics at current level to expose only their
	immediate children, except for the last topic at the current
	level, in which even its immediate children are hidden.
\(outline-exposure -2 : -1 *)
        Expose children and grandchildren of first topic at current
	level, and expose children of subsequent topics at current
	level *except* for the last, which should be opened completely."
  (list 'save-excursion
	'(if (not (or (outline-goto-prefix)
		      (outline-next-heading)))
	     (error "outline-new-exposure: Can't find any outline topics"))
	(list 'outline-expose-topic (list 'quote spec))))
;;;_   > outline-exposure '()
(defmacro outline-exposure (&rest spec)
  "Being deprecated - use more recent `outline-new-exposure' instead.

Literal frontend for `outline-old-expose-topic', doesn't evaluate arguments
and retains start position."
  (list 'save-excursion
	'(if (not (or (outline-goto-prefix)
		      (outline-next-heading)))
	     (error "Can't find any outline topics"))
	(cons 'outline-old-expose-topic
	      (mapcar (function (lambda (x) (list 'quote x))) spec))))

;;;_ #7 Systematic outline presentation - copying, printing, flattening

;;;_  - Mapping and processing of topics
;;;_   ( See also Subtree Charting, in Navigation code.)
;;;_   > outline-stringify-flat-index (flat-index)
(defun outline-stringify-flat-index (flat-index &optional context)
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
;;;_   > outline-stringify-flat-index-plain (flat-index)
(defun outline-stringify-flat-index-plain (flat-index)
  "Convert list representing section/subsection/... to document string."
  (let ((delim ".")
	result)
	(while flat-index
	  (setq result (cons (int-to-string (car flat-index))
			     (if result
				 (cons delim result))))
	  (setq flat-index (cdr flat-index)))
    (apply 'concat result)))
;;;_   > outline-stringify-flat-index-indented (flat-index)
(defun outline-stringify-flat-index-indented (flat-index)
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
;;;_   > outline-listify-exposed (&optional start end format)
(defun outline-listify-exposed (&optional start end format)

  "Produce a list representing exposed topics in current region.

This list can then be used by `outline-process-exposed' to manipulate
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
      (if (> (outline-goto-prefix) start)
	  ;; First topic follows beginning point - register preliminary stuff:
	  (setq result (list (list 0 "" nil
				   (buffer-substring start (1- (point)))))))
      (while (and (not done)
		  (not (eobp))		; Loop until we've covered the region.
		  (not (> (point) end)))
	(setq depth (outline-recent-depth) 	; Current topics depth,
	      bullet (outline-recent-bullet)	; ... bullet,
	      prefix (outline-recent-prefix)
	      beg (progn (outline-end-of-prefix t) (point))) ; and beginning.
	(setq done			; The boundary for the current topic:
	      (not (outline-next-visible-heading 1)))
	(setq new-depth (outline-recent-depth))
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
				       outline-distinctive-bullets-string)
				      bullet)))
		     (cond ((listp format)
			    (list depth
				  (if outline-abbreviate-flattened-numbering
				      (outline-stringify-flat-index format
								    gone-out)
				      (outline-stringify-flat-index-plain
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
			   (t (error "outline-listify-exposed: %s %s"
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
;;;_   > outline-process-exposed (&optional func from to frombuf
;;;					    tobuf format)
(defun outline-process-exposed (&optional func from to frombuf tobuf
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
  FUNCTION:	`outline-insert-listified'
  FROM:		region start, if region active, else start of buffer
  TO:		region end, if region active, else end of buffer
  FROMBUF:	current buffer
  TOBUF:	buffer name derived: \"*current-buffer-name exposed*\"
  FORMAT:	nil"

					; Resolve arguments,
					; defaulting if necessary:
  (if (not func) (setq func 'outline-insert-listified))
  (if (not (and from to))
      (if (my-region-active-p)
	  (setq from (region-beginning) to (region-end))
	(setq from (point-min) to (point-max))))
  (if frombuf
      (if (not (bufferp frombuf))
	  ;; Specified but not a buffer - get it:
	  (let ((got (get-buffer frombuf)))
	    (if (not got)
		(error (concat "outline-process-exposed: source buffer "
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
		 (outline-listify-exposed from to format))))
    (set-buffer tobuf)
    (mapcar func listified)
    (pop-to-buffer tobuf)))

;;;_  - Copy exposed
;;;_   > outline-insert-listified (listified)
(defun outline-insert-listified (listified)
  "Insert contents of listified outline portion in current buffer.

Listified is a list representing each topic header and body:

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
	  (insert "\n")))
    (insert "\n")))
;;;_   > outline-copy-exposed-to-buffer (&optional arg tobuf format)
(defun outline-copy-exposed-to-buffer (&optional arg tobuf format)
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
	 (beg (if arg (outline-back-to-current-heading) (point-min)))
	 (end (if arg (outline-end-of-current-subtree) (point-max)))
	 (buf (current-buffer))
	 (start-list ()))
    (if (eq format 'flat)
	(setq format (if arg (save-excursion
				   (goto-char beg)
				   (outline-topic-flat-index))
			   '(1))))
    (save-excursion (set-buffer tobuf)(erase-buffer))
    (outline-process-exposed 'outline-insert-listified
			     beg
			     end
			     (current-buffer)
			     tobuf
			     format start-list)
    (goto-char (point-min))
    (pop-to-buffer buf)
    (goto-char start-pt)))
;;;_   > outline-flatten-exposed-to-buffer (&optional arg tobuf)
(defun outline-flatten-exposed-to-buffer (&optional arg tobuf)
  "Present numeric outline of outline's exposed portions in another buffer.

The resulting outline is not compatable with outline mode - use
`outline-copy-exposed-to-buffer' if you want that.

Use `outline-indented-exposed-to-buffer' for indented presentation.

With repeat count, copy the exposed portions of only current topic.

Other buffer has current buffers name with \" exposed\" appended to
it, unless optional second arg TOBUF is specified, in which case it is
used verbatim."
  (interactive "P")
  (outline-copy-exposed-to-buffer arg tobuf 'flat))
;;;_   > outline-indented-exposed-to-buffer (&optional arg tobuf)
(defun outline-indented-exposed-to-buffer (&optional arg tobuf)
  "Present indented outline of outline's exposed portions in another buffer.

The resulting outline is not compatable with outline mode - use
`outline-copy-exposed-to-buffer' if you want that.

Use `outline-flatten-exposed-to-buffer' for numeric sectional presentation.

With repeat count, copy the exposed portions of only current topic.

Other buffer has current buffers name with \" exposed\" appended to
it, unless optional second arg TOBUF is specified, in which case it is
used verbatim."
  (interactive "P")
  (outline-copy-exposed-to-buffer arg tobuf 'indent))

;;;_  - LaTeX formatting
;;;_   > outline-latex-verb-quote (str &optional flow)
(defun outline-latex-verb-quote (str &optional flow)
  "Return copy of STRING for literal reproduction across latex processing.
Expresses the original characters \(including carriage returns) of the
string across latex processing."
  (mapconcat (function
	      (lambda (char)
		(cond ((memq char '(?\\ ?$ ?% ?# ?& ?{ ?} ?_ ?^ ?- ?*))
		       (concat "\\char" (number-to-string char) "{}"))
		      ((= char ?\n) "\\\\")
		      (t (char-to-string char)))))
	     str
	     ""))
;;;_   > outline-latex-verbatim-quote-curr-line ()
(defun outline-latex-verbatim-quote-curr-line ()
  "Express line for exact \(literal) representation across latex processing.

Adjust line contents so it is unaltered \(from the original line)
across latex processing, within the context of a `verbatim'
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
;;;_   > outline-insert-latex-header (buf)
(defun outline-insert-latex-header (buf)
  "Insert initial latex commands at point in BUFFER."
  ;; Much of this is being derived from the stuff in appendix of E in
  ;; the TeXBook, pg 421.
  (set-buffer buf)
  (let ((doc-style (format "\n\\documentstyle{%s}\n"
			   "report"))
	(page-numbering (if outline-number-pages
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
			  outline-title-style))
	(labelcmd (format "\\newcommand{\\labelcmd}[1]{{%s #1}}\n"
			  outline-label-style))
	(headlinecmd (format "\\newcommand{\\headlinecmd}[1]{{%s #1}}\n"
			     outline-head-line-style))
	(bodylinecmd (format "\\newcommand{\\bodylinecmd}[1]{{%s #1}}\n"
			     outline-body-line-style))
	(setlength (format "%s%s%s%s"
			   "\\newlength{\\stepsize}\n"
			   "\\setlength{\\stepsize}{"
			   outline-indent
			   "}\n"))
	(oneheadline (format "%s%s%s%s%s%s%s"
			     "\\newcommand{\\OneHeadLine}[3]{%\n"
			     "\\noindent%\n"
			     "\\hspace*{#2\\stepsize}%\n"
			     "\\labelcmd{#1}\\hspace*{.2cm}"
			     "\\headlinecmd{#3}\\\\["
			     outline-line-skip
			     "]\n}\n"))
	(onebodyline (format "%s%s%s%s%s%s"
			       "\\newcommand{\\OneBodyLine}[2]{%\n"
			       "\\noindent%\n"
			       "\\hspace*{#1\\stepsize}%\n"
			       "\\bodylinecmd{#2}\\\\["
			       outline-line-skip
			       "]\n}\n"))
	(begindoc "\\begin{document}\n\\begin{center}\n")
	(title (format "%s%s%s%s"
		       "\\titlecmd{"
		       (outline-latex-verb-quote (if outline-title
						(condition-case err
						    (eval outline-title)
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
;;;_   > outline-insert-latex-trailer (buf)
(defun outline-insert-latex-trailer (buf)
  "Insert concluding latex commands at point in BUFFER."
  (set-buffer buf)
  (insert "\n\\end{document}\n"))
;;;_   > outline-latexify-one-item (depth prefix bullet text)
(defun outline-latexify-one-item (depth prefix bullet text)
  "Insert LaTeX commands for formatting one outline item.

Args are the topics numeric DEPTH, the header PREFIX lead string, the
BULLET string, and a list of TEXT strings for the body."
  (let* ((head-line (if text (car text)))
	 (body-lines (cdr text))
	 (curr-line)
	 body-content bop)
					; Do the head line:
    (insert (concat "\\OneHeadLine{\\verb\1 " 
		    (outline-latex-verb-quote bullet)
		    "\1}{"
		    depth
		    "}{\\verb\1 "
		    (if head-line
			(outline-latex-verb-quote head-line)
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
	(outline-latex-verbatim-quote-curr-line)
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
;;;_   > outline-latexify-exposed (arg &optional tobuf)
(defun outline-latexify-exposed (arg &optional tobuf)
  "Format current topics exposed portions to TOBUF for latex processing.
TOBUF defaults to a buffer named the same as the current buffer, but
with \"*\" prepended and \" latex-formed*\" appended.

With repeat count, copy the exposed portions of entire buffer."

  (interactive "P")
  (if (not tobuf)
      (setq tobuf
	    (get-buffer-create (concat "*" (buffer-name) " latexified*"))))
  (let* ((start-pt (point))
	 (beg (if arg (point-min) (outline-back-to-current-heading)))
	 (end (if arg (point-max) (outline-end-of-current-subtree)))
	 (buf (current-buffer)))
    (set-buffer tobuf)
    (erase-buffer)
    (outline-insert-latex-header tobuf)
    (goto-char (point-max))
    (outline-process-exposed 'outline-latexify-one-item
			     beg
			     end
			     buf
			     tobuf)
    (goto-char (point-max))
    (outline-insert-latex-trailer tobuf)
    (goto-char (point-min))
    (pop-to-buffer buf)
    (goto-char start-pt)))

;;;_ #8 miscellaneous
;;;_  > outline-mark-topic ()
(defun outline-mark-topic ()
  "Put the region around topic currently containing point."
  (interactive)
  (beginning-of-line)
  (outline-goto-prefix)
  (push-mark (point))
  (outline-end-of-current-subtree)
  (exchange-point-and-mark))
;;;_  > outlineify-sticky ()
;; outlinify-sticky is correct spelling; provide this alias for sticklers:
(defalias 'outlinify-sticky 'outlineify-sticky)
(defun outlineify-sticky (&optional arg)
  "Activate outline mode and establish file var so it is started subsequently.

See doc-string for `outline-layout' and `outline-init' for details on
setup for auto-startup."

  (interactive "P")

  (outline-mode t)

  (save-excursion
    (goto-char (point-min))
    (if (looking-at outline-regexp)
	t
      (outline-open-topic 2)
      (insert (concat "Dummy outline topic header - see"
		      "`outline-mode' docstring: `^Hm'."))
      (forward-line 1)
      (goto-char (point-max))
      (open-line 1)
      (outline-open-topic 0)
      (insert "Local emacs vars.\n")
      (outline-open-topic 1)
      (insert "(`outline-layout' is for allout.el outline-mode)\n")
      (outline-open-topic 0)
      (insert "Local variables:\n")
      (outline-open-topic 0)
      (insert (format "outline-layout: %s\n"
			     (or outline-layout
				 '(-1 : 0))))
      (outline-open-topic 0)
      (insert "End:\n"))))
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
;;;_  > my-region-active-p ()
(defmacro my-region-active-p ()
  (if (fboundp 'region-active-p)
      '(region-active-p)
    'mark-active))
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
;;;_  : my-mark-marker to accomodate divergent emacsen:
(defun my-mark-marker (&optional force buffer)
  "Accomodate the different signature for mark-marker across emacsen.

GNU XEmacs takes two optional args, while mainline GNU Emacs does not,
so pass them along when appropriate."
  (if (string-match " XEmacs " emacs-version)
      (mark-marker force buffer)
    (mark-marker)))

;;;_ #9 Under development
;;;_  > outline-bullet-isearch (&optional bullet)
(defun outline-bullet-isearch (&optional bullet)
  "Isearch \(regexp) for topic with bullet BULLET."
  (interactive)
  (if (not bullet)
      (setq bullet (solicit-char-in-string
		    "ISearch for topic with bullet: "
		    (regexp-sans-escapes outline-bullets-string))))

  (let ((isearch-regexp t)
	(isearch-string (concat "^"
				outline-header-prefix
				"[ \t]*"
				bullet)))
    (isearch-repeat 'forward)
    (isearch-mode t)))
;;;_  ? Re hooking up with isearch - use isearch-op-fun rather than
;;;	wrapping the isearch functions.

;;;_* Local emacs vars.
;;; The following `outline-layout' local variable setting:
;;;  - closes all topics from the first topic to just before the third-to-last,
;;;  - shows the children of the third to last (config vars)
;;;  - and the second to last (code section),
;;;  - and closes the last topic (this local-variables section).
;;;Local variables:
;;;outline-layout: (0 : -1 -1 0)
;;;End:

;;; allout.el ends here
