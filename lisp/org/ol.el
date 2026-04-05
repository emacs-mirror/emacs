;;; ol.el --- Org links library                      -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2026 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten.dominik@gmail.com>
;; Keywords: outlines, hypermedia, calendar, text

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
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

;; This library provides tooling to handle both external and internal
;; links.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-compat)
(require 'org-macs)
(require 'org-fold)

(defvar clean-buffer-list-kill-buffer-names)
(defvar org-agenda-buffer-name)
(defvar org-comment-string)
(defvar org-highlight-links)
(defvar org-id-link-to-org-use-id)
(defvar org-inhibit-startup)
(defvar org-outline-regexp-bol)
(defvar org-src-source-file-name)
(defvar org-ts-regexp)

(declare-function calendar-cursor-to-date "calendar" (&optional error event))
(declare-function dired-get-filename "dired" (&optional localp no-error-if-not-filep))
(declare-function org-back-to-heading "org" (&optional invisible-ok))
(declare-function org-before-first-heading-p "org" ())
(declare-function org-do-occur "org" (regexp &optional cleanup))
(declare-function org-element-at-point "org-element" (&optional pom cached-only))
(declare-function org-element-parse-secondary-string "org-element"
                  (string restriction &optional parent))
(declare-function org-element-restriction "org-element" (element))
(declare-function org-element-cache-refresh "org-element" (pos))
(declare-function org-element-cache-reset "org-element" (&optional all no-persistence))
(declare-function org-element-context "org-element" (&optional element))
(declare-function org-element-lineage "org-element-ast" (datum &optional types with-self))
(declare-function org-element-link-parser "org-element" ())
(declare-function org-element-property "org-element-ast" (property node))
(declare-function org-element-begin "org-element" (node))
(declare-function org-element-end "org-element" (node))
(declare-function org-element-type-p "org-element-ast" (node types))
(declare-function org-element-update-syntax "org-element" ())
(declare-function org-entry-get "org" (pom property &optional inherit literal-nil))
(declare-function org-find-property "org" (property &optional value))
(declare-function org-get-heading "org" (&optional no-tags no-todo no-priority no-comment))
(declare-function org-id-find-id-file "org-id" (id))
(declare-function org-insert-heading "org" (&optional arg invisible-ok top))
(declare-function org-load-modules-maybe "org" (&optional force))
(declare-function org-mark-ring-push "org" (&optional pos buffer))
(declare-function org-mode "org" ())
(declare-function org-occur "org" (regexp &optional keep-previous callback))
(declare-function org-open-file "org" (path &optional in-emacs line search))
(declare-function org-cycle-overview "org-cycle" ())
(declare-function org-restart-font-lock "org" ())
(declare-function org-run-like-in-org-mode "org" (cmd))
(declare-function org-src-coderef-format "org-src" (&optional element))
(declare-function org-src-coderef-regexp "org-src" (fmt &optional label))
(declare-function org-src-edit-buffer-p "org-src" (&optional buffer))
(declare-function org-src-source-buffer "org-src" ())
(declare-function org-src-source-type "org-src" ())
(declare-function org-time-stamp-format "org" (&optional long inactive custom))
(declare-function image-flush "image" (spec &optional frame))
(declare-function org-entry-end-position "org" ())
(declare-function org-element-contents-begin "org-element" (node))
(declare-function org-element-contents-end "org-element" (node))
(declare-function org-property-or-variable-value "org" (var &optional inherit))


;;; Customization

(defgroup org-link nil
  "Options concerning links in Org mode."
  :tag "Org Link"
  :group 'org)

(defcustom org-link-parameters nil
  "Alist of properties that defines all the links in Org mode.

The key in each association is a string of the link type.
Subsequent optional elements make up a property list for that
type.

All properties are optional.  However, the most important ones
are, in this order, `:follow', `:export', and `:store', described
below.

`:follow'

  Function used to follow the link, when the `org-open-at-point'
  command runs on it.  It is called with two arguments: the path,
  as a string, and a universal prefix argument.

  Here, you may use `org-link-open-as-file' helper function for
  types similar to \"file\".

`:export'

  Function that accepts four arguments:
  - the path, as a string,
  - the description as a string, or nil,
  - the export backend,
  - the export communication channel, as a plist.

  When nil, export for that type of link is delegated to the
  backend.

`:store'

  Function responsible for storing the link.  See the function
  `org-store-link-functions' for a description of the expected
  arguments.

Additional properties provide more specific control over the
link.

`:activate-func'

  Function to run at the end of Font Lock activation.  It must
  accept four arguments:
  - the buffer position at the start of the link,
  - the buffer position at its end,
  - the path, as a string,
  - a boolean, non-nil when the link has brackets.

`:complete'

  Function that inserts a link with completion.  The function
  takes one optional prefix argument.

`:insert-description'

  String or function used as a default when prompting users for a
  link's description.  A string is used as-is, a function is
  called with two arguments: the link location (a string such as
  \"~/foobar\", \"id:some-org-id\" or \"https://www.foo.com\")
  and the description generated by `org-insert-link'.  It should
  return the description to use (this reflects the behavior of
  `org-link-make-description-function').  If it returns nil, no
  default description is used, but no error is thrown (from the
  user's perspective, this is equivalent to a default description
  of \"\").

`:display'

  Value for `invisible' text property on the hidden parts of the
  link.  The most useful value is `full', which will not fold the
  link in descriptive display.  Default is `org-link'.

`:face'

  Face for the link, or a function returning a face.  The
  function takes one argument, which is the path.

  The default face is `org-link'.

`:preview'

  Function to run to generate an in-buffer preview for the link.  It
  must accept three arguments:
  - an overlay placed from the start to the end of the link
  - the link path, as a string
  - the syntax node for the link

  This function must return a non-nil value to indicate success.
  A return value of nil implies that the preview failed, and the
  overlay placed on the link will be removed.

`:help-echo'

  String or function used as a value for the `help-echo' text
  property.  The function is called with one argument, the help
  string to display, and should return a string.

`:htmlize-link'

  Function or plist for the `htmlize-link' text property.  The
  function takes no argument.

  Default is (:uri \"type:path\")

`:keymap'

  Active keymap when point is on the link.  Default is
  `org-mouse-map'.

`:mouse-face'

  Face used when hovering over the link.  Default is
  `highlight'."
  :group 'org-link
  :package-version '(Org . "9.8")
  :type '(alist :tag "Link display parameters"
		:value-type plist)
  :risky t)

(defun org-link--set-link-display (symbol value)
  "Set `org-link-descriptive' (SYMBOL) to VALUE.
Also, ensure that links are updated in current buffer.

This function is intended to be used as a :set function."
  (set symbol value)
  (dolist (buf (org-buffer-list))
    (with-current-buffer buf
      (org-restart-font-lock))))

(defcustom org-link-descriptive t
  "Non-nil means Org displays descriptive links.

E.g. [[https://orgmode.org][Org website]] is displayed as
\"Org Website\", hiding the link itself and just displaying its
description.  When set to nil, Org displays the full links
literally.

You can interactively set the value of this variable by calling
`org-toggle-link-display' or from the \"Org > Hyperlinks\" menu."
  :group 'org-link
  :set #'org-link--set-link-display
  :type 'boolean
  :safe #'booleanp)

(defcustom org-link-make-description-function nil
  "Function to use for generating link descriptions from links.
This function must take two parameters: the first one is the
link, the second one is the description generated by
`org-insert-link'.  The function should return the description to
use.  If it returns nil, no default description is used, but no
error is thrown (from the user’s perspective, this is equivalent
to a default description of \"\")."
  :group 'org-link
  :type '(choice (const nil) (function))
  :safe #'null)

(defcustom org-link-file-path-type 'adaptive
  "How the path name in file links should be stored.
Valid values are:

relative  Relative to the current directory, i.e. the directory of the file
          into which the link is being inserted.
absolute  Absolute path, if possible with ~ for home directory.
noabbrev  Absolute path, no abbreviation of home directory.
adaptive  Use relative path for files in the current directory and sub-
          directories of it.  For other files, use an absolute path.

Alternatively, users may supply a custom function that takes the
filename in the link as an argument and returns the path."
  :group 'org-link
  :type '(choice
	  (const relative)
	  (const absolute)
	  (const noabbrev)
	  (const adaptive)
	  (function))
  :package-version '(Org . "9.5")
  :safe #'symbolp)

(defcustom org-link-abbrev-alist nil
  "Alist of link abbreviations.
The car of each element is a string, to be replaced at the start of a link.
The cdrs are replacement values, like (\"linkkey\" . REPLACE).  Abbreviated
links in Org buffers can have an optional tag after a double colon, e.g.,

     [[linkkey:tag][description]]

The `linkkey' must be a single word, starting with a letter, followed
by letters, numbers, `-' or `_'.

If REPLACE is a string, the tag will simply be appended to create the link.
If the string contains \"%s\", the tag will be inserted there.  If the string
contains \"%h\", it will cause a url-encoded version of the tag to be inserted
at that point (see the function `url-hexify-string').  If the string contains
the specifier \"%(my-function)\", then the custom function `my-function' will
be invoked: this function takes the tag as its only argument and must return
a string.

REPLACE may also be a function that will be called with the tag as the
only argument to create the link, which should be returned as a string.

See the manual for examples."
  :group 'org-link
  :type '(repeat
	  (cons (string :tag "Protocol")
		(choice
		 (string :tag "Format")
		 (function))))
  :safe (lambda (alist)
          (when (listp alist)
            (catch :unsafe
              (dolist (val alist)
	        (pcase val
	          (`(,(pred stringp) . ,(pred stringp)) t)
	          (_ (throw :unsafe nil))))
              t))))

(defgroup org-link-follow nil
  "Options concerning following links in Org mode."
  :tag "Org Follow Link"
  :group 'org-link)

(defcustom org-link-translation-function nil
  "Function to translate links with different syntax to Org syntax.
This can be used to translate links created for example by the Planner
or emacs-wiki packages to Org syntax.
The function must accept two parameters, a TYPE containing the link
protocol name like \"rmail\" or \"gnus\" as a string, and the linked path,
which is everything after the link protocol.  It should return a cons
with possibly modified values of type and path."
  :group 'org-link-follow
  :type '(choice (const nil) (function))
  :safe #'null)

(defcustom org-link-frame-setup
  '((vm . vm-visit-folder-other-frame)
    (vm-imap . vm-visit-imap-folder-other-frame)
    (gnus . org-gnus-no-new-news)
    (file . find-file-other-window)
    (wl . wl-other-frame))
  "Setup the frame configuration for following links.
When following a link with Emacs, it may often be useful to display
this link in another window or frame.  This variable can be used to
set this up for the different types of links.
For VM, use any of
    `vm-visit-folder'
    `vm-visit-folder-other-window'
    `vm-visit-folder-other-frame'
For Gnus, use any of
    `gnus'
    `gnus-other-frame'
    `org-gnus-no-new-news'
    `org-gnus-no-new-news-other-frame'
For FILE, use any of
    `find-file'
    `find-file-other-window'
    `find-file-other-frame'
For Wanderlust use any of
    `wl'
    `wl-other-frame'
For the calendar, use the variable `calendar-setup'.
For BBDB, it is currently only possible to display the matches in
another window."
  :group 'org-link-follow
  :package-version  '(Org . "9.8")
  :type '(list
	  (cons (const vm)
		(choice
		 (const vm-visit-folder)
		 (const vm-visit-folder-other-window)
		 (const vm-visit-folder-other-frame)))
	  (cons (const vm-imap)
		(choice
		 (const vm-visit-imap-folder)
		 (const vm-visit-imap-folder-other-window)
		 (const vm-visit-imap-folder-other-frame)))
	  (cons (const gnus)
		(choice
		 (const gnus)
		 (const gnus-other-frame)
		 (const org-gnus-no-new-news)
                 (const org-gnus-no-new-news-other-frame)))
	  (cons (const file)
		(choice
		 (const find-file)
		 (const find-file-other-window)
		 (const find-file-other-frame)))
	  (cons (const wl)
		(choice
		 (const wl)
		 (const wl-other-frame))))
  :risky t)

(defcustom org-link-search-must-match-exact-headline 'query-to-create
  "Control fuzzy link behavior when specific matches not found.

When nil, if a fuzzy link does not match a more specific
target (such as a heading, named block, target, or code ref),
attempt a regular text search.  When set to the special value
`query-to-create', offer to create a new heading matching the
link instead.  Otherwise, signal an error rather than attempting
a regular text search.

This option only affects behavior in Org buffers.  Spaces and
statistics cookies are ignored during heading searches."
  :group 'org-link-follow
  :version "24.1"
  :type '(choice
	  (const :tag "Use fuzzy text search" nil)
	  (const :tag "Match only exact headline" t)
	  (const :tag "Match exact headline or query to create it"
		 query-to-create))
  :safe #'symbolp)

(defcustom org-link-use-indirect-buffer-for-internals nil
  "Non-nil means use indirect buffer to display infile links.
Activating internal links (from one location in a file to another location
in the same file) normally just jumps to the location.  When the link is
activated with a `\\[universal-argument]' prefix (or with mouse-3), the link \
is displayed in
another window.  When this option is set, the other window actually displays
an indirect buffer clone of the current buffer, to avoid any visibility
changes to the current buffer."
  :group 'org-link-follow
  :type 'boolean
  :safe #'booleanp)

(defcustom org-link-shell-confirm-function 'yes-or-no-p
  "Non-nil means ask for confirmation before executing shell links.

Shell links can be dangerous: just think about a link

     [[shell:rm -rf ~/*][Web Search]]

This link would show up in your Org document as \"Web Search\",
but really it would remove your entire home directory.
Therefore we advise against setting this variable to nil.
Just change it to `y-or-n-p' if you want to confirm with a
single keystroke rather than having to type \"yes\"."
  :group 'org-link-follow
  :type '(choice
	  (const :tag "with yes-or-no (safer)" yes-or-no-p)
	  (const :tag "with y-or-n (faster)" y-or-n-p)
	  (const :tag "no confirmation (dangerous)" nil)))

(defcustom org-link-shell-skip-confirm-regexp ""
  "Regexp to skip confirmation for shell links."
  :group 'org-link-follow
  :version "24.1"
  :type 'regexp)

(defcustom org-link-elisp-confirm-function 'yes-or-no-p
  "Non-nil means ask for confirmation before executing Emacs Lisp links.
Elisp links can be dangerous: just think about a link

     [[elisp:(shell-command \"rm -rf ~/*\")][Web Search]]

This link would show up in your Org document as \"Web Search\",
but really it would remove your entire home directory.
Therefore we advise against setting this variable to nil.
Just change it to `y-or-n-p' if you want to confirm with a
single keystroke rather than having to type \"yes\"."
  :group 'org-link-follow
  :type '(choice
	  (const :tag "with yes-or-no (safer)" yes-or-no-p)
	  (const :tag "with y-or-n (faster)" y-or-n-p)
	  (const :tag "no confirmation (dangerous)" nil)))

(defcustom org-link-elisp-skip-confirm-regexp ""
  "A regexp to skip confirmation for Elisp links."
  :group 'org-link-follow
  :version "24.1"
  :type 'regexp)

(defgroup org-link-store nil
  "Options concerning storing links in Org mode."
  :tag "Org Store Link"
  :group 'org-link)

(defcustom org-link-context-for-files t
  "Non-nil means file links from `org-store-link' contain context.
\\<org-mode-map>
A search string is added to the file name with \"::\" as separator
and used to find the context when the link is activated by the command
`org-open-at-point'.  When this option is t, the entire active region
is be placed in the search string of the file link.  If set to a
positive integer N, only the first N lines of context are stored.

Using a prefix argument to the command `org-store-link' \
\(`\\[universal-argument] \\[org-store-link]')
negates this setting for the duration of the command."
  :group 'org-link-store
  :type '(choice boolean integer)
  :safe (lambda (val) (or (booleanp val) (integerp val))))

(defcustom org-link-email-description-format "Email %c: %s"
  "Format of the description part of a link to an email or Usenet message.
The following %-escapes will be replaced by corresponding information:

%F   full \"From\" field
%f   name, taken from \"From\" field, address if no name
%T   full \"To\" field
%t   first name in \"To\" field, address if no name
%c   correspondent.  Usually \"from NAME\", but if you sent it yourself, it
     will be \"to NAME\".  See also the variable `org-link-from-user-regexp'.
%s   subject
%d   date
%m   message-id.

You may use normal field width specification between the % and the letter.
This is for example useful to limit the length of the subject.

Examples: \"%f on: %.30s\", \"Email from %f\", \"Email %c\""
  :group 'org-link-store
  :package-version '(Org . "9.3")
  :type 'string
  :safe #'stringp)

(defcustom org-link-from-user-regexp
  (let ((mail (and (org-string-nw-p user-mail-address)
		   (format "\\<%s\\>" (regexp-quote user-mail-address))))
	(name (and (org-string-nw-p user-full-name)
		   (format "\\<%s\\>" (regexp-quote user-full-name)))))
    (if (and mail name) (concat mail "\\|" name) (or mail name)))
  "Regexp matched against the \"From:\" header of an email or Usenet message.
It should match if the message is from the user him/herself."
  :group 'org-link-store
  :type 'regexp
  :safe #'stringp)

(defcustom org-link-keep-stored-after-insertion nil
  "Non-nil means keep link in list for entire session.
\\<org-mode-map>
The command `org-store-link' adds a link pointing to the current
location to an internal list.  These links accumulate during a session.
The command `org-insert-link' can be used to insert links into any
Org file (offering completion for all stored links).

When this option is nil, every link which has been inserted once using
`\\[org-insert-link]' will be removed from the list, to make completing the \
unused
links more efficient."
  :group 'org-link-store
  :type 'boolean
  :safe #'booleanp)

(defcustom org-link-preview-delay 0.05
  "Idle delay in seconds between link previews when using `org-link-preview'.
Links are previewed in batches (see
`org-link-preview-batch-size') spaced out by this delay.  Set
this to a small number for more immediate previews, but at the
expense of higher lag."
  :group 'org-link
  :package-version '(Org . "9.8")
  :type 'number
  :safe #'numberp)

(defcustom org-link-preview-batch-size 6
  "Number of links that are previewed at once with `org-link-preview'.
Links are previewed asynchronously, in
batches spaced out in time (see `org-link-preview-delay').  Set
this to a large integer for more immediate previews, but at the
expense of higher lag."
  :group 'org-link
  :package-version '(Org . "9.8")
  :type 'natnum
  :safe #'natnump)

(defcustom org-display-remote-inline-images 'skip
  "How to display remote inline images.
Possible values of this option are:

skip        Don't display remote images.
download    Always download and display remote images.
t
cache       Display remote images, and open them in separate buffers
            for caching.  Silently update the image buffer when a file
            change is detected."
  :group 'org-appearance
  :package-version '(Org . "9.7")
  :type '(choice
	  (const :tag "Ignore remote images" skip)
	  (const :tag "Always display remote images" download)
	  (const :tag "Display and silently update remote images" cache))
  :safe #'symbolp)

(defcustom org-image-max-width 'fill-column
  "When non-nil, limit the displayed image width.
This setting only takes effect when `org-image-actual-width' is set to
t or when #+ATTR* is set to t.

Possible values:
- `fill-column' :: limit width to `fill-column'
- `window'      :: limit width to window width
- integer       :: limit width to number in pixels
- float         :: limit width to that fraction of window width
- nil             :: do not limit image width"
  :group 'org-appearance
  :package-version '(Org . "9.7")
  :type '(choice
          (const :tag "Do not limit image width" nil)
          (const :tag "Limit to `fill-column'" fill-column)
          (const :tag "Limit to window width" window)
          (integer :tag "Limit to a number of pixels")
          (float :tag "Limit to a fraction of window width"))
  :safe (lambda (x) (or (numberp x) (member x '(nil 'fill-column 'window)))))

(defcustom org-image-align 'left
  "How to align images previewed using `org-link-preview-region'.

Only stand-alone image links are affected by this setting.  These
are links without surrounding text.

Possible values of this option are:

left     Insert image at specified position.
center   Center image previews.
right    Right-align image previews."
  :group 'org-appearance
  :package-version '(Org . "9.7")
  :type '(choice
          (const :tag "Left align (or don\\='t align) image previews" left)
	  (const :tag "Center image previews" center)
	  (const :tag "Right align image previews" right))
  :safe #'symbolp)

;;; Public variables

(defconst org-target-regexp (let ((border "[^<>\n\r \t]"))
			      (format "<<\\(%s\\|%s[^<>\n\r]*%s\\)>>"
				      border border border))
  "Regular expression matching a link target.")

(defconst org-radio-target-regexp (format "<%s>" org-target-regexp)
  "Regular expression matching a radio target.")

(defvar-local org-target-link-regexp nil
  "Regular expression matching radio targets in plain text.")
(defconst org-target-link-regexp-limit (ash 2 12)
  "Maximum allowed length of regexp.
The number should generally be ~order of magnitude smaller than
MAX_BUF_SIZE in src/regex-emacs.c.  The number of regexp-emacs.c is
for processed regexp, which appears to be larger compared to the
original string length.")
(defvar-local org-target-link-regexps nil
  "List of regular expressions matching radio targets in plain text.
This list is non-nil, when a single regexp would be too long to match
all the possible targets, exceeding Emacs's regexp length limit.")

(defvar org-link-types-re nil
  "Matches a link that has a url-like prefix like \"http:\".")

(defvar org-link-angle-re nil
  "Matches link with angular brackets, spaces are allowed.")

(defvar org-link-plain-re nil
  "Matches plain link, without spaces.
Group 1 must contain the link type (i.e. https).
Group 2 must contain the link path (i.e. //example.com).
Used by `org-element-link-parser'.")

(defvar org-link-bracket-re nil
  "Matches a link in double brackets.")

(defvar org-link-any-re nil
  "Regular expression matching any link.")

(defvar-local org-link-abbrev-alist-local nil
  "Buffer-local version of `org-link-abbrev-alist', which see.
The value of this is taken from the LINK keywords.")

(defvar org-stored-links nil
  "Contains the links stored with `org-store-link'.")

(defvar org-store-link-plist nil
  "Plist with info about the most recently link created with `org-store-link'.")

(defvar org-create-file-search-functions nil
  "List of functions to construct the right search string for a file link.

These functions are called in turn with point at the location to
which the link should point.

A function in the hook should first test if it would like to
handle this file type, for example by checking the `major-mode'
or the file extension.  If it decides not to handle this file, it
should just return nil to give other functions a chance.  If it
does handle the file, it must return the search string to be used
when following the link.  The search string will be part of the
file link, given after a double colon, and `org-open-at-point'
will automatically search for it.  If special measures must be
taken to make the search successful, another function should be
added to the companion hook `org-execute-file-search-functions',
which see.

A function in this hook may also use `org-link-store-props' and set
`:description' property to provide a suggestion for the descriptive
text to be used for this link when it gets inserted into an Org buffer
with \\[org-insert-link].")

(defvar org-execute-file-search-functions nil
  "List of functions to execute a file search triggered by a link.

Functions added to this hook must accept a single argument, the
search string that was part of the file link, the part after the
double colon.  The function must first check if it would like to
handle this search, for example by checking the `major-mode' or
the file extension.  If it decides not to handle this search, it
should just return nil to give other functions a chance.  If it
does handle the search, it must return a non-nil value to keep
other functions from trying.

Each function can access the current prefix argument through the
variable `current-prefix-arg'.  Note that a single prefix is used
to force opening a link in Emacs, so it may be good to only use a
numeric or double prefix to guide the search function.

In case this is needed, a function in this hook can also restore
the window configuration before `org-open-at-point' was called using:

    (set-window-configuration org-window-config-before-follow-link)")

(defvar org-open-link-functions nil
  "Hook for functions finding a plain text link.
These functions must take a single argument, the link content.
They will be called for links that look like [[link text][description]]
when LINK TEXT does not have a protocol like \"http:\" and does not look
like a filename (e.g. \"./blue.png\").

These functions will be called *before* Org attempts to resolve the
link by doing text searches in the current buffer - so if you want a
link \"[[target]]\" to still find \"<<target>>\", your function should
handle this as a special case.

When the function does handle the link, it must return a non-nil value.
If it decides that it is not responsible for this link, it must return
nil to indicate that Org can continue with other options like
exact and fuzzy text search.")


;;; Internal Variables

(defconst org-link--forbidden-chars "]\t\n\r<>"
  "Characters forbidden within a link, as a string.")

(defvar org-link--history nil
  "History for inserted links.")

(defvar org-link--insert-history nil
  "Minibuffer history for links inserted with `org-insert-link'.")

(defvar org-link--search-failed nil
  "Non-nil when last link search failed.")

(defvar-local org-link-preview-overlays nil)
;; Preserve when switching modes or when restarting Org.
;; If we clear the overlay list and later enable Or mode, the existing
;; image overlays will never be cleared by `org-link-preview'
;; and `org-link-preview-clear'.
(put 'org-link-preview-overlays 'permanent-local t)

(defvar-local org-link-preview--timer nil
  "Timer for previewing Org links in buffer.

This timer creates previews for specs in
`org-link-preview--queue'.")

(defvar-local org-link-preview--queue nil
  "Queue of pending previews for Org links in buffer.

Each element of this queue is a list of the form

(PREVIEW-FUNC OVERLAY PATH LINK)

where PREVIEW-FUNC places a preview of PATH using OVERLAY.  LINK
is the Org element being previewed.")


;;; Internal Functions

(defun org-link--try-special-completion (type)
  "If there is completion support for link type TYPE, offer it."
  (let ((fun (org-link-get-parameter type :complete)))
    (if (functionp fun)
	(funcall fun)
      (read-string "Link (no completion support): " (concat type ":")))))

(defun org-link--prettify (link)
  "Return a human-readable representation of LINK.
The car of LINK must be a raw link.  The cdr of LINK must be
either a link description or nil."
  (let ((desc (or (cadr link) "<no description>")))
    (concat (format "%-45s" (substring desc 0 (min (length desc) 40)))
	    "<" (car link) ">")))

(defun org-link--decode-compound (hex)
  "Unhexify Unicode hex-chars HEX.
E.g. \"%C3%B6\" is the German o-Umlaut.  Note: this function also
decodes single byte encodings like \"%E1\" (a-acute) if not
followed by another \"%[A-F0-9]{2}\" group."
  (save-match-data
    (let* ((bytes (cdr (split-string hex "%")))
	   (ret "")
	   (eat 0)
	   (sum 0))
      (while bytes
	(let* ((val (string-to-number (pop bytes) 16))
	       (shift-xor
		(if (= 0 eat)
		    (cond
		     ((>= val 252) (cons 6 252))
		     ((>= val 248) (cons 5 248))
		     ((>= val 240) (cons 4 240))
		     ((>= val 224) (cons 3 224))
		     ((>= val 192) (cons 2 192))
		     (t (cons 0 0)))
		  (cons 6 128))))
	  (when (>= val 192) (setq eat (car shift-xor)))
	  (setq val (logxor val (cdr shift-xor)))
	  (setq sum (+ (ash sum (car shift-xor)) val))
	  (when (> eat 0) (setq eat (- eat 1)))
	  (cond
	   ((= 0 eat)			;multi byte
	    (setq ret (concat ret (char-to-string sum)))
	    (setq sum 0))
	   ((not bytes)			; single byte(s)
	    (setq ret (org-link--decode-single-byte-sequence hex))))))
      ret)))

(defun org-link--decode-single-byte-sequence (hex)
  "Unhexify hex-encoded single byte character sequence HEX."
  (mapconcat (lambda (byte)
	       (char-to-string (string-to-number byte 16)))
	     (cdr (split-string hex "%"))
	     ""))

(defun org-link--fontify-links-to-this-file ()
  "Fontify links to the current file in `org-stored-links'."
  (let ((f (buffer-file-name)) a b)
    (setq a (mapcar (lambda(l)
		      (let ((ll (car l)))
			(when (and (string-match "^file:\\(.+\\)::" ll)
				   (equal f (expand-file-name (match-string 1 ll))))
			  ll)))
		    org-stored-links))
    (when (featurep 'org-id)
      (setq b (mapcar (lambda(l)
			(let ((ll (car l)))
			  (when (and (string-match "^id:\\(.+\\)$" ll)
				     (equal f (expand-file-name
					       (or (org-id-find-id-file
						    (match-string 1 ll)) ""))))
			    ll)))
		      org-stored-links)))
    (mapcar (lambda(l)
	      (put-text-property 0 (length l) 'face 'font-lock-comment-face l))
	    (delq nil (append a b)))))

(defun org-link--buffer-for-internals ()
  "Return buffer used for displaying the target of internal links."
  (cond
   ((not org-link-use-indirect-buffer-for-internals) (current-buffer))
   ((string-suffix-p "(Clone)" (buffer-name))
    (message "Buffer is already a clone, not making another one")
    ;; We also do not modify visibility in this case.
    (current-buffer))
   (t		   ;make a new indirect buffer for displaying the link
    (let* ((indirect-buffer-name (concat (buffer-name) "(Clone)"))
	   (indirect-buffer
	    (or (get-buffer indirect-buffer-name)
		(make-indirect-buffer (current-buffer)
				      indirect-buffer-name
				      'clone))))
      (with-current-buffer indirect-buffer (org-cycle-overview))
      indirect-buffer))))

(defun org-link--search-radio-target (target)
  "Search a radio target matching TARGET in current buffer.
White spaces are not significant."
  (let ((re (format "<<<%s>>>"
		    (mapconcat #'regexp-quote
			       (split-string target)
			       "[ \t]+\\(?:\n[ \t]*\\)?")))
	(origin (point)))
    (goto-char (point-min))
    (catch :radio-match
      (while (re-search-forward re nil t)
	(forward-char -1)
	(let ((object (org-element-context)))
	  (when (org-element-type-p object 'radio-target)
	    (goto-char (org-element-begin object))
	    (org-fold-show-context 'link-search)
	    (throw :radio-match nil))))
      (goto-char origin)
      (user-error "No match for radio target: %s" target))))

(defun org-link--context-from-region ()
  "Return context string from active region, or nil."
  (when (org-region-active-p)
    (let ((context (buffer-substring (region-beginning) (region-end))))
      (when (and (wholenump org-link-context-for-files)
		 (> org-link-context-for-files 0))
	(let ((lines (org-split-string context "\n")))
	  (setq context
		(mapconcat #'identity
			   (cl-subseq lines 0 org-link-context-for-files)
			   "\n"))))
      context)))

(defun org-link--normalize-string (string &optional context)
  "Remove ignored contents from STRING string and return it.
This function removes contiguous white spaces and statistics
cookies.  When optional argument CONTEXT is non-nil, it assumes
STRING is a context string, and also removes special search
syntax around the string."
  (let ((string
	 (org-trim
	  (replace-regexp-in-string
	   (rx (one-or-more (any " \t")))
	   " "
	   (replace-regexp-in-string
	    ;; Statistics cookie regexp.
	    (rx (seq "[" (0+ digit) (or "%" (seq "/" (0+ digit))) "]"))
	    " "
	    string)))))
    (when context
      (while (cond ((and (string-prefix-p "(" string)
			 (string-suffix-p ")" string))
		    (setq string (org-trim (substring string 1 -1))))
		   ((string-match "\\`[#*]+[ \t]*" string)
		    (setq string (substring string (match-end 0))))
		   (t nil))))
    string))

(defun org-link--reveal-maybe (region _)
  "Reveal folded link in REGION when needed.
This function is intended to be used as :fragile property of a folding
spec."
  (org-with-point-at (car region)
    (not (org-in-regexp org-link-any-re))))

(defun org-link--try-link-store-functions (interactive?)
  "Try storing external links, prompting if more than one is possible.

Each function returned by `org-store-link-functions' is called in
turn.  If multiple functions return non-nil, prompt for which
link should be stored.

Argument INTERACTIVE? indicates whether `org-store-link' was
called interactively and is passed to the link store functions.

Return t when a link has been stored in `org-link-store-props'."
  (let ((results-alist nil))
    (dolist (f (org-store-link-functions))
      (when (condition-case nil
                (funcall f interactive?)
              ;; FIXME: The store function used (< Org 9.7) to accept
              ;; no arguments; provide backward compatibility support
              ;; for them.
              (wrong-number-of-arguments
               (funcall f)))
        ;; FIXME: return value is not link's plist, so we store the
        ;; new value before it is modified.  It would be cleaner to
        ;; ask store link functions to return the plist instead.
        (push (cons f (copy-sequence org-store-link-plist))
              results-alist)))
    (pcase results-alist
      (`nil nil)
      (`((,_ . ,_)) t)	;single choice: nothing to do
      (`((,name . ,_) . ,_)
       ;; Reinstate link plist associated to the chosen
       ;; function.
       (apply #'org-link-store-props
              (cdr (assoc-string
                    (if (not interactive?)
                        (car (nth 0 results-alist))
                      (completing-read
                       (format "Store link with (default %s): " name)
                       (mapcar #'car results-alist)
                       nil t nil nil (symbol-name name)))
                    results-alist)))
       t))))

(defun org-link--add-to-stored-links (link desc)
  "Add LINK to `org-stored-links' with description DESC."
  (cond
   ((not (member (list link desc) org-stored-links))
    (push (list link desc) org-stored-links)
    (message "Stored: %s" (or desc link)))
   ((equal (list link desc) (car org-stored-links))
    (message "This link has already been stored"))
   (t
    (setq org-stored-links
          (delete (list link desc) org-stored-links))
    (push (list link desc) org-stored-links)
    (message "Link moved to front: %s" (or desc link)))))

(defun org-link--file-link-to-here ()
  "Return as (LINK . DESC) a file link with search string to here."
  (let ((link (concat "file:"
                      (abbreviate-file-name
                       (buffer-file-name (buffer-base-buffer)))))
        desc)
    (when org-link-context-for-files
      (pcase (org-link-precise-link-target)
        (`nil nil)
        (`(,search-string ,search-desc ,_position)
         (setq link (format "%s::%s" link search-string))
         (setq desc search-desc))))
    (cons link desc)))

(defun org-link-preview--get-overlays (&optional beg end)
  "Return link preview overlays between BEG and END."
  (let* ((beg (or beg (point-min)))
         (end (or end (point-max)))
         (overlays (overlays-in beg end))
         result)
    (dolist (ov overlays result)
      (when (memq ov org-link-preview-overlays)
        (push ov result)))))

(defun org-link-preview--remove-overlay (ov after _beg _end &optional _len)
  "Remove link-preview overlay OV if a corresponding region is modified.

AFTER is true when this function is called post-change."
  (when (and ov after)
    (setq org-link-preview-overlays (delq ov org-link-preview-overlays))
    ;; Clear image from cache to avoid image not updating upon
    ;; changing on disk.  See Emacs bug#59902.
    (when-let* ((disp (overlay-get ov 'display))
                ((if (fboundp 'imagep)
                     (imagep disp)
                   (eq 'image (car-safe disp)))))
      (image-flush disp))
    (delete-overlay ov)))



;;;; Utilities for image preview display

;; For without-x builds.
(declare-function image-flush "image" (spec &optional frame))

(defun org--create-inline-image (file width)
  "Create image located at FILE, or return nil.
WIDTH is the width of the image.  The image may not be created
according to the value of `org-display-remote-inline-images'."
  (let* ((remote? (file-remote-p file))
	 (file-or-data
	  (pcase org-display-remote-inline-images
	    ((guard (not remote?)) file)
	    (`download (with-temp-buffer
			 (set-buffer-multibyte nil)
			 (insert-file-contents-literally file)
			 (buffer-string)))
	    ((or `cache `t)
             (let ((revert-without-query '(".")))
	       (with-current-buffer (find-file-noselect file)
		 (buffer-string))))
	    (`skip nil)
	    (other
	     (message "Invalid value of `org-display-remote-inline-images': %S"
		      other)
	     nil))))
    (when file-or-data
      (create-image file-or-data
		    (and (image-type-available-p 'imagemagick)
			 width
			 'imagemagick)
		    remote?
		    :width width
                    :max-width
                    (pcase org-image-max-width
                      (`fill-column (* fill-column (frame-char-width (selected-frame))))
                      (`window (window-width nil t))
                      ((pred integerp) org-image-max-width)
                      ((pred floatp) (floor (* org-image-max-width (window-width nil t))))
                      (`nil nil)
                      (_ (error "Unsupported value of `org-image-max-width': %S"
                                org-image-max-width)))
                    :scale 1))))

(declare-function org-export-read-attribute "ox"
                  (attribute element &optional property))
(defvar visual-fill-column-width) ; Silence compiler warning
(defun org-display-inline-image--width (link)
  "Determine the display width of the image LINK, in pixels.
- When `org-image-actual-width' is t, the image's pixel width is used.
- When `org-image-actual-width' is a number, that value will is used.
- When `org-image-actual-width' is nil or a list, :width attribute of
  #+attr_org or the first #+attr_...  (if it exists) is used to set the
  image width.  A width of X% is divided by 100.  If the value is a
  float between 0 and 2, it interpreted as that proportion of the text
  width in the buffer.

  If no :width attribute is given and `org-image-actual-width' is a
  list with a number as the car, then that number is used as the
  default value."
  ;; Apply `org-image-actual-width' specifications.
  ;; Support subtree-level property "ORG-IMAGE-ACTUAL-WIDTH" specified
  ;; width.
  (let ((org-image-actual-width (org-property-or-variable-value 'org-image-actual-width)))
    (cond
     ((eq org-image-actual-width t) nil)
     ((listp org-image-actual-width)
      (require 'ox)
      (let* ((par (org-element-lineage link 'paragraph))
             ;; Try to find an attribute providing a :width.
             ;; #+ATTR_ORG: :width ...
             (attr-width (org-export-read-attribute :attr_org par :width))
             (width-unreadable?
              (lambda (value)
                (or (not (stringp value))
                    (unless (string= value "t")
                      (or (not (string-match
                              (rx bos (opt "+")
                                  (or
                                   ;; Number of pixels
                                   ;; must be a lone number, not
                                   ;; things like 4in
                                   (seq (1+ (in "0-9")) (? "px") eos)
                                   ;; Numbers ending with %
                                   (seq (1+ (in "0-9.")) (group-n 1 "%"))
                                   ;; Fractions
                                   (seq (0+ (in "0-9")) "." (1+ (in "0-9")))))
                              value))
                          (let ((number (string-to-number value)))
                            (and (floatp number)
                                 (not (match-string 1 value)) ; X%
                                 (not (<= 0.0 number 2.0)))))))))
             ;; #+ATTR_BACKEND: :width ...
             (attr-other
              (catch :found
                (org-element-properties-map
                 (lambda (prop _)
                   (when (and
                          (not (eq prop :attr_org))
                          (string-match-p "^:attr_" (symbol-name prop))
                          (not (funcall width-unreadable? (org-export-read-attribute prop par :width))))
                     (throw :found prop)))
                 par)))
             (attr-width
              (if (not (funcall width-unreadable? attr-width))
                  attr-width
                ;; When #+attr_org: does not have readable :width
                (and attr-other
                     (org-export-read-attribute attr-other par :width))))
             (width
              (cond
               ;; Treat :width t as if `org-image-actual-width' were t.
               ((string= attr-width "t") nil)
               ;; Fallback to `org-image-actual-width' if no interprable width is given.
               ((funcall width-unreadable? attr-width)
                (car org-image-actual-width))
               ;; Convert numeric widths to numbers, converting percentages.
               ((string-match-p "\\`[[+]?[0-9.]+%" attr-width)
                (/ (string-to-number attr-width) 100.0))
               (t (string-to-number attr-width)))))
        (if (and (floatp width) (<= 0.0 width 2.0))
            ;; A float in [0,2] should be interpereted as this portion of
            ;; the text width in the window.  This works well with cases like
            ;; #+attr_latex: :width 0.X\{line,page,column,etc.}width,
            ;; as the "0.X" is pulled out as a float.  We use 2 as the upper
            ;; bound as cases such as 1.2\linewidth are feasible.
            (round (* width
                      (window-pixel-width)
                      (/ (or (and (bound-and-true-p visual-fill-column-mode)
                                  (or visual-fill-column-width auto-fill-function))
                             (when auto-fill-function fill-column)
                             (- (window-text-width) (line-number-display-width)))
                         (float (window-total-width)))))
          width)))
     ((numberp org-image-actual-width)
      org-image-actual-width)
     (t nil))))

(defun org-image--align (link)
  "Determine the alignment of the image LINK.
LINK is a link object.

In decreasing order of priority, this is controlled:
- Per image by the value of `:center' or `:align' in the
affiliated keyword `#+attr_org'.
- By the `#+attr_html' or `#+attr_latex` keywords with valid
  `:center' or `:align' values.
- Globally by the user option `org-image-align'.

The result is either nil or one of the strings \"left\",
\"center\" or \"right\".

\"center\" will cause the image preview to be centered, \"right\"
will cause it to be right-aligned.  A value of \"left\" or nil
implies no special alignment."
  (let ((par (org-element-lineage link 'paragraph)))
    ;; Only align when image is not surrounded by paragraph text:
    (when (and par ; when image is not in paragraph, but in table/headline/etc, do not align
               (= (org-element-begin link)
                  (save-excursion
                    (goto-char (org-element-contents-begin par))
                    (skip-chars-forward "\t ")
                    (point)))           ;account for leading space
                                        ;before link
               (<= (- (org-element-contents-end par)
                     (org-element-end link))
                  1))                  ;account for trailing newline
                                        ;at end of paragraph
      (save-match-data
        ;; Look for a valid ":center t" or ":align left|center|right"
        ;; attribute.
        ;;
        ;; An attr_org keyword has the highest priority, with
        ;; any attr.* next.  Choosing between these is
        ;; unspecified.
        (let ((center-re ":\\(center\\)[[:space:]]+t\\b")
              (align-re ":align[[:space:]]+\\(left\\|center\\|right\\)\\b")
              attr-align)
          (catch 'exit
            (org-element-properties-mapc
             (lambda (propname propval)
               (when (and propval
                          (string-match-p ":attr.*" (symbol-name propname)))
                 (setq propval (car-safe propval))
                 (when (or (string-match center-re propval)
                           (string-match align-re propval))
                   (setq attr-align (match-string 1 propval))
                   (when (eq propname :attr_org)
                     (throw 'exit t)))))
             par))
          (if attr-align
              (when (member attr-align '("center" "right")) attr-align)
            ;; No image-specific keyword, check global alignment property
            (when (memq org-image-align '(center right))
              (symbol-name org-image-align))))))))

;;; Public API

(defun org-link-types ()
  "Return a list of known link types."
  (mapcar #'car org-link-parameters))

(defun org-link-get-parameter (type key)
  "Get TYPE link property for KEY.
TYPE is a string and KEY is a plist keyword.  See
`org-link-parameters' for supported keywords."
  (plist-get (cdr (assoc type org-link-parameters))
	     key))

(defun org-link-set-parameters (type &rest parameters)
  "Set link TYPE properties to PARAMETERS.
PARAMETERS should be keyword value pairs.  See
`org-link-parameters' for supported keys."
  (when (member type '("coderef" "custom-id" "fuzzy" "radio"))
    (error "Cannot override reserved link type: %S" type))
  (let ((data (assoc type org-link-parameters)))
    (if data (setcdr data (org-combine-plists (cdr data) parameters))
      (push (cons type parameters) org-link-parameters)
      (org-link-make-regexps)
      (when (featurep 'org-element) (org-element-update-syntax)))))

;; This way, one can add multiple functions as, say, :follow parameter.
;; For example,
;; (add-function :before-until (org-link-get-parameter "id" :follow) #'my-function)
;; See https://orgmode.org/list/a123389c-8f86-4836-a4fe-1e3f4281d33b@app.fastmail.com
(gv-define-setter org-link-get-parameter (value type key)
  `(org-link-set-parameters ,type ,key ,value))

(defun org-link-make-regexps ()
  "Update the link regular expressions.
This should be called after the variable `org-link-parameters' has changed."
  (let ((types-re (regexp-opt (org-link-types) t)))
    (setq org-link-types-re
	  (concat "\\`" types-re ":")
	  org-link-angle-re
	  (format "<%s:\\([^>\n]*\\(?:\n[ \t]*[^> \t\n][^>\n]*\\)*\\)>"
		  types-re)
	  org-link-plain-re
          (let* ((non-space-bracket "[^][ \t\n()<>]")
	         (parenthesis
		  `(seq (any "<([")
		        (0+ (or (regex ,non-space-bracket)
			        (seq (any "<([")
				     (0+ (regex ,non-space-bracket))
				     (any "])>"))))
		        (any "])>"))))
	    ;; Heuristics for an URL link inspired by
	    ;; https://daringfireball.net/2010/07/improved_regex_for_matching_urls
	    (rx-to-string
	     `(seq word-start
                   ;; Link type: match group 1.
		   (regexp ,types-re)
		   ":"
                   ;; Link path: match group 2.
                   (group
		    (1+ (or (regex ,non-space-bracket)
			    ,parenthesis))
		    (or (regexp "[^[:punct:][:space:]\n]")
                        ;; Allow "-" punctuation, as an exception
                        ;; See https://list.orgmode.org/orgmode/87sexh9ddv.fsf@ice9.digital/
                        ;; This is also in line with the heuristics
                        ;; above - it also does not include "-"
                        ;; punctuation.
                        ?-
		        ?/
		        ,parenthesis)))))
          org-link-bracket-re
          (rx (seq "[["
	           ;; URI part: match group 1.
	           (group
	            (one-or-more
                     (or (not (any "[]\\"))
			 (and "\\" (zero-or-more "\\\\") (any "[]"))
			 (and (one-or-more "\\") (not (any "[]"))))))
		   "]"
		   ;; Description (optional): match group 2.
		   (opt "[" (group (+? anything)) "]")
		   "]"))
	  org-link-any-re
	  (concat "\\(" org-link-bracket-re "\\)\\|\\("
		  org-link-angle-re "\\)\\|\\("
		  org-link-plain-re "\\)"))))

(defun org-link-complete-file (&optional arg)
  "Create a file link using completion.
With optional ARG \\='(16), abbreviate the file name in the link."
  (let ((file (read-file-name "File: "))
	(pwd (file-name-as-directory (expand-file-name ".")))
	(pwd1 (file-name-as-directory (abbreviate-file-name
				       (expand-file-name ".")))))
    (cond ((equal arg '(16))
	   (concat "file:"
		   (abbreviate-file-name (expand-file-name file))))
	  ((string-match
	    (concat "^" (regexp-quote pwd1) "\\(.+\\)") file)
	   (concat "file:" (match-string 1 file)))
	  ((string-match
	    (concat "^" (regexp-quote pwd) "\\(.+\\)")
	    (expand-file-name file))
	   (concat "file:"
		   (match-string 1 (expand-file-name file))))
	  (t (concat "file:" file)))))

(defun org-link-email-description (&optional fmt)
  "Return the description part of an email link.
This takes information from `org-store-link-plist' and formats it
according to FMT (default from `org-link-email-description-format')."
  (setq fmt (or fmt org-link-email-description-format))
  (let* ((p org-store-link-plist)
	 (to (plist-get p :toaddress))
	 (from (plist-get p :fromaddress))
	 (table
	  (list
	   (cons "%c" (plist-get p :fromto))
	   (cons "%F" (plist-get p :from))
	   (cons "%f" (or (plist-get p :fromname) (plist-get p :fromaddress) "?"))
	   (cons "%T" (plist-get p :to))
	   (cons "%t" (or (plist-get p :toname) (plist-get p :toaddress) "?"))
	   (cons "%s" (plist-get p :subject))
	   (cons "%d" (plist-get p :date))
	   (cons "%m" (plist-get p :message-id)))))
    (when (string-match "%c" fmt)
      ;; Check if the user wrote this message
      (if (and org-link-from-user-regexp from to
	       (save-match-data (string-match org-link-from-user-regexp from)))
	  (setq fmt (replace-match "to %t" t t fmt))
	(setq fmt (replace-match "from %f" t t fmt))))
    (org-replace-escapes fmt table)))

(defun org-link-store-props (&rest plist)
  "Store link properties PLIST.
The properties are pre-processed by extracting names, addresses
and dates."
  (let ((x (plist-get plist :from)))
    (when x
      (let ((adr (mail-extract-address-components x)))
	(setq plist (plist-put plist :fromname (car adr)))
	(setq plist (plist-put plist :fromaddress (nth 1 adr))))))
  (let ((x (plist-get plist :to)))
    (when x
      (let ((adr (mail-extract-address-components x)))
	(setq plist (plist-put plist :toname (car adr)))
	(setq plist (plist-put plist :toaddress (nth 1 adr))))))
  (let ((x (ignore-errors (date-to-time (plist-get plist :date)))))
    (when x
      (setq plist (plist-put plist :date-timestamp
			     (format-time-string
			      (org-time-stamp-format t) x)))
      (setq plist (plist-put plist :date-timestamp-inactive
			     (format-time-string
			      (org-time-stamp-format t t) x)))))
  (let ((from (plist-get plist :from))
	(to (plist-get plist :to)))
    (when (and from to org-link-from-user-regexp)
      (setq plist
	    (plist-put plist :fromto
		       (if (string-match org-link-from-user-regexp from)
			   (concat "to %t")
			 (concat "from %f"))))))
  (setq org-store-link-plist plist))

(defun org-link-add-props (&rest plist)
  "Add these properties to the link property list PLIST."
  (let (key value)
    (while plist
      (setq key (pop plist) value (pop plist))
      (setq org-store-link-plist
	    (plist-put org-store-link-plist key value)))))

(defun org-link-encode (text table)
  "Return percent escaped representation of string TEXT.
TEXT is a string with the text to escape.  TABLE is a list of
characters that should be escaped."
  (mapconcat
   (lambda (c)
     (if (memq c table)
	 (mapconcat (lambda (e) (format "%%%.2X" e))
		    (or (encode-coding-char c 'utf-8)
			(error "Unable to percent escape character: %c" c))
		    "")
       (char-to-string c)))
   text ""))

(defun org-link-decode (s)
  "Decode percent-encoded parts in string S.
E.g. \"%C3%B6\" becomes the German o-Umlaut."
  (replace-regexp-in-string "\\(%[0-9A-Za-z]\\{2\\}\\)+"
			    #'org-link--decode-compound s t t))

(defun org-link-escape (link)
  "Backslash-escape sensitive characters in string LINK."
  (replace-regexp-in-string
   (rx (seq (group (zero-or-more "\\")) (group (or string-end (any "[]")))))
   (lambda (m)
     (concat (match-string 1 m)
	     (match-string 1 m)
	     (and (/= (match-beginning 2) (match-end 2)) "\\")))
   link nil t 1))

(defun org-link-unescape (link)
  "Remove escaping backslash characters from string LINK."
  (replace-regexp-in-string
   (rx (group (one-or-more "\\")) (or string-end (any "[]")))
   (lambda (_)
     (concat (make-string (/ (- (match-end 1) (match-beginning 1)) 2) ?\\)))
   link nil t 1))

(defun org-link-get-description (link &optional default-description)
  "Return description for LINK.
When link has :insert-description link property defined, set description
accordingly.  Otherwise, try `org-link-make-description-function'.
When the above two methods are not defined, return DEFAULT-DESCRIPTION."
  (let* ((abbrevs org-link-abbrev-alist-local)
         (all-prefixes (append (mapcar #'car abbrevs)
			       (mapcar #'car org-link-abbrev-alist)
			       (org-link-types)))
         (type
          (cond
           ((and all-prefixes
                 (string-match (rx-to-string `(: string-start (submatch (or ,@all-prefixes)) ":")) link))
            (match-string 1 link))
           ((file-name-absolute-p link) "file")
           ((string-match "\\`\\.\\.?/" link) "file"))))
    (cond
     ((org-link-get-parameter type :insert-description)
      (let ((def (org-link-get-parameter type :insert-description)))
        (condition-case nil
            (cond
             ((stringp def) def)
             ((functionp def)
              (funcall def link default-description)))
          (error
           (message "Can't get link description from org link parameter `:insert-description': %S"
                    def)
           (sit-for 2)
           nil))))
     (org-link-make-description-function
      (condition-case nil
          (funcall org-link-make-description-function link default-description)
        (error
         (message "Can't get link description from %S"
                  org-link-make-description-function)
         (sit-for 2)
         nil)))
     (t default-description))))

(defun org-link-make-string (link &optional description)
  "Make a bracket link, consisting of LINK and DESCRIPTION.
LINK is escaped with backslashes for inclusion in buffer."
  (let* ((zero-width-space (string ?\x200B))
	 (description
	  (and (org-string-nw-p description)
	       ;; Description cannot contain two consecutive square
	       ;; brackets, or end with a square bracket.  To prevent
	       ;; this, insert a zero width space character between
	       ;; the brackets, or at the end of the description.
	       (replace-regexp-in-string
		"\\(]\\)\\(]\\)"
		(concat "\\1" zero-width-space "\\2")
		(replace-regexp-in-string "]\\'"
					  (concat "\\&" zero-width-space)
					  (org-trim description))))))
    (if (not (org-string-nw-p link))
        (or description
            (error "Empty link"))
      (format "[[%s]%s]"
	      (org-link-escape link)
	      (if description (format "[%s]" description) "")))))

(defun org-link-make-string-for-buffer (link &optional description interactive-p)
  "Format LINK with DESCRIPTION for current buffer.
When BUFFER is nil, format LINK for current buffer.
When DESCRIPTION is nil, set it according to :insert-description link
parameter or `org-link-make-description-function'.
When current buffer is a file buffer and link points to that file,
strip the file name from the link.
Make sure that file: link follows `org-link-file-path-type'.
Strip <..> brackets from LINK, if it is a plain link.

When INTERACTIVE-P is non-nil, prompt user for description.

Return link with description, as a string."
  (when (and (string-match org-link-plain-re link)
	     (not (string-match org-ts-regexp link)))
    ;; URL-like link, normalize the use of angular brackets.
    (setq link (org-unbracket-string "<" ">" link)))

  ;; Check if we are linking to the current file with a search
  ;; option If yes, simplify the link by using only the search
  ;; option.
  (when (and (buffer-file-name (buffer-base-buffer))
	     (let ((case-fold-search nil))
	       (string-match "\\`file:\\(.+?\\)::" link)))
    (let ((path (match-string-no-properties 1 link))
	  (search (substring-no-properties link (match-end 0))))
      (save-match-data
	(when (equal (file-truename (buffer-file-name (buffer-base-buffer)))
		     (file-truename path))
	  ;; We are linking to this same file, with a search option
	  (setq link search)))))

  ;; Check if we can/should use a relative path.  If yes, simplify
  ;; the link.
  (let ((case-fold-search nil))
    (when (string-match "\\`\\(file\\|docview\\):" link)
      (let* ((type (match-string-no-properties 0 link))
	     (path-start (match-end 0))
	     (search (and (string-match "::\\(.*\\)\\'" link path-start)
			  (match-string 1 link)))
	     (path
	      (if search
		  (substring-no-properties
		   link path-start (match-beginning 0))
		(substring-no-properties link (match-end 0))))
             ;; file:::search
             (path (if (org-string-nw-p path) path (buffer-file-name)))
	     (origpath path))
	(setq path (org-link--normalize-filename path org-link-file-path-type))
	(setq link (concat type path (and search (concat "::" search))))
	(when (equal description origpath)
	  (setq description path)))))

  (unless description
    (setq description (org-link-get-description link)))

  (when interactive-p
    (setq description (read-string "Description: " description)))

  (unless (org-string-nw-p description)
    (setq description nil))

  (org-link-make-string link description))

(defun org-store-link-functions ()
  "List of functions that are called to create and store a link.

The functions are defined in the `:store' property of
`org-link-parameters'.  Each function should accept an argument
INTERACTIVE? which indicates whether the user has initiated
`org-store-link' interactively.

Each function will be called in turn with a single argument
INTERACTIVE? - non-nil when user interaction is allowed.  Each function
should check if it is responsible for creating this link (for example
by looking at the major mode).  If not, it must return nil.  If yes,
it should return a non-nil value after calling `org-link-store-props'
with a list of properties and values.  Special properties are:

:type         The link prefix, like \"http\".  This must be given.
:link         The link, like \"http://www.astro.uva.nl/~dominik\".
              This is obligatory as well.
:description  Optional default description for the second pair
              of brackets in an Org mode link.  The user can still change
              this when inserting this link into an Org mode buffer.

In addition to these, any additional properties can be specified
and then used in capture templates."
  (cl-loop for link in org-link-parameters
	   with store-func
	   do (setq store-func (org-link-get-parameter (car link) :store))
	   if store-func
	   collect store-func))

(defun org-link-expand-abbrev (link)
  "Replace link abbreviations in LINK string.
Abbreviations are defined in `org-link-abbrev-alist'."
  (if (not (string-match "^\\([^:]*\\)\\(::?\\(.*\\)\\)?$" link)) link
    (let* ((key (match-string 1 link))
	   (as (or (assoc key org-link-abbrev-alist-local)
		   (assoc key org-link-abbrev-alist)))
	   (tag (and (match-end 2) (match-string 3 link)))
	   rpl)
      (if (not as)
	  link
	(setq rpl (cdr as))
        (cl-macrolet
            ((eval-or-disable (&rest body)
               "Run BODY and disable AS abbrev if it errs."
               `(condition-case err
	            (progn ,@body)
                  (error
                   (org-display-warning
                    (format "Disabling link abbrev %s <- %s after expansion failure: %S"
                            rpl link (error-message-string err)))
                   (setq org-link-abbrev-alist-local (delete as org-link-abbrev-alist-local)
	                 org-link-abbrev-alist (delete as org-link-abbrev-alist))
                   link))))
          ;; Drop any potentially dangerous text properties like
          ;; `modification-hooks' that may be used as an attack vector.
          (substring-no-properties
           (cond
            ((symbolp rpl)
             (eval-or-disable
              (let ((expanded (funcall rpl tag)))
                (unless (stringp expanded)
                  (error "%s did not return a string: %S" rpl expanded))
                expanded)))
            ((string-match "%(\\([^)]+\\))" rpl)
             (let ((rpl-fun-symbol (intern-soft (match-string 1 rpl))))
               ;; Using `unsafep-function' is not quite enough because
               ;; Emacs considers functions like `getenv' safe, while
               ;; they can potentially be used to expose private system
               ;; data to attacker if abbreviated link is clicked.
               (if (or (eq t (get rpl-fun-symbol 'org-link-abbrev-safe))
                       (eq t (get rpl-fun-symbol 'pure)))
                   (eval-or-disable
                    (replace-match
                     (save-match-data
                       (funcall rpl-fun-symbol tag))
                     t t rpl))
                 (org-display-warning
                  (format "Disabling unsafe link abbrev: %s
You may mark function safe via (put '%s 'org-link-abbrev-safe t)"
                          rpl (match-string 1 rpl)))
                 (setq org-link-abbrev-alist-local (delete as org-link-abbrev-alist-local)
                       org-link-abbrev-alist (delete as org-link-abbrev-alist))
                 link
                 )))
            ((string-match "%s" rpl) (replace-match (or tag "") t t rpl))
            ((string-match "%h" rpl)
             (replace-match (url-hexify-string (or tag "")) t t rpl))
            (t (concat rpl tag)))))))))

(defun org-link-open (link &optional arg)
  "Open a link object LINK.

ARG is an optional prefix argument.  Some link types may handle
it.  For example, it determines what application to run when
opening a \"file\" link.

Functions responsible for opening the link are either hard-coded
for internal and \"file\" links, or stored as a parameter in
`org-link-parameters', which see."
  (let ((type (org-element-property :type link))
	(path (org-element-property :path link)))
    (pcase type
      ;; Opening a "file" link requires special treatment since we
      ;; first need to integrate search option, if any.
      ("file"
       (let* ((option (org-element-property :search-option link))
	      (path (if option (concat path "::" option) path)))
	 (org-link-open-as-file path
				(pcase (org-element-property :application link)
				  ((guard arg) arg)
				  ("emacs" 'emacs)
				  ("sys" 'system)))))
      ;; Internal links.
      ((or "coderef" "custom-id" "fuzzy" "radio")
       (unless (run-hook-with-args-until-success 'org-open-link-functions path)
	 (if (not arg) (org-mark-ring-push)
	   (switch-to-buffer-other-window (org-link--buffer-for-internals)))
	 (let ((destination
		(org-with-wide-buffer
		 (if (equal type "radio")
		     (org-link--search-radio-target path)
		   (org-link-search
		    (pcase type
		      ("custom-id" (concat "#" path))
		      ("coderef" (format "(%s)" path))
		      (_ path))
		    ;; Prevent fuzzy links from matching themselves.
		    (and (equal type "fuzzy")
			 (+ 2 (org-element-begin link)))))
		 (point))))
	   (unless (and (<= (point-min) destination)
			(>= (point-max) destination))
	     (widen))
	   (goto-char destination))))
      (_
       ;; Look for a dedicated "follow" function in custom links.
       (let ((f (org-link-get-parameter type :follow)))
	 (when (functionp f)
	   ;; Function defined in `:follow' parameter may use a single
	   ;; argument, as it was mandatory before Org 9.4.  This is
	   ;; deprecated, but support it for now.
	   (condition-case nil
	       (funcall f path arg)
	     (wrong-number-of-arguments
	      (funcall f path)))))))))

(defun org-link-open-from-string (s &optional arg)
  "Open a link in the string S, as if it was in Org mode.
Optional argument ARG is passed to `org-open-file' when S is a
\"file\" link."
  (interactive "sLink: \nP")
  (pcase (with-temp-buffer
	   (let ((org-inhibit-startup nil))
	     (insert s)
	     (org-mode)
	     (goto-char (point-min))
	     (org-element-link-parser)))
    (`nil (user-error "No valid link in %S" s))
    ((and link (guard (not (equal (org-element-end link) (1+ (length s))))))
     (user-error "Garbage after link in %S (%S)"
                 s (substring s (1- (org-element-end link)))))
    (link (org-link-open link arg))))

(defun org-link-search (s &optional avoid-pos stealth new-heading-container)
  "Search for a search string S in the accessible part of the buffer.

If S starts with \"#\", it triggers a custom ID search.

If S is enclosed within parenthesis, it initiates a coderef
search.

If S is surrounded by forward slashes, it is interpreted as
a regular expression.  In Org mode files, this will create an
`org-occur' sparse tree.  In ordinary files, `occur' will be used
to list matches.  If the current buffer is in `dired-mode', grep
will be used to search in all files.

When AVOID-POS is given, ignore matches near that position.

When optional argument STEALTH is non-nil, do not modify
visibility around point, thus ignoring `org-fold-show-context-detail'
variable.

When optional argument NEW-HEADING-CONTAINER is an element, any
new heading that is created (see
`org-link-search-must-match-exact-headline') will be added as a
subheading of NEW-HEADING-CONTAINER.  Otherwise, new headings are
created at level 1 at the end of the accessible part of the
buffer.

Search is case-insensitive and ignores white spaces.  Return type
of matched result, which is either `dedicated' or `fuzzy'.  Search
respects buffer narrowing."
  (unless (org-string-nw-p s) (error "Invalid search string \"%s\"" s))
  (let* ((case-fold-search t)
	 (origin (point))
	 (normalized (replace-regexp-in-string "\n[ \t]*" " " s))
	 (starred (eq (string-to-char normalized) ?*))
	 (words (split-string (if starred (substring s 1) s)))
	 (s-multi-re (mapconcat #'regexp-quote words "\\(?:[ \t\n]+\\)"))
	 (s-single-re (mapconcat #'regexp-quote words "[ \t]+"))
	 type)
    (cond
     ;; Check if there are any special search functions.
     ((run-hook-with-args-until-success 'org-execute-file-search-functions s))
     ((eq (string-to-char s) ?#)
      ;; Look for a custom ID S if S starts with "#".
      (let* ((id (substring normalized 1))
	     (match (org-find-property "CUSTOM_ID" id)))
	(if match (progn (goto-char match) (setf type 'dedicated))
	  (error "No match for custom ID: %s" id))))
     ((string-match "\\`(\\(.*\\))\\'" normalized)
      ;; Look for coderef targets if S is enclosed within parenthesis.
      (let ((coderef (match-string-no-properties 1 normalized))
	    (re (substring s-single-re 1 -1)))
	(goto-char (point-min))
	(catch :coderef-match
	  (while (re-search-forward re nil t)
	    (let ((element (org-element-at-point)))
	      (when (and (org-element-type-p element '(example-block src-block))
			 (org-match-line
			  (concat ".*?" (org-src-coderef-regexp
					 (org-src-coderef-format element)
					 coderef))))
		(setq type 'dedicated)
		(goto-char (match-beginning 2))
		(throw :coderef-match nil))))
	  (goto-char origin)
	  (error "No match for coderef: %s" coderef))))
     ((string-match "\\`/\\(.*\\)/\\'" normalized)
      ;; Look for a regular expression.
      (funcall (if (derived-mode-p 'org-mode) #'org-occur #'org-do-occur)
	       (match-string 1 s)))
     ;; From here, we handle fuzzy links.
     ;;
     ;; Look for targets, only if not in a headline search.
     ((and (not starred)
	   (let ((target (format "<<%s>>" s-multi-re)))
	     (catch :target-match
	       (goto-char (point-min))
	       (while (re-search-forward target nil t)
		 (backward-char)
		 (let ((context (org-element-context)))
		   (when (org-element-type-p context 'target)
		     (setq type 'dedicated)
		     (goto-char (org-element-begin context))
		     (throw :target-match t))))
	       nil))))
     ;; Look for elements named after S, only if not in a headline
     ;; search.
     ((and (not starred)
	   (let ((name (format "^[ \t]*#\\+NAME: +%s[ \t]*$" s-single-re)))
	     (catch :name-match
	       (goto-char (point-min))
	       (while (re-search-forward name nil t)
		 (let* ((element (org-element-at-point))
			(name (org-element-property :name element)))
		   (when (and name (equal (mapcar #'upcase words) (mapcar #'upcase (split-string name))))
		     (setq type 'dedicated)
		     (forward-line 0)
		     (throw :name-match t))))
	       nil))))
     ;; Regular text search.  Prefer headlines in Org mode buffers.
     ;; Ignore COMMENT keyword, TODO keywords, priority cookies,
     ;; statistics cookies and tags.
     ((and (derived-mode-p 'org-mode)
	   (let ((title-re
		  (format "%s.*\\(?:%s[ \t]\\)?.*%s"
			  org-outline-regexp-bol
			  org-comment-string
			  (mapconcat #'regexp-quote words ".+"))))
	     (goto-char (point-min))
	     (catch :found
	       (while (re-search-forward title-re nil t)
		 (when (equal (mapcar #'upcase words)
                              (mapcar #'upcase
			              (split-string
			               (org-link--normalize-string
				        (org-get-heading t t t t)))))
		   (throw :found t)))
	       nil)))
      (forward-line 0)
      (setq type 'dedicated))
     ;; Offer to create non-existent headline depending on
     ;; `org-link-search-must-match-exact-headline'.
     ((and (derived-mode-p 'org-mode)
	   (eq org-link-search-must-match-exact-headline 'query-to-create)
	   (yes-or-no-p "No match - create this as a new heading? "))
      (let* ((container-ok (and new-heading-container
                                (org-element-type-p new-heading-container '(headline))))
             (new-heading-position (if container-ok
                                       (- (org-element-end new-heading-container) 1)
                                     (point-max)))
             (new-heading-level (if container-ok
                                    (+ 1 (org-element-property :level new-heading-container))
                                  1)))
        ;; Need to widen when target is outside accessible portion of
        ;; buffer, since the we want the user to end up there.
        (unless (and (<= (point-min) new-heading-position)
                     (>= (point-max) new-heading-position))
          (widen))
        (goto-char new-heading-position)
        (unless (bolp) (newline))
        (org-insert-heading nil t new-heading-level)
        (insert (if starred (substring s 1) s) "\n")
        (forward-line -1)))
     ;; Only headlines are looked after.  No need to process
     ;; further: throw an error.
     ((and (derived-mode-p 'org-mode)
	   (or starred org-link-search-must-match-exact-headline))
      (goto-char origin)
      (error "No match for fuzzy expression: %s" normalized))
     ;; Regular text search.
     ((catch :fuzzy-match
	(goto-char (point-min))
	(while (re-search-forward s-multi-re nil t)
	  ;; Skip match if it contains AVOID-POS or it is included in
	  ;; a link with a description but outside the description.
	  (unless (or (and avoid-pos
			   (<= (match-beginning 0) avoid-pos)
			   (> (match-end 0) avoid-pos))
		      (and (save-match-data
			     (org-in-regexp org-link-bracket-re))
			   (match-beginning 3)
			   (or (> (match-beginning 3) (point))
			       (<= (match-end 3) (point)))
			   (org-element-lineage
			    (save-match-data (org-element-context))
			    'link t)))
	    (goto-char (match-beginning 0))
	    (setq type 'fuzzy)
	    (throw :fuzzy-match t)))
	nil))
     ;; All failed.  Throw an error.
     (t (goto-char origin)
	(error "No match for fuzzy expression: %s" normalized)))
    ;; Disclose surroundings of match, if appropriate.
    (when (and (derived-mode-p 'org-mode) (not stealth))
      (org-fold-show-context 'link-search))
    type))

(defun org-link-heading-search-string (&optional string)
  "Make search string for the current headline or STRING.

Search string starts with an asterisk.  COMMENT keyword and
statistics cookies are removed, and contiguous spaces are packed
into a single one.

When optional argument STRING is non-nil, assume it a headline,
without any asterisk, TODO or COMMENT keyword, and without any
priority cookie or tag."
  (concat "*"
	  (org-link--normalize-string
	   (or string (org-get-heading t t t t)))))

(defun org-link-precise-link-target ()
  "Determine search string and description for storing a link.

If a search string (see `org-link-search') is found, return
list (SEARCH-STRING DESC POSITION).  Otherwise, return nil.

If there is an active region, the contents (or a part of it, see
`org-link-context-for-files') is used as the search string.

In Org buffers, if point is at a named element (such as a source
block), the name is used for the search string.  If at a heading,
its CUSTOM_ID is used to form a search string of the form
\"#id\", if present, otherwise the current heading text is used
in the form \"*Heading\".

If none of those finds a suitable search string, the current line
is used as the search string.

The description DESC is nil (meaning the user will be prompted
for a description when inserting the link) for search strings
based on a region or the current line.  For other cases, DESC is
a cleaned-up version of the name or heading at point.

POSITION is the buffer position at which the search string
matches."
  (let* ((region (org-link--context-from-region))
         (result
          (cond
           (region
            (list (org-link--normalize-string region t)
                  nil
                  (region-beginning)))

           ((derived-mode-p 'org-mode)
            (let* ((element (org-element-at-point))
                   (name (org-element-property :name element))
                   (heading (org-element-lineage element '(headline inlinetask) t))
                   (custom-id (org-entry-get heading "CUSTOM_ID")))
              (cond
               (name
                (list name
                      name
                      (org-element-begin element)))
               ((org-before-first-heading-p)
                (list (org-link--normalize-string (org-current-line-string) t)
                      nil
                      (line-beginning-position)))
               (heading
                (list (if custom-id (concat "#" custom-id)
                        (org-link-heading-search-string))
                      (org-link--normalize-string
                       (org-get-heading t t t t))
                      (org-element-begin heading))))))

           ;; Not in an org-mode buffer, no region
           (t
            (list (org-link--normalize-string (org-current-line-string) t)
                  nil
                  (line-beginning-position))))))

    ;; Only use search option if there is some text.
    (when (org-string-nw-p (car result))
      result)))

(defun org-link-open-as-file (path in-emacs)
  "Pretend PATH is a file name and open it.

IN-EMACS is passed to `org-open-file'.

According to \"file\"-link syntax, PATH may include additional
search options, separated from the file name with \"::\".

This function is meant to be used as a possible tool for
`:follow' property in `org-link-parameters'."
  (let* ((option (and (string-match "::\\(.*\\)\\'" path)
		      (match-string 1 path)))
	 (file-name (if (not option) path
		      (substring path 0 (match-beginning 0)))))
    (if (and (string-match "[*?{]" (file-name-nondirectory file-name))
             (not (file-exists-p file-name)))
        (dired file-name)
      (apply #'org-open-file
	     file-name
	     in-emacs
	     (cond ((not option) nil)
		   ((string-match-p "\\`[0-9]+\\'" option)
		    (list (string-to-number option)))
		   (t (list nil option)))))))

(defun org-link-display-format (s)
  "Replace links in string S with their description.
If there is no description, use the link target."
  (save-match-data
    (replace-regexp-in-string
     org-link-bracket-re
     (lambda (m) (or (match-string 2 m) (match-string 1 m)))
     s nil t)))

(defun org-link-add-angle-brackets (s)
  "Wrap string S within angle brackets."
  (unless (equal (substring s 0 1) "<") (setq s (concat "<" s)))
  (unless (equal (substring s -1) ">") (setq s (concat s ">")))
  s)

;;;###autoload
(defun org-link-preview (&optional arg beg end)
  "Toggle display of link previews in the buffer.

When region BEG..END is active, preview links in the
region.

When point is at a link, display a preview for that link only.
Otherwise, display previews for links in current entry.

With numeric prefix ARG 1, also preview links with description in
the active region, at point or in the current section.

With prefix ARG `\\[universal-argument]', clear link previews at
point or in the current entry.

With prefix ARG `\\[universal-argument] \\[universal-argument]',
 display link previews in the accessible portion of the
 buffer.  With numeric prefix ARG 11, do the same, but include
 links with descriptions.

With prefix ARG `\\[universal-argument] \\[universal-argument] \\[universal-argument]',
hide all link previews in the accessible portion of the buffer.

This command is designed for interactive use.  From Elisp, you can
also use `org-link-preview-region'."
  (interactive (cons current-prefix-arg
                     (when (use-region-p)
                       (list (region-beginning) (region-end)))))
  (let* ((include-linked
          (cond
           ((member arg '(nil (4) (16)) ) nil)
           ((member arg '(1 11)) 'include-linked)
           (t 'include-linked)))
         (interactive? (called-interactively-p 'any))
         (toggle-previews
          (lambda (&optional beg end scope remove)
            (let* ((beg (or beg (point-min)))
                   (end (or end (point-max)))
                   (old (org-link-preview--get-overlays beg end))
                   (scope (or scope (format "%d:%d" beg end))))
              (if remove
                  (progn
                    (org-link-preview-clear beg end)
                    (when interactive?
                      (message
                       "[%s] Inline link previews turned off (removed %d images)"
                       scope (length old))))
	        (org-link-preview-region include-linked t beg end)
                (when interactive?
                  (let ((new (org-link-preview--get-overlays beg end)))
                    (message
                     (if new
		         (format "[%s] Displaying %d images inline %s"
			         scope (length new)
                                 (if include-linked "(including images with description)"
                                   ""))
                       (if (equal scope "buffer")
		           (format "[%s] No images to display inline" scope)
                         (format
                          (substitute-command-keys
                           "[%s] No images to display inline.  Use `\\[universal-argument] \\[universal-argument]' or 11 argument to preview the whole buffer")
                          scope)))))))))))
    (cond
     ;; Region selected :: display previews in region.
     ((and beg end)
      (funcall toggle-previews beg end "region"
               (and (equal arg '(4)) 'remove)))
     ;; C-u argument: clear image at point or in entry
     ((equal arg '(4))
      (if-let* ((ov (cdr (get-char-property-and-overlay
                         (point) 'org-image-overlay))))
          ;; clear link preview at point
          (funcall toggle-previews
                   (overlay-start ov) (overlay-end ov)
                   "preview at point" 'remove)
        ;; Clear link previews in entry
        (funcall toggle-previews
                 (if (org-before-first-heading-p) (point-min)
                   (save-excursion
                     (org-with-limited-levels (org-back-to-heading t) (point))))
                 (org-with-limited-levels (org-entry-end-position))
                 "current section" 'remove)))
     ;; C-u C-u or C-11 argument :: display images in the whole buffer.
     ((member arg '(11 (16))) (funcall toggle-previews nil nil "buffer"))
     ;; C-u C-u C-u argument :: unconditionally hide images in the buffer.
     ((equal arg '(64)) (funcall toggle-previews nil nil "buffer" 'remove))
     ;; Argument nil or 1, no region selected :: display images in
     ;; current section or image link at point.
     ((and (member arg '(nil 1)) (null beg) (null end))
      (let ((context (org-element-context)))
        ;; toggle display of inline image link at point.
        (if (org-element-type-p context 'link)
            (let* ((ov (cdr-safe (get-char-property-and-overlay
                                  (point) 'org-image-overlay)))
                   (remove? (and ov (memq ov org-link-preview-overlays)
                                 'remove)))
              (funcall toggle-previews
                       (org-element-begin context)
                       (org-element-end context)
                       "image at point" remove?))
          (let ((beg (if (org-before-first-heading-p) (point-min)
	               (save-excursion
	                 (org-with-limited-levels (org-back-to-heading t) (point)))))
                (end (org-with-limited-levels (org-entry-end-position))))
            (funcall toggle-previews beg end "current section")))))
     ;; Any other non-nil argument.
     ((not (null arg)) (funcall toggle-previews beg end "region")))))

;;;###autoload
(defun org-link-preview-refresh ()
  "Assure display of link previews in buffer and refresh them."
  (interactive)
  (org-link-preview-region nil t (point-min) (point-max)))

(defun org-link-preview-region (&optional include-linked refresh beg end)
  "Display link previews.

A previewable link type is one that has a `:preview' link
parameter, see `org-link-parameters'.

By default, a file link or attachment is previewable if it
follows either of these conventions:

  1. Its path is a file with an extension matching return value
     from `image-file-name-regexp' and it has no contents.

  2. Its description consists in a single link of the previous
     type.  In this case, that link must be a well-formed plain
     or angle link, i.e., it must have an explicit \"file\" or
     \"attachment\" type.

File links are equipped with the keymap `image-map'.

When optional argument INCLUDE-LINKED is non-nil, links with a
text description part will also be inlined.  This can be nice for
a quick look at those images, but it does not reflect what
exported files will look like.

When optional argument REFRESH is non-nil, refresh existing
images between BEG and END.  This will create new image displays
only if necessary.

BEG and END define the considered part.  They default to the
buffer boundaries with possible narrowing."
  (interactive "P")
  (when refresh (org-link-preview-clear beg end))
  (org-with-point-at (or beg (point-min))
    (let ((case-fold-search t)
          preview-queue)
      ;; Collect links to preview
      (while (re-search-forward org-link-any-re end t)
        (forward-char -1)               ;ensure we are on the link
        (when-let*
            ((link (org-element-lineage (org-element-context) 'link t))
             (path (or
                    ;; Link without description or link with description
                    ;; that is requested to be previewed anyway.
                    (and (or include-linked
                             (not (org-element-contents-begin link)))
                         (org-element-property :path link))
                    ;; Special case: link with description where
                    ;; description is itself a sole link
                    (and (org-element-contents-begin link)
                         (setq link
                               (org-with-point-at (org-element-contents-begin link)
                                 (org-element-put-property
                                  (org-element-link-parser) :parent link)))
                         (org-element-type-p link 'link)
                         (equal (org-element-end link)
                                (org-element-contents-end
                                 (org-element-parent link)))
                         (org-element-property :path link))))
             (linktype (org-element-property :type link))
             (preview-func (org-link-get-parameter linktype :preview)))
          ;; Create an overlay to hold the preview
          (let ((ov (or (cdr-safe (get-char-property-and-overlay
                                   (org-element-begin link) 'org-image-overlay))
                        (make-overlay
                         (org-element-begin link)
                         (progn
		           (goto-char
		            (org-element-end link))
		           (unless (eolp) (skip-chars-backward " \t"))
		           (point))))))
            (overlay-put ov 'modification-hooks
                         (list 'org-link-preview--remove-overlay))
            (push ov org-link-preview-overlays)
            (push (list preview-func ov path link) preview-queue))))
      ;; Collect previews in buffer-local LIFO preview queue
      (setq org-link-preview--queue
            (nconc (nreverse preview-queue) org-link-preview--queue))
      ;; Run preview possibly asynchronously
      (when org-link-preview--queue
        (org-link-preview--process-queue (current-buffer))))))

(defun org-link-preview--process-queue (org-buffer)
  "Preview pending Org link previews in ORG-BUFFER.

Previews are generated from the specs in
`org-link-preview--queue', which see."
  (and (buffer-live-p org-buffer)
       (with-current-buffer org-buffer
         (cl-loop
          for spec in org-link-preview--queue
          for ov = (cadr spec)    ;SPEC is (preview-func ov path link)
          for count from org-link-preview-batch-size above 0
          do (pop org-link-preview--queue)
          if (overlay-buffer ov) do
          (if (apply spec)
              (overlay-put ov 'org-image-overlay t)
            ;; Preview was unsuccessful, delete overlay
            (delete-overlay ov)
            (setq org-link-preview-overlays
                  (delq ov org-link-preview-overlays)))
          else do (cl-incf count) end
          finally do
          (setq org-link-preview--timer
                (and org-link-preview--queue
                     (run-with-idle-timer
                      (time-add (or (current-idle-time) 0)
                                org-link-preview-delay)
                      nil #'org-link-preview--process-queue org-buffer)))))))

(defun org-link-preview-clear (&optional beg end)
  "Clear link previews in region BEG to END."
  (interactive (and (use-region-p) (list (region-beginning) (region-end))))
  (let* ((beg (or beg (point-min)))
         (end (or end (point-max)))
         (overlays (overlays-in beg end)))
    (dolist (ov overlays)
      (when (memq ov org-link-preview-overlays)
        ;; Remove pending preview tasks between BEG and END
        (when-let* ((spec (cl-find ov org-link-preview--queue
                                   :key #'cadr)))
          (setq org-link-preview--queue (delq spec org-link-preview--queue)))
        ;; Remove placed overlays between BEG and END
        (when-let* ((image (overlay-get ov 'display)))
          (when (if (fboundp 'imagep) (imagep image) (eq 'image (car-safe image)))
            (image-flush image)))
        (setq org-link-preview-overlays (delq ov org-link-preview-overlays))
        (delete-overlay ov)))
    ;; Clear removed overlays.
    (dolist (ov org-link-preview-overlays)
      (unless (overlay-buffer ov)
        (setq org-link-preview-overlays (delq ov org-link-preview-overlays))))))

(defun org-link-frame-setup-function (link-type)
  "Return the frame setup function for the link type LINK-TYPE.
This signals an error if the value of the key LINK-TYPE in
`org-link-frame-setup' is not a function."
  (let ((fun (cdr (assq link-type org-link-frame-setup))))
    (if (functionp fun)
        fun
      (error "The frame setup configuration `%S' for `%s' link type is ill-defined"
             fun link-type))))


;;; Built-in link types

;;;; "elisp" link type
(defun org-link--open-elisp (path _)
  "Open a \"elisp\" type link.
PATH is the sexp to evaluate, as a string."
  (if (or (and (org-string-nw-p org-link-elisp-skip-confirm-regexp)
	       (string-match-p org-link-elisp-skip-confirm-regexp path))
	  (not org-link-elisp-confirm-function)
	  (funcall org-link-elisp-confirm-function
		   (format "Execute %s as Elisp? "
			   (org-add-props path nil 'face 'org-warning))))
      (message "%s => %s" path
	       (if (eq ?\( (string-to-char path))
		   (eval (read path))
		 (call-interactively (read path))))
    (user-error "Abort")))

(org-link-set-parameters "elisp" :follow #'org-link--open-elisp)

;;;; "file" link type
(org-link-set-parameters "file"
                         :complete #'org-link-complete-file
                         :preview #'org-link-preview-file)

(defun org-link-preview-file (ov path link)
  "Display image file PATH in overlay OV for LINK.

LINK is the Org element being previewed.

Equip each image with the keymap `image-map'.

This is intended to be used as the `:preview' link property of
file links, see `org-link-parameters'."
  (when (display-graphic-p)
    (require 'image)
    (when-let* ((file-full (expand-file-name path))
                (file (substitute-in-file-name file-full))
                ((string-match-p (image-file-name-regexp) file))
                ((file-exists-p file)))
      (let* ((width (org-display-inline-image--width link))
	     (align (org-image--align link))
             (image (org--create-inline-image file width)))
        (when image            ; Add image to overlay
	  ;; See bug#59902.  We cannot rely
          ;; on Emacs to update image if the file
          ;; has changed.
          (image-flush image)
	  (overlay-put ov 'display image)
	  (overlay-put ov 'face 'default)
	  (overlay-put ov 'keymap image-map)
          (when align
            (overlay-put
             ov 'before-string
             (propertize
              " " 'face 'default
              'display
              (pcase align
                ("center" `(space :align-to (- center (0.5 . ,image))))
                ("right"  `(space :align-to (- right ,image)))))))
          t)))))

;;;; "help" link type
(defun org-link--open-help (path _)
  "Open a \"help\" type link.
PATH is a symbol name, as a string."
  (pcase (intern path)
    ((and (pred fboundp) function) (describe-function function))
    ((and (pred boundp) variable) (describe-variable variable))
    (name (user-error "Unknown function or variable: %s" name))))

(defun org-link--store-help (&optional _interactive?)
  "Store \"help\" type link."
  (when (eq major-mode 'help-mode)
    (let ((symbol
           (save-excursion
	     (goto-char (point-min))
             ;; In case the help is about the key-binding, store the
             ;; function instead.
             (search-forward "runs the command " (line-end-position) t)
             (read (current-buffer)))))
      (org-link-store-props :type "help"
                            :link (format "help:%s" symbol)
                            :description nil))))

(org-link-set-parameters "help"
                         :follow #'org-link--open-help
                         :store #'org-link--store-help)

(defvar shortdoc--groups)
(declare-function shortdoc-display-group "shortdoc"
                  (group &optional function same-window))
(defun org-link--open-shortdoc (path _)
  "Open a \"shortdoc\" type link.
PATH is a group name, \"group::#function\" or \"group::search
string\"."
  (string-match "\\`\\([^:]*\\)\\(?:::\\(.*\\)\\'\\)?" path)
  (let* ((group (match-string 1 path))
         (str (match-string 2 path))
         (fn (and str
                  (eq ?# (string-to-char str))
                  (intern-soft (substring str 1)))))
    (condition-case nil
        (progn
          (shortdoc-display-group group fn)
          (and str (not fn) (search-forward str nil t)))
      (error (user-error "Unknown shortdoc group or malformed link: `%s'"
                         path)))))

(defun org-link--store-shortdoc (&optional _interactive?)
  "Store \"shortdoc\" type link."
  (when (derived-mode-p 'shortdoc-mode)
    (let* ((buffer (buffer-name))
           (group (when (string-match "*Shortdoc \\(.*\\)\\*" buffer)
                    (match-string 1 buffer))))
      (if (and group (assoc (intern-soft group) shortdoc--groups))
          (org-link-store-props :type "shortdoc"
                                :link (format "shortdoc:%s" group)
                                :description nil)
        (user-error "Unknown shortdoc group: %s" group)))))

(defun org-link--complete-shortdoc ()
  "Create a \"shortdoc\" link using completion."
  (concat "shortdoc:"
          (completing-read "Shortdoc summary for functions in: "
                           (mapcar #'car shortdoc--groups))))

;; FIXME: Remove the condition when we drop Emacs 27 support.
;;;; "shortdoc" link type
(when (version<= "28.0.90" emacs-version)
  (org-link-set-parameters "shortdoc"
                           :follow #'org-link--open-shortdoc
                           :store #'org-link--store-shortdoc
                           :complete #'org-link--complete-shortdoc))

;;;; "http", "https", "mailto", "ftp", and "news" link types
(dolist (scheme '("ftp" "http" "https" "mailto" "news"))
  (org-link-set-parameters scheme
			   :follow
			   (lambda (url arg)
			     (browse-url (concat scheme ":" url) arg))))

;;;; "shell" link type
(defun org-link--open-shell (path _)
  "Open a \"shell\" type link.
PATH is the command to execute, as a string."
  (if (or (and (org-string-nw-p org-link-shell-skip-confirm-regexp)
	       (string-match-p org-link-shell-skip-confirm-regexp path))
	  (not org-link-shell-confirm-function)
	  (funcall org-link-shell-confirm-function
		   (format "Execute %s in shell? "
			   (org-add-props path nil 'face 'org-warning))))
      (let ((buf (generate-new-buffer "*Org Shell Output*")))
	(message "Executing %s" path)
	(shell-command path buf)
	(when (featurep 'midnight)
	  (setq clean-buffer-list-kill-buffer-names
		(cons (buffer-name buf)
		      clean-buffer-list-kill-buffer-names))))
    (user-error "Abort")))

(org-link-set-parameters "shell" :follow #'org-link--open-shell)


;;; Interactive Functions

;;;###autoload
(defun org-next-link (&optional search-backward)
  "Move forward to the next link.
If the link is in hidden text, expose it.  When SEARCH-BACKWARD
is non-nil, move backward."
  (interactive)
  (let ((pos (point))
	(search-fun (if search-backward #'re-search-backward
		      #'re-search-forward)))
    ;; Tweak initial position.  If last search failed, wrap around.
    ;; Otherwise, make sure we do not match current link.
    (cond
     ((not (and org-link--search-failed (eq this-command last-command)))
      (cond
       ((and (not search-backward) (looking-at org-link-any-re))
	(goto-char (match-end 0)))
       (search-backward
	(pcase (org-in-regexp org-link-any-re nil t)
	  (`(,beg . ,_) (goto-char beg))
	  (_ nil)))
       (t nil)))
     (search-backward
      (goto-char (point-max))
      (message "Link search wrapped back to end of buffer"))
     (t
      (goto-char (point-min))
      (message "Link search wrapped back to beginning of buffer")))
    (setq org-link--search-failed nil)
    (catch :found
      (while (funcall search-fun org-link-any-re nil t)
	(let ((context (save-excursion
			 (unless search-backward (forward-char -1))
			 (org-element-context))))
	  (pcase (org-element-lineage context 'link t)
	    (`nil nil)
	    (link
	     (goto-char (org-element-begin link))
	     (when (org-invisible-p) (org-fold-show-context 'link-search))
	     (throw :found t)))))
      (goto-char pos)
      (setq org-link--search-failed t)
      (message "No further link found"))))

;;;###autoload
(defun org-previous-link ()
  "Move backward to the previous link.
If the link is in hidden text, expose it."
  (interactive)
  (org-next-link t))

;;;###autoload
(defun org-toggle-link-display ()
  "Toggle the literal or descriptive display of links in current buffer."
  (interactive)
  (setq org-link-descriptive (not org-link-descriptive))
  (org-restart-font-lock))

;;;###autoload
(defun org-store-link (arg &optional interactive?)
  "Store a link to the current location.
\\<org-mode-map>
This link is added to `org-stored-links' and can later be inserted
into an Org buffer with `org-insert-link' (`\\[org-insert-link]').
When optional argument INTERACTIVE? is nil, the link is not stored in
`org-stored-links', but returned as a string.

For some link types, a `\\[universal-argument]' prefix ARG is interpreted.  \
A single
`\\[universal-argument]' negates `org-link-context-for-files' for file links or
`org-gnus-prefer-web-links' for links to Usenet articles.

A `\\[universal-argument] \\[universal-argument]' prefix ARG forces \
skipping storing functions that are not
part of Org core.

A `\\[universal-argument] \\[universal-argument] \\[universal-argument]' \
prefix ARG forces storing a link for each line in the
active region.

Assume the function is called interactively if INTERACTIVE? is
non-nil.

In Org buffers, an additional \"human-readable\" simple file link
is stored as an alternative to persistent org-id or other links,
if at a heading with a CUSTOM_ID property or an element with a
NAME."
  (interactive "P\np")
  (org-load-modules-maybe)
  (if (and (equal arg '(64)) (org-region-active-p))
      (save-excursion
	(let ((end (region-end)))
	  (goto-char (region-beginning))
	  (set-mark (point))
          (while (< (line-end-position) end)
	    (move-end-of-line 1) (activate-mark)
	    (let (current-prefix-arg)
	      (call-interactively 'org-store-link))
	    (move-beginning-of-line 2)
	    (set-mark (point)))))
    (setq org-store-link-plist nil)
    ;; Negate `org-link-context-for-files' when given a single universal arg.
    (let ((org-link-context-for-files (org-xor org-link-context-for-files
                                               (equal arg '(4))))
          link desc search agenda-link) ;; description
      (cond
       ;; Store a link using an external link type, if any function is
       ;; available, unless external link types are skipped for this
       ;; call using two universal args.  If more than one function
       ;; can generate a link from current location, ask the user
       ;; which one to use.
       ((and (not (equal arg '(16)))
             (org-link--try-link-store-functions interactive?))
        (setq link (plist-get org-store-link-plist :link))
        ;; If store function actually set `:description' property, use
        ;; it, even if it is nil.  Otherwise, fallback to nil (ask user).
	(setq desc (plist-get org-store-link-plist :description)))

       ;; Store a link from a remote editing buffer.
       ((org-src-edit-buffer-p)
	(let ((coderef-format (org-src-coderef-format))
	      (format-link
	       (lambda (label)
		 (if org-src-source-file-name
		     (format "file:%s::(%s)" org-src-source-file-name label)
		   (format "(%s)" label)))))
	  (cond
	   ;; Code references do not exist in this type of buffer.
	   ;; Pretend we're linking from the source buffer directly.
	   ((not (memq (org-src-source-type) '(example-block src-block)))
	    (with-current-buffer (org-src-source-buffer)
	      (org-store-link arg interactive?))
	    (setq link nil))
	   ;; A code reference exists.  Use it.
	   ((save-excursion
	      (forward-line 0)
	      (re-search-forward (org-src-coderef-regexp coderef-format)
				 (line-end-position)
				 t))
	    (setq link (funcall format-link (match-string-no-properties 3))))
	   ;; No code reference.  Create a new one then store the link
	   ;; to it, but only in the function is called interactively.
	   (interactive?
	    (end-of-line)
	    (let* ((label (read-string "Code line label: "))
		   (reference (format coderef-format label))
		   (gc (- 79 (length reference))))
	      (if (< (current-column) gc)
		  (org-move-to-column gc t)
		(insert " "))
	      (insert reference)
	      (setq link (funcall format-link label))))
	   ;; No code reference, and non-interactive call.  Don't know
	   ;; what to do.  Give up.
	   (t (setq link nil)))))

       ;; We are in the agenda, link to referenced location
       ((eq major-mode 'org-agenda-mode)
	(let ((m (or (get-text-property (point) 'org-hd-marker)
		     (get-text-property (point) 'org-marker))))
	  (when m
	    (org-with-point-at m
	      (setq agenda-link (org-store-link nil interactive?))))))

       ;; Calendar mode
       ((eq major-mode 'calendar-mode)
	(let ((cd (calendar-cursor-to-date)))
	  (setq link
		(format-time-string
                 (org-time-stamp-format)
		 (org-encode-time 0 0 0 (nth 1 cd) (nth 0 cd) (nth 2 cd))))
	  (org-link-store-props :type "calendar" :date cd)))

       ;; Image mode
       ((eq major-mode 'image-mode)
	(setq link (concat "file:"
		           (abbreviate-file-name buffer-file-name)))
	(org-link-store-props :type "image" :file buffer-file-name))

       ;; In dired, store a link to the file of the current line
       ((derived-mode-p 'dired-mode)
	(let ((file (dired-get-filename nil t)))
	  (setq file (if file
			 (abbreviate-file-name
			  (expand-file-name (dired-get-filename nil t)))
		       ;; Otherwise, no file so use current directory.
		       default-directory))
	  (setq link (concat "file:" file))))

       ;; Try `org-create-file-search-functions`.  If any are
       ;; successful, create a file link to the current buffer with
       ;; the provided search string.
       ((setq search (run-hook-with-args-until-success
		      'org-create-file-search-functions))
	(setq link (concat "file:" (abbreviate-file-name buffer-file-name)
			   "::" search)
              desc (plist-get org-store-link-plist :description)))

       ;; Main logic for storing built-in link types in org-mode
       ;; buffers
       ((and (buffer-file-name (buffer-base-buffer)) (derived-mode-p 'org-mode))
	(org-with-limited-levels
	 (cond
	  ;; Store a link using the target at point
	  ((org-in-regexp "[^<]<<\\([^<>]+\\)>>[^>]" 1)
	   (setq link
		 (concat "file:"
			 (abbreviate-file-name
			  (buffer-file-name (buffer-base-buffer)))
			 "::" (match-string 1))
                 ;; Target may be shortened when link is inserted.
                 ;; Avoid [[target][file:~/org/test.org::target]]
                 ;; links.  Maybe the case of identical target and
                 ;; description should be handled by `org-insert-link'.
                 desc nil))
          (t
	   ;; Just link to current headline.
           (let ((here (org-link--file-link-to-here)))
             (setq link (car here))
             (setq desc (cdr here)))))))

       ;; Buffer linked to file, but not an org-mode buffer.
       ((buffer-file-name (buffer-base-buffer))
	;; Just link to this file here.
        (let ((here (org-link--file-link-to-here)))
          (setq link (car here))
          (setq desc (cdr here))))

       (interactive?
	(user-error "No method for storing a link from this buffer"))

       (t (setq link nil)))

      ;; We're done setting link and desc, clean up
      (when (consp link) (setq link (or (cdr link) (car link))))
      (cond ((not desc))
	    ((equal desc "NONE") (setq desc nil))
	    (t (setq desc (org-link-display-format desc))))
      ;; Store and return the link
      (if (not (and interactive? link))
	  (or agenda-link (and link (org-link-make-string link desc)))
        (org-link--add-to-stored-links link desc)
        ;; In org buffers, store an additional "human-readable" link
        ;; using custom id, if available.
        (when (and (buffer-file-name (buffer-base-buffer))
                   (derived-mode-p 'org-mode)
                   (org-entry-get nil "CUSTOM_ID"))
          (let ((here (org-link--file-link-to-here)))
            (setq link (car here))
            (setq desc (cdr here)))
          (unless (equal (list link desc) (car org-stored-links))
            (org-link--add-to-stored-links link desc)))
        (car org-stored-links)))))

(defun org-link--normalize-filename (filename &optional method)
  "Return FILENAME as required by METHOD.
METHOD defaults to the value of `org-link-file-path-type'."
  (setq method (or method org-link-file-path-type))
  (cond
   ((eq method 'absolute)
    (abbreviate-file-name (expand-file-name filename)))
   ((eq method 'noabbrev)
    (expand-file-name filename))
   ((eq method 'relative)
    (file-relative-name filename))
   ((functionp method)
    (funcall method filename))
   (t
    (save-match-data
      (if (string-match (concat "^" (regexp-quote
				     (expand-file-name
				      (file-name-as-directory
				       default-directory))))
			(expand-file-name filename))
	  ;; We are linking a file with relative path name.
	  (substring (expand-file-name filename)
		     (match-end 0))
	(abbreviate-file-name (expand-file-name filename)))))))

;;;###autoload
(defun org-insert-link (&optional complete-file link-location description)
  "Insert a link.  At the prompt, enter the link.

Completion can be used to insert any of the link protocol prefixes in use.

The history can be used to select a link previously stored with
`org-store-link'.  When the empty string is entered (i.e. if you just
press `RET' at the prompt), the link defaults to the most recently
stored link.  As `SPC' triggers completion in the minibuffer, you need to
use `M-SPC' or `C-q SPC' to force the insertion of a space character.
Completion candidates include link descriptions.

If there is a link under cursor then edit it.

You will also be prompted for a description, and if one is given, it will
be displayed in the buffer instead of the link.

If there is already a link at point, this command will allow you to edit
link and description parts.

With a `\\[universal-argument]' prefix, prompts for a file to link to.  The \
file name can be
selected using completion.  The path to the file will be relative to the
current directory if the file is in the current directory or a subdirectory.
Otherwise, the link will be the absolute path as completed in the minibuffer
\(i.e. normally ~/path/to/file).  You can configure this behavior using the
option `org-link-file-path-type'.

With a `\\[universal-argument] \\[universal-argument]' prefix, enforce an \
absolute path even if the file is in
the current directory or below.

A `\\[universal-argument] \\[universal-argument] \\[universal-argument]' \
prefix negates `org-link-keep-stored-after-insertion'.

If the LINK-LOCATION parameter is non-nil, this value will be used as
the link location instead of reading one interactively.

If the DESCRIPTION parameter is non-nil, this value will be used
as the default description.  If not, and the chosen link type has
a non-nil `:insert-description' parameter, that is used to
generate a description as described in `org-link-parameters'
docstring.  Otherwise, if `org-link-make-description-function' is
non-nil, this function will be called with the link target, and
the result will be the default link description.  When called
non-interactively, don't allow editing the default description."
  (interactive "P")
  (let* ((wcf (current-window-configuration))
	 (origbuf (current-buffer))
	 (region (when (org-region-active-p)
		   (buffer-substring (region-beginning) (region-end))))
	 (remove (and region (list (region-beginning) (region-end))))
	 (desc region)
	 (link link-location)
	 (abbrevs org-link-abbrev-alist-local)
	 (all-prefixes (append (mapcar #'car abbrevs)
			       (mapcar #'car org-link-abbrev-alist)
			       (org-link-types)))
         entry)
    (cond
     (link-location)		      ; specified by arg, just use it.
     ((org-in-regexp org-link-bracket-re 1)
      ;; We do have a link at point, and we are going to edit it.
      (setq remove (list (match-beginning 0) (match-end 0)))
      (setq desc (when (match-end 2) (match-string-no-properties 2)))
      (setq link (read-string "Link: "
			      (org-link-unescape
			       (match-string-no-properties 1)))))
     ((or (org-in-regexp org-link-angle-re)
	  (org-in-regexp org-link-plain-re))
      ;; Convert to bracket link
      (setq remove (list (match-beginning 0) (match-end 0))
	    link (read-string "Link: "
			      (org-unbracket-string "<" ">" (match-string 0)))))
     ((member complete-file '((4) (16)))
      ;; Completing read for file names.
      (setq link (org-link-complete-file complete-file)))
     (t
      ;; Read link, with completion for stored links.
      (org-link--fontify-links-to-this-file)
      (switch-to-buffer-other-window "*Org Links*")
      (with-current-buffer "*Org Links*"
        (read-only-mode 1)
        (let ((inhibit-read-only t)
              ;; FIXME Duplicate: Also in 'ox.el'.
              (propertize-help-key
               (lambda (key)
                 ;; Add `face' *and* `font-lock-face' to "work
                 ;; reliably in any buffer", per a comment in
                 ;; `help--key-description-fontified'.
                 (propertize key
                             'font-lock-face 'help-key-binding
                             'face 'help-key-binding))))
          (erase-buffer)
          (insert
           (apply #'format "Type %s to complete link type, then %s to complete destination.\n"
                  (mapcar propertize-help-key
                          (list "TAB" "RET"))))
	  (when org-stored-links
            (insert (apply #'format "\nStored links accessible with %s/%s or %s/%s are:\n\n"
                           (mapcar propertize-help-key
                                   (list "<up>" "<down>"
                                         "M-p" "M-n"
                                         "RET"))))
	    (insert (mapconcat #'org-link--prettify
			       (reverse org-stored-links)
			       "\n"))))
        (goto-char (point-min)))
      (when (get-buffer-window "*Org Links*" 'visible)
        (let ((cw (selected-window)))
	  (select-window (get-buffer-window "*Org Links*" 'visible))
	  (with-current-buffer "*Org Links*" (setq truncate-lines t))
	  (unless (pos-visible-in-window-p (point-max))
	    (org-fit-window-to-buffer))
	  (and (window-live-p cw) (select-window cw))))
      (unwind-protect
	  ;; Fake a link history, containing the stored links.
	  (let ((org-link--history
		 (append (mapcar #'car org-stored-links)
			 org-link--insert-history)))
	    (setq link
		  (org-completing-read
                   (org-format-prompt "Insert link" (caar org-stored-links))
		   (append
		    (mapcar (lambda (x) (concat x ":")) all-prefixes)
		    (mapcar #'car org-stored-links)
                    ;; Allow description completion.  Avoid "nil" option
                    ;; in the case of `completing-read-default' when
                    ;; some links have no description.
                    (delq nil (mapcar 'cadr org-stored-links)))
		   nil nil nil
		   'org-link--history
		   (caar org-stored-links)))
	    (unless (org-string-nw-p link) (user-error "No link selected"))
	    (dolist (l org-stored-links)
	      (when (equal link (cadr l))
		(setq link (car l))))
	    (when (or (member link all-prefixes)
		      (and (equal ":" (substring link -1))
			   (member (substring link 0 -1) all-prefixes)
			   (setq link (substring link 0 -1))))
	      (setq link (with-current-buffer origbuf
			   (org-link--try-special-completion link)))))
        (when-let* ((window (get-buffer-window "*Org Links*" t)))
          (quit-window 'kill window))
	(set-window-configuration wcf)
	(when (get-buffer "*Org Links*")
          (kill-buffer "*Org Links*")))
      (setq entry (assoc link org-stored-links))
      (or entry (push link org-link--insert-history))
      (setq desc (or desc (nth 1 entry)))))

    (let (new-link)
      (let ((org-link-file-path-type
             (if (equal complete-file '(16))
                 'absolute
               org-link-file-path-type)))
        (setq new-link (org-link-make-string-for-buffer
                        link (or description desc)
                        (called-interactively-p 'any))))

      (when (funcall (if (equal complete-file '(64)) 'not 'identity)
                     (not org-link-keep-stored-after-insertion))
        (setq org-stored-links (delq (assoc link org-stored-links) org-stored-links)))
      (when remove (apply #'delete-region remove))
      (insert new-link))
    ;; Redisplay so as the new link has proper invisible characters.
    (sit-for 0)))

;;;###autoload
(defun org-insert-all-links (arg &optional pre post)
  "Insert all links in `org-stored-links'.
When a universal prefix, do not delete the links from `org-stored-links'.
When `ARG' is a number, insert the last N link(s).
`PRE' and `POST' are optional arguments to define a string to
prepend or to append."
  (interactive "P")
  (let ((org-link-keep-stored-after-insertion (equal arg '(4)))
	(links (copy-sequence org-stored-links))
	(pr (or pre "- "))
	(po (or post "\n"))
	(cnt 1) l)
    (if (null org-stored-links)
	(message "No link to insert")
      (while (and (or (listp arg) (>= arg cnt))
		  (setq l (if (listp arg)
			      (pop links)
			    (pop org-stored-links))))
	(setq cnt (1+ cnt))
	(insert pr)
	(org-insert-link nil (car l) (or (cadr l) "<no description>"))
	(insert po)))))

;;;###autoload
(defun org-insert-last-stored-link (arg)
  "Insert the last link stored in `org-stored-links'."
  (interactive "p")
  (org-insert-all-links arg "" "\n"))

;;;###autoload
(defun org-insert-link-global ()
  "Insert a link like Org mode does.
This command can be called in any mode to insert a link in Org syntax."
  (interactive)
  (org-load-modules-maybe)
  (org-run-like-in-org-mode 'org-insert-link))

(defun org--re-list-search-forward (regexp-list &optional bound noerror count)
  "Like `re-search-forward', but REGEXP-LIST is a list of regexps.
BOUND, NOERROR, and COUNT are passed to `re-search-forward'."
  (let (result (min-found most-positive-fixnum)
               (pos-found nil)
               (min-found-data nil)
               (tail regexp-list))
    (while tail
      (setq result (save-excursion (re-search-forward (pop tail) bound t count)))
      (when (and result (< result min-found))
        (setq min-found result
              pos-found (match-end 0)
              min-found-data (match-data))))
    (if (= most-positive-fixnum min-found)
        (pcase noerror
          (`t nil)
          (_ (re-search-forward (car regexp-list) bound noerror count)))
      (set-match-data min-found-data)
      (goto-char pos-found))))

(defun org--re-list-looking-at (regexp-list &optional inhibit-modify)
  "Like `looking-at', but REGEXP-LIST is a list of regexps.
INHIBIT-MODIFY is passed to `looking-at'."
  (catch :found
    (while regexp-list
      (when
          (if inhibit-modify
              (looking-at-p (pop regexp-list))
            ;; FIXME: In Emacs <29, `looking-at' does not accept
            ;; optional INHIBIT-MODIFY argument.
            (looking-at (pop regexp-list)))
        (throw :found t)))))

;;;###autoload
(defun org-update-radio-target-regexp ()
  "Find all radio targets in this file and update the regular expression.
Also refresh fontification if needed."
  (interactive)
  (let ((old-regexp org-target-link-regexp)
	;; Some languages, e.g., Chinese, do not use spaces to
        ;; separate words.  Also allow surrounding radio targets with
	;; line-breakable characters.
	(before-re "\\(?:^\\|[^[:alnum:]]\\|\\c|\\)\\(")
	(after-re "\\)\\(?:$\\|[^[:alnum:]]\\|\\c|\\)")
	(targets
	 (org-with-wide-buffer
	  (goto-char (point-min))
	  (let (rtn)
	    (while (re-search-forward org-radio-target-regexp nil t)
	      ;; Make sure point is really within the object.
	      (backward-char)
	      (let ((obj (org-element-context)))
		(when (org-element-type-p obj 'radio-target)
		  (cl-pushnew (org-element-property :value obj) rtn
			      :test #'equal))))
	    rtn))))
    (setq targets
          (sort targets
                (lambda (a b)
                  (> (length a) (length b)))))
    (setq org-target-link-regexp
	  (and targets
	       (concat before-re
		       (mapconcat
			(lambda (x)
			  (replace-regexp-in-string
			   " +" "\\s-+" (regexp-quote x) t t))
			targets
			"\\|")
		       after-re)))
    (setq org-target-link-regexps nil)
    (let (current-length sub-targets)
      (when (<= org-target-link-regexp-limit (length org-target-link-regexp))
        (while (or targets sub-targets)
          (when (and sub-targets
                     (or (not targets)
                         (>= (+ current-length (length (car targets)))
                            org-target-link-regexp-limit)))
            (push (concat before-re
                          (mapconcat
			   (lambda (x)
			     (replace-regexp-in-string
			      " +" "\\s-+" (regexp-quote x) t t))
			   (nreverse sub-targets)
			   "\\|")
		          after-re)
                  org-target-link-regexps)
            (setq current-length nil
                  sub-targets nil))
          (unless current-length
            (setq current-length (+ (length before-re) (length after-re))))
          (when targets (push (pop targets) sub-targets))
          (cl-incf current-length (length (car sub-targets))))
        (setq org-target-link-regexps (nreverse org-target-link-regexps))))
    (unless (equal old-regexp org-target-link-regexp)
      ;; Clean-up cache.
      (let ((regexp (cond ((not old-regexp) org-target-link-regexp)
			  ((not org-target-link-regexp) old-regexp)
			  (t
			   (concat before-re
				   (mapconcat
				    (lambda (re)
				      (substring re (length before-re)
						 (- (length after-re))))
				    (list old-regexp org-target-link-regexp)
				    "\\|")
				   after-re)))))
	(when (and (featurep 'org-element)
                   (not (bound-and-true-p org-mode-loading)))
          (if org-target-link-regexps
              (org-element-cache-reset)
	    (org-with-point-at 1
	      (while (re-search-forward regexp nil t)
	        (org-element-cache-refresh (match-beginning 1)))))))
      ;; Re fontify buffer.
      (when (memq 'radio org-highlight-links)
	(org-restart-font-lock)))))


;;; Initialize Regexps

(org-link-make-regexps)

(provide 'ol)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; ol.el ends here
