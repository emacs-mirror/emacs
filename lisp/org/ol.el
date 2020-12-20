;;; ol.el --- Org links library                      -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2020 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp

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

(require 'org-compat)
(require 'org-macs)

(defvar clean-buffer-list-kill-buffer-names)
(defvar org-agenda-buffer-name)
(defvar org-comment-string)
(defvar org-highlight-links)
(defvar org-id-link-to-org-use-id)
(defvar org-inhibit-startup)
(defvar org-outline-regexp-bol)
(defvar org-src-source-file-name)
(defvar org-time-stamp-formats)
(defvar org-ts-regexp)

(declare-function calendar-cursor-to-date "calendar" (&optional error event))
(declare-function dired-get-filename "dired" (&optional localp no-error-if-not-filep))
(declare-function org-at-heading-p "org" (&optional _))
(declare-function org-back-to-heading "org" (&optional invisible-ok))
(declare-function org-before-first-heading-p "org" ())
(declare-function org-do-occur "org" (regexp &optional cleanup))
(declare-function org-element-at-point "org-element" ())
(declare-function org-element-cache-refresh "org-element" (pos))
(declare-function org-element-context "org-element" (&optional element))
(declare-function org-element-lineage "org-element" (datum &optional types with-self))
(declare-function org-element-link-parser "org-element" ())
(declare-function org-element-property "org-element" (property element))
(declare-function org-element-type "org-element" (element))
(declare-function org-element-update-syntax "org-element" ())
(declare-function org-entry-get "org" (pom property &optional inherit literal-nil))
(declare-function org-find-property "org" (property &optional value))
(declare-function org-get-heading "org" (&optional no-tags no-todo no-priority no-comment))
(declare-function org-id-find-id-file "org-id" (id))
(declare-function org-id-store-link "org-id" ())
(declare-function org-insert-heading "org" (&optional arg invisible-ok top))
(declare-function org-load-modules-maybe "org" (&optional force))
(declare-function org-mark-ring-push "org" (&optional pos buffer))
(declare-function org-mode "org" ())
(declare-function org-occur "org" (regexp &optional keep-previous callback))
(declare-function org-open-file "org" (path &optional in-emacs line search))
(declare-function org-overview "org" ())
(declare-function org-restart-font-lock "org" ())
(declare-function org-run-like-in-org-mode "org" (cmd))
(declare-function org-show-context "org" (&optional key))
(declare-function org-src-coderef-format "org-src" (&optional element))
(declare-function org-src-coderef-regexp "org-src" (fmt &optional label))
(declare-function org-src-edit-buffer-p "org-src" (&optional buffer))
(declare-function org-src-source-buffer "org-src" ())
(declare-function org-src-source-type "org-src" ())
(declare-function org-time-stamp-format "org" (&optional long inactive))
(declare-function outline-next-heading "outline" ())


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
  - the export back-end,
  - the export communication channel, as a plist.

  When nil, export for that type of link is delegated to the
  back-end.

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

`:display'

  Value for `invisible' text property on the hidden parts of the
  link.  The most useful value is `full', which will not fold the
  link in descriptive display.  Default is `org-link'.

`:face'

  Face for the link, or a function returning a face.  The
  function takes one argument, which is the path.

  The default face is `org-link'.

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
  :package-version '(Org . "9.1")
  :type '(alist :tag "Link display parameters"
		:value-type plist)
  :safe nil)

(defcustom org-link-descriptive t
  "Non-nil means Org displays descriptive links.

E.g. [[https://orgmode.org][Org website]] is be displayed as
\"Org Website\", hiding the link itself and just displaying its
description.  When set to nil, Org displays the full links
literally.

You can interactively set the value of this variable by calling
`org-toggle-link-display' or from the \"Org > Hyperlinks\" menu."
  :group 'org-link
  :type 'boolean
  :safe #'booleanp)

(defcustom org-link-make-description-function nil
  "Function to use for generating link descriptions from links.
This function must take two parameters: the first one is the
link, the second one is the description generated by
`org-insert-link'.  The function should return the description to
use."
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
          directories of it.  For other files, use an absolute path."
  :group 'org-link
  :type '(choice
	  (const relative)
	  (const absolute)
	  (const noabbrev)
	  (const adaptive))
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
  :safe (lambda (val)
	  (pcase val
	    (`(,(pred stringp) . ,(pred stringp)) t)
	    (_ nil))))

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
with possibly modified values of type and path.
Org contains a function for this, so if you set this variable to
`org-translate-link-from-planner', you should be able follow many
links created by planner."
  :group 'org-link-follow
  :type '(choice (const nil) (function))
  :safe #'null)

(defcustom org-link-doi-server-url "https://doi.org/"
  "The URL of the DOI server."
  :group 'org-link-follow
  :version "24.3"
  :type 'string
  :safe #'stringp)

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
		 (const org-gnus-no-new-news)))
	  (cons (const file)
		(choice
		 (const find-file)
		 (const find-file-other-window)
		 (const find-file-other-frame)))
	  (cons (const wl)
		(choice
		 (const wl)
		 (const wl-other-frame))))
  :safe nil)

(defcustom org-link-search-must-match-exact-headline 'query-to-create
  "Non-nil means internal fuzzy links can only match headlines.

When nil, the a fuzzy link may point to a target or a named
construct in the document.  When set to the special value
`query-to-create', offer to create a new headline when none
matched.

Spaces and statistics cookies are ignored during heading searches."
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

     [[shell:rm -rf ~/*][Google Search]]

This link would show up in your Org document as \"Google Search\",
but really it would remove your entire home directory.
Therefore we advise against setting this variable to nil.
Just change it to `y-or-n-p' if you want to confirm with a
single keystroke rather than having to type \"yes\"."
  :group 'org-link-follow
  :type '(choice
	  (const :tag "with yes-or-no (safer)" yes-or-no-p)
	  (const :tag "with y-or-n (faster)" y-or-n-p)
	  (const :tag "no confirmation (dangerous)" nil))
  :safe nil)

(defcustom org-link-shell-skip-confirm-regexp ""
  "Regexp to skip confirmation for shell links."
  :group 'org-link-follow
  :version "24.1"
  :type 'regexp
  :safe nil)

(defcustom org-link-elisp-confirm-function 'yes-or-no-p
  "Non-nil means ask for confirmation before executing Emacs Lisp links.
Elisp links can be dangerous: just think about a link

     [[elisp:(shell-command \"rm -rf ~/*\")][Google Search]]

This link would show up in your Org document as \"Google Search\",
but really it would remove your entire home directory.
Therefore we advise against setting this variable to nil.
Just change it to `y-or-n-p' if you want to confirm with a
single keystroke rather than having to type \"yes\"."
  :group 'org-link-follow
  :type '(choice
	  (const :tag "with yes-or-no (safer)" yes-or-no-p)
	  (const :tag "with y-or-n (faster)" y-or-n-p)
	  (const :tag "no confirmation (dangerous)" nil))
  :safe nil)

(defcustom org-link-elisp-skip-confirm-regexp ""
  "A regexp to skip confirmation for Elisp links."
  :group 'org-link-follow
  :version "24.1"
  :type 'regexp
  :safe nil)

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
  "Format of the description part of a link to an email or usenet message.
The following %-escapes will be replaced by corresponding information:

%F   full \"From\" field
%f   name, taken from \"From\" field, address if no name
%T   full \"To\" field
%t   first name in \"To\" field, address if no name
%c   correspondent.  Usually \"from NAME\", but if you sent it yourself, it
     will be \"to NAME\".  See also the variable `org-from-is-user-regexp'.
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

;;; Public variables

(defconst org-target-regexp (let ((border "[^<>\n\r \t]"))
			      (format "<<\\(%s\\|%s[^<>\n\r]*%s\\)>>"
				      border border border))
  "Regular expression matching a link target.")

(defconst org-radio-target-regexp (format "<%s>" org-target-regexp)
  "Regular expression matching a radio target.")

(defvar-local org-target-link-regexp nil
  "Regular expression matching radio targets in plain text.")

(defvar org-link-types-re nil
  "Matches a link that has a url-like prefix like \"http:\"")

(defvar org-link-angle-re nil
  "Matches link with angular brackets, spaces are allowed.")

(defvar org-link-plain-re nil
  "Matches plain link, without spaces.")

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

A function in this hook may also use `setq' to set the variable
`description' to provide a suggestion for the descriptive text to
be used for this link when it gets inserted into an Org buffer
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
nil to indicate that that Org can continue with other options like
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
	  (setq sum (+ (lsh sum (car shift-xor)) val))
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
      (with-current-buffer indirect-buffer (org-overview))
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
	  (when (eq (org-element-type object) 'radio-target)
	    (goto-char (org-element-property :begin object))
	    (org-show-context 'link-search)
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
	  (concat
	   "\\<" types-re ":"
	   "\\([^][ \t\n()<>]+\\(?:([[:word:]0-9_]+)\\|\\([^[:punct:] \t\n]\\|/\\)\\)\\)")
	  ;;	 "\\([^]\t\n\r<>() ]+[^]\t\n\r<>,.;() ]\\)")
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
  "Create a file link using completion."
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
  "Store link properties.
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
  "Add these properties to the link property list."
  (let (key value)
    (while plist
      (setq key (pop plist) value (pop plist))
      (setq org-store-link-plist
	    (plist-put org-store-link-plist key value)))))

(defun org-link-encode (text table)
  "Return percent escaped representation of string TEXT.
TEXT is a string with the text to escape. TABLE is a list of
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
E.g. \"%C3%B6\" becomes the german o-Umlaut."
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
    (if (not (org-string-nw-p link)) description
      (format "[[%s]%s]"
	      (org-link-escape link)
	      (if description (format "[%s]" description) "")))))

(defun org-store-link-functions ()
  "List of functions that are called to create and store a link.

The functions are defined in the `:store' property of
`org-link-parameters'.

Each function will be called in turn until one returns a non-nil
value.  Each function should check if it is responsible for
creating this link (for example by looking at the major mode).
If not, it must exit and return nil.  If yes, it should return
a non-nil value after calling `org-link-store-props' with a list
of properties and values.  Special properties are:

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
	(cond
	 ((symbolp rpl) (funcall rpl tag))
	 ((string-match "%(\\([^)]+\\))" rpl)
	  (replace-match
	   (save-match-data
	     (funcall (intern-soft (match-string 1 rpl)) tag))
	   t t rpl))
	 ((string-match "%s" rpl) (replace-match (or tag "") t t rpl))
	 ((string-match "%h" rpl)
	  (replace-match (url-hexify-string (or tag "")) t t rpl))
	 (t (concat rpl tag)))))))

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
			 (+ 2 (org-element-property :begin link)))))
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
	       (funcall (org-link-get-parameter type :follow) path arg)
	     (wrong-number-of-arguments
	      (funcall (org-link-get-parameter type :follow) path)))))))))

(defun org-link-open-from-string (s &optional arg)
  "Open a link in the string S, as if it was in Org mode.
Optional argument is passed to `org-open-file' when S is
a \"file\" link."
  (interactive "sLink: \nP")
  (pcase (with-temp-buffer
	   (let ((org-inhibit-startup nil))
	     (insert s)
	     (org-mode)
	     (goto-char (point-min))
	     (org-element-link-parser)))
    (`nil (user-error "No valid link in %S" s))
    (link (org-link-open link arg))))

(defun org-link-search (s &optional avoid-pos stealth)
  "Search for a search string S.

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
visibility around point, thus ignoring `org-show-context-detail'
variable.

Search is case-insensitive and ignores white spaces.  Return type
of matched result, which is either `dedicated' or `fuzzy'."
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
	      (when (and (memq (org-element-type element)
			       '(example-block src-block))
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
		   (when (eq (org-element-type context) 'target)
		     (setq type 'dedicated)
		     (goto-char (org-element-property :begin context))
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
		   (when (and name (equal words (split-string name)))
		     (setq type 'dedicated)
		     (beginning-of-line)
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
		 (when (equal words
			      (split-string
			       (org-link--normalize-string
				(org-get-heading t t t t))))
		   (throw :found t)))
	       nil)))
      (beginning-of-line)
      (setq type 'dedicated))
     ;; Offer to create non-existent headline depending on
     ;; `org-link-search-must-match-exact-headline'.
     ((and (derived-mode-p 'org-mode)
	   (eq org-link-search-must-match-exact-headline 'query-to-create)
	   (yes-or-no-p "No match - create this as a new heading? "))
      (goto-char (point-max))
      (unless (bolp) (newline))
      (org-insert-heading nil t t)
      (insert s "\n")
      (beginning-of-line 0))
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
			    '(link) t)))
	    (goto-char (match-beginning 0))
	    (setq type 'fuzzy)
	    (throw :fuzzy-match t)))
	nil))
     ;; All failed.  Throw an error.
     (t (goto-char origin)
	(error "No match for fuzzy expression: %s" normalized)))
    ;; Disclose surroundings of match, if appropriate.
    (when (and (derived-mode-p 'org-mode) (not stealth))
      (org-show-context 'link-search))
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

(defun org-link-open-as-file (path arg)
  "Pretend PATH is a file name and open it.

According to \"file\"-link syntax, PATH may include additional
search options, separated from the file name with \"::\".

This function is meant to be used as a possible tool for
`:follow' property in `org-link-parameters'."
  (let* ((option (and (string-match "::\\(.*\\)\\'" path)
		      (match-string 1 path)))
	 (file-name (if (not option) path
		      (substring path 0 (match-beginning 0)))))
    (if (string-match "[*?{]" (file-name-nondirectory file-name))
	(dired file-name)
      (apply #'org-open-file
	     file-name
	     arg
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


;;; Built-in link types

;;;; "doi" link type
(defun org-link--open-doi (path arg)
  "Open a \"doi\" type link.
PATH is a the path to search for, as a string."
  (browse-url (url-encode-url (concat org-link-doi-server-url path)) arg))

(org-link-set-parameters "doi" :follow #'org-link--open-doi)

;;;; "elisp" link type
(defun org-link--open-elisp (path _)
  "Open a \"elisp\" type link.
PATH is the sexp to evaluate, as a string."
  (if (or (and (org-string-nw-p org-link-elisp-skip-confirm-regexp)
	       (string-match-p org-link-elisp-skip-confirm-regexp path))
	  (not org-link-elisp-confirm-function)
	  (funcall org-link-elisp-confirm-function
		   (format "Execute %S as Elisp? "
			   (org-add-props path nil 'face 'org-warning))))
      (message "%s => %s" path
	       (if (eq ?\( (string-to-char path))
		   (eval (read path))
		 (call-interactively (read path))))
    (user-error "Abort")))

(org-link-set-parameters "elisp" :follow #'org-link--open-elisp)

;;;; "file" link type
(org-link-set-parameters "file" :complete #'org-link-complete-file)

;;;; "help" link type
(defun org-link--open-help (path _)
  "Open a \"help\" type link.
PATH is a symbol name, as a string."
  (pcase (intern path)
    ((and (pred fboundp) variable) (describe-function variable))
    ((and (pred boundp) function) (describe-variable function))
    (name (user-error "Unknown function or variable: %s" name))))

(org-link-set-parameters "help" :follow #'org-link--open-help)

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
		   (format "Execute %S in shell? "
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
	  (pcase (org-element-lineage context '(link) t)
	    (`nil nil)
	    (link
	     (goto-char (org-element-property :begin link))
	     (when (org-invisible-p) (org-show-context))
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
  "Toggle the literal or descriptive display of links."
  (interactive)
  (if org-link-descriptive (remove-from-invisibility-spec '(org-link))
    (add-to-invisibility-spec '(org-link)))
  (org-restart-font-lock)
  (setq org-link-descriptive (not org-link-descriptive)))

;;;###autoload
(defun org-store-link (arg &optional interactive?)
  "Store a link to the current location.
\\<org-mode-map>
This link is added to `org-stored-links' and can later be inserted
into an Org buffer with `org-insert-link' (`\\[org-insert-link]').

For some link types, a `\\[universal-argument]' prefix ARG is interpreted.  \
A single
`\\[universal-argument]' negates `org-context-in-file-links' for file links or
`org-gnus-prefer-web-links' for links to Usenet articles.

A `\\[universal-argument] \\[universal-argument]' prefix ARG forces \
skipping storing functions that are not
part of Org core.

A `\\[universal-argument] \\[universal-argument] \\[universal-argument]' \
prefix ARG forces storing a link for each line in the
active region.

Assume the function is called interactively if INTERACTIVE? is
non-nil."
  (interactive "P\np")
  (org-load-modules-maybe)
  (if (and (equal arg '(64)) (org-region-active-p))
      (save-excursion
	(let ((end (region-end)))
	  (goto-char (region-beginning))
	  (set-mark (point))
	  (while (< (point-at-eol) end)
	    (move-end-of-line 1) (activate-mark)
	    (let (current-prefix-arg)
	      (call-interactively 'org-store-link))
	    (move-beginning-of-line 2)
	    (set-mark (point)))))
    (setq org-store-link-plist nil)
    (let (link cpltxt desc description search custom-id agenda-link)
      (cond
       ;; Store a link using an external link type, if any function is
       ;; available. If more than one can generate a link from current
       ;; location, ask which one to use.
       ((and (not (equal arg '(16)))
	     (let ((results-alist nil))
	       (dolist (f (org-store-link-functions))
		 (when (funcall f)
		   ;; XXX: return value is not link's plist, so we
		   ;; store the new value before it is modified.  It
		   ;; would be cleaner to ask store link functions to
		   ;; return the plist instead.
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
			       (completing-read
				"Which function for creating the link? "
				(mapcar #'car results-alist)
				nil t (symbol-name name))
			       results-alist)))
		  t))))
	(setq link (plist-get org-store-link-plist :link))
	(setq desc (or (plist-get org-store-link-plist :description)
		       link)))

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
	      (beginning-of-line)
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
       ((equal (bound-and-true-p org-agenda-buffer-name) (buffer-name))
	(let ((m (or (get-text-property (point) 'org-hd-marker)
		     (get-text-property (point) 'org-marker))))
	  (when m
	    (org-with-point-at m
	      (setq agenda-link (org-store-link nil interactive?))))))

       ((eq major-mode 'calendar-mode)
	(let ((cd (calendar-cursor-to-date)))
	  (setq link
		(format-time-string
		 (car org-time-stamp-formats)
		 (apply 'encode-time
			(list 0 0 0 (nth 1 cd) (nth 0 cd) (nth 2 cd)
			      nil nil nil))))
	  (org-link-store-props :type "calendar" :date cd)))

       ((eq major-mode 'help-mode)
	(let ((symbol (replace-regexp-in-string
		       ;; Help mode escapes backquotes and backslashes
		       ;; before displaying them.  E.g., "`" appears
		       ;; as "\'" for reasons.  Work around this.
		       (rx "\\" (group (or "`" "\\"))) "\\1"
		       (save-excursion
			 (goto-char (point-min))
			 (looking-at "^[^ ]+")
			 (match-string 0)))))
	  (setq link (concat "help:" symbol)))
	(org-link-store-props :type "help"))

       ((eq major-mode 'w3-mode)
	(setq cpltxt (if (and (buffer-name)
			      (not (string-match "Untitled" (buffer-name))))
			 (buffer-name)
		       (url-view-url t))
	      link (url-view-url t))
	(org-link-store-props :type "w3" :url (url-view-url t)))

       ((eq major-mode 'image-mode)
	(setq cpltxt (concat "file:"
			     (abbreviate-file-name buffer-file-name))
	      link cpltxt)
	(org-link-store-props :type "image" :file buffer-file-name))

       ;; In dired, store a link to the file of the current line
       ((derived-mode-p 'dired-mode)
	(let ((file (dired-get-filename nil t)))
	  (setq file (if file
			 (abbreviate-file-name
			  (expand-file-name (dired-get-filename nil t)))
		       ;; otherwise, no file so use current directory.
		       default-directory))
	  (setq cpltxt (concat "file:" file)
		link cpltxt)))

       ((setq search (run-hook-with-args-until-success
		      'org-create-file-search-functions))
	(setq link (concat "file:" (abbreviate-file-name buffer-file-name)
			   "::" search))
	(setq cpltxt (or description link)))

       ((and (buffer-file-name (buffer-base-buffer)) (derived-mode-p 'org-mode))
	(org-with-limited-levels
	 (setq custom-id (org-entry-get nil "CUSTOM_ID"))
	 (cond
	  ;; Store a link using the target at point
	  ((org-in-regexp "[^<]<<\\([^<>]+\\)>>[^>]" 1)
	   (setq cpltxt
		 (concat "file:"
			 (abbreviate-file-name
			  (buffer-file-name (buffer-base-buffer)))
			 "::" (match-string 1))
		 link cpltxt))
	  ((and (featurep 'org-id)
		(or (eq org-id-link-to-org-use-id t)
		    (and interactive?
			 (or (eq org-id-link-to-org-use-id 'create-if-interactive)
			     (and (eq org-id-link-to-org-use-id
				      'create-if-interactive-and-no-custom-id)
				  (not custom-id))))
		    (and org-id-link-to-org-use-id (org-entry-get nil "ID"))))
	   ;; Store a link using the ID at point
	   (setq link (condition-case nil
			  (prog1 (org-id-store-link)
			    (setq desc (or (plist-get org-store-link-plist
						      :description)
					   "")))
			(error
			 ;; Probably before first headline, link only to file
			 (concat "file:"
				 (abbreviate-file-name
				  (buffer-file-name (buffer-base-buffer))))))))
	  (t
	   ;; Just link to current headline.
	   (setq cpltxt (concat "file:"
				(abbreviate-file-name
				 (buffer-file-name (buffer-base-buffer)))))
	   ;; Add a context search string.
	   (when (org-xor org-link-context-for-files (equal arg '(4)))
	     (let* ((element (org-element-at-point))
		    (name (org-element-property :name element))
		    (context
		     (cond
		      ((let ((region (org-link--context-from-region)))
			 (and region (org-link--normalize-string region t))))
		      (name)
		      ((org-before-first-heading-p)
		       (org-link--normalize-string (org-current-line-string) t))
		      (t (org-link-heading-search-string)))))
	       (when (org-string-nw-p context)
		 (setq cpltxt (format "%s::%s" cpltxt context))
		 (setq desc
		       (or name
			   ;; Although description is not a search
			   ;; string, use `org-link--normalize-string'
			   ;; to prettify it (contiguous white spaces)
			   ;; and remove volatile contents (statistics
			   ;; cookies).
			   (and (not (org-before-first-heading-p))
				(org-link--normalize-string
				 (org-get-heading t t t t)))
			   "NONE")))))
	   (setq link cpltxt)))))

       ((buffer-file-name (buffer-base-buffer))
	;; Just link to this file here.
	(setq cpltxt (concat "file:"
			     (abbreviate-file-name
			      (buffer-file-name (buffer-base-buffer)))))
	;; Add a context search string.
	(when (org-xor org-link-context-for-files (equal arg '(4)))
	  (let ((context (org-link--normalize-string
			  (or (org-link--context-from-region)
			      (org-current-line-string))
			  t)))
	    ;; Only use search option if there is some text.
	    (when (org-string-nw-p context)
	      (setq cpltxt (format "%s::%s" cpltxt context))
	      (setq desc "NONE"))))
	(setq link cpltxt))

       (interactive?
	(user-error "No method for storing a link from this buffer"))

       (t (setq link nil)))

      ;; We're done setting link and desc, clean up
      (when (consp link) (setq cpltxt (car link) link (cdr link)))
      (setq link (or link cpltxt)
	    desc (or desc cpltxt))
      (cond ((not desc))
	    ((equal desc "NONE") (setq desc nil))
	    (t (setq desc (org-link-display-format desc))))
      ;; Store and return the link
      (if (not (and interactive? link))
	  (or agenda-link (and link (org-link-make-string link desc)))
	(if (member (list link desc) org-stored-links)
	    (message "This link already exists")
	  (push (list link desc) org-stored-links)
	  (message "Stored: %s" (or desc link))
	  (when custom-id
	    (setq link (concat "file:"
			       (abbreviate-file-name
				(buffer-file-name (buffer-base-buffer)))
			       "::#" custom-id))
	    (push (list link desc) org-stored-links)))
	(car org-stored-links)))))

;;;###autoload
(defun org-insert-link (&optional complete-file link-location description)
  "Insert a link.  At the prompt, enter the link.

Completion can be used to insert any of the link protocol prefixes in use.

The history can be used to select a link previously stored with
`org-store-link'.  When the empty string is entered (i.e. if you just
press `RET' at the prompt), the link defaults to the most recently
stored link.  As `SPC' triggers completion in the minibuffer, you need to
use `M-SPC' or `C-q SPC' to force the insertion of a space character.

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

If the DESCRIPTION parameter is non-nil, this value will be used as the
default description.  Otherwise, if `org-link-make-description-function'
is non-nil, this function will be called with the link target, and the
result will be the default link description.  When called non-interactively,
don't allow to edit the default description."
  (interactive "P")
  (let* ((wcf (current-window-configuration))
	 (origbuf (current-buffer))
	 (region (when (org-region-active-p)
		   (buffer-substring (region-beginning) (region-end))))
	 (remove (and region (list (region-beginning) (region-end))))
	 (desc region)
	 (link link-location)
	 (abbrevs org-link-abbrev-alist-local)
	 entry all-prefixes auto-desc)
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
      (org-switch-to-buffer-other-window "*Org Links*")
      (with-current-buffer "*Org Links*"
	(erase-buffer)
	(insert "Insert a link.
Use TAB to complete link prefixes, then RET for type-specific completion support\n")
	(when org-stored-links
	  (insert "\nStored links are available with <up>/<down> or M-p/n \
\(most recent with RET):\n\n")
	  (insert (mapconcat #'org-link--prettify
			     (reverse org-stored-links)
			     "\n")))
	(goto-char (point-min)))
      (let ((cw (selected-window)))
	(select-window (get-buffer-window "*Org Links*" 'visible))
	(with-current-buffer "*Org Links*" (setq truncate-lines t))
	(unless (pos-visible-in-window-p (point-max))
	  (org-fit-window-to-buffer))
	(and (window-live-p cw) (select-window cw)))
      (setq all-prefixes (append (mapcar #'car abbrevs)
				 (mapcar #'car org-link-abbrev-alist)
				 (org-link-types)))
      (unwind-protect
	  ;; Fake a link history, containing the stored links.
	  (let ((org-link--history
		 (append (mapcar #'car org-stored-links)
			 org-link--insert-history)))
	    (setq link
		  (org-completing-read
		   "Link: "
		   (append
		    (mapcar (lambda (x) (concat x ":")) all-prefixes)
		    (mapcar #'car org-stored-links))
		   nil nil nil
		   'org-link--history
		   (caar org-stored-links)))
	    (unless (org-string-nw-p link) (user-error "No link selected"))
	    (dolist (l org-stored-links)
	      (when (equal link (cadr l))
		(setq link (car l))
		(setq auto-desc t)))
	    (when (or (member link all-prefixes)
		      (and (equal ":" (substring link -1))
			   (member (substring link 0 -1) all-prefixes)
			   (setq link (substring link 0 -1))))
	      (setq link (with-current-buffer origbuf
			   (org-link--try-special-completion link)))))
	(set-window-configuration wcf)
	(kill-buffer "*Org Links*"))
      (setq entry (assoc link org-stored-links))
      (or entry (push link org-link--insert-history))
      (setq desc (or desc (nth 1 entry)))))

    (when (funcall (if (equal complete-file '(64)) 'not 'identity)
		   (not org-link-keep-stored-after-insertion))
      (setq org-stored-links (delq (assoc link org-stored-links)
				   org-stored-links)))

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
	       (search (and (string-match "::\\(.*\\)\\'" link)
			    (match-string 1 link)))
	       (path
		(if search
		    (substring-no-properties
		     link path-start (match-beginning 0))
		  (substring-no-properties link (match-end 0))))
	       (origpath path))
	  (cond
	   ((or (eq org-link-file-path-type 'absolute)
		(equal complete-file '(16)))
	    (setq path (abbreviate-file-name (expand-file-name path))))
	   ((eq org-link-file-path-type 'noabbrev)
	    (setq path (expand-file-name path)))
	   ((eq org-link-file-path-type 'relative)
	    (setq path (file-relative-name path)))
	   (t
	    (save-match-data
	      (if (string-match (concat "^" (regexp-quote
					     (expand-file-name
					      (file-name-as-directory
					       default-directory))))
				(expand-file-name path))
		  ;; We are linking a file with relative path name.
		  (setq path (substring (expand-file-name path)
					(match-end 0)))
		(setq path (abbreviate-file-name (expand-file-name path)))))))
	  (setq link (concat type path (and search (concat "::" search))))
	  (when (equal desc origpath)
	    (setq desc path)))))

    (unless auto-desc
      (let ((initial-input
	     (cond
	      (description)
	      ((not org-link-make-description-function) desc)
	      (t (condition-case nil
		     (funcall org-link-make-description-function link desc)
		   (error
		    (message "Can't get link description from %S"
			     (symbol-name org-link-make-description-function))
		    (sit-for 2)
		    nil))))))
	(setq desc (if (called-interactively-p 'any)
		       (read-string "Description: " initial-input)
		     initial-input))))

    (unless (org-string-nw-p desc) (setq desc nil))
    (when remove (apply #'delete-region remove))
    (insert (org-link-make-string link desc))
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

;;;###autoload
(defun org-update-radio-target-regexp ()
  "Find all radio targets in this file and update the regular expression.
Also refresh fontification if needed."
  (interactive)
  (let ((old-regexp org-target-link-regexp)
	;; Some languages, e.g., Chinese, do not use spaces to
	;; separate words.  Also allow to surround radio targets with
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
		(when (eq (org-element-type obj) 'radio-target)
		  (cl-pushnew (org-element-property :value obj) rtn
			      :test #'equal))))
	    rtn))))
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
	(when (featurep 'org-element)
	  (org-with-point-at 1
	    (while (re-search-forward regexp nil t)
	      (org-element-cache-refresh (match-beginning 1))))))
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
