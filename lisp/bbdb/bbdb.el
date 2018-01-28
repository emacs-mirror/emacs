;;; bbdb.el --- core of BBDB -*- lexical-binding: t -*-

;; Copyright (C) 2010-2017  Free Software Foundation, Inc.

;; Version: 3.2
;; Package-Requires: ((emacs "24"))

;; This file is part of the Insidious Big Brother Database (aka BBDB),

;; BBDB is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; BBDB is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with BBDB.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file is the core of the Insidious Big Brother Database (aka BBDB),
;; See the BBDB info manual for documentation.
;;
;;  -----------------------------------------------------------------------
;; |  There is a mailing list for discussion of BBDB:                      |
;; |         bbdb-user@nongnu.org                                          |
;; |  To join, go to https://lists.nongnu.org/mailman/listinfo/bbdb-user   |
;; |                                                                       |
;; |  When joining this list or reporting bugs, please mention which       |
;; |  version of BBDB you have.                                            |
;;  -----------------------------------------------------------------------

;;; Code:

(require 'timezone)
(require 'bbdb-site)

;; When running BBDB, we have (require 'bbdb-autoloads)
(declare-function widget-group-match "wid-edit")
(declare-function bbdb-migrate "bbdb-migrate")
(declare-function bbdb-do-records "bbdb-com")
(declare-function bbdb-append-display-p "bbdb-com")
(declare-function bbdb-toggle-records-layout "bbdb-com")
(declare-function bbdb-dwim-mail "bbdb-com")
(declare-function bbdb-layout-prefix "bbdb-com")
(declare-function bbdb-completing-read-records "bbdb-com")
(declare-function bbdb-merge-records "bbdb-com")
(declare-function mail-position-on-field "sendmail")
(declare-function vm-select-folder-buffer "vm-folder")

;; cannot use autoload for variables...
(defvar message-mode-map) ;; message.el
(defvar mail-mode-map) ;; sendmail.el
(defvar gnus-article-buffer) ;; gnus-art.el

;; Custom groups

(defgroup bbdb nil
  "The Insidious Big Brother Database."
  :group 'news
  :group 'mail)

(defgroup bbdb-record-display nil
  "Variables that affect the display of BBDB records"
  :group 'bbdb)

(defgroup bbdb-record-edit nil
  "Variables that affect the editing of BBDB records"
  :group 'bbdb)

(defgroup bbdb-sendmail nil
  "Variables that affect sending mail."
  :group 'bbdb)

(defgroup bbdb-mua nil
  "Variables that specify the BBDB-MUA interface"
  :group 'bbdb)

(defgroup bbdb-mua-gnus nil
  "Gnus-specific BBDB customizations"
  :group 'bbdb-mua)
(put 'bbdb-mua-gnus 'custom-loads '(bbdb-gnus))

(defgroup bbdb-mua-gnus-scoring nil
  "Gnus-specific scoring BBDB customizations"
  :group 'bbdb-mua-gnus)
(put 'bbdb-mua-gnus-scoring 'custom-loads '(bbdb-gnus))

(defgroup bbdb-mua-gnus-splitting nil
  "Gnus-specific splitting BBDB customizations"
  :group 'bbdb-mua-gnus)
(put 'bbdb-mua-gnus-splitting 'custom-loads '(bbdb-gnus))

(defgroup bbdb-mua-vm nil
  "VM-specific BBDB customizations"
  :group 'bbdb-mua)
(put 'bbdb-mua-vm 'custom-loads '(bbdb-vm))

(defgroup bbdb-mua-message nil
  "Message-specific BBDB customizations"
  :group 'bbdb-mua)
(put 'bbdb-mua-message 'custom-loads '(bbdb-message))

(defgroup bbdb-utilities nil
  "Customizations for BBDB Utilities"
  :group 'bbdb)

(defgroup bbdb-utilities-dialing nil
  "BBDB Customizations for phone number dialing"
  :group 'bbdb)

(defgroup bbdb-utilities-tex nil
  "Customizations for TeXing BBDB."
  :group 'bbdb)
(put 'bbdb-utilities-tex 'custom-loads '(bbdb-tex))

(defgroup bbdb-utilities-anniv nil
  "Customizations for BBDB Anniversaries"
  :group 'bbdb-utilities)
(put 'bbdb-utilities-anniv 'custom-loads '(bbdb-anniv))

(defgroup bbdb-utilities-ispell nil
  "Customizations for BBDB ispell interface"
  :group 'bbdb-utilities)
(put 'bbdb-utilities-ispell 'custom-loads '(bbdb-ispell))

(defgroup bbdb-utilities-snarf nil
  "Customizations for BBDB snarf"
  :group 'bbdb-utilities)
(put 'bbdb-utilities-snarf 'custom-loads '(bbdb-snarf))

(defgroup bbdb-utilities-pgp nil
  "Customizations for BBDB pgp"
  :group 'bbdb-utilities)
(put 'bbdb-utilities-pgp 'custom-loads '(bbdb-pgp))

(defgroup bbdb-utilities-sc nil
  "Customizations for using Supercite with the BBDB."
  :group 'bbdb-utilities
  :prefix "bbdb-sc")
(put 'bbdb-utilities-sc 'custom-loads '(bbdb-sc))

;;; Customizable variables
(defcustom bbdb-file (locate-user-emacs-file "bbdb" ".bbdb")
  "The name of the Insidious Big Brother Database file."
  :group 'bbdb
  :type 'file)

;; This should be removed, and the following put in place:
;; a hierarchical structure of bbdb files, some perhaps read-only,
;; perhaps caching in the local bbdb. This way one could have, e.g. an
;; organization address book, with each person having access to it, and
;; then a local address book with personal stuff in it.
(defcustom bbdb-file-remote nil
  "The remote file to save the BBDB database to.
When this is non-nil, it should be a file name.
When BBDB reads `bbdb-file', it also checks this file,
and if it is newer than `bbdb-file', it loads this file.
When BBDB writes `bbdb-file', it also writes this file.

This feature allows one to keep the database in one place while using
different computers, thus reducing the need for merging different files."
  :group 'bbdb
  :type '(choice (const :tag "none" nil)
                 (file :tag "remote file name")))

(defcustom bbdb-file-remote-save-always t
  "If t `bbdb-file-remote' is saved automatically when `bbdb-file' is saved.
When nil, ask."
  :group 'bbdb
  :type 'boolean)

(defcustom bbdb-read-only nil
  "If t then BBDB will not modify `bbdb-file'.
If you have more than one Emacs running at the same time, you might want
to set this to t in all but one of them."
  :group 'bbdb
  :type '(choice (const :tag "Database is read-only" t)
                 (const :tag "Database is writable" nil)))

(defcustom bbdb-auto-revert nil
  "If t revert unchanged database without querying.
If t and `bbdb-file' has changed on disk, while the database
has not been modified inside Emacs, revert the database automatically.
If nil or the database has been changed inside Emacs, always query
before reverting."
  :group 'bbdb
  :type '(choice (const :tag "Revert unchanged database without querying" t)
                 (const :tag "Ask before reverting database" nil)))

(defcustom bbdb-check-auto-save-file nil
  "If t BBDB will check its auto-save file.
If this file is newer than `bbdb-file', BBDB will offer to revert."
  :group 'bbdb
  :type '(choice (const :tag "Check auto-save file" t)
                 (const :tag "Do not check auto-save file" nil)))

(defcustom bbdb-before-save-hook nil
  "Hook run before saving `bbdb-file'."
  :group 'bbdb
  :type 'hook)

(defcustom bbdb-after-save-hook nil
  "Hook run after saving `bbdb-file'."
  :group 'bbdb
  :type 'hook)

(defcustom bbdb-create-hook nil
  "*Hook run each time a new BBDB record is created.
Run with one argument, the new record.  This is called before the record is
added to the database, followed by a call of `bbdb-change-hook'.

If a record has been created by analyzing a mail message, hook functions
can use the variable `bbdb-update-records-address' to determine the header
and class of the mail address according to `bbdb-message-headers'."
  :group 'bbdb
  :type 'hook)

(defcustom bbdb-change-hook nil
  "*Hook run each time a BBDB record is changed.
Run with one argument, the record.  This is called before the database
is modified.  If a new bbdb record is created, `bbdb-create-hook' is called
first, followed by a call of this hook."
  :group 'bbdb
  :type 'hook)

(defcustom bbdb-merge-records-function nil
  "If non-nil, a function for merging two records.
This function is called when loading a record into BBDB that has the same uuid
as an exisiting record.  If nil use `bbdb-merge-records'.
This function should take two arguments RECORD1 and RECORD2, with RECORD2
being the already existing record.  It should merge RECORD1 into RECORD2,
and return RECORD2."
  :group 'bbdb
  :type 'function)

(defcustom bbdb-time-stamp-format "%Y-%m-%d %T %z"
  "The BBDB time stamp format.  See `format-time-string'.
This function is called with arg UNIVERSAL being non-nil."
  :group 'bbdb
  :type 'string)

(defcustom bbdb-after-change-hook nil
  "Hook run each time a BBDB record is changed.
Run with one argument, the record.  This is called after the database
is modified.  So if you want to modify a record when it is created or changed,
use instead `bbdb-create-hook' and / or `bbdb-change-hook'."
  :group 'bbdb
  :type 'hook)

(defcustom bbdb-after-read-db-hook nil
  "Hook run (with no arguments) after `bbdb-file' is read.
Note that this can be called more than once if the BBDB is reverted."
  :group 'bbdb
  :type 'hook)

(defcustom bbdb-initialize-hook nil
  "Normal hook run after the BBDB initialization function `bbdb-initialize'."
  :group 'bbdb
  :type 'hook)

(defcustom bbdb-mode-hook nil
  "Normal hook run when the *BBDB* buffer is created."
  :group 'bbdb
  :type 'hook)

(defcustom bbdb-silent nil
  "If t, BBDB suppresses all its informational messages and queries.
Be very very certain you want to set this to t, because it will suppress
queries to alter record names, assign names to addresses, etc.
Lisp Hackers: See also `bbdb-silent-internal'."
  :group 'bbdb
  :type '(choice (const :tag "Run silently" t)
                 (const :tag "Disable silent running" nil)))

(defcustom bbdb-info-file nil
  "Location of the bbdb info file, if it's not in the standard place."
  :group 'bbdb
  :type '(choice (const :tag "Standard location" nil)
                 (file :tag "Nonstandard location")))


;;; Record display

(defcustom bbdb-pop-up-window-size 0.5
  "Vertical size of BBDB window (vertical split).
If it is an integer number, it is the number of lines used by BBDB.
If it is a fraction between 0.0 and 1.0 (inclusive), it is the fraction
of the tallest existing window that BBDB will take over.
If it is t use `display-buffer'/`pop-to-buffer' to create the BBDB window.
See also `bbdb-mua-pop-up-window-size'."
  :group 'bbdb-record-display
  :type '(choice (number :tag "BBDB window size")
                 (const :tag "Use `pop-to-buffer'" t)))

(defcustom bbdb-dedicated-window nil
  "Make *BBDB* window a dedicated window.
Allowed values include nil (not dedicated) 'bbdb (weakly dedicated)
and t (strongly dedicated)."
  :group 'bbdb-record-display
  :type '(choice (const :tag "BBDB window not dedicated" nil)
                 (const :tag "BBDB window weakly dedicated" bbdb)
                 (const :tag "BBDB window strongly dedicated" t)))

(defcustom bbdb-layout-alist
  '((one-line           (order     . (phone mail-alias mail notes))
                        (name-end  . 24)
                        (toggle    . t))
    (multi-line         (omit      . (uuid creation-date timestamp
                                           name-format name-face))
                        (toggle    . t)
                        (indentation . 21))
    (pop-up-multi-line  (omit      . (uuid creation-date timestamp
                                           name-format name-face))
                        (indentation . 21))
    (full-multi-line    (indentation . 21)))
  "Alist describing each display layout.
The format of an element is (LAYOUT-NAME OPTION-ALIST).

By default there are four different layout types used by BBDB, which are
`one-line', `multi-line', `pop-up-multi-line' (used for pop-ups) and
`full-multi-line' (showing all fields of a record).

OPTION-ALIST specifies the options for the layout.  Valid options are:

                           ------- Availability --------
    Format                  one-line        multi-line        default if unset
------------------------------------------------------------------------------
 (toggle . BOOL)                 +               +              nil
 (order . FIELD-LIST)            +               +              '(phone ...)
 (omit . FIELD-LIST)             +               +              nil
 (name-end . INTEGER)            +               -              40
 (indentation . INTEGER)         -               +              21
 (primary . BOOL)                -               +              nil
 (display-p . FUNCTION)          +               +              nil

- toggle: controls if this layout is included when toggeling the layout
- order: defines a user specific order for the fields, where t is a place
  holder for all remaining fields
- omit: is a list of xfields which should not be displayed
  or t to exclude all xfields except those listed in the order option
- name-end: sets the column where the name should end in one-line layout.
- indentation: sets the level of indentation for multi-line display.
- primary: controls whether only the primary mail is shown or all are shown.
- display-p: a function controlling whether the record is to be displayed.

When you add a new layout FOO, you can write a corresponding layout
function `bbdb-display-record-layout-FOO'.  If you do not write your own
layout function, the multi-line layout will be used."
  :group 'bbdb-record-display
  :type
  `(repeat
    (cons :tag "Layout Definition"
          (choice :tag "Layout type"
                  (const one-line)
                  (const multi-line)
                  (const pop-up-multi-line)
                  (const full-multi-line)
                  (symbol))
          (set :tag "Properties"
               (cons :tag "Order"
                     (const :tag "List of fields to order by" order)
                     (repeat (choice (const phone)
                                     (const address)
                                     (const mail)
                                     (const AKA)
                                     (const notes)
                                     (symbol :tag "other")
                                     (const :tag "Remaining fields" t))))
               (choice :tag "Omit"
                       :value (omit . nil)
                       (cons :tag "List of fields to omit"
                             (const :tag "Fields not to display" omit)
                             (repeat (choice (const phone)
                                             (const address)
                                             (const mail)
                                             (const AKA)
                                             (const notes)
                                             (symbol :tag "other"))))
                       (const :tag "Exclude all fields except those listed in the order note" t))
               (cons :tag "Indentation"
                     :value (indentation . 14)
                     (const :tag "Level of indentation for multi-line layout"
                            indentation)
                     (number :tag "Column"))
               (cons :tag "End of name field"
                     :value (name-end . 24)
                     (const :tag "The column where the name should end in one-line layout"
                            name-end)
                     (number :tag "Column"))
               (cons :tag "Toggle"
                     (const :tag "The layout is included when toggling layout" toggle)
                     boolean)
               (cons :tag "Primary Mail Only"
                     (const :tag "Only the primary mail address is included" primary)
                     boolean)
               (cons :tag "Display-p"
                     (const :tag "Show only records passing this test" display-p)
                     (choice (const :tag "No test" nil)
                             (function :tag "Predicate")))))))

(defcustom bbdb-layout 'multi-line
  "Default display layout."
  :group 'bbdb-record-display
  :type '(choice (const one-line)
                 (const multi-line)
                 (const full-multi-line)
                 (symbol)))

(defcustom bbdb-pop-up-layout 'pop-up-multi-line
  "Default layout for pop-up BBDB buffers (mail, news, etc.)."
  :group 'bbdb-record-display
  :type '(choice (const one-line)
                 (const multi-line)
                 (const full-multi-line)
                 (symbol)))

(defcustom bbdb-wrap-column nil
  "Wrap column for multi-line display.  If nil do not wrap lines."
  :group 'bbdb-record-display
  :type '(choice (const :tag "No line wrapping" nil)
                 (number :tag "Wrap column")))

(defcustom bbdb-case-fold-search (default-value 'case-fold-search)
  "Value of `case-fold-search' used by BBDB and friends.
This variable lets the case-sensitivity of the BBDB commands
be different from standard commands like command `isearch-forward'."
  :group 'bbdb-record-display
  :type 'boolean)

(defcustom bbdb-name-format 'first-last
  "Format for displaying names.
If first-last names are displayed as \"Firstname Lastname\".
If last-first names are displayed as \"Lastname, Firstname\".
This can be overriden per record via the xfield name-format,
which should take the same values.
See also `bbdb-read-name-format'."
  :group 'bbdb-record-display
  :type '(choice (const :tag "Firstname Lastname" first-last)
                 (const :tag "Lastname, Firstname" last-first)))

;; See http://en.wikipedia.org/wiki/Postal_address
;; http://www.upu.int/en/activities/addressing/postal-addressing-systems-in-member-countries.html
(defcustom bbdb-address-format-list
  '((("Argentina") "spcSC" "@%s\n@%p, @%c@, %S@\n%C@" "@%c@")
    (("Australia") "scSpC" "@%s\n@%c@ %S@ %p@\n%C@" "@%c@")
    (("Austria" "Germany" "Spain" "Switzerland")
     "spcSC" "@%s\n@%p @%c@ (%S)@\n%C@" "@%c@")
    (("Canada") "scSCp" "@%s\n@%c@, %S@\n%C@ %p@" "@%c@")
    (("China") "scpSC" "@%s\n@%c@\n%p@ %S@\n%C@" "@%c@") ; English format
    ; (("China") "CpScs" "@%C @%p\n@%S @%c@ %s@" "@%c@") ; Chinese format
    (("India") "scpSC" "@%s\n@%c@ %p@ (%S)@\n%C@" "@%c@")
    (("USA") "scSpC" "@%s\n@%c@, %S@ %p@\n%C@" "@%c@")
    (t bbdb-edit-address-default bbdb-format-address-default "@%c@"))
  "List of address editing and formatting rules for BBDB.
Each rule is a list (IDENTIFIER EDIT FORMAT FORMAT).
The first rule for which IDENTIFIER matches an address is used for editing
and formatting the address.

IDENTIFIER may be a list of countries.
IDENTIFIER may also be a function that is called with one arg, the address
to be used.  The rule applies if the function returns non-nil.
See `bbdb-address-continental-p' for an example.
If IDENTIFIER is t, this rule always applies.  Usually, this should be
the last rule that becomes a fall-back (default).

EDIT may be a function that is called with one argument, the address.
See `bbdb-edit-address-default' for an example.

EDIT may also be an editting format string.  It is a string containing
the five letters s, c, p, S, and C that specify the order for editing
the five elements of an address:

s  streets
c  city
p  postcode
S  state
C  country

The first FORMAT of each rule is used for multi-line layout, the second FORMAT
is used for one-line layout.

FORMAT may be a function that is called with one argument, the address.
See `bbdb-format-address-default' for an example.

FORMAT may also be a format string.  It consists of formatting elements
separated by a delimiter defined via the first (and last) character of FORMAT.
Each formatting element may contain one of the following format specifiers:

%s  streets (used repeatedly for each street part)
%c  city
%p  postcode
%S  state
%C  country

A formatting element will be applied only if the corresponding part
of the address is a non-empty string.

See also `bbdb-tex-address-format-list'."
  :group 'bbdb-record-display
  :type '(repeat (list (choice (const :tag "Default" t)
                               (function :tag "Function")
                               (repeat (string)))
                       (choice (string)
                               (function :tag "Function"))
                       (choice (string)
                               (function :tag "Function"))
                       (choice (string)
                               (function :tag "Function")))))

(defcustom bbdb-continental-postcode-regexp
  "^\\s *[A-Z][A-Z]?\\s *-\\s *[0-9][0-9][0-9]"
  "Regexp matching continental postcodes.
Used by address format identifier `bbdb-address-continental-p'.
The regexp should match postcodes of the form CH-8052, NL-2300RA,
and SE-132 54."
  :group 'bbdb-record-display
  :type 'regexp)

(defcustom bbdb-default-separator '("[,;]" ", ")
  "The default field separator.  It is a list (SPLIT-RE JOIN).
This is used for fields which do not have an entry in `bbdb-separator-alist'.
Whitespace surrounding SPLIT-RE is ignored."
  :group 'bbdb-record-display
  :type '(list regexp string))

(defcustom bbdb-separator-alist
  '((record "\n\n" "\n\n") ; used by `bbdb-copy-fields-as-kill'
    (name-first-last "[ ,;]" " ") (name-last-first "[ ,;]" ", ")
    (name-field ":\n" ":\n") ; used by `bbdb-copy-fields-as-kill'
    (phone "[,;]" ", ") (address ";\n" ";\n") ; ditto
    (organization "[,;]" ", ") (affix "[,;]"  ", ") (aka "[,;]" ", ")
    (mail "[,;]" ", ") (mail-alias "[,;]" ", ") (vm-folder "[,;]" ", ")
    (birthday "\n" "\n") (wedding "\n" "\n") (anniversary "\n" "\n")
    (notes "\n" "\n") (tex-name "#" " # "))
  "Alist of field separators.
Each element is of the form (FIELD SPLIT-RE JOIN).
Whitespace surrounding SPLIT-RE is ignored.
For fields lacking an entry here `bbdb-default-separator' is used instead."
  :group 'bbdb-record-display
  :type '(repeat (list symbol regexp string)))

(defcustom bbdb-user-menu-commands nil
  "User defined menu entries which should be appended to the BBDB menu.
This should be a list of menu entries.
When set to a function, it is called with two arguments RECORD and FIELD
and it should either return nil or a list of menu entries.
Used by `bbdb-mouse-menu'."
  :group 'bbdb-record-display
  :type 'sexp)

(defcustom bbdb-display-hook nil
  "Hook run after the *BBDB* is filled in."
  :group 'bbdb-record-display
  :type 'hook)

(defcustom bbdb-multiple-buffers nil
  "When non-nil we create a new buffer of every buffer causing pop-ups.
You can also set this to a function returning a buffer name.
Here a value may be the predefined function `bbdb-multiple-buffers-default'."
  :group 'bbdb-record-display
  :type '(choice (const :tag "Disabled" nil)
                 (function :tag "Enabled" bbdb-multiple-buffers-default)
                 (function :tag "User defined function")))

(defcustom bbdb-image nil
  "If non-nil display records with an image.
If a symbol this should be an xfield holding the name of the image file
associated with the record.  If it is `name' or `fl-name', the first and last
name of the record are used as file name.  If it is `lf-name', the last and
first name of the record are used as file name.
If a function it is called with one arg, the record, and it should return
the name of the image file.
The file is searched in the directories in `bbdb-image-path'.
File name suffixes are appended according to `bbdb-image-suffixes'.
See `locate-file'."
  :group 'bbdb-record-display
  :type '(choice (const :tag "Disabled" nil)
                 (function :tag "User defined function")
                 (symbol :tag "Record field")))

(defcustom bbdb-image-path nil
  "List of directories to search for `bbdb-image'."
  :group 'bbdb-record-display
  :type '(repeat (directory)))

(defcustom bbdb-image-suffixes '(".png" ".jpg" ".gif" ".xpm")
  "List of file name suffixes searched for `bbdb-image'."
  :group 'bbdb-record-display
  :type '(repeat (string :tag "File suffix")))

(defcustom bbdb-read-name-format 'fullname
  "Default format for reading names via `bbdb-read-name'.
If it is 'first-last read first and last name separately.
If it is 'last-first read last and first name separately.
With any other value read full name at once.
See also `bbdb-name-format'."
  :group 'bbdb-record-display
  :type '(choice (const :tag "Firstname Lastname" first-last)
                 (const :tag "Lastname, Firstname" last-first)
                 (const :tag "Full name" fullname)))


;;; Record editing
(defcustom bbdb-lastname-prefixes
 '("von" "de" "di")
  "List of lastname prefixes recognized in name fields.
Used to enhance dividing name strings into firstname and lastname parts.
Case is ignored."
  :group 'bbdb-record-edit
  :type '(repeat string))

(defcustom bbdb-lastname-re
  (concat "[- \t]*\\(\\(?:\\<"
          (regexp-opt bbdb-lastname-prefixes)
          ;; multiple last names concatenated by `-'
          "\\>[- \t]+\\)?\\(?:\\w+[ \t]*-[ \t]*\\)*\\w+\\)\\'")
  "Regexp matching the last name of a full name.
Its first parenthetical subexpression becomes the last name."
  :group 'bbdb-record-edit
  :type 'regexp)

(defcustom bbdb-lastname-suffixes
 '("Jr" "Sr" "II" "III")
  "List of lastname suffixes recognized in name fields.
Used to dividing name strings into firstname and lastname parts.
All suffixes are complemented by optional `.'.  Case is ignored."
  :group 'bbdb-record-edit
  :type '(repeat string))

(defcustom bbdb-lastname-suffix-re
  (concat "[-,. \t/\\]+\\("
          (regexp-opt bbdb-lastname-suffixes)
          ;; suffices are complemented by optional `.'.
          "\\.?\\)\\W*\\'")
  "Regexp matching the suffix of a last name.
Its first parenthetical subexpression becomes the suffix."
  :group 'bbdb-record-edit
  :type 'regexp)

(defcustom bbdb-default-domain nil
  "Default domain to append when reading a new mail address.
If a mail address does not contain `[@%!]', append @`bbdb-default-domain' to it.

The address is not altered if `bbdb-default-domain' is nil
or if a prefix argument is given to the command `bbdb-insert-field'."
  :group 'bbdb-record-edit
  :type '(choice (const :tag "none" nil)
                 (string :tag "Default Domain")))

(defcustom bbdb-phone-style 'nanp
  "Phone numbering plan assumed by BBDB.
The value 'nanp refers to the North American Numbering Plan.
The value nil refers to a free-style numbering plan.

You can have both styles of phone number in your database by providing a
prefix argument to the command `bbdb-insert-field'."
  :group 'bbdb-record-edit
  :type '(choice (const :tag "NANP" nanp)
                 (const :tag "none" nil)))

(defcustom bbdb-default-area-code nil
  "Default area code to use when reading a new phone number.
This variable also affects dialing."
  :group 'bbdb-record-edit
  :type '(choice (const :tag "none" nil)
                 (integer :tag "Default Area Code"))
  :set (lambda( symb val )
         (if (or (and (stringp val)
                      (string-match "^[0-9]+$" val))
                 (integerp val)
                 (null val))
             (set symb val)
           (error "%s must contain digits only." symb))))

(defcustom bbdb-allow-duplicates nil
  "When non-nil BBDB allows records with duplicate names and email addresses.
In rare cases, this may lead to confusion with BBDB's MUA interface."
  :group 'bbdb-record-edit
  :type 'boolean)

(defcustom bbdb-default-label-list '("home" "work" "other")
  "Default list of labels for Address and Phone fields."
  :group 'bbdb-record-edit
  :type '(repeat string))

(defcustom bbdb-address-label-list bbdb-default-label-list
  "List of labels for Address field."
  :group 'bbdb-record-edit
  :type '(repeat string))

(defcustom bbdb-phone-label-list '("home" "work" "cell" "other")
  "List of labels for Phone field."
  :group 'bbdb-record-edit
  :type '(repeat string))

(defcustom bbdb-default-country "Emacs";; what do you mean, it's not a country?
  "Default country to use if none is specified."
  :group 'bbdb-record-edit
  :type '(choice (const :tag "None" nil)
                 (string :tag "Default Country")))

(defcustom bbdb-check-postcode t
  "If non-nil, require legal postcodes when entering an address.
The format of legal postcodes is determined by the variable
`bbdb-legal-postcodes'."
  :group 'bbdb-record-edit
  :type 'boolean)

(defcustom bbdb-legal-postcodes
  '(;; empty string
    "^$"
    ;; Matches 1 to 6 digits.
    "^[ \t\n]*[0-9][0-9]?[0-9]?[0-9]?[0-9]?[0-9]?[ \t\n]*$"
    ;; Matches 5 digits and 3 or 4 digits.
    "^[ \t\n]*\\([0-9][0-9][0-9][0-9][0-9]\\)[ \t\n]*-?[ \t\n]*\\([0-9][0-9][0-9][0-9]?\\)[ \t\n]*$"
    ;; Match postcodes for Canada, UK, etc. (result is ("LL47" "U4B")).
    "^[ \t\n]*\\([A-Za-z0-9]+\\)[ \t\n]+\\([A-Za-z0-9]+\\)[ \t\n]*$"
    ;; Match postcodes for continental Europe.  Examples "CH-8057"
    ;; or "F - 83320" (result is ("CH" "8057") or ("F" "83320")).
    ;; Support for "NL-2300RA" added at request from Carsten Dominik
    ;; <dominik@astro.uva.nl>
    "^[ \t\n]*\\([A-Z]+\\)[ \t\n]*-?[ \t\n]*\\([0-9]+ ?[A-Z]*\\)[ \t\n]*$"
    ;; Match postcodes from Sweden where the five digits are grouped 3+2
    ;; at the request from Mats Lofdahl <MLofdahl@solar.stanford.edu>.
    ;; (result is ("SE" (133 36)))
    "^[ \t\n]*\\([A-Z]+\\)[ \t\n]*-?[ \t\n]*\\([0-9]+\\)[ \t\n]+\\([0-9]+\\)[ \t\n]*$")
  "List of regexps that match legal postcodes.
Whether this is used at all depends on the variable `bbdb-check-postcode'."
  :group 'bbdb-record-edit
  :type '(repeat regexp))

(defcustom bbdb-default-xfield 'notes
  "Default xfield when editing BBDB records."
  :group 'bbdb-record-edit
  :type '(symbol :tag "Xfield"))

(defcustom bbdb-edit-foo (cons bbdb-default-xfield 'current-fields)
  "Fields to edit with command `bbdb-edit-foo'.
This is a cons pair (WITHOUT-PREFIX . WITH-PREFIX).
The car is used if the command is called without a prefix.
The cdr is used if the command is called with a prefix.

WITHOUT-PREFIX and WITH-PREFIX may take the values:
 name            The full name
 affix           The list of affixes
 organization    The list of organizations
 aka             the list of AKAs
 mail            the list of email addresses
 phone           the list of phone numbers
 address         the list of addresses
 current-fields  Read the field to edit using a completion table
                   that includes all fields of the current record.
 all-fields      Read the field to edit using a completion table
                   that includes all fields currently known to BBDB.

Any other symbol is interpreted as the label of an xfield."
  :group 'bbdb-record-edit
  :type '(cons (symbol :tag "Field without prefix")
               (symbol :tag "Field with prefix")))


;;; MUA interface

(defcustom bbdb-annotate-field bbdb-default-xfield
  "Field to annotate via `bbdb-annotate-record' and friends.
This may take the values:
 affix           The list of affixes
 organization    The list of organizations
 aka             the list of AKAs
 mail            the list of email addresses
 all-fields      Read the field to edit using a completion table
                   that includes all fields currently known to BBDB.

Any other symbol is interpreted as the label of an xfield."
  :group 'bbdb-mua
  :type '(symbol :tag "Field to annotate"))

(defcustom bbdb-mua-edit-field bbdb-default-xfield
  "Field to edit with command `bbdb-mua-edit-field' and friends.
This may take the values:
 name            The full name
 affix           The list of affixes
 organization    The list of organizations
 aka             the list of AKAs
 mail            the list of email addresses
 all-fields      Read the field to edit using a completion table
                   that includes all fields currently known to BBDB.

Any other symbol is interpreted as the label of an xfield."
  :group 'bbdb-mua
  :type '(symbol :tag "Field to edit"))

(defcustom bbdb-mua-update-interactive-p '(search . query)
  "How BBDB's interactive MUA commands update BBDB records.
This is a cons pair (WITHOUT-PREFIX . WITH-PREFIX).
The car is used if the command is called without a prefix.
The cdr is used if the command is called with a prefix (and if the prefix
        is not used for another purpose).

WITHOUT-PREFIX and WITH-PREFIX may take the values
\(here ADDRESS is an email address found in a message):
 nil          Do nothing.
 search       Search for existing records matching ADDRESS.
 update       Search for existing records matching ADDRESS;
                update name and mail field if necessary.
 query        Search for existing records matching ADDRESS;
                query for creation of a new record if the record does not exist.
 create or t  Search for existing records matching ADDRESS;
                create a new record if it does not yet exist.
 a function   This functions will be called with no arguments.
                It should return one of the above values.
 read         Read the value interactively."
  :group 'bbdb-mua
  :type '(cons (choice (const :tag "do nothing" nil)
                       (const :tag "search for existing records" search)
                       (const :tag "update existing records" update)
                       (const :tag "query annotation of all messages" query)
                       (const :tag "annotate all messages" create)
                       (function :tag "User-defined function")
                       (const :tag "read arg interactively" read))
               (choice (const :tag "do nothing" nil)
                       (const :tag "search for existing records" search)
                       (const :tag "update existing records" update)
                       (const :tag "query annotation of all messages" query)
                       (const :tag "annotate all messages" create)
                       (function :tag "User-defined function")
                       (const :tag "read arg interactively" read))))

(defcustom bbdb-mua-auto-update-p 'bbdb-select-message
  "How `bbdb-mua-auto-update' updates BBDB records automatically.

Allowed values are (here ADDRESS is an email address found in a message):
 nil          Do nothing.
 search       Search for existing records matching ADDRESS.
 update       Search for existing records matching ADDRESS;
                update name and mail field if necessary.
 query        Search for existing records matching ADDRESS;
                query for creation of a new record if the record does not exist.
 create or t  Search for existing records matching ADDRESS;
                create a new record if it does not yet exist.
 a function   This functions will be called with no arguments.
                It should return one of the above values.
                For an example, see `bbdb-select-message' with
                `bbdb-mua-update-records-p', `bbdb-accept-message-alist'
                and `bbdb-ignore-message-alist'.

To initiate auto-updating of BBDB records, call `bbdb-mua-auto-update-init'
for the respective MUAs in your init file."
  :group 'bbdb-mua
  :type '(choice (const :tag "do nothing" nil)
                 (const :tag "search for existing records" search)
                 (const :tag "update existing records" update)
                 (const :tag "query annotation of all messages" query)
                 (const :tag "annotate all messages" create)
                 (function :tag "User-defined function")))

(defcustom bbdb-update-records-p 'search
  "Return value for `bbdb-select-message' and friends.
These functions can select messages for further processing by BBDB,
The amount of subsequent processing is determined by `bbdb-update-records-p'.

Allowed values are (here ADDRESS is an email address selected
by `bbdb-select-message'):
 nil          Do nothing.
 search       Search for existing records matching ADDRESS.
 update       Search for existing records matching ADDRESS;
                update name and mail field if necessary.
 query        Search for existing records matching ADDRESS;
                query for creation of a new record if the record does not exist.
 create or t  Search for existing records matching ADDRESS;
                create a new record if it does not yet exist.
 a function   This functions will be called with no arguments.
                It should return one of the above values."
  ;; Also: Used for communication between `bbdb-update-records'
  ;; and `bbdb-query-create'.
  :group 'bbdb-mua
  :type '(choice (const :tag "do nothing" nil)
                 (const :tag "search for existing records" search)
                 (const :tag "update existing records" update)
                 (const :tag "query annotation of all messages" query)
                 (const :tag "annotate all messages" create)
                 (function :tag "User-defined function")))

(defcustom bbdb-message-headers
  '((sender     "From" "Resent-From" "Reply-To" "Sender")
    (recipients "Resent-To" "Resent-CC" "To" "CC" "BCC"))
  "Alist of headers to search for sender and recipients mail addresses.
Each element is of the form

  (CLASS HEADER ...)

The symbol CLASS defines a class of headers.
The strings HEADER belong to CLASS."
  :group 'bbdb-mua
  :type 'list)

(defcustom bbdb-message-all-addresses nil
  "If t `bbdb-update-records' returns all mail addresses of a message.
Otherwise this function returns only the first mail address of each message."
  :group 'bbdb-mua
  :type 'boolean)

(defcustom bbdb-message-try-all-headers nil
  "If t try all message headers to extract an email address from a message.
Several BBDB commands extract either the sender or the recipients' email
addresses from a message according to `bbdb-message-headers'.  If BBDB does not
find any email address in this subset of message headers (for example, because
an email address is excluded because of `bbdb-user-mail-address-re')
but `bbdb-message-try-all-headers' is t, then these commands will also consider
the email addresses in the remaining headers."
  :group 'bbdb-mua
  :type 'boolean)

(defcustom bbdb-accept-message-alist t
  "Alist describing which messages to automatically create BBDB records for.
The format of this alist is
   ((HEADER-NAME . REGEXP) ...)
For example, if
   ((\"From\" . \"@.*\\.maximegalon\\.edu\")
    (\"Subject\" . \"time travel\"))
BBDB records are only created for messages sent by people at Maximegalon U.,
or people posting about time travel.
If t accept all messages.  If nil do not accept any messages.

See also `bbdb-ignore-message-alist', which has the opposite effect."
  :group 'bbdb-mua
  :type '(repeat (cons
                  (string :tag "Header name")
                  (regexp :tag "Regexp to match on header value"))))

(defcustom bbdb-ignore-message-alist nil
  "Alist describing which messages not to automatically create BBDB records for.
The format of this alist is
   ((HEADER-NAME . REGEXP) ... )
For example, if
   ((\"From\" . \"mailer-daemon\")
    ((\"To\" \"CC\") . \"mailing-list-1\\\\|mailing-list-2\"))
no BBDB records are created for messages from any mailer daemon,
or messages sent to or CCed to either of two mailing lists.
If t ignore all messages.  If nil do not ignore any messages.

See also `bbdb-accept-message-alist', which has the opposite effect."
  :group 'bbdb-mua
  :type '(repeat (cons
                  (string :tag "Header name")
                  (regexp :tag "Regexp to match on header value"))))

(defcustom bbdb-user-mail-address-re
  (and (stringp user-mail-address)
       (string-match "\\`\\([^@]*\\)\\(@\\|\\'\\)" user-mail-address)
       (concat "\\<" (regexp-quote (match-string 1 user-mail-address)) "\\>"))
  "A regular expression matching your mail addresses.
Several BBDB commands extract either the sender or the recipients' email
addresses from a message according to `bbdb-message-headers'.  Yet an email
address will be ignored if it matches `bbdb-user-mail-address-re'.  This way
the commands will not operate on your own record.
See also `bbdb-message-try-all-headers'."
  :group 'bbdb-mua
  :type '(regexp :tag "Regexp matching your mail addresses"))

(defcustom bbdb-add-name 'query
  "How to handle new names for existing BBDB records.
This handles messages where the real name differs from the name
in a BBDB record with the same mail address, as in \"John Smith <jqs@frob.com>\"
versus \"John Q. Smith <jqs@frob.com>\".
Allowed values are:
 t           Automatically change the name to the new value.
 query       Query whether to use the new name.
 nil         Ignore the new name.
 a number    Number of seconds BBDB displays the name mismatch.
               (without further action).
 a function  This is called with two args, the record and the new name.
               It should return one of the above values.
 a regexp    If the new name matches this regexp ignore it.
               Otherwise query to add it.
See also `bbdb-add-aka'."
  :group 'bbdb-mua
  :type '(choice (const :tag "Automatically use the new name" t)
                 (const :tag "Query for name changes" query)
                 (const :tag "Ignore the new name" nil)
                 (integer :tag "Number of seconds to display name mismatch")
                 (function :tag "Function for analyzing name handling")
                 (regexp :tag "If the new name matches this regexp ignore it.")))

(defcustom bbdb-add-aka 'query
  "How to handle alternate names for existing BBDB records.
Allowed values are:
 t           Automatically store alternate names as AKA.
 query       Query whether to store alternate names as an AKA.
 nil         Ignore alternate names.
 a function  This is called with two args, the record and the new name.
               It should return one of the above values.
 a regexp    If the alternate name matches this regexp ignore it.
               Otherwise query to add it.
See also `bbdb-add-name'."
  :group 'bbdb-mua
  :type '(choice (const :tag "Automatically store alternate names as AKA" t)
                 (const :tag "Query for alternate names" query)
                 (const :tag "Ignore alternate names" nil)
                 (function :tag "Function for alternate name handling")
                 (regexp :tag "If the alternate name matches this regexp ignore it.")))

(defcustom bbdb-add-mails 'query
  "How to handle new mail addresses for existing BBDB records.
This handles messages where the mail address differs from the mail addresses
in a BBDB record with the same name as in \"John Q. Smith <jqs@foo.com>\"
versus \"John Q. Smith <jqs@bar.com>\".
Allowed values are:
 t           Automatically add new mail addresses to the list of mail addresses.
 query       Query whether to add it.
 nil         Ignore new mail addresses.
 a number    Number of seconds BBDB displays the new address
               (without further action).
 a function  This is called with two args, the record and the new mail address.
               It should return one of the above values.
 a regexp    If the new mail address matches this regexp ignore the new address.
               Otherwise query to add it.
See also `bbdb-new-mails-primary' and `bbdb-ignore-redundant-mails'."
  :group 'bbdb-mua
  :type '(choice (const :tag "Automatically add new mail addresses" t)
                 (const :tag "Query before adding new mail addresses" query)
                 (const :tag "Never add new mail addresses" nil)
                 (number :tag "Number of seconds to display new addresses")
                 (function :tag "Function for analyzing name handling")
                 (regexp :tag "If the new address matches this regexp ignore it.")))

(defcustom bbdb-new-mails-primary 'query
  "Where to put new mail addresses for existing BBDB records.
A new mail address may either become the new primary mail address,
when it is put at the beginning of the list of mail addresses.
Or the new mail address is added at the end of the list of mail addresses.
Allowed values are:
 t           Make a new address automatically the primary address.
 query       Query whether to make it the primary address.
 nil         Add the new address to the end of the list.
 a function  This is called with two args, the record and the new mail address.
               It should return one of the above values.
 a regexp    If the new mail address matches this regexp put it at the end.
               Otherwise query to make it the primary address.
See also `bbdb-add-mails'."
  :group 'bbdb-mua
  :type '(choice (const :tag "New address automatically made primary" t)
                 (const :tag "Query before making a new address primary" query)
                 (const :tag "Do not make new address primary" nil)
                 (function :tag "Function for analyzing primary handling")
                 (regexp :tag "If the new mail address matches this regexp put it at the end.")))

(defcustom bbdb-canonicalize-mail-function nil
  "If non-nil, it should be a function of one arg: a mail address string.
When BBDB \"notices\" a message, the corresponding mail addresses are passed
to this function first.  It acts as a kind of \"filter\" to transform
the mail addresses before they are compared against or added to the database.
See `bbdb-canonicalize-mail-1' for a more complete example.
If this function returns nil, BBDB assumes that there is no mail address.

See also `bbdb-ignore-redundant-mails'."
  :group 'bbdb-mua
  :type 'function)

(defcustom bbdb-ignore-redundant-mails 'query
  "How to handle redundant mail addresses for existing BBDB records.
For example, \"foo@bar.baz.com\" is redundant w.r.t. \"foo@baz.com\".
This affects two things, whether a new redundant mail address is added
to BBDB and whether an old mail address, which has become redundant
because of a newly added mail address, is removed from BBDB.

Allowed values are:
 t           Automatically ignore redundant mail addresses.
 query       Query whether to ignore them.
 nil         Do not ignore redundant mail addresses.
 a number    Number of seconds BBDB displays redundant mail addresses
               (without further action).
 a function  This is called with two args, the record and the new mail address.
               It should return one of the above values.
 a regexp    If the new mail address matches this regexp never ignore
               this mail address.  Otherwise query to ignore it.
See also `bbdb-add-mails' and `bbdb-canonicalize-mail-function'."
  :group 'bbdb-mua
  :type '(choice (const :tag "Automatically ignore redundant mail addresses" t)
                 (const :tag "Query whether to ignore them" query)
                 (const :tag "Do not ignore redundant mail addresses" nil)
                 (number :tag "Number of seconds to display redundant addresses")
                 (function :tag "Function for handling redundant mail addresses")
                 (regexp :tag "If the new address matches this regexp never ignore it.")))
(define-obsolete-variable-alias 'bbdb-canonicalize-redundant-mails
  'bbdb-ignore-redundant-mails "3.0")

(defcustom bbdb-message-clean-name-function 'bbdb-message-clean-name-default
  "Function to clean up the name in the header of a message.
It takes one argument, the name as extracted by
`mail-extract-address-components'."
  :group 'bbdb-mua
  :type 'function)

(defcustom bbdb-message-mail-as-name t
  "If non-nil use mail address of message as fallback for name of new records."
  :group 'bbdb-mua
  :type 'boolean)

(defcustom bbdb-notice-mail-hook nil
  "Hook run each time a mail address of a record is \"noticed\" in a message.
This means that the mail address in a message belongs to an existing BBDB record
or to a record BBDB has created for the mail address.

Run with one argument, the record.  It is up to the hook function
to determine which MUA is used and to act appropriately.
Hook functions can use the variable `bbdb-update-records-address'
to determine the header and class of the mail address according
to `bbdb-message-headers'.  See `bbdb-auto-notes' for how to annotate records
using `bbdb-update-records-address' and the headers of a mail message.

If a message contains multiple mail addresses belonging to one BBDB record,
this hook is run for each mail address.  Use `bbdb-notice-record-hook'
if you want to notice each record only once per message."
  :group 'bbdb-mua
  :type 'hook)

(defcustom bbdb-notice-record-hook nil
  "Hook run each time a BBDB record is \"noticed\" in a message.
This means that one of the mail addresses in a message belongs to an existing
record or it is a record BBDB has created for the mail address.  If a message
contains multiple mail addresses belonging to one BBDB record, this hook
is nonetheless run only once.  Use `bbdb-notice-mail-hook' if you want to run
a hook function for each mail address in a message.

Hook is run with one argument, the record."
  :group 'bbdb-mua
  :type 'hook)

(define-widget 'bbdb-alist-with-header 'group
  "My group"
  :match 'bbdb-alist-with-header-match
  :value-to-internal (lambda (_widget value)
                       (if value (list (car value) (cdr value))))
  :value-to-external (lambda (_widget value)
                       (if value (append (list (car value)) (cadr value)))))

(defun bbdb-alist-with-header-match (widget value)
  (widget-group-match widget
                      (widget-apply widget :value-to-internal value)))

(defvar bbdb-auto-notes-rules-expanded nil
  "Expanded `bbdb-auto-notes-rules'.") ; Internal variable

(defcustom bbdb-auto-notes-rules nil
  "List of rules for adding notes to records of mail addresses of messages.
This automatically annotates the BBDB record of the sender or recipient
of a message based on the value of a header such as the Subject header.
This requires that `bbdb-notice-mail-hook' contains `bbdb-auto-notes'
and that the record already exists or `bbdb-update-records-p' returns such that
the record will be created.  Messages matching `bbdb-auto-notes-ignore-messages'
are ignored.

The elements of this list are

   (MUA FROM-TO HEADER ANNOTATE ...)
   (FROM-TO HEADER ANNOTATE ...)
   (HEADER ANNOTATE ...)

MUA is the active MUA or a list of MUAs (see `bbdb-mua').
If MUA is missing or t, use this rule for all MUAs.

FROM-TO is a list of headers and/or header classes as in `bbdb-message-headers'.
The record corresponding to a mail address of a message is considered for
annotation if this mail address was found in a header matching FROM-TO.
If FROM-TO is missing or t, records for each mail address are considered
irrespective of where the mail address was found in a message.

HEADER is a message header that is considered for generating the annotation.

ANNOTATE may take the following values:

  (REGEXP . STRING)       [this is equivalent to (REGEXP notes STRING)]
  (REGEXP FIELD STRING)
  (REGEXP FIELD STRING REPLACE)

REGEXP must match the value of HEADER for generating an annotation.
However, if the value of HEADER also matches an element of
`bbdb-auto-notes-ignore-headers' no annotation is generated.

The annotation will be added to FIELD of the respective record.
FIELD defaults to `bbdb-default-xfield'.

STRING defines a replacement for the match of REGEXP in the value of HEADER.
It may contain \\& or \\N specials used by `replace-match'.
The resulting string becomes the annotation.
If STRING is an integer N, the Nth matching subexpression is used.
If STRING is a function, it will be called with one arg, the value of HEADER.
The return value (which must be a string) is then used.

If REPLACE is t, the resulting string replaces the old contents of FIELD.
If it is nil, the string is appended to the contents of FIELD (unless the
annotation is already part of the content of field).

For example,

   ((\"To\" (\"-vm@\" . \"VM mailing list\"))
    (\"Subject\" (\"sprocket\" . \"mail about sprockets\")
               (\"you bonehead\" . \"called me a bonehead\")))

will cause the text \"VM mailing list\" to be added to the notes field
of the records corresponding to anyone you get mail from via one of the VM
mailing lists.

If multiple clauses match the message, all of the corresponding strings
will be added.

See also variables `bbdb-auto-notes-ignore-messages' and
`bbdb-auto-notes-ignore-headers'.

For speed-up, the function `bbdb-auto-notes' actually use expanded rules
stored in the internal variable `bbdb-auto-notes-rules-expanded'.
If you change the value of `bbdb-auto-notes-rules' outside of customize,
set `bbdb-auto-notes-rules-expanded' to nil, so that the expanded rules
will be re-evaluated."
  :group 'bbdb-mua
  :set (lambda (symbol value)
         (set-default symbol value)
         (setq bbdb-auto-notes-rules-expanded nil))
  :type '(repeat
          (bbdb-alist-with-header
           (repeat (choice
                    (const sender)
                    (const recipients)))
           (string :tag "Header name")
           (repeat (choice
                    (cons :tag "Value Pair"
                          (regexp :tag "Regexp to match on header value")
                          (string :tag "String for notes if regexp matches"))
                    (list :tag "Replacement list"
                          (regexp :tag "Regexp to match on header value")
                          (choice :tag "Record field"
                                  (const notes :tag "xfields")
                                  (const organization :tag "Organization")
                                  (symbol :tag "Other"))
                          (choice :tag "Regexp match"
                                  (string :tag "Replacement string")
                                  (integer :tag "Subexpression match")
                                  (function :tag "Callback Function"))
                          (choice :tag "Replace previous contents"
                                  (const :tag "No" nil)
                                  (const :tag "Yes" t))))))))

(defcustom bbdb-auto-notes-ignore-messages nil
  "List of rules for ignoring entire messages in `bbdb-auto-notes'.
The elements may have the following values:
  a function  This function is called with one arg, the record
              that would be annotated.
              Ignore this message if the function returns non-nil.
              This function may use `bbdb-update-records-address'.
  MUA         Ignore messages from MUA (see `bbdb-mua').
  (HEADER . REGEXP)  Ignore messages where HEADER matches REGEXP.
              For example,  (\"From\" . bbdb-user-mail-address-re)
              disables any recording of notes for mail addresses
              found in messages coming from yourself, see
              `bbdb-user-mail-address-re'.
  (MUA HEADER REGEXP)  Ignore messages from MUA where HEADER
              matches REGEXP.
See also `bbdb-auto-notes-ignore-headers'."
  :group 'bbdb-mua
  :type '(repeat (cons
                  (string :tag "Header name")
                  (regexp :tag "Regexp to match on header value"))))

(defcustom bbdb-auto-notes-ignore-headers nil
  "Alist of headers and regexps to ignore in `bbdb-auto-notes'.
Each element is of the form

    (HEADER . REGEXP)

For example,

    (\"Organization\" . \"^Gatewayed from\\\\\|^Source only\")

will exclude the phony `Organization:' headers in GNU mailing-lists
gatewayed to gnu.* newsgroups.
See also `bbdb-auto-notes-ignore-messages'."
  :group 'bbdb-mua
  :type '(repeat (cons
                  (string :tag "Header name")
                  (regexp :tag "Regexp to match on header value"))))

(defcustom bbdb-mua-pop-up t
  "If non-nil, display an auto-updated BBDB window while using a MUA.
If 'horiz, stack the window horizontally if there is room.
If this is nil, BBDB is updated silently.

See also `bbdb-mua-pop-up-window-size' and `bbdb-horiz-pop-up-window-size'."
  :group 'bbdb-mua
  :type '(choice (const :tag "MUA BBDB window stacked vertically" t)
                 (const :tag "MUA BBDB window stacked horizontally" horiz)
                 (const :tag "No MUA BBDB window" nil)))
(define-obsolete-variable-alias 'bbdb-message-pop-up 'bbdb-mua-pop-up "3.0")

(defcustom bbdb-mua-pop-up-window-size bbdb-pop-up-window-size
  "Vertical size of MUA pop-up BBDB window (vertical split).
If it is an integer number, it is the number of lines used by BBDB.
If it is a fraction between 0.0 and 1.0 (inclusive), it is the fraction
of the tallest existing window that BBDB will take over.
If it is t use `pop-to-buffer' to create the BBDB window.
See also `bbdb-pop-up-window-size'."
  :group 'bbdb-mua
  :type '(choice (number :tag "BBDB window size")
                 (const :tag "Use `pop-to-buffer'" t)))

(defcustom bbdb-horiz-pop-up-window-size '(112 . 0.3)
  "Horizontal size of a MUA pop-up BBDB window (horizontal split).
It is a cons pair (TOTAL . BBDB-SIZE).
The window that will be considered for horizontal splitting must have
at least TOTAL columns. BBDB-SIZE is the horizontal size of the BBDB window.
If it is an integer number, it is the number of columns used by BBDB.
If it is a fraction between 0 and 1, it is the fraction of the
window width that BBDB will take over."
  :group 'bbdb-mua
  :type '(cons (number :tag "Total number of columns")
               (number :tag "Horizontal size of BBDB window")))


;;; xfields processing
(defcustom bbdb-xfields-sort-order
  '((notes . 0) (url . 1) (ftp . 2) (gopher . 3) (telnet . 4) (mail-alias . 5)
    (mail-folder . 6) (lpr . 7))
  "The order for sorting the xfields.
If an xfield is not in the alist, it is assigned weight 100, so all xfields
with weights less then 100 will be in the beginning, and all xfields with
weights more than 100 will be in the end."
  :group 'bbdb-mua
  :type '(repeat (cons
                  (symbol :tag "xfield")
                  (number :tag "Weight"))))
(define-obsolete-variable-alias 'bbdb-notes-sort-order 'bbdb-xfields-sort-order "3.0")

(defcustom bbdb-merge-xfield-function-alist nil
  "Alist defining merging functions for particular xfields.
Each element is of the form (LABEL . MERGE-FUN).
For merging xfield LABEL, this will use MERGE-FUN."
  :group 'bbdb-mua
  :type '(repeat (cons
                  (symbol :tag "xfield")
                  (function :tag "merge function"))))
(define-obsolete-variable-alias 'bbdb-merge-notes-function-alist
  'bbdb-merge-xfield-function-alist "3.0")

(defcustom bbdb-mua-summary-unification-list
  '(name mail message-name message-mail message-address)
  "List of FIELDs considered by `bbdb-mua-summary-unify'.
For the RECORD matching the address of a message, `bbdb-mua-summary-unify'
returns the first non-empty field value matching an element FIELD from this list.
Each element FIELD may be a valid argument of `bbdb-record-field' for RECORD.
In addition, this list may also include the following elements:
  message-name     The name in the address of the message
  message-mail     The mail in the address of the message
  message-address  The complete address of the message
These provide a fallback if a message does not have a matching RECORD
or if some FIELD of RECORD is empty."
  :group 'bbdb-mua
  :type '(repeat (symbol :tag "Field")))

(defcustom bbdb-mua-summary-mark-field 'mark-char
  "BBDB xfield whose value is used to mark message addresses known to BBDB.
This may also be a function, called with one arg, the record, which should
return the mark.  See `bbdb-mua-summary-mark' and `bbdb-mua-summary-unify'.
See also `bbdb-mua-summary-mark'."
  :group 'bbdb-mua-gnus
  :type 'symbol)

(defcustom bbdb-mua-summary-mark "+"
  "Default mark for message addresses known to BBDB.
If nil do not mark message addresses known to BBDB.
See `bbdb-mua-summary-mark' and `bbdb-mua-summary-unify'.
See also `bbdb-mua-summary-mark-field'."
  :group 'bbdb-mua
  :type '(choice (string :tag "Mark used")
                 (const :tag "Do not mark known posters" nil)))

(defcustom bbdb-mua-summary-unify-format-letter "B"
  "Letter required for `bbdb-mua-summary-unify' in the MUA Summary format string.
For Gnus, combine it with the %u specifier in `gnus-summary-line-format'
\(see there), for example use \"%U%R%z%I%(%[%4L: %-23,23uB%]%) %s\\n\".
For VM, combine it with the %U specifier in `vm-summary-format' (see there),
for example, use \"%n %*%a %-17.17UB %-3.3m %2d %4l/%-5c %I\\\"%s\\\"\\n\".
This customization of `gnus-summary-line-format' / `vm-summary-format'
is required to use `bbdb-mua-summary-unify'.
Currently no other MUAs support this BBDB feature."
  :group 'bbdb-mua
  :type 'string)

(defcustom bbdb-mua-summary-mark-format-letter "b"
  "Letter required for `bbdb-mua-summary-mark' in the MUA Summary format string.
For Gnus, combine it with the %u specifier in `gnus-summary-line-format'
\(see there), for example, use \"%U%R%z%I%(%[%4L: %ub%-23,23f%]%) %s\\n\".
For VM, combine it with the %U specifier in `vm-summary-format' (see there),
for example, use \"%n %*%a %Ub%-17.17F %-3.3m %2d %4l/%-5c %I\\\"%s\\\"\\n\".
This customization of `gnus-summary-line-format' / `vm-summary-format'
is required to use `bbdb-mua-summary-mark'.
Currently no other MUAs support this BBDB feature."
  :group 'bbdb-mua
  :type 'string)


;;; Sending mail
(defcustom bbdb-mail-user-agent mail-user-agent
  "Mail user agent used by BBDB.
Allowed values are those allowed for `mail-user-agent'."
  :group 'bbdb-sendmail
  :type '(radio (function-item :tag "Message package"
                               :format "%t\n"
                               message-user-agent)
                (function-item :tag "Mail package"
                               :format "%t\n"
                               sendmail-user-agent)
                (function-item :tag "Emacs interface to MH"
                               :format "%t\n"
                               mh-e-user-agent)
                (function-item :tag "Message with full Gnus features"
                               :format "%t\n"
                               gnus-user-agent)
                (function-item :tag "VM"
                               :format "%t\n"
                               vm-user-agent)
                (function :tag "Other")
                (const :tag "Default" nil)))

(defcustom bbdb-mail-name-format 'first-last
  "Format for names when sending mail.
If first-last format names as \"Firstname Lastname\".
If last-first format names as \"Lastname, Firstname\".
If `bbdb-mail-name' returns the full name as a single string, this takes
precedence over `bbdb-mail-name-format'.  Likewise, if the mail address itself
includes a name, this is not reformatted."
  :group 'bbdb-sendmail
  :type '(choice (const :tag "Firstname Lastname" first-last)
                 (const :tag "Lastname, Firstname" last-first)))

(defcustom bbdb-mail-name 'mail-name
  "Xfield holding the full name for a record when sending mail.
This may also be a function taking one argument, a record.
If it returns the full mail name as a single string, this is used \"as is\".
If it returns a cons pair (FIRST . LAST) with the first and last name
for this record, these are formatted obeying `bbdb-mail-name-format'."
  :group 'bbdb-sendmail
  :type '(choice (symbol :tag "xfield")
                 (function :tag "mail name function")))

(defcustom bbdb-mail-alias-field 'mail-alias
  "Xfield holding the mail alias for a record.
Used by `bbdb-mail-aliases'.  See also `bbdb-mail-alias'."
  :group 'bbdb-sendmail
  :type 'symbol)

(defcustom bbdb-mail-alias 'first
  "Defines which mail aliases are generated for a BBDB record.
first: Generate one alias \"<alias>\" that expands to the first mail address
       of a record.
star:  Generate a second alias \"<alias>*\" that expands to all mail addresses
       of a record.
all:   Generate the aliases \"<alias>\" and \"<alias>*\" (as for 'star)
       and aliases \"<alias>n\" for each mail address, where n is the position
       of the mail address of a record."
  :group 'bbdb-sendmail
  :type '(choice (symbol :tag "Only first" first)
                 (symbol :tag "<alias>* for all mails" star)
                 (symbol :tag "All aliases" all)))

(defcustom bbdb-mail-avoid-redundancy nil
  "Mail address to use for BBDB records when sending mail.
If non-nil do not use full name in mail address when same as mail.
If value is mail-only never use full name."
  :group 'bbdb-sendmail
  :type '(choice (const :tag "Allow redundancy" nil)
                 (const :tag "Never use full name" mail-only)
                 (const :tag "Avoid redundancy" t)))

(defcustom bbdb-complete-mail t
  "If t MUA insinuation provides key binding for command `bbdb-complete-mail'."
  :group 'bbdb-sendmail
  :type 'boolean)

(defcustom bbdb-completion-list t
  "Controls the behaviour of `bbdb-complete-mail'.
If a list of symbols, it specifies which fields to complete.  Symbols include
  fl-name (= first and last name)
  lf-name (= last and first name)
  organization
  aka
  mail (= all email addresses of each record)
  primary (= first email address of each record)
If t, completion is done for all of the above.
If nil, no completion is offered."
  ;; These symbols match the fields for which BBDB provides entries in
  ;; `bbdb-hashtable'.
  :group 'bbdb-sendmail
  :type '(choice (const :tag "No Completion" nil)
                 (const :tag "Complete across all fields" t)
                 (repeat :tag "Field"
                         (choice (const fl-name)
                                 (const lf-name)
                                 (const aka)
                                 (const organization)
                                 (const primary)
                                 (const mail)))))

(defcustom bbdb-complete-mail-allow-cycling nil
  "If non-nil cycle mail addresses when calling `bbdb-complete-mail'."
  :group 'bbdb-sendmail
  :type 'boolean)

(defcustom bbdb-complete-mail-hook nil
  "List of functions called after a sucessful completion."
  :group 'bbdb-sendmail
  :type 'hook)

(defcustom bbdb-mail-abbrev-expand-hook nil
  ;; Replacement for function `mail-abbrev-expand-hook'.
  "Function (not hook) run each time an alias is expanded.
The function is called with two args the alias and the list
of corresponding mail addresses."
  :group 'bbdb-sendmail
  :type 'function)

(defcustom bbdb-completion-display-record t
  "If non-nil `bbdb-complete-mail' displays the BBDB record after completion."
  :group 'bbdb-sendmail
  :type '(choice (const :tag "Update the BBDB buffer" t)
                 (const :tag "Do not update the BBDB buffer" nil)))


;;;Dialing
(defcustom bbdb-dial-local-prefix-alist
  '(((if (integerp bbdb-default-area-code)
         (format "(%03d)" bbdb-default-area-code)
       (or bbdb-default-area-code ""))
     . ""))
  "Mapping to remove local prefixes from numbers.
If this is non-nil, it should be an alist of
\(PREFIX . REPLACEMENT) elements. The first part of a phone number
matching the regexp returned by evaluating PREFIX will be replaced by
the corresponding REPLACEMENT when dialing."
  :group 'bbdb-utilities-dialing
  :type 'sexp)

(defcustom bbdb-dial-local-prefix nil
  "Local prefix digits.
If this is non-nil, it should be a string of digits which your phone
system requires before making local calls (for example, if your phone system
requires you to dial 9 before making outside calls.) In BBDB's
opinion, you're dialing a local number if it starts with a 0 after
processing `bbdb-dial-local-prefix-alist'."
  :group 'bbdb-utilities-dialing
  :type '(choice (const :tag "No digits required" nil)
                 (string :tag "Dial this first" "9")))

(defcustom bbdb-dial-long-distance-prefix nil
  "Long distance prefix digits.
If this is non-nil, it should be a string of digits which your phone
system requires before making a long distance call (one not in your local
area code).  For example, in some areas you must dial 1 before an area
code. Note that this is used to replace the + sign in phone numbers
when dialling (international dialing prefix.)"
  :group 'bbdb-utilities-dialing
  :type '(choice (const :tag "No digits required" nil)
                 (string :tag "Dial this first" "1")))

(defcustom bbdb-dial-function nil
  "If non-nil this should be a function used for dialing phone numbers.
This function is used by `bbdb-dial-number'.  It requires one
argument which is a string for the number that is dialed.
If nil then `bbdb-dial-number' uses the tel URI syntax passed to `browse-url'
to make the call."
  :group 'bbdb-utilities-dialing
  :type 'function)


;; Faces for font-lock
(defgroup bbdb-faces nil
  "Faces used by BBDB."
  :group 'bbdb
  :group 'faces)

(defface bbdb-name
  '((t (:inherit font-lock-function-name-face)))
  "Face used for BBDB names."
  :group 'bbdb-faces)

;; KEY needs to match the value of the xfield name-face, which is a string.
;; To avoid confusion, we make KEY a string, too, though symbols might be
;; faster.
(defcustom bbdb-name-face-alist nil
  "Alist used for font-locking the name of a record.
Each element should be a cons cell (KEY . FACE) with string KEY and face FACE.
To use FACE for font-locking the name of a record,
the xfield name-face of this record should have the value KEY.
The value of name-face may also be a face which is then used directly.
If none of these schemes succeeds, the face `bbdb-name' is used."
  :group 'bbdb-faces
  :type '(repeat (cons (symbol :tag "Key") (face :tag "Face"))))

(defface bbdb-organization
  '((t (:inherit font-lock-comment-face)))
  "Face used for BBDB names."
  :group 'bbdb-faces)

(defface bbdb-field-name
  '((t (:inherit font-lock-variable-name-face)))
  "Face used for BBDB names."
  :group 'bbdb-faces)

;;; Internal variables
(eval-and-compile
  (defvar bbdb-debug t
    "Enable debugging if non-nil during compile time.
You really should not disable debugging.  But it will speed things up."))

(defconst bbdb-file-format 9
  "BBDB file format.")

(defconst bbdb-record-type
  '(vector (or string (const nil)) ; first name
           (or string (const nil)) ; last name
           (repeat string) ; affix
           (repeat string) ; aka
           (repeat string) ; organization
           (repeat (or (vector string string)
                       (vector string integer integer integer integer))) ; phone
           (repeat (vector string (repeat string) string string
                           string string)) ; address
           (repeat string) ; mail
           (repeat (cons symbol sexp)) ; xfields
           (cons symbol string) ; uuid
           (cons symbol string) ; creation-date
           (cons symbol string) ; timestamp
           sexp) ; cache
  "Pseudo-code for the structure of a record.  Used by `bbdb-check-type'.")

(defconst bbdb-file-coding-system 'utf-8
  "Coding system used for reading and writing `bbdb-file'.")

(defvar bbdb-mail-aliases-need-rebuilt nil
  "Non-nil if mail aliases need to be rebuilt.")

(defvar bbdb-buffer nil "Buffer visiting `bbdb-file'.")

(defvar bbdb-buffer-name "*BBDB*" "Name of the BBDB buffer.")

(defvar bbdb-silent-internal nil
  "Bind this to t to quiet things down - do not set it.
See also `bbdb-silent'.")

(defvar bbdb-init-forms
  '((gnus                       ; gnus 3.15 or newer
     (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus))
    (mh-e                       ; MH-E
     (add-hook 'mh-folder-mode-hook 'bbdb-insinuate-mh))
    (rmail                      ; RMAIL
     (add-hook 'rmail-mode-hook 'bbdb-insinuate-rmail))
    (vm                        ; newer versions of vm do not have `vm-load-hook'
     (eval-after-load "vm" '(bbdb-insinuate-vm)))
    (mail                       ; the standard mail user agent
     (add-hook 'mail-setup-hook 'bbdb-insinuate-mail))
    (sendmail
     (progn (message "BBDB: sendmail insinuation deprecated. Use mail.")
            (add-hook 'mail-setup-hook 'bbdb-insinuate-mail)))
    (message                    ; the gnus mail user agent
     (add-hook 'message-setup-hook 'bbdb-insinuate-message))
    (mu4e                       ; the mu4e user agent
     (add-hook 'mu4e-main-mode-hook 'bbdb-insinuate-mu4e))

    (sc                         ; supercite
     (add-hook 'sc-load-hook 'bbdb-insinuate-sc))
    (anniv                      ; anniversaries
     (add-hook 'diary-list-entries-hook 'bbdb-anniv-diary-entries))
    (pgp                        ; pgp-mail
     (progn
       (add-hook 'message-send-hook 'bbdb-pgp)
       (add-hook 'mail-send-hook 'bbdb-pgp)))
    (wl
     (add-hook 'wl-init-hook 'bbdb-insinuate-wl)))
  "Alist mapping features to insinuation forms.")

(defvar bbdb-search-invert nil
  "Bind this variable to t in order to invert the result of `bbdb-search'.")

(defvar bbdb-do-all-records nil
  "Controls the behavior of the command `bbdb-do-all-records'.")

(defvar bbdb-append-display nil
  "Controls the behavior of the command `bbdb-append-display'.")

(defvar bbdb-offer-to-create nil
  "For communication between `bbdb-update-records' and `bbdb-query-create'.")

(defvar bbdb-update-records-address nil
  "For communication between `bbdb-update-records' and `bbdb-query-create'.
It is a list with elements (NAME MAIL HEADER HEADER-CLASS MUA).")

;;; Buffer-local variables for the database.
(defvar bbdb-records nil
  "BBDB records list.
In buffer `bbdb-file' this list includes all records.
In the *BBDB* buffers it includes the records that are actually displayed
and its elements are (RECORD DISPLAY-FORMAT MARKER-POS).")
(make-variable-buffer-local 'bbdb-records)

(defvar bbdb-changed-records nil
  "List of records that has been changed since BBDB was last saved.
Use `bbdb-search-changed' to display these records.")

(defvar bbdb-end-marker nil
  "Marker holding the buffer position of the end of the last record.")

(defvar bbdb-hashtable (make-hash-table :test 'equal)
  "Hash table for BBDB records.
Hashes the fields first-last-name, last-first-name, organization, aka, and mail.")

(defvar bbdb-uuid-table (make-hash-table :test 'equal)
  "Hash table for uuid's of BBDB records.")

(defvar bbdb-xfield-label-list nil
  "List of labels for xfields.")

(defvar bbdb-organization-list nil
  "List of organizations known to BBDB.")

(defvar bbdb-street-list nil
  "List of streets known to BBDB.")

(defvar bbdb-city-list nil
  "List of cities known to BBDB.")

(defvar bbdb-state-list nil
  "List of states known to BBDB.")

(defvar bbdb-postcode-list nil
  "List of post codes known to BBDB.")

(defvar bbdb-country-list nil
  "List of countries known to BBDB.")

(defvar bbdb-modeline-info (make-vector 6 nil)
  "Precalculated mode line info for BBDB commands.
This is a vector [APPEND-M APPEND INVERT-M INVERT ALL-M ALL].
APPEND-M is the mode line info if `bbdb-append-display' is non-nil.
INVERT-M is the mode line info if `bbdb-search-invert' is non-nil.
ALL-M is the mode line info if `bbdb-do-all-records' is non-nil.
APPEND, INVERT, and ALL appear in the message area.")

(defvar bbdb-update-unchanged-records nil
  "If non-nil update unchanged records in the database.
Normally calls of `bbdb-change-hook' and updating of a record are suppressed,
if an editing command did not really change the record.  Bind this to t
if you want to call `bbdb-change-hook' and update the record unconditionally.")

;;; Keymap
(defvar bbdb-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km "*"          'bbdb-do-all-records)
    (define-key km "+"          'bbdb-append-display)
    (define-key km "!"          'bbdb-search-invert)
    (define-key km "a"          'bbdb-add-mail-alias)
    (define-key km "A"          'bbdb-mail-aliases)
    (define-key km "c"          'bbdb-create)
    (define-key km "e"          'bbdb-edit-field)
    (define-key km ";"          'bbdb-edit-foo)
    (define-key km "n"          'bbdb-next-record)
    (define-key km "p"          'bbdb-prev-record)
    (define-key km "N"          'bbdb-next-field)
    (define-key km "\t"         'bbdb-next-field) ; TAB
    (define-key km "P"          'bbdb-prev-field)
    (define-key km "\d"         'bbdb-prev-field) ; DEL
    (define-key km "d"          'bbdb-delete-field-or-record)
    (define-key km "\C-k"       'bbdb-delete-field-or-record)
    (define-key km "i"          'bbdb-insert-field)
    (define-key km "s"          'bbdb-save)
    (define-key km "\C-x\C-s"   'bbdb-save)
    (define-key km "t"          'bbdb-toggle-records-layout)
    (define-key km "T"          'bbdb-display-records-completely)
    (define-key km "o"          'bbdb-omit-record)
    (define-key km "m"          'bbdb-mail)
    (define-key km "M"          'bbdb-mail-address)
    (define-key km "\M-d"       'bbdb-dial)
    (define-key km "h"          'bbdb-info)
    (define-key km "?"          'bbdb-help)
    ;; (define-key km "q"       'quit-window) ; part of `special-mode' bindings
    (define-key km "\C-x\C-t"   'bbdb-transpose-fields)
    (define-key km "Cr"         'bbdb-copy-records-as-kill)
    (define-key km "Cf"         'bbdb-copy-fields-as-kill)
    (define-key km "u"          'bbdb-browse-url)
    (define-key km "\C-c\C-t"   'bbdb-tex)
    (define-key km "="          'delete-other-windows)

    ;; Search keys
    (define-key km "b"          'bbdb)
    (define-key km "/1"         'bbdb-display-records)
    (define-key km "/n"         'bbdb-search-name)
    (define-key km "/o"         'bbdb-search-organization)
    (define-key km "/p"         'bbdb-search-phone)
    (define-key km "/a"         'bbdb-search-address)
    (define-key km "/m"         'bbdb-search-mail)
    (define-key km "/N"         'bbdb-search-xfields)
    (define-key km "/x"         'bbdb-search-xfields)
    (define-key km "/c"         'bbdb-search-changed)
    (define-key km "/d"         'bbdb-search-duplicates)
    (define-key km "\C-xnw"     'bbdb-display-all-records)
    (define-key km "\C-xnd"     'bbdb-display-current-record)

    (define-key km [delete]     'scroll-down) ; 24.1: part of `special-mode'
    (define-key km " "          'scroll-up)   ; 24.1: part of `special-mode'

    (define-key km [mouse-3]    'bbdb-mouse-menu)
    (define-key km [mouse-2]    (lambda (event)
                                  ;; Toggle record layout
                                  (interactive "e")
                                  (save-excursion
                                    (posn-set-point (event-end event))
                                    (bbdb-toggle-records-layout
                                     (bbdb-do-records t) current-prefix-arg))))
    km)
  "Keymap for Insidious Big Brother Database.
This is a child of `special-mode-map'.")

(easy-menu-define
  bbdb-menu bbdb-mode-map "BBDB Menu"
  '("BBDB"
    ("Display"
     ["Previous field" bbdb-prev-field t]
     ["Next field" bbdb-next-field t]
     ["Previous record" bbdb-prev-record t]
     ["Next record" bbdb-next-record t]
     "--"
     ["Show all records" bbdb-display-all-records t]
     ["Show current record" bbdb-display-current-record t]
     ["Omit record" bbdb-omit-record t]
     "--"
     ["Toggle layout" bbdb-toggle-records-layout t]
     ["Show all fields" bbdb-display-records-completely t])
    ("Searching"
     ["General search" bbdb t]
     ["Search one record" bbdb-display-records t]
     ["Search name" bbdb-search-name t]
     ["Search organization" bbdb-search-organization t]
     ["Search phone" bbdb-search-phone t]
     ["Search address" bbdb-search-address t]
     ["Search mail" bbdb-search-mail t]
     ["Search xfields" bbdb-search-xfields t]
     ["Search changed records" bbdb-search-changed t]
     ["Search duplicates" bbdb-search-duplicates t]
     "--"
     ["Old time stamps" bbdb-timestamp-older t]
     ["New time stamps" bbdb-timestamp-newer t]
     ["Old creation date" bbdb-creation-older t]
     ["New creation date" bbdb-creation-newer t]
     ["Creation date = time stamp" bbdb-creation-no-change t]
     "--"
     ["Append search" bbdb-append-display t]
     ["Invert search" bbdb-search-invert t])
    ("Mail"
     ["Send mail" bbdb-mail t]
     ["Save mail address" bbdb-mail-address t]
     "--"
     ["Add mail alias" bbdb-add-mail-alias t]
     ["(Re-)Build mail aliases" bbdb-mail-aliases t])
    ("Use database"
     ["Prefix: do all records" bbdb-do-all-records t]
     "--"
     ["Send mail" bbdb-mail t]
     ["Dial phone number" bbdb-dial t]
     ["Browse URL" bbdb-browse-url t]
     ["Copy records as kill" bbdb-copy-records-as-kill t]
     ["Copy fields as kill" bbdb-copy-fields-as-kill t]
     "--"
     ["TeX records" bbdb-tex t])
    ("Manipulate database"
     ["Prefix: do all records" bbdb-do-all-records t]
     "--"
     ["Create new record" bbdb-create t]
     ["Edit current field" bbdb-edit-field t]
     ["Insert new field" bbdb-insert-field t]
     ["Edit some field" bbdb-edit-foo t]
     ["Transpose fields" bbdb-transpose-fields t]
     ["Delete record or field" bbdb-delete-field-or-record t]
     "--"
     ["Sort addresses" bbdb-sort-addresses t]
     ["Sort phones" bbdb-sort-phones t]
     ["Sort xfields" bbdb-sort-xfields t]
     ["Merge records" bbdb-merge-records t]
     ["Sort database" bbdb-sort-records t]
     ["Delete duplicate mails" bbdb-delete-redundant-mails t]
     "--"
     ["Save BBDB" bbdb-save t]
     ["Revert BBDB" revert-buffer t])
    ("Help"
     ["Brief help" bbdb-help t]
     ["BBDB Manual" bbdb-info t])
    "--"
    ["Quit" quit-window t]))

(defvar bbdb-completing-read-mails-map
  (let ((map (copy-keymap minibuffer-local-completion-map)))
    (define-key map " " 'self-insert-command)
    (define-key map "\t" 'bbdb-complete-mail)
    (define-key map "\M-\t" 'bbdb-complete-mail)
    map)
  "Keymap used by `bbdb-completing-read-mails'.")



;;; Helper functions

(defun bbdb-warn (&rest args)
  "Display a message at the bottom of the screen.
ARGS are passed to `message'."
  (ding t)
  (apply 'message args))

(defun bbdb-string-trim (string &optional null)
  "Remove leading and trailing whitespace and all properties from STRING.
If STRING is nil return an empty string unless NULL is non-nil."
  (if (null string)
      (unless null "")
    (setq string (substring-no-properties string))
    (if (string-match "\\`[ \t\n]+" string)
        (setq string (substring-no-properties string (match-end 0))))
    (if (string-match "[ \t\n]+\\'" string)
        (setq string (substring-no-properties string 0 (match-beginning 0))))
    (unless (and null (string= "" string))
      string)))

(defsubst bbdb-string= (str1 str2)
  "Return t if strings STR1 and STR2 are equal, ignoring case."
  (and (stringp str1) (stringp str2)
       (eq t (compare-strings str1 0 nil str2 0 nil t))))

(defun bbdb-split (separator string)
  "Split STRING into list of substrings bounded by matches for SEPARATORS.
SEPARATOR may be a regexp.  SEPARATOR may also be a symbol
\(a field name).  Then look up the value in `bbdb-separator-alist'
or use `bbdb-default-separator'.
Whitespace around SEPARATOR is ignored unless SEPARATOR matches
the string \" \\t\\n\".
Almost the inverse function of `bbdb-concat'."
  (if (symbolp separator)
      (setq separator (car (or (cdr (assq separator bbdb-separator-alist))
                               bbdb-default-separator))))
  (if (<= 24.4 (string-to-number emacs-version))
      ;; `split-string' applied to an empty STRING gives nil.
      (split-string string separator t
                    (unless (string-match separator " \t\n") "[ \t\n]*"))
    (unless (string-match separator " \t\n")
      (setq separator (concat "[ \t\n]*" separator "[ \t\n]*")))
    (split-string (bbdb-string-trim string) separator t)))

(defun bbdb-concat (separator &rest strings)
  "Concatenate STRINGS to a string sticking in SEPARATOR.
STRINGS may be strings or lists of strings.  Empty strings are ignored.
SEPARATOR may be a string.
SEPARATOR may also be a symbol (a field name).  Then look up the value
of SEPARATOR in `bbdb-separator-alist' or use `bbdb-default-separator'.
The inverse function of `bbdb-split'."
  (if (symbolp separator)
      (setq separator (nth 1 (or (cdr (assq separator bbdb-separator-alist))
                                 bbdb-default-separator))))
  (mapconcat 'identity
             (delete "" (apply 'append (mapcar (lambda (x) (if (stringp x)
                                                               (list x) x))
                                               strings))) separator))

(defun bbdb-list-strings (list)
  "Remove all elements from LIST which are not non-empty strings."
  (let (new-list)
    (dolist (elt list)
      (if (and (stringp elt) (not (string= "" elt)))
          (push elt new-list)))
    (nreverse new-list)))

;; A call of `indent-region' swallows any indentation
;; that might be part of the field itself.  So we indent manually.
(defsubst bbdb-indent-string (string column)
  "Indent nonempty lines in STRING to COLUMN (except first line).
This happens in addition to any pre-defined indentation of STRING."
  (replace-regexp-in-string "\n\\([^\n]\\)"
                            (concat "\n" (make-string column ?\s) "\\1")
                            string))

(defun bbdb-read-string (prompt &optional init collection require-match)
  "Read a string, trimming whitespace and text properties.
PROMPT is a string to prompt with.
INIT appears as initial input which is useful for editing existing records.
COLLECTION and REQUIRE-MATCH have the same meaning as in `completing-read'."
  (bbdb-string-trim
   (if collection
       ;; Hack: In `minibuffer-local-completion-map' remove
       ;; the binding of SPC to `minibuffer-complete-word'
       ;; and of ? to `minibuffer-completion-help'.
       (minibuffer-with-setup-hook
           (lambda ()
             (use-local-map
              (let ((map (make-sparse-keymap)))
                (set-keymap-parent map (current-local-map))
                (define-key map " " nil)
                (define-key map "?" nil)
                map)))
         (completing-read prompt collection nil require-match init))
     (read-string prompt init))))

;; The following macros implement variants of `pushnew' (till emacs 24.2)
;; or `cl-pushnew' (since emacs 24.3).  To be compatible with older and newer
;; versions of emacs we use our own macros.  We call these macros often.
;; So we keep them simple.  Nothing fancy is needed here.
(defmacro bbdb-pushnew (element listname)
  "Add ELEMENT to the value of LISTNAME if it isn't there yet.
The test for presence of ELEMENT is done with `equal'.
The return value is the new value of LISTNAME."
  `(let ((elt ,element))
     (if (member elt ,listname)
         ,listname
       (setq ,listname (cons elt ,listname)))))

(defmacro bbdb-pushnewq (element listname)
  "Add ELEMENT to the value of LISTNAME if it isn't there yet.
The test for presence of ELEMENT is done with `eq'.
The return value is the new value of LISTNAME."
  `(let ((elt ,element))
     (if (memq elt ,listname)
         ,listname
       (setq ,listname (cons elt ,listname)))))

(defmacro bbdb-pushnewt (element listname)
  "Add ELEMENT to the value of LISTNAME if it isn't there yet and non-nil.
The test for presence of ELEMENT is done with `equal'.
The return value is the new value of LISTNAME."
  `(let ((elt ,element))
     (if (or (not elt)
             (member elt ,listname))
         ,listname
       (setq ,listname (cons elt ,listname)))))

(defun bbdb-current-record (&optional full)
  "Return the record point is at.
If FULL is non-nil record includes the display information."
  (unless (eq major-mode 'bbdb-mode)
    (error "This only works while in BBDB buffers."))
  (let ((num (get-text-property (if (and (not (bobp)) (eobp))
                                    (1- (point)) (point))
                                'bbdb-record-number))
        record)
    (unless num (error "Not a BBDB record"))
    (setq record (nth num bbdb-records))
    (if full record (car record))))

(defun bbdb-current-field ()
  "Return current field point is on."
  (unless (bbdb-current-record) (error "Not a BBDB record"))
  (get-text-property (point) 'bbdb-field))

(defmacro bbdb-debug (&rest body)
  "Excecute BODY just like `progn' with debugging capability.
Debugging is enabled if variable `bbdb-debug' is non-nil during compile.
You really should not disable debugging.  But it will speed things up."
  (declare (indent 0))
  (if bbdb-debug ; compile-time switch
      `(let ((debug-on-error t))
         ,@body)))

;; inspired by `gnus-bind-print-variables'
(defmacro bbdb-with-print-loadably (&rest body)
  "Bind print-* variables for BBDB and evaluate BODY.
This macro is used with `prin1', `prin1-to-string', etc. in order to ensure
printed Lisp objects are loadable by BBDB."
  (declare (indent 0))
  `(let ((print-escape-newlines t) ;; BBDB needs this!
         print-escape-nonascii print-escape-multibyte
         print-quoted print-length print-level)
         ;; print-circle print-gensym
         ;; print-continuous-numbering
         ;; print-number-table
         ;; float-output-format
     ,@body))

(defun bbdb-timestamp (_record)
  ""
  (unless (get 'bbdb-timestamp 'bbdb-obsolete)
    (put 'bbdb-timestamp 'bbdb-obsolete t)
    (message "Function `bbdb-timestamp' is obsolete.  Remove it from any hooks.")
    (sit-for 2)))
(make-obsolete 'bbdb-timestamp nil "2017-08-09")

(defun bbdb-creation-date (_record)
  ""
  (unless (get 'bbdb-creation-date 'bbdb-obsolete)
    (put 'bbdb-creation-date 'bbdb-obsolete t)
    (message "Function `bbdb-creation-date' is obsolete.  Remove it from any hooks.")
    (sit-for 2)))
(make-obsolete 'bbdb-creation-date nil "2017-08-09")

;; Copied from org-id.el
(defun bbdb-uuid ()
  "Return string with random (version 4) UUID."
  (let ((rnd (md5 (format "%s%s%s%s%s%s%s"
			  (random)
			  (current-time)
			  (user-uid)
			  (emacs-pid)
			  (user-full-name)
			  user-mail-address
			  (recent-keys)))))
    (format "%s-%s-4%s-%s%s-%s"
	    (substring rnd 0 8)
	    (substring rnd 8 12)
	    (substring rnd 13 16)
	    (format "%x"
		    (logior
		     #b10000000
		     (logand
		      #b10111111
		      (string-to-number
		       (substring rnd 16 18) 16))))
	    (substring rnd 18 20)
	    (substring rnd 20 32))))

(defun bbdb-multiple-buffers-default ()
  "Default function for guessing a name for new *BBDB* buffers.
May be used as value of variable `bbdb-multiple-buffers'."
  (save-current-buffer
    (cond ((memq major-mode '(vm-mode vm-summary-mode vm-presentation-mode
                                      vm-virtual-mode))
           (vm-select-folder-buffer)
           (buffer-name))
          ((memq major-mode '(gnus-summary-mode gnus-group-mode))
           (set-buffer gnus-article-buffer)
           (buffer-name))
          ((memq major-mode '(mail-mode vm-mail-mode message-mode))
           "message composition"))))

(defsubst bbdb-add-job (spec record string)
  "Internal function: Evaluate SPEC for RECORD and STRING.
If SPEC is a function call it with args RECORD and STRING.  Return value.
If SPEC is a regexp, return 'query unless SPEC matches STRING.
Otherwise return SPEC.
Used with variable `bbdb-add-name' and friends."
  (cond ((functionp spec)
         (funcall spec record string))
        ((stringp spec)
         (unless (string-match spec string) 'query)) ; be least aggressive
        (spec)))

(defsubst bbdb-eval-spec (spec prompt)
  "Internal function: Evaluate SPEC using PROMPT.
Return t if either SPEC equals t, or SPEC equals 'query and `bbdb-silent'
is non-nil or `y-or-no-p' returns t using PROMPT.
Used with return values of `bbdb-add-job'."
  (or (eq spec t)
      (and (eq spec 'query)
           (or bbdb-silent (y-or-n-p prompt)))))

(defun bbdb-clean-address-components (components)
  "Clean mail address COMPONENTS.
COMPONENTS is a list (FULL-NAME CANONICAL-ADDRESS) as returned
by `mail-extract-address-components'.
Pass FULL-NAME through `bbdb-message-clean-name-function'
and CANONICAL-ADDRESS through `bbdb-canonicalize-mail-function'."
  (list (if (car components)
            (if bbdb-message-clean-name-function
                (funcall bbdb-message-clean-name-function (car components))
              (car components)))
        (if (cadr components)
            (if bbdb-canonicalize-mail-function
                (funcall bbdb-canonicalize-mail-function (cadr components))
              ;; Minimalistic clean-up
              (bbdb-string-trim (cadr components))))))

(defun bbdb-extract-address-components (address &optional all)
  "Given an RFC-822 address ADDRESS, extract full name and canonical address.
This function behaves like `mail-extract-address-components', but it passes
its return value through `bbdb-clean-address-components'.
See also `bbdb-decompose-bbdb-address'."
  (if all
      (mapcar 'bbdb-clean-address-components
              (mail-extract-address-components address t))
    (bbdb-clean-address-components (mail-extract-address-components address))))

;; Inspired by `gnus-extract-address-components' from gnus-utils.
(defun bbdb-decompose-bbdb-address (mail)
  "Given an RFC-822 address MAIL, extract full name and canonical address.
In general, this function behaves like the more sophisticated function
`mail-extract-address-components'.  Yet for an address `<Joe_Smith@foo.com>'
lacking a real name the latter function returns the name \"Joe Smith\".
This is useful when analyzing the headers of email messages we receive
from the outside world.  Yet when analyzing the mail addresses stored
in BBDB, this pollutes the mail-aka space.  So we define here
an intentionally much simpler function for decomposing the names
and canonical addresses in the mail field of BBDB records."
  (let (name address)
    ;; First find the address - the thing with the @ in it.
    (cond (;; Check `<foo@bar>' first in order to handle the quite common
	   ;; form `"abc@xyz" <foo@bar>' (i.e. `@' as part of a comment)
	   ;; correctly.
	   (string-match "<\\([^@ \t<>]+[!@][^@ \t<>]+\\)>" mail)
	   (setq address (match-string 1 mail)))
	  ((string-match "\\b[^@ \t<>]+[!@][^@ \t<>]+\\b" mail)
	   (setq address (match-string 0 mail))))
    ;; Then check whether the `name <address>' format is used.
    (and address
	 ;; Linear white space is not required.
	 (string-match (concat "[ \t]*<" (regexp-quote address) ">") mail)
	 (setq name (substring mail 0 (match-beginning 0)))
         ;; Strip any quotes mail the name.
         (string-match "^\".*\"$" name)
         (setq name (substring name 1 (1- (match-end 0)))))
    ;; If not, then check whether the `address (name)' format is used.
    (or name
	(and (string-match "(\\([^)]+\\))" mail)
	     (setq name (match-string 1 mail))))
    (list (if (equal name "") nil name) (or address mail))))

;;; Massage of mail addresses

(defcustom bbdb-canonical-hosts
  ;; Example
  (regexp-opt '("cs.cmu.edu" "ri.cmu.edu"))
  "Regexp matching the canonical part of the domain part of a mail address.
If the domain part of a mail address matches this regexp, the domain
is replaced by the substring that actually matched this address.

Used by  `bbdb-canonicalize-mail-1'.  See also `bbdb-ignore-redundant-mails'."
  :group 'bbdb-mua
  :type '(regexp :tag "Regexp matching sites"))

(defun bbdb-canonicalize-mail-1 (address)
  "Example of `bbdb-canonicalize-mail-function'.
However, this function is too specific to be useful for the general user.
Take it as a source of inspiration for what can be done."
  (setq address (bbdb-string-trim address))
  (cond
   ;; Rewrite mail-drop hosts.
   ;; RW: The following is now also handled by `bbdb-ignore-redundant-mails'
   ((string-match
     (concat "\\`\\([^@%!]+@\\).*\\.\\(" bbdb-canonical-hosts "\\)\\'")
     address)
    (concat (match-string 1 address) (match-string 2 address)))
   ;;
   ;; Here at Lucid, our workstation names sometimes get into our mail
   ;; addresses in the form "jwz%thalidomide@lucid.com" (instead of simply
   ;; "jwz@lucid.com").  This removes the workstation name.
   ((string-match "\\`\\([^@%!]+\\)%[^@%!.]+@\\(lucid\\.com\\)\\'" address)
    (concat (match-string 1 address) "@" (match-string 2 address)))
   ;;
   ;; Another way that our local mailer is misconfigured: sometimes addresses
   ;; which should look like "user@some.outside.host" end up looking like
   ;; "user%some.outside.host" or even "user%some.outside.host@lucid.com"
   ;; instead.  This rule rewrites it into the original form.
   ((string-match "\\`\\([^@%]+\\)%\\([^@%!]+\\)\\(@lucid\\.com\\)?\\'" address)
    (concat (match-string 1 address) "@" (match-string 2 address)))
   ;;
   ;; Sometimes I see addresses like "foobar.com!user@foobar.com".
   ;; That's totally redundant, so this rewrites it as "user@foobar.com".
   ((string-match "\\`\\([^@%!]+\\)!\\([^@%!]+[@%]\\1\\)\\'" address)
    (match-string 2 address))
   ;;
   ;; Sometimes I see addresses like "foobar.com!user".  Turn it around.
   ((string-match "\\`\\([^@%!.]+\\.[^@%!]+\\)!\\([^@%]+\\)\\'" address)
    (concat (match-string 2 address) "@" (match-string 1 address)))
   ;;
   ;; The mailer at hplb.hpl.hp.com tends to puke all over addresses which
   ;; pass through mailing lists which are maintained there: it turns normal
   ;; addresses like "user@foo.com" into "user%foo.com@hplb.hpl.hp.com".
   ;; This reverses it.  (I actually could have combined this rule with
   ;; the similar lucid.com rule above, but then the regexp would have been
   ;; more than 80 characters long...)
   ((string-match "\\`\\([^@!]+\\)%\\([^@%!]+\\)@hplb\\.hpl\\.hp\\.com\\'"
          address)
    (concat (match-string 1 address) "@" (match-string 2 address)))
   ;;
   ;; Another local mail-configuration botch: sometimes mail shows up
   ;; with addresses like "user@workstation", where "workstation" is a
   ;; local machine name.  That should really be "user" or "user@netscape.com".
   ;; (I'm told this one is due to a bug in SunOS 4.1.1 sendmail.)
   ((string-match "\\`\\([^@%!]+\\)[@%][^@%!.]+\\'" address)
    (match-string 1 address))
   ;;
   ;; Sometimes I see addresses like "foo%somewhere%uunet.uu.net@somewhere.else".
   ;; This is silly, because I know that I can send mail to uunet directly.
   ((string-match ".%uunet\\.uu\\.net@[^@%!]+\\'" address)
    (concat (substring address 0 (+ (match-beginning 0) 1)) "@UUNET.UU.NET"))
   ;;
   ;; Otherwise, leave it as it is.
   (t address)))

(defun bbdb-message-clean-name-default (name)
  "Default function for `bbdb-message-clean-name-function'.
This strips garbage from the user full NAME string."
  ;; Remove leading non-alpha chars
  (if (string-match "\\`[^[:alpha:]]+" name)
      (setq name (substring name (match-end 0))))

  (if (string-match "^\\([^@]+\\)@" name)
      ;; The name is really a mail address and we use the part preceeding "@".
      ;; Everything following "@" is ignored.
      (setq name (match-string 1 name)))

  ;; Replace "firstname.surname" by "firstname surname".
  ;; Do not replace ". " with " " because that could be an initial.
  (setq name (replace-regexp-in-string "\\.\\([^ ]\\)" " \\1" name))

  ;; Replace tabs, spaces, and underscores with a single space.
  (setq name (replace-regexp-in-string "[ \t\n_]+" " " name))

  ;; Remove trailing comments separated by "(" or " [-#]"
  ;; This does not work all the time because some of our friends in
  ;; northern europe have brackets in their names...
  (if (string-match "[^ \t]\\([ \t]*\\((\\| [-#]\\)\\)" name)
      (setq name (substring name 0 (match-beginning 1))))

  ;; Remove phone extensions (like "x1234" and "ext. 1234")
  (let ((case-fold-search t))
    (setq name (replace-regexp-in-string
                "\\W+\\(x\\|ext\\.?\\)\\W*[-0-9]+" "" name)))

  ;; Remove trailing non-alpha chars
  (if (string-match "[^[:alpha:]]+\\'" name)
      (setq name (substring name 0 (match-beginning 0))))

  ;; Remove text properties
  (substring-no-properties name))

;; BBDB data structure
(defmacro bbdb-defstruct (name &rest elts)
  "Define two functions to operate on vector NAME for each symbol ELT in ELTS.
The function bbdb-NAME-ELT returns the element ELT in vector NAME.
The function bbdb-NAME-set-ELT sets ELT.
Also define a constant bbdb-NAME-length that holds the number of ELTS
in vector NAME."
  (declare (indent 1))
  (let* ((count 0)
         (sname (symbol-name name))
         (uname (upcase sname))
         (cname (concat "bbdb-" sname "-"))
         body)
    (dolist (elt elts)
      (let* ((selt (symbol-name elt))
             (setname  (intern (concat cname "set-" selt))))
        (push (list 'defsubst (intern (concat cname selt)) `(,name)
                    (format "For BBDB %s read element %i `%s'."
                            uname count selt)
                    ;; Use `elt' instead of `aref' so that these functions
                    ;; also work for the `bbdb-record-type' pseudo-code.
                    `(elt ,name ,count)) body)
        (push (list 'defsubst setname `(,name value)
                    (format "For BBDB %s set element %i `%s' to VALUE.  \
Return VALUE.
Do not call this function directly.  Call instead `bbdb-record-set-field'
which ensures the integrity of the database.  Also, this makes your code
more robust with respect to possible future changes of BBDB's innermost
internals."
                            uname count selt)
                    `(aset ,name ,count value)) body))
      (setq count (1+ count)))
    (push (list 'defconst (intern (concat cname "length")) count
                (concat "Length of BBDB `" sname "'.")) body)
    (cons 'progn body)))

;; Define RECORD:
(bbdb-defstruct record
  firstname lastname affix aka organization phone address mail xfields
  uuid creation-date timestamp cache)

;; Define PHONE:
(bbdb-defstruct phone
  label area exchange suffix extension)

;; Define ADDRESS:
(bbdb-defstruct address
  label streets city state postcode country)

;; Define record CACHE:
;; - fl-name (first and last name of the person referred to by the record),
;; - lf-name (last and first name of the person referred to by the record),
;; - mail-aka (list of names associated with mail addresses)
;; - mail-canon (list of canonical mail addresses)
;; - sortkey (the concatenation of the elements used for sorting the record),
;; - marker  (position of beginning of record in `bbdb-file')
(bbdb-defstruct cache
  fl-name lf-name mail-aka mail-canon sortkey marker)

(defsubst bbdb-record-mail-aka (record)
  "Record cache function: Return mail-aka for RECORD."
  (bbdb-cache-mail-aka (bbdb-record-cache record)))

(defsubst bbdb-record-mail-canon (record)
  "Record cache function: Return mail-canon for RECORD."
  (bbdb-cache-mail-canon (bbdb-record-cache record)))

(defun bbdb-empty-record ()
  "Return a new empty record structure with a cache.
It is the caller's responsibility to make the new record known to BBDB."
  (let ((record (make-vector bbdb-record-length nil)))
    (bbdb-record-set-cache record (make-vector bbdb-cache-length nil))
    record))

;; `bbdb-hashtable' associates with each KEY a list of matching records.
;; KEY includes fl-name, lf-name, organizations, AKAs and email addresses.
;; When loading the database the hash table is initialized by calling
;; `bbdb-hash-record' for each record.  This function is also called
;; when new records are added to the database.
;; `bbdb-delete-record-internal' with arg REMHASH non-nil removes a record
;; from the hash table (besides deleting the record from the database).
;; When an existing record is modified, the code that modifies the record
;; needs to update the hash table, too.  This includes removing the outdated
;; associations between KEYs and record as well as adding the new associations.
;; This is one reason to modify records by calling `bbdb-record-set-field'
;; which properly updates the hash table.
;; The hash table can be accessed via `bbdb-gethash'
;; and via functions like `completing-read'.

(defun bbdb-puthash (key record)
  "Associate RECORD with KEY in `bbdb-hashtable'.
KEY must be a string or nil.  Empty strings and nil are ignored."
  (if (and key (not (string= "" key))) ; do not hash empty strings
      (let* ((key (downcase key))
             (records (gethash key bbdb-hashtable)))
        (puthash key (if records (bbdb-pushnewq record records)
                       (list record))
                 bbdb-hashtable))))

(defun bbdb-gethash (key &optional predicate)
  "Return list of records associated with KEY in `bbdb-hashtable'.
KEY must be a string or nil.  Empty strings and nil are ignored.
PREDICATE may take the same values as `bbdb-completion-list'."
  (when (and key (not (string= "" key)))
    (let* ((key (downcase key))
           (all-records (gethash key bbdb-hashtable))
           records)
      (if (or (not predicate) (eq t predicate))
          all-records
        (dolist (record all-records)
          (if (catch 'bbdb-hash-ok
                (bbdb-hash-p key record predicate))
              (push record records)))
        records))))

(defun bbdb-hash-p (key record predicate)
  "Throw `bbdb-hash-ok' non-nil if KEY matches RECORD acording to PREDICATE.
PREDICATE may take the same values as the elements of `bbdb-completion-list'."
  (if (and (memq 'fl-name predicate)
           (bbdb-string= key (or (bbdb-record-name record) "")))
      (throw 'bbdb-hash-ok 'fl-name))
  (if (and (memq 'lf-name predicate)
           (bbdb-string= key (or (bbdb-record-name-lf record) "")))
      (throw 'bbdb-hash-ok 'lf-name))
  (if (memq 'organization predicate)
      (mapc (lambda (organization) (if (bbdb-string= key organization)
                                       (throw 'bbdb-hash-ok 'organization)))
            (bbdb-record-organization record)))
  (if (memq 'aka predicate)
      (mapc (lambda (aka) (if (bbdb-string= key aka)
                              (throw 'bbdb-hash-ok 'aka)))
            (bbdb-record-field record 'aka-all)))
  (if (and (memq 'primary predicate)
           (bbdb-string= key (car (bbdb-record-mail-canon record))))
      (throw 'bbdb-hash-ok 'primary))
  (if (memq 'mail predicate)
      (mapc (lambda (mail) (if (bbdb-string= key mail)
                               (throw 'bbdb-hash-ok 'mail)))
            (bbdb-record-mail-canon record)))
  nil)

(defun bbdb-remhash (key record)
  "Remove RECORD from list of records associated with KEY.
KEY must be a string or nil.  Empty strings and nil are ignored."
  (if (and key (not (string= "" key)))
      (let* ((key (downcase key))
             (records (gethash key bbdb-hashtable)))
        (when records
          (setq records (delq record records))
          (if records
              (puthash key records bbdb-hashtable)
            (remhash key bbdb-hashtable))))))

(defun bbdb-hash-record (record)
  "Insert RECORD in `bbdb-hashtable'.
This performs all initializations required for a new record.
Do not call this for existing records that require updating."
  (bbdb-puthash (bbdb-record-name record) record)
  (bbdb-puthash (bbdb-record-name-lf record) record)
  (dolist (organization (bbdb-record-organization record))
    (bbdb-puthash organization record))
  (dolist (aka (bbdb-record-aka record))
    (bbdb-puthash aka record))
  (bbdb-puthash-mail record)
  (puthash (bbdb-record-uuid record) record bbdb-uuid-table))

(defun bbdb-puthash-mail (record)
  "For RECORD put mail into `bbdb-hashtable'."
  (let (mail-aka mail-canon address)
    (dolist (mail (bbdb-record-mail record))
      (setq address (bbdb-decompose-bbdb-address mail))
      (when (car address)
        (push (car address) mail-aka)
        (bbdb-puthash (car address) record))
      (push (nth 1 address) mail-canon)
      (bbdb-puthash (nth 1 address) record))
    (bbdb-cache-set-mail-aka (bbdb-record-cache record)
                             (nreverse mail-aka))
    (bbdb-cache-set-mail-canon (bbdb-record-cache record)
                               (nreverse mail-canon))))

(defun bbdb-hash-update (record old new)
  "Update hash for RECORD.  Remove OLD, insert NEW.
Both OLD and NEW are lists of values."
  (dolist (elt old)
    (bbdb-remhash elt record))
  (dolist (elt new)
    (bbdb-puthash elt record)))

(defun bbdb-check-name (first last &optional record)
  "Check whether the name FIRST LAST is a valid name.
This throws an error if the name is already used by another record
and `bbdb-allow-duplicates' is nil.  If RECORD is non-nil, FIRST and LAST
may correspond to RECORD without raising an error."
  ;; Are there more useful checks for names beyond checking for duplicates?
  (unless bbdb-allow-duplicates
    (let* ((name (bbdb-concat 'name-first-last first last))
           (records (bbdb-gethash name '(fl-name lf-name aka))))
      (if (or (and (not record) records)
              (remq record records))
          (error "%s is already in BBDB" name)))))

(defun bbdb-record-name (record)
  "Record cache function: Return the full name FIRST_LAST of RECORD.
Return empty string if both the first and last name are nil.
If the name is not available in the name cache, the name cache value
is generated and stored."
  (or (bbdb-cache-fl-name (bbdb-record-cache record))
      ;; Build the name cache for a record.
      (bbdb-record-set-name record t t)))

(defun bbdb-record-name-lf (record)
  "Record cache function: Return the full name LAST_FIRST of RECORD.
If the name is not available in the name cache, the name cache value
is generated and stored."
  (or (bbdb-cache-lf-name (bbdb-record-cache record))
      ;; Build the name cache for a record.
      (progn (bbdb-record-set-name record t t)
             (bbdb-cache-lf-name (bbdb-record-cache record)))))

(defun bbdb-record-set-name (record first last)
  "Record cache function: For RECORD set full name based on FIRST and LAST.
If FIRST or LAST are t use respective existing entries of RECORD.
Set full name in cache and hash.  Return first-last name."
  (let* ((cache (bbdb-record-cache record))
         (fl-name (bbdb-cache-fl-name cache))
         (lf-name (bbdb-cache-lf-name cache)))
    (if fl-name (bbdb-remhash fl-name record))
    (if lf-name (bbdb-remhash lf-name record)))
  (if (eq t first)
      (setq first (bbdb-record-firstname record))
    (bbdb-record-set-firstname record first))
  (if (eq t last)
      (setq last (bbdb-record-lastname record))
    (bbdb-record-set-lastname record last))
  (let ((fl-name (bbdb-concat 'name-first-last first last))
        (lf-name (bbdb-concat 'name-last-first last first))
        (cache (bbdb-record-cache record)))
    ;; Set cache of RECORD
    (bbdb-cache-set-fl-name cache fl-name)
    (bbdb-cache-set-lf-name cache lf-name)
    ;; Set hash.  For convenience, the hash contains the full name
    ;; as first-last and last-fist.
    (bbdb-puthash fl-name record)
    (bbdb-puthash lf-name record)
    fl-name))

(defun bbdb-record-sortkey (record)
  "Record cache function: Return the sortkey for RECORD.
Set and store it if necessary."
  (or (bbdb-cache-sortkey (bbdb-record-cache record))
      (bbdb-record-set-sortkey record)))

(defun bbdb-record-set-sortkey (record)
  "Record cache function: Set and return RECORD's sortkey."
  (bbdb-cache-set-sortkey
   (bbdb-record-cache record)
   (downcase
    (bbdb-concat "" (bbdb-record-lastname record)
                 (bbdb-record-firstname record)
                 (bbdb-record-organization record)))))

(defsubst bbdb-record-marker (record)
  "Record cache function: Return the marker for RECORD."
  (bbdb-cache-marker (bbdb-record-cache record)))

(defsubst bbdb-record-set-marker (record marker)
  "Record cache function: Set and return RECORD's MARKER."
  (bbdb-cache-set-marker (bbdb-record-cache record) marker))

(defsubst bbdb-record-xfield (record label)
  "For RECORD return value of xfield LABEL.
Return nil if xfield LABEL is undefined."
  (cdr (assq label (bbdb-record-xfields record))))

;; The values of xfields are normally strings.  The following function
;; comes handy if we want to treat these values as symbols.
(defun bbdb-record-xfield-intern (record label)
  "For RECORD return interned value of xfield LABEL.
Return nil if xfield LABEL does not exist."
  (let ((value (bbdb-record-xfield record label)))
    ;; If VALUE is not a string, return whatever it is.
    (if (stringp value) (intern value) value)))

(defun bbdb-record-xfield-string (record label)
  "For RECORD return value of xfield LABEL as string.
Return nil if xfield LABEL does not exist."
  (let ((value (bbdb-record-xfield record label)))
    (if (string-or-null-p value)
        value
      (let ((print-escape-newlines t))
        (prin1-to-string value)))))

(defsubst bbdb-record-xfield-split (record label)
  "For RECORD return value of xfield LABEL split as a list.
Splitting is based on `bbdb-separator-alist'."
  (let ((val (bbdb-record-xfield record label)))
    (cond ((stringp val) (bbdb-split label val))
          (val (error "Cannot split `%s'" val)))))

(defun bbdb-record-set-xfield (record label value)
  "For RECORD set xfield LABEL to VALUE.
If VALUE is nil or an empty string, remove xfield LABEL from RECORD.
Return VALUE."
  ;; In principle we can also have xfield labels `name' or `mail', etc.
  ;; Yet the actual code would get rather confused.  So we throw an error.
  (if (memq label '(name firstname lastname affix organization
                         mail aka phone address xfields))
      (error "xfield label `%s' illegal" label))
  (if (eq label 'mail-alias)
      (setq bbdb-mail-aliases-need-rebuilt 'edit))
  (if (stringp value) (setq value (bbdb-string-trim value t)))
  (let ((old-xfield (assq label (bbdb-record-xfields record))))
    ;; Do nothing if both OLD-XFIELD and VALUE are nil.
    (cond ((and old-xfield value) ; update
           (setcdr old-xfield value))
          (value ; new xfield
           (bbdb-pushnewq label bbdb-xfield-label-list)
           (bbdb-record-set-xfields record
                                    (append (bbdb-record-xfields record)
                                            (list (cons label value)))))
          (old-xfield ; remove
           (bbdb-record-set-xfields record
                                    (delq old-xfield
                                          (bbdb-record-xfields record))))))
  value)

(defun bbdb-check-type (object type &optional abort extended)
  "Return non-nil if OBJECT is of type TYPE.
TYPE is a pseudo-code as in `bbdb-record-type'.
If ABORT is non-nil, abort with error message if type checking fails.
If EXTENDED is non-nil, consider extended atomic types which may include
symbols, numbers, markers, and strings."
  (let (tmp)
    ;; Add more predicates?  Compare info node `(elisp.info)Type Predicates'.
    (or (cond ((eq type 'symbol) (symbolp object))
              ((eq type 'integer) (integerp object))
              ((eq type 'marker) (markerp object))
              ((eq type 'number) (numberp object))
              ((eq type 'string) (stringp object))
              ((eq type 'sexp) t) ; matches always
              ((eq type 'face) (facep object))
              ;; not quite a type
              ((eq type 'bound) (and (symbolp object) (boundp object)))
              ((eq type 'function) (functionp object))
              ((eq type 'vector) (vectorp object))
              ((and extended
                    (cond ((symbolp type) (setq tmp (eq type object)) t)
                          ((or (numberp type) (markerp type))
                           (setq tmp (= type object)) t)
                          ((stringp type)
                           (setq tmp (and (stringp object)
                                          (string= type object))) t)))
               tmp)
              ((not (consp type))
               (error "Atomic type `%s' undefined" type))
              ((eq 'const (setq tmp (car type)))
               (equal (nth 1 type) object))
              ((eq tmp 'cons)
               (and (consp object)
                    (bbdb-check-type (car object) (nth 1 type) abort extended)
                    (bbdb-check-type (cdr object) (nth 2 type) abort extended)))
              ((eq tmp 'list)
               (and (listp object)
                    (eq (length (cdr type)) (length object))
                    (let ((type (cdr type)) (object object) (ok t))
                      (while type
                        (unless (bbdb-check-type (pop object) (pop type)
                                                 abort extended)
                          (setq ok nil type nil)))
                      ok)))
              ((eq tmp 'repeat)
               (and (listp object)
                    (let ((tp (nth 1 type)) (object object) (ok t))
                      (while object
                        (unless (bbdb-check-type (pop object) tp abort extended)
                          (setq ok nil object nil)))
                      ok)))
              ((eq tmp 'vector)
               (and (vectorp object)
                    (let* ((i 0) (type (cdr type))
                           (ok (eq (length object) (length type))))
                      (when ok
                        (while type
                          (if (bbdb-check-type (aref object i) (pop type)
                                               abort extended)
                              (setq i (1+ i))
                            (setq ok nil type nil)))
                        ok))))
              ((eq tmp 'or) ; like customize `choice' type
               (let ((type (cdr type)) ok)
                 (while type
                   (if (bbdb-check-type object (pop type) nil extended)
                       (setq ok t type nil)))
                 ok))
              ;; User-defined predicate
              ((eq tmp 'user-p) (funcall (nth 1 type) object))
              (t (error "Compound type `%s' undefined" tmp)))
        (and abort
             (error "Type mismatch: expect %s, got `%s'" type object)))))

;; (bbdb-check-type 'bar 'symbol)
;; (bbdb-check-type 'bar 'bar)
;; (bbdb-check-type "foo" 'symbol t)
;; (bbdb-check-type "foo" '(or symbol string))
;; (bbdb-check-type nil '(const nil))
;; (bbdb-check-type '(bar . "foo") '(cons symbol string))
;; (bbdb-check-type '(bar "foo") '(list symbol string))
;; (bbdb-check-type '("bar" "foo") '(repeat string))
;; (bbdb-check-type (vector 'bar "foo") '(vector symbol string))
;; (bbdb-check-type (vector 'bar "foo") 'vector)
;; (bbdb-check-type '(bar (bar . "foo")) '(list symbol (cons symbol string)))
;; (bbdb-check-type '("aa" . "bb") '(or (const nil) (cons string string)) t)
;; (bbdb-check-type nil '(or nil (cons string string)) t t)
;; (bbdb-check-type "foo" '(user-p (lambda (a) (stringp a))))
;; (bbdb-check-type 'set 'function)

(defun bbdb-record-field (record field)
  "For RECORD return the value of FIELD.

FIELD may take the following values
 firstname     Return the first name of RECORD
 lastname      Return the last name of RECORD
 name          Return the full name of RECORD (first name first)
 name-lf       Return the full name of RECORD (last name first)
 affix         Return the list of affixes
 organization  Return the list of organizations
 aka           Return the list of AKAs
 aka-all       Return the list of AKAs plus mail-akas.
 mail          Return the list of email addresses
 mail-aka      Return the list of name parts in mail addresses
 mail-canon    Return the list of canonical mail addresses.
 phone         Return the list of phone numbers
 address       Return the list of addresses
 uuid          Return the uuid of RECORD
 creation-date Return the creation-date
 timestamp     Return the timestamp
 xfields       Return the list of all xfields

Any other symbol is interpreted as the label for an xfield.
Then return the value of this xfield.

See also `bbdb-record-set-field'."
  (cond ((eq field 'firstname) (bbdb-record-firstname record))
        ((eq field 'lastname) (bbdb-record-lastname record))
        ((eq field 'name)     (bbdb-record-name record))
        ((eq field 'name-lf)  (bbdb-record-name-lf record))
        ((eq field 'affix)    (bbdb-record-affix record))
        ((eq field 'organization)  (bbdb-record-organization record))
        ((eq field 'mail)     (bbdb-record-mail record))
        ((eq field 'mail-canon) (bbdb-record-mail-canon record)) ; derived (cached) field
        ((eq field 'mail-aka) (bbdb-record-mail-aka record)) ; derived (cached) field
        ((eq field 'aka)      (bbdb-record-aka record))
        ((eq field 'aka-all)  (append (bbdb-record-aka record) ; derived field
                                      (bbdb-record-mail-aka record)))
        ((eq field 'phone)    (bbdb-record-phone record))
        ((eq field 'address)  (bbdb-record-address record))
        ((eq field 'uuid)     (bbdb-record-uuid record))
        ((eq field 'creation-date) (bbdb-record-creation-date record))
        ((eq field 'timestamp) (bbdb-record-timestamp record))
        ;; Return all xfields
        ((eq field 'xfields)  (bbdb-record-xfields record))
        ;; Return xfield FIELD (e.g., `notes') or nil if FIELD is not defined.
        ((symbolp field) (bbdb-record-xfield record field))
        (t (error "Unknown field type `%s'" field))))
(define-obsolete-function-alias 'bbdb-record-get-field 'bbdb-record-field "3.0")

(defun bbdb-record-set-field (record field value &optional merge check)
  "For RECORD set FIELD to VALUE.  Return VALUE.
If MERGE is non-nil, merge VALUE with the current value of FIELD.
If CHECK is non-nil, check syntactically whether FIELD may take VALUE.
This function also updates the hash table.  However, it does not update
RECORD in the database.  Use `bbdb-change-record' for that.

FIELD may take the following values
 firstname     VALUE is the first name of RECORD
 lastname      VALUE is the last name of RECORD
 name          VALUE is the full name of RECORD either as one string
                 or as a cons pair (FIRST . LAST)
 affix         VALUE is the list of affixes
 organization  VALUE is the list of organizations
 aka           VALUE is the list of AKAs
 mail          VALUE is the list of email addresses
 phone         VALUE is the list of phone numbers
 address       VALUE is the list of addresses
 uuid          VALUE is the uuid of RECORD
 creation-date VALUE is the creation-date
 timestamp     VALUE is the timestamp
 xfields       VALUE is the list of all xfields

Any other symbol is interpreted as the label for an xfield.
Then VALUE is the value of this xfield.

See also `bbdb-record-field'."
  (bbdb-editable)
  (if (memq field '(name-lf mail-aka mail-canon aka-all))
      (error "`%s' is not allowed as the name of a field" field))
  (let ((record-type (cdr bbdb-record-type)))
    (cond ((eq field 'firstname) ; First name
           (if merge (error "Does not merge names"))
           (if check (bbdb-check-type value (bbdb-record-firstname record-type) t))
           (bbdb-check-name value (bbdb-record-lastname record) record)
           (bbdb-record-set-name record value t))

          ;; Last name
          ((eq field 'lastname)
           (if merge (error "Does not merge names"))
           (if check (bbdb-check-type value (bbdb-record-lastname record-type) t))
           (bbdb-check-name (bbdb-record-firstname record) value record)
           (bbdb-record-set-name record t value))

          ;; Name
          ((eq field 'name)
           (if merge (error "Does not merge names"))
           (if (stringp value)
               (setq value (bbdb-divide-name value))
             (if check (bbdb-check-type value '(cons string string) t)))
           (let ((fn (car value)) (ln (cdr value)))
             (bbdb-check-name fn ln record)
             (bbdb-record-set-name record fn ln)))

          ;; Affix
          ((eq field 'affix)
           (if merge (setq value (bbdb-merge-lists (bbdb-record-affix record)
                                                   value 'bbdb-string=)))
           (if check (bbdb-check-type value (bbdb-record-affix record-type) t))
           (setq value (bbdb-list-strings value))
           (bbdb-record-set-affix record value))

          ;; Organization
          ((eq field 'organization)
           (if merge (setq value (bbdb-merge-lists (bbdb-record-organization record)
                                                   value 'bbdb-string=)))
           (if check (bbdb-check-type value (bbdb-record-organization record-type) t))
           (setq value (bbdb-list-strings value))
           (bbdb-hash-update record (bbdb-record-organization record) value)
           (dolist (organization value)
             (bbdb-pushnew organization bbdb-organization-list))
           (bbdb-record-set-organization record value))

          ;; AKA
          ((eq field 'aka)
           (if merge (setq value (bbdb-merge-lists (bbdb-record-aka record)
                                                   value 'bbdb-string=)))
           (if check (bbdb-check-type value (bbdb-record-aka record-type) t))
           (setq value (bbdb-list-strings value))
           (unless bbdb-allow-duplicates
             (dolist (aka value)
               (let ((old (remq record (bbdb-gethash aka '(fl-name lf-name aka)))))
                 (if old (error "Alternate name address \"%s\" is used by \"%s\""
                                aka (mapconcat 'bbdb-record-name old ", "))))))
           (bbdb-hash-update record (bbdb-record-aka record) value)
           (bbdb-record-set-aka record value))

          ;; Mail
          ((eq field 'mail)
           (if merge (setq value (bbdb-merge-lists (bbdb-record-mail record)
                                                   value 'bbdb-string=)))
           (if check (bbdb-check-type value (bbdb-record-mail record-type) t))
           (setq value (bbdb-list-strings value))
           (unless bbdb-allow-duplicates
             (dolist (mail value)
               (let ((old (remq record (bbdb-gethash mail '(mail)))))
                 (if old (error "Mail address \"%s\" is used by \"%s\""
                                mail (mapconcat 'bbdb-record-name old ", "))))))
           (dolist (aka (bbdb-record-mail-aka record))
             (bbdb-remhash aka record))
           (dolist (mail (bbdb-record-mail-canon record))
             (bbdb-remhash mail record))
           (bbdb-record-set-mail record value)
           (bbdb-puthash-mail record))

          ;; Phone
          ((eq field 'phone)
           (if merge (setq value (bbdb-merge-lists (bbdb-record-phone record)
                                                   value 'equal)))
           (if check (bbdb-check-type value (bbdb-record-phone record-type) t))
           (dolist (phone value)
             (bbdb-pushnew (bbdb-phone-label phone) bbdb-phone-label-list))
           (bbdb-record-set-phone record value))

          ;; Address
          ((eq field 'address)
           (if merge (setq value (bbdb-merge-lists (bbdb-record-address record)
                                                   value 'equal)))
           (if check (bbdb-check-type value (bbdb-record-address record-type) t))
           (dolist (address value)
             (bbdb-pushnew (bbdb-address-label address) bbdb-address-label-list)
             (mapc (lambda (street) (bbdb-pushnewt street bbdb-street-list))
                   (bbdb-address-streets address))
             (bbdb-pushnewt (bbdb-address-city address) bbdb-city-list)
             (bbdb-pushnewt (bbdb-address-state address) bbdb-state-list)
             (bbdb-pushnewt (bbdb-address-postcode address) bbdb-postcode-list)
             (bbdb-pushnewt (bbdb-address-country address) bbdb-country-list))
           (bbdb-record-set-address record value))

          ;; uuid
          ((eq field 'uuid)
           ;; MERGE not meaningful
           (if check (bbdb-check-type value (bbdb-record-uuid record-type) t))
           (let ((old-uuid (bbdb-record-uuid record)))
             (unless (string= old-uuid value)
               (remhash old-uuid bbdb-uuid-table)
               (bbdb-record-set-uuid record value)
               (puthash value record bbdb-uuid-table))))

          ;; creation-date
          ((eq field 'creation-date)
           ;; MERGE not meaningful
           (if check (bbdb-check-type value (bbdb-record-creation-date record-type) t))
           (bbdb-record-set-creation-date record value))

          ;; timestamp
          ((eq field 'timestamp)
           ;; MERGE not meaningful
           (if check (bbdb-check-type value (bbdb-record-timestamp record-type) t))
           (bbdb-record-set-timestamp record value))

          ;; all xfields
          ((eq field 'xfields)
           (if merge
               (let ((xfields (bbdb-record-xfields record))
                     xfield)
                 (dolist (nv value)
                   (if (setq xfield (assq (car nv) xfields))
                       (setcdr xfield (bbdb-merge-xfield
                                       (car nv) (cdr xfield) (cdr nv)))
                     (setq xfields (append xfields (list nv)))))
                 (setq value xfields)))
           (if check (bbdb-check-type value (bbdb-record-xfields record-type) t))
           (let (new-xfields)
             (dolist (xfield value)
               ;; Ignore junk
               (when (and (cdr xfield) (not (equal "" (cdr xfield))))
                 (push xfield new-xfields)
                 (bbdb-pushnewq (car xfield) bbdb-xfield-label-list)))
             (bbdb-record-set-xfields record (nreverse new-xfields))))

          ;; Single xfield
          ((symbolp field)
           (if merge
               (setq value (bbdb-merge-xfield field (bbdb-record-xfield record field)
                                              value)))
           ;; The following test always succeeds
           ;; (if check (bbdb-check-type value 'sexp t))
           ;; This removes xfield FIELD if its value is nil.
           (bbdb-record-set-xfield record field value))

          (t (error "Unknown field type `%s'" field)))))

;; Currently unused (but possible entry for `bbdb-merge-xfield-function-alist')
(defun bbdb-merge-concat (string1 string2 &optional separator)
  "Return the concatenation of STRING1 and STRING2.
SEPARATOR defaults to \"\\n\"."
  (concat string1 (or separator "\n") string2))

;; Currently unused (but possible entry for `bbdb-merge-xfield-function-alist')
(defun bbdb-merge-concat-remove-duplicates (string1 string2)
  "Concatenate STRING1 and STRING2, but remove duplicate lines."
  (let ((lines (split-string string1 "\n")))
    (dolist (line (split-string string2 "\n"))
      (bbdb-pushnew line lines))
    (bbdb-concat "\n" lines)))

(defun bbdb-merge-string-least (string1 string2)
  "Return the string out of STRING1 and STRING2 that is `string-lessp'."
  (if (string-lessp string1 string2)
      string1
    string2))

(defun bbdb-merge-string-most (string1 string2)
  "Return the string out of STRING1 and STRING2 that is not `string-lessp'."
  (if (string-lessp string1 string2)
      string2
    string1))

(defun bbdb-merge-lists (l1 l2 cmp)
  "Merge two lists L1 and L2 based on comparison CMP.
An element from L2 is added to L1 if CMP returns nil for all elements of L1.
If L1 or L2 are not lists, they are replaced by (list L1) and (list L2)."
  (let (merge)
    (unless (listp l1) (setq l1 (list l1)))
    (dolist (e2 (if (listp l2) l2 (list l2)))
      (let ((ll1 l1) e1 fail)
        (while (setq e1 (pop ll1))
          (if (funcall cmp e1 e2)
              (setq ll1 nil
                    fail t)))
        (unless fail (push e2 merge))))
    (append l1 (nreverse merge))))

(defun bbdb-merge-xfield (label value1 value2)
  "For LABEL merge VALUE1 with VALUE2.
If LABEL has an entry in `bbdb-merge-xfield-function-alist', use it.
If VALUE1 or VALUE2 is a substring of the other, return the longer one.
Otherwise use `bbdb-concat'.  Return nil if we have nothing to merge."
  (if (stringp value1) (setq value1 (bbdb-string-trim value1 t)))
  (if (stringp value2) (setq value2 (bbdb-string-trim value2 t)))
  (cond ((and value1 value2)
         (let ((fun (cdr (assq label bbdb-merge-xfield-function-alist))))
           (cond (fun (funcall fun value1 value2))
                 ((not (and (stringp value1) (stringp value2)))
                  (cons value1 value2)) ; concatenate lists
                 ((string-match (regexp-quote value1) value2) value2)
                 ((string-match (regexp-quote value2) value1) value1)
                 (t (bbdb-concat label value1 value2)))))
        (value1)
        (value2)))

;;; Parsing other things

(defun bbdb-divide-name (string)
  "Divide STRING into a first name and a last name.
Case is ignored.  Return name as (FIRST . LAST).
LAST is always a string (possibly empty).  FIRST may be nil."
  (let ((case-fold-search t)
        first suffix)
    ;; Separate a suffix.
    (if (string-match bbdb-lastname-suffix-re string)
        (setq suffix (concat " " (match-string 1 string))
              string (substring string 0 (match-beginning 0))))
    (cond ((string-match "\\`\\(.+\\),[ \t\n]*\\(.+\\)\\'" string)
           ;; If STRING contains a comma, this probably means that STRING
           ;; is of the form "Last, First".
           (setq first (match-string 2 string)
                 string (match-string 1 string)))
          ((string-match bbdb-lastname-re string)
           (setq first (and (not (zerop (match-beginning 0)))
                            (substring string 0 (match-beginning 0)))
                 string (match-string 1 string))))
    (cons (and first (bbdb-string-trim first))
          (bbdb-string-trim (concat string suffix)))))

(defun bbdb-parse-postcode (string)
  "Check whether STRING is a legal postcode.
Do this only if `bbdb-check-postcode' is non-nil."
  (if bbdb-check-postcode
      (let ((postcodes bbdb-legal-postcodes) re done)
        (while (setq re (pop postcodes))
          (if (string-match re string)
              (setq done t postcodes nil)))
        (if done string
          (error "not a valid postcode.")))
    string))

(defun bbdb-phone-string (phone)
  "Massage string PHONE into a standard format."
  ;; Phone numbers should come in two forms:
  (if (= 2 (length phone))
      ;; (1) ["where" "the number"]
      (if (stringp (aref phone 1))
          (aref phone 1)
        (error "Not a valid phone number: %s" (aref phone 1)))
    ;; (2) ["where" 415 555 1212 99]
    (unless (and (integerp (aref phone 2))
                 (integerp (aref phone 3)))
      (error "Not an NANP number: %s %s" (aref phone 2) (aref phone 3)))
    (concat (if (/= 0 (bbdb-phone-area phone))
                (format "(%03d) " (bbdb-phone-area phone))
                "")
            (if (/= 0 (bbdb-phone-exchange phone))
                (format "%03d-%04d"
                        (bbdb-phone-exchange phone) (bbdb-phone-suffix phone))
                "")
            (if (and (bbdb-phone-extension phone)
                     (/= 0 (bbdb-phone-extension phone)))
                (format " x%d" (bbdb-phone-extension phone))
                ""))))

(defsubst bbdb-record-lessp (record1 record2)
  (string< (bbdb-record-sortkey record1)
           (bbdb-record-sortkey record2)))

(defmacro bbdb-error-retry (&rest body)
  "Repeatedly execute BODY ignoring errors till no error occurs."
  `(catch '--bbdb-error-retry--
     (while t
       (condition-case --c--
           (throw '--bbdb-error-retry-- (progn ,@body))
         (error (ding)
                (message "Error: %s" (nth 1 --c--))
                (sit-for 2))))))


;;; Reading and Writing the BBDB

(defun bbdb-buffer ()
  "Return buffer that visits the BBDB file `bbdb-file'.
Ensure that this buffer is in sync with `bbdb-file'.
Revert the buffer if necessary.
If `bbdb-file-remote' is non-nil and it is newer than `bbdb-file',
copy it to `bbdb-file'."
  (unless (buffer-live-p bbdb-buffer)
    (if (and bbdb-file-remote
             (file-newer-than-file-p bbdb-file-remote bbdb-file))
        (copy-file bbdb-file-remote bbdb-file t t))

    (with-current-buffer (setq bbdb-buffer (find-file-noselect bbdb-file))

      ;; Check whether auto-save file is newer than `bbdb-file'
      ;; Do this only when reading `bbdb-file'.
      (let ((auto-save-file (make-auto-save-file-name)))
        (when (and bbdb-check-auto-save-file
                   (file-newer-than-file-p auto-save-file buffer-file-name))
          (recover-file buffer-file-name) ; this queries
          (bury-buffer) ; `recover-file' selects `bbdb-buffer'
          (auto-save-mode 1) ; turn auto-save back on
          ;; Delete auto-save file even if the user rejected to recover it,
          ;; so we do not keep asking.
          (condition-case nil
              (delete-file auto-save-file)
            (file-error nil))))))

  ;; Make sure `bbdb-buffer' is not out of sync with disk.
  (with-current-buffer bbdb-buffer
    (cond ((verify-visited-file-modtime))
          ((bbdb-revert-buffer))
          ;; This is the case where `bbdb-file' has changed; the buffer
          ;; has changed as well; and the user has answered "no" to the
          ;; "flush your changes and revert" question.  The only other
          ;; alternative is to save the file right now.  If they answer
          ;; no to the following question, they will be asked the
          ;; preceeding question again and again some large (but finite)
          ;; number of times.  `bbdb-buffer' is called a lot, you see...
          ((buffer-modified-p)
           ;; this queries
           (bbdb-save t t))
          (t ; Buffer and file are inconsistent, but we let them stay that way
           (message "Continuing with inconsistent BBDB buffers")))

    ;; `bbdb-revert-buffer' kills all local variables.
    (unless (assq 'bbdb-records (buffer-local-variables))
      ;; We are reading / reverting `bbdb-buffer'.
      (set (make-local-variable 'revert-buffer-function)
           'bbdb-revert-buffer)

      (setq buffer-file-coding-system bbdb-file-coding-system
            buffer-read-only bbdb-read-only
            bbdb-mail-aliases-need-rebuilt 'parse
            bbdb-changed-records nil)

      ;; `bbdb-before-save-hook' and `bbdb-after-save-hook' are user variables.
      ;; To avoid confusion, we hide the hook functions `bbdb-before-save'
      ;; and `bbdb-after-save' from the user as these are essential for BBDB.
      (dolist (hook (cons 'bbdb-before-save bbdb-before-save-hook))
        (add-hook 'before-save-hook hook nil t))
      (dolist (hook (cons 'bbdb-after-save bbdb-after-save-hook))
        (add-hook 'after-save-hook hook nil t))

      (clrhash bbdb-hashtable)
      (clrhash bbdb-uuid-table)

      (if (/= (point-min) (point-max))
          (bbdb-parse-records) ; normal case: nonempty db
        ;; Empty db: the following does not require `insert-before-markers'
        ;; because there are no db-markers in this buffer.
        (insert (format (concat ";; -*- mode: Emacs-Lisp; coding: %s; -*-"
                                "\n;;; file-format: %d\n")
                        bbdb-file-coding-system bbdb-file-format))
        ;; We pretend that `bbdb-buffer' is still unmodified,
        ;; so that we will (auto-)save it only if we also add records to it.
        (set-buffer-modified-p nil)
        (setq bbdb-end-marker (point-marker)
              ;; Setting `bbdb-records' makes it buffer-local,
              ;; so that we can use it as a test whether we have
              ;; initialized BBDB.
              bbdb-records nil))

      (run-hooks 'bbdb-after-read-db-hook)))

  ;; return `bbdb-buffer'
  bbdb-buffer)

(defmacro bbdb-with-db-buffer (&rest body)
  "Execute the forms in BODY with `bbdb-buffer' temporarily current.
If `bbdb-debug' was non-nil at compile-time, and `bbdb-buffer' is visible
in a window, temporarilly switch to that window.  So when we come out,
that window has been scrolled to the record we have just modified."
  (declare (indent 0))
  (if bbdb-debug
      `(let* ((buffer (bbdb-buffer))
              (window (get-buffer-window buffer)))
         (if window
             (with-selected-window window
               ,@body)
           (with-current-buffer buffer
             ,@body)))
    `(with-current-buffer (bbdb-buffer)
       ,@body)))

(defun bbdb-editable ()
  "Ensure that BBDB is editable, otherwise throw an error.
If BBDB is out of sync try to revert.
BBDB is not editable if it is read-only."
  (if bbdb-read-only (error "BBDB is read-only"))
  (let ((buffer (bbdb-buffer))) ; this reverts if necessary / possible
    ;; Is the following possible?  Superfluous tests do not hurt.
    ;; It is relevant only for editing commands in a BBDB buffer,
    ;; but not for MUA-related editing functions.
    (if (and (eq major-mode 'bbdb-mode)
             bbdb-records
             (not (memq (caar bbdb-records)
                        (with-current-buffer buffer bbdb-records))))
        (error "BBDB is out of sync")))
  t)

;;;###autoload
(defsubst bbdb-records ()
  "Return a list of all BBDB records; read in and parse the db if necessary.
This function also notices if the corresponding file on disk has been modified."
  (with-current-buffer (bbdb-buffer)
    bbdb-records))

(defun bbdb-revert-buffer (&optional ignore-auto noconfirm)
  "The `revert-buffer-function' for `bbdb-buffer' visiting `bbdb-file'.
IGNORE-AUTO and NOCONFIRM have same meaning as in `revert-buffer'.
See also variable `bbdb-auto-revert'.
Return t if the reversion was successful (or not needed).
Return nil otherwise."
  (interactive (list (not current-prefix-arg))) ; as in `revert-buffer'
  (unless (buffer-live-p bbdb-buffer)
    (error "No live BBDB buffer to revert"))
  (with-current-buffer bbdb-buffer
    (cond ((not buffer-file-number)
           ;; We have not yet created `bbdb-file'
           (when (or noconfirm
                     (yes-or-no-p "Flush your changes? "))
             (erase-buffer)
             (kill-all-local-variables)  ; clear database
             (bbdb-buffer)               ; re-initialize
             (set-buffer-modified-p nil)
             (bbdb-undisplay-records t)))
          ;; If nothing has changed do nothing, return t.
          ((and (verify-visited-file-modtime)
                (not (buffer-modified-p))))
          ((or (and (not (verify-visited-file-modtime bbdb-buffer))
                    ;; File changed on disk
                    (or noconfirm
                        (and bbdb-auto-revert
                             (not (buffer-modified-p)))
                        (yes-or-no-p
                         (if (buffer-modified-p)
                             "BBDB changed on disk; flush your changes and revert? "
                           "BBDB changed on disk; revert? "))))
               (and (verify-visited-file-modtime bbdb-buffer)
                    ;; File not changed on disk, but buffer modified
                    (buffer-modified-p)
                    (or noconfirm
                        (yes-or-no-p "Flush your changes and revert BBDB? "))))
           (unless (file-exists-p bbdb-file)
             (error "BBDB: file %s no longer exists" bbdb-file))
           (kill-all-local-variables)  ; clear database
           ;; `revert-buffer-function' has the permanent-local property
           ;; So to avoid looping, we need to bind it to nil explicitly.
           (let (revert-buffer-function)
             (revert-buffer ignore-auto t))
           (bbdb-buffer)                      ; re-initialize
           (bbdb-undisplay-records t)
           t)))) ; return nil if the user rejected to revert

(defun bbdb-goto-first-record ()
  "Go to where first record begins,  Move to end of file if no records."
  (goto-char (point-min))
  (if (search-forward "\n[" nil 'move)
      (forward-char -1)))

(defun bbdb-parse-records ()
  "Parse BBDB records and initialize various internal variables.
If `bbdb-file' uses an outdated format, migrate to `bbdb-file-format'."
  (save-excursion
    (save-restriction
      (widen)
      (bbdb-goto-first-record)
      (let* ((file (abbreviate-file-name buffer-file-name))
             (file-format (save-excursion
                            (if (re-search-backward
                                 "^;+[ \t]*file-\\(format\\|version\\):[ \t]*\\([0-9]+\\)[ \t]*$" nil t)
                                (string-to-number (match-string 2))
                              ;; No file-format line.
                              (error "BBDB corrupted: no file-format line"))))
             (migrate (< file-format bbdb-file-format))
             records)
        (if (> file-format bbdb-file-format)
            (error "%s understands file format %s but not %s."
                   (bbdb-version) bbdb-file-format file-format))

        (if (and migrate
                 (not (yes-or-no-p
                       (format (concat "Migrate `%s' to BBDB file format %s "
                                       "(back-up recommended)? ")
                               file bbdb-file-format))))
            (progn
              (message "Abort loading %s" file)
              (sleep-for 2)
              (setq bbdb-records nil
                    ;; Avoid unexpected surprises
                    buffer-read-only t)
              'abort)

          (or (eobp) (looking-at "\\[")
              (error "BBDB corrupted: no following bracket"))

          (unless bbdb-silent (message "Parsing BBDB file `%s'..." file))

          ;; narrow the buffer to skip over the rubbish before the first record.
          (narrow-to-region (point) (point-max))
          (let ((modp (buffer-modified-p))
                ;; Make sure those parens get cleaned up.
                ;; This code had better stay simple!
                (inhibit-quit t)
                (buffer-undo-list t)
                buffer-read-only)
            (goto-char (point-min)) (insert "(\n")
            (goto-char (point-max)) (insert "\n)")
            (goto-char (point-min))
            (unwind-protect
                (setq records (read (current-buffer)))
              (goto-char (point-min)) (delete-char 2)
              (goto-char (point-max)) (delete-char -2)
              (set-buffer-modified-p modp)))
          (widen)

          ;; Migrate if `bbdb-file' is outdated.
          (if migrate (setq records (bbdb-migrate records file-format)))

          ;; We could first set `bbdb-phone-label-list' and
          ;; `bbdb-address-label-list' to their customized values.  Bother?
          (setq bbdb-records records
                bbdb-xfield-label-list nil
                bbdb-organization-list nil
                bbdb-street-list nil
                bbdb-city-list nil
                bbdb-state-list nil
                bbdb-postcode-list nil
                bbdb-country-list nil)

          (bbdb-goto-first-record)
          (dolist (record records)
            ;; We assume that the markers for each record need to go at each
            ;; newline.  If this is not the case, things can go *very* wrong.
            (bbdb-debug
              (unless (looking-at "\\[")
                (error "BBDB corrupted: junk between records at %s" (point))))

            (bbdb-cache-set-marker
             (bbdb-record-set-cache record (make-vector bbdb-cache-length nil))
             (point-marker))
            (forward-line 1)

            ;; Every record must have a unique uuid in `bbdb-uuid-table'.
            (if (gethash (bbdb-record-uuid record) bbdb-uuid-table)
                ;; Is there a more useful action than throwing an error?
                ;; We are just loading BBDB, so we are not yet ready
                ;; for sophisticated solutions.
                (error "Duplicate UUID %s" (bbdb-record-uuid record)))

            ;; Set the completion lists
            (dolist (phone (bbdb-record-phone record))
              (bbdb-pushnew (bbdb-phone-label phone) bbdb-phone-label-list))
            (dolist (address (bbdb-record-address record))
              (bbdb-pushnew (bbdb-address-label address) bbdb-address-label-list)
              (mapc (lambda (street) (bbdb-pushnewt street bbdb-street-list))
                    (bbdb-address-streets address))
              (bbdb-pushnewt (bbdb-address-city address) bbdb-city-list)
              (bbdb-pushnewt (bbdb-address-state address) bbdb-state-list)
              (bbdb-pushnewt (bbdb-address-postcode address) bbdb-postcode-list)
              (bbdb-pushnewt (bbdb-address-country address) bbdb-country-list))
            (dolist (xfield (bbdb-record-xfields record))
              (bbdb-pushnewq (car xfield) bbdb-xfield-label-list))
            (dolist (organization (bbdb-record-organization record))
              (bbdb-pushnew organization bbdb-organization-list))

            (let ((name (bbdb-concat 'name-first-last
                                     (bbdb-record-firstname record)
                                     (bbdb-record-lastname record))))
              (when (and (not bbdb-allow-duplicates)
                         (bbdb-gethash name '(fl-name aka)))
                ;; This does not check for duplicate mail fields.
                ;; Yet under normal circumstances, this should really
                ;; not be necessary each time BBDB is loaded as BBDB checks
                ;; whether creating a new record or modifying an existing one
                ;; results in duplicates.
                ;; Alternatively, you can use `bbdb-search-duplicates'.
                (message "Duplicate BBDB record encountered: %s" name)
                (sit-for 1)))

            ;; If `bbdb-allow-duplicates' is non-nil, we allow that two records
            ;; (with different uuids) refer to the same person (same name etc.).
            ;; Such duplicate records are always hashed.
            ;; Otherwise, an unhashed record would not be available for things
            ;; like completion (and we would not know which record to keeep
            ;; and which one to hide).  We trust the user she knows what
            ;; she wants if she keeps duplicate records in the database though
            ;; `bbdb-allow-duplicates' is nil.
            (bbdb-hash-record record))

          ;; Note that `bbdb-xfield-label-list' serves two purposes:
          ;;  - check whether an xfield is new to BBDB
          ;;  - list of known xfields for minibuffer completion
          ;; Only in the latter case, we might want to exclude
          ;; those xfields that are handled automatically.
          ;; So the following is not a satisfactory solution.

          ;; (dolist (label (bbdb-layout-get-option 'multi-line 'omit))
          ;;   (setq bbdb-xfield-label-list (delq label bbdb-xfield-label-list)))

          ;; `bbdb-end-marker' allows to put comments at the end of `bbdb-file'
          ;; that are ignored.
          (setq bbdb-end-marker (point-marker))

          (when migrate
            (dolist (record bbdb-records)
              (bbdb-overwrite-record-internal record))
            ;; update file format
            (goto-char (point-min))
            (if (re-search-forward (format "^;;; file-\\(version\\|format\\): %d$"
                                           file-format)
                                   nil t)
                (replace-match (format ";;; file-format: %d" bbdb-file-format))))

          (unless bbdb-silent (message "Parsing BBDB file `%s'...done" file))
          bbdb-records)))))

(defun bbdb-before-save ()
  "Run before saving `bbdb-file' as buffer-local part of `before-save-hook'."
  (when (and bbdb-file-remote
             (or bbdb-file-remote-save-always
                 (y-or-n-p (format "Save the remote BBDB file %s too? "
                                   bbdb-file-remote))))
    ;; Write the current buffer `bbdb-file' into `bbdb-file-remote'.
    (let ((coding-system-for-write bbdb-file-coding-system))
      (write-region (point-min) (point-max) bbdb-file-remote))))

(defun bbdb-after-save ()
  "Run after saving `bbdb-file' as buffer-local part of `after-save-hook'."
  (setq bbdb-changed-records nil)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (if (eq major-mode 'bbdb-mode)
          (set-buffer-modified-p nil)))))

(defun bbdb-change-record (record &rest ignored)
  "Update the database after a change of RECORD.
Return RECORD if RECORD got changed compared with the database,
return nil otherwise.
Hash RECORD if it is new.  If RECORD is not new, it is the the caller's
responsibility to update the hashtables for RECORD.  (Up-to-date hashtables are
ensured if the fields are modified by calling `bbdb-record-set-field'.)
Redisplay RECORD if it is not new.

Args IGNORED are ignored and their use is discouraged.
They are present only for backward compatibility."
  (when (and ignored (get 'bbdb-change-record 'bbdb-outdated))
    (put 'bbdb-change-record 'bbdb-outdated t)
    (message "Outdated usage of `bbdb-change-record'")
    (sit-for 2))

  (if bbdb-read-only
      (error "The Insidious Big Brother Database is read-only."))
  ;; The call of `bbdb-records' checks file synchronization.
  ;; If RECORD refers to an existing record that has been changed,
  ;; yet in the meanwhile we reverted the BBDB file, then RECORD
  ;; no longer refers to a record in `bbdb-records'.  RECORD will then
  ;; be treated as new, when we try to merge it with the known record.
  (let ((tail (memq record (bbdb-records))))
    (if tail ; RECORD is not new
        ;; If the string we currently have for RECORD in `bbdb-buffer'
        ;; is `equal' to the string we would write to `bbdb-buffer',
        ;; we really did not change RECORD at all.  So we don't update RECORD
        ;; unless `bbdb-update-unchanged-records' tells us to do so anyway.
        ;; Also, we only call `bbdb-change-hook' and `bbdb-after-change-hook'
        ;; if RECORD got changed.
        (when (or bbdb-update-unchanged-records
                  (not (string= (bbdb-with-db-buffer
                                  (buffer-substring-no-properties
                                   (bbdb-record-marker record)
                                   (1- (if (cdr tail)
                                           (bbdb-record-marker (cadr tail))
                                         bbdb-end-marker))))
                                (let ((cache (bbdb-record-cache record))
                                      (inhibit-quit t))
                                  (bbdb-record-set-cache record nil)
                                  (prog1 (bbdb-with-print-loadably
                                           (prin1-to-string record))
                                    (bbdb-record-set-cache record cache))))))
          (bbdb-record-set-timestamp
           record (format-time-string bbdb-time-stamp-format nil t))
          (run-hook-with-args 'bbdb-change-hook record)
          (let ((sort (not (equal (bbdb-cache-sortkey (bbdb-record-cache record))
                                  (bbdb-record-set-sortkey record)))))
            (if (not sort) ;; If we do not need to sort, overwrite RECORD.
                (bbdb-overwrite-record-internal record)
              ;; Since we need to sort, delete then insert RECORD.
              ;; Do not mess with the hash tables here.
              ;; We assume they got updated by the caller.
              (bbdb-delete-record-internal record)
              (bbdb-insert-record-internal record))
            (bbdb-pushnewq record bbdb-changed-records)
            (run-hook-with-args 'bbdb-after-change-hook record)
            (bbdb-redisplay-record-globally record sort))
          record)

      ;; Record is new and not yet in BBDB.
      (unless (bbdb-record-cache record)
        (bbdb-record-set-cache record (make-vector bbdb-cache-length nil)))
      (unless (bbdb-record-uuid record)
        (bbdb-record-set-uuid record (bbdb-uuid)))
      (unless (bbdb-record-creation-date record)
        (bbdb-record-set-creation-date
         record (format-time-string bbdb-time-stamp-format nil t))
        (run-hook-with-args 'bbdb-create-hook record))

      (let ((old-record (gethash (bbdb-record-uuid record) bbdb-uuid-table)))
        (if old-record
            ;; RECORD is really OLD-RECORD.  Merge and return OLD-RECORD.
            (if bbdb-merge-records-function
                (funcall bbdb-merge-records-function record old-record)
              (bbdb-merge-records record old-record))

          ;; RECORD is really new.
          (bbdb-record-set-timestamp
           record (format-time-string bbdb-time-stamp-format nil t))
          (run-hook-with-args 'bbdb-change-hook record)
          (bbdb-insert-record-internal record)
          (bbdb-hash-record record)
          (bbdb-pushnewq record bbdb-changed-records)
          (run-hook-with-args 'bbdb-after-change-hook record)
          record)))))

(defun bbdb-delete-record-internal (record &optional completely)
  "Delete RECORD in the database file.
With COMPLETELY non-nil, also undisplay RECORD and remove it
from the hash table."
  (unless (bbdb-record-marker record) (error "BBDB: marker absent"))
  (if completely (bbdb-redisplay-record-globally record nil t))
  (bbdb-with-db-buffer
    (barf-if-buffer-read-only)
    (let ((tail (memq record bbdb-records))
          (inhibit-quit t))
      (unless tail (error "BBDB record absent: %s" record))
      (delete-region (bbdb-record-marker record)
                     (if (cdr tail)
                         (bbdb-record-marker (car (cdr tail)))
                       bbdb-end-marker))
      (setq bbdb-records (delq record bbdb-records))
      (when completely
        (bbdb-remhash (bbdb-record-name record) record)
        (bbdb-remhash (bbdb-record-name-lf record) record)
        (dolist (organization (bbdb-record-organization record))
          (bbdb-remhash organization record))
        (dolist (mail (bbdb-record-mail-canon record))
          (bbdb-remhash mail record))
        (dolist (aka (bbdb-record-field record 'aka-all))
          (bbdb-remhash aka record))))))

(defun bbdb-insert-record-internal (record)
  "Insert RECORD into the database file.  Return RECORD.
Do not call this function directly, call instead `bbdb-change-record'
that calls the hooks, too."
  (unless (bbdb-record-marker record)
    (bbdb-record-set-marker record (make-marker)))
  (bbdb-with-db-buffer
    (barf-if-buffer-read-only)
    ;; splice record into `bbdb-records'
    (bbdb-debug (if (memq record bbdb-records)
                    (error "BBDB record not unique: - %s" record)))
    (if (or (not bbdb-records) ; first record in new database
            (bbdb-record-lessp record (car bbdb-records)))
        (push record bbdb-records)
      (let ((records bbdb-records))
        (while (and (cdr records)
                    (bbdb-record-lessp (nth 1 records) record))
          (setq records (cdr records)))
        (setcdr records (cons record (cdr records)))))

    (let ((next (car (cdr (memq record bbdb-records)))))
      (goto-char (if next
                     (bbdb-record-marker next)
                   bbdb-end-marker)))
    ;; Before writing the record, remove the cache (we do not want that
    ;; written to the file.)  After writing, put the cache back and update
    ;; the cache's marker.
    (let ((cache (bbdb-record-cache record))
          (point (point))
          (inhibit-quit t))
      (bbdb-debug
        (if (= point (point-min))
            (error "Inserting at point-min (%s)" point))
        (if (and (/= point bbdb-end-marker)
                 (not (looking-at "^\\[")))
            (error "Not inserting before a record (%s)" point)))
      (bbdb-record-set-cache record nil)
      (insert-before-markers
       (bbdb-with-print-loadably (prin1-to-string record)) "\n")
      (set-marker (bbdb-cache-marker cache) point)
      (bbdb-record-set-cache record cache))
    record))

(defun bbdb-overwrite-record-internal (record)
  "Overwrite RECORD in the database file.  Return RECORD.
Do not call this function directly, call instead `bbdb-change-record'
that calls the hooks, too."
  (bbdb-with-db-buffer
    (barf-if-buffer-read-only)
    (let* ((tail (memq record bbdb-records))
           (_ (unless tail (error "BBDB record absent: %s" record)))
           (cache (bbdb-record-cache record))
           (inhibit-quit t))
      (bbdb-debug
        (if (<= (bbdb-cache-marker cache) (point-min))
            (error "Cache marker is %s" (bbdb-cache-marker cache))))
      (goto-char (bbdb-cache-marker cache))
      (bbdb-debug
        (if (and (/= (point) bbdb-end-marker)
                 (not (looking-at "\\[")))
            (error "Not inserting before a record (%s)" (point))))

      (bbdb-record-set-cache record nil)
      (insert (bbdb-with-print-loadably (prin1-to-string record)) "\n")
      (delete-region (point)
                     (if (cdr tail)
                         (bbdb-record-marker (car (cdr tail)))
                       bbdb-end-marker))
      (bbdb-record-set-cache record cache)

      (bbdb-debug
        (if (<= (if (cdr tail)
                    (bbdb-record-marker (car (cdr tail)))
                  bbdb-end-marker)
                (bbdb-record-marker record))
            (error "Overwrite failed")))

      record)))

;; Record formatting:
;; This does not insert anything into the *BBDB* buffer,
;; which is handled in a second step by the display functions.

(defun bbdb-layout-get-option (layout option)
  "For LAYOUT return value of OPTION according to `bbdb-layout-alist'."
  (let ((layout-spec (if (listp layout)
                         layout
                       (assq layout bbdb-layout-alist)))
        option-value)
    (and layout-spec
         (setq option-value (assq option layout-spec))
         (cdr option-value))))

(defun bbdb-address-continental-p (address)
  "Return non-nil if ADDRESS is a continental address.
This is done by comparing the postcode to `bbdb-continental-postcode-regexp'.

This is a possible identifying function for
`bbdb-address-format-list' and `bbdb-tex-address-format-list'."
  (string-match bbdb-continental-postcode-regexp
                (bbdb-address-postcode address)))

;; This function can provide some guidance for writing
;; your own address formatting function
(defun bbdb-format-address-default (address)
  "Return formatted ADDRESS as a string.
This is the default format; it is used in the US, for example.
The result looks like this:
       label: street
              street
              ...
              city, state postcode
              country.

This function is a possible formatting function for
`bbdb-address-format-list'."
  (let ((country (bbdb-address-country address))
        (streets (bbdb-address-streets address)))
    (concat (if streets
                (concat (mapconcat 'identity streets "\n") "\n"))
            (bbdb-concat ", " (bbdb-address-city address)
                         (bbdb-concat " " (bbdb-address-state address)
                                      (bbdb-address-postcode address)))
            (unless (or (not country) (string= "" country))
              (concat "\n" country)))))

(defun bbdb-format-address (address layout)
  "Format ADDRESS using LAYOUT.  Return result as a string.
The formatting rules are defined in `bbdb-address-format-list'."
  (let ((list bbdb-address-format-list)
        (country (bbdb-address-country address))
        elt string)
    (while (and (not string) (setq elt (pop list)))
      (let ((identifier (car elt))
            (format (nth layout elt))
            ;; recognize case for format identifiers
            case-fold-search str)
        (when (or (eq t identifier) ; default
                  (and (functionp identifier)
                       (funcall identifier address))
                  (and country
                       (listp identifier)
                       ;; ignore case for countries
                       (member-ignore-case country identifier)))
          (cond ((functionp format)
                 (setq string (funcall format address)))
                ((stringp format)
                 (setq string "")
                 (dolist (form (split-string (substring format 1 -1)
                                             (substring format 0 1) t))
                   (cond ((string-match "%s" form) ; street
                          (mapc (lambda (s) (setq string (concat string (format form s))))
                                (bbdb-address-streets address)))
                         ((string-match "%c" form) ; city
                          (unless (or (not (setq str (bbdb-address-city address))) (string= "" str))
                            (setq string (concat string (format (replace-regexp-in-string "%c" "%s" form) str)))))
                         ((string-match "%p" form) ; postcode
                          (unless (or (not (setq str (bbdb-address-postcode address))) (string= "" str))
                            (setq string (concat string (format (replace-regexp-in-string "%p" "%s" form) str)))))
                         ((string-match "%S" form) ; state
                          (unless (or (not (setq str (bbdb-address-state address))) (string= "" str))
                            (setq string (concat string (format (replace-regexp-in-string "%S" "%s" form t) str)))))
                         ((string-match "%C" form) ; country
                          (unless (or (not country) (string= ""  country))
                            (setq string (concat string (format (replace-regexp-in-string "%C" "%s" form t) country)))))
                         (t (error "Malformed address format element %s" form)))))
                (t (error "Malformed address format %s" format))))))
    (unless string
      (error "No match of `bbdb-address-format-list'"))
    string))

;;; Record display:
;; This inserts formatted (pieces of) records into the BBDB buffer.

(defsubst bbdb-field-property (start field)
  "Set text property bbdb-field of text between START and point to FIELD."
  (put-text-property start (point) 'bbdb-field field))

(defsubst bbdb-display-text (text field &optional face)
  "Insert TEXT at point.  Set its text property bbdb-field to FIELD.
If FACE is non-nil, also add face FACE."
  (let ((start (point)))
    (insert text)
    (bbdb-field-property start field)
    (if face (put-text-property start (point) 'face face))))

(defun bbdb-display-list (list field &optional terminator face indent)
  "Insert elements of LIST at point.
For inserted text, set text property bbdb-field to FIELD.
If TERMINATOR is non-nil use it to terminate the inserted text.
If FACE is non-nil use it as FACE for inserted text.
If INDENT and `bbdb-wrap-column' are integers, insert line breaks in between
elements of LIST if otherwise inserted text exceeds `bbdb-wrap-column'."
  ;; `truncate-lines' is fine for one-line layout.  But it is annyoing
  ;; for records that are displayed with multi-line layout.
  ;; Non-nil `word-wrap' would be much nicer.  How can we switch between
  ;; non-nil `truncate-lines' and non-nil `word-wrap' on a per-record basis?
  ;; The following code is an alternative solution using `bbdb-wrap-column'.
  (let* ((separator (nth 1 (or (cdr (assq field bbdb-separator-alist))
                               bbdb-default-separator)))
         (indent-flag (and (integerp bbdb-wrap-column)
                           (integerp indent)))
         (prefix (if indent-flag
                     (concat separator "\n" (make-string indent ?\s))))
        elt)
    (while (setq elt (pop list))
      (bbdb-display-text elt (list field elt) face)
      (cond ((and list indent-flag
                  (> (+ (current-column) (length (car list)))
                     bbdb-wrap-column))
             (bbdb-display-text prefix (list field) face))
            (list
             (bbdb-display-text separator (list field) face))
            (terminator
             (bbdb-display-text terminator (list field) face))))))

(defun bbdb-display-name-organization (record)
  "Insert name, affix, and organization of RECORD.
If RECORD has an xfield name-face, its value is used for font-locking name.
The value of name-face may be a face that is used directly.
The value may also be a key in `bbdb-name-face-alist'.  Then the
corresponding cdr is used.  If none of these schemes succeeds the face
`bbdb-face' is used."
  ;; Should this be further customizable?  We could build the following
  ;; from a customizable list containing function calls and strings.
  ;; Name
  (let ((name (if (eq 'last-first
                      (or (bbdb-record-xfield-intern record 'name-format)
                          bbdb-name-format))
                  (bbdb-record-name-lf record)
                ;; default: Firstname Lastname
                (bbdb-record-name record)))
        (name-face (bbdb-record-xfield record 'name-face)))
    (if (string= "" name) (setq name "???"))
    (bbdb-display-text name (list 'name name)
                       (if name-face
                           (cond ((facep name-face) name-face)
                                 ((cdr (assoc name-face bbdb-name-face-alist)))
                                 (t 'bbdb-name))
                         'bbdb-name)))
  ;; Affix
  (let ((affix (bbdb-record-affix record)))
    (when affix
      (insert ", ")
      (bbdb-display-list affix 'affix)))
  ;; Organization
  (let ((organization (bbdb-record-organization record)))
    (when organization
      (insert " - ")
      (bbdb-display-list organization 'organization nil
                         'bbdb-organization)))
  ;; Image
  (if (and bbdb-image (display-images-p))
      (let ((image (cond ((functionp bbdb-image)
                          (funcall bbdb-image record))
                         ((memq bbdb-image '(name fl-name))
                          (bbdb-record-name record))
                         ((eq bbdb-image 'lf-name)
                          (bbdb-record-name-lf record))
                         (t
                          (bbdb-record-xfield record bbdb-image)))))
        (when (and image
                   (setq image (locate-file image bbdb-image-path
                                            bbdb-image-suffixes))
                   (setq image (create-image image)))
          (insert " ")
          (insert-image image)))))

(defun bbdb-display-record-one-line (record layout field-list)
  "Format RECORD for the one-line FORMAT using LAYOUT.
See `bbdb-layout-alist' for more info on layouts.
FIELD-LIST is the list of actually displayed FIELDS."
  ;; Name, affix, and organizations
  (bbdb-display-name-organization record)
  (let ((name-end (or (bbdb-layout-get-option layout 'name-end)
                      40))
        (start (line-beginning-position)))
    (when (> (- (point) start -1) name-end)
      (put-text-property (+ start name-end -4) (point) 'invisible t)
      (insert "..."))
    (indent-to name-end))
  ;; rest of the fields
  (let (formatfun start)
    (dolist (field field-list)
      (cond (;; customized formatting
             (setq formatfun (intern-soft (format "bbdb-display-%s-one-line" field)))
             (funcall formatfun record))
            ;; phone
            ((eq field 'phone)
             (let ((phones (bbdb-record-phone record)) phone)
               (if phones
                   (while (setq phone (pop phones))
                     (bbdb-display-text (format "%s " (aref phone 0))
                                        `(phone ,phone field-label)
                                        'bbdb-field-name)
                     (bbdb-display-text (format "%s%s" (aref phone 1)
                                                (if phones " " "; "))
                                        `(phone ,phone))))))
            ;; address
            ((eq field 'address)
             (dolist (address (bbdb-record-address record))
               (setq start (point))
               (insert (bbdb-format-address address 3))
               (bbdb-field-property start `(address ,address))
               (insert "; ")))
            ;; mail
            ((eq field 'mail)
             (let ((mail (bbdb-record-mail record)))
               (if mail
                   (bbdb-display-list (if (bbdb-layout-get-option layout 'primary)
                                          (list (car mail)) mail)
                                      'mail "; "))))
            ;; AKA
            ((eq field 'aka)
             (let ((aka (bbdb-record-aka record)))
               (if aka
                   (bbdb-display-list aka 'aka "; "))))
            ;; uuid
            ((eq field 'uuid)
             (let ((uuid (bbdb-record-uuid record)))
               (bbdb-display-text (format "%s; " uuid) `(uuid ,uuid))))
            ;; creation-date
            ((eq field 'creation-date)
             (let ((creation-date (bbdb-record-creation-date record)))
               (bbdb-display-text (format "%s; " creation-date) `(creation-date ,creation-date))))
            ;; timestamp
            ((eq field 'timestamp)
             (let ((timestamp (bbdb-record-timestamp record)))
               (bbdb-display-text (format "%s; " timestamp) `(timestamp ,timestamp))))
            ;; xfields
            (t
             (let* ((xfield (assq field (bbdb-record-xfields record)))
                    (value (cdr xfield)))
               (if value
                   (bbdb-display-text
                    (concat (if (stringp value)
                                (replace-regexp-in-string
                                 "\n" "; " value)
                              ;; value of xfield is a sexp
                              (let ((print-escape-newlines t))
                                (prin1-to-string value)))
                            "; ")
                    `(xfields ,xfield)))))))
    ;; delete the trailing "; "
    (if (looking-back "; " nil)
        (backward-delete-char 2))
    (insert "\n")))

(defun bbdb-display-record-multi-line (record layout field-list)
  "Format RECORD for the multi-line FORMAT using LAYOUT.
See `bbdb-layout-alist' for more info on layouts.
FIELD-LIST is the list of actually displayed FIELDS."
  (bbdb-display-name-organization record)
  (insert "\n")
  (let* ((indent (or (bbdb-layout-get-option layout 'indentation) 21))
         ;; The format string FMT adds three extra characters.
         ;; So we subtract those from the value of INDENT.
         (fmt (format " %%%ds: " (- indent 3)))
         start formatfun)
    (dolist (field field-list)
      (setq start (point))
      (cond (;; customized formatting
             (setq formatfun (intern-soft (format "bbdb-display-%s-multi-line" field)))
             (funcall formatfun record indent))
            ;; phone
            ((eq field 'phone)
             (dolist (phone (bbdb-record-phone record))
               (bbdb-display-text (format fmt (concat "phone ("
                                                      (bbdb-phone-label phone)
                                                      ")"))
                                  `(phone ,phone field-label)
                                  'bbdb-field-name)
               (bbdb-display-text (concat (bbdb-phone-string phone) "\n")
                                  `(phone ,phone))))
            ;; address
            ((eq field 'address)
             (dolist (address (bbdb-record-address record))
               (bbdb-display-text (format fmt (concat "address ("
                                                      (bbdb-address-label address)
                                                      ")"))
                                  `(address ,address field-label)
                                  'bbdb-field-name)
               (setq start (point))
               (insert (bbdb-indent-string (bbdb-format-address address 2) indent)
                       "\n")
               (bbdb-field-property start `(address ,address))))
            ;; mail
            ((eq field 'mail)
             (let ((mail (bbdb-record-mail record)))
               (when mail
                 (bbdb-display-text (format fmt "mail") '(mail nil field-label)
                                    'bbdb-field-name)
                 (bbdb-display-list (if (bbdb-layout-get-option layout 'primary)
                                        (list (car mail)) mail)
                                    'mail "\n" nil indent))))
            ;; AKA
            ((eq field 'aka)
             (let ((aka (bbdb-record-aka record)))
               (when aka
                 (bbdb-display-text (format fmt "AKA") '(aka nil field-label)
                                    'bbdb-field-name)
                 (bbdb-display-list aka 'aka "\n"))))
            ;; uuid
            ((eq field 'uuid)
             (let ((uuid (bbdb-record-uuid record)))
               (bbdb-display-text (format fmt "uuid") `(uuid ,uuid field-label)
                                  'bbdb-field-name)
               (bbdb-display-text (format "%s\n" uuid) `(uuid ,uuid))))
            ;; creation-date
            ((eq field 'creation-date)
             (let ((creation-date (bbdb-record-creation-date record)))
               (bbdb-display-text (format fmt "creation-date") `(creation-date ,creation-date field-label)
                                  'bbdb-field-name)
               (bbdb-display-text (format "%s\n" creation-date) `(creation-date ,creation-date))))
            ;; timestamp
            ((eq field 'timestamp)
             (let ((timestamp (bbdb-record-timestamp record)))
               (bbdb-display-text (format fmt "timestamp") `(timestamp ,timestamp field-label)
                                  'bbdb-field-name)
               (bbdb-display-text (format "%s\n" timestamp) `(timestamp ,timestamp))))
            ;; xfields
            (t
             (let* ((xfield (assq field (bbdb-record-xfields record)))
                    (value (cdr xfield)))
               (when value
                 (bbdb-display-text (format fmt field)
                                    `(xfields ,xfield field-label)
                                    'bbdb-field-name)
                 (setq start (point))
                 (insert (bbdb-indent-string
                          (if (stringp value)
                              value
                            ;; value of xfield is a sexp
                            (let ((string (pp-to-string value)))
                              (if (string-match "[ \t\n]+\\'" string)
                                  (substring-no-properties
                                   string 0 (match-beginning 0))
                                string)))
                          indent) "\n")
                 (bbdb-field-property start `(xfields ,xfield)))))))
    (insert "\n")))

(defalias 'bbdb-display-record-full-multi-line
  'bbdb-display-record-multi-line)

(defalias 'bbdb-display-record-pop-up-multi-line
  'bbdb-display-record-multi-line)

(defun bbdb-display-record (record layout number)
  "Insert a formatted RECORD into the current buffer at point.
LAYOUT can be a symbol describing a layout in `bbdb-layout-alist'.
If it is nil, use `bbdb-layout'.
NUMBER is the number of RECORD among the displayed records.
Move point to the end of the inserted record."
  (unless layout (setq layout bbdb-layout))
  (unless (assq layout bbdb-layout-alist)
    (error "Unknown layout `%s'" layout))
  (let ((display-p  (bbdb-layout-get-option layout 'display-p))
        (omit-list  (bbdb-layout-get-option layout 'omit)) ; omitted fields
        (order-list (bbdb-layout-get-option layout 'order)); requested field order
        (all-fields (append '(phone address mail aka) ; default field order
                            (mapcar 'car (bbdb-record-xfields record))
                            '(uuid creation-date timestamp)))
        (beg (point))
        format-function field-list)
    (when (or (not display-p)
              (and display-p
                   (funcall display-p)))
      (if (functionp omit-list)
          (setq omit-list (funcall omit-list record layout)))
      (if (functionp order-list)
          (setq order-list (funcall order-list record layout)))
      ;; first omit unwanted fields
      (when (and omit-list (or (not order-list) (memq t order-list)))
        (if (listp omit-list)
            ;; show all fields except those listed here
            (dolist (omit omit-list)
              (setq all-fields (delq omit all-fields)))
          (setq all-fields nil))) ; show nothing
      ;; then order them
      (cond ((not order-list)
             (setq field-list all-fields))
            ((not (memq t order-list))
             (setq field-list order-list))
            (t
             (setq order-list (reverse order-list)
                   all-fields (delq nil (mapcar (lambda (f)
                                                  (unless (memq f order-list)
                                                    f))
                                                all-fields)))
             (dolist (order order-list)
               (if (eq t order)
                   (setq field-list (append all-fields field-list))
                 (push order field-list)))))
      ;; call the actual format function
      (setq format-function
            (intern-soft (format "bbdb-display-record-%s" layout)))
      (if (functionp format-function)
          (funcall format-function record layout field-list)
        (bbdb-display-record-multi-line record layout field-list))
      (put-text-property beg (point) 'bbdb-record-number number))))

(defun bbdb-display-records (records &optional layout append
                                     select horiz-p)
  "Display RECORDS using LAYOUT.
If APPEND is non-nil append RECORDS to the already displayed records.
Otherwise RECORDS overwrite the displayed records.
SELECT and HORIZ-P have the same meaning as in `bbdb-pop-up-window'."
  (interactive (list (bbdb-completing-read-records "Display records: ")
                     (bbdb-layout-prefix)))
  (if (bbdb-append-display-p) (setq append t))
  ;; `bbdb-redisplay-record' calls `bbdb-display-records'
  ;; with display information already amended to RECORDS.
  (unless (or (null records)
              (consp (car records)))
    ;; add layout and a marker to the local list of records
    (setq layout (or layout bbdb-layout)
          records (mapcar (lambda (record)
                            (list record layout (make-marker)))
                          records)))

  (let ((first-new (caar records)) ; first new record
        new-name)

    ;; If `bbdb-multiple-buffers' is non-nil we create a new BBDB buffer
    ;; when not already within one.  The new buffer name starts with a space,
    ;; i.e. it does not clutter the buffer list.
    (when (and bbdb-multiple-buffers
               (not (assq 'bbdb-buffer-name (buffer-local-variables))))
      (setq new-name (concat " *BBDB " (if (functionp bbdb-multiple-buffers)
                                           (funcall bbdb-multiple-buffers)
                                         (buffer-name))
                              "*"))
      ;; `bbdb-buffer-name' becomes buffer-local in the current buffer
      ;; as well as in the buffer `bbdb-buffer-name'
      (set (make-local-variable 'bbdb-buffer-name) new-name))

    (with-current-buffer (get-buffer-create bbdb-buffer-name) ; *BBDB*
      ;; If we are appending RECORDS to the ones already displayed,
      ;; then first remove any duplicates, and then sort them.
      (if append
          (let ((old-rec (mapcar 'car bbdb-records)))
            (dolist (record records)
              (unless (memq (car record) old-rec)
                (push record bbdb-records)))
            (setq records
                  (sort bbdb-records
                        (lambda (x y) (bbdb-record-lessp (car x) (car y)))))))

      (bbdb-mode)
      ;; Normally `bbdb-records' is the only BBDB-specific buffer-local variable
      ;; in the *BBDB* buffer.  It is intentionally not permanent-local.
      ;; A value of nil indicates that we need to (re)process the records.
      (setq bbdb-records records)
      (if new-name
          (set (make-local-variable 'bbdb-buffer-name) new-name))

      (unless (or bbdb-silent-internal bbdb-silent)
        (message "Formatting BBDB..."))
      (let ((record-number 0)
            buffer-read-only all-records)
        (erase-buffer)
        (bbdb-debug (setq all-records (bbdb-records)))
        (dolist (record records)
          (bbdb-debug (unless (memq (car record) all-records)
                        (error "Record %s does not exist" (car record))))
          (set-marker (nth 2 record) (point))
          (bbdb-display-record (nth 0 record) (nth 1 record) record-number)
          (setq record-number (1+ record-number)))

        (run-hooks 'bbdb-display-hook))

      (unless (or bbdb-silent-internal bbdb-silent)
        (message "Formatting BBDB...done."))
      (set-buffer-modified-p nil)

      (bbdb-pop-up-window select horiz-p)
      (if (not first-new)
          (goto-char (point-min))
        ;; Put point on first new record in *BBDB* buffer.
        (goto-char (nth 2 (assq first-new bbdb-records)))
        (set-window-start (get-buffer-window (current-buffer)) (point))))))

(defun bbdb-undisplay-records (&optional all-buffers)
  "Undisplay records in *BBDB* buffer, leaving this buffer empty.
If ALL-BUFFERS is non-nil undisplay records in all BBDB buffers."
  (dolist (buffer (cond (all-buffers (buffer-list))
                        ((let ((buffer (get-buffer bbdb-buffer-name)))
                           (and (buffer-live-p buffer) (list buffer))))))
    (with-current-buffer buffer
      (when (eq major-mode 'bbdb-mode)
        (let (buffer-read-only)
          (erase-buffer))
        (setq bbdb-records nil)
        (set-buffer-modified-p nil)))))

(defun bbdb-redisplay-record (record &optional sort delete-p)
  "Redisplay RECORD in current BBDB buffer.
If SORT is t, usually because RECORD has a new sortkey, re-sort
the displayed records.
If DELETE-P is non-nil RECORD is removed from the BBDB buffer."
  ;; For deletion in the *BBDB* buffer we use the full information
  ;; about the record in the database. Therefore, we need to delete
  ;; the record in the *BBDB* buffer before deleting the record in
  ;; the database.
  ;; FIXME: If point is initially inside RECORD, `bbdb-redisplay-record'
  ;; puts point at the beginning of the redisplayed RECORD.
  ;; Ideally, `bbdb-redisplay-record' should put point such that it
  ;; matches the previous value `bbdb-ident-point'.
  (let ((full-record (assq record bbdb-records)))
    (unless full-record
      (error "Record `%s' not displayed" (bbdb-record-name record)))
    (if (and sort (not delete-p))
        ;; FIXME: For records requiring re-sorting it may be more efficient
        ;; to insert these records in their proper location instead of
        ;; re-displaying all records.
        (bbdb-display-records (list record) nil t)
      (let ((marker (nth 2 full-record))
            (end-marker (nth 2 (car (cdr (memq full-record bbdb-records)))))
            buffer-read-only record-number)
        ;; If point is inside record, put it at the beginning of the record.
        (if (and (<= marker (point))
                 (< (point) (or end-marker (point-max))))
            (goto-char marker))
        (save-excursion
          (goto-char marker)
          (setq record-number (get-text-property (point) 'bbdb-record-number))
          (unless delete-p
            ;; First insert the reformatted record, then delete the old one,
            ;; so that the marker of this record cannot collapse with the
            ;; marker of the subsequent record
            (bbdb-display-record (car full-record) (nth 1 full-record)
                                 record-number))
          (delete-region (point) (or end-marker (point-max)))
          ;; If we deleted a record we need to update the subsequent
          ;; record numbers.
          (when delete-p
            (let* ((markers (append (mapcar (lambda (x) (nth 2 x))
                                            (cdr (memq full-record bbdb-records)))
                                    (list (point-max))))
                   (start (pop markers)))
              (dolist (end markers)
                (put-text-property start end
                                   'bbdb-record-number record-number)
                (setq start end
                      record-number (1+ record-number))))
            (setq bbdb-records (delq full-record bbdb-records)))
          (run-hooks 'bbdb-display-hook))))))

(defun bbdb-redisplay-record-globally (record &optional sort delete-p)
  "Redisplay RECORD in all BBDB buffers.
If SORT is t, usually because RECORD has a new sortkey, re-sort
the displayed records.
If DELETE-P is non-nil RECORD is removed from the BBDB buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (if (and (eq major-mode 'bbdb-mode)
               (memq record (mapcar 'car bbdb-records)))
          (let ((window (get-buffer-window bbdb-buffer-name)))
            (if window
                (with-selected-window window
                  (bbdb-redisplay-record record sort delete-p))
              (bbdb-redisplay-record record sort delete-p)))))))
(define-obsolete-function-alias 'bbdb-maybe-update-display
  'bbdb-redisplay-record-globally "3.0")


;;; window configuration hackery
(defun bbdb-pop-up-window (&optional select horiz-p)
  "Display *BBDB* buffer by popping up a new window.
Finds the largest window on the screen, splits it, displaying the
*BBDB* buffer in the bottom `bbdb-pop-up-window-size' lines (unless
the *BBDB* buffer is already visible, in which case do nothing.)
Select this window if SELECT is non-nil.

If `bbdb-mua-pop-up' is 'horiz, and the first window matching
the predicate HORIZ-P is wider than the car of `bbdb-horiz-pop-up-window-size'
then the window will be split horizontally rather than vertically."
  (let ((buffer (get-buffer bbdb-buffer-name)))
    (unless buffer
      (error "No %s buffer to display" bbdb-buffer-name))
    (cond ((let ((window (get-buffer-window buffer t)))
             ;; We already have a BBDB window so that at most we select it
             (and window
                  (or (not select) (select-window window)))))

          ;; try horizontal split
          ((and (eq bbdb-mua-pop-up 'horiz)
                horiz-p
                (>= (frame-width) (car bbdb-horiz-pop-up-window-size))
                (let ((window-list (window-list))
                      (b-width (cdr bbdb-horiz-pop-up-window-size))
                      (search t) s-window)
                  (while (and (setq s-window (pop window-list))
                              (setq search (not (funcall horiz-p s-window)))))
                  (unless (or search (<= (window-width s-window)
                                         (car bbdb-horiz-pop-up-window-size)))
                    (condition-case nil ; `split-window' might fail
                        (let ((window (split-window
                                       s-window
                                       (if (integerp b-width)
                                           (- (window-width s-window) b-width)
                                         (round (* (- 1 b-width) (window-width s-window))))
                                       t))) ; horizontal split
                          (set-window-buffer window buffer)
                          (cond (bbdb-dedicated-window
                                 (set-window-dedicated-p window bbdb-dedicated-window))
                                ((fboundp 'display-buffer-record-window) ; GNU Emacs >= 24.1
                                 (set-window-prev-buffers window nil)
                                 (display-buffer-record-window 'window window buffer)))
                          (if select (select-window window))
                          t)
                      (error nil))))))

          ((eq t bbdb-pop-up-window-size)
           (bbdb-pop-up-window-simple buffer select))

          (t ;; vertical split
           (let* ((window (selected-window))
                  (window-height (window-height window)))
             ;; find the tallest window...
             (mapc (lambda (w)
                     (let ((w-height (window-height w)))
                       (if (> w-height window-height)
                           (setq window w window-height w-height))))
                   (window-list))
             (condition-case nil
                 (progn
                   (unless (eql bbdb-pop-up-window-size 1.0)
                     (setq window (split-window ; might fail
                                   window
                                   (if (integerp bbdb-pop-up-window-size)
                                       (- window-height 1 ; for mode line
                                          (max window-min-height bbdb-pop-up-window-size))
                                     (round (* (- 1 bbdb-pop-up-window-size)
                                               window-height))))))
                   (set-window-buffer window buffer) ; might fail
                   (cond (bbdb-dedicated-window
                          (set-window-dedicated-p window bbdb-dedicated-window))
                         ((and (fboundp 'display-buffer-record-window) ; GNU Emacs >= 24.1
                               (not (eql bbdb-pop-up-window-size 1.0)))
                          (set-window-prev-buffers window nil)
                          (display-buffer-record-window 'window window buffer)))
                   (if select (select-window window)))
               (error (bbdb-pop-up-window-simple buffer select))))))))

(defun bbdb-pop-up-window-simple (buffer select)
  "Display BUFFER in some window, selecting it if SELECT is non-nil.
If `bbdb-dedicated-window' is non-nil, mark the window as dedicated."
  (let ((window (if select
                    (progn (pop-to-buffer buffer)
                           (get-buffer-window))
                  (display-buffer buffer))))
    (if bbdb-dedicated-window
        (set-window-dedicated-p window bbdb-dedicated-window))))


;;; BBDB mode

;;;###autoload
(define-derived-mode bbdb-mode special-mode "BBDB"
  "Major mode for viewing and editing the Insidious Big Brother Database.
Letters no longer insert themselves.  Numbers are prefix arguments.
You can move around using the usual cursor motion commands.
\\<bbdb-mode-map>
\\[bbdb-add-mail-alias]\t Add new mail alias to visible records or \
remove it.
\\[bbdb-edit-field]\t Edit the field on the current line.
\\[bbdb-delete-field-or-record]\t Delete the field on the \
current line.  If the current line is the\n\t first line of a record, then \
delete the entire record.
\\[bbdb-insert-field]\t Insert a new field into the current record.  \
Note that this\n\t will let you add new fields of your own as well.
\\[bbdb-transpose-fields]\t Swap the field on the current line with the \
previous field.
\\[bbdb-dial]\t Dial the current phone field.
\\[bbdb-next-record], \\[bbdb-prev-record]\t Move to the next or the previous \
displayed record, respectively.
\\[bbdb-create]\t Create a new record.
\\[bbdb-toggle-records-layout]\t Toggle whether the current record is displayed in a \
one-line\n\t listing, or a full multi-line listing.
\\[bbdb-do-all-records]\\[bbdb-toggle-records-layout]\t Do that \
for all displayed records.
\\[bbdb-merge-records]\t Merge the contents of the current record with \
some other, and then\n\t delete the current record.
\\[bbdb-omit-record]\t Remove the current record from the display without \
deleting it from\n\t the database.  This is often a useful thing to do \
before using one\n\t of the `*' commands.
\\[bbdb]\t Search for records in the database (on all fields).
\\[bbdb-search-mail]\t Search for records by mail address.
\\[bbdb-search-organization]\t Search for records by organization.
\\[bbdb-search-xfields]\t Search for records by xfields.
\\[bbdb-search-name]\t Search for records by name.
\\[bbdb-search-changed]\t Display records that have changed since the database \
was saved.
\\[bbdb-mail]\t Compose mail to the person represented by the \
current record.
\\[bbdb-do-all-records]\\[bbdb-mail]\t Compose mail \
to everyone whose record is displayed.
\\[bbdb-save]\t Save the BBDB file to disk.
\\[bbdb-tex]\t Create a TeX listing of the current record.
\\[bbdb-do-all-records]\\[bbdb-tex]\t Do that for all \
displayed record.
\\[other-window]\t Move to another window.
\\[bbdb-info]\t Read the Info documentation for BBDB.
\\[bbdb-help]\t Display a one line command summary in the echo area.
\\[bbdb-browse-url]\t Visit Web sites listed in the `url' field(s) of the current \
record.

For address completion using the names and mail addresses in the database:
\t in Mail mode, type \\<mail-mode-map>\\[bbdb-complete-mail].
\t in Message mode, type \\<message-mode-map>\\[bbdb-complete-mail].

Important variables:
\t `bbdb-auto-revert'
\t `bbdb-ignore-redundant-mails'
\t `bbdb-case-fold-search'
\t `bbdb-completion-list'
\t `bbdb-default-area-code'
\t `bbdb-default-domain'
\t `bbdb-layout'
\t `bbdb-file'
\t `bbdb-phone-style'
\t `bbdb-check-auto-save-file'
\t `bbdb-pop-up-layout'
\t `bbdb-pop-up-window-size'
\t `bbdb-add-name'
\t `bbdb-add-aka'
\t `bbdb-add-mails'
\t `bbdb-new-mails-primary'
\t `bbdb-read-only'
\t `bbdb-mua-pop-up'
\t `bbdb-user-mail-address-re'

There are numerous hooks.  M-x apropos ^bbdb.*hook RET

\\{bbdb-mode-map}"
  (setq truncate-lines t
        default-directory (file-name-directory bbdb-file)
        mode-line-buffer-identification
        (list 24 (buffer-name) "  "
              '(:eval (format "%d/%d/%d"
                              (1+ (or (get-text-property
                                       (point) 'bbdb-record-number) -1))
                              (length bbdb-records)
                              ;; This code gets called a lot.
                              ;; So we keep it as simple as possible.
                              (with-current-buffer bbdb-buffer
                                (length bbdb-records))))
              '(:eval (concat "  "
                              (bbdb-concat " " (elt bbdb-modeline-info 0)
                                           (elt bbdb-modeline-info 2)
                                           (elt bbdb-modeline-info 4)))))
        mode-line-modified
        ;; For the mode-line we want to be fast. So we skip the checks
        ;; performed by `bbdb-with-db-buffer'.
        '(:eval (if (buffer-modified-p bbdb-buffer)
                    (if bbdb-read-only "%*" "**")
                  (if bbdb-read-only "%%" "--"))))
  ;; `bbdb-revert-buffer' acts on `bbdb-buffer'.  Yet this command is usually
  ;; called from the *BBDB* buffer.
  (set (make-local-variable 'revert-buffer-function)
       'bbdb-revert-buffer)
  (add-hook 'post-command-hook 'force-mode-line-update nil t))



(defun bbdb-sendmail-menu (record)
  "Menu items for email addresses of RECORD."
  (let ((mails (bbdb-record-mail record)))
    (list
     (if (cdr mails)
         ;; Submenu for multiple mail addresses
         (cons "Send mail to..."
               (mapcar (lambda (address)
                         (vector address `(bbdb-compose-mail
                                           ,(bbdb-dwim-mail record address))
                                 t))
                       mails))
       ;; Single entry for single mail address
       (vector (concat "Send mail to " (car mails))
               `(bbdb-compose-mail ,(bbdb-dwim-mail record (car mails)))
               t)))))

(defun bbdb-field-menu (record field)
  "Menu items specifically for FIELD of RECORD."
  (let ((type (car field)))
    (append
     (list
      (format "Commands for %s Field:"
              (cond ((eq type 'xfields)
                     (format "\"%s\"" (symbol-name (car (nth 1 field)))))
                    ((eq type 'name) "Name")
                    ((eq type 'affix) "Affix")
                    ((eq type 'organization) "Organization")
                    ((eq type 'aka) "Alternate Names")
                    ((eq type 'mail) "Mail Addresses")
                    ((memq type '(address phone))
                     (format "\"%s\" %s" (aref (nth 1 field) 0)
                             (capitalize (symbol-name type)))))))
     (cond ((eq type 'phone)
            (list (vector (concat "Dial " (bbdb-phone-string (nth 1 field)))
                          `(bbdb-dial ',field nil) t)))
           ((eq type 'xfields)
            (let* ((field (cadr field))
                   (type (car field)))
              (cond ((eq type 'url )
                     (list (vector (format "Browse \"%s\"" (cdr field))
                                   `(bbdb-browse-url ,record) t)))))))
     '(["Edit Field" bbdb-edit-field t])
     (unless (eq type 'name)
       '(["Delete Field" bbdb-delete-field-or-record t])))))

(defun bbdb-insert-field-menu (record)
  "Submenu for inserting a new field for RECORD."
  (cons "Insert New Field..."
        (mapcar
         (lambda (field)
           (if (stringp field) field
             (vector (symbol-name field)
                     `(bbdb-insert-field
                       ,record ',field (bbdb-read-field ,record ',field
                                                        ,current-prefix-arg))
                     (not (or (and (eq field 'affix) (bbdb-record-affix record))
                              (and (eq field 'organization)
                                   (bbdb-record-organization record))
                              (and (eq field 'mail) (bbdb-record-mail record))
                              (and (eq field 'aka) (bbdb-record-aka record))
                              (assq field (bbdb-record-xfields record)))))))
         (append '(affix organization aka phone address mail)
                 '("--") bbdb-xfield-label-list))))

(defun bbdb-mouse-menu (event)
  "BBDB mouse menu for EVENT,"
  (interactive "e")
  (mouse-set-point event)
  (let* ((record (bbdb-current-record))
         (field  (bbdb-current-field))
         (menu (if (and record field (functionp bbdb-user-menu-commands))
                   (funcall bbdb-user-menu-commands record field)
                 bbdb-user-menu-commands)))
    (if record
        (popup-menu
         (append
          (list
           (format "Commands for record \"%s\":" (bbdb-record-name record))
           ["Delete Record" bbdb-delete-records t]
           ["Toggle Record Display Layout" bbdb-toggle-records-layout t]
           (if (and (not (eq 'full-multi-line
                             (nth 1 (assq record bbdb-records))))
                    (bbdb-layout-get-option 'multi-line 'omit))
               ["Fully Display Record" bbdb-display-records-completely t])
           ["Omit Record" bbdb-omit-record t]
           ["Merge Record" bbdb-merge-records t])
          (if (bbdb-record-mail record)
              (bbdb-sendmail-menu record))
          (list "--" (bbdb-insert-field-menu record))
          (if field
              (cons "--" (bbdb-field-menu record field)))
          (if menu
              (append '("--" "User Defined Commands") menu)))))))



(defun bbdb-scan-property (property predicate n)
  "Scan for change of PROPERTY matching PREDICATE for N times.
Return position of beginning of matching interval."
  (let ((fun (if (< 0 n) 'next-single-property-change
               'previous-single-property-change))
        (limit (if (< 0 n) (point-max) (point-min)))
        (nn (abs n))
        (i 0)
        (opoint (point))
        npoint)
    ;; For backward search, move point to beginning of interval with PROPERTY.
    (if (and (<= n 0)
             (< (point-min) opoint)
             (let ((prop (get-text-property opoint property)))
               (and (eq prop (get-text-property (1- opoint) property))
                    (funcall predicate prop))))
        (setq opoint (previous-single-property-change opoint property nil limit)))
    (if (zerop n)
        opoint ; Return beginning of interval point is in
      (while (and (< i nn)
                  (let (done)
                    (while (and (not done)
                                (setq npoint (funcall fun opoint property nil limit)))
                      (cond ((and (/= opoint npoint)
                                  (funcall predicate (get-text-property
                                                      npoint property)))
                             (setq opoint npoint done t))
                            ((= opoint npoint)
                             ;; Search reached beg or end of buffer: abort.
                             (setq done t i nn npoint nil))
                            (t (setq opoint npoint))))
                    done))
        (setq i (1+ i)))
      npoint)))

(defun bbdb-next-record (n)
  "Move point to the beginning of the next BBDB record.
With prefix N move forward N records."
  (interactive "p")
  (let ((npoint (bbdb-scan-property 'bbdb-record-number 'integerp n)))
    (if npoint (goto-char npoint)
      (error "No %s record" (if (< 0 n) "next" "previous")))))

(defun bbdb-prev-record (n)
  "Move point to the beginning of the previous BBDB record.
With prefix N move backwards N records."
  (interactive "p")
  (bbdb-next-record (- n)))

(defun bbdb-next-field (n)
  "Move point to next (sub)field.
With prefix N move forward N (sub)fields."
  (interactive "p")
  (let ((npoint (bbdb-scan-property
                 'bbdb-field
                 (lambda (p) (and (nth 1 p)
                                  (not (eq (nth 2 p) 'field-label))))
                 n)))
    (if npoint (goto-char npoint)
      (error "No %s field" (if (< 0 n) "next" "previous")))))

(defun bbdb-prev-field (n)
  "Move point to previous (sub)field.
With prefix N move backwards N (sub)fields."
  (interactive "p")
  (bbdb-next-field (- n)))

(defun bbdb-save (&optional prompt noisy)
  "Save the BBDB if it is modified.
If PROMPT is non-nil prompt before saving.
If NOISY is non-nil as in interactive calls issue status messages."
  (interactive (list nil t))
  (bbdb-with-db-buffer
    (if (buffer-modified-p)
        (if (or (not prompt)
                (y-or-n-p
                 (if bbdb-read-only
                     "Save the BBDB, even though it is supposedly read-only? "
                   "Save the BBDB now? ")))
            (save-buffer))
      (if noisy (message "(No BBDB changes need to be saved)")))))

;;;###autoload
(defun bbdb-version (&optional arg)
  "Return string describing the version of BBDB.
With prefix ARG, insert string at point."
  (interactive (list (or (and current-prefix-arg 1) t)))
  (let* ((version
          (if (string-match "\\`[ \t\n]*[1-9]" bbdb-version)
              bbdb-version
            (let ((source (find-function-noselect 'bbdb-version)))
              (if source
                  (with-current-buffer (car source)
                    (prog1 (save-excursion
                             (goto-char (point-min))
                             (when (re-search-forward
                                    "^;;+ *Version: \\(.*\\)" nil t)
                               (match-string-no-properties 1)))
                      (unless (get-buffer-window nil t)
                        (kill-buffer (current-buffer)))))))))
         (version-string (format "BBDB version %s" (or version "<unknown>"))))
    (cond ((numberp arg) (insert (message version-string)))
          ((eq t arg) (message version-string))
          (t version-string))))



(defun bbdb-sort-records ()
  "Sort BBDB database.
This is not needed when using BBDB itself.  It might be necessary,
however, after having used other programs to add records to the BBDB."
  (interactive)
  (let* ((records (copy-sequence (bbdb-records))))
    (bbdb-with-db-buffer
      (setq bbdb-records (sort bbdb-records 'bbdb-record-lessp))
      (if (equal records bbdb-records)
          (message "BBDB already sorted properly")
        (message "BBDB was mis-sorted; fixing...")
        (bbdb-goto-first-record)
        (delete-region (point) bbdb-end-marker)
        (let ((buf (current-buffer))
              (inhibit-quit t) ; really, don't mess with this
              cache)
          (dolist (record bbdb-records)
            ;; Before printing the record, remove cache (we do not want that
            ;; written to the file.)  Ater writing, put the cache back
            ;; and update the cache's marker.
            (setq cache (bbdb-record-cache record))
            (set-marker (bbdb-cache-marker cache) (point))
            (bbdb-record-set-cache record nil)
            (bbdb-with-print-loadably (prin1 record buf))
            (bbdb-record-set-cache record cache)
            (insert ?\n)))
        (dolist (buffer (buffer-list))
          (with-current-buffer buffer
            (if (eq major-mode 'bbdb-mode)
                ; Redisplay all records
                (bbdb-display-records nil nil t))))
        (message "BBDB was mis-sorted; fixing...done")))))



;;;###autoload
(defun bbdb-initialize (&rest muas)
  "Initialize BBDB for MUAS and miscellaneous packages.
List MUAS may include the following symbols to initialize the respective
mail/news readers, composers, and miscellaneous packages:
  gnus       Gnus mail/news reader.
  mh-e       MH-E mail reader.
  mu4e       Mu4e mail reader.
  rmail      Rmail mail reader.
  vm         VM mail reader.
  mail       Mail (M-x mail).
  message    Message mode.
  wl         Wanderlust mail reader.

  anniv      Anniversaries in Emacs diary.

  sc         Supercite.  However, this is not the full story.
               See bbdb-sc.el for how to fully hook BBDB into Supercite.

  pgp        PGP support:  this adds `bbdb-pgp' to `message-send-hook'
               and `mail-send-hook' so that `bbdb-pgp' runs automatically
               when a message is sent.
               Yet see info node `(message)Signing and encryption'
               why you might not want to rely for encryption on a hook
               function which runs just before the message is sent,
               that is, you might want to call the command `bbdb-pgp' manually,
               then call `mml-preview'.

See also `bbdb-mua-auto-update-init'.  The latter is a separate function
as this allows one to initialize the auto update feature for some MUAs only,
for example only for outgoing messages."
  (dolist (mua muas)
    (let ((init (assq mua bbdb-init-forms)))
      (if init
          ;; Should we make sure that each insinuation happens only once?
          (eval (cadr init))
        (bbdb-warn "Do not know how to insinuate `%s'" mua))))
  (run-hooks 'bbdb-initialize-hook))


(provide 'bbdb)

;;; bbdb.el ends here
