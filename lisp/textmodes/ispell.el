;;; ispell.el --- interface to International Ispell Versions 3.1 and 3.2

;; Copyright (C) 1994, 1995, 1997, 1998, 1999, 2000, 2001, 2002 Free Software Foundation, Inc.

;; Author:           Ken Stevens <k.stevens@ieee.org>
;; Maintainer:       Ken Stevens <k.stevens@ieee.org>
;; Stevens Mod Date: Fri Aug  4 09:41:50 PDT 2000
;; Stevens Revision: 3.4
;; Status          : Release with 3.1.12+ and 3.2.0+ ispell.
;; Bug Reports     : ispell-el-bugs@itcorp.com
;; Web Site        : http://kdstevens.com/~stevens/ispell-page.html
;; Keywords: unix wp

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

;; Note: version numbers and time stamp are not updated
;;   when this file is edited for release with GNU emacs.

;;; Commentary:

;; INSTRUCTIONS

;;   This code contains a section of user-settable variables that you
;; should inspect prior to installation.  Look past the end of the history
;; list.  Set them up for your locale and the preferences of the majority
;; of the users.  Otherwise the users may need to set a number of variables
;; themselves.
;;   You particularly may want to change the default dictionary for your
;; country and language.
;;   Most dictionary changes should be made in this file so all users can
;; enjoy them.  Local or modified dictionaries are supported in your .emacs
;; file.  Modify the variable `ispell-local-dictionary-alist' to include
;; these dictionaries, and they will be installed when ispell.el is loaded.

;;  Depending on the mail system you use, you may want to include these:
;;  (add-hook 'news-inews-hook 'ispell-message)
;;  (add-hook 'mail-send-hook  'ispell-message)
;;  (add-hook 'mh-before-send-letter-hook 'ispell-message)

;;   Ispell has a TeX parser and a nroff parser (the default).
;; The parsing is controlled by the variable ispell-parser.  Currently
;; it is just a "toggle" between TeX and nroff, but if more parsers are
;; added it will be updated.  See the variable description for more info.


;; TABLE OF CONTENTS

;;   ispell-word
;;   ispell-region
;;   ispell-buffer
;;   ispell-message
;;   ispell-comments-and-strings
;;   ispell-continue
;;   ispell-complete-word
;;   ispell-complete-word-interior-frag
;;   ispell-change-dictionary
;;   ispell-kill-ispell
;;   ispell-pdict-save
;;   ispell-skip-region-alist

;; Commands in ispell-region:
;; Character replacement: Replace word with choice.  May query-replace.
;; ` ': Accept word this time.
;; `i': Accept word and insert into private dictionary.
;; `a': Accept word for this session.
;; `A': Accept word and place in buffer-local dictionary.
;; `r': Replace word with typed-in value.  Rechecked.
;; `R': Replace word with typed-in value. Query-replaced in buffer. Rechecked.
;; `?': Show these commands
;; `x': Exit spelling buffer.  Move cursor to original point.
;; `X': Exit spelling buffer.  Leaves cursor at the current point, and permits
;;      the check to be completed later.
;; `q': Quit spelling session (Kills ispell process).
;; `l': Look up typed-in replacement in alternate dictionary.  Wildcards okay.
;; `u': Like `i', but the word is lower-cased first.
;; `m': Place entered value in personal dictionary, then recheck current word.
;; `C-l': redraws screen
;; `C-r': recursive edit
;; `C-z': suspend emacs or iconify frame

;; Buffer-Local features:
;; There are a number of buffer-local features that can be used to customize
;;  ispell for the current buffer.  This includes language dictionaries,
;;  personal dictionaries, parsing, and local word spellings.  Each of these
;;  local customizations are done either through local variables, or by
;;  including the keyword and argument(s) at the end of the buffer (usually
;;  prefixed by the comment characters).  See the end of this file for
;;  examples.  The local keywords and variables are:

;;  ispell-dictionary-keyword   language-dictionary
;;      uses local variable ispell-local-dictionary
;;  ispell-pdict-keyword        personal-dictionary
;;      uses local variable ispell-local-pdict
;;  ispell-parsing-keyword      mode-arg extended-char-arg
;;  ispell-words-keyword        any number of local word spellings

;; Region skipping:
;;  Place new regular expression definitions of regions you prefer not to
;;  spell check in `ispell-skip-region-alist'.  Mode-dependent features can
;;  be added to latex by modifying `ispell-tex-skip-alists'.
;;  `ispell-message' contains some custom skipping code for e-mail messages.

;; BUGS:
;;  Need a way to select between different character mappings without separate
;;    dictionary entries.
;;  Multi-byte characters if not defined by current dictionary may result in the
;;    evil "misalignment error" in some versions of MULE emacs.
;;  On some versions of emacs, growing the minibuffer fails.
;;    see `ispell-help-in-bufferp'.
;;  Recursive edits (?C-r or ?R) inside a keyboard text replacement check (?r)
;;    can cause misalignment errors.

;; HISTORY

;; Modifications made in latest versions:

;; Revision 3.4 2000/8/4 09:41:50	kss
;; Support new color display functions.
;; Fixed misalignment offset bug when replacing a string after a shift made.
;; Set to standard Author/Maintainer heading,
;; ensure localwords lists are separated from the text by newline. (Dave Love)
;; Added dictionary definition for Italian (William Deakin)
;; HTML region skipping greatly improved. (Chuck D. Phillips)
;; improved menus.  Fixed regexp matching http/email addresses.
;; one arg always for xemacs sleep-for (gunnar Evermann)
;; support for synchronous processes (Eli Zaretskii)

;; Revision 3.3  1999/11/29 11:38:34     kss
;; Only word replacements entered in from the keyboard are rechecked.
;; This fixes a bug in tex parsing and misalignment.
;; Exceptions exist for recursive edit and query-replace, with tex error
;; condition tested.  Recursive editing improved.
;; XEmacs repair for when `enable-multibyte-characters' defined - Didier Verna.
;; ispell-help fixed for XEmacs.  Choices minibuffer now displayed in XEmacs.
;; Only list valid dictionaries in Spell menu.  Russian dictionary doesn't allow
;; run-together words, and uses koi8-r font.  Don't skip text in html <TT>
;; fonts.

;; Revision 3.2  1999/5/7 14:25:14	kss
;; Accept ispell versions 3.X.Y where X>=1
;; fine tuned latex region skipping.  Fixed bug in ispell-word that did not
;; point in right place on words < 2 chars.  Simplified ispell-minor-mode.
;; Fixed bug in TeX parsing when math commands are in the comments.
;; Removed calls to `when' macro.

;; Revision 3.1  1998/12/1 13:21:52	kss
;; Improved and fixed customize support.
;; Improved and fixed comments in variables and messages.
;; A coding system is now required for all languages.
;; casechars improved for castellano, castellano8, and norsk dictionaries.
;; Dictionary norsk7-tex removed.  Dictionary polish added.
;; Dictionaries redefined at load-time to support dictionary changes.
;; Menu redefined at load time to support dictionary changes.
;; ispell-check-version added as an alias for `check-ispell-version'.
;; Spelling suggestions returned in order generated by ispell.
;; Small bug fixed in matching ispell error messages.
;; Robustness added to ensure `case-fold-search' doesn't get redefined.
;; Fixed bug that didn't respect case of word in `ispell-complete-word'.
;; Multibyte character coding support added for process interactions.
;; Ensure ispell process has terminated before starting new process.
;;  This can otherwise confuse process filters and hang ispell.
;; Improved skipping support for SGML.
;; Fixed bug using ^M rather than \r in `ispell-minor-check'.
;; Improved message reference matching in `ispell-message'.
;; Fixed bug in returning to nroff mode from tex mode.


;;; Code:

;;; Custom.el macros require recompiling this when they are not present.
;;; Add in backward compatible custom support.
(eval-when-compile
  (if (not (fboundp 'defcustom))
      (defmacro defcustom (symbol value doc &rest args)
	"Empty replacement for defcustom when not supplied."
	`(defvar ,symbol ,value ,doc))))

(eval-when-compile
  (if (fboundp 'defgroup)
      (defgroup ispell nil
	"User variables for emacs ispell interface."
	:group 'applications)))

(if (not (fboundp 'buffer-substring-no-properties))
    (defun buffer-substring-no-properties (start end)
      (buffer-substring start end)))

;;;###autoload
(defconst xemacsp (string-match "Lucid\\|XEmacs" emacs-version)
  "Non nil if using XEmacs.")

(defconst ispell-graphic-p
  (if (fboundp 'display-graphic-p)
      (display-graphic-p)
    xemacsp)
  "True if running on a `graphics capable' display.")

(defalias 'ispell-check-version 'check-ispell-version)

;;; **********************************************************************
;;; The following variables should be set according to personal preference
;;; and location of binaries:
;;; **********************************************************************


;;;  ******* THIS FILE IS WRITTEN FOR ISPELL VERSION 3.1+

(defcustom ispell-highlight-p 'block
  "*Highlight spelling errors when non-nil.
When set to `block', assumes a block cursor with TTY displays."
  :type '(choice (const block) (const :tag "off" nil) (const :tag "on" t))
  :group 'ispell)

(defcustom ispell-highlight-face 'highlight
  "*The face used for Ispell highlighting.  For Emacses with overlays.
Possible values are `highlight', `modeline', `secondary-selection',
`region', and `underline'.
This variable can be set by the user to whatever face they desire.
It's most convenient if the cursor color and highlight color are
slightly different."
  :type 'face
  :group 'ispell)

(defcustom ispell-check-comments t
  "*Spelling of comments checked when non-nil.
When set to `exclusive', ONLY comments are checked.  (For code comments).
Warning!  Not checking comments, when a comment start is embedded in strings,
may produce undesired results."
  :type '(choice (const exclusive) (const :tag "off" nil) (const :tag "on" t))
  :group 'ispell)

(defcustom ispell-query-replace-choices nil
  "*Corrections made throughout region when non-nil.
Uses `query-replace' (\\[query-replace]) for corrections."
  :type 'boolean
  :group 'ispell)

(defcustom ispell-skip-tib nil
  "*Does not spell check `tib' bibliography references when non-nil.
Skips any text between strings matching regular expressions
`ispell-tib-ref-beginning' and `ispell-tib-ref-end'.

TeX users beware:  Any field starting with [. will skip until a .] -- even
your whole buffer -- unless you set `ispell-skip-tib' to nil.  That includes
a [.5mm] type of number...."
  :type 'boolean
  :group 'ispell)

(defvar ispell-tib-ref-beginning "[[<]\\."
  "Regexp matching the beginning of a Tib reference.")

(defvar ispell-tib-ref-end "\\.[]>]"
  "Regexp matching the end of a Tib reference.")

(defcustom ispell-keep-choices-win t
  "*When not nil, the `*Choices*' window remains for spelling session.
This minimizes redisplay thrashing."
  :type 'boolean
  :group 'ispell)

(defcustom ispell-choices-win-default-height 2
  "*The default size of the `*Choices*' window, including mode line.
Must be greater than 1."
  :type 'integer
  :group 'ispell)

(defcustom ispell-program-name "ispell"
  "Program invoked by \\[ispell-word] and \\[ispell-region] commands."
  :type 'string
  :group 'ispell)

(defcustom ispell-alternate-dictionary
  (cond ((file-exists-p "/usr/dict/web2") "/usr/dict/web2")
	((file-exists-p "/usr/share/dict/web2") "/usr/share/dict/web2")
	((file-exists-p "/usr/dict/words") "/usr/dict/words")
	((file-exists-p "/usr/lib/dict/words") "/usr/lib/dict/words")
	((file-exists-p "/usr/share/dict/words") "/usr/share/dict/words")
	((file-exists-p "/usr/share/lib/dict/words")
	 "/usr/share/lib/dict/words")
	((file-exists-p "/sys/dict") "/sys/dict")
	(t "/usr/dict/words"))
  "*Alternate dictionary for spelling help."
  :type '(choice file (const :tag "None" nil))
  :group 'ispell)

(defcustom ispell-complete-word-dict ispell-alternate-dictionary
  "*Dictionary used for word completion."
  :type '(choice file (const :tag "None" nil))
  :group 'ispell)

(defcustom ispell-message-dictionary-alist nil
  "*List used by `ispell-message' to select a new dictionary.
It consists of pairs (REGEXP . DICTIONARY).  If REGEXP is found
in the message headers, `ispell-local-dictionary' will be set to
DICTIONARY if `ispell-local-dictionary' is not buffer-local.
E.g. you may use the following value:
  '((\"^Newsgroups:[ \\t]*de\\\\.\" . \"deutsch8\")
    (\"^To:[^\\n,]+\\\\.de[ \\t\\n,>]\" . \"deutsch8\"))"
  :type '(repeat (cons regexp string))
  :group 'ispell)


(defcustom ispell-grep-command "egrep"
  "Name of the grep command for search processes."
  :type 'string
  :group 'ispell)

(defcustom ispell-grep-options "-i"
  "String of options to use when running the program in `ispell-grep-command'.
Should probably be \"-i\" or \"-e\".
Some machines (like the NeXT) don't support \"-i\""
  :type 'string
  :group 'ispell)

(defcustom ispell-look-command
  (cond ((file-exists-p "/bin/look") "/bin/look")
	((file-exists-p "/usr/local/bin/look") "/usr/local/bin/look")
	((file-exists-p "/usr/bin/look") "/usr/bin/look")
	(t "look"))
  "Name of the look command for search processes.
This must be an absolute file name."
  :type 'file
  :group 'ispell)

(defcustom ispell-look-p (file-exists-p ispell-look-command)
  "*Non-nil means use `look' rather than `grep'.
Default is based on whether `look' seems to be available."
  :type 'boolean
  :group 'ispell)

(defcustom ispell-have-new-look nil
  "*Non-nil means use the `-r' option (regexp) when running `look'."
  :type 'boolean
  :group 'ispell)

(defcustom ispell-look-options (if ispell-have-new-look "-dfr" "-df")
  "String of command options for `ispell-look-command'."
  :type 'string
  :group 'ispell)

(defcustom ispell-use-ptys-p nil
  "When non-nil, Emacs uses ptys to communicate with Ispell.
When nil, Emacs uses pipes."
  :type 'boolean
  :group 'ispell)

(defcustom ispell-following-word nil
  "*Non-nil means `ispell-word' checks the word around or after point.
Otherwise `ispell-word' checks the preceding word."
  :type 'boolean
  :group 'ispell)

(defcustom ispell-help-in-bufferp nil
  "*Non-nil means display interactive keymap help in a buffer.
The following values are supported:
  nil        Expand the minibuffer and display a short help message
             there for a couple of seconds.
  t          Pop up a new buffer and display a short help message there
             for a couple of seconds.
  electric   Pop up a new buffer and display a long help message there.
             User can browse and then exit the help mode."
  :type '(choice (const electric) (const :tag "off" nil) (const :tag "on" t))
  :group 'ispell)

(defcustom ispell-quietly nil
  "*Non-nil means suppress messages in `ispell-word'."
  :type 'boolean
  :group 'ispell)

(defcustom ispell-format-word (function upcase)
  "*Formatting function for displaying word being spell checked.
The function must take one string argument and return a string."
  :type 'function
  :group 'ispell)

(defcustom ispell-use-framepop-p nil
  "When non-nil ispell uses framepop to display choices in a dedicated frame.
You can set this variable to dynamically use framepop if you are in a
window system by evaluating the following on startup to set this variable:
  (and window-system (condition-case () (require 'framepop) (error nil)))"
  :type 'boolean
  :group 'ispell)

;;;###autoload
(defcustom ispell-personal-dictionary nil
  "*File name of your personal spelling dictionary, or nil.
If nil, the default personal dictionary, \"~/.ispell_DICTNAME\" is used,
where DICTNAME is the name of your default dictionary."
  :type '(choice file
		 (const :tag "default" nil))
  :group 'ispell)

(defcustom ispell-silently-savep nil
  "*When non-nil, save the personal dictionary without confirmation."
  :type 'boolean
  :group 'ispell)

;;; This is the local dictionary to use.  When nil the default dictionary will
;;; be used.  Change set-default call to use a new default dictionary.
(defcustom ispell-local-dictionary nil
  "If non-nil, the dictionary to be used for Ispell commands.
The value must be a string dictionary name in `ispell-dictionary-alist'.

Setting `ispell-local-dictionary' to a value has the same effect as
calling \\[ispell-change-dictionary] with that value.  This variable
is automatically set when defined in the file with either
`ispell-dictionary-keyword' or the Local Variable syntax.

To create a non-standard default dictionary (not from `ispell-dictionary-alist')
call function `set-default' with the new dictionary name."
  :type '(choice string
		 (const :tag "default" nil))
  :group 'ispell)

(make-variable-buffer-local 'ispell-local-dictionary)

;; Call this function set up the default dictionary if not English.
;;(set-default 'ispell-local-dictionary nil)


(defcustom ispell-extra-args nil
  "*If non-nil, a list of extra switches to pass to the Ispell program.
For example, (\"-W\" \"3\") to cause it to accept all 1-3 character
words as correct.  See also `ispell-dictionary-alist', which may be used
for language-specific arguments."
  :type '(repeat string)
  :group 'ispell)



(defcustom ispell-skip-html 'use-mode-name
  "*Indicates whether ispell should skip spell checking of SGML markup.
If t, always skip SGML markup; if nil, never skip; if non-t and non-nil,
guess whether SGML markup should be skipped according to the name of the
buffer's major mode."
  :type '(choice (const :tag "always" t) (const :tag "never" nil)
		 (const :tag "use-mode-name" use-mode-name))
  :group 'ispell)


;;; Define definitions here only for personal dictionaries.
;;;###autoload
(defcustom ispell-local-dictionary-alist nil
  "*Contains local or customized dictionary definitions.
See `ispell-dictionary-alist'."
  :type '(repeat (list (choice :tag "Dictionary"
			       (string :tag "Dictionary name")
			       (const :tag "default" nil))
		       (regexp :tag "Case characters")
		       (regexp :tag "Non case characters")
		       (regexp :tag "Other characters")
		       (boolean :tag "Many other characters")
		       (repeat :tag "Ispell command line args"
			       (string :tag "Arg"))
		       (choice :tag "Extended character mode"
			       (const "~tex") (const "~plaintex")
			       (const "~nroff") (const "~list")
			       (const "~latin1") (const "~latin3")
 			       (const :tag "default" nil))
		       (choice :tag "Character set"
			       (const iso-8859-1)
			       (const iso-8859-2)
			       (const koi8-r))))
  :group 'ispell)


;;; split dictionary so line length is smaller in loaddefs.el

;;; First part of dictionary, shortened for loaddefs.el
;;;###autoload
(setq
 ispell-dictionary-alist-1
 '((nil					; default (English.aff)
    "[A-Za-z]" "[^A-Za-z]" "[']" nil ("-B") nil iso-8859-1)
   ("american"				; Yankee English
    "[A-Za-z]" "[^A-Za-z]" "[']" nil ("-B") nil iso-8859-1)
   ("brasileiro"			; Brazilian mode
    "[A-Z\301\311\315\323\332\300\310\314\322\331\303\325\307\334\302\312\324a-z\341\351\355\363\372\340\350\354\362\371\343\365\347\374\342\352\364]"
    "[^A-Z\301\311\315\323\332\300\310\314\322\331\303\325\307\334\302\312\324a-z\341\351\355\363\372\340\350\354\362\371\343\365\347\374\342\352\364]"
    "[']" nil ("-d" "brasileiro") nil iso-8859-1)
   ("british"				; British version
    "[A-Za-z]" "[^A-Za-z]" "[']" nil ("-B" "-d" "british") nil iso-8859-1)
   ("castellano"			; Spanish mode
    "[A-Z\301\311\315\321\323\332\334a-z\341\351\355\361\363\372\374]"
    "[^A-Z\301\311\315\321\323\332\334a-z\341\351\355\361\363\372\374]"
    "[-]" nil ("-B" "-d" "castellano") "~tex" iso-8859-1)
   ("castellano8"			; 8 bit Spanish mode
    "[A-Z\301\311\315\321\323\332\334a-z\341\351\355\361\363\372\374]"
    "[^A-Z\301\311\315\321\323\332\334a-z\341\351\355\361\363\372\374]"
    "[-]" nil ("-B" "-d" "castellano") "~latin1" iso-8859-1)))


;;; Second part of dictionary, shortened for loaddefs.el
;;;###autoload
(setq
 ispell-dictionary-alist-2
 '(("czech"
    "[A-Za-z\301\311\314\315\323\332\331\335\256\251\310\330\317\253\322\341\351\354\355\363\372\371\375\276\271\350\370\357\273\362]"
    "[^A-Za-z\301\311\314\315\323\332\331\335\256\251\310\330\317\253\322\341\351\354\355\363\372\371\375\276\271\350\370\357\273\362]"
    "" nil ("-B" "-d" "czech") nil iso-8859-2)
   ("dansk"				; Dansk.aff
    "[A-Z\306\330\305a-z\346\370\345]" "[^A-Z\306\330\305a-z\346\370\345]"
    "[']" nil ("-C") nil iso-8859-1)
   ("deutsch"				; Deutsch.aff
    "[a-zA-Z\"]" "[^a-zA-Z\"]" "[']" t ("-C") "~tex" iso-8859-1)
   ("deutsch8"
    "[a-zA-Z\304\326\334\344\366\337\374]"
    "[^a-zA-Z\304\326\334\344\366\337\374]"
    "[']" t ("-C" "-d" "deutsch") "~latin1" iso-8859-1)
   ("english"				; make English explicitly selectable
    "[A-Za-z]" "[^A-Za-z]" "[']" nil ("-B") nil iso-8859-1)))


;;; Third part of dictionary, shortened for loaddefs.el
;;;###autoload
(setq
 ispell-dictionary-alist-3
 '(("esperanto"
    "[A-Za-z\246\254\266\274\306\330\335\336\346\370\375\376]"
    "[^A-Za-z\246\254\266\274\306\330\335\336\346\370\375\376]"
    "[-']" t ("-C") "~latin3" iso-8859-1)
   ("esperanto-tex"
    "[A-Za-z^\\]" "[^A-Za-z^\\]"
    "[-'`\"]" t ("-C" "-d" "esperanto") "~tex" iso-8859-1)
   ("francais7"
    "[A-Za-z]" "[^A-Za-z]" "[`'^---]" t nil nil iso-8859-1)
   ("francais"				; Francais.aff
    "[A-Za-z\300\302\306\307\310\311\312\313\316\317\324\331\333\334\340\342\347\350\351\352\353\356\357\364\371\373\374]"
    "[^A-Za-z\300\302\306\307\310\311\312\313\316\317\324\331\333\334\340\342\347\350\351\352\353\356\357\364\371\373\374]"
    "[-']" t nil "~list" iso-8859-1)))


;;; Fourth part of dictionary, shortened for loaddefs.el
;;;###autoload
(setq
 ispell-dictionary-alist-4
 '(("francais-tex"			; Francais.aff
    "[A-Za-z\300\302\306\307\310\311\312\313\316\317\324\331\333\334\340\342\347\350\351\352\353\356\357\364\371\373\374\\]"
    "[^A-Za-z\300\302\306\307\310\311\312\313\316\317\324\331\333\334\340\342\347\350\351\352\353\356\357\364\371\373\374\\]"
    "[-'^`\"]" t nil "~tex" iso-8859-1)
   ("german"                         ; german.aff
    "[a-zA-Z\"]" "[^a-zA-Z\"]" "[']" t ("-C") "~tex" iso-8859-1)
   ("german8"
    "[a-zA-Z\304\326\334\344\366\337\374]"
    "[^a-zA-Z\304\326\334\344\366\337\374]"
    "[']" t ("-C" "-d" "german") "~latin1" iso-8859-1)
   ("italiano"                         ; Italian.aff
    "[A-Z\300\301\310\311\314\315\322\323\331\332a-z\340\341\350\351\354\355\363\371\372]"
    "[^A-Z\300\301\310\311\314\315\322\323\331\332a-z\340\341\350\351\354\355\363\371\372]"
    "[-]" nil ("-B" "-d" "italian") "~tex" iso-8859-1)))


;;; Fifth part of dictionary, shortened for loaddefs.el
;;;###autoload
(setq
 ispell-dictionary-alist-5
 '(("nederlands"			; Nederlands.aff
    "[A-Za-z\300-\305\307\310-\317\322-\326\331-\334\340-\345\347\350-\357\361\362-\366\371-\374]"
    "[^A-Za-z\300-\305\307\310-\317\322-\326\331-\334\340-\345\347\350-\357\361\362-\366\371-\374]"
    "[']" t ("-C") nil iso-8859-1)
   ("nederlands8"			; Dutch8.aff
    "[A-Za-z\300-\305\307\310-\317\322-\326\331-\334\340-\345\347\350-\357\361\362-\366\371-\374]"
    "[^A-Za-z\300-\305\307\310-\317\322-\326\331-\334\340-\345\347\350-\357\361\362-\366\371-\374]"
    "[']" t ("-C") nil iso-8859-1)
   ("norsk"				; 8 bit Norwegian mode
    "[A-Za-z\305\306\307\310\311\322\324\330\345\346\347\350\351\362\364\370]"
    "[^A-Za-z\305\306\307\310\311\322\324\330\345\346\347\350\351\362\364\370]"
    "[\"]" nil ("-d" "norsk") "~list" iso-8859-1)
   ("norsk7-tex"			; 7 bit Norwegian TeX mode
    "[A-Za-z{}\\'^`]" "[^A-Za-z{}\\'^`]"
    "[\"]" nil ("-d" "norsk") "~plaintex" iso-8859-1)))


;;; Sixth part of dictionary, shortened for loaddefs.el
;;;###autoload
(setq
 ispell-dictionary-alist-6
 ;; include Russian iso character set too?
 ;;   "[']" t ("-d" "russian") "~latin1" iso-8859-1
 '(("polish"				; polish mode
    "[A-Za-z\241\243\246\254\257\261\263\266\274\277\306\312\321\323\346\352\361\363]"
    "[^A-Za-z\241\243\246\254\257\261\263\266\274\277\306\312\321\323\346\352\361\363]"
    "" nil ( "-d" "polish") nil iso-8859-2)
   ("russian"				; Russian.aff (KOI8-R charset)
    "[\341\342\367\347\344\345\263\366\372\351\352\353\354\355\356\357\360\362\363\364\365\346\350\343\376\373\375\370\371\377\374\340\361\301\302\327\307\304\305\243\326\332\311\312\313\314\315\316\317\320\322\323\324\325\306\310\303\336\333\335\330\331\337\334\300\321]"
    "[^\341\342\367\347\344\345\263\366\372\351\352\353\354\355\356\357\360\362\363\364\365\346\350\343\376\373\375\370\371\377\374\340\361\301\302\327\307\304\305\243\326\332\311\312\313\314\315\316\317\320\322\323\324\325\306\310\303\336\333\335\330\331\337\334\300\321]"
    "" nil ("-d" "russian") nil koi8-r)
   ("svenska"				; Swedish mode
    "[A-Za-z\345\344\366\351\340\374\350\346\370\347\305\304\326\311\300\334\310\306\330\307]"
    "[^A-Za-z\345\344\366\351\340\374\350\346\370\347\305\304\326\311\300\334\310\306\330\307]"
    "[']" nil ("-C") "~list" iso-8859-1)
   ("portugues"
    "[a-zA-Z\301\302\311\323\340\341\342\351\352\355\363\343\372]"
    "[^a-zA-Z\301\302\311\323\340\341\342\351\352\355\363\343\372]"
    "[']" t ("-C" "-d" "portugues") "~latin1" iso-8859-1)
   ("slovak"
    "[A-Za-z\301\304\311\315\323\332\324\300\305\245\335\256\251\310\317\253\322\341\344\351\355\363\372\364\340\345\265\375\276\271\350\357\273\362]"
    "[^A-Za-z\301\304\311\315\323\332\324\300\305\245\335\256\251\310\317\253\322\341\344\351\355\363\372\364\340\345\265\375\276\271\350\357\273\362]"
    "" nil ("-B" "-d" "slovak") nil iso-8859-2)))

;;;###autoload
(defcustom ispell-dictionary-alist
  (append ispell-local-dictionary-alist	; dictionary customizations
	  ispell-dictionary-alist-1 ispell-dictionary-alist-2
	  ispell-dictionary-alist-3 ispell-dictionary-alist-4
	  ispell-dictionary-alist-5 ispell-dictionary-alist-6)
  "An alist of dictionaries and their associated parameters.

Each element of this list is also a list:

\(DICTIONARY-NAME CASECHARS NOT-CASECHARS OTHERCHARS MANY-OTHERCHARS-P
        ISPELL-ARGS EXTENDED-CHARACTER-MODE CHARACTER-SET\)

DICTIONARY-NAME is a possible string value of variable `ispell-dictionary',
nil means the default dictionary.

CASECHARS is a regular expression of valid characters that comprise a
word.

NOT-CASECHARS is the opposite regexp of CASECHARS.

OTHERCHARS is a regexp of characters in the NOT-CASECHARS set but which can be
used to construct words in some special way.  If OTHERCHARS characters follow
and precede characters from CASECHARS, they are parsed as part of a word,
otherwise they become word-breaks.  As an example in English, assume the
regular expression \"[']\" for OTHERCHARS.  Then \"they're\" and
\"Steven's\" are parsed as single words including the \"'\" character, but
\"Stevens'\" does not include the quote character as part of the word.
If you want OTHERCHARS to be empty, use the empty string.
Hint: regexp syntax requires the hyphen to be declared first here.

MANY-OTHERCHARS-P is non-nil when multiple OTHERCHARS are allowed in a word.
Otherwise only a single OTHERCHARS character is allowed to be part of any
single word.

ISPELL-ARGS is a list of additional arguments passed to the ispell
subprocess.

EXTENDED-CHARACTER-MODE should be used when dictionaries are used which
have been configured in an Ispell affix file.  (For example, umlauts
can be encoded as \\\"a, a\\\", \"a, ...)  Defaults are ~tex and ~nroff
in English.  This has the same effect as the command-line `-T' option.
The buffer Major Mode controls Ispell's parsing in tex or nroff mode,
but the dictionary can control the extended character mode.
Both defaults can be overruled in a buffer-local fashion. See
`ispell-parsing-keyword' for details on this.

CHARACTER-SET used for languages with multibyte characters.

Note that the CASECHARS and OTHERCHARS slots of the alist should
contain the same character set as casechars and otherchars in the
LANGUAGE.aff file \(e.g., english.aff\)."
  :type '(repeat (list (choice :tag "Dictionary"
			       (string :tag "Dictionary name")
			       (const :tag "default" nil))
		       (regexp :tag "Case characters")
		       (regexp :tag "Non case characters")
		       (regexp :tag "Other characters")
		       (boolean :tag "Many other characters")
		       (repeat :tag "Ispell command line args"
			       (string :tag "Arg"))
		       (choice :tag "Extended character mode"
			       (const "~tex") (const "~plaintex")
			       (const "~nroff") (const "~list")
			       (const "~latin1") (const "~latin3")
 			       (const :tag "default" nil))
		       (choice :tag "Character set"
			       (const iso-8859-1)
			       (const iso-8859-2)
			       (const koi8-r))))
  :group 'ispell)

;;; update the dictionaries at load time
(setq ispell-dictionary-alist
      (append ispell-local-dictionary-alist	; dictionary customizations
	      ispell-dictionary-alist-1 ispell-dictionary-alist-2
	      ispell-dictionary-alist-3 ispell-dictionary-alist-4
	      ispell-dictionary-alist-5 ispell-dictionary-alist-6))




;;; **********************************************************************
;;; The following are used by ispell, and should not be changed.
;;; **********************************************************************



;;; The version must be 3.1 or greater for this version of ispell.el
;;; There is an incompatibility between version 3.1.12 and lower versions.
(defconst ispell-required-version '(3 1 12)
  "Ispell versions with which this version of ispell.el is known to work.")
(defvar ispell-offset -1
  "Offset that maps protocol differences between ispell 3.1 versions.")

(defconst ispell-version "ispell.el 3.4 -- Fri Aug  4 09:41:50 PDT 2000")


(defun check-ispell-version (&optional interactivep)
  "Ensure that `ispell-program-name' is valid and the correct version.
Returns version number if called interactively.
Otherwise returns the library directory name, if that is defined."
  ;; This is a little wasteful as we actually launch ispell twice: once
  ;; to make sure it's the right version, and once for real.  But people
  ;; get confused by version mismatches *all* the time (and I've got the
  ;; email to prove it) so I think this is worthwhile.  And the -v[ersion]
  ;; option is the only way I can think of to do this that works with
  ;; all versions, since versions earlier than 3.0.09 didn't identify
  ;; themselves on startup.
  (interactive "p")
  (let ((case-fold-search-val case-fold-search)
	;; avoid bugs when syntax of `.' changes in various default modes
	(default-major-mode 'fundamental-mode)
	result status)
    (save-excursion
      (set-buffer (get-buffer-create " *ispell-tmp*"))
      (erase-buffer)
      (setq status (call-process
		    ispell-program-name nil t nil
		    ;; aspell doesn't accept the -vv switch.
		    (let ((case-fold-search
			   (memq system-type '(ms-dos windows-nt)))
			  (speller
			   (file-name-nondirectory ispell-program-name)))
		      ;; Assume anything that isn't `aspell' is Ispell.
		      (if (string-match "\\`aspell" speller) "-v" "-vv"))))
      (goto-char (point-min))
      (if interactivep
	  (progn
	    (end-of-line)
	    (setq result (concat (buffer-substring-no-properties (point-min)
								 (point))
				 ", "
				 ispell-version))
	    (message result))
	;; return library directory.
	(if (re-search-forward "LIBDIR = \\\"\\([^ \t\n]*\\)\\\"" nil t)
	    (setq result (buffer-substring (match-beginning 1) (match-end 1)))))
      (goto-char (point-min))
      (if (not (memq status '(0 nil)))
	  (error "%s exited with %s %s" ispell-program-name
		 (if (stringp status) "signal" "code") status))
      (setq case-fold-search t
	    status (re-search-forward
		    (concat "\\<\\("
			    (format "%d" (car ispell-required-version))
			    "\\)\\.\\([0-9]*\\)\\.\\([0-9]*\\)\\>")
		    nil t)
	    case-fold-search case-fold-search-val)
      (if (or (not status)	; major version mismatch
	      (< (car (read-from-string (buffer-substring-no-properties
					 (match-beginning 2) (match-end 2))))
		 (car (cdr ispell-required-version)))) ; minor version mismatch
	  (error "%s version 3 release %d.%d.%d or greater is required"
		 ispell-program-name (car ispell-required-version)
		 (car (cdr ispell-required-version))
		 (car (cdr (cdr ispell-required-version))))
	;; check that it is the correct version.
	(if (and (= (car (read-from-string (buffer-substring-no-properties
					    (match-beginning 2)(match-end 2))))
		    (car (cdr ispell-required-version)))
		 (< (car (read-from-string (buffer-substring-no-properties
					    (match-beginning 3)(match-end 3))))
		    (car (cdr (cdr ispell-required-version)))))
	    (setq ispell-offset 0)))
      (kill-buffer (current-buffer)))
    result))



;;; The preparation of the menu bar menu must be autoloaded
;;; because otherwise this file gets autoloaded every time Emacs starts
;;; so that it can set up the menus and determine keyboard equivalents.


;;;###autoload
(defvar ispell-menu-map nil "Key map for ispell menu.")
;;; redo menu when loading ispell to get dictionary modifications
(setq ispell-menu-map nil)

;;;###autoload
(defvar ispell-menu-xemacs nil
  "Spelling menu for XEmacs.
If nil when package is loaded, a standard menu will be set,
and added as a submenu of the \"Edit\" menu.")

;;; Break out XEmacs menu and split into several calls to avoid having
;;; long lines in loaddefs.el.  Detect need off following constant.

;;; Set up dictionary
;;;###autoload
(defvar ispell-menu-map-needed
  ;; only needed when not version 18 and not XEmacs.
  (and (not ispell-menu-map)
       (not xemacsp)
       'reload))

(defvar ispell-library-directory (condition-case ()
				     (check-ispell-version)
				   (error nil))
  "Directory where ispell dictionaries reside.")

(defvar ispell-process nil
  "The process object for Ispell.")

(defvar ispell-async-processp (and (fboundp 'kill-process)
				   (fboundp 'process-send-string)
				   (fboundp 'accept-process-output)
				   ;;(fboundp 'start-process)
				   ;;(fboundp 'set-process-filter)
				   ;;(fboundp 'process-kill-without-query)
				   )
  "Non-nil means that the OS supports asynchronous processes.")

;;;###autoload
(if ispell-menu-map-needed
    (let ((dicts (reverse (cons (cons "default" nil) ispell-dictionary-alist)))
	  (dir (if (boundp 'ispell-library-directory) ispell-library-directory))
	  name load-dict)
      (setq ispell-menu-map (make-sparse-keymap "Spell"))
      ;; add the dictionaries to the bottom of the list.
      (while dicts
	(setq name (car (car dicts))
	      load-dict (car (cdr (member "-d" (nth 5 (car dicts)))))
	      dicts (cdr dicts))
	(cond ((not (stringp name))
	       (define-key ispell-menu-map [default]
		 '("Select Default Dict"
		   "Dictionary for which Ispell was configured"
		   . (lambda () (interactive)
		       (ispell-change-dictionary "default")))))
	      ((or (not dir)		; load all if library dir not defined
		   (file-exists-p (concat dir "/" name ".hash"))
		   (file-exists-p (concat dir "/" name ".has"))
		   (and load-dict
			(or (file-exists-p (concat dir "/" load-dict ".hash"))
			    (file-exists-p (concat dir "/" load-dict ".has")))))
	       (define-key ispell-menu-map (vector (intern name))
		 (cons (concat "Select " (capitalize name) " Dict")
		       `(lambda () (interactive)
			  (ispell-change-dictionary ,name)))))))))


;;; define commands in menu in opposite order you want them to appear.
;;;###autoload
(if ispell-menu-map-needed
    (progn
      (define-key ispell-menu-map [ispell-change-dictionary]
	'(menu-item "Change Dictionary..." ispell-change-dictionary
		    :help "Supply explicit dictionary file name"))
      (define-key ispell-menu-map [ispell-kill-ispell]
	'(menu-item "Kill Process" ispell-kill-ispell
		    :enable (and (boundp 'ispell-process) ispell-process
				 (eq (ispell-process-status) 'run))
		    :help "Terminate Ispell subprocess"))
      (define-key ispell-menu-map [ispell-pdict-save]
	'(menu-item "Save Dictionary"
		    (lambda () (interactive) (ispell-pdict-save t t))
		    :help "Save personal dictionary"))
      (define-key ispell-menu-map [ispell-customize]
	'(menu-item "Customize..."
		    (lambda () (interactive) (customize-group 'ispell))
		    :help "Customize spell checking options"))
      (define-key ispell-menu-map [ispell-help]
	;; use (x-popup-menu last-nonmenu-event(list "" ispell-help-list)) ?
	'(menu-item "Help"
		    (lambda () (interactive) (describe-function 'ispell-help))
		    :help "Show standard Ispell keybindings and commands"))
      (define-key ispell-menu-map [flyspell-mode]
	'(menu-item "Automatic spell checking (Flyspell)"
		    flyspell-mode
		    :help "Check spelling while you edit the text"
		    :button (:toggle . flyspell-mode)))
      (define-key ispell-menu-map [ispell-complete-word]
	'(menu-item "Complete Word" ispell-complete-word
		    :help "Complete word at cursor using dictionary"))
      (define-key ispell-menu-map [ispell-complete-word-interior-frag]
	'(menu-item "Complete Word Fragment" ispell-complete-word-interior-frag
		    :help "Complete word fragment at cursor"))))

;;;###autoload
(if ispell-menu-map-needed
    (progn
      (define-key ispell-menu-map [ispell-continue]
	'(menu-item "Continue Spell-Checking" ispell-continue
		    :enable (and (boundp 'ispell-region-end)
				 (marker-position ispell-region-end)
				 (equal (marker-buffer ispell-region-end)
					(current-buffer)))
		    :help "Continue spell checking last region"))
      (define-key ispell-menu-map [ispell-word]
	'(menu-item "Spell-Check Word" ispell-word
		    :help "Spell-check word at cursor"))
      (define-key ispell-menu-map [ispell-comments-and-strings]
	'(menu-item "Spell-Check Comments" ispell-comments-and-strings
		    :help "Spell-check only comments and strings"))))

;;;###autoload
(if ispell-menu-map-needed
    (progn
      (define-key ispell-menu-map [ispell-region]
	'(menu-item "Spell-Check Region" ispell-region
		    :enable mark-active
		    :help "Spell-check text in marked region"))
      (define-key ispell-menu-map [ispell-message]
	'(menu-item "Spell-Check Message" ispell-message
		    :help "Skip headers and included message text"))
      (define-key ispell-menu-map [ispell-buffer]
	'(menu-item "Spell-Check Buffer" ispell-buffer
		    :help "Check spelling of selected buffer"))
      ;;(put 'ispell-region 'menu-enable 'mark-active)
      (fset 'ispell-menu-map (symbol-value 'ispell-menu-map))))

;;; XEmacs versions 19 & 20
(if (and xemacsp
	 (featurep 'menubar)
	 (null ispell-menu-xemacs)
	 (not (and (boundp 'infodock-version) infodock-version)))
    (let ((dicts (cons (cons "default" nil) ispell-dictionary-alist))
	  (current-menubar (or current-menubar default-menubar))
	  (menu
	   '(["Help"		(describe-function 'ispell-help) t]
	     ;;["Help"		(popup-menu ispell-help-list)	t]
	     ["Check Message"	ispell-message			t]
	     ["Check Buffer"	ispell-buffer			t]
	     ["Check Comments"	ispell-comments-and-strings	t]
	     ["Check Word"	ispell-word			t]
	     ["Check Region"	ispell-region  (or (not zmacs-regions) (mark))]
	     ["Continue Check"	ispell-continue			t]
	     ["Complete Word Frag"ispell-complete-word-interior-frag t]
	     ["Complete Word"	ispell-complete-word		t]
	     ["Kill Process"	ispell-kill-ispell		t]
	     "-"
	     ["Save Personal Dict"(ispell-pdict-save t t)	t]
	     ["Change Dictionary" ispell-change-dictionary	t]
	     ["Select Default"  (ispell-change-dictionary "default") t]))
	  name load-dict)
      (while dicts
	(setq name (car (car dicts))
	      load-dict (car (cdr (member "-d" (nth 5 (car dicts)))))
	      dicts (cdr dicts))
	;; Include if the dictionary is in the library, or dir not defined.
	(if (and (stringp name)
		 (or (not ispell-library-directory)
		     (file-exists-p (concat ispell-library-directory "/"
					    name ".hash"))
		     (file-exists-p (concat ispell-library-directory "/"
					    name ".has"))
		     (and load-dict
			  (or (file-exists-p (concat ispell-library-directory "/"
						     load-dict ".hash"))
			      (file-exists-p (concat ispell-library-directory "/"
						     load-dict ".has"))))))
	    (setq menu (append menu
			       (list
				 (vector (concat "Select " (capitalize name))
					 (list 'ispell-change-dictionary name)
					 t))))))
      (setq ispell-menu-xemacs menu)
      (if current-menubar
	  (progn
	    (delete-menu-item '("Edit" "Spell")) ; in case already defined
	    (add-menu '("Edit") "Spell" ispell-menu-xemacs)))))

;;; Allow incrementing characters as integers in XEmacs 20
(if (and xemacsp
	 (fboundp 'int-char))
    (fset 'ispell-int-char 'int-char)
  ;; Emacs and XEmacs 19 or earlier
  (fset 'ispell-int-char 'identity))


;;; **********************************************************************


;;; This variable contains the current dictionary being used if the ispell
;;; process is running.  Otherwise it contains the global default.
(defvar ispell-dictionary nil
  "The name of the current dictionary, or nil for the default.
When `ispell-local-dictionary' is nil, `ispell-dictionary' is used to select
the dictionary for new buffers.

This is passed to the ispell process using the `-d' switch and is
used as key in `ispell-dictionary-alist' (which see).")

(defun ispell-decode-string (str)
  "Decodes multibyte character strings.
Protects against bogus binding of `enable-multibyte-characters' in XEmacs."
  (if (and (or xemacsp
	       (and (boundp 'enable-multibyte-characters)
		    enable-multibyte-characters))
	   (fboundp 'decode-coding-string)
	   (ispell-get-coding-system))
      (decode-coding-string str (ispell-get-coding-system))
    str))

(defun ispell-get-casechars ()
  (ispell-decode-string
   (nth 1 (assoc ispell-dictionary ispell-dictionary-alist))))
(defun ispell-get-not-casechars ()
  (ispell-decode-string
   (nth 2 (assoc ispell-dictionary ispell-dictionary-alist))))
(defun ispell-get-otherchars ()
  (ispell-decode-string
   (nth 3 (assoc ispell-dictionary ispell-dictionary-alist))))
(defun ispell-get-many-otherchars-p ()
  (nth 4 (assoc ispell-dictionary ispell-dictionary-alist)))
(defun ispell-get-ispell-args ()
  (nth 5 (assoc ispell-dictionary ispell-dictionary-alist)))
(defun ispell-get-extended-character-mode ()
  (nth 6 (assoc ispell-dictionary ispell-dictionary-alist)))
(defun ispell-get-coding-system ()
  (nth 7 (assoc ispell-dictionary ispell-dictionary-alist)))


(defvar ispell-pdict-modified-p nil
  "Non-nil means personal dictionary has modifications to be saved.")

;;; If you want to save the dictionary when quitting, must do so explicitly.
;;; When non-nil, the spell session is terminated.
;;; When numeric, contains cursor location in buffer, and cursor remains there.
(defvar ispell-quit nil)

(defvar ispell-process-directory nil
  "The directory where `ispell-process' was started.")

(defvar ispell-filter nil
  "Output filter from piped calls to Ispell.")

(defvar ispell-filter-continue nil
  "Control variable for Ispell filter function.")

(defvar ispell-output-buffer nil
  "Buffer used for reading output of a synchronous Ispell subprocess.")

(defvar ispell-session-buffer nil
  "Buffer used for passing input to a synchronous Ispell subprocess.")

(defvar ispell-cmd-args nil
  "Command-line arguments to pass to a synchronous Ispell subprocess.")

(defvar ispell-query-replace-marker (make-marker)
  "Marker for `query-replace' processing.")

(defvar ispell-recursive-edit-marker (make-marker)
  "Marker for return point from recursive edit.")

(defvar ispell-checking-message nil
  "Non-nil when we're checking a mail message.")

(defconst ispell-choices-buffer "*Choices*")

(defvar ispell-overlay nil "Overlay variable for Ispell highlighting.")

;;; *** Buffer Local Definitions ***

(defconst ispell-words-keyword "LocalWords: "                                 
  "The keyword for local oddly-spelled words to accept.
The keyword will be followed by any number of local word spellings.
There can be multiple of these keywords in the file.")

(defconst ispell-dictionary-keyword "Local IspellDict: "
  "The keyword for a local dictionary to use.
The keyword must be followed by a correct dictionary name in
`ispell-dictionary-alist'.  When multiple occurrences exist, the last keyword
definition is used.")

(defconst ispell-pdict-keyword "Local IspellPersDict: "
  "The keyword for defining buffer local dictionaries.
Keyword must be followed by the filename of a personal dictionary.
The last occurring definition in the buffer will be used.")

(defconst ispell-parsing-keyword "Local IspellParsing: "
  "The keyword for overriding default Ispell parsing.
The above keyword string should be followed by `latex-mode' or
`nroff-mode' to put the current buffer into the desired parsing mode.

Extended character mode can be changed for this buffer by placing
a `~' followed by an extended-character mode -- such as `~.tex'.
The last occurring definition in the buffer will be used.")

;;;###autoload
(defvar ispell-skip-region-alist
  '((ispell-words-keyword	   forward-line)
    (ispell-dictionary-keyword	   forward-line)
    (ispell-pdict-keyword	   forward-line)
    (ispell-parsing-keyword	   forward-line)
    ("^---*BEGIN PGP [A-Z ]*--*" . "^---*END PGP [A-Z ]*--*")
    ("^---* \\(Start of \\)?[Ff]orwarded [Mm]essage"   . "^---* End of [Ff]orwarded [Mm]essage")
    ;; Matches e-mail addresses, file names, http addresses, etc.  The `-+'
    ;; pattern necessary for performance reasons when `-' part of word syntax.
    ("\\(-+\\|\\(/\\|\\(\\(\\w\\|[-_]\\)+[.:@]\\)\\)\\(\\w\\|[-_]\\)*\\([.:/@]+\\(\\w\\|[-_]\\|~\\)+\\)+\\)")
    ;; This is a pretty complex regexp.  It can be simplified to the following:
    ;; "\\(\\w\\|[-_]\\)*\\([.:/@]+\\(\\w\\|[-_]\\|~\\)+\\)+"
    ;; but some valid text will be skipped, e.g. "his/her".  This could be
    ;; fixed up (at the expense of a moderately more complex regexp)
    ;; by not allowing "/" to be the character which triggers the
    ;; identification of the computer name, e.g.:
    ;; "\\(\\w\\|[-_]\\)+[.:@]\\(\\w\\|[-_]\\)*\\([.:/@]+\\(\\w\\|[-_]\\|~\\)+\\)+"
    )
  "Alist expressing beginning and end of regions not to spell check.
The alist key must be a regular expression.
Valid forms include:
  (KEY) - just skip the key.
  (KEY . REGEXP) - skip to the end of REGEXP.  REGEXP may be string or symbol.
  (KEY REGEXP) - skip to end of REGEXP.  REGEXP must be a string.
  (KEY FUNCTION ARGS) - FUNCTION called with ARGS returns end of region.")



;;;###autoload
(defvar ispell-tex-skip-alists
  '((;;("%\\[" . "%\\]") ; AMStex block comment...
     ;; All the standard LaTeX keywords from L. Lamport's guide:
     ;; \cite, \hspace, \hspace*, \hyphenation, \include, \includeonly, \input,
     ;; \label, \nocite, \rule (in ispell - rest included here)
     ("\\\\addcontentsline"              ispell-tex-arg-end 2)
     ("\\\\add\\(tocontents\\|vspace\\)" ispell-tex-arg-end)
     ("\\\\\\([aA]lph\\|arabic\\)"	 ispell-tex-arg-end)
     ;;("\\\\author"			 ispell-tex-arg-end)
     ("\\\\bibliographystyle"		 ispell-tex-arg-end)
     ("\\\\makebox"			 ispell-tex-arg-end 0)
     ("\\\\e?psfig"			 ispell-tex-arg-end)
     ("\\\\document\\(class\\|style\\)" .
      "\\\\begin[ \t\n]*{[ \t\n]*document[ \t\n]*}"))
    (;; delimited with \begin.  In ispell: displaymath, eqnarray, eqnarray*,
     ;; equation, minipage, picture, tabular, tabular* (ispell)
     ("\\(figure\\|table\\)\\*?"	 ispell-tex-arg-end 0)
     ("list"				 ispell-tex-arg-end 2)
     ("program"		. "\\\\end[ \t\n]*{[ \t\n]*program[ \t\n]*}")
     ("verbatim\\*?"	. "\\\\end[ \t\n]*{[ \t\n]*verbatim\\*?[ \t\n]*}")))
  "*Lists of regions to be skipped in TeX mode.
First list is used raw.
Second list has key placed inside \\begin{}.

Delete or add any regions you want to be automatically selected
for skipping in latex mode.")


(defvar ispell-local-pdict ispell-personal-dictionary
  "A buffer local variable containing the current personal dictionary.
If non-nil, the value must be a string, which is a file name.

If you specify a personal dictionary for the current buffer which is
different from the current personal dictionary, the effect is similar
to calling \\[ispell-change-dictionary].  This variable is automatically
set when defined in the file with either `ispell-pdict-keyword' or the
local variable syntax.")

(make-variable-buffer-local 'ispell-local-pdict)

(defvar ispell-buffer-local-name nil
  "Contains the buffer name if local word definitions were used.
Ispell is then restarted because the local words could conflict.")

(defvar ispell-parser 'use-mode-name
   "*Indicates whether ispell should parse the current buffer as TeX Code.
Special value `use-mode-name' tries to guess using the name of `major-mode'.
Default parser is `nroff'.
Currently the only other valid parser is `tex'.

You can set this variable in hooks in your init file -- eg:

(add-hook 'tex-mode-hook (function (lambda () (setq ispell-parser 'tex))))")

(defvar ispell-region-end (make-marker)
  "Marker that allows spelling continuations.")

(defvar ispell-check-only nil
  "If non-nil, `ispell-word' does not try to correct the word.")


;;; **********************************************************************
;;; **********************************************************************



;;;###autoload
(define-key esc-map "$" 'ispell-word)


(defun ispell-accept-output (&optional timeout-secs timeout-msecs)
  "Wait for output from ispell process, or TIMEOUT-SECS and TIMEOUT-MSECS.
If asynchronous subprocesses are not supported, call `ispell-filter' and
pass it the output of the last ispell invocation."
  (if ispell-async-processp
      (accept-process-output ispell-process timeout-secs timeout-msecs)
    (if (null ispell-process)
	(error "No Ispell process to read output from!")
      (let ((buf ispell-output-buffer)
	    ispell-output)
	(if (not (bufferp buf))
	    (setq ispell-filter nil)
	  (save-excursion
	    (set-buffer buf)
	    (setq ispell-output (buffer-substring-no-properties
				 (point-min) (point-max))))
	  (ispell-filter t ispell-output)
	  (save-excursion
	    (set-buffer buf)
	    (erase-buffer)))))))


(defun ispell-send-string (string)
  "Send the string STRING to the Ispell process."
  (if ispell-async-processp
      (process-send-string ispell-process string)
    ;; Asynchronous subprocesses aren't supported on this losing system.
    ;; We keep all the directives passed to Ispell during the entire
    ;; session in a buffer, and pass them anew each time we invoke
    ;; Ispell to process another chunk of text.  (Yes, I know this is a
    ;; terrible kludge, and it's a bit slow, but it does get the work done.)
    (let ((cmd (aref string 0))
	  ;; The following commands are not passed to Ispell until
	  ;; we have a *reall* reason to invoke it.
	  (cmds-to-defer '(?* ?@ ?~ ?+ ?- ?! ?%))
	  (default-major-mode 'fundamental-mode)
	  (session-buf ispell-session-buffer)
	  (output-buf ispell-output-buffer)
	  (ispell-args ispell-cmd-args)
	  (defdir ispell-process-directory)
	  prev-pos)
      (save-excursion
	(set-buffer session-buf)
	(setq prev-pos (point))
	(setq default-directory defdir)
	(insert string)
	(if (not (memq cmd cmds-to-defer))
	    (let (coding-system-for-read coding-system-for-write status)
	      (if (or xemacsp
		      (and (boundp 'enable-multibyte-characters)
			   enable-multibyte-characters))
		  (setq coding-system-for-read (ispell-get-coding-system)
			coding-system-for-write (ispell-get-coding-system)))
	      (set-buffer output-buf)
	      (erase-buffer)
	      (set-buffer session-buf)
	      (setq status
		    (apply 'call-process-region (point-min) (point-max)
			   ispell-program-name nil
			   output-buf nil
			   "-a" "-m" ispell-args))
	      (set-buffer output-buf)
	      (goto-char (point-min))
	      (save-match-data
		(if (not (looking-at "@(#) "))
		    (error "Ispell error: %s"
			   (buffer-substring-no-properties
			    (point) (progn (end-of-line) (point)))))
		;; If STRING is "^Z\n", we just started Ispell and need
		;; to retain its version ID line in the output buffer.
		;; Otherwise, remove the ID line, as it will confuse
		;; `ispell-filter'.
		(or (string= string "\032\n")
		    (progn
		      (forward-line)
		      (delete-region (point-min) (point))))
		;; If STRING begins with ^ or any normal character, we need
		;; to remove the last line from the session buffer, since it
		;; was just spell-checked, and we don't want to check it again.
		;; The same goes for the # command, since Ispell already saved
		;; the personal dictionary.
		(set-buffer session-buf)
		(delete-region prev-pos (point))
		;; Ispell run synchronously saves the personal dictionary
		;; after each successful command.  So we can remove any
		;; lines in the session buffer that insert words into the
		;; dictionary.
		(if (memq status '(0 nil))
		    (let ((more-lines t))
		      (goto-char (point-min))
		      (while more-lines
			(if (looking-at "^\\*")
			    (let ((start (point)))
			      (forward-line)
			      (delete-region start (point)))
			  (setq more-lines (= 0 (forward-line))))))))))))))



;;;###autoload
(defun ispell-word (&optional following quietly continue)
  "Check spelling of word under or before the cursor.
If the word is not found in dictionary, display possible corrections
in a window allowing you to choose one.

If optional argument FOLLOWING is non-nil or if `ispell-following-word'
is non-nil when called interactively, then the following word
\(rather than preceding\) is checked when the cursor is not over a word.
When the optional argument QUIETLY is non-nil or `ispell-quietly' is non-nil
when called interactively, non-corrective messages are suppressed.

With a prefix argument (or if CONTINUE is non-nil),
resume interrupted spell-checking of a buffer or region.

Word syntax described by `ispell-dictionary-alist' (which see).

This will check or reload the dictionary.  Use \\[ispell-change-dictionary]
or \\[ispell-region] to update the Ispell process.

return values:
nil           word is correct or spelling is accpeted.
0             word is inserted into buffer-local definitions.
\"word\"        word corrected from word list.
\(\"word\" arg\)  word is hand entered.
quit          spell session exited."

  (interactive (list nil nil current-prefix-arg))
  (if continue
      (ispell-continue)
    (if (interactive-p)
	(setq following ispell-following-word
	      quietly ispell-quietly))
    (ispell-accept-buffer-local-defs)	; use the correct dictionary
    (let ((cursor-location (point))	; retain cursor location
	  (word (ispell-get-word following))
	  start end poss new-word replace)
      ;; De-structure return word info list.
      (setq start (car (cdr word))
	    end (car (cdr (cdr word)))
	    word (car word))

      ;; now check spelling of word if it has 3 or more characters.
      (cond
       ((> (length word) 2)
	(or quietly
	    (message "Checking spelling of %s..."
		     (funcall ispell-format-word word)))
	(ispell-send-string "%\n")	; put in verbose mode
	(ispell-send-string (concat "^" word "\n"))
	;; wait until ispell has processed word
	(while (progn
		 (ispell-accept-output)
		 (not (string= "" (car ispell-filter)))))
	;;(ispell-send-string "!\n") ;back to terse mode.
	(setq ispell-filter (cdr ispell-filter)) ; remove extra \n
	(if (and ispell-filter (listp ispell-filter))
	    (if (> (length ispell-filter) 1)
		(error "Ispell and its process have different character maps")
	      (setq poss (ispell-parse-output (car ispell-filter)))))
	(cond ((eq poss t)
	       (or quietly
		   (message "%s is correct"
			    (funcall ispell-format-word word))))
	      ((stringp poss)
	       (or quietly
		   (message "%s is correct because of root %s"
			    (funcall ispell-format-word word)
			    (funcall ispell-format-word poss))))
	      ((null poss) (message "Error in ispell process"))
	      (ispell-check-only	; called from ispell minor mode.
	       (beep)
	       (message "%s is incorrect" (funcall ispell-format-word word)))
	      (t			; prompt for correct word.
	       (save-window-excursion
		 (setq replace (ispell-command-loop
				(car (cdr (cdr poss)))
				(car (cdr (cdr (cdr poss))))
				(car poss) start end)))
	       (cond ((equal 0 replace)
		      (ispell-add-per-file-word-list (car poss)))
		     (replace
		      (setq new-word (if (atom replace) replace (car replace))
			    cursor-location (+ (- (length word) (- end start))
					       cursor-location))
		      (if (not (equal new-word (car poss)))
			  (progn
			    (delete-region start end)
			    (setq start (point))
			    (insert new-word)
			    (setq end (point))))
		      (if (not (atom replace)) ;recheck spelling of replacement
			  (progn
			    (if (car (cdr replace)) ; query replace requested
				(save-window-excursion
				  (query-replace word new-word t)))
			    (goto-char start)
			    ;; single word could be split into multiple words
			    (setq ispell-quit (not (ispell-region start end)))
			    ))))
	       ;; keep if rechecking word and we keep choices win.
	       (if (get-buffer ispell-choices-buffer)
		   (kill-buffer ispell-choices-buffer))))
	(ispell-pdict-save ispell-silently-savep)
	;; NB: Cancels ispell-quit incorrectly if called from ispell-region
	(if ispell-quit (setq ispell-quit nil replace 'quit))))
      (goto-char cursor-location)	; return to original location
      replace)))


(defun ispell-get-word (following &optional extra-otherchars)
  "Return the word for spell-checking according to ispell syntax.
If optional argument FOLLOWING is non-nil or if `ispell-following-word'
is non-nil when called interactively, then the following word
\(rather than preceding\) is checked when the cursor is not over a word.
Optional second argument contains otherchars that can be included in word
many times.

Word syntax described by `ispell-dictionary-alist' (which see)."
  (let* ((ispell-casechars (ispell-get-casechars))
	 (ispell-not-casechars (ispell-get-not-casechars))
	 (ispell-otherchars (ispell-get-otherchars))
	 (ispell-many-otherchars-p (ispell-get-many-otherchars-p))
	 (word-regexp (concat ispell-casechars
			      "+\\("
			      (if (not (string= "" ispell-otherchars))
				  (concat ispell-otherchars "?"))
			      (if extra-otherchars
				  (concat extra-otherchars "?"))
			      ispell-casechars
			      "+\\)"
			      (if (or ispell-many-otherchars-p
				      extra-otherchars)
				  "*" "?")))
	 did-it-once prevpt
	 start end word)
    ;; find the word
    (if (not (looking-at ispell-casechars))
	(if following
	    (re-search-forward ispell-casechars (point-max) t)
	  (re-search-backward ispell-casechars (point-min) t)))
    ;; move to front of word
    (re-search-backward ispell-not-casechars (point-min) 'start)
    (while (and (or (and (not (string= "" ispell-otherchars))
			 (looking-at ispell-otherchars))
		    (and extra-otherchars (looking-at extra-otherchars)))
		(not (bobp))
		(or (not did-it-once)
		    ispell-many-otherchars-p)
		(not (eq prevpt (point))))
      (if (and extra-otherchars (looking-at extra-otherchars))
	  (progn
	    (backward-char 1)
	    (if (looking-at ispell-casechars)
		(re-search-backward ispell-not-casechars (point-min) 'move)))
	(setq did-it-once t
	      prevpt (point))
	(backward-char 1)
	(if (looking-at ispell-casechars)
	    (re-search-backward ispell-not-casechars (point-min) 'move)
	  (backward-char -1))))
    ;; Now mark the word and save to string.
    (if (not (re-search-forward word-regexp (point-max) t))
	(if ispell-check-only
	    ;; return dummy word when just flagging misspellings
	    (list "" (point) (point))
	  (error "No word found to check!"))
      (setq start (match-beginning 0)
	    end (point)
	    word (buffer-substring-no-properties start end))
      (list word start end))))


;;; Global ispell-pdict-modified-p is set by ispell-command-loop and
;;; tracks changes in the dictionary.  The global may either be
;;; a value or a list, whose value is the state of whether the
;;; dictionary needs to be saved.

;;;###autoload
(defun ispell-pdict-save (&optional no-query force-save)
  "Check to see if the personal dictionary has been modified.
If so, ask if it needs to be saved."
  (interactive (list ispell-silently-savep t))
  (if (and ispell-pdict-modified-p (listp ispell-pdict-modified-p))
      (setq ispell-pdict-modified-p (car ispell-pdict-modified-p)))
  (if (or ispell-pdict-modified-p force-save)
      (if (or no-query (y-or-n-p "Personal dictionary modified.  Save? "))
	  (progn
	    (ispell-send-string "#\n")	; save dictionary
	    (message "Personal dictionary saved."))))
  ;; unassert variable, even if not saved to avoid questioning.
  (setq ispell-pdict-modified-p nil))


(defun ispell-command-loop (miss guess word start end)
  "Display possible corrections from list MISS.
GUESS lists possibly valid affix construction of WORD.
Returns nil to keep word.
Returns 0 to insert locally into buffer-local dictionary.
Returns string for new chosen word.
Returns list for new replacement word (will be rechecked).
  Query-replace when list length is 2.
  Automatic query-replace when second element is `query-replace'.
Highlights the word, which is assumed to run from START to END.
Global `ispell-pdict-modified-p' becomes a list where the only value
indicates whether the dictionary has been modified when option `a' or `i' is
used.
Global `ispell-quit' set to start location to continue spell session."
  (let ((count ?0)
	(line ispell-choices-win-default-height)
	;; ensure 4 context lines.
	(max-lines (- (ispell-adjusted-window-height) 4))
	(choices miss)
	(window-min-height (min window-min-height
				ispell-choices-win-default-height))
	(command-characters '( ?  ?i ?a ?A ?r ?R ?? ?x ?X ?q ?l ?u ?m ))
	(dedicated (window-dedicated-p (selected-window)))
	(skipped 0)
	char num result textwin dedicated-win)

    ;; setup the *Choices* buffer with valid data.
    (save-excursion
      (set-buffer (get-buffer-create ispell-choices-buffer))
      (setq mode-line-format (concat "--  %b  --  word: " word))
      ;; XEmacs: prevent thick modeline vs increasing height in overlay-window
      ;;(and (fboundp 'set-specifier)
      ;;     (set-specifier has-modeline-p (cons (current-buffer) nil)))
      (erase-buffer)
      (if guess
	  (progn
	    (insert "Affix rules generate and capitalize "
		    "this word as shown below:\n\t")
	    (while guess
	      (if (> (+ 4 (current-column) (length (car guess)))
		     (window-width))
		  (progn
		    (insert "\n\t")
		    (setq line (1+ line))))
	      (insert (car guess) "    ")
	      (setq guess (cdr guess)))
	    (insert "\nUse option `i' if this is a correct composition"
		    " from the derivative root.\n")
	    (setq line (+ line (if choices 3 2)))))
      (while (and choices
		  (< (if (> (+ 7 (current-column) (length (car choices))
			       (if (> count ?~) 3 0))
			    (window-width))
			 (progn
			   (insert "\n")
			   (setq line (1+ line)))
		       line)
		     max-lines))
	;; not so good if there are over 20 or 30 options, but then, if
	;; there are that many you don't want to scan them all anyway...
	(while (memq count command-characters) ; skip command characters.
	  (setq count (ispell-int-char (1+ count))
		skipped (1+ skipped)))
	(insert "(" count ") " (car choices) "  ")
	(setq choices (cdr choices)
	      count (ispell-int-char (1+ count))))
      (setq count (ispell-int-char (- count ?0 skipped))))

    ;; ensure word is visible
    (if (not (pos-visible-in-window-p end))
	(sit-for 0))

    ;; allow temporary split of dedicated windows...
    (if dedicated
	(progn
	  (setq dedicated-win (selected-window))
	  (set-window-dedicated-p dedicated-win nil)))

    ;; Display choices for misspelled word.
    (ispell-show-choices line end)
    (select-window (setq textwin (next-window)))

    ;; highlight word, protecting current buffer status
    (unwind-protect
	(progn
	  (and ispell-highlight-p
	       (ispell-highlight-spelling-error start end t))
	  ;; Loop until a valid choice is made.
	  (while
	      (eq
	       t
	       (setq
		result
		(progn
		  (undo-boundary)
		  (let (message-log-max)
		    (message (concat "C-h or ? for more options; SPC to leave "
				     "unchanged, Character to replace word")))
		  (let ((inhibit-quit t))
		    (setq char (if (fboundp 'read-char-exclusive)
				   (read-char-exclusive)
				 (read-char))
			  skipped 0)
		    (if (or quit-flag (= char ?\C-g)) ; C-g is like typing X
			(setq char ?X
			      quit-flag nil)))
		  ;; Adjust num to array offset skipping command characters.
		  (let ((com-chars command-characters))
		    (while com-chars
		      (if (and (> (car com-chars) ?0) (< (car com-chars) char))
			  (setq skipped (1+ skipped)))
		      (setq com-chars (cdr com-chars)))
		    (setq num (- char ?0 skipped)))

		  (cond
		   ((= char ? ) nil)	; accept word this time only
		   ((= char ?i)		; accept and insert word into pers dict
		    (ispell-send-string (concat "*" word "\n"))
		    (setq ispell-pdict-modified-p '(t)) ; dictionary modified!
		    nil)
		   ((or (= char ?a) (= char ?A)) ; accept word without insert
		    (ispell-send-string (concat "@" word "\n"))
		    (if (null ispell-pdict-modified-p)
			(setq ispell-pdict-modified-p
			      (list ispell-pdict-modified-p)))
		    (if (= char ?A) 0))	; return 0 for ispell-add buffer-local
		   ((or (= char ?r) (= char ?R)) ; type in replacement
		    (and (eq 'block ispell-highlight-p) ; refresh tty's
			 (ispell-highlight-spelling-error start end nil t))
		    (let ((result
			   (if (or (= char ?R) ispell-query-replace-choices)
			       (list (read-string
				      (format "Query-replacement for %s: "word)
				      word)
				     t)
			     (cons (read-string "Replacement for: " word)
				   nil))))
		      (and (eq 'block ispell-highlight-p)
			   (ispell-highlight-spelling-error start end nil
							    'block))
		      result))
		   ((or (= char ??) (= char help-char) (= char ?\C-h))
		    (and (eq 'block ispell-highlight-p)
			 (ispell-highlight-spelling-error start end nil t))
		    (ispell-help)
		    (and (eq 'block ispell-highlight-p)
			 (ispell-highlight-spelling-error start end nil
							  'block))
		    t)
		   ;; Quit and move point back.
		   ((= char ?x)
		    (ispell-pdict-save ispell-silently-savep)
		    (message "Exited spell-checking")
		    (setq ispell-quit t)
		    nil)
		   ;; Quit and preserve point.
		   ((= char ?X)
		    (ispell-pdict-save ispell-silently-savep)
		    (message "%s"
		     (substitute-command-keys
		      (concat "Spell-checking suspended;"
			      " use C-u \\[ispell-word] to resume")))
		    (setq ispell-quit start)
		    nil)
		   ((= char ?q)
		    (if (y-or-n-p "Really kill Ispell process? ")
			(progn
			  (ispell-kill-ispell t) ; terminate process.
			  (setq ispell-quit (or (not ispell-checking-message)
						(point))
				ispell-pdict-modified-p nil))
		      t))		; continue if they don't quit.
		   ((= char ?l)
		    (and (eq 'block ispell-highlight-p) ; refresh tty displays
			 (ispell-highlight-spelling-error start end nil t))
		    (let ((new-word (read-string
				     "Lookup string (`*' is wildcard): "
				     word)))
		      (if new-word
			  (progn
			    (save-excursion
			      (set-buffer (get-buffer-create
					   ispell-choices-buffer))
			      (erase-buffer)
			      (setq count ?0
				    skipped 0
				    mode-line-format (concat
						      "--  %b  --  word: "
						      new-word)
				    miss (lookup-words new-word)
				    choices miss
				    line ispell-choices-win-default-height)
			      (while (and choices ; adjust choices window.
					  (< (if (> (+ 7 (current-column)
						       (length (car choices))
						       (if (> count ?~) 3 0))
						    (window-width))
						 (progn
						   (insert "\n")
						   (setq line (1+ line)))
					       line)
					     max-lines))
				(while (memq count command-characters)
				  (setq count (ispell-int-char (1+ count))
					skipped (1+ skipped)))
				(insert "(" count ") " (car choices) "  ")
				(setq choices (cdr choices)
				      count (ispell-int-char (1+ count))))
			      (setq count (ispell-int-char
					   (- count ?0 skipped))))
			    (ispell-show-choices line end)
			    (select-window (next-window)))))
		    (and (eq 'block ispell-highlight-p)
			 (ispell-highlight-spelling-error start end nil
							  'block))
		    t)			; reselect from new choices
		   ((= char ?u)		; insert lowercase into dictionary
		    (ispell-send-string (concat "*" (downcase word) "\n"))
		    (setq ispell-pdict-modified-p '(t)) ; dictionary modified!
		    nil)
		   ((= char ?m)		; type in what to insert
		    (ispell-send-string
		     (concat "*" (read-string "Insert: " word) "\n"))
		    (setq ispell-pdict-modified-p '(t))
		    (cons word nil))
		   ((and (>= num 0) (< num count))
		    (if ispell-query-replace-choices ; Query replace flag
			(list (nth num miss) 'query-replace)
		      (nth num miss)))
		   ((= char ?\C-l)
		    (redraw-display) t)
		   ((= char ?\C-r)
		    ;; This may have alignment errors if current line is edited
		    (if (marker-position ispell-recursive-edit-marker)
			(progn
			  (message "Only one recursive edit session supported")
			  (beep)
			  (sit-for 2))
		      (set-marker ispell-recursive-edit-marker start)
		      ;;(set-marker ispell-region-end reg-end)
		      (and ispell-highlight-p		; unhighlight
			   (ispell-highlight-spelling-error start end))
		      (unwind-protect
			  (progn
			    (message
			     "%s"
			     (substitute-command-keys
			      (concat "Exit recursive edit with"
				      " \\[exit-recursive-edit]")))
			    (save-window-excursion (save-excursion
						     (recursive-edit))))
			;; protected
			(goto-char ispell-recursive-edit-marker)
			(if (not (equal (marker-buffer
					 ispell-recursive-edit-marker)
					(current-buffer)))
			    (progn
			      (set-marker ispell-recursive-edit-marker nil)
			      (error
			       "Cannot continue ispell from this buffer.")))
			(set-marker ispell-recursive-edit-marker nil)))
		    (list word nil))	; recheck starting at this word.
		   ((= char ?\C-z)
		    (funcall (key-binding "\C-z"))
		    t)
		   (t (ding) t))))))
	  result)
      ;; protected
      (and ispell-highlight-p		; unhighlight
	   (save-window-excursion
	     (select-window textwin)
	     (ispell-highlight-spelling-error start end)))
      (if dedicated
	  (set-window-dedicated-p dedicated-win t)))))



(defun ispell-show-choices (line end)
  "Shows the choices in another buffer or frame."
  (if (and ispell-use-framepop-p (fboundp 'framepop-display-buffer))
      (progn
	(framepop-display-buffer (get-buffer ispell-choices-buffer))
	(get-buffer-window ispell-choices-buffer t)
	(select-window (previous-window))) ; *Choices* window
    ;; standard selection by splitting a small buffer out of this window.
    (let ((choices-window (get-buffer-window ispell-choices-buffer)))
      (if choices-window
	  (if (= line (ispell-adjusted-window-height choices-window))
	      (select-window choices-window)
	    ;; *Choices* window changed size.  Adjust the choices window
	    ;; without scrolling the spelled window when possible
	    (let ((window-line
		   (- line (ispell-adjusted-window-height choices-window)))
		  (visible (progn (vertical-motion -1) (point))))
	      (if (< line ispell-choices-win-default-height)
		  (setq window-line (+ window-line
				       (- ispell-choices-win-default-height
					  line))))
	      (move-to-window-line 0)
	      (vertical-motion window-line)
	      (set-window-start (selected-window)
				(if (> (point) visible) visible (point)))
	      (goto-char end)
	      (select-window (previous-window)) ; *Choices* window
	      (enlarge-window window-line)))
	;; Overlay *Choices* window when it isn't showing
	(ispell-overlay-window (max line ispell-choices-win-default-height)))
      (switch-to-buffer ispell-choices-buffer)
      (goto-char (point-min)))))


;;;###autoload
(defun ispell-help ()
  "Display a list of the options available when a misspelling is encountered.

Selections are:

DIGIT: Replace the word with a digit offered in the *Choices* buffer.
SPC:   Accept word this time.
`i':   Accept word and insert into private dictionary.
`a':   Accept word for this session.
`A':   Accept word and place in `buffer-local dictionary'.
`r':   Replace word with typed-in value.  Rechecked.
`R':   Replace word with typed-in value. Query-replaced in buffer. Rechecked.
`?':   Show these commands.
`x':   Exit spelling buffer.  Move cursor to original point.
`X':   Exit spelling buffer.  Leaves cursor at the current point, and permits
        the aborted check to be completed later.
`q':   Quit spelling session (Kills ispell process).
`l':   Look up typed-in replacement in alternate dictionary.  Wildcards okay.
`u':   Like `i', but the word is lower-cased first.
`m':   Place typed-in value in personal dictionary, then recheck current word.
`C-l':  redraws screen
`C-r':  recursive edit
`C-z':  suspend emacs or iconify frame"

  (if (equal ispell-help-in-bufferp 'electric)
      (progn
	(require 'ehelp)
	(with-electric-help
	 (function (lambda ()
		     ;;This shouldn't be necessary: with-electric-help needs
		     ;; an optional argument telling it about the smallest
		     ;; acceptable window-height of the help buffer.
		     (if (< (window-height) 15)
			 (enlarge-window
			  (- 15 (ispell-adjusted-window-height))))
		     (princ "Selections are:

DIGIT: Replace the word with a digit offered in the *Choices* buffer.
SPC:   Accept word this time.
`i':   Accept word and insert into private dictionary.
`a':   Accept word for this session.
`A':   Accept word and place in `buffer-local dictionary'.
`r':   Replace word with typed-in value.  Rechecked.
`R':   Replace word with typed-in value. Query-replaced in buffer. Rechecked.
`?':   Show these commands.
`x':   Exit spelling buffer.  Move cursor to original point.
`X':   Exit spelling buffer.  Leaves cursor at the current point, and permits
        the aborted check to be completed later.
`q':   Quit spelling session (Kills ispell process).
`l':   Look up typed-in replacement in alternate dictionary.  Wildcards okay.
`u':   Like `i', but the word is lower-cased first.
`m':   Place typed-in value in personal dictionary, then recheck current word.
`C-l':  redraws screen
`C-r':  recursive edit
`C-z':  suspend emacs or iconify frame")
		     nil	;undocumented requirement of with-electric-help
		     ))))


    (let ((help-1 (concat "[r/R]eplace word; [a/A]ccept for this session; "
			  "[i]nsert into private dictionary"))
	  (help-2 (concat "[l]ook a word up in alternate dictionary;  "
			  "e[x/X]it;  [q]uit session"))
	  (help-3 (concat "[u]ncapitalized insert into dict.  "
			  "Type 'x C-h d ispell-help' for more help")))
      (save-window-excursion
	(if ispell-help-in-bufferp
	    (progn
	      (ispell-overlay-window 4)
	      (switch-to-buffer (get-buffer-create "*Ispell Help*"))
	      (insert (concat help-1 "\n" help-2 "\n" help-3))
	      (sit-for 5)
	      (kill-buffer "*Ispell Help*"))
	  (unwind-protect
	      (progn
		(select-window (minibuffer-window))
		(erase-buffer)
		(message nil)
		;;(set-minibuffer-window (selected-window))
		(enlarge-window 2)
		(insert (concat help-1 "\n" help-2 "\n" help-3))
		(sit-for 5))
	    (erase-buffer)))))))


(defun lookup-words (word &optional lookup-dict)
  "Look up WORD in optional word-list dictionary LOOKUP-DICT.
A `*' serves as a wild card.  If no wild cards, `look' is used if it exists.
Otherwise the variable `ispell-grep-command' contains the command used to
search for the words (usually egrep).

Optional second argument contains the dictionary to use; the default is
`ispell-alternate-dictionary'."
  ;; We don't use the filter for this function, rather the result is written
  ;; into a buffer.  Hence there is no need to save the filter values.
  (if (null lookup-dict)
      (setq lookup-dict ispell-alternate-dictionary))

  (let* ((process-connection-type ispell-use-ptys-p)
	 (wild-p (string-match "\\*" word))
	 (look-p (and ispell-look-p	; Only use look for an exact match.
		      (or ispell-have-new-look (not wild-p))))
	 (ispell-grep-buffer (get-buffer-create "*Ispell-Temp*")) ; result buf
	 (prog (if look-p ispell-look-command ispell-grep-command))
	 (args (if look-p ispell-look-options ispell-grep-options))
	 status results loc)
    (unwind-protect
	(save-window-excursion
	  (message "Starting \"%s\" process..." (file-name-nondirectory prog))
	  (set-buffer ispell-grep-buffer)
	  (if look-p
	      nil
	    ;; convert * to .*
	    (insert "^" word "$")
	    (while (search-backward "*" nil t) (insert "."))
	    (setq word (buffer-string))
	    (erase-buffer))
	  (setq status (call-process prog nil t nil args word lookup-dict))
	  ;; grep returns status 1 and no output when word not found, which
	  ;; is a perfectly normal thing.
	  (if (stringp status)
	      (setq results (cons (format "error: %s exited with signal %s"
					  (file-name-nondirectory prog) status)
				  results))
	    ;; else collect words into `results' in FIFO order
	    (goto-char (point-max))
	    ;; assure we've ended with \n
	    (or (bobp) (= (preceding-char) ?\n) (insert ?\n))
	    (while (not (bobp))
	      (setq loc (point))
	      (forward-line -1)
	      (setq results (cons (buffer-substring-no-properties (point)
								  (1- loc))
				  results)))))
      ;; protected
      (kill-buffer ispell-grep-buffer)
      (if (and results (string-match ".+: " (car results)))
	  (error "%s error: %s" ispell-grep-command (car results))))
    results))


;;; "ispell-filter" is a list of output lines from the generating function.
;;;   Each full line (ending with \n) is a separate item on the list.
;;; "output" can contain multiple lines, part of a line, or both.
;;; "start" and "end" are used to keep bounds on lines when "output" contains
;;;   multiple lines.
;;; "ispell-filter-continue" is true when we have received only part of a
;;;   line as output from a generating function ("output" did not end with \n)
;;; THIS FUNCTION WILL FAIL IF THE PROCESS OUTPUT DOESN'T END WITH \n!
;;;   This is the case when a process dies or fails. The default behavior
;;;   in this case treats the next input received as fresh input.

(defun ispell-filter (process output)
  "Output filter function for ispell, grep, and look."
  (let ((start 0)
	(continue t)
	end)
    (while continue
      (setq end (string-match "\n" output start)) ; get text up to the newline.
      ;; If we get out of sync and ispell-filter-continue is asserted when we
      ;; are not continuing, treat the next item as a separate list.  When
      ;; ispell-filter-continue is asserted, ispell-filter *should* always be a
      ;; list!

      ;; Continue with same line (item)?
      (if (and ispell-filter-continue ispell-filter (listp ispell-filter))
	  ;; Yes.  Add it to the prev item
	  (setcar ispell-filter
		  (concat (car ispell-filter) (substring output start end)))
	;; No. This is a new line and item.
	(setq ispell-filter
	      (cons (substring output start end) ispell-filter)))
      (if (null end)
	  ;; We've completed reading the output, but didn't finish the line.
	  (setq ispell-filter-continue t continue nil)
	;; skip over newline, this line complete.
	(setq ispell-filter-continue nil end (1+ end))
	(if (= end (length output))	; No more lines in output
	    (setq continue nil)		;  so we can exit the filter.
	  (setq start end))))))		; else move start to next line of input


;;; This function destroys the mark location if it is in the word being
;;; highlighted.
(defun ispell-highlight-spelling-error-generic (start end &optional highlight
						      refresh)
  "Highlight the word from START to END with a kludge using `inverse-video'.
When the optional third arg HIGHLIGHT is set, the word is highlighted;
otherwise it is displayed normally.
Uses block cursor to highlight one character.
Optional REFRESH will unhighlighted then highlight, using block cursor
 highlighting when REFRESH is equal to `block'."
  (and (eq 'block ispell-highlight-p)
       (or (eq 'block refresh)
	   (setq start (1+ start))))	; On block non-refresh, inc start.
  (let ((modified (buffer-modified-p))	; don't allow this fn to modify buffer
	(buffer-read-only nil)		; Allow highlighting read-only buffers.
	(text (buffer-substring-no-properties start end)) ; Save hilight region
	(inhibit-quit t)		; inhibit interrupt processing here.
	(buffer-undo-list t))		; don't clutter the undo list.
    (goto-char end)
    (delete-region start end)
    (insert-char ?  (- end start))	; minimize amount of redisplay
    (sit-for 0)				; update display
    (if highlight (setq inverse-video (not inverse-video))) ; toggle video
    (delete-region start end)		; delete whitespace
    (insert text)			; insert text in inverse video.
    (sit-for 0)				; update display showing inverse video.
    (if (not highlight)
	(goto-char end)
      (setq inverse-video (not inverse-video)) ; toggle video
      (and (eq 'block ispell-highlight-p)
	   (goto-char (1- start))))	; use block cursor to "highlight" char
    (set-buffer-modified-p modified)	; don't modify if flag not set.
    (and refresh			; re-highlight
	 (ispell-highlight-spelling-error-generic
	  (if (eq 'block refresh) start (- start 2)) end t))))


(defun ispell-highlight-spelling-error-xemacs (start end &optional highlight)
  "Highlight the word from START to END using `isearch-highlight'.
When the optional third arg HIGHLIGHT is set, the word is highlighted,
otherwise it is displayed normally."
  (if highlight
      (isearch-highlight start end)
    (isearch-dehighlight t))
  ;;(sit-for 0)
  )


(defun ispell-highlight-spelling-error-overlay (start end &optional highlight)
  "Highlight the word from START to END using overlays.
When the optional third arg HIGHLIGHT is set, the word is highlighted
otherwise it is displayed normally.

The variable `ispell-highlight-face' selects the face to use for highlighting."
  (if highlight
      (progn
	(setq ispell-overlay (make-overlay start end))
	(overlay-put ispell-overlay 'face ispell-highlight-face))
    (delete-overlay ispell-overlay)))


(defun ispell-highlight-spelling-error (start end &optional highlight refresh)
  (cond
   (xemacsp
    (ispell-highlight-spelling-error-xemacs start end highlight))
   ((and (featurep 'faces)
	 (or (and (fboundp 'display-color-p) (display-color-p))
	     window-system))
    (ispell-highlight-spelling-error-overlay start end highlight))
   (t (ispell-highlight-spelling-error-generic start end highlight refresh))))

(defun ispell-adjusted-window-height (&optional window)
  "Like `window-height', adjusted to correct for the effect of tall mode-lines.
The value returned is actually the nominal number of text-lines in the
window plus 1.  On a terminal, this is the same value returned by
`window-height', but if the window has a mode-line is taller than a normal
text line, the returned value may be smaller than that from
`window-height'."
  (cond ((fboundp 'window-text-height)
	 (1+ (window-text-height window)))
	(ispell-graphic-p
	 (1- (window-height window)))
	(t
	 (window-height window))))

(defun ispell-overlay-window (height)
  "Create a window covering the top HEIGHT lines of the current window.
Ensure that the line above point is still visible but otherwise avoid
scrolling the current window.  Leave the new window selected."
  (save-excursion
    (let ((oldot (save-excursion (vertical-motion -1) (point)))
	  (top (save-excursion (move-to-window-line height) (point))))
      ;; If line above old point (line starting at oldot) would be
      ;; hidden by new window, scroll it to just below new win
      ;; otherwise set top line of other win so it doesn't scroll.
      (if (< oldot top) (setq top oldot))
      ;; if frame is unsplitable, temporarily disable that...
      (if (cdr (assq 'unsplittable (frame-parameters (selected-frame))))
	  (let ((frame (selected-frame)))
	    (modify-frame-parameters frame '((unsplittable . nil)))
	    (split-window nil height)
	    (modify-frame-parameters frame '((unsplittable . t))))
	(split-window nil height))
      (let ((deficit (- height (ispell-adjusted-window-height))))
	(when (> deficit 0)
	  ;; Number of lines the window is still too short.  We ensure that
	  ;; there are at least (1- HEIGHT) lines visible in the window.
	  (enlarge-window deficit)
	  (goto-char top)
	  (vertical-motion deficit)
	  (setq top (min (point) oldot))))
      (set-window-start (next-window) top))))


;;; Should we add a compound word match return value?
(defun ispell-parse-output (output &optional accept-list shift)
  "Parse the OUTPUT string from Ispell process and return:
1: t for an exact match.
2: A string containing the root word matched via suffix removal.
3: A list of possible correct spellings of the format:
   (\"ORIGINAL-WORD\" OFFSET MISS-LIST GUESS-LIST)
   ORIGINAL-WORD is a string of the possibly misspelled word.
   OFFSET is an integer giving the line offset of the word.
   MISS-LIST and GUESS-LIST are possibly null lists of guesses and misses.
4: nil when an error has occurred.

Optional second arg ACCEPT-LIST is list of words already accepted.
Optional third arg SHIFT is an offset to apply based on previous corrections."
  (cond
   ((string= output "") t)		; for startup with pipes...
   ((string= output "*") t)		; exact match
   ((string= output "-") t)		; compound word match
   ((string= (substring output 0 1) "+") ; found because of root word
    (substring output 2))		; return root word
   ((equal 0 (string-match "[\ra-zA-Z]" output))
    (ding)				; error message from ispell!
    (message (concat "Ispell error: " output))
    (sit-for 5)
    nil)
   (t					; need to process &, ?, and #'s
    (let ((type (substring output 0 1))	; &, ?, or #
	  (original-word (substring output 2 (string-match " " output 2)))
	  (cur-count 0)			; contains number of misses + guesses
	  count miss-list guess-list offset)
      (setq output (substring output (match-end 0))) ; skip over misspelling
      (if (string= type "#")
	  (setq count 0)		; no misses for type #
	(setq count (string-to-int output) ; get number of misses.
	      output (substring output (1+ (string-match " " output 1)))))
      (setq offset (string-to-int output))
      (if (string= type "#")		; No miss or guess list.
	  (setq output nil)
	(setq output (substring output (1+ (string-match " " output 1)))))
      (while output
	(let ((end (string-match ", \\|\\($\\)" output))) ; end of miss/guess.
	  (setq cur-count (1+ cur-count))
	  (if (> cur-count count)
	      (setq guess-list (cons (substring output 0 end) guess-list))
	    (setq miss-list (cons (substring output 0 end) miss-list)))
	  (if (match-end 1)		; True only when at end of line.
	      (setq output nil)		; no more misses or guesses
	    (setq output (substring output (+ end 2))))))
      ;; return results.  Accept word if it was already accepted.
      ;; adjust offset.
      (if (member original-word accept-list)
	  t
	(list original-word
	      (if (numberp shift) (+ shift offset) offset)
	      (nreverse miss-list) (nreverse guess-list)))))))


(defun ispell-process-status ()
  "Return the status of the Ispell process.
When asynchronous processes are not supported, `run' is always returned."
  (if ispell-async-processp
      (process-status ispell-process)
    (and ispell-process 'run)))


(defun ispell-start-process ()
  "Start the ispell process, with support for no asynchronous processes.
Keeps argument list for future ispell invocations for no async support."
  (let (args)
    ;; Local dictionary becomes the global dictionary in use.
    (if ispell-local-dictionary
	(setq ispell-dictionary ispell-local-dictionary))
    (setq args (ispell-get-ispell-args))
    (if ispell-dictionary		; use specified dictionary
	(setq args
	      (append (list "-d" ispell-dictionary) args)))
    (if ispell-personal-dictionary	; use specified pers dict
	(setq args
	      (append args
		      (list "-p"
			    (expand-file-name ispell-personal-dictionary)))))
    (setq args (append args ispell-extra-args))

    (if ispell-async-processp
	(let ((process-connection-type ispell-use-ptys-p))
	  (apply 'start-process
		 "ispell" nil ispell-program-name
		 "-a"			; accept single input lines
		 "-m"			; make root/affix combos not in dict
		 args))
      (setq ispell-cmd-args args
	    ispell-output-buffer (generate-new-buffer " *ispell-output*")
	    ispell-session-buffer (generate-new-buffer " *ispell-session*"))
      (ispell-send-string "\032\n")	; so Ispell prints version and exits
      t)))



(defun ispell-init-process ()
  "Check status of Ispell process and start if necessary."
  (if (and ispell-process
	   (eq (ispell-process-status) 'run)
	   ;; If we're using a personal dictionary, ensure
	   ;; we're in the same default directory!
	   (or (not ispell-personal-dictionary)
	       (equal ispell-process-directory default-directory)))
      (setq ispell-filter nil ispell-filter-continue nil)
    ;; may need to restart to select new personal dictionary.
    (ispell-kill-ispell t)
    (message "Starting new Ispell process...")
    (sit-for 0)
    (check-ispell-version)
    (setq ispell-process-directory default-directory
	  ispell-process (ispell-start-process)
	  ispell-filter nil
	  ispell-filter-continue nil)
    (if ispell-async-processp
	(set-process-filter ispell-process 'ispell-filter))
    ;; protect against bogus binding of `enable-multibyte-characters' in XEmacs
    (if (and (or xemacsp
		 (and (boundp 'enable-multibyte-characters)
		      enable-multibyte-characters))
	     (fboundp 'set-process-coding-system))
	(set-process-coding-system ispell-process (ispell-get-coding-system)
				   (ispell-get-coding-system)))
    ;; Get version ID line
    (ispell-accept-output 3)
    ;; get more output if filter empty?
    (if (null ispell-filter) (ispell-accept-output 3))
    (cond ((null ispell-filter)
	   (error "%s did not output version line" ispell-program-name))
	  ((and
	    (stringp (car ispell-filter))
	    (if (string-match "warning: " (car ispell-filter))
		(progn
		  (ispell-accept-output 3) ; was warn msg.
		  (stringp (car ispell-filter)))
	      (null (cdr ispell-filter)))
	    (string-match "^@(#) " (car ispell-filter)))
	   ;; got the version line as expected (we already know it's the right
	   ;; version, so don't bother checking again.)
	   nil)
	  (t
	   ;; Otherwise, it must be an error message.  Show the user.
	   ;; But first wait to see if some more output is going to arrive.
	   ;; Otherwise we get cool errors like "Can't open ".
	   (sleep-for 1)
	   (ispell-accept-output 3)
	   (error "%s" (mapconcat 'identity ispell-filter "\n"))))
    (setq ispell-filter nil)		; Discard version ID line
    (let ((extended-char-mode (ispell-get-extended-character-mode)))
      (if extended-char-mode		; ~ extended character mode
	  (ispell-send-string (concat extended-char-mode "\n"))))
    (if ispell-async-processp
	(process-kill-without-query ispell-process))))

;;;###autoload
(defun ispell-kill-ispell (&optional no-error)
  "Kill current Ispell process (so that you may start a fresh one).
With NO-ERROR, just return non-nil if there was no Ispell running."
  (interactive)
  (if (not (and ispell-process
		(eq (ispell-process-status) 'run)))
      (or no-error
	  (error "There is no ispell process running!"))
    (if ispell-async-processp
	(progn
	  (process-send-eof ispell-process)
	  (if (eq (ispell-process-status) 'run)
	      (ispell-accept-output 1))
	  (if (eq (ispell-process-status) 'run)
	      (kill-process ispell-process))
	  (while (not (or (eq (ispell-process-status) 'exit)
			  (eq (ispell-process-status) 'signal)))
	    (sleep-for 0.25)))
      ;; synchronous processes
      (ispell-send-string "\n")		; make sure side effects occurred.
      (kill-buffer ispell-output-buffer)
      (kill-buffer ispell-session-buffer)
      (setq ispell-output-buffer nil
	    ispell-session-buffer nil))
    (setq ispell-process nil)
    (message "Ispell process killed")
    nil))


;;; ispell-change-dictionary is set in some people's hooks.  Maybe this should
;;;  call ispell-init-process rather than wait for a spell checking command?

;;;###autoload
(defun ispell-change-dictionary (dict &optional arg)
  "Change `ispell-dictionary' (q.v.) to DICT and kill old Ispell process.
A new one will be started as soon as necessary.

By just answering RET you can find out what the current dictionary is.

With prefix argument, set the default dictionary."
  (interactive
   (list (completing-read
	  "Use new dictionary (RET for current, SPC to complete): "
	  (cons (cons "default" nil) ispell-dictionary-alist) nil t)
	 current-prefix-arg))
  (if (equal dict "default") (setq dict nil))
  ;; This relies on completing-read's bug of returning "" for no match
  (cond ((equal dict "")
	 (message "Using %s dictionary"
		  (or ispell-local-dictionary ispell-dictionary "default")))
	((and (equal dict ispell-dictionary)
	      (or (null ispell-local-dictionary)
		  (equal dict ispell-local-dictionary)))
	 ;; Specified dictionary is the default already.  No-op
	 (and (interactive-p)
	      (message "No change, using %s dictionary" (or dict "default"))))
	(t				; reset dictionary!
	 (if (assoc dict ispell-dictionary-alist)
	     (progn
	       (if (or arg (null dict))	; set default dictionary
		   (setq ispell-dictionary dict))
	       (if (null arg)		; set local dictionary
		   (setq ispell-local-dictionary dict)))
	   (error "Undefined dictionary: %s" dict))
	 (ispell-kill-ispell t)
	 (message "(Next %sIspell command will use %s dictionary)"
		  (cond ((equal ispell-local-dictionary ispell-dictionary)
			 "")
			(arg "global ")
			(t "local "))
		  (or (if (or (equal ispell-local-dictionary ispell-dictionary)
			      (null arg))
			  ispell-local-dictionary
			ispell-dictionary)
		      "default")))))


;;; Spelling of comments are checked when ispell-check-comments is non-nil.

;;;###autoload
(defun ispell-region (reg-start reg-end &optional recheckp shift)
  "Interactively check a region for spelling errors.
Return nil if spell session is quit,
 otherwise returns shift offset amount for last line processed."
  (interactive "r")			; Don't flag errors on read-only bufs.
  (if (not recheckp)
      (ispell-accept-buffer-local-defs)) ; set up dictionary, local words, etc.
  (unwind-protect
      (save-excursion
	(message "Spell checking %s using %s dictionary..."
		 (if (and (= reg-start (point-min)) (= reg-end (point-max)))
		     (buffer-name) "region")
		 (or ispell-dictionary "default"))
	;; Returns cursor to original location.
	(save-window-excursion
	  (goto-char reg-start)
	  (let ((transient-mark-mode)
		(case-fold-search case-fold-search)
		(skip-region-start (make-marker))
		(skip-regexp (ispell-begin-skip-region-regexp))
		(skip-alist ispell-skip-region-alist)
		key)
	    (if (eq ispell-parser 'tex)
		(setq case-fold-search nil
		      skip-alist
		      (append (car ispell-tex-skip-alists)
			      (car (cdr ispell-tex-skip-alists))
			      skip-alist)))
	    (let (message-log-max)
	      (message "searching for regions to skip"))
	    (if (re-search-forward skip-regexp reg-end t)
		(progn
		  (setq key (buffer-substring-no-properties
			     (match-beginning 0) (match-end 0)))
		  (set-marker skip-region-start (- (point) (length key)))
		  (goto-char reg-start)))
	    (let (message-log-max)
	      (message "Continuing spelling check using %s dictionary..."
		       (or ispell-dictionary "default")))
	    (set-marker ispell-region-end reg-end)
	    (while (and (not ispell-quit)
			(< (point) ispell-region-end))
	      ;; spell-check region with skipping
	      (if (and (marker-position skip-region-start)
		       (<= skip-region-start (point)))
		  (progn
		    (ispell-skip-region key skip-alist) ; moves pt past region.
		    (setq reg-start (point))
		    (if (and (< reg-start ispell-region-end)
			     (re-search-forward skip-regexp
						ispell-region-end t))
			(progn
			  (setq key (buffer-substring-no-properties
				     (car (match-data))
				     (car (cdr (match-data)))))
			  (set-marker skip-region-start
				      (- (point) (length key)))
			  (goto-char reg-start))
		      (set-marker skip-region-start nil))))
	      (setq reg-end (if (marker-position skip-region-start)
				(min skip-region-start ispell-region-end)
			      (marker-position ispell-region-end)))
	      (let* ((start (point))
		     (end (save-excursion (end-of-line) (min (point) reg-end)))
		     (string (ispell-get-line start end reg-end)))
		(setq end (point))	; "end" tracks region retrieved.
		(if string		; there is something to spell check!
		    ;; (special start end)
		    (setq shift (ispell-process-line string
						     (and recheckp shift))))
		(goto-char end)))))
	(if ispell-quit
	    nil
	  (or shift 0)))
    ;; protected
    (if (and (not (and recheckp ispell-keep-choices-win))
	     (get-buffer ispell-choices-buffer))
	(kill-buffer ispell-choices-buffer))
    (if ispell-quit
	(progn
	  ;; preserve or clear the region for ispell-continue.
	  (if (not (numberp ispell-quit))
	      (set-marker ispell-region-end nil)
	    ;; Ispell-continue enabled - ispell-region-end is set.
	    (goto-char ispell-quit))
	  ;; Check for aborting
	  (if (and ispell-checking-message (numberp ispell-quit))
	      (progn
		(setq ispell-quit nil)
		(error "Message send aborted")))
	  (if (not recheckp) (setq ispell-quit nil)))
      (if (not recheckp) (set-marker ispell-region-end nil))
      ;; Only save if successful exit.
      (ispell-pdict-save ispell-silently-savep)
      (message "Spell-checking done"))))


;;; Creates the regexp for skipping a region.
;;; Makes the skip-regexp local for tex buffers adding in the
;;; tex expressions to skip as well.
;;; Call AFTER ispell-buffer-local-parsing.
(defun ispell-begin-skip-region-regexp ()
  (let ((skip-regexp (ispell-begin-skip-region)))
    (if (and (null ispell-check-comments) comment-start)
	(setq skip-regexp (concat (regexp-quote comment-start) "\\|"
				  skip-regexp)))
    (if (and (eq 'exclusive ispell-check-comments) comment-start)
	(setq skip-regexp (concat (if (string= "" comment-end) "^"
				    (regexp-quote comment-end))
				  "\\|" skip-regexp)))
    (if ispell-skip-tib
	(setq skip-regexp (concat ispell-tib-ref-beginning "\\|" skip-regexp)))
    (if ispell-skip-html
	(setq skip-regexp (concat "<[cC][oO][dD][eE]\\>[^>]*>" "\\|"
				  "<[sS][cC][rR][iI][pP][tT]\\>[^>]*>" "\\|"
				  "<[aA][pP][pP][lL][eE][tT]\\>[^>]*>" "\\|"
				  "<[vV][eE][rR][bB]\\>[^>]*>" "\\|"
				  ;; "<[tT][tT]\\>[^>]*>" "\\|"
				  "<[tT][tT]/" "\\|"
				  "<" "\\|"
				  "&" "\\|"
				  skip-regexp)))
    (if (eq ispell-parser 'tex)
	(setq skip-regexp (concat (ispell-begin-tex-skip-regexp) "\\|"
				  skip-regexp)))
    skip-regexp))


(defun ispell-begin-tex-skip-regexp ()
  "Regular expression of tex commands to skip.
Generated from `ispell-tex-skip-alists'."
  (concat
   (mapconcat (function (lambda (lst) (car lst)))
	      (car ispell-tex-skip-alists)
	      "\\|")
   "\\|"
   (mapconcat (function (lambda (lst)
			  (concat "\\\\begin[ \t\n]*{[ \t\n]*"
				  (car lst)
				  "[ \t\n]*}")))
	      (car (cdr ispell-tex-skip-alists))
	      "\\|")))


(defun ispell-begin-skip-region ()
  "Regular expression of regions to skip for all buffers.
Each selection should be a key of `ispell-skip-region-alist';
otherwise, the current line is skipped."
  (mapconcat (function (lambda (lst) (if (stringp (car lst)) (car lst)
					(eval (car lst)))))
	     ispell-skip-region-alist
	     "\\|"))


(defun ispell-tex-arg-end (&optional arg)
  (condition-case nil
      (progn
	(while (looking-at "[ \t\n]*\\[") (forward-sexp))
	(forward-sexp (or arg 1)))
    (error
     (message "error skipping s-expressions at point %d." (point))
     (beep)
     (sit-for 2))))


;;; Skips to region-end from point, or a single line.
;;; Places point at end of region skipped.
(defun ispell-skip-region (key alist)
  ;; move over key to begin checking.
  (forward-char (length key))
  (let ((start (point))
	alist-key null-skip)
    (cond
     ;; what about quoted comment, or comment inside strings?
     ((and (null ispell-check-comments) comment-start
	   (string= key comment-start))
      (if (string= "" comment-end)
	  (forward-line)
	(search-forward comment-end ispell-region-end t)))
     ((and (eq 'exclusive ispell-check-comments) comment-start
	   (string= key comment-end))
      (search-forward comment-start ispell-region-end :end))
     ((and ispell-skip-tib (string-match ispell-tib-ref-beginning key))
      (re-search-forward ispell-tib-ref-end ispell-region-end t))
     ((and ispell-skip-html (string-match "</" key))
      (search-forward ">" ispell-region-end t))
     ((and ispell-skip-html (string-match "<[cC][oO][dD][eE]\\>[^>]*>" key))
      (search-forward-regexp "</[cC][oO][dD][eE]>" ispell-region-end t))
     ((and ispell-skip-html
	   (string-match "<[sS][cC][rR][iI][pP][tT]\\>[^>]*>" key))
      (search-forward-regexp "</[sS][cC][rR][iI][pP][tT]>" ispell-region-end t))
     ((and ispell-skip-html
	   (string-match "<[aA][pP][pP][lL][eE][tT]\\>[^>]*>" key))
      (search-forward-regexp "</[aA][pP][pP][lL][eE][tT]>" ispell-region-end t))
     ((and ispell-skip-html (string-match "<[vV][eE][rR][bB]\\>[^>]*>" key))
      (search-forward-regexp "</[vV][eE][rR][bB]>" ispell-region-end t))
     ;;((and ispell-skip-html (string-match "<[tT][tT]\\>[^>]*>" key))
     ;; (search-forward-regexp "</[tT][tT]>" ispell-region-end t))
     ((and ispell-skip-html (string-match "<[tT][tT]/" key))
      (search-forward "/" ispell-region-end t))
     ((and ispell-skip-html (string-match "<" key))
      (search-forward ">" ispell-region-end t))
     ((and ispell-skip-html (string-match "&" key))
      (search-forward-regexp "[; \t\n]" ispell-region-end t))
     ;; markings from alist
     (t
      (while alist
	(setq alist-key (eval (car (car alist))))
	(if (string-match alist-key key)
	    (progn
	      (setq alist (cdr (car alist)))
	      (cond
	       ((null alist) (setq null-skip t)) ; done!  Just skip key.
	       ((not (consp alist))
		;; Search past end of spell region to find this region end.
		(re-search-forward (eval alist) (point-max) t))
	       ((consp alist)
		(if (stringp alist)
		    (re-search-forward alist (point-max) t)
		  (setq null-skip t)	; error handling in functions!
		  (if (consp (cdr alist))
		      (apply (car alist) (cdr alist))
		    (funcall (car alist))))))
	      (setq alist nil))
	  (setq alist (cdr alist))))))
    (if (and (= start (point)) (null null-skip))
	(progn
	  (message "Matching region end for `%s' point %d not found"
		   key (point))
	  (beep)
	  (sit-for 2)))))


;;; Grab the next line of data.
;;; Returns a string with the line data
(defun ispell-get-line (start end reg-end)
  (let ((ispell-casechars (ispell-get-casechars))
	string)
    (cond				; LOOK AT THIS LINE AND SKIP OR PROCESS
     ((eolp)				; END OF LINE, just go to next line.
      (forward-line))
     ;;((looking-at "[-#@*+!%~^]")	; SKIP SPECIAL ISPELL CHARACTERS
     ;; (forward-char 1))		; not needed as quoted below.
     ((or (re-search-forward ispell-casechars end t) ; TEXT EXISTS
	  (re-search-forward "[][()${}]" end t)) ; or MATH COMMANDS
      (setq string (concat "^" (buffer-substring-no-properties start end)
			   "\n"))
      (goto-char end))
     (t (goto-char end)))		; EMPTY LINE, skip it.
    string))


;;; Avoid error messages when compiling for these dynamic variables.
(eval-when-compile
  (defvar start)
  (defvar end))

(defun ispell-process-line (string shift)
  "Sends a LINE of text to ispell and processes the result.
This will modify the buffer for spelling errors.
Requires variables START and END to be defined in its lexical scope.
Returns the sum shift due to changes in word replacements."
  ;;(declare special start end)
  (let (poss accept-list)
    (if (not (numberp shift))
	(setq shift 0))
    ;; send string to spell process and get input.
    (ispell-send-string string)
    (while (progn
	     (ispell-accept-output)
	     ;; Last item of output contains a blank line.
	     (not (string= "" (car ispell-filter)))))
    ;; parse all inputs from the stream one word at a time.
    ;; Place in FIFO order and remove the blank item.
    (setq ispell-filter (nreverse (cdr ispell-filter)))
    (while (and (not ispell-quit) ispell-filter)
      ;; get next word, accounting for accepted words and start shifts
      (setq poss (ispell-parse-output (car ispell-filter)
				      accept-list shift))
      (if (and poss (listp poss))	; spelling error occurred.
	  ;; Whenever we have misspellings, we can change
	  ;; the buffer.  Keep boundaries as markers.
	  ;; Markers can move with highlighting!  This destroys
	  ;; end of region markers line-end and ispell-region-end
	  (let ((word-start
		 (copy-marker (+ start ispell-offset (car (cdr poss)))))
		(word-len (length (car poss)))
		(line-end (copy-marker end))
		(line-start (copy-marker start))
		recheck-region replace)
	    (goto-char word-start)
	    ;; Adjust the horizontal scroll & point
	    (ispell-horiz-scroll)
	    (goto-char (+ word-len word-start))
	    (ispell-horiz-scroll)
	    (goto-char word-start)
	    (ispell-horiz-scroll)

	    ;; Alignment cannot be tracked and this error will occur when
	    ;; `query-replace' makes multiple corrections on the starting line.
	    (if (/= (+ word-len (point))
		    (progn
		      ;; NB: Search can fail with Mule character sets that don't
		      ;;  display properly.  Ignore the error in this case?
		      (search-forward (car poss) (+ word-len (point)) t)
		      (point)))
		;; This occurs due to filter pipe problems
		(error (concat "Ispell misalignment: word "
			       "`%s' point %d; probably incompatible versions")
		       (car poss) (marker-position word-start)))
	    ;; ispell-cmd-loop can go recursive & change buffer
	    (if ispell-keep-choices-win
		(setq replace (ispell-command-loop
			       (car (cdr (cdr poss)))
			       (car (cdr (cdr (cdr poss))))
			       (car poss) (marker-position word-start)
			       (+ word-len (marker-position word-start))))
	      (save-window-excursion
		(setq replace (ispell-command-loop
			       (car (cdr (cdr poss)))
			       (car (cdr (cdr (cdr poss))))
			       (car poss) (marker-position word-start)
			       (+ word-len (marker-position word-start))))))

	    (goto-char word-start)
	    ;; Recheck when query replace edit changes misspelled word.
	    ;; Error in tex mode when a potential math mode change exists.
	    (if (and replace (listp replace) (= 2 (length replace)))
		(if (and (eq ispell-parser 'tex)
			 (string-match "[\\\\][]()[]\\|\\\\begin\\|\\$"
				       (regexp-quote string)))
		    (error
		     "Don't start query replace on a line with math characters"
		     )
		  (set-marker line-end (point))
		  (setq ispell-filter nil
			recheck-region t)))

	    ;; insert correction if needed
	    (cond
	     ((or (null replace)
		  (equal 0 replace))	; ACCEPT/INSERT
	      (if (equal 0 replace)	; BUFFER-LOCAL DICT ADD
		  (ispell-add-per-file-word-list (car poss)))
	      ;; do not recheck accepted word on this line
	      (setq accept-list (cons (car poss) accept-list)))
	     (t				; replacement word selected or entered
	      (delete-region (point) (+ word-len (point)))
	      (if (not (listp replace))
		  (progn
		    (insert replace)	; insert dictionary word
		    (setq accept-list (cons replace accept-list)))
		(let ((replace-word (car replace)))
		  ;; Recheck hand entered replacement word
		  (insert replace-word)
		  (if (car (cdr replace))
		      (save-window-excursion
			(delete-other-windows) ; to correctly show help.
			;; Assume case-replace &
			;; case-fold-search correct?
			(query-replace (car poss) (car replace) t)))
		  (goto-char word-start)
		  ;; do not recheck if already accepted
		  (if (member replace-word accept-list)
		      (setq accept-list (cons replace-word accept-list)
			    replace replace-word)
		    (let ((region-end (copy-marker ispell-region-end)))
		      (setq recheck-region ispell-filter
			    ispell-filter nil ; save filter
			    shift 0	; already accounted
			    shift (ispell-region
				  word-start
				  (+ word-start (length replace-word))
				  t shift))
		      (if (null shift)	; quitting check.
			  (setq shift 0))
		      (set-marker ispell-region-end region-end)
		      (set-marker region-end nil)
		      (setq ispell-filter recheck-region
			    recheck-region nil
			    replace replace-word)))))

	      (setq shift (+ shift (- (length replace) word-len)))

	      ;; Move line-start across word...
	      ;; new shift function does this now...
	      ;;(set-marker line-start (+ line-start
	      ;;			(- (length replace)
	      ;;			   (length (car poss)))))
	      ))
	    (if (not ispell-quit)
		(let (message-log-max)
		  (message "Continuing spelling check using %s dictionary..."
			   (or ispell-dictionary "default"))))
	    (sit-for 0)
	    (setq start (marker-position line-start)
		  end (marker-position line-end))
	    ;; Adjust markers when end of region lost from highlighting.
	    (if (and (not recheck-region) (< end (+ word-start word-len)))
		(setq end (+ word-start word-len)))
	    (if (= word-start ispell-region-end)
		(set-marker ispell-region-end (+ word-start word-len)))
	    ;; going out of scope - unneeded
	    (set-marker line-start nil)
	    (set-marker word-start nil)
	    (set-marker line-end nil)))
      ;; finished with misspelling!
      (setq ispell-filter (cdr ispell-filter)))
    shift))


;;;###autoload
(defun ispell-comments-and-strings ()
  "Check comments and strings in the current buffer for spelling errors."
  (interactive)
  (goto-char (point-min))
  (let (state done)
    (while (not done)
      (setq done t)
      (setq state (parse-partial-sexp (point) (point-max)
				      nil nil state 'syntax-table))
      (if (or (nth 3 state) (nth 4 state))
	  (let ((start (point)))
	    (setq state (parse-partial-sexp start (point-max)
					    nil nil state 'syntax-table))
	    (if (or (nth 3 state) (nth 4 state))
		(error "Unterminated string or comment"))
	    (save-excursion
	      (setq done (not (ispell-region start (point))))))))))


;;;###autoload
(defun ispell-buffer ()
  "Check the current buffer for spelling errors interactively."
  (interactive)
  (ispell-region (point-min) (point-max)))


;;;###autoload
(defun ispell-continue ()
  "Continue a halted spelling session beginning with the current word."
  (interactive)
  (if (not (marker-position ispell-region-end))
      (message "No session to continue.  Use 'X' command when checking!")
    (if (not (equal (marker-buffer ispell-region-end) (current-buffer)))
	(message "Must continue ispell from buffer %s"
		 (buffer-name (marker-buffer ispell-region-end)))
      (ispell-region
       ;; find beginning of current word:
       (car (cdr (ispell-get-word t)))
       (marker-position ispell-region-end)))))


;;; Horizontal scrolling
(defun ispell-horiz-scroll ()
  "Places point within the horizontal visibility of its window area."
  (if truncate-lines			; display truncating lines?
      ;; See if display needs to be scrolled.
      (let ((column (- (current-column) (max (window-hscroll) 1))))
	(if (and (< column 0) (> (window-hscroll) 0))
	    (scroll-right (max (- column) 10))
	  (if (>= column (- (window-width) 2))
	      (scroll-left (max (- column (window-width) -3) 10)))))))


;;; Interactive word completion.
;;; Forces "previous-word" processing.  Do we want to make this selectable?

;;;###autoload
(defun ispell-complete-word (&optional interior-frag)
  "Try to complete the word before or under point (see `lookup-words').
If optional INTERIOR-FRAG is non-nil then the word may be a character
sequence inside of a word.

Standard ispell choices are then available."
  (interactive "P")
  (let ((cursor-location (point))
	(case-fold-search-val case-fold-search)
	(word (ispell-get-word nil "\\*")) ; force "previous-word" processing.
	start end possibilities replacement)
    (setq start (car (cdr word))
	  end (car (cdr (cdr word)))
	  word (car word)
	  possibilities
	  (or (string= word "")		; Will give you every word
	      (lookup-words (concat (and interior-frag "*") word
				    (if (or interior-frag (null ispell-look-p))
					"*"))
			    ispell-complete-word-dict)))
    (cond ((eq possibilities t)
	   (message "No word to complete"))
	  ((null possibilities)
	   (message "No match for \"%s\"" word))
	  (t				; There is a modification...
	   (setq case-fold-search nil)	; Try and respect case of word.
	   (cond
	    ((string-equal (upcase word) word)
	     (setq possibilities (mapcar 'upcase possibilities)))
	    ((string-equal (upcase (substring word 0 1)) (substring word 0 1))
             (setq possibilities (mapcar (function
                                          (lambda (pos)
                                            (if (string-equal
						 (substring word 0 1)
						 (substring pos 0 1))
						pos
                                              (capitalize pos))))
                                         possibilities))))
	   (setq case-fold-search case-fold-search-val)
	   (save-window-excursion
	     (setq replacement
		   (ispell-command-loop possibilities nil word start end)))
	   (cond
	    ((equal 0 replacement)	; BUFFER-LOCAL ADDITION
	     (ispell-add-per-file-word-list word))
	    (replacement		; REPLACEMENT WORD
	     (delete-region start end)
	     (setq word (if (atom replacement) replacement (car replacement))
		   cursor-location (+ (- (length word) (- end start))
				      cursor-location))
	     (insert word)
	     (if (not (atom replacement)) ; recheck spelling of replacement.
		 (progn
		   (goto-char cursor-location)
		   (ispell-word nil t)))))
	   (if (get-buffer ispell-choices-buffer)
	       (kill-buffer ispell-choices-buffer))))
    (ispell-pdict-save ispell-silently-savep)
    (goto-char cursor-location)))


;;;###autoload
(defun ispell-complete-word-interior-frag ()
  "Completes word matching character sequence inside a word."
  (interactive)
  (ispell-complete-word t))


;;;###autoload
(defun ispell ()
  "Interactively check a region or buffer for spelling errors.
If `transient-mark-mode' is on, and a region is active, spell-check
that region.  Otherwise spell-check the buffer.

Ispell dictionaries are not distributed with Emacs.  If you are
looking for a dictionary, please see the distribution of the GNU ispell
program, or do an Internet search; there are various dictionaries
available on the net."
  (interactive)
  (if (and transient-mark-mode mark-active)
      (ispell-region (region-beginning) (region-end))
    (ispell-buffer)))


;;; **********************************************************************
;;; 			Ispell Minor Mode
;;; **********************************************************************

(defvar ispell-minor-mode nil
  "Non-nil if Ispell minor mode is enabled.")
;; Variable indicating that ispell minor mode is active.
(make-variable-buffer-local 'ispell-minor-mode)

(or (assq 'ispell-minor-mode minor-mode-alist)
    (setq minor-mode-alist
          (cons '(ispell-minor-mode " Spell") minor-mode-alist)))

(defvar ispell-minor-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map " " 'ispell-minor-check)
    (define-key map "\r" 'ispell-minor-check)
    map)
  "Keymap used for Ispell minor mode.")

(or (not (boundp 'minor-mode-map-alist))
    (assoc 'ispell-minor-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
          (cons (cons 'ispell-minor-mode ispell-minor-keymap)
                minor-mode-map-alist)))

;;;###autoload
(defun ispell-minor-mode (&optional arg)
  "Toggle Ispell minor mode.
With prefix arg, turn Ispell minor mode on iff arg is positive.

In Ispell minor mode, pressing SPC or RET
warns you if the previous word is incorrectly spelled.

All the buffer-local variables and dictionaries are ignored -- to read
them into the running ispell process, type \\[ispell-word] SPC."
  (interactive "P")
  (setq ispell-minor-mode
	(not (or (and (null arg) ispell-minor-mode)
		 (<= (prefix-numeric-value arg) 0))))
  (force-mode-line-update))

(defun ispell-minor-check ()
  "Check previous word then continue with the normal binding of this key.
Don't check previous word when character before point is a space or newline.
Don't read buffer-local settings or word lists."
  (interactive "*")
  (let ((ispell-minor-mode nil)
	(ispell-check-only t)
	(last-char (char-after (1- (point)))))
    (command-execute (key-binding (this-command-keys)))
    (if (not (or (eq last-char ?\ ) (eq last-char ?\n)
		 (and ispell-skip-html (eq last-char ?>))
		 (and ispell-skip-html (eq last-char ?\;))))
	(ispell-word nil t))))


;;; **********************************************************************
;;; 			Ispell Message
;;; **********************************************************************
;;; Original from D. Quinlan, E. Bradford, A. Albert, and M. Ernst


(defvar ispell-message-text-end
  (mapconcat (function identity)
	     '(
	       ;; Don't spell check signatures
	       "^-- $"
	       ;; Matches postscript files.
	       "^%!PS-Adobe-[123].0"
	       ;; Matches uuencoded text
	       "^begin [0-9][0-9][0-9] .*\nM.*\nM.*\nM"
	       ;; Matches shell files (especially auto-decoding)
	       "^#! /bin/[ck]?sh"
	       ;; Matches context difference listing
	       "\\(\\(^cd .*\n\\)?diff -c .*\\)?\n\\*\\*\\* .*\n--- .*\n\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*"
	       ;; Matches unidiff difference listing
	       "\\(diff -u .*\\)?\n--- .*\n\\+\\+\\+ .*\n@@ [-+][0-9]+,[0-9]+ [-+][0-9]+,[0-9]+ @@\n"
	       ;; Matches reporter.el bug report
	       "^current state:\n==============\n"
	       ;; Matches commonly used "cut" boundaries
	       "^\\(- \\)?[-=_]+\\s ?\\(cut here\\|Environment Follows\\)")
	     "\\|")
  "*End of text which will be checked in `ispell-message'.
If it is a string, limit at first occurrence of that regular expression.
Otherwise, it must be a function which is called to get the limit.")



;;;###autoload
(defun ispell-message ()
  "Check the spelling of a mail message or news post.
Don't check spelling of message headers except the Subject field.
Don't check included messages.

To abort spell checking of a message region and send the message anyway,
use the `x' command.  (Any subsequent regions will be checked.)
The `X' command aborts the message send so that you can edit the buffer.

To spell-check whenever a message is sent, include the appropriate lines
in your .emacs file:
   (add-hook 'message-send-hook 'ispell-message)  ;; GNUS 5
   (add-hook 'news-inews-hook 'ispell-message)    ;; GNUS 4
   (add-hook 'mail-send-hook  'ispell-message)
   (add-hook 'mh-before-send-letter-hook 'ispell-message)

You can bind this to the key C-c i in GNUS or mail by adding to
`news-reply-mode-hook' or `mail-mode-hook' the following lambda expression:
   (function (lambda () (local-set-key \"\\C-ci\" 'ispell-message)))"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let* (
	   ;; Nil when message came from outside (eg calling emacs as editor)
	   ;; Non-nil marker of end of headers.
	   (internal-messagep
	    (re-search-forward
	     (concat "^" (regexp-quote mail-header-separator) "$") nil t))
	   (end-of-headers		; Start of body.
	    (copy-marker
	     (or internal-messagep
		 (re-search-forward "^$" nil t)
		 (point-min))))
	   (limit (copy-marker		; End of region we will spell check.
		   (cond
		    ((not ispell-message-text-end) (point-max))
		    ((char-or-string-p ispell-message-text-end)
		     (if (re-search-forward ispell-message-text-end nil t)
			 (match-beginning 0)
		       (point-max)))
		    (t (min (point-max) (funcall ispell-message-text-end))))))
	   (default-prefix   ; Vanilla cite prefix (just used for cite-regexp)
	     (if (and (boundp 'mail-yank-prefix) mail-yank-prefix)
		 (ispell-non-empty-string mail-yank-prefix)
	       "   \\|\t"))
	   (cite-regexp			;Prefix of quoted text
	    (cond
	     ((functionp 'sc-cite-regexp) ; sc 3.0
	      (concat "\\(" (sc-cite-regexp) "\\)" "\\|"
		      (ispell-non-empty-string sc-reference-tag-string)))
	     ((boundp 'sc-cite-regexp)	; sc 2.3
	      (concat "\\(" sc-cite-regexp "\\)" "\\|"
		      (ispell-non-empty-string sc-reference-tag-string)))
	     ((or (equal major-mode 'news-reply-mode) ;GNUS 4 & below
		  (equal major-mode 'message-mode))   ;GNUS 5
	      (concat "In article <" "\\|"
		      "[^,;&+=\n]+ <[^,;&+=]+> writes:" "\\|"
		      message-yank-prefix "\\|"
		      default-prefix))
	     ((equal major-mode 'mh-letter-mode) ; mh mail message
	      (concat "[^,;&+=\n]+ writes:" "\\|"
		      (ispell-non-empty-string mh-ins-buf-prefix)))
	     ((not internal-messagep)	; Assume nn sent us this message.
	      (concat "In [a-zA-Z.]+ you write:" "\\|"
		      "In <[^,;&+=]+> [^,;&+=]+ writes:" "\\|"
		      " *> *"))
	     ((boundp 'vm-included-text-prefix) ; VM mail message
	      (concat "[^,;&+=\n]+ writes:" "\\|"
		      (ispell-non-empty-string vm-included-text-prefix)))
	     (t default-prefix)))
	   (ispell-skip-region-alist
	    (cons (list (concat "^\\(" cite-regexp "\\)")
			(function forward-line))
		  ispell-skip-region-alist))
	   (old-case-fold-search case-fold-search)
	   (dictionary-alist ispell-message-dictionary-alist)
	   (ispell-checking-message t))

      ;; Select dictionary for message
      (or (local-variable-p 'ispell-local-dictionary (current-buffer))
	  (while dictionary-alist
	    (goto-char (point-min))
	    (if (re-search-forward (car (car dictionary-alist))
				   end-of-headers t)
		(setq ispell-local-dictionary (cdr (car dictionary-alist))
		      dictionary-alist nil)
	      (setq dictionary-alist (cdr dictionary-alist)))))

      (unwind-protect
	  (progn
	    ;; Spell check any original Subject:
	    (goto-char (point-min))
	    (setq case-fold-search t)
	    (if (re-search-forward "^Subject: *" end-of-headers t)
		(progn
		  (goto-char (match-end 0))
		  (if (and (not (looking-at ".*Re\\>"))
			   (not (looking-at "\\[")))
		      (progn
			(setq case-fold-search old-case-fold-search)
			(ispell-region (point)
				       (progn ;Tab-initiated continuation lns.
					 (end-of-line)
					 (while (looking-at "\n[ \t]")
					   (end-of-line 2))
					 (point)))))))
	    (setq case-fold-search old-case-fold-search)
	    (goto-char end-of-headers)
	    (forward-line 1)
	    (ispell-region (point) limit))
	(set-marker end-of-headers nil)
	(set-marker limit nil)))))


(defun ispell-non-empty-string (string)
  (if (or (not string) (string-equal string ""))
      "\\'\\`" ; An unmatchable string if string is null.
    (regexp-quote string)))


;;; **********************************************************************
;;; 			Buffer Local Functions
;;; **********************************************************************


(defun ispell-accept-buffer-local-defs ()
  "Load all buffer-local information, restarting Ispell when necessary."
  (ispell-buffer-local-dict)		; May kill ispell-process.
  (ispell-buffer-local-words)		; Will initialize ispell-process.
  (ispell-buffer-local-parsing))


(defun ispell-buffer-local-parsing ()
  "Place Ispell into parsing mode for this buffer.
Overrides the default parsing mode.
Includes Latex/Nroff modes and extended character mode."
  ;; (ispell-init-process) must already be called.
  (ispell-send-string "!\n")		; Put process in terse mode.
  ;; We assume all major modes with "tex-mode" in them should use latex parsing
  ;; When exclusively checking comments, set to raw text mode (nroff).
  (if (and (not (eq 'exclusive ispell-check-comments))
	   (or (and (eq ispell-parser 'use-mode-name)
		    (string-match "[Tt][Ee][Xx]-mode"
				  (symbol-name major-mode)))
	       (eq ispell-parser 'tex)))
      (progn
	(ispell-send-string "+\n")	; set ispell mode to tex
	(if (not (eq ispell-parser 'tex))
	    (set (make-local-variable 'ispell-parser) 'tex)))
    (ispell-send-string "-\n"))		; set mode to normal (nroff)
  ;; If needed, test for SGML & HTML modes and set a buffer local nil/t value.
  (if (and ispell-skip-html (not (eq ispell-skip-html t)))
      (set (make-local-variable 'ispell-skip-html)
	   (not (null (string-match "sgml\\|html\\|xml"
				    (downcase (symbol-name major-mode)))))))
  ;; Set default extended character mode for given buffer, if any.
  (let ((extended-char-mode (ispell-get-extended-character-mode)))
    (if extended-char-mode
	(ispell-send-string (concat extended-char-mode "\n"))))
  ;; Set buffer-local parsing mode and extended character mode, if specified.
  (save-excursion
    (goto-char (point-max))
    ;; Uses last occurrence of ispell-parsing-keyword
    (if (search-backward ispell-parsing-keyword nil t)
	(let ((end (save-excursion (end-of-line) (point)))
	      string)
	  (search-forward ispell-parsing-keyword)
	  (while (re-search-forward " *\\([^ \"]+\\)" end t)
	    ;; space separated definitions.
	    (setq string (downcase (buffer-substring-no-properties
				    (match-beginning 1) (match-end 1))))
	    (cond ((and (string-match "latex-mode" string)
			(not (eq 'exclusive ispell-check-comments)))
		   (ispell-send-string "+\n~tex\n"))
		  ((string-match "nroff-mode" string)
		   (ispell-send-string "-\n~nroff\n"))
		  ((string-match "~" string) ; Set extended character mode.
		   (ispell-send-string (concat string "\n")))
		  (t (message "Invalid Ispell Parsing argument!")
		     (sit-for 2))))))))


;;; Can kill the current ispell process

(defun ispell-buffer-local-dict ()
  "Initializes local dictionary and local personal dictionary.
When a dictionary is defined in the buffer (see variable
`ispell-dictionary-keyword'), it will override the local setting
from \\[ispell-change-dictionary].
Both should not be used to define a buffer-local dictionary."
  (save-excursion
    (goto-char (point-min))
    (let (end)
      ;; Override the local variable definition.
      ;; Uses last occurrence of ispell-dictionary-keyword.
      (goto-char (point-max))
      (if (search-backward ispell-dictionary-keyword nil t)
	  (progn
	    (search-forward ispell-dictionary-keyword)
	    (setq end (save-excursion (end-of-line) (point)))
	    (if (re-search-forward " *\\([^ \"]+\\)" end t)
		(setq ispell-local-dictionary
		      (buffer-substring-no-properties (match-beginning 1)
						      (match-end 1))))))
      (goto-char (point-max))
      (if (search-backward ispell-pdict-keyword nil t)
	  (progn
	    (search-forward ispell-pdict-keyword)
	    (setq end (save-excursion (end-of-line) (point)))
	    (if (re-search-forward " *\\([^ \"]+\\)" end t)
		(setq ispell-local-pdict
		      (buffer-substring-no-properties (match-beginning 1)
						      (match-end 1))))))))
  ;; Reload if new personal dictionary defined.
  (if (and ispell-local-pdict
	   (not (equal ispell-local-pdict ispell-personal-dictionary)))
      (progn
	(ispell-kill-ispell t)
	(setq ispell-personal-dictionary ispell-local-pdict)))
  ;; Reload if new dictionary defined.
  (if (and ispell-local-dictionary
	   (not (equal ispell-local-dictionary ispell-dictionary)))
      (ispell-change-dictionary ispell-local-dictionary)))


(defun ispell-buffer-local-words ()
  "Loads the buffer-local dictionary in the current buffer."
  (if (and ispell-buffer-local-name
	   (not (equal ispell-buffer-local-name (buffer-name))))
      (progn
	(ispell-kill-ispell t)
	(setq ispell-buffer-local-name nil)))
  (ispell-init-process)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward ispell-words-keyword nil t)
      (or ispell-buffer-local-name
	  (setq ispell-buffer-local-name (buffer-name)))
      (let ((end (save-excursion (end-of-line) (point)))
	    (ispell-casechars (ispell-get-casechars))
	    string)
	;; buffer-local words separated by a space, and can contain
	;; any character other than a space.  Not rigorous enough.
	(while (re-search-forward " *\\([^ ]+\\)" end t)
	  (setq string (buffer-substring-no-properties (match-beginning 1)
						       (match-end 1)))
	  ;; This can fail when string contains a word with illegal chars.
	  ;; Error handling needs to be added between ispell and emacs.
	  (if (and (< 1 (length string))
		   (equal 0 (string-match ispell-casechars string)))
	      (ispell-send-string (concat "@" string "\n"))))))))


;;; returns optionally adjusted region-end-point.

(defun ispell-add-per-file-word-list (word)
  "Add WORD to the per-file word list."
  (or ispell-buffer-local-name
      (setq ispell-buffer-local-name (buffer-name)))
  (save-excursion
    (goto-char (point-min))
    (let ((old-case-fold-search case-fold-search)
	  line-okay search done found)
      (while (not done)
	(setq case-fold-search nil
	      search (search-forward ispell-words-keyword nil 'move)
	      found (or found search)
	      line-okay (< (+ (length word) 1 ; 1 for space after word..
			      (progn (end-of-line) (current-column)))
			   80)
	      case-fold-search old-case-fold-search)
	(if (or (and search line-okay)
		(null search))
	    (progn
	      (setq done t)
	      (if (null search)
		  (progn
		    (open-line 1)
		    (unless found (newline))
		    (insert (concat comment-start " " ispell-words-keyword))
		    (if (> (length comment-end) 0)
			(save-excursion
			  (newline)
			  (insert comment-end)))))
	      (insert (concat " " word))))))))

(add-to-list 'debug-ignored-errors "^No word found to check!$")

(provide 'ispell)


;;; LOCAL VARIABLES AND BUFFER-LOCAL VALUE EXAMPLES.

;;; Local Variable options:
;;; mode: name(-mode)
;;; eval: expression
;;; local-variable: value

;;; The following sets the buffer local dictionary to `american' English
;;; and spell checks only comments.

;;; Local Variables:
;;; mode: emacs-lisp
;;; comment-column: 40
;;; ispell-check-comments: exclusive
;;; ispell-local-dictionary: "american"
;;; End:


;;; MORE EXAMPLES OF ISPELL BUFFER-LOCAL VALUES

;;; The following places this file in nroff parsing and extended char modes.
;;; Local IspellParsing: nroff-mode ~nroff
;;; Change IspellPersDict to IspellPersDict: to enable the following line.
;;; Local IspellPersDict ~/.ispell_lisp
;;; The following were automatically generated by ispell using the 'A' command:
; LocalWords:  settable alist inews mh frag pdict Wildcards iconify arg tex kss
; LocalWords:  alists minibuffer bufferp autoload loaddefs aff Dansk KOI SPC op
; LocalWords:  Francais Nederlands charset autoloaded popup nonmenu regexp num
; LocalWords:  AMStex hspace includeonly nocite epsfig displaymath eqnarray reg
; LocalWords:  minipage modeline pers dict unhighlight buf grep sync prev inc
; LocalWords:  fn hilight oldot NB AIX msg init read's bufs pt cmd Quinlan eg
; LocalWords:  uuencoded unidiff sc nn VM SGML eval IspellPersDict unsplitable
; LocalWords:  lns XEmacs HTML casechars Multibyte

;;; ispell.el ends here
