;;; ispell.el --- Interface to International Ispell Version 3.1

;; Copyright (C) 1994, 1995, 1997, 1998, 1999 Free Software Foundation, Inc.

;; Authors         : Ken Stevens <k.stevens@ieee.org>
;; Stevens Mod Date: Fri May  7 14:25:14 PDT 1999
;; Stevens Revision: 3.2
;; Status          : Release with 3.1.12+ ispell.
;; Bug Reports     : ispell-el-bugs@itcorp.com
;; Web Site        : http://kdstevens.com/~stevens/ispell-page.html

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
;;  Accepting word definitions in latex mode can mess up math mode skipping.
;;  On some versions of emacs, growing the minibuffer fails.
;;    see `ispell-help-in-bufferp'.

;; HISTORY

;; Modifications made in latest versions:

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
;; Improved skipping support for sgml.
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
  "*The default size of the `*Choices*' window, including status line.
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
This variable becomes buffer-local when set in any fashion.

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


;; Define definitions here only for personal dictionaries.
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
   ("british"				; British version
    "[A-Za-z]" "[^A-Za-z]" "[']" nil ("-B" "-d" "british") nil iso-8859-1)
   ("castellano"			; Spanish mode
    "[A-Z\301\311\315\321\323\332\334a-z\341\351\355\361\363\372\374]"
    "[^A-Z\301\311\315\321\323\332\334a-z\341\351\355\361\363\372\374]"
    "[---]" nil ("-B" "-d" "castellano") "~tex" iso-8859-1)
   ("castellano8"			; 8 bit Spanish mode
    "[A-Z\301\311\315\321\323\332\334a-z\341\351\355\361\363\372\374]"
    "[^A-Z\301\311\315\321\323\332\334a-z\341\351\355\361\363\372\374]"
    "[---]" nil ("-B" "-d" "castellano") "~latin1" iso-8859-1)))


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
    "[---']" t nil "~list" iso-8859-1)))


;;; Fourth part of dictionary, shortened for loaddefs.el
;;;###autoload
(setq
 ispell-dictionary-alist-4
 '(("francais-tex"			; Francais.aff
    "[A-Za-z\300\302\306\307\310\311\312\313\316\317\324\331\333\334\340\342\347\350\351\352\353\356\357\364\371\373\374\\]"
    "[^A-Za-z\300\302\306\307\310\311\312\313\316\317\324\331\333\334\340\342\347\350\351\352\353\356\357\364\371\373\374\\]"
    "[---'^`\"]" t nil "~tex" iso-8859-1)
   ("nederlands"			; Nederlands.aff
    "[A-Za-z\300-\305\307\310-\317\322-\326\331-\334\340-\345\347\350-\357\361\362-\366\371-\374]"
    "[^A-Za-z\300-\305\307\310-\317\322-\326\331-\334\340-\345\347\350-\357\361\362-\366\371-\374]"
    "[']" t ("-C") nil iso-8859-1)
   ("nederlands8"			; Dutch8.aff
    "[A-Za-z\300-\305\307\310-\317\322-\326\331-\334\340-\345\347\350-\357\361\362-\366\371-\374]"
    "[^A-Za-z\300-\305\307\310-\317\322-\326\331-\334\340-\345\347\350-\357\361\362-\366\371-\374]"
    "[']" t ("-C") nil iso-8859-1)))


;;; Fifth part of dictionary, shortened for loaddefs.el
;;;###autoload
(setq
 ispell-dictionary-alist-5
 '(("norsk"				; 8 bit Norwegian mode
    "[A-Za-z\305\306\307\310\311\322\324\330\345\346\347\350\351\362\364\370]"
    "[^A-Za-z\305\306\307\310\311\322\324\330\345\346\347\350\351\362\364\370]"
    "[\"]" nil ("-d" "norsk") "~list" iso-8859-1)
   ("norsk7-tex"			; 7 bit Norwegian TeX mode
    "[A-Za-z{}\\'^`]" "[^A-Za-z{}\\'^`]"
    "[\"]" nil ("-d" "norsk") "~plaintex" iso-8859-1)
   ("polish"				; polish mode 
    "[A-Za-z\241\243\246\254\257\261\263\266\274\277\306\312\321\323\346\352\361\363]"
    "[^A-Za-z\241\243\246\254\257\261\263\266\274\277\306\312\321\323\346\352\361\363]"
    "" nil ( "-d" "polish") nil iso-8859-2)))


;;; Sixth part of dictionary, shortened for loaddefs.el
;;;###autoload
(setq
 ispell-dictionary-alist-6
 '(("russian"				; Russian.aff (KOI8-R charset)
    "[\341\342\367\347\344\345\263\366\372\351\352\353\354\355\356\357\360\362\363\364\365\346\350\343\376\373\375\370\371\377\374\340\361\301\302\327\307\304\305\243\326\332\311\312\313\314\315\316\317\320\322\323\324\325\306\310\303\336\333\335\330\331\337\334\300\321]"
    "[^\341\342\367\347\344\345\263\366\372\351\352\353\354\355\356\357\360\362\363\364\365\346\350\343\376\373\375\370\371\377\374\340\361\301\302\327\307\304\305\243\326\332\311\312\313\314\315\316\317\320\322\323\324\325\306\310\303\336\333\335\330\331\337\334\300\321]"
    "" nil ("-d" "russian") nil koi8-r)
   ("svenska"				; Swedish mode
    "[A-Za-z\345\344\366\351\340\374\350\346\370\347\305\304\326\311\300\334\310\306\330\307]"
    "[^A-Za-z\345\344\366\351\340\374\350\346\370\347\305\304\326\311\300\334\310\306\330\307]"
    "[']" nil ("-C") "~list" iso-8859-1)))



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
(defconst ispell-menu-map-needed
  ;; only needed when not version 18 and not XEmacs.
  (and (not ispell-menu-map)
       (not (string-match "18\\.[0-9]+\\.[0-9]+" emacs-version))
       (not (string-match "Lucid\\|XEmacs" emacs-version))))

;;;###autoload
(if ispell-menu-map-needed
    (let ((dicts (reverse (cons (cons "default" nil) ispell-dictionary-alist)))
	  name)
      (setq ispell-menu-map (make-sparse-keymap "Spell"))
      ;; add the dictionaries to the bottom of the list.
      (while dicts
	(setq name (car (car dicts))
	      dicts (cdr dicts))
	(if (stringp name)
	    (define-key ispell-menu-map (vector (intern name))
	      (cons (concat "Select " (capitalize name))
		    (list 'lambda () '(interactive)
			  (list 'ispell-change-dictionary name))))))))

;;; define commands in menu in opposite order you want them to appear.
;;;###autoload
(if ispell-menu-map-needed
    (progn
      (define-key ispell-menu-map [ispell-change-dictionary]
	'("Change Dictionary" . ispell-change-dictionary))
      (define-key ispell-menu-map [ispell-kill-ispell]
	'("Kill Process" . ispell-kill-ispell))
      (define-key ispell-menu-map [ispell-pdict-save]
	'("Save Dictionary" . (lambda () (interactive) (ispell-pdict-save t t))))
      (define-key ispell-menu-map [ispell-complete-word]
	'("Complete Word" . ispell-complete-word))
      (define-key ispell-menu-map [ispell-complete-word-interior-frag]
	'("Complete Word Frag" . ispell-complete-word-interior-frag))))

;;;###autoload
(if ispell-menu-map-needed
    (progn
      (define-key ispell-menu-map [ispell-continue]
	'("Continue Check" . ispell-continue))
      (define-key ispell-menu-map [ispell-word]
	'("Check Word" . ispell-word))
      (define-key ispell-menu-map [ispell-comments-and-strings]
	'("Check Comments" . ispell-comments-and-strings))
      (define-key ispell-menu-map [ispell-region]
	'("Check Region" . ispell-region))
      (define-key ispell-menu-map [ispell-buffer]
	'("Check Buffer" . ispell-buffer))))

;;;###autoload
(if ispell-menu-map-needed
    (progn
      (define-key ispell-menu-map [ispell-message]
	'("Check Message" . ispell-message))
      (define-key ispell-menu-map [ispell-help]
	;; use (x-popup-menu last-nonmenu-event(list "" ispell-help-list)) ?
	'("Help" . (lambda () (interactive) (describe-function 'ispell-help))))
      (put 'ispell-region 'menu-enable 'mark-active)
      (fset 'ispell-menu-map (symbol-value 'ispell-menu-map))))

;;; XEmacs version 19 & 20
(if (and (not (string-match "18\\.[0-9]+\\.[0-9]+" emacs-version))
	 (string-match "Lucid\\|XEmacs" emacs-version)
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
	     ["Save Dictionary"	(ispell-pdict-save t t)		t]
	     ["Change Dictionary" ispell-change-dictionary	t]))
	  name)
      (while dicts
	(setq name (car (car dicts))
	      dicts (cdr dicts))
	(if (stringp name)
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
(if (and (string-match "XEmacs" emacs-version)
	 (fboundp 'int-char))
    (fset 'ispell-int-char 'int-char)
  ;; Emacs and XEmacs 19 or earlier
  (fset 'ispell-int-char 'identity))


;;; **********************************************************************
;;; The following are used by ispell, and should not be changed.
;;; **********************************************************************


;;; The version must be 3.1 or greater for this version of ispell.el
;;; There is an incompatibility between version 3.1.12 and lower versions.
(defconst ispell-required-version '(3 1 12)
  "Ispell versions with which this version of ispell.el is known to work.")
(defvar ispell-offset -1
  "Offset that maps protocol differences between ispell 3.1 versions.")

;;; This variable contains the current dictionary being used if the ispell
;;; process is running.  Otherwise it contains the global default.
(defvar ispell-dictionary nil
  "The name of the current dictionary, or nil for the default.
When `ispell-local-dictionary' is nil, `ispell-dictionary' is used to select
the dictionary for new buffers.

This is passed to the ispell process using the `-d' switch and is
used as key in `ispell-dictionary-alist' (which see).")

(defun ispell-decode-string (str)
  "Decodes multibyte character strings."
  (if (and (boundp 'enable-multibyte-characters)
	   (fboundp 'decode-coding-string)
	   enable-multibyte-characters
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

(defvar ispell-process nil
  "The process object for Ispell.")

(defvar ispell-pdict-modified-p nil
  "Non-nil means personal dictionary has modifications to be saved.")

;;; If you want to save the dictionary when quitting, must do so explicitly.
;;; When non-nil, the spell session is terminated.
;;; When numeric, contains cursor location in buffer, and cursor remains there.
(defvar ispell-quit nil)

(defvar ispell-filter nil
  "Output filter from piped calls to Ispell.")

(defvar ispell-filter-continue nil
  "Control variable for Ispell filter function.")

(defvar ispell-process-directory nil
  "The directory where `ispell-process' was started.")

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
    ;; matches e-mail addresses, file names, http addresses, etc.
    ("\\(/\\|\\(\\(\\w\\|-\\)+[.:@]\\)\\)\\(\\w\\|-\\)*\\([.:/@]+\\(\\w\\|-\\|~\\)+\\)+")
    ;; This is a pretty complex regexp.  It can be simplified to the following:
    ;; "\\(\\w\\|-\\)*\\([.:/@]+\\(\\w\\|-\\|~\\)+\\)+"
    ;; but some valid text will be skipped, e.g. "his/her".  This could be
    ;; fixed up (at the expense of a moderately more complex regexp)
    ;; by not allowing "/" to be the character which triggers the
    ;; identification of the computer name, e.g.:
    ;; "\\(\\w\\|-\\)+[.:@]\\(\\w\\|-\\)*\\([.:/@]+\\(\\w\\|-\\|~\\)+\\)+"
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
     ;;("\\\\epsfig"		ispell-tex-arg-end)
     ("\\\\document\\(class\\|style\\)" .
      "\\\\begin[ \t\n]*{[ \t\n]*document[ \t\n]*}"))
    (;; delimited with \begin.  In ispell: displaymath, eqnarray, eqnarray*,
     ;; equation, minipage, picture, tabular, tabular* (ispell)
     ("\\(figure\\|table\\)\\*?"  ispell-tex-arg-end 0)
     ("list"			  ispell-tex-arg-end 2)
     ("program"		. "\\\\end[ \t\n]*{[ \t\n]*program[ \t\n]*}")
     ("verbatim\\*?"	. "\\\\end[ \t\n]*{[ \t\n]*verbatim\\*?[ \t\n]*}")))
  "*Lists of regions to be skipped in TeX mode.
First list is used raw.
Second list has key placed inside \\begin{}.

Delete or add any regions you want to be automatically selected
for skipping in latex mode.")


(defcustom ispell-skip-sgml 'use-mode-name
  "*Indicates whether ispell should skip spell checking of SGML markup.
If t, always skip SGML markup; if nil, never skip; if non-t and non-nil,
guess whether SGML markup should be skipped according to the name of the
buffer's major mode."
  :type '(choice (const :tag "always" t) (const :tag "never" nil)
		 (const :tag "use-mode-name" use-mode-name))
  :group 'ispell)

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



(and (not (string-match "18\\.[0-9]+\\.[0-9]+" emacs-version))
     (not (boundp 'epoch::version))
     (defalias 'ispell 'ispell-buffer)
     (defalias 'ispell-check-version 'check-ispell-version))


(if (not (fboundp 'buffer-substring-no-properties))
    (defun buffer-substring-no-properties (start end)
      (buffer-substring start end)))

;;;###autoload
(define-key esc-map "$" 'ispell-word)

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
or \\[ispell-region] to update the Ispell process."
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
	(process-send-string ispell-process "%\n") ;put in verbose mode
	(process-send-string ispell-process (concat "^" word "\n"))
	;; wait until ispell has processed word
	(while (progn
		 (accept-process-output ispell-process)
		 (not (string= "" (car ispell-filter)))))
	;;(process-send-string ispell-process "!\n") ;back to terse mode.
	(setq ispell-filter (cdr ispell-filter)) ; remove extra \n
	(if (listp ispell-filter)
	    (setq poss (ispell-parse-output (car ispell-filter))))
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
			    (ispell-region start end)))))
	       (if (get-buffer ispell-choices-buffer)
		   (kill-buffer ispell-choices-buffer))))
	(ispell-pdict-save ispell-silently-savep)
	(if ispell-quit (setq ispell-quit nil))))
      (goto-char cursor-location))))	; return to original location


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

;;; ###autoload
(defun ispell-pdict-save (&optional no-query force-save)
  "Check to see if the personal dictionary has been modified.
If so, ask if it needs to be saved."
  (interactive (list ispell-silently-savep t))
  (if (and ispell-pdict-modified-p (listp ispell-pdict-modified-p))
      (setq ispell-pdict-modified-p (car ispell-pdict-modified-p)))
  (if (or ispell-pdict-modified-p force-save)
      (if (or no-query (y-or-n-p "Personal dictionary modified.  Save? "))
	  (progn
	    (process-send-string ispell-process "#\n") ; save dictionary
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
  (let ((textbuf (current-buffer))
	(count ?0)
	(line 2)
	(max-lines (- (window-height) 4)) ; assure 4 context lines.
	(choices miss)
	(window-min-height (min window-min-height
				ispell-choices-win-default-height))
	(command-characters '( ?  ?i ?a ?A ?r ?R ?? ?x ?X ?q ?l ?u ?m ))
	(dedicated (window-dedicated-p (selected-window)))
	(skipped 0)
	char num result textwin dedicated-win highlighted)

    ;; setup the *Choices* buffer with valid data.
    (save-excursion
      (set-buffer (get-buffer-create ispell-choices-buffer))
      (setq mode-line-format (concat "--  %b  --  word: " word))
      (and (fboundp 'set-specifier)	; prevent XEmacs modeline hiding
	   (set-specifier has-modeline-p (cons (current-buffer) nil)))
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

    ;; Assure word is visible
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
		    (process-send-string ispell-process (concat "*" word "\n"))
		    (setq ispell-pdict-modified-p '(t)) ; dictionary modified!
		    nil)
		   ((or (= char ?a) (= char ?A)) ; accept word without insert
		    (process-send-string ispell-process (concat "@" word "\n"))
		    (if (null ispell-pdict-modified-p)
			(setq ispell-pdict-modified-p
			      (list ispell-pdict-modified-p)))
		    (if (= char ?A) 0))	; return 0 for ispell-add buffer-local
		   ((or (= char ?r) (= char ?R)) ; type in replacement
		    (and (eq 'block ispell-highlight-p) ; refresh tty's
			 (ispell-highlight-spelling-error start end nil t))
		    (let ((result
			   (if (or (= char ?R) ispell-query-replace-choices)
			       (list (read-string "Query-replacement for: "
						  word) t)
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
				     word))
			  (new-line 2))
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
				    choices miss)
			      (while (and choices ; adjust choices window.
					  (< (if (> (+ 7 (current-column)
						       (length (car choices))
						       (if (> count ?~) 3 0))
						    (window-width))
						 (progn
						   (insert "\n")
						   (setq new-line
							 (1+ new-line)))
					       new-line)
					     max-lines))
				(while (memq count command-characters)
				  (setq count (ispell-int-char (1+ count))
					skipped (1+ skipped)))
				(insert "(" count ") " (car choices) "  ")
				(setq choices (cdr choices)
				      count (ispell-int-char (1+ count))))
			      (setq count (ispell-int-char
					   (- count ?0 skipped))))
			    (select-window (previous-window))
			    (if (and (/= new-line line)
				     (> (max line new-line)
					ispell-choices-win-default-height))
				(let* ((minh ispell-choices-win-default-height)
				       (gr-bl (if (< line minh) ; blanks
						  (- minh line)
						0))
				       (shr-bl (if (< new-line minh) ; blanks
						   (- minh new-line)
						 0)))
				  (if (> new-line line)
				      (enlarge-window (- new-line line gr-bl))
				    (shrink-window (- line new-line shr-bl)))
				  (setq line new-line)))
			    (select-window (next-window)))))
		    (and (eq 'block ispell-highlight-p)
			 (ispell-highlight-spelling-error start end nil
							  'block))
		    t)			; reselect from new choices
		   ((= char ?u)		; insert lowercase into dictionary
		    (process-send-string ispell-process
					 (concat "*" (downcase word) "\n"))
		    (setq ispell-pdict-modified-p '(t)) ; dictionary modified!
		    nil)
		   ((= char ?m)		; type in what to insert
		    (process-send-string
		     ispell-process (concat "*" (read-string "Insert: " word)
					    "\n"))
		    (setq ispell-pdict-modified-p '(t))
		    (cons word nil))
		   ((and (>= num 0) (< num count))
		    (if ispell-query-replace-choices ; Query replace flag
			(list (nth num miss) 'query-replace)
		      (nth num miss)))
		   ((= char ?\C-l)
		    (redraw-display) t)
		   ((= char ?\C-r)
		    (if (marker-position ispell-recursive-edit-marker)
			(progn
			  (message "Only one recursive edit session supported")
			  (beep))
		      (set-marker ispell-recursive-edit-marker start)
		      ;;(set-marker ispell-region-end reg-end)
		      (and ispell-highlight-p		; unhighlight
			   (ispell-highlight-spelling-error start end))
		      (unwind-protect
			  (progn
			    (save-window-excursion (save-excursion
						     (recursive-edit)) t)
			    (if (not (equal (marker-buffer
					     ispell-recursive-edit-marker)
					    (current-buffer)))
				(error
				 "Cannot continue ispell from this buffer."))
			    (goto-char ispell-recursive-edit-marker))
			(set-marker ispell-recursive-edit-marker nil)))
		    (cons word nil))	; recheck starting at this word.
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
  (if ispell-use-framepop-p
      (progn
	(framepop-display-buffer (get-buffer ispell-choices-buffer))
	(get-buffer-window ispell-choices-buffer t)
	(select-window (previous-window))) ; *Choices* window
    ;; standard selection by splitting a small buffer out of this window.
    (let ((choices-window (get-buffer-window ispell-choices-buffer)))
      (if choices-window
	  (if (= line (window-height choices-window))
	      (select-window choices-window)
	    ;; *Choices* window changed size.  Adjust the choices window
	    ;; without scrolling the spelled window when possible
	    (let ((window-line (- line (window-height choices-window)))
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
			 (enlarge-window (- 15 (window-height))))
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
	  (select-window (minibuffer-window))
	  ;;(enlarge-window 2)
	  (erase-buffer)
	  (cond ((string-match "Lucid\\|XEmacs" emacs-version)
		 (message help-3)
		 (enlarge-window 1)
		 (message help-2)
		 (enlarge-window 1)
		 (message help-1)
		 (goto-char (point-min)))
		(t
		 (if (not (string-match "18\\.[0-9]+\\.[0-9]+" emacs-version))
		     (message nil))
		 ;;(set-minibuffer-window (selected-window))
		 (enlarge-window 2)
		 (insert (concat help-1 "\n" help-2 "\n" help-3))))
	  (sit-for 5)
	  (erase-buffer))))))


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
   ((string-match "Lucid\\|XEmacs" emacs-version)
    (ispell-highlight-spelling-error-xemacs start end highlight))
   ((and (not (string-match "18\\.[0-9]+\\.[0-9]+" emacs-version))
	 (featurep 'faces) window-system)
    (ispell-highlight-spelling-error-overlay start end highlight))
   (t (ispell-highlight-spelling-error-generic start end highlight refresh))))


(defun ispell-overlay-window (height)
  "Create a window covering the top HEIGHT lines of the current window.
Ensure that the line above point is still visible but otherwise avoid
scrolling the current window.  Leave the new window selected."
  (save-excursion
    (let ((oldot (save-excursion (forward-line -1) (point)))
	  (top (save-excursion (move-to-window-line height) (point))))
      ;; If line above old point (line starting at oldot) would be
      ;; hidden by new window, scroll it to just below new win
      ;; otherwise set top line of other win so it doesn't scroll.
      (if (< oldot top) (setq top oldot))
      ;; NB: XEmacs 19.9 bug: If a window of size N (N includes the mode
      ;; line) is demanded, the last line is not visible.
      ;; At least this happens on AIX 3.2, XEmacs w/ Motif, font 9x15.
      ;; So we increment the height for this case.
      (if (and (string-match "Lucid\\|XEmacs" emacs-version)
	       (string-match "19\\.9\\.[0-9]+" emacs-version))
	  (setq height (1+ height)))
      ;; if frame is unsplitable, temporarily disable that...
      (if (cdr (assq 'unsplittable (frame-parameters (selected-frame))))
	  (let ((frame (selected-frame)))
	    (modify-frame-parameters frame '((unsplittable . nil)))
	    (split-window nil height)
	    (modify-frame-parameters frame '((unsplittable . t))))
	(split-window nil height))
      (set-window-start (next-window) top))))


;;; Should we add a compound word match return value?
(defun ispell-parse-output (output)
  "Parse the OUTPUT string from Ispell process and return:
1: t for an exact match.
2: A string containing the root word matched via suffix removal.
3: A list of possible correct spellings of the format:
   (\"ORIGINAL-WORD\" OFFSET MISS-LIST GUESS-LIST)
   ORIGINAL-WORD is a string of the possibly misspelled word.
   OFFSET is an integer giving the line offset of the word.
   MISS-LIST and GUESS-LIST are possibly null lists of guesses and misses.
4: Nil when an error has occurred."
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
      (list original-word offset (nreverse miss-list)(nreverse guess-list))))))


(defun check-ispell-version (&optional interactivep)
  ;; This is a little wasteful as we actually launch ispell twice: once
  ;; to make sure it's the right version, and once for real.  But people
  ;; get confused by version mismatches *all* the time (and I've got the
  ;; email to prove it) so I think this is worthwhile.  And the -v[ersion]
  ;; option is the only way I can think of to do this that works with
  ;; all versions, since versions earlier than 3.0.09 didn't identify
  ;; themselves on startup.
  (interactive "p")
  (let ((result t)
	(case-fold-search-val case-fold-search)
	;; avoid bugs when syntax of `.' changes in various default modes
	(default-major-mode 'fundamental-mode)
	status)
    (save-excursion
      (set-buffer (get-buffer-create " *ispell-tmp*"))
      (erase-buffer)
      (setq status (call-process ispell-program-name nil t nil "-v"))
      (goto-char (point-min))
      (if interactivep
	  (progn
	    (end-of-line)
	    (setq result (concat (buffer-substring-no-properties (point-min)
								 (point))
				 ", "
				 ispell-version))
	    (message result)
	    (goto-char (point-min))))
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


(defun ispell-init-process ()
  "Check status of Ispell process and start if necessary."
  (if (and ispell-process
	   (eq (process-status ispell-process) 'run)
	   ;; If we're using a personal dictionary, assure
	   ;; we're in the same default directory!
	   (or (not ispell-personal-dictionary)
	       (equal ispell-process-directory default-directory)))
      (setq ispell-filter nil ispell-filter-continue nil)
    ;; may need to restart to select new personal dictionary.
    (ispell-kill-ispell t)
    (message "Starting new Ispell process...")
    (sit-for 0)
    (check-ispell-version)
    (setq ispell-process
	  (let ((process-connection-type ispell-use-ptys-p))
	    (apply 'start-process
		   "ispell" nil ispell-program-name
		   "-a"			; accept single input lines
		   "-m"			; make root/affix combos not in dict
		   (let (args)
		     ;; Local dictionary becomes the global dictionary in use.
		     (if ispell-local-dictionary
			 (setq ispell-dictionary ispell-local-dictionary))
		     (setq args (ispell-get-ispell-args))
		     (if ispell-dictionary ; use specified dictionary
			 (setq args
			       (append (list "-d" ispell-dictionary) args)))
		     (if ispell-personal-dictionary ; use specified pers dict
			 (setq args
			       (append args
				       (list "-p"
					     (expand-file-name
					      ispell-personal-dictionary)))))
		     (setq args (append args ispell-extra-args))
		     args)))
	  ispell-filter nil
	  ispell-filter-continue nil
	  ispell-process-directory default-directory)
    (set-process-filter ispell-process 'ispell-filter)
    (if (and (boundp 'enable-multibyte-characters)
	     (fboundp 'set-process-coding-system)
	     enable-multibyte-characters)
	(set-process-coding-system ispell-process (ispell-get-coding-system)
				   (ispell-get-coding-system)))
    ;; Get version ID line
    (if (not (string-match "18\\.[0-9]+\\.[0-9]+" emacs-version))
	(accept-process-output ispell-process 3)
      (accept-process-output ispell-process))
    ;; get more output if filter empty?
    (if (null ispell-filter) (accept-process-output ispell-process 3))
    (cond ((null ispell-filter)
	   (error "%s did not output version line" ispell-program-name))
	  ((and
	    (stringp (car ispell-filter))
	    (if (string-match "warning: " (car ispell-filter))
		(progn
		  (if (not (string-match "18\\.[0-9]+\\.[0-9]+" emacs-version))
		      (accept-process-output ispell-process 3) ; was warn msg.
		    (accept-process-output ispell-process))
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
	   (accept-process-output ispell-process 3)
	   (error "%s" (mapconcat 'identity ispell-filter "\n"))))
    (setq ispell-filter nil)		; Discard version ID line
    (let ((extended-char-mode (ispell-get-extended-character-mode)))
      (if extended-char-mode		; ~ extended character mode
	  (process-send-string ispell-process
			       (concat extended-char-mode "\n"))))
    (process-kill-without-query ispell-process)))

;;;###autoload
(defun ispell-kill-ispell (&optional no-error)
  "Kill current Ispell process (so that you may start a fresh one).
With NO-ERROR, just return non-nil if there was no Ispell running."
  (interactive)
  (if (not (and ispell-process
		(eq (process-status ispell-process) 'run)))
      (or no-error
	  (error "There is no ispell process running!"))
    (process-send-eof ispell-process)
    (if (eq (process-status ispell-process) 'run)
	(accept-process-output ispell-process 1))
    (if (eq (process-status ispell-process) 'run)
	(kill-process ispell-process))
    (while (not (or (eq (process-status ispell-process) 'exit)
		    (eq (process-status ispell-process) 'signal)))
      (sleep-for 0 250))
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

With prefix argument, set the default directory."
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
(defun ispell-region (reg-start reg-end)
  "Interactively check a region for spelling errors.
Return non-nil if spell session completed normally."
  (interactive "r")			; Don't flag errors on read-only bufs.
  (ispell-accept-buffer-local-defs)	; set up dictionary, local words, etc.
  (unwind-protect
      (save-excursion
	(message "Spell checking %s using %s dictionary..."
		 (if (and (= reg-start (point-min)) (= reg-end (point-max)))
		     (buffer-name) "region")
		 (or ispell-dictionary "default"))
	;; Returns cursor to original location.
	(save-window-excursion
	  (goto-char reg-start)
	  (let ((transient-mark-mode nil)
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
		    (ispell-process-line string)) ; (special start end)
		(goto-char end)))))
	(not ispell-quit))
    ;; protected
    (if (get-buffer ispell-choices-buffer)
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
	  (setq ispell-quit nil))
      (set-marker ispell-region-end nil)
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
    (if ispell-skip-sgml
	(setq skip-regexp (concat "<author>" "\\|"
				  "<[cC][oO][dD][eE]>" "\\|" 
				  "<[vV][eE][rR][bB]>" "\\|" 
				  "<[tT][tT]>" "\\|"
				  "<[tT][tT]/" "\\|"
				  "</" "\\|"
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
     ((and ispell-skip-sgml (string-match "<author>" key))
      (search-forward-regexp ".$" ispell-region-end t))
     ((and ispell-skip-sgml (string-match "</" key))
      (search-forward ">" ispell-region-end t))
     ((and ispell-skip-sgml (string-match "<[cC][oO][dD][eE]>" key))  
      (search-forward-regexp "</[cC][oO][dD][eE]>" ispell-region-end t))
     ((and ispell-skip-sgml (string-match "<[vV][eE][rR][bB]>" key))  
      (search-forward-regexp "</[vV][eE][rR][bB]>" ispell-region-end t))
     ((and ispell-skip-sgml (string-match "<[tT][tT]>" key))  
      (search-forward-regexp "</[tT][tT]>" ispell-region-end t))     
     ((and ispell-skip-sgml (string-match "<[tT][tT]/" key))  
      (search-forward "/" ispell-region-end t))      
     ((and ispell-skip-sgml (string-match "<" key))
      (search-forward-regexp "[/>]" ispell-region-end t))
     ((and ispell-skip-sgml (string-match "&" key))
      (search-forward ";" ispell-region-end t))
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
     ;;((looking-at "[---#@*+!%~^]")	; SKIP SPECIAL ISPELL CHARACTERS
     ;; (forward-char 1))		; not needed as quoted below.
     ((or (re-search-forward ispell-casechars end t) ; TEXT EXISTS
	  (re-search-forward "[][()${}]" end t)) ; or MATH COMMANDS
      (setq string (concat "^" (buffer-substring-no-properties start end)
			   "\n"))
      (goto-char end))
     (t (goto-char end)))		; EMPTY LINE, skip it.
    string))


(defun ispell-process-line (string)
  ;;(declare special start end)
  (let (poss
	;; line-offset is the change so far in the size of the line.
	;; The position values we get from ispell reflect the original
	;; text, and correction of previous words may have shifted the
	;; current word within the line.
	(line-offset 0))
    ;; send string to spell process and get input.
    (process-send-string ispell-process string)
    (while (progn
	     (accept-process-output ispell-process)
	     ;; Last item of output contains a blank line.
	     (not (string= "" (car ispell-filter)))))
    ;; parse all inputs from the stream one word at a time.
    ;; Place in FIFO order and remove the blank item.
    (setq ispell-filter (nreverse (cdr ispell-filter)))
    (while (and (not ispell-quit) ispell-filter)
      (setq poss (ispell-parse-output (car ispell-filter)))
      (if (and poss (listp poss))	; spelling error occurred.
	  ;; Whenever we have misspellings, we can change
	  ;; the buffer.  Keep boundaries as markers.
	  ;; Markers can move with highlighting!  This destroys
	  ;; end of region markers line-end and ispell-region-end
	  (let ((word-start
		 (copy-marker (+ start ispell-offset line-offset
				 (car (cdr poss)))))
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
	    (if (/= (+ word-len (point))
		    (progn
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

	    ;; Recheck when recursive edit changes misspelled word
	    (goto-char word-start)
	    (if (not (string-equal (buffer-substring-no-properties
				    (point) (+ word-len (point)))
				   (car poss)))
		(progn
		  (set-marker line-end (point))
		  (setq ispell-filter nil
			recheck-region t)))

	    (cond
	     ((and replace (listp replace))
	      ;; REPLACEMENT WORD
	      ;; Recheck line starting with the replacement word.
	      (setq ispell-filter nil
		    recheck-region t)
	      (delete-region (point) (+ word-len (point)))
	      (insert (car replace))
	      (setq line-offset (+ line-offset (- line-end end)))
	      ;; Only typed-in replacements need to be re-checked.
	      (if (not (eq 'query-replace (car (cdr replace))))
		  (backward-char (length (car replace))))
	      (set-marker line-end (point)) ; continue checking from here.
	      (if (car (cdr replace))
		  (unwind-protect
		      (save-window-excursion
			(delete-other-windows) ; to correctly show help.
			;; Assume case-replace &
			;; case-fold-search correct?
			(query-replace (car poss) (car replace) t))
		    (goto-char word-start))))
	     ((or (null replace)
		  (equal 0 replace))	; ACCEPT/INSERT
	      (if (equal 0 replace)	; BUFFER-LOCAL DICT ADD
		  (ispell-add-per-file-word-list (car poss)))
	      ;; This avoids pointing out the word that was
	      ;; just accepted (via 'i' or 'a') if it follows
	      ;; on the same line.
	      ;; Redo check following the accepted word.
	      (if (and ispell-pdict-modified-p
		       (listp ispell-pdict-modified-p))
		  ;; Word accepted.  Recheck line.
		  (progn
		    (setq ispell-pdict-modified-p ;update flag
			  (car ispell-pdict-modified-p)
			  ispell-filter nil
			  recheck-region t)
		    (set-marker line-end (marker-position word-start)))))
	     (replace			; STRING REPLACEMENT for this word.
	      (delete-region (point) (+ word-len (point)))
	      (insert replace)
	      (setq line-offset (+ line-offset (- line-end end)))

	      (set-marker line-start (+ line-start
					(- (length replace)
					   (length (car poss)))))))
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
      (setq ispell-filter (cdr ispell-filter)))))


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
  "Try to complete the word before or under point (see `lookup-words')
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
		 (and ispell-skip-sgml (eq last-char ?>))
		 (and ispell-skip-sgml (eq last-char ?\;))))
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
	     ((featurep 'supercite)	; sc 3.0
	      (concat "\\(" (sc-cite-regexp) "\\)" "\\|"
		      (ispell-non-empty-string sc-reference-tag-string)))
	     ((featurep 'sc)		; sc 2.3
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
  (process-send-string ispell-process "!\n") ; Put process in terse mode.
  ;; We assume all major modes with "tex-mode" in them should use latex parsing
  ;; When exclusively checking comments, set to raw text mode (nroff).
  (if (and (not (eq 'exclusive ispell-check-comments))
	   (or (and (eq ispell-parser 'use-mode-name)
		    (string-match "[Tt][Ee][Xx]-mode"
				  (symbol-name major-mode)))
	       (eq ispell-parser 'tex)))
      (progn
	(process-send-string ispell-process "+\n") ; set ispell mode to tex
	(if (not (eq ispell-parser 'tex))
	    (set (make-local-variable 'ispell-parser) 'tex)))
    (process-send-string ispell-process "-\n"))	; set mode to normal (nroff)
  ;; If needed, test for SGML & HTML modes and set a buffer local nil/t value.
  (if (and ispell-skip-sgml (not (eq ispell-skip-sgml t)))
      (set (make-local-variable 'ispell-skip-sgml)
	   (not (null (string-match "sgml\\|html"
				    (downcase (symbol-name major-mode)))))))
  ;; Set default extended character mode for given buffer, if any.
  (let ((extended-char-mode (ispell-get-extended-character-mode)))
    (if extended-char-mode
	(process-send-string ispell-process (concat extended-char-mode "\n"))))
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
		   (process-send-string ispell-process "+\n~tex\n"))
		  ((string-match "nroff-mode" string)
		   (process-send-string ispell-process "-\n~nroff\n"))
		  ((string-match "~" string) ; Set extended character mode.
		   (process-send-string ispell-process (concat string "\n")))
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
	      (process-send-string ispell-process
				   (concat "@" string "\n"))))))))


;;; returns optionally adjusted region-end-point.

(defun ispell-add-per-file-word-list (word)
  "Add WORD to the per-file word list."
  (or ispell-buffer-local-name
      (setq ispell-buffer-local-name (buffer-name)))
  (save-excursion
    (goto-char (point-min))
    (let ((old-case-fold-search case-fold-search)
	  line-okay search done string)
      (while (not done)
	(setq case-fold-search nil
	      search (search-forward ispell-words-keyword nil 'move)
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
		    (setq string (concat comment-start " "
					 ispell-words-keyword))
		    (insert string)
		    (if (and comment-end (not (equal "" comment-end)))
			(save-excursion
			  (open-line 1)
			  (forward-line 1)
			  (insert comment-end)))))
	      (insert (concat " " word))))))))


(defconst ispell-version "ispell.el 3.2 -- Fri May  7 14:25:14 PDT 1999")

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
;;; Local IspellDict: "american"
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
; LocalWords:  lns

;;; ispell.el ends here
