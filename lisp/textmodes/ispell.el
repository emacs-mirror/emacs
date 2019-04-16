;;; ispell.el --- interface to spell checkers  -*- lexical-binding:t -*-

;; Copyright (C) 1994-1995, 1997-2019 Free Software Foundation, Inc.

;; Author:           Ken Stevens <k.stevens@ieee.org>

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

;; INSTRUCTIONS

;;   Use the variable `ispell-local-dictionary-alist' to specify
;; your own dictionaries.

;;  Depending on the mail system you use, you may want to include these:
;;  (add-hook 'news-inews-hook #'ispell-message)
;;  (add-hook 'mail-send-hook  #'ispell-message)
;;  (add-hook 'mh-before-send-letter-hook #'ispell-message)

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
;; `C-z': suspend Emacs or iconify frame

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
;;    evil "misalignment error" in some versions of Emacs.
;;  On some versions of Emacs, growing the minibuffer fails.
;;    see `ispell-help-in-bufferp'.
;;  Recursive edits (?C-r or ?R) inside a keyboard text replacement check (?r)
;;    can cause misalignment errors.

;;; Code:

(eval-when-compile (require 'cl-lib))
(eval-when-compile (require 'subr-x))

(defvar mail-yank-prefix)

(defgroup ispell nil
  "User variables for Emacs ispell interface."
  :group 'applications)

(defalias 'check-ispell-version 'ispell-check-version)

(declare-function flyspell-unhighlight-at "flyspell" (pos))

;;; **********************************************************************
;;; The following variables should be set according to personal preference
;;; and location of binaries:
;;; **********************************************************************


;;;  ******* THIS FILE IS WRITTEN FOR ISPELL VERSION 3.1+

(defcustom ispell-highlight-p 'block
  "Highlight spelling errors when non-nil.
When set to `block', assumes a block cursor with TTY displays."
  :type '(choice (const block) (const :tag "off" nil) (const :tag "on" t))
  :group 'ispell)

(defcustom ispell-lazy-highlight (boundp 'lazy-highlight-cleanup)
  "Controls the lazy-highlighting of spelling errors.
When non-nil, all text in the buffer matching the current spelling
error is highlighted lazily using isearch lazy highlighting (see
`lazy-highlight-initial-delay' and `lazy-highlight-interval')."
  :type 'boolean
  :group 'lazy-highlight
  :group 'ispell
  :version "22.1")

(defcustom ispell-highlight-face (if ispell-lazy-highlight 'isearch 'highlight)
  "Face used for Ispell highlighting.
This variable can be set by the user to whatever face they desire.
It's most convenient if the cursor color and highlight color are
slightly different."
  :type 'face
  :group 'ispell)

(defcustom ispell-check-comments t
  "Spelling of comments checked when non-nil.
When set to `exclusive', ONLY comments are checked.  (For code comments).
Warning!  Not checking comments, when a comment start is embedded in strings,
may produce undesired results."
  :type '(choice (const exclusive) (const :tag "off" nil) (const :tag "on" t))
  :group 'ispell)
;;;###autoload
(put 'ispell-check-comments 'safe-local-variable
     (lambda (a) (memq a '(nil t exclusive))))

(defcustom ispell-query-replace-choices nil
  "Corrections made throughout region when non-nil.
Uses `query-replace' (\\[query-replace]) for corrections."
  :type 'boolean
  :group 'ispell)

(defcustom ispell-skip-tib nil
  "Does not spell check `tib' bibliography references when non-nil.
Skips any text between strings matching regular expressions
`ispell-tib-ref-beginning' and `ispell-tib-ref-end'.

TeX users beware:  Any text between [. and .] will be skipped -- even if
that's your whole buffer -- unless you set `ispell-skip-tib' to nil.
That includes the [.5mm] type of number..."
  :type 'boolean
  :group 'ispell)

(defvar ispell-tib-ref-beginning "[[<]\\."
  "Regexp matching the beginning of a Tib reference.")

(defvar ispell-tib-ref-end "\\.[]>]"
  "Regexp matching the end of a Tib reference.")

(defcustom ispell-keep-choices-win t
  "If non-nil, keep the `*Choices*' window for the entire spelling session.
This minimizes redisplay thrashing."
  :type 'boolean
  :group 'ispell)

(defcustom ispell-choices-win-default-height 2
  "The default size of the `*Choices*' window, including the mode line.
Must be greater than 1."
  :type 'integer
  :group 'ispell)

;; XXX Add enchant to this list once enchant >= 2.1.0 is widespread.
;; Before that, adding it is useless, as if it is found, it will just
;; cause an error; and one of the other spelling engines below is
;; almost certainly installed in any case, for enchant to use.
(defcustom ispell-program-name
  (or (executable-find "aspell")
      (executable-find "ispell")
      (executable-find "hunspell")
      "ispell")
  "Program invoked by \\[ispell-word] and \\[ispell-region] commands."
  :type 'string
  :set (lambda (symbol value)
         (set-default symbol value)
         (if (featurep 'ispell)
             (ispell-set-spellchecker-params)))
  :group 'ispell)

(defcustom ispell-alternate-dictionary
  (cond ((file-readable-p "/usr/dict/web2") "/usr/dict/web2")
	((file-readable-p "/usr/share/dict/web2") "/usr/share/dict/web2")
	((file-readable-p "/usr/dict/words") "/usr/dict/words")
	((file-readable-p "/usr/lib/dict/words") "/usr/lib/dict/words")
	((file-readable-p "/usr/share/dict/words") "/usr/share/dict/words")
	((file-readable-p "/usr/share/lib/dict/words")
	 "/usr/share/lib/dict/words")
	((file-readable-p "/sys/dict") "/sys/dict"))
  "Alternate plain word-list dictionary for spelling help."
  :type '(choice file (const :tag "None" nil))
  :group 'ispell)

(defcustom ispell-complete-word-dict nil
  "Plain word-list dictionary used for word completion if
different from `ispell-alternate-dictionary'."
  :type '(choice file (const :tag "None" nil))
  :group 'ispell)

(defcustom ispell-message-dictionary-alist nil
  "List used by `ispell-message' to select a new dictionary.
It consists of pairs (REGEXP . DICTIONARY).  If REGEXP is found
in the message headers, `ispell-local-dictionary' will be set to
DICTIONARY if `ispell-local-dictionary' is not buffer-local.
E.g. you may use the following value:
   ((\"^Newsgroups:[ \\t]*de\\\\.\" . \"deutsch8\")
    (\"^To:[^\\n,]+\\\\.de[ \\t\\n,>]\" . \"deutsch8\"))"
  :type '(repeat (cons regexp string))
  :group 'ispell)


(defcustom ispell-message-fcc-skip 50000
  "Query before saving Fcc message copy if attachment larger than this value.
Always stores Fcc copy of message when nil."
  :type '(choice integer (const :tag "off" nil))
  :group 'ispell)


(defcustom ispell-grep-command
  "grep"
  "Name of the grep command for search processes."
  :type 'string
  :group 'ispell)

(defcustom ispell-grep-options
  "-Ei"
  "String of options to use when running the program in `ispell-grep-command'.
Should probably be \"-Ei\"."
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
  "Non-nil means use `look' rather than `grep'.
Default is based on whether `look' seems to be available."
  :type 'boolean
  :group 'ispell)

(defcustom ispell-have-new-look nil
  "Non-nil means use the `-r' option (regexp) when running `look'."
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
  "Non-nil means `ispell-word' checks the word around or after point.
Otherwise `ispell-word' checks the preceding word."
  :type 'boolean
  :group 'ispell)

(defcustom ispell-help-in-bufferp nil
  "Non-nil means display interactive keymap help in a buffer.
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
  "Non-nil means suppress messages in `ispell-word'."
  :type 'boolean
  :group 'ispell)

(defvaralias 'ispell-format-word 'ispell-format-word-function)

(defcustom ispell-format-word-function (function upcase)
  "Formatting function for displaying word being spell checked.
The function must take one string argument and return a string."
  :type 'function
  :group 'ispell)

;; FIXME framepop.el last updated c 2003 (?),
;; probably something else replaces it these days.
(defcustom ispell-use-framepop-p nil
  "When non-nil ispell uses framepop to display choices in a dedicated frame.
You can set this variable to dynamically use framepop if you are in a
window system by evaluating the following on startup to set this variable:
  (and (display-graphic-p) (require \\='framepop nil t))"
  :type 'boolean
  :group 'ispell)

;;;###autoload
(defcustom ispell-personal-dictionary nil
  "File name of your personal spelling dictionary, or nil.
If nil, the default personal dictionary for your spelling checker is used."
  :type '(choice file
		 (const :tag "default" nil))
  :group 'ispell)

(defcustom ispell-silently-savep nil
  "When non-nil, save personal dictionary without asking for confirmation."
  :type 'boolean
  :group 'ispell)

(defvar ispell-local-dictionary-overridden nil
  "Non-nil means the user has explicitly set this buffer's Ispell dictionary.")
(make-variable-buffer-local 'ispell-local-dictionary-overridden)

(defcustom ispell-local-dictionary nil
  "If non-nil, the dictionary to be used for Ispell commands in this buffer.
The value must be a string dictionary name,
or nil, which means use the global setting in `ispell-dictionary'.
Dictionary names are defined in `ispell-local-dictionary-alist'
and `ispell-dictionary-alist'.

Setting `ispell-local-dictionary' to a value has the same effect as
calling \\[ispell-change-dictionary] with that value.  This variable
is automatically set when defined in the file with either
`ispell-dictionary-keyword' or the Local Variable syntax."
  :type '(choice string
		 (const :tag "default" nil))
  :group 'ispell)
;;;###autoload
(put 'ispell-local-dictionary 'safe-local-variable 'string-or-null-p)

(make-variable-buffer-local 'ispell-local-dictionary)

(defcustom ispell-dictionary nil
  "Default dictionary to use if `ispell-local-dictionary' is nil."
  :type '(choice string
		 (const :tag "default" nil))
  :group 'ispell)

(defcustom ispell-extra-args nil
  "If non-nil, a list of extra switches to pass to the Ispell program.
For example, (\"-W\" \"3\") to cause it to accept all 1-3 character
words as correct.  See also `ispell-dictionary-alist', which may be used
for language-specific arguments."
  :type '(repeat string)
  :group 'ispell)



(defcustom ispell-skip-html 'use-mode-name
  "Indicates whether ispell should skip spell checking of SGML markup.
If t, always skip SGML markup; if nil, never skip; if non-t and non-nil,
guess whether SGML markup should be skipped according to the name of the
buffer's major mode."
  :type '(choice (const :tag "always" t) (const :tag "never" nil)
		 (const :tag "use-mode-name" use-mode-name))
  :group 'ispell)

(make-variable-buffer-local 'ispell-skip-html)


(defcustom ispell-local-dictionary-alist nil
  "List of local or customized dictionary definitions.
These can override the values in `ispell-dictionary-alist'.

To make permanent changes to your dictionary definitions, you
will need to make your changes in this variable, save, and then
re-start Emacs."
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
		       (coding-system :tag "Coding System")))
  :group 'ispell)


(defvar ispell-dictionary-base-alist
  '((nil                                ; default
     ;; The default dictionary.  It may be English.aff, or any other
     ;; dictionary depending on locale and such things.  We should probably
     ;; ask ispell what dictionary it's using, but until we do that, let's
     ;; just use a minimal regexp. [:alpha:] will later be set if possible.
     "[A-Za-z]" "[^A-Za-z]" "[']" nil ("-B") nil iso-8859-1)
    ("american"				; Yankee English
     "[A-Za-z]" "[^A-Za-z]" "[']" nil ("-B") nil iso-8859-1)
    ("brasileiro"			; Brazilian mode
     "[A-Z\301\311\315\323\332\300\310\314\322\331\303\325\307\334\302\312\324a-z\341\351\355\363\372\340\350\354\362\371\343\365\347\374\342\352\364]"
     "[^A-Z\301\311\315\323\332\300\310\314\322\331\303\325\307\334\302\312\324a-z\341\351\355\363\372\340\350\354\362\371\343\365\347\374\342\352\364]"
     "[']" nil nil nil iso-8859-1)
    ("british"				; British version
     "[A-Za-z]" "[^A-Za-z]" "[']" nil ("-B") nil iso-8859-1)
    ("castellano"			; Spanish mode
     "[A-Z\301\311\315\321\323\332\334a-z\341\351\355\361\363\372\374]"
     "[^A-Z\301\311\315\321\323\332\334a-z\341\351\355\361\363\372\374]"
     "[-]" nil ("-B") "~tex" iso-8859-1)
    ("castellano8"			; 8 bit Spanish mode
     "[A-Z\301\311\315\321\323\332\334a-z\341\351\355\361\363\372\374]"
     "[^A-Z\301\311\315\321\323\332\334a-z\341\351\355\361\363\372\374]"
     "[-]" nil ("-B" "-d" "castellano") "~latin1" iso-8859-1)
    ("czech"
     "[A-Za-z\301\311\314\315\323\332\331\335\256\251\310\330\317\253\322\341\351\354\355\363\372\371\375\276\271\350\370\357\273\362]"
     "[^A-Za-z\301\311\314\315\323\332\331\335\256\251\310\330\317\253\322\341\351\354\355\363\372\371\375\276\271\350\370\357\273\362]"
     "" nil ("-B") nil iso-8859-2)
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
     "[A-Za-z]" "[^A-Za-z]" "[']" nil ("-B") nil iso-8859-1)
    ("esperanto"
     "[A-Za-z\246\254\266\274\306\330\335\336\346\370\375\376]"
     "[^A-Za-z\246\254\266\274\306\330\335\336\346\370\375\376]"
     "[-']" t ("-C") "~latin3" iso-8859-3)
    ("esperanto-tex"
     "[A-Za-z^\\]" "[^A-Za-z^\\]"
     "[-'`\"]" t ("-C" "-d" "esperanto") "~tex" iso-8859-3)
    ("finnish"
     "[A-Za-z\345\344\366\305\304\326]"
     "[^A-Za-z\345\344\366\305\304\326]"
     "[:]" nil ("-C") "~list" iso-8859-1)
    ("francais7"
     "[A-Za-z]" "[^A-Za-z]" "[`'^-]" t nil nil iso-8859-1)
    ("francais"				; Francais.aff
     "[A-Za-z\300\302\306\307\310\311\312\313\316\317\324\331\333\334\340\342\347\350\351\352\353\356\357\364\371\373\374]"
     "[^A-Za-z\300\302\306\307\310\311\312\313\316\317\324\331\333\334\340\342\347\350\351\352\353\356\357\364\371\373\374]"
     "[-'.@]" t nil "~list" iso-8859-1)
    ("francais-tex"			; Francais.aff
     "[A-Za-z\300\302\306\307\310\311\312\313\316\317\324\331\333\334\340\342\347\350\351\352\353\356\357\364\371\373\374\\]"
     "[^A-Za-z\300\302\306\307\310\311\312\313\316\317\324\331\333\334\340\342\347\350\351\352\353\356\357\364\371\373\374\\]"
     "[-'^`\".@]" t nil "~tex" iso-8859-1)
    ("german"				; german.aff
     "[a-zA-Z\"]" "[^a-zA-Z\"]" "[']" t ("-C") "~tex" iso-8859-1)
    ("german8"				; german.aff
     "[a-zA-Z\304\326\334\344\366\337\374]"
     "[^a-zA-Z\304\326\334\344\366\337\374]"
     "[']" t ("-C" "-d" "german") "~latin1" iso-8859-1)
    ("italiano"				; Italian.aff
     "[A-Z\300\301\310\311\314\315\322\323\331\332a-z\340\341\350\351\354\355\363\371\372]"
     "[^A-Z\300\301\310\311\314\315\322\323\331\332a-z\340\341\350\351\354\355\363\371\372]"
     "[-.]" nil ("-B" "-d" "italian") "~tex" iso-8859-1)
    ("nederlands"			; Nederlands.aff
     "[A-Za-z\300\301\302\303\304\305\307\310\311\312\313\314\315\316\317\322\323\324\325\326\331\332\333\334\340\341\342\343\344\345\347\350\351\352\353\354\355\356\357\361\362\363\364\365\366\371\372\373\374]"
     "[^A-Za-z\300\301\302\303\304\305\307\310\311\312\313\314\315\316\317\322\323\324\325\326\331\332\333\334\340\341\342\343\344\345\347\350\351\352\353\354\355\356\357\361\362\363\364\365\366\371\372\373\374]"
     "[']" t ("-C") nil iso-8859-1)
    ("nederlands8"			; Dutch8.aff
     "[A-Za-z\300\301\302\303\304\305\307\310\311\312\313\314\315\316\317\322\323\324\325\326\331\332\333\334\340\341\342\343\344\345\347\350\351\352\353\354\355\356\357\361\362\363\364\365\366\371\372\373\374]"
     "[^A-Za-z\300\301\302\303\304\305\307\310\311\312\313\314\315\316\317\322\323\324\325\326\331\332\333\334\340\341\342\343\344\345\347\350\351\352\353\354\355\356\357\361\362\363\364\365\366\371\372\373\374]"
     "[']" t ("-C") nil iso-8859-1)
    ("norsk"				; 8 bit Norwegian mode
     "[A-Za-z\305\306\307\310\311\322\324\330\345\346\347\350\351\362\364\370]"
     "[^A-Za-z\305\306\307\310\311\322\324\330\345\346\347\350\351\362\364\370]"
     "[\"]" nil nil "~list" iso-8859-1)
    ("norsk7-tex"			; 7 bit Norwegian TeX mode
     "[A-Za-z{}\\'^`]" "[^A-Za-z{}\\'^`]"
     "[\"]" nil ("-d" "norsk") "~plaintex" iso-8859-1)
    ("polish"				; Polish mode
     "[A-Za-z\241\243\246\254\257\261\263\266\274\277\306\312\321\323\346\352\361\363]"
     "[^A-Za-z\241\243\246\254\257\261\263\266\274\277\306\312\321\323\346\352\361\363]"
     "[.]" nil nil nil iso-8859-2)
    ("portugues"                        ; Portuguese mode
     "[a-zA-Z\301\302\307\311\323\340\341\342\351\352\355\363\343\347\372]"
     "[^a-zA-Z\301\302\307\311\323\340\341\342\351\352\355\363\343\347\372]"
     "[']" t ("-C") "~latin1" iso-8859-1)
    ("russian"				; Russian.aff (KOI8-R charset)
     "[\341\342\367\347\344\345\263\366\372\351\352\353\354\355\356\357\360\362\363\364\365\346\350\343\376\373\375\370\371\377\374\340\361\301\302\327\307\304\305\243\326\332\311\312\313\314\315\316\317\320\322\323\324\325\306\310\303\336\333\335\330\331\337\334\300\321]"
     "[^\341\342\367\347\344\345\263\366\372\351\352\353\354\355\356\357\360\362\363\364\365\346\350\343\376\373\375\370\371\377\374\340\361\301\302\327\307\304\305\243\326\332\311\312\313\314\315\316\317\320\322\323\324\325\306\310\303\336\333\335\330\331\337\334\300\321]"
     "" nil nil nil koi8-r)
    ("russianw"				; russianw.aff (CP1251 charset)
     "[\300\301\302\303\304\305\250\306\307\310\311\312\313\314\315\316\317\320\321\322\323\324\325\326\327\330\331\334\333\332\335\336\337\340\341\342\343\344\345\270\346\347\350\351\352\353\354\355\356\357\360\361\362\363\364\365\366\367\370\371\374\373\372\375\376\377]"
     "[^\300\301\302\303\304\305\250\306\307\310\311\312\313\314\315\316\317\320\321\322\323\324\325\326\327\330\331\334\333\332\335\336\337\340\341\342\343\344\345\270\346\347\350\351\352\353\354\355\356\357\360\361\362\363\364\365\366\367\370\371\374\373\372\375\376\377]"
     "" nil nil nil windows-1251)
    ("slovak"				; Slovakian
     "[A-Za-z\301\304\311\315\323\332\324\300\305\245\335\256\251\310\317\253\322\341\344\351\355\363\372\364\340\345\265\375\276\271\350\357\273\362]"
     "[^A-Za-z\301\304\311\315\323\332\324\300\305\245\335\256\251\310\317\253\322\341\344\351\355\363\372\364\340\345\265\375\276\271\350\357\273\362]"
     "" nil ("-B") nil iso-8859-2)
    ("slovenian"                        ; Slovenian
     "[A-Za-z\301\304\311\315\323\332\324\300\305\245\335\256\251\310\317\253\322\341\344\351\355\363\372\364\340\345\265\375\276\271\350\357\273\362]"
     "[^A-Za-z\301\304\311\315\323\332\324\300\305\245\335\256\251\310\317\253\322\341\344\351\355\363\372\364\340\345\265\375\276\271\350\357\273\362]"
     "" nil ("-B" "-d" "slovenian") nil iso-8859-2)
    ("svenska"				; Swedish mode
     "[A-Za-z\345\344\366\351\340\374\350\346\370\347\305\304\326\311\300\334\310\306\330\307]"
     "[^A-Za-z\345\344\366\351\340\374\350\346\370\347\305\304\326\311\300\334\310\306\330\307]"
     "[']" nil ("-C") "~list" iso-8859-1)
    ("hebrew" "[\340\341\342\343\344\345\346\347\350\351\353\352\354\356\355\360\357\361\362\364\363\367\366\365\370\371\372]" "[^\340\341\342\343\344\345\346\347\350\351\353\352\354\356\355\360\357\361\362\364\363\367\366\365\370\371\372]" "" nil ("-B") nil cp1255))
  "Base value for `ispell-dictionary-alist'.")

(defvar ispell-dictionary-alist nil
  "An alist of dictionaries and their associated parameters.

Each element of this list is also a list:

 (DICTIONARY-NAME CASECHARS NOT-CASECHARS OTHERCHARS MANY-OTHERCHARS-P
        ISPELL-ARGS EXTENDED-CHARACTER-MODE CHARACTER-SET)

DICTIONARY-NAME is a possible string value of variable `ispell-dictionary',
nil means the default dictionary.

CASECHARS is a regular expression of valid characters that comprise a word.

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

CASECHARS, NOT-CASECHARS, and OTHERCHARS must be unibyte strings
containing bytes of CHARACTER-SET.  In addition, if they contain
non-ASCII bytes, the regular expression must be a single
`character set' construct that doesn't specify a character range
for non-ASCII bytes.

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
Both defaults can be overruled in a buffer-local fashion.  See
`ispell-parsing-keyword' for details on this.

CHARACTER-SET used to encode text sent to the ispell subprocess
when the language uses non-ASCII characters.

Note that with \"ispell\" as the speller, the CASECHARS and
OTHERCHARS slots of the alist should contain the same character
set as casechars and otherchars in the LANGUAGE.aff file \(e.g.,
english.aff).  Aspell and Hunspell don't have this limitation.")

(defvar ispell-really-aspell nil
  "Non-nil if we can use Aspell extensions.")
(defvar ispell-really-hunspell nil
  "Non-nil if we can use Hunspell extensions.")
(defvar ispell-really-enchant nil
  "Non-nil if we can use Enchant extensions.")
(defvar ispell-encoding8-command nil
  "Command line option prefix to select encoding if supported, nil otherwise.
If setting the encoding is supported by spellchecker and is selectable from
the command line, this variable will contain \"--encoding=\" for Aspell
and \"-i \" for Hunspell, so the appropriate mime charset can be selected.
That will be set in `ispell-check-version' for Hunspell >= 1.1.6 and
Aspell >= 0.60.

For Aspell, non-nil also means to try to automatically find its dictionaries.

Earlier Aspell versions do not consistently support charset encoding.  Handling
this would require some extra guessing in `ispell-aspell-find-dictionary'.")

(defvar ispell-aspell-supports-utf8 nil
  "Non-nil if Aspell has consistent command line UTF-8 support.  Obsolete.
ispell.el and flyspell.el will use for this purpose the more generic
variable `ispell-encoding8-command' for both Aspell and Hunspell.  Is left
here just for backwards compatibility.")

(make-obsolete-variable 'ispell-aspell-supports-utf8
                        'ispell-encoding8-command "23.1")

(defvar ispell-dicts-name2locale-equivs-alist
  '(("american"      "en_US")
    ("brasileiro"    "pt_BR")
    ("british"       "en_GB")
    ("castellano"    "es_ES")
    ("castellano8"   "es_ES")
    ("czech"         "cs_CZ")
    ("dansk"         "da_DK")
    ("deutsch"       "de_DE")
    ("deutsch8"      "de_DE")
    ("english"       "en_US")
    ("esperanto"     "eo")
    ("esperanto-tex" "eo")
    ("finnish"       "fi_FI")
    ("francais7"     "fr_FR")
    ("francais"      "fr_FR")
    ("francais-tex"  "fr_FR")
    ("german"        "de_DE")
    ("german8"       "de_DE")
    ("italiano"      "it_IT")
    ("nederlands"    "nl_NL")
    ("nederlands8"   "nl_NL")
    ("norsk"         "nn_NO")
    ("norsk7-tex"    "nn_NO")
    ("polish"        "pl_PL")
    ("portugues"     "pt_PT")
    ("russian"       "ru_RU")
    ("russianw"      "ru_RU")
    ("slovak"        "sk_SK")
    ("slovenian"     "sl_SI")
    ("svenska"       "sv_SE")
    ("hebrew"        "he_IL"))
  "Alist with known matching locales for standard dict names in
  `ispell-dictionary-base-alist'.")


;;; **********************************************************************
;;; The following are used by ispell, and should not be changed.
;;; **********************************************************************

(defun ispell-check-version (&optional interactivep)
  "Ensure that `ispell-program-name' is valid and has the correct version.
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
  (let ((default-directory (or (and (boundp 'temporary-file-directory)
				    temporary-file-directory)
			       default-directory))
	(get-config-var
	 (lambda (var)
	   (when (re-search-forward
		  (concat var " = \\\"\\(.+?\\)\\\"") nil t)
	     (match-string 1))))
	result libvar status ispell-program-version)

    (with-temp-buffer
      (setq status (ispell-call-process
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
	  ;; Report version information of ispell
	  (progn
	    (end-of-line)
	    (setq result (buffer-substring-no-properties (point-min)
                                                         (point)))
	    (message "%s" result))
	;; return LIBDIR or LIBRARYVAR (overrides LIBDIR) env.
	(progn
	  (setq result (funcall get-config-var "LIBDIR")
		libvar (funcall get-config-var "LIBRARYVAR"))
	  (when libvar
	    (setq libvar (getenv libvar))
	    (unless (member libvar '(nil "")) (setq result libvar)))))
      (goto-char (point-min))
      (if (not (memq status '(0 nil)))
	  (error "%s exited with %s %s" ispell-program-name
		 (if (stringp status) "signal" "code") status))

      ;; Get relevant version strings.
      (let (case-fold-search)
	(setq ispell-program-version
	      (and (search-forward-regexp "\\([0-9]+\\.[0-9.]+\\)" nil t)
		   (match-string 1)))

	;; Make sure these variables are (re-)initialized to the default value
	(setq ispell-really-aspell nil
              ispell-really-hunspell nil
              ispell-really-enchant nil
	      ispell-encoding8-command nil)

	(goto-char (point-min))
	(or (setq ispell-really-aspell
		  (and
                   (search-forward-regexp
                    "(but really Aspell \\([0-9]+\\.[0-9.]+\\([-._+ ]?[a-zA-Z0-9]+\\)?\\)?)"
                    nil t)
		   (match-string 1)))
	    (setq ispell-really-hunspell
		  (and
                   (search-forward-regexp
		    "(but really Hunspell \\([0-9]+\\.[0-9.]+\\([-._+ ]?[a-zA-Z0-9]+\\)?\\)?)"
                    nil t)
		   (match-string 1)))
            (setq ispell-really-enchant
		  (and
                   (search-forward-regexp
		    "(but really Enchant \\([0-9]+\\.[0-9.]+\\([-._+ ]?[a-zA-Z0-9]+\\)?\\)?)"
                    nil t)
		   (match-string 1)))))

      (let* ((aspell8-minver   "0.60")
             (ispell-minver    "3.1.12")
             (hunspell8-minver "1.1.6")
             (enchant-minver   "2.1.0")
             (minver (cond
                      ((not (version<= ispell-minver ispell-program-version))
                       ispell-minver)
                      ((and ispell-really-aspell
                            (not (version<= aspell8-minver ispell-really-aspell)))
                       aspell8-minver)
                      ((and ispell-really-enchant
                            (not (version<= enchant-minver ispell-really-enchant)))
                       enchant-minver))))

        (if minver
	    (error "%s release %s or greater is required"
                   ispell-program-name
                   minver))

	(cond
	 (ispell-really-aspell
	  (setq ispell-encoding8-command "--encoding="))
	 (ispell-really-hunspell
	  (if (version<= hunspell8-minver ispell-really-hunspell)
	      (setq ispell-encoding8-command "-i")
	    (setq ispell-really-hunspell nil))))))
    result))

(defun ispell-call-process (&rest args)
  "Like `call-process' but defend against bad `default-directory'."
  (let ((default-directory default-directory))
    (unless (file-accessible-directory-p default-directory)
      (setq default-directory (expand-file-name "~/")))
    (apply 'call-process args)))

(defun ispell-call-process-region (&rest args)
  "Like `call-process-region' but defend against bad `default-directory'."
  (let ((default-directory default-directory))
    (unless (file-accessible-directory-p default-directory)
      (setq default-directory (expand-file-name "~/")))
    (apply 'call-process-region args)))

(defvar ispell-debug-buffer)

(defun ispell-create-debug-buffer (&optional append)
  "Create an ispell debug buffer for debugging output.
If APPEND is non-nil, append the info to previous buffer if exists,
otherwise is reset.  Returns name of ispell debug buffer.
See `ispell-buffer-with-debug' for an example of use."
  (let ((ispell-debug-buffer (get-buffer-create "*ispell-debug*")))
    (with-current-buffer ispell-debug-buffer
      (if append
	  (insert
	   (format "-----------------------------------------------\n"))
	(erase-buffer)))
    ispell-debug-buffer))

(defsubst ispell-print-if-debug (format &rest args)
  "Print message using FORMAT and ARGS to `ispell-debug-buffer' buffer if enabled."
  (if (boundp 'ispell-debug-buffer)
      (with-current-buffer ispell-debug-buffer
	(goto-char (point-max))
	(insert (apply #'format format args)))))


;; The preparation of the menu bar menu must be autoloaded
;; because otherwise this file gets autoloaded every time Emacs starts
;; so that it can set up the menus and determine keyboard equivalents.

(defvar ispell-library-directory (condition-case ()
				     (ispell-check-version)
				   (error nil))
  "Directory where ispell dictionaries reside.")

(defvar ispell-process nil
  "The process object for Ispell.")

(defvar ispell-async-processp (and (fboundp 'delete-process)
				   (fboundp 'process-send-string)
				   (fboundp 'accept-process-output))
  "Non-nil means that the OS supports asynchronous processes.")

;; Make ispell.el work better with aspell.

(defvar ispell-aspell-dictionary-alist nil
  "An alist of parsed Aspell dicts and associated parameters.
Internal use.")

(defun ispell-find-aspell-dictionaries ()
  "Find Aspell's dictionaries, and record in `ispell-aspell-dictionary-alist'."
  (let* ((dictionaries
	  (split-string
	   (with-temp-buffer
	     (ispell-call-process ispell-program-name nil t nil "dicts")
	     (buffer-string))))
	 ;; Search for the named dictionaries.
	 (found
	  (delq nil
		(mapcar #'ispell-aspell-find-dictionary dictionaries))))
    ;; Ensure aspell's alias dictionary will override standard
    ;; definitions.
    (setq found (ispell-aspell-add-aliases found))
    ;; Merge into FOUND any elements from the standard ispell-dictionary-base-alist
    ;; which have no element in FOUND at all.
    (dolist (dict ispell-dictionary-base-alist)
      (unless (assoc (car dict) found)
	(setq found (nconc found (list dict)))))
    (setq ispell-aspell-dictionary-alist found)
    ;; Add a default entry
    (let ((default-dict
           '(nil "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-B") nil utf-8)))
      (push default-dict ispell-aspell-dictionary-alist))))

(defvar ispell-aspell-data-dir nil
  "Data directory of Aspell.")

(defvar ispell-aspell-dict-dir nil
  "Dictionary directory of Aspell.")

(defun ispell-get-aspell-config-value (key)
  "Return value of Aspell configuration option KEY.
Assumes that value contains no whitespace."
  (with-temp-buffer
    (ispell-call-process ispell-program-name nil t nil "config" key)
    (car (split-string (buffer-string)))))

(defun ispell-aspell-find-dictionary (dict-name)
  "For Aspell dictionary DICT-NAME, return a list of parameters if an
associated data file is found or nil otherwise.  List format is that
of `ispell-dictionary-base-alist' elements."

  ;; Make sure `ispell-aspell-dict-dir' is defined
  (or ispell-aspell-dict-dir
      (setq ispell-aspell-dict-dir
	    (ispell-get-aspell-config-value "dict-dir")))

  ;; Make sure `ispell-aspell-data-dir' is defined
  (or ispell-aspell-data-dir
      (setq ispell-aspell-data-dir
	    (ispell-get-aspell-config-value "data-dir")))

  ;; Try finding associated datafile. aspell will look for master .dat
  ;; file in `dict-dir' and `data-dir'. Associated .dat files must be
  ;; in the same directory as master file.
  (let ((data-file
	 (catch 'datafile
	   (dolist ( tmp-path (list ispell-aspell-dict-dir
				    ispell-aspell-data-dir ))
	     ;; Try xx.dat first, strip out variant, country code, etc,
	     ;; then try xx_YY.dat (without stripping country code),
	     ;; then try xx-alt.dat, for de-alt etc.
	     (dolist (tmp-regexp (list "^[[:alpha:]]+"
				       "^[[:alpha:]_]+"
                                       "^[[:alpha:]]+-\\(alt\\|old\\)"))
	       (let ((fullpath
		      (concat tmp-path "/"
			      (and (string-match tmp-regexp dict-name)
				   (match-string 0 dict-name)) ".dat")))
		 (if (file-readable-p fullpath)
		     (throw 'datafile fullpath)))))))
	otherchars)

    (if data-file
	(with-temp-buffer
	  (insert-file-contents data-file)
	  ;; There is zero or one line with special characters declarations.
	  (when (search-forward-regexp "^special" nil t)
	    (let ((specials (split-string
			     (buffer-substring (point)
					       (progn (end-of-line) (point))))))
	      ;; The line looks like: special ' -** - -** . -** : -*-
	      ;; -** means that this character
	      ;;    - doesn't appear at word start
	      ;;    * may appear in the middle of a word
	      ;;    * may appear at word end
	      ;; `otherchars' is about the middle case.
	      (while specials
		(when (eq (aref (cadr specials) 1) ?*)
		  (push (car specials) otherchars))
		(setq specials (cddr specials)))))
	  (list dict-name
		"[[:alpha:]]"
		"[^[:alpha:]]"
		(regexp-opt otherchars)
		t			     ; We can't tell, so set this to t
		(list "-d" dict-name)
		nil				; aspell doesn't support this
		;; Here we specify the encoding to use while communicating with
		;; aspell.  This doesn't apply to command line arguments, so
		;; just don't pass words to spellcheck as arguments...
		'utf-8)))))

(defun ispell-aspell-add-aliases (alist)
  "Find Aspell's dictionary aliases and add them to dictionary ALIST.
Return the new dictionary alist."
  (let ((aliases
         (file-expand-wildcards
		  (concat (or ispell-aspell-dict-dir
			      (setq ispell-aspell-dict-dir
				    (ispell-get-aspell-config-value "dict-dir")))
			  "/*.alias"))))
    (dolist (alias-file aliases)
      (with-temp-buffer
	(insert-file-contents alias-file)
	;; Look for a line "add FOO.multi", extract FOO
	(when (search-forward-regexp "^add \\([^.]+\\)\\.multi" nil t)
	  (let* ((aliasname (file-name-base alias-file))
		 (already-exists-p (assoc aliasname alist))
		 (realname (match-string 1))
		 (realdict (assoc realname alist)))
	    (when (and realdict (not already-exists-p))
	      (push (cons aliasname (cdr realdict)) alist))))))
    ;; Add entries for standard dict-names with found locale-matching entry
    (dolist (dict-map-entry ispell-dicts-name2locale-equivs-alist)
      (let ((name (car dict-map-entry))
	    (locale (cadr dict-map-entry)))
	(unless (assoc name alist) ;; skip if already present
	  (if (assoc locale alist)
	      (push (cons name (cdr (assoc locale alist))) alist)))))
    alist))

;; Make ispell.el work better with hunspell.

(defvar ispell-hunspell-dict-paths-alist nil
  "Alist of parsed Hunspell dicts and associated affix files.
Will be used to parse corresponding .aff file and create associated
parameters to be inserted into `ispell-hunspell-dictionary-alist'.
Internal use.")

(defvar ispell-hunspell-dictionary-alist nil
  "Alist of parsed Hunspell dicts and associated parameters.
This alist will initially contain names of found dicts.  Associated
parameters will be added when dict is used for the first time.
Internal use.")

(defun ispell-hunspell-fill-dictionary-entry (dict)
  "Fill uninitialized entries in `ispell-dictionary-alist' for DICT and aliases.
Value of those entries will be extracted from Hunspell affix file and used for
all uninitialized dicts using that affix file."
  (if (cadr (assoc dict ispell-dictionary-alist))
      (message "ispell-hfde: Non void entry for %s. Skipping.\n" dict)
    (let ((dict-alias
           (cadr (assoc dict ispell-dicts-name2locale-equivs-alist)))
	  (use-for-dicts (list dict))
	  (dict-args-cdr (cdr (ispell-parse-hunspell-affix-file dict)))
	  newlist)
      ;; Get a list of uninitialized dicts using the same affix file.
      (dolist (dict-equiv-alist-entry ispell-dicts-name2locale-equivs-alist)
	(let ((dict-equiv-key (car dict-equiv-alist-entry))
	      (dict-equiv-value (cadr dict-equiv-alist-entry)))
	  (if (or (member dict dict-equiv-alist-entry)
		  (member dict-alias dict-equiv-alist-entry))
	      (dolist (tmp-dict (list dict-equiv-key dict-equiv-value))
		(if (cadr (assoc tmp-dict ispell-dictionary-alist))
		    (ispell-print-if-debug
                     "ispell-hfde: %s already expanded; skipping.\n" tmp-dict)
		  (cl-pushnew tmp-dict use-for-dicts :test #'equal))))))
      (ispell-print-if-debug
       "ispell-hfde: Filling %s entry.  Use for %s.\n" dict use-for-dicts)
      ;; The final loop.
      (dolist (entry ispell-dictionary-alist)
	(cl-pushnew (if (member (car entry) use-for-dicts)
                        (cons (car entry) dict-args-cdr)
                      entry)
                    newlist :test #'equal))
      (setq ispell-dictionary-alist newlist))))

(defun ispell-parse-hunspell-affix-file (dict-key)
  "Parse Hunspell affix file to extract parameters for DICT-KEY.
Return a list in `ispell-dictionary-alist' format.

DICT_KEY can be in the \"DICT1,DICT2,DICT3\" format, to invoke Hunspell
with a list of dictionaries.  The first dictionary in the list must have
a corresponding .aff affix file; the rest are allowed to have no affix
files, and will then use the affix file of the preceding dictionary that
did."
  (let ((dict-list (split-string dict-key "," t))
        (first-p t)
        (dict-arg "")
        otherchars-list)
    (dolist (dict-key dict-list)
      (let ((affix-file
             (cadr (assoc dict-key ispell-hunspell-dict-paths-alist))))
        (unless affix-file
          (error "ispell-phaf: No matching entry for %s in `ispell-hunspell-dict-paths-alist'.\n" dict-key))
        (if (and first-p (not (file-exists-p affix-file)))
            (error "ispell-phaf: File \"%s\" not found.\n" affix-file))
        (and first-p (setq first-p nil))
        (let ((dict-name (file-name-sans-extension
                          (file-name-nondirectory affix-file)))
              otherchars-string)
          (with-temp-buffer
            (insert-file-contents affix-file)
            (setq otherchars-string
                  (save-excursion
                    (goto-char (point-min))
                    (if (search-forward-regexp "^WORDCHARS +" nil t )
                        (buffer-substring (point)
                                          (progn (end-of-line) (point))))))
            ;; Remove trailing whitespace and extra stuff.  Make list
            ;; if non-nil.
            (if otherchars-string
                (let* ((otherchars-string
                        ;; Remove trailing junk.
                        (substring otherchars-string
                                   0 (string-match " +" otherchars-string)))
                       (chars-list (append otherchars-string nil)))
                  (setq chars-list (delq ?\  chars-list))
                  (dolist (ch chars-list)
                    (cl-pushnew ch otherchars-list :test #'equal)))))
          ;; Cons the argument for the -d switch.
          (setq dict-arg (concat dict-arg
                                 (if (> (length dict-arg) 0) ",")
                                 dict-name)))))

    ;; Fill dict entry
    (list dict-key
          "[[:alpha:]]"
          "[^[:alpha:]]"
          (if otherchars-list
              (regexp-opt (mapcar #'char-to-string otherchars-list))
            "")
          t                   ; many-otherchars-p: We can't tell, set to t.
          (list "-d" dict-arg)
          nil              ; extended-char-mode: not supported by hunspell!
          'utf-8)))

(defun ispell-hunspell-add-multi-dic (dict)
  "Add DICT of the form \"DICT1,DICT2,...\" to `ispell-dictionary-alist'.

Invoke this command before you want to start Hunspell for the first time
with a particular combination of dictionaries.  The first dictionary
in the list must have an affix file where Hunspell affix files are kept."
  (interactive "sMulti-dictionary combination: ")
  ;; Make sure the first dictionary in the list is known to us.
  (let ((first-dict (car (split-string dict "," t))))
    (unless ispell-hunspell-dictionary-alist
      (ispell-find-hunspell-dictionaries)
      (setq ispell-dictionary-alist ispell-hunspell-dictionary-alist))
    (or (assoc first-dict ispell-local-dictionary-alist)
        (assoc first-dict ispell-dictionary-alist)
        (error "Unknown dictionary: %s" first-dict)))
  (cl-pushnew (list dict '()) ispell-dictionary-alist :test #'equal)
  (ispell-hunspell-fill-dictionary-entry dict))

(defun ispell-find-hunspell-dictionaries ()
  "Look for installed Hunspell dictionaries.
Will initialize `ispell-hunspell-dictionary-alist' according
to dictionaries found, and will remove aliases from the list
in `ispell-dicts-name2locale-equivs-alist' if an explicit
dictionary from that list was found."
  (let ((hunspell-found-dicts
	 (split-string
	  (with-temp-buffer
	    (ispell-call-process ispell-program-name
				 null-device
				 t
				 nil
                                 "-D"
                                 ;; Use -a to prevent Hunspell from
                                 ;; trying to initialize its
                                 ;; curses/termcap UI, which causes it
                                 ;; to crash or fail to start in some
                                 ;; MS-Windows ports.
                                 "-a"
                                 ;; Hunspell 1.7.0 (and later?) won't
                                 ;; show LOADED DICTIONARY unless
                                 ;; there's at least one file argument
                                 ;; on the command line.  So we feed
                                 ;; it with the null device.
				 null-device)
	    (buffer-string))
	  "[\n\r]+"
	  t))
	hunspell-default-dict
	hunspell-default-dict-entry
	hunspell-multi-dict)
    (dolist (dict hunspell-found-dicts)
      (let* ((full-name (file-name-nondirectory dict))
	     (basename  (file-name-sans-extension full-name))
	     (affix-file (concat dict ".aff")))
	(if (string-match "\\.aff$" dict)
	    ;; Found default dictionary
	    (progn
	      (if hunspell-default-dict
		  (setq hunspell-multi-dict
			(concat (or hunspell-multi-dict
				    (car hunspell-default-dict))
				"," basename))
		(setq affix-file dict)
		;; FIXME: The cdr of the list we cons below is never
		;; used.  Why do we need a list?
		(setq hunspell-default-dict (list basename affix-file)))
	      (ispell-print-if-debug
	       "++ ispell-fhd: default dict-entry:%s name:%s basename:%s\n"
	       dict full-name basename))
	  (if (and (not (assoc basename ispell-hunspell-dict-paths-alist))
		   (file-exists-p affix-file))
	      ;; Entry has an associated .aff file and no previous value.
	      (let ((affix-file (expand-file-name affix-file)))
		(ispell-print-if-debug
                 "++ ispell-fhd: dict-entry:%s name:%s basename:%s affix-file:%s\n"
                 dict full-name basename affix-file)
		(cl-pushnew (list basename affix-file)
                            ispell-hunspell-dict-paths-alist :test #'equal))
	    (ispell-print-if-debug
             "-- ispell-fhd: Skipping entry: %s\n" dict)))))
    ;; Remove entry from aliases alist if explicit dict was found.
    (let (newlist)
      (dolist (dict ispell-dicts-name2locale-equivs-alist)
	(if (assoc (car dict) ispell-hunspell-dict-paths-alist)
	    (ispell-print-if-debug
             "-- ispell-fhd: Excluding %s alias.  Standalone dict found.\n"
             (car dict))
	  (cl-pushnew dict newlist :test #'equal)))
      (setq ispell-dicts-name2locale-equivs-alist newlist))
    ;; Add known hunspell aliases
    (dolist (dict-equiv ispell-dicts-name2locale-equivs-alist)
      (let ((dict-equiv-key (car dict-equiv))
	    (dict-equiv-value (cadr dict-equiv))
	    (exclude-aliases (list   ;; Exclude TeX aliases
			      "esperanto-tex"
			      "francais7"
			      "francais-tex"
			      "norsk7-tex")))
	(if (and (assoc dict-equiv-value ispell-hunspell-dict-paths-alist)
		 (not (assoc dict-equiv-key ispell-hunspell-dict-paths-alist))
		 (not (member dict-equiv-key exclude-aliases)))
	    (let ((affix-file (cadr (assoc dict-equiv-value
                                           ispell-hunspell-dict-paths-alist))))
	      (ispell-print-if-debug "++ ispell-fhd: Adding alias %s -> %s.\n"
                                     dict-equiv-key affix-file)
	      (cl-pushnew (list dict-equiv-key affix-file)
                          ispell-hunspell-dict-paths-alist :test #'equal)))))
    ;; Parse and set values for default dictionary.
    (setq hunspell-default-dict (or hunspell-multi-dict
				    (car hunspell-default-dict)))
    ;; If hunspell-default-dict is nil, ispell-parse-hunspell-affix-file
    ;; will barf with an error message that doesn't help users figure
    ;; out what is wrong.  Produce an error message that points to the
    ;; root cause of the problem.
    (or hunspell-default-dict
        (error "Can't find Hunspell dictionary with a .aff affix file"))
    (setq hunspell-default-dict-entry
	  (ispell-parse-hunspell-affix-file hunspell-default-dict))
    ;; Create an alist of found dicts with only names, except for default dict.
    (setq ispell-hunspell-dictionary-alist
	  (list (cons nil (cdr hunspell-default-dict-entry))))
    (dolist (dict (mapcar #'car ispell-hunspell-dict-paths-alist))
      (cl-pushnew (if (string= dict hunspell-default-dict)
                      hunspell-default-dict-entry
                    (list dict))
                  ispell-hunspell-dictionary-alist :test #'equal))))

;; Make ispell.el work better with enchant.

(defvar ispell-enchant-dictionary-alist nil
  "An alist of parsed Enchant dicts and associated parameters.
Internal use.")

(defun ispell--call-enchant-lsmod (&rest args)
  "Call enchant-lsmod with ARGS and return the output as string."
  (with-output-to-string
    (with-current-buffer
        standard-output
      (apply #'ispell-call-process
             (replace-regexp-in-string "enchant\\(-[0-9]\\)?\\'"
                                       "enchant-lsmod\\1"
                                       ispell-program-name)
             nil t nil args))))

(defun ispell--get-extra-word-characters (&optional lang)
  "Get the extra word characters for LANG as a character class.
If LANG is omitted, get the extra word characters for the default language."
  (let ((extra (string-trim-right
                (apply 'ispell--call-enchant-lsmod
                       (append '("-word-chars") (if lang `(,lang)))))))
    (if (string= extra "") "" (concat "[" extra "]"))))

(defun ispell-find-enchant-dictionaries ()
  "Find Enchant's dictionaries, and record in `ispell-enchant-dictionary-alist'."
  (let* ((dictionaries
	  (split-string
	   (ispell--call-enchant-lsmod "-list-dicts" (buffer-string)) " ([^)]+)\n"))
         (found
          (mapcar #'(lambda (lang)
                      `(,lang "[[:alpha:]]" "[^[:alpha:]]"
                              ,(ispell--get-extra-word-characters) t nil nil utf-8))
                  dictionaries)))
    ;; Merge into FOUND any elements from the standard ispell-dictionary-base-alist
    ;; which have no element in FOUND at all.
    (dolist (dict ispell-dictionary-base-alist)
      (unless (assoc (car dict) found)
	(setq found (nconc found (list dict)))))
    (setq ispell-enchant-dictionary-alist found)
    ;; Add a default entry
    (let ((default-dict
            `(nil "[[:alpha:]]" "[^[:alpha:]]"
                  ,(ispell--get-extra-word-characters)
                  t nil nil utf-8)))
      (push default-dict ispell-enchant-dictionary-alist))))

;; Set params according to the selected spellchecker

(defvar ispell-last-program-name nil
  "Last value of `ispell-program-name'.  Internal use.")

;; Allow dynamically binding ispell-base-dicts-override-alist as
;; advertised in the doc string of ispell-initialize-spellchecker-hook.
(defvar ispell-base-dicts-override-alist)

(defvar ispell-initialize-spellchecker-hook nil
  "Normal hook run on spellchecker initialization.
This hook is run when a spellchecker is used for the first
time, before `ispell-dictionary-alist' is set.  It is intended for
sysadmins to override entries in `ispell-dictionary-base-alist'
by putting those overrides in `ispell-base-dicts-override-alist', which is
a dynamically scoped var with same format as `ispell-dictionary-alist'.
This alist will not override the auto-detected values (e.g. if a recent
aspell is used along with Emacs).")

(defun ispell-set-spellchecker-params ()
  "Initialize some spellchecker parameters when changed or first used."
  (unless (eq ispell-last-program-name ispell-program-name)
    (ispell-kill-ispell t)
    (if (and (condition-case ()
		 (progn
		   (setq ispell-library-directory (ispell-check-version))
		   t)
	       (error nil))
	     (or ispell-encoding8-command ispell-really-enchant))
	;; auto-detection will only be used if spellchecker is not
	;; ispell and supports a way to set communication to UTF-8.
	(if ispell-really-aspell
	    (or ispell-aspell-dictionary-alist
		(ispell-find-aspell-dictionaries))
	  (if ispell-really-hunspell
	      (or ispell-hunspell-dictionary-alist
		  (ispell-find-hunspell-dictionaries))
            (if ispell-really-enchant
                (or ispell-enchant-dictionary-alist
                    (ispell-find-enchant-dictionaries))))))

    ;; Substitute ispell-dictionary-alist with the list of
    ;; dictionaries corresponding to the given spellchecker.
    ;; With programs that support it, use the list of really
    ;; installed dictionaries and add to it elements of the original
    ;; list that are not present there. Allow distro info.
    (let ((found-dicts-alist
	   (if ispell-encoding8-command
               (if ispell-really-aspell
		   ispell-aspell-dictionary-alist
		 (if ispell-really-hunspell
		     ispell-hunspell-dictionary-alist))
	     (if ispell-really-enchant
                 ispell-enchant-dictionary-alist
               nil)))
	  (ispell-dictionary-base-alist ispell-dictionary-base-alist)
	  ispell-base-dicts-override-alist ; Override only base-dicts-alist
	  all-dicts-alist)

      ;; While ispell and aspell (through aliases) use the traditional
      ;; dict naming originally expected by ispell.el, hunspell & Enchant
      ;; use locale-based names with no alias.  We need to map
      ;; standard names to locale based names to make default dict
      ;; definitions available to these programs.
      (if (or ispell-really-hunspell ispell-really-enchant)
	  (let (tmp-dicts-alist)
	    (dolist (adict ispell-dictionary-base-alist)
	      (let* ((dict-name (nth 0 adict))
		     (dict-equiv
		      (cadr (assoc dict-name
				   ispell-dicts-name2locale-equivs-alist)))
		     (ispell-args (nth 5 adict))
		     (ispell-args-has-d (member "-d" ispell-args))
		     skip-dict)
		;; Remove "-d" option from `ispell-args' if present
		(if ispell-args-has-d
		    (let ((ispell-args-after-d
			   (cdr (cdr ispell-args-has-d)))
			  (ispell-args-before-d
			   (butlast ispell-args (length ispell-args-has-d))))
		      (setq ispell-args
			    (nconc ispell-args-before-d
				   ispell-args-after-d))))
		;; Unless default dict, re-add "-d" option with the mapped value
		(if dict-name
		    (if dict-equiv
			(setq ispell-args
			      (nconc ispell-args (list "-d" dict-equiv)))
		      (message
		       "ispell-set-spellchecker-params: Missing equivalent for \"%s\". Skipping."
		       dict-name)
		      (setq skip-dict t)))

		(unless skip-dict
		  (cl-pushnew (list
                               dict-name      ; dict name
                               (nth 1 adict)  ; casechars
                               (nth 2 adict)  ; not-casechars
                               (nth 3 adict)  ; otherchars
                               (nth 4 adict)  ; many-otherchars-p
                               ispell-args    ; ispell-args
                               (nth 6 adict)  ; extended-character-mode
                               (nth 7 adict)  ; dict encoding
                               )
                              tmp-dicts-alist :test #'equal)))
	      (setq ispell-dictionary-base-alist tmp-dicts-alist))))

      (run-hooks 'ispell-initialize-spellchecker-hook)

      ;; Add dicts to `ispell-dictionary-alist' unless already present.
      (dolist (dict (append found-dicts-alist
			    ispell-base-dicts-override-alist
			    ispell-dictionary-base-alist))
	(unless (assoc (car dict) all-dicts-alist)
	  (push dict all-dicts-alist)))
      (setq ispell-dictionary-alist all-dicts-alist))

    ;; If spellchecker supports UTF-8 via command-line option, use it
    ;; in communication.  This does not affect definitions in your
    ;; init file.
    (let (tmp-dicts-alist)
      (dolist (adict ispell-dictionary-alist)
        (cl-pushnew (if (cadr adict) ;; Do not touch hunspell uninitialized entries
                        (list
                         (nth 0 adict)   ; dict name
                         (nth 1 adict)   ; casechars
                         (nth 2 adict)   ; not-casechars
                         (nth 3 adict)   ; otherchars
                         (nth 4 adict)   ; many-otherchars-p
                         (nth 5 adict)   ; ispell-args
                         (nth 6 adict)   ; extended-character-mode
                         (if (or ispell-encoding8-command ispell-really-enchant)
                             'utf-8
                           (nth 7 adict)))
                      adict)
                    tmp-dicts-alist :test #'equal))
      (setq ispell-dictionary-alist tmp-dicts-alist)))
      (setq ispell-last-program-name ispell-program-name))

(defun ispell-valid-dictionary-list ()
  "Return a list of valid dictionaries.
The variable `ispell-library-directory' defines their location."
  ;; Initialize variables and dictionaries alists for desired spellchecker.
  ;; Make sure ispell.el is loaded to avoid some autoload loops.
  (if (featurep 'ispell)
      (ispell-set-spellchecker-params))

  (let ((dicts (append ispell-local-dictionary-alist ispell-dictionary-alist))
	(dict-list (cons "default" nil))
	(dict-locate
	 (lambda (dict &optional dir)
	   (locate-file (file-name-nondirectory dict)
			`(,(or dir (file-name-directory dict)))
			(unless (file-name-extension dict) '(".hash" ".has")))))
	name dict-explt dict-bname)
    (dolist (dict dicts)
      (setq name (car dict)
	    ;; Explicitly (via ispell-args) specified dictionary.
	    dict-explt (car (cdr (member "-d" (nth 5 dict))))
	    dict-bname (or dict-explt name))
      (if (and name
	       (or
		;; Include all for Aspell (we already know existing dicts)
		ispell-really-aspell
		;; Include all if `ispell-library-directory' is nil (Hunspell)
		(not ispell-library-directory)
		;; If explicit (-d with an absolute path) and existing dict.
		(and dict-explt
		     (file-name-absolute-p dict-explt)
		     (funcall dict-locate dict-explt))
		;; If dict located in `ispell-library-directory'.
		(funcall dict-locate dict-bname ispell-library-directory)))
	  (push name dict-list)))
    dict-list))

;;;###autoload
(defconst ispell-menu-map
  ;; Use `defconst' so as to redo the menu when loading ispell, like the
  ;; previous code did.

  ;; Define commands in menu in opposite order you want them to appear.
  (let ((map (make-sparse-keymap "Spell")))
    (define-key map [ispell-change-dictionary]
      `(menu-item ,(purecopy "Change Dictionary...") ispell-change-dictionary
		  :help ,(purecopy "Supply explicit dictionary file name")))
    (define-key map [ispell-kill-ispell]
      `(menu-item ,(purecopy "Kill Process")
		  (lambda () (interactive) (ispell-kill-ispell nil 'clear))
		  :enable (and (boundp 'ispell-process) ispell-process
			       (eq (ispell-process-status) 'run))
		  :help ,(purecopy "Terminate Ispell subprocess")))
    (define-key map [ispell-pdict-save]
      `(menu-item ,(purecopy "Save Dictionary")
		  (lambda () (interactive) (ispell-pdict-save t t))
		  :help ,(purecopy "Save personal dictionary")))
    (define-key map [ispell-customize]
      `(menu-item ,(purecopy "Customize...")
		  (lambda () (interactive) (customize-group 'ispell))
		  :help ,(purecopy "Customize spell checking options")))
    (define-key map [ispell-help]
      ;; use (x-popup-menu last-nonmenu-event(list "" ispell-help-list)) ?
      `(menu-item ,(purecopy "Help")
		  (lambda () (interactive) (describe-function 'ispell-help))
		  :help ,(purecopy "Show standard Ispell keybindings and commands")))
    (define-key map [flyspell-mode]
      `(menu-item ,(purecopy "Automatic spell checking (Flyspell)")
		  flyspell-mode
		  :help ,(purecopy "Check spelling while you edit the text")
		  :button (:toggle . (bound-and-true-p flyspell-mode))))
    (define-key map [ispell-complete-word]
      `(menu-item ,(purecopy "Complete Word") ispell-complete-word
		  :help ,(purecopy "Complete word at cursor using dictionary")))
    (define-key map [ispell-complete-word-interior-frag]
      `(menu-item ,(purecopy "Complete Word Fragment")
                  ispell-complete-word-interior-frag
		  :help ,(purecopy "Complete word fragment at cursor")))

    (define-key map [ispell-continue]
      `(menu-item ,(purecopy "Continue Spell-Checking") ispell-continue
		  :enable (and (boundp 'ispell-region-end)
			       (marker-position ispell-region-end)
			       (equal (marker-buffer ispell-region-end)
				      (current-buffer)))
		  :help ,(purecopy "Continue spell checking last region")))
    (define-key map [ispell-word]
      `(menu-item ,(purecopy "Spell-Check Word") ispell-word
		  :help ,(purecopy "Spell-check word at cursor")))
    (define-key map [ispell-comments-and-strings]
      `(menu-item ,(purecopy "Spell-Check Comments")
                  ispell-comments-and-strings
		  :help ,(purecopy "Spell-check only comments and strings")))

    (define-key map [ispell-region]
      `(menu-item ,(purecopy "Spell-Check Region") ispell-region
		  :enable mark-active
		  :help ,(purecopy "Spell-check text in marked region")))
    (define-key map [ispell-message]
      `(menu-item ,(purecopy "Spell-Check Message") ispell-message
		  :visible (eq major-mode 'mail-mode)
		  :help ,(purecopy "Skip headers and included message text")))
    (define-key map [ispell-buffer]
      `(menu-item ,(purecopy "Spell-Check Buffer") ispell-buffer
		  :help ,(purecopy "Check spelling of selected buffer")))
    map)
  "Key map for ispell menu.")
;;;###autoload
(fset 'ispell-menu-map (symbol-value 'ispell-menu-map))

;;; **********************************************************************

(defvar ispell-current-dictionary nil
  "The name of the current dictionary, or nil for the default.
This is passed to the Ispell process using the `-d' switch and is
used as key in `ispell-local-dictionary-alist' and `ispell-dictionary-alist'.")

(defvar ispell-current-personal-dictionary nil
  "The name of the current personal dictionary, or nil for the default.
This is passed to the Ispell process using the `-p' switch.")

;; Return a string decoded from Nth element of the current dictionary.
(defun ispell-get-decoded-string (n)
  "Get the decoded string in slot N of the descriptor of the current dict."
  (let* ((slot (or
		(assoc ispell-current-dictionary ispell-local-dictionary-alist)
		(assoc ispell-current-dictionary ispell-dictionary-alist)
		(error "No data for dictionary \"%s\" in `ispell-local-dictionary-alist' or `ispell-dictionary-alist'"
		       ispell-current-dictionary)))
         (str (nth n slot)))
    (if (stringp str)
        (decode-coding-string str (ispell-get-coding-system) t))))

(defun ispell-get-casechars ()
  (ispell-get-decoded-string 1))
(defun ispell-get-not-casechars ()
  (ispell-get-decoded-string 2))
(defun ispell-get-otherchars ()
  (ispell-get-decoded-string 3))
(defun ispell-get-many-otherchars-p ()
  (nth 4 (or (assoc ispell-current-dictionary ispell-local-dictionary-alist)
	     (assoc ispell-current-dictionary ispell-dictionary-alist))))
(defun ispell-get-ispell-args ()
  (nth 5 (or (assoc ispell-current-dictionary ispell-local-dictionary-alist)
	     (assoc ispell-current-dictionary ispell-dictionary-alist))))
(defun ispell-get-extended-character-mode ()
  (if ispell-really-hunspell     ;; hunspell treats ~word as ordinary words
      nil                        ;; in pipe mode. Disable extended-char-mode
    (nth 6 (or (assoc ispell-current-dictionary ispell-local-dictionary-alist)
	       (assoc ispell-current-dictionary ispell-dictionary-alist)))))
(defun ispell-get-coding-system ()
  (nth 7 (or (assoc ispell-current-dictionary ispell-local-dictionary-alist)
	     (assoc ispell-current-dictionary ispell-dictionary-alist))))


(defvar ispell-pdict-modified-p nil
  "Non-nil means personal dictionary has modifications to be saved.")

;; If you want to save the dictionary when quitting, must do so explicitly.
;; When non-nil, the spell session is terminated.
;; When numeric, contains cursor location in buffer, and cursor remains there.
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
  "Non-nil when we're checking a mail message.
Set to the MIME boundary locations when checking messages.")

(defconst ispell-choices-buffer "*Choices*")

(defvar ispell-overlay nil "Overlay variable for Ispell highlighting.")

;;; *** Buffer Local Definitions ***

(defconst ispell-words-keyword "LocalWords: "
  "The keyword for local oddly-spelled words to accept.
The keyword will be followed by any number of local word spellings.
There can be multiple instances of this keyword in the file.")

(defconst ispell-dictionary-keyword "Local IspellDict: "
  "The keyword for a local dictionary to use.
The keyword must be followed by a valid dictionary name, defined in
`ispell-local-dictionary-alist' or `ispell-dictionary-alist'.
When multiple occurrences exist, the last keyword
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

(defun ispell--\\w-filter (char)
  "Return CHAR in a string when CHAR doesn't have \"word\" syntax,
nil otherwise.  CHAR must be a character."
  (let ((str (string char)))
    (and
     (not (string-match "\\w" str))
     str)))

(defun ispell--make-\\w-expression (chars)
  "Make a regular expression like \"\\(\\w\\|[-_]\\)\".
This (parenthesized) expression matches either a character of
\"word\" syntax or one in CHARS.

CHARS is a string of characters.  A member of CHARS is omitted
from the expression if it already has word syntax.  (Be careful
about special characters such as ?\\, ?^, ?], and ?- in CHARS.)
If after this filtering there are no chars left, or only one, a
special form of the expression is generated."
  (let ((filtered
	 (mapconcat #'ispell--\\w-filter chars "")))
    (concat
     "\\(\\w"
     (cond
      ((equal filtered "")
       "\\)")
      ((eq (length filtered) 1)
       (concat "\\|" filtered "\\)"))
      (t
       (concat "\\|[" filtered "]\\)"))))))

(defun ispell--make-filename-or-URL-re ()
  "Construct a regexp to match some file names or URLs or email addresses.
The expression is crafted to match as great a variety of these
objects as practicable, without too many false matches happening."
  (concat ;"\\(--+\\|_+\\|"
          "\\(/\\w\\|\\("
          (ispell--make-\\w-expression "-_")
          "+[.:@]\\)\\)"
          (ispell--make-\\w-expression "-_")
          "*\\([.:/@]+"
          (ispell--make-\\w-expression "-_~=?&")
          "+\\)+"
          ;"\\)"
          ))

;;;###autoload
(defvar ispell-skip-region-alist
  `((ispell-words-keyword	   forward-line)
    (ispell-dictionary-keyword	   forward-line)
    (ispell-pdict-keyword	   forward-line)
    (ispell-parsing-keyword	   forward-line)
    (,(purecopy "^---*BEGIN PGP [A-Z ]*--*")
     . ,(purecopy "^---*END PGP [A-Z ]*--*"))
    ;; assume multiline uuencoded file? "\nM.*$"?
    (,(purecopy "^begin [0-9][0-9][0-9] [^ \t]+$") . ,(purecopy "\nend\n"))
    (,(purecopy "^%!PS-Adobe-[123].0")	 . ,(purecopy "\n%%EOF\n"))
    (,(purecopy "^---* \\(Start of \\)?[Ff]orwarded [Mm]essage")
     . ,(purecopy "^---* End of [Ff]orwarded [Mm]essage"))
    ;; Matches e-mail addresses, file names, http addresses, etc.  The
    ;; `-+' `_+' patterns are necessary for performance reasons when
    ;; `-' or `_' part of word syntax.
;    (,(purecopy "\\(--+\\|_+\\|\\(/\\w\\|\\(\\(\\w\\|[-_]\\)+[.:@]\\)\\)\\(\\w\\|[-_]\\)*\\([.:/@]+\\(\\w\\|[-_~=?&]\\)+\\)+\\)"))
    ;; above checks /.\w sequences
    ;;("\\(--+\\|\\(/\\|\\(\\(\\w\\|[-_]\\)+[.:@]\\)\\)\\(\\w\\|[-_]\\)*\\([.:/@]+\\(\\w\\|[-_~=?&]\\)+\\)+\\)")
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
(put 'ispell-skip-region-alist 'risky-local-variable t)


;;;###autoload
(defvar ispell-tex-skip-alists
  (purecopy
  '((;;("%\\[" . "%\\]") ; AMStex block comment...
     ;; All the standard LaTeX keywords from L. Lamport's guide:
     ;; \cite, \hspace, \hspace*, \hyphenation, \include, \includeonly, \input,
     ;; \label, \nocite, \rule (in ispell - rest included here)
     ("\\\\addcontentsline"              ispell-tex-arg-end 2)
     ("\\\\add\\(tocontents\\|vspace\\)" ispell-tex-arg-end)
     ("\\\\\\([aA]lph\\|arabic\\)"	 ispell-tex-arg-end)
     ;;("\\\\author"			 ispell-tex-arg-end)
     ("\\\\cref"			 ispell-tex-arg-end)
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
     ("verbatim\\*?"	. "\\\\end[ \t\n]*{[ \t\n]*verbatim\\*?[ \t\n]*}"))))
  "Lists of regions to be skipped in TeX mode.
First list is used raw.
Second list has key placed inside \\begin{}.

Delete or add any regions you want to be automatically selected
for skipping in latex mode.")
(put 'ispell-tex-skip-alist 'risky-local-variable t)


;;;###autoload
(defconst ispell-html-skip-alists
  '(("<[cC][oO][dD][eE]\\>[^>]*>"	  "</[cC][oO][dD][eE]*>")
    ("<[sS][cC][rR][iI][pP][tT]\\>[^>]*>" "</[sS][cC][rR][iI][pP][tT]>")
    ("<[aA][pP][pP][lL][eE][tT]\\>[^>]*>" "</[aA][pP][pP][lL][eE][tT]>")
    ("<[vV][eE][rR][bB]\\>[^>]*>"         "<[vV][eE][rR][bB]\\>[^>]*>")
    ;;("<[tT][tT]\\>[^>]*>"		  "<[tT][tT]\\>[^>]*>")
    ("<[tT][tT]/"			  "/")
    ("<[^ \t\n>]"			  ">")
    ("&[^ \t\n;]"			  "[; \t\n]"))
  "Lists of start and end keys to skip in HTML buffers.
Same format as `ispell-skip-region-alist'.
Note - substrings of other matches must come last
 (e.g. \"<[tT][tT]/\" and \"<[^ \\t\\n>]\").")
(put 'ispell-html-skip-alists 'risky-local-variable t)

(defvar ispell-local-pdict ispell-personal-dictionary
  "A buffer local variable containing the current personal dictionary.
If non-nil, the value must be a string, which is a file name.

If you specify a personal dictionary for the current buffer which is
different from the current personal dictionary, the effect is similar
to calling \\[ispell-change-dictionary].  This variable is automatically
set when defined in the file with either `ispell-pdict-keyword' or the
local variable syntax.")

(make-variable-buffer-local 'ispell-local-pdict)
;;;###autoload(put 'ispell-local-pdict 'safe-local-variable 'stringp)

(defvar ispell-buffer-local-name nil
  "Contains the buffer name if local word definitions were used.
Ispell is then restarted because the local words could conflict.")

(defvar ispell-buffer-session-localwords nil
  "List of words accepted for session in this buffer.")

(make-variable-buffer-local 'ispell-buffer-session-localwords)

(defvar ispell-parser 'use-mode-name
  "Indicates whether ispell should parse the current buffer as TeX Code.
Special value `use-mode-name' tries to guess using the name of `major-mode'.
Default parser is `nroff'.
Currently the only other valid parser is `tex'.

You can set this variable in hooks in your init file -- eg:

\(add-hook \\='tex-mode-hook (lambda () (setq ispell-parser \\='tex)))")

(defvar ispell-region-end (make-marker)
  "Marker that allows spelling continuations.")

(defvar ispell-check-only nil
  "If non-nil, `ispell-word' does not try to correct the word.")


;;; **********************************************************************
;;; **********************************************************************



;;;###autoload (define-key esc-map "$" 'ispell-word)


(defun ispell-accept-output (&optional timeout-secs timeout-msecs)
  "Wait for output from Ispell process, or for TIMEOUT-SECS + TIMEOUT-MSECS.
\(The TIMEOUT-MSECS argument is obsolete and should be avoided.)
If asynchronous subprocesses are not supported, call function `ispell-filter'
and pass it the output of the last Ispell invocation."
  (if ispell-async-processp
      (let ((timeout (if timeout-msecs
			 (+ (or timeout-secs 0) (/ timeout-msecs 1000.0))
		       timeout-secs)))
	(accept-process-output ispell-process timeout))
    (if (null ispell-process)
	(error "No Ispell process to read output from!")
      (let ((buf ispell-output-buffer)
	    ispell-output)
	(if (not (bufferp buf))
	    (setq ispell-filter nil)
	  (with-current-buffer buf
	    (setq ispell-output (buffer-substring-no-properties
				 (point-min) (point-max))))
	  (ispell-filter t ispell-output)
	  (with-current-buffer buf
	    (erase-buffer)))))))

(defun ispell-send-replacement (misspelled replacement)
  "Notify spell checker that MISSPELLED should be spelled REPLACEMENT.
This allows improving the suggestion list based on actual misspellings.
Only works for Aspell and Enchant."
  (and (or ispell-really-aspell ispell-really-enchant)
       (ispell-send-string (concat "$$ra " misspelled "," replacement "\n"))))


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
	  ;; we have a *real* reason to invoke it.
	  (cmds-to-defer '(?* ?@ ?~ ?+ ?- ?! ?%))
	  (session-buf ispell-session-buffer)
	  (output-buf ispell-output-buffer)
	  (ispell-args ispell-cmd-args)
	  (defdir ispell-process-directory)
	  prev-pos)
      (with-current-buffer session-buf
	(setq prev-pos (point))
	(setq default-directory defdir)
	(insert string)
	(if (not (memq cmd cmds-to-defer))
	    (let* ((coding-system-for-read (ispell-get-coding-system))
                   (coding-system-for-write coding-system-for-read)
                   status)
	      (set-buffer output-buf)
	      (erase-buffer)
	      (set-buffer session-buf)
	      (setq status
		    (apply 'ispell-call-process-region
			   (point-min) (point-max)
			   ispell-program-name nil
			   output-buf nil
			   "-a"
			   ;; hunspell -m option means something different
			   (if ispell-really-hunspell "" "-m")
			   ispell-args))
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
(defun ispell-word (&optional following quietly continue region)
  "Check spelling of word under or before the cursor.
If the word is not found in dictionary, display possible corrections
in a window allowing you to choose one.

If optional argument FOLLOWING is non-nil or if `ispell-following-word'
is non-nil when called interactively, then the following word
\(rather than preceding) is checked when the cursor is not over a word.
When the optional argument QUIETLY is non-nil or `ispell-quietly' is non-nil
when called interactively, non-corrective messages are suppressed.

With a prefix argument (or if CONTINUE is non-nil),
resume interrupted spell-checking of a buffer or region.

Interactively, in Transient Mark mode when the mark is active, call
`ispell-region' to check the active region for spelling errors.

Word syntax is controlled by the definition of the chosen dictionary,
which is in `ispell-local-dictionary-alist' or `ispell-dictionary-alist'.

This will check or reload the dictionary.  Use \\[ispell-change-dictionary]
or \\[ispell-region] to update the Ispell process.

Return values:
nil           word is correct or spelling is accepted.
0             word is inserted into buffer-local definitions.
\"word\"        word corrected from word list.
\(\"word\" arg)  word is hand entered.
quit          spell session exited."
  (interactive (list ispell-following-word ispell-quietly current-prefix-arg t))
  (cond
   ((and region
	 (if (featurep 'emacs)
	     (use-region-p)
	   (and (boundp 'transient-mark-mode) transient-mark-mode
		(boundp 'mark-active) mark-active
		(not (eq (region-beginning) (region-end))))))
    (ispell-region (region-beginning) (region-end)))
   (continue (ispell-continue))
   (t
    (ispell-set-spellchecker-params)    ; Initialize variables and dicts alists
    (ispell-accept-buffer-local-defs)	; use the correct dictionary
    (let ((cursor-location (point))	; retain cursor location
	  (word (ispell-get-word following))
	  start end poss new-word replace)
      ;; De-structure return word info list.
      (setq start (car (cdr word))
	    end (car (cdr (cdr word)))
	    word (car word))

      ;; At this point it used to ignore 2-letter words.
      ;; But that is silly; if the user asks for it, we should do it. - rms.
      (or quietly
	  (message "Checking spelling of %s..."
		   (funcall ispell-format-word-function word)))
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
			  (funcall ispell-format-word-function word))))
	    ((stringp poss)
	     (or quietly
		 (message "%s is correct because of root %s"
			  (funcall ispell-format-word-function word)
			  (funcall ispell-format-word-function poss))))
	    ((null poss)
	     (message "Error checking word %s using %s with %s dictionary"
		      (funcall ispell-format-word-function word)
		      (file-name-nondirectory ispell-program-name)
		      (or ispell-current-dictionary "default")))
	    (ispell-check-only	      ; called from ispell minor mode.
	     (progn
               (beep)
	       (message "%s is incorrect"
                        (funcall ispell-format-word-function word))))
	    (t				; prompt for correct word.
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
			  (goto-char start)
			  ;; Insert first and then delete,
			  ;; to avoid collapsing markers before and after
			  ;; into a single place.
			  (insert new-word)
			  (delete-region (point) end)
			  ;; It is meaningless to preserve the cursor position
			  ;; inside a word that has changed.
			  (setq cursor-location (point))
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
      (if ispell-quit (setq ispell-quit nil replace 'quit))
      (goto-char cursor-location)	; return to original location
      replace))))


(defun ispell-get-word (following &optional extra-otherchars)
  "Return the word for spell-checking according to ispell syntax.
If optional argument FOLLOWING is non-nil or if `ispell-following-word'
is non-nil when called interactively, then the following word
\(rather than preceding) is checked when the cursor is not over a word.
Optional second argument contains otherchars that can be included in word
many times (see the doc string of `ispell-dictionary-alist' for details
about otherchars).

Word syntax is controlled by the definition of the chosen dictionary,
which is in `ispell-local-dictionary-alist' or `ispell-dictionary-alist'."
  (ispell-set-spellchecker-params)    ; Initialize variables and dicts alists
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
	  (user-error "No word found to check!"))
      (setq start (copy-marker (match-beginning 0))
	    end (point-marker)
	    word (buffer-substring-no-properties start end))
      (list word start end))))


;; Global ispell-pdict-modified-p is set by ispell-command-loop and
;; tracks changes in the dictionary.  The global may either be
;; a value or a list, whose value is the state of whether the
;; dictionary needs to be saved.

;;;###autoload
(defun ispell-pdict-save (&optional no-query force-save)
  "Check to see if the personal dictionary has been modified.
If so, ask if it needs to be saved."
  (interactive (list ispell-silently-savep t))
  (if (and ispell-pdict-modified-p (listp ispell-pdict-modified-p))
      (setq ispell-pdict-modified-p (car ispell-pdict-modified-p)))
  (when (and (or ispell-pdict-modified-p force-save)
	     (or no-query
		 (y-or-n-p "Personal dictionary modified.  Save? ")))
    (ispell-send-string "#\n")	; save dictionary
    (message "Personal dictionary saved."))
  ;; unassert variable, even if not saved to avoid questioning.
  (setq ispell-pdict-modified-p nil))


(defvar ispell-update-post-hook nil
  "A normal hook invoked from the ispell command loop.
It is called once per iteration, before displaying a prompt to
the user.")

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
indicates whether the dictionary has been modified when option `a'
or `i' is used.
Global `ispell-quit' set to start location to continue spell session."
  (let ((count ?0)
	(choices miss)
	(window-min-height (min window-min-height
				ispell-choices-win-default-height))
	(command-characters '( ?  ?i ?a ?A ?r ?R ?? ?x ?X ?q ?l ?u ?m ))
	(skipped 0)
	char num result textwin)

    ;; setup the *Choices* buffer with valid data.
    (with-current-buffer (get-buffer-create ispell-choices-buffer)
      (setq mode-line-format
	    (concat
             "--  %b  --  word: " word
             "  --  dict: " (or ispell-current-dictionary "default")
             "  --  prog: " (file-name-nondirectory ispell-program-name)))
      ;; No need for horizontal scrollbar in choices window
      (with-no-warnings
       (setq horizontal-scroll-bar nil))
      (erase-buffer)
      (if guess
	  (progn
	    (insert "Affix rules generate and capitalize "
		    "this word as shown below:\n\t")
	    (while guess
	      (when (> (+ 4 (current-column) (length (car guess)))
		       (window-width))
		(insert "\n\t"))
	      (insert (car guess) "    ")
	      (setq guess (cdr guess)))
	    (insert (substitute-command-keys
		     "\nUse option `i' to accept this spelling and put it in your private dictionary.\n"))))
      (while choices
	(when (> (+ 7 (current-column)
		    (length (car choices))
		    (if (> count ?~) 3 0))
		 (window-width))
	  (insert "\n"))
	;; not so good if there are over 20 or 30 options, but then, if
	;; there are that many you don't want to scan them all anyway...
	(while (memq count command-characters) ; skip command characters.
	  (setq count (1+ count)
		skipped (1+ skipped)))
	(insert "(" count ") " (car choices) "  ")
	(setq choices (cdr choices)
	      count (1+ count)))
      (setq count (- count ?0 skipped)))

    (run-hooks 'ispell-update-post-hook)

    ;; ensure word is visible
    (if (not (pos-visible-in-window-group-p end))
	(sit-for 0))

    ;; Display choices for misspelled word.
    (setq textwin (selected-window))
    (ispell-show-choices)
    (select-window textwin)

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
		  (let ((inhibit-quit t)
			(input-valid t))
		    (setq char nil skipped 0)
		    ;; If the user types C-g, or generates some other
		    ;; non-character event (such as a frame switch
		    ;; event), stop ispell.  As a special exception,
		    ;; ignore mouse events occurring in the same frame.
		    (while (and input-valid (not (characterp char)))
		      (setq char (read-key))
		      (setq input-valid
			    (or (characterp char)
				(and (mouse-event-p char)
				     (eq (selected-frame)
					 (window-frame
					  (posn-window (event-start char))))))))
		    (when (or quit-flag (not input-valid) (= char ?\C-g))
		      (setq char ?X quit-flag nil)))
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

                    (when flyspell-mode
                      (flyspell-unhighlight-at start))
		    nil)
		   ((or (= char ?a) (= char ?A)) ; accept word without insert
		    (ispell-send-string (concat "@" word "\n"))
		    (cl-pushnew word ispell-buffer-session-localwords
                                :test #'equal)
		    (when flyspell-mode
                      (flyspell-unhighlight-at start))
		    (or ispell-buffer-local-name ; session localwords might conflict
			(setq ispell-buffer-local-name (buffer-name)))
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
		      (concat
                       "Spell-checking suspended; use "
		       "\\[universal-argument] \\[ispell-word] to resume")))
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
			    (with-current-buffer (get-buffer-create
                                                  ispell-choices-buffer)
			      (erase-buffer)
			      (setq count ?0
				    skipped 0
				    mode-line-format ;; setup the *Choices* buffer with valid data.
				    (concat "--  %b  --  word: " new-word
					    "  --  word-list: "
					    (or ispell-complete-word-dict
						ispell-alternate-dictionary))
				    miss (ispell-lookup-words new-word)
				    choices miss)
			      (while choices
				(when (> (+ 7 (current-column)
					    (length (car choices))
					    (if (> count ?~) 3 0))
					 (window-width))
				  (insert "\n"))
				(while (memq count command-characters)
				  (setq count (1+ count)
					skipped (1+ skipped)))
				(insert "(" count ") " (car choices) "  ")
				(setq choices (cdr choices)
				      count (1+ count)))
			      (setq count (- count ?0 skipped)))
			    (setq textwin (selected-window))
			    (ispell-show-choices)
			    (select-window textwin))))
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
	     (ispell-highlight-spelling-error start end))))))



(defun ispell-show-choices ()
  "Show the choices in another buffer or frame."
  (if (and ispell-use-framepop-p (fboundp 'framepop-display-buffer))
      (progn
	(framepop-display-buffer (get-buffer ispell-choices-buffer))
        ;; (get-buffer-window ispell-choices-buffer t)
	(select-window (previous-window))) ; *Choices* window
    ;; Display choices above selected window.
    (ispell-display-buffer (get-buffer-create ispell-choices-buffer))))


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
`R':   Replace word with typed-in value.  Query-replaced in buffer.  Rechecked.
`?':   Show these commands.
`x':   Exit spelling buffer.  Move cursor to original point.
`X':   Exit spelling buffer.  Leaves cursor at the current point, and permits
        the aborted check to be completed later.
`q':   Quit spelling session (Kills ispell process).
`l':   Look up typed-in replacement in alternate dictionary.  Wildcards okay.
`u':   Like `i', but the word is lower-cased first.
`m':   Place typed-in value in personal dictionary, then recheck current word.
`C-l':  Redraw screen.
`C-r':  Recursive edit.
`C-z':  Suspend Emacs or iconify frame."

  (if (equal ispell-help-in-bufferp 'electric)
      (progn
	(require 'ehelp)
	(with-electric-help
	 (function (lambda ()
		     ;;This shouldn't be necessary: with-electric-help needs
		     ;; an optional argument telling it about the smallest
		     ;; acceptable window-height of the help buffer.
		     ;;(if (< (window-height) 15)
		     ;;	 (enlarge-window
		     ;;	  (- 15 (ispell-adjusted-window-height))))
		     (princ "Selections are:

DIGIT: Replace the word with a digit offered in the *Choices* buffer.
SPC:   Accept word this time.
`i':   Accept word and insert into private dictionary.
`a':   Accept word for this session.
`A':   Accept word and place in `buffer-local dictionary'.
`r':   Replace word with typed-in value.  Rechecked.
`R':   Replace word with typed-in value.  Query-replaced in buffer.  Rechecked.
`?':   Show these commands.
`x':   Exit spelling buffer.  Move cursor to original point.
`X':   Exit spelling buffer.  Leaves cursor at the current point, and permits
        the aborted check to be completed later.
`q':   Quit spelling session (Kills ispell process).
`l':   Look up typed-in replacement in alternate dictionary.  Wildcards okay.
`u':   Like `i', but the word is lower-cased first.
`m':   Place typed-in value in personal dictionary, then recheck current word.
`C-l':  Redraw screen.
`C-r':  Recursive edit.
`C-z':  Suspend Emacs or iconify frame.")
		     nil))))


    (let ((help-1 (concat "[r/R]eplace word; [a/A]ccept for this session; "
			  "[i]nsert into private dictionary"))
	  (help-2 (concat "[l]ook a word up in alternate dictionary;  "
			  "e[x/X]it;  [q]uit session"))
	  (help-3 (concat "[u]ncapitalized insert into dict.  "
			  "Type 'x C-h f ispell-help' for more help")))
      (save-window-excursion
	(if ispell-help-in-bufferp
	    (let ((buffer (get-buffer-create "*Ispell Help*")))
	      (with-current-buffer buffer
		(insert (concat help-1 "\n" help-2 "\n" help-3)))
	      (ispell-display-buffer buffer)
	      (sit-for 5)
	      (kill-buffer "*Ispell Help*"))
	  (unwind-protect
	      (let ((resize-mini-windows 'grow-only))
		(select-window (minibuffer-window))
		(erase-buffer)
		(message nil)
		;;(set-minibuffer-window (selected-window))
		(enlarge-window 2)
		(insert (concat help-1 "\n" help-2 "\n" help-3))
		(sit-for 5))
	    (erase-buffer)))))))

(define-obsolete-function-alias 'lookup-words 'ispell-lookup-words "24.4")

(defun ispell-lookup-words (word &optional lookup-dict)
  "Look up WORD in optional word-list dictionary LOOKUP-DICT.
A `*' serves as a wild card.  If no wild cards, `look' is used if it exists.
Otherwise the variable `ispell-grep-command' contains the command
\(usually \"grep\") used to search for the words.

Optional second argument contains the dictionary to use; the default is
`ispell-alternate-dictionary', overridden by `ispell-complete-word-dict'
if defined."
  ;; We don't use the filter for this function, rather the result is written
  ;; into a buffer.  Hence there is no need to save the filter values.
  (if (null lookup-dict)
      (setq lookup-dict (or ispell-complete-word-dict
			    ispell-alternate-dictionary)))

  (if lookup-dict
      (unless (file-readable-p lookup-dict)
	(error "lookup-words error: Unreadable or missing plain word-list %s."
	       lookup-dict))
    (error (concat "lookup-words error: No plain word-list found at system"
                   "default locations.  "
                   "Customize `ispell-alternate-dictionary' to set yours.")))

  (let* ((process-connection-type ispell-use-ptys-p)
	 (wild-p (string-match "\\*" word))
	 (look-p (and ispell-look-p	; Only use look for an exact match.
		      (or ispell-have-new-look (not wild-p))))
	 (prog (if look-p ispell-look-command ispell-grep-command))
	 (args (if look-p ispell-look-options ispell-grep-options))
	 status results loc)
    (with-temp-buffer
      (message "Starting \"%s\" process..." (file-name-nondirectory prog))
      (if look-p
          nil
        (insert "^" word)
        ;; When there are no wildcards, append one, for consistency
        ;; with `look' behavior.
        (unless wild-p (insert "*"))
        (insert "$")
        ;; Convert * to .*
        (while (search-backward "*" nil t) (insert "."))
        (setq word (buffer-string))
        (erase-buffer))
      (setq status (apply 'ispell-call-process prog nil t nil
                          (nconc (if (and args (> (length args) 0))
                                     (list args)
                                   (if look-p nil
                                     (list "-e")))
                                 (list word)
                                 (if lookup-dict (list lookup-dict)))))
      ;; `grep' returns status 1 and no output when word not found, which
      ;; is a perfectly normal thing.
      (if (stringp status)
          (error "error: %s exited with signal %s"
                 (file-name-nondirectory prog) status)
        ;; Else collect words into `results' in FIFO order.
        (goto-char (point-max))
        ;; Assure we've ended with \n.
        (or (bobp) (= (preceding-char) ?\n) (insert ?\n))
        (while (not (bobp))
          (setq loc (point))
          (forward-line -1)
          (push (buffer-substring-no-properties (point)
                                                (1- loc))
                results))))
    (if (and results (string-match ".+: " (car results)))
        (error "%s error: %s" ispell-grep-command (car results)))
    results))


;; "ispell-filter" is a list of output lines from the generating function.
;;   Each full line (ending with \n) is a separate item on the list.
;; "output" can contain multiple lines, part of a line, or both.
;; "start" and "end" are used to keep bounds on lines when "output" contains
;;   multiple lines.
;; "ispell-filter-continue" is true when we have received only part of a
;;   line as output from a generating function ("output" did not end with \n)
;; THIS FUNCTION WILL FAIL IF THE PROCESS OUTPUT DOESN'T END WITH \n!
;;   This is the case when a process dies or fails. The default behavior
;;   in this case treats the next input received as fresh input.

(defun ispell-filter (_process output)
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


;; This function destroys the mark location if it is in the word being
;; highlighted.
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
	(text (buffer-substring-no-properties start end))
					; Save highlight region.
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


(defun ispell-highlight-spelling-error-overlay (start end &optional highlight)
  "Highlight the word from START to END using overlays.
When the optional third arg HIGHLIGHT is set, the word is highlighted
otherwise it is displayed normally.

The variable `ispell-highlight-face' selects the face to use for highlighting."
  (if highlight
      (if ispell-overlay
	  (move-overlay ispell-overlay start end (current-buffer))
	(setq ispell-overlay (make-overlay start end))
	(overlay-put ispell-overlay 'priority 1001) ;higher than lazy overlays
	(overlay-put ispell-overlay 'face ispell-highlight-face))
    (if ispell-overlay
	(delete-overlay ispell-overlay)))
  (if (and ispell-lazy-highlight (boundp 'lazy-highlight-cleanup))
      (if highlight
	  (let ((isearch-string
		 (concat
		  "\\b"
		  (regexp-quote (buffer-substring-no-properties start end))
		  "\\b"))
		(isearch-regexp t)
		(isearch-regexp-function nil)
		(isearch-case-fold-search nil)
		(isearch-forward t)
		(isearch-other-end start)
		(isearch-error nil))
	    (isearch-lazy-highlight-new-loop
	     (if (boundp 'reg-start) reg-start)
	     (if (boundp 'reg-end)   reg-end)))
	(lazy-highlight-cleanup lazy-highlight-cleanup)
	(setq isearch-lazy-highlight-last-string nil))))


(defun ispell-highlight-spelling-error (start end &optional highlight refresh)
  (if (display-color-p)
      (ispell-highlight-spelling-error-overlay start end highlight)
    (ispell-highlight-spelling-error-generic start end highlight refresh)))

(defun ispell-display-buffer (buffer)
  "Show BUFFER in new window above selected one.
Also position fit window to BUFFER and select it."
  (let* ((unsplittable
	  (cdr (assq 'unsplittable (frame-parameters (selected-frame)))))
	 (window
	  (or (get-buffer-window buffer)
	      (and unsplittable
		   ;; If frame is unsplittable, temporarily disable that...
		   (let ((frame (selected-frame)))
		     (modify-frame-parameters frame '((unsplittable . nil)))
		     (prog1
			 (condition-case nil
			     (split-window
                              ;; Chose the last of a window group, since
                              ;; otherwise, the lowering of another window's
                              ;; TL corner would cause the logical order of
                              ;; the windows to be changed.
			      (car (last (selected-window-group)))
                              (- ispell-choices-win-default-height) 'above)
			   (error nil))
		       (modify-frame-parameters frame '((unsplittable . t))))))
	      (and (not unsplittable)
		   (condition-case nil
		       (split-window
                        ;; See comment above.
			(car (last (selected-window-group)))
                        (- ispell-choices-win-default-height) 'above)
		     (error nil)))
	      (display-buffer buffer))))
    (if (not window)
	(error "Couldn't make window for *Choices*")
      (select-window window)
      (set-window-buffer window buffer)
      (set-window-point window (point-min))
      (fit-window-to-buffer window nil nil nil nil t))))

;; Should we add a compound word match return value?
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
   ((eq (aref output 0) ?+)		; found because of root word
    (substring output 2))		; return root word
   ((equal 0 (string-match "[\ra-zA-Z]" output))
    (ding)				; error message from ispell!
    (message "Ispell error: %s" output)
    (sit-for 5)
    nil)
   (t					; need to process &, ?, and #'s
    (let ((type (aref output 0))	; &, ?, or #
	  (original-word (substring output 2 (string-match " " output 2)))
	  (cur-count 0)			; contains number of misses + guesses
	  count miss-list guess-list offset)
      (setq output (substring output (match-end 0))) ; skip over misspelling
      (if (eq type ?#)
	  (setq count 0)		; no misses for type #
	(setq count (string-to-number output) ; get number of misses.
	      output (substring output (1+ (string-match " " output 1)))))
      (setq offset (string-to-number output))
      (setq output (if (eq type ?#)     ; No miss or guess list.
                       nil
                     (substring output (1+ (string-match " " output 1)))))
      (while output
	(let ((end (string-match ", \\|\\($\\)" output))) ; end of miss/guess.
	  (setq cur-count (1+ cur-count))
	  (if (> cur-count count)
	      (push (substring output 0 end) guess-list)
	    (push (substring output 0 end) miss-list))
	  (setq output (if (match-end 1) ; True only when at end of line.
                           nil           ; No more misses or guesses.
                         (substring output (+ end 2))))))
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
  "Start the Ispell process, with support for no asynchronous processes.
Keeps argument list for future Ispell invocations for no async support."
  ;; `ispell-current-dictionary' and `ispell-current-personal-dictionary'
  ;; are properly set in `ispell-internal-change-dictionary'.

  ;; Parse hunspell affix file if using hunspell and entry is uninitialized.
  (if ispell-really-hunspell
      (or (cadr (assoc ispell-current-dictionary ispell-dictionary-alist))
	  (ispell-hunspell-fill-dictionary-entry ispell-current-dictionary)))

  (let* ((default-directory
           (if (file-accessible-directory-p default-directory)
               default-directory
             ;; Defend against bad `default-directory'.
             (expand-file-name "~/")))
	 (orig-args (ispell-get-ispell-args))
         (args
          (append
           (if (and ispell-current-dictionary      ; Not for default dict (nil)
                    (not (member "-d" orig-args))) ; Only define if not overridden.
               (list "-d" ispell-current-dictionary))
           orig-args
           (if ispell-current-personal-dictionary ; Use specified pers dict.
               (list "-p" ispell-current-personal-dictionary))
           ;; If we are using recent aspell or hunspell, make sure we use the
           ;; right encoding for communication. ispell or older aspell/hunspell
           ;; does not support this.
           (if ispell-encoding8-command
	       (if ispell-really-hunspell
		   (list ispell-encoding8-command
			 (upcase (symbol-name (ispell-get-coding-system))))
		 (list
		  (concat ispell-encoding8-command
			  (symbol-name (ispell-get-coding-system))))))
           ispell-extra-args)))

    ;; Initially we don't know any buffer's local words.
    (setq ispell-buffer-local-name nil)

    (if ispell-async-processp
	(let ((process-connection-type ispell-use-ptys-p))
	  (apply 'start-process
		 "ispell" nil ispell-program-name
		 "-a"                   ; Accept single input lines.
                 ;; Make root/affix combos not in dict.
                 ;; hunspell -m option means different.
		 (if ispell-really-hunspell "" "-m")
		 args))
      (setq ispell-cmd-args args
	    ispell-output-buffer (generate-new-buffer " *ispell-output*")
	    ispell-session-buffer (generate-new-buffer " *ispell-session*"))
      (ispell-send-string "\032\n")	; so Ispell prints version and exits
      t)))

(defun ispell-init-process ()
  "Check status of Ispell process and start if necessary."
  (let* (;; Basename of dictionary used by the spell-checker
	 (dict-bname (or (car (cdr (member "-d" (ispell-get-ispell-args))))
			 ispell-current-dictionary))
	 ;; The default directory for the process.
	 ;; Use "~/" as default-directory unless using Ispell with per-dir
	 ;; personal dictionaries
	 (default-directory
	   (if (or ispell-really-aspell
		   ispell-really-hunspell
		   ;; Protect against bad default-directory
		   (not (file-accessible-directory-p default-directory))
		   ;; Ispell and per-dir personal dicts available
		   (not (or (file-readable-p (concat default-directory
						     ".ispell_words"))
			    (file-readable-p (concat default-directory
						     ".ispell_"
						     (or dict-bname
							 "default")))))
		   ;; Ispell, in a minibuffer
		   (window-minibuffer-p))
	       (expand-file-name "~/")
	     (expand-file-name default-directory))))
    ;; Check if process needs restart
    (if (and ispell-process
	     (eq (ispell-process-status) 'run)
	     ;; Unless we are using an explicit personal dictionary, ensure
	     ;; we're in the same default directory!  Restart check for
	     ;; personal dictionary is done in
	     ;; `ispell-internal-change-dictionary', called from
	     ;; `ispell-buffer-local-dict'
	     (or (or ispell-local-pdict ispell-personal-dictionary)
		 (equal ispell-process-directory default-directory)))
	(setq ispell-filter nil ispell-filter-continue nil)
      ;; may need to restart to select new personal dictionary.
      (ispell-kill-ispell t)
      (message "Starting new Ispell process %s with %s dictionary..."
	       ispell-program-name
	       (or ispell-local-dictionary ispell-dictionary "default"))
      (sit-for 0)
      (setq ispell-library-directory (ispell-check-version)
            ;; Assign a non-nil value to ispell-process-directory
            ;; before calling ispell-start-process, since that
            ;; function needs it to set default-directory when
            ;; ispell-async-processp is nil.
	    ispell-process-directory default-directory
	    ispell-process (ispell-start-process)
	    ispell-filter nil
	    ispell-filter-continue nil)

      (unless (equal ispell-process-directory (expand-file-name "~/"))
	;; At this point, `ispell-process-directory' will be "~/" unless using
	;; Ispell with directory-specific dicts.
	;; If not, kill ispell process when killing buffer.  It may be in a
	;; removable device that would otherwise become un-mountable.
	(with-current-buffer
	    (if (window-minibuffer-p)                  ;; In minibuffer
		;; In this case kill ispell only when parent buffer is killed
		;; to avoid over and over ispell kill.
		(window-buffer (minibuffer-selected-window))
	      (current-buffer))
          (add-hook 'kill-buffer-hook
		    (lambda () (ispell-kill-ispell t)) nil 'local)))

      (if ispell-async-processp
	  (set-process-filter ispell-process 'ispell-filter))
      (if (and enable-multibyte-characters
               ;; Evidently, some people use the synchronous mode even
               ;; when async subprocesses are supported, in which case
               ;; set-process-coding-system is bound, but
               ;; ispell-process is not a process object.
               ispell-async-processp)
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
	     (error "%s" (mapconcat #'identity ispell-filter "\n"))))
      (setq ispell-filter nil)		; Discard version ID line
      (let ((extended-char-mode (ispell-get-extended-character-mode)))
	(if extended-char-mode		; ~ extended character mode
	    (ispell-send-string (concat extended-char-mode "\n"))))
      (when ispell-async-processp
        (set-process-query-on-exit-flag ispell-process nil)))))

;;;###autoload
(defun ispell-kill-ispell (&optional no-error clear)
  "Kill current Ispell process (so that you may start a fresh one).
With NO-ERROR, just return non-nil if there was no Ispell running.
With CLEAR, buffer session localwords are cleaned."
  (interactive)
  ;; This hook is typically used by flyspell to flush some variables used
  ;; to optimize the common cases.
  (run-hooks 'ispell-kill-ispell-hook)
  (if (or clear
	  (called-interactively-p 'interactive))
      (setq ispell-buffer-session-localwords nil))
  (if (not (and ispell-process
		(eq (ispell-process-status) 'run)))
      (or no-error
	  (error "There is no Ispell process running!"))
    (if ispell-async-processp
	(delete-process ispell-process)
      ;; Synchronous processes.
      (ispell-send-string "\n")		; Make sure side effects occurred.
      (kill-buffer ispell-output-buffer)
      (kill-buffer ispell-session-buffer)
      (setq ispell-output-buffer nil
	    ispell-session-buffer nil))
    (setq ispell-process nil)
    (message "Ispell process killed")
    nil))

;; ispell-change-dictionary is set in some people's hooks.  Maybe this should
;;  call ispell-init-process rather than wait for a spell checking command?

;;;###autoload
(defun ispell-change-dictionary (dict &optional arg)
  "Change to dictionary DICT for Ispell.
With a prefix arg, set it \"globally\", for all buffers.
Without a prefix arg, set it \"locally\", just for this buffer.

By just answering RET you can find out what the current dictionary is."
  (interactive
   (list (completing-read
	  "Use new dictionary (RET for current, SPC to complete): "
	  (and (fboundp 'ispell-valid-dictionary-list)
	       (mapcar #'list (ispell-valid-dictionary-list)))
	  nil t)
	 current-prefix-arg))
  (ispell-set-spellchecker-params) ; Initialize variables and dicts alists
  (unless arg (ispell-buffer-local-dict 'no-reload))
  (if (equal dict "default") (setq dict nil))
  ;; This relies on completing-read's bug of returning "" for no match
  (cond ((equal dict "")
	 (ispell-internal-change-dictionary)
	 (message "Using %s dictionary"
		  (or (and (not arg) ispell-local-dictionary)
		      ispell-dictionary "default")))
	((equal dict (or (and (not arg) ispell-local-dictionary)
			 ispell-dictionary "default"))
	 ;; Specified dictionary is the default already. Could reload
	 ;; the dictionaries if needed.
	 (ispell-internal-change-dictionary)
	 (when (called-interactively-p 'interactive)
	      (message "No change, using %s dictionary" dict)))
	(t				; reset dictionary!
	 (if (or (assoc dict ispell-local-dictionary-alist)
		 (assoc dict ispell-dictionary-alist))
	     (if arg
		 ;; set default dictionary
		 (setq ispell-dictionary dict)
	       ;; set local dictionary
	       (setq ispell-local-dictionary dict)
	       (setq ispell-local-dictionary-overridden t))
	   (error "Undefined dictionary: %s" dict))
	 (ispell-internal-change-dictionary)
	 (setq ispell-buffer-session-localwords nil)
	 (message "%s Ispell dictionary set to %s"
		  (if arg "Global" "Local")
		  dict))))

(defun ispell-internal-change-dictionary ()
  "Update the dictionary and the personal dictionary used by Ispell.
This may kill the Ispell process; if so, a new one will be started
when needed."
  (let* ((dict (or ispell-local-dictionary ispell-dictionary))
	 (pdict (or ispell-local-pdict ispell-personal-dictionary))
	 (expanded-pdict (if pdict (expand-file-name pdict))))
    (unless (and (equal ispell-current-dictionary dict)
		 (equal ispell-current-personal-dictionary
			expanded-pdict))
      (ispell-kill-ispell t)
      (setq ispell-current-dictionary dict
	    ispell-current-personal-dictionary expanded-pdict))))

;; Avoid error messages when compiling for these dynamic variables.
(defvar ispell-start)
(defvar ispell-end)

;; Spelling of comments are checked when ispell-check-comments is non-nil.

;;;###autoload
(defun ispell-region (reg-start reg-end &optional recheckp shift)
  "Interactively check a region for spelling errors.
Return nil if spell session was terminated, otherwise returns shift offset
amount for last line processed."
  (interactive "r")			; Don't flag errors on read-only bufs.
  (ispell-set-spellchecker-params)      ; Initialize variables and dicts alists
  (if (not recheckp)
      (ispell-accept-buffer-local-defs)) ; set up dictionary, local words, etc.
  (let ((skip-region-start (make-marker))
	(rstart (make-marker))
	(region-type (if (and (= reg-start (point-min)) (= reg-end (point-max)))
			 (buffer-name) "region"))
	(program-basename (file-name-nondirectory ispell-program-name))
	(dictionary (or ispell-current-dictionary "default")))
    (unwind-protect
	(save-excursion
	  (message "Spell-checking %s using %s with %s dictionary..."
		   region-type program-basename dictionary)
	  ;; Returns cursor to original location.
	  (save-window-excursion
	    (goto-char reg-start)
	    (let ((transient-mark-mode)
		  (case-fold-search case-fold-search)
		  (query-fcc t)
		  in-comment key)
	      (ispell-print-if-debug
               "ispell-region: (ispell-skip-region-list):\n%s
ispell-region: (ispell-begin-skip-region-regexp):\n%s
ispell-region: Search for first region to skip after (ispell-begin-skip-region-regexp)\n"
               (ispell-skip-region-list)
               (ispell-begin-skip-region-regexp))
	      (if (re-search-forward (ispell-begin-skip-region-regexp) reg-end t)
		  (progn
		    (setq key (match-string-no-properties 0))
		    (set-marker skip-region-start (- (point) (length key)))
		    (goto-char reg-start)
		    (ispell-print-if-debug
                     "ispell-region: First skip: %s at (pos,line,column): (%s,%s,%s).\n"
                     key
                     (save-excursion (goto-char skip-region-start) (point))
                     (line-number-at-pos skip-region-start)
                     (save-excursion (goto-char skip-region-start) (current-column)))))
	      (ispell-print-if-debug
               "ispell-region: Continue spell-checking with %s and %s dictionary...\n"
		program-basename dictionary)
	      (set-marker rstart reg-start)
	      (set-marker ispell-region-end reg-end)
	      (while (and (not ispell-quit)
			  (< (point) ispell-region-end))
		;; spell-check region with skipping
		(if (and (marker-position skip-region-start)
			 (<= skip-region-start (point)))
		    (progn
		      ;; If region inside line comment, must keep comment start.
		      (setq in-comment (point)
			    in-comment
			    (and comment-start
				 (or (null comment-end) (string= "" comment-end))
				 (save-excursion
				   (beginning-of-line)
				   (re-search-forward comment-start in-comment t))
				 comment-start))
		      ;; Can change skip-regexps (in ispell-message)
		      (ispell-skip-region key) ; moves pt past region.
		      (set-marker rstart (point))
		      ;; check for saving large attachments...
		      (setq query-fcc (and query-fcc
					   (ispell-ignore-fcc skip-region-start
							      rstart)))
		      (if (and (< rstart ispell-region-end)
			       (re-search-forward
				(ispell-begin-skip-region-regexp)
				ispell-region-end t))
			  (progn
			    (setq key (match-string-no-properties 0))
			    (set-marker skip-region-start
					(- (point) (length key)))
			    (goto-char rstart)
			    (ispell-print-if-debug
                             "ispell-region: Next skip: %s at (pos,line,column): (%s,%s,%s).\n"
                             key
                             (save-excursion (goto-char skip-region-start) (point))
                             (line-number-at-pos skip-region-start)
                             (save-excursion (goto-char skip-region-start) (current-column))))
			(set-marker skip-region-start nil))))
		(setq reg-end (max (point)
				   (if (marker-position skip-region-start)
				       (min skip-region-start ispell-region-end)
				     (marker-position ispell-region-end))))
		(let* ((ispell-start (point))
		       (ispell-end (min (point-at-eol) reg-end))
		       ;; See if line must be prefixed by comment string to let ispell know this is
		       ;; part of a comment string.  This is only supported in some modes.
		       ;; In particular, this is not supported in autoconf mode where adding the
		       ;; comment string messes everything up because ispell tries to spellcheck the
		       ;; `dnl' string header causing misalignments in some cases (debbugs.gnu.org: #12768).
		       (add-comment (and in-comment
					 (not (string= in-comment "dnl "))
					 in-comment))
		       (string (ispell-get-line
				ispell-start ispell-end add-comment)))
		  (ispell-print-if-debug
                   "ispell-region: string pos (%s->%s), eol: %s, [in-comment]: [%s], [add-comment]: [%s], [string]: [%s]\n"
                   ispell-start ispell-end (point-at-eol) in-comment add-comment string)
		  (if add-comment		; account for comment chars added
		      (setq ispell-start (- ispell-start (length add-comment))
			    ;; Reset `in-comment' (and indirectly `add-comment') for new line
			    in-comment nil))
		  (setq ispell-end (point)) ; "end" tracks region retrieved.
		  (if string		; there is something to spell check!
		      ;; (special start end)
		      (setq shift (ispell-process-line string
						       (and recheckp shift))))
		  (goto-char ispell-end)))))
	  (if ispell-quit
	      nil
	    (or shift 0)))
      ;; protected
      (if (and (not (and recheckp ispell-keep-choices-win))
	       (get-buffer ispell-choices-buffer))
	  (kill-buffer ispell-choices-buffer))
      (set-marker skip-region-start nil)
      (set-marker rstart nil)
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
	(message "Spell-checking %s using %s with %s dictionary...done"
		 region-type program-basename dictionary)))))


(defun ispell-begin-skip-region-regexp ()
  "Return a regexp of the search keys for region skipping.
Includes `ispell-skip-region-alist' plus tex, tib, html, and comment keys.
Must be called after `ispell-buffer-local-parsing' due to dependence on mode."
  (mapconcat
   #'identity
   (delq nil
         (list
          ;; messages
          (if (and ispell-checking-message
                   (not (eq t ispell-checking-message)))
              (mapconcat #'car ispell-checking-message "\\|"))
          ;; tex
          (if (eq ispell-parser 'tex)
              (ispell-begin-tex-skip-regexp))
          ;; html stuff
          (if ispell-skip-html
              (ispell-begin-skip-region ispell-html-skip-alists))
          ;; tib
          (if ispell-skip-tib ispell-tib-ref-beginning)
          ;; Comments
          (if (and (eq 'exclusive ispell-check-comments) comment-start)
              ;; search from end of current comment to start of next comment.
              (if (string= "" comment-end) "^" (regexp-quote comment-end)))
          (if (and (null ispell-check-comments) comment-start)
              (regexp-quote comment-start))
          ;; If they set ispell-skip-region-alist to nil, mapconcat
          ;; will produce an empty string, which will then match
          ;; anything without moving point, something
          ;; ispell-skip-region doesn't expect.  Perhaps we should be
          ;; more defensive and delq "" above as well, in addition to
          ;; deleting nil elements.
          (if ispell-skip-region-alist
              (ispell-begin-skip-region ispell-skip-region-alist))
          (ispell--make-filename-or-URL-re)))
   "\\|"))


(defun ispell-begin-skip-region (skip-alist)
  "Regular expression for start of regions to skip generated from SKIP-ALIST.
Each selection should be a key of SKIP-ALIST;
otherwise, the current line is skipped."
  (mapconcat (lambda (lst) (if (stringp (car lst)) (car lst) (eval (car lst))))
	     skip-alist
	     "\\|"))


(defun ispell-begin-tex-skip-regexp ()
  "Regular expression of tex commands to skip.
Generated from `ispell-tex-skip-alists'."
  (concat
   ;; raw tex keys
   (mapconcat (function (lambda (lst) (car lst)))
	      (car ispell-tex-skip-alists)
	      "\\|")
   "\\|"
   ;; keys wrapped in begin{}
   (mapconcat (function (lambda (lst)
			  (concat "\\\\begin[ \t\n]*{[ \t\n]*"
				  (car lst)
				  "[ \t\n]*}")))
	      (car (cdr ispell-tex-skip-alists))
	      "\\|")))


(defun ispell-skip-region-list ()
  "Return a list describing key and body regions to skip for this buffer.
Includes regions defined by `ispell-skip-region-alist', tex mode,
`ispell-html-skip-alists', and `ispell-checking-message'.
Manual checking must include comments and tib references.
The list is of the form described by variable `ispell-skip-region-alist'.
Must be called after `ispell-buffer-local-parsing' due to dependence on mode."
  (let ((skip-alist ispell-skip-region-alist))
    (setq skip-alist (append (list (list (ispell--make-filename-or-URL-re)))
                             skip-alist))
    ;; only additional explicit region definition is tex.
    (if (eq ispell-parser 'tex)
	(setq case-fold-search nil
	      skip-alist (append (car ispell-tex-skip-alists)
				 (car (cdr ispell-tex-skip-alists))
				 skip-alist)))
    (if ispell-skip-html
	(setq skip-alist (append ispell-html-skip-alists skip-alist)))
    (if (and ispell-checking-message
	     (not (eq t ispell-checking-message)))
	(setq skip-alist (append ispell-checking-message skip-alist)))
    skip-alist))


(defun ispell-tex-arg-end (&optional arg)
  "Skip across ARG number of braces."
  (condition-case nil
      (progn
	(while (looking-at "[ \t\n]*\\[") (forward-sexp))
	(forward-sexp (or arg 1)))
    (error
     (message "Error skipping s-expressions at point %d." (point))
     (beep)
     (sit-for 2))))


(defun ispell-ignore-fcc (start end)
  "Delete the Fcc: message header when large attachments are included.
Return value nil if file with large attachments is saved.
This can be used to avoid multiple questions for multiple large attachments.
Returns point to starting location afterwards."
  (let ((result t))
    (if (and ispell-checking-message ispell-message-fcc-skip)
	(if (< ispell-message-fcc-skip (- end start))
	    (let (case-fold-search head-end)
	      (goto-char (point-min))
	      (setq head-end
		    (or (re-search-forward
			 (concat "^" (regexp-quote mail-header-separator) "$")
			 nil t)
			(re-search-forward "^$" nil t)
			(point-min)))
	      (goto-char (point-min))
	      (if (re-search-forward "^Fcc:" head-end t)
		  (if (y-or-n-p
		       "Save copy of this message with large attachments? ")
		      (setq result nil)
		    (beginning-of-line)
		    (kill-line 1)))
	      (goto-char end))))
    result))


(defun ispell-skip-region (key)
  "Skip across KEY and then to end of region.
Key lookup determines region to skip.
Point is placed at end of skipped region."
  ;; move over key to begin checking.
  (forward-char (length key))
  (let ((start (point))
	;; Regenerate each call... This function can change region definition.
	(alist (ispell-skip-region-list))
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
	       ((and (= 1 (length alist))
		     (stringp (car alist)))
		(re-search-forward (car alist) (point-max) t))
	       (t
		(setq null-skip t)	; error handling in functions!
		(if (consp (cdr alist))
		    (apply (car alist) (cdr alist))
		  (funcall (car alist)))))
	      (setq alist nil))
	  (setq alist (cdr alist))))))
    (if (and (= start (point)) (null null-skip))
	(progn
	  (message "Matching region end for `%s' point %d not found"
		   key (point))
	  (beep)
	  (sit-for 2)))))


(defun ispell-get-line (start end in-comment)
  "Grab the next line of data.
Returns a string with the line data."
  (let ((ispell-casechars (ispell-get-casechars))
	string)
    (cond				; LOOK AT THIS LINE AND SKIP OR PROCESS
     ((eolp)				; END OF LINE, just go to next line.
      (forward-line))
     ;;((looking-at "[-#@*+!%~^]")	; SKIP SPECIAL ISPELL CHARACTERS
     ;; (forward-char 1))		; not needed as quoted below.
     ((or (re-search-forward ispell-casechars end t) ; TEXT EXISTS
	  (re-search-forward "[][()${}]" end t)) ; or MATH COMMANDS
      (setq string (concat "^" in-comment
			   (buffer-substring-no-properties start end)
			   "\n"))
      (goto-char end))
     (t (goto-char end)))		; EMPTY LINE, skip it.
    string))


(defun ispell-looking-at (string)
  (let ((coding (ispell-get-coding-system))
	(len (length string)))
    (and (<= (+ (point) len) (point-max))
	 (equal (encode-coding-string string coding)
		(encode-coding-string (buffer-substring-no-properties
				       (point) (+ (point) len))
				      coding)))))

(defun ispell-process-line (string shift)
  "Send STRING, a line of text, to ispell and process the result.
This will modify the buffer for spelling errors.
Requires variables ISPELL-START and ISPELL-END to be defined in its
dynamic scope.
Returns the sum SHIFT due to changes in word replacements."
  ;;(declare special ispell-start ispell-end)
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
                 ;; There is a -1 offset here as the string is escaped
                 ;; with '^' to prevent us accidentally sending any
                 ;; ispell commands.
		 (copy-marker (+ ispell-start -1 (car (cdr poss)))))
		(word-len (length (car poss)))
		(line-end (copy-marker ispell-end))
		(line-start (copy-marker ispell-start))
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
	    (or (ispell-looking-at (car poss))
		;; This error occurs due to filter pipe problems
		(let* ((ispell-pipe-word (car poss))
		       (actual-point (marker-position word-start))
		       (actual-line (line-number-at-pos actual-point))
		       (actual-column (save-excursion (goto-char actual-point)
                                                      (current-column))))
		  (ispell-print-if-debug
                   "ispell-process-line: Ispell misalignment error:
  [Word from ispell pipe]: [%s], actual (point,line,column): (%s,%s,%s)\n"
                   ispell-pipe-word actual-point actual-line actual-column)
                  (error (concat "Ispell misalignment: word "
                                 "`%s' point %d; probably incompatible versions")
                         ispell-pipe-word actual-point)))
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
                         (string-match "[\\][]()[]\\|\\\\begin\\|\\$"
                                       (regexp-quote string)))
                    (error
                     "Don't start query replace on a line with math characters"
                     )
                  (set-marker line-end (point))
                  (setq ispell-filter nil
                        recheck-region t)))

            ;; Insert correction if needed.
            (cond
             ((or (null replace)
                  (equal 0 replace))	; ACCEPT/INSERT
              (if (equal 0 replace)     ; BUFFER-LOCAL DICT ADD
                  (ispell-add-per-file-word-list (car poss)))
              ;; Do not recheck accepted word on this line.
              (setq accept-list (cons (car poss) accept-list)))
             (t				; Replacement word selected or entered.
              (delete-region (point) (+ word-len (point)))
              (if (not (listp replace))
                  (progn
                    (insert replace)    ; Insert dictionary word.
                    (ispell-send-replacement (car poss) replace)
                    (setq accept-list (cons replace accept-list)))
                (let ((replace-word (car replace)))
                  ;; Recheck hand entered replacement word.
                  (insert replace-word)
                  (ispell-send-replacement (car poss) replace-word)
                  (if (car (cdr replace))
                      (save-window-excursion
                        (delete-other-windows) ; to correctly show help.
                        ;; Assume case-replace &
                        ;; case-fold-search correct?
                        (query-replace (car poss) (car replace) t)))
                  (goto-char word-start)
                  ;; Do not recheck if already accepted.
                  (if (member replace-word accept-list)
                      (setq accept-list (cons replace-word accept-list)
                            replace replace-word)
                    (let ((region-end (copy-marker ispell-region-end)))
                      (setq recheck-region ispell-filter
                            ispell-filter nil ; Save filter.
                            shift 0           ; Already accounted.
                            shift (ispell-region
                                   word-start
                                   (+ word-start (length replace-word))
                                   t shift))
                      (if (null shift)	; Quitting check.
                          (setq shift 0))
                      (set-marker ispell-region-end region-end)
                      (set-marker region-end nil)
                      (setq ispell-filter recheck-region
                            recheck-region nil
                            replace replace-word)))))
              (setq shift (+ shift (- (length replace) word-len)))))

            (if (not ispell-quit)
                (let (message-log-max)
                  (message
                   "Continuing spelling check using %s with %s dictionary..."
                   (file-name-nondirectory ispell-program-name)
                   (or ispell-current-dictionary "default"))))
            (sit-for 0)
            (setq ispell-start (marker-position line-start)
                  ispell-end (marker-position line-end))
            ;; Adjust markers when end of region lost from highlighting.
            (if (and (not recheck-region)
                     (< ispell-end (+ word-start word-len)))
                (setq ispell-end (+ word-start word-len)))
            (if (= word-start ispell-region-end)
                (set-marker ispell-region-end (+ word-start word-len)))
            ;; Going out of scope - unneeded.
            (set-marker line-start nil)
            (set-marker word-start nil)
            (set-marker line-end nil)))
      ;; Finished with misspelling!
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
(defun ispell-buffer-with-debug (&optional append)
  "`ispell-buffer' with some output sent to `ispell-debug-buffer' buffer.
If APPEND is non-n il, append the info to previous buffer if exists."
  (interactive)
  (let ((ispell-debug-buffer (ispell-create-debug-buffer append)))
    (ispell-buffer)))

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
  "Place point within the horizontal visibility of its window area."
  (if truncate-lines			; display truncating lines?
      ;; See if display needs to be scrolled.
      (let ((column (- (current-column) (max (window-hscroll) 1))))
	(if (and (< column 0) (> (window-hscroll) 0))
	    (scroll-right (max (- column) 10))
	  (if (>= column (- (window-width) 2))
	      (scroll-left (max (- column (window-width) -3) 10)))))))


;;; Interactive word completion.
;; Forces "previous-word" processing.  Do we want to make this selectable?

;;;###autoload
(defun ispell-complete-word (&optional interior-frag)
  "Try to complete the word before or at point.
If optional INTERIOR-FRAG is non-nil, then the word may be a character
sequence inside of a word.

Standard ispell choices are then available."
  ;; FIXME: completion-at-point-function.
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
	      (ispell-lookup-words
	       (concat (and interior-frag "*") word
		       (and interior-frag "*"))
	       (or ispell-complete-word-dict
		   ispell-alternate-dictionary))))
    (cond ((eq possibilities t)
	   (message "No word to complete"))
	  ((null possibilities)
	   (message "No match for \"%s\"" word))
	  (t				; There is a modification...
	   (setq case-fold-search nil)	; Try and respect case of word.
	   (cond
	    ((string-equal (upcase word) word)
	     (setq possibilities (mapcar #'upcase possibilities)))
	    ((eq (upcase (aref word 0)) (aref word 0))
             (setq possibilities (mapcar (function
                                          (lambda (pos)
                                            (if (eq (aref word 0) (aref pos 0))
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
  (if (and (boundp 'transient-mark-mode) transient-mark-mode
	   (boundp 'mark-active) mark-active)
      (ispell-region (region-beginning) (region-end))
    (ispell-buffer)))


;;; **********************************************************************
;;; 			Ispell Minor Mode
;;; **********************************************************************

(defvar ispell-minor-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map " " 'ispell-minor-check)
    (define-key map "\r" 'ispell-minor-check)
    map)
  "Keymap used for Ispell minor mode.")

;;;###autoload
(define-minor-mode ispell-minor-mode
  "Toggle last-word spell checking (Ispell minor mode).

Ispell minor mode is a buffer-local minor mode.  When enabled,
typing SPC or RET warns you if the previous word is incorrectly
spelled.

All the buffer-local variables and dictionaries are ignored.  To
read them into the running Ispell process, type \\[ispell-word]
SPC.

For spell-checking \"on the fly\", not just after typing SPC or
RET, use `flyspell-mode'."
  nil " Spell" ispell-minor-keymap)

(defun ispell-minor-check ()
  "Check previous word, then continue with the normal binding of this key.
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

(defvar ispell-message-text-end
  (mapconcat (function identity)
	     '(
	       ;; Don't spell check signatures
	       "^-- $"
	       ;; Matches PostScript files.
	       ;;"^%!PS-Adobe-[123].0"
	       ;; Matches uuencoded text
	       ;;"^begin [0-9][0-9][0-9] .*\nM.*\nM.*\nM"
	       ;; Matches shell files (especially auto-decoding)
	       "^#! /bin/[ck]?sh"
	       ;; Matches context difference listing
	       "\\(\\(^cd .*\n\\)?diff -c .*\\)?\n\\*\\*\\* .*\n--- .*\n\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*"
	       ;; Matches unidiff difference listing
	       "\\(diff -u .*\\)?\n--- .*\n\\+\\+\\+ .*\n@@ [-+][0-9]+,[0-9]+ [-+][0-9]+,[0-9]+ @@"
	       ;; Matches reporter.el bug report
	       "^current state:\n==============\n"
	       ;; Matches commonly used "cut" boundaries
	       "^\\(- \\)?[-=_]+\\s ?\\(cut here\\|Environment Follows\\)")
	     "\\|")
  "Text beyond which `ispell-message' will not spell-check.
If it is a string, limit is the first occurrence of that regular expression.
Otherwise, it must be a function which is called to get the limit.")
(put 'ispell-message-text-end 'risky-local-variable t)


(defun ispell-mime-multipartp (&optional limit)
  "Return multipart message start boundary or nil if none."
  ;; caller must ensure `case-fold-search' is set to t
  (and
   (re-search-forward
    "Content-Type: *multipart/\\([^ \t\n]*;[ \t]*[\n]?[ \t]*\\)+boundary="
    limit t)
   (let (boundary)
     (if (looking-at "\"")
	 (let (start)
	   (forward-char)
	   (setq start (point))
	   (while (not (looking-at "\""))
	     (forward-char 1))
	   (setq boundary (buffer-substring-no-properties start (point))))
       (let ((start (point)))
	 (while (looking-at "[-0-9a-zA-Z'()+_,./:=?]")
	   (forward-char))
	 (setq boundary (buffer-substring-no-properties start (point)))))
     (if (< (length boundary) 1)
	 (setq boundary nil)
       (concat "--" boundary)))))


(defun ispell-mime-skip-part (boundary)
  "Move point across header, or entire MIME part if message is encoded.
All specified types except `7bit' `8bit' and `quoted-printable' are considered
encoded and therefore skipped.  See rfc 1521, 2183, ...
If no boundary is given, then entire message is skipped.

This starts one line ABOVE the MIME content messages, on the boundary marker,
for operation with the generic region-skipping code.
This places new MIME boundaries into variable `ispell-checking-message'."
  (forward-line)			; skip over boundary to headers
  (let ((save-case-fold-search case-fold-search)
	(continuep t)
	textp)
    (setq case-fold-search t
	  ispell-skip-html nil)
    (while continuep
      (setq continuep nil)
      (if (looking-at "Content-Type: *text/")
	  (progn
	    (goto-char (match-end 0))
	    (if (looking-at "html")
		(setq ispell-skip-html t))
	    (setq textp t
		  continuep t)
	    (re-search-forward "\\(.*;[ \t]*[\n]\\)*.*$" nil t)
	    (forward-line)))
      (if (looking-at "Content-Transfer-Encoding: *\\([^ \t\n]*\\)")
	  (let ((match (buffer-substring (match-beginning 1) (match-end 1))))
	    (setq textp (member (upcase match)
				;; only spell check the following encodings:
				'("7BIT" "8BIT" "QUOTED-PRINTABLE" "BINARY"))
		  continuep t)
	    (goto-char (match-end 0))
	    (re-search-forward "\\(.*;[ \t]*[\n]\\)*.*$" nil t)
	    (forward-line)))
      ;; hierarchical boundary definition
      (if (looking-at "Content-Type: *multipart/")
	  (let ((new-boundary (ispell-mime-multipartp)))
	    (if (string-match new-boundary boundary)
		(setq continuep t)
	      ;; first pass redefine skip function to include new boundary
	      ;;(re-search-backward boundary nil t)
	      (forward-line)
	      (setq ispell-checking-message
		    (cons
		     (list new-boundary 'ispell-mime-skip-part new-boundary)
		     (if (eq t ispell-checking-message) nil
		       ispell-checking-message))
		    textp t
		    continuep t)))
	;; Skip all MIME headers that don't affect spelling
	(if (looking-at "Content-[^ \t]*: *\\(.*;[ \t]*[\n]\\)*.*$")
	    (progn
	      (setq continuep t)
	      (goto-char (match-end 0))
	      (forward-line)))))

    (setq case-fold-search save-case-fold-search)
    (if textp
	(point)
      ;; encoded message.  Skip to boundary, or entire message.
      (if (not boundary)
	  (goto-char (point-max))
	(re-search-forward boundary nil t)
	(beginning-of-line)
	(point)))))


;;;###autoload
(defun ispell-message ()
  "Check the spelling of a mail message or news post.
Don't check spelling of message headers except the Subject field.
Don't check included messages.

To abort spell checking of a message region and send the message anyway,
use the `x' command.  (Any subsequent regions will be checked.)
The `X' command aborts sending the message so that you can edit the buffer.

To spell-check whenever a message is sent, include the appropriate lines
in your init file:
   (add-hook \\='message-send-hook #\\='ispell-message)  ;; GNUS 5
   (add-hook \\='news-inews-hook #\\='ispell-message)    ;; GNUS 4
   (add-hook \\='mail-send-hook  #\\='ispell-message)
   (add-hook \\='mh-before-send-letter-hook #\\='ispell-message)

You can bind this to the key C-c i in GNUS or mail by adding to
`news-reply-mode-hook' or `mail-mode-hook' the following lambda expression:
   (function (lambda () (local-set-key \"\\C-ci\" \\='ispell-message)))"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let* (boundary mimep
	   (ispell-skip-region-alist-save ispell-skip-region-alist)
	   ;; Nil when message came from outside (eg calling Emacs as editor)
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
	     (if (ispell-non-empty-string mail-yank-prefix)
	       "   \\|\t"))
	   (cite-regexp			;Prefix of quoted text
	    (cond
	     ((functionp 'sc-cite-regexp)	; supercite >= 3.0
	      (with-no-warnings
	       (concat "\\(" (sc-cite-regexp) "\\)" "\\|"
		       (ispell-non-empty-string sc-reference-tag-string))))
	     ((equal major-mode 'message-mode)  ; GNUS >= 5
	      (concat "In article <" "\\|"
		      "[^,;&+=\n]+ <[^,;&+=]+> writes:" "\\|"
		      (with-no-warnings message-cite-prefix-regexp)
		      "\\|"
		      default-prefix))
	     ((equal major-mode 'mh-letter-mode) ; mh mail message
	      (concat "[^,;&+=\n]+ writes:" "\\|"
		      (with-no-warnings
		       (ispell-non-empty-string mh-ins-buf-prefix))))
	     ((not internal-messagep)	; Assume nn sent us this message.
	      (concat "In [a-zA-Z.]+ you write:" "\\|"
		      "In <[^,;&+=]+> [^,;&+=]+ writes:" "\\|"
		      " *> *"))
	     ((boundp 'vm-included-text-prefix) ; VM mail message
	      (concat "[^,;&+=\n]+ writes:" "\\|"
		      (ispell-non-empty-string vm-included-text-prefix)))
	     (t default-prefix)))
	   (ispell-skip-region-alist
	    (cons (list (ispell--make-filename-or-URL-re))
                  (cons (list (concat "^\\(" cite-regexp "\\)")
                              (function forward-line))
                        ispell-skip-region-alist)))
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
	    (setq case-fold-search t
		  mimep (re-search-forward "MIME-Version:" end-of-headers t))
	    (goto-char (point-min))
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
	    (if mimep
		(progn
		  (goto-char (point-min))
		  (setq boundary (ispell-mime-multipartp end-of-headers))))
	    ;; Adjust message limit to MIME message if necessary.
	    (and boundary
		 (re-search-forward (concat boundary "--") nil t)
		 (re-search-backward boundary nil t)
		 (< (point) (marker-position limit))
		 (set-marker limit (point)))
	    (goto-char (point-min))
	    ;; Select type or skip checking if this is a non-multipart message
	    ;; Point moved to end of buffer if region is encoded.
	    (when (and mimep (not boundary))
		  (goto-char (point-min))
		  (re-search-forward "Content-[^ \t]*:" end-of-headers t)
		  (forward-line -1)	; following fn starts one line above
		  (ispell-mime-skip-part nil)
		  ;; if message-text-end region, limit may be less than point.
		  (if (> (point) limit)
                  (set-marker limit (point))))
	    (goto-char (max end-of-headers (point)))
	    (forward-line 1)
	    (setq case-fold-search old-case-fold-search)
	    ;; Define MIME regions to skip.
	    (if boundary
		(setq ispell-checking-message
		      (list (list boundary 'ispell-mime-skip-part boundary))))
	    (ispell-region (point) limit))
	(set-marker end-of-headers nil)
	(set-marker limit nil)
	(setq ispell-skip-region-alist ispell-skip-region-alist-save
	      ispell-skip-html nil
	      case-fold-search old-case-fold-search)))))


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
Includes LaTeX/Nroff modes and extended character mode."
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
      (setq ispell-skip-html
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
	(let ((end (point-at-eol))
	      string)
	  (search-forward ispell-parsing-keyword)
	  (while (re-search-forward " *\\([^ \"]+\\)" end t)
	    ;; space separated definitions.
	    (setq string (downcase (match-string-no-properties 1)))
	    (cond ((and (string-match "latex-mode" string)
			(not (eq 'exclusive ispell-check-comments)))
		   (ispell-send-string "+\n~tex\n"))
		  ((string-match "nroff-mode" string)
		   (ispell-send-string "-\n~nroff\n"))
		  ((string-match "~" string) ; Set extended character mode.
		   (ispell-send-string (concat string "\n")))
		  (t (message "Invalid Ispell Parsing argument!")
		     (sit-for 2))))))))


;; Can kill the current ispell process

(defun ispell-buffer-local-dict (&optional no-reload)
  "Initializes local dictionary and local personal dictionary.
If optional NO-RELOAD is non-nil, do not reload any dictionary.
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
      (unless ispell-local-dictionary-overridden
	(if (search-backward ispell-dictionary-keyword nil t)
	    (progn
	      (search-forward ispell-dictionary-keyword)
	      (setq end (point-at-eol))
	      (if (re-search-forward " *\\([^ \"]+\\)" end t)
		  (setq ispell-local-dictionary
			(match-string-no-properties 1))))))
      (goto-char (point-max))
      (if (search-backward ispell-pdict-keyword nil t)
	  (progn
	    (search-forward ispell-pdict-keyword)
	    (setq end (point-at-eol))
	    (if (re-search-forward " *\\([^ \"]+\\)" end t)
		(setq ispell-local-pdict
		      (match-string-no-properties 1)))))))
  (unless no-reload
    ;; Reload if new dictionary (maybe the personal one) defined.
    (ispell-internal-change-dictionary)))


(defun ispell-buffer-local-words ()
  "Load the buffer-local dictionary in the current buffer."
  ;; If there's an existing ispell process that's wrong for this use,
  ;; kill it.
  (if (and ispell-buffer-local-name
	   (not (equal ispell-buffer-local-name (buffer-name))))
      (ispell-kill-ispell t))
  ;; Actually start a new ispell process, because we need
  ;; to send commands now to specify the local words to it.
  (ispell-init-process)
  (dolist (session-localword ispell-buffer-session-localwords)
    (ispell-send-string (concat "@" session-localword "\n")))
  (or ispell-buffer-local-name
      (if ispell-buffer-session-localwords
	  (setq ispell-buffer-local-name (buffer-name))))
  (save-excursion
    (goto-char (point-min))
    (while (search-forward ispell-words-keyword nil t)
      (or ispell-buffer-local-name
	  (setq ispell-buffer-local-name (buffer-name)))
      (let ((end (point-at-eol))
	    (ispell-casechars (ispell-get-casechars))
	    string)
	;; buffer-local words separated by a space, and can contain
	;; any character other than a space.  Not rigorous enough.
	(while (re-search-forward " *\\([^ ]+\\)" end t)
	  (setq string (match-string-no-properties 1))
	  ;; This can fail when string contains a word with invalid chars.
	  ;; Error handling needs to be added between ispell and Emacs.
	  (if (and (< 1 (length string))
		   (equal 0 (string-match ispell-casechars string)))
	      (ispell-send-string (concat "@" string "\n"))))))))


;; Returns optionally adjusted region-end-point.

;; If comment-normalize-vars is defined, newcomment must be loaded.
(declare-function comment-normalize-vars "newcomment" (&optional noerror))

(defun ispell-add-per-file-word-list (word)
  "Add WORD to the per-file word list."
  (or ispell-buffer-local-name
      (setq ispell-buffer-local-name (buffer-name)))
  (save-excursion
    (goto-char (point-min))
    (let (line-okay search done found)
      (while (not done)
        (let ((case-fold-search nil))
          (setq search (search-forward ispell-words-keyword nil 'move)
	      found (or found search)
	      line-okay (< (+ (length word) 1 ; 1 for space after word..
			      (progn (end-of-line) (current-column)))
                             fill-column)))
	(if (or (and search line-okay)
		(null search))
	    (progn
	      (setq done t)
	      (if (null search)
		  (progn
		    (open-line 1)
		    (unless found (newline))
		    (insert (if comment-start
                                (concat
                                  (progn
                                   ;; Try and use the proper comment marker,
                                   ;; e.g. ";;" rather than ";".
                                    (comment-normalize-vars)
                                    (comment-padright comment-start
                                                      (comment-add nil))
                                    comment-start)
                                  " ")
                              "")
                            ispell-words-keyword)
                    (if (and comment-end (> (length comment-end) 0))
			(save-excursion
			  (newline)
			  (insert comment-end)))))
	      (insert (concat " " word))))))))

(provide 'ispell)


;;; LOCAL VARIABLES AND BUFFER-LOCAL VALUE EXAMPLES.

;; Local Variable options:
;; mode: name(-mode)
;; eval: expression
;; local-variable: value

;; The following sets the buffer local dictionary to `american' English
;; and spell checks only comments.

;; Local Variables:
;; mode: emacs-lisp
;; comment-column: 40
;; ispell-check-comments: exclusive
;; ispell-local-dictionary: "american"
;; End:


;;; MORE EXAMPLES OF ISPELL BUFFER-LOCAL VALUES

;; The following places this file in nroff parsing and extended char modes.
;; Local IspellParsing: nroff-mode ~nroff
;; Change IspellPersDict to IspellPersDict: to enable the following line.
;; Local IspellPersDict ~/.ispell_lisp
;; The following were automatically generated by ispell using the 'A' command:
; LocalWords:  settable alist inews mh frag pdict Wildcards iconify arg tex kss
; LocalWords:  alists minibuffer bufferp autoload loaddefs aff Dansk KOI SPC op
; LocalWords:  Francais Nederlands charset autoloaded popup nonmenu regexp num
; LocalWords:  AMStex hspace includeonly nocite epsfig displaymath eqnarray reg
; LocalWords:  minipage pers dict unhighlight buf grep sync prev inc
; LocalWords:  fn oldot NB AIX msg init read's bufs pt cmd Quinlan eg
; LocalWords:  uuencoded unidiff sc nn VM SGML eval IspellPersDict
; LocalWords:  lns HTML casechars Multibyte

;;; ispell.el ends here
