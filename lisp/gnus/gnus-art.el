;;; gnus-art.el --- article mode commands for Gnus
;; Copyright (C) 1996,97 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
;; Keywords: news

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'custom)
(require 'gnus)
(require 'gnus-sum)
(require 'gnus-spec)
(require 'gnus-int)
(require 'browse-url)

(defgroup gnus-article nil
  "Article display."
  :link '(custom-manual "(gnus)The Article Buffer")
  :group 'gnus)

(defgroup gnus-article-hiding nil
  "Hiding article parts."
  :link '(custom-manual "(gnus)Article Hiding")
  :group 'gnus-article)

(defgroup gnus-article-highlight nil
  "Article highlighting."
  :link '(custom-manual "(gnus)Article Highlighting")
  :group 'gnus-article
  :group 'gnus-visual)

(defgroup gnus-article-signature nil
  "Article signatures."
  :link '(custom-manual "(gnus)Article Signature")
  :group 'gnus-article)

(defgroup gnus-article-headers nil
  "Article headers."
  :link '(custom-manual "(gnus)Hiding Headers")
  :group 'gnus-article)

(defgroup gnus-article-washing nil
  "Special commands on articles."
  :link '(custom-manual "(gnus)Article Washing")
  :group 'gnus-article)

(defgroup gnus-article-emphasis nil
  "Fontisizing articles."
  :link '(custom-manual "(gnus)Article Fontisizing")
  :group 'gnus-article)

(defgroup gnus-article-saving nil
  "Saving articles."
  :link '(custom-manual "(gnus)Saving Articles")
  :group 'gnus-article)

(defgroup gnus-article-mime nil
  "Worshiping the MIME wonder."
  :link '(custom-manual "(gnus)Using MIME")
  :group 'gnus-article)

(defgroup gnus-article-buttons nil
  "Pushable buttons in the article buffer."
  :link '(custom-manual "(gnus)Article Buttons")
  :group 'gnus-article)

(defgroup gnus-article-various nil
  "Other article options."
  :link '(custom-manual "(gnus)Misc Article")
  :group 'gnus-article)

(defcustom gnus-ignored-headers
  '("^Path:" "^Posting-Version:" "^Article-I.D.:" "^Expires:"
    "^Date-Received:" "^References:" "^Control:" "^Xref:" "^Lines:"
    "^Posted:" "^Relay-Version:" "^Message-ID:" "^Nf-ID:" "^Nf-From:"
    "^Approved:" "^Sender:" "^Received:" "^Mail-from:")
  "All headers that match this regexp will be hidden.
This variable can also be a list of regexps of headers to be ignored.
If `gnus-visible-headers' is non-nil, this variable will be ignored."
  :type '(choice :custom-show nil
		 regexp
		 (repeat regexp))
  :group 'gnus-article-hiding)

(defcustom gnus-visible-headers
  "^From:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^Followup-To:\\|^Reply-To:\\|^Organization:\\|^Summary:\\|^Keywords:\\|^To:\\|^Cc:\\|^Posted-To:\\|^Mail-Copies-To:\\|^Apparently-To:\\|^Gnus-Warning:\\|^Resent-From"
  "All headers that do not match this regexp will be hidden.
This variable can also be a list of regexp of headers to remain visible.
If this variable is non-nil, `gnus-ignored-headers' will be ignored."
  :type '(repeat :value-to-internal (lambda (widget value)
				      (custom-split-regexp-maybe value))
		 :match (lambda (widget value)
			  (or (stringp value)
			      (widget-editable-list-match widget value)))
		 regexp)
  :group 'gnus-article-hiding)

(defcustom gnus-sorted-header-list
  '("^From:" "^Subject:" "^Summary:" "^Keywords:" "^Newsgroups:"
    "^Followup-To:" "^To:" "^Cc:" "^Date:" "^Organization:")
  "This variable is a list of regular expressions.
If it is non-nil, headers that match the regular expressions will
be placed first in the article buffer in the sequence specified by
this list."
  :type '(repeat regexp)
  :group 'gnus-article-hiding)

(defcustom gnus-boring-article-headers '(empty followup-to reply-to)
  "Headers that are only to be displayed if they have interesting data.
Possible values in this list are `empty', `newsgroups', `followup-to',
`reply-to', and `date'."
  :type '(set (const :tag "Headers with no content." empty)
	      (const :tag "Newsgroups with only one group." newsgroups)
	      (const :tag "Followup-to identical to newsgroups." followup-to)
	      (const :tag "Reply-to identical to from." reply-to)
	      (const :tag "Date less than four days old." date))
  :group 'gnus-article-hiding)

(defcustom gnus-signature-separator '("^-- $" "^-- *$")
  "Regexp matching signature separator.
This can also be a list of regexps.  In that case, it will be checked
from head to tail looking for a separator.  Searches will be done from
the end of the buffer."
  :type '(repeat string)
  :group 'gnus-article-signature)

(defcustom gnus-signature-limit nil
   "Provide a limit to what is considered a signature.
If it is a number, no signature may not be longer (in characters) than
that number.  If it is a floating point number, no signature may be
longer (in lines) than that number.  If it is a function, the function
will be called without any parameters, and if it returns nil, there is
no signature in the buffer.  If it is a string, it will be used as a
regexp.  If it matches, the text in question is not a signature."
  :type '(choice integer number function regexp)
  :group 'gnus-article-signature)

(defcustom gnus-hidden-properties '(invisible t intangible t)
  "Property list to use for hiding text."
  :type 'sexp
  :group 'gnus-article-hiding)

(defcustom gnus-article-x-face-command
  "{ echo '/* Width=48, Height=48 */'; uncompface; } | icontopbm | xv -quit -"
  "String or function to be executed to display an X-Face header.
If it is a string, the command will be executed in a sub-shell
asynchronously.	 The compressed face will be piped to this command."
  :type 'string				;Leave function case to Lisp.
  :group 'gnus-article-washing)

(defcustom gnus-article-x-face-too-ugly nil
  "Regexp matching posters whose face shouldn't be shown automatically."
  :type 'regexp
  :group 'gnus-article-washing)

(defcustom gnus-emphasis-alist
  (let ((format
	 "\\(\\s-\\|^\\|[-\"]\\|\\s(\\|\\s)\\)\\(%s\\(\\w+\\(\\s-+\\w+\\)*[.,]?\\)%s\\)\\(\\s-\\|[-?!.,;:\"]\\|\\s(\\|\\s)\\)")
	(types
	 '(("_" "_" underline)
	   ("/" "/" italic)
	   ("\\*" "\\*" bold)
	   ("_/" "/_" underline-italic)
	   ("_\\*" "\\*_" underline-bold)
	   ("\\*/" "/\\*" bold-italic)
	   ("_\\*/" "/\\*_" underline-bold-italic))))
    `(("\\(\\s-\\|^\\)\\(_\\(\\(\\w\\|_[^_]\\)+\\)_\\)\\(\\s-\\|[?!.,;]\\)"
       2 3 gnus-emphasis-underline)
      ,@(mapcar
	 (lambda (spec)
	   (list
	    (format format (car spec) (cadr spec))
	    2 3 (intern (format "gnus-emphasis-%s" (caddr spec)))))
	 types)))
  "Alist that says how to fontify certain phrases.
Each item looks like this:

  (\"_\\\\(\\\\w+\\\\)_\" 0 1 'underline)

The first element is a regular expression to be matched.  The second
is a number that says what regular expression grouping used to find
the entire emphasized word.  The third is a number that says what
regexp grouping should be displayed and highlighted.  The fourth
is the face used for highlighting."
  :type '(repeat (list :value ("" 0 0 default)
		       regexp
		       (integer :tag "Match group")
		       (integer :tag "Emphasize group")
		       face))
  :group 'gnus-article-emphasis)

(defface gnus-emphasis-bold '((t (:bold t)))
  "Face used for displaying strong emphasized text (*word*)."
  :group 'gnus-article-emphasis)

(defface gnus-emphasis-italic '((t (:italic t)))
  "Face used for displaying italic emphasized text (/word/)."
  :group 'gnus-article-emphasis)

(defface gnus-emphasis-underline '((t (:underline t)))
  "Face used for displaying underlined emphasized text (_word_)."
  :group 'gnus-article-emphasis)

(defface gnus-emphasis-underline-bold '((t (:bold t :underline t)))
  "Face used for displaying underlined bold emphasized text (_*word*_)."
  :group 'gnus-article-emphasis)

(defface gnus-emphasis-underline-italic '((t (:italic t :underline t)))
  "Face used for displaying underlined italic emphasized text (_*word*_)."
  :group 'gnus-article-emphasis)

(defface gnus-emphasis-bold-italic '((t (:bold t :italic t)))
  "Face used for displaying bold italic emphasized text (/*word*/)."
  :group 'gnus-article-emphasis)

(defface gnus-emphasis-underline-bold-italic
  '((t (:bold t :italic t :underline t)))
  "Face used for displaying underlined bold italic emphasized text.
Esample: (_/*word*/_)."
  :group 'gnus-article-emphasis)

(defcustom gnus-article-time-format "%a, %b %d %Y %T %Z"
  "Format for display of Date headers in article bodies.
See `format-time-zone' for the possible values."
  :type 'string
  :link '(custom-manual "(gnus)Article Date")
  :group 'gnus-article-washing)

(eval-and-compile
  (autoload 'hexl-hex-string-to-integer "hexl")
  (autoload 'timezone-make-date-arpa-standard "timezone")
  (autoload 'mail-extract-address-components "mail-extr"))

(defcustom gnus-save-all-headers t
  "*If non-nil, don't remove any headers before saving."
  :group 'gnus-article-saving
  :type 'boolean)

(defcustom gnus-prompt-before-saving 'always
  "*This variable says how much prompting is to be done when saving articles.
If it is nil, no prompting will be done, and the articles will be
saved to the default files.  If this variable is `always', each and
every article that is saved will be preceded by a prompt, even when
saving large batches of articles.  If this variable is neither nil not
`always', there the user will be prompted once for a file name for
each invocation of the saving commands."
  :group 'gnus-article-saving
  :type '(choice (item always)
		 (item :tag "never" nil)
		 (sexp :tag "once" :format "%t")))

(defcustom gnus-saved-headers gnus-visible-headers
  "Headers to keep if `gnus-save-all-headers' is nil.
If `gnus-save-all-headers' is non-nil, this variable will be ignored.
If that variable is nil, however, all headers that match this regexp
will be kept while the rest will be deleted before saving."
  :group 'gnus-article-saving
  :type '(repeat string))

(defcustom gnus-default-article-saver 'gnus-summary-save-in-rmail
  "A function to save articles in your favourite format.
The function must be interactively callable (in other words, it must
be an Emacs command).

Gnus provides the following functions:

* gnus-summary-save-in-rmail (Rmail format)
* gnus-summary-save-in-mail (Unix mail format)
* gnus-summary-save-in-folder (MH folder)
* gnus-summary-save-in-file (article format)
* gnus-summary-save-in-vm (use VM's folder format)
* gnus-summary-write-to-file (article format -- overwrite)."
  :group 'gnus-article-saving
  :type '(radio (function-item gnus-summary-save-in-rmail)
		(function-item gnus-summary-save-in-mail)
		(function-item gnus-summary-save-in-folder)
		(function-item gnus-summary-save-in-file)
		(function-item gnus-summary-save-in-vm)
		(function-item gnus-summary-write-to-file)))

(defcustom gnus-rmail-save-name 'gnus-plain-save-name
  "A function generating a file name to save articles in Rmail format.
The function is called with NEWSGROUP, HEADERS, and optional LAST-FILE."
  :group 'gnus-article-saving
  :type 'function)

(defcustom gnus-mail-save-name 'gnus-plain-save-name
  "A function generating a file name to save articles in Unix mail format.
The function is called with NEWSGROUP, HEADERS, and optional LAST-FILE."
  :group 'gnus-article-saving
  :type 'function)

(defcustom gnus-folder-save-name 'gnus-folder-save-name
  "A function generating a file name to save articles in MH folder.
The function is called with NEWSGROUP, HEADERS, and optional LAST-FOLDER."
  :group 'gnus-article-saving
  :type 'function)

(defcustom gnus-file-save-name 'gnus-numeric-save-name
  "A function generating a file name to save articles in article format.
The function is called with NEWSGROUP, HEADERS, and optional
LAST-FILE."
  :group 'gnus-article-saving
  :type 'function)

(defcustom gnus-split-methods
  '((gnus-article-archive-name)
    (gnus-article-nndoc-name))
  "Variable used to suggest where articles are to be saved.
For instance, if you would like to save articles related to Gnus in
the file \"gnus-stuff\", and articles related to VM in \"vm-stuff\",
you could set this variable to something like:

 '((\"^Subject:.*gnus\\|^Newsgroups:.*gnus\" \"gnus-stuff\")
   (\"^Subject:.*vm\\|^Xref:.*vm\" \"vm-stuff\"))

This variable is an alist where the where the key is the match and the
value is a list of possible files to save in if the match is non-nil.

If the match is a string, it is used as a regexp match on the
article.  If the match is a symbol, that symbol will be funcalled
from the buffer of the article to be saved with the newsgroup as the
parameter.  If it is a list, it will be evaled in the same buffer.

If this form or function returns a string, this string will be used as
a possible file name; and if it returns a non-nil list, that list will
be used as possible file names."
  :group 'gnus-article-saving
  :type '(repeat (choice (list function)
			 (cons regexp (repeat string))
			 sexp)))

(defcustom gnus-strict-mime t
  "*If nil, MIME-decode even if there is no Mime-Version header."
  :group 'gnus-article-mime
  :type 'boolean)

(defcustom gnus-show-mime-method 'metamail-buffer
  "Function to process a MIME message.
The function is called from the article buffer."
  :group 'gnus-article-mime
  :type 'function)

(defcustom gnus-decode-encoded-word-method 'gnus-article-de-quoted-unreadable
  "*Function to decode MIME encoded words.
The function is called from the article buffer."
  :group 'gnus-article-mime
  :type 'function)

(defcustom gnus-page-delimiter "^\^L"
  "*Regexp describing what to use as article page delimiters.
The default value is \"^\^L\", which is a form linefeed at the
beginning of a line."
  :type 'regexp
  :group 'gnus-article-various)

(defcustom gnus-article-mode-line-format "Gnus: %%b %S"
  "*The format specification for the article mode line.
See `gnus-summary-mode-line-format' for a closer description."
  :type 'string
  :group 'gnus-article-various)

(defcustom gnus-article-mode-hook nil
  "*A hook for Gnus article mode."
  :type 'hook
  :group 'gnus-article-various)

(defcustom gnus-article-menu-hook nil
  "*Hook run after the creation of the article mode menu."
  :type 'hook
  :group 'gnus-article-various)

(defcustom gnus-article-prepare-hook nil
  "*A hook called after an article has been prepared in the article buffer.
If you want to run a special decoding program like nkf, use this hook."
  :type 'hook
  :group 'gnus-article-various)

(defcustom gnus-article-button-face 'bold
  "Face used for highlighting buttons in the article buffer.

An article button is a piece of text that you can activate by pressing
`RET' or `mouse-2' above it."
  :type 'face
  :group 'gnus-article-buttons)

(defcustom gnus-article-mouse-face 'highlight
  "Face used for mouse highlighting in the article buffer.

Article buttons will be displayed in this face when the cursor is
above them."
  :type 'face
  :group 'gnus-article-buttons)

(defcustom gnus-signature-face 'italic
  "Face used for highlighting a signature in the article buffer."
  :type 'face
  :group 'gnus-article-highlight
  :group 'gnus-article-signature)

(defface gnus-header-from-face
  '((((class color)
      (background dark))
     (:foreground "spring green" :bold t))
    (((class color)
      (background light))
     (:foreground "red3" :bold t))
    (t
     (:bold t :italic t)))
  "Face used for displaying from headers."
  :group 'gnus-article-headers
  :group 'gnus-article-highlight)

(defface gnus-header-subject-face
  '((((class color)
      (background dark))
     (:foreground "SeaGreen3" :bold t))
    (((class color)
      (background light))
     (:foreground "red4" :bold t))
    (t
     (:bold t :italic t)))
  "Face used for displaying subject headers."
  :group 'gnus-article-headers
  :group 'gnus-article-highlight)

(defface gnus-header-newsgroups-face
  '((((class color)
      (background dark))
     (:foreground "yellow" :bold t :italic t))
    (((class color)
      (background light))
     (:foreground "MidnightBlue" :bold t :italic t))
    (t
     (:bold t :italic t)))
  "Face used for displaying newsgroups headers."
  :group 'gnus-article-headers
  :group 'gnus-article-highlight)

(defface gnus-header-name-face
  '((((class color)
      (background dark))
     (:foreground "SeaGreen"))
    (((class color)
      (background light))
     (:foreground "maroon"))
    (t
     (:bold t)))
  "Face used for displaying header names."
  :group 'gnus-article-headers
  :group 'gnus-article-highlight)

(defface gnus-header-content-face
  '((((class color)
      (background dark))
     (:foreground "forest green" :italic t))
    (((class color)
      (background light))
     (:foreground "indianred4" :italic t))
    (t
     (:italic t)))  "Face used for displaying header content."
  :group 'gnus-article-headers
  :group 'gnus-article-highlight)

(defcustom gnus-header-face-alist
  '(("From" nil gnus-header-from-face)
    ("Subject" nil gnus-header-subject-face)
    ("Newsgroups:.*," nil gnus-header-newsgroups-face)
    ("" gnus-header-name-face gnus-header-content-face))
  "Controls highlighting of article header.

An alist of the form (HEADER NAME CONTENT).

HEADER is a regular expression which should match the name of an
header header and NAME and CONTENT are either face names or nil.

The name of each header field will be displayed using the face
specified by the first element in the list where HEADER match the
header name and NAME is non-nil.  Similarly, the content will be
displayed by the first non-nil matching CONTENT face."
  :group 'gnus-article-headers
  :group 'gnus-article-highlight
  :type '(repeat (list (regexp :tag "Header")
		       (choice :tag "Name"
			       (item :tag "skip" nil)
			       (face :value default))
		       (choice :tag "Content"
			       (item :tag "skip" nil)
			       (face :value default)))))

;;; Internal variables

(defvar gnus-article-mode-syntax-table
  (let ((table (copy-syntax-table text-mode-syntax-table)))
    (modify-syntax-entry ?- "w" table)
    (modify-syntax-entry ?> ")" table)
    (modify-syntax-entry ?< "(" table)
    table)
  "Syntax table used in article mode buffers.
Initialized from `text-mode-syntax-table.")

(defvar gnus-save-article-buffer nil)

(defvar gnus-article-mode-line-format-alist
    (nconc '((?w (gnus-article-wash-status) ?s))
	   gnus-summary-mode-line-format-alist))

(defvar gnus-number-of-articles-to-be-saved nil)

(defvar gnus-inhibit-hiding nil)

(defsubst gnus-article-hide-text (b e props)
  "Set text PROPS on the B to E region, extending `intangible' 1 past B."
  (add-text-properties b e props)
  (when (memq 'intangible props)
    (put-text-property
     (max (1- b) (point-min))
     b 'intangible (cddr (memq 'intangible props)))))

(defsubst gnus-article-unhide-text (b e)
  "Remove hidden text properties from region between B and E."
  (remove-text-properties b e gnus-hidden-properties)
  (when (memq 'intangible gnus-hidden-properties)
    (put-text-property (max (1- b) (point-min))
		       b 'intangible nil)))

(defun gnus-article-hide-text-type (b e type)
  "Hide text of TYPE between B and E."
  (gnus-article-hide-text
   b e (cons 'article-type (cons type gnus-hidden-properties))))

(defun gnus-article-unhide-text-type (b e type)
  "Hide text of TYPE between B and E."
  (remove-text-properties
   b e (cons 'article-type (cons type gnus-hidden-properties)))
  (when (memq 'intangible gnus-hidden-properties)
    (put-text-property (max (1- b) (point-min))
		       b 'intangible nil)))

(defun gnus-article-hide-text-of-type (type)
  "Hide text of TYPE in the current buffer."
  (save-excursion
    (let ((b (point-min))
	  (e (point-max)))
      (while (setq b (text-property-any b e 'article-type type))
	(add-text-properties b (incf b) gnus-hidden-properties)))))

(defun gnus-article-delete-text-of-type (type)
  "Delete text of TYPE in the current buffer."
  (save-excursion
    (let ((e (point-min))
	  b)
      (while (setq b (text-property-any e (point-max) 'article-type type))
	(setq e (text-property-not-all b (point-max) 'article-type type))
	(delete-region b e)))))

(defun gnus-article-delete-invisible-text ()
  "Delete all invisible text in the current buffer."
  (save-excursion
    (let ((e (point-min))
	  b)
      (while (setq b (text-property-any e (point-max) 'invisible t))
	(setq e (text-property-not-all b (point-max) 'invisible t))
	(delete-region b e)))))

(defun gnus-article-text-type-exists-p (type)
  "Say whether any text of type TYPE exists in the buffer."
  (text-property-any (point-min) (point-max) 'article-type type))

(defsubst gnus-article-header-rank ()
  "Give the rank of the string HEADER as given by `gnus-sorted-header-list'."
  (let ((list gnus-sorted-header-list)
	(i 0))
    (while list
      (when (looking-at (car list))
	(setq list nil))
      (setq list (cdr list))
      (incf i))
    i))

(defun article-hide-headers (&optional arg delete)
  "Toggle whether to hide unwanted headers and possibly sort them as well.
If given a negative prefix, always show; if given a positive prefix,
always hide."
  (interactive (gnus-article-hidden-arg))
  (if (gnus-article-check-hidden-text 'headers arg)
      ;; Show boring headers as well.
      (gnus-article-show-hidden-text 'boring-headers)
    ;; This function might be inhibited.
    (unless gnus-inhibit-hiding
      (save-excursion
	(save-restriction
	  (let ((buffer-read-only nil)
		(props (nconc (list 'article-type 'headers)
			      gnus-hidden-properties))
		(max (1+ (length gnus-sorted-header-list)))
		(ignored (when (not gnus-visible-headers)
			   (cond ((stringp gnus-ignored-headers)
				  gnus-ignored-headers)
				 ((listp gnus-ignored-headers)
				  (mapconcat 'identity gnus-ignored-headers
					     "\\|")))))
		(visible
		 (cond ((stringp gnus-visible-headers)
			gnus-visible-headers)
		       ((and gnus-visible-headers
			     (listp gnus-visible-headers))
			(mapconcat 'identity gnus-visible-headers "\\|"))))
		(inhibit-point-motion-hooks t)
		want-list beg)
	    ;; First we narrow to just the headers.
	    (widen)
	    (goto-char (point-min))
	    ;; Hide any "From " lines at the beginning of (mail) articles.
	    (while (looking-at "From ")
	      (forward-line 1))
	    (unless (bobp)
	      (if delete
		  (delete-region (point-min) (point))
		(gnus-article-hide-text (point-min) (point) props)))
	    ;; Then treat the rest of the header lines.
	    (narrow-to-region
	     (point)
	     (if (search-forward "\n\n" nil t) ; if there's a body
		 (progn (forward-line -1) (point))
	       (point-max)))
	    ;; Then we use the two regular expressions
	    ;; `gnus-ignored-headers' and `gnus-visible-headers' to
	    ;; select which header lines is to remain visible in the
	    ;; article buffer.
	    (goto-char (point-min))
	    (while (re-search-forward "^[^ \t]*:" nil t)
	      (beginning-of-line)
	      ;; Mark the rank of the header.
	      (put-text-property
	       (point) (1+ (point)) 'message-rank
	       (if (or (and visible (looking-at visible))
		       (and ignored
			    (not (looking-at ignored))))
		   (gnus-article-header-rank)
		 (+ 2 max)))
	      (forward-line 1))
	    (message-sort-headers-1)
	    (when (setq beg (text-property-any
			     (point-min) (point-max) 'message-rank (+ 2 max)))
	      ;; We make the unwanted headers invisible.
	      (if delete
		  (delete-region beg (point-max))
		;; Suggested by Sudish Joseph <joseph@cis.ohio-state.edu>.
		(gnus-article-hide-text-type beg (point-max) 'headers))
	      ;; Work around XEmacs lossage.
	      (put-text-property (point-min) beg 'invisible nil))))))))

(defun article-hide-boring-headers (&optional arg)
  "Toggle hiding of headers that aren't very interesting.
If given a negative prefix, always show; if given a positive prefix,
always hide."
  (interactive (gnus-article-hidden-arg))
  (when (and (not (gnus-article-check-hidden-text 'boring-headers arg))
	     (not gnus-show-all-headers))
    (save-excursion
      (save-restriction
	(let ((buffer-read-only nil)
	      (list gnus-boring-article-headers)
	      (inhibit-point-motion-hooks t)
	      elem)
	  (nnheader-narrow-to-headers)
	  (while list
	    (setq elem (pop list))
	    (goto-char (point-min))
	    (cond
	     ;; Hide empty headers.
	     ((eq elem 'empty)
	      (while (re-search-forward "^[^:]+:[ \t]*\n[^ \t]" nil t)
		(forward-line -1)
		(gnus-article-hide-text-type
		 (progn (beginning-of-line) (point))
		 (progn
		   (end-of-line)
		   (if (re-search-forward "^[^ \t]" nil t)
		       (match-beginning 0)
		     (point-max)))
		 'boring-headers)))
	     ;; Hide boring Newsgroups header.
	     ((eq elem 'newsgroups)
	      (when (equal (gnus-fetch-field "newsgroups")
			   (gnus-group-real-name
			    (if (boundp 'gnus-newsgroup-name)
				gnus-newsgroup-name
			      "")))
		(gnus-article-hide-header "newsgroups")))
	     ((eq elem 'followup-to)
	      (when (equal (message-fetch-field "followup-to")
			   (message-fetch-field "newsgroups"))
		(gnus-article-hide-header "followup-to")))
	     ((eq elem 'reply-to)
	      (let ((from (message-fetch-field "from"))
		    (reply-to (message-fetch-field "reply-to")))
		(when (and
		       from reply-to
		       (ignore-errors
			 (equal
			  (nth 1 (mail-extract-address-components from))
			  (nth 1 (mail-extract-address-components reply-to)))))
		  (gnus-article-hide-header "reply-to"))))
	     ((eq elem 'date)
	      (let ((date (message-fetch-field "date")))
		(when (and date
			   (< (gnus-days-between (current-time-string) date)
			      4))
		  (gnus-article-hide-header "date")))))))))))

(defun gnus-article-hide-header (header)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (concat "^" header ":") nil t)
      (gnus-article-hide-text-type
       (progn (beginning-of-line) (point))
       (progn
	 (end-of-line)
	 (if (re-search-forward "^[^ \t]" nil t)
	     (match-beginning 0)
	   (point-max)))
       'boring-headers))))

;; Written by Per Abrahamsen <amanda@iesd.auc.dk>.
(defun article-treat-overstrike ()
  "Translate overstrikes into bold text."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (search-forward "\n\n" nil t)
      (let ((buffer-read-only nil))
	(while (search-forward "\b" nil t)
	  (let ((next (following-char))
		(previous (char-after (- (point) 2))))
	    ;; We do the boldification/underlining by hiding the
	    ;; overstrikes and putting the proper text property
	    ;; on the letters.
	    (cond
	     ((eq next previous)
	      (gnus-article-hide-text-type (- (point) 2) (point) 'overstrike)
	      (put-text-property (point) (1+ (point)) 'face 'bold))
	     ((eq next ?_)
	      (gnus-article-hide-text-type
	       (1- (point)) (1+ (point)) 'overstrike)
	      (put-text-property
	       (- (point) 2) (1- (point)) 'face 'underline))
	     ((eq previous ?_)
	      (gnus-article-hide-text-type (- (point) 2) (point) 'overstrike)
	      (put-text-property
	       (point) (1+ (point)) 'face 'underline)))))))))

(defun article-fill ()
  "Format too long lines."
  (interactive)
  (save-excursion
    (let ((buffer-read-only nil))
      (widen)
      (goto-char (point-min))
      (search-forward "\n\n" nil t)
      (end-of-line 1)
      (let ((paragraph-start "^[>|#:<;* ]*[ \t]*$")
	    (adaptive-fill-regexp "[ \t]*\\([|#:<;>*]+ *\\)?")
	    (adaptive-fill-mode t))
	(while (not (eobp))
	  (and (>= (current-column) (min fill-column (window-width)))
	       (/= (preceding-char) ?:)
	       (fill-paragraph nil))
	  (end-of-line 2))))))

(defun article-remove-cr ()
  "Remove carriage returns from an article."
  (interactive)
  (save-excursion
    (let ((buffer-read-only nil))
      (goto-char (point-min))
      (while (search-forward "\r" nil t)
	(replace-match "" t t)))))

(defun article-remove-trailing-blank-lines ()
  "Remove all trailing blank lines from the article."
  (interactive)
  (save-excursion
    (let ((buffer-read-only nil))
      (goto-char (point-max))
      (delete-region
       (point)
       (progn
	 (while (and (not (bobp))
		     (looking-at "^[ \t]*$"))
	   (forward-line -1))
	 (forward-line 1)
	 (point))))))

(defun article-display-x-face (&optional force)
  "Look for an X-Face header and display it if present."
  (interactive (list 'force))
  (save-excursion
    ;; Delete the old process, if any.
    (when (process-status "article-x-face")
      (delete-process "article-x-face"))
    (let ((inhibit-point-motion-hooks t)
	  (case-fold-search nil)
	  from)
      (save-restriction
	(nnheader-narrow-to-headers)
	(setq from (message-fetch-field "from"))
	(goto-char (point-min))
	(when (and gnus-article-x-face-command
		   (or force
		       ;; Check whether this face is censored.
		       (not gnus-article-x-face-too-ugly)
		       (and gnus-article-x-face-too-ugly from
			    (not (string-match gnus-article-x-face-too-ugly
					       from))))
		   ;; Has to be present.
		   (re-search-forward "^X-Face: " nil t))
	  ;; We now have the area of the buffer where the X-Face is stored.
	  (let ((beg (point))
		(end (1- (re-search-forward "^\\($\\|[^ \t]\\)" nil t))))
	    ;; We display the face.
	    (if (symbolp gnus-article-x-face-command)
		;; The command is a lisp function, so we call it.
		(if (gnus-functionp gnus-article-x-face-command)
		    (funcall gnus-article-x-face-command beg end)
		  (error "%s is not a function" gnus-article-x-face-command))
	      ;; The command is a string, so we interpret the command
	      ;; as a, well, command, and fork it off.
	      (let ((process-connection-type nil))
		(process-kill-without-query
		 (start-process
		  "article-x-face" nil shell-file-name shell-command-switch
		  gnus-article-x-face-command))
		(process-send-region "article-x-face" beg end)
		(process-send-eof "article-x-face")))))))))

(defalias 'gnus-decode-rfc1522 'article-decode-rfc1522)
(defalias 'gnus-article-decode-rfc1522 'article-decode-rfc1522)
(defun article-decode-rfc1522 ()
  "Hack to remove QP encoding from headers."
  (let ((case-fold-search t)
	(inhibit-point-motion-hooks t)
	(buffer-read-only nil)
	string)
    (save-restriction
      (narrow-to-region
       (goto-char (point-min))
       (or (search-forward "\n\n" nil t) (point-max)))
      (goto-char (point-min))
      (while (re-search-forward
	      "=\\?iso-8859-1\\?q\\?\\([^?\t\n]*\\)\\?=" nil t)
	(setq string (match-string 1))
	(save-restriction
	  (narrow-to-region (match-beginning 0) (match-end 0))
	  (delete-region (point-min) (point-max))
	  (insert string)
	  (article-mime-decode-quoted-printable
	   (goto-char (point-min)) (point-max))
	  (subst-char-in-region (point-min) (point-max) ?_ ? )
	  (goto-char (point-max)))
	(goto-char (point-min))))))

(defun article-de-quoted-unreadable (&optional force)
  "Do a naive translation of a quoted-printable-encoded article.
This is in no way, shape or form meant as a replacement for real MIME
processing, but is simply a stop-gap measure until MIME support is
written.
If FORCE, decode the article whether it is marked as quoted-printable
or not."
  (interactive (list 'force))
  (save-excursion
    (let ((case-fold-search t)
	  (buffer-read-only nil)
	  (type (gnus-fetch-field "content-transfer-encoding")))
      (gnus-article-decode-rfc1522)
      (when (or force
		(and type (string-match "quoted-printable" (downcase type))))
	(goto-char (point-min))
	(search-forward "\n\n" nil 'move)
	(article-mime-decode-quoted-printable (point) (point-max))))))

(defun article-mime-decode-quoted-printable-buffer ()
  "Decode Quoted-Printable in the current buffer."
  (article-mime-decode-quoted-printable (point-min) (point-max)))

(defun article-mime-decode-quoted-printable (from to)
  "Decode Quoted-Printable in the region between FROM and TO."
  (interactive "r")
  (goto-char from)
  (while (search-forward "=" to t)
    (cond ((eq (following-char) ?\n)
	   (delete-char -1)
	   (delete-char 1))
	  ((looking-at "[0-9A-F][0-9A-F]")
	   (subst-char-in-region
	    (1- (point)) (point) ?=
	    (hexl-hex-string-to-integer
	     (buffer-substring (point) (+ 2 (point)))))
	   (delete-char 2))
	  ((looking-at "=")
	   (delete-char 1))
	  ((gnus-message 3 "Malformed MIME quoted-printable message")))))

(defun article-hide-pgp (&optional arg)
  "Toggle hiding of any PGP headers and signatures in the current article.
If given a negative prefix, always show; if given a positive prefix,
always hide."
  (interactive (gnus-article-hidden-arg))
  (unless (gnus-article-check-hidden-text 'pgp arg)
    (save-excursion
      (let ((inhibit-point-motion-hooks t)
	    buffer-read-only beg end)
	(widen)
	(goto-char (point-min))
	;; Hide the "header".
	(when (search-forward "\n-----BEGIN PGP SIGNED MESSAGE-----\n" nil t)
	  (gnus-article-hide-text-type (1+ (match-beginning 0))
				       (match-end 0) 'pgp))
	(setq beg (point))
	;; Hide the actual signature.
	(and (search-forward "\n-----BEGIN PGP SIGNATURE-----\n" nil t)
	     (setq end (1+ (match-beginning 0)))
	     (gnus-article-hide-text-type
	      end
	      (if (search-forward "\n-----END PGP SIGNATURE-----\n" nil t)
		  (match-end 0)
		;; Perhaps we shouldn't hide to the end of the buffer
		;; if there is no end to the signature?
		(point-max))
	      'pgp))
	;; Hide "- " PGP quotation markers.
	(when (and beg end)
	  (narrow-to-region beg end)
	  (goto-char (point-min))
	  (while (re-search-forward "^- " nil t)
	    (gnus-article-hide-text-type
	     (match-beginning 0) (match-end 0) 'pgp))
	  (widen))))))

(defun article-hide-pem (&optional arg)
  "Toggle hiding of any PEM headers and signatures in the current article.
If given a negative prefix, always show; if given a positive prefix,
always hide."
  (interactive (gnus-article-hidden-arg))
  (unless (gnus-article-check-hidden-text 'pem arg)
    (save-excursion
      (let (buffer-read-only end)
	(widen)
	(goto-char (point-min))
	;; hide the horrendously ugly "header".
	(and (search-forward "\n-----BEGIN PRIVACY-ENHANCED MESSAGE-----\n"
			     nil
			     t)
	     (setq end (1+ (match-beginning 0)))
	     (gnus-article-hide-text-type
	      end
	      (if (search-forward "\n\n" nil t)
		  (match-end 0)
		(point-max))
	      'pem))
	;; hide the trailer as well
	(and (search-forward "\n-----END PRIVACY-ENHANCED MESSAGE-----\n"
			     nil
			     t)
	     (gnus-article-hide-text-type
	      (match-beginning 0) (match-end 0) 'pem))))))

(defun article-hide-signature (&optional arg)
  "Hide the signature in the current article.
If given a negative prefix, always show; if given a positive prefix,
always hide."
  (interactive (gnus-article-hidden-arg))
  (unless (gnus-article-check-hidden-text 'signature arg)
    (save-excursion
      (save-restriction
	(let ((buffer-read-only nil))
	  (when (gnus-article-narrow-to-signature)
	    (gnus-article-hide-text-type
	     (point-min) (point-max) 'signature)))))))

(defun article-strip-leading-blank-lines ()
  "Remove all blank lines from the beginning of the article."
  (interactive)
  (save-excursion
    (let ((inhibit-point-motion-hooks t)
	  buffer-read-only)
      (goto-char (point-min))
      (when (search-forward "\n\n" nil t)
	(while (and (not (eobp))
		    (looking-at "[ \t]*$"))
	  (gnus-delete-line))))))

(defun article-strip-multiple-blank-lines ()
  "Replace consecutive blank lines with one empty line."
  (interactive)
  (save-excursion
    (let ((inhibit-point-motion-hooks t)
	  buffer-read-only)
      ;; First make all blank lines empty.
      (goto-char (point-min))
      (search-forward "\n\n" nil t)
      (while (re-search-forward "^[ \t]+$" nil t)
	(replace-match "" nil t))
      ;; Then replace multiple empty lines with a single empty line.
      (goto-char (point-min))
      (search-forward "\n\n" nil t)
      (while (re-search-forward "\n\n\n+" nil t)
	(replace-match "\n\n" t t)))))

(defun article-strip-leading-space ()
  "Remove all white space from the beginning of the lines in the article."
  (interactive)
  (save-excursion
    (let ((inhibit-point-motion-hooks t)
	  buffer-read-only)
      (goto-char (point-min))
      (search-forward "\n\n" nil t)
      (while (re-search-forward "^[ \t]+" nil t)
	(replace-match "" t t)))))

(defun article-strip-blank-lines ()
  "Strip leading, trailing and multiple blank lines."
  (interactive)
  (article-strip-leading-blank-lines)
  (article-remove-trailing-blank-lines)
  (article-strip-multiple-blank-lines))

(defvar mime::preview/content-list)
(defvar mime::preview-content-info/point-min)
(defun gnus-article-narrow-to-signature ()
  "Narrow to the signature; return t if a signature is found, else nil."
  (widen)
  (when (and (boundp 'mime::preview/content-list)
	     mime::preview/content-list)
    ;; We have a MIMEish article, so we use the MIME data to narrow.
    (let ((pcinfo (car (last mime::preview/content-list))))
      (ignore-errors
	(narrow-to-region
	 (funcall (intern "mime::preview-content-info/point-min") pcinfo)
	 (point-max)))))

  (when (gnus-article-search-signature)
    (forward-line 1)
    ;; Check whether we have some limits to what we consider
    ;; to be a signature.
    (let ((limits (if (listp gnus-signature-limit) gnus-signature-limit
		    (list gnus-signature-limit)))
	  limit limited)
      (while (setq limit (pop limits))
	(if (or (and (integerp limit)
		     (< (- (point-max) (point)) limit))
		(and (floatp limit)
		     (< (count-lines (point) (point-max)) limit))
		(and (gnus-functionp limit)
		     (funcall limit))
		(and (stringp limit)
		     (not (re-search-forward limit nil t))))
	    ()				; This limit did not succeed.
	  (setq limited t
		limits nil)))
      (unless limited
	(narrow-to-region (point) (point-max))
	t))))

(defun gnus-article-search-signature ()
  "Search the current buffer for the signature separator.
Put point at the beginning of the signature separator."
  (let ((cur (point)))
    (goto-char (point-max))
    (if (if (stringp gnus-signature-separator)
	    (re-search-backward gnus-signature-separator nil t)
	  (let ((seps gnus-signature-separator))
	    (while (and seps
			(not (re-search-backward (car seps) nil t)))
	      (pop seps))
	    seps))
	t
      (goto-char cur)
      nil)))

(eval-and-compile
  (autoload 'w3-parse-buffer "w3-parse"))

(defun gnus-article-treat-html ()
  "Render HTML."
  (interactive)
  (let ((cbuf (current-buffer)))
    (set-buffer gnus-article-buffer)
    (let (buf buffer-read-only b e)
      (goto-char (point-min))
      (narrow-to-region
       (if (search-forward "\n\n" nil t)
	   (setq b (point))
	 (point-max))
       (setq e (point-max)))
      (nnheader-temp-write nil
	(insert-buffer-substring gnus-article-buffer b e)
	(save-window-excursion
	  (setq buf (car (w3-parse-buffer (current-buffer))))))
      (when buf
	(delete-region (point-min) (point-max))
	(insert-buffer-substring buf)
	(kill-buffer buf))
      (widen)
      (goto-char (point-min))
      (set-window-start (get-buffer-window (current-buffer)) (point-min))
      (set-buffer cbuf))))

(defun gnus-article-hidden-arg ()
  "Return the current prefix arg as a number, or 0 if no prefix."
  (list (if current-prefix-arg
	    (prefix-numeric-value current-prefix-arg)
	  0)))

(defun gnus-article-check-hidden-text (type arg)
  "Return nil if hiding is necessary.
Arg can be nil or a number.  Nil and positive means hide, negative
means show, 0 means toggle."
  (save-excursion
    (save-restriction
      (widen)
      (let ((hide (gnus-article-hidden-text-p type)))
	(cond
	 ((or (null arg)
	      (> arg 0))
	  nil)
	 ((< arg 0)
	  (gnus-article-show-hidden-text type))
	 (t
	  (if (eq hide 'hidden)
	      (gnus-article-show-hidden-text type)
	    nil)))))))

(defun gnus-article-hidden-text-p (type)
  "Say whether the current buffer contains hidden text of type TYPE."
  (let ((start (point-min))
	(pos (text-property-any (point-min) (point-max) 'article-type type)))
    (while (and pos
		(not (get-text-property pos 'invisible)))
      (setq pos
	    (text-property-any (1+ pos) (point-max) 'article-type type)))
    (if pos
	'hidden
      'shown)))

(defun gnus-article-show-hidden-text (type &optional hide)
  "Show all hidden text of type TYPE.
If HIDE, hide the text instead."
  (save-excursion
    (let ((buffer-read-only nil)
	  (inhibit-point-motion-hooks t)
	  (end (point-min))
	  beg)
      (while (setq beg (text-property-any end (point-max) 'article-type type))
	(goto-char beg)
	(setq end (or
		   (text-property-not-all beg (point-max) 'article-type type)
		   (point-max)))
	(if hide
	    (gnus-article-hide-text beg end gnus-hidden-properties)
	  (gnus-article-unhide-text beg end))
	(goto-char end))
      t)))

(defconst article-time-units
  `((year . ,(* 365.25 24 60 60))
    (week . ,(* 7 24 60 60))
    (day . ,(* 24 60 60))
    (hour . ,(* 60 60))
    (minute . 60)
    (second . 1))
  "Mapping from time units to seconds.")

(defun article-date-ut (&optional type highlight header)
  "Convert DATE date to universal time in the current article.
If TYPE is `local', convert to local time; if it is `lapsed', output
how much time has lapsed since DATE."
  (interactive (list 'ut t))
  (let* ((header (or header
		     (mail-header-date gnus-current-headers)
		     (message-fetch-field "date")
		     ""))
	 (date (if (vectorp header) (mail-header-date header)
		 header))
	 (date-regexp "^Date:[ \t]\\|^X-Sent:[ \t]")
	 (inhibit-point-motion-hooks t)
	 bface eface)
    (when (and date (not (string= date "")))
      (save-excursion
	(save-restriction
	  (nnheader-narrow-to-headers)
	  (let ((buffer-read-only nil))
	    ;; Delete any old Date headers.
	    (if (re-search-forward date-regexp nil t)
		(progn
		  (setq bface (get-text-property (gnus-point-at-bol) 'face)
			eface (get-text-property (1- (gnus-point-at-eol))
						 'face))
		  (message-remove-header date-regexp t)
		  (beginning-of-line))
	      (goto-char (point-max)))
	    (insert (article-make-date-line date type))
	    ;; Do highlighting.
	    (forward-line -1)
	    (when (looking-at "\\([^:]+\\): *\\(.*\\)$")
	      (put-text-property (match-beginning 1) (match-end 1)
				 'face bface)
	      (put-text-property (match-beginning 2) (match-end 2)
				 'face eface))))))))

(defun article-make-date-line (date type)
  "Return a DATE line of TYPE."
  (cond
   ;; Convert to the local timezone.  We have to slap a
   ;; `condition-case' round the calls to the timezone
   ;; functions since they aren't particularly resistant to
   ;; buggy dates.
   ((eq type 'local)
    (concat "Date: " (condition-case ()
			 (timezone-make-date-arpa-standard date)
		       (error date))
	    "\n"))
   ;; Convert to Universal Time.
   ((eq type 'ut)
    (concat "Date: "
	    (condition-case ()
		(timezone-make-date-arpa-standard date nil "UT")
	      (error date))
	    "\n"))
   ;; Get the original date from the article.
   ((eq type 'original)
    (concat "Date: " date "\n"))
   ;; Let the user define the format.
   ((eq type 'user)
    (concat
     "Date: "
     (format-time-string gnus-article-time-format
			 (ignore-errors
			   (gnus-encode-date
			    (timezone-make-date-arpa-standard
			     date nil "UT"))))
     "\n"))
   ;; Do an X-Sent lapsed format.
   ((eq type 'lapsed)
    ;; If the date is seriously mangled, the timezone functions are
    ;; liable to bug out, so we ignore all errors.
    (let* ((now (current-time))
	   (real-time
	    (ignore-errors
	      (gnus-time-minus
	       (gnus-encode-date
		(timezone-make-date-arpa-standard
		 (current-time-string now)
		 (current-time-zone now) "UT"))
	       (gnus-encode-date
		(timezone-make-date-arpa-standard
		 date nil "UT")))))
	   (real-sec (and real-time
			  (+ (* (float (car real-time)) 65536)
			     (cadr real-time))))
	   (sec (and real-time (abs real-sec)))
	   num prev)
      (cond
       ((null real-time)
	"X-Sent: Unknown\n")
       ((zerop sec)
	"X-Sent: Now\n")
       (t
	(concat
	 "X-Sent: "
	 ;; This is a bit convoluted, but basically we go
	 ;; through the time units for years, weeks, etc,
	 ;; and divide things to see whether that results
	 ;; in positive answers.
	 (mapconcat
	  (lambda (unit)
	    (if (zerop (setq num (ffloor (/ sec (cdr unit)))))
		;; The (remaining) seconds are too few to
		;; be divided into this time unit.
		""
	      ;; It's big enough, so we output it.
	      (setq sec (- sec (* num (cdr unit))))
	      (prog1
		  (concat (if prev ", " "") (int-to-string
					     (floor num))
			  " " (symbol-name (car unit))
			  (if (> num 1) "s" ""))
		(setq prev t))))
	  article-time-units "")
	 ;; If dates are odd, then it might appear like the
	 ;; article was sent in the future.
	 (if (> real-sec 0)
	     " ago\n"
	   " in the future\n"))))))
   (t
    (error "Unknown conversion type: %s" type))))

(defun article-date-local (&optional highlight)
  "Convert the current article date to the local timezone."
  (interactive (list t))
  (article-date-ut 'local highlight))

(defun article-date-original (&optional highlight)
  "Convert the current article date to what it was originally.
This is only useful if you have used some other date conversion
function and want to see what the date was before converting."
  (interactive (list t))
  (article-date-ut 'original highlight))

(defun article-date-lapsed (&optional highlight)
  "Convert the current article date to time lapsed since it was sent."
  (interactive (list t))
  (article-date-ut 'lapsed highlight))

(defun article-date-user (&optional highlight)
  "Convert the current article date to the user-defined format.
This format is defined by the `gnus-article-time-format' variable."
  (interactive (list t))
  (article-date-ut 'user highlight))

(defun article-show-all ()
  "Show all hidden text in the article buffer."
  (interactive)
  (save-excursion
    (let ((buffer-read-only nil))
      (gnus-article-unhide-text (point-min) (point-max)))))

(defun article-emphasize (&optional arg)
  "Emphasize text according to `gnus-emphasis-alist'."
  (interactive (gnus-article-hidden-arg))
  (unless (gnus-article-check-hidden-text 'emphasis arg)
    (save-excursion
      (let ((alist gnus-emphasis-alist)
	    (buffer-read-only nil)
	    (props (append '(article-type emphasis)
			   gnus-hidden-properties))
	    regexp elem beg invisible visible face)
	(goto-char (point-min))
	(search-forward "\n\n" nil t)
	(setq beg (point))
	(while (setq elem (pop alist))
	  (goto-char beg)
	  (setq regexp (car elem)
		invisible (nth 1 elem)
		visible (nth 2 elem)
		face (nth 3 elem))
	  (while (re-search-forward regexp nil t)
 	    (when (and (match-beginning visible) (match-beginning invisible))
 	      (gnus-article-hide-text
 	       (match-beginning invisible) (match-end invisible) props)
 	      (gnus-article-unhide-text-type
 	       (match-beginning visible) (match-end visible) 'emphasis)
 	      (gnus-put-text-property-excluding-newlines
 	       (match-beginning visible) (match-end visible) 'face face)
 	      (goto-char (match-end invisible)))))))))

(defvar gnus-summary-article-menu)
(defvar gnus-summary-post-menu)

;;; Saving functions.

(defun gnus-article-save (save-buffer file &optional num)
  "Save the currently selected article."
  (unless gnus-save-all-headers
    ;; Remove headers according to `gnus-saved-headers'.
    (let ((gnus-visible-headers
	   (or gnus-saved-headers gnus-visible-headers))
	  (gnus-article-buffer save-buffer))
      (gnus-article-hide-headers 1 t)))
  (save-window-excursion
    (if (not gnus-default-article-saver)
	(error "No default saver is defined.")
      ;; !!! Magic!  The saving functions all save
      ;; `gnus-original-article-buffer' (or so they think), but we
      ;; bind that variable to our save-buffer.
      (set-buffer gnus-article-buffer)
      (let* ((gnus-save-article-buffer save-buffer)
	     (filename
	      (cond
	       ((not gnus-prompt-before-saving) 'default)
	       ((eq gnus-prompt-before-saving 'always) nil)
	       (t file)))
	     (gnus-number-of-articles-to-be-saved
	      (when (eq gnus-prompt-before-saving t)
		num)))			; Magic
	(set-buffer gnus-summary-buffer)
	(funcall gnus-default-article-saver filename)))))

(defun gnus-read-save-file-name (prompt &optional filename
					function group headers variable)
  (let ((default-name
	  (funcall function group headers (symbol-value variable)))
	result)
    (setq
     result
     (cond
      ((eq filename 'default)
       default-name)
      ((eq filename t)
       default-name)
      (filename filename)
      (t
       (let* ((split-name (gnus-get-split-value gnus-split-methods))
	      (prompt
	       (format prompt
		       (if (and gnus-number-of-articles-to-be-saved
				(> gnus-number-of-articles-to-be-saved 1))
			   (format "these %d articles"
				   gnus-number-of-articles-to-be-saved)
			 "this article")))
	      (file
	       ;; Let the split methods have their say.
	       (cond
		;; No split name was found.
		((null split-name)
		 (read-file-name
		  (concat prompt " (default "
			  (file-name-nondirectory default-name) ") ")
		  (file-name-directory default-name)
		  default-name))
		;; A single group name is returned.
		((stringp split-name)
		 (setq default-name
		       (funcall function split-name headers
				(symbol-value variable)))
		 (read-file-name
		  (concat prompt " (default "
			  (file-name-nondirectory default-name) ") ")
		  (file-name-directory default-name)
		  default-name))
		;; A single split name was found
		((= 1 (length split-name))
		 (let* ((name (car split-name))
			(dir (cond ((file-directory-p name)
				    (file-name-as-directory name))
				   ((file-exists-p name) name)
				   (t gnus-article-save-directory))))
		   (read-file-name
		    (concat prompt " (default " name ") ")
		    dir name)))
		;; A list of splits was found.
		(t
		 (setq split-name (nreverse split-name))
		 (let (result)
		   (let ((file-name-history
			  (nconc split-name file-name-history)))
		     (setq result
			   (expand-file-name
			    (read-file-name
			     (concat prompt " (`M-p' for defaults) ")
			     gnus-article-save-directory
			     (car split-name))
			    gnus-article-save-directory)))
		   (car (push result file-name-history)))))))
	 ;; Create the directory.
	 (gnus-make-directory (file-name-directory file))
	 ;; If we have read a directory, we append the default file name.
	 (when (file-directory-p file)
	   (setq file (concat (file-name-as-directory file)
			      (file-name-nondirectory default-name))))
	 ;; Possibly translate some characters.
	 (nnheader-translate-file-chars file)))))
    (gnus-make-directory (file-name-directory result))
    (set variable result)))

(defun gnus-article-archive-name (group)
  "Return the first instance of an \"Archive-name\" in the current buffer."
  (let ((case-fold-search t))
    (when (re-search-forward "archive-name: *\\([^ \n\t]+\\)[ \t]*$" nil t)
      (nnheader-concat gnus-article-save-directory
		       (match-string 1)))))

(defun gnus-article-nndoc-name (group)
  "If GROUP is an nndoc group, return the name of the parent group."
  (when (eq (car (gnus-find-method-for-group group)) 'nndoc)
    (gnus-group-get-parameter group 'save-article-group)))

(defun gnus-summary-save-in-rmail (&optional filename)
  "Append this article to Rmail file.
Optional argument FILENAME specifies file name.
Directory to save to is default to `gnus-article-save-directory'."
  (interactive)
  (gnus-set-global-variables)
  (setq filename (gnus-read-save-file-name
		  "Save %s in rmail file:" filename
		  gnus-rmail-save-name gnus-newsgroup-name
		  gnus-current-headers 'gnus-newsgroup-last-rmail))
  (gnus-eval-in-buffer-window gnus-save-article-buffer
    (save-excursion
      (save-restriction
	(widen)
	(gnus-output-to-rmail filename)))))

(defun gnus-summary-save-in-mail (&optional filename)
  "Append this article to Unix mail file.
Optional argument FILENAME specifies file name.
Directory to save to is default to `gnus-article-save-directory'."
  (interactive)
  (gnus-set-global-variables)
  (setq filename (gnus-read-save-file-name
		  "Save %s in Unix mail file:" filename
		  gnus-mail-save-name gnus-newsgroup-name
		  gnus-current-headers 'gnus-newsgroup-last-mail))
  (gnus-eval-in-buffer-window gnus-save-article-buffer
    (save-excursion
      (save-restriction
	(widen)
	(if (and (file-readable-p filename)
		 (mail-file-babyl-p filename))
	    (gnus-output-to-rmail filename t)
	  (gnus-output-to-mail filename))))))

(defun gnus-summary-save-in-file (&optional filename overwrite)
  "Append this article to file.
Optional argument FILENAME specifies file name.
Directory to save to is default to `gnus-article-save-directory'."
  (interactive)
  (gnus-set-global-variables)
  (setq filename (gnus-read-save-file-name
		  "Save %s in file:" filename
		  gnus-file-save-name gnus-newsgroup-name
		  gnus-current-headers 'gnus-newsgroup-last-file))
  (gnus-eval-in-buffer-window gnus-save-article-buffer
    (save-excursion
      (save-restriction
	(widen)
	(when (and overwrite
		   (file-exists-p filename))
	  (delete-file filename))
	(gnus-output-to-file filename)))))

(defun gnus-summary-write-to-file (&optional filename)
  "Write this article to a file.
Optional argument FILENAME specifies file name.
The directory to save in defaults to `gnus-article-save-directory'."
  (interactive)
  (gnus-summary-save-in-file nil t))

(defun gnus-summary-save-body-in-file (&optional filename)
  "Append this article body to a file.
Optional argument FILENAME specifies file name.
The directory to save in defaults to `gnus-article-save-directory'."
  (interactive)
  (gnus-set-global-variables)
  (setq filename (gnus-read-save-file-name
		  "Save %s body in file:" filename
		  gnus-file-save-name gnus-newsgroup-name
		  gnus-current-headers 'gnus-newsgroup-last-file))
  (gnus-eval-in-buffer-window gnus-save-article-buffer
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(when (search-forward "\n\n" nil t)
	  (narrow-to-region (point) (point-max)))
	(gnus-output-to-file filename)))))

(defun gnus-summary-save-in-pipe (&optional command)
  "Pipe this article to subprocess."
  (interactive)
  (gnus-set-global-variables)
  (setq command
	(cond ((eq command 'default)
	       gnus-last-shell-command)
	      (command command)
	      (t (read-string
		  (format
		   "Shell command on %s: "
		   (if (and gnus-number-of-articles-to-be-saved
			    (> gnus-number-of-articles-to-be-saved 1))
		       (format "these %d articles"
			       gnus-number-of-articles-to-be-saved)
		     "this article"))
		  gnus-last-shell-command))))
  (when (string-equal command "")
    (setq command gnus-last-shell-command))
  (gnus-eval-in-buffer-window gnus-article-buffer
    (save-restriction
      (widen)
      (shell-command-on-region (point-min) (point-max) command nil)))
  (setq gnus-last-shell-command command))

;;; Article file names when saving.

(defun gnus-capitalize-newsgroup (newsgroup)
  "Capitalize NEWSGROUP name."
  (when (not (zerop (length newsgroup)))
    (concat (char-to-string (upcase (aref newsgroup 0)))
	    (substring newsgroup 1))))

(defun gnus-Numeric-save-name (newsgroup headers &optional last-file)
  "Generate file name from NEWSGROUP, HEADERS, and optional LAST-FILE.
If variable `gnus-use-long-file-name' is non-nil, it is ~/News/News.group/num.
Otherwise, it is like ~/News/news/group/num."
  (let ((default
	  (expand-file-name
	   (concat (if (gnus-use-long-file-name 'not-save)
		       (gnus-capitalize-newsgroup newsgroup)
		     (gnus-newsgroup-directory-form newsgroup))
		   "/" (int-to-string (mail-header-number headers)))
	   gnus-article-save-directory)))
    (if (and last-file
	     (string-equal (file-name-directory default)
			   (file-name-directory last-file))
	     (string-match "^[0-9]+$" (file-name-nondirectory last-file)))
	default
      (or last-file default))))

(defun gnus-numeric-save-name (newsgroup headers &optional last-file)
  "Generate file name from NEWSGROUP, HEADERS, and optional LAST-FILE.
If variable `gnus-use-long-file-name' is non-nil, it is
~/News/news.group/num.	Otherwise, it is like ~/News/news/group/num."
  (let ((default
	  (expand-file-name
	   (concat (if (gnus-use-long-file-name 'not-save)
		       newsgroup
		     (gnus-newsgroup-directory-form newsgroup))
		   "/" (int-to-string (mail-header-number headers)))
	   gnus-article-save-directory)))
    (if (and last-file
	     (string-equal (file-name-directory default)
			   (file-name-directory last-file))
	     (string-match "^[0-9]+$" (file-name-nondirectory last-file)))
	default
      (or last-file default))))

(defun gnus-Plain-save-name (newsgroup headers &optional last-file)
  "Generate file name from NEWSGROUP, HEADERS, and optional LAST-FILE.
If variable `gnus-use-long-file-name' is non-nil, it is
~/News/News.group.  Otherwise, it is like ~/News/news/group/news."
  (or last-file
      (expand-file-name
       (if (gnus-use-long-file-name 'not-save)
	   (gnus-capitalize-newsgroup newsgroup)
	 (concat (gnus-newsgroup-directory-form newsgroup) "/news"))
       gnus-article-save-directory)))

(defun gnus-plain-save-name (newsgroup headers &optional last-file)
  "Generate file name from NEWSGROUP, HEADERS, and optional LAST-FILE.
If variable `gnus-use-long-file-name' is non-nil, it is
~/News/news.group.  Otherwise, it is like ~/News/news/group/news."
  (or last-file
      (expand-file-name
       (if (gnus-use-long-file-name 'not-save)
	   newsgroup
	 (concat (gnus-newsgroup-directory-form newsgroup) "/news"))
       gnus-article-save-directory)))

(eval-and-compile
  (mapcar
   (lambda (func)
     (let (afunc gfunc)
       (if (consp func)
	   (setq afunc (car func)
		 gfunc (cdr func))
	 (setq afunc func
	       gfunc (intern (format "gnus-%s" func))))
       (fset gfunc
	     (if (not (fboundp afunc))
		 nil
	       `(lambda (&optional interactive &rest args)
		  ,(documentation afunc t)
		  (interactive (list t))
		  (save-excursion
		    (set-buffer gnus-article-buffer)
		    (if interactive
			(call-interactively ',afunc)
		      (apply ',afunc args))))))))
   '(article-hide-headers
     article-hide-boring-headers
     article-treat-overstrike
     (article-fill . gnus-article-word-wrap)
     article-remove-cr
     article-display-x-face
     article-de-quoted-unreadable
     article-mime-decode-quoted-printable
     article-hide-pgp
     article-hide-pem
     article-hide-signature
     article-remove-trailing-blank-lines
     article-strip-leading-blank-lines
     article-strip-multiple-blank-lines
     article-strip-leading-space
     article-strip-blank-lines
     article-date-local
     article-date-original
     article-date-ut
     article-date-user
     article-date-lapsed
     article-emphasize
     (article-show-all . gnus-article-show-all-headers))))

;;;
;;; Gnus article mode
;;;

(put 'gnus-article-mode 'mode-class 'special)

(when t
  (gnus-define-keys gnus-article-mode-map
    " " gnus-article-goto-next-page
    "\177" gnus-article-goto-prev-page
    [delete] gnus-article-goto-prev-page
    "\C-c^" gnus-article-refer-article
    "h" gnus-article-show-summary
    "s" gnus-article-show-summary
    "\C-c\C-m" gnus-article-mail
    "?" gnus-article-describe-briefly
    gnus-mouse-2 gnus-article-push-button
    "\r" gnus-article-press-button
    "\t" gnus-article-next-button
    "\M-\t" gnus-article-prev-button
    "e" gnus-article-edit
    "<" beginning-of-buffer
    ">" end-of-buffer
    "\C-c\C-i" gnus-info-find-node
    "\C-c\C-b" gnus-bug

    "\C-d" gnus-article-read-summary-keys
    "\M-*" gnus-article-read-summary-keys
    "\M-#" gnus-article-read-summary-keys
    "\M-^" gnus-article-read-summary-keys
    "\M-g" gnus-article-read-summary-keys)

  (substitute-key-definition
   'undefined 'gnus-article-read-summary-keys gnus-article-mode-map))

(defun gnus-article-make-menu-bar ()
  (gnus-turn-off-edit-menu 'article)
  (unless (boundp 'gnus-article-article-menu)
    (easy-menu-define
     gnus-article-article-menu gnus-article-mode-map ""
     '("Article"
       ["Scroll forwards" gnus-article-goto-next-page t]
       ["Scroll backwards" gnus-article-goto-prev-page t]
       ["Show summary" gnus-article-show-summary t]
       ["Fetch Message-ID at point" gnus-article-refer-article t]
       ["Mail to address at point" gnus-article-mail t]))

    (easy-menu-define
     gnus-article-treatment-menu gnus-article-mode-map ""
     '("Treatment"
       ["Hide headers" gnus-article-hide-headers t]
       ["Hide signature" gnus-article-hide-signature t]
       ["Hide citation" gnus-article-hide-citation t]
       ["Treat overstrike" gnus-article-treat-overstrike t]
       ["Remove carriage return" gnus-article-remove-cr t]
       ["Remove quoted-unreadable" gnus-article-de-quoted-unreadable t]))

    (when nil
      (when (boundp 'gnus-summary-article-menu)
	(define-key gnus-article-mode-map [menu-bar commands]
	  (cons "Commands" gnus-summary-article-menu))))

    (when (boundp 'gnus-summary-post-menu)
      (define-key gnus-article-mode-map [menu-bar post]
	(cons "Post" gnus-summary-post-menu)))

    (run-hooks 'gnus-article-menu-hook)))

(defun gnus-article-mode ()
  "Major mode for displaying an article.

All normal editing commands are switched off.

The following commands are available in addition to all summary mode
commands:
\\<gnus-article-mode-map>
\\[gnus-article-next-page]\t Scroll the article one page forwards
\\[gnus-article-prev-page]\t Scroll the article one page backwards
\\[gnus-article-refer-article]\t Go to the article referred to by an article id near point
\\[gnus-article-show-summary]\t Display the summary buffer
\\[gnus-article-mail]\t Send a reply to the address near point
\\[gnus-article-describe-briefly]\t Describe the current mode briefly
\\[gnus-info-find-node]\t Go to the Gnus info node"
  (interactive)
  (when (gnus-visual-p 'article-menu 'menu)
    (gnus-article-make-menu-bar))
  (kill-all-local-variables)
  (gnus-simplify-mode-line)
  (setq mode-name "Article")
  (setq major-mode 'gnus-article-mode)
  (make-local-variable 'minor-mode-alist)
  (unless (assq 'gnus-show-mime minor-mode-alist)
    (push (list 'gnus-show-mime " MIME") minor-mode-alist))
  (use-local-map gnus-article-mode-map)
  (gnus-update-format-specifications nil 'article-mode)
  (set (make-local-variable 'page-delimiter) gnus-page-delimiter)
  (set (make-local-variable 'gnus-page-broken) nil)
  (set (make-local-variable 'gnus-button-marker-list) nil)
  (gnus-set-default-directory)
  (buffer-disable-undo (current-buffer))
  (setq buffer-read-only t)
  (set-syntax-table gnus-article-mode-syntax-table)
  (run-hooks 'gnus-article-mode-hook))

(defun gnus-article-setup-buffer ()
  "Initialize the article buffer."
  (let* ((name (if gnus-single-article-buffer "*Article*"
		 (concat "*Article " gnus-newsgroup-name "*")))
	 (original
	  (progn (string-match "\\*Article" name)
		 (concat " *Original Article"
			 (substring name (match-end 0))))))
    (setq gnus-article-buffer name)
    (setq gnus-original-article-buffer original)
    ;; This might be a variable local to the summary buffer.
    (unless gnus-single-article-buffer
      (save-excursion
	(set-buffer gnus-summary-buffer)
	(setq gnus-article-buffer name)
	(setq gnus-original-article-buffer original)
	(gnus-set-global-variables)))
    ;; Init original article buffer.
    (save-excursion
      (set-buffer (get-buffer-create gnus-original-article-buffer))
      (buffer-disable-undo (current-buffer))
      (setq major-mode 'gnus-original-article-mode)
      (gnus-add-current-to-buffer-list)
      (make-local-variable 'gnus-original-article))
    (if (get-buffer name)
	(save-excursion
	  (set-buffer name)
	  (buffer-disable-undo (current-buffer))
	  (setq buffer-read-only t)
	  (gnus-add-current-to-buffer-list)
	  (unless (eq major-mode 'gnus-article-mode)
	    (gnus-article-mode))
	  (current-buffer))
      (save-excursion
	(set-buffer (get-buffer-create name))
	(gnus-add-current-to-buffer-list)
	(gnus-article-mode)
	(make-local-variable 'gnus-summary-buffer)
	(current-buffer)))))

;; Set article window start at LINE, where LINE is the number of lines
;; from the head of the article.
(defun gnus-article-set-window-start (&optional line)
  (set-window-start
   (get-buffer-window gnus-article-buffer t)
   (save-excursion
     (set-buffer gnus-article-buffer)
     (goto-char (point-min))
     (if (not line)
	 (point-min)
       (gnus-message 6 "Moved to bookmark")
       (search-forward "\n\n" nil t)
       (forward-line line)
       (point)))))

(defun gnus-article-prepare (article &optional all-headers header)
  "Prepare ARTICLE in article mode buffer.
ARTICLE should either be an article number or a Message-ID.
If ARTICLE is an id, HEADER should be the article headers.
If ALL-HEADERS is non-nil, no headers are hidden."
  (save-excursion
    ;; Make sure we start in a summary buffer.
    (unless (eq major-mode 'gnus-summary-mode)
      (set-buffer gnus-summary-buffer))
    (setq gnus-summary-buffer (current-buffer))
    ;; Make sure the connection to the server is alive.
    (unless (gnus-server-opened
	     (gnus-find-method-for-group gnus-newsgroup-name))
      (gnus-check-server (gnus-find-method-for-group gnus-newsgroup-name))
      (gnus-request-group gnus-newsgroup-name t))
    (let* ((gnus-article (if header (mail-header-number header) article))
	   (summary-buffer (current-buffer))
	   (internal-hook gnus-article-internal-prepare-hook)
	   (group gnus-newsgroup-name)
	   result)
      (save-excursion
	(gnus-article-setup-buffer)
	(set-buffer gnus-article-buffer)
	;; Deactivate active regions.
	(when (and (boundp 'transient-mark-mode)
		   transient-mark-mode)
	  (setq mark-active nil))
	(if (not (setq result (let ((buffer-read-only nil))
				(gnus-request-article-this-buffer
				 article group))))
	    ;; There is no such article.
	    (save-excursion
	      (when (and (numberp article)
			 (not (memq article gnus-newsgroup-sparse)))
		(setq gnus-article-current
		      (cons gnus-newsgroup-name article))
		(set-buffer gnus-summary-buffer)
		(setq gnus-current-article article)
		(gnus-summary-mark-article article gnus-canceled-mark))
	      (unless (memq article gnus-newsgroup-sparse)
		(gnus-error
		 1 "No such article (may have expired or been canceled)")))
	  (if (or (eq result 'pseudo) (eq result 'nneething))
	      (progn
		(save-excursion
		  (set-buffer summary-buffer)
		  (setq gnus-last-article gnus-current-article
			gnus-newsgroup-history (cons gnus-current-article
						     gnus-newsgroup-history)
			gnus-current-article 0
			gnus-current-headers nil
			gnus-article-current nil)
		  (if (eq result 'nneething)
		      (gnus-configure-windows 'summary)
		    (gnus-configure-windows 'article))
		  (gnus-set-global-variables))
		(gnus-set-mode-line 'article))
	    ;; The result from the `request' was an actual article -
	    ;; or at least some text that is now displayed in the
	    ;; article buffer.
	    (when (and (numberp article)
		       (not (eq article gnus-current-article)))
	      ;; Seems like a new article has been selected.
	      ;; `gnus-current-article' must be an article number.
	      (save-excursion
		(set-buffer summary-buffer)
		(setq gnus-last-article gnus-current-article
		      gnus-newsgroup-history (cons gnus-current-article
						   gnus-newsgroup-history)
		      gnus-current-article article
		      gnus-current-headers
		      (gnus-summary-article-header gnus-current-article)
		      gnus-article-current
		      (cons gnus-newsgroup-name gnus-current-article))
		(unless (vectorp gnus-current-headers)
		  (setq gnus-current-headers nil))
		(gnus-summary-show-thread)
		(run-hooks 'gnus-mark-article-hook)
		(gnus-set-mode-line 'summary)
		(when (gnus-visual-p 'article-highlight 'highlight)
		  (run-hooks 'gnus-visual-mark-article-hook))
		;; Set the global newsgroup variables here.
		;; Suggested by Jim Sisolak
		;; <sisolak@trans4.neep.wisc.edu>.
		(gnus-set-global-variables)
		(setq gnus-have-all-headers
		      (or all-headers gnus-show-all-headers))
		(and gnus-use-cache
		     (vectorp (gnus-summary-article-header article))
		     (gnus-cache-possibly-enter-article
		      group article
		      (gnus-summary-article-header article)
		      (memq article gnus-newsgroup-marked)
		      (memq article gnus-newsgroup-dormant)
		      (memq article gnus-newsgroup-unreads)))))
	    (when (or (numberp article)
		      (stringp article))
	      ;; Hooks for getting information from the article.
	      ;; This hook must be called before being narrowed.
	      (let (buffer-read-only)
		(run-hooks 'internal-hook)
		(run-hooks 'gnus-article-prepare-hook)
		;; Decode MIME message.
		(when gnus-show-mime
		  (if (or (not gnus-strict-mime)
			  (gnus-fetch-field "Mime-Version"))
		      (funcall gnus-show-mime-method)
		    (funcall gnus-decode-encoded-word-method)))
		;; Perform the article display hooks.
		(run-hooks 'gnus-article-display-hook))
	      ;; Do page break.
	      (goto-char (point-min))
	      (setq gnus-page-broken
		    (when gnus-break-pages
		      (gnus-narrow-to-page)
		      t)))
	    (gnus-set-mode-line 'article)
	    (gnus-configure-windows 'article)
	    (goto-char (point-min))
	    t))))))

(defun gnus-article-wash-status ()
  "Return a string which display status of article washing."
  (save-excursion
    (set-buffer gnus-article-buffer)
    (let ((cite (gnus-article-hidden-text-p 'cite))
	  (headers (gnus-article-hidden-text-p 'headers))
	  (boring (gnus-article-hidden-text-p 'boring-headers))
	  (pgp (gnus-article-hidden-text-p 'pgp))
	  (pem (gnus-article-hidden-text-p 'pem))
	  (signature (gnus-article-hidden-text-p 'signature))
	  (overstrike (gnus-article-hidden-text-p 'overstrike))
	  (emphasis (gnus-article-hidden-text-p 'emphasis))
	  (mime gnus-show-mime))
      (format "%c%c%c%c%c%c%c"
	      (if cite ?c ? )
	      (if (or headers boring) ?h ? )
	      (if (or pgp pem) ?p ? )
	      (if signature ?s ? )
	      (if overstrike ?o ? )
	      (if mime ?m ? )
	      (if emphasis ?e ? )))))

(defun gnus-article-hide-headers-if-wanted ()
  "Hide unwanted headers if `gnus-have-all-headers' is nil.
Provided for backwards compatibility."
  (or (save-excursion (set-buffer gnus-summary-buffer) gnus-have-all-headers)
      gnus-inhibit-hiding
      (gnus-article-hide-headers)))

;;; Article savers.

(defun gnus-output-to-file (file-name)
  "Append the current article to a file named FILE-NAME."
  (let ((artbuf (current-buffer)))
    (nnheader-temp-write nil
      (insert-buffer-substring artbuf)
      ;; Append newline at end of the buffer as separator, and then
      ;; save it to file.
      (goto-char (point-max))
      (insert "\n")
      (append-to-file (point-min) (point-max) file-name))))

(defun gnus-narrow-to-page (&optional arg)
  "Narrow the article buffer to a page.
If given a numerical ARG, move forward ARG pages."
  (interactive "P")
  (setq arg (if arg (prefix-numeric-value arg) 0))
  (save-excursion
    (set-buffer gnus-article-buffer)
    (goto-char (point-min))
    (widen)
    ;; Remove any old next/prev buttons.
    (when (gnus-visual-p 'page-marker)
      (let ((buffer-read-only nil))
	(gnus-remove-text-with-property 'gnus-prev)
	(gnus-remove-text-with-property 'gnus-next)))
    (when
	(cond ((< arg 0)
	       (re-search-backward page-delimiter nil 'move (1+ (abs arg))))
	      ((> arg 0)
	       (re-search-forward page-delimiter nil 'move arg)))
      (goto-char (match-end 0)))
    (narrow-to-region
     (point)
     (if (re-search-forward page-delimiter nil 'move)
	 (match-beginning 0)
       (point)))
    (when (and (gnus-visual-p 'page-marker)
	       (not (= (point-min) 1)))
      (save-excursion
	(goto-char (point-min))
	(gnus-insert-prev-page-button)))
    (when (and (gnus-visual-p 'page-marker)
	       (< (+ (point-max) 2) (buffer-size)))
      (save-excursion
	(goto-char (point-max))
	(gnus-insert-next-page-button)))))

;; Article mode commands

(defun gnus-article-goto-next-page ()
  "Show the next page of the article."
  (interactive)
  (when (gnus-article-next-page)
    (goto-char (point-min))
    (gnus-article-read-summary-keys nil (gnus-character-to-event ?n))))

(defun gnus-article-goto-prev-page ()
  "Show the next page of the article."
  (interactive)
  (if (bobp) (gnus-article-read-summary-keys nil (gnus-character-to-event ?p))
    (gnus-article-prev-page nil)))

(defun gnus-article-next-page (&optional lines)
  "Show the next page of the current article.
If end of article, return non-nil.  Otherwise return nil.
Argument LINES specifies lines to be scrolled up."
  (interactive "p")
  (move-to-window-line -1)
  (if (save-excursion
	(end-of-line)
	(and (pos-visible-in-window-p)	;Not continuation line.
	     (eobp)))
      ;; Nothing in this page.
      (if (or (not gnus-page-broken)
	      (save-excursion
		(save-restriction
		  (widen) (forward-line 1) (eobp)))) ;Real end-of-buffer?
	  t				;Nothing more.
	(gnus-narrow-to-page 1)		;Go to next page.
	nil)
    ;; More in this page.
    (let ((scroll-in-place nil))
      (condition-case ()
	  (scroll-up lines)
	(end-of-buffer
	 ;; Long lines may cause an end-of-buffer error.
	 (goto-char (point-max)))))
    (move-to-window-line 0)
    nil))

(defun gnus-article-prev-page (&optional lines)
  "Show previous page of current article.
Argument LINES specifies lines to be scrolled down."
  (interactive "p")
  (move-to-window-line 0)
  (if (and gnus-page-broken
	   (bobp)
	   (not (save-restriction (widen) (bobp)))) ;Real beginning-of-buffer?
      (progn
	(gnus-narrow-to-page -1)	;Go to previous page.
	(goto-char (point-max))
	(recenter -1))
    (let ((scroll-in-place nil))
      (prog1
	  (condition-case ()
	      (scroll-down lines)
	    (beginning-of-buffer
	     (goto-char (point-min))))
	(move-to-window-line 0)))))

(defun gnus-article-refer-article ()
  "Read article specified by message-id around point."
  (interactive)
  (let ((point (point)))
    (search-forward ">" nil t)		;Move point to end of "<....>".
    (if (re-search-backward "\\(<[^<> \t\n]+>\\)" nil t)
	(let ((message-id (match-string 1)))
	  (goto-char point)
	  (set-buffer gnus-summary-buffer)
	  (gnus-summary-refer-article message-id))
      (goto-char (point))
      (error "No references around point"))))

(defun gnus-article-show-summary ()
  "Reconfigure windows to show summary buffer."
  (interactive)
  (if (not (gnus-buffer-live-p gnus-summary-buffer))
      (error "There is no summary buffer for this article buffer")
    (gnus-configure-windows 'article)
    (gnus-summary-goto-subject gnus-current-article)))

(defun gnus-article-describe-briefly ()
  "Describe article mode commands briefly."
  (interactive)
  (gnus-message 6
		(substitute-command-keys "\\<gnus-article-mode-map>\\[gnus-article-goto-next-page]:Next page	 \\[gnus-article-goto-prev-page]:Prev page  \\[gnus-article-show-summary]:Show summary  \\[gnus-info-find-node]:Run Info  \\[gnus-article-describe-briefly]:This help")))

(defun gnus-article-summary-command ()
  "Execute the last keystroke in the summary buffer."
  (interactive)
  (let ((obuf (current-buffer))
	(owin (current-window-configuration))
	func)
    (switch-to-buffer gnus-summary-buffer 'norecord)
    (setq func (lookup-key (current-local-map) (this-command-keys)))
    (call-interactively func)
    (set-buffer obuf)
    (set-window-configuration owin)
    (set-window-point (get-buffer-window (current-buffer)) (point))))

(defun gnus-article-summary-command-nosave ()
  "Execute the last keystroke in the summary buffer."
  (interactive)
  (let (func)
    (pop-to-buffer gnus-summary-buffer 'norecord)
    (setq func (lookup-key (current-local-map) (this-command-keys)))
    (call-interactively func)))

(defun gnus-article-read-summary-keys (&optional arg key not-restore-window)
  "Read a summary buffer key sequence and execute it from the article buffer."
  (interactive "P")
  (let ((nosaves
	 '("q" "Q"  "c" "r" "R" "\C-c\C-f" "m"	"a" "f" "F"
	   "Zc" "ZC" "ZE" "ZQ" "ZZ" "Zn" "ZR" "ZG" "ZN" "ZP"
	   "=" "^" "\M-^" "|"))
	(nosave-but-article
	 '("A\r"))
	(nosave-in-article
	 '("\C-d"))
	keys)
    (save-excursion
      (set-buffer gnus-summary-buffer)
      (let (gnus-pick-mode)
	(push (or key last-command-event) unread-command-events)
	(setq keys (read-key-sequence nil))))
    (message "")

    (if (or (member keys nosaves)
	    (member keys nosave-but-article)
	    (member keys nosave-in-article))
	(let (func)
	  (save-window-excursion
	    (pop-to-buffer gnus-summary-buffer 'norecord)
	    ;; We disable the pick minor mode commands.
	    (let (gnus-pick-mode)
	      (setq func (lookup-key (current-local-map) keys))))
	  (if (not func)
	      (ding)
	    (unless (member keys nosave-in-article)
	      (set-buffer gnus-summary-buffer))
	    (call-interactively func))
	  (when (member keys nosave-but-article)
	    (pop-to-buffer gnus-article-buffer 'norecord)))
      ;; These commands should restore window configuration.
      (let ((obuf (current-buffer))
	    (owin (current-window-configuration))
	    (opoint (point))
	    func in-buffer)
	(if not-restore-window
	    (pop-to-buffer gnus-summary-buffer 'norecord)
	  (switch-to-buffer gnus-summary-buffer 'norecord))
	(setq in-buffer (current-buffer))
	;; We disable the pick minor mode commands.
	(if (setq func (let (gnus-pick-mode)
			 (lookup-key (current-local-map) keys)))
	    (call-interactively func)
	  (ding))
	(when (eq in-buffer (current-buffer))
	  (set-buffer obuf)
	  (unless not-restore-window
	    (set-window-configuration owin))
	  (set-window-point (get-buffer-window (current-buffer)) opoint))))))

(defun gnus-article-hide (&optional arg force)
  "Hide all the gruft in the current article.
This means that PGP stuff, signatures, cited text and (some)
headers will be hidden.
If given a prefix, show the hidden text instead."
  (interactive (list current-prefix-arg 'force))
  (gnus-article-hide-headers arg)
  (gnus-article-hide-pgp arg)
  (gnus-article-hide-citation-maybe arg force)
  (gnus-article-hide-signature arg))

(defun gnus-article-maybe-highlight ()
  "Do some article highlighting if `article-visual' is non-nil."
  (when (gnus-visual-p 'article-highlight 'highlight)
    (gnus-article-highlight-some)))

(defun gnus-request-article-this-buffer (article group)
  "Get an article and insert it into this buffer."
  (let (do-update-line)
    (prog1
	(save-excursion
	  (erase-buffer)
	  (gnus-kill-all-overlays)
	  (setq group (or group gnus-newsgroup-name))

	  ;; Open server if it has closed.
	  (gnus-check-server (gnus-find-method-for-group group))

	  ;; Using `gnus-request-article' directly will insert the article into
	  ;; `nntp-server-buffer' - so we'll save some time by not having to
	  ;; copy it from the server buffer into the article buffer.

	  ;; We only request an article by message-id when we do not have the
	  ;; headers for it, so we'll have to get those.
	  (when (stringp article)
	    (let ((gnus-override-method gnus-refer-article-method))
	      (gnus-read-header article)))

	  ;; If the article number is negative, that means that this article
	  ;; doesn't belong in this newsgroup (possibly), so we find its
	  ;; message-id and request it by id instead of number.
	  (when (and (numberp article)
		     gnus-summary-buffer
		     (get-buffer gnus-summary-buffer)
		     (buffer-name (get-buffer gnus-summary-buffer)))
	    (save-excursion
	      (set-buffer gnus-summary-buffer)
	      (let ((header (gnus-summary-article-header article)))
		(when (< article 0)
		  (cond
		   ((memq article gnus-newsgroup-sparse)
		    ;; This is a sparse gap article.
		    (setq do-update-line article)
		    (setq article (mail-header-id header))
		    (let ((gnus-override-method gnus-refer-article-method))
		      (gnus-read-header article))
		    (setq gnus-newsgroup-sparse
			  (delq article gnus-newsgroup-sparse)))
		   ((vectorp header)
		    ;; It's a real article.
		    (setq article (mail-header-id header)))
		   (t
		    ;; It is an extracted pseudo-article.
		    (setq article 'pseudo)
		    (gnus-request-pseudo-article header))))

		(let ((method (gnus-find-method-for-group
			       gnus-newsgroup-name)))
		  (if (not (eq (car method) 'nneething))
		      ()
		    (let ((dir (concat (file-name-as-directory (nth 1 method))
				       (mail-header-subject header))))
		      (when (file-directory-p dir)
			(setq article 'nneething)
			(gnus-group-enter-directory dir))))))))

	  (cond
	   ;; Refuse to select canceled articles.
	   ((and (numberp article)
		 gnus-summary-buffer
		 (get-buffer gnus-summary-buffer)
		 (buffer-name (get-buffer gnus-summary-buffer))
		 (eq (cdr (save-excursion
			    (set-buffer gnus-summary-buffer)
			    (assq article gnus-newsgroup-reads)))
		     gnus-canceled-mark))
	    nil)
	   ;; We first check `gnus-original-article-buffer'.
	   ((and (get-buffer gnus-original-article-buffer)
		 (numberp article)
		 (save-excursion
		   (set-buffer gnus-original-article-buffer)
		   (and (equal (car gnus-original-article) group)
			(eq (cdr gnus-original-article) article))))
	    (insert-buffer-substring gnus-original-article-buffer)
	    'article)
	   ;; Check the backlog.
	   ((and gnus-keep-backlog
		 (gnus-backlog-request-article group article (current-buffer)))
	    'article)
	   ;; Check asynchronous pre-fetch.
	   ((gnus-async-request-fetched-article group article (current-buffer))
	    (gnus-async-prefetch-next group article gnus-summary-buffer)
	    'article)
	   ;; Check the cache.
	   ((and gnus-use-cache
		 (numberp article)
		 (gnus-cache-request-article article group))
	    'article)
	   ;; Get the article and put into the article buffer.
	   ((or (stringp article) (numberp article))
	    (let ((gnus-override-method
		   (and (stringp article) gnus-refer-article-method))
		  (buffer-read-only nil))
	      (erase-buffer)
	      (gnus-kill-all-overlays)
	      (when (gnus-request-article article group (current-buffer))
		(when (numberp article)
		  (gnus-async-prefetch-next group article gnus-summary-buffer)
		  (when gnus-keep-backlog
		    (gnus-backlog-enter-article
		     group article (current-buffer))))
		'article)))
	   ;; It was a pseudo.
	   (t article)))

      ;; Take the article from the original article buffer
      ;; and place it in the buffer it's supposed to be in.
      (when (and (get-buffer gnus-article-buffer)
		 ;;(numberp article)
		 (equal (buffer-name (current-buffer))
			(buffer-name (get-buffer gnus-article-buffer))))
	(save-excursion
	  (if (get-buffer gnus-original-article-buffer)
	      (set-buffer (get-buffer gnus-original-article-buffer))
	    (set-buffer (get-buffer-create gnus-original-article-buffer))
	    (buffer-disable-undo (current-buffer))
	    (setq major-mode 'gnus-original-article-mode)
	    (setq buffer-read-only t)
	    (gnus-add-current-to-buffer-list))
	  (let (buffer-read-only)
	    (erase-buffer)
	    (insert-buffer-substring gnus-article-buffer))
	  (setq gnus-original-article (cons group article))))

      ;; Update sparse articles.
      (when (and do-update-line
		 (or (numberp article)
		     (stringp article)))
	(let ((buf (current-buffer)))
	  (set-buffer gnus-summary-buffer)
	  (gnus-summary-update-article do-update-line)
	  (gnus-summary-goto-subject do-update-line nil t)
	  (set-window-point (get-buffer-window (current-buffer) t)
			    (point))
	  (set-buffer buf))))))

;;;
;;; Article editing
;;;

(defcustom gnus-article-edit-mode-hook nil
  "Hook run in article edit mode buffers."
  :group 'gnus-article-various
  :type 'hook)

(defvar gnus-article-edit-done-function nil)

(defvar gnus-article-edit-mode-map nil)

(unless gnus-article-edit-mode-map
  (setq gnus-article-edit-mode-map (copy-keymap text-mode-map))

  (gnus-define-keys gnus-article-edit-mode-map
    "\C-c\C-c" gnus-article-edit-done
    "\C-c\C-k" gnus-article-edit-exit)

  (gnus-define-keys (gnus-article-edit-wash-map
		     "\C-c\C-w" gnus-article-edit-mode-map)
    "f" gnus-article-edit-full-stops))

(defun gnus-article-edit-mode ()
  "Major mode for editing articles.
This is an extended text-mode.

\\{gnus-article-edit-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'gnus-article-edit-mode)
  (setq mode-name "Article Edit")
  (use-local-map gnus-article-edit-mode-map)
  (make-local-variable 'gnus-article-edit-done-function)
  (make-local-variable 'gnus-prev-winconf)
  (setq buffer-read-only nil)
  (buffer-enable-undo)
  (widen)
  (run-hooks 'text-mode 'gnus-article-edit-mode-hook))

(defun gnus-article-edit (&optional force)
  "Edit the current article.
This will have permanent effect only in mail groups.
If FORCE is non-nil, allow editing of articles even in read-only
groups."
  (interactive "P")
  (when (and (not force)
	     (gnus-group-read-only-p))
    (error "The current newsgroup does not support article editing."))
  (gnus-article-edit-article
   `(lambda ()
      (gnus-summary-edit-article-done
       ,(or (mail-header-references gnus-current-headers) "")
       ,(gnus-group-read-only-p) ,gnus-summary-buffer))))

(defun gnus-article-edit-article (exit-func)
  "Start editing the contents of the current article buffer."
  (let ((winconf (current-window-configuration)))
    (set-buffer gnus-article-buffer)
    (gnus-article-edit-mode)
    (set-text-properties (point-min) (point-max) nil)
    (gnus-configure-windows 'edit-article)
    (setq gnus-article-edit-done-function exit-func)
    (setq gnus-prev-winconf winconf)
    (gnus-message 6 "C-c C-c to end edits")))

(defun gnus-article-edit-done ()
  "Update the article edits and exit."
  (interactive)
  (let ((func gnus-article-edit-done-function)
	(buf (current-buffer))
	(start (window-start)))
    (gnus-article-edit-exit)
    (save-excursion
      (set-buffer buf)
      (let ((buffer-read-only nil))
	(funcall func)))
    (set-buffer buf)
    (set-window-start (get-buffer-window buf) start)
    (set-window-point (get-buffer-window buf) (point))))

(defun gnus-article-edit-exit ()
  "Exit the article editing without updating."
  (interactive)
  ;; We remove all text props from the article buffer.
  (let ((buf (format "%s" (buffer-string)))
	(curbuf (current-buffer))
	(p (point))
	(window-start (window-start)))
    (erase-buffer)
    (insert buf)
    (let ((winconf gnus-prev-winconf))
      (gnus-article-mode)
      ;; The cache and backlog have to be flushed somewhat.
      (when gnus-use-cache
	(gnus-cache-update-article
	 (car gnus-article-current) (cdr gnus-article-current)))
      (when gnus-keep-backlog
	(gnus-backlog-remove-article
	 (car gnus-article-current) (cdr gnus-article-current)))
      ;; Flush original article as well.
      (save-excursion
	(when (get-buffer gnus-original-article-buffer)
	  (set-buffer gnus-original-article-buffer)
	  (setq gnus-original-article nil)))
      (set-window-configuration winconf)
      ;; Tippy-toe some to make sure that point remains where it was.
      (let ((buf (current-buffer)))
	(set-buffer curbuf)
	(set-window-start (get-buffer-window (current-buffer)) window-start)
	(goto-char p)
	(set-buffer buf)))))

(defun gnus-article-edit-full-stops ()
  "Interactively repair spacing at end of sentences."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (search-forward-regexp "^$" nil t)
    (let ((case-fold-search nil))
      (query-replace-regexp "\\([.!?][])}]* \\)\\([[({A-Z]\\)" "\\1 \\2"))))

;;;
;;; Article highlights
;;;

;; Written by Per Abrahamsen <abraham@iesd.auc.dk>.

;;; Internal Variables:

(defcustom gnus-button-url-regexp "\\b\\(s?https?\\|ftp\\|file\\|gopher\\|news\\|telnet\\|wais\\|mailto\\):\\(//[-a-zA-Z0-9_.]+:[0-9]*\\)?\\([-a-zA-Z0-9_=!?#$@~`%&*+|\\/:;.,]\\|\\w\\)+\\([-a-zA-Z0-9_=#$@~`%&*+|\\/]\\|\\w\\)"
  "Regular expression that matches URLs."
  :group 'gnus-article-buttons
  :type 'regexp)

(defcustom gnus-button-alist
  `(("<\\(url: ?\\)?news:\\([^>\n\t ]*@[^>\n\t ]*\\)>" 0 t
     gnus-button-message-id 2)
    ("\\bnews:\\([^>\n\t ]*@[^>\n\t ]*+\\)" 0 t gnus-button-message-id 1)
    ("\\(\\b<\\(url: ?\\)?news:\\(//\\)?\\([^>\n\t ]*\\)>\\)" 1 t
     gnus-button-fetch-group 4)
    ("\\bnews:\\(//\\)?\\([^>\n\t ]+\\)" 0 t gnus-button-fetch-group 2)
    ("\\bin\\( +article\\)? +\\(<\\([^\n @<>]+@[^\n @<>]+\\)>\\)" 2
     t gnus-button-message-id 3)
    ("\\(<URL: *\\)mailto: *\\([^> \n\t]+\\)>" 0 t gnus-url-mailto 1)
    ("\\bmailto:\\([^ \n\t]+\\)" 0 t gnus-url-mailto 2)
    ;; This is how URLs _should_ be embedded in text...
    ("<URL: *\\([^>]*\\)>" 0 t gnus-button-embedded-url 1)
    ;; Raw URLs.
    (,gnus-button-url-regexp 0 t gnus-button-url 0))
  "Alist of regexps matching buttons in article bodies.

Each entry has the form (REGEXP BUTTON FORM CALLBACK PAR...), where
REGEXP: is the string matching text around the button,
BUTTON: is the number of the regexp grouping actually matching the button,
FORM: is a lisp expression which must eval to true for the button to
be added,
CALLBACK: is the function to call when the user push this button, and each
PAR: is a number of a regexp grouping whose text will be passed to CALLBACK.

CALLBACK can also be a variable, in that case the value of that
variable it the real callback function."
  :group 'gnus-article-buttons
  :type '(repeat (list regexp
		       (integer :tag "Button")
		       (sexp :tag "Form")
		       (function :tag "Callback")
		       (repeat :tag "Par"
			       :inline t
			       (integer :tag "Regexp group")))))

(defcustom gnus-header-button-alist
  `(("^\\(References\\|Message-I[Dd]\\):" "<[^>]+>"
     0 t gnus-button-message-id 0)
    ("^\\(From\\|Reply-To\\):" ": *\\(.+\\)$" 1 t gnus-button-reply 1)
    ("^\\(Cc\\|To\\):" "[^ \t\n<>,()\"]+@[^ \t\n<>,()\"]+"
     0 t gnus-button-mailto 0)
    ("^X-[Uu][Rr][Ll]:" ,gnus-button-url-regexp 0 t gnus-button-url 0)
    ("^[^:]+:" ,gnus-button-url-regexp 0 t gnus-button-url 0)
    ("^[^:]+:" "\\(<\\(url: \\)?news:\\([^>\n ]*\\)>\\)" 1 t
     gnus-button-message-id 3))
  "Alist of headers and regexps to match buttons in article heads.

This alist is very similar to `gnus-button-alist', except that each
alist has an additional HEADER element first in each entry:

\(HEADER REGEXP BUTTON FORM CALLBACK PAR)

HEADER is a regexp to match a header.  For a fuller explanation, see
`gnus-button-alist'."
  :group 'gnus-article-buttons
  :group 'gnus-article-headers
  :type '(repeat (list (regexp :tag "Header")
		       regexp
		       (integer :tag "Button")
		       (sexp :tag "Form")
		       (function :tag "Callback")
		       (repeat :tag "Par"
			       :inline t
			       (integer :tag "Regexp group")))))

(defvar gnus-button-regexp nil)
(defvar gnus-button-marker-list nil)
;; Regexp matching any of the regexps from `gnus-button-alist'.

(defvar gnus-button-last nil)
;; The value of `gnus-button-alist' when `gnus-button-regexp' was build.

;;; Commands:

(defun gnus-article-push-button (event)
  "Check text under the mouse pointer for a callback function.
If the text under the mouse pointer has a `gnus-callback' property,
call it with the value of the `gnus-data' text property."
  (interactive "e")
  (set-buffer (window-buffer (posn-window (event-start event))))
  (let* ((pos (posn-point (event-start event)))
         (data (get-text-property pos 'gnus-data))
	 (fun (get-text-property pos 'gnus-callback)))
    (when fun
      (funcall fun data))))

(defun gnus-article-press-button ()
  "Check text at point for a callback function.
If the text at point has a `gnus-callback' property,
call it with the value of the `gnus-data' text property."
  (interactive)
  (let* ((data (get-text-property (point) 'gnus-data))
	 (fun (get-text-property (point) 'gnus-callback)))
    (when fun
      (funcall fun data))))

(defun gnus-article-prev-button (n)
  "Move point to N buttons backward.
If N is negative, move forward instead."
  (interactive "p")
  (gnus-article-next-button (- n)))

(defun gnus-article-next-button (n)
  "Move point to N buttons forward.
If N is negative, move backward instead."
  (interactive "p")
  (let ((function (if (< n 0) 'previous-single-property-change
		    'next-single-property-change))
	(inhibit-point-motion-hooks t)
	(backward (< n 0))
	(limit (if (< n 0) (point-min) (point-max))))
    (setq n (abs n))
    (while (and (not (= limit (point)))
		(> n 0))
      ;; Skip past the current button.
      (when (get-text-property (point) 'gnus-callback)
	(goto-char (funcall function (point) 'gnus-callback nil limit)))
      ;; Go to the next (or previous) button.
      (gnus-goto-char (funcall function (point) 'gnus-callback nil limit))
      ;; Put point at the start of the button.
      (when (and backward (not (get-text-property (point) 'gnus-callback)))
	(goto-char (funcall function (point) 'gnus-callback nil limit)))
      ;; Skip past intangible buttons.
      (when (get-text-property (point) 'intangible)
	(incf n))
      (decf n))
    (unless (zerop n)
      (gnus-message 5 "No more buttons"))
    n))

(defun gnus-article-highlight (&optional force)
  "Highlight current article.
This function calls `gnus-article-highlight-headers',
`gnus-article-highlight-citation',
`gnus-article-highlight-signature', and `gnus-article-add-buttons' to
do the highlighting.  See the documentation for those functions."
  (interactive (list 'force))
  (gnus-article-highlight-headers)
  (gnus-article-highlight-citation force)
  (gnus-article-highlight-signature)
  (gnus-article-add-buttons force)
  (gnus-article-add-buttons-to-head))

(defun gnus-article-highlight-some (&optional force)
  "Highlight current article.
This function calls `gnus-article-highlight-headers',
`gnus-article-highlight-signature', and `gnus-article-add-buttons' to
do the highlighting.  See the documentation for those functions."
  (interactive (list 'force))
  (gnus-article-highlight-headers)
  (gnus-article-highlight-signature)
  (gnus-article-add-buttons))

(defun gnus-article-highlight-headers ()
  "Highlight article headers as specified by `gnus-header-face-alist'."
  (interactive)
  (save-excursion
    (set-buffer gnus-article-buffer)
    (save-restriction
      (let ((alist gnus-header-face-alist)
	    (buffer-read-only nil)
	    (case-fold-search t)
	    (inhibit-point-motion-hooks t)
	    entry regexp header-face field-face from hpoints fpoints)
	(message-narrow-to-head)
	(while (setq entry (pop alist))
	  (goto-char (point-min))
	  (setq regexp (concat "^\\("
			       (if (string-equal "" (nth 0 entry))
				   "[^\t ]"
				 (nth 0 entry))
			       "\\)")
		header-face (nth 1 entry)
		field-face (nth 2 entry))
	  (while (and (re-search-forward regexp nil t)
		      (not (eobp)))
	    (beginning-of-line)
	    (setq from (point))
	    (unless (search-forward ":" nil t)
	      (forward-char 1))
	    (when (and header-face
		       (not (memq (point) hpoints)))
	      (push (point) hpoints)
	      (gnus-put-text-property from (point) 'face header-face))
	    (when (and field-face
		       (not (memq (setq from (point)) fpoints)))
	      (push from fpoints)
	      (if (re-search-forward "^[^ \t]" nil t)
		  (forward-char -2)
		(goto-char (point-max)))
	      (gnus-put-text-property from (point) 'face field-face))))))))

(defun gnus-article-highlight-signature ()
  "Highlight the signature in an article.
It does this by highlighting everything after
`gnus-signature-separator' using `gnus-signature-face'."
  (interactive)
  (save-excursion
    (set-buffer gnus-article-buffer)
    (let ((buffer-read-only nil)
	  (inhibit-point-motion-hooks t))
      (save-restriction
	(when (and gnus-signature-face
		   (gnus-article-narrow-to-signature))
	  (gnus-overlay-put (gnus-make-overlay (point-min) (point-max))
			    'face gnus-signature-face)
	  (widen)
	  (gnus-article-search-signature)
	  (let ((start (match-beginning 0))
		(end (set-marker (make-marker) (1+ (match-end 0)))))
	    (gnus-article-add-button start (1- end) 'gnus-signature-toggle
				     end)))))))

(defun gnus-button-in-region-p (b e prop)
  "Say whether PROP exists in the region."
  (text-property-not-all b e prop nil))

(defun gnus-article-add-buttons (&optional force)
  "Find external references in the article and make buttons of them.
\"External references\" are things like Message-IDs and URLs, as
specified by `gnus-button-alist'."
  (interactive (list 'force))
  (save-excursion
    (set-buffer gnus-article-buffer)
    (let ((buffer-read-only nil)
	  (inhibit-point-motion-hooks t)
	  (case-fold-search t)
	  (alist gnus-button-alist)
	  beg entry regexp)
      ;; Remove all old markers.
      (let (marker entry)
	(while (setq marker (pop gnus-button-marker-list))
	  (goto-char marker)
	  (when (setq entry (gnus-button-entry))
	    (put-text-property (match-beginning (nth 1 entry))
			       (match-end (nth 1 entry))
			       'gnus-callback nil))
	  (set-marker marker nil)))
      ;; We skip the headers.
      (goto-char (point-min))
      (unless (search-forward "\n\n" nil t)
	(goto-char (point-max)))
      (setq beg (point))
      (while (setq entry (pop alist))
	(setq regexp (car entry))
	(goto-char beg)
	(while (re-search-forward regexp nil t)
	  (let* ((start (and entry (match-beginning (nth 1 entry))))
		 (end (and entry (match-end (nth 1 entry))))
		 (from (match-beginning 0)))
	    (when (and (or (eq t (nth 2 entry))
			   (eval (nth 2 entry)))
		       (not (gnus-button-in-region-p
			     start end 'gnus-callback)))
	      ;; That optional form returned non-nil, so we add the
	      ;; button.
	      (gnus-article-add-button
	       start end 'gnus-button-push
	       (car (push (set-marker (make-marker) from)
			  gnus-button-marker-list))))))))))

;; Add buttons to the head of an article.
(defun gnus-article-add-buttons-to-head ()
  "Add buttons to the head of the article."
  (interactive)
  (save-excursion
    (set-buffer gnus-article-buffer)
    (let ((buffer-read-only nil)
	  (inhibit-point-motion-hooks t)
	  (case-fold-search t)
	  (alist gnus-header-button-alist)
	  entry beg end)
      (nnheader-narrow-to-headers)
      (while alist
	;; Each alist entry.
	(setq entry (car alist)
	      alist (cdr alist))
	(goto-char (point-min))
	(while (re-search-forward (car entry) nil t)
	  ;; Each header matching the entry.
	  (setq beg (match-beginning 0))
	  (setq end (or (and (re-search-forward "^[^ \t]" nil t)
			     (match-beginning 0))
			(point-max)))
	  (goto-char beg)
	  (while (re-search-forward (nth 1 entry) end t)
	    ;; Each match within a header.
	    (let* ((entry (cdr entry))
		   (start (match-beginning (nth 1 entry)))
		   (end (match-end (nth 1 entry)))
		   (form (nth 2 entry)))
	      (goto-char (match-end 0))
	      (when (eval form)
		(gnus-article-add-button
		 start end (nth 3 entry)
		 (buffer-substring (match-beginning (nth 4 entry))
				   (match-end (nth 4 entry)))))))
	  (goto-char end))))
    (widen)))

;;; External functions:

(defun gnus-article-add-button (from to fun &optional data)
  "Create a button between FROM and TO with callback FUN and data DATA."
  (when gnus-article-button-face
    (gnus-overlay-put (gnus-make-overlay from to)
		      'face gnus-article-button-face))
  (gnus-add-text-properties
   from to
   (nconc (and gnus-article-mouse-face
	       (list gnus-mouse-face-prop gnus-article-mouse-face))
	  (list 'gnus-callback fun)
	  (and data (list 'gnus-data data)))))

;;; Internal functions:

(defun gnus-signature-toggle (end)
  (save-excursion
    (set-buffer gnus-article-buffer)
    (let ((buffer-read-only nil)
	  (inhibit-point-motion-hooks t))
      (if (get-text-property end 'invisible)
	  (gnus-article-unhide-text end (point-max))
	(gnus-article-hide-text end (point-max) gnus-hidden-properties)))))

(defun gnus-button-entry ()
  ;; Return the first entry in `gnus-button-alist' matching this place.
  (let ((alist gnus-button-alist)
	(entry nil))
    (while alist
      (setq entry (pop alist))
      (if (looking-at (car entry))
	  (setq alist nil)
	(setq entry nil)))
    entry))

(defun gnus-button-push (marker)
  ;; Push button starting at MARKER.
  (save-excursion
    (set-buffer gnus-article-buffer)
    (goto-char marker)
    (let* ((entry (gnus-button-entry))
	   (inhibit-point-motion-hooks t)
	   (fun (nth 3 entry))
	   (args (mapcar (lambda (group)
			   (let ((string (match-string group)))
			     (gnus-set-text-properties
			      0 (length string) nil string)
			     string))
			 (nthcdr 4 entry))))
      (cond
       ((fboundp fun)
	(apply fun args))
       ((and (boundp fun)
	     (fboundp (symbol-value fun)))
	(apply (symbol-value fun) args))
       (t
	(gnus-message 1 "You must define `%S' to use this button"
		      (cons fun args)))))))

(defun gnus-button-message-id (message-id)
  "Fetch MESSAGE-ID."
  (save-excursion
    (set-buffer gnus-summary-buffer)
    (gnus-summary-refer-article message-id)))

(defun gnus-button-fetch-group (address)
  "Fetch GROUP specified by ADDRESS."
  (if (not (string-match "[:/]" address))
      ;; This is just a simple group url.
      (gnus-group-read-ephemeral-group address gnus-select-method)
    (if (not (string-match "^\\([^:/]+\\)\\(:\\([^/]+\\)/\\)?\\(.*\\)$"
			   address))
	(error "Can't parse %s" address)
      (gnus-group-read-ephemeral-group
       (match-string 4 address)
       `(nntp ,(match-string 1 address)
	      (nntp-address ,(match-string 1 address))
	      (nntp-port-number ,(if (match-end 3)
				     (match-string 3 address)
				   "nntp")))))))

(defun gnus-split-string (string pattern)
  "Return a list of substrings of STRING which are separated by PATTERN."
  (let (parts (start 0))
    (while (string-match pattern string start)
      (setq parts (cons (substring string start (match-beginning 0)) parts)
	    start (match-end 0)))
    (nreverse (cons (substring string start) parts))))

(defun gnus-url-parse-query-string (query &optional downcase)
  (let (retval pairs cur key val)
    (setq pairs (gnus-split-string query "&"))
    (while pairs
      (setq cur (car pairs)
            pairs (cdr pairs))
      (if (not (string-match "=" cur))
          nil                           ; Grace
        (setq key (gnus-url-unhex-string (substring cur 0 (match-beginning 0)))
              val (gnus-url-unhex-string (substring cur (match-end 0) nil)))
        (if downcase
            (setq key (downcase key)))
        (setq cur (assoc key retval))
        (if cur
            (setcdr cur (cons val (cdr cur)))
          (setq retval (cons (list key val) retval)))))
    retval))

(defun gnus-url-unhex (x)
  (if (> x ?9)
      (if (>= x ?a)
          (+ 10 (- x ?a))
        (+ 10 (- x ?A)))
    (- x ?0)))

(defun gnus-url-unhex-string (str &optional allow-newlines)
  "Remove %XXX embedded spaces, etc in a url.
If optional second argument ALLOW-NEWLINES is non-nil, then allow the
decoding of carriage returns and line feeds in the string, which is normally
forbidden in URL encoding."
  (setq str (or str ""))
  (let ((tmp "")
        (case-fold-search t))
    (while (string-match "%[0-9a-f][0-9a-f]" str)
      (let* ((start (match-beginning 0))
             (ch1 (gnus-url-unhex (elt str (+ start 1))))
             (code (+ (* 16 ch1)
                      (gnus-url-unhex (elt str (+ start 2))))))
        (setq tmp (concat
                   tmp (substring str 0 start)
                   (cond
                    (allow-newlines
                     (char-to-string code))
                    ((or (= code ?\n) (= code ?\r))
                     " ")
                    (t (char-to-string code))))
              str (substring str (match-end 0)))))
    (setq tmp (concat tmp str))
    tmp))

(defun gnus-url-mailto (url)
  ;; Send mail to someone
  (when (string-match "mailto:/*\\(.*\\)" url)
    (setq url (substring url (match-beginning 1) nil)))
  (let (to args source-url subject func)
    (if (string-match (regexp-quote "?") url)
        (setq to (gnus-url-unhex-string (substring url 0 (match-beginning 0)))
              args (gnus-url-parse-query-string
                    (substring url (match-end 0) nil) t))
      (setq to (gnus-url-unhex-string url)))
    (setq args (cons (list "to" to) args)
          subject (cdr-safe (assoc "subject" args)))
    (message-mail)
    (while args
      (setq func (intern-soft (concat "message-goto-" (downcase (caar args)))))
      (if (fboundp func)
          (funcall func)
        (message-position-on-field (caar args)))
      (insert (mapconcat 'identity (cdar args) ", "))
      (setq args (cdr args)))
    (if subject
        (message-goto-body)
      (message-goto-subject))))

(defun gnus-button-mailto (address)
  ;; Mail to ADDRESS.
  (set-buffer (gnus-copy-article-buffer))
  (message-reply address))

(defun gnus-button-reply (address)
  ;; Reply to ADDRESS.
  (message-reply address))

(defun gnus-button-url (address)
  "Browse ADDRESS."
  (funcall browse-url-browser-function address))

(defun gnus-button-embedded-url (address)
  "Browse ADDRESS."
  (funcall browse-url-browser-function (gnus-strip-whitespace address)))

;;; Next/prev buttons in the article buffer.

(defvar gnus-next-page-line-format "%{%(Next page...%)%}\n")
(defvar gnus-prev-page-line-format "%{%(Previous page...%)%}\n")

(defvar gnus-prev-page-map nil)
(unless gnus-prev-page-map
  (setq gnus-prev-page-map (make-sparse-keymap))
  (define-key gnus-prev-page-map gnus-mouse-2 'gnus-button-prev-page)
  (define-key gnus-prev-page-map "\r" 'gnus-button-prev-page))

(defun gnus-insert-prev-page-button ()
  (let ((buffer-read-only nil))
    (gnus-eval-format
     gnus-prev-page-line-format nil
     `(gnus-prev t local-map ,gnus-prev-page-map
		 gnus-callback gnus-article-button-prev-page))))

(defvar gnus-next-page-map nil)
(unless gnus-next-page-map
  (setq gnus-next-page-map (make-keymap))
  (suppress-keymap gnus-prev-page-map)
  (define-key gnus-next-page-map gnus-mouse-2 'gnus-button-next-page)
  (define-key gnus-next-page-map "\r" 'gnus-button-next-page))

(defun gnus-button-next-page ()
  "Go to the next page."
  (interactive)
  (let ((win (selected-window)))
    (select-window (get-buffer-window gnus-article-buffer t))
    (gnus-article-next-page)
    (select-window win)))

(defun gnus-button-prev-page ()
  "Go to the prev page."
  (interactive)
  (let ((win (selected-window)))
    (select-window (get-buffer-window gnus-article-buffer t))
    (gnus-article-prev-page)
    (select-window win)))

(defun gnus-insert-next-page-button ()
  (let ((buffer-read-only nil))
    (gnus-eval-format gnus-next-page-line-format nil
		      `(gnus-next t local-map ,gnus-next-page-map
				  gnus-callback
				  gnus-article-button-next-page))))

(defun gnus-article-button-next-page (arg)
  "Go to the next page."
  (interactive "P")
  (let ((win (selected-window)))
    (select-window (get-buffer-window gnus-article-buffer t))
    (gnus-article-next-page)
    (select-window win)))

(defun gnus-article-button-prev-page (arg)
  "Go to the prev page."
  (interactive "P")
  (let ((win (selected-window)))
    (select-window (get-buffer-window gnus-article-buffer t))
    (gnus-article-prev-page)
    (select-window win)))

(gnus-ems-redefine)

(provide 'gnus-art)

(run-hooks 'gnus-art-load-hook)

;;; gnus-art.el ends here
