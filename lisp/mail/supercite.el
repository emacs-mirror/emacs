;;; supercite.el --- minor mode for citing mail and news replies

;; Author: 1993 Barry A. Warsaw, Century Computing, Inc. <bwarsaw@cen.com>
;; Maintainer:    supercite-help@anthem.nlm.nih.gov
;; Created:       February 1993
;; Version:       3.1
;; Last Modified: 1993/09/22 18:58:46
;; Keywords: mail, news

;; supercite.el revision: 3.54

;; Copyright (C) 1993 Barry A. Warsaw

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; LCD Archive Entry
;; supercite|Barry A. Warsaw|supercite-help@anthem.nlm.nih.gov
;; |Mail and news reply citation package
;; |1993/09/22 18:58:46|3.1|

;; Code:


(require 'regi)

;; start user configuration variables
;; vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

(defvar sc-auto-fill-region-p t
  "*If non-nil, automatically fill each paragraph after it has been cited.")

(defvar sc-blank-lines-after-headers 1
  "*Number of blank lines to leave after mail headers have been nuked.
Set to nil, to use whatever blank lines happen to occur naturally.")

(defvar sc-citation-leader "    "
  "*String comprising first part of a citation.")
(defvar sc-citation-delimiter ">"
  "*String comprising third part of a citation.
This string is used in both nested and non-nested citations.")
(defvar sc-citation-separator " "
  "*String comprising fourth and last part of a citation.")

(defvar sc-citation-leader-regexp "[ \t]*"
  "*Regexp describing citation leader for a cited line.
This should NOT have a leading `^' character.")

;; Nemacs and Mule users note: please see the texinfo manual for
;; suggestions on setting these variables.
(defvar sc-citation-root-regexp "[-._a-zA-Z0-9]*"
  "*Regexp describing variable root part of a citation for a cited line.
This should NOT have a leading `^' character.  See also
`sc-citation-nonnested-root-regexp'.")
(defvar sc-citation-nonnested-root-regexp "[-._a-zA-Z0-9]+"
  "*Regexp describing the variable root part of a nested citation.
This should NOT have a leading `^' character.  This variable is
related to `sc-citation-root-regexp' but where as that varariable
describes both nested and non-nested citation roots, this variable
describes only nested citation roots.")
(defvar sc-citation-delimiter-regexp "[>]+"
  "*Regexp describing citation delimiter for a cited line.
This should NOT have a leading `^' character.")
(defvar sc-citation-separator-regexp "[ \t]*"
  "*Regexp describing citation separator for a cited line.
This should NOT have a leading `^' character.")

(defvar sc-cite-blank-lines-p nil
  "*If non-nil, put a citation on blank lines.")

(defvar sc-cite-frame-alist '()
  "*Alist for frame selection during citing.
Each element of this list has the following form:

   (INFOKEY ((REGEXP . FRAME)
             (REGEXP . FRAME)
             (...)))

Where INFOKEY is a key for `sc-mail-field', REGEXP is a regular
expression to match against the INFOKEY's value.  FRAME is a citation
frame, or a variable containing a citation frame.")
(defvar sc-uncite-frame-alist '()
  "*Alist for frame selection during unciting.
See the variable `sc-cite-frame-alist' for details.")
(defvar sc-recite-frame-alist '()
  "*Alist for frame selection during reciting.
See the variable `sc-cite-frame-alist' for details.")

(defvar sc-default-cite-frame
  '(;; initialize fill state and temporary variables when entering
    ;; frame. this makes things run much faster
    (begin (progn
	     (sc-fill-if-different)
	     (setq sc-tmp-nested-regexp (sc-cite-regexp "")
		   sc-tmp-nonnested-regexp (sc-cite-regexp)
		   sc-tmp-dumb-regexp
		   (concat "\\("
			   (sc-cite-regexp "")
			   "\\)"
			   (sc-cite-regexp sc-citation-nonnested-root-regexp))
		   )))
    ;; blank lines mean paragraph separators, so fill the last cited
    ;; paragraph, unless sc-cite-blank-lines-p is non-nil, in which
    ;; case we treat blank lines just like any other line.
    ("^[ \t]*$"                 (if sc-cite-blank-lines-p
				    (sc-cite-line)
				  (sc-fill-if-different "")))
    ;; do nothing if looking at a reference tag. make sure that the
    ;; tag string isn't the empty string since this will match every
    ;; line.  it cannot be nil.
    (sc-reference-tag-string    (if (string= sc-reference-tag-string "")
				    (list 'continue)
				  nil))
    ;; this regexp catches nested citations in which the author cited
    ;; a non-nested citation with a dumb citer.
    (sc-tmp-dumb-regexp         (sc-cite-coerce-dumb-citer))
    ;; if we are looking at a nested citation then add a citation level
    (sc-tmp-nested-regexp       (sc-add-citation-level))
    ;; if we're looking at a non-nested citation, coerce it to our style
    (sc-tmp-nonnested-regexp    (sc-cite-coerce-cited-line))
    ;; we must be looking at an uncited line. if we are in nested
    ;; citations, just add a citation level
    (sc-nested-citation-p       (sc-add-citation-level))
    ;; we're looking at an uncited line and we are in non-nested
    ;; citations, so cite it with a non-nested citation
    (t                          (sc-cite-line))
    ;; be sure when we're done that we fill the last cited paragraph.
    (end                        (sc-fill-if-different ""))
    )
  "*Default REGI frame for citing a region.")

(defvar sc-default-uncite-frame
  '(;; do nothing on a blank line
    ("^[ \t]*$"       nil)
    ;; if the line is cited, uncite it
    ((sc-cite-regexp) (sc-uncite-line))
    )
  "*Default REGI frame for unciting a region.")

(defvar sc-default-recite-frame
  '(;; initialize fill state when entering frame
    (begin            (sc-fill-if-different))
    ;; do nothing on a blank line
    ("^[ \t]*$"       nil)
    ;; if we're looking at a cited line, recite it
    ((sc-cite-regexp) (sc-recite-line (sc-cite-regexp)))
    ;; otherwise, the line is uncited, so just cite it
    (t                (sc-cite-line))
    ;; be sure when we're done that we fill the last cited paragraph.
    (end              (sc-fill-if-different ""))
    )
  "*Default REGI frame for reciting a region.")

(defvar sc-cite-region-limit t
  "*This variable controls automatic citation of yanked text.
Legal values are:

non-nil   -- cite the entire region, regardless of its size
nil       -- do not cite the region at all
<integer> -- a number indicating the threshold for citation.  When
	     the number of lines in the region is greater than this
	     value, a warning message will be printed and the region
	     will not be cited.  Lines in region are counted with
	     `count-lines'. 

The gathering of attribution information is not affected by the value
of this variable.  The number of lines in the region is calculated
*after* all mail headers are removed.  This variable is only consulted
during the initial citing via `sc-cite-original'.")

(defvar sc-confirm-always-p t
  "*If non-nil, always confirm attribution string before citing text body.")

(defvar sc-default-attribution "Anon"
  "*String used when author's attribution cannot be determined.")
(defvar sc-default-author-name "Anonymous"
  "*String used when author's name cannot be determined.")

(defvar sc-downcase-p nil
  "*Non-nil means downcase the attribution and citation strings.")

(defvar sc-electric-circular-p t
  "*If non-nil, treat electric references as circular.")
(defvar sc-electric-mode-hook nil
  "*Hook for `sc-electric-mode' electric references mode.")
(defvar sc-electric-references-p nil
  "*Use electric references if non-nil.")

(defvar sc-fixup-whitespace-p nil
  "*If non-nil, delete all leading white space before citing.")

(defvar sc-load-hook nil
  "*Hook which gets run once after Supercite loads.")
(defvar sc-pre-hook nil
  "*Hook which gets run before each invocation of `sc-cite-original'.")
(defvar sc-post-hook nil
  "*Hook which gets run after each invocation of `sc-cite-original'.")

(defvar sc-mail-warn-if-non-rfc822-p t
  "*Warn if mail headers don't conform to RFC822.")
(defvar sc-mumble ""
  "*Value returned by `sc-mail-field' if field isn't in mail headers.")

(defvar sc-name-filter-alist
  '(("^\\(Mr\\|Mrs\\|Ms\\|Dr\\)[.]?$" . 0)
    ("^\\(Jr\\|Sr\\)[.]?$" . last)
    ("^ASTS$" . 0)
    ("^[I]+$" . last))
  "*Name list components which are filtered out as noise.
This variable contains an association list where each element is of
the form:  (REGEXP . POSITION).

REGEXP is a regular expression which matches the name list component.
Match is performed using `string-match'.  POSITION is the position in
the name list which can match the regular expression, starting at zero
for the first element.  Use `last' to match the last element in the
list and `any' to match all elements.")

(defvar sc-nested-citation-p nil
  "*Controls whether to use nested or non-nested citation style.
Non-nil uses nested citations, nil uses non-nested citations.")

(defvar sc-nuke-mail-headers 'all
  "*Controls mail header nuking.
Used in conjunction with `sc-nuke-mail-header-list'.  Legal values are:

`all'       -- nuke all mail headers
`none'      -- don't nuke any mail headers
`specified' -- nuke headers specified in `sc-nuke-mail-header-list'
`keep'      -- keep headers specified in `sc-nuke-mail-header-list'")

(defvar sc-nuke-mail-header-list nil
  "*List of mail header regexps to remove or keep in body of reply.
This list contains regular expressions describing the mail headers to
keep or nuke, depending on the value of `sc-nuke-mail-headers'.")

(defvar sc-preferred-attribution-list
  '("sc-lastchoice" "x-attribution" "firstname" "initials" "lastname")
  "*Specifies what to use as the attribution string.
Supercite creates a list of possible attributions when it scans the
mail headers from the original message.  Each attribution choice is
associated with a key in an attribution alist.  Supercite tries to
pick a \"preferred\" attribution by matching the attribution alist
keys against the elements in `sc-preferred-attribution-list' in order.
The first non-empty string value found is used as the preferred
attribution.

Note that Supercite now honors the X-Attribution: mail field.  If
present in the original message, the value of this field should always
be used to select the most preferred attribution since it reflects how
the original author would like to be distinguished.  It should be
considered bad taste to put any attribution preference key before
\"x-attribution\" in this list, except perhaps for \"sc-lastchoice\"
\(see below).

Supercite remembers the last attribution used when reciting an already
cited paragraph.  This attribution will always be saved with the
\"sc-lastchoice\" key, which can be used in this list.  Note that the
last choice is always reset after every call of `sc-cite-original'.

Barring error conditions, the following preferences are always present
in the attribution alist:

\"emailname\"    -- email terminus name
\"initials\"     -- initials of author
\"firstname\"    -- first name of author
\"lastname\"     -- last name of author
\"middlename-1\" -- first middle name of author
\"middlename-2\" -- second middle name of author
...

Middle name indexes can be any positive integer greater than 0,
although it is unlikely that many authors will supply more than one
middle name, if that many.  The string of all middle names is
associated with the key \"middlenames\".")

(defvar sc-attrib-selection-list nil
  "*An alist for selecting preferred attribution based on mail headers.
Each element of this list has the following form:

   (INFOKEY ((REGEXP . ATTRIBUTION)
             (REGEXP . ATTRIBUTION)
             (...)))

Where INFOKEY is a key for `sc-mail-field', REGEXP is a regular
expression to match against the INFOKEY's value. ATTRIBUTION can be a
string or a list.  If its a string, then it is the attribution that is
selected by `sc-select-attribution'.  If it is a list, it is `eval'd
and the return value must be a string, which is used as the selected
attribution.  Note that the variable `sc-preferred-attribution-list'
must contain an element of the string \"sc-consult\" for this variable
to be consulted during attribution selection.")

(defvar sc-attribs-preselect-hook nil
  "*Hook to run before selecting an attribution.")
(defvar sc-attribs-postselect-hook nil
  "*Hook to run after selecting an attribution, but before confirmation.")

(defvar sc-pre-cite-hook nil
  "*Hook to run before citing a region of text.")
(defvar sc-pre-uncite-hook nil
  "*Hook to run before unciting a region of text.")
(defvar sc-pre-recite-hook nil
  "*Hook to run before reciting a region of text.")

(defvar sc-preferred-header-style 4
  "*Index into `sc-rewrite-header-list' specifying preferred header style.
Index zero accesses the first function in the list.")

(defvar sc-reference-tag-string ">>>>> "
  "*String used at the beginning of built-in reference headers.")

(defvar sc-rewrite-header-list
  '((sc-no-header)
    (sc-header-on-said)
    (sc-header-inarticle-writes)
    (sc-header-regarding-adds)
    (sc-header-attributed-writes)
    (sc-header-author-writes)
    (sc-header-verbose)
    (sc-no-blank-line-or-header)
    )
  "*List of reference header rewrite functions.
The variable `sc-preferred-header-style' controls which function in
this list is chosen for automatic reference header insertions.
Electric reference mode will cycle through this list of functions.")

(defvar sc-titlecue-regexp "\\s +-+\\s +"
  "*Regular expression describing the separator between names and titles.
Set to nil to treat entire field as a name.")

(defvar sc-use-only-preference-p nil
  "*Controls what happens when the preferred attribution cannot be found.
If non-nil, then `sc-default-attribution' will be used.  If nil, then
some secondary scheme will be employed to find a suitable attribution
string.")

;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;; end user configuration variables

(defconst sc-version "3.1"
  "Supercite version number.")
(defconst sc-help-address "supercite-help@anthem.nlm.nih.gov"
  "Address accepting submissions of bug reports.")

(defvar sc-mail-info nil
  "Alist of mail header information gleaned from reply buffer.")
(defvar sc-attributions nil
  "Alist of attributions for use when citing.")

(defconst sc-emacs-features
  (let ((version 'v18)
	(flavor  'GNU))
    (if (string= (substring emacs-version 0 2) "19")
	(setq version 'v19))
    (if (string-match "Lucid" emacs-version)
	(setq flavor 'Lucid))
    ;; cobble up list
    (list version flavor))
  "A list describing what version of Emacs we're running on.
Known flavors are:

All GNU18's: (v18 GNU)
FSF19.x    : (v19 GNU)
Lucid19.x  : (v19 Lucid)")


(defvar sc-tmp-nested-regexp nil
  "Temporary regepx describing nested citations.")
(defvar sc-tmp-nonnested-regexp nil
  "Temporary regexp describing non-nested citations.")
(defvar sc-tmp-dumb-regexp nil
  "Temp regexp describing non-nested citation cited with a nesting citer.")

(defvar sc-minor-mode nil
  "Supercite minor mode on flag.")
(defvar sc-mode-string " SC"
  "Supercite minor mode string.")

(make-variable-buffer-local 'sc-mail-info)
(make-variable-buffer-local 'sc-attributions)
(make-variable-buffer-local 'sc-minor-mode)


;; ======================================================================
;; supercite keymaps

(defvar sc-mode-map-prefix "\C-c\C-p"
  "*Key binding to install Supercite keymap.
If this is nil, Supercite keymap is not installed.")

(defvar sc-T-keymap ()
  "Keymap for sub-keymap of setting and toggling functions.")
(if sc-T-keymap
    ()
  (setq sc-T-keymap (make-sparse-keymap))
  (define-key sc-T-keymap "a" 'sc-S-preferred-attribution-list)
  (define-key sc-T-keymap "b" 'sc-T-mail-nuke-blank-lines)
  (define-key sc-T-keymap "c" 'sc-T-confirm-always)
  (define-key sc-T-keymap "d" 'sc-T-downcase)
  (define-key sc-T-keymap "e" 'sc-T-electric-references)
  (define-key sc-T-keymap "f" 'sc-T-auto-fill-region)
  (define-key sc-T-keymap "h" 'sc-T-describe)
  (define-key sc-T-keymap "l" 'sc-S-cite-region-limit)
  (define-key sc-T-keymap "n" 'sc-S-mail-nuke-mail-headers)
  (define-key sc-T-keymap "N" 'sc-S-mail-header-nuke-list)
  (define-key sc-T-keymap "o" 'sc-T-electric-circular)
  (define-key sc-T-keymap "p" 'sc-S-preferred-header-style)
  (define-key sc-T-keymap "s" 'sc-T-nested-citation)
  (define-key sc-T-keymap "u" 'sc-T-use-only-preferences)
  (define-key sc-T-keymap "w" 'sc-T-fixup-whitespace)
  (define-key sc-T-keymap "?" 'sc-T-describe)
  )

(defvar sc-mode-map ()
  "Keymap for Supercite quasi-mode.")
(if sc-mode-map
    ()
  (setq sc-mode-map (make-sparse-keymap))
  (define-key sc-mode-map "c"    'sc-cite-region)
  (define-key sc-mode-map "f"    'sc-mail-field-query)
  (define-key sc-mode-map "g"    'sc-mail-process-headers)
  (define-key sc-mode-map "h"    'sc-describe)
  (define-key sc-mode-map "i"    'sc-insert-citation)
  (define-key sc-mode-map "o"    'sc-open-line)
  (define-key sc-mode-map "r"    'sc-recite-region)
  (define-key sc-mode-map "\C-p" 'sc-raw-mode-toggle)
  (define-key sc-mode-map "u"    'sc-uncite-region)
  (define-key sc-mode-map "v"    'sc-version)
  (define-key sc-mode-map "w"    'sc-insert-reference)
  (define-key sc-mode-map "\C-t"  sc-T-keymap)
  (define-key sc-mode-map "\C-b" 'sc-submit-bug-report)
  (define-key sc-mode-map "?"    'sc-describe)
  )
  
(defvar sc-electric-mode-map ()
  "Keymap for `sc-electric-mode' electric references mode.")
(if sc-electric-mode-map
    nil
  (setq sc-electric-mode-map (make-sparse-keymap))
  (define-key sc-electric-mode-map "p"    'sc-eref-prev)
  (define-key sc-electric-mode-map "n"    'sc-eref-next)
  (define-key sc-electric-mode-map "s"    'sc-eref-setn)
  (define-key sc-electric-mode-map "j"    'sc-eref-jump)
  (define-key sc-electric-mode-map "x"    'sc-eref-abort)
  (define-key sc-electric-mode-map "q"    'sc-eref-abort)
  (define-key sc-electric-mode-map "\r"   'sc-eref-exit)
  (define-key sc-electric-mode-map "\n"   'sc-eref-exit)
  (define-key sc-electric-mode-map "g"    'sc-eref-goto)
  (define-key sc-electric-mode-map "?"    'describe-mode)
  (define-key sc-electric-mode-map "\C-h" 'describe-mode)
  (define-key sc-electric-mode-map [f1]   'describe-mode)
  (define-key sc-electric-mode-map [help] 'describe-mode)
  )

(defvar sc-minibuffer-local-completion-map nil
  "Keymap for minibuffer confirmation of attribution strings.")
(if sc-minibuffer-local-completion-map
    ()
  (setq sc-minibuffer-local-completion-map
	(copy-keymap minibuffer-local-completion-map))
  (define-key sc-minibuffer-local-completion-map "\C-t" 'sc-toggle-fn)
  (define-key sc-minibuffer-local-completion-map " "    'self-insert-command))

(defvar sc-minibuffer-local-map nil
  "Keymap for minibuffer confirmation of attribution strings.")
(if sc-minibuffer-local-map
    ()
  (setq sc-minibuffer-local-map (copy-keymap minibuffer-local-map))
  (define-key sc-minibuffer-local-map "\C-t" 'sc-toggle-fn))


;; ======================================================================
;; utility functions

(defun sc-completing-read (prompt table &optional predicate require-match
				  initial-contents history)
  "Compatibility between Emacs 18 and 19 `completing-read'.
In version 18, the HISTORY argument is ignored."
  (if (memq 'v19 sc-emacs-features)
      (funcall 'completing-read prompt table predicate require-match
	       initial-contents history)
    (funcall 'completing-read prompt table predicate require-match
	     (or (car-safe initial-contents)
		 initial-contents))))

(defun sc-read-string (prompt &optional initial-contents history)
  "Compatibility between Emacs 18 and 19 `read-string'.
In version 18, the HISTORY argument is ignored."
  (if (memq 'v19 sc-emacs-features)
      ;; maybe future versions will take a `history' argument:
      (read-string prompt initial-contents)
    (read-string prompt initial-contents)))

(if (fboundp 'match-string)
    (defalias 'sc-submatch 'match-string)
  (defun sc-submatch (matchnum &optional string)
    "Returns `match-beginning' and `match-end' sub-expression for MATCHNUM.
If optional STRING is provided, take sub-expression using `substring'
of argument, otherwise use `buffer-substring' on current buffer.  Note
that `match-data' must have already been generated and no error
checking is performed by this function."
    (if string
	(substring string (match-beginning matchnum) (match-end matchnum))
      (buffer-substring (match-beginning matchnum) (match-end matchnum)))))

(if (fboundp 'member)
    (defalias 'sc-member 'member)
  (defun sc-member (elt list)
    "Like `memq', but uses `equal' instead of `eq'.
Emacs19 has a builtin function `member' which does exactly this."
    (catch 'elt-is-member
      (while list
	(if (equal elt (car list))
	    (throw 'elt-is-member list))
	(setq list (cdr list))))))

;; One day maybe Emacs will have this...
(if (fboundp 'string-text)
    (defalias 'sc-string-text 'string-text)
  (defun sc-string-text (string)
    "Return STRING with all text properties removed."
    (let ((string (copy-sequence string)))
      (set-text-properties 0 (length string) nil string)
      string)))

(defun sc-ask (alist)
  "Ask a question in the minibuffer requiring a single character answer.
This function is kind of an extension of `y-or-n-p' where a single
letter is used to answer a question.  Question is formed from ALIST
which has members of the form:  (WORD . LETTER).  WORD is the long
word form, while LETTER is the letter for selecting that answer.  The
selected letter is returned, or nil if the question was not answered.
Note that WORD is a string and LETTER is a character.  All LETTERs in
the list should be unique."
  (let* ((prompt (concat
		  (mapconcat (function (lambda (elt) (car elt))) alist ", ")
		  "? ("
		  (mapconcat
		   (function
		    (lambda (elt) (char-to-string (cdr elt)))) alist "/")
		  ") "))
	 (p prompt)
	 (event
	  (if (memq 'Lucid sc-emacs-features)
	      (allocate-event)
	    nil)))
    (while (stringp p)
      (if (let ((cursor-in-echo-area t)
		(inhibit-quit t))
	    (message "%s" p)
	    ;; lets be good neighbors and be compatible with all emacsen
	    (cond
	     ((memq 'v18 sc-emacs-features)
	      (setq event (read-char)))
	     ((memq 'Lucid sc-emacs-features)
	      (next-command-event event))
	     (t				; must be FSF19
	      (setq event (read-event))))
	    (prog1 quit-flag (setq quit-flag nil)))
	  (progn
	    (message "%s%s" p (single-key-description event))
	    (and (memq 'Lucid sc-emacs-features)
		 (deallocate-event event))
	    (setq quit-flag nil)
	    (signal 'quit '())))
      (let ((char
	     (if (memq 'Lucid sc-emacs-features)
		 (let* ((key (and (key-press-event-p event) (event-key event)))
			(char (and key (event-to-character event))))
		   char)
	       event))
	    elt)
	(if char (setq char (downcase char)))
	(cond
	 ((setq elt (rassq char alist))
	  (message "%s%s" p (car elt))
	  (setq p (cdr elt)))
	 ((and (memq 'Lucid sc-emacs-features)
	       (button-release-event-p event)) ; ignore them
	  nil)
	 (t
	  (message "%s%s" p (single-key-description event))
	  (if (memq 'Lucid sc-emacs-features)
	      (ding nil 'y-or-n-p)
	    (ding))
	  (discard-input)
	  (if (eq p prompt)
	      (setq p (concat "Try again.  " prompt)))))))
    (and (memq 'Lucid sc-emacs-features)
	 (deallocate-event event))
    p))

(defun sc-scan-info-alist (alist)
  "Find a match in the info alist that matches a regexp in ALIST."
  (let ((sc-mumble "")
	rtnvalue)
    (while alist
      (let* ((elem    (car alist))
	     (infokey (car elem))
	     (infoval (sc-mail-field infokey))
	     (mlist   (car (cdr elem))))
	(while mlist
	  (let* ((ml-elem (car mlist))
		 (regexp  (car ml-elem))
		 (thing   (cdr ml-elem)))
	    (if (string-match regexp infoval)
		;; we found a match, time to return
		(setq rtnvalue thing
		      mlist nil
		      alist nil)
	      ;; else we didn't find a match
	      (setq mlist (cdr mlist))
	      )))			;end of mlist loop
	(setq alist (cdr alist))
	))				;end of alist loop
    rtnvalue))


;; ======================================================================
;; extract mail field information from headers in reply buffer

;; holder variables for bc happiness
(defvar sc-mail-headers-start nil
  "Start of header fields.")
(defvar sc-mail-headers-end nil
  "End of header fields.")
(defvar sc-mail-field-history nil
  "For minibuffer completion on mail field queries.")
(defvar sc-mail-field-modification-history nil
  "For minibuffer completion on mail field modifications.")
(defvar sc-mail-glom-frame
  '((begin                        (setq sc-mail-headers-start (point)))
    ("^x-attribution:[ \t]+.*$"   (sc-mail-fetch-field t) nil t)
    ("^\\S +:.*$"                 (sc-mail-fetch-field) nil t)
    ("^$"                         (list 'abort '(step . 0)))
    ("^[ \t]+"                    (sc-mail-append-field))
    (sc-mail-warn-if-non-rfc822-p (sc-mail-error-in-mail-field))
    (end                          (setq sc-mail-headers-end (point))))
  "Regi frame for glomming mail header information.")

;; regi functions
(defun sc-mail-fetch-field (&optional attribs-p)
  "Insert a key and value into `sc-mail-info' alist.
If optional ATTRIBS-P is non-nil, the key/value pair is placed in
`sc-attributions' too."
  (if (string-match "^\\(\\S *\\)\\s *:\\s +\\(.*\\)$" curline)
      (let* ((key (downcase (sc-string-text (sc-submatch 1 curline))))
	     (val (sc-string-text (sc-submatch 2 curline)))
	     (keyval (cons key val)))
	(setq sc-mail-info (cons keyval sc-mail-info))
	(if attribs-p
	    (setq sc-attributions (cons keyval sc-attributions)))
	))
  nil)

(defun sc-mail-append-field ()
  "Append a continuation line onto the last fetched mail field's info."
  (let ((keyval (car sc-mail-info)))
    (if (and keyval (string-match "^\\s *\\(.*\\)$" curline))
	(setcdr keyval (concat (cdr keyval) " "
			       (sc-string-text (sc-submatch 1 curline))))))
  nil)

(defun sc-mail-error-in-mail-field ()
  "Issue warning that mail headers don't conform to RFC 822."
  (let* ((len (min (length curline) 10))
	 (ellipsis (if (< len (length curline)) "..." ""))
	 (msg "Mail header \"%s%s\" doesn't conform to RFC 822. skipping..."))
    (message msg (substring curline 0 len) ellipsis))
  (beep)
  (sit-for 2)
  nil)

;; mail header nuking
(defvar sc-mail-last-header-nuked-p nil
  "True if the last header was nuked.")

(defun sc-mail-nuke-line ()
  "Nuke the current mail header line."
  (delete-region (regi-pos 'bol) (regi-pos 'bonl))
  '((step . -1)))

(defun sc-mail-nuke-header-line ()
  "Delete current-line and set up for possible continuation."
  (setq sc-mail-last-header-nuked-p t)
  (sc-mail-nuke-line))

(defun sc-mail-nuke-continuation-line ()
  "Delete a continuation line if the last header line was deleted."
  (if sc-mail-last-header-nuked-p
      (sc-mail-nuke-line)))

(defun sc-mail-cleanup-blank-lines ()
  "Leave some blank lines after original mail headers are nuked.
The number of lines left is specified by `sc-blank-lines-after-headers'."
  (if sc-blank-lines-after-headers
      (save-restriction
	(widen)
	(skip-chars-backward " \t\n")
	(forward-line 1)
	(delete-blank-lines)
	(beginning-of-line)
	(if (looking-at "[ \t]*$")
	    (delete-region (regi-pos 'bol) (regi-pos 'bonl)))
	(insert-char ?\n sc-blank-lines-after-headers)))
  nil)

(defun sc-mail-build-nuke-frame ()
  "Build the regiframe for nuking mail headers."
  (let (every-func entry-func nonentry-func)
    (cond
     ((eq sc-nuke-mail-headers 'all)
      (setq every-func '(progn (forward-line -1) (sc-mail-nuke-line))))
     ((eq sc-nuke-mail-headers 'specified)
      (setq entry-func    '(sc-mail-nuke-header-line)
	    nonentry-func '(setq sc-mail-last-header-nuked-p nil)))
     ((eq sc-nuke-mail-headers 'keep)
      (setq entry-func    '(setq sc-mail-last-header-nuked-p nil)
	    nonentry-func '(sc-mail-nuke-header-line)))
     ;; we never get far enough to interpret a frame if s-n-m-h == 'none
     ((eq sc-nuke-mail-headers 'none))
     (t (error "Illegal value for sc-nuke-mail-headers: %s"
	       sc-nuke-mail-headers))
     )					; end-cond
    (append
     (and entry-func
	  (regi-mapcar sc-nuke-mail-header-list entry-func nil t))
     (and nonentry-func (list (list "^\\S +:.*$" nonentry-func)))
     (and (not every-func)
	  '(("^[ \t]+" (sc-mail-nuke-continuation-line))))
     '((begin     (setq sc-mail-last-header-zapped-p nil)))
     '((end       (sc-mail-cleanup-blank-lines)))
     (and every-func (list (list 'every every-func)))
     )))

;; mail processing and zapping. this is the top level entry defun to
;; all header processing.
(defun sc-mail-process-headers (start end)
  "Process original mail message's mail headers.
After processing, mail headers may be nuked.  Header information is
stored in `sc-mail-info', and any old information is lost unless an
error occurs."
  (interactive "r")
  (let ((info (copy-alist sc-mail-info))
	(attribs (copy-alist sc-attributions)))
    (setq sc-mail-info nil
	  sc-attributions nil)
    (regi-interpret sc-mail-glom-frame start end)
    (if (null sc-mail-info)
	(progn
	  (message "No mail headers found! Restoring old information.")
	  (setq sc-mail-info info
		sc-attributions attribs))
      (regi-interpret (sc-mail-build-nuke-frame)
		      sc-mail-headers-start sc-mail-headers-end)
      )))


;; let the user change mail field information
(defun sc-mail-field (field)
  "Return the mail header field value associated with FIELD.
If there was no mail header with FIELD as its key, return the value of
`sc-mumble'.  FIELD is case insensitive."
  (or (cdr (assoc (downcase field) sc-mail-info)) sc-mumble))

(defun sc-mail-field-query (arg)
  "View the value of a mail field.
With `\\[universal-argument]', prompts for action on mail field.
Action can be one of: View, Modify, Add, or Delete."
  (interactive "P")
  (let* ((alist '(("view" . ?v) ("modify" . ?m) ("add" . ?a) ("delete" . ?d)))
	 (action (if (not arg) ?v (sc-ask alist)))
	 key)
    (if (not action)
	()
      (setq key (sc-completing-read
		 (concat (car (rassq action alist))
			      " information key: ")
		 sc-mail-info nil
		 (if (eq action ?a) nil 'noexit)
		 nil 'sc-mail-field-history))
      (cond
       ((eq action ?v)
	(message "%s: %s" key (cdr (assoc key sc-mail-info))))
       ((eq action ?d)
	(setq sc-mail-info (delq (assoc key sc-mail-info) sc-mail-info)))
       ((eq action ?m)
	(let ((keyval (assoc key sc-mail-info)))
	  ;; first put initial value onto list if not already there
	  (if (not (sc-member (cdr keyval)
			      sc-mail-field-modification-history))
	      (setq sc-mail-field-modification-history
		    (cons (cdr keyval) sc-mail-field-modification-history)))
	  (setcdr keyval (sc-read-string
			  (concat key ": ") (cdr keyval)
			  'sc-mail-field-modification-history))))
       ((eq action ?a)
	(setq sc-mail-info
	      (cons (cons key
			  (sc-read-string (concat key ": "))) sc-mail-info)))
       ))))


;; ======================================================================
;; attributions

(defvar sc-attribution-confirmation-history nil
  "History for confirmation of attribution strings.")
(defvar sc-citation-confirmation-history nil
  "History for confirmation of attribution prefixes.")

(defun sc-attribs-%@-addresses (from &optional delim)
  "Extract the author's email terminus from email address FROM.
Match addresses of the style ``name%[stuff].'' when called with DELIM
of \"%\" and addresses of the style ``[stuff]name@[stuff]'' when
called with DELIM \"@\".  If DELIM is nil or not provided, matches
addresses of the style ``name''."
  (and (string-match (concat "[-a-zA-Z0-9_.]+" delim) from 0)
       (substring from
		  (match-beginning 0)
		  (- (match-end 0) (if (null delim) 0 1)))))

(defun sc-attribs-!-addresses (from)
  "Extract the author's email terminus from email address FROM.
Match addresses of the style ``[stuff]![stuff]...!name[stuff].''"
  (let ((eos (length from))
	(mstart (string-match "![-a-zA-Z0-9_.]+\\([^-!a-zA-Z0-9_.]\\|$\\)"
			      from 0))
	(mend (match-end 0)))
    (and mstart
	 (substring from (1+ mstart) (- mend (if (= mend eos) 0 1)))
	 )))

(defun sc-attribs-<>-addresses (from)
  "Extract the author's email terminus from email address FROM.
Match addresses of the style ``<name[stuff]>.''"
  (and (string-match "<\\(.*\\)>" from)
       (sc-submatch 1 from)))

(defun sc-get-address (from author)
  "Get the full email address path from FROM.
AUTHOR is the author's name (which is removed from the address)."
  (let ((eos (length from)))
    (if (string-match (concat "\\(^\\|^\"\\)" author
			      "\\(\\s +\\|\"\\s +\\)") from 0)
	(let ((address (substring from (match-end 0) eos)))
	  (if (and (= (aref address 0) ?<)
		   (= (aref address (1- (length address))) ?>))
	      (substring address 1 (1- (length address)))
	    address))
      (if (string-match "[-a-zA-Z0-9!@%._]+" from 0)
	  (sc-submatch 0 from)
	"")
      )))

(defun sc-attribs-emailname (from)
  "Get the email terminus name from FROM."
  (or
   (sc-attribs-%@-addresses from "%")
   (sc-attribs-%@-addresses from "@")
   (sc-attribs-!-addresses  from)
   (sc-attribs-<>-addresses from)
   (sc-attribs-%@-addresses from)
   (substring from 0 10)))

(defun sc-name-substring (string start end extend)
  "Extract the specified substring of STRING from START to END.
EXTEND is the number of characters on each side to extend the
substring."
  (and start
       (let ((sos (+ start extend))
	     (eos (- end extend)))
	 (substring string sos
		    (or (string-match sc-titlecue-regexp string sos) eos)
		    ))))

(defun sc-attribs-extract-namestring (from)
  "Extract the name string from FROM.
This should be the author's full name minus an optional title."
  (let ((namestring
	 (or
	  (sc-name-substring
	   from (string-match "(.*)" from 0) (match-end 0) 1)
	  (sc-name-substring
	   from (string-match "\".*\"" from 0) (match-end 0) 1)
	  (sc-name-substring
	   from (string-match "\\([-.a-zA-Z0-9_]+\\s +\\)+<" from 0)
	   (match-end 1) 0)
	  (sc-attribs-emailname from))))
    ;; strip off any leading or trailing whitespace
    (if namestring
	(let ((bos 0)
	      (eos (1- (length namestring))))
	  (while (and (<= bos eos)
		      (memq (aref namestring bos) '(32 ?\t)))
	    (setq bos (1+ bos)))
	  (while (and (> eos bos)
		      (memq (aref namestring eos) '(32 ?\t)))
	    (setq eos (1- eos)))
	  (substring namestring bos (1+ eos))))))

(defun sc-attribs-chop-namestring (namestring)
  "Convert NAMESTRING to a list of names.
example: (sc-namestring-to-list \"John Xavier Doe\")
         => (\"John\" \"Xavier\" \"Doe\")"
  (if (string-match "\\([ \t]*\\)\\([^ \t._]+\\)\\([ \t]*\\)" namestring)
      (cons (sc-submatch 2 namestring)
	    (sc-attribs-chop-namestring (substring namestring (match-end 3)))
	    )))

(defun sc-attribs-strip-initials (namelist)
  "Extract the author's initials from the NAMELIST."
  (mapconcat
   (function
    (lambda (name)
      (if (< 0 (length name))
	  (substring name 0 1))))
   namelist ""))

(defun sc-guess-attribution (&optional string)
  "Guess attribution string on current line.
If attribution cannot be guessed, nil is returned.  Optional STRING if
supplied, is used instead of the line point is on in the current buffer."
  (let ((start 0)
	(string (or string (buffer-substring (regi-pos 'bol) (regi-pos 'eol))))
	attribution)
    (and
     (= start (or (string-match sc-citation-leader-regexp string start) -1))
     (setq start (match-end 0))
     (= start (or (string-match sc-citation-root-regexp string start) 1))
     (setq attribution (sc-submatch 0 string)
	   start (match-end 0))
     (= start (or (string-match sc-citation-delimiter-regexp string start) -1))
     (setq start (match-end 0))
     (= start (or (string-match sc-citation-separator-regexp string start) -1))
     attribution)))

(defun sc-attribs-filter-namelist (namelist)
  "Filter out noise in NAMELIST according to `sc-name-filter-alist'."
  (let ((elements (length namelist))
	(position -1)
	keepers filtered-list)
    (mapcar
     (function
      (lambda (name)
	(setq position (1+ position))
	(let ((keep-p t))
	  (mapcar
	   (function
	    (lambda (filter)
	      (let ((regexp (car filter))
		    (pos (cdr filter)))
		(if (and (string-match regexp name)
			 (or (and (numberp pos)
				  (= pos position))
			     (and (eq pos 'last)
				  (= position (1- elements)))
			     (eq pos 'any)))
		    (setq keep-p nil))
		)))
	   sc-name-filter-alist)
	  (if keep-p
	      (setq keepers (cons position keepers)))
	  )))
     namelist)
    (mapcar
     (function
      (lambda (position)
	(setq filtered-list (cons (nth position namelist) filtered-list))
	))
     keepers)
    filtered-list))

(defun sc-attribs-chop-address (from)
  "Extract attribution information from FROM.
This populates the `sc-attributions' with the list of possible attributions."
  (if (and (stringp from)
	   (< 0 (length from)))
      (let* ((sc-mumble "")	     
	     (namestring (sc-attribs-extract-namestring from))
	     (namelist   (sc-attribs-filter-namelist
			  (sc-attribs-chop-namestring namestring)))
	     (revnames   (reverse (cdr namelist)))
	     (firstname  (car namelist))
	     (midnames   (reverse (cdr revnames)))
	     (lastname   (car revnames))
	     (initials   (sc-attribs-strip-initials namelist))
	     (emailname  (sc-attribs-emailname from))
	     (n 1)
	     author middlenames)
    
	;; put basic information
	(setq
	 ;; put middle names and build sc-author entry
	 middlenames (mapconcat
		      (function
		       (lambda (midname)
			 (let ((key-attribs (format "middlename-%d" n))
			       (key-mail    (format "sc-middlename-%d" n)))
			   (setq
			    sc-attributions (cons (cons key-attribs midname)
						  sc-attributions)
			    sc-mail-info (cons (cons key-mail midname)
					       sc-mail-info)
			    n (1+ n))
			   midname)))
		      midnames " ")

	 author (concat firstname " " middlenames (and midnames " ") lastname)

	 sc-attributions (append
			  (list
			   (cons "firstname"   firstname)
			   (cons "lastname"    lastname)
			   (cons "emailname"   emailname)
			   (cons "initials"    initials))
			  sc-attributions)
	 sc-mail-info (append
		       (list
			(cons "sc-firstname"   firstname)
			(cons "sc-middlenames" middlenames)
			(cons "sc-lastname"    lastname)
			(cons "sc-emailname"   emailname)
			(cons "sc-initials"    initials)
			(cons "sc-author"      author)
			(cons "sc-from-address" (sc-get-address
						 (sc-mail-field "from")
						 namestring))
			(cons "sc-reply-address" (sc-get-address
						  (sc-mail-field "reply-to")
						  namestring))
			(cons "sc-sender-address" (sc-get-address
						   (sc-mail-field "sender")
						   namestring))
			)
		       sc-mail-info)
	 ))
    ;; from string is empty
    (setq sc-mail-info (cons (cons "sc-author" sc-default-author-name)
			     sc-mail-info))))

(defvar sc-attrib-or-cite nil
  "Used to toggle between attribution input or citation input.")

(defun sc-toggle-fn ()
  "Toggle between attribution selection and citation selection.
Only used during confirmation."
  (interactive)
  (setq sc-attrib-or-cite (not sc-attrib-or-cite))
  (throw 'sc-reconfirm t))

(defun sc-select-attribution ()
  "Select an attribution from `sc-attributions'.

Variables involved in selection process include:
     `sc-preferred-attribution-list'
     `sc-use-only-preference-p'
     `sc-confirm-always-p'
     `sc-default-attribution'
     `sc-attrib-selection-list'.

Runs the hook `sc-attribs-preselect-hook' before selecting an
attribution and the hook `sc-attribs-postselect-hook' after making the
selection but before querying is performed.  During
`sc-attribs-postselect-hook' the variable `citation' is bound to the
auto-selected citation string and the variable `attribution' is bound
to the auto-selected attribution string."
  (run-hooks 'sc-attribs-preselect-hook)
  (let ((query-p sc-confirm-always-p)
	attribution citation
	(attriblist sc-preferred-attribution-list))

    ;; first cruise through sc-preferred-attribution-list looking for
    ;; a match in either sc-attributions or sc-mail-info.  if the
    ;; element is "sc-consult", then we have to do the alist
    ;; consultation phase
    (while attriblist
      (let* ((preferred (car attriblist)))
	(cond
	 ((string= preferred "sc-consult")
	  ;; we've been told to consult the attribution vs. mail
	  ;; header key alist.  we do this until we find a match in
	  ;; the sc-attrib-selection-list.  if we do not find a match,
	  ;; we continue scanning attriblist
	  (let ((attrib (sc-scan-info-alist sc-attrib-selection-list)))
	    (cond
	     ((not attrib)
	      (setq attriblist (cdr attriblist)))
	     ((stringp attrib)
	      (setq attribution attrib
		    attriblist nil))
	     ((listp attrib)
	      (setq attribution (eval attrib)
		    attriblist nil))
	     (t (error "%s did not evaluate to a string or list!"
		       "sc-attrib-selection-list"))
	     )))
	 ((setq attribution (cdr (assoc preferred sc-attributions)))
	  (setq attriblist nil))
	 (t
	  (setq attriblist (cdr attriblist)))
	 )))

    ;; if preference was not found, we may use a secondary method to
    ;; find a valid attribution
    (if (and (not attribution)
	     (not sc-use-only-preference-p))
	;; secondary method tries to find a preference in this order
	;; 1. sc-lastchoice
	;; 2. x-attribution
	;; 3. firstname
	;; 4. lastname
	;; 5. initials
	;; 6. first non-empty attribution in alist
	(setq attribution
	      (or (cdr (assoc "sc-lastchoice" sc-attributions))
		  (cdr (assoc "x-attribution" sc-attributions))
		  (cdr (assoc "firstname" sc-attributions))
		  (cdr (assoc "lastname" sc-attributions))
		  (cdr (assoc "initials" sc-attributions))
		  (cdr (car sc-attributions)))))

    ;; still couldn't find an attribution. we're now limited to using
    ;; the default attribution, but we'll force a query when this happens
    (if (not attribution)
	(setq attribution sc-default-attribution
	      query-p t))

    ;; create the attribution prefix
    (setq citation (sc-make-citation attribution))

    ;; run the post selection hook before querying the user
    (run-hooks 'sc-attribs-postselect-hook)

    ;; query for confirmation
    (if query-p
	(let* ((query-alist (mapcar (function (lambda (entry)
						(list (cdr entry))))
				    sc-attributions))
	       (minibuffer-local-completion-map
		sc-minibuffer-local-completion-map)
	       (minibuffer-local-map sc-minibuffer-local-map)
	       (initial attribution)
	       (completer-disable t)	; in case completer.el is used
	       choice)
	  (setq sc-attrib-or-cite nil)	; nil==attribution, t==citation
	  (while
	      (catch 'sc-reconfirm
		(string= "" (setq choice
				  (if sc-attrib-or-cite
				      (sc-read-string
				       "Enter citation prefix: "
				       citation
				       'sc-citation-confirmation-history)
				    (sc-completing-read
				     "Complete attribution name: "
				     query-alist nil nil
				     (cons initial 0)
				     'sc-attribution-confirmation-history)
				    )))))
	  (if sc-attrib-or-cite
	      ;; since the citation was chosen, we have to guess at
	      ;; the attribution
	      (setq citation choice
		    attribution (or (sc-guess-attribution citation)
				    citation))
	    
	    (setq citation (sc-make-citation choice)
		  attribution choice))
	  ))

    ;; its possible that the user wants to downcase the citation and
    ;; attribution
    (if sc-downcase-p
	(setq citation (downcase citation)
	      attribution (downcase attribution)))

    ;; set up mail info alist
    (let* ((ckey "sc-citation")
	   (akey "sc-attribution")
	   (ckeyval (assoc ckey sc-mail-info))
	   (akeyval (assoc akey sc-mail-info)))
      (if ckeyval
	  (setcdr ckeyval citation)
	(setq sc-mail-info
	      (append (list (cons ckey citation)) sc-mail-info)))
      (if akeyval
	  (setcdr akeyval attribution)
	(setq sc-mail-info
	      (append (list (cons akey attribution)) sc-mail-info))))

    ;; set the sc-lastchoice attribution
    (let* ((lkey "sc-lastchoice")
	   (lastchoice (assoc lkey sc-attributions)))
      (if lastchoice
	  (setcdr lastchoice attribution)
	(setq sc-attributions
	      (cons (cons lkey attribution) sc-attributions))))
    ))


;; ======================================================================
;; filladapt hooks for supercite 3.1. you shouldn't need anything
;; extra to make gin-mode understand supercited lines.  Even this
;; stuff might not be entirely necessary...

(defun sc-cite-regexp (&optional root-regexp)
  "Return a regexp describing a Supercited line.
The regexp is the concatenation of `sc-citation-leader-regexp',
`sc-citation-root-regexp', `sc-citation-delimiter-regexp', and
`sc-citation-separator-regexp'.  If optional ROOT-REGEXP is supplied,
use it instead of `sc-citation-root-regexp'."
  (concat sc-citation-leader-regexp
	  (or root-regexp sc-citation-root-regexp)
	  sc-citation-delimiter-regexp
	  sc-citation-separator-regexp))

(defun sc-make-citation (attribution)
  "Make a non-nested citation from ATTRIBUTION."
  (concat sc-citation-leader
	  attribution
	  sc-citation-delimiter
	  sc-citation-separator))

(defun sc-setup-filladapt ()
  "Setup `filladapt-prefix-table' to handle Supercited paragraphs."
  (let* ((fa-sc-elt 'filladapt-supercite-included-text)
	 (elt (rassq fa-sc-elt filladapt-prefix-table)))
    (if elt (setcar elt (sc-cite-regexp))
      (message "Filladapt doesn't seem to know about Supercite.")
      (beep))))


;; ======================================================================
;; citing and unciting regions of text

(defvar sc-fill-begin 1
  "Buffer position to begin filling.")
(defvar sc-fill-line-prefix ""
  "Fill prefix of previous line")

;; filling
(defun sc-fill-if-different (&optional prefix)
  "Fill the region bounded by `sc-fill-begin' and point.
Only fill if optional PREFIX is different than `sc-fill-line-prefix'.
If `sc-auto-fill-region-p' is nil, do not fill region.  If PREFIX is
not supplied, initialize fill variables.  This is useful for a regi
`begin' frame-entry."
  (if (not prefix)
      (setq sc-fill-line-prefix ""
	    sc-fill-begin (regi-pos 'bol))
    (if (and sc-auto-fill-region-p
	     (not (string= prefix sc-fill-line-prefix)))
	(let ((fill-prefix sc-fill-line-prefix))
	  (if (not (string= fill-prefix ""))
	      (fill-region sc-fill-begin (regi-pos 'bol)))
	  (setq sc-fill-line-prefix prefix
		sc-fill-begin (regi-pos 'bol))))
    )
  nil)

(defun sc-cite-coerce-cited-line ()
  "Coerce a Supercited line to look like our style."
  (let* ((attribution (sc-guess-attribution))
	 (regexp (sc-cite-regexp attribution))
	 (prefix (sc-make-citation attribution)))
    (if (and attribution
	     (looking-at regexp))
	(progn
	  (delete-region
	   (match-beginning 0)
	   (save-excursion
	     (goto-char (match-end 0))
	     (if (bolp) (forward-char -1))
	     (point)))
	  (insert prefix)
	  (sc-fill-if-different prefix)))
    nil))

(defun sc-cite-coerce-dumb-citer ()
  "Coerce a non-nested citation that's been cited with a dumb nesting citer."
  (delete-region (match-beginning 1) (match-end 1))
  (beginning-of-line)
  (sc-cite-coerce-cited-line))

(defun sc-guess-nesting (&optional string)
  "Guess the citation nesting on the current line.
If nesting cannot be guessed, nil is returned.  Optional STRING if
supplied, is used instead of the line point is on in the current
buffer."
  (let ((start 0)
	(string (or string (buffer-substring (regi-pos 'bol) (regi-pos 'eol))))
	nesting)
    (and
     (= start (or (string-match sc-citation-leader-regexp string start) -1))
     (setq start (match-end 0))
     (= start (or (string-match sc-citation-delimiter-regexp string start) -1))
     (setq nesting (sc-submatch 0 string)
	   start (match-end 0))
     (= start (or (string-match sc-citation-separator-regexp string start) -1))
     nesting)))

(defun sc-add-citation-level ()
  "Add a citation level for nested citation style w/ coersion."
  (let* ((nesting (sc-guess-nesting))
	 (citation (make-string (1+ (length nesting))
				(string-to-char sc-citation-delimiter)))
	 (prefix (concat sc-citation-leader citation sc-citation-separator)))
    (if (looking-at (sc-cite-regexp ""))
	(delete-region (match-beginning 0) (match-end 0)))
    (insert prefix)
    (sc-fill-if-different prefix)))

(defun sc-cite-line (&optional citation)
  "Cite a single line of uncited text.
Optional CITATION overrides any citation automatically selected."
  (if sc-fixup-whitespace-p
      (fixup-whitespace))
  (let ((prefix (or citation
		    (cdr (assoc "sc-citation" sc-mail-info))
		    sc-default-attribution)))
    (insert prefix)
    (sc-fill-if-different prefix))
  nil)

(defun sc-uncite-line ()
  "Remove citation from current line."
  (let ((cited (looking-at (sc-cite-regexp))))
    (if cited
	(delete-region (match-beginning 0) (match-end 0))))
  nil)

(defun sc-recite-line (regexp)
  "Remove citation matching REGEXP from current line and recite line."
  (let ((cited (looking-at (concat "^" regexp)))
	(prefix (cdr (assoc "sc-citation" sc-mail-info))))
    (if cited
	(delete-region (match-beginning 0) (match-end 0)))
    (insert (or prefix sc-default-attribution))
    (sc-fill-if-different prefix))
  nil)

;; interactive functions
(defun sc-cite-region (start end &optional confirm-p)
  "Cite a region delineated by START and END.
If optional CONFIRM-P is non-nil, the attribution is confirmed before
its use in the citation string.  This function first runs
`sc-pre-cite-hook'."
  (interactive "r\nP")
  (undo-boundary)
  (let ((frame (or (sc-scan-info-alist sc-cite-frame-alist)
		   sc-default-cite-frame))
	(sc-confirm-always-p (if confirm-p t sc-confirm-always-p)))
    (run-hooks 'sc-pre-cite-hook)
    (if (interactive-p)
	(sc-select-attribution))
    (regi-interpret frame start end)))

(defun sc-uncite-region (start end)
  "Uncite a region delineated by START and END.
First runs `sc-pre-uncite-hook'."
  (interactive "r")
  (undo-boundary)
  (let ((frame (or (sc-scan-info-alist sc-uncite-frame-alist)
		   sc-default-uncite-frame)))
    (run-hooks 'sc-pre-uncite-hook)
    (regi-interpret frame start end)))
      
(defun sc-recite-region (start end)
  "Recite a region delineated by START and END.
First runs `sc-pre-recite-hook'."
  (interactive "r")
  (let ((sc-confirm-always-p t))
    (sc-select-attribution))
  (undo-boundary)
  (let ((frame (or (sc-scan-info-alist sc-recite-frame-alist)
		   sc-default-recite-frame)))
    (run-hooks 'sc-pre-recite-hook)
    (regi-interpret frame start end)))


;; ======================================================================
;; building headers

(defun sc-hdr (prefix field &optional sep return-nil-p)
  "Returns a concatenation of PREFIX and FIELD.
If FIELD is not a string or is the empty string, the empty string will
be returned.  Optional third argument SEP is concatenated on the end if
it is a string.  Returns empty string, unless optional RETURN-NIL-P is
non-nil."
  (if (and (stringp field)
	   (not (string= field "")))
      (concat prefix field (or sep ""))
    (and (not return-nil-p) "")))

(defun sc-whofrom ()
  "Return the value of (sc-mail-field \"from\") or nil."
  (let ((sc-mumble nil))
    (sc-mail-field "from")))

(defun sc-no-header ()
  "Does nothing.  Use this instead of nil to get a blank header."
  ())

(defun sc-no-blank-line-or-header()
  "Similar to `sc-no-header' except it removes the preceeding blank line."
  (if (not (bobp))
      (if (and (eolp)
	       (progn (forward-line -1)
		      (or (looking-at
			   (concat "^" (regexp-quote mail-header-separator) "$"))
			  (and (eq major-mode 'mh-letter-mode)
			       (mh-in-header-p)))))
	  (progn (forward-line)
		 (let ((kill-lines-magic t))
		   (kill-line))))))

(defun sc-header-on-said ()
  "\"On <date>, <from> said:\" unless:
1. the \"from\" field cannot be found, in which case nothing is inserted;
2. the \"date\" field is missing in which case only the from part is printed."
  (let ((sc-mumble "")
	(whofrom (sc-whofrom)))
    (if whofrom
	(insert sc-reference-tag-string
		(sc-hdr "On " (sc-mail-field "date") ", ")
		whofrom " said:\n"))))

(defun sc-header-inarticle-writes ()
  "\"In article <message-id>, <from> writes:\"
Treats \"message-id\" and \"from\" fields similar to `sc-header-on-said'."
  (let ((sc-mumble "")
	(whofrom (sc-mail-field "from")))
    (if whofrom
	(insert sc-reference-tag-string
		(sc-hdr "In article " (sc-mail-field "message-id") ", ")
		whofrom " writes:\n"))))

(defun sc-header-regarding-adds ()
  "\"Regarding <subject>; <from> adds:\"
Treats \"subject\" and \"from\" fields similar to `sc-header-on-said'."
  (let ((sc-mumble "")
	(whofrom (sc-whofrom)))
    (if whofrom
	(insert sc-reference-tag-string
		(sc-hdr "Regarding " (sc-mail-field "subject") "; ")
		whofrom " adds:\n"))))

(defun sc-header-attributed-writes ()
  "\"<sc-attribution>\" == <sc-author> <address> writes:
Treats these fields in a similar manner to `sc-header-on-said'."
  (let ((sc-mumble "")
	(whofrom (sc-whofrom)))
    (if whofrom
	(insert sc-reference-tag-string
		(sc-hdr "\"" (sc-mail-field "sc-attribution") "\" == ")
		(sc-hdr ""   (sc-mail-field "sc-author") " ")
		(or (sc-hdr "<" (sc-mail-field "sc-from-address") ">" t)
		    (sc-hdr "<" (sc-mail-field "sc-reply-address") ">"  t)
		    "")
		" writes:\n"))))

(defun sc-header-author-writes ()
  "<sc-author> writes:"
  (let ((sc-mumble "")
	(whofrom (sc-whofrom)))
    (if whofrom
	(insert sc-reference-tag-string
		(sc-hdr "" (sc-mail-field "sc-author"))
		" writes:\n"))))

(defun sc-header-verbose ()
  "Very verbose, some say gross."
  (let ((sc-mumble "")
	(whofrom (sc-whofrom))
	(tag sc-reference-tag-string))
    (if whofrom
	(insert (sc-hdr (concat tag "On ") (sc-mail-field "date") ",\n")
		(or (sc-hdr tag (sc-mail-field "sc-author") "\n" t)
		    (concat tag whofrom "\n"))
		(sc-hdr (concat tag "from the organization of ")
			(sc-mail-field "organization") "\n")
		(let ((rtag (concat tag "who can be reached at: ")))
		  (or (sc-hdr rtag (sc-mail-field "sc-from-address") "\n" t)
		      (sc-hdr rtag (sc-mail-field "sc-reply-address")  "\n" t)
		      ""))
		(sc-hdr
		 (concat tag "(whose comments are cited below with \"")
		 (sc-mail-field "sc-citation") "\"),\n")
		(sc-hdr (concat tag "had this to say in article ")
			(sc-mail-field "message-id") "\n")
		(sc-hdr (concat tag "in newsgroups ")
			(sc-mail-field "newsgroups") "\n")
		(sc-hdr (concat tag "concerning the subject of ")
			(sc-mail-field "subject") "\n")
		(sc-hdr (concat tag "(see ")
			(sc-mail-field "references")
			" for more details)\n")
		))))


;; ======================================================================
;; header rewrites

(defconst sc-electric-bufname " *sc-erefs* "
  "Supercite electric reference mode's buffer name.")
(defvar sc-eref-style 0
  "Current electric reference style.")

(defun sc-valid-index-p (index)
  "Returns INDEX if it is a valid index into `sc-rewrite-header-list'.
Otherwise returns nil."
    ;; a number, and greater than or equal to zero
    ;; less than or equal to the last index
    (and (natnump index)
	 (< index (length sc-rewrite-header-list))
	 index))

(defun sc-eref-insert-selected (&optional nomsg)
  "Insert the selected reference header in the current buffer.
Optional NOMSG, if non-nil, inhibits printing messages, unless an
error occurs."
  (let ((ref (nth sc-eref-style sc-rewrite-header-list)))
    (condition-case err
	(progn
	  (eval ref)
	  (let ((lines (count-lines (point-min) (point-max))))
	    (or nomsg (message "Ref header %d [%d line%s]: %s" 
			       sc-eref-style lines
			       (if (= lines 1) "" "s")
			       ref))))
      (void-function
       (progn (message
	       "Symbol's function definition is void: %s (Header %d)"
	       (car (cdr err)) sc-eref-style)
	      (beep)
	      ))
      )))

(defun sc-electric-mode (&optional arg)
  "
Mode for viewing Supercite reference headers.  Commands are:
\n\\{sc-electric-mode-map}

`sc-electric-mode' is not intended to be run interactively, but rather
accessed through Supercite's electric reference feature.  See
`sc-insert-reference' for more details.  Optional ARG is the initial
header style to use, unless not supplied or invalid, in which case
`sc-preferred-header-style' is used."

  (let ((info sc-mail-info))

    (setq sc-eref-style
	  (or (sc-valid-index-p arg)
	      (sc-valid-index-p sc-preferred-header-style)
	      0))

    (get-buffer-create sc-electric-bufname)
    ;; set up buffer and enter command loop
    (save-excursion
      (save-window-excursion
	(pop-to-buffer sc-electric-bufname)
	(kill-all-local-variables)
	(let ((sc-mail-info info)
	      (buffer-read-only t)
	      (mode-name "SC Electric Refs")
	      (major-mode 'sc-electric-mode))
	  (use-local-map sc-electric-mode-map)
	  (sc-eref-show sc-eref-style)
	  (run-hooks 'sc-electric-mode-hook)
	  (recursive-edit)
	  )))

    (and sc-eref-style
	 (sc-eref-insert-selected))
    (kill-buffer sc-electric-bufname)
    ))

;; functions for electric reference mode
(defun sc-eref-show (index)
  "Show reference INDEX in `sc-rewrite-header-list'."
  (let ((msg "No %ing reference headers in list.")
	(last (length sc-rewrite-header-list)))
    (setq sc-eref-style
	  (cond
	   ((sc-valid-index-p index) index)
	   ((< index 0)
	    (if sc-electric-circular-p
		(1- last)
	      (progn (error msg "preced") 0)))
	   ((>= index last)
	    (if sc-electric-circular-p
		0
	      (progn (error msg "follow") (1- last))))
	   ))
    (save-excursion
     (set-buffer sc-electric-bufname)
     (let ((buffer-read-only nil))
       (erase-buffer)
       (goto-char (point-min))
       (sc-eref-insert-selected)
       ;; now shrink the window to just contain the electric reference
       ;; header.
       (let ((hdrlines (count-lines (point-min) (point-max)))
	     (winlines (1- (window-height))))
	 (if (/= hdrlines winlines)
	     (if (> hdrlines winlines)
		 ;; we have to enlarge the window
		 (enlarge-window (- hdrlines winlines))
	       ;; we have to shrink the window
	       (shrink-window (- winlines (max hdrlines window-min-height)))
	       )))
       ))))

(defun sc-eref-next ()
  "Display next reference in other buffer."
  (interactive)
  (sc-eref-show (1+ sc-eref-style)))

(defun sc-eref-prev ()
  "Display previous reference in other buffer."
  (interactive)
  (sc-eref-show (1- sc-eref-style)))

(defun sc-eref-setn ()
  "Set reference header selected as preferred."
  (interactive)
  (setq sc-preferred-header-style sc-eref-style)
  (message "Preferred reference style set to header %d." sc-eref-style))

(defun sc-eref-goto (refnum)
  "Show reference style indexed by REFNUM.
If REFNUM is an invalid index, don't go to that reference and return
nil."
  (interactive "NGoto Reference: ")
  (if (sc-valid-index-p refnum)
      (sc-eref-show refnum)
    (error "Invalid reference: %d. (Range: [%d .. %d])"
	   refnum 0 (1- (length sc-rewrite-header-list)))
    ))

(defun sc-eref-jump ()
  "Set reference header to preferred header."
  (interactive)
  (sc-eref-show sc-preferred-header-style))

(defun sc-eref-abort ()
  "Exit from electric reference mode without inserting reference."
  (interactive)
  (setq sc-eref-style nil)
  (exit-recursive-edit))

(defun sc-eref-exit ()
  "Exit from electric reference mode and insert selected reference."
  (interactive)
  (exit-recursive-edit))

(defun sc-insert-reference (arg)
  "Insert, at point, a reference header in the body of the reply.
Numeric ARG indicates which header style from `sc-rewrite-header-list'
to use when rewriting the header.  No supplied ARG indicates use of
`sc-preferred-header-style'.

With just `\\[universal-argument]', electric reference insert mode is
entered, regardless of the value of `sc-electric-references-p'.  See
`sc-electric-mode' for more information."
  (interactive "P")
  (if (consp arg)
      (sc-electric-mode)
    (let ((preference (or (sc-valid-index-p arg)
			  (sc-valid-index-p sc-preferred-header-style)
			  sc-preferred-header-style
			  0)))
      (if sc-electric-references-p
	  (sc-electric-mode preference)
	(sc-eref-insert-selected t)
	))))


;; ======================================================================
;; variable toggling

(defun sc-raw-mode-toggle ()
  "Toggle, in one fell swoop, two important SC variables:
`sc-fixup-whitespace-p' and `sc-auto-fill-region-p'"
  (interactive)
  (setq sc-fixup-whitespace-p (not sc-fixup-whitespace-p)
	sc-auto-fill-region-p (not sc-auto-fill-region-p))
  (sc-set-mode-string)
  (force-mode-line-update))

(defun sc-toggle-var (variable)
  "Boolean toggle VARIABLE's value.
VARIABLE must be a bound symbol.  Nil values change to t, non-nil
values are changed to nil."
  (message "%s changed from %s to %s"
	   variable (symbol-value variable)
	   (set-variable variable (not (eval-expression variable))))
  (sc-set-mode-string))

(defun sc-set-variable (var)
  "Set the Supercite VARIABLE.
This function mimics `set-variable', except that the variable to set
is determined non-interactively.  The value is queried for in the
minibuffer exactly the same way that `set-variable' does it.

You can see the current value of the variable when the minibuffer is
querying you by typing `C-h'.  Note that the format is changed
slightly from that used by `set-variable' -- the current value is
printed just after the variable's name instead of at the bottom of the
help window."
  (let* ((minibuffer-help-form
	  '(funcall myhelp))
	 (myhelp
	  (function
	   (lambda ()
	     (with-output-to-temp-buffer "*Help*"
	       (prin1 var)
	       (if (boundp var)
		   (let ((print-length 20))
		     (princ "\t(Current value: ")
		     (prin1 (symbol-value var))
		     (princ ")")))
	       (princ "\n\nDocumentation:\n")
	       (princ (substring (documentation-property
				  var
				  'variable-documentation)
				  1))
	       (save-excursion
		 (set-buffer standard-output)
		 (help-mode))
	       nil)))))
    (set var (eval-minibuffer (format "Set %s to value: " var))))
  (sc-set-mode-string))

(defmacro sc-toggle-symbol (rootname)
  (list 'defun (intern (concat "sc-T-" rootname)) '()
	(list 'interactive)
	(list 'sc-toggle-var
	      (list 'quote (intern (concat "sc-" rootname "-p"))))))

(defmacro sc-setvar-symbol (rootname)
  (list 'defun (intern (concat "sc-S-" rootname)) '()
	(list 'interactive)
	(list 'sc-set-variable
	      (list 'quote (intern (concat "sc-" rootname))))))

(sc-toggle-symbol "confirm-always")
(sc-toggle-symbol "downcase")
(sc-toggle-symbol "electric-references")
(sc-toggle-symbol "auto-fill-region")
(sc-toggle-symbol "mail-nuke-blank-lines")
(sc-toggle-symbol "nested-citation")
(sc-toggle-symbol "electric-circular")
(sc-toggle-symbol "use-only-preferences")
(sc-toggle-symbol "fixup-whitespace")

(sc-setvar-symbol "preferred-attribution-list")
(sc-setvar-symbol "preferred-header-style")
(sc-setvar-symbol "mail-nuke-mail-headers")
(sc-setvar-symbol "mail-header-nuke-list")
(sc-setvar-symbol "cite-region-limit")

(defun sc-T-describe ()
  "

Supercite provides a number of key bindings which simplify the process
of setting or toggling certain variables controlling its operation.

Note on function names in this list: all functions of the form
`sc-S-<name>' actually call `sc-set-variable' on the corresponding
`sc-<name>' variable.  All functions of the form `sc-T-<name>' call
`sc-toggle-var' on the corresponding `sc-<name>-p' variable.

\\{sc-T-keymap}"
  (interactive)
  (describe-function 'sc-T-describe))

(defun sc-set-mode-string ()
  "Update the minor mode string to show state of Supercite."
  (setq sc-mode-string
	(concat " SC"
		(if (or sc-auto-fill-region-p
			sc-fixup-whitespace-p)
		    ":" "")
		(if sc-auto-fill-region-p "f" "")
		(if sc-fixup-whitespace-p "w" "")
		)))


;; ======================================================================
;; published interface to mail and news readers

;;;###autoload
(defun sc-cite-original ()
  "Workhorse citing function which performs the initial citation.
This is callable from the various mail and news readers' reply
function according to the agreed upon standard.  See `\\[sc-describe]'
for more details.  `sc-cite-original' does not do any yanking of the
original message but it does require a few things:

     1) The reply buffer is the current buffer.

     2) The original message has been yanked and inserted into the
        reply buffer.

     3) Verbose mail headers from the original message have been
        inserted into the reply buffer directly before the text of the
        original message.

     4) Point is at the beginning of the verbose headers.

     5) Mark is at the end of the body of text to be cited.

For Emacs 19's, the region need not be active (and typically isn't
when this function is called.  Also, the hook `sc-pre-hook' is run
before, and `sc-post-hook' is run after the guts of this function."
  (run-hooks 'sc-pre-hook)

  ;; before we do anything, we want to insert the supercite keymap so
  ;; we can proceed from here
  (and sc-mode-map-prefix
       (local-set-key sc-mode-map-prefix sc-mode-map))

  ;; hack onto the minor mode alist, if it hasn't been done before,
  ;; then turn on the minor mode. also, set the minor mode string with
  ;; the values of fill and fixup whitespace variables
  (if (not (get 'minor-mode-alist 'sc-minor-mode))
      (progn
	(put 'minor-mode-alist 'sc-minor-mode 'sc-minor-mode)
	(setq minor-mode-alist
	      (cons '(sc-minor-mode sc-mode-string) minor-mode-alist))
	))
  (setq sc-minor-mode t)
  (sc-set-mode-string)

  (undo-boundary)

  ;; grab point and mark since the region is probably not active when
  ;; this function gets automatically called. we want point to be a
  ;; mark so any deleting before point works properly
  (let* ((zmacs-regions nil)		; for Lemacs
	 (mark-active t)		; for FSFmacs
	 (point (point-marker))
	 (mark  (copy-marker (mark-marker))))
  
    ;; make sure point comes before mark, not all functions are
    ;; interactive "r"
    (if (< mark point)
	(let ((tmp point))
	  (setq point mark
		mark tmp)))

    ;; first process mail headers, and populate sc-mail-info
    (sc-mail-process-headers point mark)

    ;; now get possible attributions
    (sc-attribs-chop-address (or (sc-mail-field "from")
				 (sc-mail-field "reply")
				 (sc-mail-field "reply-to")
				 (sc-mail-field "sender")))
    ;; select the attribution
    (sc-select-attribution)

    ;; cite the region, but first check the value of sc-cite-region-limit
    (let ((linecnt (count-lines point mark)))
      (and sc-cite-region-limit
	   (if (or (not (numberp sc-cite-region-limit))
		   (<= linecnt sc-cite-region-limit))
	       (progn
		 ;; cite the region and insert the header rewrite
		 (sc-cite-region point mark)
		 (goto-char point)
		 (let ((sc-eref-style (or sc-preferred-header-style 0)))
		   (if sc-electric-references-p
		       (sc-electric-mode sc-eref-style)
		     (sc-eref-insert-selected t))))
	     (beep)
	     (message
	      "Region not cited. %d lines exceeds sc-cite-region-limit: %d"
	      linecnt sc-cite-region-limit))))

    ;; finally, free the point-marker
    (set-marker point nil)
    (set-marker mark nil)
    )
  (run-hooks 'sc-post-hook)
  ;; post hook could have changed the variables
  (sc-set-mode-string))


;; ======================================================================
;; bug reporting and miscellaneous commands

(defun sc-open-line (arg)
  "Like `open-line', but insert the citation prefix at the front of the line.
With numeric ARG, inserts that many new lines."
  (interactive "p")
  (save-excursion
    (let ((start (point))
	  (prefix (or (progn (beginning-of-line)
			     (if (looking-at (sc-cite-regexp))
				 (sc-submatch 0)))
		      "")))
      (goto-char start)
      (open-line arg)
      (forward-line 1)
      (while (< 0 arg)
	(insert prefix)
	(forward-line 1)
	(setq arg (1- arg))
	))))

(defun sc-insert-citation (arg)
  "Insert citation string at beginning of current line if not already cited.
With `\\[universal-argument]' insert citation even if line is already
cited."
  (interactive "P")
  (save-excursion
    (beginning-of-line)
    (if (or (not (looking-at (sc-cite-regexp)))
	    (looking-at "^[ \t]*$")
	    (consp arg))
	(insert (sc-mail-field "sc-citation"))
      (error "Line is already cited."))))

(defun sc-version (arg)
  "Echo the current version of Supercite in the minibuffer.
With \\[universal-argument] (universal-argument), or if run non-interactively,
inserts the version string in the current buffer instead."
  (interactive "P")
  (let ((verstr (format "Using Supercite.el %s" sc-version)))
    (if (or (consp arg)
	    (not (interactive-p)))
	(insert "`sc-version' says: " verstr)
      (message verstr))))

(defun sc-describe ()
  "
Supercite is a package which provides a flexible mechanism for citing
email and news replies.  Please see the associated texinfo file for
more information."
  (interactive)
  (describe-function 'sc-describe))

(defun sc-submit-bug-report ()
  "Submit a bug report on Supercite via mail."
  (interactive)
  (require 'reporter)
  (and
   (y-or-n-p "Do you want to submit a report on Supercite? ")
   (reporter-submit-bug-report
    sc-help-address
    (concat "Supercite version " sc-version)
    (list
     'sc-attrib-selection-list
     'sc-auto-fill-region-p
     'sc-blank-lines-after-headers
     'sc-citation-leader
     'sc-citation-delimiter
     'sc-citation-separator
     'sc-citation-leader-regexp
     'sc-citation-root-regexp
     'sc-citation-nonnested-root-regexp
     'sc-citation-delimiter-regexp
     'sc-citation-separator-regexp
     'sc-cite-region-limit
     'sc-confirm-always-p
     'sc-default-attribution
     'sc-default-author-name
     'sc-downcase-p
     'sc-electric-circular-p
     'sc-electric-references-p
     'sc-fixup-whitespace-p
     'sc-mail-warn-if-non-rfc822-p
     'sc-mumble
     'sc-name-filter-alist
     'sc-nested-citation-p
     'sc-nuke-mail-headers
     'sc-nuke-mail-header-list
     'sc-preferred-attribution-list
     'sc-preferred-header-style
     'sc-reference-tag-string
     'sc-rewrite-header-list
     'sc-titlecue-regexp
     'sc-use-only-preference-p
     ))))


;; useful stuff
(provide 'supercite)
(run-hooks 'sc-load-hook)

;;; supercite.el ends here
