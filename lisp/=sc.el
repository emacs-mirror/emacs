;; -*- Mode: Emacs-Lisp -*-
;; sc.el -- Version 2.3 (used to be supercite.el)

;; ========== Introduction ==========
;; Citation and attribution package for various GNU emacs news and
;; electronic mail reading subsystems.  This version of supercite will
;; interface to VM 4.40+ and MH-E 3.7 (shipped w/ emacs 18.57) as is.
;; It will also interface with GNUS 3.12+, RMAIL 18.55+, GNEWS, and
;; probably most other news/mail subsystems by using the overloading
;; functions provided in sc-oloads.el (see that file or the README for
;; more information on interfacing supercite with your reader subsystem).
;; This version should now be compatible with Lucid Emacs 19.x emacses.

;; This package does not do any yanking of messages, but instead
;; massages raw reply buffers set up by the reply/forward functions in
;; the news/mail subsystems. Therefore, such useful operations as
;; yanking and citing portions of the original article (instead of the
;; whole article) are not within the ability or responsiblity of
;; supercite.

;; ========== Disclaimer ==========
;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor, nor any
;; author's past, present, or future employers accepts responsibility
;; to anyone for the consequences of using it or for whether it serves
;; any particular purpose or works at all, unless he says so in
;; writing.

;; Some of this software was written as part of the supercite author's
;; official duty as an employee of the United States Government and is
;; thus not subject to copyright.  You are free to use that particular
;; software as you wish, but WITHOUT ANY WARRANTY WHATSOEVER.  It
;; would be nice, though if when you use any of this or other freely
;; available code, you give due credit to the author.

;; Other parts of this code were written by other people.  Wherever
;; possible, credit to that author, and the copy* notice supplied by
;; the author are included with that code. The supercite author is no
;; longer an employee of the U.S. Government so the GNU Public Licence
;; should be considered in effect for all enhancements and bug fixes
;; performed by the author.

;; ========== Author (unless otherwise stated) ========================
;; NAME: Barry A. Warsaw      USMAIL: Century Computing, Inc.
;; TELE: (301) 593-3330               1014 West Street
;; INET: bwarsaw@cen.com              Laurel, Md 20707
;; UUCP: uunet!cen.com!bwarsaw
;;
;; Want to be on the Supercite mailing list?
;;
;; Send articles to:
;;     Internet: supercite@anthem.nlm.nih.gov
;;         UUCP: uunet!anthem.nlm.nih.gov!supercite
;; 
;; Send administrivia (additions/deletions to list, etc) to:
;;     Internet: supercite-request@anthem.nlm.nih.gov
;;         UUCP: uunet!anthem.nlm.nih.gov!supercite-request

;; ========== Credits and Thanks ==========
;; This package was derived from the Superyank 1.11 package as posted
;; to the net.  Superyank 1.11 was inspired by code and ideas from
;; Martin Neitzel and Ashwin Ram.  Supercite version 2.3 has evolved
;; through the comments and suggestions of the supercite mailing list
;; which consists of many authors and users of the various mail and
;; news reading subsystems.

;; Many folks on the supercite mailing list have contributed their
;; help in debugging, making suggestions and supplying support code or
;; bug fixes for the previous versions of supercite.  I want to thank
;; everyone who helped, especially (in no particular order):
;;
;; Mark D. Baushke, Khalid Sattar, David Lawrence, Chris Davis, Kyle
;; Jones, Kayvan Sylvan, Masanobu Umeda, Dan Jacobson, Piet van
;; Oostrum, Hamish (H.I.) Macdonald, and Joe Wells.
;;
;; I don't mean to leave anyone out. All who have helped have been
;; appreciated.

;; ========== Getting Started ==========
;; Here is a quick guide to getting started with supercite. The
;; information contained here is mostly excerpted from the more
;; detailed explanations given in the accompanying README file.
;; Naturally, there are many customizations you can do to give your
;; replies that personalized flair, but the instructions in this
;; section should be sufficient for getting started.

;; With this release of supercite overloading is the only supported
;; way to get supercite hooked into your favorite news/mail reading
;; subsystems.  Overloading will be necessary for RMAIL, GNUS, GNEWS,
;; RNEWS and PCMAIL. Overloading will not be needed for VM 4.37+ or
;; MH-E 3.7+.  MH-E comes with emacs 18.57 but if you have an earlier
;; version of emacs, you should be able to ftp MH-E 3.7 separately. Or
;; you can extract the MH-E overloading stuff from version 2.1's
;; sc-oloads.el.

;; First, to connect supercite to any mail/news reading subsystem, put
;; this in your .emacs file:
;;
;; (setq mail-yank-hooks 'sc-cite-original)  ; for all but MH-E
;; (setq mh-yank-hooks   'sc-cite-original)  ; for MH-E only
;;
;; If supercite is not pre-loaded into your emacs session, you should
;; add the following autoload:
;;
;; (autoload 'sc-cite-original "sc" "Supercite 2.3" t)
;;
;; Then, if you need to overload, put the following in your .emacs file:
;;
;; (defun my-sc-overload-hook ()
;;   (require 'sc-oloads)      ; be sure this file is on your load-path
;;   (sc-overload-functions))
;;
;; (setq news-reply-mode-hook 'my-sc-overload-hook) ; for RNEWS,GNUS,GNEWS
;; (setq mail-setup-hook      'my-sc-overload-hook) ; for RMAIL, PCMAIL
;;
;; Finally, if you want to customize supercite, you should do it in a
;; function called my-supercite-hook and:
;;
;; (setq sc-load-hook 'my-supercite-hook)

(require 'sc-alist)


;; vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
;; start of user defined variables
;; vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

(defvar sc-nested-citation-p nil
  "*Controls whether to use nested or non-nested citation style.
Non-nil uses nested citations, nil uses non-nested citations.  Type
\\[sc-describe] for more information.")

(defvar sc-citation-leader "	"
  "*String comprising first part of a citation.")

(defvar sc-citation-delimiter ">"
  "*String comprising third part of a citation.
This string is used in both nested and non-nested citations.")

(defvar sc-citation-separator " "
  "*String comprising fourth and last part of a citation.")

(defvar sc-default-author-name "Anonymous"
  "*String used when author's name cannot be determined.")

(defvar sc-default-attribution "Anon"
  "*String used when author's attribution cannot be determined.")

;; Noriya KOBAYASHI (nk@ics.osaka-u.ac.jp) writes to the supercite
;; mailing list:
;; I use supercite in Nemacs-3.3.2.  In order to handle citation using
;; Kanji, [...set sc-cite-regexp to...]
;; "\\s *\\([a-zA-Z0-9]\\|\\cc\\|\\cC\\|\\ch\\|\\cH\\|\\ck\\|\\cK\\)*\\s *>+"
;;
(defvar sc-cite-regexp "\\s *[-a-zA-Z0-9_.]*>+\\s *"
  "*Regular expression describing how a already cited line begins.
The regexp is only used at the beginning of a line, so it doesn't need
to start with a '^'.")

(defvar sc-titlecue-regexp "\\s +-+\\s +"
  "*Regular expression describing the separator between names and titles.
Set to nil to treat entire field as a name.")

(defvar sc-spacify-name-chars '(?_ ?* ?+ ?=)
  "*List of characters to convert to spaces if found in an author's name.")

(defvar sc-nicknames-alist
  '(("Michael" "Mike")
    ("Daniel" "Dan")
    ("David" "Dave")
    ("Jonathan" "John")
    ("William" "Bill")
    ("Elizabeth" "Beth")
    ("Elizabeth" "Betsy")
    ("Kathleen" "Kathy")
    ("Smith" "Smitty"))
  "*Association list of names and their common nicknames.
Entries are of the form (NAME NICKNAME), and NAMEs can have more than
one nickname. Nicknames will not be automatically used as an
attribution string, since I'm not sure this is really polite, but if a
name is glommed from the author name and presented in the attribution
string completion list, the matching nicknames will also be presented.
Set this variable to nil to defeat nickname expansions. Also note that
nicknames are not put in the supercite information alist.")

(defvar sc-confirm-always-p t
  "*If non-nil, always confirm attribution string before citing text body.")

(defvar sc-preferred-attribution 'firstname
  "*Specifies which part of the author's name becomes the attribution.
The value of this variable must be one of the following quoted symbols:

     emailname   -- email terminus name
     initials    -- initials of author
     firstname   -- first name of author
     lastname    -- last name of author
     middlename1 -- first middle name of author
     middlename2 -- second middle name of author
     ...

Middle name indexes can be any positive integer greater than 0, though
it is unlikely that many authors will supply more than one middle
name, if that many.")

(defvar sc-use-only-preference-p nil
  "*Controls what happens when the preferred attribution cannot be found.
If non-nil, then sc-default-attribution will be used. If nil, then
some secondary scheme will be employed to find a suitable attribution
string.")

(defvar sc-downcase-p nil
  "*Non-nil means downcase the attribution and citation strings.")

(defvar sc-rewrite-header-list
  '((sc-no-header)
    (sc-header-on-said)
    (sc-header-inarticle-writes)
    (sc-header-regarding-adds)
    (sc-header-attributed-writes)
    (sc-header-verbose)
    (sc-no-blank-line-or-header)
    )
  "*List of reference header rewrite functions.
The variable sc-preferred-header-style controls which function in this
list is chosen for automatic reference header insertions. Electric
reference mode will cycle through this list of functions. For more
information, type \\[sc-describe].")

(defvar sc-preferred-header-style 1
  "*Index into sc-rewrite-header-list specifying preferred header style.
Index zero accesses the first function in the list.")

(defvar sc-electric-references-p t
  "*Use electric references if non-nil.")

(defvar sc-electric-circular-p t
  "*Treat electric references as circular if non-nil.")

(defvar sc-mail-fields-list
  '("date"  "message-id"  "subject" "newsgroups" "references"
    "from"  "return-path" "path"    "reply-to"   "organization"
    "reply" )
  "*List of mail header whose values will be saved by supercite.
These values can be used in header rewrite functions by accessing them
with the sc-field function. Mail headers in this list are case
insensitive and do not require a trailing colon.")

(defvar sc-mumble-string ""
  "*Value returned by sc-field if chosen field cannot be found.")

(defvar sc-nuke-mail-headers-p t
  "*Nuke or don't nuke mail headers.
If non-nil, nuke mail headers after gleaning useful information from
them.")

(defvar sc-reference-tag-string ">>>>> "
  "*String used at the beginning of built-in reference headers.")

(defvar sc-fill-paragraph-hook 'sc-fill-paragraph
  "*Hook for filling a paragraph.  
This hook gets executed when you fill a paragraph either manually or
automagically. It expects point to be within the extent of the
paragraph that is going to be filled. This hook allows you to use a
different paragraph filling package than the one supplied with
supercite.")

(defvar sc-auto-fill-region-p nil
  "*If non-nil, automatically fill each paragraph after it has been cited.")

(defvar sc-auto-fill-query-each-paragraph-p nil
  "*If non-nil, query before filling each paragraph.
No querying and no filling will be performed if sc-auto-fill-region-p
is set to nil.")

(defvar sc-fixup-whitespace-p nil
  "*If non-nil, delete all leading white space before citing.")

(defvar sc-all-but-cite-p nil
  "*If non-nil, sc-cite-original does everything but cite the text.
This is useful for manually citing large messages, or portions of
large messages. When non-nil, sc-cite-original will still set up all
necessary variables and databases, but will skip the citing routine
which modify the reply buffer's text.")

(defvar sc-load-hook nil
  "*User definable hook.
Runs after supercite is loaded. Set your customizations here.")

(defvar sc-pre-hook nil
  "*User definable hook.
Runs before sc-cite-original executes.")

(defvar sc-post-hook nil
  "*User definable hook.
Runs after sc-cite-original executes.")

(defvar sc-header-nuke-list
  '("via" "origin" "status" "received" "remailed" "cc" "sender" "replied"
    "organization" "keywords" "distribution" "xref" "references" "expires"
    "approved" "summary" "precedence" "subject" "newsgroup[s]?"
    "\\(followup\\|apparently\\|errors\\|\\(\\(in-\\)?reply\\)?-\\)?to"
    "x-[a-z0-9-]+" "[a-z-]*message-id" "\\(summary-\\)?line[s]"
    "\\(\\(return\\|reply\\)-\\)?path" "\\(posted-\\)?date"
    "\\(mail-\\)?from")
  "*List of mail headers to remove from body of reply.")



;; ======================================================================
;; keymaps

(defvar sc-default-keymap
  '(lambda ()
     (local-set-key "\C-c\C-r" 'sc-insert-reference)
     (local-set-key "\C-c\C-t" 'sc-cite)
     (local-set-key "\C-c\C-a" 'sc-recite)
     (local-set-key "\C-c\C-u" 'sc-uncite)
     (local-set-key "\C-c\C-i" 'sc-insert-citation)
     (local-set-key "\C-c\C-o" 'sc-open-line)
     (local-set-key "\C-c\C-q" 'sc-fill-paragraph-manually)
     (local-set-key "\C-cq"    'sc-fill-paragraph-manually)
     (local-set-key "\C-c\C-m" 'sc-modify-information)
     (local-set-key "\C-cf"    'sc-view-field)
     (local-set-key "\C-cg"    'sc-glom-headers)
     (local-set-key "\C-c\C-v" 'sc-version)
     (local-set-key "\C-c?"    'sc-describe)
     )
  "*Default keymap if major-mode can't be found in `sc-local-keymaps'.")

(defvar sc-local-keymaps
  '((mail-mode
     (lambda ()
       (local-set-key "\C-c\C-r" 'sc-insert-reference)
       (local-set-key "\C-c\C-t" 'sc-cite)
       (local-set-key "\C-c\C-a" 'sc-recite)
       (local-set-key "\C-c\C-u" 'sc-uncite)
       (local-set-key "\C-c\C-i" 'sc-insert-citation)
       (local-set-key "\C-c\C-o" 'sc-open-line)
       (local-set-key "\C-c\C-q" 'sc-fill-paragraph-manually)
       (local-set-key "\C-cq"    'sc-fill-paragraph-manually)
       (local-set-key "\C-c\C-m" 'sc-modify-information)
       (local-set-key "\C-cf"    'sc-view-field)
       (local-set-key "\C-cg"    'sc-glom-headers)
       (local-set-key "\C-c\C-v" 'sc-version)
       (local-set-key "\C-c?"    'sc-describe)
       ))
    (mh-letter-mode
     (lambda ()
       (local-set-key "\C-c\C-r" 'sc-insert-reference)
       (local-set-key "\C-c\C-t" 'sc-cite)
       (local-set-key "\C-c\C-a" 'sc-recite)
       (local-set-key "\C-c\C-u" 'sc-uncite)
       (local-set-key "\C-ci"    'sc-insert-citation)
       (local-set-key "\C-c\C-o" 'sc-open-line)
       (local-set-key "\C-cq"    'sc-fill-paragraph-manually)
       (local-set-key "\C-c\C-m" 'sc-modify-information)
       (local-set-key "\C-cf"    'sc-view-field)
       (local-set-key "\C-cg"    'sc-glom-headers)
       (local-set-key "\C-c\C-v" 'sc-version)
       (local-set-key "\C-c?"    'sc-describe)
       ))
    (news-reply-mode mail-mode)
    (vm-mail-mode mail-mode)
    (e-reply-mode mail-mode)
    (n-reply-mode mail-mode)
    )
  "*List of keymaps to use with the associated major-mode.")

(defvar sc-electric-mode-map nil
  "*Keymap for sc-electric-mode.")

(if sc-electric-mode-map
    nil
  (setq sc-electric-mode-map (make-sparse-keymap))
  (define-key sc-electric-mode-map "p"     'sc-eref-prev)
  (define-key sc-electric-mode-map "n"     'sc-eref-next)
  (define-key sc-electric-mode-map "s"     'sc-eref-setn)
  (define-key sc-electric-mode-map "j"     'sc-eref-jump)
  (define-key sc-electric-mode-map "x"     'sc-eref-abort)
  (define-key sc-electric-mode-map "\r"    'sc-eref-exit)
  (define-key sc-electric-mode-map "\n"    'sc-eref-exit)
  (define-key sc-electric-mode-map "q"     'sc-eref-exit)
  (define-key sc-electric-mode-map "g"     'sc-eref-goto)
  )

;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;; end of user defined variables
;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


;; ======================================================================
;; global variables, not user accessable

(defconst sc-version-number "2.3"
  "Supercite's version number.")

;; when rnewspost.el patch is installed (or function is overloaded)
;; this should be nil since supercite now does this itself.
(setq news-reply-header-hook nil)

;; autoload for sc-electric-mode
(autoload 'sc-electric-mode "sc-elec"
	  "Quasi-major mode for viewing supercite reference headers." nil)

;; global alists (gals), misc variables. make new bytecompiler happy
(defvar sc-gal-information nil
  "Internal global alist variable containing information.")
(defvar sc-gal-attributions nil
  "Internal global alist variable containing attributions.")
(defvar sc-fill-arg nil
  "Internal fill argument holder.")
(defvar sc-cite-context nil
  "Internal citation context holder.")
(defvar sc-force-confirmation-p nil
  "Internal variable.")

(make-variable-buffer-local 'sc-gal-attributions)
(make-variable-buffer-local 'sc-gal-information)
(make-variable-buffer-local 'sc-leached-keymap)
(make-variable-buffer-local 'sc-fill-arg)
(make-variable-buffer-local 'sc-cite-context)

(setq-default sc-gal-attributions nil)
(setq-default sc-gal-information  nil)
(setq-default sc-leached-keymap (current-local-map))
(setq-default sc-fill-arg nil)
(setq-default sc-cite-context nil)



;; ======================================================================
;; miscellaneous support functions

(defun sc-mark ()
  "Mark compatibility between emacs v18 and v19."
  (let ((zmacs-regions nil))
    (mark)))

(defun sc-update-gal (attribution)
  "Update the information alist.
Add ATTRIBUTION and compose the nested and non-nested citation
strings."
  (let ((attrib (if sc-downcase-p (downcase attribution) attribution)))
    (aput 'sc-gal-information "sc-attribution" attrib)
    (aput 'sc-gal-information "sc-nested-citation"
	  (concat attrib sc-citation-delimiter))
    (aput 'sc-gal-information "sc-citation"
	  (concat sc-citation-leader
		  attrib
		  sc-citation-delimiter
		  sc-citation-separator))))

(defun sc-valid-index-p (index)
  "Returns t if INDEX is a valid index into sc-rewrite-header-list."
  (let ((last (1- (length sc-rewrite-header-list))))
    (and (natnump index)  ;; a number, and greater than or equal to zero
	 (<= index last)  ;; less than or equal to the last index
	 )))

(defun sc-string-car (namestring)
  "Return the string-equivalent \"car\" of NAMESTRING.

     example: (sc-string-car \"John Xavier Doe\")
              => \"John\""
  (substring namestring
	     (progn (string-match "\\s *" namestring) (match-end 0))
	     (progn (string-match "\\s *\\S +" namestring) (match-end 0))))

(defun sc-string-cdr (namestring)
  "Return the string-equivalent \"cdr\" of NAMESTRING.

     example: (sc-string-cdr \"John Xavier Doe\")
              => \"Xavier Doe\""
  (substring namestring
	     (progn (string-match "\\s *\\S +\\s *" namestring)
		    (match-end 0))))

(defun sc-linepos (&optional position col-p)
  "Return the character position at various line positions.
Optional POSITION can be one of the following symbols:
     bol == beginning of line
     boi == beginning of indentation
     eol == end of line [default]

Optional COL-P non-nil returns current-column instead of character position."
  (let ((tpnt (point))
	rval)
    (cond
     ((eq position 'bol) (beginning-of-line))
     ((eq position 'boi) (back-to-indentation))
     (t (end-of-line)))
    (setq rval (if col-p (current-column) (point)))
    (goto-char tpnt)
    rval))


;; ======================================================================
;; this section snarfs mail fields and places them in the info alist

(defun sc-build-header-zap-regexp ()
  "Return a regexp for sc-mail-yank-clear-headers."
  (let ((headers sc-header-nuke-list)
	(regexp nil))
    (while headers
      (setq regexp (concat regexp
			   "^" (car headers) ":"
			   (if (cdr headers) "\\|" nil)))
      (setq headers (cdr headers)))
    regexp))

(defun sc-mail-yank-clear-headers (start end)
  "Nuke mail headers between START and END."
  (if (and sc-nuke-mail-headers-p sc-header-nuke-list)
      (let ((regexp (sc-build-header-zap-regexp)))
	(save-excursion
	  (goto-char start)
	  (if (search-forward "\n\n" end t)
	      (save-restriction
		(narrow-to-region start (point))
		(goto-char start)
		(while (let ((case-fold-search t))
			 (re-search-forward regexp nil t))
		  (beginning-of-line)
		  (delete-region (point)
				 (progn (re-search-forward "\n[^ \t]")
					(forward-char -1)
					(point)))
		  )))
	  ))))

(defun sc-mail-fetch-field (field)
  "Return the value of the header field FIELD.
The buffer is expected to be narrowed to just the headers of the
message."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
	  (name (concat "^" (regexp-quote field) "[ \t]*:[ \t]*")))
      (goto-char (point-min))
      (if (re-search-forward name nil t)
	  (let ((opoint (point)))
	    (while (progn (forward-line 1)
			  (looking-at "[ \t]")))
	    (buffer-substring opoint (1- (point))))))))

(defun sc-fetch-fields (start end)
  "Fetch the mail fields in the region from START to END.
These fields can be accessed in header rewrite functions with sc-field."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (let ((fields sc-mail-fields-list))
	(while fields
	  (let ((value (sc-mail-fetch-field (car fields)))
		(next (cdr fields)))
	    (and value
		 (aput 'sc-gal-information (car fields) value))
	    (setq fields next)))
	(if (sc-mail-fetch-field "from")
	    (aput 'sc-gal-information "from" (sc-mail-fetch-field "from")))))))

(defun sc-field (field)
  "Return the alist information associated with the FIELD.
If FIELD is not a valid key, return sc-mumble-string."
  (or (aget sc-gal-information field) sc-mumble-string))


;; ======================================================================
;; built-in reference header rewrite functions

(defun sc-no-header ()
  "Does nothing. Use this instead of nil to get a blank header."
  ())

(defun sc-no-blank-line-or-header()
  "Similar to sc-no-header except it removes the preceeding blank line."
  (if (not (bobp))
      (if (and (eolp)
	       (progn (forward-line -1)
		      (or (looking-at mail-header-separator)
			  (and (eq major-mode 'mh-letter-mode)
			       (mh-in-header-p)))))
	  (progn (forward-line)
		 (let ((kill-lines-magic t)) (kill-line))))))

(defun sc-header-on-said ()
  "\"On <date>, <from> said:\", unless 1. the \"from\" field cannot be
found, in which case nothing is inserted; or 2. the \"date\" field is
missing in which case only the from part is printed."
  (let* ((sc-mumble-string "")
	 (whofrom (sc-field "from"))
	 (when (sc-field "date")))
    (if (not (string= whofrom ""))
	(insert sc-reference-tag-string
		(if (not (string= when ""))
		    (concat "On " when ", ") "")
		whofrom " said:\n"))))

(defun sc-header-inarticle-writes ()
  "\"In article <message-id>, <from> writes:\"
Treats \"message-id\" and \"from\" fields similar to sc-header-on-said."
  (let* ((sc-mumble-string "")
	 (whofrom (sc-field "from"))
	 (msgid (sc-field "message-id")))
    (if (not (string= whofrom ""))
	(insert sc-reference-tag-string
		(if (not (string= msgid ""))
		    (concat "In article " msgid ", ") "")
		whofrom " writes:\n"))))

(defun sc-header-regarding-adds ()
  "\"Regarding <subject>; <from> adds:\"
Treats \"subject\" and \"from\" fields similar to sc-header-on-said."
  (let* ((sc-mumble-string "")
	 (whofrom (sc-field "from"))
	 (subj (sc-field "subject")))
    (if (not (string= whofrom ""))
	(insert sc-reference-tag-string
		(if (not (string= subj ""))
		    (concat "Regarding " subj "; ") "")
		whofrom " adds:\n"))))

(defun sc-header-attributed-writes ()
  "\"<sc-attribution>\" == <sc-author> <address> writes:
Treats these fields in a similar manner to sc-header-on-said."
  (let* ((sc-mumble-string "")
	 (whofrom (sc-field "from"))
	 (reply (sc-field "sc-reply-address"))
	 (from (sc-field "sc-from-address"))
	 (attr (sc-field "sc-attribution"))
	 (auth (sc-field "sc-author")))
    (if (not (string= whofrom ""))
	(insert sc-reference-tag-string
		(if (not (string= attr ""))
		    (concat "\"" attr "\" == " ) "")
		(if (not (string= auth ""))
		    (concat auth " ") "")
		(if (not (string= reply ""))
		    (concat "<" reply ">")
		  (if (not (string= from ""))
		      (concat "<" from ">") ""))
		" writes:\n"))))

(defun sc-header-verbose ()
  "Very verbose, some say gross."
  (let* ((sc-mumble-string "")
	 (whofrom (sc-field "from"))
	 (reply (sc-field "sc-reply-address"))
	 (from (sc-field "sc-from-address"))
	 (author (sc-field "sc-author"))
	 (date (sc-field "date"))
	 (org (sc-field "organization"))
	 (msgid (sc-field "message-id"))
	 (ngrps (sc-field "newsgroups"))
	 (subj (sc-field "subject"))
	 (refs (sc-field "references"))
	 (cite (sc-field "sc-citation"))
	 (nl sc-reference-tag-string))
    (if (not (string= whofrom ""))
	(insert (if (not (string= date ""))
		    (concat nl "On " date ",\n") "")
		(concat nl (if (not (string= author ""))
			       author
			     whofrom) "\n")
		(if (not (string= org ""))
		    (concat nl "from the organization of " org "\n") "")
		(if (not (string= reply ""))
		    (concat nl "who can be reached at: " reply "\n")
		  (if (not (string= from ""))
		      (concat nl "who can be reached at: " from "\n") ""))
		(if (not (string= cite ""))
		    (concat nl "(whose comments are cited below with \""
			    cite "\"),\n") "")
		(if (not (string= msgid ""))
		    (concat nl "had this to say in article " msgid "\n") "")
		(if (not (string= ngrps ""))
		    (concat nl "in newsgroups " ngrps "\n") "")
		(if (not (string= subj ""))
		    (concat nl "concerning the subject of " subj "\n") "")
		(if (not (string= refs ""))
		    (concat nl "(see " refs " for more details)\n") "")
		))))


;; ======================================================================
;; this section queries the user for necessary information

(defun sc-query (&optional default)
  "Query for an attribution string with the optional DEFAULT choice.
Returns the string entered by the user, if non-empty and non-nil, or
DEFAULT otherwise. If DEFAULT is not supplied, sc-default-attribution
is used."
  (if (not default) (setq default sc-default-attribution))
  (let* ((prompt (concat "Enter attribution string: (default " default ") "))
	 (query (read-string prompt)))
    (if (or (null query)
	    (string= query ""))
	default
      query)))

(defun sc-confirm ()
  "Confirm the preferred attribution with the user."
  (if (or sc-confirm-always-p
	  sc-force-confirmation-p)
      (aput 'sc-gal-attributions
	    (let* ((default (aheadsym sc-gal-attributions))
		   chosen
		   (prompt (concat "Complete "
				   (cond
				    ((eq sc-cite-context 'citing) "cite")
				    ((eq sc-cite-context 'reciting) "recite")
				    (t ""))
				   " attribution string: (default "
				   default ") "))
		   (minibuffer-local-completion-map
		    (copy-keymap minibuffer-local-completion-map)))
	      (define-key minibuffer-local-completion-map "\C-g"
		'(lambda () (interactive) (beep) (throw 'select-abort nil)))
	      (setq chosen (completing-read prompt sc-gal-attributions))
	      (if (or (not chosen)
		      (string= chosen ""))
		  default
		chosen)))))


;; ======================================================================
;; this section contains primitive functions used in the email address
;; parsing schemes.  they extract name fields from various parts of
;; the "from:" field.

(defun sc-style1-addresses (from-string &optional delim)
  "Extract the author's email terminus from email address FROM-STRING.
Match addresses of the style \"name%[stuff].\" when called with DELIM
of \"%\" and addresses of the style \"[stuff]name@[stuff]\" when
called with DELIM \"@\". If DELIM is nil or not provided, matches
addresses of the style \"name\"."
  (and (string-match (concat "[a-zA-Z0-9_-]+" delim) from-string 0)
       (substring from-string
		  (match-beginning 0)
		  (- (match-end 0) (if (null delim) 0 1)))))

(defun sc-style2-addresses (from-string)
  "Extract the author's email terminus from email address FROM-STRING.
Match addresses of the style \"[stuff]![stuff]...!name[stuff].\""
  (let ((eos (length from-string))
	(mstart (string-match "![a-zA-Z0-9_-]+\\([^!a-zA-Z0-9_-]\\|$\\)"
			      from-string 0))
	(mend (match-end 0)))
    (and mstart
	 (substring from-string (1+ mstart) (- mend (if (= mend eos) 0 1)))
	 )))

(defun sc-get-address (from-string author)
  "Get the full email address path from FROM-STRING.
AUTHOR is the author's name (which is removed from the address)."
  (let ((eos (length from-string)))
    (if (string-match (concat "\\(^\\|^\"\\)" author
			      "\\(\\s +\\|\"\\s +\\)") from-string 0)
	(let ((addr (substring from-string (match-end 0) eos)))
	  (if (and (= (aref addr 0) ?<)
		   (= (aref addr (1- (length addr))) ?>))
	      (substring addr 1 (1- (length addr)))
	    addr))
      (if (string-match "[a-zA-Z0-9!@%._-]+" from-string 0)
	  (substring from-string (match-beginning 0) (match-end 0))
	"")
      )))

(defun sc-get-emailname (from-string)
  "Get the email terminus name from FROM-STRING."
  (cond
   ((sc-style1-addresses from-string "%"))
   ((sc-style1-addresses from-string "@"))
   ((sc-style2-addresses from-string))
   ((sc-style1-addresses from-string nil))
   (t (substring from-string 0 10))))


;; ======================================================================
;; this section contains functions that will extract a list of names
;; from the name field string.

(defun sc-spacify-name-chars (name)
  (let ((len (length name))
	(s 0))
    (while (< s len)
      (if (memq (aref name s) sc-spacify-name-chars)
	  (aset name s 32))
      (setq s (1+ s)))
    name))

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

(defun sc-extract-namestring (from-string)
  "Extract the name string from FROM-STRING.
This should be the author's full name minus an optional title."
  (let ((pstart (string-match "(.*)" from-string 0))
	(pend (match-end 0))
	(qstart (string-match "\".*\"" from-string 0))
	(qend (match-end 0))
	(bstart (string-match "\\([.a-zA-Z0-9_-]+\\s *\\)+" from-string 0))
	(bend (match-end 0)))
    (sc-spacify-name-chars
     (cond
      ((sc-name-substring from-string pstart pend 1))
      ((sc-name-substring from-string qstart qend 1))
      ((sc-name-substring from-string bstart bend 0))
      ))))

(defun sc-chop-namestring (namestring)
  "Convert NAMESTRING to a list of names.

     example: (sc-namestring-to-list \"John Xavier Doe\")
              => (\"John\" \"Xavier\" \"Doe\")"
  (if (not (string= namestring ""))
      (append (list (sc-string-car namestring))
	      (sc-chop-namestring (sc-string-cdr namestring)))))

(defun sc-strip-initials (namelist)
  "Extract the author's initials from the NAMELIST."
  (if (not namelist)
      nil
    (concat (if (string= (car namelist) "")
		""
	      (substring (car namelist) 0 1))
	    (sc-strip-initials (cdr namelist)))))


;; ======================================================================
;; this section handles selection of the attribution and citation strings

(defun sc-populate-alists (from-string)
  "Put important and useful information in the alists using FROM-STRING.
Return the list of name symbols."
  (let* ((namelist (sc-chop-namestring (sc-extract-namestring from-string)))
	 (revnames (reverse (cdr namelist)))
	 (midnames (reverse (cdr revnames)))
	 (firstname (car namelist))
	 (midnames (reverse (cdr revnames)))
	 (lastname (car revnames))
	 (initials (sc-strip-initials namelist))
	 (emailname (sc-get-emailname from-string))
	 (n 1)
	 (symlist (list 'emailname 'initials 'firstname 'lastname)))
    
    ;; put basic information
    (aput 'sc-gal-attributions 'firstname firstname)
    (aput 'sc-gal-attributions 'lastname lastname)
    (aput 'sc-gal-attributions 'emailname emailname)
    (aput 'sc-gal-attributions 'initials initials)
    
    (aput 'sc-gal-information "sc-firstname" firstname)
    (aput 'sc-gal-information "sc-lastname" lastname)
    (aput 'sc-gal-information "sc-emailname" emailname)
    (aput 'sc-gal-information "sc-initials" initials)
    
    ;; put middle names and build sc-author entry
    (let ((author (concat firstname " ")))
      (while midnames
	(let ((name (car midnames))
	      (next (cdr midnames))
	      (symbol (intern (format "middlename%d" n)))
	      (string (format "sc-middlename-%d" n)))
	  ;; first put new middlename
	  (aput 'sc-gal-attributions symbol name)
	  (aput 'sc-gal-information string name)
	  (setq n (1+ n))
	  (nconc symlist (list symbol))

	  ;; now build author name
	  (setq author (concat author name " "))

	  ;; incr loop
	  (setq midnames next)
	  ))
      (setq author (concat author lastname))

      ;; put author name and email address
      (aput 'sc-gal-information "sc-author" author)
      (aput 'sc-gal-information "sc-from-address"
	    (sc-get-address from-string author))
      (aput 'sc-gal-information "sc-reply-address"
	    (sc-get-address (sc-field "reply-to") author))
      )
    ;; return value
    symlist))

(defun sc-sort-attribution-alist ()
  "Put preferred attribution at head of attributions alist."
  (asort 'sc-gal-attributions sc-preferred-attribution)
  
  ;; use backup scheme if preference is not legal
  (if (or (null sc-preferred-attribution)
	  (anot-head-p sc-gal-attributions sc-preferred-attribution)
	  (let ((prefval (aget sc-gal-attributions
			       sc-preferred-attribution)))
	    (or (null prefval)
		(string= prefval ""))))
      ;; no legal attribution
      (if sc-use-only-preference-p
	  (aput 'sc-gal-attributions 'sc-user-query
		(sc-query sc-default-attribution))
	;; else use secondary scheme
	(asort 'sc-gal-attributions 'firstname))))

(defun sc-build-attribution-alist (from-string)
  "Extract attributions from FROM-STRING, applying preferences."
  (let ((symlist (sc-populate-alists from-string))
	(headval (progn (sc-sort-attribution-alist)
			(aget sc-gal-attributions
			      (aheadsym sc-gal-attributions) t))))
    
    ;; for each element in the symlist, remove the corresponding 
    ;; key-value pair in the alist, then insert just the value.
    (while symlist
      (let ((value (aget sc-gal-attributions (car symlist) t))
	    (next (cdr symlist)))
	(if (not (or (null value)
		     (string= value "")))
	    (aput 'sc-gal-attributions value))
	(adelete 'sc-gal-attributions (car symlist))
	(setq symlist next)))

    ;; add nicknames to the completion list
    (let ((gal sc-gal-attributions))
      (while gal
	(let ((nns sc-nicknames-alist)
	      (galname (car (car gal))))
	  (while nns
	    (if (string= galname (car (car nns)))
		(aput 'sc-gal-attributions (car (cdr (car nns)))))
	    (setq nns (cdr nns)))
	  (setq gal (cdr gal)))))

    ;; now reinsert the head (preferred) attribution unless it is nil,
    ;; this effectively just moves the head value to the front of the
    ;; list.
    (if headval
	(aput 'sc-gal-attributions headval))
    
    ;; check to be sure alist is not nil
    (if (null sc-gal-attributions)
	(aput 'sc-gal-attributions sc-default-attribution))))

(defun sc-select ()
  "Select an attribution and create a citation string."
  (cond
   (sc-nested-citation-p
    (sc-update-gal ""))
   ((null (aget sc-gal-information "from" t))
    (aput 'sc-gal-information "sc-author" sc-default-author-name)
    (sc-update-gal (sc-query sc-default-attribution)))
   ((null sc-gal-attributions)
    (sc-build-attribution-alist (aget sc-gal-information "from" t))
    (sc-confirm)
    (sc-update-gal (aheadsym sc-gal-attributions)))
   (t
    (sc-confirm)
    (sc-update-gal (aheadsym sc-gal-attributions))))
  t)


;; ======================================================================
;; region citing and unciting

(defun sc-cite-region (start end)
  "Cite a region delineated by START and END."
  (save-excursion
    ;; set real end-of-region
    (goto-char end)
    (forward-line 1)
    (set-mark (point))
    ;; goto real beginning-of-region
    (goto-char start)
    (beginning-of-line)
    (let ((fstart (point))
	  (fend   (point)))
      (while (< (point) (sc-mark))
	;; remove leading whitespace if desired 
	(and sc-fixup-whitespace-p
	     (fixup-whitespace))
	;; if end of line then perhaps autofill
	(cond ((eolp)
	       (or (= fstart fend)
		   (not sc-auto-fill-region-p)
		   (and sc-auto-fill-query-each-paragraph-p
			(not (y-or-n-p "Fill this paragraph? ")))
		   (save-excursion (set-mark fend)
				   (goto-char (/ (+ fstart fend 1) 2))
				   (run-hooks 'sc-fill-paragraph-hook)))
	       (setq fstart (point)
		     fend (point)))
	      ;; not end of line so perhap cite it
	      ((not (looking-at sc-cite-regexp))
	       (insert (aget sc-gal-information "sc-citation")))
	      (sc-nested-citation-p
	       (insert (aget sc-gal-information "sc-nested-citation"))))
	(setq fend (point))
	(forward-line 1))
      (and sc-auto-fill-query-each-paragraph-p
	   (message " "))
      )))

(defun sc-uncite-region (start end cite-regexp)
  "Uncite a previously cited region delineated by START and END.
CITE-REGEXP describes how a cited line of texts starts.  Unciting also
auto-fills paragraph if sc-auto-fill-region-p is non-nil."
  (save-excursion
    (set-mark end)
    (goto-char start)
    (beginning-of-line)
    (let ((fstart (point))
	  (fend (point)))
      (while (< (point) (sc-mark))
	;; if end of line, then perhaps autofill
	(cond ((eolp)
	       (or (= fstart fend)
		   (not sc-auto-fill-region-p)
		   (and sc-auto-fill-query-each-paragraph-p
			(not (y-or-n-p "Fill this paragraph? ")))
		   (save-excursion (set-mark fend)
				   (goto-char (/ (+ fstart fend 1) 2))
				   (run-hooks 'sc-fill-paragraph-hook)))
	       (setq fstart (point)
		     fend (point)))
	      ;; not end of line so perhaps uncite it
	      ((looking-at cite-regexp)
	       (save-excursion
		 (save-restriction
		   (narrow-to-region (sc-linepos 'bol) (sc-linepos))
		   (beginning-of-line)
		   (delete-region (point-min)
				  (progn (re-search-forward cite-regexp
							    (point-max)
							    t)
					 (match-end 0)))))))
	(setq fend (point))
	(forward-line 1)))))


;; ======================================================================
;; this section contains paragraph filling support

(defun sc-guess-fill-prefix (&optional literalp)
  "Guess the fill prefix used on the current line.
Use various heuristics to find the fill prefix. Search begins on first
non-blank line after point.

     1) If fill-prefix is already bound to the empty string, return
        nil.

     2) If fill-prefix is already bound, but not to the empty
        string, return the value of fill-prefix.

     3) If the current line starts with the last chosen citation
        string, then that string is returned.

     4) If the current line starts with a string matching the regular
        expression sc-cite-regexp, return the match.  Note that if
        optional LITERALP is provided and non-nil, then the *string*
        that matches the regexp is return.  Otherwise, if LITERALP is
        not provided or is nil, the *regexp* sc-cite-regexp is
        returned.

     5) If the current line starts with any number of characters,
        followed by the sc-citation-delimiter and then white space,
        that match is returned. See comment #4 above for handling of
        LITERALP. 

     6) Nil is returned."
  (save-excursion
    ;; scan for first non-blank line in the region
    (beginning-of-line)
    (skip-chars-forward "\n\t ")
    (beginning-of-line)
    (let ((citation (aget sc-gal-information "sc-citation"))
	  (generic-citation
	   (concat "\\s *[^ \t\n" sc-citation-delimiter "]+>\\s +")))
      (cond
       ((string= fill-prefix "") nil)                   ;; heuristic #1
       (fill-prefix)                                    ;; heuristic #2
       ((looking-at (regexp-quote citation)) citation)  ;; heuristic #3
       ((looking-at sc-cite-regexp)                     ;; heuristic #4
	(if literalp
	    (buffer-substring
	     (point)
	     (progn (re-search-forward (concat sc-cite-regexp "\\s *")
				       (point-max) nil)
		    (point)))
	  sc-cite-regexp))
       ((looking-at generic-citation)                   ;; heuristic #5
	(if literalp
	    (buffer-substring
	     (point)
	     (progn (re-search-forward generic-citation) (point)))
	  generic-citation))
       (t nil)))))                                      ;; heuristic #6

(defun sc-consistant-cite-p (prefix)
  "Check current paragraph for consistant citation.
Scans to paragraph delineated by (forward|backward)-paragraph to see
if all lines start with PREFIX. Returns t if entire paragraph is
consistantly cited, nil otherwise."
  (save-excursion
    (let ((end   (progn (forward-paragraph)
			(beginning-of-line)
			(or (not (eolp))
			    (forward-char -1))
			(point)))
	  (start (progn (backward-paragraph)
			(beginning-of-line)
			(or (not (eolp))
			    (forward-char 1))
			(point)))
	  (badline t))
      (goto-char start)
      (beginning-of-line)
      (while (and (< (point) end)
		  badline)
	(setq badline (looking-at prefix))
	(forward-line 1))
      badline)))

(defun sc-fill-start (fill-prefix)
  "Find buffer position of start of region which begins with FILL-PREFIX.
Restrict scan to current paragraph."
  (save-excursion
    (let ((badline nil)
	  (top (save-excursion
		 (backward-paragraph)
		 (beginning-of-line)
		 (or (not (eolp))
		     (forward-char 1))
		 (point))))
      (while (and (not badline)
		  (> (point) top))
	(forward-line -1)
	(setq badline (not (looking-at fill-prefix)))))
    (forward-line 1)
    (point)))

(defun sc-fill-end (fill-prefix)
  "Find the buffer position of end of region which begins with FILL-PREFIX.
Restrict scan to current paragraph."
  (save-excursion
    (let ((badline nil)
	  (bot (save-excursion
		 (forward-paragraph)
		 (beginning-of-line)
		 (or (not (eolp))
		     (forward-char -1))
		 (point))))
      (while (and (not badline)
		  (< (point) bot))
	(beginning-of-line)
	(setq badline (not (looking-at fill-prefix)))
	(forward-line 1)))
    (forward-line -1)
    (point)))

(defun sc-fill-paragraph ()
  "Supercite's paragraph fill function.
Fill the paragraph containing or following point. Use
sc-guess-fill-prefix to find the fill-prefix for the paragraph.

If the paragraph is inconsistantly cited (mixed fill-prefix), then the
user is queried to restrict the the fill to only those lines around
point which begin with the fill prefix.

The variable sc-fill-arg is passed to fill-paragraph and
fill-region-as-paragraph which controls justification of the
paragraph.  sc-fill-arg is set by sc-fill-paragraph-manually."
  (save-excursion
    (let ((pnt (point))
	  (fill-prefix (sc-guess-fill-prefix t)))
      (cond
       ((not fill-prefix)
	(fill-paragraph sc-fill-arg))
       ((sc-consistant-cite-p fill-prefix)
	(fill-paragraph sc-fill-arg))
       ((y-or-n-p "Inconsistent citation found. Restrict? ")
	(message "")
	(fill-region-as-paragraph (progn (goto-char pnt)
					 (sc-fill-start fill-prefix))
				  (progn (goto-char pnt)
					 (sc-fill-end fill-prefix))
				  sc-fill-arg))
       (t
	(message "")
	(progn
	  (setq fill-prefix (aget sc-gal-information "sc-citation"))
	  (fill-paragraph sc-fill-arg)))))))


;; ======================================================================
;; the following functions are the top level, interactive commands that
;; can be bound to key strokes

(defun sc-insert-reference (arg)
  "Insert, at point, a reference header in the body of the reply.
Numeric ARG indicates which header style from sc-rewrite-header-list
to use when rewriting the header. No supplied ARG indicates use of
sc-preferred-header-style.

With just \\[universal-argument], electric reference insert mode is
entered, regardless of the value of sc-electric-references-p.  See
sc-electric-mode for more information."
  (interactive "P")
  (if (consp arg)
      (sc-electric-mode)
    (let ((pref (cond ((sc-valid-index-p arg) arg)
		      ((sc-valid-index-p sc-preferred-header-style)
		       sc-preferred-header-style)
		      (t 0))))
      (if sc-electric-references-p (sc-electric-mode pref)
	(condition-case err
	    (eval (nth pref sc-rewrite-header-list))
	  (void-function
	   (progn (message
		   "Symbol's function definition is void: %s. (Header %d)."
		   (symbol-name (car (cdr err)))
		   pref)
		  (beep)))
	  (error
	   (progn (message "Error evaluating rewrite header function %d."
			   pref)
		  (beep)))
	  )))))

(defun sc-cite (arg)
  "Cite the region of text between point and mark.
Numeric ARG, if supplied, is passed unaltered to sc-insert-reference."
  (interactive "P")
  (if (not (sc-mark))
      (error "Please designate a region to cite (i.e. set the mark)."))
  (catch 'select-abort
    (let ((sc-cite-context 'citing)
	  (sc-force-confirmation-p (interactive-p)))
      (sc-select)
      (undo-boundary)
      (let ((xchange (if (> (sc-mark) (point)) nil
		       (exchange-point-and-mark)
		       t)))
	(sc-insert-reference arg)
	(sc-cite-region (point) (sc-mark))
	;; leave point on first cited line
	(while (and (< (point) (sc-mark))
		    (not (looking-at (aget sc-gal-information
					   (if sc-nested-citation-p
					       "sc-nested-citation"
					     "sc-citation")))))
	  (forward-line 1))
	(and xchange
	     (exchange-point-and-mark))
	))))

(defun sc-uncite ()
  "Uncite the region between point and mark."
  (interactive)
  (if (not (sc-mark))
      (error "Please designate a region to uncite (i.e. set the mark)."))
  (undo-boundary)
  (let ((xchange (if (> (sc-mark) (point)) nil
		   (exchange-point-and-mark)
		   t))
	(fp (or (sc-guess-fill-prefix)
		"")))
    (sc-uncite-region (point) (sc-mark) fp)
    (and xchange
	 (exchange-point-and-mark))))

(defun sc-recite ()
  "Recite the region by first unciting then citing the text."
  (interactive)
  (if (not (sc-mark))
      (error "Please designate a region to recite (i.e. set the mark)."))
  (catch 'select-abort
    (let ((sc-cite-context 'reciting)
	  (sc-force-confirmation-p t))
      (sc-select)
      (undo-boundary)
      (let ((xchange (if (> (sc-mark) (point)) nil
		       (exchange-point-and-mark)
		       t))
	    (fp (or (sc-guess-fill-prefix)
		    "")))
	(sc-uncite-region (point) (sc-mark) fp)
	(sc-cite-region (point) (sc-mark))
	(and xchange
	     (exchange-point-and-mark))
	))))

(defun sc-insert-citation ()
  "Insert citation string at beginning of current line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (insert (aget sc-gal-information "sc-citation"))))

(defun sc-open-line (arg)
  "Insert a newline and leave point before it.
Also inserts the guessed prefix at the beginning of the new line. With
numeric ARG, inserts that many new lines."
  (interactive "p")
  (save-excursion
    (let ((start (point))
	  (string (or (sc-guess-fill-prefix t)
		      "")))
      (open-line arg)
      (goto-char start)
      (forward-line 1)
      (while (< 0 arg)
	(insert string)
	(forward-line 1)
	(setq arg (- arg 1))))))

(defun sc-fill-paragraph-manually (arg)
  "Fill current cited paragraph.
Really just runs the hook sc-fill-paragraph-hook, however it does set
the global variable sc-fill-arg to the value of ARG.  This is
currently the only way to pass an argument to a hookified function."
  (interactive "P")
  (setq sc-fill-arg arg)
  (run-hooks 'sc-fill-paragraph-hook))

(defun sc-modify-information (arg)
  "Interactively modify information in the information alist.
\\[universal-argument] if supplied, deletes the entry from the alist.
You can add an entry by supplying a key instead of completing."
  (interactive "P")
  (let* ((delete-p   (consp arg))
	 (action     (if delete-p "delete" "modify"))
	 (defaultkey (aheadsym sc-gal-information))
	 (prompt     (concat "Select information key to "
			     action ": (default "
			     defaultkey ") "))
	 (key (completing-read prompt sc-gal-information))
	 )
    (if (or (string= key "")
	    (null key))
	(setq key defaultkey))
    (if delete-p (adelete 'sc-gal-information key)
      (let* ((oldval (aget sc-gal-information key t))
	     (prompt (concat "Enter new value for key \""
			     key "\" (default \"" oldval "\") "))
	     (newval (read-input prompt)))
	(if (or (string= newval "")
		(null newval))
	    nil
	  (aput 'sc-gal-information key newval)
	  )))))

(defun sc-view-field (arg)
  "View field values in the information alist.
This is essentially an interactive version of sc-field, and is similar
to sc-modify-information, except that the field values can't be
modified. With \\[universal-argument], if supplied, inserts the value
into the current buffer as well."
  (interactive "P")
  (let* ((defaultkey (aheadsym sc-gal-information))
	 (prompt     (concat "View information key: (default "
			     defaultkey ") "))
	 (key (completing-read prompt sc-gal-information)))
    (if (or (string= key "")
	    (null key))
	(setq key defaultkey))
    (let* ((val (aget sc-gal-information key t))
	   (pval (if val (concat "\"" val "\"") "nil")))
      (message "value of key %s: %s" key pval)
      (if (and key (consp arg)) (insert val)))))

(defun sc-glom-headers ()
  "Glom information from mail headers in region between point and mark.
Any old information is lost, unless an error occurs."
  (interactive)
  (let ((attr (copy-sequence sc-gal-attributions))
	(info (copy-sequence sc-gal-information)))
    (setq sc-gal-attributions nil
	  sc-gal-information nil)
    (let ((start (region-beginning))
	  (end   (region-end))
	  (sc-force-confirmation-p t)
	  (sc-cite-context nil))
      (sc-fetch-fields start end)
      (if (null sc-gal-information)
	  (progn
	    (message "No mail headers found! Restoring old information.")
	    (setq sc-gal-attributions attr
		  sc-gal-information info))
	(sc-mail-yank-clear-headers start end)
	(if (not (catch 'select-abort
		   (condition-case foo
		       (sc-select)
		     (quit (beep) (throw 'select-abort nil)))
		   ))
	    (setq sc-gal-attributions attr
		  sc-gal-information info))
	))))

(defun sc-version (arg)
  "Show supercite version.
Universal argument (\\[universal-argument]) ARG inserts version
information in the current buffer instead of printing the message in
the echo area."
  (interactive "P")
  (if (consp arg)
      (insert "Using Supercite version " sc-version-number)
    (message "Using Supercite version %s" sc-version-number)))


;; ======================================================================
;; leach onto current mode

(defun sc-append-current-keymap ()
  "Append some useful key bindings to the current local key map.
This searches sc-local-keymap for the keymap to install based on the
major-mode of the current buffer."
  (let ((hook (car (cdr (assq major-mode sc-local-keymaps)))))
    (cond
     ((not hook)
      (run-hooks 'sc-default-keymap))
     ((not (listp hook))
      (setq hook (car (cdr (assq hook sc-local-keymaps))))
      (run-hooks 'hook))
     (t
      (run-hooks 'hook))))
  (setq sc-leached-keymap (current-local-map)))

(defun sc-snag-all-keybindings ()
  "Snag all keybindings in major-mode's current keymap."
  (let* ((curkeymap (current-local-map))
	 (symregexp ".*sc-.*\n")
	 (docstring (substitute-command-keys "\\{curkeymap}"))
	 (start 0)
	 (maxend (length docstring))
	 (spooge ""))
    (while (and (< start maxend)
		(string-match symregexp docstring start))
      (setq spooge (concat spooge (substring docstring
					     (match-beginning 0)
					     (match-end 0))))
      (setq start (match-end 0)))
    spooge))

(defun sc-spoogify-docstring ()
  "Modifies (makes into spooge) the docstring for the current major mode.
This will leach the keybinding descriptions for supercite onto the end
of the current major mode's docstring.  If major mode is preloaded,
this function will first make a copy of the list associated with the
mode, then modify this copy."
  (let* ((symfunc (symbol-function major-mode))
         (doc-cdr (and (listp symfunc) (nthcdr 2 symfunc)))
	 (doc-str (documentation major-mode)))
    (cond
     ;; is a docstring even provided?
     ((not (stringp doc-str)))
     ;; have we already leached on?
     ((string-match "Supercite" doc-str))
     ;; lets build the new doc string
     (t
      (let* ((described (sc-snag-all-keybindings))
	     (commonstr "

The major mode for this buffer has been modified to include the
Supercite 2.3 package for handling attributions and citations of
original messages in email replies.  For more information on this
package, type \"\\[sc-describe]\".")
	    (newdoc-str
	     (concat doc-str commonstr
		     (if (not (string= described ""))
			 (concat "\n\nThe following keys are bound "
				 "to Supercite commands:\n\n"
				 described)))
	     ))
        (cond
         (doc-cdr
          (condition-case nil
              (setcar doc-cdr newdoc-str)
            (error
             ;; the major mode must be preloaded, make a copy first
             (setq symfunc (copy-sequence (symbol-function major-mode))
                   doc-cdr (nthcdr 2 symfunc))
             (setcar doc-cdr newdoc-str)
             (fset major-mode symfunc))))
         ;; lemacs 19 byte-code.
         ;; Set function to a new byte-code vector with the
         ;; new documentation in the documentation slot (element 4).
         ;; We can't use aset because aset won't allow you to modify
         ;; a byte-code vector.
         ;; Include element 5 if the vector has one.
         (t
          (fset major-mode
                (apply 'make-byte-code
                       (aref symfunc 0) (aref symfunc 1)
                       (aref symfunc 2) (aref symfunc 3)
                       newdoc-str
                       (if (> (length symfunc) 5)
                           (list (aref symfunc 5)))))
	  )))))))


;; ======================================================================
;; this section contains default hooks and hook support for execution

(defun sc-cite-original ()
  "Hook version of sc-cite.
This is callable from the various mail and news readers' reply
function according to the agreed upon standard. See \\[sc-describe]
for more details.  Sc-cite-original does not do any yanking of the
original message but it does require a few things:

     1) The reply buffer is the current buffer.

     2) The original message has been yanked and inserted into the
        reply buffer.

     3) Verbose mail headers from the original message have been
        inserted into the reply buffer directly before the text of the
        original message.

     4) Point is at the beginning of the verbose headers.

     5) Mark is at the end of the body of text to be cited."
  (run-hooks 'sc-pre-hook)
  (setq sc-gal-attributions nil)
  (setq sc-gal-information nil)
  (let ((start (region-beginning))
	(end   (region-end)))
    (sc-fetch-fields start end)
    (sc-mail-yank-clear-headers start end)
    (if (not sc-all-but-cite-p)
	(sc-cite sc-preferred-header-style))
    (sc-append-current-keymap)
    (sc-spoogify-docstring)
    (run-hooks 'sc-post-hook)))


;; ======================================================================
;; describe this package
;;
(defun sc-describe ()
  "Supercite version 2.3 is now described in a texinfo manual which
makes the documenation available both for online perusal via emacs'
info system, or for hard-copy printing using the TeX facility.

To view the online document hit \\[info], then \"mSupercite <RET>\"."
  (interactive)
  (describe-function 'sc-describe))

;; ======================================================================
;; load hook
(run-hooks 'sc-load-hook)
(provide 'sc)
