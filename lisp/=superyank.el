;;; superyank.el --- smart message-yanking code for GNUS (Version 1.1)

;; Inserts the message being replied to with various user controlled
;; citation styles.
;;

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; this file, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;; NAME: Barry A. Warsaw                USMAIL: National Institute of Standards
;; TELE: (301) 975-3460                         and Technology (formerly NBS)
;; UUCP: {...}!uunet!cme-durer!warsaw           Rm. B-124, Bldg. 220
;; ARPA: warsaw@cme.nist.gov                    Gaithersburg, MD 20899

;; Modification history:
;;
;; modified: 14-Jun-1989 baw (better keymap set procedure, rewrite-headers)
;; modified: 12-Jun-1989 baw (added defvar for sy-use-only-preference-p)
;; modified:  6-Jun-1989 baw (better sy-rewrite-headers, no kill/yank)
;; modified:  5-Jun-1989 baw (requires rnewspost.el)
;; modified:  1-Jun-1989 baw (persistent attribution, sy-open-line)
;; modified: 31-May-1989 baw (fixed some gnus problems, id'd another)
;; modified: 22-May-1989 baw (documentation)
;; modified:  8-May-1989 baw (auto filling of regions)
;; modified:  1-May-1989 baw (documentation)
;; modified: 27-Apr-1989 baw (new preference scheme)
;; modified: 24-Apr-1989 baw (remove gnus headers, attrib scheme, cite lines)
;; modified: 19-Apr-1989 baw (cite key, fill p, yank region, naming scheme)
;; modified: 12-Apr-1989 baw (incorp other mail yank features seen on net)
;; created : 16-Feb-1989 baw (mod vanilla fn indent-rigidly mail-yank-original)

;; Though I wrote this package basically from scratch, as an elisp
;; learning exercise, it was inspired by postings of similar packages to
;; the gnu.emacs newsgroup over the past month or so.
;;
;; Here's a brief history of how this package developed:
;;
;; I as well as others on the net were pretty unhappy about the way emacs
;; cited replies with the tab or 4 spaces.  It looked ugly and made it hard
;; to distinguish between original and cited lines.  I hacked on the function
;; yank-original to at least give the user the ability to define the citation
;; character.  I posted this simple hack, and others did as well.  The main
;; difference between mine and others was that a space was put after the
;; citation string on on new citations, but not after previously cited lines:
;;
;; >> John wrote this originally
;; > Jane replied to that
;;
;; Then Martin Neitzel posted some code that he developed, derived in part
;; from code that Ashwin Ram posted previous to that.  In Martin's
;; posting, he introduced a new, and (IMHO) superior, citation style,
;; eliminating nested citations.  Yes, I wanted to join the Small-But-
;; Growing-Help-Stamp-Out-Nested-Citation-Movement! You should too.
;;
;; But Martin's code simply asks the user for the citation string (here
;; after called the `attribution' string), and I got to thinking, it wouldn't
;; be that difficult to automate that part.  So I started hacking this out.
;; It proved to be not as simple as I first thought.  But anyway here it
;; is.  See the wish list below for future plans (if I have time).
;;
;; Type "C-h f mail-yank-original" after this package is loaded to get a
;; description of what it does and the variables that control it.
;;
;; ======================================================================
;;
;; Changes wish list
;;
;; 1) C-x C-s yanks a region from the RMAIL buffer instead of the
;;    whole buffer
;;
;; 2) reparse nested citations to try to recast as non-nested citations
;;    perhaps by checking the References: line
;;
;; ======================================================================
;;
;; require and provide features
;;
(require 'sendmail)
;;
;; ======================================================================
;;
;; don't need rnewspost.el to rewrite the header.  This only works
;; with diffs to rnewspost.el that I posted with the original
;; superyank code.
;;
(setq news-reply-header-hook nil)

;; **********************************************************************
;; start of user defined variables
;; **********************************************************************
;;
;; this section defines variables that control the operation of
;; super-mail-yank.  Most of these are described in the comment section
;; as well as the DOCSTRING.
;;

;;
;; ----------------------------------------------------------------------
;;
;; this variable holds the default author's name for citations
;;
(defvar sy-default-attribution "Anon"
  "String that describes attribution to unknown person. This string
should not contain the citation string.")

;;
;; ----------------------------------------------------------------------
;;
;; string used as an end delimiter for both nested and non-nested citations
;;
(defvar sy-citation-string ">"
  "String to use as an end-delimiter for citations.  This string is
used in both nested and non-nested citations.  For best results, use a
single character with no trailing space.  Most commonly used string
is: \">\.")

;;
;; ----------------------------------------------------------------------
;;
;; variable controlling citation type, nested or non-nested
;;
(defvar sy-nested-citation-p nil
  "Non-nil uses nested citations, nil uses non-nested citations.
Nested citations are of the style:

I wrote this
> He wrote this
>> She replied to something he wrote

Non-nested citations are of the style:

I wrote this
John> He wrote this
Jane> She originally wrote this")


;;
;; ----------------------------------------------------------------------
;;
;; regular expression that matches existing citations
;;
(defvar sy-cite-regexp "[a-zA-Z0-9]*>"
  "Regular expression that describes how an already cited line in an
article begins.  The regexp is only used at the beginning of a line,
so it doesn't need to begin with a '^'.")

;;
;; ----------------------------------------------------------------------
;;
;; regular expression that delimits names from titles in the field that
;; looks like:   (John X. Doe -- Computer Hacker Extraordinaire)
;;
(defvar sy-titlecue-regexp "\\s +-+\\s +"
  
  "Regular expression that delineates names from titles in the name
field.  Often, people will set up their name field to look like this:

(John Xavier Doe -- Computer Hacker Extraordinaire)

Set to nil to treat entire field as a name.")

;;
;; ----------------------------------------------------------------------
;;
;;
(defvar sy-preferred-attribution 2
  
  "This is an integer indicating what the user's preference is in
attribution style, based on the following key:

0: email address name is preferred
1: initials are preferred
2: first name is preferred
3: last name is preferred

The value of this variable may also be greater than 3, which would
allow you to prefer the 2nd through nth - 1 name.  If the preferred
attribution is nil or the empty string, then the secondary preferrence
will be the first name.  After that, the entire name alist is search
until a non-empty, non-nil name is found.  If no such name is found,
then the user is either queried or the default attribution string is
used depending on the value of sy-confirm-always-p.

Examples:

assume the from: line looks like this:

from: doe@computer.some.where.com (John Xavier Doe)

The following preferences would return these strings:

0: \"doe\"
1: \"JXD\"
2: \"John\"
3: \"Doe\"
4: \"Xavier\"

anything else would return \"John\".")

;;
;; ----------------------------------------------------------------------
;;
(defvar sy-confirm-always-p t
  "If t, always confirm attribution string before inserting into
buffer.")


;;
;; ----------------------------------------------------------------------
;;
;; informative header hook
;;
(defvar sy-rewrite-header-hook 'sy-header-on-said
  "Hook for inserting informative header at the top of the yanked
message. Set to nil for no header.  Here is a list of predefined
header styles; you can use these as a model to write you own:

sy-header-on-said [default]:   On 14-Jun-1989 GMT,
                               John Xavier Doe said:

sy-header-inarticle-writes:    In article <123456789> John Xavier Doe writes:

sy-header-regarding-writes:    Regarding RE: superyank; John Xavier Doe adds:

sy-header-verbose:             On 14-Jun-1989 GMT, John Xavier Doe
                               from the organization Great Company
                               has this to say about article <123456789>
                               in newsgroups misc.misc
                               concerning RE: superyank
                               referring to previous articles <987654321>

You can use the following variables as information strings in your header:

sy-reply-yank-date:         the date field              [ex: 14-Jun-1989 GMT]
sy-reply-yank-from:         the from field              [ex: John Xavier Doe]
sy-reply-yank-message-id:   the message id              [ex: <123456789>]
sy-reply-yank-subject:      the subject line            [ex: RE: superyank]
sy-reply-yank-newsgroup:    the newsgroup name for GNUS [ex: misc.misc]
sy-reply-yank-references:   the article references      [ex: <987654321>]
sy-reply-yank-organization: the author's organization   [ex: Great Company]

If a field can't be found, because it doesn't exist or is not being
shown, perhaps because of toggle-headers, the corresponding field
variable will contain the string \"mumble mumble\".")

;;
;; ----------------------------------------------------------------------
;;
;; non-nil means downcase the author's name string
;;
(defvar sy-downcase-p nil
  "Non-nil means downcase the author's name string.")

;;
;; ----------------------------------------------------------------------
;;
;; controls removal of leading white spaces
;;
(defvar sy-left-justify-p nil
  "If non-nil, delete all leading white space before citing.")

;;
;; ----------------------------------------------------------------------
;;
;; controls auto filling of region
;;
(defvar sy-auto-fill-region-p nil
  "If non-nil, automatically fill each paragraph that is cited.  If
nil, do not auto fill each paragraph.")


;;
;; ----------------------------------------------------------------------
;;
;; controls use of preferred attribution only, or use of attribution search
;; scheme if the preferred attrib can't be found.
;;
(defvar sy-use-only-preference-p nil
  
  "If non-nil, then only the preferred attribution string will be
used.  If the preferred attribution string can not be found, then the
sy-default-attribution will be used.  If nil, and the preferred
attribution string is not found, then some secondary scheme will be
employed to find a suitable attribution string.")

;; **********************************************************************
;; end of user defined variables
;; **********************************************************************

;;
;; ----------------------------------------------------------------------
;;
;; The new citation style means we can clean out other headers in addition
;; to those previously cleaned out.  Anyway, we create our own headers.
;; Also, we want to clean out any headers that gnus puts in.  Add to this
;; for other mail or news readers you may be using.
;;
(setq mail-yank-ignored-headers "^via:\\|^origin:\\|^status:\\|^re\\(mail\\|ceiv\\)ed\\|^[a-z-]*message-id:\\|^\\(summary-\\)?line[s]?:\\|^cc:\\|^subject:\\|^\\(\\(in-\\)?reply-\\)?to:\\|^\\(\\(return\\|reply\\)-\\)?path:\\|^\\(posted-\\)?date:\\|^\\(mail-\\)?from:\\|^newsgroup[s]?:\\|^organization:\\|^keywords:\\|^distribution:\\|^references:")

;;
;; ----------------------------------------------------------------------
;;
;; global variables, not user accessable
;;
(setq sy-persist-attribution (concat sy-default-attribution "> "))
(setq sy-reply-yank-date "")
(setq sy-reply-yank-from "")
(setq sy-reply-yank-message-id "")
(setq sy-reply-yank-subject "")
(setq sy-reply-yank-newsgroups "")
(setq sy-reply-yank-references "")
(setq sy-reply-yank-organization "")

;;
;; ======================================================================
;;
;; This section contains primitive functions used in the schemes.  They
;; extract name fields from various parts of the "from:" field based on
;; the control variables described above.
;;
;; Some will use recursion to pick out the correct namefield in the namestring
;; or the list of initials.  These functions all scan a string that contains
;; the name, ie:  "John Xavier Doe".  There is no limit on the number of names
;; in the string.  Also note that all white spaces are basically ignored and
;; are stripped from the returned strings, and titles are ignored if 
;; sy-titlecue-regexp is set to non-nil.
;;
;; Others will use methods to try to extract the name from the email
;; address of the originator.  The types of addresses readable are
;; described above.

;;
;; ----------------------------------------------------------------------
;;
;; try to extract the name from an email address of the form
;; name%[stuff]
;;
;; Unlike the get-name functions above, these functions operate on the
;; buffer instead of a supplied name-string.
;;
(defun sy-%-style-address ()
  (beginning-of-line)
  (buffer-substring
   (progn (re-search-forward "%" (point-max) t)
	  (if (not (bolp)) (forward-char -1))
	  (point))
   (progn (re-search-backward "^\\|[^a-zA-Z0-9]")
	  (point))))

;;
;; ----------------------------------------------------------------------
;;
;; try to extract names from addresses with the form:
;; [stuff]name@[stuff]
;;
(defun sy-@-style-address ()
  (beginning-of-line)
  (buffer-substring
   (progn (re-search-forward "@" (point-max) t)
	  (if (not (bolp)) (forward-char -1))
	  (point))
   (progn (re-search-backward "^\\|[^a-zA-Z0-0]")
	  (if (not (bolp)) (forward-char 1))
	  (point))))

;;
;; ----------------------------------------------------------------------
;;
;; try to extract the name from addresses with the form:
;; [stuff]![stuff]...!name[stuff]
;;
(defun sy-!-style-address ()
  (beginning-of-line)
  (buffer-substring
   (progn (while (re-search-forward "!" (point-max) t))
	  (point))
   (progn (re-search-forward "[^a-zA-Z0-9]\\|$")
	  (if (not (eolp)) (forward-char -1))
	  (point))))

;;
;; ----------------------------------------------------------------------
;;
;; using the different email name schemes, try each one until you get a
;; non-nil entry
;;
(defun sy-get-emailname ()
  (let ((en1 (sy-%-style-address))
	(en2 (sy-@-style-address))
	(en3 (sy-!-style-address)))
    (cond
     ((not (string-equal en1 "")) en1)
     ((not (string-equal en2 "")) en2)
     ((not (string-equal en3 "")) en3)
     (t ""))))

;;
;; ----------------------------------------------------------------------
;;
;; returns the "car" of the namestring, really the first namefield
;;
;; (sy-string-car "John Xavier Doe")
;; => "John"
;;
(defun sy-string-car (namestring)
  (substring namestring
	     (progn (string-match "\\s *" namestring) (match-end 0))
	     (progn (string-match "\\s *\\S +" namestring) (match-end 0))))

;;
;; ----------------------------------------------------------------------
;;
;; returns the "cdr" of the namestring, really the whole string from
;; after the first name field to the end of the string.
;;
;; (sy-string-cdr "John Xavier Doe")
;; => "Xavier Doe"
;;
(defun sy-string-cdr (namestring)
  (substring namestring
	     (progn (string-match "\\s *\\S +\\s *" namestring)
		    (match-end 0))))

;;
;; ----------------------------------------------------------------------
;;
;; convert a namestring to a list of namefields
;;
;; (sy-namestring-to-list "John Xavier Doe")
;; => ("John" "Xavier" "Doe")
;;
(defun sy-namestring-to-list (namestring)
  (if (not (string-match namestring ""))
      (append (list (sy-string-car namestring))
	      (sy-namestring-to-list (sy-string-cdr namestring)))))

;;
;; ----------------------------------------------------------------------
;;
;; strip the initials from each item in the list and return a string
;; that is the concatenation of the initials
;;
(defun sy-strip-initials (raw-nlist)
  (if (not raw-nlist)
      nil
    (concat (substring (car raw-nlist) 0 1)
	    (sy-strip-initials (cdr raw-nlist)))))


;;
;; ----------------------------------------------------------------------
;;
;; using the namestring, build a list which is in the following order
;;
;; (email, initials, firstname, lastname, name1, name2, name3 ... nameN-1)
;;
(defun sy-build-ordered-namelist (namestring)
  (let* ((raw-nlist (sy-namestring-to-list namestring))
	 (initials (sy-strip-initials raw-nlist))
	 (firstname (car raw-nlist))
	 (revnames (reverse (cdr raw-nlist)))
	 (lastname (car revnames))
	 (midnames (reverse (cdr revnames)))
	 (emailnames (sy-get-emailname)))
    (append (list emailnames)
	    (list initials)
	    (list firstname)
	    (list lastname)
	    midnames)))

;;
;; ----------------------------------------------------------------------
;;
;; Query the user for the attribution string.  Supply sy-default-attribution
;; as the default choice.
;;
(defun sy-query-for-attribution ()
  (concat 
   (let* ((prompt (concat "Enter attribution string: (default "
			  sy-default-attribution
			  ") "))
	  (query (read-input prompt))
	  (attribution (if (string-equal query "")
			   sy-default-attribution
			 query)))
     (if sy-downcase-p
	 (downcase attribution)
       attribution))
   sy-citation-string))


;;
;; ----------------------------------------------------------------------
;;
;; parse the current line for the namestring
;;
(defun sy-get-namestring ()
  (save-restriction
    (beginning-of-line)
    (if (re-search-forward "(.*)" (point-max) t)
	(let ((start (progn
		       (beginning-of-line)
		       (re-search-forward "\\((\\s *\\)\\|$" (point-max) t)
		       (point)))
	      (end (progn
		     (re-search-forward
		      (concat "\\(\\s *\\()\\|" sy-titlecue-regexp "\\)\\)\\|$")
		      (point-max) t)
		     (point))))
	  (narrow-to-region start end)
	  (let ((start (progn
			 (beginning-of-line)
			 (point)))
		(end (progn
		       (end-of-line)
		       (re-search-backward
			(concat "\\s *\\()\\|" sy-titlecue-regexp "\\)$")
			(point-min) t)
		       (point))))
	    (buffer-substring start end)))
      (let ((start (progn
		     (beginning-of-line)
		     (re-search-forward "^\"*")
		     (point)))
	    (end (progn
		   (re-search-forward "\\(\\s *[a-zA-Z0-9\\.]+\\)*"
				      (point-max) t)
		   (point))))
	(buffer-substring start end)))))


;;
;; ----------------------------------------------------------------------
;;
;; scan the nlist and return the integer pointing to the first legal
;; non-empty namestring.  Returns the integer pointing to the index
;; in the nlist of the preferred namestring, or nil if no legal
;; non-empty namestring could be found.
;;
(defun sy-return-preference-n (nlist)
  (let ((p sy-preferred-attribution)
	(exception nil))
    ;;
    ;; check to be sure the index is not out-of-bounds
    ;;
    (cond
     ((< p 0) (setq p 2) (setq exception t))
     ((not (nth p nlist)) (setq p 2) (setq exception t)))
    ;;
    ;; check to be sure that the explicit preference is not empty
    ;;
    (if (string-equal (nth p nlist) "")
	(progn (setq p 0)
	       (setq exception t)))
    ;;
    ;; find the first non-empty namestring
    ;;
    (while (and (nth p nlist)
		(string-equal (nth p nlist) ""))
      (setq exception t)
      (setq p (+ p 1)))
    ;;
    ;; return the preference index if non-nil, otherwise nil
    ;;
    (if (or (and exception sy-use-only-preference-p)
	    (not (nth p nlist)))
	nil
      p)))

;;
;;
;; ----------------------------------------------------------------------
;;
;; rebuild the nlist into an alist for completing-read.  Use as a guide
;; the index of the preferred name field.  Get the actual preferred
;; name field base on other factors (see above).  If no actual preferred
;; name field is found, then query the user for the attribution string.
;;
;; also note that the nlist is guaranteed to be non-empty.  At the very
;; least it will consist of 4 empty strings ("" "" "" "")
;; 
(defun sy-nlist-to-alist (nlist)
  (let ((preference (sy-return-preference-n nlist))
	alist
	(n 0))
    ;;
    ;; check to be sure preference is not nil
    ;;
    (if (not preference)
	(setq alist (list (cons (sy-query-for-attribution) nil)))
      ;;
      ;; preference is non-nil
      ;;
      (setq alist (list (cons (nth preference nlist) nil)))
      (while (nth n nlist)
	(if (= n preference) nil
	  (setq alist (append alist (list (cons (nth n nlist) nil)))))
	(setq n (+ n 1))))
    alist))



;;
;; ----------------------------------------------------------------------
;;
;; confirm if desired after the alist has been built
;;
(defun sy-get-attribution (alist)
  (concat
   ;;
   ;; check to see if nested citations are to be used
   ;;
   (if sy-nested-citation-p
       ""
     ;;
     ;; check to see if confirmation is needed
     ;; if not, just return the preference (first element in alist)
     ;;
     (if (not sy-confirm-always-p)
	 (car (car alist))
       ;;
       ;; confirmation is requested so build the prompt, confirm
       ;; and return the chosen string
       ;;
       (let* (ignore
	      (prompt (concat "Complete attribution string: (default "
			      (car (car alist))
			      ") "))
	      ;;
	      ;; set up the local completion keymap
	      ;;
	      (minibuffer-local-must-match-map
	       (let ((map (make-sparse-keymap)))
		 (define-key map "?"    'minibuffer-completion-help)
		 (define-key map " "    'minibuffer-complete-word)
		 (define-key map "\t"   'minibuffer-complete)
		 (define-key map "\00A" 'exit-minibuffer)
		 (define-key map "\00D" 'exit-minibuffer)
		 (define-key map "\007"
		   '(lambda ()
		      (interactive)
		      (beep)
		      (exit-minibuffer)))
		 map))
	      ;;
	      ;; read the completion
	      ;;
	      (attribution (completing-read prompt alist))
	      ;;
	      ;; check attribution string for emptyness
	      ;;
	      (choice (if (or (not attribution)
			      (string-equal attribution ""))
			  (car (car alist))
			attribution)))
	 
	 (if sy-downcase-p
	     (downcase choice)
	   choice))))
   sy-citation-string))


;;
;; ----------------------------------------------------------------------
;;
;; this function will scan the current rmail buffer, narrowing it to the
;; from: line, then using this, it will try to decipher some names from
;; that line.  It will then build the name alist and try to confirm
;; its choice of attribution strings.  It returns the chosen attribution
;; string.
;;
(defun sy-scan-rmail-for-names (rmailbuffer)
  (save-excursion
    (let ((case-fold-search t)
	  alist
	  attribution)
      (switch-to-buffer rmailbuffer)
      (goto-char (point-min))
      ;;
      ;; be sure there is a from: line
      ;;
      (if (not (re-search-forward "^from:\\s *" (point-max) t))
	  (setq attribution (sy-query-for-attribution))
	;;
	;; if there is a from: line, then scan the narrow the buffer,
	;; grab the namestring, and build the alist, then using this
	;; get the attribution string.
	;;
	(save-restriction
	  (narrow-to-region (point)
			    (progn (end-of-line) (point)))
	  (let* ((namestring (sy-get-namestring))
		 (nlist (sy-build-ordered-namelist namestring)))
	    (setq alist (sy-nlist-to-alist nlist))))
	;;
	;; we've built the alist, now confirm the attribution choice
	;; if appropriate
	;;
	(setq attribution (sy-get-attribution alist)))
      attribution)))


;;
;; ======================================================================
;;
;; the following function insert of citations, writing of headers, filling
;; paragraphs and general higher level operations
;;

;;
;; ----------------------------------------------------------------------
;;
;; insert a nested citation
;;
(defun sy-insert-citation (start end cite-string)
  (save-excursion
    (goto-char end)
    (setq end (point-marker))
    (goto-char start)
    (or (bolp)
	(forward-line 1))
    
    (let ((fill-prefix (concat cite-string " "))
	  (fstart (point))
	  (fend (point)))
      
      (while (< (point) end)
	;;
	;; remove leading tabs if desired
	;;
	(if sy-left-justify-p
	    (delete-region (point)
			   (progn (skip-chars-forward " \t") (point))))
	;;
	;; check to see if the current line should be cited
	;;
	(if (or (eolp)
		(looking-at sy-cite-regexp))
	    ;;
	    ;; do not cite this line unless nested-citations are to be 
	    ;; used
	    ;;
	    (progn
	      (or (eolp)
		  (if sy-nested-citation-p
		      (insert cite-string)))
	      
	      ;; set fill start and end points
	      ;;
	      (or (= fstart  fend)
		  (not sy-auto-fill-region-p)
		  (progn (goto-char fend)
			 (or (not (eolp))
			     (setq fend (+ fend 1)))
			 (fill-region-as-paragraph fstart fend)))
	      (setq fstart (point))
	      (setq fend (point)))
	  
	  ;; else
	  ;;
	  (insert fill-prefix)
	  (end-of-line)
	  (setq fend (point)))
	
	(forward-line 1)))
    (move-marker end nil)))

;;
;; ----------------------------------------------------------------------
;;
;; yank a particular field into a holding variable
;;
(defun sy-yank-fields (start)
  (save-excursion
    (goto-char start)
    (setq sy-reply-yank-date         (mail-fetch-field "date")
	  sy-reply-yank-from         (mail-fetch-field "from")
	  sy-reply-yank-subject      (mail-fetch-field "subject")
	  sy-reply-yank-newsgroups   (mail-fetch-field "newsgroups")
	  sy-reply-yank-references   (mail-fetch-field "references")
	  sy-reply-yank-message-id   (mail-fetch-field "message-id")
	  sy-reply-yank-organization (mail-fetch-field "organization"))
    (or sy-reply-yank-date
	(setq sy-reply-yank-date "mumble mumble"))
    (or sy-reply-yank-from
	(setq sy-reply-yank-from "mumble mumble"))
    (or sy-reply-yank-subject
	(setq sy-reply-yank-subject "mumble mumble"))
    (or sy-reply-yank-newsgroups
	(setq sy-reply-yank-newsgroups "mumble mumble"))
    (or sy-reply-yank-references
	(setq sy-reply-yank-references "mumble mumble"))
    (or sy-reply-yank-message-id
	(setq sy-reply-yank-message-id "mumble mumble"))
    (or sy-reply-yank-organization
	(setq sy-reply-yank-organization "mumble mumble"))))

;;
;; ----------------------------------------------------------------------
;;
;; rewrite the header to be more conversational
;;
(defun sy-rewrite-headers (start)
  (goto-char start)
  (run-hooks 'sy-rewrite-header-hook))

;;
;; ----------------------------------------------------------------------
;;
;; some different styles of headers
;;
(defun sy-header-on-said ()
  (insert-string "\nOn " sy-reply-yank-date ",\n"
		 sy-reply-yank-from " said:\n"))

(defun sy-header-inarticle-writes ()
  (insert-string "\nIn article " sy-reply-yank-message-id
		 " " sy-reply-yank-from " writes:\n"))

(defun sy-header-regarding-writes ()
  (insert-string "\nRegarding " sy-reply-yank-subject
		 "; " sy-reply-yank-from " adds:\n"))

(defun sy-header-verbose ()
  (insert-string "\nOn " sy-reply-yank-date ",\n"
		 sy-reply-yank-from "\nfrom the organization "
		 sy-reply-yank-organization "\nhad this to say about article "
		 sy-reply-yank-message-id "\nin newsgroups "
		 sy-reply-yank-newsgroups "\nconcerning "
		 sy-reply-yank-subject "\nreferring to previous articles "
		 sy-reply-yank-references "\n"))

;;
;; ----------------------------------------------------------------------
;;
;; yank the original article in and attribute
;;
(defun sy-yank-original (arg)
  
  "Insert the message being replied to, if any (in rmail/gnus). Puts
point before the text and mark after. Calls generalized citation
function sy-insert-citation to cite all allowable lines."
  
  (interactive "P")
  (if mail-reply-buffer
      (let* ((sy-confirm-always-p (if (consp arg)
				      t
				    sy-confirm-always-p))
	     (attribution (sy-scan-rmail-for-names mail-reply-buffer))
	     (top (point))
	     (start (point))
	     (end (progn (delete-windows-on mail-reply-buffer)
			 (insert-buffer mail-reply-buffer)
			 (mark))))
	
	(sy-yank-fields start)
	(sy-rewrite-headers start)
	(setq start (point))
	(mail-yank-clear-headers top (mark))
	(setq sy-persist-attribution (concat attribution " "))
	(sy-insert-citation start end attribution))
    
    (goto-char top)
    (exchange-point-and-mark)))


;;
;; ----------------------------------------------------------------------
;;
;; this is here for compatibility with existing mail/news yankers
;; overloads the default mail-yank-original
;;
(defun mail-yank-original (arg)
  
  "Yank original message buffer into the reply buffer, citing as per
user preferences.  Numeric Argument forces confirmation.

Here is a description of the superyank.el package, what it does and
what variables control its operation.  This was written by Barry
Warsaw (warsaw@cme.nist.gov, {...}!uunet!cme-durer!warsaw).

A 'Citation' is the acknowledgement of the original author of a mail
message.  There are two general forms of citation. In 'nested
citations', indication is made that the cited line was written by
someone *other* that the current message author (or by that author at
an earlier time).  No indication is made as to the identity of the
original author.  Thus, a nested citation after multiple replies would
look like this (this is after my reply to a previous message):

>>John originally wrote this
>>and this as well
> Jane said that John didn't know
> what he was talking about
And that's what I think as well.

In non-nested citations, you won't see multiple \">\" characters at
the beginning of the line.  Non-nested citations will insert an
informative string at the beginning of a cited line, attributing that
line to an author.  The same message described above might look like
this if non-nested citations were used:

John> John originally wrote this
John> and this as well
Jane> Jane said that John didn't know
Jane> what he was talking about
And that's what I think as well.

Notice that my inclusion of Jane's inclusion of John's original
message did not result in a cited line of the form: Jane>John>.  Thus
no nested citations.  The style of citation is controlled by the
variable `sy-nested-citation-p'.  Nil uses non-nested citations and
non-nil uses old style, nested citations.

The variable `sy-citation-string' is the string to use as a marker for
a citation, either nested or non-nested.  For best results, this
string should be a single character with no trailing space and is
typically the character \">\".  In non-nested citations this string is
appended to the attribution string (author's name), along with a
trailing space.  In nested citations, a trailing space is only added
to a first level citation.

Another important variable is `sy-cite-regexp' which describes strings
that indicate a previously cited line.  This regular expression is
always used at the beginning of a line so it doesn't need to begin
with a \"^\" character.  Change this variable if you change
`sy-citation-string'.

The following section only applies to non-nested citations.

This package has a fair amount of intellegence related to deciphering
the author's name based on information provided by the original
message buffer.  In normal operation, the program will pick out the
author's first and last names, initials, terminal email address and
any other names it can find.  It will then pick an attribution string
from this list based on a user defined preference and it will ask for
confirmation if the user specifies.  This package gathers its
information from the `From:' line of the original message buffer.  It
recognizes From: lines with the following forms:

From: John Xavier Doe <doe@speedy.computer.com>
From: \"John Xavier Doe\" <doe@speedy.computer.com>
From: doe@speedy.computer.com (John Xavier Doe)
From: computer!speedy!doe (John Xavier Doe)
From: computer!speedy!doe (John Xavier Doe)
From: doe%speedy@computer.com (John Xavier Doe)

In this case, if confirmation is requested, the following strings will
be made available for completion and confirmation:

\"John\"
\"Xavier\"
\"Doe\"
\"JXD\"
\"doe\"

Note that completion is case sensitive.  If there was a problem
picking out a From: line, or any other problem getting even a single
name, then the user will be queried for an attribution string.  The
default attribution string is set in the variable
`sy-default-attribution'.

Sometimes people set their name fields so that it also includes a
title of the form:

From: doe@speedy.computer.com (John Doe -- Hacker Extraordinaire)

To avoid the inclusion of the string \"-- Hacker Extraordinaire\" in
the name list, the variable `sy-titlecue-regexp' is provided.  Its
default setting will still properly recognize names of the form:

From: xdoe@speedy.computer.com (John Xavier-Doe -- Crazed Hacker)

The variable `sy-preferred-attribution' contains an integer that
indicates which name field the user prefers to use as the attribution
string, based on the following key:

0: email address name is preferred
1: initials are preferred
2: first name is preferred
3: last name is preferred

The value can be greater than 3, in which case, you would be
preferring the 2nd throught nth -1 name.  In any case, if the
preferred name can't be found, then one of two actions will be taken
depending on the value of the variable `sy-use-only-preference-p'.  If
this is non-nil, then the `sy-default-attribution will be used.  If it
is nil, then a secondary scheme will be employed to find a suitable
attribution scheme.  First, the author's first name will be used.  If
that can't be found than the name list is searched for the first
non-nil, non-empty name string.  If still no name can be found, then
the user is either queried, or the `sy-default-attribution' is used,
depending on the value of `sy-confirm-always-p'.

If the variable `sy-confirm-always-p' is non-nil, superyank will always
confirm the attribution string with the user before inserting it into
the reply buffer.  Confirmation is with completion, but the completion
list is merely a suggestion; the user can override the list by typing
in a string of their choice.

The variable `sy-rewrite-header-hook' is a hook that contains a lambda
expression which rewrites the informative header at the top of the
yanked message.  Set to nil to avoid writing any header.

You can make superyank autofill each paragraph it cites by setting the
variable `sy-auto-fill-region-p' to non-nil.  Or set the variable to nil
and fill the paragraphs manually with sy-fill-paragraph-manually (see
below).

Finally, `sy-downcase-p' if non-nil, indicates that you always want to
downcase the attribution string before insertion, and
`sy-left-justify-p', if non-nil, indicates that you want to delete all
leading white space before citing.

Since the almost all yanking in other modes (RMAIL, GNUS) is done
through the function `mail-yank-original', and since superyank
overloads this function, cited yanking is automatically bound to the
C-c C-y key.  There are three other smaller functions that are
provided with superyank and they are bound as below.  Try C-h f on
each function to get more information on these functions.

Key Bindings:

C-c C-y     mail-yank-original (superyank's version)
C-c q       sy-fill-paragraph-manually
C-c C-q     sy-fill-paragraph-manually
C-c i       sy-insert-persist-attribution
C-c C-i     sy-insert-persist-attribution
C-c C-o     sy-open-line


Summary of variables, with their default values:

sy-default-attribution     (default: \"Anon\")
           Attribution to use if no attribution string can be deciphered
           from the original message buffer.

sy-citation-string         (default: \">\")
           String to append to the attribution string for citation, for
           best results, it should be one character with no trailing space.

sy-nested-citation-p       (default: nil)
           Nil means use non-nested citations, non-nil means use old style
           nested citations.

sy-cite-regexp             (default: \"[a-zA-Z0-9]*>\")
           Regular expression that matches the beginning of a previously
           cited line.  Always used at the beginning of a line so it does
           not need to start with a \"^\" character.

sy-titlecue-regexp         (default: \"\\s +-+\\s +\")
           Regular expression that matches a title delimiter in the name
           field.

sy-preferred-attribution   (default: 2)
           Integer indicating user's preferred attribution field.

sy-confirm-always-p        (default: t)
           Non-nil says always confirm with completion before inserting
           attribution.

sy-rewrite-header-hook     (default: 'sy-header-on-said)
           Hook for inserting informative header at the top of the yanked
           message.

sy-downcase-p              (default: nil)
           Non-nil says downcase the attribution string before insertion.

sy-left-justify-p          (default: nil)
           Non-nil says delete leading white space before citing.
  
sy-auto-fill-region-p      (default: nil)
           Non-nil says don't auto fill the region.  T says auto fill the
           paragraph.

sy-use-only-preference-p   (default: nil)
           If nil, use backup scheme when preferred attribution string
           can't be found.  If non-nil and preferred attribution string
           can't be found, then use sy-default-attribution."
  
  (interactive "P")
  
  (local-set-key "\C-cq"    'sy-fill-paragraph-manually)
  (local-set-key "\C-c\C-q" 'sy-fill-paragraph-manually)
  (local-set-key "\C-c\i"   'sy-insert-persist-attribution)
  (local-set-key "\C-c\C-i" 'sy-insert-persist-attribution)
  (local-set-key "\C-c\C-o" 'sy-open-line)
  
  (sy-yank-original arg))


;;
;; ----------------------------------------------------------------------
;;
;; based on Bruce Israel's "fill-paragraph-properly", and modified from
;; code posted by David C. Lawrence.  Modified to use the persistant
;; attribution if none could be found from the paragraph.
;;
(defun sy-fill-paragraph-manually (arg)
  "Fill paragraph containing or following point, automatically finding
the sy-cite-regexp and using it as the prefix.  If the sy-cite-regexp
is not in the first line of the paragraph, it makes a guess at what
the fill-prefix for the paragraph should be by looking at the first
line and taking anything up to the first alphanumeric character.

Prefix arg means justify both sides of paragraph as well.

This function just does fill-paragraph if the fill-prefix is set. If
what it deduces to be the paragraph prefix (based on the first line)
does not precede each line in the region, then the persistant
attribution is used.  The persistant attribution is just the last
attribution string used to cite lines."
  
  (interactive "P")
  (save-excursion
    (forward-paragraph)
    (or (bolp)
	(newline 1))
    
    (let ((end (point))
	  st
	  (fill-prefix fill-prefix))
      (backward-paragraph)
      (if (looking-at "\n")
	  (forward-char 1))
      (setq st (point))
      (if fill-prefix
	  nil
	(untabify st end) ;; die, scurvy tabs!
	;;
        ;; untabify might have made the paragraph longer character-wise,
        ;; make sure end reflects the correct location of eop.
	;;
        (forward-paragraph)
	(setq end (point))
        (goto-char st)
        (if (looking-at sy-cite-regexp)
            (setq fill-prefix (concat
			       (buffer-substring
				st (progn (re-search-forward sy-cite-regexp)
					  (point)))
			       " "))
	  ;;
          ;; this regexp is is convenient because paragraphs quoted by simple
          ;; indentation must still yield to us  <evil laugh>
	  ;;
          (while (looking-at "[^a-zA-Z0-9]")
	    (forward-char 1))
          (setq fill-prefix (buffer-substring st (point))))
        (next-line 1) (beginning-of-line)
        (while (and (< (point) end)
		    (not (string-equal fill-prefix "")))
	  ;;
          ;; if what we decided was the fill-prefix does not precede all
          ;; of the lines in the paragraph, we probably goofed.  In this
	  ;; case set it to the persistant attribution.
	  ;;
          (if (looking-at (regexp-quote fill-prefix))
	      ()
	    (setq fill-prefix sy-persist-attribution))
          (next-line 1)
	  (beginning-of-line)))
      (fill-region-as-paragraph st end arg))))

;;
;; ----------------------------------------------------------------------
;;
;; insert the persistant attribution at point
;;
(defun sy-insert-persist-attribution ()
  "Insert the persistant attribution at the beginning of the line that
point is on.  This string is the last attribution confirmed and used
in the yanked reply buffer."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (insert-string sy-persist-attribution)))


;;
;; ----------------------------------------------------------------------
;;
;; open a line putting the attribution at the beginning

(defun sy-open-line (arg)
  "Insert a newline and leave point before it.  Also inserts the
persistant attribution at the beginning of the line.  With arg,
inserts that many newlines."
  (interactive "p")
  (save-excursion
    (let ((start (point)))
      (open-line arg)
      (goto-char start)
      (forward-line)
      (while (< 0 arg)
	(sy-insert-persist-attribution)
	(forward-line 1)
	(setq arg (- arg 1))))))

(provide 'superyank)

;;; superyank.el ends here
