;;; mail-extr.el --- extract full name and address from RFC 822 mail header.

;; Copyright (C) 1992 Free Software Foundation, Inc.

;; Author: Joe Wells <jbw@cs.bu.edu>
;; Version: 1.0
;; Adapted-By: ESR
;; Keywords: mail

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Here is `mail-extr', a package for extracting full names and canonical
;; addresses from RFC 822 mail headers.  It is intended to be hooked into
;; other Emacs Lisp packages that deal with RFC 822 format messages, such as
;; Gnews, GNUS, RMAIL, MH-E, BBDB, VM, Supercite, etc.  Thus, this release is
;; mainly for Emacs Lisp developers.

;; There are two main benefits:

;; 1. Higher probability of getting the correct full name for a human than
;;    any other package I know of.  (On the other hand, it will cheerfully
;;    mangle non-human names/comments.)
;; 2. Address part is put in a canonical form.

;; The interface is not yet carved in stone; please give me suggestions.

;; I have an extensive test-case collection of funny addresses if you want to
;; work with the code.  Developing this code requires frequent testing to
;; make sure you're not breaking functionality.  I'm not posting the
;; test-cases because they take over 100K.

;; If you find an address that mail-extr fails on, please send it to me along
;; with what you think the correct results should be.  I do not consider it a
;; bug if mail-extr mangles a comment that does not correspond to a real
;; human full name, although I would prefer that mail-extr would return the
;; comment as-is.

;; Features:

;; * Full name handling:

;;   * knows where full names can be found in an address.
;;   * avoids using empty comments and quoted text.
;;   * extracts full names from mailbox names.
;;   * recognizes common formats for comments after a full name.
;;   * puts a period and a space after each initial.
;;   * understands & referring to the mailbox name capitalized.
;;   * strips name prefixes like "Prof.", etc..
;;   * understands what characters can occur in names (not just letters).
;;   * figures out middle initial from mailbox name.
;;   * removes funny nicknames.
;;   * keeps suffixes such as Jr., Sr., III, etc.
;;   * reorders "Last, First" type names.

;; * Address handling:

;;   * parses rfc822 quoted text, comments, and domain literals.
;;   * parses rfc822 multi-line headers.
;;   * does something reasonable with rfc822 GROUP addresses.
;;   * handles many rfc822 noncompliant and garbage addresses.
;;   * canonicalizes addresses (after stripping comments/phrases outside <>).
;;     * converts ! addresses into .UUCP and %-style addresses.
;;     * converts rfc822 ROUTE addresses to %-style addresses.
;;     * truncates %-style addresses at leftmost fully qualified domain name.
;;     * handles local relative precedence of ! vs. % and @ (untested).

;; It does almost no string creation.  It primarily uses the built-in
;; parsing routines with the appropriate syntax tables.  This should
;; result in greater speed.

;; TODO:

;; * handle all test cases.  (This will take forever.)
;; * software to pick the correct header to use (eg., "Senders-Name:").
;; * multiple addresses in the "From:" header (almost all of the necessary
;;   code is there).
;; * flag to not treat `,' as an address separator.  (This is useful when
;;   there is a "From:" header but no "Sender:" header, because then there
;;   is only allowed to be one address.)
;; * mailbox name does not necessarily contain full name.
;; * fixing capitalization when it's all upper or lowercase.  (Hard!)
;; * some of the domain literal handling is missing.  (But I've never even
;;   seen one of these in a mail address, so maybe no big deal.)
;; * arrange to have syntax tables byte-compiled.
;; * speed hacks.
;; * delete unused variables.
;; * arrange for testing with different relative precedences of ! vs. @
;;   and %.
;; * put mail-variant-method back into mail-extract-address-components.
;; * insert documentation strings!
;; * handle X.400-gatewayed addresses according to RFC 1148.

;;; Change Log: 
;; 
;; Mon Apr  6 23:59:09 1992  Joe Wells  (jbw at bigbird.bu.edu)
;; 
;; 	* Cleaned up some more.  Release version 1.0 to world.
;; 
;; Sun Apr  5 19:39:08 1992  Joe Wells  (jbw at bigbird.bu.edu)
;; 
;; 	* Cleaned up full name extraction extensively.
;; 
;; Sun Feb  2 14:45:24 1992  Joe Wells  (jbw at bigbird.bu.edu)
;; 
;; 	* Total rewrite.  Integrated mail-canonicalize-address into
;; 	mail-extract-address-components.  Now handles GROUP addresses more
;; 	or less correctly.  Better handling of lots of different cases.
;; 
;; Fri Jun 14 19:39:50 1991
;;	* Created.

;;; Code:

;; Variable definitions.

(defvar mail-@-binds-tighter-than-! nil)

;;----------------------------------------------------------------------
;; what orderings are meaningful?????
;;(defvar mail-operator-precedence-list '(?! ?% ?@))
;; Right operand of a % or a @ must be a domain name, period.  No other
;; operators allowed.  Left operand of a @ is an address relative to that
;; site.

;; Left operand of a ! must be a domain name.  Right operand is an
;; arbitrary address.
;;----------------------------------------------------------------------

(defconst mail-space-char 32)

(defconst mail-whitespace " \t\n")

;; Any character that can occur in a name in an RFC822 address.
;; Yes, there are weird people with digits in their names.
(defconst mail-all-letters "A-Za-z---{|}'~0-9`.")

;; Any character that can occur in a name, not counting characters that
;; separate parts of a multipart name.
(defconst mail-all-letters-but-separators "A-Za-z{|}'~0-9`")

;; Any character that can start a name
(defconst mail-first-letters "A-Za-z")

;; Any character that can end a name.
(defconst mail-last-letters "A-Za-z`'.")

;; Matches an initial not followed by both a period and a space. 
(defconst mail-bad-initials-pattern
  (format "\\(\\([^%s]\\|\\`\\)[%s]\\)\\(\\.\\([^ ]\\)\\| \\|\\([^%s .]\\)\\|\\'\\)"
	  mail-all-letters mail-first-letters mail-all-letters))

(defconst mail-non-name-chars (concat "^" mail-all-letters "."))

(defconst mail-non-begin-name-chars (concat "^" mail-first-letters))

(defconst mail-non-end-name-chars (concat "^" mail-last-letters))

;; Matches periods used instead of spaces.  Must not match the period
;; following an initial.
(defconst mail-bad-\.-pattern
  (format "\\([%s][%s]\\)\\.+\\([%s]\\)"
	  mail-all-letters mail-last-letters mail-first-letters))

;; Matches an embedded or leading nickname that should be removed.
(defconst mail-nickname-pattern
  (format "\\([ .]\\|\\`\\)[\"'`\[\(]\\([ .%s]+\\)[\]\"'\)] "
	  mail-all-letters))

;; Matches a leading title that is not part of the name (does not
;; contribute to uniquely identifying the person).
(defconst mail-full-name-prefixes
      '"\\` *\\(Prof\\|Dr\\|Mrs?\\|Rev\\|Rabbi\\|SysOp\\|LCDR\\)\\.? ")

;; Matches the occurrence of a generational name suffix, and the last
;; character of the preceding name.
(defconst mail-full-name-suffix-pattern
  (format
   "\\(,? ?\\([JjSs]r\\.?\\|V?I+V?\\)\\)\\([^%s]\\([^%s]\\|\\'\\)\\|\\'\\)"
   mail-all-letters mail-all-letters))

(defconst mail-roman-numeral-pattern
  "V?I+V?\\b")

;; Matches a trailing uppercase (with other characters possible) acronym.
;; Must not match a trailing uppercase last name or trailing initial
(defconst mail-weird-acronym-pattern "\\([A-Z]+[-_/]\\|[A-Z][A-Z][A-Z]?\\b\\)")
      
;; Matches a mixed-case or lowercase name (not an initial).
(defconst mail-mixed-case-name-pattern
  (format
   "\\b\\([a-z][%s]*[%s]\\|[%s][%s]*[a-z][%s]*[%s]\\|[%s][%s]*[a-z]\\)"
   mail-all-letters mail-last-letters
   mail-first-letters mail-all-letters mail-all-letters mail-last-letters
   mail-first-letters mail-all-letters))

;; Matches a trailing alternative address.
(defconst mail-alternative-address-pattern "[a-zA-Z.]+[!@][a-zA-Z.]")

;; Matches a variety of trailing comments not including comma-delimited
;; comments.
(defconst mail-trailing-comment-start-pattern " [-{]\\|--\\|[+@#></\;]")

;; Matches a name (not an initial).
;; This doesn't force a word boundary at the end because sometimes a
;; comment is separated by a `-' with no preceding space.
(defconst mail-name-pattern
  (format
   "\\b[%s][%s]*[%s]"
   mail-first-letters mail-all-letters mail-last-letters))

(defconst mail-initial-pattern
  (format "\\b[%s]\\([. ]\\|\\b\\)" mail-first-letters))

;; Matches a single name before a comma.
(defconst mail-last-name-first-pattern
  (concat "\\`" mail-name-pattern ","))

;; Matches telephone extensions.
(defconst mail-telephone-extension-pattern
  "\\(\\([Ee]xt\\|[Tt]el\\|[Xx]\\).?\\)? *\\+?[0-9][- 0-9]+")

;; Matches ham radio call signs.
(defconst mail-ham-call-sign-pattern
  "\\b[A-Z]+[0-9][A-Z0-9]*")

;; Matches normal single-part name
(defconst mail-normal-name-pattern
  (format
   "\\b[%s][%s]+[%s]"
   mail-first-letters mail-all-letters-but-separators mail-last-letters))

;; Matches normal two names with missing middle initial
(defconst mail-two-name-pattern
  (concat "\\`\\(" mail-normal-name-pattern
	  "\\|" mail-initial-pattern
	  "\\) +\\(" mail-normal-name-pattern "\\)\\(,\\|\\'\\)"))

(defvar address-syntax-table (make-syntax-table))
(defvar address-comment-syntax-table (make-syntax-table))
(defvar address-domain-literal-syntax-table (make-syntax-table))
(defvar address-text-comment-syntax-table (make-syntax-table))
(defvar address-text-syntax-table (make-syntax-table))
(mapcar
 (function
  (lambda (pair)
    (let ((syntax-table (symbol-value (car pair))))
      (mapcar
       (function
	(lambda (item)
	  (if (eq 2 (length item))
	      (modify-syntax-entry (car item) (car (cdr item)) syntax-table)
	    (let ((char (car item))
		  (bound (car (cdr item)))
		  (syntax (car (cdr (cdr item)))))
	      (while (<= char bound)
		(modify-syntax-entry char syntax syntax-table)
		(setq char (1+ char)))))))
       (cdr pair)))))
 '((address-syntax-table
    (0  31   "w")			;control characters
    (32      " ")			;SPC
    (?! ?~   "w")			;printable characters
    (127     "w")			;DEL
    (128 255 "w")			;high-bit-on characters
    (?\t " ")
    (?\r " ")
    (?\n " ")
    (?\( ".")
    (?\) ".")
    (?<  ".")
    (?>  ".")
    (?@  ".")
    (?,  ".")
    (?\; ".")
    (?:  ".")
    (?\\ "\\")
    (?\" "\"")
    (?.  ".")
    (?\[ ".")
    (?\] ".")
    ;; % and ! aren't RFC822 characters, but it is convenient to pretend
    (?%  ".")
    (?!  ".")
    )
   (address-comment-syntax-table
    (0 255 "w")
    (?\( "\(\)")
    (?\) "\)\(")
    (?\\ "\\"))
   (address-domain-literal-syntax-table
    (0 255 "w")
    (?\[ "\(\]")			;??????
    (?\] "\)\[")			;??????
    (?\\ "\\"))
   (address-text-comment-syntax-table
    (0 255 "w")
    (?\( "\(\)")
    (?\) "\)\(")
    (?\[ "\(\]")
    (?\] "\)\[")
    (?\{ "\(\}")
    (?\} "\)\{")
    (?\\ "\\")
    (?\" "\"")
    ;; (?\' "\)\`")
    ;; (?\` "\(\'")
    )
   (address-text-syntax-table
    (0 255 ".")
    (?A ?Z "w")
    (?a ?z "w")
    (?-    "w")
    (?\}   "w")
    (?\{   "w")
    (?|    "w")
    (?\'   "w")
    (?~    "w")
    (?0 ?9 "w"))
   ))


;; Utility functions and macros.

(defmacro mail-undo-backslash-quoting (beg end)
  (`(save-excursion
      (save-restriction
	(narrow-to-region (, beg) (, end))
	(goto-char (point-min))
	;; undo \ quoting
	(while (re-search-forward "\\\\\\(.\\)" nil t)
	  (replace-match "\\1")
	  ;; CHECK: does this leave point after the replacement?
	  )))))

(defmacro mail-nuke-char-at (pos)
  (` (save-excursion
       (goto-char (, pos))
       (delete-char 1)
       (insert mail-space-char))))

(defmacro mail-nuke-elements-outside-range (list-symbol beg-symbol end-symbol
							&optional no-replace)
  (` (progn
       (setq temp (, list-symbol))
       (while temp
	 (cond ((or (> (car temp) (, end-symbol))
		    (< (car temp) (, beg-symbol)))
		(, (or no-replace
		       (` (mail-nuke-char-at (car temp)))))
		(setcar temp nil)))
	 (setq temp (cdr temp)))
       (setq (, list-symbol) (delq nil (, list-symbol))))))

(defun mail-demarkerize (marker)
  (and marker
       (if (markerp marker)
	   (let ((temp (marker-position marker)))
	     (set-marker marker nil)
	     temp)
	 marker)))

(defun mail-markerize (pos)
  (and pos
       (if (markerp pos)
	   pos
	 (copy-marker pos))))

(defmacro mail-last-element (list)
  "Return last element of LIST."
  (` (let ((list (, list)))
       (while (not (null (cdr list)))
	 (setq list (cdr list)))
       (car list))))
  
(defmacro mail-safe-move-sexp (arg)
  "Safely skip over one balanced sexp, if there is one.  Return t if success."
  (` (condition-case error
	 (progn
	   (goto-char (scan-sexps (point) (, arg)))
	   t)
       (error
	(if (string-equal (nth 1 error) "Unbalanced parentheses")
	    nil
	  (while t
	    (signal (car error) (cdr error))))))))


;; The main function to grind addresses

(defun mail-extract-address-components (address)
  "Given an rfc 822 ADDRESS, extract full name and canonical address.
Returns a list of the form (FULL-NAME CANONICAL-ADDRESS)."
  (let ((canonicalization-buffer (get-buffer-create "*canonical address*"))
	(extraction-buffer (get-buffer-create "*extract address components*"))
	(foo 'bar)
	char
	multiple-addresses
	<-pos >-pos @-pos :-pos ,-pos !-pos %-pos \;-pos
	group-:-pos group-\;-pos route-addr-:-pos
	record-pos-symbol
	first-real-pos last-real-pos
	phrase-beg phrase-end
	comment-beg comment-end
	quote-beg quote-end
	atom-beg atom-end
	mbox-beg mbox-end
	\.-ends-name
	temp
	name-suffix
	saved-point
	fi mi li
	saved-%-pos saved-!-pos saved-@-pos
	domain-pos \.-pos insert-point)
    
    (save-excursion
      (set-buffer extraction-buffer)
      (buffer-disable-undo extraction-buffer)
      (set-syntax-table address-syntax-table)
      (widen)
      (erase-buffer)
      (setq case-fold-search nil)
      
      ;; Insert extra space at beginning to allow later replacement with <
      ;; without having to move markers.
      (insert mail-space-char address)
      
      ;; stolen from rfc822.el
      ;; Unfold multiple lines.
      (goto-char (point-min))
      (while (re-search-forward "\\([^\\]\\(\\\\\\\\\\)*\\)\n[ \t]" nil t)
	(replace-match "\\1 " t))
      
      ;; first pass grabs useful information about address
      (goto-char (point-min))
      (while (progn
	       (skip-chars-forward mail-whitespace)
	       (not (eobp)))
	(setq char (char-after (point)))
	(or first-real-pos
	    (if (not (eq char ?\())
		(setq first-real-pos (point))))
	(cond
	 ;; comment
	 ((eq char ?\()
	  (set-syntax-table address-comment-syntax-table)
	  ;; only record the first non-empty comment's position
	  (if (and (not comment-beg)
		   (save-excursion
		     (forward-char 1)
		     (skip-chars-forward mail-whitespace)
		     (not (eq ?\) (char-after (point))))))
	      (setq comment-beg (point)))
	  ;; TODO: don't record if unbalanced
	  (or (mail-safe-move-sexp 1)
	      (forward-char 1))
	  (set-syntax-table address-syntax-table)
	  (if (and comment-beg
		   (not comment-end))
	      (setq comment-end (point))))
	 ;; quoted text
	 ((eq char ?\")
	  ;; only record the first non-empty quote's position
	  (if (and (not quote-beg)
		   (save-excursion
		     (forward-char 1)
		     (skip-chars-forward mail-whitespace)
		     (not (eq ?\" (char-after (point))))))
	      (setq quote-beg (point)))
	  ;; TODO: don't record if unbalanced
	  (or (mail-safe-move-sexp 1)
	      (forward-char 1))
	  (if (and quote-beg
		   (not quote-end))
	      (setq quote-end (point))))
	 ;; domain literals
	 ((eq char ?\[)
	  (set-syntax-table address-domain-literal-syntax-table)
	  (or (mail-safe-move-sexp 1)
	      (forward-char 1))
	  (set-syntax-table address-syntax-table))
	 ;; commas delimit addresses when outside < > pairs.
	 ((and (eq char ?,)
	       (or (null <-pos)
		   (and >-pos
			;; handle weird munged addresses
			(> (mail-last-element <-pos) (car >-pos)))))
	  (setq multiple-addresses t)
	  (delete-char 1)
	  (narrow-to-region (point-min) (point)))
	 ;; record the position of various interesting chars, determine
	 ;; legality later.
	 ((setq record-pos-symbol
		(cdr (assq char
			   '((?< . <-pos) (?> . >-pos) (?@ . @-pos)
			     (?: . :-pos) (?, . ,-pos) (?! . !-pos)
			     (?% . %-pos) (?\; . \;-pos)))))
	  (set record-pos-symbol
	       (cons (point) (symbol-value record-pos-symbol)))
	  (forward-char 1))
	 ((eq char ?.)
	  (forward-char 1))
	 ((memq char '(
		       ;; comment terminator illegal
		       ?\)
		       ;; domain literal terminator illegal
		       ?\]
		       ;; \ allowed only within quoted strings,
		       ;; domain literals, and comments
		       ?\\
		       ))
	  (mail-nuke-char-at (point))
	  (forward-char 1))
	 (t
	  (forward-word 1)))
	(or (eq char ?\()
	    (setq last-real-pos (point))))
      
      ;; Use only the leftmost <, if any.  Replace all others with spaces.
      (while (cdr <-pos)
	(mail-nuke-char-at (car <-pos))
	(setq <-pos (cdr <-pos)))
      
      ;; Use only the rightmost >, if any.  Replace all others with spaces.
      (while (cdr >-pos)
	(mail-nuke-char-at (nth 1 >-pos))
	(setcdr >-pos (nthcdr 2 >-pos)))
      
      ;; If multiple @s and a :, but no < and >, insert around buffer.
      ;; This commonly happens on the UUCP "From " line.  Ugh.
      (cond ((and (> (length @-pos) 1)
		  :-pos			;TODO: check if between @s
		  (not <-pos))
	     (goto-char (point-min))
	     (delete-char 1)
	     (setq <-pos (list (point)))
	     (insert ?<)))
      
      ;; If < but no >, insert > in rightmost possible position
      (cond ((and <-pos
		  (null >-pos))
	     (goto-char (point-max))
	     (setq >-pos (list (point)))
	     (insert ?>)))
      
      ;; If > but no <, replace > with space.
      (cond ((and >-pos
		  (null <-pos))
	     (mail-nuke-char-at (car >-pos))
	     (setq >-pos nil)))

      ;; Turn >-pos and <-pos into non-lists
      (setq >-pos (car >-pos)
	    <-pos (car <-pos))
      
      ;; Trim other punctuation lists of items outside < > pair to handle
      ;; stupid MTAs.
      (cond (<-pos			; don't need to check >-pos also
	     ;; handle bozo software that violates RFC 822 by sticking
	     ;; punctuation marks outside of a < > pair
	     (mail-nuke-elements-outside-range @-pos <-pos >-pos t)
	     ;; RFC 822 says nothing about these two outside < >, but
	     ;; remove those positions from the lists to make things
	     ;; easier.
	     (mail-nuke-elements-outside-range !-pos <-pos >-pos t)
	     (mail-nuke-elements-outside-range %-pos <-pos >-pos t)))
      
      ;; Check for : that indicates GROUP list and for : part of
      ;; ROUTE-ADDR spec.
      ;; Can't possibly be more than two :.  Nuke any extra.
      (while :-pos
	(setq temp (car :-pos)
	      :-pos (cdr :-pos))
	(cond ((and <-pos >-pos
		    (> temp <-pos)
		    (< temp >-pos))
	       (if (or route-addr-:-pos
		       (< (length @-pos) 2)
		       (> temp (car @-pos))
		       (< temp (nth 1 @-pos)))
		   (mail-nuke-char-at temp)
		 (setq route-addr-:-pos temp)))
	      ((or (not <-pos)
		   (and <-pos
			(< temp <-pos)))
	       (setq group-:-pos temp))))
      
      ;; Nuke any ; that is in or to the left of a < > pair or to the left
      ;; of a GROUP starting :.  Also, there may only be one ;.
      (while \;-pos
	(setq temp (car \;-pos)
	      \;-pos (cdr \;-pos))
	(cond ((and <-pos >-pos
		    (> temp <-pos)
		    (< temp >-pos))
	       (mail-nuke-char-at temp))
	      ((and (or (not group-:-pos)
			(> temp group-:-pos))
		    (not group-\;-pos))
	       (setq group-\;-pos temp))))
      
      ;; Handle junk like ";@host.company.dom" that sendmail adds.
      ;; **** should I remember comment positions?
      (and group-\;-pos
	   ;; this is fine for now
	   (mail-nuke-elements-outside-range !-pos group-:-pos group-\;-pos t)
	   (mail-nuke-elements-outside-range @-pos group-:-pos group-\;-pos t)
	   (mail-nuke-elements-outside-range %-pos group-:-pos group-\;-pos t)
	   (mail-nuke-elements-outside-range ,-pos group-:-pos group-\;-pos t)
	   (and last-real-pos
		(> last-real-pos (1+ group-\;-pos))
		(setq last-real-pos (1+ group-\;-pos)))
	   (and comment-end
		(> comment-end group-\;-pos)
		(setq comment-end nil
		      comment-beg nil))
	   (and quote-end
		(> quote-end group-\;-pos)
		(setq quote-end nil
		      quote-beg nil))
	   (narrow-to-region (point-min) group-\;-pos))
      
      ;; Any commas must be between < and : of ROUTE-ADDR.  Nuke any
      ;; others.
      ;; Hell, go ahead an nuke all of the commas.
      ;; **** This will cause problems when we start handling commas in
      ;; the PHRASE part .... no it won't ... yes it will ... ?????
      (mail-nuke-elements-outside-range ,-pos 1 1)
      
      ;; can only have multiple @s inside < >.  The fact that some MTAs
      ;; put de-bracketed ROUTE-ADDRs in the UUCP-style "From " line is
      ;; handled above.
      
      ;; Locate PHRASE part of ROUTE-ADDR.
      (cond (<-pos
	     (goto-char <-pos)
	     (skip-chars-backward mail-whitespace)
	     (setq phrase-end (point))
	     (goto-char (or ;;group-:-pos
			    (point-min)))
	     (skip-chars-forward mail-whitespace)
	     (if (< (point) phrase-end)
		 (setq phrase-beg (point))
	       (setq phrase-end nil))))
      
      ;; handle ROUTE-ADDRS with real ROUTEs.
      ;; If there are multiple @s, then we assume ROUTE-ADDR syntax, and
      ;; any % or ! must be semantically meaningless.
      ;; TODO: do this processing into canonicalization buffer
      (cond (route-addr-:-pos
	     (setq !-pos nil
		   %-pos nil
		   >-pos (copy-marker >-pos)
		   route-addr-:-pos (copy-marker route-addr-:-pos))
	     (goto-char >-pos)
	     (insert-before-markers ?X)
	     (goto-char (car @-pos))
	     (while (setq @-pos (cdr @-pos))
	       (delete-char 1)
	       (setq %-pos (cons (point-marker) %-pos))
	       (insert "%")
	       (goto-char (1- >-pos))
	       (save-excursion
		 (insert-buffer-substring extraction-buffer
					  (car @-pos) route-addr-:-pos)
		 (delete-region (car @-pos) route-addr-:-pos))
	       (or (cdr @-pos)
		   (setq saved-@-pos (list (point)))))
	     (setq @-pos saved-@-pos)
	     (goto-char >-pos)
	     (delete-char -1)
	     (mail-nuke-char-at route-addr-:-pos)
	     (mail-demarkerize route-addr-:-pos)
	     (setq route-addr-:-pos nil
		   >-pos (mail-demarkerize >-pos)
		   %-pos (mapcar 'mail-demarkerize %-pos))))
      
      ;; de-listify @-pos
      (setq @-pos (car @-pos))
      
      ;; TODO: remove comments in the middle of an address
      
      (set-buffer canonicalization-buffer)
      
      (buffer-disable-undo canonicalization-buffer)
      (set-syntax-table address-syntax-table)
      (setq case-fold-search nil)
      
      (widen)
      (erase-buffer)
      (insert-buffer-substring extraction-buffer)
      
      (if <-pos
	  (narrow-to-region (progn
			      (goto-char (1+ <-pos))
			      (skip-chars-forward mail-whitespace)
			      (point))
			    >-pos)
	;; ****** Oh no!  What if the address is completely empty!
	(narrow-to-region first-real-pos last-real-pos))
      
      (and @-pos %-pos
	   (mail-nuke-elements-outside-range %-pos (point-min) @-pos))
      (and %-pos !-pos
	   (mail-nuke-elements-outside-range !-pos (point-min) (car %-pos)))
      (and @-pos !-pos (not %-pos)
	   (mail-nuke-elements-outside-range !-pos (point-min) @-pos))
      
      ;; Error condition:?? (and %-pos (not @-pos))

      (cond (!-pos
	     ;; **** I don't understand this save-restriction and the
	     ;; narrow-to-region inside it.  Why did I do that?
	     (save-restriction
	       (cond ((and @-pos
			   mail-@-binds-tighter-than-!)
		      (goto-char @-pos)
		      (setq %-pos (cons (point) %-pos)
			    @-pos nil)
		      (delete-char 1)
		      (insert "%")
		      (setq insert-point (point-max)))
		     (mail-@-binds-tighter-than-!
		      (setq insert-point (point-max)))
		     (%-pos
		      (setq insert-point (mail-last-element %-pos)
			    saved-%-pos (mapcar 'mail-markerize %-pos)
			    %-pos nil
			    @-pos (mail-markerize @-pos)))
		     (@-pos
		      (setq insert-point @-pos)
		      (setq @-pos (mail-markerize @-pos)))
		     (t
		      (setq insert-point (point-max))))
	       (narrow-to-region (point-min) insert-point)
	       (setq saved-!-pos (car !-pos))
	       (while !-pos
		 (goto-char (point-max))
		 (cond ((and (not @-pos)
			     (not (cdr !-pos)))
			(setq @-pos (point))
			(insert-before-markers "@ "))
		       (t
			(setq %-pos (cons (point) %-pos))
			(insert-before-markers "% ")))
		 (backward-char 1)
		 (insert-buffer-substring 
		  (current-buffer)
		  (if (nth 1 !-pos)
		      (1+ (nth 1 !-pos))
		    (point-min))
		  (car !-pos))
		 (delete-char 1)
		 (or (save-excursion
		       (mail-safe-move-sexp -1)
		       (skip-chars-backward mail-whitespace)
		       (eq ?. (preceding-char)))
		     (insert-before-markers
		      (if (save-excursion
			    (skip-chars-backward mail-whitespace)
			    (eq ?. (preceding-char)))
			  ""
			".")
		      "uucp"))
		 (setq !-pos (cdr !-pos))))
	     (and saved-%-pos
		  (setq %-pos (append (mapcar 'mail-demarkerize saved-%-pos)
					%-pos)))
	     (setq @-pos (mail-demarkerize @-pos))
	     (narrow-to-region (1+ saved-!-pos) (point-max))))
      (cond ((and %-pos
		  (not @-pos))
	     (goto-char (car %-pos))
	     (delete-char 1)
	     (setq @-pos (point))
	     (insert "@")
	     (setq %-pos (cdr %-pos))))
      (setq %-pos (nreverse %-pos))
      ;; RFC 1034 doesn't approve of this, oh well:
      (downcase-region (or (car %-pos) @-pos (point-max)) (point-max))
      (cond (%-pos			; implies @-pos valid
	     (setq temp %-pos)
	     (catch 'truncated
	       (while temp
		 (goto-char (or (nth 1 temp)
				@-pos))
		 (skip-chars-backward mail-whitespace)
		 (save-excursion
		   (mail-safe-move-sexp -1)
		   (setq domain-pos (point))
		   (skip-chars-backward mail-whitespace)
		   (setq \.-pos (eq ?. (preceding-char))))
		 (cond ((and \.-pos
			     (get
			      (intern
			       (buffer-substring domain-pos (point)))
			      'domain-name))
			(narrow-to-region (point-min) (point))
			(goto-char (car temp))
			(delete-char 1)
			(setq @-pos (point))
			(setcdr temp nil)
			(setq %-pos (delq @-pos %-pos))
			(insert "@")
			(throw 'truncated t)))
		 (setq temp (cdr temp))))))
      (setq mbox-beg (point-min)
	    mbox-end (if %-pos (car %-pos)
		       (or @-pos
			   (point-max))))
      
      ;; Done canonicalizing address.
      
      (set-buffer extraction-buffer)
      
      ;; Find the full name
      
      (cond ((and phrase-beg
		  (eq quote-beg phrase-beg)
		  (<= quote-end phrase-end))
	     (narrow-to-region (1+ quote-beg) (1- quote-end))
	     (mail-undo-backslash-quoting (point-min) (point-max)))
	    (phrase-beg
	     (narrow-to-region phrase-beg phrase-end))
	    (comment-beg
	     (narrow-to-region (1+ comment-beg) (1- comment-end))
	     (mail-undo-backslash-quoting (point-min) (point-max)))
	    (t
	     ;; *** Work in canon buffer instead?  No, can't.  Hmm.
	     (delete-region (point-min) (point-max))
	     (insert-buffer-substring canonicalization-buffer
				      mbox-beg mbox-end)
	     (goto-char (point-min))
	     (setq \.-ends-name (search-forward "_" nil t))
	     (goto-char (point-min))
	     (while (progn
		      (skip-chars-forward mail-whitespace)
		      (not (eobp)))
	       (setq char (char-after (point)))
	       (cond
		((eq char ?\")
		 (setq quote-beg (point))
		 (or (mail-safe-move-sexp 1)
		     ;; TODO: handle this error condition!!!!!
		     (forward-char 1))
		 ;; take into account deletions
		 (setq quote-end (- (point) 2))
		 (save-excursion
		   (backward-char 1)
		   (delete-char 1)
		   (goto-char quote-beg)
		   (delete-char 1))
		 (mail-undo-backslash-quoting quote-beg quote-end)
		 (or (eq mail-space-char (char-after (point)))
		     (insert " "))
		 (setq \.-ends-name t))
		((eq char ?.)
		 (if (eq (char-after (1+ (point))) ?_)
		     (progn
		       (forward-char 1)
		       (delete-char 1)
		       (insert mail-space-char))
		   (if \.-ends-name
		       (narrow-to-region (point-min) (point))
		     (delete-char 1)
		     (insert " "))))
		((memq (char-syntax char) '(?. ?\\))
		 (delete-char 1)
		 (insert " "))
		(t
		 (setq atom-beg (point))
		 (forward-word 1)
		 (setq atom-end (point))
		 (save-restriction
		   (narrow-to-region atom-beg atom-end)
		   (goto-char (point-min))
		   (while (re-search-forward "\\([^_]+\\)_" nil t)
		     (replace-match "\\1 "))
		   (goto-char (point-max))))))))
      
      (set-syntax-table address-text-syntax-table)
      
      (setq xxx (mail-variant-method (buffer-string)))
      (delete-region (point-min) (point-max))
      (insert xxx)
      (goto-char (point-min))

;;       ;; Compress whitespace
;;       (goto-char (point-min))
;;       (while (re-search-forward "[ \t\n]+" nil t)
;; 	(replace-match " "))
;;       
;;       ;; Fix . used as space
;;       (goto-char (point-min))
;;       (while (re-search-forward mail-bad-\.-pattern nil t)
;; 	(replace-match "\\1 \\2"))
;; 
;;       ;; Delete trailing parenthesized comment
;;       (goto-char (point-max))
;;       (skip-chars-backward mail-whitespace)
;;       (cond ((memq (char-after (1- (point))) '(?\) ?\} ?\]))
;; 	     (setq comment-end (point))
;; 	     (set-syntax-table address-text-comment-syntax-table)
;; 	     (or (mail-safe-move-sexp -1)
;; 		 (backward-char 1))
;; 	     (set-syntax-table address-text-syntax-table)
;; 	     (setq comment-beg (point))
;; 	     (skip-chars-backward mail-whitespace)
;; 	     (if (bobp)
;; 		 (narrow-to-region (1+ comment-beg) (1- comment-end))
;; 	       (narrow-to-region (point-min) (point)))))
;;       
;;       ;; Find, save, and delete any name suffix
;;       ;; *** Broken!
;;       (goto-char (point-min))
;;       (cond ((re-search-forward mail-full-name-suffix-pattern nil t)
;; 	     (setq name-suffix (buffer-substring (match-beginning 3)
;; 						 (match-end 3)))
;; 	     (replace-match "\\1 \\4")))
;;       
;;       ;; Delete ALL CAPS words and after, if preceded by mixed-case or
;;       ;; lowercase words.  Eg. XT-DEM.
;;       (goto-char (point-min))
;;       ;; ## This will lose on something like "SMITH MAX".
;;       ;; ## maybe it should be
;;       ;; ##  " \\([A-Z]+[-_/][A-Z]+\\|[A-Z][A-Z][A-Z]\\)\\b.*[^A-Z \t]"
;;       ;; ## that is, three-letter-upper-case-word with non-upper-case
;;       ;; ## characters following it.
;;       (if (re-search-forward mail-mixed-case-name-pattern nil t)
;; 	  (if (re-search-forward mail-weird-acronym-pattern nil t)
;; 	      (narrow-to-region (point-min) (match-beginning 0))))
;;       
;;       ;; Delete trailing alternative address
;;       (goto-char (point-min))
;;       (if (re-search-forward mail-alternative-address-pattern nil t)
;; 	  (narrow-to-region (point-min) (match-beginning 0)))
;;       
;;       ;; Delete trailing comment
;;       (goto-char (point-min))
;;       (if (re-search-forward mail-trailing-comment-start-pattern nil t)
;; 	  (or (progn
;; 		(goto-char (match-beginning 0))
;; 		(skip-chars-backward mail-whitespace)
;; 		(bobp))
;; 	      (narrow-to-region (point-min) (match-beginning 0))))
;;       
;;       ;; Delete trailing comma-separated comment
;;       (goto-char (point-min))
;;       ;; ## doesn't this break "Smith, John"?  Yes.
;;       (re-search-forward mail-last-name-first-pattern nil t)
;;       (while (search-forward "," nil t)
;; 	(or (save-excursion
;; 	      (backward-char 2)
;; 	      (looking-at mail-full-name-suffix-pattern))
;; 	    (narrow-to-region (point-min) (1- (point)))))
;;       
;;       ;; Delete telephone numbers and ham radio call signs
;;       (goto-char (point-min))
;;       (if (re-search-forward mail-telephone-extension-pattern nil t)
;; 	  (narrow-to-region (point-min) (match-beginning 0)))
;;       (goto-char (point-min))
;;       (if (re-search-forward mail-ham-call-sign-pattern nil t)
;; 	  (if (eq (match-beginning 0) (point-min))
;; 	      (narrow-to-region (match-end 0) (point-max))
;; 	    (narrow-to-region (point-min) (match-beginning 0))))
;;       
;;       ;; Delete trailing word followed immediately by .
;;       (goto-char (point-min))
;;       ;; ## what's this for?  doesn't it mess up "Public, Harry Q."?  No.
;;       (if (re-search-forward "\\b[A-Za-z][A-Za-z]+\\. *\\'" nil t)
;; 	  (narrow-to-region (point-min) (match-beginning 0)))
;;       
;;       ;; Handle & substitution
;;       ;; TODO: remember to disable middle initial guessing
;;       (goto-char (point-min))
;;       (cond ((re-search-forward "\\( \\|\\`\\)&\\( \\|\\'\\)" nil t)
;; 	     (goto-char (match-end 1))
;; 	     (delete-char 1)
;; 	     (capitalize-region
;; 	      (point)
;; 	      (progn
;; 		(insert-buffer-substring canonicalization-buffer
;; 					 mbox-beg mbox-end)
;; 		(point)))))
;;       
;;       ;; Delete nickname
;;       (goto-char (point-min))
;;       (if (re-search-forward mail-nickname-pattern nil t)
;; 	  (replace-match (if (eq (match-beginning 2) (1- (match-end 2)))
;; 			     " \\2 "
;; 			   " ")))
;;       
;;       ;; Fixup initials
;;       (while (progn
;; 	       (goto-char (point-min))
;; 	       (re-search-forward mail-bad-initials-pattern nil t))
;; 	(replace-match
;; 	 (if (match-beginning 4)
;; 	     "\\1. \\4"
;; 	   (if (match-beginning 5)
;; 	       "\\1. \\5"
;; 	     "\\1. "))))
;;       
;;       ;; Delete title
;;       (goto-char (point-min))
;;       (if (re-search-forward mail-full-name-prefixes nil t)
;; 	  (narrow-to-region (point) (point-max)))
;;       
;;       ;; Delete trailing and preceding non-name characters
;;       (goto-char (point-min))
;;       (skip-chars-forward mail-non-begin-name-chars)
;;       (narrow-to-region (point) (point-max))
;;       (goto-char (point-max))
;;       (skip-chars-backward mail-non-end-name-chars)
;;       (narrow-to-region (point-min) (point))
      
      ;; If name is "First Last" and userid is "F?L", then assume
      ;; the middle initial is the second letter in the userid.
      ;; initially by Jamie Zawinski <jwz@lucid.com>
      (cond ((and (eq 3 (- mbox-end mbox-beg))
		  (progn
		    (goto-char (point-min))
		    (looking-at mail-two-name-pattern)))
	     (setq fi (char-after (match-beginning 0))
		   li (char-after (match-beginning 3)))
	     (save-excursion
	       (set-buffer canonicalization-buffer)
	       ;; char-equal is ignoring case here, so no need to upcase
	       ;; or downcase.
	       (let ((case-fold-search t))
		 (and (char-equal fi (char-after mbox-beg))
		      (char-equal li (char-after (1- mbox-end)))
		      (setq mi (char-after (1+ mbox-beg))))))
	     (cond ((and mi
			 ;; TODO: use better table than syntax table
			 (eq ?w (char-syntax mi)))
		    (goto-char (match-beginning 3))
		    (insert (upcase mi) ". ")))))
      
;;       ;; Restore suffix
;;       (cond (name-suffix
;; 	     (goto-char (point-max))
;; 	     (insert ", " name-suffix)
;; 	     (backward-word 1)
;; 	     (cond ((memq (following-char) '(?j ?J ?s ?S))
;; 		    (capitalize-word 1)
;; 		    (or (eq (following-char) ?.)
;; 			(insert ?.)))
;; 		   (t
;; 		    (upcase-word 1)))))
      
      ;; Result
      (list (buffer-string)
	    (progn
	      (set-buffer canonicalization-buffer)
	      (buffer-string)))
      )))

;; TODO: put this back in the above function now that it's proven:
(defun mail-variant-method (string)
  (let ((variant-buffer (get-buffer-create "*variant method buffer*"))
	(word-count 0)
	mixed-case-flag lower-case-flag upper-case-flag
	suffix-flag last-name-comma-flag
	comment-beg comment-end initial beg end
	)
    (save-excursion
      (set-buffer variant-buffer)
      (buffer-disable-undo variant-buffer)
      (set-syntax-table address-text-syntax-table)
      (widen)
      (erase-buffer)
      (setq case-fold-search nil)
      
      (insert string)
      
      ;; Fix . used as space
      (goto-char (point-min))
      (while (re-search-forward mail-bad-\.-pattern nil t)
	(replace-match "\\1 \\2"))

      ;; Skip any initial garbage.
      (goto-char (point-min))
      (skip-chars-forward mail-non-begin-name-chars)
      (skip-chars-backward "& \"")
      (narrow-to-region (point) (point-max))
      
      (catch 'stop
	(while t
	  (skip-chars-forward mail-whitespace)
	  
	  (cond
	   
	   ;; Delete title
	   ((and (eq word-count 0)
		 (looking-at mail-full-name-prefixes))
	    (goto-char (match-end 0))
	    (narrow-to-region (point) (point-max)))
	   
	   ;; Stop after name suffix
	   ((and (>= word-count 2)
		 (looking-at mail-full-name-suffix-pattern))
	    (skip-chars-backward mail-whitespace)
	    (setq suffix-flag (point))
	    (if (eq ?, (following-char))
		(forward-char 1)
	      (insert ?,))
	    ;; Enforce at least one space after comma
	    (or (eq mail-space-char (following-char))
		(insert mail-space-char))
	    (skip-chars-forward mail-whitespace)
	    (cond ((memq (following-char) '(?j ?J ?s ?S))
		   (capitalize-word 1)
		   (if (eq (following-char) ?.)
		       (forward-char 1)
		     (insert ?.)))
		  (t
		   (upcase-word 1)))
	    (setq word-count (1+ word-count))
	    (throw 'stop t))
	   
	   ;; Handle SCA names
	   ((looking-at "MKA \\(.+\\)")	; "Mundanely Known As"
	    (setq word-count 0)
	    (goto-char (match-beginning 1))
	    (narrow-to-region (point) (point-max)))
	   
	   ;; Various stopping points
	   ((or
	     ;; Stop before ALL CAPS acronyms, if preceded by mixed-case or
	     ;; lowercase words.  Eg. XT-DEM.
	     (and (>= word-count 2)
		  (or mixed-case-flag lower-case-flag)
		  (looking-at mail-weird-acronym-pattern)
		  (not (looking-at mail-roman-numeral-pattern)))
	     ;; Stop before 4-or-more letter lowercase words preceded by
	     ;; mixed case or uppercase words.
	     (and (>= word-count 2)
		  (or upper-case-flag mixed-case-flag)
		  (looking-at "[a-z][a-z][a-z][a-z]+\\b"))
	     ;; Stop before trailing alternative address
	     (looking-at mail-alternative-address-pattern)
	     ;; Stop before trailing comment not introduced by comma
	     (looking-at mail-trailing-comment-start-pattern)
	     ;; Stop before telephone numbers
	     (looking-at mail-telephone-extension-pattern))
	    (throw 'stop t))
	   
	   ;; Check for initial last name followed by comma
	   ((and (eq ?, (following-char))
		 (eq word-count 1))
	    (forward-char 1)
	    (setq last-name-comma-flag t)
	    (or (eq mail-space-char (following-char))
		(insert mail-space-char)))
	   
	   ;; Stop before trailing comma-separated comment
	   ((eq ?, (following-char))
	    (throw 'stop t))
	   
	   ;; Delete parenthesized/quoted comment/nickname
	   ((memq (following-char) '(?\( ?\{ ?\[ ?\" ?\' ?\`))
	    (setq comment-beg (point))
	    (set-syntax-table address-text-comment-syntax-table)
	    (cond ((memq (following-char) '(?\' ?\`))
		   (if (eq ?\' (following-char))
		       (forward-char 1))
		   (or (search-forward "'" nil t)
		       (delete-char 1)))
		  (t
		   (or (mail-safe-move-sexp 1)
		       (goto-char (point-max)))))
	    (set-syntax-table address-text-syntax-table)
	    (setq comment-end (point))
	    (cond
	     ;; Handle case of entire name being quoted
	     ((and (eq word-count 0)
		   (looking-at " *\\'")
		   (>= (- comment-end comment-beg) 2))
	      (narrow-to-region (1+ comment-beg) (1- comment-end))
	      (goto-char (point-min)))
	     (t
	      ;; Handle case of quoted initial
	      (if (and (or (= 3 (- comment-end comment-beg))
			   (and (= 4 (- comment-end comment-beg))
				(eq ?. (char-after (+ 2 comment-beg)))))
		       (not (looking-at " *\\'")))
		  (setq initial (char-after (1+ comment-beg)))
		(setq initial nil))
	      (delete-region comment-beg comment-end)
	      (if initial
		  (insert initial ". ")))))
	   
	   ;; Delete ham radio call signs
	   ((looking-at mail-ham-call-sign-pattern)
	    (delete-region (match-beginning 0) (match-end 0)))
	   
	   ;; Handle & substitution
	   ;; TODO: remember to disable middle initial guessing
	   ((and (or (bobp)
		     (eq mail-space-char (preceding-char)))
		 (looking-at "&\\( \\|\\'\\)"))
	    (delete-char 1)
	    (capitalize-region
	     (point)
	     (progn
	       (insert-buffer-substring canonicalization-buffer
					mbox-beg mbox-end)
	       (point))))
	   
	   ;; Fixup initials
	   ((looking-at mail-initial-pattern)
	    (or (eq (following-char) (upcase (following-char)))
		(setq lower-case-flag t))
	    (forward-char 1)
	    (if (eq ?. (following-char))
		(forward-char 1)
	      (insert ?.))
	    (or (eq mail-space-char (following-char))
		(insert mail-space-char))
	    (setq word-count (1+ word-count)))
	   
	   ;; Regular name words
	   ((looking-at mail-name-pattern)
	    (setq beg (point))
	    (setq end (match-end 0))
	    (set (if (re-search-forward "[a-z]" end t)
		     (if (progn
			   (goto-char beg)
			   (re-search-forward "[A-Z]" end t))
			 'mixed-case-flag
		       'lower-case-flag)
		   'upper-case-flag) t)
	    (goto-char end)
	    (setq word-count (1+ word-count)))

	   (t
	    (throw 'stop t)))))
      
      (narrow-to-region (point-min) (point))

      ;; Delete trailing word followed immediately by .
      (cond ((not suffix-flag)
	     (goto-char (point-min))
	     (if (re-search-forward "\\b[A-Za-z][A-Za-z]+\\. *\\'" nil t)
		 (narrow-to-region (point-min) (match-beginning 0)))))
      
      ;; If last name first put it at end (but before suffix)
      (cond (last-name-comma-flag
	     (goto-char (point-min))
	     (search-forward ",")
	     (setq end (1- (point)))
	     (goto-char (or suffix-flag (point-max)))
	     (or (eq mail-space-char (preceding-char))
		 (insert mail-space-char))
	     (insert-buffer-substring (current-buffer) (point-min) end)
	     (narrow-to-region (1+ end) (point-max))))
      
      (goto-char (point-max))
      (skip-chars-backward mail-non-end-name-chars)
      (if (eq ?. (following-char))
	  (forward-char 1))
      (narrow-to-region (point)
			(progn
			  (goto-char (point-min))
			  (skip-chars-forward mail-non-begin-name-chars)
			  (point)))
      
      ;; Compress whitespace
      (goto-char (point-min))
      (while (re-search-forward "[ \t\n]+" nil t)
	(replace-match " "))

      (buffer-substring (point-min) (point-max))

      )))

;; The country names are just in there for show right now, and because
;; Jamie thought it would be neat.  They aren't used yet.

;; Keep in mind that the country abbreviations follow ISO-3166.  There is
;; a U.S. FIPS that specifies a different set of two-letter country
;; abbreviations.

;; TODO: put this in its own obarray, instead of cluttering up the main
;; symbol table with junk.

(mapcar
 (function
  (lambda (x)
    (if (symbolp x)
	(put x 'domain-name t)
      (put (car x) 'domain-name (nth 1 x)))))
 '((ag "Antigua")
   (ar "Argentina")			; Argentine Republic
   arpa					; Advanced Projects Research Agency
   (at "Austria")			; The Republic of _
   (au "Australia")
   (bb "Barbados")
   (be "Belgium")			; The Kingdom of _
   (bg "Bulgaria")
   bitnet				; Because It's Time NET
   (bo "Bolivia")			; Republic of _
   (br "Brazil")			; The Federative Republic of _
   (bs "Bahamas")
   (bz "Belize")
   (ca "Canada")
   (ch "Switzerland")			; The Swiss Confederation
   (cl "Chile")				; The Republic of _
   (cn "China")				; The People's Republic of _
   (co "Columbia")
   com					; Commercial
   (cr "Costa Rica")			; The Republic of _
   (cs "Czechoslovakia")
   (de "Germany")
   (dk "Denmark")
   (dm "Dominica")
   (do "Dominican Republic")		; The _
   (ec "Ecuador")			; The Republic of _
   edu					; Educational
   (eg "Egypt")				; The Arab Republic of _
   (es "Spain")				; The Kingdom of _
   (fi "Finland")			; The Republic of _
   (fj "Fiji")
   (fr "France")
   gov					; Government (U.S.A.)
   (gr "Greece")			; The Hellenic Republic
   (hk "Hong Kong")
   (hu "Hungary")			; The Hungarian People's Republic (???)
   (ie "Ireland")
   (il "Israel")			; The State of _
   (in "India")				; The Republic of _
   int					; something British, don't know what
   (is "Iceland")			; The Republic of _
   (it "Italy")				; The Italian Republic
   (jm "Jamaica")
   (jp "Japan")
   (kn "St. Kitts and Nevis")
   (kr "South Korea")
   (lc "St. Lucia")
   (lk "Sri Lanka")		       ; The Democratic Socialist Republic of _
   mil					; Military (U.S.A.)
   (mx "Mexico")			; The United Mexican States
   (my "Malaysia")			; changed to Myanmar????
   (na "Namibia")
   nato					; North Atlantic Treaty Organization
   net					; Network
   (ni "Nicaragua")			; The Republic of _
   (nl "Netherlands")			; The Kingdom of the _
   (no "Norway")			; The Kingdom of _
   (nz "New Zealand")
   org					; Organization
   (pe "Peru")
   (pg "Papua New Guinea")
   (ph "Philippines")			; The Republic of the _
   (pl "Poland")
   (pr "Puerto Rico")
   (pt "Portugal")			; The Portugese Republic
   (py "Paraguay")
   (se "Sweden")			; The Kingdom of _
   (sg "Singapore")			; The Republic of _
   (sr "Suriname")
   (su "Soviet Union")
   (th "Thailand")			; The Kingdom of _
   (tn "Tunisia")
   (tr "Turkey")			; The Republic of _
   (tt "Trinidad and Tobago")
   (tw "Taiwan")
   (uk "United Kingdom")		; The _ of Great Britain
   unter-dom				; something German
   (us "U.S.A.")			; The United States of America
   uucp					; Unix to Unix CoPy
   (uy "Uruguay")			; The Eastern Republic of _
   (vc "St. Vincent and the Grenadines")
   (ve "Venezuela")			; The Republic of _
   (yu "Yugoslavia")			; The Socialist Federal Republic of _
   ;; Also said to be Zambia ...
   (za "South Africa")			; The Republic of _ (why not Zaire???)
   (zw "Zimbabwe")			; Republic of _
   ))
;; fipnet


;; Code for testing.

(defun time-extract ()
  (let (times list)
    (setq times (cons (current-time-string) times)
	  list problem-address-alist)
    (while list
      (mail-extract-address-components (car (car list)))
      (setq list (cdr list)))
    (setq times (cons (current-time-string) times))
    (nreverse times)))

(defun test-extract (&optional starting-point)
  (interactive)
  (set-buffer (get-buffer-create "*Testing*"))
  (erase-buffer)
  (sit-for 0)
  (mapcar 'test-extract-internal
	  (if starting-point
	      (memq starting-point problem-address-alist)
	     problem-address-alist)))

(defvar failed-item)
(defun test-extract-internal (item)
  (setq failed-item item)
  (let* ((address (car item))
	 (correct-name (nth 1 item))
	 (correct-canon (nth 2 item))
	 (result (mail-extract-address-components address))
	 (name (car result))
	 (canon (nth 1 result))
	 (name-correct (or (null correct-name)
			   (string-equal (downcase correct-name)
					 (downcase name))))
	 (canon-correct (or (null correct-canon)
			    (string-equal correct-canon canon))))
    (cond ((not (and name-correct canon-correct))
	   (pop-to-buffer "*Testing*")
	   (select-window (get-buffer-window (current-buffer)))
	   (goto-char (point-max))
	   (insert "Address: " address "\n")
	   (if (not name-correct)
	       (insert " Correct Name:  [" correct-name
		       "]\; Result: [" name "]\n"))
	   (if (not canon-correct)
	       (insert " Correct Canon: [" correct-canon
		       "]\; Result: [" canon "]\n"))
	   (insert "\n")
	   (sit-for 0))))
  (setq failed-item nil))

(defun test-continue-extract ()
  (interactive)
  (test-extract failed-item))


;; Assorted junk.

;;	warsaw@nlm.nih.gov (A Bad Dude -- Barry Warsaw)

;;'(from
;;  reply-to
;;  return-path
;;  x-uucp-from
;;  sender
;;  resent-from
;;  resent-sender
;;  resent-reply-to)

;;; mail-extr.el ends here
