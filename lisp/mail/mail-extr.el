;;; mail-extr.el --- extract full name and address from RFC 822 mail header.

;; Copyright (C) 1991, 1992, 1993, 1994 Free Software Foundation, Inc.

;; Author: Joe Wells <jbw@cs.bu.edu>
;; Maintainer: Jamie Zawinski <jwz@lucid.com>
;; Version: 1.8
;; Keywords: mail

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

;;; Commentary:

;; The entry point of this code is
;;
;;    mail-extract-address-components: (address)
;;  
;;    Given an RFC-822 ADDRESS, extract full name and canonical address.
;;    Returns a list of the form (FULL-NAME CANONICAL-ADDRESS).
;;    If no name can be extracted, FULL-NAME will be nil.
;;    ADDRESS may be a string or a buffer.  If it is a buffer, the visible 
;;     (narrowed) portion of the buffer will be interpreted as the address.
;;     (This feature exists so that the clever caller might be able to avoid
;;     consing a string.)
;;    If ADDRESS contains more than one RFC-822 address, only the first is
;;     returned.
;;
;; This code is more correct (and more heuristic) parser than the code in
;; rfc822.el.  And despite its size, it's fairly fast.
;;
;; There are two main benefits:
;;
;; 1. Higher probability of getting the correct full name for a human than
;;    any other package we know of.  (On the other hand, it will cheerfully
;;    mangle non-human names/comments.)
;; 2. Address part is put in a canonical form.
;;
;; The interface is not yet carved in stone; please give us suggestions.
;;
;; We have an extensive test-case collection of funny addresses if you want to
;; work with the code.  Developing this code requires frequent testing to
;; make sure you're not breaking functionality.  The test cases aren't included
;; because they are over 100K.
;;
;; If you find an address that mail-extr fails on, please send it to the 
;; maintainer along with what you think the correct results should be.  We do
;; not consider it a bug if mail-extr mangles a comment that does not
;; correspond to a real human full name, although we would prefer that 
;; mail-extr would return the comment as-is.
;;
;; Features:
;;
;; * Full name handling:
;;
;;   * knows where full names can be found in an address.
;;   * avoids using empty comments and quoted text.
;;   * extracts full names from mailbox names.
;;   * recognizes common formats for comments after a full name.
;;   * puts a period and a space after each initial.
;;   * understands & referring to the mailbox name, capitalized.
;;   * strips name prefixes like "Prof.", etc.
;;   * understands what characters can occur in names (not just letters).
;;   * figures out middle initial from mailbox name.
;;   * removes funny nicknames.
;;   * keeps suffixes such as Jr., Sr., III, etc.
;;   * reorders "Last, First" type names.
;;
;; * Address handling:
;;
;;   * parses rfc822 quoted text, comments, and domain literals.
;;   * parses rfc822 multi-line headers.
;;   * does something reasonable with rfc822 GROUP addresses.
;;   * handles many rfc822 noncompliant and garbage addresses.
;;   * canonicalizes addresses (after stripping comments/phrases outside <>).
;;     * converts ! addresses into .UUCP and %-style addresses.
;;     * converts rfc822 ROUTE addresses to %-style addresses.
;;     * truncates %-style addresses at leftmost fully qualified domain name.
;;     * handles local relative precedence of ! vs. % and @ (untested).
;;
;; It does almost no string creation.  It primarily uses the built-in
;; parsing routines with the appropriate syntax tables.  This should
;; result in greater speed.
;;
;; TODO:
;;
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
;; * insert documentation strings!
;; * handle X.400-gatewayed addresses according to RFC 1148.

;;; Change Log: 
;; 
;; Thu Feb 17 17:57:33 1994  Jamie Zawinski (jwz@lucid.com)
;;
;;	* merged with jbw's latest version
;;
;; Wed Feb  9 21:56:27 1994  Jamie Zawinski (jwz@lucid.com)
;;
;;      * high-bit chars in comments weren't treated as word syntax
;;
;; Sat Feb  5 03:13:40 1994  Jamie Zawinski (jwz@lucid.com)
;;
;;      * call replace-match with fixed-case arg
;;
;; Thu Dec 16 21:56:45 1993  Jamie Zawinski (jwz@lucid.com)
;;
;;      * some more cleanup, doc, added provide
;;
;; Tue Mar 23 21:23:18 1993  Joe Wells  (jbw at csd.bu.edu)
;; 
;; 	* Made mail-full-name-prefixes a user-customizable variable.
;;        Allow passing the address as a buffer as well as as a string.
;;        Allow [ and ] as name characters (Finnish character set).
;; 
;; Mon Mar 22 21:20:56 1993  Joe Wells  (jbw at bigbird.bu.edu)
;; 
;; 	* Handle "null" addresses.  Handle = used for spacing in mailbox
;; 	  name.  Fix bug in handling of ROUTE-ADDR-type addresses that are
;; 	  missing their brackets.  Handle uppercase "JR".  Extract full
;; 	  names from X.400 addresses encoded in RFC-822.  Fix bug in
;;        handling of multiple addresses where first has trailing comment.
;;        Handle more kinds of telephone extension lead-ins.
;; 
;; Mon Mar 22 20:16:57 1993  Joe Wells  (jbw at bigbird.bu.edu)
;; 
;; 	* Handle HZ encoding for embedding GB encoded chinese characters.
;; 
;; Mon Mar 22 00:46:12 1993  Joe Wells  (jbw at bigbird.bu.edu)
;; 
;; 	* Fixed too broad matching of ham radio call signs.  Fixed bug in
;; 	  handling an unmatched ' in a name string.  Enhanced recognition
;; 	  of when . in the mailbox name terminates the name portion.
;; 	  Narrowed conversion of . to space to only the necessary
;; 	  situation.  Deal with VMS's stupid date stamps.  Handle a unique
;; 	  way of introducing an alternate address.  Fixed spacing bug I
;; 	  introduced in switching last name order.  Fixed bug in handling
;; 	  address with ! and % but no @.  Narrowed the cases in which
;; 	  certain trailing words are discarded.
;; 
;; Sun Mar 21 21:41:06 1993  Joe Wells  (jbw at bigbird.bu.edu)
;; 
;; 	* Fixed bugs in handling GROUP addresses.  Certain words in the
;; 	  middle of a name no longer terminate it.  Handle LISTSERV list
;;        names.  Ignore comment field containing mailbox name.
;; 
;; Sun Mar 21 14:39:38 1993  Joe Wells  (jbw at bigbird.bu.edu)
;; 
;; 	* Moved variant-method code back into main function.  Handle
;; 	underscores as spaces in comments.  Handle leading nickname.  Add
;; 	flag to ignore single-word names.  Other changes.
;; 
;; Mon Feb  1 22:23:31 1993  Joe Wells  (jbw at bigbird.bu.edu)
;; 
;; 	* Added in changes by Rod Whitby and Jamie Zawinski.  This
;;        includes the flag mail-extr-guess-middle-initial and the fix for
;;        handling multiple addresses correctly.  (Whitby just changed
;;	  a > to a <.)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; User configuration variable definitions.
;;

(defvar mail-extr-guess-middle-initial nil
  "*Whether to try to guess middle initial from mail address.
If true, then when we see an address like \"John Smith <jqs@host.com>\"
we will assume that \"John Q. Smith\" is the fellow's name.")

(defvar mail-extr-ignore-single-names t
  "*Whether to ignore a name that is just a single word.
If true, then when we see an address like \"Idiot <dumb@stupid.com>\"
we will act as though we couldn't find a full name in the address.")

;; Matches a leading title that is not part of the name (does not
;; contribute to uniquely identifying the person).
(defvar mail-extr-full-name-prefixes
  (purecopy
   "\\(Prof\\|D[Rr]\\|Mrs?\\|Rev\\|Rabbi\\|SysOp\\|LCDR\\)\\.?[ \t\n]")
  "*Matches prefixes to the full name that identify a person's position.
These are stripped from the full name because they do not contribute to
uniquely identifying the person.")

(defvar mail-extr-@-binds-tighter-than-! nil
  "*Whether the local mail transport agent looks at ! before @.")

(defvar mail-extr-mangle-uucp nil
  "*Whether to throw away information in UUCP addresses
by translating things like \"foo!bar!baz@host\" into \"baz@bar.UUCP\".")

;;----------------------------------------------------------------------
;; what orderings are meaningful?????
;;(defvar mail-operator-precedence-list '(?! ?% ?@))
;; Right operand of a % or a @ must be a domain name, period.  No other
;; operators allowed.  Left operand of a @ is an address relative to that
;; site.

;; Left operand of a ! must be a domain name.  Right operand is an
;; arbitrary address.
;;----------------------------------------------------------------------



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Constant definitions.
;;

;;           Codes in
;; Names in  ISO 8859-1 Name
;; ISO 10XXX ISO 8859-2 in
;; ISO 6937  ISO 10646  RFC            Swedish
;; etc.      Hex Oct    1345 TeX Split ASCII Description
;; --------- ---------- ---- --- ----- ----- -------------------------------
;; %a        E4  344    a:   \"a ae    {     latin small   a + diaeresis   d
;; %o        F6  366    o:   \"o oe    |     latin small   o + diaeresis   v
;; @a        E5  345    aa   \oa aa    }     latin small   a + ring above  e
;; %u        FC  374    u:   \"u ue    ~     latin small   u + diaeresis   |
;; /e        E9  351    e'   \'e       `     latin small   e + acute       i
;; %A        C4  304    A:   \"A AE    [     latin capital a + diaeresis   D
;; %O        D6  326    O:   \"O OE    \     latin capital o + diaeresis   V
;; @A        C5  305    AA   \oA AA    ]     latin capital a + ring above  E
;; %U        DC  334    U:   \"U UE    ^     latin capital u + diaeresis   \
;; /E        C9  311    E'   \'E       @     latin capital e + acute       I

;; NOTE: @a and @A are not in ISO 8859-2 (the codes mentioned above invoke
;; /l and /L).  Some of this data was retrieved from
;; listserv@jhuvm.hcf.jhu.edu.

;; Any character that can occur in a name, not counting characters that
;; separate parts of a multipart name (hyphen and period).
;; Yes, there are weird people with digits in their names.
;; You will also notice the consideration for the
;; Swedish/Finnish/Norwegian character set.
(defconst mail-extr-all-letters-but-separators
  (purecopy "][A-Za-z{|}'~0-9`\200-\377"))

;; Any character that can occur in a name in an RFC822 address including
;; the separator (hyphen and possibly period) for multipart names.
;; #### should . be in here?
(defconst mail-extr-all-letters
  (purecopy (concat mail-extr-all-letters-but-separators "---")))

;; Any character that can start a name.
;; Keep this set as minimal as possible.
(defconst mail-extr-first-letters (purecopy "A-Za-z\200-\377"))

;; Any character that can end a name.
;; Keep this set as minimal as possible.
(defconst mail-extr-last-letters (purecopy "A-Za-z\200-\377`'."))

(defconst mail-extr-leading-garbage
  (purecopy (format "[^%s]+" mail-extr-first-letters)))

;; (defconst mail-extr-non-name-chars 
;;   (purecopy (concat "^" mail-extr-all-letters ".")))
;; (defconst mail-extr-non-begin-name-chars
;;   (purecopy (concat "^" mail-extr-first-letters)))
;; (defconst mail-extr-non-end-name-chars
;;   (purecopy (concat "^" mail-extr-last-letters)))

;; Matches an initial not followed by both a period and a space. 
;; (defconst mail-extr-bad-initials-pattern
;;   (purecopy 
;;    (format "\\(\\([^%s]\\|\\`\\)[%s]\\)\\(\\.\\([^ ]\\)\\| \\|\\([^%s .]\\)\\|\\'\\)"
;;            mail-extr-all-letters mail-extr-first-letters mail-extr-all-letters)))

;; Matches periods used instead of spaces.  Must not match the period
;; following an initial.
(defconst mail-extr-bad-dot-pattern
  (purecopy
   (format "\\([%s][%s]\\)\\.+\\([%s]\\)"
	   mail-extr-all-letters
	   mail-extr-last-letters
	   mail-extr-first-letters)))

;; Matches an embedded or leading nickname that should be removed.
;; (defconst mail-extr-nickname-pattern
;;   (purecopy
;;    (format "\\([ .]\\|\\`\\)[\"'`\[\(]\\([ .%s]+\\)[\]\"'\)] "
;;            mail-extr-all-letters)))

;; Matches the occurrence of a generational name suffix, and the last
;; character of the preceding name.  This is important because we want to
;; keep such suffixes: they help to uniquely identify the person.
;; *** Perhaps this should be a user-customizable variable.  However, the
;; *** regular expression is fairly tricky to alter, so maybe not.
(defconst mail-extr-full-name-suffix-pattern
  (purecopy
   (format
    "\\(,? ?\\([JjSs][Rr]\\.?\\|V?I+V?\\)\\)\\([^%s]\\([^%s]\\|\\'\\)\\|\\'\\)"
    mail-extr-all-letters mail-extr-all-letters)))

(defconst mail-extr-roman-numeral-pattern (purecopy "V?I+V?\\b"))

;; Matches a trailing uppercase (with other characters possible) acronym.
;; Must not match a trailing uppercase last name or trailing initial
(defconst mail-extr-weird-acronym-pattern
  (purecopy "\\([A-Z]+[-_/]\\|[A-Z][A-Z][A-Z]?\\b\\)"))
      
;; Matches a mixed-case or lowercase name (not an initial).
;; #### Match Latin1 lower case letters here too?
;; (defconst mail-extr-mixed-case-name-pattern
;;   (purecopy
;;    (format
;;     "\\b\\([a-z][%s]*[%s]\\|[%s][%s]*[a-z][%s]*[%s]\\|[%s][%s]*[a-z]\\)"
;;     mail-extr-all-letters mail-extr-last-letters
;;     mail-extr-first-letters mail-extr-all-letters mail-extr-all-letters
;;     mail-extr-last-letters mail-extr-first-letters mail-extr-all-letters)))

;; Matches a trailing alternative address.
;; #### Match Latin1 letters here too?
;; #### Match _ before @ here too?  
(defconst mail-extr-alternative-address-pattern
  (purecopy "\\(aka *\\)?[a-zA-Z.]+[!@][a-zA-Z.]"))

;; Matches a variety of trailing comments not including comma-delimited
;; comments.
(defconst mail-extr-trailing-comment-start-pattern
  (purecopy " [-{]\\|--\\|[+@#></\;]"))

;; Matches a name (not an initial).
;; This doesn't force a word boundary at the end because sometimes a
;; comment is separated by a `-' with no preceding space.
(defconst mail-extr-name-pattern
  (purecopy (format "\\b[%s][%s]*[%s]"
		    mail-extr-first-letters
		    mail-extr-all-letters
		    mail-extr-last-letters)))

(defconst mail-extr-initial-pattern
  (purecopy (format "\\b[%s]\\([. ]\\|\\b\\)" mail-extr-first-letters)))

;; Matches a single name before a comma.
;; (defconst mail-extr-last-name-first-pattern
;;   (purecopy (concat "\\`" mail-extr-name-pattern ",")))

;; Matches telephone extensions.
(defconst mail-extr-telephone-extension-pattern
  (purecopy
   "\\(\\([Ee]xt\\|\\|[Tt]ph\\|[Tt]el\\|[Xx]\\).?\\)? *\\+?[0-9][- 0-9]+"))

;; Matches ham radio call signs.
;; Help from: Mat Maessen N2NJZ <maessm@rpi.edu>, Mark Feit
;; <mark@era.com>, Michael Covington <mcovingt@ai.uga.edu>.
;; Examples: DX504 DX515 K5MRU K8DHK KA9WGN KA9WGN KD3FU KD6EUI KD6HBW
;; KE9TV KF0NV N1API N3FU N3GZE N3IGS N4KCC N7IKQ N9HHU W4YHF W6ANK WA2SUH
;; WB7VZI N2NJZ NR3G KJ4KK AB4UM AL7NI KH6OH WN3KBT N4TMI W1A N0NZO
(defconst mail-extr-ham-call-sign-pattern
  (purecopy "\\b\\(DX[0-9]+\\|[AKNW][A-Z]?[0-9][A-Z][A-Z]?[A-Z]?\\)"))

;; Possible trailing suffixes: "\\(/\\(KT\\|A[AEG]\\|[R0-9]\\)\\)?"
;; /KT == Temporary Technician (has CSC but not "real" license)
;; /AA == Temporary Advanced
;; /AE == Temporary Extra
;; /AG == Temporary General
;; /R  == repeater
;; /#  == stations operating out of home district
;; I don't include these in the regexp above because I can't imagine
;; anyone putting them with their name in an e-mail address.

;; Matches normal single-part name
(defconst mail-extr-normal-name-pattern
  (purecopy (format "\\b[%s][%s]+[%s]"
		    mail-extr-first-letters
		    mail-extr-all-letters-but-separators
		    mail-extr-last-letters)))

;; Matches a single word name.
;; (defconst mail-extr-one-name-pattern
;;   (purecopy (concat "\\`" mail-extr-normal-name-pattern "\\'")))
  
;; Matches normal two names with missing middle initial
;; The first name is not allowed to have a hyphen because this can cause
;; false matches where the "middle initial" is actually the first letter
;; of the second part of the first name.
(defconst mail-extr-two-name-pattern
  (purecopy
   (concat "\\`\\(" mail-extr-normal-name-pattern
	   "\\|" mail-extr-initial-pattern
	   "\\) +\\(" mail-extr-name-pattern "\\)\\(,\\|\\'\\)")))

(defconst mail-extr-listserv-list-name-pattern
  (purecopy "Multiple recipients of list \\([-A-Z]+\\)"))

(defconst mail-extr-stupid-vms-date-stamp-pattern
  (purecopy
   "[0-9][0-9]-[JFMASOND][aepuco][nbrylgptvc]-[0-9][0-9][0-9][0-9] [0-9]+ *"))

;;; HZ -- GB (PRC Chinese character encoding) in ASCII embedding protocol
;;
;; In ASCII mode, a byte is interpreted as an ASCII character, unless a '~' is
;; encountered. The character '~' is an escape character. By convention, it
;; must be immediately followed ONLY by '~', '{' or '\n' (<LF>), with the
;; following special meaning.
;; 
;; o The escape sequence '~~' is interpreted as a '~'.
;; o The escape-to-GB sequence '~{' switches the mode from ASCII to GB.
;; o The escape sequence '~\n' is a line-continuation marker to be consumed
;;   with no output produced.
;; 
;; In GB mode, characters are interpreted two bytes at a time as (pure) GB
;; codes until the escape-from-GB code '~}' is read. This code switches the
;; mode from GB back to ASCII.  (Note that the escape-from-GB code '~}'
;; ($7E7D) is outside the defined GB range.)
(defconst mail-extr-hz-embedded-gb-encoded-chinese-pattern
  (purecopy "~{\\([^~].\\|~[^\}]\\)+~}"))

;; The leading optional lowercase letters are for a bastardized version of
;; the encoding, as is the optional nature of the final slash.
(defconst mail-extr-x400-encoded-address-pattern
  (purecopy "[a-z]?[a-z]?\\(/[A-Za-z]+\\(\\.[A-Za-z]+\\)?=[^/]+\\)+/?\\'"))

(defconst mail-extr-x400-encoded-address-field-pattern-format
  (purecopy "/%s=\\([^/]+\\)\\(/\\|\\'\\)"))

(defconst mail-extr-x400-encoded-address-surname-pattern
  ;; S stands for Surname (family name).
  (purecopy
   (format mail-extr-x400-encoded-address-field-pattern-format "[Ss]")))

(defconst mail-extr-x400-encoded-address-given-name-pattern
  ;; G stands for Given name.
  (purecopy
   (format mail-extr-x400-encoded-address-field-pattern-format "[Gg]")))

(defconst mail-extr-x400-encoded-address-full-name-pattern
  ;; PN stands for Personal Name.  When used it represents the combination
  ;; of the G and S fields.
  ;; "The one system I used having this field asked it with the prompt
  ;; `Personal Name'.  But they mapped it into G and S on outgoing real
  ;; X.400 addresses.  As they mapped G and S into PN on incoming..."
  (purecopy
   (format mail-extr-x400-encoded-address-field-pattern-format "[Pp][Nn]")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Syntax tables used for quick parsing.
;;

(defconst mail-extr-address-syntax-table (make-syntax-table))
(defconst mail-extr-address-comment-syntax-table (make-syntax-table))
(defconst mail-extr-address-domain-literal-syntax-table (make-syntax-table))
(defconst mail-extr-address-text-comment-syntax-table (make-syntax-table))
(defconst mail-extr-address-text-syntax-table (make-syntax-table))
(mapcar
 (function
  (lambda (pair)
    (let ((syntax-table (symbol-value (car pair))))
      (mapcar
       (function
	(lambda (item)
	  (if (eq 2 (length item))
	      ;; modifying syntax of a single character
	      (modify-syntax-entry (car item) (car (cdr item)) syntax-table)
	    ;; modifying syntax of a range of characters
	    (let ((char (nth 0 item))
		  (bound (nth 1 item))
		  (syntax (nth 2 item)))
	      (while (<= char bound)
		(modify-syntax-entry char syntax syntax-table)
		(setq char (1+ char)))))))
       (cdr pair)))))
 '((mail-extr-address-syntax-table
    (?\000 ?\037 "w")			;control characters
    (?\040	 " ")			;SPC
    (?! ?~	 "w")			;printable characters
    (?\177	 "w")			;DEL
    (?\200 ?\377 "w")			;high-bit-on characters
    (?\240	 " ")			;nobreakspace
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
    (?!  ".") ;; this needs to be word-constituent when not in .UUCP mode
    )
   (mail-extr-address-comment-syntax-table
    (?\000 ?\377 "w")
    (?\040 " ")
    (?\240 " ")
    (?\t " ")
    (?\r " ")
    (?\n " ")
    (?\( "\(\)")
    (?\) "\)\(")
    (?\\ "\\"))
   (mail-extr-address-domain-literal-syntax-table
    (?\000 ?\377 "w")
    (?\040 " ")
    (?\240 " ")
    (?\t " ")
    (?\r " ")
    (?\n " ")
    (?\[ "\(\]")			;??????
    (?\] "\)\[")			;??????
    (?\\ "\\"))
   (mail-extr-address-text-comment-syntax-table
    (?\000 ?\377 "w")
    (?\040 " ")
    (?\240 " ")
    (?\t " ")
    (?\r " ")
    (?\n " ")
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
   (mail-extr-address-text-syntax-table
    (?\000 ?\177 ".")
    (?\200 ?\377 "w")
    (?\040 " ")
    (?\t " ")
    (?\r " ")
    (?\n " ")
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Utility functions and macros.
;;

(defmacro mail-extr-delete-char (n)
  ;; in v19, delete-char is compiled as a function call, but delete-region
  ;; is byte-coded, so it's much much faster.
  (list 'delete-region '(point) (list '+ '(point) n)))

(defmacro mail-extr-skip-whitespace-forward ()
  ;; v19 fn skip-syntax-forward is more tasteful, but not byte-coded.
  '(skip-chars-forward " \t\n\r\240"))

(defmacro mail-extr-skip-whitespace-backward ()
  ;; v19 fn skip-syntax-backward is more tasteful, but not byte-coded.
  '(skip-chars-backward " \t\n\r\240"))


(defmacro mail-extr-undo-backslash-quoting (beg end)
  (`(save-excursion
      (save-restriction
	(narrow-to-region (, beg) (, end))
	(goto-char (point-min))
	;; undo \ quoting
	(while (search-forward "\\" nil t)
	  (mail-extr-delete-char -1)
	  (or (eobp)
	      (forward-char 1))
	  )))))

(defmacro mail-extr-nuke-char-at (pos)
  (` (save-excursion
       (goto-char (, pos))
       (mail-extr-delete-char 1)
       (insert ?\ ))))

(put 'mail-extr-nuke-outside-range
     'edebug-form-spec '(symbolp &optional form form atom))

(defmacro mail-extr-nuke-outside-range (list-symbol
					beg-symbol end-symbol
					&optional no-replace)
  ;; LIST-SYMBOL names a variable holding a list of buffer positions
  ;; BEG-SYMBOL and END-SYMBOL name variables delimiting a range
  ;; Each element of LIST-SYMBOL which lies outside of the range is
  ;;  deleted from the list.
  ;; Unless NO-REPLACE is true, at each of the positions in LIST-SYMBOL
  ;;  which lie outside of the range, one character at that position is
  ;;  replaced with a SPC.
  (or (memq no-replace '(t nil))
      (error "no-replace must be t or nil, evalable at macroexpand-time."))
  (` (let ((temp (, list-symbol))
	   ch)
       (while temp
	 (setq ch (car temp))
	 (cond ((or (> ch (, end-symbol))
		    (< ch (, beg-symbol)))
		(,@ (if no-replace
			nil
		      (` ((mail-extr-nuke-char-at ch)))))
		(setcar temp nil)))
	 (setq temp (cdr temp)))
       (setq (, list-symbol) (delq nil (, list-symbol))))))

(defun mail-extr-demarkerize (marker)
  ;; if arg is a marker, destroys the marker, then returns the old value.
  ;; otherwise returns the arg.
  (if (markerp marker)
      (let ((temp (marker-position marker)))
	(set-marker marker nil)
	temp)
    marker))

(defun mail-extr-markerize (pos)
  ;; coerces pos to a marker if non-nil.
  (if (or (markerp pos) (null pos))
      pos
    (copy-marker pos)))

(defmacro mail-extr-last (list)
  ;; Returns last element of LIST.
  ;; Could be a subst.
  (` (let ((list (, list)))
       (while (not (null (cdr list)))
	 (setq list (cdr list)))
       (car list))))
  
(defmacro mail-extr-safe-move-sexp (arg)
  ;; Safely skip over one balanced sexp, if there is one.  Return t if success.
  (` (condition-case error
	 (progn
	   (goto-char (scan-sexps (point) (, arg)))
	   t)
       (error
	;; #### kludge kludge kludge kludge kludge kludge kludge !!!
	(if (string-equal (nth 1 error) "Unbalanced parentheses")
	    nil
	  (while t
	    (signal (car error) (cdr error))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The main function to grind addresses
;;

(defvar disable-initial-guessing-flag)	; dynamic assignment
(defvar cbeg)				; dynamic assignment
(defvar cend)				; dynamic assignment

;;;###autoload
(defun mail-extract-address-components (address)
  "Given an RFC-822 ADDRESS, extract full name and canonical address.
Returns a list of the form (FULL-NAME CANONICAL-ADDRESS).
If no name can be extracted, FULL-NAME will be nil.
ADDRESS may be a string or a buffer.  If it is a buffer, the visible 
 (narrowed) portion of the buffer will be interpreted as the address.
 (This feature exists so that the clever caller might be able to avoid
 consing a string.)
If ADDRESS contains more than one RFC-822 address, only the first is
 returned.  Some day this function may be extended to extract multiple
 addresses, or perhaps return the position at which parsing stopped."
  (let ((canonicalization-buffer (get-buffer-create " *canonical address*"))
	(extraction-buffer (get-buffer-create " *extract address components*"))
	char
;;	multiple-addresses
	<-pos >-pos @-pos :-pos ,-pos !-pos %-pos \;-pos
	group-:-pos group-\;-pos route-addr-:-pos
	record-pos-symbol
	first-real-pos last-real-pos
	phrase-beg phrase-end
	cbeg cend			; dynamically set from -voodoo
	quote-beg quote-end
	atom-beg atom-end
	mbox-beg mbox-end
	\.-ends-name
	temp
;;	name-suffix
	fi mi li			; first, middle, last initial
	saved-%-pos saved-!-pos saved-@-pos
	domain-pos \.-pos insert-point
;;	mailbox-name-processed-flag
	disable-initial-guessing-flag	; dynamically set from -voodoo
	)
    
    (save-excursion
      (set-buffer extraction-buffer)
      (fundamental-mode)
      (kill-all-local-variables)
      (buffer-disable-undo extraction-buffer)
      (set-syntax-table mail-extr-address-syntax-table)
      (widen)
      (erase-buffer)
      (setq case-fold-search nil)
      
      ;; Insert extra space at beginning to allow later replacement with <
      ;; without having to move markers.
      (insert ?\ )

      ;; Insert the address itself.
      (cond ((stringp address)
	     (insert address))
	    ((bufferp address)
	     (insert-buffer-substring address))
	    (t
	     (error "Illegal address: %s" address)))
      
      ;; stolen from rfc822.el
      ;; Unfold multiple lines.
      (goto-char (point-min))
      (while (re-search-forward "\\([^\\]\\(\\\\\\\\\\)*\\)\n[ \t]" nil t)
	(replace-match "\\1 " t))
      
      ;; first pass grabs useful information about address
      (goto-char (point-min))
      (while (progn
	       (mail-extr-skip-whitespace-forward)
	       (not (eobp)))
	(setq char (char-after (point)))
	(or first-real-pos
	    (if (not (eq char ?\())
		(setq first-real-pos (point))))
	(cond
	 ;; comment
	 ((eq char ?\()
	  (set-syntax-table mail-extr-address-comment-syntax-table)
	  ;; only record the first non-empty comment's position
	  (if (and (not cbeg)
		   (save-excursion
		     (forward-char 1)
		     (mail-extr-skip-whitespace-forward)
		     (not (eq ?\) (char-after (point))))))
	      (setq cbeg (point)))
	  ;; TODO: don't record if unbalanced
	  (or (mail-extr-safe-move-sexp 1)
	      (forward-char 1))
	  (set-syntax-table mail-extr-address-syntax-table)
	  (if (and cbeg
		   (not cend))
	      (setq cend (point))))
	 ;; quoted text
	 ((eq char ?\")
	  ;; only record the first non-empty quote's position
	  (if (and (not quote-beg)
		   (save-excursion
		     (forward-char 1)
		     (mail-extr-skip-whitespace-forward)
		     (not (eq ?\" (char-after (point))))))
	      (setq quote-beg (point)))
	  ;; TODO: don't record if unbalanced
	  (or (mail-extr-safe-move-sexp 1)
	      (forward-char 1))
	  (if (and quote-beg
		   (not quote-end))
	      (setq quote-end (point))))
	 ;; domain literals
	 ((eq char ?\[)
	  (set-syntax-table mail-extr-address-domain-literal-syntax-table)
	  (or (mail-extr-safe-move-sexp 1)
	      (forward-char 1))
	  (set-syntax-table mail-extr-address-syntax-table))
	 ;; commas delimit addresses when outside < > pairs.
	 ((and (eq char ?,)
	       (or (and (null <-pos)
			;; Handle ROUTE-ADDR address that is missing its <.
			(not (eq ?@ (char-after (1+ (point))))))
		   (and >-pos
			;; handle weird munged addresses
			;; BUG FIX: This test was reversed.  Thanks to the
			;; brilliant Rod Whitby <rwhitby@research.canon.oz.au>
			;; for discovering this!
			(< (mail-extr-last <-pos) (car >-pos)))))
;; It'd be great if some day this worked, but for now, punt.
;;	  (setq multiple-addresses t)
;;	  ;; *** Why do I want this:
;;	  (mail-extr-delete-char 1)
;;	  (narrow-to-region (point-min) (point))
	  (delete-region (point) (point-max))
	  (setq char ?\() ; HAVE I NO SHAME??
	  )
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
	  (mail-extr-nuke-char-at (point))
	  (forward-char 1))
	 (t
	  (forward-word 1)))
	(or (eq char ?\()
	    ;; At the end of first address of a multiple address header.
	    (and (eq char ?,)
		 (eobp))
	    (setq last-real-pos (point))))
      
      ;; Use only the leftmost <, if any.  Replace all others with spaces.
      (while (cdr <-pos)
	(mail-extr-nuke-char-at (car <-pos))
	(setq <-pos (cdr <-pos)))
      
      ;; Use only the rightmost >, if any.  Replace all others with spaces.
      (while (cdr >-pos)
	(mail-extr-nuke-char-at (nth 1 >-pos))
	(setcdr >-pos (nthcdr 2 >-pos)))
      
      ;; If multiple @s and a :, but no < and >, insert around buffer.
      ;; Example: @foo.bar.dom,@xxx.yyy.zzz:mailbox@aaa.bbb.ccc
      ;; This commonly happens on the UUCP "From " line.  Ugh.
      (cond ((and (> (length @-pos) 1)
		  (eq 1 (length :-pos))	;TODO: check if between last two @s
		  (not \;-pos)
		  (not <-pos))
	     (goto-char (point-min))
	     (mail-extr-delete-char 1)
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
	     (mail-extr-nuke-char-at (car >-pos))
	     (setq >-pos nil)))

      ;; Turn >-pos and <-pos into non-lists
      (setq >-pos (car >-pos)
	    <-pos (car <-pos))
      
      ;; Trim other punctuation lists of items outside < > pair to handle
      ;; stupid MTAs.
      (cond (<-pos			; don't need to check >-pos also
	     ;; handle bozo software that violates RFC 822 by sticking
	     ;; punctuation marks outside of a < > pair
	     (mail-extr-nuke-outside-range @-pos <-pos >-pos t)
	     ;; RFC 822 says nothing about these two outside < >, but
	     ;; remove those positions from the lists to make things
	     ;; easier.
	     (mail-extr-nuke-outside-range !-pos <-pos >-pos t)
	     (mail-extr-nuke-outside-range %-pos <-pos >-pos t)))
      
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
		   (mail-extr-nuke-char-at temp)
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
	       (mail-extr-nuke-char-at temp))
	      ((and (or (not group-:-pos)
			(> temp group-:-pos))
		    (not group-\;-pos))
	       (setq group-\;-pos temp))))
      
      ;; Nuke unmatched GROUP syntax characters.
      (cond ((and group-:-pos (not group-\;-pos))
	     ;; *** Do I really need to erase it?
	     (mail-extr-nuke-char-at group-:-pos)
	     (setq group-:-pos nil)))
      (cond ((and group-\;-pos (not group-:-pos))
	     ;; *** Do I really need to erase it?
	     (mail-extr-nuke-char-at group-\;-pos)
	     (setq group-\;-pos nil)))
      
      ;; Handle junk like ";@host.company.dom" that sendmail adds.
      ;; **** should I remember comment positions?
      (cond
       (group-\;-pos
	;; this is fine for now
	(mail-extr-nuke-outside-range !-pos group-:-pos group-\;-pos t)
	(mail-extr-nuke-outside-range @-pos group-:-pos group-\;-pos t)
	(mail-extr-nuke-outside-range %-pos group-:-pos group-\;-pos t)
	(mail-extr-nuke-outside-range ,-pos group-:-pos group-\;-pos t)
	(and last-real-pos
	     (> last-real-pos (1+ group-\;-pos))
	     (setq last-real-pos (1+ group-\;-pos)))
	;; *** This may be wrong:
        (and cend
             (> cend group-\;-pos)
             (setq cend nil
                   cbeg nil))
	(and quote-end
	     (> quote-end group-\;-pos)
	     (setq quote-end nil
		   quote-beg nil))
	;; This was both wrong and unnecessary:
	;;(narrow-to-region (point-min) group-\;-pos)

	;; *** The entire handling of GROUP addresses seems rather lame.
	;; *** It deserves a complete rethink, except that these addresses
	;; *** are hardly ever seen.
	))
      
      ;; Any commas must be between < and : of ROUTE-ADDR.  Nuke any
      ;; others.
      ;; Hell, go ahead an nuke all of the commas.
      ;; **** This will cause problems when we start handling commas in
      ;; the PHRASE part .... no it won't ... yes it will ... ?????
      (mail-extr-nuke-outside-range ,-pos 1 1)
      
      ;; can only have multiple @s inside < >.  The fact that some MTAs
      ;; put de-bracketed ROUTE-ADDRs in the UUCP-style "From " line is
      ;; handled above.
      
      ;; Locate PHRASE part of ROUTE-ADDR.
      (cond (<-pos
	     (goto-char <-pos)
	     (mail-extr-skip-whitespace-backward)
	     (setq phrase-end (point))
	     (goto-char (or ;;group-:-pos
			    (point-min)))
	     (mail-extr-skip-whitespace-forward)
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
	       (mail-extr-delete-char 1)
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
	     (mail-extr-delete-char -1)
	     (mail-extr-nuke-char-at route-addr-:-pos)
	     (mail-extr-demarkerize route-addr-:-pos)
	     (setq route-addr-:-pos nil
		   >-pos (mail-extr-demarkerize >-pos)
		   %-pos (mapcar 'mail-extr-demarkerize %-pos))))
      
      ;; de-listify @-pos
      (setq @-pos (car @-pos))
      
      ;; TODO: remove comments in the middle of an address
      
      (set-buffer canonicalization-buffer)
      (fundamental-mode)
      (kill-all-local-variables)
      (buffer-disable-undo canonicalization-buffer)
      (set-syntax-table mail-extr-address-syntax-table)
      (setq case-fold-search nil)
      
      (widen)
      (erase-buffer)
      (insert-buffer-substring extraction-buffer)
      
      (if <-pos
	  (narrow-to-region (progn
			      (goto-char (1+ <-pos))
			      (mail-extr-skip-whitespace-forward)
			      (point))
			    >-pos)
	(if (and first-real-pos last-real-pos)
	    (narrow-to-region first-real-pos last-real-pos)
	  ;; ****** Oh no!  What if the address is completely empty!
	  ;; *** Is this correct?
	  (narrow-to-region (point-max) (point-max))
	  ))
      
      (and @-pos %-pos
	   (mail-extr-nuke-outside-range %-pos (point-min) @-pos))
      (and %-pos !-pos
	   (mail-extr-nuke-outside-range !-pos (point-min) (car %-pos)))
      (and @-pos !-pos (not %-pos)
	   (mail-extr-nuke-outside-range !-pos (point-min) @-pos))
      
      ;; Error condition:?? (and %-pos (not @-pos))
      
      ;; WARNING: THIS CODE IS DUPLICATED BELOW.
      (cond ((and %-pos
		  (not @-pos))
	     (goto-char (car %-pos))
	     (mail-extr-delete-char 1)
	     (setq @-pos (point))
	     (insert "@")
	     (setq %-pos (cdr %-pos))))

      (if mail-extr-mangle-uucp
      (cond (!-pos
	     ;; **** I don't understand this save-restriction and the
	     ;; narrow-to-region inside it.  Why did I do that?
	     (save-restriction
	       (cond ((and @-pos
			   mail-extr-@-binds-tighter-than-!)
		      (goto-char @-pos)
		      (setq %-pos (cons (point) %-pos)
			    @-pos nil)
		      (mail-extr-delete-char 1)
		      (insert "%")
		      (setq insert-point (point-max)))
		     (mail-extr-@-binds-tighter-than-!
		      (setq insert-point (point-max)))
		     (%-pos
		      (setq insert-point (mail-extr-last %-pos)
			    saved-%-pos (mapcar 'mail-extr-markerize %-pos)
			    %-pos nil
			    @-pos (mail-extr-markerize @-pos)))
		     (@-pos
		      (setq insert-point @-pos)
		      (setq @-pos (mail-extr-markerize @-pos)))
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
		 (mail-extr-delete-char 1)
		 (or (save-excursion
		       (mail-extr-safe-move-sexp -1)
		       (mail-extr-skip-whitespace-backward)
		       (eq ?. (preceding-char)))
		     (insert-before-markers
		      (if (save-excursion
			    (mail-extr-skip-whitespace-backward)
			    (eq ?. (preceding-char)))
			  ""
			".")
		      "uucp"))
		 (setq !-pos (cdr !-pos))))
	     (and saved-%-pos
		  (setq %-pos (append (mapcar 'mail-extr-demarkerize
					      saved-%-pos)
				      %-pos)))
	     (setq @-pos (mail-extr-demarkerize @-pos))
	     (narrow-to-region (1+ saved-!-pos) (point-max)))))

      ;; WARNING: THIS CODE IS DUPLICATED ABOVE.
      (cond ((and %-pos
		  (not @-pos))
	     (goto-char (car %-pos))
	     (mail-extr-delete-char 1)
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
		 (mail-extr-skip-whitespace-backward)
		 (save-excursion
		   (mail-extr-safe-move-sexp -1)
		   (setq domain-pos (point))
		   (mail-extr-skip-whitespace-backward)
		   (setq \.-pos (eq ?. (preceding-char))))
		 (cond ((and \.-pos
			     ;; #### string consing
			     (let ((s (intern-soft
				       (buffer-substring domain-pos (point))
				       mail-extr-all-top-level-domains)))
			       (and s (get s 'domain-name))))
			(narrow-to-region (point-min) (point))
			(goto-char (car temp))
			(mail-extr-delete-char 1)
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
      
      ;; Decide what part of the address to search to find the full name.
      (cond (
	     ;; Example: "First M. Last" <fml@foo.bar.dom>
	     (and phrase-beg
		  (eq quote-beg phrase-beg)
		  (<= quote-end phrase-end))
	     (narrow-to-region (1+ quote-beg) (1- quote-end))
	     (mail-extr-undo-backslash-quoting (point-min) (point-max)))

	    ;; Example: First Last <fml@foo.bar.dom>
	    (phrase-beg
	     (narrow-to-region phrase-beg phrase-end))

	    ;; Example: fml@foo.bar.dom (First M. Last)
	    (cbeg
	     (narrow-to-region (1+ cbeg) (1- cend))
	     (mail-extr-undo-backslash-quoting (point-min) (point-max))
	     
	     ;; Deal with spacing problems
	     (goto-char (point-min))
;	     (cond ((not (search-forward " " nil t))
;		    (goto-char (point-min))
;		    (cond ((search-forward "_" nil t)
;			   ;; Handle the *idiotic* use of underlines as spaces.
;			   ;; Example: fml@foo.bar.dom (First_M._Last)
;			   (goto-char (point-min))
;			   (while (search-forward "_" nil t)
;			     (replace-match " " t)))
;			  ((search-forward "." nil t)
;			   ;; Fix . used as space
;			   ;; Example: danj1@cb.att.com (daniel.jacobson)
;			   (goto-char (point-min))
;			   (while (re-search-forward mail-extr-bad-dot-pattern nil t)
;			     (replace-match "\\1 \\2" t))))))
	     )
	    
	    ;; Otherwise we try to get the name from the mailbox portion
	    ;; of the address.
	    ;; Example: First_M_Last@foo.bar.dom
	    (t
	     ;; *** Work in canon buffer instead?  No, can't.  Hmm.
	     (goto-char (point-max))
	     (narrow-to-region (point) (point))
	     (insert-buffer-substring canonicalization-buffer
				      mbox-beg mbox-end)
	     (goto-char (point-min))
	     
	     ;; Example: First_Last.XXX@foo.bar.dom
	     (setq \.-ends-name (re-search-forward "[_0-9]" nil t))
	     
	     (goto-char (point-min))

	     (if (not mail-extr-mangle-uucp)
		 (modify-syntax-entry ?! "w" (syntax-table)))

	     (while (progn
		      (mail-extr-skip-whitespace-forward)
		      (not (eobp)))
	       (setq char (char-after (point)))
	       (cond
		((eq char ?\")
		 (setq quote-beg (point))
		 (or (mail-extr-safe-move-sexp 1)
		     ;; TODO: handle this error condition!!!!!
		     (forward-char 1))
		 ;; take into account deletions
		 (setq quote-end (- (point) 2))
		 (save-excursion
		   (backward-char 1)
		   (mail-extr-delete-char 1)
		   (goto-char quote-beg)
		   (mail-extr-delete-char 1))
		 (mail-extr-undo-backslash-quoting quote-beg quote-end)
		 (or (eq ?\  (char-after (point)))
		     (insert " "))
;;		 (setq mailbox-name-processed-flag t)
		 (setq \.-ends-name t))
		((eq char ?.)
		 (if (memq (char-after (1+ (point))) '(?_ ?=))
		     (progn
		       (forward-char 1)
		       (mail-extr-delete-char 1)
		       (insert ?\ ))
		   (if \.-ends-name
		       (narrow-to-region (point-min) (point))
		     (mail-extr-delete-char 1)
		     (insert " ")))
;;		 (setq mailbox-name-processed-flag t)
		 )
		((memq (char-syntax char) '(?. ?\\))
		 (mail-extr-delete-char 1)
		 (insert " ")
;;		 (setq mailbox-name-processed-flag t)
		 )
		(t
		 (setq atom-beg (point))
		 (forward-word 1)
		 (setq atom-end (point))
		 (goto-char atom-beg)
		 (save-restriction
		   (narrow-to-region atom-beg atom-end)
		   (cond
		    
		    ;; Handle X.400 addresses encoded in RFC-822.
		    ;; *** Shit!  This has to handle the case where it is
		    ;; *** embedded in a quote too!
		    ;; *** Shit!  The input is being broken up into atoms
		    ;; *** by periods!
		    ((looking-at mail-extr-x400-encoded-address-pattern)
		     
		     ;; Copy the contents of the individual fields that
		     ;; might hold name data to the beginning.
		     (mapcar
		      (function
		       (lambda (field-pattern)
			 (cond
			  ((save-excursion
			     (re-search-forward field-pattern nil t))
			   (insert-buffer-substring (current-buffer)
						    (match-beginning 1)
						    (match-end 1))
			   (insert " ")))))
		      (list mail-extr-x400-encoded-address-given-name-pattern
			    mail-extr-x400-encoded-address-surname-pattern
			    mail-extr-x400-encoded-address-full-name-pattern))
		     
		     ;; Discard the rest, since it contains stuff like
		     ;; routing information, not part of a name.
		     (mail-extr-skip-whitespace-backward)
		     (delete-region (point) (point-max))
		     
		     ;; Handle periods used for spacing.
		     (while (re-search-forward mail-extr-bad-dot-pattern nil t)
		       (replace-match "\\1 \\2" t))
		     
;;		     (setq mailbox-name-processed-flag t)
		     )
		    
		    ;; Handle normal addresses.
		    (t
		     (goto-char (point-min))
		     ;; Handle _ and = used for spacing.
		     (while (re-search-forward "\\([^_=]+\\)[_=]" nil t)
		       (replace-match "\\1 " t)
;;		       (setq mailbox-name-processed-flag t)
		       )
		     (goto-char (point-max))))))))

	     ;; undo the dirty deed
	     (if (not mail-extr-mangle-uucp)
		 (modify-syntax-entry ?! "." (syntax-table)))
	     ;;
	     ;; If we derived the name from the mailbox part of the address,
	     ;; and we only got one word out of it, don't treat that as a
	     ;; name.  "foo@bar" --> (nil "foo@bar"), not ("foo" "foo@bar")
             ;; (if (not mailbox-name-processed-flag)
             ;;     (delete-region (point-min) (point-max)))
	     ))
      
      (set-syntax-table mail-extr-address-text-syntax-table)
      
      (mail-extr-voodoo mbox-beg mbox-end canonicalization-buffer)
      (goto-char (point-min))

      ;; If name is "First Last" and userid is "F?L", then assume
      ;; the middle initial is the second letter in the userid.
      ;; Initial code by Jamie Zawinski <jwz@lucid.com>
      ;; *** Make it work when there's a suffix as well.
      (goto-char (point-min))
      (cond ((and mail-extr-guess-middle-initial
		  (not disable-initial-guessing-flag)
		  (eq 3 (- mbox-end mbox-beg))
		  (progn
		    (goto-char (point-min))
		    (looking-at mail-extr-two-name-pattern)))
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
      
      ;; Nuke name if it is the same as mailbox name.
      (let ((buffer-length (- (point-max) (point-min)))
	    (i 0)
	    (names-match-flag t))
	(cond ((and (> buffer-length 0)
		    (eq buffer-length (- mbox-end mbox-beg)))
	       (goto-char (point-max))
	       (insert-buffer-substring canonicalization-buffer
					mbox-beg mbox-end)
	       (while (and names-match-flag
			   (< i buffer-length))
		 (or (eq (downcase (char-after (+ i (point-min))))
			 (downcase
			  (char-after (+ i buffer-length (point-min)))))
		     (setq names-match-flag nil))
		 (setq i (1+ i)))
	       (delete-region (+ (point-min) buffer-length) (point-max))
	       (if names-match-flag
		   (narrow-to-region (point) (point))))))
      
      ;; Nuke name if it's just one word.
      (goto-char (point-min))
      (and mail-extr-ignore-single-names
	   (not (re-search-forward "[- ]" nil t))
	   (narrow-to-region (point) (point)))
      
      ;; Result
      (list (if (not (= (point-min) (point-max)))
		(buffer-string))
	    (progn
	      (set-buffer canonicalization-buffer)
	      (if (not (= (point-min) (point-max)))
		  (buffer-string))))
      )))

(defun mail-extr-voodoo (mbox-beg mbox-end canonicalization-buffer)
  (let ((word-count 0)
	(case-fold-search nil)
	mixed-case-flag lower-case-flag ;;upper-case-flag
	suffix-flag last-name-comma-flag
	;;cbeg cend
	initial
	begin-again-flag
	drop-this-word-if-trailing-flag
	drop-last-word-if-trailing-flag
	word-found-flag
	this-word-beg last-word-beg
	name-beg name-end
	name-done-flag
	)
    (save-excursion
      (set-syntax-table mail-extr-address-text-syntax-table)
      
      ;; This was moved above.
      ;; Fix . used as space
      ;; But it belongs here because it occurs not only as
      ;;   rypens@reks.uia.ac.be (Piet.Rypens)
      ;; but also as
      ;;   "Piet.Rypens" <rypens@reks.uia.ac.be>
      ;;(goto-char (point-min))
      ;;(while (re-search-forward mail-extr-bad-dot-pattern nil t)
      ;;  (replace-match "\\1 \\2" t))

      (cond ((not (search-forward " " nil t))
	     (goto-char (point-min))
	     (cond ((search-forward "_" nil t)
		    ;; Handle the *idiotic* use of underlines as spaces.
		    ;; Example: fml@foo.bar.dom (First_M._Last)
		    (goto-char (point-min))
		    (while (search-forward "_" nil t)
		      (replace-match " " t)))
		   ((search-forward "." nil t)
		    ;; Fix . used as space
		    ;; Example: danj1@cb.att.com (daniel.jacobson)
		    (goto-char (point-min))
		    (while (re-search-forward mail-extr-bad-dot-pattern nil t)
		      (replace-match "\\1 \\2" t))))))


      ;; Loop over the words (and other junk) in the name.
      (goto-char (point-min))
      (while (not name-done-flag)
	
	(cond (word-found-flag
	       ;; Last time through this loop we skipped over a word.
	       (setq last-word-beg this-word-beg)
	       (setq drop-last-word-if-trailing-flag
		     drop-this-word-if-trailing-flag)
	       (setq word-found-flag nil)))

	(cond (begin-again-flag
	       ;; Last time through the loop we found something that
	       ;; indicates we should pretend we are beginning again from
	       ;; the start.
	       (setq word-count 0)
	       (setq last-word-beg nil)
	       (setq drop-last-word-if-trailing-flag nil)
	       (setq mixed-case-flag nil)
	       (setq lower-case-flag nil)
;;	       (setq upper-case-flag nil)
	       (setq begin-again-flag nil)
	       ))
	
	;; Initialize for this iteration of the loop.
	(mail-extr-skip-whitespace-forward)
	(if (eq word-count 0) (narrow-to-region (point) (point-max)))
	(setq this-word-beg (point))
	(setq drop-this-word-if-trailing-flag nil)
	
	;; Decide what to do based on what we are looking at.
	(cond
	 
	 ;; Delete title
	 ((and (eq word-count 0)
	       (looking-at mail-extr-full-name-prefixes))
	  (goto-char (match-end 0))
	  (narrow-to-region (point) (point-max)))
	 
	 ;; Stop after name suffix
	 ((and (>= word-count 2)
	       (looking-at mail-extr-full-name-suffix-pattern))
	  (mail-extr-skip-whitespace-backward)
	  (setq suffix-flag (point))
	  (if (eq ?, (following-char))
	      (forward-char 1)
	    (insert ?,))
	  ;; Enforce at least one space after comma
	  (or (eq ?\  (following-char))
	      (insert ?\ ))
	  (mail-extr-skip-whitespace-forward)
	  (cond ((memq (following-char) '(?j ?J ?s ?S))
		 (capitalize-word 1)
		 (if (eq (following-char) ?.)
		     (forward-char 1)
		   (insert ?.)))
		(t
		 (upcase-word 1)))
	  (setq word-found-flag t)
	  (setq name-done-flag t))
	 
	 ;; Handle SCA names
	 ((looking-at "MKA \\(.+\\)")	; "Mundanely Known As"
	  (goto-char (match-beginning 1))
	  (narrow-to-region (point) (point-max))
	  (setq begin-again-flag t))
	 
	 ;; Check for initial last name followed by comma
	 ((and (eq ?, (following-char))
	       (eq word-count 1))
	  (forward-char 1)
	  (setq last-name-comma-flag t)
	  (or (eq ?\  (following-char))
	      (insert ?\ )))
	 
	 ;; Stop before trailing comma-separated comment
	 ;; THIS CASE MUST BE AFTER THE PRECEDING CASES.
	 ;; *** This case is redundant???
	 ;;((eq ?, (following-char))
	 ;; (setq name-done-flag t))
	 
	 ;; Delete parenthesized/quoted comment/nickname
	 ((memq (following-char) '(?\( ?\{ ?\[ ?\" ?\' ?\`))
	  (setq cbeg (point))
	  (set-syntax-table mail-extr-address-text-comment-syntax-table)
	  (cond ((memq (following-char) '(?\' ?\`))
		 (or (search-forward "'" nil t
				     (if (eq ?\' (following-char)) 2 1))
		     (mail-extr-delete-char 1)))
		(t
		 (or (mail-extr-safe-move-sexp 1)
		     (goto-char (point-max)))))
	  (set-syntax-table mail-extr-address-text-syntax-table)
	  (setq cend (point))
	  (cond
	   ;; Handle case of entire name being quoted
	   ((and (eq word-count 0)
		 (looking-at " *\\'")
		 (>= (- cend cbeg) 2))
	    (narrow-to-region (1+ cbeg) (1- cend))
	    (goto-char (point-min)))
	   (t
	    ;; Handle case of quoted initial
	    (if (and (or (= 3 (- cend cbeg))
			 (and (= 4 (- cend cbeg))
			      (eq ?. (char-after (+ 2 cbeg)))))
		     (not (looking-at " *\\'")))
		(setq initial (char-after (1+ cbeg)))
	      (setq initial nil))
	    (delete-region cbeg cend)
	    (if initial
		(insert initial ". ")))))
	 
	 ;; Handle & substitution
	 ((and (or (bobp)
		   (eq ?\  (preceding-char)))
	       (looking-at "&\\( \\|\\'\\)"))
	  (mail-extr-delete-char 1)
	  (capitalize-region
	   (point)
	   (progn
	     (insert-buffer-substring canonicalization-buffer
				      mbox-beg mbox-end)
	     (point)))
	  (setq disable-initial-guessing-flag t)
	  (setq word-found-flag t))
	 
	 ;; Handle *Stupid* VMS date stamps
	 ((looking-at mail-extr-stupid-vms-date-stamp-pattern)
	  (replace-match "" t))
	 
	 ;; Handle Chinese characters.
	 ((looking-at mail-extr-hz-embedded-gb-encoded-chinese-pattern)
	  (goto-char (match-end 0))
	  (setq word-found-flag t))
	 
	 ;; Skip initial garbage characters.
	 ;; THIS CASE MUST BE AFTER THE PRECEDING CASES.
	 ((and (eq word-count 0)
	       (looking-at mail-extr-leading-garbage))
	  (goto-char (match-end 0))
	  ;; *** Skip backward over these???
	  ;; (skip-chars-backward "& \"")
	  (narrow-to-region (point) (point-max)))
	 
	 ;; Various stopping points
	 ((or
	   
	   ;; Stop before ALL CAPS acronyms, if preceded by mixed-case
	   ;; words.  Example: XT-DEM.
	   (and (>= word-count 2)
		mixed-case-flag
		(looking-at mail-extr-weird-acronym-pattern)
		(not (looking-at mail-extr-roman-numeral-pattern)))
	   
	   ;; Stop before trailing alternative address
	   (looking-at mail-extr-alternative-address-pattern)
	   
	   ;; Stop before trailing comment not introduced by comma
	   ;; THIS CASE MUST BE AFTER AN EARLIER CASE.
	   (looking-at mail-extr-trailing-comment-start-pattern)
	   
	   ;; Stop before telephone numbers
	   (looking-at mail-extr-telephone-extension-pattern))
	  (setq name-done-flag t))
	 
	 ;; Delete ham radio call signs
	 ((looking-at mail-extr-ham-call-sign-pattern)
	  (delete-region (match-beginning 0) (match-end 0)))
	 
	 ;; Fixup initials
	 ((looking-at mail-extr-initial-pattern)
	  (or (eq (following-char) (upcase (following-char)))
	      (setq lower-case-flag t))
	  (forward-char 1)
	  (if (eq ?. (following-char))
	      (forward-char 1)
	    (insert ?.))
	  (or (eq ?\  (following-char))
	      (insert ?\ ))
	  (setq word-found-flag t))
	 
	 ;; Handle BITNET LISTSERV list names.
	 ((and (eq word-count 0)
	       (looking-at mail-extr-listserv-list-name-pattern))
	  (narrow-to-region (match-beginning 1) (match-end 1))
	  (setq word-found-flag t)
	  (setq name-done-flag t))
	 
	 ;; Regular name words
	 ((looking-at mail-extr-name-pattern)
	  (setq name-beg (point))
	  (setq name-end (match-end 0))
	  
	  ;; Certain words will be dropped if they are at the end.
	  (and (>= word-count 2)
	       (not lower-case-flag)
	       (or
		;; A trailing 4-or-more letter lowercase words preceded by
		;; mixed case or uppercase words will be dropped.
		(looking-at "[a-z][a-z][a-z][a-z]+[ \t]*\\'")
		;; Drop a trailing word which is terminated with a period.
		(eq ?. (char-after (1- name-end))))
	       (setq drop-this-word-if-trailing-flag t))
	  
	  ;; Set the flags that indicate whether we have seen a lowercase
	  ;; word, a mixed case word, and an uppercase word.
	  (if (re-search-forward "[a-z]" name-end t)
	      (if (progn
		    (goto-char name-beg)
		    (re-search-forward "[A-Z]" name-end t))
		  (setq mixed-case-flag t)
		(setq lower-case-flag t))
;;	    (setq upper-case-flag t)
	    )
	  
	  (goto-char name-end)
	  (setq word-found-flag t))

	 (t
	  (setq name-done-flag t)
	  ))
	
	;; Count any word that we skipped over.
	(if word-found-flag
	    (setq word-count (1+ word-count))))
      
      ;; If the last thing in the name is 2 or more periods, or one or more
      ;; other sentence terminators (but not a single period) then keep them
      ;; and the preceeding word.  This is for the benefit of whole sentences
      ;; in the name field: it's better behavior than dropping the last word
      ;; of the sentence...
      (if (and (not suffix-flag)
	       (looking-at "\\(\\.+\\|[?!;:.][?!;:.]+\\|[?!;:][?!;:.]*\\)\\'"))
	  (goto-char (setq suffix-flag (point-max))))

      ;; Drop everything after point and certain trailing words.
      (narrow-to-region (point-min)
			(or (and drop-last-word-if-trailing-flag
				 last-word-beg)
			    (point)))
      
      ;; Xerox's mailers SUCK!!!!!!
      ;; We simply refuse to believe that any last name is PARC or ADOC.
      ;; If it looks like that is the last name, that there is no meaningful
      ;; here at all.  Actually I guess it would be best to map patterns
      ;; like foo.hoser@xerox.com into foo@hoser.xerox.com, but I don't
      ;; actually know that that is what's going on.
      (cond ((not suffix-flag)
	     (goto-char (point-min))
	     (let ((case-fold-search t))
	       (if (looking-at "[-A-Za-z_]+[. ]\\(PARC\\|ADOC\\)\\'")
		   (erase-buffer)))))

      ;; If last name first put it at end (but before suffix)
      (cond (last-name-comma-flag
	     (goto-char (point-min))
	     (search-forward ",")
	     (setq name-end (1- (point)))
	     (goto-char (or suffix-flag (point-max)))
	     (or (eq ?\  (preceding-char))
		 (insert ?\ ))
	     (insert-buffer-substring (current-buffer) (point-min) name-end)
	     (goto-char name-end)
	     (skip-chars-forward "\t ,")
	     (narrow-to-region (point) (point-max))))
      
      ;; Delete leading and trailing junk characters.
      ;; *** This is probably completly unneeded now.
      ;;(goto-char (point-max))
      ;;(skip-chars-backward mail-extr-non-end-name-chars)
      ;;(if (eq ?. (following-char))
      ;;    (forward-char 1))
      ;;(narrow-to-region (point)
      ;;                  (progn
      ;;                    (goto-char (point-min))
      ;;                    (skip-chars-forward mail-extr-non-begin-name-chars)
      ;;                    (point)))
      
      ;; Compress whitespace
      (goto-char (point-min))
      (while (re-search-forward "[ \t\n]+" nil t)
	(replace-match (if (eobp) "" " ") t))
      )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Table of top-level domain names.
;;
;; This is used during address canonicalization; be careful of format changes.
;; Keep in mind that the country abbreviations follow ISO-3166.  There is
;; a U.S. FIPS that specifies a different set of two-letter country
;; abbreviations.

(defconst mail-extr-all-top-level-domains
  (let ((ob (make-vector 509 0)))
    (mapcar
     (function
      (lambda (x)
	(put (intern (downcase (car x)) ob)
	     'domain-name
	     (if (nth 2 x)
		 (format (nth 2 x) (nth 1 x))
	       (nth 1 x)))))
     '(
       ;; ISO 3166 codes:
       ("ae" "United Arab Emirates")
       ("ag" "Antigua and Barbuda")
       ("al" "Albania")
       ("ao" "Angola")
       ("aq" "Antarctica")		; continent
       ("ar" "Argentina"	"Argentine Republic")
       ("at" "Austria"		"The Republic of %s")
       ("au" "Australia")
       ("az" "Azerbaijan")
       ("bb" "Barbados")
       ("bd" "Bangladesh")
       ("be" "Belgium"		"The Kingdom of %s")
       ("bf" "Burkina Faso")
       ("bg" "Bulgaria")
       ("bh" "Bahrain")
       ("bm" "Bermuda")
       ("bo" "Bolivia"		"Republic of %s")
       ("br" "Brazil"		"The Federative Republic of %s")
       ("bs" "Bahamas")
       ("bw" "Botswana")
       ("by" "Belarus")
       ("bz" "Belize")
       ("ca" "Canada")
       ("cg" "Congo")
       ("ch" "Switzerland"	"The Swiss Confederation")
       ("ci" "Ivory Coast")
       ("cl" "Chile"		"The Republic of %s")
       ("cm" "Cameroon")		; In .fr domain
       ("cn" "China"		"The People's Republic of %s")
       ("co" "Colombia")
       ("cr" "Costa Rica"	"The Republic of %s")
       ("cs" "Czechoslovakia")
       ("cu" "Cuba")
       ("cy" "Cyprus")
       ("cz" "Czech Republic")
       ("de" "Germany")
       ("dk" "Denmark")
       ("dm" "Dominica")
       ("do" "Dominican Republic"	"The %s")
       ("dz" "Algeria")
       ("ec" "Ecuador"		"The Republic of %s")
       ("ee" "Estonia")
       ("eg" "Egypt"		"The Arab Republic of %s")
       ("er" "Eritrea")
       ("es" "Spain"		"The Kingdom of %s")
       ("fi" "Finland"		"The Republic of %s")
       ("fj" "Fiji")
       ("fo" "Faroe Islands")
       ("fr" "France")
       ("gb" "Great Britain")
       ("gd" "Grenada")
       ("ge" "Georgia")
       ("gf" "Guyana (Fr.)")
       ("gp" "Guadeloupe (Fr.)")
       ("gr" "Greece"		"The Hellenic Republic (%s)")
       ("gt" "Guatemala")
       ("gu" "Guam (U.S.)")
       ("hk" "Hong Kong")
       ("hn" "Honduras")
       ("hr" "Croatia")
       ("ht" "Haiti")
       ("hu" "Hungary"		"The Hungarian Republic")	;???
       ("id" "Indonesia")
       ("ie" "Ireland")
       ("il" "Israel"		"The State of %s")
       ("in" "India"		"The Republic of %s")
       ("ir" "Iran")
       ("is" "Iceland"		"The Republic of %s")
       ("it" "Italy"		"The Italian Republic")
       ("jm" "Jamaica")
       ("jp" "Japan")
       ("ke" "Kenya")
       ("kn" "St. Kitts, Nevis, and Anguilla")
       ("kp" "Korea (North)")
       ("kr" "Korea (South)")
       ("kw" "Kuwait")
       ("kz" "Kazachstan")
       ("lb" "Lebanon")
       ("lc" "St. Lucia")
       ("li" "Liechtenstein")
       ("lk" "Sri Lanka"	"The Democratic Socialist Republic of %s")
       ("ls" "Lesotho")
       ("lt" "Lithuania")
       ("lu" "Luxembourg")
       ("lv" "Latvia")
       ("ma" "Morocco")
       ("md" "Moldova")
       ("mg" "Madagascar")
       ("mk" "Macedonia")
       ("ml" "Mali")
       ("mo" "Macau")
       ("mt" "Malta")
       ("mu" "Mauritius")
       ("mw" "Malawi")
       ("mx" "Mexico"		"The United Mexican States")
       ("my" "Malaysia"		"%s (changed to Myanmar?)")		;???
       ("mz" "Mozambique")
       ("na" "Namibia")
       ("nc" "New Caledonia (Fr.)")
       ("ne" "Niger")			; In .fr domain
       ("ni" "Nicaragua"	"The Republic of %s")
       ("nl" "Netherlands"	"The Kingdom of the %s")
       ("no" "Norway"		"The Kingdom of %s")
       ("np" "Nepal")			; Via .in domain
       ("nz" "New Zealand")
       ("pa" "Panama")
       ("pe" "Peru")
       ("pf" "Polynesia (Fr.)")
       ("pg" "Papua New Guinea")
       ("ph" "Philippines"	"The Republic of the %s")
       ("pk" "Pakistan")
       ("pl" "Poland")
       ("pr" "Puerto Rico (U.S.)")
       ("pt" "Portugal"		"The Portugese Republic")
       ("py" "Paraguay")
       ("re" "Reunion (Fr.)")		; In .fr domain
       ("ro" "Romania")
       ("ru" "Russian Federation")
       ("sa" "Saudi Arabia")
       ("sc" "Seychelles")
       ("sd" "Sudan")
       ("se" "Sweden"		"The Kingdom of %s")
       ("sg" "Singapore"	"The Republic of %s")
       ("si" "Slovenia")
       ("sj" "Svalbard and Jan Mayen Is.") ; In .no domain
       ("sk" "Slovakia"		"The Slovak Republic")
       ("sn" "Senegal")
       ("sr" "Suriname")
       ("su" "Soviet Union")
       ("sz" "Swaziland")
       ("tg" "Togo")
       ("th" "Thailand"		"The Kingdom of %s")
       ("tm" "Turkmenistan")		; In .su domain
       ("tn" "Tunisia")
       ("tr" "Turkey"		"The Republic of %s")
       ("tt" "Trinidad and Tobago")
       ("tw" "Taiwan")
       ("ua" "Ukraine")
       ("uk" "United Kingdom"	"The %s of Great Britain and Northern Ireland")
       ("us" "United States"	"The %s of America")
       ("uy" "Uruguay"		"The Eastern Republic of %s")
       ("vc" "St. Vincent and the Grenadines")
       ("ve" "Venezuela"	"The Republic of %s")
       ("vi" "Virgin Islands (U.S.)")
       ("vn" "Vietnam")
       ("vu" "Vanuatu")
       ("yu" "Yugoslavia"	"The Socialist Federal Republic of %s")
       ("za" "South Africa"	"The Republic of %s (or Zambia? Zaire?)")
       ("zw" "Zimbabwe"		"Republic of %s")
       ;; Special top-level domains:
       ("arpa" t		"Advanced Research Projects Agency (U.S. DoD)")
       ("bitnet" t		"Because It's Time NET")
       ("com" t			"Commercial")
       ("edu" t			"Educational")
       ("gov" t			"Government (U.S.)")
       ("int" t			"International (NATO)")
       ("mil" t			"Military (U.S.)")
       ("nato" t		"North Atlantic Treaty Organization")
       ("net" t			"Network")
       ("org" t			"Non-profit Organization")
       ;;("unter-dom" t		"? (Ger.)")
       ("uucp" t		"Unix to Unix CoPy")
       ;;("fipnet" nil		"?")
       ))
    ob))

;;;###autoload
(defun what-domain (domain)
  "Convert mail domain DOMAIN to the country it corresponds to."
  (interactive
   (let ((completion-ignore-case t))
     (list (completing-read "Domain: "
			    mail-extr-all-top-level-domains nil t))))
  (or (setq domain (intern-soft (downcase domain)
				mail-extr-all-top-level-domains))
      (error "no such domain"))
  (message "%s: %s" (upcase (symbol-name domain)) (get domain 'domain-name)))


;(let ((all nil))
;  (mapatoms #'(lambda (x)
;		(if (and (boundp x) 
;			 (string-match "^mail-extr-" (symbol-name x)))
;		    (setq all (cons x all)))))
;  (setq all (sort all #'string-lessp))
;  (cons 'setq
;	(apply 'nconc (mapcar #'(lambda (x)
;				  (list x (symbol-value x)))
;			      all))))


(provide 'mail-extr)

;;; mail-extr.el ends here
