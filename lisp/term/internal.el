;;; internal.el --- support for PC internal terminal -*- coding: raw-text; -*-

;; Copyright (C) 1993, 1994, 1998 Free Software Foundation, Inc.

;; Author: Morten Welinder <terra@diku.dk>

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

;;; Code:

;; ---------------------------------------------------------------------------
;; keyboard setup -- that's simple!
(set-input-mode nil nil 0)
(define-key function-key-map [backspace] "\177") ; Normal behaviour for BS
(define-key function-key-map [delete] "\C-d")    ; ... and Delete
(define-key function-key-map [tab] [?\t])
(define-key function-key-map [linefeed] [?\n])
(define-key function-key-map [clear] [11])
(define-key function-key-map [return] [13])
(define-key function-key-map [escape] [?\e])
(define-key function-key-map [M-backspace] [?\M-\d])
(define-key function-key-map [M-delete] [?\M-\d])
(define-key function-key-map [M-tab] [?\M-\t])
(define-key function-key-map [M-linefeed] [?\M-\n])
(define-key function-key-map [M-clear] [?\M-\013])
(define-key function-key-map [M-return] [?\M-\015])
(define-key function-key-map [M-escape] [?\M-\e])
(put 'backspace 'ascii-character 127)
(put 'delete 'ascii-character 127)
(put 'tab 'ascii-character ?\t)
(put 'linefeed 'ascii-character ?\n)
(put 'clear 'ascii-character 12)
(put 'return 'ascii-character 13)
(put 'escape 'ascii-character ?\e)

;; ----------------------------------------------------------------------
;;   DOS display setup
;;   =================
;;
;;   DOS can only support a single font.  On most systems (with the
;;   possible exception of Far Eastern DOS versions), this means that
;;   two character sets are available at any given time: the ASCII
;;   charset, and a single national charset, usually mapped to codes
;;   above 128 (i.e., with 8th bit set).  Which national charset is
;;   supported depends on the codepage loaded by the system when it
;;   boots; usually, this codepage cannot be changed without
;;   rebooting.
;;
;;   Since each codepage can usually display character of a single
;;   MULE charset, Emacs can display a single MULE charset with the
;;   glyphs of the current codepage.  The mapping from DOS codepages
;;   to MULE charsets is established by the charset property of the
;;   cpNNN-decode-table variables in codepage.el, which also
;;   defines translation tables for each such pair, and a bunch of
;;   functions to generate coding systems that use those translation
;;   tables to convert codepage-encoded text to the appropriate MULE
;;   charset and back.  When Emacs starts on DOS, it automatically
;;   sets its default coding systems for file I/O and terminal output
;;   according to the currend DOS codepage, given by the
;;   `dos-codepage' variable.
;;
;;   This leaves us with the problem of displaying character sets
;;   other than the one which maps directly into the current codepage.
;;   The following functions and variables handle this nuisance by
;;   defining a display table where each character that doesn't have a
;;   glyph in some codepage is mapped to a string which represents it.
;;   For example, a small c with cedilla is mapped to the string
;;   "{,c}" (the braces serve as a sign that this is a single
;;   character).  A nice feature of the display tables is that Emacs
;;   knows that the string represents a single character, and thus
;;   cursor motion works as you'd expect: a single `C-f' moves past
;;   the entire string which represents a single character.
;; ----------------------------------------------------------------------

(defvar IT-character-translations
  '(
    (latin-iso8859-1
     . [255 "!I" "|c" "Pd" "$$" "Ye" "|" "SE" "\"" "(c)"
        "_a" "<<" "~" "--" "(R)" "'-" "^o" "+-" "^2" "^3"
	"'" "u" ".P" "^." "'," "^1" "_o" ">>" "1/4" "1/2"
	"3/4" "?I" "`A" "A'" "A^" "~A" "\"A" "Ao" "AE" ",C"
	"`E" "E'" "E^" "\"E" "`I" "I'" "I^" "\"I" "D-" "~N"
	"`O" "O'" "O^" "~O" "\"O" "*x" "/O" "`U" "U'" "U^"
	"\"U" "Y'" "TH" "ss" "`a" "a'" "a^" "~a" "\"a" "ao"
	"ae" ",c" "`e" "e'" "e^" "\"e" "`i" "i'" "i^" "\"i"
	"d-" "~n" "`o" "o'" "o^" "~o" "\"o" "-:" "/o" "`u"
	"u'" "u^" "\"u" "y'" "th" "\"y"]
     )
    (latin-iso8859-2
     . [255 "A;" "'(" "/L" "$$" "L<" "S'" "SE" "\"" "S<"
	",S" "T<" "Z'" "--" "Z<" "Z^." "^o" "a;" "';" "/l"
	"'" "l<" "s'" "'<" "'," "s<" ",s" "t<" "z'" "'"
	"z<" "z^." "R'" "A'" "A^" "A(" "\"A" "L'" "C'" ",C"
	"C<" "E'" "E;" "E:" "E<" "I'" "I^" "D<" "/D" "N'"
	"N<" "O'" "O^" "O''" "\"O" "*x" "R<" "U^0" "U'" "U''"
	"\"U" "Y'" ",T" "ss" "r'" "a'" "a^" "a(" "\"a" "l'"
	"c'" ",c" "c<" "e'" "e;" "\"e" "e<" "i'" "i^" "d<"
	"/d" "n'" "n<" "o'" "o^" "o''" "\"o" "-:" "r<" "u^0"
	"u'" "u''" "\"u" "y'" ",t" "'."]
     )
    (latin-iso8859-3
     . [255 "/H" "'(" "Pd" "$$" " " "H^" "SE" "\"" "I^."
	",S" "G(" "J^" "--" " " "Z^." "^o" "/h" "^2" "^3"
	"'" "u" "h^" "." "'," "i^." ",s" "g(" "j^" "1/2"
	" " "z^." "`A" "A'" "A^" " " "\"A" "C^." "C^" ",C"
	"`E" "E'" "E^" "\"E" "`I" "I'" "I^" "\"I" " " "~N"
	"`O" "O'" "O^" "G^." "\"O" "*x" "G^" "`U" "U'" "U^"
	"\"U" "U(" "S^" "ss" "`a" "a'" "a^" " " "\"a" "c^."
	"c^" ",c" "`e" "e'" "e^" "\"e" "`i" "i'" "i^" "\"i"
	" " "~n" "`o" "o'" "o^" "g^." "\"o" "-:" "g^" "`u"
	"u'" "u^" "\"u" "u(" "s^" "^."]
     )
    (latin-iso8859-4
     . [255 "A;" "kk" ",R" "$$" "?I" ",L" "SE" "\"" "S<"
	"E-" ",G" "/T" "--" "Z<" "'-" "^o" "a;" "';" ",r"
	"'" "~i" ",l" "'<" "'," "s<" "e-" ",g" "/t" "NG"
	"z<" "ng" "A-" "A'" "A^" "~A" "\"A" "Ao" "AE" "I;"
	"C<" "E'" "E;" "\"E" "E^." "I'" "I^" "I-" "/D" ",N"
	"O-" ",K" "O^" "~O" "\"O" "*x" "/O" "U;" "U'" "U^"
	"\"U" "~U" "U-" "ss" "a-" "a'" "a^" "~a" "\"a" "ao"
	"ae" "i;" "c<" "e'" "e;" "\"e" "e^." "i'" "i^" "i-"
	"/d" ",n" "o-" ",k" "o^" "~o" "\"o" "-:" "/o" "u;"
	"u'" "u^" "\"u" "~u" "u-" "^."]
     )
    (cyrillic-iso8859-5
     . [255 "\"E" "Dj" "Gj" "IE" "Dz" "Ii" "Ji" "JE" "Lj"
	"Nj" "Ts" "Kj" 240 "V%" "Dzh"  65  "B="  66 226
	68  69  "Z%"  51  85 "J="  75 "L="  77  72
	79  "P="  80  67  84  89 232  88 "C=" "C%"
	"S%" "Sc" "=\"" "Y=" "%\"" "Ee" "Yu" "Ya" 97  98
	"v=" "g=" 103 101 "z%" "z=" 117 "j=" 107 "l="
	"m=" "n=" 111 110 112 99 "t=" 121 "f=" 120
	"c=" "c%" "s%" "sc" "='" "y=" "%'" "ee" "yu" "ya"
	"N0" "\"e" "dj" "gj" "ie" "dz" "ii" "ji" "je" "lj"
	"nj" "ts" "kj"  21 "v%" "dzh"]
     )
    (arabic-iso8859-6
     . [255 nil nil nil "$$" nil nil nil nil nil
	nil nil ",+" "--" nil nil nil nil nil nil
	nil nil nil nil nil nil nil ";+" nil nil
	nil "?+" nil "H'" "aM" "aH" "wH" "ah" "yH"
	"a+" "b+" "tm" "t+" "tk" "g+" "hk" "x+" "d+" "dk"
	"r+" "z+" "s+" "sn" "c+" "dd" "tj" "zH" "e+" "i+"
	nil nil nil nil nil "++" "f+" "q+" "k+" "l+"
	"m+" "n+" "h+" "w+" "j+" "y+" ":+" "\"+" "=+" "/+"
	"'+" "1+" "3+" "0+" nil nil nil nil nil nil
	nil nil nil nil nil nil nil]
     )
    (greek-iso8859-7
     . [255 "9'" "'9" "Pd" nil nil "|" "SE" "\"" "(c)"
	nil "<<" "~" "--" nil "-M" "^o" "+-" "^2" "^3"
	"'" "'%" "A%" "^." "E%" "Y%" "I%" ">>" "O%" "1/2"
	"U%" "W%" "i3" "A*" "B*" "G*" "D*" "E*" "Z*" "Y*"
	"H*" "I*" "K*" "L*" "M*" "N*" "C*" "O*" "P*" "R*"
	nil "S*" "T*" "U*" "F*" "X*" "Q*" "W*" "J*" "V*"
	"a%" "e%" "y%" "i%" "u3" "a*" "b*" "g*" "d*" "e*"
	"z*" "y*" "h*" "i*" "k*" "l*" "m*" "n*" "c*" "o*"
	"p*" "r*" "*s" "s*" "t*" "u*" "f*" "x*" "q*" "w*"
	"j*" "v*" "o%" "u%" "w%" nil]
     )
    (hebrew-iso8859-8
     . [255 nil "|c" "Pd" "$$" "Ye" "|" "SE" "\"" "(c)"
	"*x" "<<" "~" "--" "(R)" "'-" "^o" "+-" "^2" "^3"
	"'" "u" ".P" "^." "'," "^1" "-:" ">>" "1/4" "1/2"
	"3/4" nil nil nil nil nil nil nil nil nil
	nil nil nil nil nil nil nil nil nil nil
	nil nil nil nil nil nil nil nil nil nil
	nil nil nil "=2" "A+" "B+" "G+" "D+" "H+" "W+"
	"Z+" "X+" "Tj" "J+" "K%" "K+" "L+" "M%" "M+" "N%"
	"N+" "S+" "E+" "P%" "P+" "Zj" "ZJ" "Q+" "R+" "Sh"
	"T+" nil nil nil nil nil]
     )
    (latin-iso8859-9
     . [255 "!I" "|c" "Pd" "$$" "Ye" "|" "SE" "\"" "(c)"
        "_a" "<<" "~" "--" "(R)" "'-" "^o" "+-" "^2" "^3"
	"'" "u" ".P" "^." "'," "^1" "_o" ">>" "1/4" "1/2"
	"3/4" "?I" "`A" "A'" "A^" "~A" "\"A" "Ao" "AE" ",C"
	"`E" "E'" "E^" "\"E" "`I" "I'" "I^" "\"I" "G(" "~N"
	"`O" "O'" "O^" "~O" "\"O" "*x" "/O" "`U" "U'" "U^"
	"\"U" "I^." ",S" "ss" "`a" "a'" "a^" "~a" "\"a" "ao"
	"ae" ",c" "`e" "e'" "e^" "\"e" "e^." "i'" "i^" "i-"
	"g(" "~n" "`o" "o'" "o^" "~o" "\"o" "-:" "/o" "`u"
	"u'" "u^" "\"u" "i^." ",s" "\"y"]
     )
    )
  "An alist of MULE ISO-8859 character sets and the strings that
should be used to represent the characters from each set on a DOS
terminal which does not have corresponding glyphs built into the
installed codepage.")

(defun IT-display-table-setup (codepage &optional table)
  "Set up display table TABLE for a DOS terminal which supports
glyphs built into the codepage CODEPAGE.

If TABLE is nil or omitted, `standard-display-table' is used."
  (let* ((surrogates IT-character-translations)
	 (disp-tab (or table standard-display-table))
	 (built-in-set (cp-charset-for-codepage codepage))
	 (offset (cp-offset-for-codepage codepage))
	 (cp-decoder
	  (symbol-value (intern-soft (format "%s-decode-table" codepage))))
	 (cp-decoder-len (length cp-decoder))
	 (c offset)
	 association chset)
    ;; Undo the effects of previous call (where they may have used
    ;; a different codepage) by reverting the display table for the
    ;; built-in charset to its pristine shape.
    (while (< c 256)
      (aset disp-tab (make-char built-in-set c) nil)
      (setq c (1+ c)))
    (while surrogates
      (setq association (car surrogates))
      (setq chset (car association))
      (let* ((vector (cdr association))
	     (veclen (length vector))
	     (i 0)
	     glyph)
	(while (< i veclen)
	  (setq glyph (aref vector i))
	  (if (and glyph
		   (or (not (equal chset built-in-set))
		       (>= i cp-decoder-len)
		       (null (aref cp-decoder i))))
	      (aset disp-tab (make-char chset (+ i (logand offset 127)))
		    (vconcat
		     (if (numberp glyph)
			 (char-to-string glyph)
		       (if (> (length glyph) 1) (concat "{" glyph "}")
			 glyph)))))
	  (setq i (1+ i))))
      (setq surrogates (cdr surrogates)))))

(defun dos-cpNNN-setup (codepage)
  "Set up the MULE environment using the DOS codepage CODEPAGE.

This function creates the coding system cpNNN (where NNN is the value
of the argument CODEPAGE), and then uses this coding system to set up
display tables, and the language environment options as appropriate."
  (let* ((cp (format "cp%s" codepage))
	 (charset (cp-charset-for-codepage cp))
	 (offset (cp-offset-for-codepage cp)))
    (cp-make-coding-systems-for-codepage cp charset offset)
    ;; This is done by set-language-environment.
    ;;(setq nonascii-translation-table
    ;;     (symbol-value (intern (concat cp "-nonascii-translation-table"))))
    (set-language-environment (cp-language-for-codepage cp))
    (set-default-coding-systems (intern (concat cp "-dos")))
    (set-selection-coding-system (intern (concat cp "-dos")))
    (set-terminal-coding-system
     (setq default-terminal-coding-system (intern (concat cp
							  "-unix"))))
    (IT-display-table-setup cp)
    (prefer-coding-system (intern (concat cp "-dos")))
    (if default-enable-multibyte-characters
	;; We want this in multibyte version only, since unibyte version
	;; should not convert non-ASCII characters at all.
	(setq unibyte-display-via-language-environment t)
      ;; Let the unibyte version behave as Emacs 19 did.  In particular,
      ;; let it use and display native codepage-specific glyphs for
      ;; non-ASCII characters.  For this to work correctly, we need to
      ;; establish the correspondence between lower-case letters and their
      ;; upper-case brethren, as appropriate for the codepage in use.  The
      ;; code below makes this happen.
      ;; (In the multibyte mode, the appropriate tables are prepared
      ;; elsewhere, since multibyte Emacs uses normal MULE character sets,
      ;; which are supported on all platforms.)
      (let* ((i 128)
	     (modify (function
		      (lambda (ch sy) 
			(modify-syntax-entry ch sy text-mode-syntax-table)
			(if (boundp 'tex-mode-syntax-table)
			    (modify-syntax-entry ch sy tex-mode-syntax-table))
			(modify-syntax-entry ch sy (standard-syntax-table))
			)))
	     (table (standard-case-table))
	     ;; The following are strings of letters, first lower then
	     ;; upper case.  This will look funny on terminals which
	     ;; display other code pages.  In particular, what is
	     ;; displayed as blanks or triangles are not what they
	     ;; look lile at all!  (Use `C-x =' to see what they
	     ;; really are.)
	     (chars
	      (cond
	       ((= codepage 850)
		"áÄÅöÇêÉ∂ÑéÖ∑Üè∆«†µà“â”ä‘ãÿå◊çﬁ°÷ëíì‚îôï„¢‡õùñÍ£ÈóÎòYÏÌ°I£È§•–—ÁË")
	       ((= codepage 865)
		"áÄÅöÇêÉAÑéÖAÜèàEâEäEãIåIçIëíìOîôïOñU£UòYõù†A°I¢O£U§•")
	       ;; default is 437
	       (t "áÄÅöÇêÉAÑéÖAÜèàEâEäEãIåIçIëíìOîôïOñU£UòY†A°I¢O£U§•"))))

	(while (< i 256)
	  (funcall modify i "_")
	  (setq i (1+ i)))

	(setq i 0)
	(while (< i (length chars))
	  (let ((ch1 (aref chars i))
		(ch2 (aref chars (1+ i))))
	    (if (> ch2 127)
		(set-case-syntax-pair ch2 ch1 table))
	    (setq i (+ i 2))))
	(save-excursion
	  (mapcar (lambda (b) (set-buffer b) (set-case-table table))
		  (buffer-list)))
	(set-standard-case-table table)))
    ;; Some codepages have sporadic support for Latin-1, Greek, and
    ;; symbol glyphs, which don't belong to their native character
    ;; set.  It's a nuisance to have all those glyphs here, for all
    ;; the codepages (for starters, I don't even have references for
    ;; all the codepages).  So I provide a hook for those who want to
    ;; squeeze every bit of support out of their terminal/font.
    (run-hooks 'dos-codepage-setup-hook)
    ))

(defvar cjk-codepages-alist
  '((932 "Japanese" japanese-shift-jis)
    (950 "Chinese-BIG5" cn-big5)
    (936 "Chinese-GB" cn-gb-2312)
    (949 "Korean" euc-kr))
  "An alist of Far-Eastern codepages and the names of the associated
language and supported coding system.")

(defun dos-codepage-setup ()
  "Set up the MULE environment as appropriate for the installed DOS codepage.

This function sets coding systems, display tables, and the language
environment options as appropriate for the current value of `dos-codepage'.

This function is automatically run at startup via the `term-setup-hook'
list.  You can (and should) also run it whenever the value of
`dos-codepage' changes."
  (interactive)
  (let* ((desc (cdr (assq dos-codepage cjk-codepages-alist)))
	 (lang (car desc))
	 (coding (car (cdr desc)))
	 coding-dos coding-unix)
    (if (null desc)
	(dos-cpNNN-setup dos-codepage)
      ;; We've got one of the Far-Eastern codepages which support
      ;; MULE native coding systems directly.
      (setq coding-dos (intern (format "%s-dos" coding))
	    coding-unix (intern (format "%s-unix" coding)))
      (set-language-environment lang)
      (set-selection-coding-system coding-dos)
      (setq file-name-coding-system coding-unix)
      (set-terminal-coding-system
       (setq default-terminal-coding-system coding-unix))
      ;; Assume they support non-ASCII Latin characters like the IBM
      ;; codepage 437 does.
      (IT-display-table-setup "cp437")
      (prefer-coding-system coding-dos)
      (if default-enable-multibyte-characters
	  (setq unibyte-display-via-language-environment t))
      )))

;; We want to delay the terminal and other codepage-related setup
;; until after the terminal is set and user's .emacs is processed,
;; because people might define their `dos-codepage-setup-hook' there.
(add-hook 'term-setup-hook 'dos-codepage-setup)

;; In multibyte mode, we want unibyte buffers to be displayed using
;; the terminal coding system, so that they display correctly on the
;; DOS terminal; in unibyte mode we want to see all 8-bit characters
;; verbatim.  In both cases, we want the entire range of 8-bit
;; characters to arrive at our display code verbatim.
(standard-display-8bit 127 255)

;;; internal.el ends here

