;;; latin1-disp.el --- display tables for other ISO 8859 on Latin-1 terminals -*- coding: emacs-mule -*-

;; Copyright (C) 2000 Free Software Foundation, Inc.

;; Author: Dave Love <fx@gnu.org>
;; Keywords: i18n

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

;;; Commentary:

;; This package sets up display of ISO 8859-n for n>1 by substituting
;; Latin-1 characters and sequences of them for characters which can't
;; be displayed, either because we're on a tty or because we don't
;; have the relevant window system fonts available.  For instance,
;; Latin-9 is very similar to Latin-1, so we can display most Latin-9
;; characters using the Latin-1 characters at the same code point and
;; fall back on more-or-less mnemonic ASCII sequences for the rest.

;; For the Latin charsets the ASCII sequences are mostly consistent
;; with the Quail prefix input sequences.  Latin-4 uses the Quail
;; postfix sequences since a prefix method isn't defined for Latin-4.

;; [A different approach is taken in the DOS display tables in
;; term/internal.el, and the relevant ASCII sequences from there are
;; available as an alternative; see `latin1-display-mnemonic'.  Only
;; these sequences are used for Cyrillic, Greek and Hebrew.]

;; If you don't even have Latin-1, see iso-ascii.el and use the
;; complete tables from internal.el.  The ASCII sequences used here
;; are mostly in the same style as iso-ascii.

;;; Code:

;; Ensure `standard-display-table' is set up:
(require 'disp-table)

(defconst latin1-display-sets '(latin-2 latin-3 latin-4 latin-5 latin-8
		                latin-9 cyrillic greek hebrew)
  "The ISO8859 character sets with defined Latin-1 display sequences.
These are the nicknames for the sets and correspond to Emacs language
environments.")

(defgroup latin1-display ()
  "Set up display tables for ISO8859 characters using Latin-1."
  :version "21.1"
  :link '(emacs-commentary-link "latin1-disp")
  :group 'i18n)

(defcustom latin1-display-format "{%s}"
  "A format string used to display the ASCII sequences.
The default encloses the sequence in braces, but you could just use
\"%s\" to avoid the braces."
  :group 'latin1-display
  :type 'string)

;;;###autoload
(defcustom latin1-display nil
  "Set up Latin-1/ASCII display for ISO8859 character sets.
This is done for each character set in the list `latin1-display-sets',
if no font is available to display it.  Characters are displayed using
the corresponding Latin-1 characters where they match.  Otherwise
ASCII sequences are used, mostly following the Latin prefix input
methods.  Some different ASCII sequences are used if
`latin1-display-mnemonic' is non-nil.

Setting this variable directly does not take effect;
use either M-x customize of the function `latin1-display'."
  :group 'latin1-display
  :type 'boolean
  :require 'latin1-disp
  :initialize 'custom-initialize-default
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (mapc (if value
		   #'latin1-display-setup
		 #'latin1-display-reset)
	       latin1-display-sets)
	 (redraw-display)))

;;;###autoload
(defun latin1-display (&rest sets)
  "Set up Latin-1/ASCII display for the arguments character SETS.
See option `latin1-display' for the method.  The members of the list
must be in `latin1-display-sets'.  With no arguments, reset the
display for all of `latin1-display-sets'. See also `latin1-display-setup'."
  (if sets
      (progn (mapc #'latin1-display-setup sets)
	     (setq latin1-display t))
    (mapc #'latin1-display-reset latin1-display-sets)
    (setq latin1-display nil))
  (redraw-display))

(defcustom latin1-display-mnemonic nil
  "Non-nil means to display potentially more mnemonic sequences.
These are taken from the tables in `internal.el' rather than the Quail
input sequences."
  :type 'boolean
  :group 'latin1-display)

(defun latin1-display-char (char display &optional alt-display)
  "Make an entry in `standard-display-table' for CHAR using string DISPLAY.
If ALT-DISPLAY is provided, use that instead if
`latin1-display-mnemonic' is non-nil.  The actual string displayed is
formatted using `latin1-display-format'."
  (if (and (stringp alt-display)
	   latin1-display-mnemonic)
      (setq display alt-display))
  (if (stringp display)
      (standard-display-ascii char (format latin1-display-format display))
    (aset standard-display-table char display)))

(defun latin1-display-identities (charset)
  "Display each character in CHARSET as the corresponding Latin-1 character.
CHARSET is a symbol which is the nickname of a language environment
using an ISO8859 character set."
  (if (eq charset 'cyrillic)
      (setq charset 'cyrillic-iso))
  (let ((i 32)
	(set (car (remq 'ascii (get-language-info charset 'charset)))))
    (while (<= i 127)
      (aset standard-display-table
	    (make-char set i)
	    (vector (make-char 'latin-iso8859-1 i)))
      (setq i (1+ i)))))

(defun latin1-display-reset (language)
  "Set up the default display for each character of LANGUAGE's charset.
LANGUAGE is a symbol naming a language environment using an ISO8859
character set."
  (if (eq language 'cyrillic)
      (setq language 'cyrillic-iso))
  (let ((charset (car (remq 'ascii (get-language-info language
							'charset)))))
    (standard-display-default (make-char charset 32)
			      (make-char charset 127)))
  (sit-for 0))

(defun latin1-display-check-font (language)
  "Return non-nil if we have a font with an encoding for LANGUAGE.
LANGUAGE is a symbol naming a language environment using an ISO8859
character set: `latin-2', `hebrew' etc."
  (if (eq language 'cyrillic)
      (setq language 'cyrillic-iso))
  (let* ((info (get-language-info language 'charset))
	 (char (make-char (car (remq 'ascii info)) ?\ )))
    (latin1-char-displayable-p char)))

;; This should be moved into mule-utils or somewhere after 21.1.
(defun latin1-char-displayable-p (char)
  (cond ((< char 256)
	 ;; Single byte characters are always displayable.
	 t)
	((display-multi-font-p)
	 ;; On a window system, a character is displayable if we have
	 ;; a font for that character in the default face of the
	 ;; currently selected frame.
	 (let ((fontset (frame-parameter (selected-frame) 'font))
	       font-pattern)
	   (if (query-fontset fontset)
	       (setq font-pattern (fontset-font fontset char)))
	   (or font-pattern
	       (setq font-pattern (fontset-font "fontset-default" char)))
	   (if font-pattern
	       (progn
		 ;; Now FONT-PATTERN is a string or a cons of family
		 ;; field pattern and registry field pattern.
		 (or (stringp font-pattern)
		     (setq font-pattern (concat (or (car font-pattern) "*")
						"-*-"
						(cdr font-pattern))))
		 (x-list-fonts font-pattern 'default (selected-frame) 1)))))
	(t
	 (let ((coding (terminal-coding-system)))
	   (if coding
	       (let ((safe-chars (coding-system-get coding 'safe-chars))
		     (safe-charsets (coding-system-get coding 'safe-charsets)))
		 (or (and safe-chars
			  (aref safe-chars char))
		     (and safe-charsets
			  (memq (char-charset char) safe-charsets)))))))))

(defun latin1-display-setup (set &optional force)
  "Set up Latin-1 display for characters in the given SET.
SET must be a member of `latin1-display-sets'.  Normally, check
whether a font for SET is available and don't set the display if it
is.  If FORCE is non-nil, set up the display regardless."
  (cond
   ((eq set 'latin-2)
    (when (or force
	      (not (latin1-display-check-font set)))
      (latin1-display-identities set)
      (mapc
       (lambda (l)
	 (apply 'latin1-display-char l))
       '((?‚Æ "'C" "C'")
	 (?‚Ð "'D" "/D")
	 (?‚¦ "'S" "S'")
	 (?‚æ "'c" "c'")
	 (?‚ð "'d" "/d")
	 (?‚Å "'L" "L'")
	 (?‚ñ "'n" "n'")
	 (?‚Ñ "'N" "N'")
	 (?‚à "'r" "r'")
	 (?‚À "'R" "R'")
	 (?‚¶ "'s" "s'")
	 (?‚¼ "'z" "z'")
	 (?‚¬ "'Z" "Z'")
	 (?‚¡ "`A" "A;")
	 (?‚Ê "`E" "E;")
	 (?‚£ "`L" "/L")
	 (?‚ª "`S" ",S")
	 (?‚Þ "`T" ",T")
	 (?‚¯ "`Z" "Z^.")
	 (?‚± "`a" "a;")
	 (?‚³ "`l" "/l")
	 (?‚ê "`e" "e;")
	 (?‚º "`s" ",s")
	 (?‚þ "`t" ",t")
	 (?‚¿ "`z" "z^.")
	 (?‚ÿ "`." "'.")
	 (?‚Ã "~A" "A(")
	 (?‚È "~C" "C<")
	 (?‚Ï "~D" "D<")
	 (?‚Ì "~E" "E<")
	 (?‚ì "~e" "e<")
	 (?‚¥ "~L" "L<")
	 (?‚Ò "~N" "N<")
	 (?‚Õ "~O" "O''")
	 (?‚Ø "~R" "R<")
	 (?‚© "~S" "S<")
	 (?‚« "~T" "T<")
	 (?‚Û "~U" "U''")
	 (?‚® "~Z" "Z<")
	 (?‚ã "~a" "a(}")
	 (?‚è "~c" "c<")
	 (?‚ï "~d" "d<")
	 (?‚µ "~l" "l<")
	 (?‚ò "~n" "n<")
	 (?‚õ "~o" "o''")
	 (?‚ø "~r" "r<")
	 (?‚¹ "~s" "s<")
	 (?‚» "~t" "t<")
	 (?‚û "~u" "u''")
	 (?‚¾ "~z" "z<")
	 (?‚· "~v" "'<")			; ?‚¢ in latin-pre
	 (?‚¢ "~~" "'(")
	 (?‚ù "uu" "u^0")
	 (?‚Ù "UU" "U^0")
	 (?‚Ä "\"A")
	 (?‚ä "\"a")
	 (?‚Ë "\"E" "E:")
	 (?‚ë "\"e")
	 (?‚½ "''" "'")
	 (?‚· "'<")			; Lynx's rendering of caron
	 ))))

   ((eq set 'latin-3)
    (when (or force
	      (not (latin1-display-check-font set)))
      (latin1-display-identities set)
      (mapc
       (lambda (l)
	 (apply 'latin1-display-char l))
       '((?ƒ¡ "/H")
	 (?ƒ¢ "~`" "'(")
	 (?ƒ¦ "^H" "H^")
	 (?ƒ¶ "^h" "h^")
	 (?ƒ© ".I" "I^.")
	 (?ƒª ",S")
	 (?ƒ« "~G" "G(")
	 (?ƒ¬ "^J" "J^")
	 (?ƒ¯ ".Z" "Z^.")
	 (?ƒ± "/h")
	 (?ƒ¹ ".i" "i^.")
	 (?ƒº ",s")
	 (?ƒ» "~g" "g(")
	 (?ƒ¼ "^j" "j^")
	 (?ƒ¿ ".Z" "z^.")
	 (?ƒÅ ".c" "C^.")
	 (?ƒÆ "^C" "C^")
	 (?ƒÕ ".G" "G^.")
	 (?ƒØ "^G" "G^")
	 (?ƒÝ "~U" "U(")
	 (?ƒÞ "^S" "S^")
	 (?ƒå ".C" "c^.")
	 (?ƒæ "^c" "c^")
	 (?ƒõ ".g" "g^.")
	 (?ƒø "^g" "g^")
	 (?ƒý "~u" "u(")
	 (?ƒþ "^s" "s^")
	 (?ƒÿ "/." "^.")))))

   ((eq set 'latin-4)
    (when (or force
	      (not (latin1-display-check-font set)))
      (latin1-display-identities set)
      (mapc
       (lambda (l)
	 (apply 'latin1-display-char l))
       '((?„¡ "A," "A;")
	 (?„¢ "k/" "kk")
	 (?„£ "R," ",R")
	 (?„¥ "I~" "?I")
	 (?„¦ "L," ",L")
	 (?„© "S~" "S<")
	 (?„ª "E-")
	 (?„« "G," ",G")
	 (?„¬ "T/" "/T")
	 (?„® "Z~" "Z<")
	 (?„± "a," "a;")
	 (?„² "';")
	 (?„³ "r," ",r")
	 (?„µ "i~" "~i")
	 (?„¶ "l," ",l")
	 (?„· "'<")
	 (?„¹ "s~" "s<")
	 (?„º "e-")
	 (?„» "g," ",g")
	 (?„¼ "t/" "/t")
	 (?„½ "N/" "NG")
	 (?„¾ "z~" "z<")
	 (?„¿ "n/" "ng")
	 (?„À "A-")
	 (?„Ç "I," "I;")
	 (?„È "C~" "C<")
	 (?„Ê "E," "E;")
	 (?„Ì "E." "E^.")
	 (?„Ï "I-")
	 (?„Ñ "N," ",N")
	 (?„Ò "O-")
	 (?„Ó "K," ",K")
	 (?„Ù "U," "U;")
	 (?„Ý "U~" "~U")
	 (?„Þ "U-")
	 (?„à "a-")
	 (?„ç "i," "i;")
	 (?„è "c~" "c<")
	 (?„ê "e," "e;")
	 (?„ì "e." "e^.")
	 (?„ï "i-")
	 (?„ð "d/" "/d")
	 (?„ñ "n," ",n")
	 (?„ò "o-")
	 (?„ó "k," ",k")
	 (?„ù "u," "u;")
	 (?„ý "u~" "~u")
	 (?„þ "u-")
	 (?„ÿ "^.")))))

   ((eq set 'latin-5)
    (when (or force
	      (not (latin1-display-check-font set)))
      (latin1-display-identities set)
      (mapc
       (lambda (l)
	 (apply 'latin1-display-char l))
       '((?ð "~g" "g(")
	 (?Ð "~G" "G(")
	 (?Ý ".I" "I^.")
	 (?þ ",s")
	 (?Þ ",S")
	 (?ê "^e" "e<")			; from latin-post
	 (?ì ".e" "e^.")
	 (?ï "\"i" "i-")		; from latin-post
	 (?ý ".i" "i.")))))

   ((eq set 'latin-8)
    (when (or force
	      (not (latin1-display-check-font set)))
      (latin1-display-identities set)
      (mapc
       (lambda (l)
	 (apply 'latin1-display-char l))
       '((?¡ ".B" "B`")
	 (?¢ ".b" "b`")
	 (?¥ ".c" "c`")
	 (?¤ ".C" "C`")
	 (?¦ ".D" "D`")
	 (?« ".d" "d`")
	 (?¸ "`w")
	 (?¨ "`W")
	 (?º "'w" "w'")
	 (?ª "'W" "W'")
	 (?¼ "`y")
	 (?¬ "`Y")
	 (?± ".f" "f`")
	 (?° ".F" "F`")
	 (?³ ".g" "g`")
	 (?² ".G" "G`")
	 (?µ ".m" "m`")
	 (?´ ".M" "M`")
	 (?¹ ".p" "p`")
	 (?· ".P" "P`")
	 (?¿ ".s" "s`")
	 (?» ".S" "S`")
	 (?¾ "\"w")
	 (?½ "\"W")
	 (?ð "^w" "w^")
	 (?Ð "^W" "W^")
	 (?÷ ".t" "t`")
	 (?× ".T" "T`")
	 (?þ "^y" "y^")
	 (?Þ "^Y" "Y^")
	 (?¯ "\"Y")))))

   ((eq set 'latin-9)
    (when (or force
	      (not (latin1-display-check-font set)))
      (latin1-display-identities set)
      (mapc
       (lambda (l)
	 (apply 'latin1-display-char l))
       '((?Ž¨ "~s" "s<")
	 (?Ž¦ "~S" "S<")
	 (?Ž¤ "Euro" "E=")
	 (?Ž¸ "~z" "z<")
	 (?Ž´ "~Z" "Z<")
	 (?Ž¾ "\"Y")
	 (?Ž½ "oe")
	 (?Ž¼ "OE")))))

   ((eq set 'greek)
    (when (or force
	      (not (latin1-display-check-font set)))
      (mapc
       (lambda (l)
	 (apply 'latin1-display-char l))
       '((?†¡ "9'")
	 (?†¢ "'9")
	 (?†¯ "-M")
	 (?†µ "'%")
	 (?†¶ "'A")
	 (?†¸ "'E")
	 (?†¹ "'H")
	 (?†º "'I")
	 (?†¼ "'O")
	 (?†¾ "'Y")
	 (?†¿ "W%")
	 (?†À "i3")
	 (?†Ã "G*")
	 (?†Ä "D*")
	 (?†È "TH")
	 (?†Ë "L*")
	 (?†Î "C*")
	 (?†Ð "P*")
	 (?†Ó "S*")
	 (?†Ö "F*")
	 (?†Ø "Q*")
	 (?†Ù "W*")
	 (?†Ú "\"I")
	 (?†Û "\"Y")
	 (?†Ü "a%")
	 (?†Ý "e%")
	 (?†Þ "y%")
	 (?†ß "i%")
	 (?†à "u3")
	 (?†á "a*")
	 (?†â "b*")
	 (?†ã "g*")
	 (?†ä "d*")
	 (?†å "e*")
	 (?†æ "z*")
	 (?†ç "y*")
	 (?†è "h*")
	 (?†é "i*")
	 (?†ê "k")
	 (?†ë "l*")
	 (?†ì "m*")
	 (?†í "n*")
	 (?†î "c*")
	 (?†ð "p*")
	 (?†ñ "r*")
	 (?†ò "*s")
	 (?†ó "s*")
	 (?†ô "t*")
	 (?†õ "u")
	 (?†ö "f*")
	 (?†÷ "x*")
	 (?†ø "q*")
	 (?†ù "w*")
	 (?†ú "\"i")
	 (?†û "\"u")
	 (?†ü "'o")
	 (?†ý "'u")
	 (?†þ "'w")))
      (mapc
       (lambda (l)
	 (aset standard-display-table (car l) (string-to-vector (cadr l))))
       '((?†Á "A")
	 (?†Â "B")
	 (?†Å "E")
	 (?†Æ "Z")
	 (?†Ç "H")
	 (?†É "I")
	 (?†Ê "J")
	 (?†Ì "M")
	 (?†Í "N")
	 (?†Ï "O")
	 (?†Ñ "P")
	 (?†Ô "T")
	 (?†Õ "Y")
	 (?†× "X")
	 (?†ï "o")))))

   ((eq set 'hebrew)
    (when (or force
	      (not (latin1-display-check-font set)))
      ;; Don't start with identities, since we don't have definitions
      ;; for a lot of Hebrew in internal.el.  (Intlfonts is also
      ;; missing some glyphs.)
      (let ((i 34))
	(while (<= i 62)
	  (aset standard-display-table
		(make-char 'hebrew-iso8859-8 i)
		(vector (make-char 'latin-iso8859-1 i)))
	  (setq i (1+ i))))
      (mapc
       (lambda (l)
	 (aset standard-display-table (car l) (string-to-vector (cadr l))))
       '((?ˆß "=2")
	 (?ˆà "A+")
	 (?ˆá "B+")
	 (?ˆâ "G+")
	 (?ˆã "D+")
	 (?ˆä "H+")
	 (?ˆå "W+")
	 (?ˆæ "Z+")
	 (?ˆç "X+")
	 (?ˆè "Tj")
	 (?ˆé "J+")
	 (?ˆê "K%")
	 (?ˆë "K+")
	 (?ˆì "L+")
	 (?ˆí "M%")
	 (?ˆî "M+")
	 (?ˆï "N%")
	 (?ˆð "N+")
	 (?ˆñ "S+")
	 (?ˆò "E+")
	 (?ˆó "P%")
	 (?ˆô "P+")
	 (?ˆõ "Zj")
	 (?ˆö "ZJ")
	 (?ˆ÷ "Q+")
	 (?ˆø "R+")
	 (?ˆù "Sh")
	 (?ˆú "T+")))))

   ((eq set 'cyrillic)
    (setq set 'cyrillic-iso)
    (when (or force
	      (not (latin1-display-check-font set)))
      (mapc
       (lambda (l)
	 (apply 'latin1-display-char l))
       '((?Œ¢ "Dj")
	 (?Œ£ "Gj")
	 (?Œ¤ "IE")
	 (?Œ© "Lj")
	 (?Œª "Nj")
	 (?Œ« "Ts")
	 (?Œ¬ "Kj")
	 (?Œ® "V%")
	 (?Œ¯ "Dzh")
	 (?Œ± "B=")
	 (?Œ³ "â")
	 (?Œ´ "D")
	 (?Œ¶ "Z%")
	 (?Œ· "3")
	 (?Œ¸ "U")
	 (?Œ¹ "J=")
	 (?Œ» "L=")
	 (?Œ¿ "P=")
	 (?ŒÃ "Y")
	 (?ŒÄ "è")
	 (?ŒÆ "C=")
	 (?ŒÇ "C%")
	 (?ŒÈ "S%")
	 (?ŒÉ "Sc")
	 (?ŒÊ "=\"")
	 (?ŒË "Y=")
	 (?ŒÌ "%\"")
	 (?ŒÍ "Ee")
	 (?ŒÎ "Yu")
	 (?ŒÏ "Ya")
	 (?ŒÑ "b")
	 (?ŒÒ "v=")
	 (?ŒÓ "g=")
	 (?ŒÔ "g")
	 (?ŒÖ "z%")
	 (?Œ× "z=")
	 (?ŒØ "u")
	 (?ŒÙ "j=")
	 (?ŒÚ "k")
	 (?ŒÛ "l=")
	 (?ŒÜ "m=")
	 (?ŒÝ "n=")
	 (?Œß "n")
	 (?Œà "p")
	 (?Œâ "t=")
	 (?Œä "f=")
	 (?Œæ "c=")
	 (?Œç "c%")
	 (?Œè "s%")
	 (?Œé "sc")
	 (?Œê "='")
	 (?Œë "y=")
	 (?Œì "%'")
	 (?Œí "ee")
	 (?Œî "yu")
	 (?Œï "ya")
	 (?Œð "N0")
	 (?Œò "dj")
	 (?Œó "gj")
	 (?Œô "ie")
	 (?Œù "lj")
	 (?Œú "nj")
	 (?Œû "ts")
	 (?Œü "kj")
	 (?Œþ "v%")
	 (?Œÿ "dzh")))
      (mapc
       (lambda (l)
	 (aset standard-display-table (car l) (string-to-vector (cadr l))))
       '((?Œ¡ "Ë")
	 (?Œ¥ "S")
	 (?Œ¦ "I")
	 (?Œ§ "Ï")
	 (?Œ¨ "J")
	 (?Œñ "ë")
	 (?Œý "§")
	 (?Œ­ "-")
	 (?Œ° "A")
	 (?Œ² "B")
	 (?Œµ "E")
	 (?Œº "K")
	 (?Œ¼ "M")
	 (?Œ½ "H")
	 (?Œ¾ "O")
	 (?ŒÀ "P")
	 (?ŒÁ "C")
	 (?ŒÂ "T")
	 (?ŒÅ "X")
	 (?ŒÐ "a")
	 (?ŒÕ "e")
	 (?ŒÞ "o")
	 (?Œá "c")
	 (?Œã "y")
	 (?Œå "x")
	 (?Œõ "s")
	 (?Œö "i")
	 (?Œ÷ "ï")
	 (?Œø "j")))))

   (t (error "Unsupported character set: %S" set)))
   
  (sit-for 0))

(provide 'latin1-disp)

;;; latin1-disp.el ends here
