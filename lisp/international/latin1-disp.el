;;; latin1-disp.el --- display tables for other ISO 8859 on Latin-1 terminals -*- coding: iso-2022-7bit -*-

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
       '((?,BF(B "'C" "C'")
	 (?,BP(B "'D" "/D")
	 (?,B&(B "'S" "S'")
	 (?,Bf(B "'c" "c'")
	 (?,Bp(B "'d" "/d")
	 (?,BE(B "'L" "L'")
	 (?,Bq(B "'n" "n'")
	 (?,BQ(B "'N" "N'")
	 (?,B`(B "'r" "r'")
	 (?,B@(B "'R" "R'")
	 (?,B6(B "'s" "s'")
	 (?,B<(B "'z" "z'")
	 (?,B,(B "'Z" "Z'")
	 (?,B!(B "`A" "A;")
	 (?,BJ(B "`E" "E;")
	 (?,B#(B "`L" "/L")
	 (?,B*(B "`S" ",S")
	 (?,B^(B "`T" ",T")
	 (?,B/(B "`Z" "Z^.")
	 (?,B1(B "`a" "a;")
	 (?,B3(B "`l" "/l")
	 (?,Bj(B "`e" "e;")
	 (?,B:(B "`s" ",s")
	 (?,B~(B "`t" ",t")
	 (?,B?(B "`z" "z^.")
	 (?,B(B "`." "'.")
	 (?,BC(B "~A" "A(")
	 (?,BH(B "~C" "C<")
	 (?,BO(B "~D" "D<")
	 (?,BL(B "~E" "E<")
	 (?,Bl(B "~e" "e<")
	 (?,B%(B "~L" "L<")
	 (?,BR(B "~N" "N<")
	 (?,BU(B "~O" "O''")
	 (?,BX(B "~R" "R<")
	 (?,B)(B "~S" "S<")
	 (?,B+(B "~T" "T<")
	 (?,B[(B "~U" "U''")
	 (?,B.(B "~Z" "Z<")
	 (?,Bc(B "~a" "a(}")
	 (?,Bh(B "~c" "c<")
	 (?,Bo(B "~d" "d<")
	 (?,B5(B "~l" "l<")
	 (?,Br(B "~n" "n<")
	 (?,Bu(B "~o" "o''")
	 (?,Bx(B "~r" "r<")
	 (?,B9(B "~s" "s<")
	 (?,B;(B "~t" "t<")
	 (?,B{(B "~u" "u''")
	 (?,B>(B "~z" "z<")
	 (?,B7(B "~v" "'<")			; ?,B"(B in latin-pre
	 (?,B"(B "~~" "'(")
	 (?,By(B "uu" "u^0")
	 (?,BY(B "UU" "U^0")
	 (?,BD(B "\"A")
	 (?,Bd(B "\"a")
	 (?,BK(B "\"E" "E:")
	 (?,Bk(B "\"e")
	 (?,B=(B "''" "'")
	 (?,B7(B "'<")			; Lynx's rendering of caron
	 ))))

   ((eq set 'latin-3)
    (when (or force
	      (not (latin1-display-check-font set)))
      (latin1-display-identities set)
      (mapc
       (lambda (l)
	 (apply 'latin1-display-char l))
       '((?,C!(B "/H")
	 (?,C"(B "~`" "'(")
	 (?,C&(B "^H" "H^")
	 (?,C6(B "^h" "h^")
	 (?,C)(B ".I" "I^.")
	 (?,C*(B ",S")
	 (?,C+(B "~G" "G(")
	 (?,C,(B "^J" "J^")
	 (?,C/(B ".Z" "Z^.")
	 (?,C1(B "/h")
	 (?,C9(B ".i" "i^.")
	 (?,C:(B ",s")
	 (?,C;(B "~g" "g(")
	 (?,C<(B "^j" "j^")
	 (?,C?(B ".Z" "z^.")
	 (?,CE(B ".c" "C^.")
	 (?,CF(B "^C" "C^")
	 (?,CU(B ".G" "G^.")
	 (?,CX(B "^G" "G^")
	 (?,C](B "~U" "U(")
	 (?,C^(B "^S" "S^")
	 (?,Ce(B ".C" "c^.")
	 (?,Cf(B "^c" "c^")
	 (?,Cu(B ".g" "g^.")
	 (?,Cx(B "^g" "g^")
	 (?,C}(B "~u" "u(")
	 (?,C~(B "^s" "s^")
	 (?,C(B "/." "^.")))))

   ((eq set 'latin-4)
    (when (or force
	      (not (latin1-display-check-font set)))
      (latin1-display-identities set)
      (mapc
       (lambda (l)
	 (apply 'latin1-display-char l))
       '((?,D!(B "A," "A;")
	 (?,D"(B "k/" "kk")
	 (?,D#(B "R," ",R")
	 (?,D%(B "I~" "?I")
	 (?,D&(B "L," ",L")
	 (?,D)(B "S~" "S<")
	 (?,D*(B "E-")
	 (?,D+(B "G," ",G")
	 (?,D,(B "T/" "/T")
	 (?,D.(B "Z~" "Z<")
	 (?,D1(B "a," "a;")
	 (?,D2(B "';")
	 (?,D3(B "r," ",r")
	 (?,D5(B "i~" "~i")
	 (?,D6(B "l," ",l")
	 (?,D7(B "'<")
	 (?,D9(B "s~" "s<")
	 (?,D:(B "e-")
	 (?,D;(B "g," ",g")
	 (?,D<(B "t/" "/t")
	 (?,D=(B "N/" "NG")
	 (?,D>(B "z~" "z<")
	 (?,D?(B "n/" "ng")
	 (?,D@(B "A-")
	 (?,DG(B "I," "I;")
	 (?,DH(B "C~" "C<")
	 (?,DJ(B "E," "E;")
	 (?,DL(B "E." "E^.")
	 (?,DO(B "I-")
	 (?,DQ(B "N," ",N")
	 (?,DR(B "O-")
	 (?,DS(B "K," ",K")
	 (?,DY(B "U," "U;")
	 (?,D](B "U~" "~U")
	 (?,D^(B "U-")
	 (?,D`(B "a-")
	 (?,Dg(B "i," "i;")
	 (?,Dh(B "c~" "c<")
	 (?,Dj(B "e," "e;")
	 (?,Dl(B "e." "e^.")
	 (?,Do(B "i-")
	 (?,Dp(B "d/" "/d")
	 (?,Dq(B "n," ",n")
	 (?,Dr(B "o-")
	 (?,Ds(B "k," ",k")
	 (?,Dy(B "u," "u;")
	 (?,D}(B "u~" "~u")
	 (?,D~(B "u-")
	 (?,D(B "^.")))))

   ((eq set 'latin-5)
    (when (or force
	      (not (latin1-display-check-font set)))
      (latin1-display-identities set)
      (mapc
       (lambda (l)
	 (apply 'latin1-display-char l))
       '((?,Mp(B "~g" "g(")
	 (?,MP(B "~G" "G(")
	 (?,M](B ".I" "I^.")
	 (?,M~(B ",s")
	 (?,M^(B ",S")
	 (?,Mj(B "^e" "e<")			; from latin-post
	 (?,Ml(B ".e" "e^.")
	 (?,Mo(B "\"i" "i-")		; from latin-post
	 (?,M}(B ".i" "i.")))))

   ((eq set 'latin-8)
    (when (or force
	      (not (latin1-display-check-font set)))
      (latin1-display-identities set)
      (mapc
       (lambda (l)
	 (apply 'latin1-display-char l))
       '((?,_!(B ".B" "B`")
	 (?,_"(B ".b" "b`")
	 (?,_%(B ".c" "c`")
	 (?,_$(B ".C" "C`")
	 (?,_&(B ".D" "D`")
	 (?,_+(B ".d" "d`")
	 (?,_8(B "`w")
	 (?,_((B "`W")
	 (?,_:(B "'w" "w'")
	 (?,_*(B "'W" "W'")
	 (?,_<(B "`y")
	 (?,_,(B "`Y")
	 (?,_1(B ".f" "f`")
	 (?,_0(B ".F" "F`")
	 (?,_3(B ".g" "g`")
	 (?,_2(B ".G" "G`")
	 (?,_5(B ".m" "m`")
	 (?,_4(B ".M" "M`")
	 (?,_9(B ".p" "p`")
	 (?,_7(B ".P" "P`")
	 (?,_?(B ".s" "s`")
	 (?,_;(B ".S" "S`")
	 (?,_>(B "\"w")
	 (?,_=(B "\"W")
	 (?,_p(B "^w" "w^")
	 (?,_P(B "^W" "W^")
	 (?,_w(B ".t" "t`")
	 (?,_W(B ".T" "T`")
	 (?,_~(B "^y" "y^")
	 (?,_^(B "^Y" "Y^")
	 (?,_/(B "\"Y")))))

   ((eq set 'latin-9)
    (when (or force
	      (not (latin1-display-check-font set)))
      (latin1-display-identities set)
      (mapc
       (lambda (l)
	 (apply 'latin1-display-char l))
       '((?,b((B "~s" "s<")
	 (?,b&(B "~S" "S<")
	 (?,b$(B "Euro" "E=")
	 (?,b8(B "~z" "z<")
	 (?,b4(B "~Z" "Z<")
	 (?,b>(B "\"Y")
	 (?,b=(B "oe")
	 (?,b<(B "OE")))))

   ((eq set 'greek)
    (when (or force
	      (not (latin1-display-check-font set)))
      (mapc
       (lambda (l)
	 (apply 'latin1-display-char l))
       '((?,F!(B "9'")
	 (?,F"(B "'9")
	 (?,F/(B "-M")
	 (?,F5(B "'%")
	 (?,F6(B "'A")
	 (?,F8(B "'E")
	 (?,F9(B "'H")
	 (?,F:(B "'I")
	 (?,F<(B "'O")
	 (?,F>(B "'Y")
	 (?,F?(B "W%")
	 (?,F@(B "i3")
	 (?,FC(B "G*")
	 (?,FD(B "D*")
	 (?,FH(B "TH")
	 (?,FK(B "L*")
	 (?,FN(B "C*")
	 (?,FP(B "P*")
	 (?,FS(B "S*")
	 (?,FV(B "F*")
	 (?,FX(B "Q*")
	 (?,FY(B "W*")
	 (?,FZ(B "\"I")
	 (?,F[(B "\"Y")
	 (?,F\(B "a%")
	 (?,F](B "e%")
	 (?,F^(B "y%")
	 (?,F_(B "i%")
	 (?,F`(B "u3")
	 (?,Fa(B "a*")
	 (?,Fb(B "b*")
	 (?,Fc(B "g*")
	 (?,Fd(B "d*")
	 (?,Fe(B "e*")
	 (?,Ff(B "z*")
	 (?,Fg(B "y*")
	 (?,Fh(B "h*")
	 (?,Fi(B "i*")
	 (?,Fj(B "k")
	 (?,Fk(B "l*")
	 (?,Fl(B "m*")
	 (?,Fm(B "n*")
	 (?,Fn(B "c*")
	 (?,Fp(B "p*")
	 (?,Fq(B "r*")
	 (?,Fr(B "*s")
	 (?,Fs(B "s*")
	 (?,Ft(B "t*")
	 (?,Fu(B "u")
	 (?,Fv(B "f*")
	 (?,Fw(B "x*")
	 (?,Fx(B "q*")
	 (?,Fy(B "w*")
	 (?,Fz(B "\"i")
	 (?,F{(B "\"u")
	 (?,F|(B "'o")
	 (?,F}(B "'u")
	 (?,F~(B "'w")))
      (mapc
       (lambda (l)
	 (aset standard-display-table (car l) (string-to-vector (cadr l))))
       '((?,FA(B "A")
	 (?,FB(B "B")
	 (?,FE(B "E")
	 (?,FF(B "Z")
	 (?,FG(B "H")
	 (?,FI(B "I")
	 (?,FJ(B "J")
	 (?,FL(B "M")
	 (?,FM(B "N")
	 (?,FO(B "O")
	 (?,FQ(B "P")
	 (?,FT(B "T")
	 (?,FU(B "Y")
	 (?,FW(B "X")
	 (?,Fo(B "o")))))

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
       '((?,H_(B "=2")
	 (?,H`(B "A+")
	 (?,Ha(B "B+")
	 (?,Hb(B "G+")
	 (?,Hc(B "D+")
	 (?,Hd(B "H+")
	 (?,He(B "W+")
	 (?,Hf(B "Z+")
	 (?,Hg(B "X+")
	 (?,Hh(B "Tj")
	 (?,Hi(B "J+")
	 (?,Hj(B "K%")
	 (?,Hk(B "K+")
	 (?,Hl(B "L+")
	 (?,Hm(B "M%")
	 (?,Hn(B "M+")
	 (?,Ho(B "N%")
	 (?,Hp(B "N+")
	 (?,Hq(B "S+")
	 (?,Hr(B "E+")
	 (?,Hs(B "P%")
	 (?,Ht(B "P+")
	 (?,Hu(B "Zj")
	 (?,Hv(B "ZJ")
	 (?,Hw(B "Q+")
	 (?,Hx(B "R+")
	 (?,Hy(B "Sh")
	 (?,Hz(B "T+")))))

   ((eq set 'cyrillic)
    (setq set 'cyrillic-iso)
    (when (or force
	      (not (latin1-display-check-font set)))
      (mapc
       (lambda (l)
	 (apply 'latin1-display-char l))
       '((?,L"(B "Dj")
	 (?,L#(B "Gj")
	 (?,L$(B "IE")
	 (?,L)(B "Lj")
	 (?,L*(B "Nj")
	 (?,L+(B "Ts")
	 (?,L,(B "Kj")
	 (?,L.(B "V%")
	 (?,L/(B "Dzh")
	 (?,L1(B "B=")
	 (?,L3(B "â")
	 (?,L4(B "D")
	 (?,L6(B "Z%")
	 (?,L7(B "3")
	 (?,L8(B "U")
	 (?,L9(B "J=")
	 (?,L;(B "L=")
	 (?,L?(B "P=")
	 (?,LC(B "Y")
	 (?,LD(B "è")
	 (?,LF(B "C=")
	 (?,LG(B "C%")
	 (?,LH(B "S%")
	 (?,LI(B "Sc")
	 (?,LJ(B "=\"")
	 (?,LK(B "Y=")
	 (?,LL(B "%\"")
	 (?,LM(B "Ee")
	 (?,LN(B "Yu")
	 (?,LO(B "Ya")
	 (?,LQ(B "b")
	 (?,LR(B "v=")
	 (?,LS(B "g=")
	 (?,LT(B "g")
	 (?,LV(B "z%")
	 (?,LW(B "z=")
	 (?,LX(B "u")
	 (?,LY(B "j=")
	 (?,LZ(B "k")
	 (?,L[(B "l=")
	 (?,L\(B "m=")
	 (?,L](B "n=")
	 (?,L_(B "n")
	 (?,L`(B "p")
	 (?,Lb(B "t=")
	 (?,Ld(B "f=")
	 (?,Lf(B "c=")
	 (?,Lg(B "c%")
	 (?,Lh(B "s%")
	 (?,Li(B "sc")
	 (?,Lj(B "='")
	 (?,Lk(B "y=")
	 (?,Ll(B "%'")
	 (?,Lm(B "ee")
	 (?,Ln(B "yu")
	 (?,Lo(B "ya")
	 (?,Lp(B "N0")
	 (?,Lr(B "dj")
	 (?,Ls(B "gj")
	 (?,Lt(B "ie")
	 (?,Ly(B "lj")
	 (?,Lz(B "nj")
	 (?,L{(B "ts")
	 (?,L|(B "kj")
	 (?,L~(B "v%")
	 (?,L(B "dzh")))
      (mapc
       (lambda (l)
	 (aset standard-display-table (car l) (string-to-vector (cadr l))))
       '((?,L!(B ",AK(B")
	 (?,L%(B "S")
	 (?,L&(B "I")
	 (?,L'(B ",AO(B")
	 (?,L((B "J")
	 (?,Lq(B ",Ak(B")
	 (?,L}(B ",A'(B")
	 (?,L-(B "-")
	 (?,L0(B "A")
	 (?,L2(B "B")
	 (?,L5(B "E")
	 (?,L:(B "K")
	 (?,L<(B "M")
	 (?,L=(B "H")
	 (?,L>(B "O")
	 (?,L@(B "P")
	 (?,LA(B "C")
	 (?,LB(B "T")
	 (?,LE(B "X")
	 (?,LP(B "a")
	 (?,LU(B "e")
	 (?,L^(B "o")
	 (?,La(B "c")
	 (?,Lc(B "y")
	 (?,Le(B "x")
	 (?,Lu(B "s")
	 (?,Lv(B "i")
	 (?,Lw(B ",Ao(B")
	 (?,Lx(B "j")))))

   (t (error "Unsupported character set: %S" set)))
   
  (sit-for 0))

(provide 'latin1-disp)

;;; latin1-disp.el ends here
