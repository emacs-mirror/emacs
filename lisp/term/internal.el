;;; internal.el --- support for PC internal terminal -*- coding: raw-text; no-byte-compile: t -*-

;; Copyright (C) 1993, 1994, 1998, 1999, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: Morten Welinder <terra@diku.dk>

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

;; ---------------------------------------------------------------------------
;; keyboard setup -- that's simple!
(set-input-mode nil nil 0)
(define-key local-function-key-map [backspace] "\177") ; Normal behaviour for BS
(define-key local-function-key-map [delete] "\C-d") ; ... and Delete
(define-key local-function-key-map [tab] [?\t])
(define-key local-function-key-map [linefeed] [?\n])
(define-key local-function-key-map [clear] [11])
(define-key local-function-key-map [return] [13])
(define-key local-function-key-map [escape] [?\e])
(define-key local-function-key-map [M-backspace] [?\M-\d])
(define-key local-function-key-map [M-delete] [?\M-d])
(define-key local-function-key-map [M-tab] [?\M-\t])
(define-key local-function-key-map [M-linefeed] [?\M-\n])
(define-key local-function-key-map [M-clear] [?\M-\013])
(define-key local-function-key-map [M-return] [?\M-\015])
(define-key local-function-key-map [M-escape] [?\M-\e]))
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
	"`E" "E'" "E^" "\"E" "`I" "I'" "I^" "\"I" "-D" "~N"
	"`O" "O'" "O^" "~O" "\"O" "*x" "/O" "`U" "U'" "U^"
	"\"U" "Y'" "-P" "ss" "`a" "a'" "a^" "~a" "\"a" "ao"
	"ae" ",c" "`e" "e'" "e^" "\"e" "`i" "i'" "i^" "\"i"
	"-d" "~n" "`o" "o'" "o^" "~o" "\"o" "-:" "/o" "`u"
	"u'" "u^" "\"u" "y'" "-p" "\"y"]
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
	"'" "'%" "'A" "^." "'E" "'H" "'I" ">>" "'O" "1/2"
	"'Y" "W%" "i3" 65 66 "G*" "D*" 69 90 72
	"TH" 73 74 "L*" 77 78 "C*" 79 "P*" 80
	nil "S*" 84 89 "F*" 88 "Q*" "W*" "\"I" "\"Y"
	"a%" "e%" "y%" "i%" "u3" "a*" "b*" "g*" "d*" "e*"
	"z*" "y*" "h*" "i*" 107 "l*" "m*" "n*" "c*" 111
	"p*" "r*" "*s" "s*" "t*" 117 "f*" "x*" "q*" "w*"
	"\"i" "\"u" "'o" "'u" "'w" nil]
     )
    ;; Note: some of the characters undefined according to ISO 8859-8
    ;; in the ranges 190..220 and 250..255 are replaced with SI 1311-1
    ;; points (Niqud) and bidi formatting characters
    (hebrew-iso8859-8
     . [255 nil "|c" "Pd" "$$" "Ye" "|" "SE" "\"" "(c)"
	"*x" "<<" "~" "--" "(R)" "'-" "^o" "+-" "^2" "^3"
	"'" "u" ".P" "^." "'," "^1" "-:" ">>" "1/4" "1/2"
	"3/4" nil ":'" "v:" "-:" "-':" ".'" ".." "v'" "-'"
	"-," "`." nil "\\." "(.)" "|'" "`-" "`=" "||" nil
	nil "::" nil nil nil nil nil nil nil "LRO"
	"RLO" "PDF" nil "=2" "A+" "B+" "G+" "D+" "H+" "W+"
	"Z+" "X+" "Tj" "J+" "K%" "K+" "L+" "M%" "M+" "N%"
	"N+" "S+" "E+" "P%" "P+" "Zj" "ZJ" "Q+" "R+" "Sh"
	"T+" "LRE" "RLE" "LRM" "RLM" nil]
     )
    (latin-iso8859-9
     . [255 "!I" "|c" "Pd" "$$" "Ye" "|" "SE" "\"" "(c)"
        "_a" "<<" "~" "--" "(R)" "'-" "^o" "+-" "^2" "^3"
	"'" "u" ".P" "^." "'," "^1" "_o" ">>" "1/4" "1/2"
	"3/4" "?I" "`A" "A'" "A^" "~A" "\"A" "Ao" "AE" ",C"
	"`E" "E'" "E^" "\"E" "`I" "I'" "I^" "\"I" "G(" "~N"
	"`O" "O'" "O^" "~O" "\"O" "*x" "/O" "`U" "U'" "U^"
	"\"U" "I^." ",S" "ss" "`a" "a'" "a^" "~a" "\"a" "ao"
	"ae" ",c" "`e" "e'" "e<" "\"e" "e^." "i'" "i^" "i-"
	"g(" "~n" "`o" "o'" "o^" "~o" "\"o" "-:" "/o" "`u"
	"u'" "u^" "\"u" "i." ",s" "\"y"]
     )
    (latin-iso8859-14
     . [255 "B`" "b`" "Pd" "C`" "c`" "D`" "SE" "`W" "(c)"
	"W'" "d`" "`Y" "--" "(R)" "\"Y" "F`" "f`" "G`" "g`"
	"M`" "m`" ".P" "P`" "`w" "p`" "w'" "S`" "`y" "\"W"
	"\"w" "s`" "`A" "A'" "A^" "~A" "\"A" "Ao" "AE" ",C"
	"`E" "E'" "E^" "\"E" "`I" "I'" "I^" "\"I" "W^" "~N"
	"`O" "O'" "O^" "~O" "\"O" "T`" "/O" "`U" "U'" "U^"
	"\"U" "Y'" "Y^" "ss" "`a" "a'" "a^" "~a" "\"a" "ao"
	"ae" ",c" "`e" "e'" "e^" "\"e" "`i" "i'" "i^" "\"i"
	"w^" "~n" "`o" "o'" "o^" "~o" "\"o" "t`" "/o" "`u"
	"u'" "u^" "\"u" "y'" "y^" "\"y"]
    )
    (latin-iso8859-15
     . [255 "!I" "|c" "Pd" "E=" "Ye" "S<" "SE" "s<" "(c)"
        "_a" "<<" "~" "--" "(R)" "'-" "^o" "+-" "^2" "^3"
	"Z<" "u" ".P" "^." "z<" "^1" "_o" ">>" "OE" "oe"
	"\"Y" "?I" "`A" "A'" "A^" "~A" "\"A" "Ao" "AE" ",C"
	"`E" "E'" "E^" "\"E" "`I" "I'" "I^" "\"I" "-D" "~N"
	"`O" "O'" "O^" "~O" "\"O" "*x" "/O" "`U" "U'" "U^"
	"\"U" "Y'" "|P" "ss" "`a" "a'" "a^" "~a" "\"a" "ao"
	"ae" ",c" "`e" "e'" "e^" "\"e" "`i" "i'" "i^" "\"i"
	"-d" "~n" "`o" "o'" "o^" "~o" "\"o" "-:" "/o" "`u"
	"u'" "u^" "\"u" "y'" "|p" "\"y"]
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
	  (or glyph (setq glyph dos-unsupported-char-glyph))
	  (if (or (not (equal chset built-in-set))
		  (>= i cp-decoder-len)
		  (null (aref cp-decoder i)))
	      (aset disp-tab (make-char chset (+ i (logand offset 127)))
		    (vconcat
		     (if (numberp glyph)
			 (char-to-string glyph)
		       (if (> (length glyph) 1) (concat "{" glyph "}")
			 glyph)))))
	  (setq i (1+ i))))
      (setq surrogates (cdr surrogates)))))

(defvar IT-unicode-translations
  '(
    (mule-unicode-0100-24ff		; charset
     256				; base
     256 563				; first, last
     [ "A-" "a-" "A(" "a(" "A;" "a;" "C'" "c'" "C>" "c>" ; Latin Extended-A
       "C." "c." "C<" "c<" "D<" "d<" "D/" "d/" "E-" "e-"
       "E(" "e(" "E." "e." "E;" "e;" "E<" "e<" "G>" "g>"
       "G(" "g(" "G." "g." "G," "g," "H>" "h>" "H/" "h/"
       "I~" "i~" "I-" "i-" "I(" "i(" "I;" "i;" "I." "i."
       "IJ" "ij" "J>" "j>" "K," "k," "kk" "L'" "l'" "L,"
       "l," "L<" "l<" "L." "l." "L/" "l/" "N'" "n'" "N,"
       "n," "N<" "n<" "'n" "NG" "ng" "O-" "o-" "O(" "o("
       "O\"" "o\"" "OE" "oe" "R'" "r'" "R," "r," "R<" "r<"
       "S'" "s'" "S>" "s>" "S," "s," "S<" "s<" "T," "t,"
       "T<" "t<" "T/" "t/" "U~" "u~" "U-" "u-" "U(" "u("
       "U0" "u0" "U\"" "u\"" "U;" "u;" "W>" "w>" "Y>" "y>"
       "Y:" "Z'" "z'" "Z." "z." "Z<" "z<" "s1"                ; 017f
       "b/" "B2" "=B" "=b" "B6" "b6" "!C" "C2" "c2" "-D" ;Lat. Extended-B
       "D2" "=D" "=d" "!d" "!E" "-E" "Eps" "F2" "f2" "G2"
       "V0" "hv" "io" "-I" "K2" "k2" "-l"  "la-" "!M" "2N"
       "n_" "-O" "O9" "o9" "OI" "oi" "P2" "p2" "'R" "!S"
       "!s" "Esh" "!esh" "t~" "T2" "t2" "T~" "U9" "u9" "Ups"
       "V2" "Y2" "y2" "Z/" "z/" "ED" "!ED" "!ed" "ed;" "2/"
       "5-" "_5-" "ts" "wn" "|_" "||" "|=" "!_" "DZ<" "Dz<"
       "dz<" "LJ3" "Lj3" "lj3" "NJ3" "Nj3" "nj3" "A<" "a<" "I<"
       "i<" "O<" "o<" "U<" "u<" "U:-" "u:-" "U:'" "u:'" "U:<"
       "u:<" "U:!" "u:!" "e1" "A:-" "a:-" "A.-" "a.-" "AE-" "ae-"
       "G/" "g/" "G<" "g<" "K<" "k<" "O;" "o;" "O1" "o1"
       "EZ" "ez" "j<" "DZ3" "Dz3" "dz3" "G'" "g'" "Hv" "Wn"
       "N`" "n`" "AA'" "aa'" "AE'" "ae'" "O/'" "o/'" "A!!" "a!!"
       "A)" "a)" "E!!" "e!!" "E)" "e)" "I!!" "i!!" "I)" "i)"
       "O!!" "o!!" "O)" "o)" "R!!" "r!!" "R)" "r)" "U!!" "u!!"
       "U)" "u)" ",S" ",s" ",T" ",t" "'3" "'3_" "H<" "h<"
       nil nil "8" "8_" "Z2" "z2" "A." "a." "E," "e,"
       "O:-" "o:-" "O~-" "o~-" "O." "o." "O.-" "o.-" "Y-" "y-"] ; 0x233

     )

    (mule-unicode-0100-24ff		; charset
     256				; base
     884 1123				; first, last
     [ "'" "," nil nil nil nil "j3" nil nil nil        ; Greek
       "?;" nil nil nil nil nil "'*" "'%" "A%" ".*"
       "E%" "Y%" "I%" nil "O%" nil "U%" "W%" "i3" "A*"
       "B*" "G*" "D*" "E*" "Z*" "H*" "Th*" "I*" "K*" "L*"
       "M*" "N*" "C*" "O*" "P*" "R*" nil "S*" "T*" "U*"
       "F*" "X*" "Q*" "W*" "J*" "V*" "a%" "e%" "y%" "i%"
       "u3" "a*" "b*" "g*" "d*" "e*" "z*" "h*" "th*" "i*"
       "k*" "l*" "m*" "n*" "c*" "o*" "p*" "r*" "*s" "s*"
       "t*" "u*" "f*" "x*" "q*" "w*" "j*" "v*" "o%" "u%"
       "w%" nil "b3" "th%" "U2*" "'U2*" "U:2*" "ph*" "pi*" "ka*"
       nil nil "Sti" "sti" "Dig" "dig" "Kop" "kop" "Sam" "sam"
       "She" "she" "Fei" "fei" "Khe" "khe" "Hor" "hor" "Gan" "gan"
       "Shi" "shi" "Dei" "dei" "ka*" "rh*" "ls*" "yo*" nil nil
       nil nil nil nil nil nil nil nil nil nil
       "IE'" "E:" "D%" "G%" "IE" "DS" "II" "YI" "J%" "LJ" ; Cyrillic
       "NJ" "Ts"  "KJ" "`I=" "V%" "DZ" 65 "B=" 66 "G="
       68 69 "Z%" 51 85 "J=" 75 "L=" 77 72
       79 "P=" 80 67 84 89 "F=" 88 "C=" "C%"
       "S%" "Sc" "=\"" "Y=" "%\"" "Ee" "JU" "JA" 97 "b="
       98 "g=" 103 101 "z%" "z=" 117 "j=" 107 "l="
       109 "n=" 111 "p=" 112 99 "t=" 121 "f=" 120
       "c=" "c%" "s%" "sc" "='" "y=" "%'" "ee" "ju" "ja"
       "ie'" "e:" "d%" "g%" "ie" "ds" "ii" "yi" "j%" "lj"
       "nj" "ts" "kj" "v%" "`i=" "dz" "OM=" "om=" "Y3" "y3"] ; 0x463
     )

    (mule-unicode-0100-24ff		; charset
     256				; base
     1454 1645				; first, last
     [ nil nil ":'" "v:" "-:" "-':" ".'" ".." "v'" "-'"
       "-," "`." nil "\\." "(.)" "|'" "`-" nil "||" nil
       nil "::"  nil nil nil nil nil nil nil nil
       nil nil nil nil
       "A+" "B+" "G+" "D+" "H+" "W+" "Z+" "X+" "Tj" "J+" ; Hebrew
       "K%" "K+" "L+" "M%" "M+" "N%" "N+" "S+" "E+" "P%"
       "P+" "Zj" "ZJ" "Q+" "R+" "Sh" "T+" nil nil nil
       nil nil "WW+" "WJ+" "JJ+" "'+" "\"+" nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       ",+" nil nil nil nil nil nil nil nil nil          ; Arabic
       nil nil nil nil nil ";+" nil nil nil "?+"
       nil "H'" "aM" "aH" "wH" "ah" "yH" "a+" "b+" "tm"
       "t+" "tk" "g+" "hk" "x+" "d+" "dk" "r+" "z+" "s+"
       "sn" "c+" "dd" "tj" "zH" "e+" "i+" nil nil nil
       nil nil "++" "f+" "q+" "k+" "l+" "m+" "n+" "h+"
       "w+" "j+" "y+" ":+" "\"+" "=+" "/+" "'+" "1+" "3+"
       "0+" nil nil nil nil nil nil nil nil nil
       nil nil nil nil "0a" "1a" "2a" "3a" "4a" "5a"
       "6a" "7a" "8a" "9a" "a%" "a." "a," "a*" ]
     )

    (mule-unicode-0100-24ff		; charset
     256				; base
     7680 9450				; first, last
     [ "A-0" "a-0" "B." "b." "B-." "b-." "B_" "b_" "C,'" "c,'" ; Lat Ext Add
       "D." "d." "D-." "d-." "D_" "d_" "D," "d," "D->" "d->"
       "E-!" "e-!" "E-'" "e-'" "E->" "e->" "E-?" "e-?" "E,(" "e,("
       "F." "f." "G-" "g-" "H." "h." "H-." "h-." "H:" "h:"
       "H," "h," "H-(" "h-(" "I-?" "i-?" "I:'" "i:'" "K'" "k'"
       "K-." "k-." "K_" "k_" "L-." "l-." "_L-." "_l-." "L_" "l_"
       "L->" "l->" "M'" "m'" "M." "m." "M-." "m-." "N." "n."
       "N-." "n-." "N_" "n_" "N->" "n->" "O?'" "o?'" "O?:" "o?:"
       "O-!" "o-!" "O-'" "o-'" "P'" "p'" "P." "p." "R." "r."
       "R-." "r-." "_R-." "_r-." "R_" "r_" "S." "s." "S-." "s-."
       "S'." "s'." "S<." "s<." ".S-." ".s-." "T." "t." "T-." "t-."
       "T_" "t_" "T->" "t->" "U_:" "u_:" "U-?" "u-?" "U->" "u->"
       "U?'" "u?'" "U-:" "u-:" "V?" "v?" "V-." "v-." "W!" "w!"
       "W'" "w'" "W:" "w:" "W." "w." "W-." "w-." "X."  "x."
       "X:" "x:" "Y." "y." "Z>" "z>" "Z-." "z-." "Z_" "z_"
       "h_" "t:" "w0" "y0" "a))" "s1." nil nil nil nil
       "A-." "a-." "A2" "a2" "A>'" "a>'" "A>!" "a>!" "A>2" "a>2"
       "A>~" "a>~" ".A>" ".a>" "A('" "a('" "A(!" "a(!" "A(2" "a(2"
       "A(~" "a(~" ".A(" ".a(" "E-." "e-." "E2" "e2" "E~" "e~"
       "E>'" "e>'" "E>!" "e>!" "E>2" "e>2" "E>~" "e>~" ".E>" ".e>"
       "I2" "i2" "I-." "i-." "O-." "o-." "O2" "o2" "O>'" "o>'"
       "O>!" "o>!" "O>2" "o>2" "O>~" "o>~" ".O>" ".o>" "O9'" "o9'"
       "O9!" "o9!" "O92" "o92" "O9~" "o9~" ".O9" ".o9" "U-." "u-."
       "U2" "u2" "U9'" "u9'" "U9!" "u9!" "U92" "u92" "U9~" "u9~"
       ".U9" ".u9" "Y!" "y!" "Y-." "y-." "Y2" "y2" "Y~" "y~"
       nil nil nil nil nil nil "a*," "a*;" nil nil ; Greek Ext (0x1f00)
       nil nil nil nil "A*," "A*;" nil nil nil nil
       nil nil "e*," "e*;" nil nil nil nil nil nil
       "E*," "E*;" nil nil nil nil nil nil "y*," "y*;"
       nil nil nil nil nil nil "Y*," "Y*;" nil nil
       nil nil nil nil "i*," "i*;" nil nil nil nil
       nil nil "I*," "I*;" nil nil nil nil nil nil
       "o*," "o*;" nil nil nil nil nil nil "O*," "O*;"
       nil nil nil nil nil nil "u*," "u*;" nil nil
       nil nil nil nil nil "U*;" nil nil nil nil
       nil nil "w*," "w*;" nil nil nil nil nil nil
       "W*," "W*;" nil nil nil nil nil nil "a*!" "a*'"
       "e*!" "e*'" "y*!" "y*'" "i*!" "i*'" "o*!" "o*'" "u*!" "u*'"
       "w*!" "w*'" nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil "a*(" "a*-" nil "a*j" nil nil "a*~" nil
       "A*(" "A*-" "A*!" "A*'" "A*J" ")*" "J3" ",," "?*" "?:"
       nil "y*j" nil nil "y*?" nil "E*!"  "E*'" "Y*!" "Y*'"
       "Y*J" ",!" ",'" "?," "i*(" "i*-" nil nil nil nil
       "i*?" nil "I*(" "I*-" "I*!" "I*'" nil ";!" ";'" "?;"
       "u*(" "u*-" nil nil "r*," "r*;" "u*?" nil "U*(" "U*-"
       "U*!" "U*'" "R*;" "!:" ":'" "!*" nil nil nil "w*j"
       nil nil "w*?" nil "O*!" "O*'" "W*!" "W*'" "W*J" "/*"
       ";;" nil nil nil "1N" "1M" "3M" "4M" "6M" nil          ; Gen Punct
       nil "1T" "1H" nil nil nil "LRM" "RLM" "-1" nil
       nil "--" "---" "===" "!2" "=2" "6`" "'9" ".9" "9'"
       "``" "''" ":9" "9``" "/-" "/=" "sb" "3b" nil ".."
       "..." ".-" "LSep" "PSep" "LR[" "RL[" "PDF" "LRO" "RLO" 255
       "%o" "%oo" "'" "''" "\"'" "`" "``" "```" ".^" "<,"
       ",>" ":X" "!!" "?!" "'-" nil nil nil nil "-b"
       "/f" nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil "^0" nil nil nil "^4" "^5"
       "^6" "^7" "^8" "^9" "^+" "^-" "^=" "^(" "^)" "^n"
       "_0" "_1" "_2" "_3" "_4" "_5" "_6" "_7" "_8" "_9"
       "_+" "_-" "_=" "_(" "_)" nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil "Ff" "Li" nil nil "Pt"
       nil "W=" "NIS" nil "E=" nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil "a/c" "a/s"
       nil "oC" nil "c/o" "c/u" "=e" nil "oF" nil nil
       nil nil "-h" "=h" nil nil nil nil nil nil
       "N0" "PO" nil nil nil nil "Re" nil "Rx" nil
       "SM" "TEL" "TM" nil nil nil "Om" nil nil nil
       "oK" "AO" nil nil "Est" nil nil nil nil nil
       nil "Aleph" "Bet" "Gimel" "Dalet" "=i=" nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil "1/3" "2/3" "1/5" "2/5" "3/5" "4/5" "1/6" "5/6" "1/8"
       "3/8" "5/8" "7/8" "1/" ".I" "II" "III" "IV" ".V" "VI"
       "VII" "VIII" "IX" "X" "XI" "XII" ".L" ".C" ".D" ".M"
       ".i" "ii" "iii" "iv" ".v" "vi" "vii" "viii" "ix" ".x"
       "xi" ".l" ".c" ".d" ".m" "CD" "DD" "CoD" "CI" nil
       nil nil nil nil nil nil nil nil nil nil
       nil "<-" "|^" "->" "|v" "<->" "v|^" "^\\" "/^" "\\v"
       "v/" "<-/" "/->" "<~" "~>" "<<-" "|^^" "->>" "|vv" "<-<"
       ">->" "<-|" "_|^" "|->" "-|v" "_v|^" "<-?" "?->" "<-o" "o->"
       "<~>" "<-/>" nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil "<=/" "<=/>" "/=>" "<=" "||^" "=>" "||v"
       "<=>" "v||^" "^\\\\" "//^" "\\\\v" "v//" "<-=" "=->" nil nil
       nil nil "<.." ":^" "..>" ":v" nil nil "<::" "::^"
       "::>" "::v" nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil "FA" "C." "dP" "TE" "~TE" "/0"
       "DE" "NB" "(-" "~(-" "e-" "-)" "~-)" "-e" "QED" "*P"
       nil "+Z" "--" "-+" ".+" "./" ".\\" "*-" "Ob" "Sb"
       "SQR" "CBR" nil "0(" "00" "-L" "-V" nil nil ".|"
       "~.|" "||" "/||" "AND" "OR" "(U" ")U" "In" "DI" nil
       "Io" nil nil nil nil nil ".:" ":." ":R" "::"
       ".-." "-:" ":-:" ":~:" "?~" "~?" "??" nil nil "/~"
       "-~" "~-" "/~-" "~=" "~/=" "/~=" "~~" "/~~" nil nil
       "=?" ")(" "v^" "^_" ".=" "=;" ".=." nil ":=" "=:"
       nil "=o" "=)" "=^" "=v" "*=" "=<>" "=df" nil "?="
       "!=" "-=" "!-=" "==" "=<" ">=" nil nil nil nil
       "<<" ">>" "()" "/)(" "!<" "!>" nil nil nil nil
       nil nil nil nil nil nil "<'" "`>" "=<'" "`>="
       "~<'" "`>~" "/<'" "/`>" "(C" ")C" "/(C" "/)C" "(_" ")_"
       "/(_" "/)_" nil nil nil nil nil nil nil nil
       nil nil nil "0+" "0-" "0x" "0/" "0." "0o" "0*"
       "0=" "0_" nil nil nil nil "|T" "T|" "-T" "_T"
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil "-,-"
       nil "XOR" "NAND" "NOR" nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil "<." ".>"
       "<<<" ">>>" nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil ":3" "..." nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil "Eh" nil  nil nil nil nil "<7" ">7"
       "7<" "7>" nil nil nil nil "~I" nil "(A" nil
       nil "TR" nil "=||" "88" nil nil nil nil nil
       nil nil "Iu" "Il" nil nil "-^-" "-`-" "D->" nil
       nil "</" "/>" "<-D" nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil "NUL" "SOH" "STX" "ETX"
       "EOT" "ENQ" "ACK" "BEL" "BS" "HT" "LF" "VT" "FF" "CR"
       "SS" "SI" "DLE" "DC1" "DC2" "DC3" "DC4" "NAK" "SYN" "ETB"
       "CAN" "EM" "SUB" "ESC" "FS" "GS" "RS" "US" "SP" "DEL"
       "b/" ",_," "NL" nil "?^" nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil "1-o" "2-o" "3-o" "4-o" "5-o" "6-o" "7-o" "8-o"
       "9-o" "10-o" "11-o" "12-o" "13-o" "14-o" "15-o" "16-o" "17-o" "18-o"
       "19-o" "20-o" "(1)" "(2)" "(3)" "(4)" "(5)" "(6)" "(7)" "(8)"
       "(9)" "(10)" "(11)" "(12)" "(13)" "(14)" "(15)" "(16)" "(17)" "(18)"
       "(19)" "(20)" "1." "2." "3." "4." "5." "6." "7." "8."
       "9." "10." "11." "12." "13." "14." "15." "16." "17." "18."
       "19." "20." "(a)" "(b)" "(c)" "(d)" "(e)" "(f)" "(g)" "(h)"
       "(i)" "(j)" "(k)" "(l)" "(m)" "(n)" "(o)" "(p)" "(q)" "(r)"
       "(s)" "(t)" "(u)" "(v)" "(w)" "(x)" "(y)" "(z)" "A-o" "B-o"
       "C-o" "D-o" "E-o" "F-o" "G-o" "H-o" "I-o" "J-o" "K-o" "L-o"
       "M-o" "N-o" "O-o" "P-o" "Q-o" "R-o" "S-o" "T-o" "U-o" "V-o"
       "W-o" "X-o" "Y-o" "Z-o" "a-o" "b-o" "c-o" "d-o" "e-o" "f-o"
       "g-o" "h-o" "i-o" "j-o" "k-o" "l-o" "m-o" "n-o" "o-o" "p-o"
       "q-o" "r-o" "s-o" "t-o" "u-o" "v-o" "w-o" "x-o" "y-o" "z-o"
       "0-o" ]
     )
    )

  "A list of mule-unicode-* character sets and the strings that
should be used to represent the characters from each set on a DOS
terminal which does not have corresponding glyphs built into the
installed codepage.")

(defun IT-setup-unicode-display (&optional table)
  "Set up display table TABLE for displaying mule-unicode-* characters
on a DOS terminal.  If TABLE is nil or omitted, `standard-display-table'
is used."
  (interactive)
  (let ((disp-tab (or table standard-display-table))
	(tail IT-unicode-translations)
	translation)
    (while tail
      (setq translation (car tail) tail (cdr tail))
      (let* ((chset (car translation))
	     (base (nth 1 translation))
	     (first (nth 2 translation))
	     (last (nth 3 translation))
	     (table (nth 4 translation))
	     (i 0)
	     (this (- first base))
	     glyph)
	(while (<= i (- last first))
	  (setq glyph (aref table i))
	  (or glyph (setq glyph dos-unsupported-char-glyph))
	  (aset disp-tab (make-char chset
				    (+ (/ this 96) 32)
				    (+ (% this 96) 32))
		(vconcat
		 (if (numberp glyph)
		     (char-to-string glyph)
		   (if (> (length glyph) 1) (concat "{" glyph "}")
		     glyph))))
	  (setq i (1+ i) this (1+ this)))))))

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
    ;; It's time: too many input methods in leim/quail produce
    ;; Unicode characters.  Let the user see them.
    (IT-setup-unicode-display)
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
	     ;; look like at all!  (Use `C-x =' to see what they
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
      ;; It's time: too many input methods in leim/quail produce
      ;; Unicode characters.  Let the user see them.
      (IT-setup-unicode-display)
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

;; arch-tag: eea04c06-7311-4b5a-b531-3c1be1b070af
;;; internal.el ends here
