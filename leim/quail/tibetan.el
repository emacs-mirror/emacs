;;; quail/tibetan.el -- Quail package for inputting Tibetan characters

;; Copyright (C) 1995 Free Software Foundation, Inc.
;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.

;; Keywords: multilingual, input method, Tibetan

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

;; Author: Toru TOMABECHI, <Toru.Tomabechi@orient.unil.ch>

;; Created: Feb. 17. 1997

;; History:
;; 1997.03.13 Support for inputting special signs and punctuations added.
;;            (Only Ext. Wylie input)

;;; Code:

(require 'quail)

;;;
;;; Functions for making some composite punctuations.
;;;

(defun tibetan-quail-bzhi-shad (&rest ignore)
  (quail-delete-region)
  (quail-delete-overlays)
  (insert (compose-chars ?$(7!>(B '(mr . ml) ?\x20 '(mr . ml) ?$(7!>(B))
  (throw 'quail-tag nil))

(defun tibetan-quail-nyi-zla (&rest ignore)
  (quail-delete-region)
  (quail-delete-overlays)
  (insert (compose-chars ?$(7#R(B '(mr . ml) ?$(7#S(B))
  (throw 'quail-tag nil))

(defun tibetan-quail-nyi-zla-phur-shad (&rest ignore)
  (quail-delete-region)
  (quail-delete-overlays)
  (insert (compose-chars ?$(7#R(B '(mr . ml) ?$(7#S(B '(bc . tl) ?$(7!I(B))
  (throw 'quail-tag nil))

(defun tibetan-quail-nyi-zla-double (&rest ignore)
  (quail-delete-region)
  (quail-delete-overlays)
  (insert (compose-chars ?$(7#R(B '(mr . ml) ?$(7#S(B '(mr . ml) ?$(7#S(B))
  (throw 'quail-tag nil))

(defun tibetan-quail-nyi-zla-triple (&rest ignore)
  (quail-delete-region)
  (quail-delete-overlays)
  (insert (compose-chars ?$(7#R(B '(mr . ml) ?$(7#S(B '(mr . ml) ?$(7#S(B '(mr . ml) ?$(7#S(B))
  (throw 'quail-tag nil))

;;;
;;; Setting-ups for Extended Wylie input.
;;;

(defun quail-tibetan-input-wylie (key &rest ignore)
  (let (pc)
    (quail-delete-region)
    (quail-delete-overlays)
    (setq pc (preceding-char))
    (if (not (eq (point) (point-min)))
	(delete-backward-char 1 nil))
    (insert (tibetan-composition pc key))
    (throw 'quail-tag nil)))


(quail-define-package "tibetan-wylie" "Tibetan" "TIBw" t
"Tibetan character input by Extended Wylie key assignment.

    +-------------------------------------+
    |2$(7"!`#T1$(8!;(B k |2$(7""`#T1$(8!;(B kh |2$(7"#`#T1$(8!;(B g  |2$(7"$`#T1$(8!;(B gh |2$(7"%`#T1$(8!;(B ng|   $(7"S(B i          $(8!=(B        /
    |2$(7"&`#T1$(8!;(B c |2$(7"'`#T1$(8!;(B ch |2$(7"(`#T1$(8!;(B j  |       |$(7"*$(8!;(B ny|   $(7"U(B u          $(7!>(B       //
    |$(7"+$(8!;(B T |$(7",$(8!;(B TH |$(7"-$(8!;(B D  |$(7".$(8!;(B DH |$(7"/$(8!;(B N |   $(7"[(B e          2$(7!>P(B P$(7!>1(B    ////
    |$(7"0$(8!;(B t |$(7"1$(8!;(B th |$(7"2$(8!;(B d  |$(7"3$(8!;(B dh |$(7"4$(8!;(B n |   $(7"](B o          $(7!A(B       ;
    |$(7"5$(8!;(B p |$(7"6$(8!;(B ph |$(7"7$(8!;(B b  |$(7"8$(8!;(B bh |$(7"9$(8!;(B m |   $(7"\(B ai (ee, E) $(8!?(B        $
    |$(7":$(8!;(B ts|$(7";$(8!;(B tsh|$(7"<$(8!;(B dz |$(7"=$(8!;(B dzh|$(7">$(8!;(B w |   $(7"^(B au (oo, O) $(8!@(B        &
    |$(7"?$(8!;(B zh|$(7"@$(8!;(B z  |$(7"A$(8!;(B '  |       |$(7"B$(8!;(B y |   $(7"a(B I          2$(7#RP#SP#S1(B   *
    |$(7"C$(8!;(B r |$(7"D$(8!;(B l  |$(7"E$(8!;(B sh |$(7"F$(8!;(B SH |$(7"G$(8!;(B s |   $(7"`(B :          2$(7#RP#SP#SP#S1(B #
    |$(7"H$(8!;(B h |$(7"I$(8!;(B A  |$(7"J$(8!;(B kSH|       |      |   $(7"_(B M           $(7!l(B $(7!m(B   < >
    +-------------------------------------+   $(8!D(B  %
    (The consonant $(7"I$(8!;(B must be typed explicitly.)

  NOT SPECIFIED IN EXT. WYLIE:
    +--------------------------------------------------------+
    |$(7"c(B = ~ |$(7"d(B = ` |$(7"e(B = , |$(7"f(B = @ |$(7!g(B = _o|$(7!e(B = _O|2$(7#RP#S_!I1(B = ^|
    +--------------------------------------------------------+
    |$(7"i(B = x |$(7"j(B = X |$(7"g(B = v |$(7"h(B = V |$(7"k(B = q |$(7"l(B = Q |
    +-----------------------------------------------+

  SPECIAL KEYS
  +     :  Consonant Stacking
          \(Consonant stacking for ordinary Tibetan is done automatically)
  -     : No Consonant Stacking
          \(To suppress automatic stacking for \"g-y\",
            and to get da-drag in  -r-d, -l-d .)
  |     : Special signs.

  Tsheg is assigned to SPC. Space is assigned to period '.'.
"
 nil nil nil)

(quail-define-rules
 ("."    ?\x20)
 ("k"    quail-tibetan-input-wylie)
 ("g"    quail-tibetan-input-wylie)
 ("c"    quail-tibetan-input-wylie)
 ("j"    quail-tibetan-input-wylie)
 ("T"    quail-tibetan-input-wylie)
 ("D"    quail-tibetan-input-wylie)
 ("N"    quail-tibetan-input-wylie)
 ("t"    quail-tibetan-input-wylie)
 ("d"    quail-tibetan-input-wylie)
 ("n"    quail-tibetan-input-wylie)
 ("p"    quail-tibetan-input-wylie)
 ("b"    quail-tibetan-input-wylie)
 ("m"    quail-tibetan-input-wylie)
 ("w"    quail-tibetan-input-wylie)
 ("z"    quail-tibetan-input-wylie)
 ("'"    quail-tibetan-input-wylie)
 ("y"    quail-tibetan-input-wylie)
 ("r"    quail-tibetan-input-wylie)
 ("l"    quail-tibetan-input-wylie)
 ("SH"   quail-tibetan-input-wylie)
 ("s"    quail-tibetan-input-wylie)
 ("h"    quail-tibetan-input-wylie)
 ("H"    quail-tibetan-input-wylie)
 ("A"    quail-tibetan-input-wylie)
 ("+k"   quail-tibetan-input-wylie)
 ("+g"   quail-tibetan-input-wylie)
 ("+c"   quail-tibetan-input-wylie)
 ("+j"   quail-tibetan-input-wylie)
 ("+T"   quail-tibetan-input-wylie)
 ("+D"   quail-tibetan-input-wylie)
 ("+N"   quail-tibetan-input-wylie)
 ("+t"   quail-tibetan-input-wylie)
 ("+d"   quail-tibetan-input-wylie)
 ("+n"   quail-tibetan-input-wylie)
 ("+p"   quail-tibetan-input-wylie)
 ("+b"   quail-tibetan-input-wylie)
 ("+m"   quail-tibetan-input-wylie)
 ("+w"   quail-tibetan-input-wylie)
 ("+z"   quail-tibetan-input-wylie)
 ("+'"   quail-tibetan-input-wylie)
 ("+y"   quail-tibetan-input-wylie)
 ("+r"   quail-tibetan-input-wylie)
 ("+l"   quail-tibetan-input-wylie)
 ("+SH"  quail-tibetan-input-wylie)
 ("+s"   quail-tibetan-input-wylie)
 ("+h"   quail-tibetan-input-wylie)
 ("+H"   quail-tibetan-input-wylie)
 ("+A"   quail-tibetan-input-wylie)
 ("-d"  ?$(7"2(B)				; To avoid default stacking
 ("-y"  ?$(7"B(B)				; Idem.
 ("a"    quail-tibetan-input-wylie)	; invisible vowel sign
 ("i"    quail-tibetan-input-wylie)
 ("u"    quail-tibetan-input-wylie)
 ("e"    quail-tibetan-input-wylie)
 ("o"    quail-tibetan-input-wylie)
 ("I"    quail-tibetan-input-wylie)
 ("E"    quail-tibetan-input-wylie)
 ("O"    quail-tibetan-input-wylie)
 ("M"    quail-tibetan-input-wylie)
 ("~"    quail-tibetan-input-wylie)
 ("`"    quail-tibetan-input-wylie)
 (","    quail-tibetan-input-wylie)
 ("x"    quail-tibetan-input-wylie)
 ("X"    quail-tibetan-input-wylie)
 ("v"    quail-tibetan-input-wylie)
 ("V"    quail-tibetan-input-wylie)
 ("q"    quail-tibetan-input-wylie)
 ("Q"    quail-tibetan-input-wylie)
 ("_o"    quail-tibetan-input-wylie)
 ("_O"    quail-tibetan-input-wylie)
;;; ("_/"    quail-tibetan-input-wylie)
 (":"   ?$(8"`(B)
 (" "   ?$(8!;(B)
 ("/"   ?$(8!=(B)
 ("//"  ?$(7!>(B)
 ("////" tibetan-quail-bzhi-shad)
 ("$"   ?$(8!?(B)
 ("/\"" ?$(8!@(B)				; Not defined in Ext. Wylie.
 ("&"   ?$(8!@(B)
 (";"   ?$(8!A(B)
 ("%"   ?$(8!D(B)
 ("!"   ?$(7!8(B)
 ("<"   ?$(7!l(B)
 (">"   ?$(7!m(B)
 ("@"   ?$(7"f(B)
 ("*" tibetan-quail-nyi-zla-double)
 ("#" tibetan-quail-nyi-zla-triple)
 ("^" tibetan-quail-nyi-zla-phur-shad)
 ("0" ?$(7!P(B)
 ("1" ?$(7!Q(B)
 ("2" ?$(7!R(B)
 ("3" ?$(7!S(B)
 ("4" ?$(7!T(B)
 ("5" ?$(7!U(B)
 ("6" ?$(7!V(B)
 ("7" ?$(7!W(B)
 ("8" ?$(7!X(B)
 ("9" ?$(7!Y(B)
 ("-0" ?$(7!c(B)
 ("-1" ?$(7!Z(B)
 ("-2" ?$(7![(B)
 ("-3" ?$(7!\(B)
 ("-4" ?$(7!](B)
 ("-5" ?$(7!^(B)
 ("-6" ?$(7!_(B)
 ("-7" ?$(7!`(B)
 ("-8" ?$(7!a(B)
 ("-9" ?$(7!b(B)
 ("|"  "$(7!1!2!3!9!:!B!C!E!F!G!H!I!J!K!L!M!N!O!d!f!h!j!k!n!o(B")
 )

;;;
;;; Setting-ups for TibKey input
;;;

(defconst tibetan-tibkey-to-transcription-alist
  '(
    ("`" . "`")				; sna ldan
    ("~" . "~")				; sna ldan + nada
    ("q" . "k")				; ka
    ("Q" ."kSH")			; kSHa
    ("w" . "kh")			; kha
    ("e" . "g")				; ga
    ("r" . "ng")			; nga
    ("t" . "c")				; ca
    ("T" . "I")				; gi gu log
    ("y" . "ch")			; cha
    ("u" . "j")				; ja
    ("i" . "ny")			; nya
    ("o" . "t")				; ta
    ("O" . "T")				; Ta
    ("p" . "th")			; tha
    ("P" . "TH")			; THa
    ("[" . "d")				; da 
    ("{" . "D")				; Da
    ("]" . "n")				; na
    ("}" . "N")				; Na
    ("a" . "p")				; pa
    ("A" . "a")				; Vowel a (not used in original TibKey)
    ("s" . "ph")			; pha
    ("d" . "b")				; ba
    ("f" . "m")				; ma
    ("F" . "M")				; anusvara
    ("g" . "u")				; zhabs kyu
    ("G" . "i")				; gi gu
    ("H" . ",")				; viraama
    ("j" . "o")				; naro
    ("J" . "e")				; 'greng bu
    ("k" . "ts")			; tsa
    ("l" . "tsh")			; tsha
    (";" . "dz")                        ; dza
    ("'" . "w")				; wa
    ("\"" . "+w")			; wa zur
    ("z" . "zh")			; zha
    ("x" . "z")				; za
    ("c" . "'")				; 'a
    ("C" . "+'")			; 'a chung
    ("v" . "y")				; ya
    ("V" . "+y")			; ya btags
    ("b" . "r")				; ra
    ("B" . "+r")			; ra btags
    ("n" . "l")				; la
    ("N" . "+l")			; la btags
    ("m" . "sh")			; sha
    ("M" . "SH")			; SHa
    ("," . "s")				; sa
    ("." . "h")				; ha
    ("/" . "A")				; Aa
    ;;subjoined
    ("hq" . "+k")			; ka
    ("hQ" ."+kSH")			; kSHa
    ("hw" . "+kh")			; kha
    ("he" . "+g")			; ga
    ("hr" . "+ng")			; nga
    ("ht" . "+c")			; ca
    ("hy" . "+ch")			; cha
    ("hu" . "+j")			; ja
    ("hi" . "+ny")			; nya
    ("ho" . "+t")			; ta
    ("hO" . "+T")			; Ta
    ("hp" . "+th")			; tha
    ("hP" . "+TH")			; THa
    ("h[" . "+d")			; da 
    ("h{" . "+D")			; Da
    ("h]" . "+n")			; na
    ("h}" . "+N")			; Na
    ("ha" . "+p")			; pa 
    ("hs" . "+ph")			; pha
    ("hd" . "+b")			; ba
    ("hf" . "+m")			; ma
    ("hk" . "+ts")			; tsa
    ("hl" . "+tsh")			; tsha
    ("h;" . "+dz")                      ; dza
    ("h'" . "+w")			; wa
    ("hz" . "+zh")			; zha
    ("hx" . "+z")			; za
    ("hc" . "+'")			; 'a
    ("hv" . "+y")			; ya
    ("hb" . "+r")			; ra
    ("hn" . "+l")			; la
    ("hm" . "+sh")			; sha
    ("hM" . "+SH")			; SHa
    ("h," . "+s")			; sa
    ("h." . "+h")			; ha
    ("h/" . "+A")			; Aa
    ))

(defun quail-tibetan-input-tibkey (key &rest ignore)
  (let (trans pc)
    (setq trans (cdr (assoc key tibetan-tibkey-to-transcription-alist)))
    (quail-delete-region)
    (quail-delete-overlays)
    (setq pc (preceding-char))
    (if (not (eq (point) (point-min)))
	(delete-backward-char 1 nil))
    (insert (tibetan-composition pc trans))
    (throw 'quail-tag nil)))



(quail-define-package "tibetan-tibkey" "Tibetan" "TIBt" t
"Tibetan character input by TibKey key assignment.

(This implementation is still incomplete.
 Therefore, the following key assignment is a provisional one.)

  [NOT SHIFTED]

  +-------------------------------------------------------+
  |`$(7"d(B|1$(7!Q(B|2$(7!R(B|3$(7!S(B|4$(7!T(B|5$(7!U(B|6$(7!V(B|7$(7!W(B|8$(7!X(B|9$(7!Y(B|0$(7!P(B|-  |=  |\\$(7!8(B|
  +-------------------------------------------------------+
     |q$(7"!(B|w$(7""(B|e$(7"#(B|r$(7"%(B|t$(7"&(B|y$(7"'(B|u$(7"((B|i$(7"*(B|o$(7"0(B|p$(7"1(B|[$(7"2(B|]$(7"4(B|
     +-----------------------------------------------+
      |a$(7"5(B| s$(7"6(B| d$(7"7(B|f$(7"9(B|g$(7"U(B|h  |j$(7"](B|k$(7":(B|l$(7";(B|;$(7"<(B|'$(7">(B|
      +---------------------------------------------+
         |z$(7"?(B|x$(7"@(B|c$(7"A(B|v$(7"B(B|b$(7"C(B|n$(7"D(B|m$(7"E(B|,$(7"G(B|.$(7"H(B|/$(7"I(B|
         +---------------------------------------+
  The key 'h' is used for consonant stacking.

  [SHIFTED]

  +----------------------------------------------------------+
  |~$(7"c(B|!2$(7#RP#S1(B|@$(7#S(B|#  |$  |%$(8!D(B |^$(7!1(B|&  |*  |($(7!l(B|)$(7!m(B|_  |+$(7!A(B| |$(7!8(B|
  +----------------------------------------------------------+
     |Q$(7"J(B|W$(7#T(B|E  |R  |T$(7"a(B|Y  |U  |I$(7"f(B|O$(7"+(B|P$(7",(B|{$(7"-(B|}$(7"/(B|
     +-----------------------------------------------+
      |A  |S  |D  |F$(7"_(B|G$(7"S(B|H$(7"e(B|J$(7"[(B|K  |L  |:$(7"`(B|\"$(7#>(B|
      +-------------------------------------------+
         |Z  |X  |C$(7#A(B|V$(7#B(B|B$(7#C(B|N$(7#D(B|M$(7"F(B|<  |>  |?$(8!=(B |
         +---------------------------------------+        

  DIFFERENCE FROM THE ORIGINAL TIBKEY:

    1. Vowel 'a' should be typed explicitly by the key 'A'.
       This is really inconvenient. But to make the coding
       scheme clear, it is desirable to have an explicite
       vowel sign for 'a'.
    2. Tsheg is assigned to SPC key. You can input a space
       by typing '>'.
    4. To avoid the default stacking $(7$B(B and to obtain $(7"#"B(B,
       type 'E' instead of 'v' (=$(7"B(B).
    3. There are many characters that are not supported in the
       current implementation (especially special signs). I hope
       I'll complete in a future revision.
"
 nil t)

(quail-define-rules
 ("`" quail-tibetan-input-tibkey)	; sna ldan, not supported yet
 ("~" quail-tibetan-input-tibkey)	; sna ldan + nada
 ("1" ?$(7!Q(B)
 ("!" tibetan-quail-nyi-zla)		; nyi zla long
 ("2" ?$(7!R(B)
 ("@" ?$(7#S(B)				; nyi zla simple
 ("3" ?$(7!S(B)
;;; ("#" )
 ("4" ?$(7!T(B)
;;; ("$" )
 ("5" ?$(7!U(B)
 ("%" ?$(8!D(B)
 ("6" ?$(7!V(B)
 ("^" ?$(7!1(B)
 ("7" ?$(7!W(B)
 ("8" ?$(7!X(B)
;;; ("*" ) ; avagraha, not supported yet
 ("9" ?$(7!Y(B)
 ("(" ?$(7!l(B)
 ("0" ?$(7!P(B)
 (")" ?$(7!m(B)
;;; ("-" ) ; enphatic, not yet supported 
;;; ("_" ) ; id.
;;; ("=" ) ; special sign, not yet supported
 ("+" ?$(8!A(B)
 ("\\" ?$(8!?(B)
 ("|" ?$(7!8(B)
 ("q" quail-tibetan-input-tibkey)	; ka
 ("Q" quail-tibetan-input-tibkey)	; kSHa
 ("w" quail-tibetan-input-tibkey)	; kha
 ("e" quail-tibetan-input-tibkey)	; ga
 ("E" ?$(7"B(B)
 ("r" quail-tibetan-input-tibkey)	; nga
 ("t" quail-tibetan-input-tibkey)	; ca
 ("T" quail-tibetan-input-tibkey)	; gi gu log
 ("y" quail-tibetan-input-tibkey)	; cha
 ("u" quail-tibetan-input-tibkey)	; ja
 ("i" quail-tibetan-input-tibkey)	; nya
 ("I" ?$(7"f(B)				; avagraha
 ("o" quail-tibetan-input-tibkey)	; ta
 ("O" quail-tibetan-input-tibkey)	; Ta
 ("p" quail-tibetan-input-tibkey)	; tha
 ("P" quail-tibetan-input-tibkey)	; THa
 ("[" quail-tibetan-input-tibkey)	; da 
 ("{" quail-tibetan-input-tibkey)	; Da
 ("]" quail-tibetan-input-tibkey)	; na
 ("}" quail-tibetan-input-tibkey)	; Na
 ("a" quail-tibetan-input-tibkey)	; pa 
 ("A" quail-tibetan-input-tibkey)	; Vowel sign a
 ("s" quail-tibetan-input-tibkey)	; pha
 ("d" quail-tibetan-input-tibkey)	; ba
;;; ("D" ) ; special sign, not supported yet
 ("f" quail-tibetan-input-tibkey)	; ma
 ("F" quail-tibetan-input-tibkey)	; anusvara
 ("g" quail-tibetan-input-tibkey)	; zhabs kyu
 ("G" quail-tibetan-input-tibkey)	; gi gu
 ("H" quail-tibetan-input-tibkey)	; viraama
 ("j" quail-tibetan-input-tibkey)	; naro
 ("J" quail-tibetan-input-tibkey)	; 'greng bu
 ("k" quail-tibetan-input-tibkey);;tsa
;;; ("K" ) ; tsadru, not supported yet
 ("l" quail-tibetan-input-tibkey)	; tsha
 (";" quail-tibetan-input-tibkey)       ; dza
 (":" ?$(8"`(B)
 ("'" quail-tibetan-input-tibkey)	; wa
 ("\"" quail-tibetan-input-tibkey)	; wa zur
 ("z" quail-tibetan-input-tibkey)	; zha
 ("x" quail-tibetan-input-tibkey)	; za
 ("c" quail-tibetan-input-tibkey)	; 'a
 ("C" quail-tibetan-input-tibkey)	; 'a chung
 ("v" quail-tibetan-input-tibkey)	; ya
 ("V" quail-tibetan-input-tibkey)	; ya btags
 ("b" quail-tibetan-input-tibkey)	; ra
 ("B" quail-tibetan-input-tibkey)	; ra btags
 ("n" quail-tibetan-input-tibkey)	; la
 ("N" quail-tibetan-input-tibkey)	; la btags
 ("m" quail-tibetan-input-tibkey)	; sha
 ("M" quail-tibetan-input-tibkey)	; SHa
 ("," quail-tibetan-input-tibkey)	; sa
 ("." quail-tibetan-input-tibkey)	; ha
;;; (">" ?$(8!;(B) ; to be assigned to SPC
 (">" ?\x20)
 ("/" quail-tibetan-input-tibkey)	; Aa
 ("?" ?$(8!=(B)
 ("??" ?$(7!>(B)
 ("????" tibetan-quail-bzhi-shad)
 (" " ?$(8!;(B)
 ;;subjoined
 ("hq" quail-tibetan-input-tibkey)	; ka
 ("hQ" quail-tibetan-input-tibkey)	; kSHa
 ("hw" quail-tibetan-input-tibkey)	; kha
 ("he" quail-tibetan-input-tibkey)	; ga
 ("hr" quail-tibetan-input-tibkey)	; nga
 ("ht" quail-tibetan-input-tibkey)	; ca
 ("hy" quail-tibetan-input-tibkey)	; cha
 ("hu" quail-tibetan-input-tibkey)	; ja
 ("hi" quail-tibetan-input-tibkey)	; nya
 ("ho" quail-tibetan-input-tibkey)	; ta
 ("hO" quail-tibetan-input-tibkey)	; Ta
 ("hp" quail-tibetan-input-tibkey)	; tha
 ("hP" quail-tibetan-input-tibkey)	; THa
 ("h[" quail-tibetan-input-tibkey)	; da 
 ("h{" quail-tibetan-input-tibkey)	; Da
 ("h]" quail-tibetan-input-tibkey)	; na
 ("h}" quail-tibetan-input-tibkey)	; Na
 ("ha" quail-tibetan-input-tibkey)	; pa 
 ("hs" quail-tibetan-input-tibkey)	; pha
 ("hd" quail-tibetan-input-tibkey)	; ba
 ("hf" quail-tibetan-input-tibkey)	; ma
 ("hk" quail-tibetan-input-tibkey)	; tsa
 ("hl" quail-tibetan-input-tibkey)	; tsha
 ("h;" quail-tibetan-input-tibkey)      ; dza
 ("h'" quail-tibetan-input-tibkey)	; wa
 ("hz" quail-tibetan-input-tibkey)	; zha
 ("hx" quail-tibetan-input-tibkey)	; za
 ("hc" quail-tibetan-input-tibkey)	; 'a
 ("hv" quail-tibetan-input-tibkey)	; ya
 ("hb" quail-tibetan-input-tibkey)	; ra
 ("hn" quail-tibetan-input-tibkey)	; la
 ("hm" quail-tibetan-input-tibkey)	; sha
 ("hM" quail-tibetan-input-tibkey)	; SHa
 ("h," quail-tibetan-input-tibkey)	; sa
 ("h." quail-tibetan-input-tibkey)	; ha
 ("h/" quail-tibetan-input-tibkey)	; Aa
 )

;;; quail/tibetan.el ends here.



