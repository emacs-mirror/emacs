;;; quail/vntelex.el -- Quail package for Vietnamese by Telex method

;; Copyright (C) 2001 Free Software Foundation.

;; Author:   Werner Lemberg <wl@gnu.org>
;; Keywords: multilingual, input method, Vietnamese

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

;; based on the files VietnameseTelex.kmap (written by Nguyen Thanh
;; Bien <biennt@linuxvn.com>) and VNtelex.kmap (written by Nguyen Dai
;; Quy <DaiQuy.Nguyen@ulg.ac.be>) from the yudit 2.4 package

;;; Code:

(require 'quail)


(quail-define-package
 "vietnamese-telex"              ; NAME
 "Vietnamese"                    ; LANGUAGE
 "VT"                            ; TITLE
 t                               ; GUIDANCE
 "Vietnamese telex input method

Vowels with circumflex:

  aa -> ,Ab(B, EE -> ,AJ(B, etc.

Other diacritics:

  effect     postfix   examples
  ------------------------------
  breve         w      aw -> ,1e(B
  horn          w      ow -> ,1=(B

  acute         s      as -> ,1a(B
  grave         f      af -> ,A`(B
  hook above    r      ar -> ,1d(B
  tilde         x      ,Ac(B  -> ,Ac(B
  dot below     j      aj -> ,1U(B

  d bar                dd -> ,1p(B

Combinations:

  AWF -> ,2"(B, owx -> ,1^(B, etc.

Alternatives:

  EE = Ee -> ,AJ(B, AWF = Awf -> ,2"(B, etc.

Doubling the postfix (but not in combinations) separates the letter
and postfix: Eee -> Ee, ajj -> aj, etc.
"                                ; DOCSTRING
 nil                             ; TRANSLATION-KEYS
 t                               ; FORGET-LAST-SELECTION
 nil                             ; DETERMINISTIC
 nil                             ; KBD-TRANSLATE
 nil                             ; SHOW-LAYOUT
 nil                             ; CREATE-DECODE-MAP
 nil                             ; MAXIMUM-SHORTEST
 nil                             ; OVERLAY-PLIST
 nil                             ; UPDATE-TRANSLATION-FUNCTION
 nil                             ; CONVERSION-KEYS
 t)                              ; SIMPLE

(quail-define-rules
 ("af" ?,A`(B)	; LATIN SMALL LETTER A WITH GRAVE
 ("AF" ?,A@(B)	; LATIN CAPITAL LETTER A WITH GRAVE
 ("Af" ?,A@(B)
 ("as" ?,Aa(B)	; LATIN SMALL LETTER A WITH ACUTE
 ("AS" ?,AA(B)	; LATIN CAPITAL LETTER A WITH ACUTE
 ("As" ?,AA(B)
 ("aa" ?,Ab(B)	; LATIN SMALL LETTER A WITH CIRCUMFLEX
 ("AA" ?,AB(B)	; LATIN CAPITAL LETTER A WITH CIRCUMFLEX
 ("Aa" ?,AB(B)
 ("ax" ?,Ac(B)	; LATIN SMALL LETTER A WITH TILDE
 ("AX" ?,AC(B)	; LATIN CAPITAL LETTER A WITH TILDE
 ("Ax" ?,AC(B)
 ("ef" ?,Ah(B)	; LATIN SMALL LETTER E WITH GRAVE
 ("EF" ?,AH(B)	; LATIN CAPITAL LETTER E WITH GRAVE
 ("Ef" ?,AH(B)
 ("es" ?,Ai(B)	; LATIN SMALL LETTER E WITH ACUTE
 ("ES" ?,AI(B)	; LATIN CAPITAL LETTER E WITH ACUTE
 ("Es" ?,AI(B)
 ("ee" ?,Aj(B)	; LATIN SMALL LETTER E WITH CIRCUMFLEX
 ("EE" ?,AJ(B)	; LATIN CAPITAL LETTER E WITH CIRCUMFLEX
 ("Ee" ?,AJ(B)
 ("if" ?,Al(B)	; LATIN SMALL LETTER I WITH GRAVE
 ("IF" ?,AL(B)	; LATIN CAPITAL LETTER I WITH GRAVE
 ("If" ?,AL(B)
 ("is" ?,Am(B)	; LATIN SMALL LETTER I WITH ACUTE
 ("IS" ?,AM(B)	; LATIN CAPITAL LETTER I WITH ACUTE
 ("Is" ?,AM(B)
 ("of" ?,Ar(B)	; LATIN SMALL LETTER O WITH GRAVE
 ("OF" ?,AR(B)	; LATIN CAPITAL LETTER O WITH GRAVE
 ("Of" ?,AR(B)
 ("os" ?,As(B)	; LATIN SMALL LETTER O WITH ACUTE
 ("OS" ?,AS(B)	; LATIN CAPITAL LETTER O WITH ACUTE
 ("Os" ?,AS(B)
 ("oo" ?,At(B)	; LATIN SMALL LETTER O WITH CIRCUMFLEX
 ("OO" ?,AT(B)	; LATIN CAPITAL LETTER O WITH CIRCUMFLEX
 ("Oo" ?,AT(B)
 ("ox" ?,Au(B)	; LATIN SMALL LETTER O WITH TILDE
 ("OX" ?,AU(B)	; LATIN CAPITAL LETTER O WITH TILDE
 ("Ox" ?,AU(B)
 ("uf" ?,Ay(B)	; LATIN SMALL LETTER U WITH GRAVE
 ("UF" ?,AY(B)	; LATIN CAPITAL LETTER U WITH GRAVE
 ("Uf" ?,AY(B)
 ("us" ?,Az(B)	; LATIN SMALL LETTER U WITH ACUTE
 ("US" ?,AZ(B)	; LATIN CAPITAL LETTER U WITH ACUTE
 ("Us" ?,AZ(B)
 ("ys" ?,A}(B)	; LATIN SMALL LETTER Y WITH ACUTE
 ("YS" ?,A](B)	; LATIN CAPITAL LETTER Y WITH ACUTE
 ("Ys" ?,A](B)
 ("aw" ?,Bc(B)	; LATIN SMALL LETTER A WITH BREVE
 ("AW" ?,BC(B)	; LATIN CAPITAL LETTER A WITH BREVE
 ("Aw" ?,BC(B)
 ("ix" ?,D5(B)	; LATIN SMALL LETTER I WITH TILDE
 ("IX" ?,D%(B)	; LATIN CAPITAL LETTER I WITH TILDE
 ("Ix" ?,D%(B)
 ("ux" ?,D}(B)	; LATIN SMALL LETTER U WITH TILDE
 ("UX" ?,D](B)	; LATIN CAPITAL LETTER U WITH TILDE
 ("Ux" ?,D](B)
 ("ow" ?,1=(B)	; LATIN SMALL LETTER O WITH HORN
 ("OW" ?,2=(B)	; LATIN CAPITAL LETTER O WITH HORN
 ("Ow" ?,2=(B)
 ("uw" ?,1_(B)	; LATIN SMALL LETTER U WITH HORN
 ("UW" ?,2_(B)	; LATIN CAPITAL LETTER U WITH HORN
 ("Uw" ?,2_(B)
 ("aj" ?,1U(B)	; LATIN SMALL LETTER A WITH DOT BELOW
 ("AJ" ?,2U(B)	; LATIN CAPITAL LETTER A WITH DOT BELOW
 ("Aj" ?,2U(B)
 ("ar" ?,1d(B)	; LATIN SMALL LETTER A WITH HOOK ABOVE
 ("AR" ?,2d(B)	; LATIN CAPITAL LETTER A WITH HOOK ABOVE
 ("Ar" ?,2d(B)
 ("aas" ?,1$(B)	; LATIN SMALL LETTER A WITH CIRCUMFLEX AND ACUTE
 ("AAS" ?,2$(B)	; LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND ACUTE
 ("Aas" ?,2$(B)
 ("aaf" ?,1%(B)	; LATIN SMALL LETTER A WITH CIRCUMFLEX AND GRAVE
 ("AAF" ?,2%(B)	; LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND GRAVE
 ("Aaf" ?,2%(B)
 ("aar" ?,1&(B)	; LATIN SMALL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE
 ("AAR" ?,2&(B)	; LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE
 ("Aar" ?,2&(B)
 ("aax" ?,1g(B)	; LATIN SMALL LETTER A WITH CIRCUMFLEX AND TILDE
 ("AAX" ?,2g(B)	; LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND TILDE
 ("Aax" ?,2g(B)
 ("aaj" ?,1'(B)	; LATIN SMALL LETTER A WITH CIRCUMFLEX AND DOT BELOW
 ("AAJ" ?,2'(B)	; LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND DOT BELOW
 ("Aaj" ?,2'(B)
 ("aws" ?,1!(B)	; LATIN SMALL LETTER A WITH BREVE AND ACUTE
 ("AWS" ?,2!(B)	; LATIN CAPITAL LETTER A WITH BREVE AND ACUTE
 ("Aws" ?,2!(B)
 ("awf" ?,1"(B)	; LATIN SMALL LETTER A WITH BREVE AND GRAVE
 ("AWF" ?,2"(B)	; LATIN CAPITAL LETTER A WITH BREVE AND GRAVE
 ("Awf" ?,2"(B)
 ("awr" ?,1F(B)	; LATIN SMALL LETTER A WITH BREVE AND HOOK ABOVE
 ("AWR" ?,2F(B)	; LATIN CAPITAL LETTER A WITH BREVE AND HOOK ABOVE
 ("Awr" ?,2F(B)
 ("awx" ?,1G(B)	; LATIN SMALL LETTER A WITH BREVE AND TILDE
 ("AWX" ?,2G(B)	; LATIN CAPITAL LETTER A WITH BREVE AND TILDE
 ("Awx" ?,2G(B)
 ("awj" ?,1#(B)	; LATIN SMALL LETTER A WITH BREVE AND DOT BELOW
 ("AWJ" ?,2#(B)	; LATIN CAPITAL LETTER A WITH BREVE AND DOT BELOW
 ("Awj" ?,2#(B)
 ("ej" ?,1)(B)	; LATIN SMALL LETTER E WITH DOT BELOW
 ("EJ" ?,2)(B)	; LATIN CAPITAL LETTER E WITH DOT BELOW
 ("Ej" ?,2)(B)
 ("er" ?,1k(B)	; LATIN SMALL LETTER E WITH HOOK ABOVE
 ("ER" ?,2k(B)	; LATIN CAPITAL LETTER E WITH HOOK ABOVE
 ("Er" ?,2k(B)
 ("ex" ?,1((B)	; LATIN SMALL LETTER E WITH TILDE
 ("EX" ?,2((B)	; LATIN CAPITAL LETTER E WITH TILDE
 ("Ex" ?,2((B)
 ("ees" ?,1*(B)	; LATIN SMALL LETTER E WITH CIRCUMFLEX AND ACUTE
 ("EES" ?,2*(B)	; LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND ACUTE
 ("Ees" ?,2*(B)
 ("eef" ?,1+(B)	; LATIN SMALL LETTER E WITH CIRCUMFLEX AND GRAVE
 ("EEF" ?,2+(B)	; LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND GRAVE
 ("Eef" ?,2+(B)
 ("eer" ?,1,(B)	; LATIN SMALL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE
 ("EER" ?,2,(B)	; LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE
 ("Eer" ?,2,(B)
 ("eex" ?,1-(B)	; LATIN SMALL LETTER E WITH CIRCUMFLEX AND TILDE
 ("EEX" ?,2-(B)	; LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND TILDE
 ("Eex" ?,2-(B)
 ("eej" ?,1.(B)	; LATIN SMALL LETTER E WITH CIRCUMFLEX AND DOT BELOW
 ("EEJ" ?,2.(B)	; LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND DOT BELOW
 ("Eej" ?,2.(B)
 ("ir" ?,1o(B)	; LATIN SMALL LETTER I WITH HOOK ABOVE
 ("IR" ?,2o(B)	; LATIN CAPITAL LETTER I WITH HOOK ABOVE
 ("Ir" ?,2o(B)
 ("ij" ?,18(B)	; LATIN SMALL LETTER I WITH DOT BELOW
 ("IJ" ?,28(B)	; LATIN CAPITAL LETTER I WITH DOT BELOW
 ("Ij" ?,28(B)
 ("oj" ?,1w(B)	; LATIN SMALL LETTER O WITH DOT BELOW
 ("OJ" ?,2w(B)	; LATIN CAPITAL LETTER O WITH DOT BELOW
 ("Oj" ?,2w(B)
 ("or" ?,1v(B)	; LATIN SMALL LETTER O WITH HOOK ABOVE
 ("OR" ?,2v(B)	; LATIN CAPITAL LETTER O WITH HOOK ABOVE
 ("Or" ?,2v(B)
 ("oos" ?,1/(B)	; LATIN SMALL LETTER O WITH CIRCUMFLEX AND ACUTE
 ("OOS" ?,2/(B)	; LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND ACUTE
 ("Oos" ?,2/(B)
 ("oof" ?,10(B)	; LATIN SMALL LETTER O WITH CIRCUMFLEX AND GRAVE
 ("OOF" ?,20(B)	; LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND GRAVE
 ("Oof" ?,20(B)
 ("oor" ?,11(B)	; LATIN SMALL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE
 ("OOR" ?,21(B)	; LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE
 ("Oor" ?,21(B)
 ("oox" ?,12(B)	; LATIN SMALL LETTER O WITH CIRCUMFLEX AND TILDE
 ("OOX" ?,22(B)	; LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND TILDE
 ("Oox" ?,22(B)
 ("ooj" ?,15(B)	; LATIN SMALL LETTER O WITH CIRCUMFLEX AND DOT BELOW
 ("OOJ" ?,25(B)	; LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND DOT BELOW
 ("Ooj" ?,25(B)
 ("ows" ?,1>(B)	; LATIN SMALL LETTER O WITH HORN AND ACUTE
 ("OWS" ?,2>(B)	; LATIN CAPITAL LETTER O WITH HORN AND ACUTE
 ("Ows" ?,2>(B)
 ("owf" ?,16(B)	; LATIN SMALL LETTER O WITH HORN AND GRAVE
 ("OWF" ?,26(B)	; LATIN CAPITAL LETTER O WITH HORN AND GRAVE
 ("Owf" ?,26(B)
 ("owr" ?,17(B)	; LATIN SMALL LETTER O WITH HORN AND HOOK ABOVE
 ("OWR" ?,27(B)	; LATIN CAPITAL LETTER O WITH HORN AND HOOK ABOVE
 ("Owr" ?,27(B)
 ("owx" ?,1^(B)	; LATIN SMALL LETTER O WITH HORN AND TILDE
 ("OWX" ?,2^(B)	; LATIN CAPITAL LETTER O WITH HORN AND TILDE
 ("Owx" ?,2^(B)
 ("owj" ?,1~(B)	; LATIN SMALL LETTER O WITH HORN AND DOT BELOW
 ("OWJ" ?,2~(B)	; LATIN CAPITAL LETTER O WITH HORN AND DOT BELOW
 ("Owj" ?,2~(B)
 ("uj" ?,1x(B)	; LATIN SMALL LETTER U WITH DOT BELOW
 ("UJ" ?,2x(B)	; LATIN CAPITAL LETTER U WITH DOT BELOW
 ("Uj" ?,2x(B)
 ("ur" ?,1|(B)	; LATIN SMALL LETTER U WITH HOOK ABOVE
 ("UR" ?,2|(B)	; LATIN CAPITAL LETTER U WITH HOOK ABOVE
 ("Ur" ?,2|(B)
 ("uws" ?,1Q(B)	; LATIN SMALL LETTER U WITH HORN AND ACUTE
 ("UWS" ?,2Q(B)	; LATIN CAPITAL LETTER U WITH HORN AND ACUTE
 ("Uws" ?,2Q(B)
 ("uwf" ?,1W(B)	; LATIN SMALL LETTER U WITH HORN AND GRAVE
 ("UWF" ?,2W(B)	; LATIN CAPITAL LETTER U WITH HORN AND GRAVE
 ("Uwf" ?,2W(B)
 ("uwr" ?,1X(B)	; LATIN SMALL LETTER U WITH HORN AND HOOK ABOVE
 ("UWR" ?,2X(B)	; LATIN CAPITAL LETTER U WITH HORN AND HOOK ABOVE
 ("Uwr" ?,2X(B)
 ("uwx" ?,1f(B)	; LATIN SMALL LETTER U WITH HORN AND TILDE
 ("UWX" ?,2f(B)	; LATIN CAPITAL LETTER U WITH HORN AND TILDE
 ("Uwx" ?,2f(B)
 ("uwj" ?,1q(B)	; LATIN SMALL LETTER U WITH HORN AND DOT BELOW
 ("UWJ" ?,2q(B)	; LATIN CAPITAL LETTER U WITH HORN AND DOT BELOW
 ("Uwj" ?,2q(B)
 ("yf" ?,1O(B)	; LATIN SMALL LETTER Y WITH GRAVE
 ("YF" ?,2O(B)	; LATIN CAPITAL LETTER Y WITH GRAVE
 ("Yf" ?,2O(B)
 ("yj" ?,1\(B)	; LATIN SMALL LETTER Y WITH DOT BELOW
 ("YJ" ?,2\(B)	; LATIN CAPITAL LETTER Y WITH DOT BELOW
 ("Yj" ?,2\(B)
 ("yr" ?,1V(B)	; LATIN SMALL LETTER Y WITH HOOK ABOVE
 ("YR" ?,2V(B)	; LATIN CAPITAL LETTER Y WITH HOOK ABOVE
 ("Yr" ?,2V(B)
 ("yx" ?,1[(B)	; LATIN SMALL LETTER Y WITH TILDE
 ("YX" ?,2[(B)	; LATIN CAPITAL LETTER Y WITH TILDE
 ("Yx" ?,2[(B)
 ("dd" ?,Bp(B)	; LATIN SMALL LETTER D WITH STROKE
 ("DD" ?,BP(B)	; LATIN CAPITAL LETTER D WITH STROKE
 ("Dd" ?,BP(B)
;("$$" ?$,1tK(B)	; U+20AB DONG SIGN

 ("aff" ["aff"])
 ("AFF" ["AF"])
 ("Aff" ["Af"])
 ("ass" ["as"])
 ("ASS" ["AS"])
 ("Ass" ["As"])
 ("aaa" ["aa"])
 ("AAA" ["AA"])
 ("Aaa" ["Aa"])
 ("axx" ["ax"])
 ("AXX" ["AX"])
 ("Axx" ["Ax"])
 ("eff" ["ef"])
 ("EFF" ["EF"])
 ("Eff" ["Ef"])
 ("ess" ["es"])
 ("ESS" ["ES"])
 ("Ess" ["Es"])
 ("eee" ["ee"])
 ("EEE" ["EE"])
 ("Eee" ["Ee"])
 ("iff" ["if"])
 ("IFF" ["IF"])
 ("Iff" ["If"])
 ("iss" ["is"])
 ("ISS" ["IS"])
 ("Iss" ["Is"])
 ("off" ["of"])
 ("OFF" ["OF"])
 ("Off" ["Of"])
 ("oss" ["os"])
 ("OSS" ["OS"])
 ("Oss" ["Os"])
 ("ooo" ["oo"])
 ("OOO" ["OO"])
 ("Ooo" ["Oo"])
 ("oxx" ["ox"])
 ("OXX" ["OX"])
 ("Oxx" ["Ox"])
 ("uff" ["uf"])
 ("UFF" ["UF"])
 ("Uff" ["Uf"])
 ("uss" ["us"])
 ("USS" ["US"])
 ("Uss" ["Us"])
 ("yss" ["ys"])
 ("YSS" ["YS"])
 ("Yss" ["Ys"])
 ("aww" ["aw"])
 ("AWW" ["AW"])
 ("Aww" ["Aw"])
 ("ixx" ["ix"])
 ("IXX" ["IX"])
 ("Ixx" ["Ix"])
 ("uxx" ["ux"])
 ("UXX" ["UX"])
 ("Uxx" ["ux"])
 ("oww" ["ow"])
 ("OWW" ["OW"])
 ("Oww" ["Ow"])
 ("uww" ["uw"])
 ("UWW" ["UW"])
 ("Uww" ["Uw"])
 ("ajj" ["aj"])
 ("AJJ" ["AJ"])
 ("Ajj" ["Aj"])
 ("arr" ["ar"])
 ("ARR" ["AR"])
 ("Arr" ["Ar"])
 ("ejj" ["ej"])
 ("EJJ" ["EJ"])
 ("Ejj" ["Ej"])
 ("err" ["er"])
 ("ERR" ["ER"])
 ("Err" ["Er"])
 ("exx" ["ex"])
 ("EXX" ["EX"])
 ("Exx" ["Ex"])
 ("irr" ["ir"])
 ("IRR" ["IR"])
 ("Irr" ["Ir"])
 ("ijj" ["ij"])
 ("IJJ" ["IJ"])
 ("Ijj" ["Ij"])
 ("ojj" ["oj"])
 ("OJJ" ["OJ"])
 ("Ojj" ["Oj"])
 ("orr" ["or"])
 ("ORR" ["OR"])
 ("Orr" ["Or"])
 ("ujj" ["uj"])
 ("UJJ" ["UJ"])
 ("Ujj" ["Uj"])
 ("urr" ["ur"])
 ("URR" ["UR"])
 ("Urr" ["Ur"])
 ("yff" ["yf"])
 ("YFF" ["YF"])
 ("Yff" ["Yf"])
 ("yjj" ["yj"])
 ("YJJ" ["YJ"])
 ("Yjj" ["Yj"])
 ("yrr" ["yr"])
 ("YRR" ["YR"])
 ("Yrr" ["Yr"])
 ("yxx" ["yx"])
 ("YXX" ["YX"])
 ("Yxx" ["Yx"])
 ("ddd" ["dd"])
 ("DDD" ["DD"])
 ("Ddd" ["Dd"])
;("$$$" ["$$"])
)

;; Local Variables:
;; coding: iso-2022-7bit
;; End:
