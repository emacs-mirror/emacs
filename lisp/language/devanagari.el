;;; devanagari.el --- support for Devanagari -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1996 Free Software Foundation, Inc.

;; Author: KAWABATA, Taichi <kawabata@is.s.u-tokyo.ac.jp>

;; Keywords: multilingual, Indian, Devanagari

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

;; History:
;; 1996.10.18 written by KAWABATA, Taichi <kawabata@is.s.u-tokyo.ac.jp>
;; 1997.1.20 fixed some bugs.

;;; Code:

(make-coding-system
 'in-is13194-devanagari 2 ?D
 "8-bit encoding for ASCII (MSB=0) and IS13194-Devanagari (MSB=1)"
 '(ascii indian-is13194 nil nil
   nil ascii-eol)
 '((safe-charsets ascii indian-is13194)
   (post-read-conversion . in-is13194-devanagari-post-read-conversion)
   (pre-write-conversion . in-is13194-devanagari-pre-write-conversion)))

(define-coding-system-alias 'devanagari 'in-is13194-devanagari)

(set-language-info-alist
 "Devanagari" '((charset indian-is13194 indian-2-column indian-1-column)
		(coding-system in-is13194-devanagari)
		(coding-priority in-is13194-devanagari)
		(input-method . "devanagari-itrans")
		(features devan-util)
		(documentation . "\
Such languages using Devanagari script as Hindi and Marathi
are supported in this language environment.

But, the current implementation is very imcomplete.  Although we have
already implemented a better Indic scripts support, it is not yet
included in this Emacs because this version is only for bug fixing.

The web page <http://www.m17n.org/emacs-indian/> describes how to use
that new Indic scripts support.  Please visit that page."))
'("Indian"))

;;
;; Devanagari Glyph List
;;
;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
;;2120   $(5!!!"!#!$!%!&!'!(!)!*!+!,!-!.!/(B
;;2130 $(5!0!1!2!3!4!5!6!7!8!9!:!;!<!=!>!?(B
;;2140 $(5!@!A!B!C!D!E!F!G!H!I!J!K!L!M!N!O(B
;;2150 $(5!P!Q!R!S!T!U!V!W!X!Y!Z![!\!]!^!_(B
;;2160 $(5!`!a!b!c!d!e!f!g!h!i!j!k!l!m!n!o(B
;;2170 $(5!p!q!r!s!t!u!v!w!x!y!z!{!|!}!~(B
;;
;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
;;2220   $(5"!"""#"$"%"&"'"(")"*"+","-"."/(B
;;2230 $(5"0"1"2"3"4"5"6"7"8"9":";"<"=">"?(B
;;2240 $(5"@"A"B"C"D"E"F"G"H"I"J"K"L"M"N"O(B
;;2250 $(5"P"Q"R"S"T"U"V"W"X"Y"Z"["\"]"^"_(B
;;2260 $(5"`"a"b"c"d"e"f"g"h"i"j"k"l"m"n"o(B
;;2270 $(5"p"q"r"s"t"u"v"w"x"y"z"{"|"}"~(B
;;
;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
;;2320   $(5#!#"###$#%#&#'#(#)#*#+#,#-#.#/(B
;;2330 $(5#0#1#2#3#4#5#6#7#8#9#:#;#<#=#>#?(B
;;2340 $(5#@#A#B#C#D#E#F#G#H#I#J#K#L#M#N#O(B
;;2350 $(5#P#Q#R#S#T#U#V#W#X#Y#Z#[#\#]#^#_(B
;;2360 $(5#`#a#b#c#d#e#f#g#h#i#j#k#l#m#n#o(B
;;2370 $(5#p#q#r#s#t#u#v#w#x#y#z#{#|#}#~(B
;;
;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
;;2420   $(5$!$"$#$$$%$&$'$($)$*$+$,$-$.$/(B
;;2430 $(5$0$1$2$3$4$5$6$7$8$9$:$;$<$=$>$?(B
;;2440 $(5$@$A$B$C$D$E$F$G$H$I$J$K$L$M$N$O(B
;;2450 $(5$P$Q$R$S$T$U$V$W$X$Y$Z$[$\$]$^$_(B
;;2460 $(5$`$a$b$c$d$e$f$g$h$i$j$k$l$m$n$o(B
;;2470 $(5$p$q$r$s$t$u$v$w$x$y$z${$|$}$~(B
;;
;;     0123456789abcdef
;;2120  $(6!!!"!#!$!%!&!'!(!)!*!+!,!-!.!/(B
;;2130 $(6!0!1!2!3!4!5!6!7!8!9!:!;!<!=!>!?(B
;;2140 $(6!@!A!B!C!D!E!F!G!H!I!J!K!L!M!N!O(B
;;2150 $(6!P!Q!R!S!T!U!V!W!X!Y!Z![!\!]!^!_(B
;;2160 $(6!`!a!b!c!d!e!f!g!h!i!j!k!l!m!n!o(B
;;2170 $(6!p!q!r!s!t!u!v!w!x!y!z!{!|!}!~(B
;;
;;     0123456789abcdef
;;2220  $(6"!"""#"$"%"&"'"(")"*"+","-"."/(B
;;2230 $(6"0"1"2"3"4"5"6"7"8"9":";"<"=">"?(B
;;2240 $(6"@"A"B"C"D"E"F"G"H"I"J"K"L"M"N"O(B
;;2250 $(6"P"Q"R"S"T"U"V"W"X"Y"Z"["\"]"^"_(B
;;2260 $(6"`"a"b"c"d"e"f"g"h"i"j"k"l"m"n"o(B
;;2270 $(6"p"q"r"s"t"u"v"w"x"y"z"{"|"}"~(B
;;2320  $(6#!#"###$#%#&#'#(#)#*#+#,#-#.#/(B
;;2330 $(6#0#1#2#3#4#5#6#7#8#9#:#;#<#=#>#?(B
;;2340 $(6#@#A#B#C#D#E#F#G#H#I#J#K#L#M#N#O(B
;;2350 $(6#P#Q#R#S#T#U#V#W#X#Y#Z#[#\#]#^#_(B
;;2360 $(6#`#a#b#c#d#e#f#g#h#i#j#k#l#m#n#o(B
;;2370 $(6#p#q#r#s#t#u#v#w#x#y#z#{#|#}#~(B
;;
;;     0123456789abcdef
;;2320  $(6$!$"$#$$$%$&$'$($)$*$+$,$-$.$/(B
;;2430 $(6$0$1$2$3$4$5$6$7$8$9$:$;$<$=$>$?(B
;;2440 $(6$@$A$B$C$D$E$F$G$H$I$J$K$L$M$N$O(B
;;2450 $(6$P$Q$R$S$T$U$V$W$X$Y$Z$[$\$]$^$_(B
;;2460 $(6$`$a$b$c$d$e$f$g$h$i$j$k$l$m$n$o(B
;;2470 $(6$p$q$r$s$t$u$v$w$x$y$z${$|$}$~(B
;;
;;
;; Modify the following table if you change the set of 1-column font.
;;
(defconst devanagari-1-column-char 
  '((?$(5!!(B . ?$(6!!(B)
    (?$(5!"(B . ?$(6!"(B)
    (?$(5!#(B . ?$(6!#(B)
    (?$(5!$(B . nil)
    (?$(5!%(B . nil)
    (?$(5!&(B . ?$(6!&(B)
    (?$(5!'(B . ?$(6!'(B)
    (?$(5!((B . ?$(6!((B)
    (?$(5!)(B . nil)
    (?$(5!*(B . nil)
    (?$(5!+(B . nil)
    (?$(5!,(B . nil)
    (?$(5!-(B . nil)
    (?$(5!.(B . nil)
    (?$(5!/(B . nil)
    (?$(5!0(B . nil)
    (?$(5!1(B . nil)
    (?$(5!2(B . nil)
    (?$(5!3(B . nil)
    (?$(5!4(B . nil)
    (?$(5!5(B . ?$(6!5(B)
    (?$(5!6(B . nil)
    (?$(5!7(B . nil)
    (?$(5!8(B . nil)
    (?$(5!9(B . nil)
    (?$(5!:(B . nil)
    (?$(5!;(B . nil)
    (?$(5!<(B . nil)
    (?$(5!=(B . ?$(6!=(B)
    (?$(5!>(B . ?$(6!>(B)
    (?$(5!?(B . ?$(6!?(B)
    (?$(5!@(B . ?$(6!@(B)
    (?$(5!A(B . nil)
    (?$(5!B(B . ?$(6!B(B)
    (?$(5!C(B . ?$(6!C(B)
    (?$(5!D(B . ?$(6!D(B)
    (?$(5!E(B . ?$(6!E(B)
    (?$(5!F(B . ?$(6!F(B)
    (?$(5!G(B . ?$(6!G(B)
    (?$(5!H(B . ?$(6!H(B)
    (?$(5!I(B . nil)
    (?$(5!J(B . ?$(6!J(B)
    (?$(5!K(B . ?$(6!K(B)
    (?$(5!L(B . ?$(6!L(B)
    (?$(5!M(B . ?$(6!M(B)
    (?$(5!N(B . ?$(6!N(B)
    (?$(5!O(B . ?$(6!O(B)
    (?$(5!P(B . ?$(6!P(B)
    (?$(5!Q(B . nil)
    (?$(5!R(B . nil)
    (?$(5!S(B . nil)
    (?$(5!T(B . ?$(6!T(B)
    (?$(5!U(B . nil)
    (?$(5!V(B . ?$(6!V(B)
    (?$(5!W(B . ?$(6!W(B)
    (?$(5!X(B . ?$(6!X(B)
    (?$(5!Y(B . nil)
    (?$(5!Z(B . ?$(6!Z(B)
    (?$(5![(B . ?$(6![(B)
    (?$(5!\(B . ?$(6!\(B)
    (?$(5!](B . ?$(6!](B)
    (?$(5!^(B . ?$(6!^(B)
    (?$(5!_(B . ?$(6!_(B)
    (?$(5!`(B . ?$(6!`(B)
    (?$(5!a(B . ?$(6!a(B)
    (?$(5!b(B . ?$(6!b(B)
    (?$(5!c(B . ?$(6!c(B)
    (?$(5!d(B . ?$(6!d(B)
    (?$(5!e(B . ?$(6!e(B)
    (?$(5!f(B . ?$(6!f(B)
    (?$(5!g(B . ?$(6!g(B)
    (?$(5!h(B . ?$(6!h(B)
    (?$(5!i(B . ?$(6!i(B)
    (?$(5!j(B . ?$(6!j(B)
    (nil . nil)
    (nil . nil)
    (nil . nil)
    (nil . nil)
    (nil . nil)
    (nil . nil)
    (?$(5!q(B . ?$(6!q(B)
    (?$(5!r(B . ?$(6!r(B)
    (?$(5!s(B . ?$(6!s(B)
    (?$(5!t(B . ?$(6!t(B)
    (?$(5!u(B . ?$(6!u(B)
    (?$(5!v(B . ?$(6!v(B)
    (?$(5!w(B . ?$(6!w(B)
    (?$(5!x(B . ?$(6!x(B)
    (?$(5!y(B . ?$(6!y(B)
    (?$(5!z(B . ?$(6!z(B)
    (nil . nil)
    (nil . nil)
    (nil . nil)
    (nil . nil)
    (?$(5"!(B . nil)
    (?$(5""(B . nil)
    (?$(5"#(B . nil)
    (?$(5"$(B . ?$(6"$(B)
    (?$(5"%(B . ?$(6"%(B)
    (?$(5"&(B . ?$(6"&(B)
    (?$(5"'(B . nil)
    (?$(5"((B . nil)
    (?$(5")(B . nil)
    (?$(5"*(B . nil)
    (?$(5"+(B . nil)
    (?$(5",(B . ?$(6",(B)
    (?$(5"-(B . nil)
    (?$(5".(B . ?$(6".(B)
    (?$(5"/(B . nil)
    (?$(5"0(B . nil)
    (?$(5"1(B . nil)
    (?$(5"2(B . nil)
    (?$(5"3(B . ?$(6"3(B)
    (?$(5"4(B . ?$(6"4(B)
    (?$(5"5(B . ?$(6"5(B)
    (?$(5"6(B . ?$(6"6(B)
    (?$(5"7(B . nil)
    (?$(5"8(B . ?$(6"8(B)
    (?$(5"9(B . nil)
    (?$(5":(B . ?$(6":(B)
    (?$(5";(B . ?$(6";(B)
    (?$(5"<(B . ?$(6"<(B)
    (?$(5"=(B . nil)
    (?$(5">(B . nil)
    (?$(5"?(B . nil)
    (?$(5"@(B . nil)
    (?$(5"A(B . ?$(6"A(B)
    (?$(5"B(B . ?$(6"B(B)
    (?$(5"C(B . ?$(6"C(B)
    (?$(5"D(B . nil)
    (?$(5"E(B . ?$(6"E(B)
    (?$(5"F(B . ?$(6"F(B)
    (?$(5"G(B . ?$(6"G(B)
    (?$(5"H(B . ?$(6"H(B)
    (?$(5"I(B . ?$(6"I(B)
    (?$(5"J(B . ?$(6"J(B)
    (?$(5"K(B . ?$(6"K(B)
    (?$(5"L(B . ?$(6"L(B)
    (?$(5"M(B . ?$(6"M(B)
    (?$(5"N(B . ?$(6"N(B)
    (?$(5"O(B . nil)
    (?$(5"P(B . nil)
    (?$(5"Q(B . ?$(6"Q(B)
    (?$(5"R(B . nil)
    (?$(5"S(B . nil)
    (?$(5"T(B . ?$(6"T(B)
    (?$(5"U(B . ?$(6"U(B)
    (?$(5"V(B . ?$(6"V(B)
    (?$(5"W(B . ?$(6"W(B)
    (?$(5"X(B . nil)
    (?$(5"Y(B . nil)
    (?$(5"Z(B . nil)
    (?$(5"[(B . nil)
    (?$(5"\(B . nil)
    (?$(5"](B . ?$(6"](B)
    (?$(5"^(B . nil)
    (?$(5"_(B . nil)
    (?$(5"`(B . ?$(6"`(B)
    (?$(5"a(B . ?$(6"a(B)
    (?$(5"b(B . ?$(6"b(B)
    (?$(5"c(B . ?$(6"c(B)
    (?$(5"d(B . ?$(6"d(B)
    (?$(5"e(B . ?$(6"e(B)
    (?$(5"f(B . ?$(6"f(B)
    (?$(5"g(B . ?$(6"g(B)
    (?$(5"h(B . ?$(6"h(B)
    (?$(5"i(B . ?$(6"i(B)
    (?$(5"j(B . ?$(6"j(B)
    (?$(5"k(B . ?$(6"k(B)
    (?$(5"l(B . ?$(6"l(B)
    (?$(5"m(B . ?$(6"m(B)
    (?$(5"n(B . nil)
    (?$(5"o(B . nil)
    (?$(5"p(B . ?$(6"p(B)
    (?$(5"q(B . ?$(6"q(B)
    (?$(5"r(B . ?$(6"r(B)
    (?$(5"s(B . ?$(6"s(B)
    (?$(5"t(B . ?$(6"t(B)
    (?$(5"u(B . ?$(6"u(B)
    (?$(5"v(B . nil)
    (?$(5"w(B . nil)
    (?$(5"x(B . nil)
    (?$(5"y(B . ?$(6"y(B)
    (?$(5"z(B . ?$(6"z(B)
    (?$(5"{(B . nil)
    (?$(5"|(B . nil)
    (?$(5"}(B . nil)
    (?$(5"~(B . nil)
    (?$(5#!(B . nil)
    (?$(5#"(B . nil)
    (?$(5##(B . nil)
    (?$(5#$(B . nil)
    (?$(5#%(B . nil)
    (?$(5#&(B . nil)
    (?$(5#'(B . nil)
    (?$(5#((B . nil)
    (?$(5#)(B . nil)
    (?$(5#*(B . nil)
    (?$(5#+(B . nil)
    (?$(5#,(B . nil)
    (?$(5#-(B . nil)
    (?$(5#.(B . nil)
    (?$(5#/(B . nil)
    (?$(5#0(B . nil)
    (?$(5#1(B . nil)
    (?$(5#2(B . nil)
    (?$(5#3(B . nil)
    (?$(5#4(B . nil)
    (?$(5#5(B . ?$(6#5(B)
    (?$(5#6(B . nil)
    (?$(5#7(B . nil)
    (?$(5#8(B . nil)
    (?$(5#9(B . nil)
    (?$(5#:(B . nil)
    (?$(5#;(B . nil)
    (?$(5#<(B . nil)
    (?$(5#=(B . nil)
    (?$(5#>(B . nil)
    (?$(5#?(B . ?$(6#?(B)
    (?$(5#@(B . ?$(6#@(B)
    (?$(5#A(B . nil)
    (?$(5#B(B . nil)
    (?$(5#C(B . nil)
    (?$(5#D(B . nil)
    (?$(5#E(B . nil)
    (?$(5#F(B . nil)
    (?$(5#G(B . nil)
    (?$(5#H(B . nil)
    (?$(5#I(B . nil)
    (?$(5#J(B . ?$(6#J(B)
    (?$(5#K(B . ?$(6#K(B)
    (?$(5#L(B . ?$(6#L(B)
    (?$(5#M(B . ?$(6#M(B)
    (?$(5#N(B . nil)
    (?$(5#O(B . nil)
    (?$(5#P(B . nil)
    (?$(5#Q(B . nil)
    (?$(5#R(B . ?$(6#R(B)
    (?$(5#S(B . nil)
    (?$(5#T(B . nil)
    (?$(5#U(B . nil)
    (?$(5#V(B . nil)
    (?$(5#W(B . nil)
    (?$(5#X(B . nil)
    (?$(5#Y(B . nil)
    (?$(5#Z(B . nil)
    (?$(5#[(B . nil)
    (?$(5#\(B . nil)
    (?$(5#](B . nil)
    (?$(5#^(B . nil)
    (?$(5#_(B . nil)
    (?$(5#`(B . nil)
    (?$(5#a(B . ?$(6#a(B)
    (?$(5#b(B . ?$(6#b(B)
    (?$(5#c(B . nil)
    (?$(5#d(B . nil)
    (?$(5#e(B . nil)
    (?$(5#f(B . nil)
    (?$(5#g(B . nil)
    (?$(5#h(B . nil)
    (?$(5#i(B . nil)
    (?$(5#j(B . ?$(6#j(B)
    (?$(5#k(B . ?$(6#k(B)
    (?$(5#l(B . ?$(6#l(B)
    (?$(5#m(B . nil)
    (?$(5#n(B . nil)
    (?$(5#o(B . nil)
    (?$(5#p(B . nil)
    (?$(5#q(B . nil)
    (?$(5#r(B . nil)
    (?$(5#s(B . nil)
    (?$(5#t(B . nil)
    (?$(5#u(B . nil)
    (?$(5#v(B . nil)
    (?$(5#w(B . nil)
    (?$(5#x(B . nil)
    (?$(5#y(B . nil)
    (?$(5#z(B . nil)
    (?$(5#{(B . nil)
    (?$(5#|(B . nil)
    (?$(5#}(B . nil)
    (?$(5#~(B . nil)
    (?$(5$!(B . nil)
    (?$(5$"(B . nil)
    (?$(5$#(B . nil)
    (?$(5$$(B . nil)
    (?$(5$%(B . nil)
    (?$(5$&(B . nil)
    (?$(5$'(B . nil)
    (?$(5$((B . nil)
    (?$(5$)(B . nil)
    (?$(5$*(B . nil)
    (?$(5$+(B . nil)
    (?$(5$,(B . nil)
    (?$(5$-(B . nil)
    (?$(5$.(B . nil)
    (?$(5$/(B . nil)
    (?$(5$0(B . nil)
    (?$(5$1(B . nil)
    (?$(5$2(B . nil)
    (?$(5$3(B . nil)
    (?$(5$4(B . nil)
    (?$(5$5(B . nil)
    (?$(5$6(B . nil)
    (?$(5$7(B . nil)
    (?$(5$8(B . nil)
    (?$(5$9(B . nil)
    (?$(5$:(B . nil)
    (?$(5$;(B . nil)
    (?$(5$<(B . nil)
    (?$(5$=(B . nil)
    (?$(5$>(B . nil)
    (?$(5$?(B . nil)
    (?$(5$@(B . nil)
    (?$(5$A(B . ?$(6$A(B)
    (?$(5$B(B . nil)
    (?$(5$C(B . nil)
    (?$(5$D(B . nil)
    (?$(5$E(B . ?$(6$E(B)
    (?$(5$F(B . nil)
    (?$(5$G(B . nil)
    (?$(5$H(B . ?$(6$H(B)
    (?$(5$I(B . ?$(6$I(B)
    (?$(5$J(B . ?$(6$J(B)
    (?$(5$K(B . nil)
    (?$(5$L(B . nil)
    (?$(5$M(B . nil)
    (?$(5$N(B . ?$(6$N(B)
    (?$(5$O(B . nil)
    (?$(5$P(B . ?$(6$P(B)
    (?$(5$Q(B . ?$(6$Q(B)
    (?$(5$R(B . ?$(6$R(B)
    (?$(5$S(B . nil)
    (?$(5$T(B . nil)
    (?$(5$U(B . nil)
    (?$(5$V(B . nil)
    (?$(5$W(B . nil)
    (?$(5$X(B . nil)
    (?$(5$Y(B . nil)
    (?$(5$Z(B . nil)
    (?$(5$[(B . nil)
    (?$(5$\(B . nil)
    (?$(5$](B . nil)
    (?$(5$^(B . nil)
    (?$(5$_(B . nil)
    (?$(5$`(B . nil)
    (?$(5$a(B . nil)
    (?$(5$b(B . nil)
    (?$(5$c(B . nil)
    (?$(5$d(B . nil)
    (?$(5$e(B . nil)
    (?$(5$f(B . nil)
    (?$(5$g(B . nil)
    (?$(5$h(B . ?$(6$h(B)
    (?$(5$i(B . ?$(6$i(B)
    (?$(5$j(B . ?$(6$j(B)
    (?$(5$k(B . nil)
    (?$(5$l(B . ?$(6$l(B)
    (?$(5$m(B . ?$(6$m(B)
    (?$(5$n(B . ?$(6$n(B)
    (?$(5$o(B . nil)
    (?$(5$p(B . ?$(6$p(B)
    (?$(5$q(B . ?$(6$q(B)
    (?$(5$r(B . ?$(6$r(B)
    (?$(5$s(B . nil)
    (?$(5$t(B . nil)
    (?$(5$u(B . ?$(6$u(B)
    (?$(5$v(B . ?$(6$v(B)
    (?$(5$w(B . nil)
    (?$(5$x(B . ?$(6$x(B)
    (?$(5$y(B . ?$(6$y(B)
    (?$(5$z(B . nil)
    (?$(5${(B . nil)
    (?$(5$|(B . nil)
    (?$(5$}(B . nil)
    (?$(5$~(B . nil)
    ))

(provide 'devanagari)

;;; devanagari.el ends here
