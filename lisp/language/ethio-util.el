;; ethio-util.el -- utilities for Ethiopic

;; Copyright (C) 1995 Free Software Foundation, Inc.
;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.

;; Keywords: mule, multilingual, Chinese

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

;;; Code:

;;
;; ETHIOPIC UTILITY FUNCTIONS
;;

;; To automatically convert Ethiopic text to SERA format when sending mail,
;;   (add-hook 'mail-send-hook 'fidel-to-sera-mail)
;;
;; To automatically convert SERA format to Ethiopic when receiving mail,
;;   (add-hook 'rmail-show-message-hook 'sera-to-fidel-mail)
;;
;; To automatically convert Ethiopic text to SERA format when posting news,
;;   (add-hook 'news-inews-hook 'fidel-to-sera-mail)
;;
;; If the filename ends in ".sera", editing will be done in fidel
;; while file I/O will be done in sera.

;;
;; SERA to FIDEL
;;
   
(defconst sera-to-fidel-table
  [
   nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
;;; SP  !   "   #   $   %   &   '    (   )   *   +    ,     -    .     /
   nil nil nil nil nil nil nil ("") nil nil nil nil ("$(2$Q(B") nil ("$(2$P(B") nil
;;; 0   1   2   3   4   5   6   7   8   9    :      ;     <   =   >   ?   @
   nil nil nil nil nil nil nil nil nil nil ("$(2$S(B") ("$(2$R(B") nil nil nil nil nil
;;; A
   ("$(2"V(B" (?2 "$(2#b(B"))
;;; B
   ("$(2!F(B" (?e "$(2!A(B") (?u "$(2!B(B") (?i "$(2!C(B") (?a "$(2!D(B") (?E "$(2!E(B") (?o "$(2!G(B") (?| "$(2!F(B")
         (?W "$(2!H(B" (?a "$(2!H(B")
	          (?e "$(2!F#L(B") (?u "$(2!F#M(B") (?i "$(2!F#N(B") (?E "$(2!F#P(B") (?' "$(2!F#M(B")))
;;; C
   ("$(2"8(B" (?e "$(2"3(B") (?u "$(2"4(B") (?i "$(2"5(B") (?a "$(2"6(B") (?E "$(2"7(B") (?o "$(2"9(B") (?| "$(2"8(B")
         (?W "$(2":(B" (?a "$(2":(B")
	          (?e "$(2"8#L(B") (?u "$(2"8#M(B") (?i "$(2"8#N(B") (?E "$(2"8#P(B") (?' "$(2"8#M(B")))
;;; D
   ("$(2$0(B" (?e "$(2$+(B") (?u "$(2$,(B") (?i "$(2$-(B") (?a "$(2$.(B") (?E "$(2$/(B") (?o "$(2$1(B") (?| "$(2$0(B"))
;;; E
   ("$(2"W(B" (?2 "$(2#c(B"))
;;; F
   ("$(2"@(B" (?e "$(2";(B") (?u "$(2"<(B") (?i "$(2"=(B") (?a "$(2">(B") (?E "$(2"?(B") (?o "$(2"A(B") (?| "$(2"@(B")
         (?W "$(2"B(B" (?a "$(2"B(B")
	          (?e "$(2"@#L(B") (?u "$(2"@#M(B") (?i "$(2"@#N(B") (?E "$(2"@#P(B") (?' "$(2"@#M(B")))
;;; G
   ("$(2$>(B" (?e "$(2$9(B") (?u "$(2$:(B") (?i "$(2$;(B") (?a "$(2$<(B") (?E "$(2$=(B") (?o "$(2$?(B") (?| "$(2$>(B"))
;;; H
   ("$(2$"(B" (?e "$(2#{(B") (?u "$(2#|(B") (?i "$(2#}(B") (?a "$(2#~(B") (?E "$(2$!(B") (?o "$(2$#(B") (?| "$(2$"(B"))
;;; I
   ("$(2"X(B" (?2 "$(2#d(B"))
;;; J
   ("$(2$7(B" (?e "$(2$2(B") (?u "$(2$3(B") (?i "$(2$4(B") (?a "$(2$5(B") (?E "$(2$6(B") (?o "$(2$8(B") (?| "$(2$7(B"))
;;; K
   ("$(2"x(B" (?e "$(2"s(B") (?u "$(2"t(B") (?i "$(2"u(B") (?a "$(2"v(B") (?E "$(2"w(B") (?o "$(2"y(B") (?| "$(2"x(B")
         (?W "$(2"{(B" (?e "$(2"z(B") (?u "$(2"{(B") (?i "$(2"|(B") (?a "$(2"}(B") (?E "$(2"~(B")))
;;; L
   ("$(2!&(B" (?e "$(2!!(B") (?u "$(2!"(B") (?i "$(2!#(B") (?a "$(2!$(B") (?E "$(2!%(B") (?o "$(2!'(B") (?| "$(2!&(B")
         (?W "$(2!((B" (?a "$(2!((B")
                  (?e "$(2!&#L(B") (?u "$(2!&#M(B") (?i "$(2!&#N(B") (?E "$(2!&#P(B") (?' "$(2!&#M(B")))
;;; M
   ("$(2!.(B" (?e "$(2!)(B") (?u "$(2!*(B") (?i "$(2!+(B") (?a "$(2!,(B") (?E "$(2!-(B") (?o "$(2!/(B") (?| "$(2!.(B")
         (?W "$(2!0(B" (?a "$(2!0(B")
	          (?e "$(2!.#L(B") (?u "$(2!.#M(B") (?i "$(2!.#N(B") (?E "$(2!.#P(B") (?' "$(2!.#M(B")))
;;; N
   ("$(2!n(B" (?e "$(2!i(B") (?u "$(2!j(B") (?i "$(2!k(B") (?a "$(2!l(B") (?E "$(2!m(B") (?o "$(2!o(B") (?| "$(2!n(B")
         (?W "$(2!p(B" (?a "$(2!p(B")
	          (?e "$(2!n#L(B") (?u "$(2!n#M(B") (?i "$(2!n#N(B") (?E "$(2!n#P(B") (?' "$(2!n#M(B")))
;;; O
   ("$(2"Y(B" (?2 "$(2#e(B"))
;;; P
   ("$(2$E(B" (?e "$(2$@(B") (?u "$(2$A(B") (?i "$(2$B(B") (?a "$(2$C(B") (?E "$(2$D(B") (?o "$(2$F(B") (?| "$(2$E(B"))
;;; Q
   ("$(2#2(B" (?e "$(2#-(B") (?u "$(2#.(B") (?i "$(2#/(B") (?a "$(2#0(B") (?E "$(2#1(B") (?o "$(2#3(B") (?| "$(2#2(B")
         (?W "$(2#5(B" (?e "$(2#4(B") (?u "$(2#5(B") (?i "$(2#6(B") (?a "$(2#7(B") (?E "$(2#8(B")))
;;; R
   ("$(2!6(B" (?e "$(2!1(B") (?u "$(2!2(B") (?i "$(2!3(B") (?a "$(2!4(B") (?E "$(2!5(B") (?o "$(2!7(B") (?| "$(2!6(B")
         (?W "$(2!8(B" (?a "$(2!8(B")
	          (?e "$(2!6#L(B") (?u "$(2!6#M(B") (?i "$(2!6#N(B") (?E "$(2!6#P(B") (?' "$(2!6#M(B")))
;;; S
   ("$(2"P(B" (?e "$(2"K(B") (?u "$(2"L(B") (?i "$(2"M(B") (?a "$(2"N(B") (?E "$(2"O(B") (?o "$(2"Q(B") (?| "$(2"P(B")
         (?W "$(2"R(B" (?a "$(2"R(B")
                  (?e "$(2"P#L(B") (?u "$(2"P#M(B") (?i "$(2"P#N(B") (?E "$(2"P#P(B") (?' "$(2"P#M(B"))
	 (?2 "$(2#](B" (?| "$(2#](B")
	     (?e "$(2#X(B") (?u "$(2#Y(B") (?i "$(2#Z(B") (?a "$(2#[(B") (?E "$(2#\(B") (?o "$(2#^(B")
	     (?W "$(2"R(B"
		 (?a "$(2"R(B")
		 (?e "$(2#]#L(B") (?u "$(2#]#M(B") (?i "$(2#]#N(B") (?E "$(2#]#P(B") (?' "$(2#]#M(B"))))
		
;;; T
   ("$(2"0(B" (?e "$(2"+(B") (?u "$(2",(B") (?i "$(2"-(B") (?a "$(2".(B") (?E "$(2"/(B") (?o "$(2"1(B") (?| "$(2"0(B")
         (?W "$(2"2(B" (?a "$(2"2(B")
	          (?e "$(2"0#L(B") (?u "$(2"0#M(B") (?i "$(2"0#N(B") (?E "$(2"0#P(B") (?' "$(2"0#M(B")))
;;; U
   ("$(2"T(B" (?2 "$(2#`(B"))
;;; V
   ("$(2!N(B" (?e "$(2!I(B") (?u "$(2!J(B") (?i "$(2!K(B") (?a "$(2!L(B") (?E "$(2!M(B") (?o "$(2!O(B") (?| "$(2!N(B")
         (?W "$(2!P(B" (?a "$(2!P(B")
	          (?e "$(2!N#L(B") (?u "$(2!N#M(B") (?i "$(2!N#N(B") (?E "$(2!N#P(B") (?' "$(2!N#M(B")))
;;; W
   ("$(2#M(B" (?e "$(2#L(B") (?u "$(2#M(B") (?i "$(2#N(B") (?a "$(2#O(B") (?E "$(2#P(B"))
;;; X
   ("$(2#y(B" (?e "$(2#t(B") (?u "$(2#u(B") (?i "$(2#v(B") (?a "$(2#w(B") (?E "$(2#x(B") (?o "$(2#z(B") (?| "$(2#y(B"))
;;; Y
   ("$(2$)(B" (?e "$(2$$(B") (?u "$(2$%(B") (?i "$(2$&(B") (?a "$(2$'(B") (?E "$(2$((B") (?o "$(2$*(B") (?| "$(2$)(B"))
;;; Z
   ("$(2!~(B" (?e "$(2!y(B") (?u "$(2!z(B") (?i "$(2!{(B") (?a "$(2!|(B") (?E "$(2!}(B") (?o "$(2"!(B") (?| "$(2!~(B")
         (?W "$(2""(B" (?a "$(2""(B")
	          (?e "$(2!~#L(B") (?u "$(2!~#M(B") (?i "$(2!~#N(B") (?E "$(2!~#P(B") (?' "$(2!~#M(B")))
;;; [   \   ]   ^   _
   nil nil nil nil nil
;;; `
   ("`"
    (?e "$(2#_(B") (?u "$(2#`(B") (?U "$(2#`(B") (?i "$(2#a(B") (?a "$(2#b(B") (?A "$(2#b(B")
    (?E "$(2#c(B") (?I "$(2#d(B") (?o "$(2#e(B") (?O "$(2#e(B")
    (?s "$(2#V(B"
	(?e "$(2#Q(B") (?u "$(2#R(B") (?i "$(2#S(B") (?a "$(2#T(B") (?E "$(2#U(B") (?o "$(2#W(B") (?| "$(2#V(B")
	(?W "$(2"J(B" (?a "$(2"J(B")
	         (?e "$(2#V#L(B") (?u "$(2#V#M(B") (?i "$(2#V#N(B") (?E "$(2#V#P(B") (?' "$(2#V#M(B")))
    (?S "$(2#](B"
	(?e "$(2#X(B") (?u "$(2#Y(B") (?i "$(2#Z(B") (?a "$(2#[(B") (?E "$(2#\(B") (?o "$(2#^(B") (?| "$(2#](B")
	(?W "$(2"R(B" (?a "$(2"R(B")
	         (?e "$(2#]#L(B") (?u "$(2#]#M(B") (?i "$(2#]#N(B") (?E "$(2#]#P(B") (?' "$(2#]#M(B")))
    (?h "$(2#k(B"
	(?e "$(2#f(B") (?u "$(2#g(B") (?i "$(2#h(B") (?a "$(2#i(B") (?E "$(2#j(B") (?o "$(2#l(B") (?| "$(2#k(B")
        (?W "$(2"c(B" (?e "$(2"b(B") (?u "$(2"c(B") (?i "$(2"d(B") (?a "$(2"e(B") (?E "$(2"f(B")))
    (?k "$(2#r(B"
        (?e "$(2#m(B") (?u "$(2#n(B") (?i "$(2#o(B") (?a "$(2#p(B") (?E "$(2#q(B") (?o "$(2#s(B") (?| "$(2#r(B")))
;;; a
   ("$(2"S(B" (?2 "$(2#b(B"))

;;; b
   ("$(2!F(B" (?e "$(2!A(B") (?u "$(2!B(B") (?i "$(2!C(B") (?a "$(2!D(B") (?E "$(2!E(B") (?o "$(2!G(B") (?| "$(2!F(B")
         (?W "$(2!H(B" (?a "$(2!H(B")
	          (?e "$(2!F#L(B") (?u "$(2!F#M(B") (?i "$(2!F#N(B") (?E "$(2!F#P(B") (?' "$(2!F#M(B")))
;;; c
   ("$(2!^(B" (?e "$(2!Y(B") (?u "$(2!Z(B") (?i "$(2![(B") (?a "$(2!\(B") (?E "$(2!](B") (?o "$(2!_(B") (?| "$(2!^(B")
         (?W "$(2!`(B" (?a "$(2!`(B")
	          (?e "$(2!^#L(B") (?u "$(2!^#M(B") (?i "$(2!^#N(B") (?E "$(2!^#P(B") (?' "$(2!^#M(B")))
;;; d
   ("$(2"((B" (?e "$(2"#(B") (?u "$(2"$(B") (?i "$(2"%(B") (?a "$(2"&(B") (?E "$(2"'(B") (?o "$(2")(B") (?| "$(2"((B")
         (?W "$(2"*(B" (?a "$(2"*(B")
	          (?e "$(2"(#L(B") (?u "$(2"(#M(B") (?i "$(2"(#N(B") (?E "$(2"(#P(B") (?' "$(2"(#M(B")))
;;; e
   ("$(2"S(B" (?2 "$(2#_(B") (?3 "$(2"Z(B"))
;;; f
   ("$(2"@(B" (?e "$(2";(B") (?u "$(2"<(B") (?i "$(2"=(B") (?a "$(2">(B") (?E "$(2"?(B") (?o "$(2"A(B") (?| "$(2"@(B")
         (?W "$(2"B(B" (?a "$(2"B(B")
	          (?e "$(2"@#L(B") (?u "$(2"@#M(B") (?i "$(2"@#N(B") (?E "$(2"@#P(B") (?' "$(2"@#M(B")))
;;; g
   ("$(2#>(B" (?e "$(2#9(B") (?u "$(2#:(B") (?i "$(2#;(B") (?a "$(2#<(B") (?E "$(2#=(B") (?o "$(2#?(B") (?| "$(2#>(B")
         (?W "$(2#A(B" (?e "$(2#@(B") (?u "$(2#A(B") (?i "$(2#B(B") (?a "$(2#C(B") (?E "$(2#D(B")))
;;; h
   ("$(2"`(B" (?e "$(2"[(B") (?u "$(2"\(B") (?i "$(2"](B") (?a "$(2"^(B") (?E "$(2"_(B") (?o "$(2"a(B") (?| "$(2"`(B")
         (?W "$(2"c(B" (?e "$(2"b(B") (?u "$(2"c(B") (?i "$(2"d(B") (?a "$(2"e(B") (?E "$(2"f(B"))
	 (?2 "$(2#k(B" (?e "$(2#f(B") (?u "$(2#g(B") (?i "$(2#h(B") (?a "$(2#i(B") (?E "$(2#j(B") (?o "$(2#l(B")
	          (?| "$(2#k(B")
	          (?W "$(2"c(B" (?e "$(2"b(B") (?u "$(2"c(B") (?i "$(2"d(B") (?a "$(2"e(B") (?E "$(2"f(B"))))
;;; i
   ("$(2"U(B" (?2 "$(2#a(B"))
;;; j
   ("$(2$7(B" (?e "$(2$2(B") (?u "$(2$3(B") (?i "$(2$4(B") (?a "$(2$5(B") (?E "$(2$6(B") (?o "$(2$8(B") (?| "$(2$7(B"))
;;; k
   ("$(2"l(B" (?e "$(2"g(B") (?u "$(2"h(B") (?i "$(2"i(B") (?a "$(2"j(B") (?E "$(2"k(B") (?o "$(2"m(B") (?| "$(2"l(B")
         (?W "$(2"o(B" (?e "$(2"n(B") (?u "$(2"o(B") (?i "$(2"p(B") (?a "$(2"q(B") (?E "$(2"r(B"))
	 (?2 "$(2#r(B" (?e "$(2#m(B") (?u "$(2#n(B") (?i "$(2#o(B") (?a "$(2#p(B") (?E "$(2#q(B") (?o "$(2#s(B")
	          (?| "$(2#r(B")))
;;; l
   ("$(2!&(B" (?e "$(2!!(B") (?u "$(2!"(B") (?i "$(2!#(B") (?a "$(2!$(B") (?E "$(2!%(B") (?o "$(2!'(B") (?| "$(2!&(B")
         (?W "$(2!((B" (?a "$(2!((B")
                  (?e "$(2!&#L(B") (?u "$(2!&#M(B") (?i "$(2!&#N(B") (?E "$(2!&#P(B") (?' "$(2!&#M(B")))
;;; m
   ("$(2!.(B" (?e "$(2!)(B") (?u "$(2!*(B") (?i "$(2!+(B") (?a "$(2!,(B") (?E "$(2!-(B") (?o "$(2!/(B") (?| "$(2!.(B")
         (?W "$(2!0(B" (?a "$(2!0(B")
	          (?e "$(2!.#L(B") (?u "$(2!.#M(B") (?i "$(2!.#N(B") (?E "$(2!.#P(B") (?' "$(2!.#M(B")))
;;; n
   ("$(2!f(B" (?e "$(2!a(B") (?u "$(2!b(B") (?i "$(2!c(B") (?a "$(2!d(B") (?E "$(2!e(B") (?o "$(2!g(B") (?| "$(2!f(B")
         (?W "$(2!h(B" (?a "$(2!h(B")
	          (?e "$(2!f#L(B") (?u "$(2!f#M(B") (?i "$(2!f#N(B") (?E "$(2!f#P(B") (?' "$(2!f#M(B")))
;;; o
   ("$(2"Y(B" (?2 "$(2#e(B"))
;;; p
   ("$(2$L(B" (?e "$(2$G(B") (?u "$(2$H(B") (?i "$(2$I(B") (?a "$(2$J(B") (?E "$(2$K(B") (?o "$(2$M(B") (?| "$(2$L(B"))
;;; q
   ("$(2#&(B" (?e "$(2#!(B") (?u "$(2#"(B") (?i "$(2##(B") (?a "$(2#$(B") (?E "$(2#%(B") (?o "$(2#'(B") (?| "$(2#&(B")
         (?W "$(2#)(B" (?e "$(2#((B") (?u "$(2#)(B") (?i "$(2#*(B") (?a "$(2#+(B") (?E "$(2#,(B")))
;;; r
   ("$(2!6(B" (?e "$(2!1(B") (?u "$(2!2(B") (?i "$(2!3(B") (?a "$(2!4(B") (?E "$(2!5(B") (?o "$(2!7(B") (?| "$(2!6(B")
         (?W "$(2!8(B" (?a "$(2!8(B")
	          (?e "$(2!6#L(B") (?u "$(2!6#M(B") (?i "$(2!6#N(B") (?E "$(2!6#P(B") (?' "$(2!6#M(B")))
;;; s
   ("$(2"H(B" (?e "$(2"C(B") (?u "$(2"D(B") (?i "$(2"E(B") (?a "$(2"F(B") (?E "$(2"G(B") (?o "$(2"I(B") (?| "$(2"H(B")
         (?W "$(2"J(B" (?a "$(2"J(B")
	          (?e "$(2"H#L(B") (?u "$(2"H#M(B") (?i "$(2"H#N(B") (?E "$(2"H#P(B") (?' "$(2"H#M(B"))
	 (?2 "$(2#V(B" (?e "$(2#Q(B") (?u "$(2#R(B") (?i "$(2#S(B") (?a "$(2#T(B") (?E "$(2#U(B") (?o "$(2#W(B")
	          (?| "$(2#V(B")
		  (?W "$(2"J(B" (?a "$(2"J(B")
		           (?e "$(2#V#L(B") (?u "$(2#V#M(B") (?i "$(2#V#N(B") (?E "$(2#V#P(B")
			   (?' "$(2#V#M(B"))))
;;; t
   ("$(2!V(B" (?e "$(2!Q(B") (?u "$(2!R(B") (?i "$(2!S(B") (?a "$(2!T(B") (?E "$(2!U(B") (?o "$(2!W(B") (?| "$(2!V(B")
         (?W "$(2!X(B" (?a "$(2!X(B")
	          (?e "$(2!V#L(B") (?u "$(2!V#M(B") (?i "$(2!V#N(B") (?E "$(2!V#P(B") (?' "$(2!V#M(B")))
;;; u
   ("$(2"T(B" (?2 "$(2#`(B"))
;;; v
   ("$(2!N(B" (?e "$(2!I(B") (?u "$(2!J(B") (?i "$(2!K(B") (?a "$(2!L(B") (?E "$(2!M(B") (?o "$(2!O(B") (?| "$(2!N(B")
         (?W "$(2!P(B" (?a "$(2!P(B")
	          (?e "$(2!N#L(B") (?u "$(2!N#M(B") (?i "$(2!N#N(B") (?E "$(2!N#P(B") (?' "$(2!N#M(B")))
;;; w
   ("$(2#J(B" (?e "$(2#E(B") (?u "$(2#F(B") (?i "$(2#G(B") (?a "$(2#H(B") (?E "$(2#I(B") (?o "$(2#K(B") (?| "$(2#J(B")
         (?W "$(2#M(B" (?e "$(2#L(B") (?u "$(2#M(B") (?i "$(2#N(B") (?a "$(2#O(B") (?E "$(2#P(B")))
;;; x
   ("$(2!>(B" (?e "$(2!9(B") (?u "$(2!:(B") (?i "$(2!;(B") (?a "$(2!<(B") (?E "$(2!=(B") (?o "$(2!?(B") (?| "$(2!>(B")
         (?W "$(2!@(B" (?a "$(2!@(B")
	          (?e "$(2!>#L(B") (?u "$(2!>#M(B") (?i "$(2!>#N(B") (?E "$(2!>#P(B") (?' "$(2!>#M(B")))
;;; y
   ("$(2$)(B" (?e "$(2$$(B") (?u "$(2$%(B") (?i "$(2$&(B") (?a "$(2$'(B") (?E "$(2$((B") (?o "$(2$*(B") (?| "$(2$)(B"))
;;; z
   ("$(2!v(B" (?e "$(2!q(B") (?u "$(2!r(B") (?i "$(2!s(B") (?a "$(2!t(B") (?E "$(2!u(B") (?o "$(2!w(B") (?| "$(2!v(B")
         (?W "$(2!x(B" (?a "$(2!x(B")
	          (?e "$(2!v#L(B") (?u "$(2!v#M(B") (?i "$(2!v#N(B") (?E "$(2!v#P(B") (?' "$(2!v#M(B")))
   ])

;;;###autoload
(defun sera-to-fidel-region (beg end &optional ascii-mode force)
  "Translates the characters in region from SERA to FIDEL.

If the 1st optional parameter ASCII-MODE is non-NIL, assumes that the
region begins in ASCII script.

If the 2nd optional parametr FORCE is non-NIL, translates even if the
buffer is read-only."

  (interactive "r\nP")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (sera-to-fidel-buffer ascii-mode force))))

;;;###autoload
(defun sera-to-fidel-buffer (&optional ascii-mode force)
  "Translates the current buffer from SERA to FIDEL.

If the 1st optional parameter ASCII-MODE is non-NIL, assumes that the
current buffer begins in ASCII script.

If the 2nd optional panametr FORCE is non-NIL, translates even if the
buffer is read-only."

  (interactive "P")
  (if (and buffer-read-only
	   (not force)
	   (not (y-or-n-p "Buffer is read-only.  Force to convert? ")))
      (error ""))
  (let (start pre fol hard table table2 (buffer-read-only nil))
    (goto-char (point-min))
    (while (not (eobp))
      (setq start (point))
      (forward-char 1)
      (setq pre (preceding-char)
	    fol (following-char))

      (if ascii-mode
	  (cond

	   ;; ascii mode, pre != \ 
	   ((/= pre ?\\ ))

	   ;; ascii mode, pre = \, fol = !
	   ((= fol ?!)
	    (backward-delete-char 1)
	    (delete-char 1)
	    (setq ascii-mode nil
		  hard (not hard)))

	   ;; hard ascii mode, pre = \, fol != !
	   (hard)

	   ;; soft ascii mode, pre = \, fol = {\ _ * < > 0..9 ~}
	   ((or (backward-delete-char 1) ; always nil
		(eobp)
		(sera-to-fidel-backslash)))

	   ;; soft ascii mode, pre = \, fol = SPC
	   ((= fol 32)
	    (delete-char 1)
	    (setq ascii-mode nil))

	   ;; soft ascii mode, pre = \, fol = .
	   ((= fol ?.)
	    (delete-char 1)
	    (insert ?$(2$P(B))

	   ;; soft ascii mode, pre = \, fol = ,
	   ((= fol ?,)
	    (delete-char 1)
	    (insert ?$(2$Q(B))

	   ;; soft ascii mode, pre = \, fol = ;
	   ((= fol ?\;)
	    (delete-char 1)
	    (insert ?$(2$R(B))

	   ;; soft ascii mode, pre = \, fol = :
	   ((= fol ?:)
	    (delete-char 1)
	    (insert ?$(2$S(B))

	   ;; soft ascii mode, pre = \, fol = others
	   (t
	    (setq ascii-mode nil)))

	(cond

	 ;; very special: skip "<" to ">" (or "&" to ";") if in w3-mode
	 ((and (boundp 'sera-being-called-by-w3)
	       sera-being-called-by-w3
	       (or (= pre ?<) (= pre ?&)))
	  (search-forward (if (= pre ?<) ">" ";")
			  nil 0))

	 ;; ethio mode, pre != sera
	 ((or (< pre ?') (> pre ?z)))

	 ;; ethio mode, pre != \ 
	 ((/= pre ?\\ )
	  (setq table (aref sera-to-fidel-table pre))
	  (while (setq table2 (cdr (assoc (following-char) table)))
	    (setq table table2)
	    (forward-char 1))
	  (if (car table)
	      (progn
		(delete-region start (point))
		(insert (car table)))))

	 ;; ethio mode, pre = \, fol = !
	 ((= fol ?!)
	  (backward-delete-char 1)
	  (delete-char 1)
	  (setq ascii-mode t
		hard (not hard)))

	 ;; hard ethio mode, pre = \, fol != !
	 (hard)

	 ;; soft ethio mode, pre = \, fol = {\ _ * < > 0..9 ~}
	 ((or (backward-delete-char 1)	; always nil
	      (eobp)
	      (sera-to-fidel-backslash)))

	 ;; soft ethio mode, pre = \, fol = SPC
	 ((= fol 32)
	  (delete-char 1)
	  (setq ascii-mode t))

	 ;; soft ethio mode, pre = \, fol = {. , ; : | ' `}
	 ((memq fol '(?. ?, ?\; ?: ?| ?' ?`))
	  (forward-char 1))

	 ;; soft ethio mode, pre = \, fol = others
	 (t
	  (setq ascii-mode t))))))
  (goto-char (point-min)))

(defun sera-to-fidel-backslash ()
  "Handle SERA backslash escapes common to ethio- and ascii-mode.
Returns t if something has been processed."
  (let ((ch (following-char))
	(converted t))
    (if (and (>= ch ?1) (<= ch ?9))
	(ethio-convert-digit)
      (delete-char 1)
      (cond
       ((= ch ?\\ )
	(insert ?\\ ))
       ((= ch ?_)
	(insert ?$(2$O(B))
       ((= ch ?*)
	(insert ?$(2$T(B))
       ((= ch ?<)
	(insert ?$(2$U(B))
       ((= ch ?>)
	(insert ?$(2$V(B))
       ((= ch ?~)
	(setq ch (following-char))
	(delete-char 1)
	(cond
	 ((= ch ?e)
	  (insert "$(2$k(B"))
	 ((= ch ?E)
	  (insert "$(2$l(B"))
	 ((= ch ?a)
	  (insert "$(2$m(B"))
	 ((= ch ?A)
	  (insert "$(2$n(B"))))
       (t
	(insert ch)
	(backward-char 1)
	(setq converted nil))))
    converted))

(defun ethio-convert-digit ()
  "Convert Arabic digits to Ethiopic digits."
  (let (ch z)
    (while (and (>= (setq ch (following-char)) ?1)
		(<= ch ?9))
      (delete-char 1)

      ;; count up following zeros
      (setq z 0)
      (while (= (following-char) ?0)
	(delete-char 1)
	(setq z (1+ z)))

      (cond

       ;; first digit is 10, 20, ..., or 90
       ((= (mod z 2) 1)
	;; (- ch 40) means ?1 -> 9, ?2 -> 10, etc.
	(insert (aref [?$(2$`(B ?$(2$a(B ?$(2$b(B ?$(2$c(B ?$(2$d(B ?$(2$e(B ?$(2$f(B ?$(2$g(B ?$(2$h(B] (- ch ?1)))
	(setq z (1- z)))

       ;; first digit is 2, 3, ..., or 9
       ((/= ch ?1)
	(insert (aref [?$(2$X(B ?$(2$Y(B ?$(2$Z(B ?$(2$[(B ?$(2$\(B ?$(2$](B ?$(2$^(B ?$(2$_(B] (- ch ?2))))

       ;; single 1
       ((= z 0)
	(insert "$(2$W(B")))

      ;; 100
      (if (= (mod z 4) 2)
	  (insert"$(2$i(B"))

      ;; 10000
      (insert-char ?$(2$j(B (/ z 4)))))

;;;###autoload
(defun sera-to-fidel-mail (&optional arg)
  "Does SERA to FIDEL conversion for reading/writing mail and news.

If the buffer contains the markers \"<sera>\" and \"</sera>\",
converts the segment between the two markers in Ethio start mode and
the subject field in ASCII start mode.

If invoked interactively and there is no marker, converts both the
whole body and the subject field in Ethio start mode.

For backward compatibility, \"<ethiopic>\" and \"<>\" can be used instead of
\"<sera>\" and \"</sera>\"."

  (interactive "p")
  (let* ((buffer-read-only nil) border)

    (save-excursion
      (goto-char (point-min))
      (setq border
	    (search-forward
	     (if (eq major-mode 'rmail-mode)
		 "\n\n"
	       (concat "\n" mail-header-separator "\n"))))

      (cond

       ;; with markers
       ((re-search-forward "^<sera>\n" nil t)
	(goto-char (match-beginning 0))
	(while (re-search-forward "^<sera>\n" nil t)
	  (replace-match "" nil t)
	  (sera-to-fidel-region
	   (point)
	   (progn
	     (if (re-search-forward "^</sera>\n" nil 0)
		 (replace-match "" nil t))
	     (point))))

	(goto-char (point-min))
	(if (re-search-forward "^Subject: " border t)
	    (sera-to-fidel-region
	     (point)
	     (progn (end-of-line) (point))
	     'ascii-start)))

       ;; backward compatibility
       ((re-search-forward "^<ethiopic>\n" nil t)
	(goto-char (match-beginning 0))
	(while (re-search-forward "^<ethiopic>\n" nil t)
	  (replace-match "" nil t)
	  (sera-to-fidel-region
	   (setq border (point))
	   (progn
	     (if (re-search-forward "^<>\n" nil 0)
		 (replace-match "" nil t))
	     (point))))

	(goto-char (point-min))
	(if (re-search-forward "^Subject: " border t)
	    (sera-to-fidel-region
	     (point)
	     (progn (end-of-line) (point))
	     'ascii-start)))

       ;; interactive & no markers
       (arg
	(sera-to-fidel-region border (point-max))
	(goto-char (point-min))
	(if (re-search-forward "^Subject: " border t)
	    (sera-to-fidel-region
	     (point)
	     (progn (end-of-line) (point))))))

      ;; adjust the rmail marker
      (if (eq major-mode 'rmail-mode)
	  (set-marker
	   (aref rmail-message-vector (1+ rmail-current-message))
	   (point-max))))))

;;;###autoload
(defun sera-to-fidel-marker ()
  "If the buffer contains the markers \"<sera>\" and \"</sera>\",
converts the segment between the two markers from SERA to Fidel
in Ethio start mode.  The markers will not be removed."

  (interactive)
  (if (and buffer-read-only
	   (not (y-or-n-p "Buffer is read-only.  Force to convert? ")))
      (error ""))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "<sera>" nil t)
      (sera-to-fidel-region
       (point)
       (if (re-search-forward "</sera>" nil t)
	   (match-beginning 0)
	 (point-max))
       nil
       'force))))

;;
;; FIDEL to SERA
;;

(defconst fidel-to-sera-map
  ["le" "lu" "li" "la" "lE" "l" "lo" "lWa"
   "me" "mu" "mi" "ma" "mE" "m" "mo" "mWa"
   "re" "ru" "ri" "ra" "rE" "r" "ro" "rWa"
   "xe" "xu" "xi" "xa" "xE" "x" "xo" "xWa"
   "be" "bu" "bi" "ba" "bE" "b" "bo" "bWa"
   "ve" "vu" "vi" "va" "vE" "v" "vo" "vWa"
   "te" "tu" "ti" "ta" "tE" "t" "to" "tWa"
   "ce" "cu" "ci" "ca" "cE" "c" "co" "cWa"
   "ne" "nu" "ni" "na" "nE" "n" "no" "nWa"
   "Ne" "Nu" "Ni" "Na" "NE" "N" "No" "NWa"
   "ze" "zu" "zi" "za" "zE" "z" "zo" "zWa"
   "Ze" "Zu" "Zi" "Za" "ZE" "Z" "Zo" "ZWa"
   "de" "du" "di" "da" "dE" "d" "do" "dWa"
   "Te" "Tu" "Ti" "Ta" "TE" "T" "To" "TWa"
   "Ce" "Cu" "Ci" "Ca" "CE" "C" "Co" "CWa"
   "fe" "fu" "fi" "fa" "fE" "f" "fo" "fWa"
   "se" "su" "si" "sa" "sE" "s" "so" "sWa"
   "Se" "Su" "Si" "Sa" "SE" "S" "So" "SWa"
   "a"  "u"  "i"  "A"  "E"  "I" "o"  "e3"
   "he" "hu" "hi" "ha" "hE" "h" "ho" "hWe" "hWu" "hWi" "hWa" "hWE"
   "ke" "ku" "ki" "ka" "kE" "k" "ko" "kWe" "kWu" "kWi" "kWa" "kWE"
   "Ke" "Ku" "Ki" "Ka" "KE" "K" "Ko" "KWe" "KWu" "KWi" "KWa" "KWE"
   "qe" "qu" "qi" "qa" "qE" "q" "qo" "qWe" "qWu" "qWi" "qWa" "qWE"
   "Qe" "Qu" "Qi" "Qa" "QE" "Q" "Qo" "QWe" "QWu" "QWi" "QWa" "QWE"
   "ge" "gu" "gi" "ga" "gE" "g" "go" "gWe" "gWu" "gWi" "gWa" "gWE"
   "we" "wu" "wi" "wa" "wE" "w" "wo" "wWe" "wWu" "wWi" "wWa" "wWE"
   "`se" "`su" "`si" "`sa" "`sE" "`s" "`so"
   "`Se" "`Su" "`Si" "`Sa" "`SE" "`S" "`So"
   "`e"  "`u"  "`i"  "`a"  "`E"  "`I" "`o"
   "`he" "`hu" "`hi" "`ha" "`hE" "`h" "`ho"
   "`ke" "`ku" "`ki" "`ka" "`kE" "`k" "`ko"
   "Xe" "Xu" "Xi" "Xa" "XE" "X" "Xo"
   "He" "Hu" "Hi" "Ha" "HE" "H" "Ho"
   "ye" "yu" "yi" "ya" "yE" "y" "yo"
   "De" "Du" "Di" "Da" "DE" "D" "Do"
   "je" "ju" "ji" "ja" "jE" "j" "jo"
   "Ge" "Gu" "Gi" "Ga" "GE" "G" "Go"
   "Pe" "Pu" "Pi" "Pa" "PE" "P" "Po"
   "pe" "pu" "pi" "pa" "pE" "p" "po"
   " " "\\_" "." "," ";" ":" "\\*" "\\<" "\\>"
   "1" "2" "3" "4" "5" "6" "7" "8" "9"
   "10" "20" "30" "40" "50" "60" "70" "80" "90"
   "100" "10000"
   "\\~e" "\\~E" "\\~a" "\\~A"])

(defvar ethio-use-tigrigna-style nil
  "*If non-NIL, use \"e\" instead of \"a\" for the first lone vowel
translation in sera-to-fidel and fidel-to-sera conversions.")

(defvar ethio-quote-vowel-always nil
  "*If non-NIL, lone vowels are always transcribed by \"an apostrophe
+ the vowel\" except at word initial.  Otherwise, they are quoted by
an apostrophe only if the preceding Ethiopic character is a lone
consonant.")

(defvar ethio-W-sixth-always nil
  "*If non-NIL, the Wu-form of a 12-form consonant is transcribed by
\"W'\" instead of \"Wu\".")

(defvar ethio-numeric-reduction 0
  "*Degree of reduction in transcribing Ethiopic digits by Arabic
digits.  For example, $(2$`$_$i$g$](B ({10}{9}{100}{80}{7}) will be
transcribed by:
    \10\9\100\80\7	if ETHIO-NUMERIC-REDUCTION is 0,
    \109100807					   is 1,
    \10900807					   is 2.")

;;;###autoload
(defun fidel-to-sera-region (begin end &optional ascii-mode force)
  "Replaces all the FIDEL characters in the region to sera format.

If the 1st optional parameter ASCII-MODE is non-NIL, converts the
region so that it begins in ASCII script.

If the 2nd optional parameter FORCE is non-NIL, converts even if the
buffer is read-only."

  (interactive "r\nP")
  (save-excursion
    (save-restriction
      (narrow-to-region begin end)
      (fidel-to-sera-buffer ascii-mode force))))

;;;###autoload
(defun fidel-to-sera-buffer (&optional ascii-mode force)
  "Replace all the FIDEL characters in the current buffer to sera format.

If the 1st optional parameter ASCII-MODE is non-NIL,
convert the current buffer so that it begins in ASCII script.

If the 2nd optional parameter FORCE is non-NIL, converts even if the
buffer is read-only.

See also the description of the variables ethio-use-tigrigna-style,
ethio-quote-vowel-on-demand and ethio-numeric-reduction."

  (interactive "P")
  (if (and buffer-read-only
	   (not force)
	   (not (y-or-n-p "Buffer is read-only.  Force to convert? ")))
      (error ""))

  ;; user's preference in transcription
  (aset fidel-to-sera-map 144 (if ethio-use-tigrigna-style "e" "a"))
  (let ((i 160)
	(x (if ethio-W-sixth-always
	       '("hW'" "kW'" "KW'" "qW'" "QW'" "gW'" "wW'")
	     '("hWu" "kWu" "KWu" "qWu" "QWu" "gWu" "wWu"))))
    (while x
      (aset fidel-to-sera-map i (car x))
      (setq i (+ i 12)
	    x (cdr x))))

  ;; main conversion routine
  (let ((lonec nil) ; if lonec = t, previous char was a lone consonant.
	(fidel nil) ; if fidel = t, previous char was a fidel.
	(digit nil) ; if digit = t, previous char was an Ethiopic digit.
	(buffer-read-only nil)
	ch)
    (goto-char (point-min))
    (while (not (eobp))
      (setq ch (following-char))

      ;; ethiopic charactes
      (if (eq (char-charset ch) 'ethiopic)
	  (progn
	    (setq ch (char-to-ethiocode ch))
	    (delete-char 1)

	    (cond

	     ;; fidels
	     ((<= ch 326)
	      (if ascii-mode
		  (insert "\\ "))
	      (if (and (memq ch '(144 145 146 147 148 150 151)) ; (auiAEoe3)
		       (or lonec
			   (and ethio-quote-vowel-always
				fidel)))
		  (insert "'"))
	      (insert (aref fidel-to-sera-map ch))
	      (setq ascii-mode nil
		    lonec (ethio-lone-consonant-p ch)
		    fidel t
		    digit nil))

	     ;; punctuations and symbols
	     ((or (< ch 336) (> ch 355))
	      (if (and ascii-mode
		       (memq ch '(329 330 331 332))) ; (.,;:)
		  (insert "\\"))
	      (insert (aref fidel-to-sera-map ch))
	      (setq lonec nil
		    fidel nil
		    digit nil))

	     ;; now CH must be an ethiopic digit

	     ;; reduction = 0 or leading digit
	     ((or (= ethio-numeric-reduction 0)
		  (not digit))
	      (insert "\\" (aref fidel-to-sera-map ch))
	      (setq lonec nil
		    fidel nil
		    digit t))

	     ;; reduction = 2 and following 10s, 100s, 10000s 
	     ((and (= ethio-numeric-reduction 2)
		   (memq ch '(345 354 355)))
	      (insert (substring (aref fidel-to-sera-map ch) 1))
	      (setq lonec nil
		    fidel nil
		    digit t))

	     ;; ordinary following digits
	     (t
	      (insert (aref fidel-to-sera-map ch))
	      (setq lonec nil
		    fidel nil
		    digit t))))

	;; non-ethiopic characters
	(cond

	 ;; backslash is always quoted
	 ((= ch ?\\ )
	  (insert "\\"))

	 ;; nothing to do if in ascii-mode
	 (ascii-mode)

	 ;; ethio-mode -> ascii-mode
	 ((or (and (>= ch ?a) (<= ch ?z))
	      (and (>= ch ?A) (<= ch ?Z))
	      (memq ch '(?| ?' ?`)))
	  (insert "\\ ")
	  (setq ascii-mode t))

	 ;; ascii punctuations in ethio-mode
	 ((memq ch '(?. ?, ?\; ?:))
	  (insert "\\")))

	(forward-char 1)
	(setq lonec nil
	      fidel nil
	      digit nil)))

    ;; a few modifications for readability
    (goto-char (point-min))
    (while (re-search-forward "\\([]!\"#$%&()*+/<=>?@[^_-]+\\)\\\\ " nil t)
      (replace-match "\\\\ \\1"))

    (goto-char (point-min))
    (while (re-search-forward "\n\\([ \t]*\\)\\\\ " nil t)
      (replace-match "\\\\\n\\1")))

  (goto-char (point-min)))

(defun ethio-lone-consonant-p (code)
  "If the ethiocode CODE is an Ethiopic lone consonant, return t."
  (cond
   ((< code 144)
    (= (mod code 8) 5))
   ((< code 153)
    nil)
   ((< code 236)
    (= (mod code 12) 1))
   ((< code 327)
    (= (mod code 7) 3))))

;;;###autoload
(defun fidel-to-sera-mail ()
  "Does FIDEL to SERA conversion for reading/writing mail and news.

If the buffer contains at least one Ethiopic character,
 1) inserts the string \"<sera>\" right after the header-body separator,
 2) inserts \"</sera>\" at the end of the buffer,
 3) converts the body into SERA in Ethiopic start mode, and
 4) converts the subject field in ASCII start mode."

  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "\\cE" nil t)
	(let ((buffer-read-only nil) border)

	  (goto-char (point-min))
	  (setq border
		(search-forward
		 (if (eq major-mode 'rmail-mode)
		     "\n\n"
		   (concat "\n" mail-header-separator "\n"))))
	  (insert "<sera>\n")

	  (fidel-to-sera-region (point) (point-max))

	  (goto-char (point-max))
	  (if (/= (preceding-char) ?\n)
	      (insert "\n"))
	  (insert "</sera>\n")

	  (goto-char (point-min))
	  (if (re-search-forward "^Subject: " border t)
	      (fidel-to-sera-region
	       (point)
	       (progn (end-of-line) (point))
	       'ascii-start))

	  ;; adjust the rmail marker
	  (if (eq major-mode 'rmail-mode)
	      (set-marker
	       (aref rmail-message-vector (1+ rmail-current-message))
	       (point-max))))

      (message "No Ethiopic characters in this buffer."))))

;;;###autoload
(defun fidel-to-sera-marker ()
  "If the buffer contains the markers \"<sera>\" and \"</sera>\",
converts the segment between the two markers from Fidel to SERA
in Ethio start mode.  The markers will not be removed."

  (interactive)
  (if (and buffer-read-only
	   (not (y-or-n-p "Buffer is read-only.  Force to convert? ")))
      (error ""))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^<sera>\n" nil t)
      (fidel-to-sera-region
       (point)
       (if (re-search-forward "^</sera>\n" nil t)
	   (match-beginning 0)
	 (point-max))
       nil
       'force))))

;;
;; file I/O hooks
;;

(if (not (assoc "\\.sera$" auto-mode-alist))
    (setq auto-mode-alist
	  (cons '("\\.sera$" . sera-to-fidel-find-file) auto-mode-alist)))
(add-hook 'write-file-hooks 'fidel-to-sera-write-file)
(add-hook 'after-save-hook 'sera-to-fidel-after-save)

;;;###autoload
(defun sera-to-fidel-find-file ()
  "Intended to be called when a file whose name ends in \".sera\" is read in."
  (sera-to-fidel-buffer nil 'force)
  (set-buffer-modified-p nil)
  nil)

;;;###autoload
(defun fidel-to-sera-write-file ()
  "Intended to be used as write-file-hooks for the files
whose name ends in \".sera\"."
  (if (string-match "\\.sera$" (buffer-file-name))
      (save-excursion
	(fidel-to-sera-buffer nil 'force)
	(set-buffer-modified-p nil)))
  nil)

;;;###autoload
(defun sera-to-fidel-after-save ()
  "Intended to be used as after-save-hook for the files
whose name ends in \".sera\"."
  (if (string-match "\\.sera$" (buffer-file-name))
      (save-excursion
	(sera-to-fidel-buffer nil 'force)
	(set-buffer-modified-p nil)))
  nil)

;;
;; vowel modification
;;

;;;###autoload
(defun ethio-modify-vowel ()
  "Modify the vowel of the FIDEL that is under the cursor."
  (interactive)
  (let ((ch (following-char)) newch base vowel)
    (if (eq (char-charset ch) 'ethiopic)
	(setq ch (char-to-ethiocode ch))
      (error "Not a valid character."))
    (if (or (and (>= ch 144) (<= ch 151)) ; lone vowels
	    (and (>= ch 250) (<= ch 256)) ; secondary lone vowels
	    (>= ch 327))		  ; not FIDEL
	(error "Not a valid character."))
    (message "Modify vowel to: ")
    (if (null (setq vowel (memq (read-char) '(?e ?u ?i ?a ?E ?' ?o))))
	(error "Not a valid vowel.")
      ;; ?e -> 0, ?u -> 1, ?i -> 2, ?a -> 3, ?E -> 4, ?' -> 5, ?o -> 6
      (setq vowel (- 7 (length vowel))))

    (cond

     ;; 8-form consonant
     ((<= ch 143)
      (setq base (* (/ ch 8) 8))
      (cond
       ((< (mod ch 8) 7)		; e-form <= ch <= o-form
	(setq newch (+ base vowel)))
       ((= vowel 3)			; 3 = a
	(setq newch (+ base 7)))	; (+ base 7) = Wa-form
       ((= vowel 5)			; 5 = '
	(setq newch
	      (cons (+ base 5)		; (+ base 5) = lone consonant
		    232)))		; 232 = Wu
       (t
	(setq newch
	      (cons (+ base 5)		; (+ base 5) = lone consonant
		    (+ 231 vowel))))))	; 231 = We

     ;; 12-form consonant
     ((<= ch 235)
      (setq ch (- ch 152)		; 152 = 12-form consonant offset
	    base (* (/ ch 12) 12))
      (cond
       ((< (mod ch 12) 7)		; e-form <= ch <= o-form
	(setq newch (+ base vowel 152)))
       ((< vowel 5)			; We-form <= ch <= WE-form
	(setq newch (+ base vowel 159))) ; 159 = 152 (offset) + 7 (We-form)
       ((= vowel 5)			; 5 = ' (= u in this case)
	(setq newch (+ base 160)))	; 160 = 152 (offset) + 8 (Wu-form)
       (t
	(error "Not a valid vowel."))))

     ;; 7-form consonant
     (t					; 236 = 7-form consonant offset
      (setq newch (+ (* (/ (- ch 236) 7) 7) vowel 236))))

    (delete-char 1)

    (cond
     ((consp newch)
      (insert (ethiocode-to-char (car newch))
	      (ethiocode-to-char (cdr newch)))
      (backward-char 2))
     (t
      (insert (ethiocode-to-char newch))
      (backward-char 1)))))

(defun ethiocode-to-char (code)
  (make-char 'ethiopic (/ code 94) (mod code 94)))

(defun char-to-ethiocode (ch)
  (and (eq (char-charset ch) 'ethiopic)
       (let ((char-components (split-char ch)))
	 (+ (* (- (nth char-components 1) 161) 94)
	    (- (nth char-components 2) 161)))))

;;
;; space replacement
;;

;;;###autoload
(defun ethio-replace-space (ch begin end)
  "In the specified region, replace spaces between two Ethiopic characters."
  (interactive "*cReplace spaces to: 1 (sg col), 2 (dbl col), 3 (Ethiopic)\nr")
  (if (not (memq ch '(?1 ?2 ?3)))
      (error ""))
  (save-excursion
    (save-restriction
      (narrow-to-region begin end)
      (goto-char (point-min))

      (cond

       ((= ch ?1)

	;; A double column space or an Ethiopic word separator is always
	;; converted to an ASCII space.
	(while (re-search-forward "[$(2$N$O(B]" nil t)
	  (replace-match " " nil nil)))

       ((= ch ?2)

	;; An Ethiopic word separator is always converted to
	;; a double column space.
	(while (search-forward "$(2$O(B" nil t)
	  (replace-match "$(2$N(B"))

	(goto-char (point-min))

	;; ASCII spaces are converted only if they are placed
	;; between two Ethiopic characters.
	(while (re-search-forward "\\(\\cE\\)\\( \\)\\( *\\cE\\)" nil t)

	  ;; Converting the first ASCII space
	  (replace-match "\\1$(2$N(B\\3")

	  ;; A double column space is \cE, so going back to the just
	  ;; converted double column space makes it possible to find
	  ;; the following ASCII spaces.
	  (goto-char (match-beginning 2))))

       ((= ch ?3)

	;; If more than one consecutive space (either ASCII or double
	;; width) is found between two Ethiopic characters, the first
	;; space will be converted to an Ethiopic word separator.
	(let (pred succ)
	  (while (re-search-forward "[ $(2$N(B]\\([ $(2$N(B]*\\)" nil t)
	    (and (setq pred (char-before (match-beginning 0)))
		 (eq (char-charset pred) 'ethiopic)
		 (setq succ (char-after (match-end 0)))
		 (eq (char-charset succ) 'ethiopic)
		 (replace-match "$(2$O(B\\1" nil nil)))))))))

;;
;; special characters
;;

;;;###autoload
(defun ethio-input-special-character (arg)
  "Allow the user to input special characters."
  (interactive "*cInput number: 1.$(2$k(B  2.$(2$l(B  3.$(2$m(B  4.$(2$n(B")
  (cond
   ((= arg ?1)
    (insert ?$(2$k(B))
   ((= arg ?2)
    (insert ?$(2$l(B))
   ((= arg ?3)
    (insert ?$(2$m(B))
   ((= arg ?4)
    (insert ?$(2$n(B))
   (t
    (error ""))))

;;
(provide 'language/ethio-util)

;;; Local Variables:
;;; generated-autoload-file: "../loaddefs.el"
;;; End:
;;; ethio-util.el ends here
