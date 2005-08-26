;;; latin-pre.el --- Quail packages for inputting various European characters  -*-coding: iso-2022-7bit;-*-

;; Copyright (C) 1997 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 2000, 2002 Free Software Foundation, Inc.

;; Keywords: mule, multilingual, latin, input method

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Key translation maps were originally copied from iso-acc.el.
;; latin-1-prefix: extra special characters added, adapted from the vim
;;                 digraphs (from J.H.M.Dassen <jdassen@wi.leidenuniv.nl>)
;;                 by R.F. Smith <rsmith@xs4all.nl>
;;
;; polish-slash:
;; Author: W,B3(Bodek Bzyl <matwb@univ.gda.pl>
;; Maintainer: W,B3(Bodek Bzyl <matwb@univ.gda.pl>
;;
;; latin-[89]-prefix: Dave Love <fx@gnu.org>

;; You might make extra input sequences on the basis of the X
;; locale/*/Compose files (which have both prefix and postfix
;; sequences), but bear in mind that sequences which are logical in
;; that context may not be sensible when they're not signalled with
;; the Compose key.  An example is a double space for NBSP.

;;; Code:

(require 'quail)

(quail-define-package
 "latin-1-prefix" "Latin-1" "1>" t
 "Latin-1 characters input method with prefix modifiers

    effect   | prefix | examples
 ------------+--------+----------
    acute    |   '    | 'a -> ,Aa(B, '' -> ,A4(B
    grave    |   `    | `a -> ,A`(B
  circumflex |   ^    | ^a -> ,Ab(B
  diaeresis  |   \"    | \"a -> ,Ad(B  \"\" -> ,A((B
    tilde    |   ~    | ~a -> ,Ac(B
   cedilla   |   ~    | ~c -> ,Ag(B
    misc     | \" ~ /  | \"s -> ,A_(B  ~d -> ,Ap(B  ~t -> ,A~(B  /a -> ,Ae(B  /e -> ,Af(B  /o -> ,Ax(B
   symbol    |   ~    | ~> -> ,A;(B  ~< -> ,A+(B  ~! -> ,A!(B  ~? -> ,A?(B  ~~ -> ,A8(B
             |   ~    | ~s -> ,A'(B  ~x -> ,A$(B  ~. -> ,A7(B  ~$ -> ,A#(B  ~u -> ,A5(B
             |   ~    | ~p -> ,A6(B  ~- -> ,A-(B  ~= -> ,A/(B  ~| -> ,A&(B
   symbol    |  _ /   | _o -> ,A:(B  _a -> ,A*(B  // -> ,A0(B  /\\ -> ,AW(B  _y -> ,A%(B
             |  _ /   | _: -> ,Aw(B  /c -> ,A"(B  /2 -> ,A=(B  /4 -> ,A<(B  /3 -> ,A>(B
             |  _ /   | /= -> ,A,(B
   symbol    |   ^    | ^r -> ,A.(B  ^c -> ,A)(B  ^1 -> ,A9(B  ^2 -> ,A2(B  ^3 -> ,A3(B
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("'A" ?,AA(B)
 ("'E" ?,AI(B)
 ("'I" ?,AM(B)
 ("'O" ?,AS(B)
 ("'U" ?,AZ(B)
 ("'Y" ?,A](B)
 ("'a" ?,Aa(B)
 ("'e" ?,Ai(B)
 ("'i" ?,Am(B)
 ("'o" ?,As(B)
 ("'u" ?,Az(B)
 ("'y" ?,A}(B)
 ("''" ?,A4(B)
 ("' " ?')
 ("`A" ?,A@(B)
 ("`E" ?,AH(B)
 ("`I" ?,AL(B)
 ("`O" ?,AR(B)
 ("`U" ?,AY(B)
 ("`a" ?,A`(B)
 ("`e" ?,Ah(B)
 ("`i" ?,Al(B)
 ("`o" ?,Ar(B)
 ("`u" ?,Ay(B)
 ("``" ?`)
 ("` " ?`)
 ("^A" ?,AB(B)
 ("^E" ?,AJ(B)
 ("^I" ?,AN(B)
 ("^O" ?,AT(B)
 ("^U" ?,A[(B)
 ("^a" ?,Ab(B)
 ("^e" ?,Aj(B)
 ("^i" ?,An(B)
 ("^o" ?,At(B)
 ("^u" ?,A{(B)
 ("^^" ?^)
 ("^ " ?^)
 ("\"A" ?,AD(B)
 ("\"E" ?,AK(B)
 ("\"I" ?,AO(B)
 ("\"O" ?,AV(B)
 ("\"U" ?,A\(B)
 ("\"a" ?,Ad(B)
 ("\"e" ?,Ak(B)
 ("\"i" ?,Ao(B)
 ("\"o" ?,Av(B)
 ("\"s" ?,A_(B)
 ("\"u" ?,A|(B)
 ("\"y" ?,A(B)
 ("\"\"" ?,A((B)
 ("\" " ?\")
 ("~A" ?,AC(B)
 ("~C" ?,AG(B)
 ("~D" ?,AP(B)
 ("~N" ?,AQ(B)
 ("~O" ?,AU(B)
 ("~T" ?,A^(B)
 ("~a" ?,Ac(B)
 ("~c" ?,Ag(B)
 ("~d" ?,Ap(B)
 ("~n" ?,Aq(B)
 ("~o" ?,Au(B)
 ("~t" ?,A~(B)
 ("~>" ?\,A;(B)
 ("~<" ?\,A+(B)
 ("~!" ?,A!(B)
 ("~?" ?,A?(B)
 ("~~" ?,A8(B)
 ("~ " ?~)
 ("/A" ?,AE(B)
 ("/E" ?,AF(B)
 ("/O" ?,AX(B)
 ("/a" ?,Ae(B)
 ("/e" ?,Af(B)
 ("/o" ?,Ax(B)
 ("//" ?,A0(B)
 ("/ " ?/)
 ("_o" ?,A:(B)
 ("_a" ?,A*(B)
 ("_ " ?,A (B)
;; Symbols added by Roland Smith <rsmith@xs4all.nl>
 ("_+" ?,A1(B)
 ("_y" ?,A%(B)
 ("_:" ?,Aw(B)
 ("__" ?_)
 ("/c" ?,A"(B)
 ("/\\" ?,AW(B)
 ("/2" ?,A=(B)
 ("/4" ?,A<(B)
 ("/3" ?,A>(B)
 ("~s" ?,A'(B)
 ("~p" ?,A6(B)
 ("~x" ?,A$(B)
 ("~." ?,A7(B)
 ("~$" ?,A#(B)
 ("~u" ?,A5(B)
 ("^r" ?,A.(B)
 ("^c" ?,A)(B)
 ("^1" ?,A9(B)
 ("^2" ?,A2(B)
 ("^3" ?,A3(B)
 ("~-" ?,A-(B)
 ("~|" ?,A&(B)
 ("/=" ?,A,(B)
 ("~=" ?,A/(B)
)

(quail-define-package
 "catalan-prefix" "Latin-1" "CA>" t
 "Catalan and Spanish input method with prefix modifiers

    effect   | prefix | examples
 ------------+--------+----------
    acute    |   '    | 'a -> ,Aa(B   '' -> ,A4(B
    grave    |   `    | `a -> ,A`(B
  diaeresis  |   \"    | \"i -> ,Ao(B   \"\" -> ,A((B
    tilde    |   ~    | ~n -> ,Aq(B
   cedilla   |   ~    | ~c -> ,Ag(B
   symbol    |   ~    | ~> -> ,A;(B   ~< -> ,A+(B   ~! -> ,A!(B   ~? -> ,A?(B
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("'A" ?,AA(B)
 ("'E" ?,AI(B)
 ("'I" ?,AM(B)
 ("'O" ?,AS(B)
 ("'U" ?,AZ(B)
 ("'a" ?,Aa(B)
 ("'e" ?,Ai(B)
 ("'i" ?,Am(B)
 ("'o" ?,As(B)
 ("'u" ?,Az(B)
 ("' " ?')
 ("`A" ?,A@(B)
 ("`E" ?,AH(B)
 ("`O" ?,AR(B)
 ("`a" ?,A`(B)
 ("`e" ?,Ah(B)
 ("`o" ?,Ar(B)
 ("` " ?`)
 ("\"I" ?,AO(B)
 ("\"U" ?,A\(B)
 ("\"i" ?,Ao(B)
 ("\"u" ?,A|(B)
 ("\" " ?\")
 ("~C" ?,AG(B)
 ("~N" ?,AQ(B)
 ("~c" ?,Ag(B)
 ("~n" ?,Aq(B)
 ("~>" ?\,A;(B)
 ("~<" ?\,A+(B)
 ("~!" ?,A!(B)
 ("~?" ?,A?(B)
 ("~ " ?~)
)

(quail-define-package
 "esperanto-prefix" "Latin-3" "EO>" t
 "Esperanto input method with prefix modifiers
Key translation rules are:
 ^H -> ?,C&(B   ^J -> ?,C,(B   ^h -> ?,C6(B   ^j -> ?,C<(B   ^C -> ?,CF(B   ^G -> ?,CX(B,
 ^S -> ?,C^(B   ^c -> ?,Cf(B   ^g -> ?,Cx(B   ^s -> ?,C~(B   ~U -> ?,C](B   ~u -> ?,C}(B
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("^H" ?,C&(B)
 ("^J" ?,C,(B)
 ("^h" ?,C6(B)
 ("^j" ?,C<(B)
 ("^C" ?,CF(B)
 ("^G" ?,CX(B)
 ("^S" ?,C^(B)
 ("^c" ?,Cf(B)
 ("^g" ?,Cx(B)
 ("^s" ?,C~(B)
 ("^^" ?^)
 ("^ " ?^)
 ("~U" ?,C](B)
 ("~u" ?,C}(B)
 ("~ " ?~)
)

(quail-define-package
 "french-prefix" "French" "FR>" t
 "French (Fran,Ag(Bais) input method with prefix modifiers

    effect   | prefix | examples
 ------------+--------+----------
    acute    |   '    | 'e -> ,Ai(B
    grave    |   `    | `a -> ,A`(B
  circumflex |   ^    | ^a -> ,Ab(B
  diaeresis  |   \"    | \"i -> ,Ao(B
   cedilla   | ~ or , | ~c -> ,Ag(B   ,c -> ,Ag(B
   symbol    |   ~    | ~> -> ,A;(B   ~< -> ,A+(B
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("'E" ?,AI(B)
 ("'C" ?,AG(B)
 ("'e" ?,Ai(B)
 ("'c" ?,Ag(B)
 ("' " ?')
 ("`A" ?,A@(B)
 ("`E" ?,AH(B)
 ("`U" ?,AY(B)
 ("`a" ?,A`(B)
 ("`e" ?,Ah(B)
 ("`u" ?,Ay(B)
 ("` " ?`)
 ("^A" ?,AB(B)
 ("^E" ?,AJ(B)
 ("^I" ?,AN(B)
 ("^O" ?,AT(B)
 ("^U" ?,A[(B)
 ("^a" ?,Ab(B)
 ("^e" ?,Aj(B)
 ("^i" ?,An(B)
 ("^o" ?,At(B)
 ("^u" ?,A{(B)
 ("^ " ?^)
 ("\"E" ?,AK(B)
 ("\"I" ?,AO(B)
 ("\"e" ?,Ak(B)
 ("\"i" ?,Ao(B)
 ("\" " ?\")
 ("~<" ?\,A+(B)
 ("~>" ?\,A;(B)
 ("~C" ?,AG(B)
 ("~c" ?,Ag(B)
 ("~ " ?~)
 (",C" ?,AG(B)
 (",c" ?,Ag(B)
 (", " ?,)
)

(quail-define-package
 "romanian-prefix" "Romanian" "RO>" t
 "Romanian (rom,Bb(Bne,B:(Bte) input method with prefix modifiers

    effect   | prefix | examples
 ------------+--------+------------------
    tilde    |   ~    | ~a -> ,Bc(B
  circumflex |   ^    | ^a -> ,Bb(B, ^i -> ,Bn(B
   cedilla   |   ,    | ,s -> ,B:(B, ,t -> ,B~(B
   ~         |   ~    | ~~ -> ~
   ^         |   ^    | ^^ -> ^
   ,         |   ,    | ,, -> ,
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("~A" ?,BC(B) ("~a" ?,Bc(B)
 ("^A" ?,BB(B) ("^a" ?,Bb(B)
 ("^I" ?,BN(B) ("^i" ?,Bn(B)
 (",S" ?,B*(B) (",s" ?,B:(B)
 (",T" ?,B^(B) (",t" ?,B~(B)
 ("^^" ?^) ("~~" ?~) (",," ?,))

(quail-define-package
 "romanian-alt-prefix" "Romanian" "RO>" t
 "Alternative Romanian (rom,Bb(Bne,B:(Bte) input method with prefix modifiers

    effect   | prefix | examples
 ------------+--------+------------------
    tilde    |   \"    | \"a -> ,Bb(B
  circumflex |   '    | 'a -> ,Bb(B, 'i -> ,Bn(B
   cedilla   |   '    | 's -> ,B:(B, 't -> ,B~(B
   '         |   '    | '' -> '
   \"         |   \"    | \"\" -> \"
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("'A" ?,BC(B) ("'a" ?,Bc(B)
 ("\"A" ?,BB(B) ("\"a" ?,Bb(B)
 ("'I" ?,BN(B) ("'i" ?,Bn(B)
 ("'S" ?,B*(B) ("'s" ?,B:(B)
 ("'T" ?,B^(B) ("'t" ?,B~(B)
 ("''" ?') ("\"\"" ?\"))

(quail-define-package
 "german-prefix" "German" "DE>" t
 "German (Deutsch) input method with prefix modifiers
Key translation rules are:
 \"A -> ,AD(B ->   \"O -> ,AV(B   \"U -> ,A\(B   \"s -> ,A_(B
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("\"A" ?,AD(B)
 ("\"O" ?,AV(B)
 ("\"U" ?,A\(B)
 ("\"a" ?,Ad(B)
 ("\"o" ?,Av(B)
 ("\"u" ?,A|(B)
 ("\"s" ?,A_(B)
 ("\" " ?\")
)

(quail-define-package
 "irish-prefix" "Latin-1" "GA>" t
 "Irish input method with prefix modifiers
Key translation rules are:
 'A -> ,AA(B   'E -> ,AI(B   'I -> ,AM(B   'O -> ,AS(B   'U -> ,AZ(B
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("'A" ?,AA(B)
 ("'E" ?,AI(B)
 ("'I" ?,AM(B)
 ("'O" ?,AS(B)
 ("'U" ?,AZ(B)
 ("'a" ?,Aa(B)
 ("'e" ?,Ai(B)
 ("'i" ?,Am(B)
 ("'o" ?,As(B)
 ("'u" ?,Az(B)
 ("' " ?')
)

(quail-define-package
 "portuguese-prefix" "Latin-1" "PT>" t
 "Portuguese input method with prefix modifiers

    effect   | prefix | examples
 ------------+--------+----------
    acute    |   '    | 'a -> ,Aa(B   '' -> ,A4(B
    grave    |   `    | `a -> ,A`(B
  circumflex |   ^    | ^a -> ,Ab(B
  diaeresis  |   \"    | \"u -> ,A|(B
    tilde    |   ~    | ~a -> ,Ac(B
   cedilla   | ' or , | 'c -> ,Ag(B   ,c -> ,Ag(B
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("'A" ?,AA(B)
 ("'E" ?,AI(B)
 ("'I" ?,AM(B)
 ("'O" ?,AS(B)
 ("'U" ?,AZ(B)
 ("'C" ?,AG(B)
 ("'a" ?,Aa(B)
 ("'e" ?,Ai(B)
 ("'i" ?,Am(B)
 ("'o" ?,As(B)
 ("'u" ?,Az(B)
 ("'c" ?,Ag(B)
 ("' " ?')
 ("`A" ?,A@(B)
 ("`a" ?,A`(B)
 ("` " ?`)
 ("^A" ?,AB(B)
 ("^E" ?,AJ(B)
 ("^O" ?,AT(B)
 ("^a" ?,Ab(B)
 ("^e" ?,Aj(B)
 ("^o" ?,At(B)
 ("^ " ?^)
 ("\"U" ?,A\(B)
 ("\"u" ?,A|(B)
 ("\" " ?\")
 ("~A" ?,AC(B)
 ("~O" ?,AU(B)
 ("~a" ?,Ac(B)
 ("~o" ?,Au(B)
 ("~ " ?~)
 (",c" ?,Ag(B)
 (",C" ?,AG(B)
 (",," ?,)
)

(quail-define-package
 "spanish-prefix" "Spanish" "ES>" t
 "Spanish (Espa,Aq(Bol) input method with prefix modifiers

    effect   | prefix | examples
 ------------+--------+----------
    acute    |   '    | 'a -> ,Aa(B
  diaeresis  |   \"    | \"u -> ,A|(B
    tilde    |   ~    | ~n -> ,Aq(B
   symbol    |   ~    | ~> -> ,A;(B   ~< -> ,A+(B   ~! -> ,A!(B   ~? -> ,A?(B
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("'A" ?,AA(B)
 ("'E" ?,AI(B)
 ("'I" ?,AM(B)
 ("'O" ?,AS(B)
 ("'U" ?,AZ(B)
 ("'a" ?,Aa(B)
 ("'e" ?,Ai(B)
 ("'i" ?,Am(B)
 ("'o" ?,As(B)
 ("'u" ?,Az(B)
 ("' " ?')
 ("\"U" ?,A\(B)
 ("\"u" ?,A|(B)
 ("\" " ?\")
 ("~N" ?,AQ(B)
 ("~n" ?,Aq(B)
 ("~>" ?\,A;(B)
 ("~<" ?\,A+(B)
 ("~!" ?,A!(B)
 ("~?" ?,A?(B)
 ("~ " ?~)
)

(quail-define-package
 "latin-2-prefix" "Latin-2" "2>" t
 "Latin-2 characters input method with prefix modifiers

    effect   | prefix | examples
 ------------+--------+----------
    acute    |   '    | 'a -> ,Ba(B   '' -> ?,B4(B
  circumflex |   ^    | ^a -> ,Bb(B
  diaeresis  |   \"    | \"a -> ,Bd(B   \"\" -> ,B((B
    breve    |   ~    | ~a -> ,Bc(B
    caron    |   ~    | ~c -> ,Bh(B
   cedilla   |   `    | `c -> ,Bg(B   `e -> ?,Bj(B
    misc     | ' ` ~  | 'd -> ,Bp(B   `l -> ,B3(B   `z -> ,B?(B   ~o -> ,Bu(B   ~u -> ,B{(B
   symbol    |   ~    | `. -> ,B(B   ~~ -> ,B"(B   ~. -> ?,B8(B
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("'A" ?,BA(B)
 ("'C" ?,BF(B)
 ("'D" ?,BP(B)
 ("'E" ?,BI(B)
 ("'I" ?,BM(B)
 ("'L" ?,BE(B)
 ("'N" ?,BQ(B)
 ("'O" ?,BS(B)
 ("'R" ?,B@(B)
 ("'S" ?,B&(B)
 ("'U" ?,BZ(B)
 ("'Y" ?,B](B)
 ("'Z" ?,B,(B)
 ("'a" ?,Ba(B)
 ("'c" ?,Bf(B)
 ("'d" ?,Bp(B)
 ("'e" ?,Bi(B)
 ("'i" ?,Bm(B)
 ("'l" ?,Be(B)
 ("'n" ?,Bq(B)
 ("'o" ?,Bs(B)
 ("'r" ?,B`(B)
 ("'s" ?,B6(B)
 ("'u" ?,Bz(B)
 ("'y" ?,B}(B)
 ("'z" ?,B<(B)
 ("''" ?,B4(B)
 ("' " ?')
 ("`A" ?,B!(B)
 ("`C" ?,BG(B)
 ("`E" ?,BJ(B)
 ("`L" ?,B#(B)
 ("`S" ?,B*(B)
 ("`T" ?,B^(B)
 ("`Z" ?,B/(B)
 ("`a" ?,B1(B)
 ("`l" ?,B3(B)
 ("`c" ?,Bg(B)
 ("`e" ?,Bj(B)
 ("`s" ?,B:(B)
 ("`t" ?,B~(B)
 ("`z" ?,B?(B)
 ("``" ?,B*(B)
 ("`." ?,B(B)
 ("` " ?`)
 ("^A" ?,BB(B)
 ("^I" ?,BN(B)
 ("^O" ?,BT(B)
 ("^a" ?,Bb(B)
 ("^i" ?,Bn(B)
 ("^o" ?,Bt(B)
 ("^^" ?^)
 ("^ " ?^)
 ("\"A" ?,BD(B)
 ("\"E" ?,BK(B)
 ("\"O" ?,BV(B)
 ("\"U" ?,B\(B)
 ("\"a" ?,Bd(B)
 ("\"e" ?,Bk(B)
 ("\"o" ?,Bv(B)
 ("\"s" ?,B_(B)
 ("\"u" ?,B|(B)
 ("\"\"" ?,B((B)
 ("\" " ?\")
 ("~A" ?,BC(B)
 ("~C" ?,BH(B)
 ("~D" ?,BO(B)
 ("~E" ?,BL(B)
 ("~L" ?,B%(B)
 ("~N" ?,BR(B)
 ("~O" ?,BU(B)
 ("~R" ?,BX(B)
 ("~S" ?,B)(B)
 ("~T" ?,B+(B)
 ("~U" ?,B[(B)
 ("~Z" ?,B.(B)
 ("~a" ?,Bc(B)
 ("~c" ?,Bh(B)
 ("~d" ?,Bo(B)
 ("~e" ?,Bl(B)
 ("~l" ?,B5(B)
 ("~n" ?,Br(B)
 ("~o" ?,Bu(B)
 ("~r" ?,Bx(B)
 ("~s" ?,B9(B)
 ("~t" ?,B;(B)
 ("~u" ?,B{(B)
 ("~z" ?,B>(B)
 ("~v" ?,B"(B)
 ("~~" ?,B"(B)
 ("~." ?,B8(B)
 ("~ " ?~)
)

(quail-define-package
 "latin-3-prefix" "Latin-3" "3>" t
 "Latin-3 characters input method with prefix modifiers

    effect   | prefix | examples
 ------------+--------+----------
    acute    |   '    | 'a -> ,Ca(B   '' -> ?,C4(B
    grave    |   `    | `a -> ,C`(B
  circumflex |   ^    | ^a -> ,Cb(B
  diaeresis  |   \"    | \"a -> ,Cd(B   \"\" -> ,C((B
   cedilla   |   ~    | ~c -> ,Cg(B   ~s -> ,C:(B   ~~ -> ,C8(B
  dot above  |   / .  | /g -> ,Cu(B   .o -> ,Cu(B
    misc     | \" ~ /  | \"s -> ,C_(B   ~g -> ,C;(B   ~u -> ,C}(B   /h -> ,C1(B   /i -> ,C9(B
   symbol    |   ~    | ~` -> ,C"(B   /# -> ,C#(B   /$ -> ,C$(B   // -> ,C0(B
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("'A" ?,CA(B)
 ("'E" ?,CI(B)
 ("'I" ?,CM(B)
 ("'O" ?,CS(B)
 ("'U" ?,CZ(B)
 ("'a" ?,Ca(B)
 ("'e" ?,Ci(B)
 ("'i" ?,Cm(B)
 ("'o" ?,Cs(B)
 ("'u" ?,Cz(B)
 ("''" ?,C4(B)
 ("' " ?')
 ("`A" ?,C@(B)
 ("`E" ?,CH(B)
 ("`I" ?,CL(B)
 ("`O" ?,CR(B)
 ("`U" ?,CY(B)
 ("`a" ?,C`(B)
 ("`e" ?,Ch(B)
 ("`i" ?,Cl(B)
 ("`o" ?,Cr(B)
 ("`u" ?,Cy(B)
 ("``" ?`)
 ("` " ?`)
 ("^A" ?,CB(B)
 ("^C" ?,CF(B)
 ("^E" ?,CJ(B)
 ("^G" ?,CX(B)
 ("^H" ?,C&(B)
 ("^I" ?,CN(B)
 ("^J" ?,C,(B)
 ("^O" ?,CT(B)
 ("^S" ?,C^(B)
 ("^U" ?,C[(B)
 ("^a" ?,Cb(B)
 ("^c" ?,Cf(B)
 ("^e" ?,Cj(B)
 ("^g" ?,Cx(B)
 ("^h" ?,C6(B)
 ("^i" ?,Cn(B)
 ("^j" ?,C<(B)
 ("^o" ?,Ct(B)
 ("^s" ?,C~(B)
 ("^u" ?,C{(B)
 ("^^" ?^)
 ("^ " ?^)
 ("\"A" ?,CD(B)
 ("\"E" ?,CK(B)
 ("\"I" ?,CO(B)
 ("\"O" ?,CV(B)
 ("\"U" ?,C\(B)
 ("\"a" ?,Cd(B)
 ("\"e" ?,Ck(B)
 ("\"i" ?,Co(B)
 ("\"o" ?,Cv(B)
 ("\"u" ?,C|(B)
 ("\"s" ?,C_(B)
 ("\"\"" ?,C((B)
 ("\" " ?\")
 ("~C" ?,CG(B)
 ("~N" ?,CQ(B)
 ("~c" ?,Cg(B)
 ("~n" ?,Cq(B)
 ("~S" ?,C*(B)
 ("~s" ?,C:(B)
 ("~G" ?,C+(B)
 ("~g" ?,C;(B)
 ("~U" ?,C](B)
 ("~u" ?,C}(B)
 ("~`" ?,C"(B)
 ("~~" ?,C8(B)
 ("~ " ?~)
 ("/C" ?,CE(B)
 ("/G" ?,CU(B)
 ("/H" ?,C!(B)
 ("/I" ?,C)(B)
 ("/Z" ?,C/(B)
 ("/c" ?,Ce(B)
 ("/g" ?,Cu(B)
 ("/h" ?,C1(B)
 ("/i" ?,C9(B)
 ("/z" ?,C?(B)
 ("/." ?,C(B)
 ("/#" ?,C#(B)
 ("/$" ?,C$(B)
 ("//" ?,C0(B)
 ("/ " ?/)
 (".C" ?,CE(B)
 (".G" ?,CU(B)
 (".I" ?,C)(B)
 (".Z" ?,C/(B)
 (".c" ?,Ce(B)
 (".g" ?,Cu(B)
 (".z" ?,C?(B)
)


(quail-define-package
 "polish-slash" "Polish" "PL>" nil
 "Polish diacritics and slash character are input as `/[acelnosxzACELNOSXZ/]'.
For example, the character named `aogonek' is obtained by `/a'."
 nil t t t nil nil nil nil nil nil t)

(quail-define-rules
 ("//" ?/)
 ("/a" ?,B1(B)
 ("/c" ?,Bf(B)
 ("/e" ?,Bj(B)
 ("/l" ?,B3(B)
 ("/n" ?,Bq(B)
 ("/o" ?,Bs(B)
 ("/s" ?,B6(B)
 ("/x" ?,B<(B)
 ("/z" ?,B?(B)
 ("/A" ?,B!(B)
 ("/C" ?,BF(B)
 ("/E" ?,BJ(B)
 ("/L" ?,B#(B)
 ("/N" ?,BQ(B)
 ("/O" ?,BS(B)
 ("/S" ?,B&(B)
 ("/X" ?,B,(B)
 ("/Z" ?,B/(B))

(quail-define-package
 "latin-9-prefix" "Latin-9" "0>" t
 "Latin-9 characters input method with prefix modifiers

    effect   | prefix | examples
 ------------+--------+----------
    acute    |   '    | 'a -> ,ba(B
    grave    |   `    | `a -> ,b`(B
  circumflex |   ^    | ^a -> ,bb(B
  diaeresis  |   \"    | \"a -> ,bd(B, \"Y -> ,b>(B
    tilde    |   ~    | ~a -> ,bc(B
    caron    |   ~    | ~z -> ,b8(B
   cedilla   |   ~    | ~c -> ,bg(B
    misc     | \" ~ /  | \"s -> ,b_(B  ~d -> ,bp(B  ~t -> ,b~(B  /a -> ,be(B  /e -> ,bf(B  /o -> ,bx(B
             | \" ~ /  | /o -> ,b=(B
   symbol    |   ~    | ~> -> ,b;(B  ~< -> ,b+(B  ~! -> ,b!(B  ~? -> ,b?(B  ~~ -> ,b8(B
             |   ~    | ~s -> ,b'(B  ~e -> ,b$(B  ~. -> ,b7(B  ~$ -> ,b#(B  ~u -> ,b5(B
             |   ~    | ~- -> ,b-(B  ~= -> ,b/(B
   symbol    |  _ /   | _o -> ,b:(B  _a -> ,b*(B  // -> ,b0(B  /\\ -> ,bW(B  _y -> ,b%(B
             |  _ /   | _: -> ,bw(B  /c -> ,b"(B  ~p -> ,b6(B
             |  _ /   | /= -> ,b,(B
   symbol    |   ^    | ^r -> ,b.(B  ^c -> ,b)(B  ^1 -> ,b9(B  ^2 -> ,b2(B  ^3 -> ,b3(B  _a -> ,b*(B
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("'A" ?,bA(B)
 ("'E" ?,bI(B)
 ("'I" ?,bM(B)
 ("'O" ?,bS(B)
 ("'U" ?,bZ(B)
 ("'Y" ?,b](B)
 ("'a" ?,ba(B)
 ("'e" ?,bi(B)
 ("'i" ?,bm(B)
 ("'o" ?,bs(B)
 ("'u" ?,bz(B)
 ("'y" ?,b}(B)
 ("' " ?')
 ("`A" ?,b@(B)
 ("`E" ?,bH(B)
 ("`I" ?,bL(B)
 ("`O" ?,bR(B)
 ("`U" ?,bY(B)
 ("`a" ?,b`(B)
 ("`e" ?,bh(B)
 ("`i" ?,bl(B)
 ("`o" ?,br(B)
 ("`u" ?,by(B)
 ("``" ?`)
 ("` " ?`)
 ("^A" ?,bB(B)
 ("^E" ?,bJ(B)
 ("^I" ?,bN(B)
 ("^O" ?,bT(B)
 ("^U" ?,b[(B)
 ("^a" ?,bb(B)
 ("^e" ?,bj(B)
 ("^i" ?,bn(B)
 ("^o" ?,bt(B)
 ("^u" ?,b{(B)
 ("^^" ?^)
 ("^ " ?^)
 ("\"A" ?,bD(B)
 ("\"E" ?,bK(B)
 ("\"I" ?,bO(B)
 ("\"O" ?,bV(B)
 ("\"U" ?,b\(B)
 ("\"a" ?,bd(B)
 ("\"e" ?,bk(B)
 ("\"i" ?,bo(B)
 ("\"o" ?,bv(B)
 ("\"s" ?,b_(B)
 ("\"u" ?,b|(B)
 ("\"y" ?,b(B)
 ("\" " ?\")
 ("~A" ?,bC(B)
 ("~C" ?,bG(B)
 ("~D" ?,bP(B)
 ("~N" ?,bQ(B)
 ("~O" ?,bU(B)
 ("~S" ?,b&(B)
 ("~T" ?,b^(B)
 ("~Z" ?,b4(B)
 ("~a" ?,bc(B)
 ("~c" ?,bg(B)
 ("~d" ?,bp(B)
 ("~n" ?,bq(B)
 ("~o" ?,bu(B)
 ("~s" ?,b((B)
 ("~t" ?,b~(B)
 ("~z" ?,b8(B)
 ("~>" ?\,b;(B)
 ("~<" ?\,b+(B)
 ("~!" ?,b!(B)
 ("~?" ?,b?(B)
 ("~ " ?~)
 ("/A" ?,bE(B)
 ("/E" ?,bF(B)
 ("/O" ?,bX(B)
 ("/a" ?,be(B)
 ("/e" ?,bf(B)
 ("/o" ?,bx(B)
 ("//" ?,b0(B)
 ("/ " ?/)
 ("_o" ?,b:(B)
 ("_a" ?,b*(B)
 ("_+" ?,b1(B)
 ("_y" ?,b%(B)
 ("_:" ?,bw(B)
 ("_ " ?,b (B)
 ("__" ?_)
 ("/c" ?,b"(B)
 ("/\\" ?,bW(B)
 ("/o" ?,b=(B)				; clash with ,bx(B, but ,bf(B uses /
 ("/O" ?,b<(B)
 ("\"Y" ?,b>(B)
 ("~s" ?,b'(B)
 ("~p" ?,b6(B)
 ;; Is this the best option for Euro entry?
 ("~e" ?,b$(B)
 ("~." ?,b7(B)
 ("~$" ?,b#(B)
 ("~u" ?,b5(B)
 ("^r" ?,b.(B)
 ("^c" ?,b)(B)
 ("^1" ?,b9(B)
 ("^2" ?,b2(B)
 ("^3" ?,b3(B)
 ("~-" ?,b-(B)
 ("~=" ?,b/(B)
 ("/=" ?,b,(B))

;; Latin-8 was done by an Englishman -- Johnny Celt should take a
;; squint at it.

(quail-define-package
 "latin-8-prefix" "Latin-8" "8>" t
 "Latin-8 characters input method with prefix modifiers

    effect   | prefix | examples
 ------------+--------+----------
    acute    |   '    | 'a -> ,_a(B
    grave    |   `    | `a -> ,_`(B
  circumflex |   ^    | ^w -> ,_p(B
  diaeresis  |   \"    | \"a -> ,_d(B
  dot above  |   .    | .b -> ,_"(B
    tilde    |   ~    | ~a -> ,_c(B
   cedilla   |   ~    | ~c -> ,_g(B
    misc     | \" ~ /  | \"s -> ,__(B   /a -> ,_e(B  /e -> ,_f(B  /o -> ,_x(B
             |   ~    | ~s -> ,_'(B  ~$ -> ,_#(B  ~p -> ,_6(B
   symbol    |   ^    | ^r -> ,_.(B  ^c -> ,_)(B
" nil t nil nil nil nil nil nil nil nil t)

;; Basically following Latin-1, plus dottiness from Latin-3.
(quail-define-rules
 (".B" ?,_!(B)
 (".b" ?,_"(B)
 (".c" ?,_%(B)
 (".C" ?,_$(B)
 (".D" ?,_&(B)
 (".d" ?,_+(B)
 (".f" ?,_1(B)
 (".F" ?,_0(B)
 (".g" ?,_3(B)
 (".G" ?,_2(B)
 (".m" ?,_5(B)
 (".M" ?,_4(B)
 (".p" ?,_9(B)
 (".P" ?,_7(B)
 (".s" ?,_?(B)
 (".S" ?,_;(B)
 (".t" ?,_w(B)
 (".T" ?,_W(B)
 ("'A" ?,_A(B)
 ("'E" ?,_I(B)
 ("'I" ?,_M(B)
 ("'O" ?,_S(B)
 ("'U" ?,_Z(B)
 ("'Y" ?,_](B)
 ("'W" ?,_*(B)
 ("'a" ?,_a(B)
 ("'e" ?,_i(B)
 ("'i" ?,_m(B)
 ("'o" ?,_s(B)
 ("'u" ?,_z(B)
 ("'w" ?,_:(B)
 ("'y" ?,_}(B)
 ("' " ?')
 ("`A" ?,_@(B)
 ("`E" ?,_H(B)
 ("`I" ?,_L(B)
 ("`O" ?,_R(B)
 ("`U" ?,_Y(B)
 ("`W" ?,_((B)
 ("`Y" ?,_,(B)
 ("`a" ?,_`(B)
 ("`e" ?,_h(B)
 ("`i" ?,_l(B)
 ("`o" ?,_r(B)
 ("`u" ?,_y(B)
 ("`w" ?,_8(B)
 ("`y" ?,_<(B)
 ("``" ?`)
 ("` " ?`)
 ("^A" ?,_B(B)
 ("^E" ?,_J(B)
 ("^I" ?,_N(B)
 ("^O" ?,_T(B)
 ("^U" ?,_[(B)
 ("^a" ?,_b(B)
 ("^e" ?,_j(B)
 ("^i" ?,_n(B)
 ("^o" ?,_t(B)
 ("^u" ?,_{(B)
 ("^w" ?,_p(B)
 ("^W" ?,_P(B)
 ("^y" ?,_~(B)
 ("^Y" ?,_^(B)
 ("^^" ?^)
 ("^ " ?^)
 ("\"A" ?,_D(B)
 ("\"E" ?,_K(B)
 ("\"I" ?,_O(B)
 ("\"O" ?,_V(B)
 ("\"U" ?,_\(B)
 ("\"a" ?,_d(B)
 ("\"e" ?,_k(B)
 ("\"i" ?,_o(B)
 ("\"o" ?,_v(B)
 ("\"s" ?,__(B)
 ("\"u" ?,_|(B)
 ("\"w" ?,_>(B)
 ("\"W" ?,_=(B)
 ("\"y" ?,_(B)
 ("\"Y" ?,_/(B)
 ("\" " ?\")
 ("~A" ?,_C(B)
 ("~C" ?,_G(B)
 ("~N" ?,_Q(B)
 ("~O" ?,_U(B)
 ("~a" ?,_c(B)
 ("~c" ?,_g(B)
 ("~n" ?,_q(B)
 ("~o" ?,_u(B)
 ("~ " ?~)
 ("/A" ?,_E(B)
 ("/E" ?,_F(B)
 ("/O" ?,_X(B)
 ("/a" ?,_e(B)
 ("/e" ?,_f(B)
 ("/o" ?,_x(B)
 ("/ " ?/)
 ("~p" ?,_6(B)
 ("~s" ?,_'(B)
 ("~$" ?,_#(B)
 ("^r" ?,_.(B)
 ("^c" ?,_)(B))

(quail-define-package
 "latin-prefix" "Latin" "L>" t
 "Latin characters input method with prefix modifiers.
This is the union of various input methods originally made for input
of characters from a single Latin-N charset.

    effect   | prefix | examples
 ------------+--------+----------
    acute    |   '    | 'a -> ,Aa(B, '' -> ,A4(B
    grave    |   `    | `a -> ,A`(B
  circumflex |   ^    | ^a -> ,Ab(B
  diaeresis  |   \"    | \"a -> ,Ad(B  \"\" -> ,A((B
    tilde    |   ~    | ~a -> ,Ac(B
   cedilla   |   ~    | ~c -> ,Ag(B
    breve    |   ~    | ~a -> $,1 #(B
    caron    |   ~    | ~c -> $,1 -(B
  dot above  | ~ / .  | ~o -> $,1 A(B   /o -> $,1 A(B   .o -> $,1 A(B
    misc     | \" ~ /  | \"s -> ,A_(B  ~d -> ,Ap(B  ~t -> ,A~(B  /a -> ,Ae(B  /e -> ,Af(B  /o -> ,Ax(B
   symbol    |   ~    | ~> -> ,A;(B  ~< -> ,A+(B  ~! -> ,A!(B  ~? -> ,A?(B  ~~ -> ,A8(B
   symbol    |  _ /   | _o -> ,A:(B  _a -> ,A*(B  // -> ,A0(B  /\\ -> ,AW(B  _y -> ,A%(B
   symbol    |   ^    | ^r -> ,A.(B  ^c -> ,A)(B  ^1 -> ,A9(B  ^2 -> ,A2(B  ^3 -> ,A3(B
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("' " ?')
 ("''" ?,A4(B)
 ("'A" ?,AA(B)
 ("'E" ?,AI(B)
 ("'I" ?,AM(B)
 ("'O" ?,AS(B)
 ("'U" ?,AZ(B)
 ("'W" ?$,1nb(B)
 ("'Y" ?,A](B)
 ("'a" ?,Aa(B)
 ("'e" ?,Ai(B)
 ("'i" ?,Am(B)
 ("'o" ?,As(B)
 ("'u" ?,Az(B)
 ("'w" ?$,1nc(B)
 ("'y" ?,A}(B)
 (".B" ?$,1mB(B)
 (".C" ?$,1 *(B)
 (".D" ?$,1mJ(B)
 (".F" ?$,1m^(B)
 (".G" ?$,1 @(B)
 (".I" ?$,1 P(B)
 (".M" ?$,1n (B)
 (".P" ?$,1n6(B)
 (".S" ?$,1n@(B)
 (".T" ?$,1nJ(B)
 (".Z" ?$,1!;(B)
 (".b" ?$,1mC(B)
 (".c" ?$,1 +(B)
 (".d" ?$,1mK(B)
 (".f" ?$,1m_(B)
 (".g" ?$,1 A(B)
 (".m" ?$,1n!(B)
 (".p" ?$,1n7(B)
 (".s" ?$,1nA(B)
 (".t" ?$,1nK(B)
 (".z" ?$,1!<(B)
 ("/ " ?/)
 ("/#" ?,A#(B)
 ("/$" ?,A$(B)
 ("/." ?$,1$y(B)
 ("//" ?,A0(B)
 ("/2" ?,A=(B)
 ("/3" ?,A>(B)
 ("/4" ?,A<(B)
 ("/=" ?,A,(B)
 ("/A" ?,AE(B)
 ("/C" ?$,1 *(B)
 ("/E" ?,AF(B)
 ("/G" ?$,1 @(B)
 ("/H" ?$,1 F(B)
 ("/I" ?$,1 P(B)
 ("/O" ?,AX(B)
 ("/O" ?$,1 r(B)
 ("/Z" ?$,1!;(B)
 ("/\\" ?,AW(B)
 ("/a" ?,Ae(B)
 ("/c" ?,A"(B)
 ("/c" ?$,1 +(B)
 ("/e" ?,Af(B)
 ("/g" ?$,1 A(B)
 ("/h" ?$,1 G(B)
 ("/i" ?$,1 Q(B)
 ("/o" ?,Ax(B)
 ("/o" ?$,1 s(B)
 ("/z" ?$,1!<(B)
 ("\" " ?\")
 ("\"A" ?,AD(B)
 ("\"E" ?,AK(B)
 ("\"I" ?,AO(B)
 ("\"O" ?,AV(B)
 ("\"U" ?,A\(B)
 ("\"W" ?$,1nd(B)
 ("\"Y" ?$,1!8(B)
 ("\"\"" ?,A((B)
 ("\"a" ?,Ad(B)
 ("\"e" ?,Ak(B)
 ("\"i" ?,Ao(B)
 ("\"o" ?,Av(B)
 ("\"s" ?,A_(B)
 ("\"u" ?,A|(B)
 ("\"w" ?$,1ne(B)
 ("\"y" ?,A(B)
 ("^ " ?^)
 ("^1" ?,A9(B)
 ("^2" ?,A2(B)
 ("^3" ?,A3(B)
 ("^A" ?,AB(B)
 ("^C" ?$,1 ((B)
 ("^E" ?,AJ(B)
 ("^G" ?$,1 <(B)
 ("^H" ?$,1 D(B)
 ("^I" ?,AN(B)
 ("^J" ?$,1 T(B)
 ("^O" ?,AT(B)
 ("^S" ?$,1 |(B)
 ("^U" ?,A[(B)
 ("^W" ?$,1!4(B)
 ("^Y" ?$,1!6(B)
 ("^^" ?^)
 ("^a" ?,Ab(B)
 ("^c" ?,A)(B)
 ("^c" ?$,1 )(B)
 ("^e" ?,Aj(B)
 ("^g" ?$,1 =(B)
 ("^h" ?$,1 E(B)
 ("^i" ?,An(B)
 ("^j" ?$,1 U(B)
 ("^o" ?,At(B)
 ("^r" ?,A.(B)
 ("^s" ?$,1 }(B)
 ("^u" ?,A{(B)
 ("^w" ?$,1!5(B)
 ("^y" ?$,1!7(B)
 ("_+" ?,A1(B)
 ("_:" ?,Aw(B)
 ("_a" ?,A*(B)
 ("_o" ?,A:(B)
 ("_y" ?,A%(B)
 ("_ " ?,A (B)
 ("` " ?`)
 ("`A" ?,A@(B)
 ("`E" ?,AH(B)
 ("`I" ?,AL(B)
 ("`O" ?,AR(B)
 ("`U" ?,AY(B)
 ("`W" ?$,1n`(B)
 ("`Y" ?$,1or(B)
 ("``" ?`)
 ("`a" ?,A`(B)
 ("`e" ?,Ah(B)
 ("`i" ?,Al(B)
 ("`o" ?,Ar(B)
 ("`u" ?,Ay(B)
 ("`w" ?$,1na(B)
 ("`y" ?$,1os(B)
 ("~ " ?~)
 ("~!" ?,A!(B)
 ("~$" ?,A#(B)
 ("~-" ?,A-(B)
 ("~." ?,A7(B)
 ("~<" ?\,A+(B)
 ("~=" ?,A/(B)
 ("~>" ?\,A;(B)
 ("~?" ?,A?(B)
 ("~A" ?,AC(B)
 ("~C" ?,AG(B)
 ("~D" ?,AP(B)
 ("~G" ?$,1 >(B)
 ("~N" ?,AQ(B)
 ("~O" ?,AU(B)
 ("~O" ?$,1 @(B)
 ("~S" ?$,1 ~(B)
 ("~S" ?$,1! (B)
 ("~T" ?,A^(B)
 ("~U" ?$,1!,(B)
 ("~Z" ?$,1!=(B)
 ("~`" ?$,1$x(B)
 ("~a" ?,Ac(B)
 ("~c" ?,Ag(B)
 ("~d" ?,Ap(B)
 ("~e" ?$,1tL(B)
 ("~g" ?$,1 ?(B)
 ("~n" ?,Aq(B)
 ("~o" ?,Au(B)
 ("~o" ?$,1 A(B)
 ("~p" ?,A6(B)
 ("~s" ?,A'(B)
 ("~s" ?$,1 (B)
 ("~s" ?$,1!!(B)
 ("~t" ?,A~(B)
 ("~u" ?,A5(B)
 ("~u" ?$,1!-(B)
 ("~x" ?,A$(B)
 ("~z" ?$,1!>(B)
 ("~|" ?,A&(B)
 ("~~" ?,A8(B)
)

;;; arch-tag: 83017837-6b84-4366-b183-e0577e3ed838
;;; latin-pre.el ends here
