;;; latin-pre.el -- Quail packages for inputting various European characters.

;; Copyright (C) 1997 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; All key translation maps are copied from iso-acc.el.

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
  diaeresis  |   \"    | \"a -> ,Ad(B   \"\" -> ,A((B
    tilde    |   ~    | ~a -> ,Ac(B
   cedilla   |   ~    | ~c -> ,Ag(B
    misc     | \" ~ /  | \"s -> ,A_(B   ~d -> ,Ap(B   ~t -> ,A~(B   /a -> ,Ae(B   /e -> ,Af(B   /o -> ,Ax(B
   symbol    |   ~    | ~> -> ,A;(B   ~< -> ,A+(B   ~! -> ,A!(B   ~? -> ,A?(B   ~~ -> ,A8(B
   symbol    |  _ /   | _o -> ,A:(B   _a -> ,A*(B   // -> ,A0(B
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
 "french-prefix" "Latin-1" "FR>" t
 "French (Fran,Ag(Bais) input method with prefix modifiers

    effect   | prefix | examples
 ------------+--------+----------
    acute    |   '    | 'e -> ,Ai(B
    grave    |   `    | `a -> ,A`(B
  circumflex |   ^    | ^a -> ,Ab(B
  diaeresis  |   \"   | \"i -> ,Ao(B
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
 ("~," ?,)
 (",C" ?,AG(B)
 (",c" ?,Ag(B)
 (", " ?~)
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
 "german-prefix" "Latin-1" "DE>" t
 "German (Deutsch) input method with prefix modifiers
Key translation rules are:
 \"A -> ,AD(B ->   \"O -> ,AV(B   \"U -> ,A\(B   \"s -> ?,A_(B
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
 "spanish-prefix" "Latin-1" "ES>" t
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
  dot above  | ~ / .  | ~o -> ,Cu(B   /o -> ,Cu(B   .o -> ,Cu(B
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
 ("~A" ?,CC(B)
 ("~C" ?,CG(B)
 ("~D" ?,CP(B)
 ("~N" ?,CQ(B)
 ("~O" ?,CU(B)
 ("~a" ?,Cc(B)
 ("~c" ?,Cg(B)
 ("~d" ?,Cp(B)
 ("~n" ?,Cq(B)
 ("~o" ?,Cu(B)
 ("~$" ?,C%(B)
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
 ("/r" ?,C.(B)
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
