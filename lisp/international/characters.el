;;; characters.el --- set syntax and category for multibyte characters

;; Copyright (C) 1995, 1997 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 2001 Free Software Foundation, Inc.
;; Copyright (C) 2001, 2002
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H13PRO009

;; Keywords: multibyte character, character set, syntax, category

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

;; This file contains multibyte characters.  Save this file always in
;; the coding system `iso-2022-7bit'.

;; This file does not define the syntax for Latin-N character sets;
;; those are defined by the files latin-N.el.

;;; Code:

;;; Predefined categories.

;; For each character set.

(define-category ?a "ASCII")
(define-category ?l "Latin")
(define-category ?t "Thai")
(define-category ?g "Greek")
(define-category ?b "Arabic")
(define-category ?w "Hebrew")
(define-category ?y "Cyrillic")
(define-category ?k "Japanese katakana")
(define-category ?r "Japanese roman")
(define-category ?c "Chinese")
(define-category ?j "Japanese")
(define-category ?h "Korean")
(define-category ?e "Ethiopic (Ge'ez)")
(define-category ?v "Vietnamese")
(define-category ?i "Indian")
(define-category ?o "Lao")
(define-category ?q "Tibetan")

;; For each group (row) of 2-byte character sets.

(define-category ?A "Alpha-numeric characters of 2-byte character sets")
(define-category ?C "Chinese (Han) characters of 2-byte character sets")
(define-category ?G "Greek characters of 2-byte character sets")
(define-category ?H "Japanese Hiragana characters of 2-byte character sets")
(define-category ?K "Japanese Katakana characters of 2-byte character sets")
(define-category ?N "Korean Hangul characters of 2-byte character sets")
(define-category ?Y "Cyrillic characters of 2-byte character sets")
(define-category ?I "Indian Glyphs")

;; For phonetic classifications.

(define-category ?0 "consonant")
(define-category ?1 "base (independent) vowel")
(define-category ?2 "upper diacritical mark (including upper vowel)")
(define-category ?3 "lower diacritical mark (including lower vowel)")
(define-category ?4 "tone mark")
(define-category ?5 "symbol")
(define-category ?6 "digit")
(define-category ?7 "vowel-modifying diacritical mark")
(define-category ?8 "vowel-signs")
(define-category ?9 "semivowel lower")

;; For filling.
(define-category ?| "While filling, we can break a line at this character.")

;; For indentation calculation.
(define-category ? 
  "This character counts as a space for indentation purposes.")

;; Keep the following for `kinsoku' processing.  See comments in
;; kinsoku.el.
(define-category ?> "A character which can't be placed at beginning of line.")
(define-category ?< "A character which can't be placed at end of line.")

;; Combining
(define-category ?^ "Combining diacritic or mark")

;;; Setting syntax and category.

;; ASCII

;; All ASCII characters have the category `a' (ASCII) and `l' (Latin).
(modify-category-entry '(32 . 127) ?a)
(modify-category-entry '(32 . 127) ?l)

;; Arabic character set

(let ((charsets '(arabic-iso8859-6
		  arabic-digit
		  arabic-1-column
		  arabic-2-column)))
  (while charsets
;;     (modify-syntax-entry (make-char (car charsets)) "w")
    (map-charset-chars
     #'(lambda (char ignore) (modify-category-entry char ?b))
     (car charsets))
    (setq charsets (cdr charsets))))
(modify-category-entry '(#x600 . #x6ff) ?b)
(modify-category-entry '(#xfb50 . #xfdff) ?b)
(modify-category-entry '(#xfe70 . #xfefe) ?b)

;; Chinese character set (GB2312)

;; (modify-syntax-entry (make-char 'chinese-gb2312) "w")
;; (modify-syntax-entry (make-char 'chinese-gb2312 33) "_")
;; (modify-syntax-entry (make-char 'chinese-gb2312 34) "_")
;; (modify-syntax-entry (make-char 'chinese-gb2312 41) "_")
(modify-syntax-entry ?\$A!2(B "($A!3(B")
(modify-syntax-entry ?\$A!4(B "($A!5(B")
(modify-syntax-entry ?\$A!6(B "($A!7(B")
(modify-syntax-entry ?\$A!8(B "($A!9(B")
(modify-syntax-entry ?\$A!:(B "($A!;(B")
(modify-syntax-entry ?\$A!<(B "($A!=(B")
(modify-syntax-entry ?\$A!>(B "($A!?(B")
(modify-syntax-entry ?\$A!3(B ")$A!2(B")
(modify-syntax-entry ?\$A!5(B ")$A!4(B")
(modify-syntax-entry ?\$A!7(B ")$A!6(B")
(modify-syntax-entry ?\$A!9(B ")$A!8(B")
(modify-syntax-entry ?\$A!;(B ")$A!:(B")
(modify-syntax-entry ?\$A!=(B ")$A!<(B")
(modify-syntax-entry ?\$A!?(B ")$A!>(B")
;; Unicode equivalents of above
(modify-syntax-entry ?\$,2=T(B "($,2=U(B")
(modify-syntax-entry ?\$,2=H(B "($,2=I(B")
(modify-syntax-entry ?\$,2=J(B "($,2=K(B")
(modify-syntax-entry ?\$,2=L(B "($,2=M(B")
(modify-syntax-entry ?\$,2=N(B "($,2=O(B")
(modify-syntax-entry ?\$,2=V(B "($,2=W(B")
(modify-syntax-entry ?\$,2=P(B "($,2=Q(B")
(modify-syntax-entry ?\$,2=U(B ")$,2=T(B")
(modify-syntax-entry ?\$,2=I(B ")$,2=H(B")
(modify-syntax-entry ?\$,2=K(B ")$,2=J(B")
(modify-syntax-entry ?\$,2=M(B ")$,2=L(B")
(modify-syntax-entry ?\$,2=O(B ")$,2=N(B")
(modify-syntax-entry ?\$,2=W(B ")$,2=V(B")
(modify-syntax-entry ?\$,2=Q(B ")$,2=P(B")

;; (modify-category-entry (make-char 'chinese-gb2312) ?c)
;; (modify-category-entry (make-char 'chinese-gb2312) ?\|)
;; (modify-category-entry (make-char 'chinese-gb2312 35) ?A)
;; (modify-category-entry (make-char 'chinese-gb2312 36) ?H)
;; (modify-category-entry (make-char 'chinese-gb2312 37) ?K)
;; (modify-category-entry (make-char 'chinese-gb2312 38) ?G)
;; (modify-category-entry (make-char 'chinese-gb2312 39) ?Y)
;; (let ((row 48))
;;   (while (< row 127)
;;     (modify-category-entry (make-char 'chinese-gb2312 row) ?C)
;;     (setq row (1+ row))))

;; Chinese character set (BIG5)

;; (let ((generic-big5-1-char (make-char 'chinese-big5-1))
;;       (generic-big5-2-char (make-char 'chinese-big5-2)))
;;   (modify-syntax-entry generic-big5-1-char "w")
;;   (modify-syntax-entry generic-big5-2-char "w")

;;   (modify-category-entry generic-big5-1-char ?c)
;;   (modify-category-entry generic-big5-2-char ?c)

;;   (modify-category-entry generic-big5-1-char ?C)
;;   (modify-category-entry generic-big5-2-char ?C)

;;   (modify-category-entry generic-big5-1-char ?\|)
;;   (modify-category-entry generic-big5-2-char ?\|))


;; Chinese character set (CNS11643)

;; (let ((cns-list '(chinese-cns11643-1
;; 		  chinese-cns11643-2
;; 		  chinese-cns11643-3
;; 		  chinese-cns11643-4
;; 		  chinese-cns11643-5
;; 		  chinese-cns11643-6
;; 		  chinese-cns11643-7))
;;       generic-char)
;;   (while cns-list
;;     (setq generic-char (make-char (car cns-list)))
;;     (modify-syntax-entry generic-char "w")
;;     (modify-category-entry generic-char ?c)
;;     (modify-category-entry generic-char ?C)
;;     (modify-category-entry generic-char ?|)
;;     (setq cns-list (cdr cns-list))))

;; Cyrillic character set (ISO-8859-5)

(modify-syntax-entry (decode-char 'iso-8859-5 160) " ")
(modify-syntax-entry ?,L-(B ".")
(modify-syntax-entry ?,Lp(B ".")
(modify-syntax-entry ?,L}(B ".")
(let ((tbl (standard-case-table)))
  (set-case-syntax-pair ?,L!(B ?,Lq(B tbl)
  (set-case-syntax-pair ?,L"(B ?,Lr(B tbl)
  (set-case-syntax-pair ?,L#(B ?,Ls(B tbl)
  (set-case-syntax-pair ?,L$(B ?,Lt(B tbl)
  (set-case-syntax-pair ?,L%(B ?,Lu(B tbl)
  (set-case-syntax-pair ?,L&(B ?,Lv(B tbl)
  (set-case-syntax-pair ?,L'(B ?,Lw(B tbl)
  (set-case-syntax-pair ?,L((B ?,Lx(B tbl)
  (set-case-syntax-pair ?,L)(B ?,Ly(B tbl)
  (set-case-syntax-pair ?,L*(B ?,Lz(B tbl)
  (set-case-syntax-pair ?,L+(B ?,L{(B tbl)
  (set-case-syntax-pair ?,L,(B ?,L|(B tbl)
  (set-case-syntax-pair ?,L.(B ?,L~(B tbl)
  (set-case-syntax-pair ?,L/(B ?,L(B tbl)
  (set-case-syntax-pair ?,L0(B ?,LP(B tbl)
  (set-case-syntax-pair ?,L1(B ?,LQ(B tbl)
  (set-case-syntax-pair ?,L2(B ?,LR(B tbl)
  (set-case-syntax-pair ?,L3(B ?,LS(B tbl)
  (set-case-syntax-pair ?,L4(B ?,LT(B tbl)
  (set-case-syntax-pair ?,L5(B ?,LU(B tbl)
  (set-case-syntax-pair ?,L6(B ?,LV(B tbl)
  (set-case-syntax-pair ?,L7(B ?,LW(B tbl)
  (set-case-syntax-pair ?,L8(B ?,LX(B tbl)
  (set-case-syntax-pair ?,L9(B ?,LY(B tbl)
  (set-case-syntax-pair ?,L:(B ?,LZ(B tbl)
  (set-case-syntax-pair ?,L;(B ?,L[(B tbl)
  (set-case-syntax-pair ?,L<(B ?,L\(B tbl)
  (set-case-syntax-pair ?,L=(B ?,L](B tbl)
  (set-case-syntax-pair ?,L>(B ?,L^(B tbl)
  (set-case-syntax-pair ?,L?(B ?,L_(B tbl)
  (set-case-syntax-pair ?,L@(B ?,L`(B tbl)
  (set-case-syntax-pair ?,LA(B ?,La(B tbl)
  (set-case-syntax-pair ?,LB(B ?,Lb(B tbl)
  (set-case-syntax-pair ?,LC(B ?,Lc(B tbl)
  (set-case-syntax-pair ?,LD(B ?,Ld(B tbl)
  (set-case-syntax-pair ?,LE(B ?,Le(B tbl)
  (set-case-syntax-pair ?,LF(B ?,Lf(B tbl)
  (set-case-syntax-pair ?,LG(B ?,Lg(B tbl)
  (set-case-syntax-pair ?,LH(B ?,Lh(B tbl)
  (set-case-syntax-pair ?,LI(B ?,Li(B tbl)
  (set-case-syntax-pair ?,LJ(B ?,Lj(B tbl)
  (set-case-syntax-pair ?,LK(B ?,Lk(B tbl)
  (set-case-syntax-pair ?,LL(B ?,Ll(B tbl)
  (set-case-syntax-pair ?,LM(B ?,Lm(B tbl)
  (set-case-syntax-pair ?,LN(B ?,Ln(B tbl)
  (set-case-syntax-pair ?,LO(B ?,Lo(B tbl)
  (set-case-syntax-pair ?$,1(!(B ?$,1(q(B tbl)
  (set-case-syntax-pair ?$,1("(B ?$,1(r(B tbl)
  (set-case-syntax-pair ?$,1(#(B ?$,1(s(B tbl)
  (set-case-syntax-pair ?$,1($(B ?$,1(t(B tbl)
  (set-case-syntax-pair ?$,1(%(B ?$,1(u(B tbl)
  (set-case-syntax-pair ?$,1(&(B ?$,1(v(B tbl)
  (set-case-syntax-pair ?$,1('(B ?$,1(w(B tbl)
  (set-case-syntax-pair ?$,1(((B ?$,1(x(B tbl)
  (set-case-syntax-pair ?$,1()(B ?$,1(y(B tbl)
  (set-case-syntax-pair ?$,1(*(B ?$,1(z(B tbl)
  (set-case-syntax-pair ?$,1(+(B ?$,1({(B tbl)
  (set-case-syntax-pair ?$,1(,(B ?$,1(|(B tbl)
  (set-case-syntax-pair ?$,1(.(B ?$,1(~(B tbl)
  (set-case-syntax-pair ?$,1(/(B ?$,1((B tbl)
  (set-case-syntax-pair ?$,1(0(B ?$,1(P(B tbl)
  (set-case-syntax-pair ?$,1(1(B ?$,1(Q(B tbl)
  (set-case-syntax-pair ?$,1(2(B ?$,1(R(B tbl)
  (set-case-syntax-pair ?$,1(3(B ?$,1(S(B tbl)
  (set-case-syntax-pair ?$,1(4(B ?$,1(T(B tbl)
  (set-case-syntax-pair ?$,1(5(B ?$,1(U(B tbl)
  (set-case-syntax-pair ?$,1(6(B ?$,1(V(B tbl)
  (set-case-syntax-pair ?$,1(7(B ?$,1(W(B tbl)
  (set-case-syntax-pair ?$,1(8(B ?$,1(X(B tbl)
  (set-case-syntax-pair ?$,1(9(B ?$,1(Y(B tbl)
  (set-case-syntax-pair ?$,1(:(B ?$,1(Z(B tbl)
  (set-case-syntax-pair ?$,1(;(B ?$,1([(B tbl)
  (set-case-syntax-pair ?$,1(<(B ?$,1(\(B tbl)
  (set-case-syntax-pair ?$,1(=(B ?$,1(](B tbl)
  (set-case-syntax-pair ?$,1(>(B ?$,1(^(B tbl)
  (set-case-syntax-pair ?$,1(?(B ?$,1(_(B tbl)
  (set-case-syntax-pair ?$,1(@(B ?$,1(`(B tbl)
  (set-case-syntax-pair ?$,1(A(B ?$,1(a(B tbl)
  (set-case-syntax-pair ?$,1(B(B ?$,1(b(B tbl)
  (set-case-syntax-pair ?$,1(C(B ?$,1(c(B tbl)
  (set-case-syntax-pair ?$,1(D(B ?$,1(d(B tbl)
  (set-case-syntax-pair ?$,1(E(B ?$,1(e(B tbl)
  (set-case-syntax-pair ?$,1(F(B ?$,1(f(B tbl)
  (set-case-syntax-pair ?$,1(G(B ?$,1(g(B tbl)
  (set-case-syntax-pair ?$,1(H(B ?$,1(h(B tbl)
  (set-case-syntax-pair ?$,1(I(B ?$,1(i(B tbl)
  (set-case-syntax-pair ?$,1(J(B ?$,1(j(B tbl)
  (set-case-syntax-pair ?$,1(K(B ?$,1(k(B tbl)
  (set-case-syntax-pair ?$,1(L(B ?$,1(l(B tbl)
  (set-case-syntax-pair ?$,1(M(B ?$,1(m(B tbl)
  (set-case-syntax-pair ?$,1(N(B ?$,1(n(B tbl)
  (set-case-syntax-pair ?$,1(O(B ?$,1(o(B tbl))

;; Devanagari character set

;;; Commented out since the categories appear not to be used anywhere
;;; and word syntax is the default.
;; (let ((deflist	'(;; chars	syntax	category
;; 		  ("$(5!!!"!#(B"	"w"	?7) ; vowel-modifying diacritical mark
;; 					    ; chandrabindu, anuswar, visarga
;; 		  ("$(5!$(B-$(5!2(B"	"w"	?1) ; independent vowel
;; 		  ("$(5!3(B-$(5!X(B"	"w"	?0) ; consonant
;; 		  ("$(5!Z(B-$(5!g(B"	"w"	?8) ; matra
;; 		  ("$(5!q(B-$(5!z(B"	"w"	?6) ; digit
;; 		  ;; Unicode equivalents
;; 		  ("$,15A5B5C(B"	"w"	?7) ; vowel-modifying diacritical mark
;; 					    ; chandrabindu, anuswar, visarga
;; 		  ("$,15E(B-$,15M(B"	"w"	?1) ; independent vowel
;; 		  ("$,15U(B-$,15y(B"	"w"	?0) ; consonant
;; 		  ("$,15~(B-$,16)(B"	"w"	?8) ; matra
;; 		  ("$,16F(B-$,16O(B"	"w"	?6) ; digit
;; 		  ))
;;       elm chars len syntax category to ch i)
;;   (while deflist
;;     (setq elm (car deflist))
;;     (setq chars (car elm)
;; 	  len (length chars)
;; 	  syntax (nth 1 elm)
;; 	  category (nth 2 elm)
;; 	  i 0)
;;     (while (< i len)
;;       (if (= (aref chars i) ?-)
;; 	  (setq i (1+ i)
;; 		to (aref chars i))
;; 	(setq ch (aref chars i)
;; 	      to ch))
;;       (while (<= ch to)
;; 	(modify-syntax-entry ch syntax)
;; 	(modify-category-entry ch category)
;; 	(setq ch (1+ ch)))
;;       (setq i (1+ i)))
;;     (setq deflist (cdr deflist))))

;; Ethiopic character set

;; (modify-category-entry (make-char 'ethiopic) ?e)
;; (modify-syntax-entry (make-char 'ethiopic) "w")
(modify-category-entry '(#x1200 . #x137b) ?e)
(let ((chars '(?$(3$h(B ?$(3$i(B ?$(3$j(B ?$(3$k(B ?$(3$l(B ?$(3$m(B ?$(3$n(B ?$(3$o(B ?$(3%i(B ?$(3%t(B ?$(3%u(B ?$(3%v(B ?$(3%w(B ?$(3%x(B
	       ;; Unicode equivalents of the above:
	       ?$,1Q!(B ?$,1Q"(B ?$,1Q#(B ?$,1Q$(B ?$,1Q%(B ?$,1Q&(B ?$,1Q'(B ?$,1Q((B ?$,3op(B ?$,3o{(B ?$,3o|(B ?$,3o}(B ?$,3o~(B ?$,3o(B)))
  (while chars
    (modify-syntax-entry (car chars) ".")
    (setq chars (cdr chars))))

;; Greek character set (ISO-8859-7)

;; (modify-category-entry (make-char 'greek-iso8859-7) ?g)
(let ((c #x370))
  (while (<= c #x3ff)
    (modify-category-entry (decode-char 'ucs c) ?g)
    (setq c (1+ c))))

;; (let ((c 182))
;;   (while (< c 255)
;;     (modify-syntax-entry (make-char 'greek-iso8859-7 c) "w")
;;     (setq c (1+ c))))
;; (modify-syntax-entry (make-char 'greek-iso8859-7 160) "w") ; NBSP
;; (modify-syntax-entry ?,F7(B ".")
;; (modify-syntax-entry ?,F;(B ".")
;; (modify-syntax-entry ?,F=(B ".")
(let ((tbl (standard-case-table)))
  ;; Fixme: non-letter syntax copied from latin-1, but that's dubious
  ;; in several cases.
  (set-case-syntax ?,F!(B "." tbl)
  (set-case-syntax ?,F"(B "." tbl)
  (set-case-syntax ?,F&(B "." tbl)
  (set-case-syntax ?,F&(B "_" tbl)
  (set-case-syntax ?,F'(B "." tbl)
  (set-case-syntax ?,F)(B "_" tbl)
  (set-case-syntax ?,F+(B "." tbl)
  (set-case-syntax ?,F,(B "_" tbl)
  (set-case-syntax ?,F-(B "_" tbl)
  (set-case-syntax ?,F/(B "." tbl)
  (set-case-syntax ?,F0(B "_" tbl)
  (set-case-syntax ?,F1(B "_" tbl)
;;  (set-case-syntax ?,F7(B "_" tbl)
;;  (set-case-syntax ?,F=(B "_" tbl)
  (set-case-syntax-pair ?,FA(B ?,Fa(B tbl)
  (set-case-syntax-pair ?,FB(B ?,Fb(B tbl)
  (set-case-syntax-pair ?,FC(B ?,Fc(B tbl)
  (set-case-syntax-pair ?,FD(B ?,Fd(B tbl)
  (set-case-syntax-pair ?,FE(B ?,Fe(B tbl)
  (set-case-syntax-pair ?,FF(B ?,Ff(B tbl)
  (set-case-syntax-pair ?,FG(B ?,Fg(B tbl)
  (set-case-syntax-pair ?,FH(B ?,Fh(B tbl)
  (set-case-syntax-pair ?,FI(B ?,Fi(B tbl)
  (set-case-syntax-pair ?,FJ(B ?,Fj(B tbl)
  (set-case-syntax-pair ?,FK(B ?,Fk(B tbl)
  (set-case-syntax-pair ?,FL(B ?,Fl(B tbl)
  (set-case-syntax-pair ?,FM(B ?,Fm(B tbl)
  (set-case-syntax-pair ?,FN(B ?,Fn(B tbl)
  (set-case-syntax-pair ?,FO(B ?,Fo(B tbl)
  (set-case-syntax-pair ?,FP(B ?,Fp(B tbl)
  (set-case-syntax-pair ?,FQ(B ?,Fq(B tbl)
  (set-case-syntax-pair ?,FS(B ?,Fs(B tbl)
  (set-case-syntax-pair ?,FT(B ?,Ft(B tbl)
  (set-case-syntax-pair ?,FU(B ?,Fu(B tbl)
  (set-case-syntax-pair ?,FV(B ?,Fv(B tbl)
  (set-case-syntax-pair ?,FW(B ?,Fw(B tbl)
  (set-case-syntax-pair ?,FX(B ?,Fx(B tbl)
  (set-case-syntax-pair ?,FY(B ?,Fy(B tbl)
  (set-case-syntax-pair ?,FZ(B ?,Fz(B tbl)
  (set-case-syntax-pair ?,F[(B ?,F{(B tbl)
  (set-case-syntax-pair ?,F?(B ?,F~(B tbl)
  (set-case-syntax-pair ?,F>(B ?,F}(B tbl)
  (set-case-syntax-pair ?,F<(B ?,F|(B tbl)
  (set-case-syntax-pair ?,F6(B ?,F\(B tbl)
  (set-case-syntax-pair ?,F8(B ?,F](B tbl)
  (set-case-syntax-pair ?,F9(B ?,F^(B tbl)
  (set-case-syntax-pair ?,F:(B ?,F_(B tbl)
  ;; Unicode equivalents
  (set-case-syntax-pair ?$,1&q(B ?$,1'1(B tbl)
  (set-case-syntax-pair ?$,1&r(B ?$,1'2(B tbl)
  (set-case-syntax-pair ?$,1&s(B ?$,1'3(B tbl)
  (set-case-syntax-pair ?$,1&t(B ?$,1'4(B tbl)
  (set-case-syntax-pair ?$,1&u(B ?$,1'5(B tbl)
  (set-case-syntax-pair ?$,1&v(B ?$,1'6(B tbl)
  (set-case-syntax-pair ?$,1&w(B ?$,1'7(B tbl)
  (set-case-syntax-pair ?$,1&x(B ?$,1'8(B tbl)
  (set-case-syntax-pair ?$,1&y(B ?$,1'9(B tbl)
  (set-case-syntax-pair ?$,1&z(B ?$,1':(B tbl)
  (set-case-syntax-pair ?$,1&{(B ?$,1';(B tbl)
  (set-case-syntax-pair ?$,1&|(B ?$,1'<(B tbl)
  (set-case-syntax-pair ?$,1&}(B ?$,1'=(B tbl)
  (set-case-syntax-pair ?$,1&~(B ?$,1'>(B tbl)
  (set-case-syntax-pair ?$,1&(B ?$,1'?(B tbl)
  (set-case-syntax-pair ?$,1' (B ?$,1'@(B tbl)
  (set-case-syntax-pair ?$,1'!(B ?$,1'A(B tbl)
  (set-case-syntax-pair ?$,1'#(B ?$,1'C(B tbl)
  (set-case-syntax-pair ?$,1'$(B ?$,1'D(B tbl)
  (set-case-syntax-pair ?$,1'%(B ?$,1'E(B tbl)
  (set-case-syntax-pair ?$,1'&(B ?$,1'F(B tbl)
  (set-case-syntax-pair ?$,1''(B ?$,1'G(B tbl)
  (set-case-syntax-pair ?$,1'((B ?$,1'H(B tbl)
  (set-case-syntax-pair ?$,1')(B ?$,1'I(B tbl)
  (set-case-syntax-pair ?$,1'*(B ?$,1'J(B tbl)
  (set-case-syntax-pair ?$,1'+(B ?$,1'K(B tbl)
  (set-case-syntax-pair ?$,1&o(B ?$,1'N(B tbl)
  (set-case-syntax-pair ?$,1&n(B ?$,1'M(B tbl)
  (set-case-syntax-pair ?$,1&l(B ?$,1'L(B tbl)
  (set-case-syntax-pair ?$,1&f(B ?$,1',(B tbl)
  (set-case-syntax-pair ?$,1&h(B ?$,1'-(B tbl)
  (set-case-syntax-pair ?$,1&i(B ?$,1'.(B tbl)
  (set-case-syntax-pair ?$,1&j(B ?$,1'/(B tbl))

;; Hebrew character set (ISO-8859-8)

;; (modify-category-entry (make-char 'hebrew-iso8859-8) ?w)
(let ((c #x591))
  (while (<= c #x5f4)
    (modify-category-entry (decode-char 'ucs c) ?w)
    (setq c (1+ c))))

;; (modify-syntax-entry (make-char 'hebrew-iso8859-8 208) ".") ; PASEQ
;; (modify-syntax-entry (make-char 'hebrew-iso8859-8 211) ".") ; SOF PASUQ
(modify-syntax-entry (decode-char 'ucs #x5be) ".") ; MAQAF
(modify-syntax-entry (decode-char 'ucs #x5c0) ".") ; PASEQ
(modify-syntax-entry (decode-char 'ucs #x5c3) ".") ; SOF PASUQ
(modify-syntax-entry (decode-char 'ucs #x5f3) ".") ; GERESH
(modify-syntax-entry (decode-char 'ucs #x5f4) ".") ; GERSHAYIM

;; (let ((c 224))
;;   (while (< c 251)
;;     (modify-syntax-entry (make-char 'hebrew-iso8859-8 c) "w")
;;     (setq c (1+ c))))
;; (modify-syntax-entry (make-char 'hebrew-iso8859-8 160) "w") ; NBSP

;; Indian character set (IS 13194 and other Emacs original Indian charsets)

;; (modify-category-entry (make-char 'indian-is13194) ?i)
;; (modify-category-entry (make-char 'indian-2-column) ?I)
;; (modify-category-entry (make-char 'indian-glyph) ?I)
;; Unicode Devanagari block
(let ((c #x901))
  (while (<= c #x970)
    (modify-category-entry (decode-char 'ucs c) ?i)
    (setq c (1+ c))))

;;; Commented out since the categories appear not to be used anywhere
;;; and word syntax is the default.
;; (let ((deflist				;
;; 	'(;; chars	syntax	category
;; 	  ("(5!"#(B"	"w"	?7) ; vowel-modifying diacritical mark
;; 				    ; chandrabindu, anuswar, visarga
;; 	  ("(5$(B-(52(B"	"w"	?1) ; base (independent) vowel
;; 	  ("(53(B-(5X(B"	"w"	?0) ; consonant
;; 	  ("(5Z(B-(5g(B"	"w"	?8) ; matra
;; 	  ("(5q(B-(5z(B"	"w"	?6) ; digit
;; 	  ))
;;       elm chars len syntax category to ch i)
;;   (while deflist
;;     (setq elm (car deflist))
;;     (setq chars (car elm)
;; 	  len (length chars)
;; 	  syntax (nth 1 elm)
;; 	  category (nth 2 elm)
;; 	  i 0)
;;     (while (< i len)
;;       (if (= (aref chars i) ?-)
;; 	  (setq i (1+ i)
;; 		to (aref chars i))
;; 	(setq ch (aref chars i)
;; 	      to ch))
;;       (while (<= ch to)
;; 	(modify-syntax-entry ch syntax)
;; 	(modify-category-entry ch category)
;; 	(setq ch (1+ ch)))
;;       (setq i (1+ i)))
;;     (setq deflist (cdr deflist))))


;; Japanese character set (JISX0201-kana, JISX0201-roman, JISX0208, JISX0212)

(map-charset-chars
 #'(lambda (char ignore)
     (if (consp char)
	 (let ((from (car char))
	       (to (car char)))
	   (while (<= from to)
	     (modify-category-entry from ?k)
	     (setq from (1+ from))))
       (modify-category-entry char ?k)))
 'katakana-jisx0201)

(map-charset-chars
 #'(lambda (char ignore)
     (if (consp char)
	 (let ((from (car char))
	       (to (cdr char)))
	   (while (<= from to)
	     (modify-category-entry from ?r)
	     (setq from (1+ from))))
       (modify-category-entry char ?r)))
 'latin-jisx0201)
(dolist (l '(katakana-jisx0201 japanese-jisx0208 japanese-jisx0212))
  (map-charset-chars
   #'(lambda (char ignore)
       (if (consp char)
	   (let ((from (car char))
		 (to (cdr char)))
	     (while (<= from to)
	       (modify-category-entry from ?j)
	       (modify-category-entry from ?\|)
	       (setq from (1+ from))))
	 (modify-category-entry char ?j)
	 (modify-category-entry char ?\|)))
   l))

;; Unicode equivalents of JISX0201-kana
(let ((c #xff61))
  (while (<= c #xff9f)
    (modify-category-entry (decode-char 'ucs c) ?k)
    (modify-category-entry (decode-char 'ucs c) ?j)
    (modify-category-entry (decode-char 'ucs c) ?\|)
    (setq c (1+ c))))

;; Katakana block
(let ((c #x30a0))
  (while (<= c #x30ff)
    ;; ?K is double width, ?k isn't specified
    (modify-category-entry (decode-char 'ucs c) ?k)
    (modify-category-entry (decode-char 'ucs c) ?j)
    (modify-category-entry (decode-char 'ucs c) ?\|) 
    (setq c (1+ c))))

;; Hiragana block
(let ((c #x3040))
  (while (<= c #x309f)
    ;; ?H is actually defined to be double width
    (modify-category-entry (decode-char 'ucs c) ?H)
    ;;(modify-category-entry (decode-char 'ucs c) ?j)
    (modify-category-entry (decode-char 'ucs c) ?\|) 
    (setq c (1+ c))))

;; JISX0208
;; (modify-syntax-entry (make-char 'japanese-jisx0208) "w")
;; (modify-syntax-entry (make-char 'japanese-jisx0208 33) "_")
;; (modify-syntax-entry (make-char 'japanese-jisx0208 34) "_")
;; (modify-syntax-entry (make-char 'japanese-jisx0208 40) "_")
;; (let ((chars '(?$B!<(B ?$B!+(B ?$B!,(B ?$B!3(B ?$B!4(B ?$B!5(B ?$B!6(B ?$B!7(B ?$B!8(B ?$B!9(B ?$B!:(B ?$B!;(B)))
;;   (while chars
;;     (modify-syntax-entry (car chars) "w")
;;     (setq chars (cdr chars))))

(modify-syntax-entry (cons (decode-char 'japanese-jisx0208 #x2121)
			   (decode-char 'japanese-jisx0208 #x227E)) "_")
(modify-syntax-entry (cons (decode-char 'japanese-jisx0208 #x2821)
			   (decode-char 'japanese-jisx0208 #x287E)) "_")
(modify-syntax-entry ?\$B!J(B "($B!K(B")
(modify-syntax-entry ?\$B!N(B "($B!O(B")
(modify-syntax-entry ?\$B!P(B "($B!Q(B")
(modify-syntax-entry ?\$B!V(B "($B!W(B")
(modify-syntax-entry ?\$B!X(B "($B!Y(B")
(modify-syntax-entry ?\$B!K(B ")$B!J(B")
(modify-syntax-entry ?\$B!O(B ")$B!N(B")
(modify-syntax-entry ?\$B!Q(B ")$B!P(B")
(modify-syntax-entry ?\$B!W(B ")$B!V(B")
(modify-syntax-entry ?\$B!Y(B ")$B!X(B")

;; (modify-category-entry (make-char 'japanese-jisx0208 35) ?A)
;; (modify-category-entry (make-char 'japanese-jisx0208 36) ?H)
;; (modify-category-entry (make-char 'japanese-jisx0208 37) ?K)
;; (modify-category-entry (make-char 'japanese-jisx0208 38) ?G)
;; (modify-category-entry (make-char 'japanese-jisx0208 39) ?Y)
;; (let ((row 48))
;;   (while (< row 127)
;;     (modify-category-entry (make-char 'japanese-jisx0208 row) ?C)
;;     (setq row (1+ row))))
(modify-category-entry ?$B!<(B ?K)
(let ((chars '(?$B!+(B ?$B!,(B)))
  (while chars
    (modify-category-entry (car chars) ?K)
    (modify-category-entry (car chars) ?H)
    (setq chars (cdr chars))))
(let ((chars '(?$B!3(B ?$B!4(B ?$B!5(B ?$B!6(B ?$B!7(B ?$B!8(B ?$B!9(B ?$B!:(B ?$B!;(B)))
  (while chars
    (modify-category-entry (car chars) ?C)
    (setq chars (cdr chars))))

;; JISX0212
;; (modify-syntax-entry (make-char 'japanese-jisx0212) "w")
;; (modify-syntax-entry (make-char 'japanese-jisx0212 33) "_")
;; (modify-syntax-entry (make-char 'japanese-jisx0212 34) "_")
;; (modify-syntax-entry (make-char 'japanese-jisx0212 35) "_")
 
(modify-syntax-entry (cons (decode-char 'japanese-jisx0212 #x2121)
			   (decode-char 'japanese-jisx0212 #x237E))
		     "_")

;; JISX0201-Kana
;; (modify-syntax-entry (make-char 'katakana-jisx0201) "w")
(let ((chars '(?(I!(B ?(I$(B ?(I%(B
	       ;; Unicode:
	       ?$,3sa(B ?$,3sd(B ?$,3se(B)))
  (while chars
    (modify-syntax-entry (car chars) ".")
    (setq chars (cdr chars))))

(modify-syntax-entry ?\(I"(B "((I#(B")
(modify-syntax-entry ?\(I#(B "((I"(B")

;; Korean character set (KSC5601)

;; (modify-syntax-entry (make-char 'korean-ksc5601) "w")
;; (modify-syntax-entry (make-char 'korean-ksc5601 33) "_")
;; (modify-syntax-entry (make-char 'korean-ksc5601 34) "_")
;; (modify-syntax-entry (make-char 'korean-ksc5601 38) "_")
;; (modify-syntax-entry (make-char 'korean-ksc5601 39) "_")
;; (modify-syntax-entry (make-char 'korean-ksc5601 40) "_")
;; (modify-syntax-entry (make-char 'korean-ksc5601 41) "_")

;; (modify-category-entry (make-char 'korean-ksc5601) ?h)
;; (modify-category-entry (make-char 'korean-ksc5601 35) ?A)
;; (modify-category-entry (make-char 'korean-ksc5601 37) ?G)
;; (modify-category-entry (make-char 'korean-ksc5601 42) ?H)
;; (modify-category-entry (make-char 'korean-ksc5601 43) ?K)
;; (modify-category-entry (make-char 'korean-ksc5601 44) ?Y)

;; Latin character set (latin-1,2,3,4,5,8,9)

;; (modify-category-entry (make-char 'latin-iso8859-1) ?l)
;; (modify-category-entry (make-char 'latin-iso8859-2) ?l)
;; (modify-category-entry (make-char 'latin-iso8859-3) ?l)
;; (modify-category-entry (make-char 'latin-iso8859-4) ?l)
;; (modify-category-entry (make-char 'latin-iso8859-9) ?l)
;; (modify-category-entry (make-char 'latin-iso8859-14) ?l)
;; (modify-category-entry (make-char 'latin-iso8859-15) ?l)

;; (modify-category-entry (make-char 'latin-iso8859-1 160) ?\ )
;; (modify-category-entry (make-char 'latin-iso8859-2 160) ?\ )
;; (modify-category-entry (make-char 'latin-iso8859-3 160) ?\ )
;; (modify-category-entry (make-char 'latin-iso8859-4 160) ?\ )
;; (modify-category-entry (make-char 'latin-iso8859-9 160) ?\ )
;; (modify-category-entry (make-char 'latin-iso8859-14 160) ?\ )
;; (modify-category-entry (make-char 'latin-iso8859-15 160) ?\ )

;; Lao character set

;; (modify-category-entry (make-char 'lao) ?o)
(dotimes (i (1+ (- #xeff #xe80)))
  (modify-category-entry (decode-char 'ucs (+ i #xe80)) ?o))

(let ((deflist	'(;; chars	syntax	category
		  ("(1!(B-(1N(B"	"w"	?0) ; consonant
		  ("(1PRS]`(B-(1d(B"	"w"	?1) ; vowel base
		  ("(1QT(B-(1W[m(B"	"w"	?2) ; vowel upper
		  ("(1XY(B"		"w"	?3) ; vowel lower
		  ("(1h(B-(1l(B"	"w"	?4) ; tone mark 
		  ("(1\(B"		"w"	?9) ; semivowel lower
		  ("(1p(B-(1y(B"	"w"	?6) ; digit
		  ("(1Of(B"		"_"	?5) ; symbol
		  ;; Unicode equivalents
		  ("$,1D!(B-$,1DN(B"	"w"	?0) ; consonant
		  ("$,1DPDRDSD]D`(B-$,1Dd(B"	"w"	?1) ; vowel base
		  ("$,1DQDT(B-$,1DWD[Dm(B"	"w"	?2) ; vowel upper
		  ("$,1DXDY(B"	"w"	?3) ; vowel lower
		  ("$,1Dh(B-$,1Dk(B"	"w"	?4) ; tone mark 
		  ("$,1D\D](B"	"w"	?9) ; semivowel lower
		  ("$,1Dp(B-$,1Dy(B"	"w"	?6) ; digit
		  ("$,1DODf(B"	"_"	?5) ; symbol
		  ))
      elm chars len syntax category to ch i)
  (while deflist
    (setq elm (car deflist))
    (setq chars (car elm)
	  len (length chars)
	  syntax (nth 1 elm)
	  category (nth 2 elm)
	  i 0)
    (while (< i len)
      (if (= (aref chars i) ?-)
	  (setq i (1+ i)
		to (aref chars i))
	(setq ch (aref chars i)
	      to ch))
      (while (<= ch to)
	(unless (string-equal syntax "w")
	  (modify-syntax-entry ch syntax))
	(modify-category-entry ch category)
	(setq ch (1+ ch)))
      (setq i (1+ i)))
    (setq deflist (cdr deflist))))

;; Thai character set (TIS620)

;; (modify-category-entry (make-char 'thai-tis620) ?t)
(dotimes (i (1+ (- #xe7f #xe00)))
  (modify-category-entry (decode-char 'ucs (+ i #xe00)) ?t))

(let ((deflist	'(;; chars	syntax	category
		  (",T!(B-,TCEG(B-,TN(B"	"w"	?0) ; consonant
		  (",TDFPRS`(B-,Te(B"	"w"	?1) ; vowel base
		  (",TQT(B-,TWgn(B"	"w"	?2) ; vowel upper
		  (",TX(B-,TZ(B"	"w"	?3) ; vowel lower
		  (",Th(B-,Tm(B"	"w"	?4) ; tone mark 
		  (",Tp(B-,Ty(B"	"w"	?6) ; digit
		  (",TOf_oz{(B"	"_"	?5) ; symbol
		  ;; Unicode equivalents
		  ("$,1Ba(B-$,1C#C%C'(B-$,1C.(B"	"w"	?0) ; consonant
		  ("$,1C$C&C0C2C3C@(B-$,1CE(B"	"w"	?1) ; vowel base
		  ("$,1C1C4(B-$,1C7CGCN(B"	"w"	?2) ; vowel upper
		  ("$,1C8(B-$,1C:(B"	"w"	?3) ; vowel lower
		  ("$,1CH(B-$,1CM(B"	"w"	?4) ; tone mark 
		  ("$,1CP(B-$,1CY(B"	"w"	?6) ; digit
		  ("$,1C/CFC?COCZC[(B"	"_"	?5) ; symbol
		  ))
      elm chars len syntax category to ch i)
  (while deflist
    (setq elm (car deflist))
    (setq chars (car elm)
	  len (length chars)
	  syntax (nth 1 elm)
	  category (nth 2 elm)
	  i 0)
    (while (< i len)
      (if (= (aref chars i) ?-)
	  (setq i (1+ i)
		to (aref chars i))
	(setq ch (aref chars i)
	      to ch))
      (while (<= ch to)
	(unless (string-equal syntax "w")
	  (modify-syntax-entry ch syntax))
	(modify-category-entry ch category)
	(setq ch (1+ ch)))
      (setq i (1+ i)))
    (setq deflist (cdr deflist))))

;; Tibetan character set

;; (modify-category-entry (make-char 'tibetan) ?q)
;; (modify-category-entry (make-char 'tibetan-1-column) ?q)
(dotimes (i (1+ (- #xfff #xf00)))
  (modify-category-entry (decode-char 'ucs (+ i #xf00)) ?q))

(let ((deflist	'(;; chars             syntax category
		  ("4$(7"!0"!1(B-4$(7"J0"J14"K0"K1(B"        	"w"	?0) ; consonant
		  ("$(7#!(B-$(7#J#K#L#M!"!#(B"       "w"     ?0) ;
		  ("$(7$!(B-$(7$e(B"              "w"     ?0) ;
		  ("$(7%!(B-$(7%u(B"              "w"     ?0) ;
		  ("$(7"S"["\"]"^"a(B"       "w"	?2) ; upper vowel
		  ("$(7"_"c"d"g"h"i"j"k"l(B" "w"	?2) ; upper modifier
		  ("$(7!I"Q"R"U"e!e!g(B"       "w"	?3) ; lowel vowel/modifier
		  ("$(7!P(B-$(7!Y!Z(B-$(7!c(B"	        "w"	?6) ; digit
		  ("$(7!;!=(B-$(7!B!D"`(B"        "."     ?|) ; line-break char
		  ("$(8!;!=!?!@!A!D"`(B"            "."     ?|) ;
		  ("$(7!8!;!=(B-$(7!B!D"`!m!d(B"  "."     ?>) ; prohibition
		  ("$(8!;!=!?!@!A!D"`(B"            "."     ?>) ;
		  ("$(7!0(B-$(7!:!l#R#S"f(B"      "."     ?<) ; prohibition
		  ("$(7!C!E(B-$(7!H!J(B-$(7!O!f!h(B-$(7!k!n!o#O#P(B-$(7#`(B" "." ?q) ; others

		  ;; Unicode version (not complete)
		  ("$,1F (B-$,1FIFJ(B"        	"w"	?0) ; consonant
		  ("$,1Fp(B-$,1G9G:G;G<(B"       "w"     ?0) ;
		  ("$,1FRFZF[F\F]F`(B"       "w"	?2) ; upper vowel
		  ("$,1F^FbFcFfFgFhFiFjFk(B" "w"	?2) ; upper modifier
		  ("$,1EYFPFQFTFdEuEw(B"       "w"	?3) ; lowel vowel/modifier
		  ("$,1E`(B-$,1EiEj(B-$,1Es(B"	        "w"	?6) ; digit
		  ("$,1EKEM(B-$,1ERETF_(B"        "."     ?|) ; line-break char
		  ("$,1EHEKEM(B-$,1ERETF_E}Et(B"  "."     ?>) ; prohibition
		  ("$,1E@(B-$,1EJE|GAGBFe(B"      "."     ?<) ; prohibition
		  ("$,1ESEU(B-$,1EXEZ(B-$,1E_EvEx(B-$,1E{E~EG>G?(B-$,1GO(B" "." ?q) ; others
		  ))
      elm chars len syntax category to ch i)
  (while deflist
    (setq elm (car deflist))
    (setq chars (car elm)
	  len (length chars)
	  syntax (nth 1 elm)
	  category (nth 2 elm)
	  i 0)
    (while (< i len)
      (if (= (aref chars i) ?-)
	  (setq i (1+ i)
		to (aref chars i))
	(setq ch (aref chars i)
	      to ch))
      (while (<= ch to)
	(unless (string-equal syntax "w")
	  (modify-syntax-entry ch syntax))
	(modify-category-entry ch category)
	(setq ch (1+ ch)))
      (setq i (1+ i)))
    (setq deflist (cdr deflist))))

;; Vietnamese character set

;; (let ((lower (make-char 'vietnamese-viscii-lower))
;;      (upper (make-char 'vietnamese-viscii-upper)))
;;   (modify-syntax-entry lower "w")
;;   (modify-syntax-entry upper "w")
;;  (modify-category-entry lower ?v)
;;  (modify-category-entry upper ?v)
;;  (modify-category-entry lower ?l)	; To make a word with
;;  (modify-category-entry upper ?l)	; latin characters.
;;  )

(let ((tbl (standard-case-table))
      (i 32))
  (while (< i 128)
    (let ((char (decode-char 'vietnamese-viscii-upper i)))
      (if char
	  (set-case-syntax-pair char (decode-char 'vietnamese-viscii-lower i)
				tbl)))
    (setq i (1+ i))))

;; Unicode (mule-unicode-0100-24ff)

(let ((tbl (standard-case-table)) c)

;; In some languages, U+0049 LATIN CAPITAL LETTER I and U+0131 LATIN
;; SMALL LETTER DOTLESS I make a case pair, and so do U+0130 LATIN
;; CAPITAL LETTER I WITH DOT ABOVE and U+0069 LATIN SMALL LETTER I.
;; Thus we have to check language-environment to handle casing
;; correctly.  Currently only I<->i is available.

  ;; Latin Extended-A, Latin Extended-B
  (setq c #x0100)
  (while (<= c #x0233)
    (modify-category-entry (decode-char 'ucs c) ?l)
    (and (or (<= c #x012e)
	     (and (>= c #x014a) (<= c #x0177)))
	 (zerop (% c 2))
	 (set-case-syntax-pair
	  (decode-char 'ucs c) (decode-char 'ucs (1+ c)) tbl))
    (and (>= c #x013a)
	 (<= c #x0148)
	 (zerop (% c 2))
	 (set-case-syntax-pair
	  (decode-char 'ucs (1- c)) (decode-char 'ucs c) tbl))
    (setq c (1+ c)))
  (set-case-syntax-pair ?$,1 R(B ?$,1 S(B tbl)
  (set-case-syntax-pair ?$,1 T(B ?$,1 U(B tbl)
  (set-case-syntax-pair ?$,1 V(B ?$,1 W(B tbl)
;;;  (set-case-syntax-pair ?$,1!8(B ?,A(B tbl)	; these two have different length!
  (set-case-syntax-pair ?$,1!9(B ?$,1!:(B tbl)
  (set-case-syntax-pair ?$,1!;(B ?$,1!<(B tbl)
  (set-case-syntax-pair ?$,1!=(B ?$,1!>(B tbl)

  ;; Latin Extended-B
  (set-case-syntax-pair ?$,1!A(B ?$,1#S(B tbl)
  (set-case-syntax-pair ?$,1!B(B ?$,1!C(B tbl)
  (set-case-syntax-pair ?$,1!D(B ?$,1!E(B tbl)
  (set-case-syntax-pair ?$,1!F(B ?$,1#T(B tbl)
  (set-case-syntax-pair ?$,1!G(B ?$,1!H(B tbl)
  (set-case-syntax-pair ?$,1!I(B ?$,1#V(B tbl)
  (set-case-syntax-pair ?$,1!J(B ?$,1#W(B tbl)
  (set-case-syntax-pair ?$,1!K(B ?$,1!L(B tbl)
  (set-case-syntax-pair ?$,1!N(B ?$,1"=(B tbl)
  (set-case-syntax-pair ?$,1!O(B ?$,1#Y(B tbl)
  (set-case-syntax-pair ?$,1!P(B ?$,1#[(B tbl)
  (set-case-syntax-pair ?$,1!Q(B ?$,1!R(B tbl)
  (set-case-syntax-pair ?$,1!S(B ?$,1#`(B tbl)
  (set-case-syntax-pair ?$,1!T(B ?$,1#c(B tbl)
  (set-case-syntax-pair ?$,1!V(B ?$,1#i(B tbl)
  (set-case-syntax-pair ?$,1!W(B ?$,1#h(B tbl)
  (set-case-syntax-pair ?$,1!X(B ?$,1!Y(B tbl)
  (set-case-syntax-pair ?$,1!\(B ?$,1#o(B tbl)
  (set-case-syntax-pair ?$,1!](B ?$,1#r(B tbl)
  (set-case-syntax-pair ?$,1!_(B ?$,1#u(B tbl)
  (set-case-syntax-pair ?$,1!`(B ?$,1!a(B tbl)
  (set-case-syntax-pair ?$,1!b(B ?$,1!c(B tbl)
  (set-case-syntax-pair ?$,1!d(B ?$,1!e(B tbl)
  (set-case-syntax-pair ?$,1!f(B ?$,1$ (B tbl)
  (set-case-syntax-pair ?$,1!g(B ?$,1!h(B tbl)
  (set-case-syntax-pair ?$,1!i(B ?$,1$#(B tbl)
  (set-case-syntax-pair ?$,1!l(B ?$,1!m(B tbl)
  (set-case-syntax-pair ?$,1!n(B ?$,1$((B tbl)
  (set-case-syntax-pair ?$,1!o(B ?$,1!p(B tbl)
  (set-case-syntax-pair ?$,1!q(B ?$,1$*(B tbl)
  (set-case-syntax-pair ?$,1!r(B ?$,1$+(B tbl)
  (set-case-syntax-pair ?$,1!s(B ?$,1!t(B tbl)
  (set-case-syntax-pair ?$,1!u(B ?$,1!v(B tbl)
  (set-case-syntax-pair ?$,1!w(B ?$,1$2(B tbl)
  (set-case-syntax-pair ?$,1!x(B ?$,1!y(B tbl)
  (set-case-syntax-pair ?$,1!|(B ?$,1!}(B tbl)
  (set-case-syntax-pair ?$,1"$(B ?$,1"&(B tbl)
  (set-case-syntax-pair ?$,1"%(B ?$,1"&(B tbl)
  (set-case-syntax-pair ?$,1"'(B ?$,1")(B tbl)
  (set-case-syntax-pair ?$,1"((B ?$,1")(B tbl)
  (set-case-syntax-pair ?$,1"*(B ?$,1",(B tbl)
  (set-case-syntax-pair ?$,1"+(B ?$,1",(B tbl)
  (set-case-syntax-pair ?$,1"-(B ?$,1".(B tbl)
  (set-case-syntax-pair ?$,1"/(B ?$,1"0(B tbl)
  (set-case-syntax-pair ?$,1"1(B ?$,1"2(B tbl)
  (set-case-syntax-pair ?$,1"3(B ?$,1"4(B tbl)
  (set-case-syntax-pair ?$,1"5(B ?$,1"6(B tbl)
  (set-case-syntax-pair ?$,1"7(B ?$,1"8(B tbl)
  (set-case-syntax-pair ?$,1"9(B ?$,1":(B tbl)
  (set-case-syntax-pair ?$,1";(B ?$,1"<(B tbl)
  (set-case-syntax-pair ?$,1">(B ?$,1"?(B tbl)
  (set-case-syntax-pair ?$,1"@(B ?$,1"A(B tbl)
  (set-case-syntax-pair ?$,1"B(B ?$,1"C(B tbl)
  (set-case-syntax-pair ?$,1"D(B ?$,1"E(B tbl)
  (set-case-syntax-pair ?$,1"F(B ?$,1"G(B tbl)
  (set-case-syntax-pair ?$,1"H(B ?$,1"I(B tbl)
  (set-case-syntax-pair ?$,1"J(B ?$,1"K(B tbl)
  (set-case-syntax-pair ?$,1"L(B ?$,1"M(B tbl)
  (set-case-syntax-pair ?$,1"N(B ?$,1"O(B tbl)
  ;; 01F0; F; 006A 030C; # LATIN SMALL LETTER J WITH CARON
  (set-case-syntax-pair ?$,1"Q(B ?$,1"S(B tbl)
  (set-case-syntax-pair ?$,1"R(B ?$,1"S(B tbl)
  (set-case-syntax-pair ?$,1"T(B ?$,1"U(B tbl)
  (set-case-syntax-pair ?$,1"V(B ?$,1!U(B tbl)
  (set-case-syntax-pair ?$,1"W(B ?$,1!(B tbl)
  (set-case-syntax-pair ?$,1"X(B ?$,1"Y(B tbl)
  (set-case-syntax-pair ?$,1"Z(B ?$,1"[(B tbl)
  (set-case-syntax-pair ?$,1"\(B ?$,1"](B tbl)
  (set-case-syntax-pair ?$,1"^(B ?$,1"_(B tbl)
  (set-case-syntax-pair ?$,1"`(B ?$,1"a(B tbl)
  (set-case-syntax-pair ?$,1"b(B ?$,1"c(B tbl)
  (set-case-syntax-pair ?$,1"d(B ?$,1"e(B tbl)
  (set-case-syntax-pair ?$,1"f(B ?$,1"g(B tbl)
  (set-case-syntax-pair ?$,1"h(B ?$,1"i(B tbl)
  (set-case-syntax-pair ?$,1"j(B ?$,1"k(B tbl)
  (set-case-syntax-pair ?$,1"l(B ?$,1"m(B tbl)
  (set-case-syntax-pair ?$,1"n(B ?$,1"o(B tbl)
  (set-case-syntax-pair ?$,1"p(B ?$,1"q(B tbl)
  (set-case-syntax-pair ?$,1"r(B ?$,1"s(B tbl)
  (set-case-syntax-pair ?$,1"t(B ?$,1"u(B tbl)
  (set-case-syntax-pair ?$,1"v(B ?$,1"w(B tbl)
  (set-case-syntax-pair ?$,1"x(B ?$,1"y(B tbl)
  (set-case-syntax-pair ?$,1"z(B ?$,1"{(B tbl)
  (set-case-syntax-pair ?$,1"|(B ?$,1"}(B tbl)
  (set-case-syntax-pair ?$,1"~(B ?$,1"(B tbl)
  (set-case-syntax-pair ?$,1#"(B ?$,1##(B tbl)
  (set-case-syntax-pair ?$,1#$(B ?$,1#%(B tbl)
  (set-case-syntax-pair ?$,1#&(B ?$,1#'(B tbl)
  (set-case-syntax-pair ?$,1#((B ?$,1#)(B tbl)
  (set-case-syntax-pair ?$,1#*(B ?$,1#+(B tbl)
  (set-case-syntax-pair ?$,1#,(B ?$,1#-(B tbl)
  (set-case-syntax-pair ?$,1#.(B ?$,1#/(B tbl)
  (set-case-syntax-pair ?$,1#0(B ?$,1#1(B tbl)
  (set-case-syntax-pair ?$,1#2(B ?$,1#3(B tbl)

  ;; Latin Extended Additional
  (setq c #x1e00)
  (while (<= c #x1ef9)
    (modify-category-entry (decode-char 'ucs c) ?l)
    (and (zerop (% c 2))
	 (or (<= c #x1e94) (>= c #x1ea0))
	 (set-case-syntax-pair
	  (decode-char 'ucs c) (decode-char 'ucs (1+ c)) tbl))
    (setq c (1+ c)))

  ;; Greek
  (setq c #x0370)
  (while (<= c #x03ff)
    (modify-category-entry (decode-char 'ucs c) ?g)
    (if (or (and (>= c #x0391) (<= c #x03a1))
	    (and (>= c #x03a3) (<= c #x03ab)))
	(set-case-syntax-pair
	 (decode-char 'ucs c) (decode-char 'ucs (+ c 32)) tbl))
    (and (>= c #x03da)
	 (<= c #x03ee)
	 (zerop (% c 2))
	 (set-case-syntax-pair
	  (decode-char 'ucs c) (decode-char 'ucs (1+ c)) tbl))
    (setq c (1+ c)))
  (set-case-syntax-pair ?$,1&f(B ?$,1',(B tbl)
  (set-case-syntax-pair ?$,1&h(B ?$,1'-(B tbl)
  (set-case-syntax-pair ?$,1&i(B ?$,1'.(B tbl)
  (set-case-syntax-pair ?$,1&j(B ?$,1'/(B tbl)
  (set-case-syntax-pair ?$,1&l(B ?$,1'L(B tbl)
  (set-case-syntax-pair ?$,1&n(B ?$,1'M(B tbl)
  (set-case-syntax-pair ?$,1&o(B ?$,1'N(B tbl)

  ;; Armenian
  (setq c #x531)
  (while (<= c #x556)
    (set-case-syntax-pair (decode-char 'ucs c)
			  (decode-char 'ucs (+ c #x30)) tbl)
    (setq c (1+ c)))

  ;; Greek Extended
  (setq c #x1f00)
  (while (<= c #x1fff)
    (modify-category-entry (decode-char 'ucs c) ?g)
    (and (<= (logand c #x000f) 7)
	 (<= c #x1fa7)
	 (not (memq c '(#x1f50 #x1f52 #x1f54 #x1f56)))
	 (/= (logand c #x00f0) 7)
	 (set-case-syntax-pair
	  (decode-char 'ucs (+ c 8)) (decode-char 'ucs c) tbl))
    (setq c (1+ c)))
  (set-case-syntax-pair ?$,1qx(B ?$,1qp(B tbl)
  (set-case-syntax-pair ?$,1qy(B ?$,1qq(B tbl)
  (set-case-syntax-pair ?$,1qz(B ?$,1q0(B tbl)
  (set-case-syntax-pair ?$,1q{(B ?$,1q1(B tbl)
  (set-case-syntax-pair ?$,1q|(B ?$,1qs(B tbl)
  (set-case-syntax-pair ?$,1r((B ?$,1q2(B tbl)
  (set-case-syntax-pair ?$,1r)(B ?$,1q3(B tbl)
  (set-case-syntax-pair ?$,1r*(B ?$,1q4(B tbl)
  (set-case-syntax-pair ?$,1r+(B ?$,1q5(B tbl)
  (set-case-syntax-pair ?$,1r,(B ?$,1r#(B tbl)
  (set-case-syntax-pair ?$,1r8(B ?$,1r0(B tbl)
  (set-case-syntax-pair ?$,1r9(B ?$,1r1(B tbl)
  (set-case-syntax-pair ?$,1r:(B ?$,1q6(B tbl)
  (set-case-syntax-pair ?$,1r;(B ?$,1q7(B tbl)
  (set-case-syntax-pair ?$,1rH(B ?$,1r@(B tbl)
  (set-case-syntax-pair ?$,1rI(B ?$,1rA(B tbl)
  (set-case-syntax-pair ?$,1rJ(B ?$,1q:(B tbl)
  (set-case-syntax-pair ?$,1rK(B ?$,1q;(B tbl)
  (set-case-syntax-pair ?$,1rL(B ?$,1rE(B tbl)
  (set-case-syntax-pair ?$,1rX(B ?$,1q8(B tbl)
  (set-case-syntax-pair ?$,1rY(B ?$,1q9(B tbl)
  (set-case-syntax-pair ?$,1rZ(B ?$,1q<(B tbl)
  (set-case-syntax-pair ?$,1r[(B ?$,1q=(B tbl)
  (set-case-syntax-pair ?$,1r\(B ?$,1rS(B tbl)

  ;; cyrillic
  (setq c #x0400)
  (while (<= c #x04ff)
    (modify-category-entry (decode-char 'ucs c) ?y)
    (and (>= c #x0400)
	 (<= c #x040f)
	 (set-case-syntax-pair
	  (decode-char 'ucs c) (decode-char 'ucs (+ c 80)) tbl))
    (and (>= c #x0410)
	 (<= c #x042f)
	 (set-case-syntax-pair
	  (decode-char 'ucs c) (decode-char 'ucs (+ c 32)) tbl))
    (and (zerop (% c 2))
	 (or (and (>= c #x0460) (<= c #x0480))
	     (and (>= c #x048c) (<= c #x04be))
	     (and (>= c #x04d0) (<= c #x04f4)))
	 (set-case-syntax-pair
	  (decode-char 'ucs c) (decode-char 'ucs (1+ c)) tbl))	 
    (setq c (1+ c)))
  (set-case-syntax-pair ?$,1*!(B ?$,1*"(B tbl)
  (set-case-syntax-pair ?$,1*#(B ?$,1*$(B tbl)
  (set-case-syntax-pair ?$,1*'(B ?$,1*((B tbl)
  (set-case-syntax-pair ?$,1*+(B ?$,1*,(B tbl)
  (set-case-syntax-pair ?$,1*X(B ?$,1*Y(B tbl)

  ;; general punctuation
  (setq c #x2000)
  (while (<= c #x200b)
    (set-case-syntax c " " tbl)
    (setq c (1+ c)))
  (setq c #x2010)
  (while (<= c #x2027)
    (set-case-syntax c "_" tbl)
    (setq c (1+ c)))

  ;; Roman numerals
  (setq c #x2160)
  (while (<= c #x216f)
    (set-case-syntax-pair (decode-char 'ucs c)
			  (decode-char 'ucs (+ c #x10)) tbl)
    (setq c (1+ c)))

  ;; Circled Latin
  (setq c #x24b6)
  (while (<= c #x24cf)
    (set-case-syntax-pair (decode-char 'ucs c)
			  (decode-char 'ucs (+ c 26)) tbl)
    (modify-category-entry (decode-char 'ucs c) ?l)
    (modify-category-entry (decode-char 'ucs (+ c 26)) ?l)
    (setq c (1+ c)))

  ;; Fullwidth Latin
  (setq c #xff21)
  (while (<= c #xff3a)
    (set-case-syntax-pair (decode-char 'ucs c)
			  (decode-char 'ucs (+ c #x20)) tbl)
    (modify-category-entry (decode-char 'ucs c) ?l)
    (modify-category-entry (decode-char 'ucs (+ c #x20)) ?l)
    (setq c (1+ c)))

  ;; Ohm, Kelvin, Angstrom
  (set-case-syntax-pair ?$,1uf(B ?$,1'I(B tbl)
;;;  These mess up the case conversion of k and ,Ae(B.
;;;  (set-case-syntax-pair ?$,1uj(B ?k tbl)
;;;  (set-case-syntax-pair ?$,1uk(B ?,Ae(B tbl)

  ;; Combining diacritics
  (setq c #x300)
  (while (<= c #x362)
    (modify-category-entry (decode-char 'ucs c) ?^)
    (setq c (1+ c)))

  ;; Combining marks
  (setq c #x20d0)
  (while (<= c #x20e3)
    (modify-category-entry (decode-char 'ucs c) ?^)
    (setq c (1+ c)))

  ;; Fixme: syntax for symbols &c
  )

;;; Setting word boundary.

(setq word-combining-categories
      '((?l . ?l)))

(setq word-separating-categories	;  (2-byte character sets)
      '((?A . ?K)			; Alpha numeric - Katakana
	(?A . ?C)			; Alpha numeric - Chinese
	(?H . ?A)			; Hiragana - Alpha numeric
	(?H . ?K)			; Hiragana - Katakana
	(?H . ?C)			; Hiragana - Chinese
	(?K . ?A)			; Katakana - Alpha numeric
	(?K . ?C)			; Katakana - Chinese
	(?C . ?A)			; Chinese - Alpha numeric
	(?C . ?K)			; Chinese - Katakana
	))


;; For each character set, put the information of the most proper
;; coding system to encode it by `preferred-coding-system' property.

(let ((l '((latin-iso8859-1	. iso-latin-1)
	   (latin-iso8859-2	. iso-latin-2)
	   (latin-iso8859-3	. iso-latin-3)
	   (latin-iso8859-4	. iso-latin-4)
	   (thai-tis620		. thai-tis620)
	   (greek-iso8859-7	. greek-iso-8bit)
	   (arabic-iso8859-6	. iso-2022-7bit)
	   (hebrew-iso8859-8	. hebrew-iso-8bit)
	   (katakana-jisx0201	. japanese-shift-jis)
	   (latin-jisx0201	. japanese-shift-jis)
	   (cyrillic-iso8859-5	. cyrillic-iso-8bit)
	   (latin-iso8859-9	. iso-latin-5)
	   (japanese-jisx0208-1978 . iso-2022-jp)
	   (chinese-gb2312	. cn-gb-2312)
	   (japanese-jisx0208	. iso-2022-jp)
	   (korean-ksc5601	. iso-2022-kr)
	   (japanese-jisx0212	. iso-2022-jp)
	   (chinese-cns11643-1	. iso-2022-cn)
	   (chinese-cns11643-2	. iso-2022-cn)
	   (chinese-big5-1	. chinese-big5)
	   (chinese-big5-2	. chinese-big5)
	   (chinese-sisheng	. iso-2022-7bit)
	   (ipa			. iso-2022-7bit)
	   (vietnamese-viscii-lower . vietnamese-viscii)
	   (vietnamese-viscii-upper . vietnamese-viscii)
	   (arabic-digit	. iso-2022-7bit)
	   (arabic-1-column	. iso-2022-7bit)
	   (lao			. lao)
	   (arabic-2-column	. iso-2022-7bit)
	   (indian-is13194	. devanagari)
	   (indian-glyph	. devanagari)
	   (tibetan-1-column	. tibetan)
	   (ethiopic		. iso-2022-7bit)
	   (chinese-cns11643-3	. iso-2022-cn)
	   (chinese-cns11643-4	. iso-2022-cn)
	   (chinese-cns11643-5	. iso-2022-cn)
	   (chinese-cns11643-6	. iso-2022-cn)
	   (chinese-cns11643-7	. iso-2022-cn)
	   (indian-2-column	. devanagari)
	   (tibetan		. tibetan)
	   (latin-iso8859-14	. iso-latin-8)
	   (latin-iso8859-15	. iso-latin-9))))
  (while l
    (put-charset-property (car (car l)) 'preferred-coding-system (cdr (car l)))
    (setq l (cdr l))))


;; Setup auto-fill-chars for charsets that should invoke auto-filling.
;; SPACE and NEWLINE are already set.  Also put `nospace-between-words'
;; property on the charsets.
(let ((l '(katakana-jisx0201
	   japanese-jisx0208 japanese-jisx0212
	   chinese-gb2312 chinese-big5-1 chinese-big5-2)))
  (while l
    ;;(aset auto-fill-chars (make-char (car l)) t)
    (put-charset-property (car l) 'nospace-between-words t)
    (setq l (cdr l))))
 

(set-char-table-range printable-chars '(0 . 31) nil)
(set-char-table-range printable-chars  '(127 . 159) nil)


;;; Local Variables:
;;; coding: iso-2022-7bit
;;; End:

;;; characters.el ends here
