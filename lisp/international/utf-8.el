;;; utf-8.el --- UTF-8 decoding/encoding support -*- coding: iso-2022-7bit -*-

;; Copyright (C) 2001, 2004 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 2001, 2002 Free Software Foundation, Inc.

;; Author: TAKAHASHI Naoto  <ntakahas@m17n.org>
;; Maintainer: FSF
;; Keywords: multilingual, Unicode, UTF-8, i18n

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

;; The coding-system `mule-utf-8' basically supports encoding/decoding
;; of the following character sets to and from UTF-8:
;;
;;   ascii
;;   eight-bit-control
;;   latin-iso8859-1
;;   mule-unicode-0100-24ff
;;   mule-unicode-2500-33ff
;;   mule-unicode-e000-ffff
;;
;; On decoding, Unicode characters that do not fit into the above
;; character sets are handled as `eight-bit-control' or
;; `eight-bit-graphic' characters to retain the information about the
;; original byte sequence and text properties record the corresponding
;; unicode.
;;
;; Fixme: note that reading and writing invalid utf-8 may not be
;; idempotent -- to represent the bytes to fix that needs a new charset.
;;
;; Characters from other character sets can be encoded with mule-utf-8
;; by populating the translation table
;; `utf-translation-table-for-encode'.  Hash tables
;; `utf-subst-table-for-decode' and `utf-subst-table-for-encode' are
;; used to support encoding and decoding of about a quarter of the CJK
;; space between U+3400 and U+DFFF.

;; UTF-8 is defined in RFC 3629.  A sketch of the encoding is:

;;        scalar       |               utf-8
;;        value        | 1st byte  | 2nd byte  | 3rd byte
;; --------------------+-----------+-----------+----------
;; 0000 0000 0xxx xxxx | 0xxx xxxx |           |
;; 0000 0yyy yyxx xxxx | 110y yyyy | 10xx xxxx |
;; zzzz yyyy yyxx xxxx | 1110 zzzz | 10yy yyyy | 10xx xxxx

;;; Code:

(defvar ucs-mule-to-mule-unicode (make-char-table 'translation-table nil)
  "Char table mapping characters to latin-iso8859-1 or mule-unicode-*.

If `unify-8859-on-encoding-mode' is non-nil, this table populates the
translation-table named `utf-translation-table-for-encode'.")

(define-translation-table 'utf-translation-table-for-encode)


;; Map Cyrillic and Greek to iso-8859 charsets, which take half the
;; space of mule-unicode.  For Latin scripts this isn't very
;; important.  Hebrew and Arabic might go here too when there's proper
;; support for them.

(defvar utf-fragmentation-table (make-char-table 'translation-table nil)
  "Char-table normally mapping non-Latin mule-unicode-* chars to iso-8859-*.

If `utf-fragment-on-decoding' is non-nil, this table populates the
translation-table named `utf-translation-table-for-decode'")

(defvar utf-defragmentation-table (make-char-table 'translation-table nil)
  "Char-table for reverse mapping of `utf-fragmentation-table'.

If `utf-fragment-on-decoding' is non-nil and
`unify-8859-on-encoding-mode' is nil, this table populates the
translation-table named `utf-translation-table-for-encode'")

(define-translation-table 'utf-translation-table-for-decode)


(defvar ucs-mule-cjk-to-unicode (make-hash-table :test 'eq)
  "Hash table mapping Emacs CJK character sets to Unicode code points.

If `utf-translate-cjk-mode' is non-nil, this table populates the
translation-hash-table named `utf-subst-table-for-encode'.")

(define-translation-hash-table 'utf-subst-table-for-encode
  ucs-mule-cjk-to-unicode)

(defvar ucs-unicode-to-mule-cjk (make-hash-table :test 'eq)
  "Hash table mapping Unicode code points to Emacs CJK character sets.

If `utf-translate-cjk-mode' is non-nil, this table populates the
translation-hash-table named `utf-subst-table-for-decode'.")

(define-translation-hash-table 'utf-subst-table-for-decode
  ucs-unicode-to-mule-cjk)

(mapc
 (lambda (pair)
   (aset utf-fragmentation-table (car pair) (cdr pair))
   (aset utf-defragmentation-table (cdr pair) (car pair)))
 '((?$,1&d(B . ?,F4(B) (?$,1&e(B . ?,F5(B) (?$,1&f(B . ?,F6(B) (?$,1&h(B . ?,F8(B) (?$,1&i(B . ?,F9(B)
   (?$,1&j(B . ?,F:(B) (?$,1&l(B . ?,F<(B) (?$,1&n(B . ?,F>(B) (?$,1&o(B . ?,F?(B) (?$,1&p(B . ?,F@(B)
   (?$,1&q(B . ?,FA(B) (?$,1&r(B . ?,FB(B) (?$,1&s(B . ?,FC(B) (?$,1&t(B . ?,FD(B) (?$,1&u(B . ?,FE(B)
   (?$,1&v(B . ?,FF(B) (?$,1&w(B . ?,FG(B) (?$,1&x(B . ?,FH(B) (?$,1&y(B . ?,FI(B) (?$,1&z(B . ?,FJ(B)
   (?$,1&{(B . ?,FK(B) (?$,1&|(B . ?,FL(B) (?$,1&}(B . ?,FM(B) (?$,1&~(B . ?,FN(B) (?$,1&(B . ?,FO(B)
   (?$,1' (B . ?,FP(B) (?$,1'!(B . ?,FQ(B) (?$,1'#(B . ?,FS(B) (?$,1'$(B . ?,FT(B) (?$,1'%(B . ?,FU(B)
   (?$,1'&(B . ?,FV(B) (?$,1''(B . ?,FW(B) (?$,1'((B . ?,FX(B) (?$,1')(B . ?,FY(B) (?$,1'*(B . ?,FZ(B)
   (?$,1'+(B . ?,F[(B) (?$,1',(B . ?,F\(B) (?$,1'-(B . ?,F](B) (?$,1'.(B . ?,F^(B) (?$,1'/(B . ?,F_(B)
   (?$,1'0(B . ?,F`(B) (?$,1'1(B . ?,Fa(B) (?$,1'2(B . ?,Fb(B) (?$,1'3(B . ?,Fc(B) (?$,1'4(B . ?,Fd(B)
   (?$,1'5(B . ?,Fe(B) (?$,1'6(B . ?,Ff(B) (?$,1'7(B . ?,Fg(B) (?$,1'8(B . ?,Fh(B) (?$,1'9(B . ?,Fi(B)
   (?$,1':(B . ?,Fj(B) (?$,1';(B . ?,Fk(B) (?$,1'<(B . ?,Fl(B) (?$,1'=(B . ?,Fm(B) (?$,1'>(B . ?,Fn(B)
   (?$,1'?(B . ?,Fo(B) (?$,1'@(B . ?,Fp(B) (?$,1'A(B . ?,Fq(B) (?$,1'B(B . ?,Fr(B) (?$,1'C(B . ?,Fs(B)
   (?$,1'D(B . ?,Ft(B) (?$,1'E(B . ?,Fu(B) (?$,1'F(B . ?,Fv(B) (?$,1'G(B . ?,Fw(B) (?$,1'H(B . ?,Fx(B)
   (?$,1'I(B . ?,Fy(B) (?$,1'J(B . ?,Fz(B) (?$,1'K(B . ?,F{(B) (?$,1'L(B . ?,F|(B) (?$,1'M(B . ?,F}(B)
   (?$,1'N(B . ?,F~(B)

   (?$,1(!(B . ?,L!(B) (?$,1("(B . ?,L"(B) (?$,1(#(B . ?,L#(B) (?$,1($(B . ?,L$(B)
   (?$,1(%(B . ?,L%(B) (?$,1(&(B . ?,L&(B) (?$,1('(B . ?,L'(B) (?$,1(((B . ?,L((B) (?$,1()(B . ?,L)(B)
   (?$,1(*(B . ?,L*(B) (?$,1(+(B . ?,L+(B) (?$,1(,(B . ?,L,(B) (?$,1(.(B . ?,L.(B) (?$,1(/(B . ?,L/(B)
   (?$,1(0(B . ?,L0(B) (?$,1(1(B . ?,L1(B) (?$,1(2(B . ?,L2(B) (?$,1(3(B . ?,L3(B) (?$,1(4(B . ?,L4(B)
   (?$,1(5(B . ?,L5(B) (?$,1(6(B . ?,L6(B) (?$,1(7(B . ?,L7(B) (?$,1(8(B . ?,L8(B) (?$,1(9(B . ?,L9(B)
   (?$,1(:(B . ?,L:(B) (?$,1(;(B . ?,L;(B) (?$,1(<(B . ?,L<(B) (?$,1(=(B . ?,L=(B) (?$,1(>(B . ?,L>(B)
   (?$,1(?(B . ?,L?(B) (?$,1(@(B . ?,L@(B) (?$,1(A(B . ?,LA(B) (?$,1(B(B . ?,LB(B) (?$,1(C(B . ?,LC(B)
   (?$,1(D(B . ?,LD(B) (?$,1(E(B . ?,LE(B) (?$,1(F(B . ?,LF(B) (?$,1(G(B . ?,LG(B) (?$,1(H(B . ?,LH(B)
   (?$,1(I(B . ?,LI(B) (?$,1(J(B . ?,LJ(B) (?$,1(K(B . ?,LK(B) (?$,1(L(B . ?,LL(B) (?$,1(M(B . ?,LM(B)
   (?$,1(N(B . ?,LN(B) (?$,1(O(B . ?,LO(B) (?$,1(P(B . ?,LP(B) (?$,1(Q(B . ?,LQ(B) (?$,1(R(B . ?,LR(B)
   (?$,1(S(B . ?,LS(B) (?$,1(T(B . ?,LT(B) (?$,1(U(B . ?,LU(B) (?$,1(V(B . ?,LV(B) (?$,1(W(B . ?,LW(B)
   (?$,1(X(B . ?,LX(B) (?$,1(Y(B . ?,LY(B) (?$,1(Z(B . ?,LZ(B) (?$,1([(B . ?,L[(B) (?$,1(\(B . ?,L\(B)
   (?$,1(](B . ?,L](B) (?$,1(^(B . ?,L^(B) (?$,1(_(B . ?,L_(B) (?$,1(`(B . ?,L`(B) (?$,1(a(B . ?,La(B)
   (?$,1(b(B . ?,Lb(B) (?$,1(c(B . ?,Lc(B) (?$,1(d(B . ?,Ld(B) (?$,1(e(B . ?,Le(B) (?$,1(f(B . ?,Lf(B)
   (?$,1(g(B . ?,Lg(B) (?$,1(h(B . ?,Lh(B) (?$,1(i(B . ?,Li(B) (?$,1(j(B . ?,Lj(B) (?$,1(k(B . ?,Lk(B)
   (?$,1(l(B . ?,Ll(B) (?$,1(m(B . ?,Lm(B) (?$,1(n(B . ?,Ln(B) (?$,1(o(B . ?,Lo(B) (?$,1(q(B . ?,Lq(B)
   (?$,1(r(B . ?,Lr(B) (?$,1(s(B . ?,Ls(B) (?$,1(t(B . ?,Lt(B) (?$,1(u(B . ?,Lu(B) (?$,1(v(B . ?,Lv(B)
   (?$,1(w(B . ?,Lw(B) (?$,1(x(B . ?,Lx(B) (?$,1(y(B . ?,Ly(B) (?$,1(z(B . ?,Lz(B) (?$,1({(B . ?,L{(B)
   (?$,1(|(B . ?,L|(B) (?$,1(~(B . ?,L~(B) (?$,1((B . ?,L(B)))


(defcustom utf-fragment-on-decoding nil
  "Whether or not to decode some chars in UTF-8/16 text into iso8859 charsets.
Setting this means that the relevant Cyrillic and Greek characters are
decoded into the iso8859 charsets rather than into
mule-unicode-0100-24ff.  The iso8859 charsets take half as much space
in the buffer, but using them may affect how the buffer can be re-encoded
and may require a different input method to search for them, for instance.
See `unify-8859-on-decoding-mode' and `unify-8859-on-encoding-mode'
for mechanisms to make this largely transparent.

Setting this variable outside customize has no effect."
  :set (lambda (s v)
	 (if v
	     (progn
	       (define-translation-table 'utf-translation-table-for-decode
		 utf-fragmentation-table)
	       ;; Even if unify-8859-on-encoding-mode is off, make
	       ;; mule-utf-* encode characters in
	       ;; utf-fragmentation-table.
	       (unless (eq (get 'utf-translation-table-for-encode
				'translation-table)
			   ucs-mule-to-mule-unicode)
		 (define-translation-table 'utf-translation-table-for-encode
		   utf-defragmentation-table)))
	   (define-translation-table 'utf-translation-table-for-decode)
	   ;; When unify-8859-on-encoding-mode is off, be sure to make
	   ;; mule-utf-* disabled for characters in
	   ;; utf-fragmentation-table.
	   (unless (eq (get 'utf-translation-table-for-encode
			    'translation-table)
		       ucs-mule-to-mule-unicode)
	     (define-translation-table 'utf-translation-table-for-encode)))
	 (set-default s v))
  :version "21.4"
  :type 'boolean
  :group 'mule)

(define-minor-mode utf-translate-cjk-mode
  "Whether the UTF based coding systems should decode/encode CJK characters.
Enabling this loads tables which allow the coding systems mule-utf-8,
mule-utf-16le and mule-utf-16be to encode characters in the charsets
`korean-ksc5601', `chinese-gb2312', `chinese-big5-1',
`chinese-big5-2', `japanese-jisx0208' and `japanese-jisx0212', and to
decode the corresponding unicodes into such characters.

Where the charsets overlap, the one preferred for decoding is chosen
according to the language environment in effect when this option is
turned on: ksc5601 for Korean, gb2312 for Chinese-GB, big5 for
Chinese-Big5 and jisx for other environments.

The tables are large (over 40000 entries), so this option is not the
default.  Also, installing them may be rather slow."
  :init-value nil
  :version "21.4"
  :type 'boolean
  :set-after '(current-language-environment)
  :group 'mule
  :global t
  (if utf-translate-cjk-mode
      ;; Fixme: Allow the use of the CJK charsets to be
      ;; customized by reordering and possible omission.
      (progn
	;; Redefine them with realistic initial sizes and a
	;; smallish rehash size to avoid wasting significant
	;; space after they're built.
	(setq ucs-mule-cjk-to-unicode
	      (make-hash-table :test 'eq :size 43000 :rehash-size 1000)
	      ucs-unicode-to-mule-cjk
	      (make-hash-table :test 'eq :size 21500 :rehash-size 1000))
	;; Load the files explicitly, to avoid having to keep
	;; around the large tables they contain (as well as the
	;; ones which get built).
	(cond
	 ((string= "Korean" current-language-environment)
	  (load "subst-jis")
	  (load "subst-big5")
	  (load "subst-gb2312")
	  (load "subst-ksc"))
	 ((string= "Chinese-BIG5" current-language-environment)
	  (load "subst-jis")
	  (load "subst-ksc")
	  (load "subst-gb2312")
	  (load "subst-big5"))
	 ((string= "Chinese-GB" current-language-environment)
	  (load "subst-jis")
	  (load "subst-ksc")
	  (load "subst-big5")
	  (load "subst-gb2312"))
	 (t
	  (load "subst-ksc")
	  (load "subst-gb2312")
	  (load "subst-big5")
	  (load "subst-jis")))	  ; jis covers as much as big5, gb2312
	(define-translation-hash-table 'utf-subst-table-for-decode
	  ucs-unicode-to-mule-cjk)
	(define-translation-hash-table 'utf-subst-table-for-encode
	  ucs-mule-cjk-to-unicode)
	(set-char-table-extra-slot (get 'utf-translation-table-for-encode
					'translation-table)
				   1 ucs-mule-cjk-to-unicode))
    (define-translation-hash-table 'utf-subst-table-for-decode
      (make-hash-table :test 'eq))
    (define-translation-hash-table 'utf-subst-table-for-encode
      (make-hash-table :test 'eq))
    (set-char-table-extra-slot (get 'utf-translation-table-for-encode
				    'translation-table)
			       1 nil)))

(define-ccl-program ccl-decode-mule-utf-8
  ;;
  ;;        charset         | bytes in utf-8 | bytes in emacs
  ;; -----------------------+----------------+---------------
  ;;         ascii          |       1        |       1
  ;; -----------------------+----------------+---------------
  ;;    eight-bit-control   |       2        |       2
  ;;    eight-bit-graphic   |       2        |       1
  ;;     latin-iso8859-1    |       2        |       2
  ;; -----------------------+----------------+---------------
  ;; mule-unicode-0100-24ff |       2        |       4
  ;;        (< 0800)        |                |
  ;; -----------------------+----------------+---------------
  ;; mule-unicode-0100-24ff |       3        |       4
  ;;        (>= 8000)       |                |
  ;; mule-unicode-2500-33ff |       3        |       4
  ;; mule-unicode-e000-ffff |       3        |       4
  ;;
  ;; Thus magnification factor is two.
  ;;
  `(2
    ((r5 = ,(charset-id 'eight-bit-control))
     (r6 = ,(charset-id 'eight-bit-graphic))
     (loop
      (r0 = -1)
      (read r0)

      ;; 1byte encoding, i.e., ascii
      (if (r0 < #x80)
	  ((write r0))
	(if (r0 < #xc0)		    ; continuation byte (invalid here)
	    ((if (r0 < #xa0)
		 (write-multibyte-character r5 r0)
	       (write-multibyte-character r6 r0)))
	  ;; 2 byte encoding 00000yyyyyxxxxxx = 110yyyyy 10xxxxxx
	  (if (r0 < #xe0)
	      ((r1 = -1)
	       (read r1)

	       (if ((r1 & #b11000000) != #b10000000)
		   ;; Invalid 2-byte sequence
		   ((if (r0 < #xa0)
			(write-multibyte-character r5 r0)
		      (write-multibyte-character r6 r0))
		    (if (r1 < #x80)
			(write r1)
		      (if (r1 < #xa0)
			  (write-multibyte-character r5 r1)
			(write-multibyte-character r6 r1))))

		 ((r3 = r0)	   ; save in case of overlong sequence
		  (r2 = r1)
		  (r0 &= #x1f)
		  (r0 <<= 6)
		  (r1 &= #x3f)
		  (r1 += r0)
		  ;; Now r1 holds scalar value

		  (if (r1 < 128)	; `overlong sequence'
		      ((if (r3 < #xa0)
			   (write-multibyte-character r5 r3)
			 (write-multibyte-character r6 r3))
		       (if (r2 < #x80)
			   (write r2)
			 (if (r2 < #xa0)
			     (write-multibyte-character r5 r2)
			   (write-multibyte-character r6 r2))))

		    ;; eight-bit-control
		    (if (r1 < 160)
			((write-multibyte-character r5 r1))

		      ;; latin-iso8859-1
		      (if (r1 < 256)
			  ((r0 = ,(charset-id 'latin-iso8859-1))
			   (r1 -= 128)
			   (write-multibyte-character r0 r1))

			;; mule-unicode-0100-24ff (< 0800)
			((r0 = ,(charset-id 'mule-unicode-0100-24ff))
			 (r1 -= #x0100)
			 (r2 = (((r1 / 96) + 32) << 7))
			 (r1 %= 96)
			 (r1 += (r2 + 32))
			 (translate-character
			  utf-translation-table-for-decode r0 r1)
			 (write-multibyte-character r0 r1))))))))

	    ;; 3byte encoding
	    ;; zzzzyyyyyyxxxxxx = 1110zzzz 10yyyyyy 10xxxxxx
	    (if (r0 < #xf0)
		((r1 = -1)
		 (r2 = -1)
		 (read r1 r2)

		 ;; This is set to 1 if the encoding is invalid.
		 (r4 = 0)

		 (r3 = (r1 & #b11000000))
		 (r3 |= ((r2 >> 2) & #b00110000))
		 (if (r3 != #b10100000)
		     (r4 = 1)
		   ((r3 = ((r0 & #x0f) << 12))
		    (r3 += ((r1 & #x3f) << 6))
		    (r3 += (r2 & #x3f))
		    (if (r3 < #x0800)
			(r4 = 1))))

		 (if (r4 != 0)
		     ;; Invalid 3-byte sequence
		     ((if (r0 < #xa0)
			  (write-multibyte-character r5 r0)
			(write-multibyte-character r6 r0))
		      (if (r1 < #x80)
			  (write r1)
			(if (r1 < #xa0)
			    (write-multibyte-character r5 r1)
			  (write-multibyte-character r6 r1)))
		      (if (r2 < #x80)
			  (write r2)
			(if (r2 < #xa0)
			    (write-multibyte-character r5 r2)
			  (write-multibyte-character r6 r2))))

		   ;; mule-unicode-0100-24ff (>= 0800)
		   ((if (r3 < #x2500)
			((r0 = ,(charset-id 'mule-unicode-0100-24ff))
			 (r3 -= #x0100)
			 (r3 //= 96)
			 (r1 = (r7 + 32))
			 (r1 += ((r3 + 32) << 7))
			 (translate-character
			  utf-translation-table-for-decode r0 r1)
			 (write-multibyte-character r0 r1))

		      ;; mule-unicode-2500-33ff
		      (if (r3 < #x3400)
			  ((r4 = r3)	; don't zap r3
			   (lookup-integer utf-subst-table-for-decode r4 r5)
			   (if r7
			       ;; got a translation
			       ((write-multibyte-character r4 r5)
				;; Zapped through register starvation.
				(r5 = ,(charset-id 'eight-bit-control)))
			     ((r0 = ,(charset-id 'mule-unicode-2500-33ff))
			      (r3 -= #x2500)
			      (r3 //= 96)
			      (r1 = (r7 + 32))
			      (r1 += ((r3 + 32) << 7))
			      (write-multibyte-character r0 r1))))

			;; U+3400 .. U+D7FF
			;; Try to convert to CJK chars, else keep
			;; them as eight-bit-{control|graphic}.
			(if (r3 < #xd800)
			    ((r4 = r3)	; don't zap r3
			     (lookup-integer utf-subst-table-for-decode r4 r5)
			     (if r7
				 ;; got a translation
				 ((write-multibyte-character r4 r5)
				  ;; Zapped through register starvation.
				  (r5 = ,(charset-id 'eight-bit-control)))
			       ;; #xe0 <= r0 < #xf0, so r0 is eight-bit-graphic
			       ((r3 = r6)
				(write-multibyte-character r3 r0)
				(if (r1 < #xa0)
				    (r3 = r5))
				(write-multibyte-character r3 r1)
				(if (r2 < #xa0)
				    (r3 = r5)
				  (r3 = r6))
				(write-multibyte-character r3 r2))))

			  ;; Surrogates, U+D800 .. U+DFFF
			  (if (r3 < #xe000)
			      ((r3 = r6)
			       (write-multibyte-character r3 r0) ; eight-bit-graphic
			       (if (r1 < #xa0)
				   (r3 = r5))
			       (write-multibyte-character r3 r1)
			       (if (r2 < #xa0)
				   (r3 = r5)
				 (r3 = r6))
			       (write-multibyte-character r3 r2))

			    ;; mule-unicode-e000-ffff
			    ;; Fixme: fffe and ffff are invalid.
			    ((r4 = r3)	; don't zap r3
			     (lookup-integer utf-subst-table-for-decode r4 r5)
			     (if r7
				 ;; got a translation
				 ((write-multibyte-character r4 r5)
				  ;; Zapped through register starvation.
				  (r5 = ,(charset-id 'eight-bit-control)))
			       ((r0 = ,(charset-id 'mule-unicode-e000-ffff))
				(r3 -= #xe000)
				(r3 //= 96)
				(r1 = (r7 + 32))
				(r1 += ((r3 + 32) << 7))
				(write-multibyte-character r0 r1)))))))))))

	      (if (r0 < #xfe)
		  ;; 4byte encoding
		  ;; keep those bytes as eight-bit-{control|graphic}
		  ;; Fixme: allow lookup in utf-subst-table-for-decode.
		  ((r1 = -1)
		   (r2 = -1)
		   (r3 = -1)
		   (read r1 r2 r3)
		   ;; r0 > #xf0, thus eight-bit-graphic
		   (write-multibyte-character r6 r0)
		   (if (r1 < #xa0)
		       (if (r1 < #x80)	; invalid byte
			   (write r1)
			 (write-multibyte-character r5 r1))
		     (write-multibyte-character r6 r1))
		   (if (r2 < #xa0)
		       (if (r2 < #x80)	; invalid byte
			   (write r2)
			 (write-multibyte-character r5 r2))
		     (write-multibyte-character r6 r2))
		   (if (r3 < #xa0)
		       (if (r3 < #x80)	; invalid byte
			   (write r3)
			 (write-multibyte-character r5 r3))
		     (write-multibyte-character r6 r3))
		   (if (r0 >= #xf8)	; 5- or 6-byte encoding
		       ((r0 = -1)
			(read r0)
			(if (r0 < #xa0)
			    (if (r0 < #x80) ; invalid byte
				(write r0)
			      (write-multibyte-character r5 r0))
			  (write-multibyte-character r6 r0))
			(if (r0 >= #xfc) ; 6-byte
			    ((r0 = -1)
			     (read r0)
			     (if (r0 < #xa0)
				 (if (r0 < #x80) ; invalid byte
				     (write r0)
				   (write-multibyte-character r5 r0))
			       (write-multibyte-character r6 r0)))))))
		;; else invalid byte >= #xfe
		(write-multibyte-character r6 r0))))))
      (repeat)))

    ;; At EOF...
    (if (r0 >= 0)
	((if (r0 < #x80)
	     (write r0)
	   (if (r0 < #xa0)
	       (write-multibyte-character r5 r0)
	     ((write-multibyte-character r6 r0))))
	 (if (r1 >= 0)
	     ((if (r1 < #x80)
		  (write r1)
		(if (r1 < #xa0)
		    (write-multibyte-character r5 r1)
		  ((write-multibyte-character r6 r1))))
	      (if (r2 >= 0)
		  ((if (r2 < #x80)
		       (write r2)
		     (if (r2 < #xa0)
			 (write-multibyte-character r5 r2)
		       ((write-multibyte-character r6 r2))))
		   (if (r3 >= 0)
		       (if (r3 < #x80)
			   (write r3)
			 (if (r3 < #xa0)
			     (write-multibyte-character r5 r3)
			   ((write-multibyte-character r6 r3))))))))))))

  "CCL program to decode UTF-8.
Basic decoding is done into the charsets ascii, latin-iso8859-1 and
mule-unicode-*, but see also `utf-fragmentation-table' and
`ucs-mule-cjk-to-unicode'.
Encodings of un-representable Unicode characters are decoded asis into
eight-bit-control and eight-bit-graphic characters.")

(define-ccl-program ccl-encode-mule-utf-8
  `(1
    ((r5 = -1)
     (loop
      (if (r5 < 0)
	  ((r1 = -1)
	   (read-multibyte-character r0 r1)
	   (translate-character utf-translation-table-for-encode r0 r1))
	(;; We have already done read-multibyte-character.
	 (r0 = r5)
	 (r1 = r6)
	 (r5 = -1)))

      (if (r0 == ,(charset-id 'ascii))
	  (write r1)

	(if (r0 == ,(charset-id 'latin-iso8859-1))
	    ;; r1          scalar                  utf-8
	    ;;       0000 0yyy yyxx xxxx    110y yyyy 10xx xxxx
	    ;; 20    0000 0000 1010 0000    1100 0010 1010 0000
	    ;; 7f    0000 0000 1111 1111    1100 0011 1011 1111
	    ((r0 = (((r1 & #x40) >> 6) | #xc2))
	     (r1 &= #x3f)
	     (r1 |= #x80)
	     (write r0 r1))

	  (if (r0 == ,(charset-id 'mule-unicode-0100-24ff))
	      ((r0 = ((((r1 & #x3f80) >> 7) - 32) * 96))
	       ;; #x3f80 == (0011 1111 1000 0000)b
	       (r1 &= #x7f)
	       (r1 += (r0 + 224))	; 240 == -32 + #x0100
	       ;; now r1 holds scalar value
	       (if (r1 < #x0800)
		   ;; 2byte encoding
		   ((r0 = (((r1 & #x07c0) >> 6) | #xc0))
		    ;; #x07c0 == (0000 0111 1100 0000)b
		    (r1 &= #x3f)
		    (r1 |= #x80)
		    (write r0 r1))
		 ;; 3byte encoding
		 ((r0 = (((r1 & #xf000) >> 12) | #xe0))
		  (r2 = ((r1 & #x3f) | #x80))
		  (r1 &= #x0fc0)
		  (r1 >>= 6)
		  (r1 |= #x80)
		  (write r0 r1 r2))))

	    (if (r0 == ,(charset-id 'mule-unicode-2500-33ff))
		((r0 = ((((r1 & #x3f80) >> 7) - 32) * 96))
		 (r1 &= #x7f)
		 (r1 += (r0 + 9440))	; 9440 == -32 + #x2500
		 (r0 = (((r1 & #xf000) >> 12) | #xe0))
		 (r2 = ((r1 & #x3f) | #x80))
		 (r1 &= #x0fc0)
		 (r1 >>= 6)
		 (r1 |= #x80)
		 (write r0 r1 r2))

	      (if (r0 == ,(charset-id 'mule-unicode-e000-ffff))
		  ((r0 = ((((r1 & #x3f80) >> 7) - 32) * 96))
		   (r1 &= #x7f)
		   (r1 += (r0 + 57312))	; 57312 == -32 + #xe000
		   (r0 = (((r1 & #xf000) >> 12) | #xe0))
		   (r2 = ((r1 & #x3f) | #x80))
		   (r1 &= #x0fc0)
		   (r1 >>= 6)
		   (r1 |= #x80)
		   (write r0 r1 r2))

		(if (r0 == ,(charset-id 'eight-bit-control))
		    ;; r1          scalar                  utf-8
		    ;;       0000 0yyy yyxx xxxx    110y yyyy 10xx xxxx
		    ;; 80    0000 0000 1000 0000    1100 0010 1000 0000
		    ;; 9f    0000 0000 1001 1111    1100 0010 1001 1111
		    ((write #xc2)
		     (write r1))

		  (if (r0 == ,(charset-id 'eight-bit-graphic))
		      ;; r1          scalar                  utf-8
		      ;;       0000 0yyy yyxx xxxx    110y yyyy 10xx xxxx
		      ;; a0    0000 0000 1010 0000    1100 0010 1010 0000
		      ;; ff    0000 0000 1111 1111    1101 1111 1011 1111
		      ((write r1)
		       (r1 = -1)
		       (read-multibyte-character r0 r1)
		       (if (r0 != ,(charset-id 'eight-bit-graphic))
			   (if (r0 != ,(charset-id 'eight-bit-control))
			       ((r5 = r0)
				(r6 = r1))))
		       (if (r5 < 0)
			   ((read-multibyte-character r0 r2)
			    (if (r0 != ,(charset-id 'eight-bit-graphic))
				(if (r0 != ,(charset-id 'eight-bit-control))
				    ((r5 = r0)
				     (r6 = r2))))
			    (if (r5 < 0)
				(write r1 r2)
			      (if (r1 < #xa0)
				  (write r1)
				((write #xc2)
				 (write r1)))))))

		    ((lookup-character utf-subst-table-for-encode r0 r1)
		     (if r7		; lookup succeeded
			 ((r1 = (((r0 & #xf000) >> 12) | #xe0))
			  (r2 = ((r0 & #x3f) | #x80))
			  (r0 &= #x0fc0)
			  (r0 >>= 6)
			  (r0 |= #x80)
			  (write r1 r0 r2))
		       ;; Unsupported character.
		       ;; Output U+FFFD, which is `ef bf bd' in UTF-8.
		       ((write #xef)
			(write #xbf)
			(write #xbd)))))))))))
      (repeat)))
    (if (r1 >= #xa0)
	(write r1)
      (if (r1 >= #x80)
	  ((write #xc2)
	   (write r1)))))

  "CCL program to encode into UTF-8.")


(define-ccl-program ccl-untranslated-to-ucs
  `(0
    (if (r0 < #xf0)			; 3-byte encoding, as above
	((r4 = 0)
	 (r3 = (r1 & #b11000000))
	 (r3 |= ((r2 >> 2) & #b00110000))
	 (if (r3 != #b10100000)
	     (r4 = 1)
	   ((r3 = ((r0 & #x0f) << 12))
	    (r3 += ((r1 & #x3f) << 6))
	    (r3 += (r2 & #x3f))
	    (if (r3 < #x0800)
		(r4 = 1))))
	 (if (r4 != 0)
	     (r0 = 0)
	   (r0 = r3)))
      (if (r0 < #xf8)			; 4-byte (Mule-UCS recipe)
	  ((r4 = (r1 >> 6))
	   (if (r4 != #b10)
	       (r0 = 0)
	     ((r4 = (r2 >> 6))
	      (if (r4 != #b10)
		  (r0 = 0)
		((r4 = (r3 >> 6))
		 (if (r4 != #b10)
		     (r0 = 0)
		   ((r1 = ((r1  & #x3F) << 12))
		    (r2 = ((r2  & #x3F) << 6))
		    (r3 &= #x3F)
		    (r0 = (((((r0 & #x07) << 18) | r1) | r2) | r3)))))))))
	(r0 = 0))))
  "Decode 3- or 4-byte sequences in r0, r1, r2 [,r3] to unicodes in r0.
r0 == 0 for invalid sequence.")

(defvar utf-8-ccl-regs (make-vector 8 0))

(defsubst utf-8-untranslated-to-ucs ()
  "Return the UCS code for an untranslated sequence of raw bytes t point.
Only for 3- or 4-byte sequences."
  (aset utf-8-ccl-regs 0 (or (char-after) 0))
  (aset utf-8-ccl-regs 1 (or (char-after (1+ (point))) 0))
  (aset utf-8-ccl-regs 2 (or (char-after (+ 2 (point))) 0))
  (aset utf-8-ccl-regs 3 (or (char-after (+ 3 (point))) 0))
  (ccl-execute 'ccl-untranslated-to-ucs utf-8-ccl-regs)
  (aref utf-8-ccl-regs 0))

(defun utf-8-help-echo (window object position)
  (format "Untranslated Unicode U+%04X"
	  (get-char-property position 'untranslated-utf-8 object)))

;; We compose the untranslatable sequences into a single character.
;; This is infelicitous for editing, because there's currently no
;; mechanism for treating compositions as atomic, but is OK for
;; display.  They are composed to U+FFFD with help-echo which
;; indicates the unicodes they represent.  This function GCs too much.
(defsubst utf-8-compose ()
  "Put a suitable composition on an untranslatable sequence.
Return the sequence's length."
  (let* ((u (utf-8-untranslated-to-ucs))
	 (l (unless (zerop u)
	      (if (>= u #x10000)
		       4
		     3))))
    (when l
      (put-text-property (point) (min (point-max) (+ l (point)))
			 'untranslated-utf-8 u)
      (put-text-property (point) (min (point-max) (+ l (point)))
			 'help-echo 'utf-8-help-echo)
      (compose-region (point) (+ l (point)) ?$,3u=(B)
      l)))

(defcustom utf-8-compose-scripts nil
  "*Non-nil means compose various scripts on decoding utf-8 text."
  :group 'mule
  :version "21.4"
  :type 'boolean)

(defun utf-8-post-read-conversion (length)
  "Compose untranslated utf-8 sequences into single characters.
Also compose particular scripts if `utf-8-compose-scripts' is non-nil."
  (save-excursion
    ;; Can't do eval-when-compile to insert a multibyte constant
    ;; version of the string in the loop, since it's always loaded as
    ;; unibyte from a byte-compiled file.
    (let ((range (string-as-multibyte "^\xe1-\xf7")))
      (while (and (skip-chars-forward range)
		  (not (eobp)))
	(forward-char (utf-8-compose)))))
  ;; Fixme: Takahashi-san implies it may not work this easily.  I
  ;; asked why but didn't get a reply. -- fx
  (when (and utf-8-compose-scripts (> length 1))
    ;; These currently have definitions which cover the relevant
    ;; unicodes.  We could avoid loading thai-util &c by checking
    ;; whether the region contains any characters with the appropriate
    ;; categories.  There aren't yet Unicode-based rules for Tibetan.
    (save-excursion (setq length (diacritic-post-read-conversion length)))
    (save-excursion (setq length (thai-post-read-conversion length)))
    (save-excursion (setq length (lao-post-read-conversion length)))
    (save-excursion (setq length (devanagari-post-read-conversion length)))
    (save-excursion (setq length (malayalam-post-read-conversion length)))
    (save-excursion (setq length (tamil-post-read-conversion length))))
  length)

;; ucs-tables is preloaded
;; (defun utf-8-pre-write-conversion (beg end)
;;   "Semi-dummy pre-write function effectively to autoload ucs-tables."
;;   ;; Ensure translation-table is loaded.
;;   (require 'ucs-tables)
;;   ;; Don't do this again.
;;   (coding-system-put 'mule-utf-8 'pre-write-conversion nil)
;;   nil)

(make-coding-system
 'mule-utf-8 4 ?u
 "UTF-8 encoding for Emacs-supported Unicode characters.
It supports Unicode characters of these ranges:
    U+0000..U+33FF, U+E000..U+FFFF.
They correspond to these Emacs character sets:
    ascii, latin-iso8859-1, mule-unicode-0100-24ff,
    mule-unicode-2500-33ff, mule-unicode-e000-ffff

On decoding (e.g. reading a file), Unicode characters not in the above
ranges are decoded into sequences of eight-bit-control and
eight-bit-graphic characters to preserve their byte sequences.  The
byte sequence is preserved on i/o for valid utf-8, but not necessarily
for invalid utf-8.

On encoding (e.g. writing a file), Emacs characters not belonging to
any of the character sets listed above are encoded into the UTF-8 byte
sequence representing U+FFFD (REPLACEMENT CHARACTER)."

 '(ccl-decode-mule-utf-8 . ccl-encode-mule-utf-8)
 '((safe-charsets
    ascii
    eight-bit-control
    eight-bit-graphic
    latin-iso8859-1
    mule-unicode-0100-24ff
    mule-unicode-2500-33ff
    mule-unicode-e000-ffff)
   (mime-charset . utf-8)
   (coding-category . coding-category-utf-8)
   (valid-codes (0 . 255))
;;    (pre-write-conversion . utf-8-pre-write-conversion)
   (post-read-conversion . utf-8-post-read-conversion)
   (translation-table-for-encode . utf-translation-table-for-encode)
   (dependency unify-8859-on-encoding-mode
	       unify-8859-on-decoding-mode
	       utf-fragment-on-decoding
	       utf-translate-cjk-mode)))

(define-coding-system-alias 'utf-8 'mule-utf-8)

;; I think this needs special private charsets defined for the
;; untranslated sequences, if it's going to work well.

;;; (defun utf-8-compose-function (pos to pattern &optional string)
;;;   (let* ((prop (get-char-property pos 'composition string))
;;; 	 (l (and prop (- (cadr prop) (car prop)))))
;;;     (cond ((and l (> l (- to pos)))
;;; 	   (delete-region pos to))
;;; 	  ((and (> (char-after pos) 224)
;;; 		(< (char-after pos) 256)
;;; 		(save-restriction
;;; 		  (narrow-to-region pos to)
;;; 		  (utf-8-compose)))
;;; 	   t))))

;;; (dotimes (i 96)
;;;   (aset composition-function-table
;;; 	(+ 128 i)
;;; 	`((,(string-as-multibyte "[\200-\237\240-\377]")
;;; 	   . utf-8-compose-function))))

;;; arch-tag: b08735b7-753b-4ae6-b754-0f3efe4515c5
;;; utf-8.el ends here
