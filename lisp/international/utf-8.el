;;; utf-8.el --- UTF-8 decoding/encoding support -*- coding: iso-2022-7bit -*-

;; Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007
;;   Free Software Foundation, Inc.
;; Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

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
  :version "22.1"
  :type 'boolean
  :group 'mule)


(defconst utf-translate-cjk-charsets '(chinese-gb2312
				       chinese-big5-1 chinese-big5-2
				       japanese-jisx0208 japanese-jisx0212
				       katakana-jisx0201
				       korean-ksc5601)
  "List of charsets supported by `utf-translate-cjk-mode'.")

(defvar utf-translate-cjk-lang-env nil
  "Language environment in which tables for `utf-translate-cjk-mode' is loaded.
The value nil means that the tables are not yet loaded.")

(defvar utf-translate-cjk-unicode-range)

;; String generated from utf-translate-cjk-unicode-range.  It is
;; suitable for an argument to skip-chars-forward.
(defvar utf-translate-cjk-unicode-range-string nil)

(defun utf-translate-cjk-set-unicode-range (range)
  (setq utf-translate-cjk-unicode-range range)
  (setq utf-translate-cjk-unicode-range-string
	(let ((decode-char-no-trans
	       #'(lambda (x)
		   (cond ((< x #x100) (make-char 'latin-iso8859-1 x))
			 ((< x #x2500)
			  (setq x (- x #x100))
			  (make-char 'mule-unicode-0100-24ff
				     (+ (/ x 96) 32) (+ (% x 96) 32)))
			 ((< x #x3400)
			  (setq x (- x #x2500))
			  (make-char 'mule-unicode-2500-33ff
				     (+ (/ x 96) 32) (+ (% x 96) 32)))
			 (t
			  (setq x (- x #xe000))
			  (make-char 'mule-unicode-e000-ffff
				     (+ (/ x 96) 32) (+ (% x 96) 32))))))
	      ranges from to)
	  (dolist (elt range)
	    (setq from (max #xA0 (car elt)) to (min #xffff (cdr elt)))
	    (if (and (>= to #x3400) (< to #xE000))
		(setq to #x33FF))
	    (cond ((< from #x100)
		   (if (>= to #xE000)
		       (setq ranges (cons (cons #xE000 to) ranges)
			     to #x33FF))
		   (if (>= to #x2500)
		       (setq ranges (cons (cons #x2500 to) ranges)
			     to #x24FF))
		   (if (>= to #x100)
		       (setq ranges (cons (cons #x100 to) ranges)
			     to #xFF)))
		  ((< from #x2500)
		   (if (>= to #xE000)
		       (setq ranges (cons (cons #xE000 to) ranges)
			     to #x33FF))
		   (if (>= to #x2500)
		       (setq ranges (cons (cons #x2500 to) ranges)
			     to #x24FF)))
		  ((< from #x3400)
		   (if (>= to #xE000)
		       (setq ranges (cons (cons #xE000 to) ranges)
			     to #x33FF))))
	    (if (<= from to)
		(setq ranges (cons (cons from to) ranges))))
	  (mapconcat #'(lambda (x)
			 (format "%c-%c"
				 (funcall decode-char-no-trans (car x))
				 (funcall decode-char-no-trans (cdr x))))
		     ranges "")))
  ;; These forces loading and settting tables for
  ;; utf-translate-cjk-mode.
  (setq utf-translate-cjk-lang-env nil
	ucs-mule-cjk-to-unicode (make-hash-table :test 'eq)
	ucs-unicode-to-mule-cjk (make-hash-table :test 'eq)))

(defcustom utf-translate-cjk-unicode-range '((#x2e80 . #xd7a3)
					     (#xff00 . #xffef))
  "List of Unicode code ranges supported by `utf-translate-cjk-mode'.
Setting this variable directly does not take effect;
use either \\[customize] or the function
`utf-translate-cjk-set-unicode-range'."
  :version "22.1"
  :type '(repeat (cons integer integer))
  :set (lambda (symbol value)
	 (utf-translate-cjk-set-unicode-range value))
  :group 'mule)

;; Return non-nil if CODE-POINT is in `utf-translate-cjk-unicode-range'.
(defsubst utf-translate-cjk-substitutable-p (code-point)
  (let ((tail utf-translate-cjk-unicode-range)
	elt)
    (while tail
      (setq elt (car tail) tail (cdr tail))
      (if (and (>= code-point (car elt)) (<= code-point (cdr elt)))
	  (setq tail nil)
	(setq elt nil)))
    elt))

(defun utf-translate-cjk-load-tables ()
  "Load tables for `utf-translate-cjk-mode'."
  ;; Fixme: Allow the use of the CJK charsets to be
  ;; customized by reordering and possible omission.
  (let ((redefined (< (hash-table-size ucs-mule-cjk-to-unicode) 43000)))
    (if redefined
	;; Redefine them with realistic initial sizes and a
	;; smallish rehash size to avoid wasting significant
	;; space after they're built.
	(setq ucs-mule-cjk-to-unicode
	      (make-hash-table :test 'eq :size 43000 :rehash-size 1000)
	      ucs-unicode-to-mule-cjk
	      (make-hash-table :test 'eq :size 21500 :rehash-size 1000)))

    ;; Load the files explicitly, to avoid having to keep
    ;; around the large tables they contain (as well as the
    ;; ones which get built).
    ;; Here we bind coding-system-for-read to nil so that coding tags
    ;; in the files are respected even if the files are not yet
    ;; byte-compiled
    (let ((coding-system-for-read nil)
	  ;; We must avoid clobbering this variable, in case the load
	  ;; files below use different coding systems.
	  (last-coding-system-used last-coding-system-used))
      (cond ((string= "Korean" current-language-environment)
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
	     (load "subst-jis")))) ; jis covers as much as big5, gb2312

    (when redefined
      (define-translation-hash-table 'utf-subst-table-for-decode
	ucs-unicode-to-mule-cjk)
      (define-translation-hash-table 'utf-subst-table-for-encode
	ucs-mule-cjk-to-unicode)
      (set-char-table-extra-slot (get 'utf-translation-table-for-encode
				      'translation-table)
				 1 ucs-mule-cjk-to-unicode))

    (setq utf-translate-cjk-lang-env current-language-environment)))

(defun utf-lookup-subst-table-for-decode (code-point)
  (if (and utf-translate-cjk-mode
	   (not utf-translate-cjk-lang-env)
	   (utf-translate-cjk-substitutable-p code-point))
      (utf-translate-cjk-load-tables))
  (gethash code-point
	   (get 'utf-subst-table-for-decode 'translation-hash-table)))


(defun utf-lookup-subst-table-for-encode (char)
  (if (and utf-translate-cjk-mode
	   (not utf-translate-cjk-lang-env)
	   (memq (char-charset char) utf-translate-cjk-charsets))
      (utf-translate-cjk-load-tables))
  (gethash char
	   (get 'utf-subst-table-for-encode 'translation-hash-table)))

(define-minor-mode utf-translate-cjk-mode
  "Toggle whether UTF based coding systems de/encode CJK characters.
If ARG is an integer, enable if ARG is positive and disable if
zero or negative.  This is a minor mode.
Enabling this allows the coding systems mule-utf-8,
mule-utf-16le and mule-utf-16be to encode characters in the charsets
`korean-ksc5601', `chinese-gb2312', `chinese-big5-1',
`chinese-big5-2', `japanese-jisx0208' and `japanese-jisx0212', and to
decode the corresponding unicodes into such characters.

Where the charsets overlap, the one preferred for decoding is chosen
according to the language environment in effect when this option is
turned on: ksc5601 for Korean, gb2312 for Chinese-GB, big5 for
Chinese-Big5 and jisx for other environments.

This mode is on by default.  If you are not interested in CJK
characters and want to avoid some overhead on encoding/decoding
by the above coding systems, you can customize the user option
`utf-translate-cjk-mode' to nil."
  :init-value t
  :version "22.1"
  :type 'boolean
  :group 'mule
  :global t
  (if utf-translate-cjk-mode
      (progn
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
			       1 nil))

  ;; Update safe-chars of mule-utf-* coding systems.
  (dolist (elt (coding-system-list t))
    (if (string-match "^mule-utf" (symbol-name elt))
	(let ((safe-charsets (coding-system-get elt 'safe-charsets))
	      (safe-chars (coding-system-get elt 'safe-chars))
	      (need-update nil))
	  (dolist (charset utf-translate-cjk-charsets)
	    (unless (eq utf-translate-cjk-mode (memq charset safe-charsets))
	      (setq safe-charsets
		    (if utf-translate-cjk-mode
			(cons charset safe-charsets)
		      (delq charset safe-charsets))
		    need-update t)
	      (aset safe-chars (make-char charset) utf-translate-cjk-mode)))
	  (when need-update
	    (coding-system-put elt 'safe-charsets safe-charsets)
	    (define-coding-system-internal elt))))))

(define-ccl-program ccl-mule-utf-untrans
  ;; R0 is an untranslatable Unicode code-point (U+3500..U+DFFF or
  ;; U+10000..U+10FFFF) or an invaid byte (#x00..#xFF).  Write
  ;; eight-bit-control/graphic sequence (2 to 4 chars) representing
  ;; UTF-8 sequence of r0.  Registers r4, r5, r6 are modified.
  ;;
  ;; This is a subrountine because we assume that this is called very
  ;; rarely (so we don't have to worry about the overhead of the
  ;; call).
  `(0
    ((r5 = ,(charset-id 'eight-bit-control))
     (r6 = ,(charset-id 'eight-bit-graphic))
     (if (r0 < #x100)
	 ((r4 = ((r0 >> 6) | #xC0))
	  (write-multibyte-character r6 r4))
       ((if (r0 < #x10000)
	    ((r4 = ((r0 >> 12) | #xE0))
	     (write-multibyte-character r6 r4))
	  ((r4 = ((r0 >> 18) | #xF0))
	   (write-multibyte-character r6 r4)
	   (r4 = (((r0 >> 12) & #x3F) | #x80))
	   (if (r4 < #xA0)
	       (write-multibyte-character r5 r4)
	     (write-multibyte-character r6 r4))))
	(r4 = (((r0 >> 6) & #x3F) | #x80))
	(if (r4 < #xA0)
	    (write-multibyte-character r5 r4)
	  (write-multibyte-character r6 r4))))
     (r4 = ((r0 & #x3F) | #x80))
     (if (r4 < #xA0)
	 (write-multibyte-character r5 r4)
       (write-multibyte-character r6 r4)))))

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
  ;; -----------------------+----------------+---------------
  ;;      invalid byte      |       1        |       2
  ;;
  ;; Thus magnification factor is two.
  ;;
  `(2
    ((r6 = ,(charset-id 'latin-iso8859-1))
     (read r0)
     (loop
      (if (r0 < #x80)
	  ;; 1-byte encoding, i.e., ascii
	  (write-read-repeat r0))
      (if (r0 < #xc2)
	  ;; continuation byte (invalid here) or 1st byte of overlong
	  ;; 2-byte sequence.
	  ((call ccl-mule-utf-untrans)
	   (r6 = ,(charset-id 'latin-iso8859-1))
	   (read r0)
	   (repeat)))

      ;; Read the 2nd byte.
      (read r1)
      (if ((r1 & #b11000000) != #b10000000) ; Invalid 2nd byte
	  ((call ccl-mule-utf-untrans)
	   (r6 = ,(charset-id 'latin-iso8859-1))
	   ;; Handle it in the next loop.
	   (r0 = r1)
	   (repeat)))

      (if (r0 < #xe0)
	  ;; 2-byte encoding 00000yyyyyxxxxxx = 110yyyyy 10xxxxxx
	  ((r1 &= #x3F)
	   (r1 |= ((r0 & #x1F) << 6))
	   ;; Now r1 holds scalar value.  We don't have to check
	   ;; `overlong sequence' because r0 >= 0xC2.

	   (if (r1 >= 256)
	       ;; mule-unicode-0100-24ff (< 0800)
	       ((r0 = r1)
		(lookup-integer utf-subst-table-for-decode r0 r1)
		(if (r7 == 0)
		    ((r0 = ,(charset-id 'mule-unicode-0100-24ff))
		     (r1 -= #x0100)
		     (r2 = (((r1 / 96) + 32) << 7))
		     (r1 %= 96)
		     (r1 += (r2 + 32))
		     (translate-character
		      utf-translation-table-for-decode r0 r1)))
		(write-multibyte-character r0 r1)
		(read r0)
		(repeat))
	     (if (r1 >= 160)
		 ;; latin-iso8859-1
		 ((r0 = r1)
		  (lookup-integer utf-subst-table-for-decode r0 r1)
		  (if (r7 == 0)
		      ((r1 -= 128)
		       (write-multibyte-character r6 r1))
		    ((write-multibyte-character r0 r1)))
		  (read r0)
		  (repeat))
	       ;; eight-bit-control
	       ((r0 = ,(charset-id 'eight-bit-control))
		(write-multibyte-character r0 r1)
		(read r0)
		(repeat))))))

      ;; Read the 3rd bytes.
      (read r2)
      (if ((r2 & #b11000000) != #b10000000) ; Invalid 3rd byte
	  ((call ccl-mule-utf-untrans)
	   (r0 = r1)
	   (call ccl-mule-utf-untrans)
	   (r6 = ,(charset-id 'latin-iso8859-1))
	   ;; Handle it in the next loop.
	   (r0 = r2)
	   (repeat)))

      (if (r0 < #xF0)
	  ;; 3byte encoding
	  ;; zzzzyyyyyyxxxxxx = 1110zzzz 10yyyyyy 10xxxxxx
	  ((r3 = ((r0 & #xF) << 12))
	   (r3 |= ((r1 & #x3F) << 6))
	   (r3 |= (r2 & #x3F))

	   (if (r3 < #x800)		; `overlong sequence'
	       ((call ccl-mule-utf-untrans)
		(r0 = r1)
		(call ccl-mule-utf-untrans)
		(r0 = r2)
		(call ccl-mule-utf-untrans)
		(r6 = ,(charset-id 'latin-iso8859-1))
		(read r0)
		(repeat)))

	   (if (r3 < #x2500)
	       ;; mule-unicode-0100-24ff (>= 0800)
	       ((r0 = r3)
		(lookup-integer utf-subst-table-for-decode r0 r1)
		(if (r7 == 0)
		    ((r0 = ,(charset-id 'mule-unicode-0100-24ff))
		     (r3 -= #x0100)
		     (r3 //= 96)
		     (r1 = (r7 + 32))
		     (r1 += ((r3 + 32) << 7))
		     (translate-character
		      utf-translation-table-for-decode r0 r1)))
		(write-multibyte-character r0 r1)
		(read r0)
		(repeat)))

	   (if (r3 < #x3400)
	       ;; mule-unicode-2500-33ff
	       ((r0 = r3)		; don't zap r3
		(lookup-integer utf-subst-table-for-decode r0 r1)
		(if (r7 == 0)
		    ((r0 = ,(charset-id 'mule-unicode-2500-33ff))
		     (r3 -= #x2500)
		     (r3 //= 96)
		     (r1 = (r7 + 32))
		     (r1 += ((r3 + 32) << 7))))
		(write-multibyte-character r0 r1)
		(read r0)
		(repeat)))

	   (if (r3 < #xE000)
	       ;; Try to convert to CJK chars, else
	       ;; keep them as eight-bit-{control|graphic}.
	       ((r0 = r3)
		(lookup-integer utf-subst-table-for-decode r3 r1)
		(if r7
		    ;; got a translation
		    ((write-multibyte-character r3 r1)
		     (read r0)
		     (repeat))
		  ((call ccl-mule-utf-untrans)
		   (r6 = ,(charset-id 'latin-iso8859-1))
		   (read r0)
		   (repeat)))))

	   ;; mule-unicode-e000-ffff
	   ;; Fixme: fffe and ffff are invalid.
	   (r0 = r3)		; don't zap r3
	   (lookup-integer utf-subst-table-for-decode r0 r1)
	   (if (r7 == 0)
	       ((r0 = ,(charset-id 'mule-unicode-e000-ffff))
		(r3 -= #xe000)
		(r3 //= 96)
		(r1 = (r7 + 32))
		(r1 += ((r3 + 32) << 7))))
	   (write-multibyte-character r0 r1)
	   (read r0)
	   (repeat)))

      ;; Read the 4th bytes.
      (read r3)
      (if ((r3 & #b11000000) != #b10000000) ; Invalid 4th byte
	  ((call ccl-mule-utf-untrans)
	   (r0 = r1)
	   (call ccl-mule-utf-untrans)
	   (r0 = r2)
	   (call ccl-mule-utf-untrans)
	   (r6 = ,(charset-id 'latin-iso8859-1))
	   ;; Handle it in the next loop.
	   (r0 = r3)
	   (repeat)))

      (if (r0 < #xF8)
	  ;; 4-byte encoding:
	  ;; wwwzzzzzzyyyyyyxxxxxx = 11110www 10zzzzzz 10yyyyyy 10xxxxxx
	  ;; keep those bytes as eight-bit-{control|graphic}
	  ;; Fixme: allow lookup in utf-subst-table-for-decode.
	  ((r4 = ((r0 & #x7) << 18))
	   (r4 |= ((r1 & #x3F) << 12))
	   (r4 |= ((r2 & #x3F) << 6))
	   (r4 |= (r3 & #x3F))

	   (if (r4 < #x10000)		; `overlong sequence'
	       ((call ccl-mule-utf-untrans)
		(r0 = r1)
		(call ccl-mule-utf-untrans)
		(r0 = r2)
		(call ccl-mule-utf-untrans)
		(r0 = r3)
		(call ccl-mule-utf-untrans))
	     ((r0 = r4)
	      (call ccl-mule-utf-untrans))))

	;; Unsupported sequence.
	((call ccl-mule-utf-untrans)
	 (r0 = r1)
	 (call ccl-mule-utf-untrans)
	 (r0 = r2)
	 (call ccl-mule-utf-untrans)
	 (r0 = r3)
	 (call ccl-mule-utf-untrans)))
      (r6 = ,(charset-id 'latin-iso8859-1))
      (read r0)
      (repeat)))


    ;; At EOF...
    (if (r0 >= 0)
	;; r0 >= #x80
	((call ccl-mule-utf-untrans)
	 (if (r1 >= 0)
	     ((r0 = r1)
	      (call ccl-mule-utf-untrans)
	      (if (r2 >= 0)
		  ((r0 = r2)
		   (call ccl-mule-utf-untrans)
		   (if (r3 >= 0)
		       ((r0 = r3)
			(call ccl-mule-utf-untrans))))))))))

  "CCL program to decode UTF-8.
Basic decoding is done into the charsets ascii, latin-iso8859-1 and
mule-unicode-*, but see also `utf-fragmentation-table' and
`ucs-mule-cjk-to-unicode'.
Encodings of un-representable Unicode characters are decoded asis into
eight-bit-control and eight-bit-graphic characters.")

(define-ccl-program ccl-mule-utf-8-encode-untrans
  ;; UTF-8 decoder generates an UTF-8 sequence represented by a
  ;; sequence eight-bit-control/graphic chars for an untranslatable
  ;; character and an invalid byte.
  ;;
  ;; This CCL parses that sequence (the first byte is already in r1),
  ;; writes out the original bytes of that sequence, and sets r5 to
  ;; -1.
  ;;
  ;; If the eight-bit-control/graphic sequence is shorter than what r1
  ;; suggests, it sets r5 and r6 to the last character read that
  ;; should be handled by the next loop of a caller.
  ;;
  ;; Note: For UTF-8 validation, we only check if a character is
  ;; eight-bit-control/graphic or not.  It may result in incorrect
  ;; handling of random binary data, but such a data can't be encoded
  ;; by UTF-8 anyway.  At least, UTF-8 decoders doesn't generate such
  ;; a sequence even if a source contains invalid byte-sequence.
  `(0
    (;; Read the 2nd byte.
     (read-multibyte-character r5 r6)
     (r0 = (r5 != ,(charset-id 'eight-bit-control)))
     (if ((r5 != ,(charset-id 'eight-bit-graphic)) & r0)
	 ((write r1)			; invalid UTF-8
	  (r1 = -1)
	  (end)))

     (if (r1 <= #xC3)
	 ;; 2-byte sequence for an originally invalid byte.
	 ((r6 &= #x3F)
	  (r6 |= ((r1 & #x1F) << 6))
	  (write r6)
	  (r5 = -1)
	  (end)))

     (write r1 r6)
     (r2 = r1)
     (r1 = -1)
     ;; Read the 3rd byte.
     (read-multibyte-character r5 r6)
     (r0 = (r5 != ,(charset-id 'eight-bit-control)))
     (if ((r5 != ,(charset-id 'eight-bit-graphic)) & r0)
	 (end))				; invalid UTF-8
     (write r6)
     (if (r2 < #xF0)
	 ;; 3-byte sequence for an untranslated character.
	 ((r5 = -1)
	  (end)))
     ;; Read the 4th byte.
     (read-multibyte-character r5 r6)
     (r0 = (r5 != ,(charset-id 'eight-bit-control)))
     (if ((r5 != ,(charset-id 'eight-bit-graphic)) & r0)
	 (end))			; invalid UTF-8
     ;; 4-byte sequence for an untranslated character.
     (write r6)
     (r5 = -1)
     (end))

    ;; At EOF...
    ((r5 = -1)
     (if (r1 >= 0)
	 (write r1)))))

(define-ccl-program ccl-encode-mule-utf-8
  `(1
    ((r5 = -1)
     (loop
      (if (r5 < 0)
	  (read-multibyte-character r0 r1)
	;; Pre-read character is in r5 (charset-ID) and r6 (code-point).
	((r0 = r5)
	 (r1 = r6)
	 (r5 = -1)))
      (translate-character utf-translation-table-for-encode r0 r1)

      (if (r0 == ,(charset-id 'ascii))
	  (write-repeat r1))

      (if (r0 == ,(charset-id 'latin-iso8859-1))
	  ;; r1          scalar                  utf-8
	  ;;       0000 0yyy yyxx xxxx    110y yyyy 10xx xxxx
	  ;; 20    0000 0000 1010 0000    1100 0010 1010 0000
	  ;; 7f    0000 0000 1111 1111    1100 0011 1011 1111
	  ((write ((r1 >> 6) | #xc2))
	   (r1 &= #x3f)
	   (r1 |= #x80)
	   (write-repeat r1)))

      (if (r0 == ,(charset-id 'mule-unicode-0100-24ff))
	  ((r0 = ((((r1 & #x3f80) >> 7) - 32) * 96))
	   ;; #x3f80 == (0011 1111 1000 0000)b
	   (r1 &= #x7f)
	   (r1 += (r0 + 224))		; 240 == -32 + #x0100
	   ;; now r1 holds scalar value
	   (if (r1 < #x0800)
	       ;; 2byte encoding
	       ((write ((r1 >> 6) | #xC0))
		(r1 &= #x3F)
		(r1 |= #x80)
		(write-repeat r1))
	     ;; 3byte encoding
	     ((write ((r1 >> 12) | #xE0))
	      (write  (((r1 & #x0FC0) >> 6) | #x80))
	      (r1 &= #x3F)
	      (r1 |= #x80)
	      (write-repeat r1)))))

      (if (r0 == ,(charset-id 'mule-unicode-2500-33ff))
	  ((r0 = ((((r1 & #x3f80) >> 7) - 32) * 96))
	   (r1 &= #x7f)
	   (r1 += (r0 + 9440))		; 9440 == -32 + #x2500
	   ;; now r1 holds scalar value
	   (write ((r1 >> 12) | #xE0))
	   (write  (((r1 & #x0FC0) >> 6) | #x80))
	   (r1 &= #x3F)
	   (r1 |= #x80)
	   (write-repeat r1)))

      (if (r0 == ,(charset-id 'mule-unicode-e000-ffff))
	  ((r0 = ((((r1 & #x3f80) >> 7) - 32) * 96))
	   (r1 &= #x7f)
	   (r1 += (r0 + 57312))		; 57312 == -32 + #xe000
	   ;; now r1 holds scalar value
	   (write ((r1 >> 12) | #xE0))
	   (write  (((r1 & #x0FC0) >> 6) | #x80))
	   (r1 &= #x3F)
	   (r1 |= #x80)
	   (write-repeat r1)))

      (if (r0 == ,(charset-id 'eight-bit-control))
	  ;; r1          scalar                  utf-8
	  ;;       0000 0yyy yyxx xxxx    110y yyyy 10xx xxxx
	  ;; 80    0000 0000 1000 0000    1100 0010 1000 0000
	  ;; 9f    0000 0000 1001 1111    1100 0010 1001 1111
	  ((write #xC2)
	   (write-repeat r1)))

      (if (r0 == ,(charset-id 'eight-bit-graphic))
	  ;; r1          scalar                  utf-8
	  ;;       0000 0yyy yyxx xxxx    110y yyyy 10xx xxxx
	  ;; a0    0000 0000 1010 0000    1100 0010 1010 0000
	  ;; ff    0000 0000 1111 1111    1101 1111 1011 1111
	  ((r0 = (r1 >= #xC0))
	   (r0 &= (r1 <= #xC3))
	   (r4 = (r1 >= #xE1))
	   (r4 &= (r1 <= #xF7))
	   (r0 |= r4)
	   (if r0
	       ((call ccl-mule-utf-8-encode-untrans)
		(repeat))
	     (write-repeat r1))))

      (lookup-character utf-subst-table-for-encode r0 r1)
      (if r7		; lookup succeeded
	  (if (r0 < #x800)
	      ;; 2byte encoding
	      ((write ((r0 >> 6) | #xC0))
	       (r0 = ((r0 & #x3F) | #x80))
	       (write-repeat r0))
	    ;; 3byte encoding
	    ((write ((r0 >> 12) | #xE0))
	     (write  (((r0 & #x0FC0) >> 6) | #x80))
	     (r0 = ((r0 & #x3F) | #x80))
	     (write-repeat r0))))

      ;; Unsupported character.
      ;; Output U+FFFD, which is `ef bf bd' in UTF-8.
      (write #xef)
      (write #xbf)
      (write-repeat #xbd))))
  "CCL program to encode into UTF-8.")


(define-ccl-program ccl-untranslated-to-ucs
  `(0
    (if (r1 == 0)
	nil
      (if (r0 <= #xC3)			; 2-byte encoding
	  ((r0 = ((r0 & #x3) << 6))
	   (r0 |= (r1 & #x3F))
	   (r1 = 2))
	(if (r2 == 0)
	    (r1 = 0)
	  (if (r0 < #xF0)		; 3-byte encoding, as above
	      ((r0 = ((r0 & #xF) << 12))
	       (r0 |= ((r1 & #x3F) << 6))
	       (r0 |= (r2 & #x3F))
	       (r1 = 3))
	    (if (r3 == 0)
		(r1 = 0)
	      ((r0 = ((r0 & #x7) << 18))
	       (r0 |= ((r1 & #x3F) << 12))
	       (r0 |= ((r2 & #x3F) << 6))
	       (r0 |= (r3 & #x3F))
	       (r1 = 4))))))))
  "Decode 2-, 3-, or 4-byte sequences in r0, r1, r2 [,r3] to unicodes in r0.
Set r1 to the byte length.  r0 == 0 for invalid sequence.")

(defvar utf-8-ccl-regs (make-vector 8 0))

(defsubst utf-8-untranslated-to-ucs ()
  "Return the UCS code for an untranslated sequence of raw bytes t point.
Only for 3- or 4-byte sequences."
  (aset utf-8-ccl-regs 0 (or (char-after) 0))
  (aset utf-8-ccl-regs 1 (or (char-after (1+ (point))) 0))
  (aset utf-8-ccl-regs 2 (or (char-after (+ 2 (point))) 0))
  (aset utf-8-ccl-regs 3 (or (char-after (+ 3 (point))) 0))
  (ccl-execute 'ccl-untranslated-to-ucs utf-8-ccl-regs))

(defun utf-8-help-echo (window object position)
  (format "Untranslated Unicode U+%04X"
	  (get-char-property position 'untranslated-utf-8 object)))

;; We compose the untranslatable sequences into a single character,
;; and move point to the next character.
;; This is infelicitous for editing, because there's currently no
;; mechanism for treating compositions as atomic, but is OK for
;; display.  They are composed to U+FFFD with help-echo which
;; indicates the unicodes they represent.  This function GCs too much.

;; If utf-translate-cjk-mode is non-nil, this function is called with
;; HASH-TABLE which translates CJK characters into some of CJK
;; charsets.

(defsubst utf-8-compose (hash-table)
  "Put a suitable composition on an untranslatable sequence at point.
If HASH-TABLE is non-nil, try to translate CJK characters by it at first.
Move point to the end of the sequence."
  (utf-8-untranslated-to-ucs)
  (let ((l (aref utf-8-ccl-regs 1))
	ch)
    (if (> l 0)
	(if (and hash-table
		 (setq ch (gethash (aref utf-8-ccl-regs 0)  hash-table)))
	    (progn
	      (insert ch)
	      (delete-region (point) (min (point-max) (+ l (point)))))
	  (setq ch (aref utf-8-ccl-regs 0))
	  (put-text-property (point) (min (point-max) (+ l (point)))
			     'untranslated-utf-8 ch)
	  (put-text-property (point) (min (point-max) (+ l (point)))
			     'help-echo 'utf-8-help-echo)
	  (if (= l 2)
	      (put-text-property (point) (min (point-max) (+ l (point)))
				 'display (propertize (format "\\%03o" ch)
						      'face 'escape-glyph))
	    (compose-region (point) (+ l (point)) ?$,3u=(B))
	  (forward-char l))
      (forward-char 1))))

(defcustom utf-8-compose-scripts nil
  "*Non-nil means compose various scripts on decoding utf-8 text."
  :group 'mule
  :version "22.1"
  :type 'boolean)

(defun utf-8-post-read-conversion (length)
  "Compose untranslated utf-8 sequences into single characters.
If `utf-translate-cjk-mode' is non-nil, tries to translate CJK characters.
Also compose particular scripts if `utf-8-compose-scripts' is non-nil."
  (save-excursion
    (save-restriction
      (narrow-to-region (point) (+ (point) length))
      ;; Can't do eval-when-compile to insert a multibyte constant
      ;; version of the string in the loop, since it's always loaded as
      ;; unibyte from a byte-compiled file.
      (let ((range (string-as-multibyte "^\xc0-\xc3\xe1-\xf7"))
	    (buffer-multibyte enable-multibyte-characters)
	    hash-table ch)
	(set-buffer-multibyte t)
	(when utf-translate-cjk-mode
	  (unless utf-translate-cjk-lang-env
	    ;; Check these characters in utf-translate-cjk-range.
	    ;; We may have to translate them to CJK charsets.
	    (skip-chars-forward
	     (concat range utf-translate-cjk-unicode-range-string))
	    (unless (eobp)
	      (utf-translate-cjk-load-tables)
	      (setq range
		    (concat range utf-translate-cjk-unicode-range-string)))
	    (setq hash-table (get 'utf-subst-table-for-decode
				  'translation-hash-table))))
	(while (and (skip-chars-forward range)
		    (not (eobp)))
	  (setq ch (following-char))
	  (if (< ch 256)
	      (utf-8-compose hash-table)
	    (if (and hash-table
		     (setq ch (gethash (encode-char ch 'ucs) hash-table)))
		(progn
		  (insert ch)
		  (delete-char 1))
	      (forward-char 1))))
	(or buffer-multibyte
	    (set-buffer-multibyte nil)))

      (when (and utf-8-compose-scripts (> length 1))
	;; These currently have definitions which cover the relevant
	;; unicodes.  We could avoid loading thai-util &c by checking
	;; whether the region contains any characters with the appropriate
	;; categories.  There aren't yet Unicode-based rules for Tibetan.
	(diacritic-compose-region (point-max) (point-min))
	(thai-compose-region (point-max) (point-min))
	(lao-compose-region (point-max) (point-min))
	(devanagari-compose-region (point-max) (point-min))
	(malayalam-compose-region (point-max) (point-min))
	(tamil-compose-region (point-max) (point-min)))
      (- (point-max) (point-min)))))

(defun utf-8-pre-write-conversion (beg end)
  "Prepare for `utf-translate-cjk-mode' to encode text between BEG and END.
This is used as a post-read-conversion of utf-8 coding system."
  (if (and utf-translate-cjk-mode
	   (not utf-translate-cjk-lang-env)
	   (if (stringp beg)
	       (string-match "\\cc\\|\\cj\\|\\ch" beg)
	     (save-excursion
	       (goto-char beg)
	       (re-search-forward "\\cc\\|\\cj\\|\\ch" end t))))
      (utf-translate-cjk-load-tables))
  nil)

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
 `((safe-charsets
    ascii
    eight-bit-control
    eight-bit-graphic
    latin-iso8859-1
    mule-unicode-0100-24ff
    mule-unicode-2500-33ff
    mule-unicode-e000-ffff
    ,@(if utf-translate-cjk-mode
	  utf-translate-cjk-charsets))
   (mime-charset . utf-8)
   (coding-category . coding-category-utf-8)
   (valid-codes (0 . 255))
   (pre-write-conversion . utf-8-pre-write-conversion)
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
