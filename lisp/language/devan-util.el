;;; devan-util.el --- Support for composing Devanagari characters -*-coding: iso2022-7bit;-*-

;; Copyright (C) 2001 Free Software Foundation, Inc.

;; Maintainer:  KAWABATA, Taichi <batta@beige.ocn.ne.jp>
;; Keywords: multilingual, Devanagari

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

;; Created: Feb. 17. 2001

;;; Commentary:

;; This file provides character(Unicode) to glyph(CDAC) conversion and
;; composition of Devanagari script characters.

;;; Code:

;;;###autoload

;; Devanagari Composable Pattern
;;    C .. Consonants
;;    V .. Vowel
;;    H .. Halant
;;    M .. Matra
;;    V .. Vowel
;;    A .. Anuswar
;;    D .. Chandrabindu
;;    (N .. Zerowidth Non Joiner)
;;    (J .. Zerowidth Joiner.  )
;; 1. vowel
;;  V(A/D)?
;; 2. syllable : maximum of 5 consecutive consonants.  (e.g. kartsnya)
;;  ((CH)?(CH)?(CH)?CH)?C(H|M?(A|D)?)?

(defconst devanagari-consonant
  "[$,15U(B-$,15y68(B-$,16?(B]")

(defconst devanagari-composable-pattern
  (concat 
   "\\([$,15E(B-$,15T6@6A(B][$,15A5B(B]?\\)\\|$,15C(B"
   "\\|\\("
   "\\(?:\\(?:[$,15U(B-$,15y68(B-$,16?(B]$,16-(B\\)?\\(?:[$,15U(B-$,15y68(B-$,16?(B]$,16-(B\\)?\\(?:[$,15U(B-$,15y68(B-$,16?(B]$,16-(B\\)?[$,15U(B-$,15y68(B-$,16?(B]$,16-(B\\)?"
   "[$,15U(B-$,15y68(B-$,16?(B]\\(?:$,16-(B\\|[$,15~(B-$,16-6B6C(B]?[$,15B5A(B]?\\)?"
   "\\)")
  "Regexp matching a composable sequence of Devanagari characters.")

(defun devanagari-compose-region (from to)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char (point-min))
      (while (re-search-forward devanagari-composable-pattern nil t)
        (devanagari-compose-syllable-region (match-beginning 0) 
                                            (match-end 0))))))
(defun devanagari-compose-string (string)
  (with-temp-buffer
    (insert (decompose-string string))
    (devanagari-compose-region (point-min) (point-max))
    (buffer-string)))

(defun devanagari-post-read-conversion (len)
  (save-excursion
    (save-restriction
      (let ((buffer-modified-p (buffer-modified-p)))
	(narrow-to-region (point) (+ (point) len))
	(devanagari-compose-region (point-min) (point-max))
	(set-buffer-modified-p buffer-modified-p)
	(- (point-max) (point-min))))))

(defun devanagari-range (from to)
  "Make the list of the integers of range FROM to TO."
  (let (result) 
    (while (<= from to) (setq result (cons to result) to (1- to))) result))

(defun devanagari-regexp-of-hashtbl-keys (hashtbl)
  "Return a regular expression that matches all keys in hashtable HASHTBL."
  (let ((max-specpdl-size 1000))
    (regexp-opt
     (sort 
      (let (dummy)
	(maphash (function (lambda (key val) (setq dummy (cons key dummy)))) hashtbl)
	dummy)
      (function (lambda (x y) (> (length x) (length y))))))))

(defun devanagari-composition-function (from to pattern &optional string)
  "Compose Devanagari characters in REGION, or STRING if specified.
Assume that the REGION or STRING must fully match the composable
PATTERN regexp."
  (if string (devanagari-compose-syllable-string string)
    (devanagari-compose-syllable-region from to))
  (- to from))

;; Register a function to compose Devanagari characters.
(mapc
 (function (lambda (ucs)
   (aset composition-function-table ucs
	 (list (cons devanagari-composable-pattern 
                     'devanagari-composition-function)))))
 (nconc '(#x0903) (devanagari-range #x0905 #x0939) (devanagari-range #x0958 #x0961)))

;; Notes on conversion steps.

;; 1. chars to glyphs 
;;
;; Rules will not be applied to the halant appeared at the end of the
;; text.  Also, the preceding/following "r" will be treated as special case.

;; 2. glyphs reordering.
;;
;; The glyphs are split by halant, and each glyph groups are
;; re-ordered in the following order.
;;
;; Note that `consonant-glyph' mentioned here does not contain the
;; vertical bar (right modifier) attached at the right of the
;; consonant.
;; 
;; If the glyph-group contains right modifier, 
;;  (1) consonant-glyphs/vowels, with nukta sign
;;  (2) spacing
;;  (3) right modifier (may be matra)
;;  (4) top matra
;;  (5) preceding "r"
;;  (6) anuswar
;;  (7) following "r"
;;  (8) bottom matra or halant.
;; 
;; Otherwise, 
;;  (1) consonant-glyph/vowels, with nukta sign
;;  (3) left matra
;;  (4) top matra 
;;  (5) preceding "r"
;;  (6) anuswar
;;  (7) following "r"
;;  (8) bottom matra or halant.
;;  (2) spacing

;; 3. glyph to glyph
;;
;; For better display, some glyph display would be tuned.

;; 4. Composition.
;;
;; left modifiers will be attached at the left.
;; others will be attached right.

;; Problem::
;;  Can we generalize this methods to other Indian scripts?

(defvar dev-char-glyph
  '(("$,15E(B" . "$,4 K(B")
    ("$,15F(B" . "$,4 K")(B")
    ("$,15~(B" . "$,4")(B")
    ("$,15G(B" . "$,4 \(B")
    ("$,15(B" . "$,4"*(B")
    ("$,155A(B" . "$,4"*(B\$,4"&(B")
    ("$,15H(B" . "$,4 \"'(B")
    ("$,15H5A(B" . "$,4 \"'"&(B")
    ("$,16 (B" . "$,4"2(B")
    ("$,16 5A(B" . "$,4"2"&(B")
    ("$,15I(B" . "$,4 ](B")
    ("$,16!(B" . "$,4"6(B")
    ("$,15J(B" . "$,4 ^"P(B")
    ("$,16"(B" . "$,4":(B")
    ("$,15K(B" . "$,4 `"Q(B")
    ("$,16#(B" . "$,4">(B")
    ;;("$,15L(B" . nil) ; not implemented.
    ("$,16$(B" . "$,4"?(B")
    ("$,15M(B" . "$,4 b"L(B")
    ("$,15M5A(B" . "$,4 b"$(B")
    ("$,15M5B(B" . "$,4 b"$(B")
    ("$,16%(B" . "\$,4"L(B")
    ("$,15N(B" . "$,4 b"@(B") 
    ("$,15N5A(B" . "$,4 b"@"&(B") 
    ("$,16&(B" . "\$,4"@(B")
    ("$,16&5A(B" . "\$,4"@(B\$,4"&(B")
    ("$,15O(B" . "$,4 b(B") 
    ("$,16'(B" . "\$,4"D(B")
    ("$,16'5A(B" . "\$,4"D(B\$,4"&(B")
    ("$,15P(B" . "$,4 b"D(B") 
    ("$,15P5A(B" . "$,4 b"D"&(B") 
    ("$,16((B" . "\$,4"H(B")
    ("$,16(5A(B" . "\$,4"H(B\$,4"&(B")
    ("$,15Q(B" . "$,4 K")"L(B") ;; special rule for reodering.
    ("$,15Q5A(B" . "$,4 K")"$(B")
    ("$,15Q5B(B" . "$,4 K")"$(B")
    ("$,16)(B" . "\$,4")"L(B")
    ("$,16)5A(B" . "\$,4")"$(B")
    ("$,16)5B(B" . "\$,4")"$(B")
    ("$,15R(B" . "$,4 K")"@(B") 
    ("$,15R5A(B" . "$,4 K")"@"&(B") 
    ("$,16*(B" . "\$,4")"@(B")
    ("$,16*5A(B" . "\$,4")"@"&(B")
    ("$,15S(B" . "$,4 K")"D(B")
    ("$,15S5A(B" . "$,4 K")"D"&(B")
    ("$,16+(B" . "\$,4")"D(B")
    ("$,16+5A(B" . "\$,4")"D"&(B")
    ("$,15T(B" . "$,4 K")"H(B") 
    ("$,15T5A(B" . "$,4 K")"H"&(B") 
    ("$,16,(B" . "\$,4")"H(B")
    ("$,16,5A(B" . "\$,4")"H"&(B")
    ("$,16@(B" . "$,4 a"Q(B") 
    ;;("$,16B(B" . nil)
    ;;("$,16A(B" . nil) 
    ;;("$,16C(B" . nil)

    ;; GRUTTALS
    ("$,15U(B" . "$,4 e"R(B")
    ("$,15U6-(B" . "$,4 c(B")
    ("$,15U6-5p(B" . "$,4 g"R(B")
    ("$,15U6-5d(B" . "$,4 h"R(B")
    ("$,15U6-5w(B" . "$,4 i")(B")
    ("$,15U6-5w6-(B" . "$,4 i(B")

    ("$,15V(B" . "$,4 j")(B")
    ("$,15V6-(B" . "$,4 j(B")
    ("$,15V6-5p(B" . "$,4 l")(B")
    ("$,15V6-5p6-(B" . "$,4 l(B")

    ("$,15W(B" . "$,4 m")(B") 
    ("$,15W6-(B" . "$,4 m(B") 
    ("$,15W6-5p(B" . "$,4 o")(B")
    ("$,15W6-5p6-(B" . "$,4 o(B")

    ("$,15X(B" . "$,4 p")(B") 
    ("$,15X6-(B" . "$,4 p(B") 
    ("$,15X6-5p(B" . "$,4 q")(B") 
    ("$,15X6-5p6-(B" . "$,4 q(B") 

    ("$,15Y(B" . "$,4 r"S(B")
    ;; PALATALS  
    ("$,15Z(B" . "$,4 s")(B") 
    ("$,15Z6-(B" . "$,4 s(B") 
    ("$,15Z6-5p(B" . "$,4 t")(B") 
    ("$,15Z6-5p6-(B" . "$,4 t(B")

    ("$,15[(B" . "$,4 u"T(B") 

    ("$,15\(B" . "$,4 v")(B") 
    ("$,15\6-(B" . "$,4 v(B") 
    ("$,15\6-5p(B" . "$,4 x")(B") 
    ("$,15\6-5p6-(B" . "$,4 x(B") 
    ("$,15\6-5^(B" . "$,4 y")(B") 
    ("$,15\6-5^6-(B" . "$,4 y(B") 

    ("$,15](B" . "$,4 z")(B") 
    ("$,15]6-(B" . "$,4 z(B") 
    ("$,15]6-5p(B" . "$,4 {")(B") 
    ("$,15]6-5p6-(B" . "$,4 {(B") 

    ("$,15^(B" . "$,4 |")(B")
    ("$,15^6-(B" . "$,4 |(B")
    ;; CEREBRALS 
    ("$,15_(B" . "$,4 }"U(B")
    ("$,15_6-5_(B" . "$,4 ~"U(B")
    ("$,15_6-5`(B" . "$,4 "U(B")

    ("$,15`(B" . "$,4! "V(B") 
    ("$,15`6-5`(B" . "$,4!!"V(B") 

    ("$,15a(B" . "$,4!""W(B") 
    ("$,15a6-5a(B" . "$,4!$"W(B") 
    ("$,15a6-5b(B" . "$,4!%"W(B") 

    ("$,15b(B" . "$,4!&"X(B") 

    ("$,15c(B" . "$,4!(")(B")
    ("$,15c6-(B" . "$,4!((B")
    ;; DENTALS   
    ("$,15d(B" . "$,4!)")(B") 
    ("$,15d6-(B" . "$,4!)(B") 
    ("$,15d6-5p(B" . "$,4!*")(B") 
    ("$,15d6-5p6-(B" . "$,4!*(B") 
    ("$,15d6-5d(B" . "$,4!+")(B") 
    ("$,15d6-5d6-(B" . "$,4!+(B") 

    ("$,15e(B" . "$,4!,")(B") 
    ("$,15e6-(B" . "$,4!,(B") 
    ("$,15e6-5p(B" . "$,4!-")(B") 
    ("$,15e6-5p6-(B" . "$,4!-(B") 

    ("$,15f(B" . "$,4!."Y(B") 
    ("$,15f6#(B" . "$,4!/"Y(B")
    ("$,15f6-5p(B" . "$,4!0"Y(B")
    ("$,15f6-5f(B" . "$,4!1"Y(B")
    ("$,15f6-5g(B" . "$,4!2"Y(B")
    ("$,15f6-5n(B" . "$,4!3(B")
    ("$,15f6-5o(B" . "$,4!4(B")
    ("$,15f6-5u(B" . "$,4!5"Y(B")

    ("$,15g(B" . "$,4!6")(B") 
    ("$,15g6-(B" . "$,4!6(B") 
    ("$,15g6-5p(B" . "$,4!7")(B") 
    ("$,15g6-5p6-(B" . "$,4!7(B") 

    ("$,15h(B" . "$,4!8")(B") 
    ("$,15h6-(B" . "$,4!8(B") 
    ("$,15h6-5p(B" . "$,4!9")(B") 
    ("$,15h6-5p6-(B" . "$,4!9")(B") 
    ("$,15h6-5h(B" . "$,4!:")(B") 
    ("$,15h6-5h6-(B" . "$,4!:(B") 

    ("$,15i(B" . "$,4!8"#")(B")
    ;; LABIALS   
    ("$,15j(B" . "$,4!;")(B") 
    ("$,15j6-(B" . "$,4!;(B") 
    ("$,15j6-5p(B" . "$,4!<")(B") 
    ("$,15j6-5p6-(B" . "$,4!<(B") 

    ("$,15k(B" . "$,4!a"[(B") 
    ("$,15k6-(B" . "$,4!=(B") 
    ("$,15k6-5p(B" . "$,4!c"[(B") 

    ("$,15l(B" . "$,4!d")(B") 
    ("$,15l6-(B" . "$,4!d(B") 
    ("$,15l6-5p(B" . "$,4!e")(B") 
    ("$,15l6-5p6-(B" . "$,4!e(B") 

    ("$,15m(B" . "$,4!f")(B") 
    ("$,15m6-(B" . "$,4!f(B") 
    ("$,15m6-5p(B" . "$,4!g")(B") 
    ("$,15m6-5p6-(B" . "$,4!g(B") 

    ("$,15n(B" . "$,4!h")(B")
    ("$,15n6-(B" . "$,4!h(B")
    ("$,15n6-5p(B" . "$,4!i")(B")
    ("$,15n6-5p6-(B" . "$,4!i(B")
    ;; SEMIVOWELS
    ("$,15o(B" . "$,4!j")(B") 
    ("$,15o6-(B" . "$,4!j(B") 
    ("$,15o6-5p(B" . "$,4!k")(B") 
    ("$,15o6-5p6-(B" . "$,4!k(B") 
    ("$,16-5o(B" . "$,4!l(B") ;; when every ohter lig. fails.

    ("$,15p(B" . "$,4!n"W(B") 
    ;; ("$,15p6-(B" . "\$,4"'(B") ;; special case.  only the topmost pos.
    ("$,15q(B" . "$,4!n"#"W(B") 
    ("$,15q6-(B" . "$,4!m(B") ;; IS 13194 speical rule.
    ("$,15p6!(B" . "$,4!o"[(B") 
    ("$,15p6"(B" . "$,4!p"\(B") 

    ("$,15r(B" . "$,4!q")(B") 
    ("$,15r6-(B" . "$,4!q(B") 
    ("$,15s(B" . "$,4!s(B") 
    ("$,15s6-(B" . "$,4!r(B") 
    ("$,15t(B" . "$,4!s"#(B")
    ("$,15t6-(B" . "$,4!r"#(B")

    ("$,15u(B" . "$,4!t")(B")
    ("$,15u6-(B" . "$,4!t(B")
    ("$,15u6-5p(B" . "$,4!u")(B")
    ("$,15u6-5p6-(B" . "$,4!u(B")
    ;; SIBILANTS 
    ("$,15v(B" . "$,4!v")(B") 
    ("$,15v6-(B" . "$,4!v(B")
    ("$,15v6-5u(B" . "$,4!w")(B")
    ("$,15v6-5u6-(B" . "$,4!w(B")
    ("$,15v6-5p(B" . "$,4!x")(B")
    ("$,15v6-5p6-(B" . "$,4!x(B")

    ("$,15w(B" . "$,4!y")(B")
    ("$,15w6-(B" . "$,4!y(B")
    ("$,15x(B" . "$,4!z")(B")
    ("$,15x6-(B" . "$,4!z(B")
    ("$,15x6-5p(B" . "$,4!{")(B")
    ("$,15x6-5p6-(B" . "$,4!{(B")

    ("$,15y(B" . "$,4!}(B")
    ("$,15y6-(B" . "$,4!|(B")
    ("$,15y6#(B" . "$,4!~(B")
    ("$,15y6-5p(B" . "$,4!(B")
    ("$,15y6-5n(B" . "$,4" (B")
    ("$,15y6-5o(B" . "$,4"!(B")
    ;; NUKTAS    
    ("$,168(B" . "$,4 f"R"S(B")
    ("$,1686-(B" . "$,4 d(B") 
    ("$,169(B" . "$,4 k")(B") 
    ("$,1696-(B" . "$,4 k(B") 
    ("$,16:(B" . "$,4 n")(B") 
    ("$,16:6-(B" . "$,4 n(B") 
    ("$,16;(B" . "$,4 w")(B") 
    ("$,16;6-(B" . "$,4 w(B") 
    ("$,16<(B" . "$,4!#"W(B") 
    ("$,16=(B" . "$,4!'"X(B") 
    ("$,16>(B" . "$,4!b"[(B") 
    ("$,16>6-(B" . "$,4!>(B") 
    ("$,16?(B" . "$,4!j"#")(B")
    ;; misc modifiers.
    ("$,15A(B" . "\$,4"$(B")
    ("$,15B(B" . "\$,4"&(B") 
    ("$,15C(B" . "$,4 F(B")
    ("$,15|(B" . "$,4"#(B")
    ("$,15}(B" . "$,4 E(B")
    ("$,16-(B" . "$,4""(B")
    ("$,16-5p(B" . "$,4"%(B") ;; following "r"
    ;; ("$,160(B" . "$,4 D(B") 
    ;; ("$,16D(B" . "$,4 J(B")
    ;; ("$,16F(B" . "") 
    ;; ("$,16G(B" . "") 
    ;; ("$,16H(B" . "") 
    ;; ("$,16I(B" . "") 
    ;; ("$,16J(B" . "") 
    ;; ("$,16K(B" . "") 
    ;; ("$,16L(B" . "") 
    ;; ("$,16M(B" . "") 
    ;; ("$,16N(B" . "") 
    ;; ("$,16O(B" . "")
    )
  "Devanagari characters to glyphs conversion table.  
Default value contains only the basic rules.  You may add your own
preferred rule from the sanskrit fonts."  )

(defvar dev-char-glyph-hash
  (let* ((hash (makehash 'equal)))
    (mapc (function (lambda (x) (puthash (car x) (cdr x) hash)))
	  dev-char-glyph)
    hash))

(defvar dev-char-glyph-regexp
  (devanagari-regexp-of-hashtbl-keys dev-char-glyph-hash))

;; glyph-to-glyph conversion table.
;; it is supposed that glyphs are ordered in 
;;   [consonant/nukta] - [matra/halant] - [preceding-r] - [anuswar].

(defvar dev-glyph-glyph
  '(("\$,4"'(B\$,4"&(B" . "\$,4"((B")
    ("\$,4"'(B\$,4"$(B" . "\$,4"((B")
    ("$,4"*(B\$,4"&(B" . "$,4"+(B")
    ("$,4"*(B\$,4"'(B" . "$,4",(B")
    ("$,4"*(B\$,4"'(B\$,4"&(B" . "$,4"-(B")
    ("$,4"2(B\$,4"&(B" . "$,4"3(B")
    ("$,4"2(B\$,4"'(B" . "$,4"4(B")
    ("$,4"2(B\$,4"'(B\$,4"&(B" . "$,4"5(B")
    ("$,4"#(B\$,4"6(B" . "$,4"7(B")
    ("$,4"%(B\$,4"6(B" . "$,4"8(B")
    ;;("$,4"6(B" . "$,4"9(B")
    ("$,4"#(B\$,4":(B" . "$,4";(B")
    ("$,4"%(B\$,4":(B" . "$,4"<(B")
    ;;("$,4":(B" . "$,4"=(B")
    ("\$,4"@(B\$,4"&(B" . "\$,4"A(B")
    ("\$,4"@(B\$,4"'(B" . "\$,4"B(B")
    ("\$,4"@(B\$,4"'(B\$,4"&(B" . "\$,4"C(B")
    ("\$,4"D(B\$,4"&(B" . "\$,4"E(B")
    ("\$,4"D(B\$,4"'(B" . "\$,4"F(B")
    ("\$,4"D(B\$,4"'(B\$,4"&(B" . "\$,4"G(B")
    ("\$,4"H(B\$,4"&(B" . "\$,4"I(B")
    ("\$,4"H(B\$,4"'(B" . "\$,4"J(B")
    ("\$,4"H(B\$,4"'(B\$,4"&(B" . "\$,4"K(B")
    ("\$,4"L(B\$,4"&(B" . "\$,4"M(B")
    ("\$,4"L(B\$,4"'(B" . "\$,4"N(B")
    ("\$,4"L(B\$,4"'(B\$,4"&(B" . "\$,4"O(B")
    ))
(defvar dev-glyph-glyph-hash
  (let* ((hash (makehash 'equal)))
    (mapc (function (lambda (x) (puthash (car x) (cdr x) hash)))
	  dev-glyph-glyph)
    hash))
(defvar dev-glyph-glyph-regexp
  (devanagari-regexp-of-hashtbl-keys dev-glyph-glyph-hash))


;; yet another glyph-to-glyph conversions.
(defvar dev-glyph-glyph-2
  '(("$,4"*(B" . "$,4".(B")
    ("$,4"+(B" . "$,4"/(B")
    ("$,4",(B" . "$,4"0(B")
    ("$,4"-(B" . "$,4"1(B")))
(defvar dev-glyph-glyph-2-hash
  (let* ((hash (makehash 'equal)))
    (mapc (function (lambda (x) (puthash (car x) (cdr x) hash)))
	  dev-glyph-glyph-2)
    hash))
(defvar dev-glyph-glyph-2-regexp
  (devanagari-regexp-of-hashtbl-keys dev-glyph-glyph-2-hash))


(defun dev-charseq (from &optional to)
  (if (null to) (setq to from))
  (mapcar (function (lambda (x) (indian-glyph-char x 'devanagari)))
          (devanagari-range from to)))

(defvar dev-glyph-cvn
  (append 
   (dev-charseq #x2b)
   (dev-charseq #x3c #xc1)
   (dev-charseq #xc3))
  "Devanagari Consonants/Vowels/Nukta Glyphs")

(defvar dev-glyph-space
  (dev-charseq #xf0 #xfe)
  "Devanagari Spacing Glyphs")

(defvar dev-glyph-right-modifier
  (append 
   (dev-charseq #xc9)
   (dev-charseq #xd2 #xd5))
  "Devanagari Modifiers attached at the right side.")

(defvar dev-glyph-right-modifier-regexp
  (concat "[" dev-glyph-right-modifier "]"))

(defvar dev-glyph-left-matra
  (dev-charseq #xca #xd1)
  "Devanagari Matras attached at the left side.")

(defvar dev-glyph-top-matra
  (dev-charseq #xe0 #xef)
  "Devanagari Matras attached at the top side.")

(defvar dev-glyph-bottom-modifier
  (append 
   (dev-charseq #xd6 #xdf)
   (dev-charseq #xc2))
  "Devanagari Modifiers attached at the bottom.")

(defvar dev-glyph-order
  `((,dev-glyph-cvn . 1)
    (,dev-glyph-space . 2)
    (,dev-glyph-right-modifier . 3)
    (,dev-glyph-left-matra . 3) ;; processed by reference point.
    (,dev-glyph-top-matra . 4)
    (,(dev-charseq #xc7 #xc8) . 5)
    (,(dev-charseq #xc6) . 6)
    (,(dev-charseq #xc5) . 7)
    (,dev-glyph-bottom-modifier . 8)))

(mapc 
 (function (lambda (x)
   (mapc 
     (function (lambda (y)
       (put-char-code-property y 'composition-order (cdr x))))
     (car x))))
  dev-glyph-order)

(mapc
  (function (lambda (x)
    (put-char-code-property x 'reference-point '(3 . 5))))
 dev-glyph-left-matra)

(defun devanagari-compose-syllable-string (string)
  (with-temp-buffer
    (insert (decompose-string string))
    (devanagari-compose-syllable-region (point-min) (point-max))
    (buffer-string)))

(defun devanagari-compose-syllable-region (from to)
  "Compose devanagari syllable in region FROM to TO."
  (let ((glyph-str nil) (cons-num 0) glyph-str-list
        (last-halant nil) (preceding-r nil) (last-modifier nil) 
        (last-char (char-before to)) match-str
        glyph-block split-pos)
    (save-excursion
      (save-restriction
          ;;; *** char-to-glyph conversion ***
        ;; Special rule 1. -- Last halant must be preserved.
        (if (eq last-char ?$,16-(B)
            (progn
              (setq last-halant t)
              (narrow-to-region from (1- to)))
          (narrow-to-region from to)
          ;; note if the last char is modifier.
          (if (or (eq last-char ?$,15A(B) (eq last-char ?$,15B(B))
              (setq last-modifier t)))
        (goto-char (point-min))
        ;; Special rule 2. -- preceding "r halant" must be modifier.
        (when (looking-at "$,15p6-(B.")
          (setq preceding-r t)
          (goto-char (+ 2 (point))))
        ;; translate the rest characters into glyphs
        (while (re-search-forward dev-char-glyph-regexp nil t)
          (setq match-str (match-string 0))
          (setq glyph-str 
                (concat glyph-str
                        (gethash match-str dev-char-glyph-hash)))
          ;; count the number of consonant-glyhs.
          (if (string-match devanagari-consonant match-str)
              (setq cons-num (1+ cons-num))))
        ;; preceding-r must be attached before the anuswar if exists.
        (if preceding-r 
            (if last-modifier
                (setq glyph-str (concat (substring glyph-str 0 -1) 
                                        "$,4"'(B" (substring glyph-str -1)))
              (setq glyph-str (concat glyph-str "$,4"'(B"))))
        (if last-halant (setq glyph-str (concat glyph-str "$,4""(B")))
          ;;; *** glyph-to-glyph conversion ***
        (when (string-match dev-glyph-glyph-regexp glyph-str)
          (setq glyph-str
                (replace-match (gethash (match-string 0 glyph-str) 
                                        dev-glyph-glyph-hash)
                               nil t glyph-str))
          (if (and (> cons-num 1)
                   (string-match dev-glyph-glyph-2-regexp glyph-str))
              (setq glyph-str
                    (replace-match (gethash (match-string 0 glyph-str)
                                            dev-glyph-glyph-2-hash)
                                   nil t glyph-str))))
          ;;; *** glyph reordering ***
        (while (setq split-pos (string-match "$,4""(B\\|.$" glyph-str))
          (setq glyph-block (substring glyph-str 0 (1+ split-pos)))
          (setq glyph-str (substring glyph-str (1+ split-pos)))
          (setq 
           glyph-block 
           (if (string-match dev-glyph-right-modifier-regexp glyph-block)
               (sort (string-to-list glyph-block)
                     (function (lambda (x y)
                        (< (get-char-code-property x 'composition-order)
                           (get-char-code-property y 'composition-order)))))
             (sort (string-to-list glyph-block)
                   (function (lambda (x y)
                      (let ((xo (get-char-code-property x 'composition-order))
                            (yo (get-char-code-property y 'composition-order)))
                        (if (= xo 2) nil (if (= yo 2) t (< xo yo)))))))))
          (setq glyph-str-list (nconc glyph-str-list glyph-block)))
          ;; concatenate and attach reference-points.
        (setq glyph-str
              (cdr 
               (apply 
                'nconc 
                (mapcar 
                 (function (lambda (x) 
                   (list
                    (or (get-char-code-property x 'reference-point)
                    '(5 . 3) ;; default reference point.
                     )
                    x)))
                 glyph-str-list))))))
      (compose-region from to glyph-str)))

(provide 'devan-util)

;;; devan-util.el ends here
