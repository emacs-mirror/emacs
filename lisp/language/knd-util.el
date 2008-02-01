;;; knd-util.el --- Support for composing Kannada characters

;; Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008
;;   Free Software Foundation, Inc.

;; Maintainer:  Maintainer:  CHOWKSEY, Kailash C. <klchxbec@m-net.arbornet.org>
;; Keywords: multilingual, Kannada

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; Created: Jul. 14. 2003

;;; Commentary:

;; This file provides character(Unicode) to glyph(CDAC) conversion and
;; composition of Kannada script characters.

;;; Code:

;;;###autoload

;; Kannada Composable Pattern
;;    C .. Consonants
;;    V .. Vowel
;;    H .. Virama
;;    M .. Matra
;;    V .. Vowel
;;    (N .. Zerowidth Non Joiner)
;;    (J .. Zerowidth Joiner.  )
;; 1. vowel
;;  V(A)?
;; 2. syllable : maximum of 5 consecutive consonants.  (e.g. kartsnya)
;;  ((CH)?(CH)?(CH)?CH)?C(H|M?)?

(defconst kannada-consonant
  "[$,1>u(B-$,1?9(B]")

(defconst kannada-consonant-needs-twirl
  "[$,1>u>w(B-$,1>{>}(B-$,1>~? (B-$,1?"?$(B-$,1?+?-?0?3(B-$,1?9(B]\\($,1?M(B[$,1>u(B-$,1?9(B]\\)*[$,1?A?B?C?D>b(B]?$")

(defconst kannada-composable-pattern
  (concat
   "\\([$,1>b(B-$,1>t?`>l(B]\\)\\|[$,1>c(B]"
   "\\|\\("
   "\\(?:\\(?:[$,1>u(B-$,1?9(B]$,1?M(B\\)?\\(?:[$,1>u(B-$,1?9(B]$,1?M(B\\)?\\(?:[$,1>u(B-$,1?9(B]$,1?M(B\\)?[$,1>u(B-$,1?9(B]$,1?M(B\\)?"
   "[$,1>u(B-$,1?9(B]\\(?:$,1?M(B\\|[$,1?>(B-$,1?M?U?C(B]?\\)?"
   "\\)")
  "Regexp matching a composable sequence of Kannada characters.")

;;;###autoload
(defun kannada-compose-region (from to)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char (point-min))
      (while (re-search-forward kannada-composable-pattern nil t)
        (kannada-compose-syllable-region (match-beginning 0)
                                            (match-end 0))))))
;;;###autoload
(defun kannada-compose-string (string)
  (with-temp-buffer
    (insert (decompose-string string))
    (kannada-compose-region (point-min) (point-max))
    (buffer-string)))

;;;###autoload
(defun kannada-post-read-conversion (len)
  (save-excursion
    (save-restriction
      (let ((buffer-modified-p (buffer-modified-p)))
	(narrow-to-region (point) (+ (point) len))
	(kannada-compose-region (point-min) (point-max))
	(set-buffer-modified-p buffer-modified-p)
	(- (point-max) (point-min))))))

(defun kannada-range (from to)
  "Make the list of the integers of range FROM to TO."
  (let (result)
    (while (<= from to) (setq result (cons to result) to (1- to))) result))

(defun kannada-regexp-of-hashtbl-keys (hashtbl)
  "Return a regular expression that matches all keys in hashtable HASHTBL."
  (let ((max-specpdl-size 1000))
    (regexp-opt
     (sort
      (let (dummy)
	(maphash (function (lambda (key val) (setq dummy (cons key dummy)))) hashtbl)
	dummy)
      (function (lambda (x y) (> (length x) (length y))))))))

(defun kannada-regexp-of-hashtbl-vals (hashtbl)
  "Return a regular expression that matches all values in hashtable HASHTBL."
  (let ((max-specpdl-size 1000))
    (regexp-opt
     (sort
      (let (dummy)
	(maphash (function (lambda (key val) (setq dummy (cons val dummy)))) hashtbl)
	dummy)
      (function (lambda (x y) (> (length x) (length y))))))))

;;;###autoload
(defun kannada-composition-function (pos &optional string)
  "Compose Kannada characters after the position POS.
If STRING is not nil, it is a string, and POS is an index to the string.
In this case, compose characters after POS of the string."
  (if string
      ;; Not yet implemented.
      nil
    (goto-char pos)
    (if (looking-at kannada-composable-pattern)
	(prog1 (match-end 0)
	  (kannada-compose-syllable-region pos (match-end 0))))))

;; Notes on conversion steps.

;; 1. chars to glyphs
;;
;; Rules will not be applied to the virama appeared at the end of the
;; text.  Also, the preceding/following "r" will be treated as special case.

;; 2. glyphs reordering.
;;
;; The glyphs are split by virama, and each glyph groups are
;; re-ordered in the following order.
;;
;; Note that `consonant-glyph' mentioned here does not contain the
;; vertical bar (right modifier) attached at the right of the
;; consonant.
;;
;; If the glyph-group contains right modifier,
;;  (1) consonant-glyphs/vowels
;;  (2) spacing
;;  (3) right modifier (may be matra)
;;  (4) top matra
;;  (5) preceding "r"
;;  (7) following "r"
;;  (8) bottom matra or virama.
;;
;; Otherwise,
;;  (1) consonant-glyph/vowels, with nukta sign
;;  (3) left matra
;;  (4) top matra
;;  (5) preceding "r"
;;  (7) following "r"
;;  (8) bottom matra or virama.
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

(defvar knd-char-glyph
  '(("$,1>e(B" . "$,43@(B")
    ("$,1>f(B" . "$,43A(B")
    ("$,1?>(B" . "$,44{(B")
    ("$,1>g(B" . "$,43B(B")
    ("$,1??(B" . nil)
    ("$,1>h(B" . "$,43C(B")
    ("$,1?@(B" . nil)
    ("$,1>i(B" . "$,43D(B")
    ("$,1?A(B" . "\$,44(B")
    ("$,1>j(B" . "$,43E(B")
    ("$,1?B(B" . "\$,45 (B")
    ("$,1>k(B" . "$,43F4(B")
    ("$,1?C(B" . "\$,45$(B")
    ("$,1?`(B" . "$,43F5 (B")
    ("$,1?D(B" . "\$,45%(B")
    ;;("$,1>l(B" . nil) ; not implemented.
    ;;("$,1?a(B" . nil)
    ("$,1>n(B" . "$,43G(B")
    ("$,1>o(B" . "$,43H(B")
    ("$,1>p(B" . "$,43I(B")
    ("$,1?F(B" . "\$,45&(B")
    ("$,1?G(B" . "\$,45&4~(B")
    ("$,1?H(B" . "\$,45&5'(B")
    ("$,1>r(B" . "$,43J(B")
    ("$,1?J(B" . "$,45&5 (B")
    ("$,1>s(B" . "$,43K(B")
    ("$,1?K(B" . "\$,45&5 4~(B")
    ("$,1>t(B" . "$,43L(B")
    ("$,1?L(B" . "\$,45((B")
    ("$,1>b(B" . "$,43M(B")
    ("$,1>c(B" . "$,43N(B")
    ("$,1>u?M(B" . "$,43O5)(B") ("$,1>u(B" . "$,43O(B") ("$,1>u??(B" . "$,43P(B") ("$,1>u?@(B" . "$,43P4~(B")
    ("$,1>v?M(B" . "$,43S5)(B") ("$,1>v(B" .  "$,43S(B") ("$,1>v??(B" . "$,43T(B") ("$,1>v?@(B" . "$,43T4~(B") ("$,1>v?F(B" . "$,43S5&(B") ("$,1>v?G(B" . "$,43S5&4~(B") ("$,1>v?H(B" . "$,43S5&5'(B") ("$,1>v?J(B" .  "$,43S5&5&5 (B") ("$,1>v?K(B" . "$,43S5&5&5 4~(B") ("$,1>v?L(B" . "$,43S5((B")
    ("$,1>w?M(B" . "$,43V5)(B") ("$,1>w(B" . "$,43V(B") ("$,1>w??(B" . "$,43W(B") ("$,1>w?@(B" . "$,43W4~(B")
    ("$,1>x?M(B" . "$,43Y5)(B") ("$,1>x(B" . "$,43Y(B") ("$,1>x??(B" . "$,43Z(B") ("$,1>x?@(B" . "$,43Z4~(B")
    ("$,1>y?M(B" . "$,43\5)(B") ("$,1>y(B" . "$,43\(B")
    ("$,1>z?M(B" . "$,43^5)(B") ("$,1>z(B" . "$,43^(B") ("$,1>z??(B" . "$,43_(B") ("$,1>z?@(B" . "$,43_4~(B")
    ("$,1>{?M(B" . "$,43a5)(B") ("$,1>{(B" . "$,43a(B") ("$,1>{??(B" . "$,43b(B") ("$,1>{?@(B" . "$,43b4~(B")
    ("$,1>|?M(B" . "$,43d5)(B") ("$,1>|(B" . "$,43d(B") ("$,1>|??(B" . "$,43f(B") ("$,1>|?@(B" . "$,43f4~(B") ("$,1>|?F(B" . "$,43e5&(B") ("$,1>|?G(B" . "$,43e5&4~(B") ("$,1>|?H(B" . "$,43e5&5'(B") ("$,1>|?J(B" .  "$,43e5&5&5 (B") ("$,1>|?K(B" . "$,43e5&5&5 4~(B") ("$,1>|?L(B" . "$,43e5((B")
    ("$,1>}?M(B" . "$,44a4z3h45)(B") ("$,1>}(B" . "$,44a4z3h4(B") ("$,1>}??(B" . "$,44b3h4(B") ("$,1>}?@(B" . "$,44b3h44~(B") ("$,1>}?B(B". "$,44a4z3h5 (B") ("$,1>}?J(B". "$,44a5&3h5 (B") ("$,1>}?K(B". "$,44a5&3h5 4~(B")
    ("$,1>~?M(B" . "$,43j5)(B") ("$,1>~(B" . "$,43j(B")
    ("$,1>?M(B" . "$,43m5)(B") ("$,1>(B" . "$,43l(B") ("$,1?#?>(B" . "$,43m4{(B") ("$,1>??(B" . "$,43n(B") ("$,1>?@(B" . "$,43n4~(B") ("$,1>?F(B" . "$,43m5&(B") ("$,1>?G(B" . "$,43m5&4~(B") ("$,1>?H(B" . "$,43m5&5'(B") ("$,1>?J(B" .  "$,43m5&5&5 (B") ("$,1>?K(B" . "$,43m5&5&5 4~(B") ("$,1>?L(B" . "$,43m5((B")
    ("$,1? ?M(B" . "$,43p5)(B") ("$,1? (B" . "$,43p(B") ("$,1? ??(B" . "$,43q(B") ("$,1? ?@(B" . "$,43q4~(B")
    ("$,1?!?M(B" . "$,43s5)(B") ("$,1?!(B" . "$,43s(B") ("$,1?!??(B" . "$,43t(B") ("$,1?!?@(B" . "$,43t4~(B")
    ("$,1?"?M(B" . "$,43v5)(B") ("$,1?"(B" . "$,43v(B") ("$,1?"??(B" . "$,43w(B") ("$,1?"?@(B" . "$,43w4~(B")
    ("$,1?#?M(B" . "$,43z5)(B") ("$,1?#(B" . "$,43y(B") ("$,1?#?>(B" . "$,43z4{(B") ("$,1?#??(B" . "$,43{(B") ("$,1?#?@(B" . "$,43{4~(B") ("$,1?#?F(B" . "$,43z5&(B") ("$,1?#?G(B" . "$,43z5&4~(B") ("$,1?#?H(B" . "$,43z5&5'(B") ("$,1?#?J(B" .  "$,43z5&5&5 (B") ("$,1?#?K(B" . "$,43z5&5&5 4~(B") ("$,1?#?L(B" . "$,43z5((B")
    ("$,1?$?M(B" . "$,43}5)(B") ("$,1?$(B" . "$,43}(B") ("$,1?$??(B" . "$,43~(B") ("$,1?$?@(B" . "$,43~4~(B")
    ("$,1?%?M(B" . "$,44B5)(B") ("$,1?%(B" . "$,44B(B") ("$,1?%??(B" . "$,44C(B") ("$,1?%?@(B" . "$,44C4~(B")
    ("$,1?&?M(B" . "$,44E5)(B") ("$,1?&(B" . "$,44E(B") ("$,1?&??(B" . "$,44F(B") ("$,1?&?@(B" . "$,44F4~(B")
    ("$,1?'?M(B" . "$,44H5)(B") ("$,1?'(B" . "$,44H(B") ("$,1?'??(B" . "$,44I(B") ("$,1?'?@(B" . "$,44I4~(B")
    ("$,1?(?M(B" . "$,44K5)(B") ("$,1?((B" . "$,44K(B") ("$,1?(??(B" . "$,44L(B") ("$,1?(?@(B" . "$,44L4~(B")
    ("$,1?*?M(B" . "$,44N5)(B") ("$,1?*(B" . "$,44N(B") ("$,1?*??(B" . "$,44O(B") ("$,1?*?@(B" . "$,44O4~(B") ("$,1?*?A(B" . "$,44N5"(B") ("$,1?*?B(B" . "$,44N5#(B") ("$,1?*?J(B" . "$,44N5&5#(B") ("$,1?*?K(B" . "$,44N5&5#4~(B")
    ("$,1?+?M(B" . "$,44Q5)(B") ("$,1?+(B" . "$,44Q(B") ("$,1?+??(B" . "$,44R(B") ("$,1?+?@(B" . "$,44R4~(B") ("$,1?+?A(B" . "$,44Q5"(B") ("$,1?+?B(B" . "$,44Q5#(B") ("$,1?+?J(B" . "$,44Q5&5#(B") ("$,1?+?K(B" . "$,44Q5&5#4~(B")
    ("$,1?,?M(B" . "$,44W5)(B") ("$,1?,(B" . "$,44V(B") ("$,1?,?>(B". "$,44W4{(B") ("$,1?,??(B" . "$,44X(B") ("$,1?,?@(B" . "$,44X4~(B") ("$,1?,?F(B" . "$,44W5&(B") ("$,1?,?G(B" . "$,44W5&4~(B") ("$,1?,?H(B" . "$,44W5&5'(B") ("$,1?,?J(B" .  "$,44W5&5&5 (B") ("$,1?,?K(B" . "$,44W5&5&5 4~(B") ("$,1?,?L(B" . "$,44W5((B")
    ("$,1?-?M(B" . "$,44Z5)(B") ("$,1?-(B" . "$,44Z(B") ("$,1?-??(B" . "$,44[(B") ("$,1?-?@(B" . "$,44[4~(B")
    ("$,1?.?M(B" . "$,44h5!5)(B") ("$,1?.(B" . "$,44h4z4(B") ("$,1?.?>(B" . "$,44h4z5!4{(B") ("$,1?.??(B" . "$,44i4(B") ("$,1?.?@(B" . "$,44i44~(B") ("$,1?.?J(B". "$,44h5&5 (B") ("$,1?.?K(B". "$,44h5&5 4~(B")
    ("$,1?/?M(B" . "$,44^4z5!5)(B") ("$,1?/(B" . "$,44^4z4(B") ("$,1?/?>(B" . "$,44^4z5!4{(B")("$,1?/??(B" . "$,44_4(B") ("$,1?/?@(B" . "$,44_44~(B") ("$,1?/?J(B" . "$,44^5&5 (B") ("$,1?/?K(B" . "$,44^5&5 4~(B")
    ("$,1?0?M(B" . "$,44a5)(B") ("$,1?0(B" . "$,44a(B") ("$,1?0??(B" . "$,44b(B") ("$,1?0?@(B" . "$,44b4~(B")
    ("$,1?0?M(B" . "$,44a5)(B") ("$,1?0(B" . "$,44a(B") ("$,1?0??(B" . "$,44b(B") ("$,1?0?@(B" . "$,44b4~(B")
    ("$,1?2?M(B" . "$,44e5)(B") ("$,1?2(B" . "$,44d(B") ("$,1?2?>(B" . "$,44e4{(B") ("$,1?2??(B" . "$,44f(B") ("$,1?2?@(B" . "$,44f4~(B") ("$,1?2?F(B" . "$,44e5&(B") ("$,1?2?G(B" . "$,44e5&4~(B") ("$,1?2?H(B" . "$,44e5&5'(B") ("$,1?2?J(B" .  "$,44e5&5&5 (B") ("$,1?2?K(B" . "$,44e5&5&5 4~(B") ("$,1?2?L(B" . "$,44e5((B")
    ("$,1?5?M(B" . "$,44h5)(B") ("$,1?5(B" . "$,44h(B") ("$,1?5??(B" . "$,44i(B") ("$,1?5?@(B" . "$,44i4~(B") ("$,1?5?A(B" . "$,44h5"(B") ("$,1?5?B(B" . "$,44h5#(B") ("$,1?5?J(B" . "$,44h5&5#(B") ("$,1?5?K(B" . "$,44h5&5#4~(B")
    ("$,1?6?M(B" . "$,44k5)(B") ("$,1?6(B" . "$,44k(B") ("$,1?6??(B" . "$,44l(B") ("$,1?6?@(B" . "$,44l4~(B")
    ("$,1?7?M(B" . "$,44n5)(B") ("$,1?7(B" . "$,44n(B") ("$,1?7??(B" . "$,44o(B") ("$,1?7?@(B" . "$,44o4~(B")
    ("$,1?8?M(B" . "$,44q5)(B") ("$,1?8(B" . "$,44q(B") ("$,1?8??(B" . "$,44r(B") ("$,1?8?@(B" . "$,44r4~(B")
    ("$,1?9?M(B" . "$,44t5)(B") ("$,1?9(B" . "$,44t(B") ("$,1?9??(B" . "$,44u(B") ("$,1?9?@(B" . "$,44u4~(B")
    ("$,1?3?M(B" . "$,44w5)(B") ("$,1?3(B" . "$,44w(B") ("$,1?3??(B" . "$,44x(B") ("$,1?3?@(B" . "$,44x4~(B"))
"Kannada characters to glyphs conversion table.
Default value contains only the basic rules.")

(defvar knd-char-glyph-hash
  (let* ((hash (make-hash-table :test 'equal)))
    (mapc (function (lambda (x) (puthash (car x) (cdr x) hash)))
	  knd-char-glyph)
    hash))

(defvar knd-char-glyph-regexp
  (kannada-regexp-of-hashtbl-keys knd-char-glyph-hash))

(defvar knd-conjunct-glyph
  '(("$,1>u(B" . "$,43Q(B") ("$,1>v(B" . "$,43U(B") ("$,1>w(B" . "$,43X(B") ("$,1>x(B" . "$,43[(B") ("$,1>y(B" . "$,43](B")
    ("$,1>z(B" . "$,43`(B") ("$,1>{(B" . "$,43c(B") ("$,1>|(B" . "$,43g(B") ("$,1>}(B" . "$,43i(B") ("$,1>~(B" . "$,43k(B")
    ("$,1>(B" . "$,43o(B") ("$,1? (B" . "$,43r(B") ("$,1?!(B" . "$,43u(B") ("$,1?"(B" . "$,43x(B") ("$,1?#(B" . "$,43|(B")
    ("$,1?$(B" . "$,44A(B") ("$,1?%(B" . "$,44D(B") ("$,1?&(B" . "$,44G(B") ("$,1?'(B" . "$,44J(B") ("$,1?((B" . "$,44M(B")
    ("$,1?*(B" . "$,44P(B") ("$,1?+(B" . "$,44U(B") ("$,1?,(B" . "$,44Y(B") ("$,1?-(B" . "$,44\(B") ("$,1?.(B" . "$,44](B")
    ("$,1?/(B" . "$,44`(B") ("$,1?0(B" . "$,44c(B") ("$,1?2(B" . "$,44g(B") ("$,1?3(B" . "$,44y(B") ("$,1?5(B" . "$,44j(B")
    ("$,1?6(B" . "$,44m(B") ("$,1?7(B" . "$,44p(B") ("$,1?8(B" . "$,44s(B") ("$,1?9(B" . "$,44v(B"))
"Kannada characters to conjunct glyphs conversion table.")

(defvar knd-conjunct-glyph-hash
  (let* ((hash (make-hash-table :test 'equal)))
    (mapc (function (lambda (x) (puthash (car x) (cdr x) hash)))
	  knd-conjunct-glyph)
    hash))
    
(defvar knd-conjunct-glyph-regexp
  (kannada-regexp-of-hashtbl-vals knd-conjunct-glyph-hash))

(mapc
  (function (lambda (x)
    (put-char-code-property (aref (cdr x) 0) 'reference-point '(5 . 3))))
  knd-conjunct-glyph)

;; glyph-to-glyph conversion table.
;; it is supposed that glyphs are ordered in
;;   [consonant/nukta] - [matra/virama] - [preceding-r] - [anuswar].

(defvar knd-glyph-glyph
  '(("$,45$4A(B" . "$,45*(B")
    ("$,45'4A(B" . "$,45+(B")
    ("$,44A3g(B" . "$,45,(B")
    ("$,45$3Q(B" . "$,45-(B")))

(defvar knd-glyph-glyph-hash
  (let* ((hash (make-hash-table :test 'equal)))
    (mapc (function (lambda (x) (puthash (car x) (cdr x) hash)))
	  knd-glyph-glyph)
    hash))
(defvar knd-glyph-glyph-regexp
  (kannada-regexp-of-hashtbl-keys knd-glyph-glyph-hash))

(defun knd-charseq (from &optional to)
  (if (null to) (setq to from))
  (number-sequence (decode-char 'kannada-cdac from)
		   (decode-char 'kannada-cdac to)))

(defvar knd-glyph-cv
  (append
   (knd-charseq #x40 #x50)
   (knd-charseq #x52 #x54)
   (knd-charseq #x56 #x57)
   (knd-charseq #x59 #x5a)
   (knd-charseq #x5c)
   (knd-charseq #x5e #x5f)
   (knd-charseq #x61 #x62)
   (knd-charseq #x64 #x66)
   (knd-charseq #x6a)
   (knd-charseq #x6c #x6e)
   (knd-charseq #x70 #x71)
   (knd-charseq #x73 #x74)
   (knd-charseq #x76 #x77)
   (knd-charseq #x79 #x7b)
   (knd-charseq #x7d #x7e)
   (knd-charseq #xa2 #xa3)
   (knd-charseq #xa5 #xa6)
   (knd-charseq #xa8 #xa9)
   (knd-charseq #xab #xac)
   (knd-charseq #xae #xaf)
   (knd-charseq #xb1 #xb2)
   (knd-charseq #xb6 #xb8)
   (knd-charseq #xb6 #xb8)
   (knd-charseq #xba #xbb)
   (knd-charseq #xbe #xbf)
   (knd-charseq #xc1 #xc2)
   (knd-charseq #xc4 #xc6)
   (knd-charseq #xc8 #xc9)
   (knd-charseq #xcb #xcc)
   (knd-charseq #xce #xcf)
   (knd-charseq #xd1 #xd2)
   (knd-charseq #xd4 #xd5)
   (knd-charseq #xd7 #xd8)
   (knd-charseq #xc3))
  "Kannada Consonants/Vowels/Nukta Glyphs")

(defvar knd-glyph-space
  (knd-charseq #xb3 #xb4)
  "Kannada Spacing Glyphs")

(defvar knd-glyph-right-modifier
  (append
   (knd-charseq #xdb #xdd)
   (knd-charseq #xdf)
   (knd-charseq #xe0 #xe3)
   (knd-charseq #xe9))
  "Kannada Modifiers attached at the right side.")

(defvar knd-glyph-right-modifier-regexp
  (concat "[" knd-glyph-right-modifier "]"))

(defvar knd-glyph-jha-tail
   (knd-charseq #x68)
  "Kannada tail for jha.")

(defvar knd-glyph-top-matra
  (append
   (knd-charseq #xda)
   (knd-charseq #xdd)
   (knd-charseq #xe6)
   (knd-charseq #xe8))
  "Kannada Matras attached at the top side.")

(defvar knd-glyph-bottom-matra
  (append
   (knd-charseq #xe4 #xe5)
   (knd-charseq #xe7))
  "Kannada Matras attached at the bottom.")

(defvar knd-glyph-end-marks
  (append
   (knd-charseq #x25)
   (knd-charseq #x4d #x4e)
   (knd-charseq #xde))
  "Kannada end marks: arkavattu, virama, au and diirghaa.")

(defvar knd-glyph-bottom-modifier
  (append
   (knd-charseq #x51)
   (knd-charseq #x55)
   (knd-charseq #x58)
   (knd-charseq #x5b)
   (knd-charseq #x5d)
   (knd-charseq #x60)
   (knd-charseq #x63)
   (knd-charseq #x67)
   (knd-charseq #x69)
   (knd-charseq #x6b)
   (knd-charseq #x6f)
   (knd-charseq #x72)
   (knd-charseq #x75)
   (knd-charseq #x78)
   (knd-charseq #x7c)
   (knd-charseq #xa1)
   (knd-charseq #xa4)
   (knd-charseq #xa7)
   (knd-charseq #xaa)
   (knd-charseq #xad)
   (knd-charseq #xb0)
   (knd-charseq #xb5)
   (knd-charseq #xb9)
   (knd-charseq #xbc #xbd)
   (knd-charseq #xc0)
   (knd-charseq #xc3)
   (knd-charseq #xc7)
   (knd-charseq #xca)
   (knd-charseq #xcd)
   (knd-charseq #xd0)
   (knd-charseq #xd3)
   (knd-charseq #xd6)
   (knd-charseq #xd9)
   (knd-charseq #xea #xef))
  "Kannada Modifiers attached at the bottom.")

(defvar knd-glyph-order
  `((,knd-glyph-cv . 1)
    (,knd-glyph-top-matra . 2)
    (,knd-glyph-jha-tail . 3)
    (,knd-glyph-right-modifier . 4)
    (,knd-glyph-space . 5)
    (,knd-glyph-bottom-modifier . 5)
    (,knd-glyph-bottom-matra . 6)
    (,knd-glyph-end-marks . 7)
    ))

(mapc
 (function (lambda (x)
   (mapc
     (function (lambda (y)
       (put-char-code-property y 'composition-order (cdr x))))
     (car x))))
  knd-glyph-order)

(defun kannada-compose-syllable-string (string)
  (with-temp-buffer
    (insert (decompose-string string))
    (kannada-compose-syllable-region (point-min) (point-max))
    (buffer-string)))

;; kch
(defun kannada-compose-syllable-region (from to)
  "Compose kannada syllable in region FROM to TO."
  (let ((glyph-str nil) (cons-num 0) (glyph-str-list nil)
        (last-virama nil) (preceding-r nil) (last-modifier nil)
        (last-char (char-before to)) match-str pos
        glyph-block split-pos (conj nil) (rest nil))
    (save-excursion
      (save-restriction
          ;;; *** char-to-glyph conversion ***
        ;; Special rule 1. -- Last virama must be preserved.
        (if (eq last-char ?$,1?M(B)
            (progn
              (setq last-virama t)
              (narrow-to-region from (1- to)))
          (narrow-to-region from to))
        (goto-char (point-min))
        ;; Special rule 2. -- preceding "r virama" must be modifier.
        (when (looking-at "$,1?0?M(B.")
          (setq preceding-r t)
          (goto-char (+ 2 (point))))
	;; remove conjunct consonants
        (while (re-search-forward knd-char-glyph-regexp nil t)
          (setq match-str (match-string 0))
	  (if (and (string-match kannada-consonant match-str)
		   (> cons-num 0))
	      (progn
		(setq conj (concat conj (gethash (match-string 0 match-str)
						 knd-conjunct-glyph-hash)))
		(setq match-str (replace-match "" t nil match-str))
		(if (string-match "$,1?M(B" rest)
		    (setq rest (replace-match "" t nil rest)))))
	  (setq rest (concat rest match-str))
	  ;; count the number of consonant-glyhs.
	  (if (string-match kannada-consonant match-str)
	      (setq cons-num (1+ cons-num))))
        ;; translate the rest characters into glyphs
	(setq pos 0)
        (while (string-match knd-char-glyph-regexp rest pos)
          (setq match-str (match-string 0 rest))
	  (setq pos (match-end 0))
	  (setq glyph-str
		(concat glyph-str (gethash match-str knd-char-glyph-hash))))

	(if conj (setq glyph-str (concat glyph-str conj)))
        (if last-virama (setq glyph-str (concat glyph-str "$,45)(B"))
	  (goto-char (point-min))
	  (if (re-search-forward kannada-consonant-needs-twirl nil t)
	      (progn
		(setq match-str (match-string 0))
		(setq glyph-str (concat glyph-str "$,44z(B")))))
        ;; preceding-r must be attached
        (if preceding-r
              (setq glyph-str (concat glyph-str "$,43%(B")))
          ;;; *** glyph-to-glyph conversion ***
        (when (string-match knd-glyph-glyph-regexp glyph-str)
          (setq glyph-str
                (replace-match (gethash (match-string 0 glyph-str)
                                        knd-glyph-glyph-hash)
                               nil t glyph-str)))
          ;;; *** glyph reordering ***
        (while (setq split-pos (string-match "$,45)(B\\|.$" glyph-str))
          (setq glyph-block (substring glyph-str 0 (1+ split-pos)))
          (setq glyph-str (substring glyph-str (1+ split-pos)))
          (setq
           glyph-block
               (sort (string-to-list glyph-block)
                     (function (lambda (x y)
                        (< (get-char-code-property x 'composition-order)
                           (get-char-code-property y 'composition-order))))))
          (setq glyph-str-list (nconc glyph-str-list glyph-block)))
	  ;;; *** insert space glyphs for kerning ***
	(if (> cons-num 0)
	    (let ((curr glyph-str-list) (prev nil) (last-bott nil) bott co)
	      (while curr
		(setq co (get-char-code-property 
				(car curr) 'composition-order)
		      bott (or (eq co 5) (eq co 6)))
		(if (and bott last-bott)
		    (setcdr prev (cons ?$,44T(B curr)))
		(setq last-bott bott prev curr curr (cdr curr)))))
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

(provide 'knd-util)

;;; arch-tag: 78d32230-a960-46a5-b622-61ed6ffcf8fc
;;; knd-util.el ends here
