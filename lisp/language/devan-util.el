;;; devan-util.el --- Support for Devanagari Script Composition

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

;; Devanagari script composition rules and related programs.

;;; Code:

;;;
;;;   Steps toward composition of Devanagari Characters.
;;;

;;; Basic functions.

;;;###autoload
(defun indian-to-devanagari (ch)
  "Convert IS 13194 characters to Devanagari basic characters."
  (let ((charcodes (split-char ch)))
    (if (eq (car charcodes) 'indian-is13194)
	(make-char 'indian-2-column ?\x21 (nth 1 charcodes))
      ch)))

;;;###autoload
(defun devanagari-to-indian (ch)
  "Convert Devanagari basic characters to IS 13194 characters."
  (let* ((charcodes (split-char ch))
	 (charset (car charcodes))
	 (code-h  (car (cdr charcodes))))
    (if (and (eq (car charcodes) 'indian-2-column)
	     (= (nth 1 charcodes) ?\x21))
	(make-char 'indian-is13194 (nth 2 charcodes))
      ch)))

;;;###autoload
(defun indian-to-devanagari-region (from to)
  "Convert IS 13194 characters in region to Devanagari basic characters."
  (interactive "r")
  (save-restriction 
    (narrow-to-region from to)
    (goto-char (point-min))
    (while (re-search-forward "\\cd" nil t)
      (let* ((devanagari-char (indian-to-devanagari (preceding-char))))
	(delete-char -1)
	(insert devanagari-char)))))

;;;###autoload
(defun devanagari-to-indian-region (from to)
  "Convert Devanagari basic characters in region to Indian characters."
  (interactive "r")
  (save-restriction
    (narrow-to-region from to)
    (goto-char (point-min))
    (while (re-search-forward "\\cD" nil t) ; Devanagari Character Code.
      (let* ((indian-char (devanagari-to-indian (preceding-char))))
	(delete-char -1)
	(insert indian-char)))))

;;;###autoload
(defun indian-to-devanagari-string (str)
  "Convert Indian String to Devanagari Basic Character String."
  (let ((pos 0) (dst "") (src str) char)
    (while (not (equal src ""))
      (setq char (string-to-char src))
      (setq src (substring src (char-bytes char)))
      (setq dst (concat dst (char-to-string (indian-to-devanagari char)))))
    dst))

;; Phase 0 - Determine whether the characters can be composed.
;;
;;;
;;; Regular expressions to split characters for composition.
;;;
;;
;; Indian script word contains one or more syllables.
;; In BNF, it can be expressed as follows:
;;
;; Word ::= {Syllable} [Cons-Syllable]
;; Syllable ::= Cons-Vowel-Syllable | Vowel-Syllable
;; Vowel-Syllable ::= V[D]
;; Cons-Vowel-Syllable ::= [Cons-Syllable] Full-Cons [M] [D]
;; Cons-Syllable ::= [Pure-Cons] [Pure-Cons] [Pure-Cons] Pure-Cons
;; Pure-Cons ::= Full-Cons H
;; Full-Cons ::= C [N]
;;
;; {} repeat, [] optional
;;
;; C - Consonant ($(5!3!4!5!6!7!8!9!:!;!<!=!>!?!@!A!B!C!D!E(B
;;                $(5!F!G!H!I!J!K!L!M!N!O!P!Q!R!S!T!U!V!W!X(B)
;; N - Nukta ($(5!i(B)
;; H - Halant($(5!h(B) or Virama
;; V - Vowel ($(5!$!%!&!'!(!)!*!+!,!-!.!/!0!1!2#&#'#*(B)
;;     ("$(5#&#'#*(B" can be obtained by IS13194 vowels with nukta.)
;; D - Vowel Modifiers, i.e. Anuswar, Chandrabindu, Visarga  ($(5!!!"!#(B)
;; M - Matra ($(5!Z![!\!]!^!_!`!a!b!c!d!e!f!g#K#L#M(B)
;;     ("$(5#K#L#M(B" can be obtained by IS13194 matras with nukta.)
;;
;; In Emacs, one syllable of Indian language is considered to be one 
;; composite glyph.  If we expand the above expression, it would be:
;;
;; [[C [N] H] [C [N] H] [C [N] H] C [N] H] C [N] [M] [D] | V [D]
;; 
;; Therefore, in worst case, the consonant syllabe will consist of
;; following characters.
;;
;; C N H C N H C N H C N H C N M D
;;
;; The example is a sanskrit word "kaurtsnya", where five consecutive
;; consonant appears.
;;
;; On the other hand, incomplete consonant syllable before inputting
;; base consonant must satisfy the following condition:
;;
;; [C [N] H] [C [N] H] [C [N] H] C [N] H
;;
;; This is acceptable BEFORE proper consonant-syllable is input.  The
;; string which doesn't match with the above expression is invalid and
;; thus must be fixed.
;;
;; Note:
;; Third case can be considered, which is acceptable syllable and can
;; not add any code more.
;;
;; [[C [N] H] [C [N] H] [C [N] H] C [N] H] C [N] [M] D
;;
;; However, to make editing possible even in this condition, we will
;; not consider about this case.

(defconst devanagari-cons-syllable-examine
  "\\(\\([$(5!3(B-$(5!X(B]$(5!i(B?$(5!h(B\\)?\\([$(5!3(B-$(5!X(B]$(5!i(B?$(5!h(B\\)?[$(5!3(B-$(5!X(B]$(5!i(B?$(5!h(B\\)?[$(5!3(B-$(5!X(B]$(5!i(B?\\([$(5!Z(B-$(5!g#K#L#M(B]\\|\\($(5!_!i(B\\)\\|\\($(5![!i(B\\)\\|\\($(5!\!i(B\\)\\)?[$(5!!!"!#(B]?"
  "Regexp matching to one Devanagari consonant syllable.")

(defconst devanagari-cons-syllable-incomplete-examine
  "\\([$(5!3(B-$(5!X(B]$(5!i(B?$(5!h(B\\)?\\([$(5!3(B-$(5!X(B]$(5!i(B?$(5!h(B\\)?\\([$(5!3(B-$(5!X(B]$(5!i(B?$(5!h(B\\)?[$(5!3(B-$(5!X(B]$(5!i(B?$(5!h(B$"
  "Regexp matching to one Devanagari incomplete consonant syllable.")

(defconst devanagari-vowel-syllable-examine
  "\\([$(5!$(B-$(5!2#&#'#*(B]\\|\\($(5!*!i(B\\)\\|\\($(5!&!i(B\\)\\|\\($(5!'!i(B\\)\\)[$(5!!!"!#(B]?"
  "Regexp matching to one Devanagari vowel syllable.")

;;
;; Also, digits and virams should be processed other than syllables.
;;
;; In IS 13194, Avagrah is obtained by Nukta after Viram, and
;; OM is obtained by Nukta after Chandrabindu
;;
(defconst devanagari-digit-viram-examine 
  "[$(5!q(B-$(5!z!j(B]")
(defconst devanagari-other-sign-examine
  "\\([$(5!!!j(B]$(5!i(B\\)\\|\\([$(5#!#J(B]\\)")

(defconst devanagari-composite-glyph-unit-examine
  (concat "\\(" devanagari-cons-syllable-incomplete-examine 
	  "\\)\\|\\(" devanagari-vowel-syllable-examine 
	  "\\)\\|\\(" devanagari-digit-viram-examine
	  "\\)\\|\\(" devanagari-cons-syllable-examine
	  "\\)\\|\\(" devanagari-other-sign-examine"\\)")
  "Regexp matching to Devanagari string to be composed form one glyph.")

;;(put-charset-property charset-devanagari-1-column
;;		      'char-to-glyph 'devanagari-compose-string)
;;(put-charset-property charset-devanagari-2-column
;;		      'char-to-glyph 'devanagari-compose-string)

;; Sample
;;
;;(string-match devanagari-cons-syllable-examine "$(5!X![(B") => 0
;;(string-match devanagari-cons-syllable-examine "$(5!F!h!D!\(B") => 0
;;(string-match devanagari-cons-syllable-examine "$(5!X![!F!h!D!\(B") => 0

;;
;; Steps toward the composition
;;  Converting Character Code to Composite Glyph.
;;
;; Example : $(5!X![(B/$(5!F!h!D!\(B
;; 
;; First, convert Characters to appropriate glyphs.
;;
;; => $(5!X![(B/$(5"F!D!\(B
;;
;; Then, determine the base glyph, apply-orders and apply-rules.
;;
;; => $(5!X(B (ml.mr) $(5![(B / $(5!D(B (ml.mr) $(5"F(B (mr ml) $(5!\(B
;;
;; Finally, convert 2-column glyphs to 1-column glyph
;; if such a glyph exist.
;;
;; => $(6![(B (ml.mr) $(6!X(B / $(6!D(B (ml.mr) $(6"F(B (mr ml) $(6!\(B
;;
;; Compose the glyph.
;;
;; => 2$(6!X@![1(B/2$(6!D@"FP!\1(B
;; => 2$(6!X@![12!D@"FP!\1(B
;;

;;
;; Phase 1: Converting Character Code to Glyph Code.
;; 
;;
;; IMPORTANT:  
;;        There may be many rules which you many want to be suppressed.
;;        In that case, please comment out that rule.
;;
;;        RULES WILL BE EVALUATED FROM FIRST TO LAST.
;;        PUT MORE SPECIFIC RULES FIRST.
;;
;; TO DO: 
;;        Prepare multiple specific list of rules for each languages
;;        which adopts Devanagari script.
;;


(defconst devanagari-char-to-glyph-rules
  '(
    ;; special form for "ru".
    ("\\($(5!O!](B\\)" . "$(5",(B")
    ("\\($(5!O!^(B\\)" . "$(5"-(B")
    ("\\($(5!P!](B\\)" . "$(5".(B")
    ("\\($(5!P!^(B\\)" . "$(5"/(B")

    ;; `r' at the top of syllable and followed by other consonants.
    ;; ("[^$(5!h(B]\\($(5!O!h(B\\)[$(5!3(B-$(5!X(B]" . "$(5"p(B")
    ("^\\($(5!O!h(B\\)[$(5!3(B-$(5!X(B]" . "$(5"p(B")

    ;; Half Form Ligature
    ;; Here is the half-form ligature which has higher priority than
    ;; the common ligature rules listed below.
    ;; special forms.
    ("\\($(5!3!h!V!h(B\\)[$(5!3(B-$(5!X(B]" . "$(5"l(B")
    ("\\($(5!:!h!<!h(B\\)[$(5!3(B-$(5!X(B]" . "$(5"m(B")
    ;; Ordinary forms.
    ("\\($(5!B!h!B!h(B\\)[$(5!3(B-$(5!X(B]" . "$(5"c(B")
    ("\\($(5!F!h!F!h(B\\)[$(5!3(B-$(5!X(B]" . "$(5"k(B")

    ;; If "r" is preceded by the vowel-suppressed consonant
    ;; (especially those with vertical line), it will be written as
    ;; slanted line below the preceding consonant character.  Some of
    ;; them are pre-composed as one glyph.

    ("\\($(5!:!i!h!O(B\\)" . "$(5"!(B")
    ("\\($(5!I!i!h!O(B\\)" . "$(5""(B")
    ("\\($(5!3!h!O(B\\)" . "$(5"#(B")
    ("\\($(5!5!h!O(B\\)" . "$(5"$(B")
    ("\\($(5!B!h!O(B\\)" . "$(5"%(B")
    ("\\($(5!H!h!O(B\\)" . "$(5"&(B")
    ("\\($(5!I!h!O(B\\)" . "$(5"'(B")
    ("\\($(5!U!h!O(B\\)" . "$(5")(B")

    ;; Special Rules
    ;; In the following case, "$(5!<!h!:(B" ligature does not occur.
    ("\\($(5!<!h(B\\)$(5!:!h!<!h(B" . "$(5"<(B")

    ;; Ligature Rules 
    ("\\($(5!3!h!B!h!O!h!M(B\\)" . "$(5$!(B")
    ("\\($(5!3!h!B!h!T(B\\)" . "$(5$"(B")
    ("\\($(5!3!h!B!h!M(B\\)" . "$(5$#(B")
    ("\\($(5!3!h!F!h!M(B\\)" . "$(5$$(B")
    ("\\($(5!3!h!O!h!M(B\\)" . "$(5$%(B")
    ("\\($(5!3!h!T!h!M(B\\)" . "$(5$&(B")
    ("\\($(5!3!h!3(B\\)" . "$(5$'(B")
    ("\\($(5!3!h!B(B\\)" . "$(5$((B")
    ("\\($(5!3!h!F(B\\)" . "$(5$)(B")
    ("\\($(5!3!h!L(B\\)" . "$(5$*(B")
    ("\\($(5!3!h!M(B\\)" . "$(5$+(B")
    ("\\($(5!3!h!Q(B\\)" . "$(5$,(B")
    ("\\($(5!3!h!T(B\\)" . "$(5$-(B")
    ("\\($(5!3!h!V(B\\)" . "$(5$.(B")
    ("\\($(5!6!h!F(B\\)" . "$(5$/(B")
    ("\\($(5!7!h!3!h!B!h!M(B\\)" . "$(5$0(B")
    ("\\($(5!7!h!3!h!V!h!T(B\\)" . "$(5$1(B")
    ("\\($(5!7!h!3!h!B(B\\)" . "$(5$2(B")
    ("\\($(5!7!h!3!h!V(B\\)" . "$(5$3(B")
    ("\\($(5!7!h!6!h!O(B\\)" . "$(5$4(B")
    ("\\($(5!7!h!3!h!M(B\\)" . "$(5$5(B")
    ("\\($(5!7!h!4!h!M(B\\)" . "$(5$6(B")
    ("\\($(5!7!h!5!h!M(B\\)" . "$(5$7(B")
    ("\\($(5!7!h!6!h!M(B\\)" . "$(5$8(B")
    ("\\($(5!7!h!3(B\\)" . "$(5$9(B")
    ("\\($(5!7!h!4(B\\)" . "$(5$:(B")
    ("\\($(5!7!h!5(B\\)" . "$(5$;(B")
    ("\\($(5!7!h!6(B\\)" . "$(5$<(B")
    ("\\($(5!7!h!7(B\\)" . "$(5$=(B")
    ("\\($(5!7!h!F(B\\)" . "$(5$>(B")
    ("\\($(5!7!h!L(B\\)" . "$(5$?(B")
    ("\\($(5!7!h!M(B\\)" . "$(5$@(B")
    ("\\($(5!8!h!8(B\\)" . "$(5$A(B")
    ("\\($(5!8!h!<(B\\)" . "$(5$B(B")
    ("\\($(5!9!h!M(B\\)" . "$(5$C(B")
    ("\\($(5!:!h!O(B\\)" . "$(5$D(B")
    ("\\($(5!:!h!<(B\\)" . "$(5$E(B")
    ("\\($(5!<!h!8(B\\)" . "$(5$F(B")
    ("\\($(5!<!h!:(B\\)" . "$(5$G(B")
    ("\\($(5!=!h!3(B\\)" . "$(5$H(B")
    ("\\($(5!=!h!=(B\\)" . "$(5$I(B")
    ("\\($(5!=!h!>(B\\)" . "$(5$J(B")
    ("\\($(5!=!h!M(B\\)" . "$(5$K(B")
    ("\\($(5!>!h!M(B\\)" . "$(5$L(B")
    ("\\($(5!?!h!5!h!M(B\\)" . "$(5$M(B")
    ("\\($(5!?!h!6!h!O(B\\)" . "$(5$N(B")
    ("\\($(5!?!h!O!h!M(B\\)" . "$(5$O(B")
    ("\\($(5!?!h!5(B\\)" . "$(5$P(B")
    ("\\($(5!?!h!6(B\\)" . "$(5$Q(B")
    ("\\($(5!?!h!?(B\\)" . "$(5$R(B")
    ("\\($(5!?!h!L(B\\)" . "$(5$S(B")
    ("\\($(5!?!h!M(B\\)" . "$(5$T(B")
    ("\\($(5!@!h!M(B\\)" . "$(5$`(B")
    ("\\($(5!B!h!B(B\\)" . "$(5$a(B")
    ("\\($(5!B!h!F(B\\)" . "$(5$b(B")
    ("\\($(5!D!h!D!h!M(B\\)" . "$(5$c(B")
    ("\\($(5!D!h!E!h!M(B\\)" . "$(5$d(B")
    ("\\($(5!D!h!K!h!M(B\\)" . "$(5$e(B")
    ("\\($(5!D!h!O!h!M(B\\)" . "$(5$f(B")
    ("\\($(5!D!h!T!h!M(B\\)" . "$(5$g(B")
    ("\\($(5!D!h!5!h!O(B\\)" . "$(5$h(B")
    ("\\($(5!D!h!6!h!O(B\\)" . "$(5$i(B")
    ("\\($(5!D!h!D!h!T(B\\)" . "$(5$j(B")
    ("\\($(5!D!h!E!h!T(B\\)" . "$(5$k(B")
    ("\\($(5!D!h!5(B\\)" . "$(5$l(B")
    ("\\($(5!D!h!6(B\\)" . "$(5$m(B")
    ("\\($(5!D!h!D(B\\)" . "$(5$n(B")
    ("\\($(5!D!h!E(B\\)" . "$(5$o(B")
    ("\\($(5!D!h!F(B\\)" . "$(5$p(B")
    ("\\($(5!D!h!J(B\\)" . "$(5$q(B")
    ("\\($(5!D!h!K(B\\)" . "$(5$r(B")
    ("\\($(5!D!h!L(B\\)" . "$(5$s(B")
    ("\\($(5!D!h!M(B\\)" . "$(5$t(B")
    ("\\($(5!D!h!T(B\\)" . "$(5$u(B")
    ("\\($(5!E!h!F(B\\)" . "$(5$v(B")
    ("\\($(5!F!h!F(B\\)" . "$(5$w(B")
    ("\\($(5!H!h!B(B\\)" . "$(5$x(B")
    ("\\($(5!H!h!F(B\\)" . "$(5$y(B")
    ("\\($(5!H!h!Q(B\\)" . "$(5$z(B")
    ("\\($(5!J!h!F(B\\)" . "$(5${(B")
    ("\\($(5!J!h!J(B\\)" . "$(5$|(B")
    ("\\($(5!J!h!T(B\\)" . "$(5$}(B")
    ("\\($(5!K!h!F(B\\)" . "$(5$~(B")
    ("\\($(5!L!h!F(B\\)" . "$(5#P(B")
    ("\\($(5!L!h!Q(B\\)" . "$(5#Q(B")
    ("\\($(5!Q!h!Q(B\\)" . "$(5#`(B")
    ("\\($(5!T!h!F(B\\)" . "$(5#a(B")
    ("\\($(5!T!h!T(B\\)" . "$(5#b(B")
    ("\\($(5!U!h!8(B\\)" . "$(5#c(B")
    ("\\($(5!U!h!F(B\\)" . "$(5#d(B")
    ("\\($(5!U!h!J(B\\)" . "$(5#e(B")
    ("\\($(5!U!h!Q(B\\)" . "$(5#f(B")
    ("\\($(5!U!h!T(B\\)" . "$(5#g(B")
    ("\\($(5!V!h!=!h!O!h!M(B\\)" . "$(5#h(B")
    ("\\($(5!V!h!=!h!M(B\\)" . "$(5#i(B")
    ("\\($(5!V!h!=!h!T(B\\)" . "$(5#j(B")
    ("\\($(5!V!h!=(B\\)" . "$(5#k(B")
    ("\\($(5!V!h!>(B\\)" . "$(5#l(B")
    ("\\($(5!W!h!F(B\\)" . "$(5#m(B")
    ("\\($(5!W!h!O(B\\)" . "$(5#n(B")
    ("\\($(5!X!h!A(B\\)" . "$(5#p(B")
    ("\\($(5!X!h!F(B\\)" . "$(5#q(B")
    ("\\($(5!X!h!L(B\\)" . "$(5#r(B")
    ("\\($(5!X!h!M(B\\)" . "$(5#s(B")
    ("\\($(5!X!h!O(B\\)" . "$(5#t(B")
    ("\\($(5!X!h!Q(B\\)" . "$(5#u(B")
    ("\\($(5!X!h!T(B\\)" . "$(5#v(B")
    ;; Special Ligature Rules 
    ("\\($(5!X!_(B\\)" . "$(5#R(B")

    ;; Half form with ligature.  Special "r" case is included.  "r"
    ;; connection which is not listed here has not been examined yet.
    ;; I don't know what to do with them.
    ;;
    ;; ordinary forms 
    ("\\($(5!5!h!O!h(B\\)[$(5!3(B-$(5!X(B]" . "$(5"`(B")
    ("\\($(5!6!h!F!h(B\\)[$(5!3(B-$(5!X(B]" . "$(5"a(B")
    ;; ("\\($(5!<!h!8!h(B\\)[$(5!3(B-$(5!X(B]" . "$(5"c(B") ; Mistake, must check later.
    ("\\($(5!B!h!O!h(B\\)[$(5!3(B-$(5!X(B]" . "$(5"d(B")
    ("\\($(5!E!h!F!h(B\\)[$(5!3(B-$(5!X(B]" . "$(5"e(B")
    ("\\($(5!E!h!O!h(B\\)[$(5!3(B-$(5!X(B]" . "$(5"f(B")
    ("\\($(5!H!h!B!h(B\\)[$(5!3(B-$(5!X(B]" . "$(5"g(B")
    ("\\($(5!U!h!8!h(B\\)[$(5!3(B-$(5!X(B]" . "$(5"h(B")
    ("\\($(5!U!h!O!h(B\\)[$(5!3(B-$(5!X(B]" . "$(5"i(B")
    ("\\($(5!U!h!T!h(B\\)[$(5!3(B-$(5!X(B]" . "$(5"j(B")
    ;; ("\\($(5!U!h!T!h(B\\)[$(5!3(B-$(5!X(B]" . "$(5"k(B") ; must check later.
    ;; Conjunction form associated with Nukta sign.
    ("\\($(5!3!i!h(B\\)[$(5!3(B-$(5!X(B]" . "$(5"s(B")
    ("\\($(5!4!i!h(B\\)[$(5!3(B-$(5!X(B]" . "$(5"t(B")
    ("\\($(5!5!i!h(B\\)[$(5!3(B-$(5!X(B]" . "$(5"u(B")
    ("\\($(5!:!i!h(B\\)[$(5!3(B-$(5!X(B]" . "$(5"z(B")
    ("\\($(5!I!i!h(B\\)[$(5!3(B-$(5!X(B]" . "$(5"y(B")

    ;; For consonants other than listed above, glyph-composition will
    ;; be applied.  If the consonant which is preceding "$(5!O(B" does not
    ;; have the vertical line (such as "$(5!?(B"), "$(5"r(B" is put beneath the
    ;; consonant.
    ;;
    ("[$(5!7!9!=!>!?!@!D!O!P!R!S!X(B]\\($(5!h!O(B\\)" . "$(5"r(B")
    ("\\($(5!J!h!O(B\\)" . "$(5!J"r(B") ; Protect from Half form conversion.
    ("\\($(5!E!h!O(B\\)" . "$(5!E"r(B") ; Will be replaced with precomposed font.
    ("\\($(5!6!h!O(B\\)" . "$(5!6"r(B")
    ("\\($(5!K!h!O(B\\)" . "$(5!K"r(B")
    ("\\($(5!T!h!O(B\\)" . "$(5!T"r(B")
    ("\\($(5!L!h!O(B\\)" . "$(5!L"r(B")
    ("\\($(5!7!h!5!h!O(B\\)" . "$(5$;"r(B") ; Ggr
    ("\\($(5!7!h!3!h!O(B\\)" . "$(5$9"r(B") ; Gkr

    ("$(5!?!i(B\\($(5!h!O(B\\)" . "$(5"r(B")
    ("$(5!@!i(B\\($(5!h!O(B\\)" . "$(5"r(B")

    ;; Nukta 
    ("\\($(5!!!i(B\\)" . "$(5#!(B")
    ("\\($(5!&!i(B\\)" . "$(5#&(B")
    ("\\($(5!'!i(B\\)" . "$(5#'(B")
    ("\\($(5!*!i(B\\)" . "$(5#*(B")
    ("\\($(5![!i(B\\)" . "$(5#L(B")
    ("\\($(5!\!i(B\\)" . "$(5#M(B")
    ("\\($(5!_!i(B\\)" . "$(5#K(B")
    ("\\($(5!3!i(B\\)" . "$(5#3(B")
    ("\\($(5!4!i(B\\)" . "$(5#4(B")
    ("\\($(5!5!i(B\\)" . "$(5#5(B")
    ("\\($(5!:!i(B\\)" . "$(5#:(B")
    ("\\($(5!?!i(B\\)" . "$(5#?(B")
    ("\\($(5!@!i(B\\)" . "$(5#@(B")
    ("\\($(5!I!i(B\\)" . "$(5#I(B")
    ("\\($(5!j!i(B\\)" . "$(5#J(B")

    ;; Half forms.
    ("\\($(5!3!h(B\\)[$(5!3(B-$(5!X(B]" . "$(5"3(B")
    ("\\($(5!4!h(B\\)[$(5!3(B-$(5!X(B]" . "$(5"4(B")
    ("\\($(5!5!h(B\\)[$(5!3(B-$(5!X(B]" . "$(5"5(B")
    ("\\($(5!6!h(B\\)[$(5!3(B-$(5!X(B]" . "$(5"6(B")
    ("\\($(5!8!h(B\\)[$(5!3(B-$(5!X(B]" . "$(5"8(B")
    ("\\($(5!:!h(B\\)[$(5!3(B-$(5!X(B]" . "$(5":(B")
    ("\\($(5!;!h(B\\)[$(5!3(B-$(5!X(B]" . "$(5";(B")
    ("\\($(5!<!h(B\\)[$(5!3(B-$(5!X(B]" . "$(5"<(B")
    ("\\($(5!A!h(B\\)[$(5!3(B-$(5!X(B]" . "$(5"A(B")
    ("\\($(5!B!h(B\\)[$(5!3(B-$(5!X(B]" . "$(5"B(B")
    ("\\($(5!C!h(B\\)[$(5!3(B-$(5!X(B]" . "$(5"C(B")
    ("\\($(5!E!h(B\\)[$(5!3(B-$(5!X(B]" . "$(5"E(B")
    ("\\($(5!F!h(B\\)[$(5!3(B-$(5!X(B]" . "$(5"F(B")
    ("\\($(5!G!h(B\\)[$(5!3(B-$(5!X(B]" . "$(5"G(B")
    ("\\($(5!H!h(B\\)[$(5!3(B-$(5!X(B]" . "$(5"H(B")
    ("\\($(5!I!h(B\\)[$(5!3(B-$(5!X(B]" . "$(5"I(B")
    ("\\($(5!J!h(B\\)[$(5!3(B-$(5!X(B]" . "$(5"J(B")
    ("\\($(5!K!h(B\\)[$(5!3(B-$(5!X(B]" . "$(5"K(B")
    ("\\($(5!L!h(B\\)[$(5!3(B-$(5!X(B]" . "$(5"L(B")
    ("\\($(5!M!h(B\\)[$(5!3(B-$(5!X(B]" . "$(5"M(B")
    ("\\($(5!N!h(B\\)[$(5!3(B-$(5!X(B]" . "$(5"N(B")
    ("\\($(5!Q!h(B\\)[$(5!3(B-$(5!X(B]" . "$(5"Q(B")
    ("\\($(5!R!h(B\\)[$(5!3(B-$(5!X(B]" . "$(5"R(B")
    ("\\($(5!S!h(B\\)[$(5!3(B-$(5!X(B]" . "$(5"S(B")
    ("\\($(5!T!h(B\\)[$(5!3(B-$(5!X(B]" . "$(5"T(B")
    ("\\($(5!U!h(B\\)[$(5!3(B-$(5!X(B]" . "$(5"U(B")
    ("\\($(5!V!h(B\\)[$(5!3(B-$(5!X(B]" . "$(5"V(B")
    ("\\($(5!W!h(B\\)[$(5!3(B-$(5!X(B]" . "$(5"W(B")

    ;; Special rule for "rR"
    ("\\($(5!O!_(B\\)" . "$(5!*"p(B")
    ;; If everything fails, "y" will connect to the front consonant.
    ("\\($(5!h!M(B\\)" . "$(5"](B")
    )
  "Alist of regexps of Devanagari character sequences vs composed characters.")

;; Example:
;;("\\($(5!F!h(B\\)[$(5!3(B-$(5!X(B]" . "$(5"F(B")
;;(string-match "\\($(5!F!h(B\\)[$(5!3(B-$(5!X(B]" "$(5!X![!F!h!D!\(B") => 8
;;(match-end 1) => 16

;;
;; Defining character properties : char-to-glyph, glyph-to-char
;;
;; *  If char-to-glyph is non-nil, it would be one of the following forms.
;;
;;  (("character-regexp" . "glyphs")
;;   .....) or
;;  (("character-regexp" . ?glyph)
;;   .....) or
;;  ("characters-regexp" . "glyphs")
;;  or
;;  ?glyph
;;
;; *  If glyph-to-char is non-nil, it would be one of the following forms.
;;
;;  (("glyph-regexp" . "characters")  ;; This is the only case in Devanagari
;;   ....) or
;;  (("glyph-regexp" . ?character)
;;   ....) or
;;  ("glyph-regexp" . "characters")
;;    or
;;  "characters"
;;    or
;;  ?character  
;;

(let ((rules devanagari-char-to-glyph-rules))
  (while rules
    (let ((rule (car rules)) 
	  (chars) (char) (glyph))
      (setq rules (cdr rules))
      (string-match "\\\\(\\(.+\\)\\\\)" (car rule))
      (setq chars (substring (car rule) (match-beginning 1) (match-end 1)))
      (setq char (string-to-char chars))
      (setq glyph (string-to-char (cdr rule))) ; assume one glyph in devan.
      (put-char-code-property 
         char 'char-to-glyph 
	 (append (get-char-code-property char 'char-to-glyph) (list rule)))
      (put-char-code-property glyph 'glyph-to-char chars))))

;;
;; Convert Character Code to Glyph Code
;;

;;;###autoload
(defun char-to-glyph-devanagari (src-str)
  "Convert Devanagari characters in the string to Devanagari glyphs.  
Ligatures and special rules are processed."
  (let ((pos 0) 
	(dst-str ""))
    (while (< pos (length src-str))
      (let ((found nil)
	    (rules (get-char-code-property 
		    (string-to-char 
		     ;; caution. other forms not supported for now.
		     (substring src-str pos)) 'char-to-glyph)))
	(while rules
	  (let* ((rule (car rules))
		 (regexp (car rule)))
	    (if (string-match regexp src-str)
		(if (= (match-beginning 1) pos)
		    (progn
		      (setq dst-str (concat dst-str (cdr rule)))
		      (setq rules nil) ; Get out of the loop.
		      (setq found t)
		      ;; proceed `pos' for replaced characters.
		      (setq pos (match-end 1)))
		  (setq rules (cdr rules)))
	      (setq rules (cdr rules)))))
	;; proceed to next position
	(if (not found)
	    (let ((nextchar (string-to-char (substring src-str pos))))
	      (setq pos (+ pos 
			   (char-bytes (string-to-char (substring src-str pos)))))
	      (setq dst-str (concat dst-str (char-to-string nextchar)))))))
    dst-str))

;; Example:
;;(char-to-glyph-devanagari "$(5!X![!F!h!D!\(B") => "$(5!X!["F!D!\(B"
;;(char-to-glyph-devanagari "$(5!O!Z!V!h!=!h!O![!M(B") => ???

;;
;; Phase 2: Compose Glyphs to form One Glyph.
;;

;; Each list consist of glyph, application-priority and application-direction.
;;
;; Glyphs will be ordered from low priority number to high priority number.
;; If application-priority is omitted, it is assumed to be 0.
;; If application-direction is omitted, it is asumbed to be '(mr . ml).
;;
;; Priority
;;          Base Glyphs = {$(5!h!i(B} = Misc > 
;;          {$(5"p"q"r(B} > Matras > {$(5!!!"!#(B}
;; Question Halant and '$(5"q"r(B' priority problem.

(defconst devanagari-composition-rules
  '((?$(5!!(B 70 (tr . br))
    (?$(5!"(B 70 (mr . mr))
    (?$(5!#(B 70)
    (?$(5!$(B 0)
    (?$(5!%(B 0)
    (?$(5!&(B 0)
    (?$(5!'(B 0)
    (?$(5!((B 0)
    (?$(5!)(B 0)
    (?$(5!*(B 0)
    (?$(5!+(B 0)
    (?$(5!,(B 0)
    (?$(5!-(B 0)
    (?$(5!.(B 0)
    (?$(5!/(B 0)
    (?$(5!0(B 0)
    (?$(5!1(B 0)
    (?$(5!2(B 0)
    (?$(5!3(B 0)
    (?$(5!4(B 0)
    (?$(5!5(B 0)
    (?$(5!6(B 0)
    (?$(5!7(B 0)
    (?$(5!8(B 0)
    (?$(5!9(B 0)
    (?$(5!:(B 0)
    (?$(5!;(B 0)
    (?$(5!<(B 0)
    (?$(5!=(B 0)
    (?$(5!>(B 0)
    (?$(5!?(B 0)
    (?$(5!@(B 0)
    (?$(5!A(B 0)
    (?$(5!B(B 0)
    (?$(5!C(B 0)
    (?$(5!D(B 0)
    (?$(5!E(B 0)
    (?$(5!F(B 0)
    (?$(5!G(B 0)
    (?$(5!H(B 0)
    (?$(5!I(B 0)
    (?$(5!J(B 0)
    (?$(5!K(B 0)
    (?$(5!L(B 0)
    (?$(5!M(B 0)
    (?$(5!N(B 0)
    (?$(5!O(B 0)
    (?$(5!P(B 0)
    (?$(5!Q(B 0)
    (?$(5!R(B 0)
    (?$(5!S(B 0)
    (?$(5!T(B 0)
    (?$(5!U(B 0)
    (?$(5!V(B 0)
    (?$(5!W(B 0)
    (?$(5!X(B 0)
    (?$(5!Y(B 0)
    (?$(5!Z(B 40)
    (?$(5![(B 40 (ml . mr))
    (?$(5!\(B 40)
    (?$(5!](B 40 (bc . tc))
    (?$(5!^(B 40 (bc . tc))
    (?$(5!_(B 40 (bc . tc))
    (?$(5!`(B 40 (mr . mr))  ; (tc . bc)
    (?$(5!a(B 40 (mr . mr))
    (?$(5!b(B 40 (mr . mr))
    (?$(5!c(B 40 (mr . mr))
    (?$(5!d(B 40)
    (?$(5!e(B 40)
    (?$(5!f(B 40)
    (?$(5!g(B 40)
    (?$(5!h(B 0 (br . tr))
    (?$(5!i(B 0 (br . tr))
    (?$(5!j(B 0)
    (nil 0)
    (nil 0)
    (nil 0)
    (nil 0)
    (nil 0)
    (nil 0)
    (?$(5!q(B 0)
    (?$(5!r(B 0)
    (?$(5!s(B 0)
    (?$(5!t(B 0)
    (?$(5!u(B 0)
    (?$(5!v(B 0)
    (?$(5!w(B 0)
    (?$(5!x(B 0)
    (?$(5!y(B 0)
    (?$(5!z(B 0)
    (nil 0)
    (nil 0)
    (nil 0)
    (nil 0)
    (?$(5"!(B 0)
    (?$(5""(B 0)
    (?$(5"#(B 0)
    (?$(5"$(B 0)
    (?$(5"%(B 0)
    (?$(5"&(B 0)
    (?$(5"'(B 0)
    (?$(5"((B 0)
    (?$(5")(B 0)
    (?$(5"*(B 0)
    (?$(5"+(B 0)
    (?$(5",(B 0)
    (?$(5"-(B 0)
    (?$(5".(B 0)
    (?$(5"/(B 0)
    (?$(5"0(B 0)
    (?$(5"1(B 0)
    (?$(5"2(B 0)
    (?$(5"3(B 0)
    (?$(5"4(B 0)
    (?$(5"5(B 0)
    (?$(5"6(B 0)
    (?$(5"7(B 0)
    (?$(5"8(B 0)
    (?$(5"9(B 0)
    (?$(5":(B 0)
    (?$(5";(B 0)
    (?$(5"<(B 0)
    (?$(5"=(B 0)
    (?$(5">(B 0)
    (?$(5"?(B 0)
    (?$(5"@(B 0)
    (?$(5"A(B 0)
    (?$(5"B(B 0)
    (?$(5"C(B 0)
    (?$(5"D(B 0)
    (?$(5"E(B 0)
    (?$(5"F(B 0)
    (?$(5"G(B 0)
    (?$(5"H(B 0)
    (?$(5"I(B 0)
    (?$(5"J(B 0)
    (?$(5"K(B 0)
    (?$(5"L(B 0)
    (?$(5"M(B 0)
    (?$(5"N(B 0)
    (?$(5"O(B 0)
    (?$(5"P(B 0)
    (?$(5"Q(B 0)
    (?$(5"R(B 0)
    (?$(5"S(B 0)
    (?$(5"T(B 0)
    (?$(5"U(B 0)
    (?$(5"V(B 0)
    (?$(5"W(B 0)
    (?$(5"X(B 0)
    (?$(5"Y(B 0)
    (?$(5"Z(B 0)
    (?$(5"[(B 0)
    (?$(5"\(B 0)
    (?$(5"](B 0)
    (?$(5"^(B 0)
    (?$(5"_(B 0)
    (?$(5"`(B 0)
    (?$(5"a(B 0)
    (?$(5"b(B 0)
    (?$(5"c(B 0)
    (?$(5"d(B 0)
    (?$(5"e(B 0)
    (?$(5"f(B 0)
    (?$(5"g(B 0)
    (?$(5"h(B 0)
    (?$(5"i(B 0)
    (?$(5"j(B 0)
    (?$(5"k(B 0)
    (?$(5"l(B 0)
    (?$(5"m(B 0)
    (?$(5"n(B 0)
    (?$(5"o(B 0)
    (?$(5"p(B 30 (mr . mr))
    (?$(5"q(B 30 (br . tr))
    (?$(5"r(B 30 (br . tr))
    (?$(5"s(B 0)
    (?$(5"t(B 0)
    (?$(5"u(B 0)
    (?$(5"v(B 0)
    (?$(5"w(B 0)
    (?$(5"x(B 0)
    (?$(5"y(B 0)
    (?$(5"z(B 0)
    (?$(5"{(B 0)
    (?$(5"|(B 0)
    (?$(5"}(B 0)
    (?$(5"~(B 0)
    (?$(5#!(B 0)
    (?$(5#"(B 0)
    (?$(5##(B 0)
    (?$(5#$(B 0)
    (?$(5#%(B 0)
    (?$(5#&(B 0)
    (?$(5#'(B 0)
    (?$(5#((B 0)
    (?$(5#)(B 0)
    (?$(5#*(B 0)
    (?$(5#+(B 0)
    (?$(5#,(B 0)
    (?$(5#-(B 0)
    (?$(5#.(B 0)
    (?$(5#/(B 0)
    (?$(5#0(B 0)
    (?$(5#1(B 0)
    (?$(5#2(B 0)
    (?$(5#3(B 0)
    (?$(5#4(B 0)
    (?$(5#5(B 0)
    (?$(5#6(B 0)
    (?$(5#7(B 0)
    (?$(5#8(B 0)
    (?$(5#9(B 0)
    (?$(5#:(B 0)
    (?$(5#;(B 0)
    (?$(5#<(B 0)
    (?$(5#=(B 0)
    (?$(5#>(B 0)
    (?$(5#?(B 0)
    (?$(5#@(B 0)
    (?$(5#A(B 0)
    (?$(5#B(B 0)
    (?$(5#C(B 0)
    (?$(5#D(B 0)
    (?$(5#E(B 0)
    (?$(5#F(B 0)
    (?$(5#G(B 0)
    (?$(5#H(B 0)
    (?$(5#I(B 0)
    (?$(5#J(B 0)
    (?$(5#K(B 40 (bc . tc))
    (?$(5#L(B 40 (bc . tc))
    (?$(5#M(B 40 (bc . tc))
    (?$(5#N(B 0)
    (?$(5#O(B 0)
    (?$(5#P(B 0)
    (?$(5#Q(B 0)
    (?$(5#R(B 0)
    (?$(5#S(B 0)
    (?$(5#T(B 0)
    (?$(5#U(B 0)
    (?$(5#V(B 0)
    (?$(5#W(B 0)
    (?$(5#X(B 0)
    (?$(5#Y(B 0)
    (?$(5#Z(B 0)
    (?$(5#[(B 0)
    (?$(5#\(B 0)
    (?$(5#](B 0)
    (?$(5#^(B 0)
    (?$(5#_(B 0)
    (?$(5#`(B 0)
    (?$(5#a(B 0)
    (?$(5#b(B 0)
    (?$(5#c(B 0)
    (?$(5#d(B 0)
    (?$(5#e(B 0)
    (?$(5#f(B 0)
    (?$(5#g(B 0)
    (?$(5#h(B 0)
    (?$(5#i(B 0)
    (?$(5#j(B 0)
    (?$(5#k(B 0)
    (?$(5#l(B 0)
    (?$(5#m(B 0)
    (?$(5#n(B 0)
    (?$(5#o(B 0)
    (?$(5#p(B 0)
    (?$(5#q(B 0)
    (?$(5#r(B 0)
    (?$(5#s(B 0)
    (?$(5#t(B 0)
    (?$(5#u(B 0)
    (?$(5#v(B 0)
    (?$(5#w(B 0)
    (?$(5#x(B 0)
    (?$(5#y(B 0)
    (?$(5#z(B 0)
    (?$(5#{(B 0)
    (?$(5#|(B 0)
    (?$(5#}(B 0)
    (?$(5#~(B 0)
    (?$(5$!(B 0)
    (?$(5$"(B 0)
    (?$(5$#(B 0)
    (?$(5$$(B 0)
    (?$(5$%(B 0)
    (?$(5$&(B 0)
    (?$(5$'(B 0)
    (?$(5$((B 0)
    (?$(5$)(B 0)
    (?$(5$*(B 0)
    (?$(5$+(B 0)
    (?$(5$,(B 0)
    (?$(5$-(B 0)
    (?$(5$.(B 0)
    (?$(5$/(B 0)
    (?$(5$0(B 0)
    (?$(5$1(B 0)
    (?$(5$2(B 0)
    (?$(5$3(B 0)
    (?$(5$4(B 0)
    (?$(5$5(B 0)
    (?$(5$6(B 0)
    (?$(5$7(B 0)
    (?$(5$8(B 0)
    (?$(5$9(B 0)
    (?$(5$:(B 0)
    (?$(5$;(B 0)
    (?$(5$<(B 0)
    (?$(5$=(B 0)
    (?$(5$>(B 0)
    (?$(5$?(B 0)
    (?$(5$@(B 0)
    (?$(5$A(B 0)
    (?$(5$B(B 0)
    (?$(5$C(B 0)
    (?$(5$D(B 0)
    (?$(5$E(B 0)
    (?$(5$F(B 0)
    (?$(5$G(B 0)
    (?$(5$H(B 0)
    (?$(5$I(B 0)
    (?$(5$J(B 0)
    (?$(5$K(B 0)
    (?$(5$L(B 0)
    (?$(5$M(B 0)
    (?$(5$N(B 0)
    (?$(5$O(B 0)
    (?$(5$P(B 0)
    (?$(5$Q(B 0)
    (?$(5$R(B 0)
    (?$(5$S(B 0)
    (?$(5$T(B 0)
    (?$(5$U(B 0)
    (?$(5$V(B 0)
    (?$(5$W(B 0)
    (?$(5$X(B 0)
    (?$(5$Y(B 0)
    (?$(5$Z(B 0)
    (?$(5$[(B 0)
    (?$(5$\(B 0)
    (?$(5$](B 0)
    (?$(5$^(B 0)
    (?$(5$_(B 0)
    (?$(5$`(B 0)
    (?$(5$a(B 0)
    (?$(5$b(B 0)
    (?$(5$c(B 0)
    (?$(5$d(B 0)
    (?$(5$e(B 0)
    (?$(5$f(B 0)
    (?$(5$g(B 0)
    (?$(5$h(B 0)
    (?$(5$i(B 0)
    (?$(5$j(B 0)
    (?$(5$k(B 0)
    (?$(5$l(B 0)
    (?$(5$m(B 0)
    (?$(5$n(B 0)
    (?$(5$o(B 0)
    (?$(5$p(B 0)
    (?$(5$q(B 0)
    (?$(5$r(B 0)
    (?$(5$s(B 0)
    (?$(5$t(B 0)
    (?$(5$u(B 0)
    (?$(5$v(B 0)
    (?$(5$w(B 0)
    (?$(5$x(B 0)
    (?$(5$y(B 0)
    (?$(5$z(B 0)
    (?$(5${(B 0)
    (?$(5$|(B 0)
    (?$(5$}(B 0)
    (?$(5$~(B 0)
    ))

;; Determine composition priority and rule of the array of Glyphs.
;; Sort the glyphs with their priority.

(defun devanagari-reorder-glyph-for-composition (glyph-alist)
  (let* ((pos 0)
	 (ordered-glyphs '()))
    (while (< pos (length glyph-alist))
      (let* ((glyph (aref glyph-alist pos)))
	(setq pos (1+ pos))
	(setq ordered-glyphs 
	      (append ordered-glyphs (list (assq glyph devanagari-composition-rules))))))
    (sort ordered-glyphs '(lambda (x y) (< (car (cdr x)) (car (cdr y)))))))

;;(devanagari-compose-to-one-glyph "$(5"5!X![(B") => "2$(6!XP"5@![1(B"

(defun devanagari-compose-to-one-glyph (devanagari-string)
  (let* ((o-glyph-list (devanagari-reorder-glyph-for-composition
			(string-to-vector devanagari-string)))
	 ;; List of glyphs to be composed.
	 (cmp-glyph-list (list (car (car o-glyph-list)))) 
	 (o-glyph-list (cdr o-glyph-list)))
    (while o-glyph-list
      (let* ((o-glyph (car o-glyph-list))
	     (glyph (if (< 2 (length o-glyph))
			;; default composition
			(list (car (cdr (cdr o-glyph))) (car o-glyph))
		      ;; composition with a specified rule
		      (list '(mr . ml) (car o-glyph)))))
	(setq o-glyph-list (cdr o-glyph-list))
	(setq cmp-glyph-list (append cmp-glyph-list glyph))))
    ;; Before applying compose-chars, convert glyphs to
    ;; 1-column width if possible.
    (setq cmp-glyph-list (devanagari-wide-to-narrow cmp-glyph-list))
    (if (= (length cmp-glyph-list) 1) (char-to-string (car cmp-glyph-list))
      (apply 'compose-chars cmp-glyph-list))))

;; Utility function for Phase 2.5
;; Check whether given glyph is a Devanagari vertical modifier or not.
;; If it is a vertical modifier, whether it should be 1-column shape or not
;; depends on previous non-vertical modifier.
   ; return nil if it is not vertical modifier.
(defun devanagari-vertical-modifier-p (glyph)
  (string-match (char-to-string glyph)
		"[$(5!]!^!_!`!a!b!c!h!i"p"q"r#K#L#M(B]"))

(defun devanagari-non-vertical-modifier-p (glyph)
  (string-match (char-to-string glyph)
		"[$(5!Z![!\!d!e!f!g(B]"))


;;
;;    Phase 2.5  Convert Appropriate Character to 1-column shape.
;;
;; This is temporary and should be removed out when Emacs supports 
;; variable width characters.
;;
;; This will convert the composing glyphs (2 column glyphs) 
;; to narrow (1 column) glyphs if they exist.
;;
;; devanagari-wide-to-narrow-old converts glyphs simply.
;; devanagari-wide-to-narrow takes care of upper/lower apply-glyphs 
;;   with 2 column base-glyph.
;;
;; Execution Examples
;;(devanagari-wide-to-narrow '(?$(5!3(B (ml . ml) ?$(5!a(B))
;;(devanagari-wide-to-narrow '(?$(5!F(B (ml . ml) ?$(5!a(B))

;(defun devanagari-wide-to-narrow (src-list)
;  (if (null src-list) '()
;    (cons 
;     (if (and (numberp (car src-list))
;	      (cdr (assq (car src-list) devanagari-1-column-char)))
;	 (cdr (assq (car src-list) devanagari-1-column-char))
;       (car src-list))
;     (devanagari-wide-to-narrow (cdr src-list)))))

(defun devanagari-wide-to-narrow (src-list)
  (devanagari-wide-to-narrow-iter src-list t))

(defun devanagari-wide-to-narrow-iter (src-list wide-p)
  (let ((glyph (car src-list)))
    (cond ((null src-list) '())
	  ; not glyph code
	  ((not (numberp glyph)) 
	   (cons glyph (devanagari-wide-to-narrow-iter (cdr src-list) wide-p)))
	  ; vertical modifier glyph
	  ((devanagari-vertical-modifier-p glyph)
	   (if (and (null wide-p)
		    (cdr (assq glyph devanagari-1-column-char)))
	       (cons (cdr (assq glyph devanagari-1-column-char))
		     (devanagari-wide-to-narrow-iter (cdr src-list) nil))
	       (cons glyph
		     (devanagari-wide-to-narrow-iter (cdr src-list) t))))
	  ; nonvertical modifier glyph
	  ((devanagari-non-vertical-modifier-p glyph)
	   (if (cdr (assq glyph devanagari-1-column-char))
	       (cons (cdr (assq glyph devanagari-1-column-char))
		     (devanagari-wide-to-narrow-iter (cdr src-list) wide-p))
	       (cons glyph
		     (devanagari-wide-to-narrow-iter (cdr src-list) wide-p))))
	  ; normal glyph
	  (t
	   (if (cdr (assq glyph devanagari-1-column-char))
	       (cons (cdr (assq glyph devanagari-1-column-char))
		     (devanagari-wide-to-narrow-iter (cdr src-list) nil))
	       (cons glyph
		     (devanagari-wide-to-narrow-iter (cdr src-list) t)))))))


;;
;; Summary
;; 

;;;###autoload
(defun devanagari-compose-string (str)
  (let ((len (length str))
	(src str) (dst "") rest match-b match-e)
    (while (string-match devanagari-composite-glyph-unit-examine src)
      (setq match-b (match-beginning 0) match-e (match-end 0))
      (setq dst 
	    (concat dst 
		    (substring src 0 match-b)
		    (devanagari-compose-to-one-glyph 
		     (char-to-glyph-devanagari
		      (substring src match-b match-e)))))
      (setq src (substring src match-e)))
    (setq dst (concat dst src))
    dst))

;;;###autoload
(defun devanagari-compose-region (from to)
  (interactive "r")
  (save-restriction
    (narrow-to-region from to)
    (goto-char (point-min))
    (while (re-search-forward devanagari-composite-glyph-unit-examine nil t)
      (let* ((match-b (match-beginning 0)) (match-e (match-end 0))
	     (cmps (devanagari-compose-to-one-glyph
		    (char-to-glyph-devanagari
		     (buffer-substring match-b match-e)))))
	(delete-region match-b match-e)
	(insert cmps)))))

;;
;; Decomposition of composite font.
;;

(defun devanagari-normalize-narrow-glyph (charlist)
  (let ((wide-char (car (rassoc (car charlist) devanagari-1-column-char))))
    (if (null charlist) nil
      (cons (if (null wide-char) (car charlist) wide-char)
	    (devanagari-normalize-narrow-glyph (cdr charlist))))))

(defvar devanagari-decomposition-rules
  '(
    (?$(5"p(B -20)
    )
  )

(defun devanagari-reorder-glyph-for-decomposition (glyphlist)
  "This function re-orders glyph list."
  (sort glyphlist 
	'(lambda (x y) 
	   (let ((xx (assoc x devanagari-decomposition-rules))
		 (yy (assoc y devanagari-decomposition-rules)))
	     (if (null xx) (setq xx 0))
	     (if (null yy) (setq yy 0))
	     (< xx yy)))))

(defun devanagari-decompose-char (char)
  "This function decomposes one Devanagari composite character to 
   basic Devanagari character."
  (let ((glyphlist (decompose-composite-char char)))
    (if (not (listp glyphlist)) 
	(setq glyphlist (list glyphlist)))
    (setq glyphlist (devanagari-normalize-narrow-glyph glyphlist))
    (mapconcat '(lambda (x) (let ((char (get-char-code-property 
					 x 'glyph-to-char)))
			   (if (null char) (char-to-string x) char)))
	       (devanagari-reorder-glyph-for-decomposition glyphlist)
	       "")))

;;;###autoload
(defun devanagari-decompose-string (str)
  "This function Decomposes Devanagari glyph string to 
basic Devanagari character string."
  (let ((src str) (dst ""))
    (while (not (equal src ""))
      (let* ((char (string-to-char src))
	     (clen (char-bytes char)))
	(setq src (substring src clen))
	(setq dst (concat dst
			  (devanagari-decompose-char char)))))
    dst))

;;;###autoload
(defun devanagari-decompose-region (from to)
  (interactive "r")
  (save-restriction
    (narrow-to-region from to)
    (goto-char (point-min))
    (while (re-search-forward "." nil t)
      (let* ((match-b (match-beginning 0)) (match-e (match-end 0))
	     (decmps (devanagari-decompose-string (buffer-substring match-b match-e))))
	(delete-char -1)
	(insert decmps)))))



;; For pre-write and post-read conversion

;;;###autoload
(defun devanagari-compose-from-is13194-region (from to)
  "Compose IS 13194 characters in the region to Devanagari characters."
  (interactive "r")
  (save-restriction
    (narrow-to-region from to)
    (indian-to-devanagari-region (point-min) (point-max))
    (devanagari-compose-region (point-min) (point-max))))

;;;###autoload
(defun devanagari-decompose-to-is13194-region (from to)
  "Decompose Devanagari characters in the region to IS 13194 characters."
  (interactive "r")
  (save-restriction
    (narrow-to-region from to)
    (devanagari-decompose-region (point-min) (point-max))
    (devanagari-to-indian-region (point-min) (point-max))))

;;
(provide 'language/devan-util)

;;; Local Variables:
;;; generated-autoload-file: "../loaddefs.el"
;;; End:
;;; devan-util.el ends here
