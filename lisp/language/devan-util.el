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
;; 1997.3.24 fixed some bugs.

;; Future work ::
;; Decompose the input characters and process them on the character basis.

;; Devanagari script composition rules and related programs.

;;; Code:

;;;
;;;   Steps toward composition of Devanagari Characters.
;;;

;;;###autoload
(defun setup-devanagari-environment ()
  "Setup multilingual environment (MULE) for languages using Devanagari."
  (interactive)
  (set-language-environment "Devanagari"))
	   
;;; Basic functions.

;;;###autoload
(defun indian-to-devanagari (char)
  "Convert IS 13194 character CHAR to Devanagari basic characters.
If CHAR is not IS 13194, return CHAR as is."
  (let ((charcodes (split-char char)))
    (if (eq (car charcodes) 'indian-is13194)
	(make-char 'indian-2-column ?\x21 (nth 1 charcodes))
      char)))

;;;###autoload
(defun devanagari-to-indian (char)
  "Convert Devanagari basic character CHAR to IS 13194 characters.
If CHAR is not Devanagari basic character, return CHAR as is."
  (let ((charcodes (split-char char)))
    (if (and (eq (car charcodes) 'indian-2-column)
	     (= (nth 1 charcodes) ?\x21))
	(make-char 'indian-is13194 (nth 2 charcodes))
      char)))

;;;###autoload
(defun indian-to-devanagari-region (from to)
  "Convert IS 13194 characters in region to Devanagari basic characters.
When called from a program, expects two arguments,
positions (integers or markers) specifying the region."
  (interactive "r")
  (save-excursion
    (goto-char from)
    (while (< (point) to)
      (let ((char (following-char)))
	(if (eq (char-charset char) 'indian-is13194)
	    (progn
	      (delete-char 1)
	      (insert (indian-to-devanagari char)))
	  (forward-char 1))))))

;;;###autoload
(defun devanagari-to-indian-region (from to)
  "Convert Devanagari basic characters in region to Indian characters.
When called from a program, expects two arguments,
positions (integers or markers) specifying the region."
  (interactive "r")
  (save-excursion
    (goto-char from)
    (while (< (point) to)
      (let ((char (following-char)))
	(if (eq (char-charset char) 'indian-2-column)
	    (progn
	      (delete-char -1)
	      (insert (devanagari-to-indian char)))
	  (forward-char 1))))))

;;;###autoload
(defun indian-to-devanagari-string (string)
  "Convert Indian characters in STRING to Devanagari Basic characters."
  (let* ((len (length string))
	 (i 0)
	 (vec (make-vector len 0)))
    (while (< i len)
      (aset vec i (indian-to-devanagari (aref string i)))
      (setq i (1+ i)))
    (concat vec)))

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
;; D - Vowel Modifiers, i.e. Anuswar, Chandrabindu  ($(5!!!"(B) 
;;     (Visaraga ($(5!#(B) is excluded.)
;; M - Matra ($(5!Z![!\!]!^!_!`!a!b!c!d!e!f!g#K#L#M(B)
;;     ("$(5#K#L#M(B" can be obtained by IS13194 matras with nukta.)
;;
;; In Emacs, one syllable of Indian language is considered to be one
;; composite glyph.  If we expand the above expression for
;; cons-vowel-syllable, it would be:
;;
;; [[C [N] H] [C [N] H] [C [N] H] C [N] H] C [N] [M] [D]
;; 
;; Therefore, in worst case, the one syllable may contain
;; following characters.
;;
;; C N H C N H C N H C N H C N M D
;;
;; The example is a sanskrit word "kArtsnya", where five consecutive
;; consonants appear.
;;
;; On the other hand, consonant-syllable, which appears at the end of 
;; the word, would have the following expression:
;;
;; [C [N] H] [C [N] H] [C [N] H] C [N] H
;;
;; This is acceptable BEFORE proper consonant-syllable is input.  The
;; string which doesn't match with the above expression is invalid and
;; thus must be fixed.
;;
;; Note:
;; Third case can be considered, which is an acceptable syllable and can
;; not add any code more.
;;
;; [[C [N] H] [C [N] H] [C [N] H] C [N] H] C [N] [M] D
;;
;; However, to make editing possible even in this condition, we will
;; not consider about this case.
;;
;; Note:
;; Currently, it seems that the only following consonants would have
;; Nukta sign attatched.
;; ($(5!3!4!5!:!?!@!I(B)
;; Therefore, [$(5!3(B-$(5!X(B]$(5!i(B? can be re-written as 
;; \\([$(5!3!4!5!:!?!@!I(B]$(5!i(B\\)\\|[$(5!3(B-$(5!X(B]

(defconst devanagari-full-cons
  "\\(\\([$(5!3!4!5!:!?!@!I(B]$(5!i(B\\)\\|[$(5!3(B-$(5!X$.$E"%(B]\\)"
  "Devanagari full consonant")

(defconst devanagari-pure-cons
  (concat "\\(" devanagari-full-cons "$(5!h(B\\)")
  "Devanagari pure consonant")

(defconst devanagari-matra
  "\\(\\([$(5!_![!\(B]$(5!i(B\\)\\|[$(5!Z(B-$(5!g#K#L#M(B]\\)"
  "Devanagari Matra Signs.  '$(5#K#L#M(B' can also be created from the combination 
of '$(5!_![!\(B' and nukta sign.")

(defconst devanagari-vowel
  "\\(\\([$(5!*!&!'(B]$(5!i(B\\)\\|[$(5!$(B-$(5!2#&#'#*(B]\\)"
  "Devanagari Vowels.  '$(5#&#'#*(B' can also be created from the combination 
of '$(5!*!&!'(B' and nukta sign.")
  
(defconst devanagari-vowel-syllable
  (concat devanagari-vowel "[$(5!!!"(B]?")
  "Devanagari vowel syllable.")

(defconst devanagari-cons-syllable
  (concat devanagari-pure-cons "?" devanagari-pure-cons "?" 
	  devanagari-pure-cons "?" devanagari-pure-cons "$")
  "Devanagari consonant syllable")

(defconst devanagari-cons-vowel-syllable
  (concat "\\(" 
	  devanagari-pure-cons "?" devanagari-pure-cons "?" 
	  devanagari-pure-cons "?" devanagari-pure-cons "\\)?"
	  devanagari-full-cons devanagari-matra "?[$(5!!!"(B]?")
  "Devanagari consonant vowel syllable.")

;;
;; Also, digits and virams should be processed other than syllables.
;;
;; In IS 13194, Avagrah is obtained by Nukta after Viram, and
;; OM is obtained by Nukta after Chandrabindu
;;

(defconst devanagari-digit-viram-visarga
 "[$(5!q(B-$(5!z!j!#(B]")

(defconst devanagari-other-sign
  "\\([$(5!!!j(B]$(5!i(B\\)\\|\\([$(5#!#J(B]\\)")

(defconst devanagari-composite-glyph-unit
  (concat "\\(" devanagari-cons-syllable
	  "\\)\\|\\(" devanagari-vowel-syllable
	  "\\)\\|\\(" devanagari-cons-vowel-syllable
	  "\\)\\|\\(" devanagari-other-sign
	  "\\)\\|\\(" devanagari-digit-viram-visarga "\\)")
  "Regexp matching to Devanagari string to be composed form one glyph.")

;;(put-charset-property charset-devanagari-1-column
;;		      'char-to-glyph 'devanagari-compose-string)
;;(put-charset-property charset-devanagari-2-column
;;		      'char-to-glyph 'devanagari-compose-string)

;; Sample
;;
;;(string-match devanagari-cons-vowel-syllable-examine "$(5!X![(B") => 0
;;(string-match devanagari-cons-vowel-syllable-examine "$(5!F!h!D!\(B") => 0
;;(string-match devanagari-cons-vowel-syllable-examine "$(5!X![!F!h!D!\(B") => 0

;;
;; Steps toward the composition
;;  Converting Character Codes to Composite Glyph.
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
;; => $(6!X(B (ml.mr) $(6![(B / $(6!D(B (ml.mr) $(6"F(B (mr ml) $(6!\(B
;;
;; Compose the glyph.
;;
;; => 4$(6!Xt%![0!X![1(B/4$(6!Dt%"Fv#!\0!D"F!\1(B
;; => 4$(6!Xt%![0!X![14!Dt%"Fv#!\0!D"F!\1(B
;;

;;
;; Phase 1: Converting Character Code to Glyph Code.
;; 
;;
;; IMPORTANT:  
;;        There may be many rules that you many want to suppress.
;;        In that case, please comment out that rule.
;;
;;        RULES WILL BE EVALUATED FROM FIRST TO LAST.
;;        PUT MORE SPECIFIC RULES FIRST.
;;
;; TO DO: 
;;        Prepare multiple specific list of rules for each languages
;;        that adopt Devanagari script.
;;

(defconst devanagari-char-to-glyph-rules
  '(

    ;; `r' at the top of syllable and followed by other consonants.
    ;; ("[^$(5!h(B]\\($(5!O!h(B\\)[$(5!3(B-$(5!X(B]" "$(5"p(B")
    ("^\\($(5!O!h(B\\)[$(5!3(B-$(5!X(B]" "$(5"p(B")

    ;; Ligature Rules 
    ("\\($(5!3!h!B!h!O!h!M(B\\)" "$(5$!(B" sanskrit)
    ("\\($(5!3!h!B!h!T(B\\)" "$(5$"(B" sanskrit)
    ("\\($(5!3!h!B!h!M(B\\)" "$(5$#(B" sanskrit)
    ("\\($(5!3!h!F!h!M(B\\)" "$(5$$(B") 
    ("\\($(5!3!h!O!h!M(B\\)" "$(5$%(B")
    ("\\($(5!3!h!O(B\\)" "$(5"#(B")                  ;                     Post "r"
    ("\\($(5!3!h!T!h!M(B\\)" "$(5$&(B" sanskrit)
    ("\\($(5!3!h(B\\)$(5!3!h(B[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"3(B")    ; Special Half Form
    ("\\($(5!3!h!3(B\\)" "$(5$'(B")
    ("\\($(5!3!h(B\\)$(5!B!h!O(B" "$(5"3(B")              ; Special Rules for "k-tr"
    ("\\($(5!3!h!B(B\\)" "$(5$((B")
    ("\\($(5!3!h!F(B\\)" "$(5$)(B")
    ("\\($(5!3!h!L(B\\)" "$(5$*(B")
    ("\\($(5!3!h!M(B\\)" "$(5$+(B")
    ("\\($(5!3!h!Q(B\\)" "$(5$,(B")
    ("\\($(5!3!h!T(B\\)" "$(5$-(B")
    ("\\($(5!3!h!V!h(B\\)[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"l(B")    ;         Half Form
    ("\\($(5$.!h(B\\)[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"l(B")        ;         Half Form
    ("\\($(5!3!h!V(B\\)" "$(5$.(B")
    ("\\($(5!3!h(B\\)[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"3(B")        ;         Half Form
    ("\\($(5!3!i!h(B\\)[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"s(B")      ; Nukta   Half Form
    ("\\($(5!3!i(B\\)" "$(5#3(B")                    ; Nukta
    ("\\($(5!4!h(B\\)[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"4(B")        ;         Half Form
    ("\\($(5!4!i!h(B\\)[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"t(B")      ; Nukta   Half Form
    ("\\($(5!4!i(B\\)" "$(5#4(B")                    ; Nukta
    ("\\($(5!5!h!O!h(B\\)[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"`(B")    ;         Half Form
    ("\\($(5!5!h!O(B\\)" "$(5"$(B")                  ;                     Post "r"
    ("\\($(5!5!h(B\\)[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"5(B")        ;         Half Form
    ("\\($(5!5!i!h(B\\)[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"u(B")      ; Nukta   Half Form
    ("\\($(5!5!i(B\\)" "$(5#5(B")                    ; Nukta
    ("\\($(5!6!h!F!h(B\\)[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"a(B")    ;         Half Form
    ("\\($(5!6!h!F(B\\)" "$(5$/(B")
    ; Slot
    ("\\($(5!6!h!O(B\\)" "$(5!6"q(B")                ;                     Post "r"
    ("\\($(5!6!h(B\\)[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"6(B")        ;         Half Form
    ("\\($(5!7!h!3!h!B!h!M(B\\)" "$(5$0(B" sanskrit)
    ("\\($(5!7!h!3!h!V!h!T(B\\)" "$(5$1(B" sanskrit)
    ("\\($(5!7!h!3!h!B(B\\)" "$(5$2(B" sanskrit)
    ("\\($(5!7!h!3!h!V(B\\)" "$(5$3(B" sanskrit)
    ("\\($(5!7!h!3!h!O(B\\)" "$(5$9"q(B")            ; Special Rule. May be precomposed font needed.
    ("\\($(5!7!h!6!h!O(B\\)" "$(5$4(B" sanskrit)
    ("\\($(5!7!h!3!h!M(B\\)" "$(5$5(B" sanskrit)
    ("\\($(5!7!h!4!h!M(B\\)" "$(5$6(B" sanskrit)
    ("\\($(5!7!h!5!h!M(B\\)" "$(5$7(B" sanskrit)
    ("\\($(5!7!h!6!h!M(B\\)" "$(5$8(B" sanskrit)
    ("\\($(5!7!h!3(B\\)" "$(5$9(B")
    ("\\($(5!7!h!4(B\\)" "$(5$:(B")
    ("\\($(5!7!h!5!h!O(B\\)" "$(5$;"q(B")            ; Special Rule. May be precomposed font needed.
    ("\\($(5!7!h!5(B\\)" "$(5$;(B")
    ("\\($(5!7!h!6(B\\)" "$(5$<(B")
    ("\\($(5!7!h!7(B\\)" "$(5$=(B")
    ("\\($(5!7!h!F(B\\)" "$(5$>(B")
    ("\\($(5!7!h!L(B\\)" "$(5$?(B")
    ("\\($(5!7!h!M(B\\)" "$(5$@(B")
    ("\\($(5!8!h(B\\)[$(5!8!<(B]$(5!h(B" "$(5"8(B")            ;         Half Form
    ("\\($(5!8!h!8(B\\)" "$(5$A(B")
    ("\\($(5!8!h!<(B\\)" "$(5$B(B")
    ("\\($(5!8!h!O!h(B\\)[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"8"q(B")  ;         Half Form   Post "r"
    ("\\($(5!8!h!O(B\\)" "$(5!8"q(B")                ;                     Post "r"
    ("\\($(5!8!h(B\\)[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"8(B")        ;         Half Form
    ("\\($(5!9!h!M(B\\)" "$(5$C(B")
    ("\\($(5!:!h!O(B\\)" "$(5$D(B")
    ("\\($(5!:!h!<!h(B\\)[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"m(B")    ;         Half Form
    ("\\($(5!:!h!<(B\\)" "$(5$E(B")
    ("\\($(5!:!h(B\\)[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5":(B")        ;         Half Form
    ("\\($(5!:!i!h!O(B\\)" "$(5"!(B")                ; Nukta               Post "r"
    ("\\($(5!:!i!h(B\\)[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"z(B")      ; Nukta   Half Form
    ("\\($(5!:!i(B\\)" "$(5#:(B")                    ; Nukta
    ("\\($(5!;!h(B\\)[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5";(B")        ;         Half Form
    ("\\($(5!<!h(B\\)$(5!8!h(B[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"<(B")    ; Special Half Form
    ("\\($(5!<!h!8(B\\)" "$(5$F(B")
    ("\\($(5!<!h(B\\)$(5!:!h(B[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"<(B")    ; Special Half Form
    ("\\($(5!<!h!:(B\\)" "$(5$G(B")
    ("\\($(5!<!h(B\\)[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"<(B")        ;         Half Form
    ("\\($(5!=!h!3(B\\)" "$(5$H(B")
    ("\\($(5!=!h!=(B\\)" "$(5$I(B")
    ("\\($(5!=!h!>(B\\)" "$(5$J(B")
    ("\\($(5!=!h!M(B\\)" "$(5$K(B")
    ("\\($(5!>!h!M(B\\)" "$(5$L(B")
    ("\\($(5!?!h!5!h!M(B\\)" "$(5$M(B" sanskrit)
    ("\\($(5!?!h!6!h!O(B\\)" "$(5$N(B" sanskrit)
    ("\\($(5!?!h!O!h!M(B\\)" "$(5$O(B")
    ("\\($(5!?!h!5(B\\)" "$(5$P(B")
    ("\\($(5!?!h!6(B\\)" "$(5$Q(B")
    ("\\($(5!?!h!?(B\\)" "$(5$R(B")
    ("\\($(5!?!h!L(B\\)" "$(5$S(B")
    ("\\($(5!?!h!M(B\\)" "$(5$T(B")
    ("\\($(5!?!i(B\\)" "$(5#?(B")                    ; Nukta
    ("\\($(5!@!h!M(B\\)" "$(5$`(B")
    ("\\($(5!@!i(B\\)" "$(5#@(B")                    ; Nukta
    ("\\($(5!A!h(B\\)[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"A(B")        ;         Half Form
    ("\\($(5!B!h(B\\)$(5!B!h!O(B" "$(5"B(B")              ; Special Rule for "t-tr"
    ("\\($(5!B!h!B!h(B\\)[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"c(B")    ;         Half Form
    ("\\($(5!B!h!B(B\\)" "$(5$a(B")
    ("\\($(5!B!h!F(B\\)" "$(5$b(B")
    ("\\($(5!B!h!O!h(B\\)[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"d(B")    ;         Half Form   Post "r"
    ("\\($(5!B!h!O(B\\)" "$(5"%(B")                  ;                     Post "r"
    ("\\($(5!B!h(B\\)[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"B(B")        ;         Half Form
    ("\\($(5!C!h!O(B\\)" "$(5!C"q(B")                ;                     Post "r"
    ("\\($(5!C!h(B\\)[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"C(B")        ;         Half Form
    ("\\($(5!D!h!D!h!M(B\\)" "$(5$c(B")
    ("\\($(5!D!h!E!h!M(B\\)" "$(5$d(B")
    ("\\($(5!D!h!K!h!M(B\\)" "$(5$e(B")
    ("\\($(5!D!h!K!h!O(B\\)" "$(5$r"r(B")            ; Special Case for "dbhr" ; ***
    ("\\($(5!D!h!O!h!M(B\\)" "$(5$f(B")
    ("\\($(5!D!h!T!h!M(B\\)" "$(5$g(B")
    ("\\($(5!D!h!5!h!O(B\\)" "$(5$h(B")
    ("\\($(5!D!h!6!h!O(B\\)" "$(5$i(B")
    ("\\($(5!D!h!D!h!T(B\\)" "$(5$j(B")
    ("\\($(5!D!h!E!h!T(B\\)" "$(5$k(B")
    ("\\($(5!D!h(B\\)$(5!E!h(B[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5!D!h(B")  ; Special Half Form (for ddhra)
    ("\\($(5!D!h!5(B\\)" "$(5$l(B")
    ("\\($(5!D!h!6(B\\)" "$(5$m(B")
    ("\\($(5!D!h!D(B\\)" "$(5$n(B")
    ("\\($(5!D!h!E(B\\)" "$(5$o(B")
    ("\\($(5!D!h!F(B\\)" "$(5$p(B")
    ("\\($(5!D!h(B\\)$(5!J!h(B" "$(5!D!h(B")              ; Suppressing "db-"
    ("\\($(5!D!h!J(B\\)" "$(5$q(B")
    ("\\($(5!D!h!K(B\\)" "$(5$r(B")
    ("\\($(5!D!h!L(B\\)" "$(5$s(B")
    ("\\($(5!D!h!M(B\\)" "$(5$t(B")
    ("\\($(5!D!h!T(B\\)" "$(5$u(B")
    ("\\($(5!E!h!F!h(B\\)[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"e(B")    ;         Half Form
    ("\\($(5!E!h!F(B\\)" "$(5$v(B")
    ("\\($(5!E!h!O!h(B\\)[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"f(B")    ;         Half Form     Post "r"
    ("\\($(5!E!h!O(B\\)" "$(5!E"q(B")                ;                       Post "r"
    ("\\($(5!E!h(B\\)[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"E(B")        ;         Half Form
    ("\\($(5!F!h!F!h(B\\)[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"k(B")    ;         Half Form
    ("\\($(5!F!h!F(B\\)" "$(5$w(B")
    ("\\($(5!F!h!O(B\\)" "$(5!F"q(B")
    ("\\($(5!F!h(B\\)[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"F(B")        ;         Half Form
    ("\\($(5!G!h(B\\)[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"G(B")        ; Nukta   Half Form
    ("\\($(5!H!h(B\\)$(5!B!h!O(B" "$(5"H(B")              ; Special Rule for "p-tr"
    ("\\($(5!H!h!B!h(B\\)[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"g(B")    ;         Half Form
    ("\\($(5!H!h!B(B\\)" "$(5$x(B")
    ("\\($(5!H!h!F(B\\)" "$(5$y(B")
    ("\\($(5!H!h!Q(B\\)" "$(5$z(B")
    ("\\($(5!H!h!O(B\\)" "$(5"&(B")                  ;                     Post "r"
    ("\\($(5!H!h(B\\)[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"H(B")        ;         Half Form
    ("\\($(5!I!h!O(B\\)" "$(5"'(B")                  ;                     Post "r"
    ("\\($(5!I!h(B\\)[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"I(B")        ;         Half Form
    ("\\($(5!I!i!h!O(B\\)" "$(5""(B")                ; Nukta               Post "r"
    ("\\($(5!I!i!h(B\\)[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"y(B")      ; Nukta   Half Form
    ("\\($(5!I!i(B\\)" "$(5#I(B")                    ; Nukta
    ("\\($(5!J!h(B\\)$(5!F!h(B[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"J(B")    ; Special Half Form
    ("\\($(5!J!h!F(B\\)" "$(5${(B")
    ("\\($(5!J!h(B\\)$(5!J!h(B[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"J(B")    ; Special Half Form
    ("\\($(5!J!h!J(B\\)" "$(5$|(B")
    ("\\($(5!J!h(B\\)$(5!T!h(B[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"J(B")    ; Special Half Form
    ("\\($(5!J!h!T(B\\)" "$(5$}(B")
    ("\\($(5!J!h!O(B\\)" "$(5!J"q(B")                ;                     Post "r"
    ("\\($(5!J!h(B\\)[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"J(B")        ;         Half Form
    ("\\($(5!K!h!F(B\\)" "$(5$~(B")
    ("\\($(5!K!h!O(B\\)" "$(5!K"q(B")                ;                     Post "r"
    ("\\($(5!K!h(B\\)[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"K(B")        ;         Half Form
    ("\\($(5!L!h!F(B\\)" "$(5#P(B")
    ("\\($(5!L!h!Q(B\\)" "$(5#Q(B")
    ("\\($(5!L!h!O(B\\)" "$(5!L"q(B")                ;                     Post "r"
    ("\\($(5!L!h(B\\)[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"L(B")        ;         Half Form
    ("\\($(5!M!h(B\\)[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"M(B")        ;         Half Form
    ("\\($(5!N!h(B\\)[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"N(B")        ;         Half Form
    ;; special form for "ru".
    ("\\($(5!O!](B\\)" "$(5",(B")
    ("\\($(5!O!^(B\\)" "$(5"-(B")
    ("\\($(5!P!](B\\)" "$(5".(B")
    ("\\($(5!P!^(B\\)" "$(5"/(B")
    ;;
    ("\\($(5!Q!h!Q(B\\)" "$(5#`(B" sanskrit)
    ("\\($(5!Q!h(B\\)[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"Q(B")        ;         Half Form
    ("\\($(5!R!h(B\\)[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"R(B")        ;         Half Form
    ("\\($(5!S!h(B\\)[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"S(B")        ;         Half Form
    ("\\($(5!T!h!F(B\\)" "$(5#a(B")
    ("\\($(5!T!h!T(B\\)" "$(5#b(B")
    ("\\($(5!T!h!O(B\\)" "$(5!T"q(B")                ;                     Post "r"
    ("\\($(5!T!h(B\\)[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"T(B")        ;         Half Form
    ("\\($(5!U!h!8!h(B\\)[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"h(B")    ;         Half Form
    ("\\($(5!U!h!8(B\\)" "$(5#c(B")
    ("\\($(5!U!h!F(B\\)" "$(5#d(B")
    ("\\($(5!U!h!J(B\\)" "$(5#e(B")
    ("\\($(5!U!h!Q(B\\)" "$(5#f(B")
    ("\\($(5!U!h(B\\)$(5!T!h!O(B" "$(5"U(B")              ; Special Half Form
    ("\\($(5!U!h!T!h(B\\)[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"j(B")    ;         Half Form
;   ("\\($(5!U!h!T(B\\)" "$(5#g(B")
    ("\\($(5!U!h!O!h!T(B\\)" "$(5#g(B")
    ("\\($(5!U!h!O!h(B\\)[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"i(B")    ;         Half Form
    ("\\($(5!U!h!O(B\\)" "$(5")(B")             ;                     Post "r"
    ("\\($(5!U!h(B\\)[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"U(B")        ;         Half Form
    ("\\($(5!V!h!=!h!O!h!M(B\\)" "$(5#h(B")
    ("\\($(5!V!h!=!h!M(B\\)" "$(5#i(B")
    ("\\($(5!V!h!=!h!T(B\\)" "$(5#j(B")
    ("\\($(5!V!h!=(B\\)" "$(5#k(B")
    ("\\($(5!V!h!>(B\\)" "$(5#l(B")
    ("\\($(5!V!h!O(B\\)" "$(5!V"q(B")                ;                     Post "r"
    ("\\($(5!V!h(B\\)[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"V(B")        ;         Half Form
    ("\\($(5!W!h!F!h(B\\)[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"W"F(B")  ; Special Half Form
    ("\\($(5!W!h!F(B\\)" "$(5#m(B")
    ("\\($(5!W!h!O(B\\)" "$(5#n(B")
    ("\\($(5!W!h(B\\)[$(5!3(B-$(5!N!P(B-$(5!X(B]" "$(5"W(B")        ;         Half Form
    ("\\($(5!X!h!A(B\\)" "$(5#p(B")
    ("\\($(5!X!h!F(B\\)" "$(5#q(B")
    ("\\($(5!X!h!L(B\\)" "$(5#r(B")
    ("\\($(5!X!h!M(B\\)" "$(5#s(B")
    ("\\($(5!X!h!O(B\\)" "$(5#t(B")
    ("\\($(5!X!h!Q(B\\)" "$(5#u(B")
    ("\\($(5!X!h!T(B\\)" "$(5#v(B")
    ;; Special Ligature Rules 
    ("\\($(5!X!_(B\\)" "$(5#R(B")

    ;; For consonants other than listed above, glyph-composition will
    ;; be applied.  If the consonant which is preceding "$(5!O(B" does not
    ;; have the vertical line (such as "$(5!?(B"), "$(5"r(B" is put beneath the
    ;; consonant.
    ;;
    ("[$(5!7!9!=!>!?!@!D!O!P!R!S!X(B]\\($(5!h!O(B\\)" "$(5"r(B")
    ("[$(5!6!8!C!E!F!H!J!K!L!M!T!V(B]\\($(5!h!O(B\\)" "$(5"q(B")
    ("$(5!?!i(B\\($(5!h!O(B\\)" "$(5"r(B")
    ("$(5!@!i(B\\($(5!h!O(B\\)" "$(5"r(B")

    ;; Nukta with Non-Consonants
    ("\\($(5!!!i(B\\)" "$(5#!(B")
    ("\\($(5!&!i(B\\)" "$(5#&(B")
    ("\\($(5!'!i(B\\)" "$(5#'(B")
    ("\\($(5!*!i(B\\)" "$(5#*(B")
    ("\\($(5![!i(B\\)" "$(5#L(B")
    ("\\($(5!\!i(B\\)" "$(5#M(B")
    ("\\($(5!_!i(B\\)" "$(5#K(B")
    ("\\($(5!j!i(B\\)" "$(5#J(B")

    ;; Special rule for "r + some vowels"
    ("\\($(5!O!_!i(B\\)" "$(5#*"p(B")
    ("\\($(5!O![!i(B\\)" "$(5#&"p(B")
    ("\\($(5!O!\!i(B\\)" "$(5#'"p(B")
    ("\\($(5!O!_(B\\)" "$(5!*"p(B")
    ;; If everything fails, "y" will connect to the front consonant.
    ("\\($(5!h!M(B\\)" "$(5"](B")
    )
  "Alist of regexps of Devanagari character sequences vs composed characters.")

(let ((rules devanagari-char-to-glyph-rules))
  (while rules
    (let ((rule (car rules)) 
	  (chars) (char) (glyphs) (glyph))
      (setq rules (cdr rules))
      (string-match "\\\\(\\(.+\\)\\\\)" (car rule))
      (setq chars (substring (car rule) (match-beginning 1) (match-end 1)))
      (setq char (string-to-char chars))
      (setq glyphs (cdr rule))
      (setq glyph (string-to-char (car glyphs)))
      (put-char-code-property 
       char 'char-to-glyph 
       ;; We don't "cons" it since priority is top to down.
       (append (get-char-code-property char 'char-to-glyph) (list rule)))

      (if (and (< ?(5z(B glyph) ; Glyphs only.
	       (null (get-char-code-property glyph 'glyph-to-char)))
	       ; One glyph may corresponds to multiple characters, 
	       ; e.g., surrounding vowel in Tamil, etc.
	       ; but for Devanagari, we put this restriction
	       ; to make sure the fact that one glyph corresponds to one char.
	  (put-char-code-property 
	   glyph 'glyph-to-char 
	   (cons (list (car glyphs) chars)
		 (get-char-code-property glyph 'glyph-to-char)
	   ))))))

;;
;; Function used in both characters-to-glyphs conversion and
;; glyphs-to-characters conversion.
;;

(defun max-match-len (regexp)
  "Return the maximum length of text that can match the pattern REGEXP.
Only [...] pattern of regexp is recognized."
  (let ((len 0)
	(index 0))
    (while (string-match "\\[\\([^\]]\\)+\\]" regexp index)
      (setq len (+ len (- (match-beginning 0) index) 1)
	    index (match-end 0)))
    len))

;; Return t iff at least one member appears in both LIST1 and LIST2.
(defun intersecting-p (list1 list2)
  (let ((found nil))
    (while (and list1 (not found))
      (if (memq (car list1) list2)
	  (setq found t)
	(setq list1 (cdr list1))))
    found))

(defun string-conversion-by-rule (source symbol &rest specs)
  "Convert string SOURCE by rules stored in SYMBOL property of each character.
The remaining arguments forms a list SPECS that restricts applicable rules.

The rules has the form ((REGEXP STR RULE-SPEC ...) ...).
Each character sequence in STRING that matches REGEXP is
replaced by STR.

If SPECS is nil, only rules with no RULE-SPECs is applied.  Otherwise
rules with no RULE-SPECS and rules that have at least one member of
SPECS in RULE-SPECs is applied.

Rules are tested in the order of the list, thus more specific rules
should be placed in front of less specific rules.

If rule is given in the forms of regexp '...\\(...\\)...', a character
sequence that matches the pattern inside of the parenthesis is the
subject of the match.  Otherwise, the entire expression is the subject
of the match."
  (let ((pos 0) 
	(dst-str ""))
    (while (< pos (length source))
      (let ((found nil)
	    (rules (get-char-code-property 
		    (string-to-char 
		     (substring source pos)) symbol)))
	(while rules
	  (let* ((rule (car rules))
		 (regexp (car rule))
		 (replace-str (car (cdr rule)))
		 (rule-specs (cdr (cdr rule)))
		 search-pos)
	    (if (not (or (null rule-specs)
			 (intersecting-p specs rule-specs)))
		(setq rules (cdr rules))
	      (if (null (string-match "\\\\(.+\\\\)" regexp))
		  (progn
		    (setq regexp (concat "\\(" regexp "\\)"))
		    (setq search-pos pos))
		(setq search-pos (- pos (max-match-len 
					 (substring regexp
						    (string-match "^[^\\\\]*" regexp)
						    (match-end 0))))))
	      (if (< search-pos 0) (setq search-pos 0))
	      (if (string-match regexp source search-pos)
		  (if (= (match-beginning 1) pos)
		      (progn
			(setq dst-str (concat dst-str replace-str))
			(setq rules nil) ; Get out of the loop.
			(setq found t)
			;; proceed `pos' for replaced characters.
			(setq pos (match-end 1)))
		    (setq rules (cdr rules)))
		(setq rules (cdr rules))))))
	;; proceed to next position
	(if (not found)
	    (setq dst-str (concat dst-str (substring source pos (1+ pos)))
		  pos (1+ pos)))))
    dst-str))


;;
;; Convert Character Code to Glyph Code
;;

;;;###autoload
(defun char-to-glyph-devanagari (string &rest langs)
  "Convert Devanagari characters in STRING to Devanagari glyphs.  
Ligatures and special rules are processed."
  (apply 
   'string-conversion-by-rule 
   (append (list string 'char-to-glyph) langs)))

;; Example:
;;(char-to-glyph-devanagari "$(5!X![!F!h!D!\(B") => "$(5!X!["F!D!\(B"
;;(char-to-glyph-devanagari "$(5!O!Z!V!h!=!h!O![!M(B") => ???

;;
;; Phase 2: Compose Glyphs to form One Glyph.
;;

;; Each list consists of glyph, application-priority and application-direction.
;;
;; Glyphs will be ordered from low priority number to high priority number.
;; If application-priority is omitted, it is assumed to be 0.
;; If application-direction is omitted, it is asumbed to be '(mr . ml).

(defconst devanagari-composition-rules
  '((?$(5!!(B 0 (tr . br))
    (?$(5!"(B 0 (mr . mr))
    (?$(5!#(B 0)
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
    (?$(5!Z(B 0)
    (?$(5![(B 0 (ml . mr))
    (?$(5!\(B 0)
    (?$(5!](B 0 (br . tr))
    (?$(5!^(B 0 (br . tr))
    (?$(5!_(B 0 (br . tr))
    (?$(5!`(B 0 (mr . mr))  ; (tc . bc)
    (?$(5!a(B 0 (mr . mr))
    (?$(5!b(B 0 (mr . mr))
    (?$(5!c(B 0 (mr . mr))
    (?$(5!d(B 0)
    (?$(5!e(B 0)
    (?$(5!f(B 0)
    (?$(5!g(B 0)
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
    (?$(5"p(B 10 (mr . mr))
    (?$(5"q(B 0 (br . br))
    (?$(5"r(B 0 (br . tr))
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
    (?$(5#K(B 0 (br . tr))
    (?$(5#L(B 0 (br . tr))
    (?$(5#M(B 0 (br . tr))
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

(defun devanagari-reorder-glyphs-for-composition (string start end)
  (let ((pos start)
	(ordered-glyphs nil))
    (while (< pos end)
      (let ((glyph (aref string pos)))
	(setq pos (1+ pos))
	(setq ordered-glyphs 
	      (append ordered-glyphs
		      (list (assq glyph devanagari-composition-rules))))))
    (sort ordered-glyphs '(lambda (x y) (< (car (cdr x)) (car (cdr y)))))))

! ;;(devanagari-compose-to-one-glyph "$(5"5!X![(B") => "4$(6!Xv#"5t%![0!X"5![1(B"

(defun devanagari-compose-to-one-glyph (devanagari-string)
  (let* ((o-glyph-list (devanagari-reorder-glyphs-for-composition
			devanagari-string 0 (length devanagari-string)))
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

(defun devanagari-composition-component (string &optional start end)
  (or start (setq start 0))
  (or end (setq end (length string)))
  (let* ((o-glyph-list (devanagari-reorder-glyphs-for-composition
			string start end))
	 ;; List of glyphs to be composed.
	 (cmp-glyph-list (list (car (car o-glyph-list)))))
    (setq o-glyph-list (cdr o-glyph-list))
    (while o-glyph-list
      (let* ((o-glyph (car o-glyph-list))
	     (glyph (if (< 2 (length o-glyph))
			;; default composition
			(list (car (cdr (cdr o-glyph))) (car o-glyph))
		      ;; composition with a specified rule
		      (list '(mr . ml) (car o-glyph)))))
	(setq o-glyph-list (cdr o-glyph-list))
	(setq cmp-glyph-list (append cmp-glyph-list glyph))))
    ;; Convert glyphs to 1-column width if possible.
    (devanagari-wide-to-narrow cmp-glyph-list)))

;; Utility function for Phase 2.5

;; Check whether GLYPH is a Devanagari vertical modifier or not.
;; If it is a vertical modifier, whether it should be 1-column shape or not
;; depends on previous non-vertical modifier.
(defun devanagari-vertical-modifier-p (glyph)
  (string-match (char-to-string glyph)
		"[$(5!"!]!^!_!`!a!b!c!h!i"p"q"r#K#L#M(B]"))

(defun devanagari-non-vertical-modifier-p (glyph)
  (string-match (char-to-string glyph)
;		"[$(5!Z![!\!d!e!f!g(B]"))
		"[$(5![(B]"))

(defun devanagari-wide-to-narrow-char (char)
  "Convert Devanagari character CHAR to the corresponding narrow character.
If there's no corresponding narrow character, return CHAR as is."
  (let ((narrow (cdr (assq char devanagari-1-column-char))))
    (or narrow char)))

;;
;;    Phase 2.5  Convert appropriate character to 1-column shape.
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

(defun devanagari-wide-to-narrow (src-list)
  (devanagari-wide-to-narrow-iter src-list t))

(defun devanagari-wide-to-narrow-iter (src-list 2-col-glyph)
  (let ((glyph (car src-list)))
    (cond ((null src-list) '())
	  ; not glyph code
	  ((not (numberp glyph)) 
	   (cons glyph
		 (devanagari-wide-to-narrow-iter (cdr src-list) 2-col-glyph)))
	  ; glyphs to be processed regardless of the value of "2-col-glyph"
	  ((devanagari-non-vertical-modifier-p glyph)
	   (cons (devanagari-wide-to-narrow-char glyph)
		 (devanagari-wide-to-narrow-iter (cdr src-list) 2-col-glyph)))
	  ; glyphs which are depends on the value of "2-col-glyph"
	  ((devanagari-vertical-modifier-p glyph)
	   (if 2-col-glyph
	       (cons glyph
		     (devanagari-wide-to-narrow-iter (cdr src-list) t))
	       (cons (devanagari-wide-to-narrow-char glyph)
		     (devanagari-wide-to-narrow-iter (cdr src-list)
						     2-col-glyph))))
	  ; normal glyph
	  (t
	   (if (cdr (assq glyph devanagari-1-column-char))
	       (cons (devanagari-wide-to-narrow-char glyph)
		     (devanagari-wide-to-narrow-iter (cdr src-list) nil))
	       (cons glyph
		     (devanagari-wide-to-narrow-iter (cdr src-list) t)))))))


;;
;; Summary
;; 

;;
;; Decomposition of composite sequence.
;;

;;;###autoload
(defun devanagari-decompose-string (str)
  "Decompose Devanagari string STR"
  (decompose-string (copy-sequence str)))

;;;###autoload
(defun devanagari-decompose-region (from to)
  (interactive "r")
  (decompose-region from to))

;;;
;;; Composition
;;;

;;;###autoload
(defun devanagari-compose-string (str &rest langs)
  (setq str (copy-sequence str))
  (let ((idx 0)
	rest match-b match-e)
    (while (string-match devanagari-composite-glyph-unit str idx)
      (let* ((match-b (match-beginning 0))
	     (match-e (match-end 0))
	     (cmps (devanagari-composition-component
		    (apply 
		     'char-to-glyph-devanagari
		     (cons (substring str match-b match-e) langs)))))
	(compose-string str match-b match-e cmps)
	(setq idx match-e))))
  str)

;;;###autoload
(defun devanagari-compose-region (from to &rest langs)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char (point-min))
      (while (re-search-forward devanagari-composite-glyph-unit nil t)
	(let* ((match-b (match-beginning 0)) (match-e (match-end 0))
	       (cmps (devanagari-composition-component
		      (apply 
		       'char-to-glyph-devanagari
		       (cons (buffer-substring match-b match-e) langs)))))
	  (compose-region match-b match-e cmps))))))

;; For pre-write and post-read conversion

;;;###autoload
(defun devanagari-compose-from-is13194-region (from to)
  "Compose IS 13194 characters in the region to Devanagari characters."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (indian-to-devanagari-region (point-min) (point-max))
      (devanagari-compose-region (point-min) (point-max))
      (- (point-max) (point-min)))))

;;;###autoload
(defun in-is13194-devanagari-post-read-conversion (len)
  (let ((pos (point)))
    (devanagari-compose-from-is13194-region pos (+ pos len))))

;;;###autoload
(defun devanagari-decompose-to-is13194-region (from to)
  "Decompose Devanagari characters in the region to IS 13194 characters."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (devanagari-decompose-region (point-min) (point-max))
      (devanagari-to-indian-region (point-min) (point-max)))))

;;;###autoload
(defun in-is13194-devanagari-pre-write-conversion (from to)
  (let ((old-buf (current-buffer)))
    (set-buffer (generate-new-buffer " *temp*"))
    (if (stringp from)
	(insert from)
      (insert-buffer-substring old-buf from to))
    (devanagari-decompose-to-is13194-region (point-min) (point-max))
    ;; Should return nil as annotations.
    nil))

;; For input/output of ITRANS

;;;###autoload
(defun devanagari-encode-itrans-region (from to)
  (interactive "r")
  (save-restriction
    (narrow-to-region from to)
    (devanagari-decompose-to-is13194-region (point-min) (point-max))
    (indian-encode-itrans-region (point-min) (point-max))))

;;;###autoload
(defun devanagari-decode-itrans-region (from to)
  (interactive "r")
  (save-restriction
    (narrow-to-region from to)
    (indian-decode-itrans-region (point-min) (point-max))
    (devanagari-compose-from-is13194-region (point-min) (point-max))))

;;
(provide 'devan-util)

;;; devan-util.el end here
