;;; lao-util.el --- utilities for Lao

;; Copyright (C) 1997 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.

;; Keywords: multilingual, Lao

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

;;; Code:

;;;###autoload
(defun setup-lao-environment ()
  "Setup multilingual environment (MULE) for Lao."
  (interactive)
  (setup-8-bit-environment "Lao" 'lao 'iso-2022-7bit
			   "lao"))

(let ((l '((?(1!(B consonant "LETTER KOR  KAI'" "CHICKEN")
	   (?(1"(B consonant "LETTER KHOR KHAI'" "EGG")
	   (?(1#(B invalid nil)
	   (?(1$(B consonant "LETTER QHOR QHWARGN" "BUFFALO")
	   (?(1%(B invalid nil)
	   (?  invalid nil)
	   (?(1'(B consonant "LETTER NGOR NGUU" "SNAKE")
	   (?(1((B consonant "LETTER JOR JUA" "BUDDHIST NOVICE")
	   (?(1)(B invalid nil)
	   (?(1*(B consonant "LETTER XOR X\"ARNG" "ELEPHANT")
	   (?(1+(B invalid nil)
	   (?(1,(B invalid nil)
	   (?(1-(B consonant "LETTER YOR YUNG" "MOSQUITO")
	   (?(1.(B invalid nil)
	   (?(1.(B invalid nil)
	   (?(1.(B invalid nil)
	   (?(1.(B invalid nil)
	   (?(1.(B invalid nil)
	   (?(1.(B invalid nil)
	   (?(14(B consonant "LETTER DOR DANG" "NOSE")
	   (?(15(B consonant "LETTER TOR TAR" "EYE")
	   (?(16(B consonant "LETTER THOR THUNG" "TO ASK,QUESTION")
	   (?(17(B consonant "LETTER DHOR DHARM" "FLAG")
	   (?(18(B invalid nil)
	   (?(19(B consonant "LETTER NOR NOK" "BIRD")
	   (?(1:(B consonant "LETTER BOR BED" "FISHHOOK")
	   (?(1;(B consonant "LETTER POR PAR" "FISH")
	   (?(1<(B consonant "LETTER HPOR HPER\"" "BEE")
	   (?(1=(B consonant "LETTER FHOR FHAR" "WALL")
	   (?(1>(B consonant "LETTER PHOR PHUU" "MOUNTAIN")
	   (?(1?(B consonant "LETTER FOR FAI" "FIRE")
	   (?(1@(B invalid nil)
	   (?(1A(B consonant "LETTER MOR MAR\"" "HORSE")
	   (?(1B(B consonant "LETTER GNOR GNAR" "MEDICINE")
	   (?(1C(B consonant "LETTER ROR ROD" "CAR")
	   (?(1D(B invalid nil)
	   (?(1E(B consonant "LETTER LOR LIING" "MONKEY")
	   (?(1F(B invalid nil)
	   (?(1G(B consonant "LETTER WOR WII" "HAND FAN")
	   (?(1H(B invalid nil)
	   (?(1I(B invalid nil)
	   (?(1J(B consonant "LETTER SOR SEA" "TIGER")
	   (?(1K(B consonant "LETTER HHOR HHAI" "JAR")
	   (?(1L(B invalid nil)
	   (?(1M(B consonant "LETTER OR OOW" "TAKE")
	   (?(1N(B consonant "LETTER HOR HEA" "BOAT")
	   (?(1O(B special "ELLIPSIS")
	   (?(1P(B vowel-base "VOWEL SIGN SARA A")
	   (?(1Q(B vowel-upper "VOWEL SIGN MAI KAN")
	   (?(1R(B vowel-base "VOWEL SIGN SARA AR")
	   (?(1S(B vowel-base "VOWEL SIGN SARA AM")
	   (?(1T(B vowel-upper "VOWEL SIGN SARA I") 
	   (?(1U(B vowel-upper "VOWEL SIGN SARA II")
	   (?(1V(B vowel-upper "VOWEL SIGN SARA EU")
	   (?(1W(B vowel-upper "VOWEL SIGN SARA UR")
	   (?(1X(B vowel-lower "VOWEL SIGN SARA U")
	   (?(1Y(B vowel-lower "VOWEL SIGN SARA UU")
	   (?(1Z(B invalid nil)
	   (?(1[(B vowel-upper "VOWEL SIGN MAI KONG")
	   (?(1\(B semivowel-lower "SEMIVOWEL SIGN LO")
	   (?(1](B vowel-base "SEMIVOWEL SIGN SARA IA")
	   (?(1^(B invalid nil)
	   (?(1_(B invalid nil)
	   (?(1`(B vowel-base "VOWEL SIGN SARA EE")
	   (?(1a(B vowel-base "VOWEL SIGN SARA AA")
	   (?(1b(B vowel-base "VOWEL SIGN SARA OO")
	   (?(1c(B vowel-base "VOWEL SIGN SARA EI MAI MUAN\"")
	   (?(1d(B vowel-base "VOWEL SIGN SARA AI MAI MAY")
	   (?(1e(B invalid nil)
	   (?(1f(B special "KO LA (REPETITION)") 
	   (?(1g(B invalid nil)
	   (?(1h(B tone "TONE MAI EK")
	   (?(1i(B tone "TONE MAI THO")
	   (?(1j(B tone "TONE MAI TI")
	   (?(1k(B tone "TONE MAI JADTAWAR")
	   (?(1l(B tone "CANCELLATION MARK")
	   (?(1m(B vowel-upper "VOWEL SIGN SARA OR")
	   (?(1n(B invalid nil)
	   (?(1o(B invalid nil)
	   (?(1p(B special "DIGIT ZERO")
	   (?(1q(B special "DIGIT ONE")
	   (?(1r(B special "DIGIT TWO")
	   (?(1s(B special "DIGIT THREE")
	   (?(1t(B special "DIGIT FOUR")
	   (?(1u(B special "DIGIT FIVE")
	   (?(1v(B special "DIGIT SIX")
	   (?(1w(B special "DIGIT SEVEN")
	   (?(1x(B special "DIGIT EIGHT")
	   (?(1y(B special "DIGIT NINE")
	   (?(1z(B invalid nil)
	   (?(1{(B invalid nil)
	   (?(1|(B consonant "LETTER NHOR NHUU" "MOUSE")
	   (?(1}(B consonant "LETTER MHOR MHAR" "DOG")
	   (?(1~(B invalid nil)
	   ))
      elm)
  (while l
    (setq elm (car l))
    (put-char-code-property (car elm) 'phonetic-type (car (cdr elm)))
    (put-char-code-property (car elm) 'name (nth 2 elm))
    (put-char-code-property (car elm) 'meaning (nth 3 elm))
    (setq l (cdr l))))

;;
(provide 'lao-util)

;;; lao-util.el ends here
