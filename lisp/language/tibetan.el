;;; tibetan.el --- support for Tibetan language -*- coding: utf-8-emacs; -*-

;; Copyright (C) 1997, 2001-2018 Free Software Foundation, Inc.
;; Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
;;   2006, 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021
;; Copyright (C) 2003
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H13PRO009

;; Author: Toru TOMABECHI <Toru.Tomabechi@orient.unil.ch>
;; Created: Feb. 17. 1997
;; Keywords: multilingual, Tibetan, i18n

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; History:

;; 1997.03.13 Modification for special signs and punctuation.

;;; Commentary:

;;; Code:

;;; Tibetan Character set.
;;; \x2130 -- \x234a is a subset of Unicode v.2 \x0f00 - \x0fb9
;;; with a slight modification. And there are some subjoined
;;; consonants which are not specified in Unicode.
;;; I hope I can add missing characters later.
;;;
;;;     00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F
;;;2120 // ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ; obsolete glyphs (2123-5)
;;;2130 ༀ ༁ ༂ ༃ ༄ ༅ ༆ ༇ ༈ ༉ ༊ ་ ༌ ། ༎ ༏ ; Punctuation,
;;;2140 ༐ ༑ ༒ ༓ ༔ ༕ ༖ ༗ ༘ ༙ ༚ ༛ ༜ ༝ ༞ ༟ ; Digits and
;;;2150 ༠ ༡ ༢ ༣ ༤ ༥ ༦ ༧ ༨ ༩ ༪ ༫ ༬ ༭ ༮ ༯ ; Special signs.
;;;2160 ༰ ༱ ༲ ༳ ༴ ༵ ༶ ༷ ༸ ༹ ༺ ༻ ༼ ༽ ༾ ༿ ;
;;;2170 ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� // ;
;;;
;;;     00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F
;;;2220 // ཀ ཁ ག གྷ ང ཅ ཆ ཇ ���� ཉ ཊ ཋ ཌ ཌྷ ཎ ; Base consonants
;;;2230 ཏ ཐ ད དྷ ན པ ཕ བ བྷ མ ཙ ཚ ཛ ཛྷ ཝ ཞ ; and
;;;2240 ཟ འ ཡ ར ལ ཤ ཥ ས ཧ ཨ ཀྵ ཪ ���� ���� ���� ���� ; Vowel signs.
;;;2250 ���� ���� ཱ ི ཱི ུ ཱུ ྲྀ ཷ ླྀ ཹ ེ ཻ ོ ཽ ཾ ; (\x2251 = vowel a)
;;;2260 ཿ ྀ ཱྀ ྂ ྃ ྄ ྅ ྆ ྇ ྈ ྉ ྊ ྋ ���� ���� ���� ; Long vowels and
;;;2270 ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� // ; vocalic r, l ARE
;;;                                                     ; atomically
;;;                                                     ; encoded.
;;;     00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F
;;;2320 // ྐ ྑ ྒ ྒྷ ྔ ྕ ྖ ྗ ���� ྙ ྚ ྛ ྜ ྜྷ ྞ ; Subjoined consonants
;;;2330 ྟ ྠ ྡ ྡྷ ྣ ྤ ྥ ྦ ྦྷ ྨ ྩ ྪ ྫ ྫྷ ྭ ྮ ;
;;;2340 ྯ ྰ ྱ ྲ ླ ྴ ྵ ྶ ྷ ྸ ྐྵ ྺ ྻ ྼ ���� ྾ ;
;;;2350 ྿ ࿀ ࿁ ࿂ ࿃ ࿄ ࿅ ࿆ ࿇ ࿈ ࿉ ࿊ ࿋ ࿌ ���� ���� ; Hereafter, the chars
;;;2360 ࿏ ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ; are not specified
;;;2370 ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� // ; in Unicode.
;;;
;;;     00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F
;;;2420 // ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ; Precomposed
;;;2430 ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ; consonants for
;;;2440 ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ; ordinary Tibetan.
;;;2450 ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ; They are decomposed
;;;2460 ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ; into base and
;;;2470 ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� // ; subjoined consonants
;;;                                                     ; when written on a
;;;     00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F ; file in Tibetan
;;;2520 // ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ; coding system.
;;;2530 ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ;
;;;2540 ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ;
;;;2550 ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ;
;;;2560 ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ;
;;;2570 ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� // ;
;;;


(define-coding-system 'tibetan-iso-8bit
  "8-bit encoding for ASCII (MSB=0) and TIBETAN (MSB=1)."
  :coding-type 'iso-2022
  :mnemonic ?Q
  :designation [ascii tibetan nil nil]
  :charset-list '(ascii tibetan))

(define-coding-system-alias 'tibetan 'tibetan-iso-8bit)

(set-language-info-alist
 "Tibetan" '((charset tibetan tibetan-1-column)
	     (coding-system tibetan-iso-8bit)
	     (coding-priority iso-2022-7bit tibetan-iso-8bit)
	     (input-method . "tibetan-wylie")
	     (features tibet-util)
	     (documentation . t)
	     (sample-text . "Tibetan (བོད་སྐ����ད་) ༄༅༅༎བཀྲ����་ཤིས་བདེ་ལེགས༎ཨོཾ་མ����་ཎི་པ����དྨེ་ཧའུྂ༎")))

;; `འ' is included in the pattern for subjoined consonants because we
;; treat it specially in tibetan-add-components.
;; modified by Tomabechi 1999/12/10
;; modified by Tomabechi 2000/06/08
;;          To allow infinite addition of vowels/modifiers
;;          as specified in Unicode v.3
;; འ is removed from the class of subjoined. Tomabechi 2000/06/08
;; (for Unicode support)
(defconst tibetan-composable-pattern
  "[ཀ-ཀྵཪ][ྐ-ྐྵྺྻྼ]*[཰����ཱི-྄ཱཽྀྀ]*[ཾྂྃ྆-ྋ༙༵༷]*"
  "Regexp matching a composable sequence of Tibetan characters.")

;;;
;;; Definitions of conversion data.
;;;


;;; alists for tibetan char <-> transcription conversion
;;; longer transcription should come first
(defconst tibetan-consonant-transcription-alist
  '(("tsh" . "ཚ")
    ("dzh" . "ཛྷ")
    ("kSH" . "ཀྵ")
    ("kh" . "ཁ")
    ("gh" . "གྷ")
    ("ng" . "ང")
    ("ch" . "ཆ")
    ("ny" . "ཉ")
    ("TH" . "ཋ")
    ("DH" . "ཌྷ")
    ("th" . "ཐ")
    ("dh" . "དྷ")
    ("ph" . "ཕ")
    ("bh" . "བྷ")
    ("ts" . "ཙ")
    ("dz" . "ཛ")
    ("zh" . "ཞ")
    ("sh" . "ཤ")
    ("SH" . "ཥ")
    ("k" . "ཀ")
    ("g" . "ག")
    ("c" . "ཅ")
    ("j" . "ཇ")
    ("T" . "ཊ")
    ("D" . "ཌ")
    ("N" . "ཎ")
    ("t" . "ཏ")
    ("d" . "ད")
    ("n" . "ན")
    ("p" . "པ")
    ("b" . "བ")
    ("m" . "མ")
    ("w" . "ཝ")
    ("z" . "ཟ")
    ("'" . "འ")
    ("y" . "ཡ")
    ("r" . "ར")
    ("l" . "ལ")
    ("s" . "ས")
    ("h" . "ཧ")
    ("H" . "ཧ")
    ("A" . "ཨ")
    ;; Added by Tomabechi 1999/12/10
    ("R" . "ཪ") ;; fixed form RA
    ))


(defconst tibetan-vowel-transcription-alist
  '(
    ;; Composite Vowels
    ;; Added by Tomabechi 2000/06/08
    ("frr" . "ཷ")
    ("fll" . "ཹ")
    ("fa" . "ཱ")
    ("fi" . "ཱི")
    ("fu" . "ཱུ")
    ("fr" . "ྲྀ")
    ("fl" . "ླྀ")
    ("fI" . "ཱྀ")
    ;; Normal Vowels
    ("ai" . "ཻ")
    ("au" . "ཽ")
    ("ee" . "ཻ")
    ("oo" . "ཽ")
    ("a" . "����")			; invisible vowel sign (\x2251)
    ("i" . "ི")
    ("u" . "ུ")
    ("e" . "ེ")
    ("o" . "ོ")
    ("E" . "ཻ")
    ("O" . "ཽ")
    ("I" . "ྀ")
    ("," . "྄")			; idem.
    ))

(defconst tibetan-modifier-transcription-alist
  '(("M" . "ཾ")
    ("~" . "ྂ")
    ("`" . "ྃ")
    ("x" . "ྈ")
    ("X" . "ྉ")
    ("v" . "྆")
    ("V" . "྇")
    ("q" . "ྊ")
    ("Q" . "ྋ")
    ("_/" . "༙")
    ("_o" . "༷")
    ("_O" . "༵")))

(defconst tibetan-precomposed-transcription-alist
  '(("phyw" . "����")
    ("tshw" . "����")
    ("rtsw" . "����")
    ("khw" . "����")
    ("nyw" . "����")
    ("tsw" . "����")
    ("zhw" . "����")
    ("shw" . "����")
    ("khy" . "����")
    ("phy" . "����")
    ("khr" . "����")
    ("thr" . "����")
    ("phr" . "����")
    ("shr" . "����")
    ("dzr" . "����")
    ("grw" . "����")
    ("rng" . "����")
    ("rny" . "����")
    ("rts" . "����")
    ("rdz" . "����")
    ("rgw" . "����")
    ("rky" . "����")
    ("rgy" . "����")
    ("rmy" . "����")
    ("lng" . "����")
    ("sng" . "����")
    ("sny" . "����")
    ("sts" . "����")
    ("sky" . "����")
    ("sgy" . "����")
    ("spy" . "����")
    ("sby" . "����")
    ("smy" . "����")
    ("skr" . "����")
    ("sgr" . "����")
    ("snr" . "����")
    ("spr" . "����")
    ("sbr" . "����")
    ("smr" . "����")
    ("kw" . "����")
    ("gw" . "����")
    ("cw" . "����")
    ("tw" . "����")
    ("dw" . "����")
    ("zw" . "����")
    ("rw" . "����")
    ("lw" . "����")
    ("sw" . "����")
    ("hw" . "����")
    ("ky" . "����")
    ("gy" . "����")
    ("py" . "����")
    ("by" . "����")
    ("my" . "����")
    ("kr" . "����")
    ("gr" . "����")
    ("tr" . "����")
    ("dr" . "����")
    ("pr" . "����")
    ("brk" . "བ����")
    ("brg" . "བ����")
    ("brng" . "བ����")
    ("brj" . "བ����")
    ("brny" . "བ����")
    ("brt" .  "བ����")
    ("brd" . "བ����")
    ("brn" . "བ����")
    ("brts" . "བ����")
    ("brdz" . "བ����")
    ("brl" . "བ����")
    ("br" . "����")
    ("mr" . "����")
    ("sr" . "����")
    ("hr" . "����")
    ("jr" . "����")
    ("kl" . "����")
    ("gl" . "����")
    ("blt" . "བ����")
    ("bld" . "བ����")
    ("bl" . "����")
    ("zl" . "����")
    ("rl" . "����")
    ("sl" . "����")
    ("rk" . "����")
    ("rg" . "����")
    ("rj" . "����")
    ("rt" . "����")
    ("rd" . "����")
    ("rn" . "����")
    ("rb" . "����")
    ("rm" . "����")
    ("lk" . "����")
    ("lg" . "����")
    ("lc" . "����")
    ("lj" . "����")
    ("lt" . "����")
    ("ld" . "����")
    ("ln" . "����")			; dummy \x2121
    ("lp" . "����")
    ("lb" . "����")
    ("lh" . "����")
    ("sk" . "����")
    ("sg" . "����")
    ("st" . "����")
    ("sd" . "����")
    ("sn" . "����")
    ("sp" . "����")
    ("sb" . "����")
    ("sm" . "����"))
  )


(defconst tibetan-subjoined-transcription-alist
  (sort '(("+k"  . "ྐ")
	  ("+kh" . "ྑ")
	  ("+g"  . "ྒ")
	  ("+gh" . "ྒྷ")
	  ("+ng" . "ྔ")
	  ("+c"  . "ྕ")
	  ("+ch" . "ྖ")
	  ("+j"  . "ྗ")
	  ("+ny"  . "ྙ")
	  ("+T"  . "ྚ")
	  ("+TH" . "ྛ")
	  ("+D"  . "ྜ")
	  ("+DH" . "ྜྷ")
	  ("+N"  . "ྞ")
	  ("+t"  . "ྟ")
	  ("+th" . "ྠ")
	  ("+d"  . "ྡ")
	  ("+dh" . "ྡྷ")
	  ("+n"  . "ྣ")
	  ("+p"  . "ྤ")
	  ("+ph" . "ྥ")
	  ("+b"  . "ྦ")
	  ("+bh" . "ྦྷ")
	  ("+m"  . "ྨ")
	  ("+ts" . "ྩ")
	  ("+tsh" . "ྪ")
	  ("+dz" . "ྫ")
	  ("+dzh" . "ྫྷ")
	  ("+w"  . "ྭ")
	  ("+zh" . "ྮ")
	  ("+z"  . "ྯ")
	  ("+'"  . "ྰ")
	  ("+y"  . "ྱ")
	  ("+r"  . "ྲ")
	  ("+l"  . "ླ")
	  ("+sh" . "ྴ")
	  ("+SH" . "ྵ")
	  ("+s"  . "ྶ")
	  ("+h"  . "ྷ")
	  ("+A"  . "ྸ")
	  ("+kSH" . "ྐྵ")
	  ;; Added by Tomabechi 1999/12/10
	  ("+W" . "ྺ") ;; fixed form subscribed WA
	  ("+Y" . "ྻ") ;; fixed form subscribed YA
	  ("+R" . "ྼ") ;; fixed form subscribed RA
	  )
	(lambda (x y) (> (length (car x)) (length (car y))))))

;;;
;;; alist for Tibetan base consonant <-> subjoined consonant conversion.
;;;
(defconst tibetan-base-to-subjoined-alist
  '(("ཀ" . "ྐ")
    ("ཁ" . "ྑ")
    ("ག" . "ྒ")
    ("གྷ" . "ྒྷ")
    ("ང" . "ྔ")
    ("ཅ" . "ྕ")
    ("ཆ" . "ྖ")
    ("ཇ" . "ྗ")
    ("ཉ" . "ྙ")
    ("ཊ" . "ྚ")
    ("ཋ" . "ྛ")
    ("ཌ" . "ྜ")
    ("ཌྷ" . "ྜྷ")
    ("ཎ" . "ྞ")
    ("ཏ" . "ྟ")
    ("ཐ" . "ྠ")
    ("ད" . "ྡ")
    ("དྷ" . "ྡྷ")
    ("ན" . "ྣ")
    ("པ" . "ྤ")
    ("ཕ" . "ྥ")
    ("བ" . "ྦ")
    ("བྷ" . "ྦྷ")
    ("མ" . "ྨ")
    ("ཙ" . "ྩ")
    ("ཚ" . "ྪ")
    ("ཛ" . "ྫ")
    ("ཛྷ" . "ྫྷ")
    ("ཝ" . "ྭ")
    ("ཞ" . "ྮ")
    ("ཟ" . "ྯ")
    ("འ" . "ྰ")
    ("ཡ" . "ྱ")
    ("ར" . "ྲ")
    ("ལ" . "ླ")
    ("ཤ" . "ྴ")
    ("ཥ" . "ྵ")
    ("ས" . "ྶ")
    ("ཧ" . "ྷ")
    ("ཨ" . "ྸ")
    ("ཀྵ" . "ྐྵ")
    ;; Added by Tomabechi 1999/12/10
    ("ཪ" . "ྼ") ;; Fixed form RA (224B->234D)
    ))

;;; alist for Tibetan composite vowels (long i, vocalic r, etc.)
;;; New variable. created by Tomabechi 2000/06/08
(defconst tibetan-composite-vowel-alist
  '(;; LONG A
    ;; ("ཱ" . ((bc . tc) ?ཱ))
    ;; LONG I
    ("ཱི" . (?ཱ (tc . bc) ?ི))
    ;; LONG U
    ("ཱུ" . (?ཱ (bc . tc) ?ུ))
    ;; VOCALIC R
    ("ྲྀ" . (?ྲ (tc . bc) ?ྀ))
    ;; LONG VOCALIC R
    ("ཷ" . (?ྲ (bc . tc) ?ཱ (tc . bc) ?ྀ))
    ;; VOCALIC L
    ("ླྀ" . (?ླ (tc . bc) ?ྀ))
    ;;་LONG VOCALIC L
    ("ཹ" . (?ླ (bc . tc) ?ཱ (tc . bc) ?ྀ))
    ;; LONG REVERSE I
    ("ཱྀ" . (?ཱ (tc . bc) ?ྀ))
    ))



;;;
;;; alist for Tibetan consonantic components <-> precomposed glyph conversion.
;;; (includes some punctuation conversion rules)
;;;
(defconst tibetan-precomposition-rule-alist
  `(("ཕྱྭ" . "����")
    ("གྲྭ" . "����")
    ("ཚྭ" . "����")
    ("རྩྭ" . "����")
    ("རྒྭ" . "����")
    ("རྐྱ" . "����")
    ("རྒྱ" . "����")
    ("རྨྱ" . "����")
    ("སྐྱ" . "����")
    ("སྒྱ" . "����")
    ("སྤྱ" . "����")
    ("སྦྱ" . "����")
    ("སྨྱ" . "����")
    ("སྐྲ" . "����")
    ("སྒྲ" . "����")
    ("སྣྲ" . "����")
    ("སྤྲ" . "����")
    ("སྦྲ" . "����")
    ("སྨྲ" . "����")
    ("ཁྭ" . "����")
    ("ཉྭ" . "����")
    ("ཙྭ" . "����")
    ("ཞྭ" . "����")
    ("ཤྭ" . "����")
    ("ཁྱ" . "����")
    ("ཕྱ" . "����")
    ("ཁྲ" . "����")
    ("ཐྲ" . "����")
    ("ཕྲ" . "����")
    ("ཤྲ" . "����")
    ("ཛྲ" . "����")
    ("རྔ" . "����")
    ("རྙ" . "����")
    ("རྩ" . "����")
    ("རྫ" . "����")
    ("ལྔ" . "����")
    ("སྔ" . "����")
    ("སྙ" . "����")
    ("སྩ" . "����")
    ("ཀྭ" . "����")
    ("གྭ" . "����")
    ("ཅྭ" . "����")
    ("ཏྭ" . "����")
    ("དྭ" . "����")
    ("ཟྭ" . "����")
    ("རྭ" . "����")
    ("ལྭ" . "����")
    ("སྭ" . "����")
    ("ཧྭ" . "����")
    ("ཀྱ" . "����")
    ("གྱ" . "����")
    ("པྱ" . "����")
    ("བྱ" . "����")
    ("མྱ" . "����")
    ("ཀྲ" . "����")
    ("གྲ" . "����")
    ("ཏྲ" . "����")
    ("དྲ" . "����")
    ("པྲ" . "����")
    ("བྲ" . "����")
    ("མྲ" . "����")
    ("སྲ" . "����")
    ("ཧྲ" . "����")
    ("ཇྲ" . "����")
    ("ཀླ" . "����")
    ("གླ" . "����")
    ("བླ" . "����")
    ("ཟླ" . "����")
    ("རླ" . "����")
    ("སླ" . "����")
    ("རྐ" . "����")
    ("རྒ" . "����")
    ("རྗ" . "����")
    ("རྟ" . "����")
    ("རྡ" . "����")
    ("རྣ" . "����")
    ("རྦ" . "����")
    ("རྨ" . "����")
    ("ལྐ" . "����")
    ("ལྒ" . "����")
    ("ལྣ" . "����") ; dummy 0x2121 added 2000/06/08 for transition l -> lng
    ("ལྕ" . "����")
    ("ལྗ" . "����")
    ("ལྟ" . "����")
    ("ལྡ" . "����")
    ("ལྤ" . "����")
    ("ལྦ" . "����")
    ("ལྷ" . "����")
    ("སྐ" . "����")
    ("སྒ" . "����")
    ("སྟ" . "����")
    ("སྡ" . "����")
    ("སྣ" . "����")
    ("སྤ" . "����")
    ("སྦ" . "����")
    ("སྨ" . "����")))

(defconst tibetan-regexp
  (let ((l (list tibetan-precomposed-transcription-alist
		 tibetan-consonant-transcription-alist
		 tibetan-vowel-transcription-alist
		 tibetan-modifier-transcription-alist
		 tibetan-subjoined-transcription-alist))
	(separator "\\|")
	tail pattern)
    (while l
      (setq tail (car l) l (cdr l))
      (while tail
	(setq pattern (cons separator (cons (car (car tail)) pattern))
	      tail (cdr tail))))
    (apply 'concat (nreverse (cdr pattern))))
  "Regexp matching a Tibetan transcription of a composable Tibetan sequence.
The result of matching is to be used for indexing alists at conversion
from a roman transcription to the corresponding Tibetan character.")

(defvar tibetan-precomposed-regexp
  (purecopy
  (let ((l tibetan-precomposed-transcription-alist)
	temp)
    (setq temp "^\\(")
    (setq temp
	  (concat temp (car (car l))))
    (setq l (cdr l))
    (while l
      (setq temp
	    (concat temp "\\|" (car (car l))))
      (setq l (cdr l)))
    (concat temp "\\)")))
  "Regexp string to match a romanized Tibetan complex consonant.
The result of matching is to be used for indexing alists when the input key
from an input method is converted to the corresponding precomposed glyph.")

(defvar tibetan-precomposition-rule-regexp
  (purecopy
  (let ((l tibetan-precomposition-rule-alist)
	temp)
    (setq temp "\\(")
    (setq temp (concat temp (car (car l))))
    (setq l (cdr l))
    (while l
      (setq temp (concat temp "\\|" (car (car l))))
      (setq l (cdr l)))
    (concat temp "\\)")))
  "Regexp string to match a sequence of Tibetan consonantic components, i.e.,
one base consonant and one or more subjoined consonants.
The result of matching is to be used for indexing alist when the component
sequence is converted to the corresponding precomposed glyph.
This also matches some punctuation characters which need conversion.")

(defvar tibetan-decomposed nil)
(defvar tibetan-decomposed-temp nil)

;; For automatic composition.
(set-char-table-range
 composition-function-table '(#xF00 . #xFD1)
 (list (vector tibetan-composable-pattern 0 'font-shape-gstring)))

(provide 'tibetan)

;;; tibetan.el ends here
