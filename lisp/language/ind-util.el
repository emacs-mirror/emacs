;;; ind-util.el --- Transliteration and Misc. Tools for Indian Languages -*- coding: utf-8-emacs; -*-

;; Copyright (C) 2001-2020 Free Software Foundation, Inc.

;; Keywords: multilingual, Indian, Devanagari

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

;;; Commentary:

;; This file provides conversion between UCS and various
;; transliteration schemes, such as ITRANS, kyoto-harvard and aiba
;; methods.  It also provides conversion between IS 13194 and UCS.
;; Finally, this program provides the compatibility support with
;; old implementation of Devanagari script.

;;; Code:

;;; Transliteration

;; The followings provide the various transliteration schemes (such as
;; ITRANS, kyoto-harvard, and Aiba) of Indian scripts.  They are also
;; used in quail/indian.el for typing Indian script in Emacs.

(eval-and-compile

(defun indian-regexp-of-hashtbl-keys (hashtbl)
  "Return the regular expression of hash table keys."
  (let (keys)
    (maphash (lambda (key val) (push key keys)) hashtbl)
    (regexp-opt keys)))

(defvar indian-dev-base-table
  '(
    (;; VOWELS  (18)
     (?अ nil) (?आ ?ा) (?इ ?ि) (?ई ?ी) (?उ ?ु) (?ऊ ?ू)
     (?ऋ ?ृ) (?ऌ ?ॢ) (?ऍ ?ॅ) (?ऎ ?ॆ) (?ए ?े) (?ऐ ?ै)
     (?ऑ ?ॉ) (?ऒ ?ॊ) (?ओ ?ो) (?औ ?ौ) (?ॠ ?ॄ) (?ॡ ?ॣ))
    (;; CONSONANTS (currently 42, including special cases)
     ?क ?ख ?ग ?घ ?ङ                  ;; GUTTRULS
     ?च ?छ ?ज ?झ ?ञ                  ;; PALATALS
     ?ट ?ठ ?ड ?ढ ?ण                  ;; CEREBRALS
     ?त ?थ ?द ?ध ?न ?ऩ              ;; DENTALS
     ?प ?फ ?ब ?भ ?म                  ;; LABIALS
     ?य ?र ?ऱ ?ल ?ळ ?ऴ ?व          ;; SEMIVOWELS
     ?श ?ष ?स ?ह                    ;; SIBILANTS
     ?क़ ?ख़ ?ग़ ?ज़ ?ड़ ?ढ़ ?फ़ ?य़      ;; NUKTAS
     "ज्ञ" "क्ष")
    (;; Misc Symbols (7)
     ?ँ ?ं ?ः ?ऽ ?् ?ॐ ?।)
    (;; Digits (10)
     ?० ?१ ?२ ?३ ?४ ?५ ?६ ?७ ?८ ?९)
    (;; Inscript-extra (4)  (#, $, ^, *, ])
     "्र" "र्" "त्र" "श्र" "़")))

;; Punjabi is also known as Gurmukhi.
(defvar indian-pnj-base-table
  '(
    (;; VOWELS
     (?ਅ nil) (?ਆ ?ਾ) (?ਇ ?ਿ) (?ਈ ?ੀ) (?ਉ ?ੁ) (?ਊ ?ੂ)
     nil nil nil nil (?ਏ ?ੇ) (?ਐ ?ੈ)
     nil nil (?ਓ ?ੋ) (?ਔ ?ੌ) nil nil)
    (;; CONSONANTS
     ?ਕ ?ਖ ?ਗ ?ਘ ?ਙ                  ;; GUTTRULS
     ?ਚ ?ਛ ?ਜ ?ਝ ?ਞ                  ;; PALATALS
     ?ਟ ?ਠ ?ਡ ?ਢ ?ਣ                  ;; CEREBRALS
     ?ਤ ?ਥ ?ਦ ?ਧ ?ਨ nil              ;; DENTALS
     ?ਪ ?ਫ ?ਬ ?ਭ ?ਮ                  ;; LABIALS
     ?ਯ ?ਰ nil ?ਲ ?ਲ਼ nil ?ਵ          ;; SEMIVOWELS
     ?ਸ਼ nil ?ਸ ?ਹ                    ;; SIBILANTS
     nil ?ਖ਼ ?ਗ਼ ?ਜ਼ ?ੜ nil ?ਫ਼ nil      ;; NUKTAS
     "ਜ੍ਞ" nil)
    (;; Misc Symbols (7)
     nil ?ਂ nil nil ?੍ nil nil) ;; ek onkar, etc.
    (;; Digits
     ?੦ ?੧ ?੨ ?੩ ?੪ ?੫ ?੬ ?੭ ?੮ ?੯)
    (;; Inscript-extra (4)  (#, $, ^, *, ])
     "੍ਰ" "ਰ੍" "ਤ੍ਰ" "ਸ਼੍ਰ" "਼")))

(defvar indian-gjr-base-table
  '(
    (;; VOWELS
     (?અ nil) (?આ ?ા) (?ઇ ?િ) (?ઈ ?ી) (?ઉ ?ુ) (?ઊ ?ૂ)
     (?ઋ ?ૃ) nil (?ઍ ?ૅ) nil (?એ ?ે) (?ઐ ?ૈ)
     (?ઑ ?ૉ) nil (?ઓ ?ો) (?ઔ ?ૌ) (?ૠ ?ૄ) nil)
    (;; CONSONANTS
     ?ક ?ખ ?ગ ?ઘ ?ઙ                  ;; GUTTRULS
     ?ચ ?છ ?જ ?ઝ ?ઞ                  ;; PALATALS
     ?ટ ?ઠ ?ડ ?ઢ ?ણ                  ;; CEREBRALS
     ?ત ?થ ?દ ?ધ ?ન nil              ;; DENTALS
     ?પ ?ફ ?બ ?ભ ?મ                  ;; LABIALS
     ?ય ?ર nil ?લ ?ળ nil ?વ          ;; SEMIVOWELS
     ?શ ?ષ ?સ ?હ                    ;; SIBILANTS
     nil nil nil nil nil nil nil nil      ;; NUKTAS
     "જ્ઞ" "ક્ષ")
    (;; Misc Symbols (7)
     ?ઁ ?ં ?ઃ ?ઽ ?્ ?ૐ nil)
    (;; Digits
     ?૦ ?૧ ?૨ ?૩ ?૪ ?૫ ?૬ ?૭ ?૮ ?૯)
    (;; Inscript-extra (4)  (#, $, ^, *, ])
     "્ર" "ર્" "ત્ર" "શ્ર" "઼")))

(defvar indian-ori-base-table
  '(
    (;; VOWELS
     (?ଅ nil) (?ଆ ?ା) (?ଇ ?ି) (?ଈ ?ୀ) (?ଉ ?ୁ) (?ଊ ?ୂ)
     (?ଋ ?ୃ) (?ଌ nil) nil nil (?ଏ ?େ) (?ଐ ?ୈ)
     nil nil (?ଓ ?ୋ) (?ଔ ?ୌ) (?ୠ nil) (?ୡ nil))
    (;; CONSONANTS
     ?କ ?ଖ ?ଗ ?ଘ ?ଙ                  ;; GUTTRULS
     ?ଚ ?ଛ ?ଜ ?ଝ ?ଞ                  ;; PALATALS
     ?ଟ ?ଠ ?ଡ ?ଢ ?ଣ                  ;; CEREBRALS
     ?ତ ?ଥ ?ଦ ?ଧ ?ନ nil              ;; DENTALS
     ?ପ ?ଫ ?ବ ?ଭ ?ମ                  ;; LABIALS
     ?ଯ ?ର nil ?ଲ ?ଳ nil nil          ;; SEMIVOWELS
     ?ଶ ?ଷ ?ସ ?ହ                    ;; SIBILANTS
     nil nil nil nil ?ଡ଼ ?ଢ଼ nil ?ୟ      ;; NUKTAS
     "ଜ୍ଞ" "କ୍ଷ")
    (;; Misc Symbols
     ?ଁ ?ଂ ?ଃ ?ଽ ?୍ nil nil)
    (;; Digits
     ?୦ ?୧ ?୨ ?୩ ?୪ ?୫ ?୬ ?୭ ?୮ ?୯)
    (;; Inscript-extra (4)  (#, $, ^, *, ])
     "୍ର" "ର୍" "ତ୍ର" "ଶ୍ର" "଼")))

(defvar indian-bng-base-table
  '(
    (;; VOWELS
     (?অ nil) (?আ ?া) (?ই ?ি) (?ঈ ?ী) (?উ ?ু) (?ঊ ?ূ)
     (?ঋ ?ৃ) (?ঌ ?ৢ) nil nil (?এ ?ে) (?ঐ ?ৈ)
     nil nil (?ও ?ো) (?ঔ ?ৌ) (?ৠ ?ৄ) (?ৡ ?ৣ))
    (;; CONSONANTS
     ?ক ?খ ?গ ?ঘ ?ঙ                  ;; GUTTRULS
     ?চ ?ছ ?জ ?ঝ ?ঞ                  ;; PALATALS
     ?ট ?ঠ ?ড ?ঢ ?ণ                  ;; CEREBRALS
     ?ত ?থ ?দ ?ধ ?ন nil              ;; DENTALS
     ?প ?ফ ?ব ?ভ ?ম                  ;; LABIALS
     ?য ?র nil ?ল nil nil nil          ;; SEMIVOWELS
     ?শ ?ষ ?স ?হ                    ;; SIBILANTS
     nil nil nil nil ?ড় ?ঢ় nil ?য়      ;; NUKTAS
     "জ্ঞ" "ক্ষ")
    (;; Misc Symbols
     ?ঁ ?ং ?ঃ nil ?্ nil nil)
    (;; Digits
     ?০ ?১ ?২ ?৩ ?৪ ?৫ ?৬ ?৭ ?৮ ?৯)
    (;; Inscript-extra (4)  (#, $, ^, *, ])
     "্র" "র্" "ত্র" "শ্র" "়")))

(defvar indian-asm-base-table
  '(
    (;; VOWELS
     (?অ nil) (?আ ?া) (?ই ?ি) (?ঈ ?ী) (?উ ?ু) (?ঊ ?ূ)
     (?ঋ ?ৃ) (?ঌ ?ৢ) nil nil (?এ ?ে) (?ঐ ?ৈ)
     nil nil (?ও ?ো) (?ঔ ?ৌ) (?ৠ ?ৄ) (?ৡ ?ৣ))
    (;; CONSONANTS
     ?ক ?খ ?গ ?ঘ ?ঙ                  ;; GUTTRULS
     ?চ ?ছ ?জ ?ঝ ?ঞ                  ;; PALATALS
     ?ট ?ঠ ?ড ?ঢ ?ণ                  ;; CEREBRALS
     ?ত ?থ ?দ ?ধ ?ন nil              ;; DENTALS
     ?প ?ফ ?ব ?ভ ?ম                  ;; LABIALS
     ?য ?ৰ nil ?ল nil nil ?ৱ          ;; SEMIVOWELS
     ?শ ?ষ ?স ?হ                    ;; SIBILANTS
     nil nil nil nil ?ড় ?ঢ় nil ?য়      ;; NUKTAS
     "জ্ঞ" "ক্ষ")
    (;; Misc Symbols
     ?ঁ ?ং ?ঃ nil ?্ nil nil)
    (;; Digits
     ?০ ?১ ?২ ?৩ ?৪ ?৫ ?৬ ?৭ ?৮ ?৯)
    (;; Inscript-extra (4)  (#, $, ^, *, ])
     "্ৰ" "ৰ্" "ত্ৰ" "শ্ৰ" "়")))

(defvar indian-tlg-base-table
  '(
    (;; VOWELS
     (?అ nil) (?ఆ ?ా) (?ఇ ?ి) (?ఈ ?ీ) (?ఉ ?ు) (?ఊ ?ూ)
     (?ఋ ?ృ) (?ఌ nil) nil (?ఏ ?ే) (?ఎ ?ె) (?ఐ ?ై)
     nil (?ఓ ?ో) (?ఒ ?ొ) (?ఔ ?ౌ) (?ౠ ?ౄ) (?ౡ nil))
    (;; CONSONANTS
     ?క ?ఖ ?గ ?ఘ ?ఙ                  ;; GUTTRULS
     ?చ ?ఛ ?జ ?ఝ ?ఞ                  ;; PALATALS
     ?ట ?ఠ ?డ ?ఢ ?ణ                  ;; CEREBRALS
     ?త ?థ ?ద ?ధ ?న nil              ;; DENTALS
     ?ప ?ఫ ?బ ?భ ?మ                  ;; LABIALS
     ?య ?ర ?ఱ ?ల ?ళ nil ?వ          ;; SEMIVOWELS
     ?శ ?ష ?స ?హ                    ;; SIBILANTS
     nil nil nil nil nil nil nil nil      ;; NUKTAS
     "జ్ఞ" "క్ష")
    (;; Misc Symbols
     ?ఁ ?ం ?ః nil ?్ nil nil)
    (;; Digits
     ?౦ ?౧ ?౨ ?౩ ?౪ ?౫ ?౬ ?౭ ?౮ ?౯)
    (;; Inscript-extra (4)  (#, $, ^, *, ])
     "్ర" "ర్" "త్ర" "శ్ర" nil)))

(defvar indian-knd-base-table
  '(
    (;; VOWELS
     (?ಅ nil) (?ಆ ?ಾ) (?ಇ ?ಿ) (?ಈ ?ೀ) (?ಉ ?ು) (?ಊ ?ೂ)
     (?ಋ ?ೃ) (?ಌ nil) nil (?ಏ ?ೇ) (?ಎ ?ೆ) (?ಐ ?ೈ)
     nil (?ಓ ?ೋ) (?ಒ ?ೊ) (?ಔ ?ೌ) (?ೠ ?ೄ) (?ೡ nil))
    (;; CONSONANTS
     ?ಕ ?ಖ ?ಗ ?ಘ ?ಙ                  ;; GUTTRULS
     ?ಚ ?ಛ ?ಜ ?ಝ ?ಞ                  ;; PALATALS
     ?ಟ ?ಠ ?ಡ ?ಢ ?ಣ                  ;; CEREBRALS
     ?ತ ?ಥ ?ದ ?ಧ ?ನ nil              ;; DENTALS
     ?ಪ ?ಫ ?ಬ ?ಭ ?ಮ                  ;; LABIALS
     ?ಯ ?ರ ?ಱ ?ಲ ?ಳ nil ?ವ          ;; SEMIVOWELS
     ?ಶ ?ಷ ?ಸ ?ಹ                    ;; SIBILANTS
     nil nil nil nil nil nil ?ೞ nil      ;; NUKTAS
     "ಜ್ಞ" "ಕ್ಷ")
    (;; Misc Symbols
     nil ?ಂ ?ಃ nil ?್ nil nil)
    (;; Digits
     ?೦ ?೧ ?೨ ?೩ ?೪ ?೫ ?೬ ?೭ ?೮ ?೯)
    (;; Inscript-extra (4)  (#, $, ^, *, ])
     "್ರ" "ರ್" "ತ್ರ" "ಶ್ರ" nil)))

(defvar indian-mlm-base-table
  '(
    (;; VOWELS
     (?അ nil) (?ആ ?ാ) (?ഇ ?ി) (?ഈ ?ീ) (?ഉ ?ു) (?ഊ ?ൂ)
     (?ഋ ?ൃ) (?ഌ ?ൢ) (?ൡ ?ൣ) (?ഏ ?േ) (?എ ?െ) (?ഐ ?ൈ)
     nil (?ഒ ?ൊ) (?ഓ ?ോ) (?ഔ ?ൗ) (?് ?്) (?ൠ ?ൄ))
    (;; CONSONANTS
     ?ക ?ഖ ?ഗ ?ഘ ?ങ                  ;; GUTTRULS
     ?ച ?ഛ ?ജ ?ഝ ?ഞ                  ;; PALATALS
     ?ട ?ഠ ?ഡ ?ഢ ?ണ                  ;; CEREBRALS
     ?ത ?ഥ ?ദ ?ധ ?ന nil              ;; DENTALS
     ?പ ?ഫ ?ബ ?ഭ ?മ                  ;; LABIALS
     ?യ ?ര ?റ ?ല ?ള ?ഴ ?വ          ;; SEMIVOWELS
     ?ശ ?ഷ ?സ ?ഹ                    ;; SIBILANTS
     nil nil nil nil nil nil nil nil      ;; NUKTAS
     "ജ്ഞ" "ക്ഷ"
     "റ്റ" "ന്റ" "ത്ത" "ത്ഥ" "ഞ്ഞ" "ങ്ങ" "ന്ന"
     "ഞ്ച" "ന്ക" "ങ്ക" "ച്ച" "ച്ഛ" "ക്ക"
     "ബ്ബ" "ക്ക" "ഗ്ഗ" "ജ്ജ" "മ്മ" "പ്പ" "വ്വ" "ക്സ" "ശ്ശ")
    (;; Misc Symbols
     nil ?ം ?ഃ nil ?് nil nil)
    (;; Digits
     ?൦ ?൧ ?൨ ?൩ ?൪ ?൫ ?൬ ?൭ ?൮ ?൯)
    (;; Chillus
     "ണ്" ?ൺ "ന്" ?ൻ "ര്" ?ർ "ല്" ?ൽ "ള്" ?ൾ)))

(defvar indian-tml-base-table
  '(
    (;; VOWELS
     (?அ nil) (?ஆ ?ா) (?இ ?ி) (?ஈ ?ீ) (?உ ?ு) (?ஊ ?ூ)
     nil nil nil (?ஏ ?ே) (?எ ?ெ) (?ஐ ?ை)
     nil (?ஓ ?ோ) (?ஒ ?ொ) (?ஔ ?ௌ) nil nil)
    (;; CONSONANTS
     ?க nil nil nil ?ங                  ;; GUTTRULS
     ?ச nil ?ஜ nil ?ஞ                  ;; PALATALS
     ?ட nil nil nil ?ண                  ;; CEREBRALS
     ?த nil nil nil ?ந ?ன              ;; DENTALS
     ?ப nil nil nil ?ம                  ;; LABIALS
     ?ய ?ர ?ற ?ல ?ள ?ழ ?வ          ;; SEMIVOWELS
     nil ?ஷ ?ஸ ?ஹ                    ;; SIBILANTS
     nil nil nil nil nil nil nil nil      ;; NUKTAS
     "ஜ்ஞ" "க்ஷ")
    (;; Misc Symbols
     nil ?ஂ ?ஃ nil ?் nil nil)
    (;; Digits
     ?௦ ?௧ ?௨ ?௩ ?௪ ?௫ ?௬ ?௭ ?௮ ?௯)
    (;; Inscript-extra (4)  (#, $, ^, *, ])
     "்ர" "ர்" "த்ர" nil nil)))

(defvar indian-base-table-to-language-alist
  '((indian-dev-base-table . "Devanagari")
    (indian-pnj-base-table . "Punjabi")
    (indian-ori-base-table . "Oriya")
    (indian-bng-base-table . "Bengali")
    (indian-asm-base-table . "Assamese")
    (indian-tlg-base-table . "Telugu")
    (indian-knd-base-table . "Kannada")
    (indian-mlm-base-table . "Malayalam")
    (indian-tml-base-table . "Tamil")))

(defvar indian-itrans-v5-table
  '(;; for encode/decode
    (;; vowels -- 18
     "a" ("aa" "A") "i" ("ii" "I") "u" ("uu" "U")
     ("RRi" "R^i") ("LLi" "L^i") (".c" "e.c") "E" "e" "ai"
     "o.c"  "O"   "o"   "au"  ("RRI" "R^I") ("LLI" "L^I"))
    (;; consonants -- 40
     "k"   "kh"  "g"   "gh"  ("~N" "N^")
     "ch" ("Ch" "chh") "j" "jh" ("~n" "JN")
     "T"   "Th"  "D"   "Dh"  "N"
     "t"   "th"  "d"   "dh"  "n"   "nh"
     "p"   "ph"  "b"   "bh"  "m"
     "y"   "r"   "rh"  "l"   ("L" "ld") nil  ("v" "w")
     "sh" ("Sh" "shh") "s" "h"
     "q" "K" "G" ("J" "z") ".D" ".Dh" "f" ("Y" "yh")
     ("GY" "dny") "x")
    (;; misc -- 7
     ".N" (".n" "M") "H" ".a" ".h" ("AUM" "OM") "..")))

(defvar indian-itrans-v5-table-for-tamil
  '(;; for encode/decode
    (;; vowels -- 18
     "a" ("aa" "A") "i" ("ii" "I") "u" ("uu" "U")
     ("RRi" "R^i") ("LLi" "L^i") (".c" "e.c") "E" "e" "ai"
     "o.c"  "O"   "o"   "au"  ("RRI" "R^I") ("LLI" "L^I"))
    (;; consonants -- 40
     "k"   "kh"  "g"   "gh"  ("~N" "N^")
     "ch" ("Ch" "chh") "j" "jh" ("~n" "JN")
     "T"   "Th"  "D"   "Dh"  "N"
     "t"   "th"  "d"   "dh"  "n"   "nh"
     "p"   "ph"  "b"   "bh"  "m"
     "y"   "r"   "rh"  "l"   ("L" "ld") ("J" "z")  ("v" "w")
     "sh" ("Sh" "shh") "s" "h"
     "q" "K" "G" nil ".D" ".Dh" "f" ("Y" "yh")
     ("GY" "dny") "x")
    (;; misc -- 7
     ".N" (".n" "M") "H" ".a" ".h" ("AUM" "OM") "..")))

(defvar indian-mlm-mozhi-table
  '(;; for encode/decode
    (;; vowels -- 18
     "a" ("aa" "A") "i" ("ii" "I") "u" ("uu" "U")
     "R" "Ll" "Lll" ("E" "ae") "e" "ai"
     nil  "o"   "O"   "au"  "~" "RR")
    (;; consonants -- 40
     ("k" "c")   "kh"  "g"   "gh"  "ng"
     "ch" ("Ch" "chh") "j" "jh" "nj"
     "T"   "Th"  "D"   "Dh"  "N"
     "th"  "thh" "d"   "dh"  "n"   nil
     "p"   ("ph" "f")  "b"   "bh"  "m"
     "y"   "r"   "rr"  "l"  "L" "zh" ("v" "w")
     ("S" "z") "sh" "s" "h"
     nil nil nil nil nil nil nil nil
     nil "X"
     ;; some of these are extra to Mozhi
     ("t" "tt") "nt" "tth" "tthh" "nnj" "nng" "nn"
     "nch" "nc" "nk" "cch" "cchh" "cc"
     "B" ("C" "K" "q") "G" "J" "M" "P" "V" "x" "Z")
    (;; misc -- 7
     nil nil "H")))

(defvar indian-kyoto-harvard-table
  '(;; for encode/decode
    (;; vowel
     "a"   ("A" "aa")  "i"   ("I" "ii")  "u"   ("U" "uu")
     "R"   ("L" "lR")  nil   nil   "e"   "ai"
     nil   nil   "o"   "au"  ("q" "RR" "Q")   ("E" "LL" "lRR"))
    (;; consonant
     "k"   "kh"  "g"   "gh"  "G"
     "c"   "ch"  "j"   "jh"  "J"
     "T"   "Th"  "D"   "Dh"  "N"
     "t"   "th"  "d"   "dh"  "n"   nil
     "p"   "ph"  "b"   "bh"  "m"
     "y"   "r"   nil   "l"   "L"   nil   "v"
     ("z" "Z")   "S"   "s"   "h"
     nil   nil   nil   nil   nil   nil   nil   nil
     nil   nil)
    (;; misc
     nil   "M"   "H"   "'"   nil   "." nil)))

(defvar indian-harvard-table
  '(;; for encode/decode
    (;; vowel
     "a"   ("A" "aa")  "i"   ("I" "ii")  "u"   ("U" "uu")
     "R"   ("L" "lR")  nil   nil   "e"   "ai"
     nil   nil   "o"   "au"  ("RR" "q" "Q")   ("LL" "E" "lRR"))
    (;; consonant
     "k"   "kh"  "g"   "gh"  "G"
     "c"   "ch"  "j"   "jh"  "J"
     "T"   "Th"  "D"   "Dh"  "N"
     "t"   "th"  "d"   "dh"  "n"   nil
     "p"   "ph"  "b"   "bh"  "m"
     "y"   "r"   nil   "l"   "L"   nil   "v"
     ("z" "Z")   "S"   "s"   "h"
     nil   nil   nil   nil   nil   nil   nil   nil
     nil   nil)
    (;; misc
     nil   "M"   "H"   "'"   nil   "." nil)))

(defvar indian-tokyo-table
  '(;; for encode/decode
    (;; vowel
     "a"   ("A" "aa")  "i"   ("I" "ii")  "u"   ("U" "uu")
     "R"   ("L" "lR")  nil   nil   "e"   "ai"
     nil   nil   "o"   "au"  ("Q" "RR" "q")   ("E" "LL" "lRR"))
    (;; consonant
     "k"   "kh"  "g"   "gh"  "G"
     "c"   "ch"  "j"   "jh"  "J"
     "T"   "Th"  "D"   "Dh"  "N"
     "t"   "th"  "d"   "dh"  "n"   nil
     "p"   "ph"  "b"   "bh"  "m"
     "y"   "r"   nil   "l"   "L"   nil   "v"
     ("Z" "z")   "S"   "s"   "h"
     nil   nil   nil   nil   nil   nil   nil   nil
     nil   nil)
    (;; misc
     nil   "M"   "H"   "'"   nil   "." nil)))

(defvar indian-aiba-table
  '(;; for encode/decode
    (;; vowel
     "a"   "aa"  "i"   "ii"  "u"   "uu"
     ".r"  ".l"   nil   nil  "e"   "ai"
     nil   nil   "o"   "au"  "~r"  "~l")
    (;; consonant
     "k"   "kh"  "g"   "gh"  "^n"
     "c"   "ch"  "j"   "jh"  "~n"
     ".t"  ".th" ".d"  ".dh" ".n"
     "t"   "th"  "d"   "dh"  "n"   nil
     "p"   "ph"  "b"   "bh"  "m"
     "y"   "r"   nil   "l"   nil  nil  "v"
     "^s"  ".s"  "s"   "h"
     nil   nil   nil   nil   nil   nil   nil   nil
     nil   nil)
    (;; misc
     nil   ".m"  ".h"  "'"   nil   "." nil)))

(defun combinatorial (head &rest tail)
  (if tail
      (apply 'append
	     (mapcar (lambda (y) (mapcar (lambda (x) (cons x y)) head))
		     (apply 'combinatorial tail)))
    (mapcar 'list head)))

(defun indian--puthash-char (char trans-char hashtbls)
  (let ((encode-hash (car hashtbls))  ;; char -> trans
	(decode-hash (cdr hashtbls))  ;; trans -> char
	)
    ;; char -- nil / char / string (/ list of vowel & matra)
    ;; trans-char -- nil / string / list of strings
    (when (and char trans-char)
      (if (stringp trans-char) (setq trans-char (list trans-char)))
      (if (characterp char) (setq char (char-to-string char)))
      (puthash char (car trans-char) encode-hash)
      (dolist (trans trans-char)
	 (puthash trans char decode-hash)))))

(defun indian--map (f l1 l2)
  (while l1
    (funcall f (pop l1) (pop l2))))

(defun indian--puthash-v (v trans-v hashtbls)
  (indian--map
   (lambda (v trans-v)
     (indian--puthash-char (car v) trans-v hashtbls))
   v trans-v))

(defun indian--puthash-c (c trans-c halant hashtbls)
  (indian--map
   (lambda (c trans-c)
     (if (characterp c) (setq c (char-to-string c)))
     (indian--puthash-char (concat c halant) trans-c hashtbls))
   c trans-c))

(defun indian--puthash-m (m trans-m hashtbls)
  (indian--map
   (lambda (m trans-m)
     (indian--puthash-char m trans-m hashtbls))
   m trans-m))

(defun indian--puthash-cv (c trans-c v trans-v hashtbls)
  (indian--map
   (lambda (c trans-c)
     (indian--map
      (lambda (v trans-v)
	(when (and c trans-c  v trans-v)
	  (if (characterp c) (setq c (char-to-string c)))
	  (setq v (if (characterp (cadr v)) (char-to-string (cadr v)) ""))
	  (if (stringp trans-c) (setq trans-c (list trans-c)))
	  (if (stringp trans-v) (setq trans-v (list trans-v)))
	  (indian--puthash-char
	   (concat c v)
	   (mapcar (lambda (x) (apply 'concat x))
		  (combinatorial trans-c trans-v))
	   hashtbls)))
      v trans-v))
   c trans-c))

(defun indian-make-hash (table trans-table)
  "Indian Transliteration Hash for decode/encode"
  (let* ((encode-hash (make-hash-table :test 'equal))
	 (decode-hash (make-hash-table :test 'equal))
	 (hashtbls (cons encode-hash decode-hash))
	 (vowels     (elt table 0))
	 (consonants (elt table 1))
	 (misc       (elt table 2))
	 (digits     (elt table 3))
	 (halant     (char-to-string (elt misc  4)))
	 (trans-vowels     (elt trans-table 0))
	 (trans-consonants (elt trans-table 1))
	 (trans-misc       (elt trans-table 2))
	 (trans-digits  '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9")))
    (indian--puthash-v vowels trans-vowels hashtbls)
    (indian--puthash-c consonants trans-consonants halant hashtbls)
    (indian--puthash-cv consonants trans-consonants
			      vowels trans-vowels hashtbls)
    (indian--puthash-m misc trans-misc hashtbls)
    (indian--puthash-m digits trans-digits hashtbls)
    hashtbls))

(defvar indian-dev-itrans-v5-hash
  (indian-make-hash indian-dev-base-table
			  indian-itrans-v5-table))
(defvar indian-dev-kyoto-harvard-hash
  (indian-make-hash indian-dev-base-table
			  indian-kyoto-harvard-table))
(defvar indian-dev-aiba-hash
  (indian-make-hash indian-dev-base-table
			  indian-aiba-table))

(defvar indian-pnj-itrans-v5-hash
  (indian-make-hash indian-pnj-base-table
			  indian-itrans-v5-table))

(defvar indian-gjr-itrans-v5-hash
  (indian-make-hash indian-gjr-base-table
			  indian-itrans-v5-table))

(defvar indian-ori-itrans-v5-hash
  (indian-make-hash indian-ori-base-table
			  indian-itrans-v5-table))

(defvar indian-bng-itrans-v5-hash
  (indian-make-hash indian-bng-base-table
			  indian-itrans-v5-table))

(defvar indian-asm-itrans-v5-hash
  (indian-make-hash indian-asm-base-table
			  indian-itrans-v5-table))

(defvar indian-tlg-itrans-v5-hash
  (indian-make-hash indian-tlg-base-table
			  indian-itrans-v5-table))

(defvar indian-knd-itrans-v5-hash
  (indian-make-hash indian-knd-base-table
			  indian-itrans-v5-table))

(defvar indian-mlm-itrans-v5-hash
  (indian-make-hash indian-mlm-base-table
			  indian-itrans-v5-table))

(defvar indian-mlm-mozhi-hash
  (indian-make-hash indian-mlm-base-table
			  indian-mlm-mozhi-table))

(defvar indian-tml-itrans-v5-hash
  (indian-make-hash indian-tml-base-table
			  indian-itrans-v5-table-for-tamil))
)

(defmacro indian-translate-region (from to hashtable encode-p)
  `(save-excursion
     (save-restriction
       (let ((regexp ,(indian-regexp-of-hashtbl-keys
		       (if encode-p (car (eval hashtable))
			 (cdr (eval hashtable))))))
	 (narrow-to-region from to)
	 (goto-char (point-min))
	 (while (re-search-forward regexp nil t)
	   (let ((matchstr (gethash (match-string 0)
				    (if ,encode-p
					(car ,hashtable)
				      (cdr ,hashtable)))))
	     (if matchstr (replace-match matchstr))))))))

;;;

(defun indian-dev-itrans-v5-encode-region (from to)
  (interactive "r")
  (indian-translate-region
   from to indian-dev-itrans-v5-hash t))

(defun indian-dev-itrans-v5-decode-region (from to)
  (interactive "r")
  (indian-translate-region
   from to indian-dev-itrans-v5-hash nil))

(defun indian-dev-kyoto-harvard-encode-region (from to)
  (interactive "r")
  (indian-translate-region
   from to indian-dev-kyoto-harvard-hash t))

(defun indian-dev-kyoto-harvard-decode-region (from to)
  (interactive "r")
  (indian-translate-region
   from to indian-dev-kyoto-harvard-hash nil))

(defun indian-dev-aiba-encode-region (from to)
  (interactive "r")
  (indian-translate-region
   from to indian-dev-aiba-hash t))

(defun indian-dev-aiba-decode-region (from to)
  (interactive "r")
  (indian-translate-region
   from to indian-dev-aiba-hash nil))




;;; IS 13194 utilities

;; The followings provide conversion between IS 13194 (ISCII) and UCS.

(let
    ;;Unicode vs IS13194  ;; only Devanagari is supported now.
    ((ucs-devanagari-to-is13194-alist
      '((?\x0900 . "[U+0900]")
	(?\x0901 . "����")
	(?\x0902 . "����")
	(?\x0903 . "����")
	(?\x0904 . "[U+0904]")
	(?\x0905 . "����")
	(?\x0906 . "����")
	(?\x0907 . "����")
	(?\x0908 . "����")
	(?\x0909 . "����")
	(?\x090a . "����")
	(?\x090b . "����")
	(?\x090c . "��������")
	(?\x090d . "����")
	(?\x090e . "����")
	(?\x090f . "����")
	(?\x0910 . "����")
	(?\x0911 . "����")
	(?\x0912 . "����")
	(?\x0913 . "����")
	(?\x0914 . "����")
	(?\x0915 . "����")
	(?\x0916 . "����")
	(?\x0917 . "����")
	(?\x0918 . "����")
	(?\x0919 . "����")
	(?\x091a . "����")
	(?\x091b . "����")
	(?\x091c . "����")
	(?\x091d . "����")
	(?\x091e . "����")
	(?\x091f . "����")
	(?\x0920 . "����")
	(?\x0921 . "����")
	(?\x0922 . "����")
	(?\x0923 . "����")
	(?\x0924 . "����")
	(?\x0925 . "����")
	(?\x0926 . "����")
	(?\x0927 . "����")
	(?\x0928 . "����")
	(?\x0929 . "����")
	(?\x092a . "����")
	(?\x092b . "����")
	(?\x092c . "����")
	(?\x092d . "����")
	(?\x092e . "����")
	(?\x092f . "����")
	(?\x0930 . "����")
	(?\x0931 . "����")
	(?\x0932 . "����")
	(?\x0933 . "����")
	(?\x0934 . "����")
	(?\x0935 . "����")
	(?\x0936 . "����")
	(?\x0937 . "����")
	(?\x0938 . "����")
	(?\x0939 . "����")
	(?\x093a . "[U+093a]")
	(?\x093b . "[U+093b]")
	(?\x093c . "����")
	(?\x093d . "��������")
	(?\x093e . "����")
	(?\x093f . "����")
	(?\x0940 . "����")
	(?\x0941 . "����")
	(?\x0942 . "����")
	(?\x0943 . "����")
	(?\x0944 . "��������")
	(?\x0945 . "����")
	(?\x0946 . "����")
	(?\x0947 . "����")
	(?\x0948 . "����")
	(?\x0949 . "����")
	(?\x094a . "����")
	(?\x094b . "����")
	(?\x094c . "����")
	(?\x094d . "����")
	(?\x094e . "[U+094e]")
	(?\x094f . "[U+094f]")
	(?\x0950 . "��������")
	(?\x0951 . "��������")
	(?\x0952 . "��������")
	(?\x0953 . "[DEVANAGARI GRAVE ACCENT]")
	(?\x0954 . "[DEVANAGARI ACUTE ACCENT]")
	(?\x0955 . "[U+0955]")
	(?\x0956 . "[U+0956]")
	(?\x0957 . "[U+0957]")
	(?\x0958 . "��������")
	(?\x0959 . "��������")
	(?\x095a . "��������")
	(?\x095b . "��������")
	(?\x095c . "��������")
	(?\x095d . "��������")
	(?\x095e . "��������")
	(?\x095f . "����")
	(?\x0960 . "��������")
	(?\x0961 . "��������")
	(?\x0962 . "��������")
	(?\x0963 . "��������")
	(?\x0964 . "����")
	(?\x0965 . "��������")
	(?\x0966 . "����")
	(?\x0967 . "����")
	(?\x0968 . "����")
	(?\x0969 . "����")
	(?\x096a . "����")
	(?\x096b . "����")
	(?\x096c . "����")
	(?\x096d . "����")
	(?\x096e . "����")
	(?\x096f . "����")
	(?\x0970 . "[U+0970]")
	(?\x0971 . "[U+0971]")
	(?\x0972 . "[U+0972]")
	(?\x0973 . "[U+0973]")
	(?\x0974 . "[U+0974]")
	(?\x0975 . "[U+0975]")
	(?\x0976 . "[U+0976]")
	(?\x0977 . "[U+0977]")
	(?\x0978 . "[U+0978]")
	(?\x0979 . "[U+0979]")
	(?\x097a . "[U+097a]")
	(?\x097b . "[U+097b]")
	(?\x097c . "[U+097c]")
	(?\x097d . "[U+097d]")
	(?\x097e . "[U+097e]")
	(?\x097f . "[U+097f]")))
     (ucs-bengali-to-is13194-alist nil)
     (ucs-assamese-to-is13194-alist nil)
     (ucs-gurmukhi-to-is13194-alist nil)
     (ucs-gujarati-to-is13194-alist nil)
     (ucs-oriya-to-is13194-alist nil)
     (ucs-tamil-to-is13194-alist nil)
     (ucs-telugu-to-is13194-alist nil)
     (ucs-malayalam-to-is13194-alist nil)
     (ucs-kannada-to-is13194-alist nil))
  (dolist (script '(devanagari bengali assamese gurmukhi gujarati
		    oriya tamil telugu malayalam kannada))
   (let ((hashtable (intern (concat "is13194-to-ucs-"
				    (symbol-name script) "-hashtbl" )))
	 (regexp    (intern (concat "is13194-to-ucs-"
				    (symbol-name script) "-regexp"))))
     (set hashtable (make-hash-table :test 'equal :size 128))
     (dolist (x (eval (intern (concat "ucs-" (symbol-name script)
				      "-to-is13194-alist"))))
       (put-char-code-property (car x) 'script script)
       (put-char-code-property (car x) 'iscii (cdr x))
       (puthash (cdr x) (char-to-string (car x)) (eval hashtable)))
      (set regexp (indian-regexp-of-hashtbl-keys (eval hashtable))))))

(defvar is13194-default-repertory 'devanagari)

(defvar is13194-repertory-to-ucs-script
  `((DEF ?\x40 ,is13194-default-repertory)
    (RMN ?\x41 ,is13194-default-repertory)
    (DEV ?\x42 devanagari)
    (BNG ?\x43 bengali)
    (TML ?\x44 tamil)
    (TLG ?\x45 telugu)
    (ASM ?\x46 bengali)
    (ORI ?\x47 oriya)
    (KND ?\x48 kannada)
    (MLM ?\x49 malayalam)
    (GJR ?\x4a gujarati)
    (PNJ ?\x4b gurmukhi)))

;; for guiding find-variable function.
(defvar is13194-to-ucs-devanagari-hashtbl nil)
(defvar is13194-to-ucs-devanagari-regexp nil)
(defvar is13194-to-ucs-bengali-hashtbl nil)
(defvar is13194-to-ucs-bengali-regexp nil)
(defvar is13194-to-ucs-assamese-hashtbl nil)
(defvar is13194-to-ucs-assamese-regexp nil)
(defvar is13194-to-ucs-gurmukhi-hashtbl nil)
(defvar is13194-to-ucs-gurmukhi-regexp nil)
(defvar is13194-to-ucs-gujarati-hashtbl nil)
(defvar is13194-to-ucs-gujarati-regexp nil)
(defvar is13194-to-ucs-oriya-hashtbl nil)
(defvar is13194-to-ucs-oriya-regexp nil)
(defvar is13194-to-ucs-tamil-hashtbl nil)
(defvar is13194-to-ucs-tamil-regexp nil)
(defvar is13194-to-ucs-telugu-hashtbl nil)
(defvar is13194-to-ucs-telugu-regexp nil)
(defvar is13194-to-ucs-malayalam-hashtbl nil)
(defvar is13194-to-ucs-malayalam-regexp nil)
(defvar is13194-to-ucs-kannada-hashtbl nil)
(defvar is13194-to-ucs-kannada-regexp nil)

(defvar indian-ucs-to-is13194-regexp
  ;; only Devanagari is supported now.
  (concat "[" (char-to-string #x0900)
          "-" (char-to-string #x097f) "]")
  "Regexp that matches to conversion")

(defun indian-ucs-to-iscii-region (from to)
  "Converts the indian UCS characters in the region to ISCII.
Returns new end position."
  (interactive "r")
  ;; only Devanagari is supported now.
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char (point-min))
      (let* ((current-repertory is13194-default-repertory))
	(while (re-search-forward indian-ucs-to-is13194-regexp nil t)
	  (replace-match
	   (get-char-code-property (string-to-char (match-string 0))
				   'iscii))))
      (point-max))))

(defun indian-iscii-to-ucs-region (from to)
  "Converts the ISCII characters in the region to UCS.
Returns new end position."
  (interactive "r")
  ;; only Devanagari is supported now.
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char (point-min))
      (let* ((current-repertory is13194-default-repertory)
	     (current-hashtable
	      (intern (concat "is13194-to-ucs-"
			      (symbol-name current-repertory) "-hashtbl")))
	     (current-regexp
	      (intern (concat "is13194-to-ucs-"
			      (symbol-name current-repertory) "-regexp")))
	     (re (eval current-regexp))
	     (hash (eval current-hashtable)))
	(while (re-search-forward re nil t)
	  (replace-match (gethash (match-string 0) hash ""))))
      (point-max))))

;;;###autoload
(defun indian-compose-region (from to)
  "Compose the region according to `composition-function-table'."
  (interactive "r")
  (save-excursion
    (save-restriction
      (let ((pos from) newpos func (max to))
	(narrow-to-region from to)
	(while (< pos max)
          ;; FIXME: The below seems to assume
          ;; composition-function-table holds functions?  That is no
          ;; longer true, since long ago.
	  (setq func (aref composition-function-table (char-after pos)))
	  (if (fboundp func)
	      (setq newpos (funcall func pos nil)
		    pos (if (and (integerp newpos) (> newpos pos))
			    newpos (1+ pos)))
	    (setq pos (1+ pos))))))))

;;;###autoload
(defun indian-compose-string (string)
  (with-temp-buffer
    (insert string)
    (indian-compose-region (point-min) (point-max))
    (buffer-string)))

;;;###autoload
(defun in-is13194-post-read-conversion (len)
  (let ((pos (point)) endpos)
    (setq endpos (indian-iscii-to-ucs-region pos (+ pos len)))
    (- endpos pos)))

;;;###autoload
(defun in-is13194-pre-write-conversion (from to)
  (let ((buf (current-buffer)))
    (set-buffer (generate-new-buffer " *temp*"))
    (if (stringp from)
	(insert from)
      (insert-buffer-substring buf from to))
    (indian-ucs-to-iscii-region (point-min) (point-max))
    nil))




;;; Backward Compatibility support programs

;; The following provides the conversion from old-implementation of
;; Emacs Devanagari script to UCS.

(defconst indian-2-colum-to-ucs
  '(
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2120   ������������������������������������������������������������
  ("����" . "ँ")
  ("����" . "ं")
  ("����" . "ः")
  ("����" . "अ")
  ("����" . "आ")
  ("����" . "इ")
  ("����" . "ई")
  ("����" . "उ")
  ("����" . "ऊ")
  ("����" . "ऋ")
  ("��������" . "रृ")
  ("����" . "ऎ")
  ("����" . "ए")
  ("����" . "ऐ")
  ("����" . "ऍ")
  ("����" . "ऒ")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2130 ����������������������������������������������������������������
  ("����" . "ओ")
  ("����" . "औ")
  ("����" . "ऑ")
  ("����" . "क")
  ("����" . "ख")
  ("����" . "ग")
  ("����" . "घ")
  ("����" . "ङ")
  ("����" . "च")
  ("����" . "छ")
  ("����" . "ज")
  ("����" . "झ")
  ("����" . "ञ")
  ("����" . "ट")
  ("����" . "ठ")
  ("����" . "ड")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2140 ����������������������������������������������������������������
  ("����" . "ढ")
  ("����" . "ण")
  ("����" . "त")
  ("����" . "थ")
  ("����" . "द")
  ("����" . "ध")
  ("����" . "न")
  ("����" . "ऩ")
  ("����" . "प")
  ("����" . "फ")
  ("����" . "ब")
  ("����" . "भ")
  ("����" . "म")
  ("����" . "य")
  ("����" . "य़")
  ("����" . "र")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2150 ����������������������������������������������������������������
  ("����" . "ऱ")
  ("����" . "ल")
  ("����" . "ळ")
  ("����" . "ऴ")
  ("����" . "व")
  ("����" . "श")
  ("����" . "ष")
  ("����" . "स")
  ("����" . "ह")
  ("����" . "ा")
  ("����" . "ि")
  ("����" . "ी")
  ("����" . "ु")
  ("����" . "ू")
  ("����" . "ृ")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2160 ����������������������������������������������������������������
  ("����" . "ॆ")
  ("����" . "े")
  ("����" . "ै")
  ("����" . "ॅ")
  ("����" . "ॊ")
  ("����" . "ो")
  ("����" . "ौ")
  ("����" . "ॉ")
  ("����" . "्")
  ("����" . "़")
  ("����" . "।")
  ("��������" . "॥")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2170 ������������������������������������������������������������
  ("����" . "०")
  ("����" . "१")
  ("����" . "२")
  ("����" . "३")
  ("����" . "४")
  ("����" . "५")
  ("����" . "६")
  ("����" . "७")
  ("����" . "८")
  ("����" . "९")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2220   ������������������������������������������������������������
  ("����" . "ज़्र")
  ("����" . "फ़्र")
  ("����" . "क्र")
  ("����" . "ग्र")
  ("����" . "त्र")
  ("����" . "प्र")
  ("����" . "फ्र")
  ("����" . "श्र")
  ("����" . "रु")
  ("����" . "रू")
  ("����" . "ऱु")
  ("����" . "ऱू")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2230 ����������������������������������������������������������������
  ("����" . "क्")
  ("����" . "ख्")
  ("����" . "ग्")
  ("����" . "घ्")
  ("����" . "च्")
  ("��������" . "च्र्")
  ("����" . "ज्")
  ("����" . "झ्")
  ("����" . "ञ्")
  ("����" . "ञ्")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2240 ����������������������������������������������������������������
  ("����" . "ण्")
  ("����" . "त्")
  ("����" . "थ्")
  ("����" . "ध्")
  ("����" . "न्")
  ("����" . "ऩ्")
  ("����" . "प्")
  ("����" . "फ्")
  ("����" . "ब्")
  ("����" . "ब्")
  ("����" . "भ्")
  ("����" . "म्")
  ("����" . "य्")
  ("����" . "य़्")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2250 ����������������������������������������������������������������
  ("����" . "ल्")
  ("����" . "ळ्")
  ("����" . "ऴ्")
  ("����" . "व्")
  ("����" . "श्")
  ("����" . "ष्")
  ("����" . "स्")
  ("����" . "्य")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2260 ����������������������������������������������������������������
  ("����" . "ग्र्")
  ("����" . "घ्न्")
  ("����" . "त्त्")
  ("����" . "त्र्")
  ("����" . "ध्न्")
  ("����" . "ध्र्")
  ("����" . "प्त्")
  ("����" . "श्च्")
  ("����" . "श्र्")
  ("����" . "श्व्")
  ("����" . "न्न्")
  ("����" . "क्ष्")
  ("����" . "ज्ञ्")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2270 ������������������������������������������������������������
  ("����" . "र्")
  ("����" . "्र")
  ("����" . "्र")
  ("����" . "क़्")
  ("����" . "ख़्")
  ("����" . "ग़्")
  ("����" . "फ़्")
  ("����" . "ज़्")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2320   ������������������������������������������������������������
  ("����" . "ॐ")
  ("����" . "ऌ")
  ("��������" . "रॄ")
  ("����" . "ॡ")
  ("��������" . "रॣ")
  ("����" . "ॠ")
  ("��������" . "रॢ")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2330 ����������������������������������������������������������������
  ("����" . "क़")
  ("����" . "ख़")
  ("����" . "ग़")
  ("����" . "ज़")
  ("����" . "ड़")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2340 ����������������������������������������������������������������
  ("����" . "ढ़")
  ("����" . "फ़")
  ("����" . "ऽ")
  ("����" . "ॄ")
  ("����" . "ॢ")
  ("����" . "ॣ")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2350 ����������������������������������������������������������������
  ("����" . "म्न")
  ("����" . "म्ल")
  ("����" . "हृ")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2360 ����������������������������������������������������������������
  ("����" . "ल्ल")
  ("����" . "व्न")
  ("����" . "व्व")
  ("����" . "श्च")
  ("����" . "श्न")
  ("����" . "श्ब")
  ("����" . "श्ल")
  ("����" . "श्व")
  ("����" . "ष्ट्र्य")
  ("����" . "ष्ट्य")
  ("����" . "ष्ट्व")
  ("����" . "ष्ट")
  ("����" . "ष्ठ")
  ("����" . "स्न")
  ("����" . "स्र")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2370 ������������������������������������������������������������
  ("����" . "ह्ण")
  ("����" . "ह्न")
  ("����" . "ह्म")
  ("����" . "ह्य")
  ("����" . "ह्र")
  ("����" . "ह्ल")
  ("����" . "ह्व")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2420   ������������������������������������������������������������
  ("����" . "क्त्र्य")
  ("����" . "क्त्व")
  ("����" . "क्त्य")
  ("����" . "क्न्य")
  ("����" . "क्र्य")
  ("����" . "क्व्य")
  ("����" . "क्क")
  ("����" . "क्त")
  ("����" . "क्न")
  ("����" . "क्म")
  ("����" . "क्य")
  ("����" . "क्ल")
  ("����" . "क्व")
  ("����" . "क्ष")
  ("����" . "घ्न")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2430 ����������������������������������������������������������������
  ("����" . "ङ्क्त्य")
  ("����" . "ङ्क्ष्व")
  ("����" . "ङ्क्त")
  ("����" . "ङ्क्ष")
  ("����" . "ङ्घ्र")
  ("����" . "ङ्क्य")
  ("����" . "ङ्ख्य")
  ("����" . "ङ्ग्य")
  ("����" . "ङ्घ्य")
  ("����" . "ङ्क")
  ("����" . "ङ्ख")
  ("����" . "ङ्ग")
  ("����" . "ङ्घ")
  ("����" . "ङ्ङ")
  ("����" . "ङ्न")
  ("����" . "ङ्म")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2440 ����������������������������������������������������������������
  ("����" . "ङ्य")
  ("����" . "च्च")
  ("����" . "च्ञ")
  ("����" . "छ्य")
  ("����" . "ज्र")
  ("����" . "ज्ञ")
  ("����" . "ञ्च")
  ("����" . "ञ्ज")
  ("����" . "ट्क")
  ("����" . "ट्ट")
  ("����" . "ट्ठ")
  ("����" . "ट्य")
  ("����" . "ठ्य")
  ("����" . "ड्ग्य")
  ("����" . "ड्घ्र")
  ("����" . "ड्र्य")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2450 ����������������������������������������������������������������
  ("����" . "ड्ग")
  ("����" . "ड्घ")
  ("����" . "ड्ड")
  ("����" . "ड्म")
  ("����" . "ड्य")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2460 ����������������������������������������������������������������
  ("����" . "ढ्य")
  ("����" . "त्त")
  ("����" . "त्न")
  ("����" . "द्द्य")
  ("����" . "द्ध्य")
  ("����" . "द्भ्य")
  ("����" . "द्र्य")
  ("����" . "द्व्य")
  ("����" . "द्ग्र")
  ("����" . "द्घ्र")
  ("����" . "द्द्व")
  ("����" . "द्ध्व")
  ("����" . "द्ग")
  ("����" . "द्घ")
  ("����" . "द्द")
  ("����" . "द्ध")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2470 ������������������������������������������������������������
  ("����" . "द्न")
  ("����" . "द्ब")
  ("����" . "द्भ")
  ("����" . "द्म")
  ("����" . "द्य")
  ("����" . "द्व")
  ("����" . "ध्न")
  ("����" . "न्न")
  ("����" . "प्त")
  ("����" . "प्न")
  ("����" . "प्ल")
  ("����" . "ब्न")
  ("����" . "ब्ब")
  ("����" . "ब्व")
  ("����" . "भ्न")))

(defconst indian-2-column-to-ucs-regexp
  "��������\\|��������\\|[����������������]����\\|[����-����]")

(put 'indian-2-column-to-ucs-chartable 'char-table-extra-slots 1)
(defconst indian-2-column-to-ucs-chartable
  (let ((table (make-char-table 'indian-2-column-to-ucs-chartable))
	(alist nil))
    (dolist (elt indian-2-colum-to-ucs)
      (if (= (length (car elt)) 1)
	  (aset table (aref (car elt) 0) (cdr elt))
	(setq alist (cons elt alist))))
    (set-char-table-extra-slot table 0 alist)
    table))

;;;###autoload
(defun indian-2-column-to-ucs-region (from to)
  "Convert old Emacs Devanagari characters to UCS."
  (interactive "r")
  (save-excursion
    (save-restriction
      (let ((pos from)
	    (alist (char-table-extra-slot indian-2-column-to-ucs-chartable 0)))
	(narrow-to-region from to)
	(decompose-region from to)
	(goto-char (point-min))
	(while (re-search-forward indian-2-column-to-ucs-regexp nil t)
	  (let ((len (- (match-end 0) (match-beginning 0)))
		subst)
	    (if (= len 1)
		(setq subst (aref indian-2-column-to-ucs-chartable
				  (char-after (match-beginning 0))))
	      (setq subst (cdr (assoc (match-string 0) alist))))
	    (replace-match (if subst subst "?"))))
	(indian-compose-region (point-min) (point-max))))))

(provide 'ind-util)

;;; ind-util.el ends here
