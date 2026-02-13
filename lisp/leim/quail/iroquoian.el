;;; iroquoian.el --- Quail packages for inputting Iroquoian languages  -*- lexical-binding: t; coding: utf-8; -*-

;; Copyright (C) 2024-2026 Free Software Foundation, Inc.

;; Author: Kierin Bell <fernseed@fernseed.me>
;; Keywords: i18n

;; This file is part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file implements input methods for Northern Iroquoian languages.

;; Input methods are implemented for the following Northern Iroquoian
;; languages:

;; - Mohawk (Kanien’kéha / Kanyen’kéha / Onkwehonwehnéha)
;; - Oneida (Onʌyote’a·ká· / Onyota’a:ká: / Ukwehuwehnéha)
;; - Onondaga (Onųdaʔgegáʔ)
;; - Cayuga (Gayogo̱ho:nǫhnéha:ˀ)
;; - Seneca (Onödowá’ga:’)
;; - Tuscarora (Skarù·ręʔ)

;; A composite input method for all of the languages above is also
;; defined: `haudenosaunee-postfix'.

;; Input methods are not yet implemented for the remaining Northern
;; Iroquoian languages, including:

;; - Wendat (Huron) / Wyandot

;;; Code:

(require 'quail)
(require 'seq)
(require 'pcase)


;;; Mohawk

;;
;; There are several orthographies used today to write Mohawk in
;; different communities, but differences are small and mainly involve
;; differences in representation of the palatal glide [j] (written <i>
;; in Eastern/Central dialects and <y> in Western dialects).  The
;; following input method should work for all of variants.
;;
;; Reference work for orthographies used by speakers of Eastern
;; (Kahnawà:ke, Kanehsatà:ke, Wáhta) and Central (Ahkwesahsne) dialects
;; of Mohawk:
;;
;; Lazore, Dorothy Karihwénhawe. 1993. The Mohawk language
;; Standardisation Project, Conference Report. Ontario: Literacy
;; Ontario.
;;
;; Reference work for the orthography commonly used by speakers of
;; Western dialects of Mohawk (Tyendinaga, Ohswé:ken):
;;
;; Brian Maracle. 2021. 1st Year Adult Immersion Program 2020-21.
;; Ohsweken, ON, Canada: Onkwawenna Kentyohkwa.  Unpublished curriculum
;; document written by staff for the Okwawenna Kentyohkwa adult
;; immersion program.
;;

(defconst iroquoian-mohawk-modifier-alist nil
  "Alist of rules for modifier letters in Mohawk input methods.
Entries are as with rules in `quail-define-rules'.")

(defconst iroquoian-mohawk-vowel-alist
  '(("a'" ?á)
    ("a`" ?à)
    ("A'" ?Á)
    ("A`" ?À)
    ("e'" ?é)
    ("e`" ?è)
    ("E'" ?É)
    ("E`" ?È)
    ("i'" ?í)
    ("i`" ?ì)
    ("I'" ?Í)
    ("I`" ?Ì)
    ("o'" ?ó)
    ("o`" ?ò)
    ("O'" ?Ó)
    ("O`" ?Ò)

    ("a''" ["a'"])
    ("a``" ["a`"])
    ("A''" ["A'"])
    ("A``" ["A`"])
    ("e''" ["e'"])
    ("e``" ["e`"])
    ("E''" ["E'"])
    ("E``" ["E`"])
    ("i''" ["i'"])
    ("i``" ["i`"])
    ("I''" ["I'"])
    ("I``" ["I`"])
    ("o''" ["o'"])
    ("o``" ["o`"])
    ("O''" ["O'"])
    ("O``" ["O`"]))
  "Alist of rules for vowel letters in Mohawk input methods.
Entries are as with rules in `quail-define-rules'.")

(defconst iroquoian-mohawk-consonant-alist
  '((";;" ?\N{RIGHT SINGLE QUOTATION MARK}))
  "Alist of rules for consonant letters in Mohawk input methods.
Entries are as with rules in `quail-define-rules'.")

(quail-define-package
 "mohawk-postfix" "Mohawk" "MOH<" t
 "Mohawk (Kanien’kéha/Kanyen’kéha) input method with postfix modifiers

Stress diacritics:

| Key  | Description  | Example |
|------+--------------+---------|
| \\='    | Acute accent | a' -> á |
| \\=`    | Grave accent | a` -> à |

Doubling any of these postfixes separates the letter and the postfix.

Vowels:

a, e, i, and o are bound to a single key.

Consonants:

| Key | Translation | Description  |
|-----+-------------+--------------|
| ;;  | \\=’           | Glottal stop |

h, k, n, r, s, t, w, and y are bound to a single key.

b, m, and p are used rarely in ideophones and loan words.  They are also
each bound to a single key.

All Haudenosaunee languages, including Mohawk, can be input
simultaneously using the input method `haudenosaunee-postfix'."
 nil t nil nil nil nil nil nil nil nil t)

(pcase-dolist (`(,key ,trans)
               (append iroquoian-mohawk-modifier-alist
                       iroquoian-mohawk-consonant-alist
                       iroquoian-mohawk-vowel-alist))
  (quail-defrule key trans))


;;; Oneida

;;
;; There are slight variations in the orthographies used today to write
;; Oneida.  The differences mainly involve in representation of vowel
;; length and glottal stops.
;;
;; Reference work for Oneida orthography:
;;
;; Michelson, K., Doxtator, M. and Doxtator, M.A.. 2002.
;; Oneida-English/English-Oneida dictionary. Toronto: University of
;; Toronto Press.
;;
;; Orthographic variation from personal familiarity with community
;; language programs and curricula.
;;

(defconst iroquoian-oneida-modifier-alist
  '(("::" ?\N{MIDDLE DOT}))
  "Alist of rules for modifier letters in Oneida input methods.
Entries are as with rules in `quail-define-rules'.")

(defconst iroquoian-oneida-vowel-alist
  '(("a'" ?á)
    ("A'" ?Á)
    ("e'" ?é)
    ("E'" ?É)
    ("i'" ?í)
    ("I'" ?Í)
    ("o'" ?ó)
    ("O'" ?Ó)
    ("u'" ?ú)
    ("U'" ?Ú)
    ("e/" ?ʌ)
    ("e/'" ["ʌ́"])
    ("E/" ?Ʌ)
    ("E/'" ["Ʌ́"])

    ("a''" ["a'"])
    ("A''" ["A'"])
    ("e''" ["e'"])
    ("E''" ["E'"])
    ("i''" ["i'"])
    ("I''" ["I'"])
    ("o''" ["o'"])
    ("O''" ["O'"])
    ("u''" ["u'"])
    ("U''" ["U'"])
    ("e//" ["e/"])
    ("e/''" ["ʌ'"])
    ("E//" ["E/"])
    ("E/''" ["Ʌ'"]))
  "Alist of rules for vowel letters in Oneida input methods.
Entries are as with rules in `quail-define-rules'.")

(defconst iroquoian-oneida-consonant-alist
  '((";;" ?\N{RIGHT SINGLE QUOTATION MARK})
    (";'" ?\N{MODIFIER LETTER GLOTTAL STOP}))
  "Alist of rules for consonant letters in Oneida input methods.
Entries are as with rules in `quail-define-rules'.")

(defconst iroquoian-oneida-devoicing-alist
  '(("_" ?\N{COMBINING LOW LINE})
    ("__" ?_))
  "Alist of rules for devoicing characters in Oneida input methods.
Entries are as with rules in `quail-define-rules'.")

(quail-define-package
 "oneida-postfix" "Oneida" "ONE<" t
 "Oneida (Onʌyote’a·ká·/Onyota’a:ká:) input method with postfix modifiers

Modifiers:

| Key | Translation | Description              |
|-----+-------------+--------------------------|
| ::  | ·           | Vowel length             |

Stress diacritics:

| Key  | Description  | Example |
|------+--------------+---------|
| \\='    | Acute accent | a' -> á |

Doubling the postfix separates the letter and the postfix.

Vowels:

| Key | Translation | Description                       |
|-----+-------------+-----------------------------------|
| e/  | ʌ           | Mid central nasal vowel           |
| E/  | Ʌ           | Mid central nasal vowel (capital) |

a, e, i, o, and u are bound to a single key.

Consonants:

| Key | Translation | Description              |
|-----+-------------+--------------------------|
| ;;  | \\=’           | Glottal stop             |
| ;\\='  | ˀ           | Glottal stop (alternate)  |

h, k, l, n, s, t, w, and y are bound to a single key.

Devoicing:

| Key | Description        | Example  |
|-----+--------------------+----------|
| _   | Combining low line | a_ -> a̲ |

Note: Not all fonts can properly display a combining low line on all
letters.

Underlining is commonly used in Oneida to indicate devoiced syllables on
pre-pausal forms (also called utterance-final forms).  Alternatively,
markup or other methods can be used to create an underlining effect.

To enter a plain underscore, type the underscore twice.

All Haudenosaunee languages, including Oneida, can be input
simultaneously using the input method `haudenosaunee-postfix'."
 nil t nil nil nil nil nil nil nil nil t)

(pcase-dolist (`(,key ,trans)
               (append iroquoian-oneida-modifier-alist
                       iroquoian-oneida-consonant-alist
                       iroquoian-oneida-vowel-alist
                       iroquoian-oneida-devoicing-alist))
  (quail-defrule key trans))


;;; Onondaga

;;
;; There are three main orthographies for Onondaga in contemporary use:
;; the community orthography used at Six Nations of the Grand River, the
;; community orthography used at Onondaga Nation in New York, and the
;; orthography used by Hanni Woodbury in her 2003 dictionary (see
;; below).  The latter is included because of its adoption in academia
;; and also by some contemporary second-language learners.
;; Additionally, Woodbury's dictionary provides a helpful description of
;; the community orthographies that is still applicable today.
;;
;; The differences between the orthographies are small, involving
;; representation of nasal vowels (ęand ǫat Six Nations of the Grand
;; River, eñ and oñ at Onondaga in New York, and ęand ųfollowing
;; Woodbury's dictionary), the low front rounded vowel (äat Six Nations
;; and Onondaga Nation and æ following Woodbury), vowel length (:
;; [colon] after a vowel in community orthographies and · [middle dot]
;; following Woodbury), and glottal stops (’ [right single quotation
;; mark] in community orthographies and ʔ [latin letter glottal stop]
;; following Woodbury).  The input method here aims to accommodate all
;; three of these orthographies.
;;
;; Reference work for Onondaga orthography:
;;
;; Hanni Woodbury. 2003. Onondaga-English/English-Onondaga
;; Dictionary. Toronto: University of Toronto Press.
;;

(defconst iroquoian-onondaga-modifier-alist
  '(("::" ?\N{MIDDLE DOT}))
  "Alist of rules for modifier letters in Onondaga input methods.
Entries are as with rules in `quail-define-rules'.")

(defconst iroquoian-onondaga-vowel-alist
  '(("a'" ?á)
    ("A'" ?Á)
    ("e'" ?é)
    ("E'" ?É)
    ("i'" ?í)
    ("I'" ?Í)
    ("o'" ?ó)
    ("O'" ?Ó)
    ("e," ?ę)
    ("e,'" ["ę́"])
    ("E," ?Ę)
    ("E,'" ["Ę́"])
    ("o," ?ǫ)
    ("o,'" ["ǫ́"])
    ("O," ?Ǫ)
    ("O,'" ["Ǫ́"])
    ("a\"" ?ä)
    ("a\"'" ["ä́"])
    ("A\"" ?Ä)
    ("A\"'" ["Ä́"])
    ;; From Woodbury (2003) orthography:
    ("a/" ?æ)
    ("a/'" ["ǽ"])
    ("A/" ?Æ)
    ("A/'" ["Ǽ"])
    ("u," ?ų)
    ("u,'" ["ų́"])
    ("U," ?Ų)
    ("U,'" ["Ų́"])

    ("a''" ["a'"])
    ("A''" ["A'"])
    ("e''" ["e'"])
    ("E''" ["E'"])
    ("i''" ["i'"])
    ("I''" ["I'"])
    ("o''" ["o'"])
    ("O''" ["O'"])
    ("e,," ["e,"])
    ("e,''" ["ę'"])
    ("E,," ["E,"])
    ("E,''" ["Ę'"])
    ("o,," ["o,"])
    ("o,''" ["ǫ'"])
    ("O,," ["O,"])
    ("O,''" ["Ǫ'"])
    ("a\"\"" ["a\""])
    ("a\"''" ["ä'"])
    ("A\"\"" ["A\""])
    ("A\"''" ["Ä'"])
    ("a//" ["a/"])
    ("a/''" ["æ'"])
    ("A//" ["A/"])
    ("A/''" ["Æ'"])
    ("u,," ["u,"])
    ("u,''" ["ų'"])
    ("U,," ["U,"])
    ("U,''" ["Ų'"]))
  "Alist of rules for vowel letters in Onondaga input methods.
Entries are as with rules in `quail-define-rules'.")

(defconst iroquoian-onondaga-consonant-alist
  '((";;" ?\N{RIGHT SINGLE QUOTATION MARK})
    (";:" ?\N{LATIN LETTER GLOTTAL STOP}))
  "Alist of rules for consonant letters in Onondaga input methods.
Entries are as with rules in `quail-define-rules'.")

(defconst iroquoian-onondaga-nasal-alist
  '(("n~" ?ñ)
    ("n-" ["ñ"])
    ("n--" ["n-"])
    ("N~" ?Ñ)
    ("N-" ["Ñ"])
    ("N--" ["N-"]))
  "Alist of rules for nasal modifier letters in Onondaga input methods.
Entries are as with rules in `quail-define-rules'.")

(quail-define-package
 "onondaga-postfix" "Onondaga" "ONO<" t
 "Onondaga (Onųdaʔgegáʔ) input method with postfix modifiers

Modifiers:

| Key | Translation | Description              |
|-----+-------------+--------------------------|
| ::  | ·           | Vowel length (alternate) |

Stress diacritics:

| Key  | Description  | Example |
|------+--------------+---------|
| \\='    | Acute accent | a' -> á |

Doubling the postfix separates the letter and the postfix.

Vowels:

| Key | Translation | Description                           |
|-----+-------------+---------------------------------------|
| Six Nations of the Grand River orthography                |
|-----------------------------------------------------------|
| e,  | ę | Mid front nasal vowel                           |
| E,  | Ę | Mid front nasal vowel (capital)                 |
| o,  | ǫ | Back high nasal vowel                           |
| O,  | Ǫ | Back high nasal vowel (capital)                 |
| a\"  | ä | Low front rounded vowel                         |
| A\"  | Ä | Low front rounded vowel (capital)               |
|-----------------------------------------------------------|
| Onondaga Nation, New York orthography                     |
|-----------------------------------------------------------|
| en~ | eñ | Mid front nasal vowel                          |
| en- | eñ | (same as above)                                |
| EN~ | EÑ | Mid front nasal vowel (capital)                |
| EN- | EÑ | (same as above)                                |
| on~ | oñ | Back high nasal vowel                          |
| on- | oñ | (same as above)                                |
| ON~ | OÑ | Back high nasal vowel (capital)                |
| ON- | OÑ | (same as above)                                |
| a\"  | ä  | Low front rounded vowel                        |
| A\"  | Ä  | Low front rounded vowel (capital)              |
|-----------------------------------------------------------|
| Dictionary orthography (Hanni Woodbury, 2003)             |
|-----------------------------------------------------------|
| e, | ę | Mid front nasal vowel                            |
| E, | Ę | Mid front nasal vowel (capital)                  |
| u, | ų | Back high nasal vowel                            |
| U, | Ų | Back high nasal vowel (capital)                  |
| a/ | æ | Low front rounded vowel                          |
| A/ | Æ | Low front rounded vowel (capital)                |

a, e, i, and o are bound to a single key.

Consonants:

| Key | Translation | Description              |
|-----+-------------+--------------------------|
| ;;  | \\=’           | Glottal stop             |
| ;:  | ʔ           | Glottal stop (alternate) |

c, d, g, h, j, k, n, s, t, w, and y are bound to a single key.

All Haudenosaunee languages, including Onondaga, can be input
simultaneously using the input method `haudenosaunee-postfix'."
 nil t nil nil nil nil nil nil nil nil t)

(pcase-dolist (`(,key ,trans)
               (append iroquoian-onondaga-modifier-alist
                       iroquoian-onondaga-consonant-alist
                       iroquoian-onondaga-nasal-alist
                       iroquoian-onondaga-vowel-alist))
  (quail-defrule key trans))


;;; Cayuga

;;
;; The primary community orthography used for the Cayuga language is
;; called the Henry orthography, after important language revitalist
;; Reginald Henry.  There are slight variations, particularly in which
;; letter is used to represent the glottal stop.  While the most common
;; seems to be <ˀ> [modifier letter glottal stop], this input method
;; provides mappings for other glottal stop letters in common use.
;; Other common orthographies should be covered by this input method as
;; well.
;;
;; Reference work for Cayuga orthography:
;;
;; Carrie Dyck, Frances Froman, Alfred Keye & Lottie Keye. 2024. A
;; grammar and dictionary of Gayogo̱hó:nǫˀ (Cayuga) (Estudios de
;; Lingüística Amerindia 1).  Berlin: Language Science Press.
;;

(defconst iroquoian-cayuga-modifier-alist nil
  "Alist of rules for modifier letters in Cayuga input methods.
Entries are as with rules in `quail-define-rules'.")

(defconst iroquoian-cayuga-vowel-alist
  '(("a'" ?á)
    ("a-" ["a̱"])
    ("A'" ?Á)
    ("A-" ["A̱"])
    ("e'" ?é)
    ("e-" ["e̱"])
    ("E'" ?É)
    ("E-" ["E̱"])
    ("i'" ?í)
    ("i-" ["i̱"])
    ("I'" ?Í)
    ("I-" ["I̱"])
    ("o'" ?ó)
    ("o-" ["o̱"])
    ("O'" ?Ó)
    ("O-" ["O̱"])
    ("u'" ?ú)
    ("u-" ["u̱"])
    ("U'" ?Ú)
    ("U-" ["U̱"])
    ("e," ?ę)
    ("e,'" ["ę́"])
    ("e,-" ["ę̱"])
    ("E," ?Ę)
    ("E,'" ["Ę́"])
    ("E,-" ["Ę̱"])
    ("o," ?ǫ)
    ("o,'" ["ǫ́"])
    ("o,-" ["ǫ̱"])
    ("O," ?Ǫ)
    ("O,'" ["Ǫ́"])
    ("O,-" ["Ǫ̱"])

    ("a''" ["a'"])
    ("a--" ["a-"])
    ("A''" ["A'"])
    ("A--" ["A-"])
    ("e''" ["e'"])
    ("e--" ["e-"])
    ("E''" ["E'"])
    ("E--" ["E-"])
    ("i''" ["i'"])
    ("i--" ["i-"])
    ("I''" ["I'"])
    ("I--" ["I-"])
    ("o''" ["o'"])
    ("o--" ["o-"])
    ("O''" ["O'"])
    ("O--" ["O-"])
    ("u''" ["u'"])
    ("u--" ["u-"])
    ("U''" ["U'"])
    ("U--" ["U-"])
    ("e,," ["e,"])
    ("e,''" ["ę'"])
    ("e,--" ["ę-"])
    ("E,," ["E,"])
    ("E,''" ["Ę'"])
    ("E,--" ["Ę-"])
    ("o,," ["o,"])
    ("o,''" ["ǫ'"])
    ("o,--" ["ǫ-"])
    ("O,," ["O,"])
    ("O,''" ["Ǫ'"])
    ("O,--" ["Ǫ-"]))
  "Alist of rules for vowel letters in Cayuga input methods.
Entries are as with rules in `quail-define-rules'.")

(defconst iroquoian-cayuga-consonant-alist
  '((";;" ?\N{MODIFIER LETTER GLOTTAL STOP})
    (";'" ?\N{RIGHT SINGLE QUOTATION MARK}))
  "Alist of rules for consonant letters in Cayuga input methods.
Entries are as with rules in `quail-define-rules'.")

(quail-define-package
 "cayuga-postfix" "Cayuga" "CAY<" t
 "Cayuga (Gayogo̱ho:nǫhnéha:ˀ) input method with postfix modifiers

Stress diacritics:

| Key  | Description  | Example |
|------+--------------+---------|
| \\='    | Acute accent | a' -> á |

Doubling the postfix separates the letter and the postfix.

Vowels:

| Key | Translation | Description                     |
|-----+-------------+---------------------------------|
| e,  | ę           | Mid front nasal vowel           |
| E,  | Ę           | Mid front nasal vowel (capital) |
| o,  | ǫ           | Mid back nasal vowel            |
| O,  | Ǫ           | Mid back nasal vowel (capital)  |

a, e, i, o, and u are bound to a single key.

Consonants:

| Key   | Translation | Description              |
|-------+-------------+--------------------------|
| ;;    | ˀ            | Glottal stop             |
| ;\\='    | \\=’           | Glottal stop (alternate) |

d, g, h, j, k, n, r, s, t, w, y, and f are bound to a single key.

Devoicing:

| Key | Description            | Example  |
|-----+------------------------+----------|
| -   | Combining macron below | a- -> a̱ |

Note: Not all fonts can properly display a combining macron low on all
vowels.

To enter a plain hyphen after a vowel, simply type the hyphen twice.

All Haudenosaunee languages, including Cayuga, can be input
simultaneously using the input method `haudenosaunee-postfix'."
 nil t nil nil nil nil nil nil nil nil t)

(pcase-dolist (`(,key ,trans)
               (append iroquoian-cayuga-modifier-alist
                       iroquoian-cayuga-consonant-alist
                       iroquoian-cayuga-vowel-alist))
  (quail-defrule key trans))


;;; Seneca

;;
;; The orthography for the Seneca language is fairly stable with only
;; minor variations, for example, <sy> vs. <š> (currently preferred in
;; community orthography) for the voiceless postalveolar fricative.
;;
;; In the common community orthography, I'm told that acute and grave
;; accents occur rarely and only on nasal vowels (personal
;; communication).  However, in works by Wallace Chafe, stress is
;; indicated on non-nasal vowels, as well.  The maximal set of letters
;; with accent diacritics is included for the input method, even though
;; many of them apparently don't occur in community orthographies.
;;
;; Reference works for Seneca orthography:
;;
;; Phyllis E. Wms. Bardeau. 2002. Onondowa'ga:' Gawe:no': New Reference
;; Edition. Salamanca, NY: The Seneca Nation of Indians Allegany
;; Education Department.
;;
;; Wallace Chafe. 2015. A Grammar of the Seneca Language. Oakland, CA:
;; University of California Press.
;;

(defconst iroquoian-seneca-modifier-alist nil
  "Alist of rules for modifier letters in Seneca input methods.
Entries are as with rules in `quail-define-rules'.")

(defconst iroquoian-seneca-vowel-alist
  '(("a'" ?á)
    ("a`" ?à)
    ("A'" ?Á)
    ("A`" ?À)
    ("e'" ?é)
    ("e`" ?è)
    ("E'" ?É)
    ("E`" ?È)
    ("i'" ?í)
    ("i`" ?ì)
    ("I'" ?Í)
    ("I`" ?Ì)
    ("o'" ?ó)
    ("o`" ?ò)
    ("O'" ?Ó)
    ("O`" ?Ò)
    ("a\"" ?ä)
    ("a\"'" ["ä́"])
    ("a\"`" ["ä̀"])
    ("A\"" ?Ä)
    ("A\"'" ["Ä́"])
    ("A\"`" ["Ä̀"])
    ("e\"" ?ë)
    ("e\"'" ["ë́"])
    ("e\"`" ["ë̀"])
    ("E\"" ?Ë)
    ("E\"'" ["Ë́"])
    ("E\"`" ["Ë̀"])
    ("o\"" ?ö)
    ("o\"'" ["ö́"])
    ("o\"`" ["ö̀"])
    ("O\"" ?Ö)
    ("O\"'" ["Ö́"])
    ("O\"`" ["Ö̀"])
    ;; Rare (e.g., niwú’u:h 'it is tiny' [Chafe 2015]):
    ("u'" ?ú)
    ("u`" ?ù)
    ("U'" ?Ú)
    ("U`" ?Ù)

    ("a''" ["a'"])
    ("a``" ["a`"])
    ("A''" ["A'"])
    ("A``" ["A`"])
    ("e''" ["e'"])
    ("e``" ["e`"])
    ("E''" ["E'"])
    ("E``" ["E`"])
    ("i''" ["i'"])
    ("i``" ["i`"])
    ("I''" ["I'"])
    ("I``" ["I`"])
    ("o''" ["o'"])
    ("o``" ["o`"])
    ("O''" ["O'"])
    ("O``" ["O`"])
    ("a\"\"" ["a\""])
    ("a\"''" ["ä'"])
    ("a\"``" ["ä`"])
    ("A\"\"" ["A\""])
    ("A\"''" ["Ä'"])
    ("A\"``" ["Ä`"])
    ("e\"\"" ["e\""])
    ("e\"''" ["ë'"])
    ("e\"``" ["ë`"])
    ("E\"\"" ["E\""])
    ("E\"''" ["Ë'"])
    ("E\"``" ["Ë`"])
    ("o\"\"" ["o\""])
    ("o\"''" ["ö'"])
    ("o\"``" ["ö`"])
    ("O\"\"" ["O\""])
    ("O\"''" ["Ö'"])
    ("O\"``" ["Ö`"])
    ("u''" ["u'"])
    ("u``" ["u`"])
    ("U''" ["U'"])
    ("U``" ["U`"]))
  "Alist of rules for vowel letters in Seneca input methods.
Entries are as with rules in `quail-define-rules'.")

(defconst iroquoian-seneca-consonant-alist
  '((";;" ?\N{RIGHT SINGLE QUOTATION MARK})
    ("s/" ?š)
    ("s//" ["s/"])
    ("S/" ?Š)
    ("S//" ["S/"]))
  "Alist of rules for consonant letters in Seneca input methods.
Entries are as with rules in `quail-define-rules'.")

(quail-define-package
 "seneca-postfix" "Seneca" "SEE<" t
 "Seneca (Onödowá’ga:’) input method with postfix modifiers

Stress diacritics:

| Key  | Description  | Example |
|------+--------------+---------|
| \\='    | Acute accent | a' -> á|
| \\=`    | Grave accent | a` -> à|

Doubling any of these postfixes separates the letter and the postfix.

Vowels:

| Key | Translation | Description                        |
|-----+-------------+------------------------------------|
| e\"  | ë           | Mid front nasal vowel              |
| E\"  | Ë           | Mid front nasal vowel (capital)    |
| o\"  | ö           | Low-mid back nasal vowel           |
| O\"  | Ö           | Low-mid back nasal vowel (capital) |
| a\"  | ä           | Low front vowel                    |
| A\"  | Ä           | Low front vowel (capital)          |

a, e, i, o, and u are bound to a single key.

Consonants:

| Key   | Translation | Description                                |
|-------+-------------+--------------------------------------------|
| ;;    | \\=’           | Glottal stop                               |
| s/    | š           | Voiceless postalveolar fricative           |
| S/    | Š           | Voiceless postalveolar fricative (capital) |

d, g, h, j, k, n, s, t, w, y, and z are bound to a single key.

b, m, and p are used rarely in ideophones and nicknames.  They are also
each bound to a single key.

All Haudenosaunee languages, including Seneca, can be input
simultaneously using the input method `haudenosaunee-postfix'."
 nil t nil nil nil nil nil nil nil nil t)

(pcase-dolist (`(,key ,trans)
               (append iroquoian-seneca-modifier-alist
                       iroquoian-seneca-consonant-alist
                       iroquoian-seneca-vowel-alist))
  (quail-defrule key trans))


;;; Tuscarora

;;
;; The primary community orthography used for Tuscarora follows that
;; used in Blair Rudes's dictionary (see below).
;;
;; Reference work for Tuscarora orthography:
;;
;; Blair Rudes. 1999. Tuscarora-English/English-Tuscarora
;; dictionary. Toronto: University of Toronto Press.
;;

(defconst iroquoian-tuscarora-modifier-alist
  '(("::" ?\N{MIDDLE DOT}))
  "Alist of rules for modifier letters in Tuscarora input methods.
Entries are as with rules in `quail-define-rules'.")

(defconst iroquoian-tuscarora-vowel-alist
  '(("a'" ?á)
    ("a`" ?à)
    ("A'" ?Á)
    ("A`" ?À)
    ("e'" ?é)
    ("e`" ?è)
    ("E'" ?É)
    ("E`" ?È)
    ("i'" ?í)
    ("i`" ?ì)
    ("I'" ?Í)
    ("I`" ?Ì)
    ("u'" ?ú)
    ("u`" ?ù)
    ("U'" ?Ú)
    ("U`" ?Ù)
    ("e," ?ę)
    ("e,'" ["ę́"])
    ("e,`" ["ę̀"])
    ("E," ?Ę)
    ("E,'" ["Ę́"])
    ("E,`" ["Ę̀"])

    ("a''" ["a'"])
    ("a``" ["a`"])
    ("A''" ["A'"])
    ("A``" ["A`"])
    ("e''" ["e'"])
    ("e``" ["e`"])
    ("E''" ["E'"])
    ("E``" ["E`"])
    ("i''" ["i'"])
    ("i``" ["i`"])
    ("I''" ["I'"])
    ("I``" ["I`"])
    ("u''" ["u'"])
    ("u``" ["u`"])
    ("U''" ["U'"])
    ("U``" ["U`"])

    ("e,," ["e,"])
    ("e,''" ["ę'"])
    ("e,``" ["ę`"])
    ("E,," ["E,"])
    ("E,''" ["Ę'"])
    ("E,``" ["Ę`"]))
  "Alist of rules for vowel letters in Tuscarora input methods.
Entries are as with rules in `quail-define-rules'.")

(defconst iroquoian-tuscarora-consonant-alist
  '((";;" ?\N{LATIN LETTER GLOTTAL STOP})
    ("c/" ?č)
    ("c//" ["c/"])
    ("C/" ?Č)
    ("C//" ["C/"])
    ("t/" ?θ)
    ("t//" ["t/"]))
  "Alist of rules for consonant letters in Tuscarora input methods.
Entries are as with rules in `quail-define-rules'.")

(defconst iroquoian-tuscarora-exception-alist
  '(("_" ?\N{COMBINING LOW LINE})
    ("__" ?_))
  "Alist of rules for phonological exception marking in Tuscarora input methods.
Entries are as with rules in `quail-define-rules'.")

(quail-define-package
 "tuscarora-postfix" "Tuscarora" "TUS<" t
 "Tuscarora (Skarù·ręʔ) input method with postfix modifiers

Modifiers:

| Key | Translation | Description              |
|-----+-------------+--------------------------|
| ::  | ·           | Vowel length             |

Stress diacritics:

| Key  | Description  | Example |
|------+--------------+---------|
| \\='    | Acute accent | a' -> á |
| \\=`    | Grave accent | a` -> à |

Doubling the postfix separates the letter and the postfix.

Vowels:

| Key | Translation | Description                     |
|-----+-------------+---------------------------------|
| e,  | ę           | Mid front nasal vowel           |
| E,  | Ę           | Mid front nasal vowel (capital) |

a, e, i, and u are bound to a single key.

Consonants:

| Key   | Translation | Description                        |
|-------+-------------+------------------------------------|
| ;;    | ˀ            | Glottal stop                       |
| c/    | č            | Postalveolar affricate            |
| C/    | Č            | Postalveolar affricate (capital)  |
| t/    | θ            | Voiceless dental fricative        |

h, k, n, r, s, t, w, and y are bound to a single key.

b, l, m, and p are used rarely in loanwords.  They are also each bound
to a single key.

Stress exception markers:

| Key | Description        | Example  |
|-----+--------------------+----------|
| _   | Combining low line | a_ -> a̲ |

Note: Not all fonts can properly display a combining low line on all
letters.

Underlining has been used by some to indicate that vowels behave
exceptionally with regard to stress placement.  Alternatively, markup or
other methods can be used to create an underlining effect.

To enter a plain underscore, type the underscore twice.

All Haudenosaunee languages, including Tuscarora can be input
simultaneously using the input method `haudenosaunee-postfix'."
 nil t nil nil nil nil nil nil nil nil t)

(pcase-dolist (`(,key ,trans)
               (append iroquoian-tuscarora-modifier-alist
                       iroquoian-tuscarora-consonant-alist
                       iroquoian-tuscarora-vowel-alist
                       iroquoian-tuscarora-exception-alist))
  (quail-defrule key trans))


;;; Haudenosaunee (composite Northern Iroquoian)

;;
;; This input method represents a composite input method for all of the
;; Northern Iroquoian languages included above.
;;
;; Although the "Iroquoian languages" is a standard term employed by
;; linguists and scholars, some believe the term "Iroquois" to be of
;; derogatory origin (see Dyck 2024).  Hence, some prefer to refer to
;; what are collectively termed by linguists the "Five Nations Iroquois"
;; languages (Mohawk, Oneida, Onondaga, Cayuga, Seneca) by the autonym
;; "Haudenosaunee" (e.g., "Haudenosaunee languages").
;;
;; However, it should be noted that the term "Haudenosaunee" is itself
;; an Anglicized form, probably from Seneca Hodínöhsö:ni:h 'they make
;; houses' or Hodínöhšo:ni:h 'People of the Long House'.  Speakers of
;; Cayuga may prefer the word Hodinǫhsǫ:nih, and speakers of Mohawk may
;; prefer Rotinonhsón:ni or Rotinonhsíón:ni.  These terms themselves
;; collectively relate to the confederacy of Indigenous nations that has
;; existed in what is now known as New York State in Northeastern North
;; America for many centuries, the founding of which is retold in oral
;; tradition in the story of The Peacemaker.
;;
;; It should also be noted that while Tuscarora and Wendat languages are
;; both sometimes included under the "Haudenosaunee languages" umbrella
;; (and by implication, those groups as a part of the Haudenosaunee
;; Confederacy), the exact extent of what defines "Haudenosaunee" has
;; occasionally caused controversy.
;;
;; Additionally, some prefer to collectively refer to the "Haudenosaunee
;; languages" using the terms Onkwehonwehnéha (Mohawk), Ukwehuwehnéha
;; (Oneida), Ǫgwehǫwekhá’ (Onondaga), Ǫgwehǫwéhneha:ˀ (Cayuga), and
;; Ögwé’öwe:ka:’ (Seneca), which all mean 'in the manner of the Original
;; People'.
;;
;; Bearing all of this in mind, I have opted to retain the term
;; "Iroquoian" in the name of this file (`iroquoian.el') (and hence, in
;; the symbol names in its namespace), while using "Haudenosaunee" in
;; the name of the input method that encompasses all of the languages so
;; far implemented: "haudenosaunee-postfix" --- this is the name shown
;; as a completion candidate after users enter M-x set-input-method RET.
;; Note that those searching for input methods for the individual
;; languages should have no problem finding them knowing only their
;; Anglicized names (e.g., Mohawk, Oneida, etc.), as these have been
;; retained in the names of the corresponding input methods.
;;
;; Above all, I hope that these decisions help those who wish to speak,
;; read, and write Onkwehonwehnéha.
;;
;; Iorihowá:nen ne aiónhnheke’ ne raotiwén:na’!
;; It is important that the language continues to live!
;;

(defconst iroquoian-haudenosaunee-modifier-alist
  (seq-uniq (append iroquoian-mohawk-modifier-alist
                    iroquoian-oneida-modifier-alist
                    iroquoian-onondaga-modifier-alist
                    iroquoian-cayuga-modifier-alist
                    iroquoian-seneca-modifier-alist
                    iroquoian-tuscarora-modifier-alist))
  "Alist of rules for modifier letters in Haudenosaunee input methods.
Entries are as with rules in `quail-define-rules'.")

(defconst iroquoian-haudenosaunee-vowel-alist
  (seq-uniq (append iroquoian-mohawk-vowel-alist
                    iroquoian-oneida-vowel-alist
                    iroquoian-onondaga-vowel-alist
                    iroquoian-cayuga-vowel-alist
                    iroquoian-seneca-vowel-alist
                    iroquoian-tuscarora-vowel-alist))
  "Alist of rules for vowel letters in Haudenosaunee input methods.
Entries are as with rules in `quail-define-rules'.")

(defconst iroquoian-haudenosaunee-consonant-alist
  (seq-uniq (append
             '((";;" ?\N{RIGHT SINGLE QUOTATION MARK})
               (";'" ?\N{MODIFIER LETTER GLOTTAL STOP})
               (";:" ?\N{LATIN LETTER GLOTTAL STOP}))
             iroquoian-mohawk-consonant-alist
             iroquoian-oneida-consonant-alist
             iroquoian-onondaga-consonant-alist
             iroquoian-cayuga-consonant-alist
             iroquoian-seneca-consonant-alist
             iroquoian-tuscarora-consonant-alist)
            (lambda (c1 c2)
              (equal (car c1) (car c2))))
  "Alist of rules for consonant letters in Haudenosaunee input methods.
Entries are as with rules in `quail-define-rules'.")

(defconst iroquoian-haudenosaunee-exception-alist
  '(("_" ?\N{COMBINING LOW LINE})
    ("__" ?_))
  "Rules alist for phonological exception markers in Haudenosaunee input methods.
Entries are as with rules in `quail-define-rules'.")

(defconst iroquoian-haudenosaunee-nasal-alist iroquoian-onondaga-nasal-alist
  "Alist of rules for nasal modifier letters in Haudenosaunee input methods.
Entries are as with rules in `quail-define-rules'.")

(quail-define-package
 "haudenosaunee-postfix" "Haudenosaunee" "HOD<" t
 "Composite input method for Haudenosaunee (Northern Iroquoian) languages

This input method can be used to enter the following languages:

- Mohawk (Kanien’kéha / Kanyen’kéha / Onkwehonwehnéha)
- Oneida (Onʌyote’a·ká· / Onyota’a:ká: / Ukwehuwehnéha)
- Cayuga (Gayogo̱ho:nǫhnéha:ˀ)
- Onondaga (Onųdaʔgegáʔ)
- Seneca (Onödowá’ga:’)
- Tuscarora (Skarù·ręʔ)

Modifiers:

| Key | Translation | Description              |
|-----+-------------+--------------------------|
| ::  | ·           | Vowel length (alternate) |

Stress diacritics:

| Key  | Description  | Example |
|------+--------------+---------|
| \\='    | Acute accent | a' -> á |
| \\=`    | Grave accent | a` -> à |

Doubling any of these postfixes separates the letter and the postfix.

Vowels:

| Key  | Translation | Description                                     |
|----------------------------------------------------------------------|
| Mohawk                                                               |
| -------------------------------------------------------------------- |
| Single-key vowels: a e i o                                           |
|----------------------------------------------------------------------|
| Oneida                                                               |
| -------------------------------------------------------------------- |
| e/   | ʌ           | Mid central nasal vowel                         |
| E/   | Ʌ           | Mid central nasal vowel (capital)               |
| Single-key vowels: a e i o u                                         |
|----------------------------------------------------------------------|
| Onondaga                                                             |
| (Six Nations of the Grand River)                                     |
| -------------------------------------------------------------------- |
| e,   | ę           | Mid front nasal vowel                           |
| E,   | Ę           | Mid front nasal vowel (capital)                 |
| o,   | ǫ           | Back high nasal vowel                           |
| O,   | Ǫ           | Back high nasal vowel (capital)                 |
| a\"   | ä           | Low front rounded vowel                         |
| A\"   | Ä           | Low front rounded vowel (capital)               |
| -------------------------------------------------------------------- |
| (Onondaga Nation, New York)                                          |
| -------------------------------------------------------------------- |
| en~  | eñ          | Mid front nasal vowel                           |
| en-  | eñ          | (same as above)                                 |
| EN~  | EÑ          | Mid front nasal vowel (capital)                 |
| EN-  | EÑ          | (same as above)                                 |
| on~  | oñ          | Back high nasal vowel                           |
| on-  | oñ          | (same as above)                                 |
| ON~  | OÑ          | Back high nasal vowel (capital)                 |
| ON-  | OÑ          | (same as above)                                 |
| a\"   | ä           | Low front rounded vowel                         |
| A\"   | Ä           | Low front rounded vowel (capital)               |
| -------------------------------------------------------------------- |
| (Hanni Woodbury, 2003)                                               |
| -------------------------------------------------------------------- |
| e,   | ę           | Mid front nasal vowel                           |
| E,   | Ę           | Mid front nasal vowel (capital)                 |
| u,   | ų           | Back high nasal vowel                           |
| U,   | Ų           | Back high nasal vowel (capital)                 |
| a/   | æ           | Low front rounded vowel                         |
| A/   | Æ           | Low front rounded vowel (capital)               |
| -------------------------------------------------------------------- |
| (all)                                                                |
| -------------------------------------------------------------------- |
| Single-key vowels: a e i o                                           |
|----------------------------------------------------------------------|
| Cayuga                                                               |
| -------------------------------------------------------------------- |
| e,   | ę           | Mid front nasal vowel                           |
| E,   | Ę           | Mid front nasal vowel (capital)                 |
| o,   | ǫ           | Mid back nasal vowel                            |
| O,   | Ǫ           | Mid back nasal vowel (capital)                  |
| Single-key vowels: a e i o u                                         |
|----------------------------------------------------------------------|
| Seneca                                                               |
| -------------------------------------------------------------------- |
| e\"   | ë           | Mid front nasal vowel                           |
| E\"   | Ë           | Mid front nasal vowel (capital)                 |
| o\"   | ö           | Low-mid back nasal vowel                        |
| O\"   | Ö           | Low-mid back nasal vowel (capital)              |
| a\"   | ä           | Low front vowel                                 |
| A\"   | Ä           | Low front vowel (capital)                       |
| Single-key vowels: a e i o u                                         |
|----------------------------------------------------------------------|
| Tuscarora                                                            |
| -------------------------------------------------------------------- |
| e,   | ę           | Mid front nasal vowel                           |
| E,   | Ę           | Mid front nasal vowel (capital)                 |
| Single-key vowels: a e i u                                           |

Consonants:

| Key   | Translation | Description                                    |
|----------------------------------------------------------------------|
| Mohawk                                                               |
| -------------------------------------------------------------------- |
| ;;    | \\=’           | Glottal stop                                   |
| Single-key consonants: h k n r s t w y (b m p)                       |
|----------------------------------------------------------------------|
| Oneida                                                               |
| -------------------------------------------------------------------- |
| ;;    | \\=’           | Glottal stop                                   |
| ;\\='    | ˀ           | Glottal stop (alternate)                        |
| Single-key consonants: h k l n s t w y                               |
|----------------------------------------------------------------------|
| Onondaga                                                             |
| -------------------------------------------------------------------- |
| ;;    | \\=’           | Glottal stop                                   |
| ;:    | ʔ           | Glottal stop (alternate)                       |
| Single-key consonants: c d g h j k n s t w y                         |
|----------------------------------------------------------------------|
| Cayuga                                                               |
| -------------------------------------------------------------------- |
| ;\\='    | ˀ           | Glottal stop                                   |
| ;;    | \\=’           | Glottal stop (alternate)                       |
| Single-key consonants: d g h j k n r s t w y (f)                     |
|----------------------------------------------------------------------|
| Seneca                                                               |
| -------------------------------------------------------------------- |
| ;;    | \\=’           | Glottal stop                                   |
| s/    | š           | Voiceless postalveolar fricative               |
| S/    | Š           | Voiceless postalveolar fricative (capital)     |
| Single-key consonants: d g h j k n s t w y z (b m p)                 |
|----------------------------------------------------------------------|
| Tuscarora                                                            |
| -------------------------------------------------------------------- |
| ;:    | ʔ           | Glottal stop (alternate)                       |
| c/    | č            | Postalveolar affricate                        |
| C/    | Č            | Postalveolar affricate (capital)              |
| t/    | θ            | Voiceless dental fricative                    |
| Single-key consonants: h k n r s t w y (b l m p)                     |

Phonological exception markers:

| Key | Description            | Examples                     |
|-----+------------------------+------------------------------|
| _   | Combining low line     | a_ -> a̲, · -> ·̲           |
| -   | Combining macron below | a- -> a̱(after vowels only)  |

Note: Not all fonts can properly display a combining low line on all
letters and a combining macron below on all vowels.

Underlining is commonly used in Oneida to indicate devoiced syllables on
pre-pausal forms (also called utterance-final forms), and it has been
used in some Tuscarora orthographies to indicate that vowels behave
exceptionally with regard to stress placement. Alternatively, markup or
other methods can be used to create an underlining effect.

To enter a plain underscore, the underscore twice.

Macron below is commonly used in Cayuga to indicate devoiced vowels.

To enter a plain hyphen after a vowel, simply type the hyphen twice.

There are individual input methods for each of the languages that can be
entered with this input method: `mohawk-postfix', `oneida-postfix',
`onondaga-postfix', `cayuga-postfix', `seneca-postfix',
`tuscarora-postfix'.."
 nil t nil nil nil nil nil nil nil nil t)

(pcase-dolist (`(,key ,trans)
               (append iroquoian-haudenosaunee-modifier-alist
                       iroquoian-haudenosaunee-consonant-alist
                       iroquoian-haudenosaunee-nasal-alist
                       iroquoian-haudenosaunee-vowel-alist
                       iroquoian-haudenosaunee-exception-alist))
  (quail-defrule key trans))

(provide 'iroquoian)
;;; iroquoian.el ends here
