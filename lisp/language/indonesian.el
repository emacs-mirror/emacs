;;; indonesian.el --- Indonesian languages support  -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2022-2026 Free Software Foundation, Inc.

;; Author: समीर सिंह Sameer Singh <lumarzeli30@gmail.com>
;; Keywords: multilingual, input method, i18n, Indonesia

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

;; This file contains definitions of Indonesia language environments, and
;; setups for displaying the scripts used there.

;;; Code:

(set-language-info-alist
 "Balinese" '((charset unicode)
              (coding-system utf-8)
              (coding-priority utf-8)
              (input-method . "balinese")
              (sample-text . "Balinese (ᬅᬓ᭄ᬱᬭᬩᬮᬶ)	ᬒᬁᬲ᭄ᬯᬲ᭄ᬢ᭄ᬬᬲ᭄ᬢᬸ")
              (documentation . "\
Balinese language and its script are supported in this language environment."))
 '("Indonesian"))

(set-language-info-alist
 "Javanese" '((charset unicode)
              (coding-system utf-8)
              (coding-priority utf-8)
              (input-method . "javanese")
              (sample-text . "Javanese (ꦲꦏ꧀ꦱꦫꦗꦮ)	ꦲꦭꦺꦴ")
              (documentation . "\
Javanese language and its script are supported in this language environment."))
 '("Indonesian"))

(set-language-info-alist
 "Sundanese" '((charset unicode)
              (coding-system utf-8)
              (coding-priority utf-8)
              (input-method . "sundanese")
              (sample-text . "Sundanese (ᮃᮊ᮪ᮞᮛᮞᮥᮔ᮪ᮓ)    ᮞᮙ᮪ᮕᮥᮛᮞᮥᮔ᮪")
              (documentation . "\
Sundanese language and its script are supported in this language environment."))
 '("Indonesian"))

(set-language-info-alist
 "Batak" '((charset unicode)
           (coding-system utf-8)
           (coding-priority utf-8)
           (input-method . "batak")
           (sample-text . "Batak (ᯘᯮᯒᯗ᯲ᯅᯗᯂ᯲)    ᯂᯬᯒᯘ᯲ / ᯔᯧᯐᯬᯀᯱᯐᯬᯀᯱ")
           (documentation . "\
Languages that use the Batak script, such as Karo, Toba, Pakpak, Mandailing
and Simalungun, are supported in this language environment."))
 '("Indonesian"))

(set-language-info-alist
 "Rejang" '((charset unicode)
            (coding-system utf-8)
            (coding-priority utf-8)
            (input-method . "rejang")
            (sample-text . "Rejang (ꥆꤰ꥓ꤼꤽ ꤽꥍꤺꥏ)    ꤸꥉꥐꤺꥉꥂꥎ")
            (documentation . "\
Rejang language and its script are supported in this language environment."))
 '("Indonesian"))

(set-language-info-alist
 "Makasar" '((charset unicode)
             (coding-system utf-8)
             (coding-priority utf-8)
             (input-method . "makasar")
             (sample-text . "Makasar (𑻪𑻢𑻪𑻢)    𑻦𑻤𑻵𑻱")
             (documentation . "\
Makassarese language and its script Makasar are supported in this language environment."))
 '("Indonesian"))

(set-language-info-alist
 "Buginese" '((charset unicode)
              (coding-system utf-8)
              (coding-priority utf-8)
              (input-method . "lontara")
              (sample-text . "Buginese (ᨒᨚᨈᨑ)    ᨖᨒᨚ")
              (documentation . "\
Buginese language and its script Lontara are supported in this language environment."))
 '("Indonesian"))

;; Balinese composition rules
(let ((consonant            "[\x1B13-\x1B33\x1B45-\x1B4B]")
      (independent-vowel    "[\x1B05-\x1B12]")
      (rerekan              "\x1B34")
      (vowel                "[\x1B35-\x1B43]")
      (modifier-above       "[\x1B00-\x1B04]")
      (adeg-adeg            "\x1B44")
      (musical-symbol       "[\x1B6B-\x1B73]"))
  (set-char-table-range composition-function-table
                        '(#x1B34 . #x1B44)
                        (list (vector
                               ;; Consonant based syllables
                               (concat consonant rerekan "?\\(?:" adeg-adeg consonant
                                       rerekan "?\\)*\\(?:" adeg-adeg "\\|" vowel "*" rerekan
                                       "?" modifier-above "?" musical-symbol "?\\)")
                               1 'font-shape-gstring)
                              (vector
                               ;; Vowels based syllables
                               (concat independent-vowel rerekan "?" adeg-adeg "?"
                                       vowel "?" modifier-above "?" musical-symbol "?")
                               1 'font-shape-gstring))))

;; Javanese composition rules
(let ((consonant            "[\xA98F-\xA9B2]")
      (independent-vowel    "[\xA984-\xA98E]")
      (telu                 "\xA9B3")
      (vowel                "[\xA9B4-\xA9BC]")
      (dependant-consonant  "[\xA9BD-\xA9BF]")
      (modifier-above       "[\xA980-\xA983]")
      (pangkon              "\xA9C0"))
  (set-char-table-range composition-function-table
                        '(#xA9B3 . #xA9C0)
                        (list (vector
                               ;; Consonant based syllables
                               (concat consonant telu "?\\(?:" pangkon consonant
                                       telu "?\\)*\\(?:" pangkon "\\|" vowel "*" telu
                                       "?" modifier-above "?" dependant-consonant "?\\)")
                               1 'font-shape-gstring)
                              (vector
                               ;; Vowels based syllables
                               (concat independent-vowel telu "?" pangkon "?"
                                       vowel "?" modifier-above "?" dependant-consonant "?")
                               1 'font-shape-gstring))))

;; Sundanese composition rules
(let ((consonant            "[\x1B8A-\x1BA0\x1BAE\x1BAF\x1BBB-\x1BBF]")
      (independent-vowel    "[\x1B83-\x1B89]")
      (vowel                "[\x1BA4-\x1BA9]")
      (dependant-consonant  "[\x1BA1-\x1BA3\x1BAC-\x1BAD]")
      (modifier-above       "[\x1B80-\x1B82]")
      (virama               "[\x1BAA\x1BAB]"))
  (set-char-table-range composition-function-table
                        '(#x1BA1 . #x1BAD)
                        (list (vector
                               ;; Consonant based syllables
                               (concat consonant "\\(?:" virama consonant
                                       "\\)*\\(?:" virama "\\|" vowel "*"
                                       modifier-above "?" dependant-consonant "?\\)")
                               1 'font-shape-gstring)
                              (vector
                               ;; Vowels based syllables
                               (concat independent-vowel virama "?"
                                       vowel "?" modifier-above "?" dependant-consonant "?")
                               1 'font-shape-gstring))))

;; Batak composition rules
(let ((akshara              "[\x1BC0-\x1BE5]")
      (vowel                "[\x1BE7-\x1BEF]")
      (dependant-consonant  "[\x1BF0\x1BF1]")
      (modifier-above       "\x1BE6")
      (virama               "[\x1BF2\x1BF3]"))
  (set-char-table-range composition-function-table
                        '(#x1BE6 . #x1BF3)
                        (list (vector
                               ;; Akshara based syllables
                               (concat akshara virama "?" vowel "*" modifier-above
                                       "?" dependant-consonant "?")
                               1 'font-shape-gstring))))

;; Rejang composition rules
(let ((akshara              "[\xA930-\xA946]")
      (vowel                "[\xA947-\xA94E]")
      (dependant-consonant  "[\xA94F\xA952]")
      (virama               "\xA953"))
  (set-char-table-range composition-function-table
                        '(#xA947 . #xA953)
                        (list (vector
                               ;; Akshara based syllables
                               (concat akshara virama "?" vowel "*"
                                       dependant-consonant "?")
                               1 'font-shape-gstring))))

;; Makasar composition rules
(let ((akshara              "[\x11EE0-\x11EF2]")
      (vowel                "[\x11EF3-\x11EF6]"))
  (set-char-table-range composition-function-table
                        '(#x11EF3 . #x11EF6)
                        (list (vector
                               ;; Akshara based syllables
                               (concat akshara vowel "*")
                               1 'font-shape-gstring))))

(provide 'indonesian)
;;; indonesian.el ends here
