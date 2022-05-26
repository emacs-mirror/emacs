;;; indonesian.el --- Indonesian languages support  -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

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
Balinese language and its script are supported in this language environment.")))


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
                               (concat consonant rerekan "?" "\\(?:" adeg-adeg consonant
                                       rerekan "?\\)*\\(?:" adeg-adeg "\\|" vowel "*" rerekan
                                       "?" modifier-above "?" musical-symbol "?\\)")
                               1 'font-shape-gstring)
                              (vector
                               ;; Vowels based syllables
                               (concat independent-vowel rerekan "?" adeg-adeg "?"
                                       vowel "?" modifier-above "?" musical-symbol "?")
                               1 'font-shape-gstring))))


(provide 'indonesian)
;;; indonesian.el ends here
