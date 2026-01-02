;;; pakistan.el --- Input methods for some languages from Pakistan -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023-2026 Free Software Foundation, Inc.

;; Author: Rahguzar <rahguzar@zohomail.eu>
;; Keywords: convenience, multilingual, input method, Urdu, Balochi, Pashto, Sindhi, Hindko, Brahui
;;
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
;;
;;; Commentary:
;; Provides a semi-phonetic input method for Urdu
;;
;;; Code:
(require 'quail)

;;;; Urdu Input Methods
;;;;; Keyboard
;; Layout taken from https://www.branah.com/urdu
(quail-define-package
 "urdu-keyboard" "Urdu" "ات" t
 "Input method for Urdu.
Uses keyboard layout from https://www.branah.com/urdu"
 nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ("q" ?ط)
 ("w" ?ص)
 ("e" ?ھ)
 ("r" ?د)
 ("t" ?ٹ)
 ("y" ?پ)
 ("u" ?ت)
 ("i" ?ب)
 ("o" ?ج)
 ("p" ?ح)
 ("a" ?م)
 ("s" ?و)
 ("d" ?ر)
 ("f" ?ن)
 ("g" ?ل)
 ("h" ?ہ)
 ("j" ?ا)
 ("k" ?ک)
 ("l" ?ی)
 ("z" ?ق)
 ("x" ?ف)
 ("c" ?ے)
 ("v" ?س)
 ("b" ?ش)
 ("n" ?غ)
 ("m" ?ع)
 ("Q" ?ظ)
 ("W" ?ض)
 ("E" ?ذ)
 ("R" ?ڈ)
 ("T" ?ث)
 ("Y" ?ّ)
 ("U" ?ۃ)
 ("I" ?ـ)
 ("O" ?چ)
 ("P" ?خ)
 ("A" ?ژ)
 ("S" ?ز)
 ("D" ?ڑ)
 ("F" ?ں)
 ("G" ?ۂ)
 ("H" ?ء)
 ("J" ?آ)
 ("K" ?گ)
 ("L" ?ي)
 ("C" ?ۓ)
 ("B" ?ؤ)
 ("N" ?ئ)
 ("[" ?\])
 ("]" ?\[)
 ("{" ?})
 ("}" ?{)
 (";" ?؛)
 ("." ?۔)
 ("," ?،)
 ("?" ?؟))

;;;;; Phonetic Keyboard
(quail-define-package
 "urdu-phonetic-keyboard" "Urdu" "اص" t
 "Input method for Urdu.
Uses phonetic keyboard layout from https://www.branah.com/urdu"
 nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ("q" ?ق)
 ("w" ?و)
 ("e" ?ع)
 ("r" ?ر)
 ("t" ?ت)
 ("y" ?ے)
 ("u" ?ء)
 ("i" ?ی)
 ("o" ?ہ)
 ("p" ?پ)
 ("a" ?ا)
 ("s" ?س)
 ("d" ?د)
 ("f" ?ف)
 ("g" ?گ)
 ("h" ?ح)
 ("j" ?ج)
 ("k" ?ک)
 ("l" ?ل)
 ("z" ?ز)
 ("x" ?ش)
 ("c" ?چ)
 ("v" ?ط)
 ("b" ?ب)
 ("n" ?ن)
 ("m" ?م)
 ("Q" ?ْ)
 ("W" ?ٔ)
 ("E" ?ٰ)
 ("R" ?ڑ)
 ("T" ?ٹ)
 ("Y" ?َ)
 ("U" ?ئ)
 ("I" ?ِ)
 ("O" ?ۃ)
 ("P" ?ُ)
 ("A" ?آ)
 ("S" ?ص)
 ("D" ?ڈ)
 ("F" ?أ)
 ("G" ?غ)
 ("H" ?ھ)
 ("J" ?ض)
 ("K" ?خ)
 ("L" ?ٖ)
 ("Z" ?ذ)
 ("X" ?ژ)
 ("C" ?ث)
 ("V" ?ظ)
 ("B" ?ً)
 ("N" ?ں)
 ("M" ?ّ)
 ("1" ?۱)
 ("2" ?۲)
 ("3" ?۳)
 ("4" ?۴)
 ("5" ?۵)
 ("6" ?٦)
 ("7" ?۷)
 ("8" ?۸)
 ("9" ?۹)
 ("0" ?۰)
 ("`" ?؏)
 ("#" ?ؔ)
 ("$" ?ؒ)
 ("%" ?٪)
 ("^" ?ؓ)
 ("&" ?ؑ)
 ("*" ?ؐ)
 ("(" ?\))
 (")" ?\()
 ("=" ?+)
 (";" ?؛)
 ("\\" ?÷)
 ("|" ?x)
 ("," ?،)
 ("." ?۔)
 ("<" ?ٗ)
 (">" ?.)
 ("?" ?؟)
 ("[" ?﷽)
 ("]" ?ﷲ)
 ("{" ?ﷺ))

;;;;; Customizable Input Method
;;;;;; Variable declarations
;; We define these variables now so that byte-compiler does not complain.
;; Later they will be changed to custom variables. Their value must be void
;; here as otherwise cutsom will not initialize them to their standard value.
(defvar pakistan-urdu-prefixes)
(defvar pakistan-urdu-translations)
(defvar pakistan-urdu-diacritics-and-other-symbols)
(defvar pakistan-urdu-poetic-symbols)
(defvar pakistan-urdu-religious-symbols)
(defvar pakistan-urdu-use-roman-digits)
(defvar pakistan-extra-balochi-brahui-translations)
(defvar pakistan-extra-pashto-translations)
(defvar pakistan-extra-saraiki-hindko-translations)
(defvar pakistan-extra-sindhi-translations)

;;;;;; Helper functions
(defun pakistan--define-quail-rules (rules &optional prefix package)
  "Define translations for `urdu-custom' input method as determined by RULES.
PACKAGE determines the input method and defaults to `urdu-custom'.  RULES is
the list of rules to define, see `quail-defrule' for details.  If non-nil
PREFIX is a string that is prefixed to each string in RULES.  PREFIX can be a
symbol in which case it is looked up in `pakistan-urdu-prefixes' to obtain the
string."
  (setq package (or package "urdu-custom"))
  (when (and prefix (symbolp prefix))
    (setq prefix (car (alist-get prefix pakistan-urdu-prefixes))))
  (dolist (rule rules)
    (quail-defrule (concat prefix (car rule)) (cadr rule) package)))

(defun pakistan--define-numeral-translations (&optional package)
  "Define translations to translate digits to arabic digits.
Translations are for PACKAGE which defaults to `urdu-custom'."
  (pakistan--define-quail-rules
   '(("0"  ?۰)
     ("1"  ?۱)
     ("2"  ?۲)
     ("3"  ?۳)
     ("4"  ?۴)
     ("5"  ?۵)
     ("6"  ?۶)
     ("7"  ?۷)
     ("8"  ?۸)
     ("9"  ?۹)
     ("%" ?٪))
   nil package))

(defun pakistan--set-numeral-translations (var val)
  "VAR should be `pakistan-urdu-use-roman-digits' and VAL its value.
This is a setter function for the custom-variable."
  (set-default-toplevel-value var val)
  (if val
      (pakistan--regenerate-translations)
    (pakistan--define-numeral-translations)))

(defun pakistan--regenerate-translations ()
  "Regenerate the translations for urdu-custom input method."
  (quail-select-package "urdu-custom")
  (quail-install-map (list nil))
  (pakistan--define-quail-rules pakistan-urdu-translations)
  (unless pakistan-urdu-use-roman-digits
    (pakistan--define-numeral-translations))
  (pakistan--define-quail-rules
   pakistan-urdu-diacritics-and-other-symbols 'diacritics)
  (pakistan--define-quail-rules pakistan-urdu-poetic-symbols 'poetic)
  (pakistan--define-quail-rules pakistan-urdu-religious-symbols 'religious)
  (pakistan--define-quail-rules
   pakistan-extra-balochi-brahui-translations 'balochi-brahui)
  (pakistan--define-quail-rules pakistan-extra-pashto-translations 'pashto)
  (pakistan--define-quail-rules
   pakistan-extra-saraiki-hindko-translations 'saraiki-hindko)
  (pakistan--define-quail-rules pakistan-extra-sindhi-translations 'sindhi))

(defun pakistan--set-prefixes (var val)
  "VAR should be `pakistan-urdu-prefixes' and VAL is the value to be set.
Setter function for `pakistan-urdu-prefixes'."
  (set-default-toplevel-value var val)
  (when (boundp 'pakistan-urdu-use-roman-digits)
    (pakistan--regenerate-translations)))

(defun pakistan--make-setter (&optional prefix)
  "Return the setter function.
The function adds rules to `urdu-custom' with PREFIX."
  (lambda (var val)
    (set-default-toplevel-value var val)
    (if (boundp 'pakistan-urdu-use-roman-digits)
        (pakistan--regenerate-translations)
      (pakistan--define-quail-rules val prefix))))

;;;;;; Package definition
(quail-define-package
 "urdu-custom" "Urdu" "اا" t
 "Intuitive and customizable transl input method for Urdu.
By default this input method doesn't try to follow the common romanization of
Urdu very closely.  The reason for this is allow to for input efficiency. It
works as follows:

1) All lower case letters on QWERTY keyboard are translated to an urdu
character.  When more than one Urdu letter corresponds to the same Roman
letter, the most common Urdu letter has been chosen.  The frequency analysis
was done on the basis of Urdu word list at
https://github.com/urduhack/urdu-words/blob/master/words.txt As a result some
of the translations are:
h → ہ
s → س , c → ص
z → ز

2) For the next common letter the uppercase English letter is used, e.g.
r → ر , R → ڑ
n → ن , N → ں

3) The letter x is used for postfix completions.  There are two subcases:
3a) When more than two urdu letter map to the same roman letter,
e.g.
t → ت, T → ٹ , tx → ط , Tx → ۃ
h → ہ , H → ھ , hx → ح , Hx → ۂ
s → س , c → ص , sx → ش , S → ث , cx → چ
z → ز , Z → ض, zx → ذ , Zx → ظ
3b) The urdu letters that are commonly romanized by a English letter + h
can be obtained by the same English letter + x i.e.
gx → غ , cx → چ, kx → خ , sx → ش

4) Y → ژ is somewhat of an abberation.  All four of z, Z, zx and Zx are
used by more common letters.  Y is used for ژ because it is sometimes
pronounced close to Y for some European languages.

These translations can be changed by customizing `pakistan-urdu-translations'.

5) o is used for prefix completion of diacrtics or اعر۱ب as well as some
poetic and religious symbols.  The most common three diacritics are mapped to
oa → zabr (a for above)
ob → zer  (b for below)
oo → pesh (o for the circle in pesh)

6) The poetic symbols are also available under G (for غزل), while religious
symbols are also available under M (for مزہب).

7) Characters from Balochi, Brahui Pashto, Saraiki and Sindhi which are not
part of Urdu alphabet can also be input.  Each set of these sets correspond to
a different prefixes. See `pakistan-urdu-prefixes' for the prefixes.

The translations and the prefixes described above can be customized. Various
customization options can be found under the customization group
`pakistan-urdu-input'."
 nil t t t t nil nil nil nil nil t)

;;;;;; Customizations
(defgroup pakistan-urdu-input nil
  "Customization group for Urdu input methods."
  :group 'quail)

(defcustom pakistan-urdu-prefixes
  '((diacritics "o")
    (poetic "G")
    (religious "M")
    (balochi-brahui "B")
    (pashto "P")
    (sindhi "C")
    (saraiki-hindko "X"))
  "Prefixes for `urdu-custom' input method."
  :set #'pakistan--set-prefixes
  :type '(repeat (list symbol string))
  :version "30.1")

(defcustom pakistan-urdu-translations
  '(("a" ?ا)
    ("y" ?ی)
    ("r" ?ر)
    ("n" ?ن)
    ("v" ?و)
    ("m" ?م)
    ("t" ?ت)
    ("l" ?ل)
    ("k" ?ک)
    ("b" ?ب)
    ("d" ?د)
    ("h" ?ہ)
    ("s" ?س)
    ("H" ?ھ)
    ("p" ?پ)
    ("N" ?ں)
    ("g" ?گ)
    ("sx" ?ش)
    ("j" ?ج)
    ("T" ?ٹ)
    ("f" ?ف)
    ("cx" ?چ)
    ("z" ?ز)
    ("u" ?ع)
    ("q" ?ق)
    ("kx" ?خ)
    ("e" ?ے)
    ("E" ?ۓ)
    ("hx" ?ح)
    ("i" ?ئ)
    ("R" ?ڑ)
    ("tx" ?ط)
    ("c" ?ص)
    ("D" ?ڈ)
    ("gx" ?غ)
    ("A" ?آ)
    ("Z" ?ض)
    ("V" ?ؤ)
    ("zx" ?ذ)
    ("S" ?ث)
    ("Zx" ?ظ)
    ("Hx" ?ۂ)
    ("ix" ?ء)
    ("Tx" ?ۃ)
    ("Y" ?ژ)
    ("ax" ?أ)
    ("." ?۔)
    ("," ?،)
    (";"  ?؛)
    ("?"  ?؟))
  "Translations for Urdu characters and common punctuations."
  :set (pakistan--make-setter)
  :type '(repeat (list string character))
  :version "30.1")

(defcustom pakistan-urdu-diacritics-and-other-symbols
  '(("a" ?َ)  ;; zabar زبر
    ("b" ?ِ)  ;; zer زير
    ("o" ?ُ)  ;; pesh پيش
    ("j" ?ْ)  ;; jazam جزم
    ("S" ?ّ)  ;; tashdid تشدید
    ("k" ?ٰ)  ;; khari zabar کھڑی زبر
    ("u" ?٘)  ;; ulti jazm الٹی جزم
    ("s" ?؎)
    ("m" ?؏)
    ("t" ?ؔ)
    ("c" ?ؐ)
    ("r" ?ؒ)
    ("R" ?ؓ)
    ("A" ?ؑ))
  "Translations to input Urdu diacrtics.
These are available under the prefix specified in `pakistan-urdu-prefixes'."
  :set (pakistan--make-setter 'diacritics)
  :type '(repeat (list string character))
  :version "30.1")

(defcustom pakistan-urdu-poetic-symbols
  '(("s" ?؎)
    ("m" ?؏)
    ("t" ?ؔ))
  "Translation to input Urdu peotic symbols.
These are available under the prefix specified in `pakistan-urdu-prefixes'."
  :set (pakistan--make-setter 'poetic)
  :type '(repeat (list string character))
  :version "30.1")

(defcustom pakistan-urdu-religious-symbols
  '(("s" ?ؐ)
    ("r" ?ؒ)
    ("R" ?ؓ)
    ("a" ?ؑ)
    ("A" ?ﷲ)
    ("S" ?ﷺ))
  "Translation to input Urdu peotic symbols.
These are available under the prefix specified in `pakistan-urdu-prefixes'."
  :set (pakistan--make-setter 'religious)
  :type '(repeat (list string character))
  :version "30.1")

;; I don't understand how many of these letters are pronounced.
;; So better translations are welcome.
(defcustom pakistan-extra-balochi-brahui-translations
  '(("v" ?ۏ)
    ("y" ?ݔ)
;; Brahui
   ("l" ?ڷ))
  "Translations to input Balochi and Brahui letters not found in Urdu.
These are available under the prefix specified in `pakistan-urdu-prefixes'."
  :set (pakistan--make-setter 'balochi-brahui)
  :type '(repeat (list string character))
  :version "30.1")

(defcustom pakistan-extra-pashto-translations
  '(("t" ?ټ)
    ("d" ?ډ)
    ("r" ?ړ)
    ("n" ?ڼ)
    ("s" ?ښ)
    ("R" ?ږ)
    ("h" ?څ)
    ("H" ?ځ))
  "Translations to input Pashto letters not found in Urdu.
These are available under the prefix specified in `pakistan-urdu-prefixes'."
  :set (pakistan--make-setter 'pashto)
  :type '(repeat (list string character))
  :version "30.1")

(defcustom pakistan-extra-sindhi-translations
  '(("k" ?ڪ)
    ("j" ?ڄ)
    ("t" ?ٺ)
    ("T" ?ٽ)
    ("tx" ?ٿ)
    ("b" ?ٻ)
    ("B" ?ڀ)
    ("r" ?ڙ)
    ("d" ?ڌ)
    ("D" ?ڏ)
    ("dx" ?ڊ)
    ("Dx" ?ڍ)
    ("h" ?ڃ)
    ("c" ?ڇ)
    ("p" ?ڦ)
    ("n" ?ڻ)
    ("g" ?ڳ)
    ("G" ?ڱ))
  "Translations to input Sindhi letters not found in Urdu.
These are available under the prefix specified in `pakistan-urdu-prefixes'."
  :set (pakistan--make-setter 'sindhi)
  :type '(repeat (list string character))
  :version "30.1")

(defcustom pakistan-extra-saraiki-hindko-translations
  '(("b" ?ٻ)
    ("h" ?ڄ)
    ("g" ?ڳ)
    ("d" ?ݙ)
    ("n" ?ݨ)
;; Hindko
    ("r" ?ݬ)
    ("v" ?ڨ)
    ("N" ?ݩ)
    ("V" ?ٷ))
"Translations to input Saraiki letters not found in Urdu.
These are available under the prefix specified in `pakistan-urdu-prefixes'."
  :set (pakistan--make-setter 'saraiki-hindko)
  :type '(repeat (list string character))
  :version "30.1")

(defcustom pakistan-urdu-use-roman-digits
  nil
  "Whether urdu-custom input method should use roman digits."
  :set #'pakistan--set-numeral-translations
  :type 'boolean
  :version "30.1")

;;;; Sindhi Input Methods
;;;;; Keyboard
;; Layout taken from https://www.branah.com/sindhi
(quail-define-package
 "sindhi-keyboard" "Sindhi" "سِ" t
 "Input method for Sindhi.
Uses keyboard layout from https://www.branah.com/sindhi ."
 nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ("q" ?ق)
 ("w" ?ص)
 ("e" ?ي)
 ("r" ?ر)
 ("t" ?ت)
 ("y" ?ٿ)
 ("u" ?ع)
 ("i" ?ڳ)
 ("o" ?و)
 ("p" ?پ)
 ("a" ?ا)
 ("s" ?س)
 ("d" ?د)
 ("f" ?ف)
 ("g" ?گ)
 ("h" ?ه)
 ("j" ?ج)
 ("k" ?ڪ)
 ("l" ?ل)
 ("z" ?ز)
 ("x" ?خ)
 ("c" ?ط)
 ("v" ?ڀ)
 ("b" ?ب)
 ("n" ?ن)
 ("m" ?م)
 ("Q" ?َ)
 ("W" ?ض)
 ("E" ?ِ)
 ("R" ?ڙ)
 ("T" ?ٽ)
 ("Y" ?ث)
 ("U" ?غ)
 ("I" ?ھ)
 ("O" ?ُ)
 ("P" ?ڦ)
 ("A" ?آ)
 ("S" ?ش)
 ("D" ?ڊ)
 ("F" ?ڦ)
 ("G" ?ً)
 ("H" ?ح)
 ("J" ?ٍ)
 ("K" ?ۡ)
 ("L" ?:)
 ("Z" ?ذ)
 ("X" ?ّ)
 ("C" ?ظ)
 ("V" ?ء)
 ("B" ?ٻ)
 ("N" ?ڻ)
 ("M" ?۾)
 ("1" ?۱)
 ("2" ?۲)
 ("3" ?۳)
 ("4" ?۴)
 ("5" ?۵)
 ("6" ?٦)
 ("7" ?۷)
 ("8" ?۸)
 ("9" ?۹)
 ("0" ?۰)
 ("`" ?’)
 ("-" ?ڏ)
 ("=" ?ڌ)
 ("~" ?‘)
 ("@" ?ى)
 ("#" ?ؔ)
 ("$" ?ؒ)
 ("%" ?٪)
 ("^" ?ؓ)
 ("&" ?۽)
 ("*" ?ؤ)
 ("(" ?\))
 (")" ?\()
 ("[" ?ڇ)
 ("]" ?چ)
 ("{" ?ڃ)
 ("}" ?ڄ)
 (";" ?ک)
 ("'" ?ڱ)
 ("\\" ?ڍ)
 (":" ?؛)
 ("|" ?ٺ)
 ("," ?،)
 ("/" ?ئ)
 ("<" ?“)
 (">" ?”)
 ("?" ?؟))


;;;; Pashto Input Methods
;;;;; Keyboard
(quail-define-package
 "pashto-keyboard" "Pashto" "پ" t
 "Input method for Pashto.
Uses keyboard layout from https://www.branah.com/pashto ."
 nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ("q" ?ض)
 ("w" ?ص)
 ("e" ?ث)
 ("r" ?ق)
 ("t" ?ف)
 ("y" ?غ)
 ("u" ?ع)
 ("i" ?ه)
 ("o" ?خ)
 ("p" ?ح)
 ("a" ?ش)
 ("s" ?س)
 ("d" ?ی)
 ("f" ?ب)
 ("g" ?ل)
 ("h" ?ا)
 ("j" ?ت)
 ("k" ?ن)
 ("l" ?م)
 ("z" ?ۍ)
 ("x" ?ې)
 ("c" ?ز)
 ("v" ?ر)
 ("b" ?ذ)
 ("n" ?د)
 ("m" ?ړ)
 ("Q" ?ْ)
 ("W" ?ٌ)
 ("E" ?ٍ)
 ("R" ?ً)
 ("T" ?ُ)
 ("Y" ?ِ)
 ("U" ?َ)
 ("I" ?ّ)
 ("O" ?څ)
 ("P" ?ځ)
 ("A" ?ښ)
 ("S" ?ﺉ)
 ("D" ?ي)
 ("F" ?پ)
 ("G" ?أ)
 ("H" ?آ)
 ("J" ?ټ)
 ("K" ?ڼ)
 ("L" ?ة)
 ("Z" ?ظ)
 ("X" ?ط)
 ("C" ?ژ)
 ("V" ?ء)
 ("B" ?‌)
 ("N" ?ډ)
 ("M" ?ؤ)
 ("1" ?۱)
 ("2" ?۲)
 ("3" ?۳)
 ("4" ?۴)
 ("5" ?۵)
 ("6" ?۶)
 ("7" ?۷)
 ("8" ?۸)
 ("9" ?۹)
 ("0" ?۰)
 ("`" ?‍)
 ("~" ?÷)
 ("@" ?٬)
 ("#" ?٫)
 ("%" ?٪)
 ("^" ?×)
 ("&" ?«)
 ("*" ?»)
 ("_" ?ـ)
 ("[" ?ج)
 ("]" ?چ)
 ("{" ?\[)
 ("}" ?\])
 (";" ?ک)
 ("'" ?ګ)
 ("\"" ?؛)
 ("|" ?٭)
 ("," ?و)
 ("." ?ږ)
 ("<" ?،)
 (">" ?.)
 ("?" ?؟))

;;; End Matter
(provide 'pakistan)
;;; pakistan.el ends here
