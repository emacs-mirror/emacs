;;; indian.el --- Quail packages for inputting Indian  -*- lexical-binding: t; -*-

;; Copyright (C) 2000-2022 Free Software Foundation, Inc.

;; Author: KAWABATA, Taichi <kawabata@m17n.org>

;; Keywords: multilingual, input method, Indian, Devanagari

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

;; History:

;; 2000.12.12
;; Totally re-written from devanagari.el to handle multiple Indian Scripts.

;;; Code:

(require 'quail)
(require 'ind-util)

;;;
;;; Input by transliteration
;;;

(defun quail-define-indian-trans-package (hashtbls pkgname
						   lang title doc)
  ;; This is a funcall to avoid `quail-update-leim-list-file'
  ;; determining that this is a quail definition (it searches for
  ;; "(quail-define-package").
  (funcall #'quail-define-package pkgname lang title t doc
	   nil nil nil nil nil nil t nil)
  (maphash
   (lambda (key val)
     (quail-defrule key (if (= (length val) 1)
			    (string-to-char val)
			  (vector val))))
   (cdr hashtbls)))

;; This needs to be seen by quail-update-leim-list-file, but cannot be
;; commented out because quail-update-leim-list-file ignores
;; commented-out lines.
(if nil
    (quail-define-package "devanagari-itrans" "Devanagari" "DevIT" t "Devanagari ITRANS"))
(quail-define-indian-trans-package
 indian-dev-itrans-v5-hash "devanagari-itrans" "Devanagari" "DevIT"
 "Devanagari transliteration by ITRANS method.")
(quail-defrule "..." ?॥)
(quail-defrule "\\'" ?॑)
(quail-defrule "\\_" ?॒)
(quail-defrule "\\__" ?_)
(quail-defrule "\\''" ?')

(if nil
    (quail-define-package "devanagari-kyoto-harvard" "Devanagari" "DevKH" t "Devanagari Kyoto-Harvard"))
(quail-define-indian-trans-package
 indian-dev-kyoto-harvard-hash
 "devanagari-kyoto-harvard" "Devanagari" "DevKH"
 "Devanagari transliteration by Kyoto-Harvard method.")

(if nil
    (quail-define-package "devanagari-aiba" "Devanagari" "DevAB" t "Devanagari Aiba"))
(quail-define-indian-trans-package
 indian-dev-aiba-hash "devanagari-aiba" "Devanagari" "DevAB"
 "Devanagari transliteration by Aiba-method.")

(if nil
    (quail-define-package "punjabi-itrans" "Punjabi" "PnjIT" t "Punjabi ITRANS"))
(quail-define-indian-trans-package
 indian-pnj-itrans-v5-hash "punjabi-itrans" "Punjabi" "PnjIT"
 "Punjabi transliteration by ITRANS method.")

(if nil
    (quail-define-package "gujarati-itrans" "Gujarati" "GjrIT" t "Gujarati ITRANS"))
(quail-define-indian-trans-package
 indian-gjr-itrans-v5-hash "gujarati-itrans" "Gujarati" "GjrIT"
 "Gujarati transliteration by ITRANS method.")

(if nil
    (quail-define-package "oriya-itrans" "Oriya" "OriIT" t "Oriya ITRANS"))
(quail-define-indian-trans-package
 indian-ori-itrans-v5-hash "oriya-itrans" "Oriya" "OriIT"
 "Oriya transliteration by ITRANS method.")

(if nil
    (quail-define-package "bengali-itrans" "Bengali" "BngIT" t "Bengali ITRANS"))
(quail-define-indian-trans-package
 indian-bng-itrans-v5-hash "bengali-itrans" "Bengali" "BngIT"
 "Bengali transliteration by ITRANS method.")

(if nil
    (quail-define-package "assamese-itrans" "Assamese" "AsmIT" t "Assamese ITRANS"))
(quail-define-indian-trans-package
 indian-asm-itrans-v5-hash "assamese-itrans" "Assamese" "AsmIT"
 "Assamese transliteration by ITRANS method.")

(if nil
    (quail-define-package "telugu-itrans" "Telugu" "TlgIT" t "Telugu ITRANS"))
(quail-define-indian-trans-package
 indian-tlg-itrans-v5-hash "telugu-itrans" "Telugu" "TlgIT"
 "Telugu transliteration by ITRANS method.")

(if nil
    (quail-define-package "kannada-itrans" "Kannada" "KndIT" t "Kannada ITRANS"))
(quail-define-indian-trans-package
 indian-knd-itrans-v5-hash "kannada-itrans" "Kannada" "KndIT"
 "Kannada transliteration by ITRANS method.")

;; ITRANS not applicable to Malayalam & could be removed eventually
(if nil
    (quail-define-package "malayalam-itrans" "Malayalam" "MlmIT" t "Malayalam ITRANS"))
(quail-define-indian-trans-package
 indian-mlm-itrans-v5-hash "malayalam-itrans" "Malayalam" "MlmIT"
 "Malayalam transliteration by ITRANS method.")

(defvar quail-tamil-itrans-syllable-table
  (let ((vowels
	 '(("அ" nil "a")
	   ("ஆ" "ா" "A")
	   ("இ" "ி" "i")
	   ("ஈ" "ீ" "I")
	   ("உ" "ு" "u")
	   ("ஊ" "ூ" "U")
	   ("எ" "ெ" "e")
	   ("ஏ" "ே" "E")
	   ("ஐ" "ை" "ai")
	   ("ஒ" "ொ" "o")
	   ("ஓ" "ோ" "O")
	   ("ஔ" "ௌ" "au")))
	(consonants
	 '(("க" "k")			; U+0B95
	   ("ங" "N^")			; U+0B99
	   ("ச" "ch")			; U+0B9A
	   ("ஞ" "JN")			; U+0B9E
	   ("ட" "T")			; U+0B9F
	   ("ண" "N")			; U+0BA3
	   ("த" "t")			; U+0BA4
	   ("ந" "n")			; U+0BA8
	   ("ப" "p")			; U+0BAA
	   ("ம" "m")			; U+0BAE
	   ("ய" "y")			; U+0BAF
	   ("ர" "r")			; U+0BB0
	   ("ல" "l")			; U+0BB2
	   ("வ" "v")			; U+0BB5
	   ("ழ" "z")			; U+0BB4
	   ("ள" "L")			; U+0BB3
	   ("ற" "rh")			; U+0BB1
	   ("ன" "nh")			; U+0BA9
	   ("ஜ" "j")			; U+0B9C
	   ("ஶ" nil)			; U+0BB6
	   ("ஷ" "Sh")			; U+0BB7
	   ("ஸ" "s")			; U+0BB8
	   ("ஹ" "h")			; U+0BB9
	   ("க்ஷ" "x" )			; U+0B95
	   ))
	(virama #x0BCD)
	clm)
    (with-temp-buffer
      (insert "\n")
      (insert "----+")
      (insert-char ?- 74)
      (insert "\n    |")
      (setq clm 6)
      (dolist (v vowels)
	(insert (propertize "\t" 'display (list 'space :align-to clm))
		(car v))
	(setq clm (+ clm 6)))
      (insert "\n    |")
      (setq clm 6)
      (dolist (v vowels)
	(insert (propertize "\t" 'display (list 'space :align-to clm))
		(nth 2 v))
	(setq clm (+ clm 6)))
      (dolist (c consonants)
	(insert "\n----+")
	(insert-char ?- 74)
	(insert "\n")
	(insert (car c) virama
		(propertize "\t" 'display '(space :align-to 4))
		"|")
	(setq clm 6)
	(dolist (v vowels)
	  (insert (propertize "\t" 'display (list 'space :align-to clm))
		  (car c) (or (nth 1 v) ""))
	  (setq clm (+ clm 6)))
	(insert "\n" (or (nth 1 c) "")
		(propertize "\t" 'display '(space :align-to 4))
		"|")
	(setq clm 6)

	(dolist (v vowels)
	  (apply #'insert (propertize "\t" 'display (list 'space :align-to clm))
		 (if (nth 1 c) (list (nth 1 c) (nth 2 v)) (list "")))
	  (setq clm (+ clm 6))))
      (insert "\n")
      (insert "----+")
      (insert-char ?- 74)
      (insert "\n")
      (buffer-string))))

(defvar quail-tamil-itrans-numerics-and-symbols-table
  (let ((numerics '((?௰ "பத்து") (?௱ "நூறு") (?௲ "ஆயிரம்")))
	(symbols '((?௳ "நாள்") (?௴ "மாதம்") (?௵ "வருடம்")
		   (?௶ "பற்று") (?௷ "வரவு") (?௸ "மேற்படி")
		   (?௹ "ரூபாய்") (?௺ "எண்")))
	clm)
    (with-temp-buffer
      (insert "\n" (make-string 18 ?-) "+" (make-string 60 ?-) "\n")
      (insert
       (propertize "\t" 'display '(space :align-to 5)) "numerics"
       (propertize "\t" 'display '(space :align-to 18)) "|"
       (propertize "\t" 'display '(space :align-to 45)) "symbols")
      (insert "\n" (make-string 18 ?-) "+" (make-string 60 ?-) "\n")
      (dotimes (i 2)
	(setq clm 0)
	(dolist (elm numerics)
	  (if (> clm 0)
	      (insert (propertize "\t" 'display (list 'space :align-to clm))))
	  (insert (nth i elm))
	  (setq clm (+ clm 5)))
	(insert (propertize "\t" 'display '(space :align-to 18)) "|")
	(setq clm 19)
	(dolist (elm symbols)
	  (if (> clm 19)
	      (insert (propertize "\t" 'display (list 'space :align-to clm))))
	  (insert (nth i elm))
	  (setq clm (+ clm 8)))
	(insert "\n"))
      (insert (make-string 18 ?-) "+" (make-string 60 ?-) "\n")
      (insert "\n")
      (buffer-string))))

(defun quail-tamil-itrans-compute-signs-table (digitp)
  "Compute the signs table for the tamil-itrans input method.
If DIGITP is non-nil, include the digits translation as well."
  (let ((various '((?ஃ . "H") ("ஸ்ரீ" . "srii") (?ௐ)))
	(digits "௦௧௨௩௪௫௬௭௮௯")
	(width 6) clm)
    (with-temp-buffer
      (insert "\n" (make-string 18 ?-) "+")
      (when digitp (insert (make-string 60 ?-)))
      (insert "\n")
      (insert
       (propertize "\t" 'display '(space :align-to 5)) "various"
       (propertize "\t" 'display '(space :align-to 18)) "|")
      (when digitp
        (insert
         (propertize "\t" 'display '(space :align-to 45)) "digits"))
      (insert "\n" (make-string 18 ?-) "+")
      (when digitp
        (insert (make-string 60 ?-)))
      (insert "\n")
      (setq clm 0)

      (dotimes (i (length various))
	(insert (propertize "\t" 'display (list 'space :align-to clm))
		(car (nth i various)))
	(setq clm (+ clm width)))
      (insert (propertize "\t" 'display '(space :align-to 18)) "|")
      (setq clm 20)
      (when digitp
        (dotimes (i 10)
	  (insert (propertize "\t" 'display (list 'space :align-to clm))
		  (aref digits i))
	  (setq clm (+ clm width))))
      (insert "\n")
      (setq clm 0)
      (dotimes (i (length various))
	(insert (propertize "\t" 'display (list 'space :align-to clm))
		(or (cdr (nth i various)) ""))
	(setq clm (+ clm width)))
      (insert (propertize "\t" 'display '(space :align-to 18)) "|")
      (setq clm 20)
      (when digitp
        (dotimes (i 10)
	  (insert (propertize "\t" 'display (list 'space :align-to clm))
		  (format "%d" i))
	  (setq clm (+ clm width))))
      (insert "\n" (make-string 18 ?-) "+")
      (when digitp
        (insert (make-string 60 ?-) "\n"))
      (buffer-string))))

(defvar quail-tamil-itrans-various-signs-and-digits-table
  (quail-tamil-itrans-compute-signs-table t))

(defvar quail-tamil-itrans-various-signs-table
  (quail-tamil-itrans-compute-signs-table nil))

(if nil
    (quail-define-package "tamil-itrans" "Tamil" "TmlIT" t "Tamil ITRANS"))
(quail-define-indian-trans-package
 indian-tml-itrans-v5-hash "tamil-itrans" "Tamil" "TmlIT"
 "Tamil transliteration by ITRANS method.

You can input characters using the following mapping tables.
    Example: To enter வணக்கம், type vaNakkam.

### Basic syllables (consonants + vowels) ###
\\=\\<quail-tamil-itrans-syllable-table>

### Miscellaneous (various signs) ###
\\=\\<quail-tamil-itrans-various-signs-table>

### Others (numerics + symbols) ###

Characters below have no ITRANS method associated with them.
Their descriptions are included for easy reference.
\\=\\<quail-tamil-itrans-numerics-and-symbols-table>

Full key sequences are listed below:")

(if nil
    (quail-define-package "tamil-itrans-digits" "Tamil" "TmlITD" t "Tamil ITRANS with digits"))
(quail-define-indian-trans-package
 indian-tml-itrans-digits-v5-hash "tamil-itrans-digits" "Tamil" "TmlITD"
 "Tamil transliteration by ITRANS method with Tamil digits support.

You can input characters using the following mapping tables.
    Example: To enter வணக்கம், type vaNakkam.

### Basic syllables (consonants + vowels) ###
\\=\\<quail-tamil-itrans-syllable-table>

### Miscellaneous (various signs + digits) ###
\\=\\<quail-tamil-itrans-various-signs-and-digits-table>

### Others (numerics + symbols) ###

Characters below have no ITRANS method associated with them.
Their descriptions are included for easy reference.
\\=\\<quail-tamil-itrans-numerics-and-symbols-table>

Full key sequences are listed below:")

;;;
;;; Input by Inscript
;;;

(defun quail-define-inscript-package (char-tables key-tables pkgname lang
                                                  title docstring)
  ;; This is a funcall to avoid `quail-update-leim-list-file'
  ;; determining that this is a quail definition (it searches for
  ;; "(quail-define-package").
  (funcall #'quail-define-package pkgname lang title nil docstring
	   nil nil nil t nil nil nil nil)
  (let (char-table key-table char key)
    (while (and char-tables key-tables)
      (setq char-table  (car char-tables)
            char-tables (cdr char-tables)
            key-table   (car key-tables)
            key-tables  (cdr key-tables))
      (while (and char-table key-table)
        (setq char       (car char-table)
              char-table (cdr char-table)
              key        (car key-table)
              key-table  (cdr key-table))
        (if (and (consp char) (consp key))
            (setq char-table (append char char-table)
                  key-table  (append key  key-table))
          (if (and key char)
              (quail-defrule
               (if (characterp key) (char-to-string key) key)
               (if (stringp char)   (vector char) char))))))))

;;

(defvar inscript-dev-keytable
  '(
    (;; VOWELS  (18)
     (?D nil) (?E ?e) (?F ?f) (?R ?r) (?G ?g) (?T ?t)
     (?+ ?=) ("F]" "f]") (?! ?@) (?Z ?z) (?S ?s) (?W ?w)
     (?| ?\\) (?~ ?`) (?A ?a) (?Q ?q) ("+]" "=]") ("R]" "r]"))
    (;; CONSONANTS (42)
     ?k ?K ?i ?I ?U                ;; GRUTTALS
     ?\; ?: ?p ?P ?}               ;; PALATALS
     ?' ?\" ?\[ ?{ ?C              ;; CEREBRALS
     ?l ?L ?o ?O ?v ?V             ;; DENTALS
     ?h ?H ?y ?Y ?c                ;; LABIALS
     ?/ ?j ?J ?n ?N "N]" ?b        ;; SEMIVOWELS
     ?M ?< ?m ?u                   ;; SIBILANTS
     "k]" "K]" "i]" "p]" "[]" "{]" "H]" "/]" ;; NUKTAS
     ?% ?&)
    (;; Misc Symbols (7)
     ?X ?x ?_ ">]" ?d "X]" ?>)
    (;; Digits
     ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)
    (;; Inscripts
     ?# ?$ ?^ ?* ?\])))

(defvar inscript-mlm-keytable
  '(
    (;; VOWELS  (18)
     (?D nil) (?E ?e) (?F ?f) (?R ?r) (?G ?g) (?T ?t)
     (?= ?+) nil nil (?S ?s) (?Z ?z) (?W ?w)
     nil (?~ ?`) (?A ?a) (?Q ?q))
    (;; CONSONANTS (42)
     ?k ?K ?i ?I ?U                ;; GRUTTALS
     ?\; ?: ?p ?P ?}               ;; PALATALS
     ?' ?\" ?\[ ?{ ?C              ;; CEREBRALS
     ?l ?L ?o ?O ?v nil            ;; DENTALS
     ?h ?H ?y ?Y ?c                ;; LABIALS
     ?/ ?j ?J ?n ?N ?B ?b          ;; SEMIVOWELS
     ?M ?< ?m ?u                   ;; SIBILANTS
     nil nil nil nil nil nil nil nil nil) ;; NUKTAS
    (;; Misc Symbols (7)
     nil ?x ?_ nil ?d)
    (;; Digits
     ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)
    (;; Chillus
     "Cd" "Cd]" "vd" "vd]" "jd" "jd]" "nd" "nd]" "Nd" "Nd]")))

(defvar inscript-tml-keytable
  '(
    (;; VOWELS  (18)
     (?D nil) (?E ?e) (?F ?f) (?R ?r) (?G ?g) (?T ?t)
     nil nil nil (?S ?s) (?Z ?z) (?W ?w)
     nil (?A ?a) (?~ ?`) (?Q ?q) nil nil)
    (;; CONSONANTS (42)
     ?k ?K ?i ?I ?U                ;; GRUTTALS
     ?\; ?: ?p ?P ?}               ;; PALATALS
     ?' ?\" ?\[ ?{ ?C              ;; CEREBRALS
     ?l ?L ?o ?O ?v ?V             ;; DENTALS
     ?h ?H ?y ?Y ?c                ;; LABIALS
     ?/ ?j ?J ?n ?N "N]" ?b        ;; SEMIVOWELS
     ?M ?< ?m ?u                   ;; SIBILANTS
     "k]" "K]" "i]" "p]" "[]" "{]" "H]" "/]" ;; NUKTAS
     ?% ?&)
    (;; Misc Symbols (7)
     ?X ?x ?_ ">]" ?d "X]" ?>)
    (;; Digits
     ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)
    (;; Inscripts
     ?# ?$ ?^ ?* ?\])))

(if nil
    (quail-define-package "devanagari-inscript" "Devanagari" "DevIS" t "Devanagari keyboard Inscript"))
(quail-define-inscript-package
 indian-dev-base-table inscript-dev-keytable
 "devanagari-inscript" "Devanagari" "DevIS"
 "Devanagari keyboard Inscript.")

(if nil
    (quail-define-package "punjabi-inscript" "Punjabi" "PnjIS" t "Punjabi keyboard Inscript"))
(quail-define-inscript-package
 indian-pnj-base-table inscript-dev-keytable
 "punjabi-inscript" "Punjabi" "PnjIS"
 "Punjabi keyboard Inscript.")

(if nil
    (quail-define-package "gujarati-inscript" "Gujarati" "GjrIS" t "Gujarati keyboard Inscript"))
(quail-define-inscript-package
 indian-gjr-base-table inscript-dev-keytable
 "gujarati-inscript" "Gujarati" "GjrIS"
 "Gujarati keyboard Inscript.")

(if nil
    (quail-define-package "oriya-inscript" "Oriya" "OriIS" t "Oriya keyboard Inscript"))
(quail-define-inscript-package
 indian-ori-base-table inscript-dev-keytable
 "oriya-inscript" "Oriya" "OriIS"
 "Oriya keyboard Inscript.")

(if nil
    (quail-define-package "bengali-inscript" "Bengali" "BngIS" t "Bengali keyboard Inscript"))
(quail-define-inscript-package
 indian-bng-base-table inscript-dev-keytable
 "bengali-inscript" "Bengali" "BngIS"
 "Bengali keyboard Inscript.")

(if nil
    (quail-define-package "assamese-inscript" "Assamese" "AsmIS" t "Assamese keyboard Inscript"))
(quail-define-inscript-package
 indian-asm-base-table inscript-dev-keytable
 "assamese-inscript" "Assamese" "AsmIS"
 "Assamese keyboard Inscript.")

(if nil
    (quail-define-package "telugu-inscript" "Telugu" "TlgIS" t "Telugu keyboard Inscript"))
(quail-define-inscript-package
 indian-tlg-base-table inscript-dev-keytable
 "telugu-inscript" "Telugu" "TlgIS"
 "Telugu keyboard Inscript.")

(if nil
    (quail-define-package "kannada-inscript" "Kannada" "KndIS" t "Kannada keyboard Inscript"))
(quail-define-inscript-package
 indian-knd-base-table inscript-dev-keytable
 "kannada-inscript" "Kannada" "KndIS"
 "Kannada keyboard Inscript.")

(if nil
    (quail-define-package "malayalam-inscript" "Malayalam" "MlmIS" t "Malayalam keyboard Inscript"))
(quail-define-inscript-package
 indian-mlm-base-table inscript-mlm-keytable
 "malayalam-inscript" "Malayalam" "MlmIS"
 "Malayalam keyboard Inscript.")

(quail-defrule "\\" ?‌)
(quail-defrule "X" ?​)

(if nil
    (quail-define-package "tamil-inscript" "Tamil" "TmlIS" t "Tamil keyboard Inscript"))
(quail-define-inscript-package
 indian-tml-base-table inscript-tml-keytable
 "tamil-inscript" "Tamil" "TmlIS"
 "Tamil keyboard Inscript.")

(if nil
    (quail-define-package "tamil-inscript-digits" "Tamil" "TmlISD" t "Tamil keyboard Inscript with digits."))
(quail-define-inscript-package
 indian-tml-base-digits-table inscript-tml-keytable
 "tamil-inscript-digits" "Tamil" "TmlISD"
 "Tamil keyboard Inscript with Tamil digits support.")

;; Probhat Input Method
(quail-define-package
 "bengali-probhat" "Bengali" "BngPB" t
 "Probhat keyboard for Bengali/Bangla" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
  ("!" ?!)
  ("1" ?১)
  ("@" ?@)
  ("2" ?২)
  ("#" ?#)
  ("3" ?৩)
  ("$" ?৳)
  ("4" ?৪)
  ("%" ?%)
  ("5" ?৫)
  ("^" ?^)
  ("6" ?৬)
  ("&" ?ঞ)
  ("7" ?৭)
  ("*" ?ৎ)
  ("8" ?৮)
  ("(" ?\()
  ("9" ?৯)
  (")" ?\))
  ("0" ?০)
  ("_" ?_)
  ("-" ?-)
  ("+" ?+)
  ("=" ?=)
  ("Q" ?ধ)
  ("q" ?দ)
  ("W" ?ঊ)
  ("w" ?ূ)
  ("E" ?ঈ)
  ("e" ?ী)
  ("R" ?ড়)
  ("r" ?র)
  ("T" ?ঠ)
  ("t" ?ট)
  ("Y" ?ঐ)
  ("y" ?এ)
  ("U" ?উ)
  ("u" ?ু)
  ("I" ?ই)
  ("i" ?ি)
  ("O" ?ঔ)
  ("o" ?ও)
  ("P" ?ফ)
  ("p" ?প)
  ("{" ?ৈ)
  ("[" ?ে)
  ("}" ?ৌ)
  ("]" ?ো)
  ("A" ?অ)
  ("a" ?া)
  ("S" ?ষ)
  ("s" ?স)
  ("D" ?ঢ)
  ("d" ?ড)
  ("F" ?থ)
  ("f" ?ত)
  ("G" ?ঘ)
  ("g" ?গ)
  ("H" ?ঃ)
  ("h" ?হ)
  ("J" ?ঝ)
  ("j" ?জ)
  ("K" ?খ)
  ("k" ?ক)
  ("L" ?ং)
  ("l" ?ল)
  (":" ?:)
  (";" ?\;)
  ("\"" ?\")
  ("'" ?')
  ("|" ?॥)
  ("" ?‌)
  ("~" ?~)
  ("`" ?‍)
  ("Z" ?য)
  ("z" ?য়)
  ("X" ?ঢ়)
  ("x" ?শ)
  ("C" ?ছ)
  ("c" ?চ)
  ("V" ?ঋ)
  ("v" ?আ)
  ("B" ?ভ)
  ("b" ?ব)
  ("N" ?ণ)
  ("n" ?ন)
  ("M" ?ঙ)
  ("m" ?ম)
  ("<" ?ৃ)
  ("," ?,)
  (">" ?ঁ)
  ("." ?।)
  ("?" ?\?)
  ("/" ?্))

(defun indian-mlm-mozhi-update-translation (control-flag)
  (let ((len (length quail-current-key)) chillu
	(vowels '(?a ?e ?i ?o ?u ?A ?E ?I ?O ?U ?R)))
    (cond ((numberp control-flag)
	   (progn (if (= control-flag 0)
		      (setq quail-current-str quail-current-key)
		    (cond (input-method-exit-on-first-char)
			  ((and (memq (aref quail-current-key
					    (1- control-flag))
				      vowels)
				(setq chillu (cl-position
					      (aref quail-current-key
						    control-flag)
					      '(?m ?N ?n ?r ?l ?L))))
			   ;; conditions for putting chillu
			   (and (or (and (= control-flag (1- len))
					 (not (setq control-flag nil)))
				    (and (= control-flag (- len 2))
					 (let ((temp (aref quail-current-key
							   (1- len))))
                                           ;; is it last char of word?
					   (not
					    (or (and (>= temp ?a) (<= temp ?z))
						(and (>= temp ?A) (<= temp ?Z))
						(eq temp ?~))))
					 (setq control-flag (1+ control-flag))))
				(setq quail-current-str     ;; put chillu
				      (concat (if (not (stringp
							quail-current-str))
						  (string quail-current-str)
						quail-current-str)
					      (string
					       (nth chillu '(?ം ?ൺ ?ൻ ?ർ ?ൽ ?ൾ)))))))))
		  (and (not input-method-exit-on-first-char) control-flag
		       (while (> len control-flag)
			 (setq len (1- len))
			 (setq unread-command-events
			       (cons (aref quail-current-key len)
				     unread-command-events))))
		  ))
	  ((null control-flag)
	   (unless quail-current-str
	     (setq quail-current-str quail-current-key)
	     ))
	  ((equal control-flag t)
	   (if (memq (aref quail-current-key (1- len))  ;; If vowel ending,
		     vowels)                            ;; may have to put
	       (setq control-flag nil)))))              ;; chillu. So don't
  control-flag)                                         ;; end translation

(quail-define-package "malayalam-mozhi" "Malayalam" "MlmMI" t
                      "Malayalam transliteration by Mozhi method."
                      nil nil t nil nil nil t nil
                      #'indian-mlm-mozhi-update-translation)

(maphash
 (lambda (key val)
   (quail-defrule key (if (= (length val) 1)
			  (string-to-char val)
			(vector val))))
 (cdr indian-mlm-mozhi-hash))

(defun indian-mlm-mozhi-underscore (_key _len) (throw 'quail-tag nil))

(quail-defrule "_" #'indian-mlm-mozhi-underscore)
(quail-defrule "|" ?‌)
(quail-defrule "||" ?​)

(quail-define-package
 "brahmi" "Brahmi" "𑀲" t "Brahmi phonetic input method.

 `\\=`' is used to switch levels instead of Alt-Gr.
" nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ("``" ?₹)
 ("1"  ?𑁧)
 ("`1" ?1)
 ("`!" ?𑁒)
 ("2"  ?𑁨)
 ("`2" ?2)
 ("`@" ?𑁓)
 ("3"  ?𑁩)
 ("`3" ?3)
 ("`#" ?𑁔)
 ("4"  ?𑁪)
 ("`4" ?4)
 ("`$" ?𑁕)
 ("5"  ?𑁫)
 ("`5" ?5)
 ("`%" ?𑁖)
 ("6"  ?𑁬)
 ("`6" ?6)
 ("`^" ?𑁗)
 ("7"  ?𑁭)
 ("`7" ?7)
 ("`&" ?𑁘)
 ("8"  ?𑁮)
 ("`8" ?8)
 ("`*" ?𑁙)
 ("9"  ?𑁯)
 ("`9" ?9)
 ("`(" ?𑁚)
 ("0"  ?𑁦)
 ("`0" ?0)
 ("`)" ?𑁛)
 ("`-" ?𑁜)
 ("`_" ?𑁝)
 ("`=" ?𑁞)
 ("`+" ?𑁟)
 ("`\\" ?𑁇)
 ("`|" ?𑁈)
 ("`"  ?𑀝)
 ("q"  ?𑀝)
 ("Q"  ?𑀞)
 ("`q" ?𑀃)
 ("`Q" ?𑁠)
 ("w"  ?𑀟)
 ("W"  ?𑀠)
 ("`w" ?𑀄)
 ("`W" ?𑁡)
 ("e"  ?𑁂)
 ("E"  ?𑁃)
 ("`e" ?𑀏)
 ("`E" ?𑀐)
 ("r"  ?𑀭)
 ("R"  ?𑀾)
 ("`r" ?𑀋)
 ("`R" ?𑀶)
 ("t"  ?𑀢)
 ("T"  ?𑀣)
 ("`t" ?𑁢)
 ("y"  ?𑀬)
 ("Y"  ?𑁣)
 ("`y" ?𑁤)
 ("`Y" ?𑁥)
 ("u"  ?𑀼)
 ("U"  ?𑀽)
 ("`u" ?𑀉)
 ("`U" ?𑀊)
 ("i"  ?𑀺)
 ("I"  ?𑀻)
 ("`i" ?𑀇)
 ("`I" ?𑀈)
 ("o"  ?𑁄)
 ("O"  ?𑁅)
 ("`o" ?𑀑)
 ("`O" ?𑀒)
 ("p"  ?𑀧)
 ("P"  ?𑀨)
 ("`p" ?𑁳)
 ("`P" ?𑁱)
 ("`[" ?𑁴)
 ("`{" ?𑁲)
 ("a"  ?𑀸)
 ("A"  ?𑀆)
 ("`a" ?𑀅)
 ("`A" ?𑀹)
 ("s"  ?𑀲)
 ("S"  ?𑀰)
 ("`s" ?𑀱)
 ("d"  ?𑀤)
 ("D"  ?𑀥)
 ("`d" ?𑀶)
 ("f"  ?𑁆)
 ("F"  ?𑀿)
 ("`f" ?𑀌)
 ("`F" ?𑁰)
 ("g"  ?𑀕)
 ("G"  ?𑀖)
 ("h"  ?𑀳)
 ("H"  ?𑀂)
 ("j"  ?𑀚)
 ("J"  ?𑀛)
 ("k"  ?𑀓)
 ("K"  ?𑀔)
 ("l"  ?𑀮)
 ("L"  ?𑀴)
 ("`l" ?𑀵)
 ("`L" ?𑁵)
 ("z"  ?𑁀)
 ("Z"  ?𑀍)
 ("`z" ?𑁁)
 ("`Z" ?𑀎)
 ("x"  ?𑁉)
 ("X"  ?𑁊)
 ("`x" ?𑁋)
 ("`X" ?𑁌)
 ("c"  ?𑀘)
 ("C"  ?𑀙)
 ("`c" #x200C)  ; ZWNJ
 ("`C" #x200D)  ; ZWJ
 ("v"  ?𑀯)
 ("V"  ?𑀷)
 ("b"  ?𑀩)
 ("B"  ?𑀪)
 ("n"  ?𑀦)
 ("N"  ?𑀡)
 ("`n" ?𑀗)
 ("`N" ?𑀜)
 ("m"  ?𑀫)
 ("M"  ?𑀁)
 ("`m" ?𑀀)
 ("<"  ?𑁍)
 ("`/" ?𑁿)
 )

(quail-define-package
 "kaithi" "Kaithi" "𑂍𑂶" t "Kaithi phonetic input method.

 `\\=`' is used to switch levels instead of Alt-Gr.
" nil t t t t nil nil nil nil nil t)

(quail-define-rules
("``" ?₹)
("1"  ?१)
("`1" ?1)
("2"  ?२)
("`2" ?2)
("3"  ?३)
("`3" ?3)
("4"  ?४)
("`4" ?4)
("5"  ?५)
("`5" ?5)
("6"  ?६)
("`6" ?6)
("7"  ?७)
("`7" ?7)
("8"  ?८)
("`8" ?8)
("9"  ?९)
("`9" ?9)
("0"  ?०)
("`0" ?0)
("`)" ?𑂻)
("`\\" ?𑃀)
("`|" ?𑃁)
("`"  ?𑂗)
("q"  ?𑂗)
("Q"  ?𑂘)
("w"  ?𑂙)
("W"  ?𑂛)
("`w" ?𑂚)
("`W" ?𑂜)
("e"  ?𑂵)
("E"  ?𑂶)
("`e" ?𑂉)
("`E" ?𑂊)
("r"  ?𑂩)
("R"  ?𑃂)
("t"  ?𑂞)
("T"  ?𑂟)
("y"  ?𑂨)
("Y"  ?⸱)
("u"  ?𑂳)
("U"  ?𑂴)
("`u" ?𑂇)
("`U" ?𑂈)
("i"  ?𑂱)
("I"  ?𑂲)
("`i" ?𑂅)
("`I" ?𑂆)
("o"  ?𑂷)
("O"  ?𑂸)
("`o" ?𑂋)
("`O" ?𑂌)
("p"  ?𑂣)
("P"  ?𑂤)
("a"  ?𑂰)
("A"  ?𑂄)
("`a" ?𑂃)
("s"  ?𑂮)
("S"  ?𑂬)
("d"  ?𑂠)
("D"  ?𑂡)
("`d" ?𑂼)
("`D" #x110BD) ; Kaithi Number Sign
("f"  ?𑂹)
("F" #x110CD) ; Kaithi Number Sign Above
("`f" ?𑂾)
("`F" ?𑂿)
("g"  ?𑂏)
("G"  ?𑂐)
("h"  ?𑂯)
("H"  ?𑂂)
("j"  ?𑂔)
("J"  ?𑂕)
("k"  ?𑂍)
("K"  ?𑂎)
("l"  ?𑂪)
("z"  ?𑂖)
("Z"  ?𑂑)
("x"  ?𑂭)
("X"  ?𑂺)
("c"  ?𑂒)
("C"  ?𑂓)
("`c" #x200C)  ; ZWNJ
("`C" #x200D)  ; ZWJ
("v"  ?𑂫)
("b"  ?𑂥)
("B"  ?𑂦)
("n"  ?𑂢)
("N"  ?𑂝)
("m"  ?𑂧)
("M"  ?𑂁)
("`m" ?𑂀)
)

(quail-define-package
 "tirhuta" "Tirhuta" "𑒞𑒱" t "Tirhuta phonetic input method.

 `\\=`' is used to switch levels instead of Alt-Gr.
" nil t t t t nil nil nil nil nil t)

(quail-define-rules
("``" ?₹)
("1"  ?𑓑)
("`1" ?1)
("2"  ?𑓒)
("`2" ?2)
("3"  ?𑓓)
("`3" ?3)
("4"  ?𑓔)
("`4" ?4)
("5"  ?𑓕)
("`5" ?5)
("6"  ?𑓖)
("`6" ?6)
("7"  ?𑓗)
("`7" ?7)
("8"  ?𑓘)
("`8" ?8)
("9"  ?𑓙)
("`9" ?9)
("0"  ?𑓐)
("`0" ?0)
("`)" ?𑓆)
("`\\" ?।)
("`|" ?॥)
("`"  ?𑒙)
("q"  ?𑒙)
("Q"  ?𑒚)
("w"  ?𑒛)
("W"  ?𑒜)
("e"  ?𑒺)
("E"  ?𑒹)
("`e" ?𑒋)
("r"  ?𑒩)
("R"  ?𑒵)
("`r" ?𑒇)
("t"  ?𑒞)
("T"  ?𑒟)
("y"  ?𑒨)
("Y"  ?𑒻)
("`y" ?𑒌)
("u"  ?𑒳)
("U"  ?𑒴)
("`u" ?𑒅)
("`U" ?𑒆)
("i"  ?𑒱)
("I"  ?𑒲)
("`i" ?𑒃)
("`I" ?𑒄)
("o"  ?𑒽)
("O"  ?𑒼)
("`o" ?𑒍)
("p"  ?𑒣)
("P"  ?𑒤)
("a"  ?𑒰)
("A"  ?𑒂)
("`a" ?𑒁)
("s"  ?𑒮)
("S"  ?𑒬)
("d"  ?𑒠)
("D"  ?𑒡)
("f"  ?𑓂)
("F" ?𑒶)
("`f" ?𑒈)
("g"  ?𑒑)
("G"  ?𑒒)
("h"  ?𑒯)
("H"  ?𑓁)
("j"  ?𑒖)
("J"  ?𑒗)
("k"  ?𑒏)
("K"  ?𑒐)
("l"  ?𑒪)
("L" ?𑒷)
("`l" ?𑒉)
("z"  ?𑒘)
("Z"  ?𑒓)
("`z" ?𑒸)
("`Z" ?𑒊)
("x"  ?𑒭)
("X"  ?𑓃)
("c"  ?𑒔)
("C"  ?𑒕)
("`c" #x200C)  ; ZWNJ
("v"  ?𑒫)
("V" ?𑒾)
("`v" ?𑒎)
("b"  ?𑒥)
("B"  ?𑒦)
("`b" ?𑒀)
("`B" ?𑓄)
("n"  ?𑒢)
("N"  ?𑒝)
("`n" ?𑓇)
("`N" ?𑓅)
("m"  ?𑒧)
("M"  ?𑓀)
("`m" ?𑒿)
)

(quail-define-package
 "sharada" "Sharada" "𑆯𑆳" t "Sharada phonetic input method.

 `\\=`' is used to switch levels instead of Alt-Gr.
" nil t t t t nil nil nil nil nil t)

(quail-define-rules
("``" ?₹)
("1"  ?𑇑)
("`1" ?1)
("2"  ?𑇒)
("`2" ?2)
("3"  ?𑇓)
("`3" ?3)
("4"  ?𑇔)
("`4" ?4)
("5"  ?𑇕)
("`5" ?5)
("6"  ?𑇖)
("`6" ?6)
("7"  ?𑇗)
("`7" ?7)
("8"  ?𑇘)
("`8" ?8)
("9"  ?𑇙)
("`9" ?9)
("0"  ?𑇐)
("`0" ?0)
("`)" ?𑇇)
("`\\" ?𑇅)
("`|" ?𑇆)
("`"  ?𑆛)
("q"  ?𑆛)
("Q"  ?𑆜)
("`q" ?𑇈)
("`Q" ?𑇉)
("w"  ?𑆝)
("W"  ?𑆞)
("`w" ?𑇋)
("`W" ?𑇍)
("e"  ?𑆼)
("E"  ?𑆽)
("`e" ?𑆍)
("`E" ?𑆎)
("r"  ?𑆫)
("R"  ?𑆸)
("`r" ?𑆉)
("`R" ?𑇎)
("t"  ?𑆠)
("T"  ?𑆡)
("y"  ?𑆪)
("u"  ?𑆶)
("U"  ?𑆷)
("`u" ?𑆇)
("`U" ?𑆈)
("i"  ?𑆴)
("I"  ?𑆵)
("`i" ?𑆅)
("`I" ?𑆆)
("o"  ?𑆾)
("O"  ?𑆿)
("`o" ?𑆏)
("`O" ?𑆐)
("p"  ?𑆥)
("P"  ?𑆦)
("`p" ?𑇃)
("a"  ?𑆳)
("A"  ?𑆄)
("`a" ?𑆃)
("s"  ?𑆱)
("S"  ?𑆯)
("d"  ?𑆢)
("D"  ?𑆣)
("`d" ?𑇚)
("`D" ?𑇛)
("f"  ?𑇀)
("F" ?𑆹)
("`f" ?𑆊)
("`F" ?𑇌)
("g"  ?𑆓)
("G"  ?𑆔)
("`g" ?𑇜)
("`G" ?𑇝)
("h"  ?𑆲)
("H"  ?𑆂)
("`h" ?𑇞)
("`H" ?𑇟)
("j"  ?𑆘)
("J"  ?𑆙)
("`j" ?᳘)
("`J" ?᳕)
("k"  ?𑆑)
("K"  ?𑆒)
("`k" ?𑇂)
("l"  ?𑆬)
("L" ?𑆭)
("`l" ?𑆺)
("`L" ?𑆋)
("z"  ?𑆚)
("Z"  ?𑆕)
("`z" ?𑆻)
("`Z" ?𑆌)
("x"  ?𑆰)
("X"  ?𑇊)
("c"  ?𑆖)
("C"  ?𑆗)
("`c" #x200C)  ; ZWNJ
("v"  ?𑆮)
("b"  ?𑆧)
("B"  ?𑆨)
("n"  ?𑆤)
("N"  ?𑆟)
("`n" ?𑇄)
("`N" ?𑇁)
("m"  ?𑆩)
("M"  ?𑆁)
("`m" ?𑆀)
("`M" ?𑇏)
)

(quail-define-package
 "siddham" "Sharada" "𑖭𑖰" t "Siddham phonetic input method.

 `\\=`' is used to switch levels instead of Alt-Gr.
" nil t t t t nil nil nil nil nil t)

(quail-define-rules
("``" ?₹)
("`1" ?𑗊)
("`!" ?𑗔)
("`2" ?𑗋)
("`@" ?𑗕)
("`3" ?𑗌)
("`#" ?𑗖)
("`4" ?𑗍)
("`$" ?𑗗)
("`5" ?𑗎)
("`%" ?𑗅)
("`6" ?𑗏)
("`^" ?𑗆)
("`7" ?𑗐)
("`&" ?𑗇)
("`8" ?𑗑)
("`*" ?𑗈)
("`9" ?𑗒)
("`(" ?𑗉)
("`0" ?𑗓)
("`)" ?𑗄)
("`\\" ?𑗂)
("`|" ?𑗃)
("`"  ?𑖘)
("q"  ?𑖘)
("Q"  ?𑖙)
("`q" ?𑗘)
("`Q" ?𑗙)
("w"  ?𑖚)
("W"  ?𑖛)
("`w" ?𑗚)
("`W" ?𑗛)
("e"  ?𑖸)
("E"  ?𑖹)
("`e" ?𑖊)
("`E" ?𑖋)
("r"  ?𑖨)
("R"  ?𑖴)
("`r" ?𑖆)
("t"  ?𑖝)
("T"  ?𑖞)
("`t" ?𑗜)
("`T" ?𑗝)
("y"  ?𑖧)
("u"  ?𑖲)
("U"  ?𑖳)
("`u" ?𑖄)
("`U" ?𑖅)
("i"  ?𑖰)
("I"  ?𑖱)
("`i" ?𑖂)
("`I" ?𑖃)
("o"  ?𑖺)
("O"  ?𑖻)
("`o" ?𑖌)
("`O" ?𑖍)
("p"  ?𑖢)
("P"  ?𑖣)
("a"  ?𑖯)
("A"  ?𑖁)
("`a" ?𑖀)
("s"  ?𑖭)
("S"  ?𑖫)
("d"  ?𑖟)
("D"  ?𑖠)
("`d" ?𑗁)
("f"  ?𑖿)
("F" ?𑖵)
("`f" ?𑖇)
("g"  ?𑖐)
("G"  ?𑖑)
("h"  ?𑖮)
("H"  ?𑖾)
("j"  ?𑖕)
("J"  ?𑖖)
("k"  ?𑖎)
("K"  ?𑖏)
("l"  ?𑖩)
("L" ?𑖈)
("`l" ?𑖉)
("z"  ?𑖗)
("Z"  ?𑖒)
("x"  ?𑖬)
("X"  ?𑗀)
("c"  ?𑖓)
("C"  ?𑖔)
("`c" #x200C)  ; ZWNJ
("v"  ?𑖪)
("b"  ?𑖤)
("B"  ?𑖥)
("n"  ?𑖡)
("N"  ?𑖜)
("m"  ?𑖦)
("M"  ?𑖽)
("`m" ?𑖼)
)


(quail-define-package
 "syloti-nagri" "Syloti Nagri" "ꠍꠤ" t "Syloti Nagri phonetic input method.

 `\\=`' is used to switch levels instead of Alt-Gr.
" nil t t t t nil nil nil nil nil t)

(quail-define-rules
("``" ?₹)
("`~" ?৳)
("1"  ?১)
("`1" ?1)
("2"  ?২)
("`2" ?2)
("3"  ?৩)
("`3" ?3)
("4"  ?৪)
("`4" ?4)
("5"  ?৫)
("`5" ?5)
("6"  ?৬)
("`6" ?6)
("7"  ?৭)
("`7" ?7)
("8"  ?৮)
("`8" ?8)
("9"  ?৯)
("`9" ?9)
("0"  ?০)
("`0" ?0)
("`\\" ?𑇅)
("`|" ?𑇆)
("`"  ?ꠐ)
("q"  ?ꠐ)
("Q"  ?ꠑ)
("`q" ?꠨)
("`Q" ?꠩)
("w"  ?ꠒ)
("W"  ?ꠓ)
("`w" ?꠪)
("`W" ?꠫)
("e"  ?ꠦ)
("E"  ?ꠄ)
("r"  ?ꠞ)
("R"  ?ꠠ)
("t"  ?ꠔ)
("T"  ?ꠕ)
("y"  ?ꠂ)
("u"  ?ꠥ)
("U"  ?ꠃ)
("i"  ?ꠤ)
("I"  ?ꠁ)
("o"  ?ꠧ)
("O"  ?ꠅ)
("p"  ?ꠙ)
("P"  ?ꠚ)
("a"  ?ꠣ)
("A"  ?ꠀ)
("s"  ?ꠡ)
("d"  ?ꠖ)
("D"  ?ꠗ)
("f"  ?꠆)
("F" ?꠬)
("g"  ?ꠉ)
("G"  ?ꠊ)
("h"  ?ꠢ)
("j"  ?ꠎ)
("J"  ?ꠏ)
("k"  ?ꠇ)
("K"  ?ꠈ)
("l"  ?ꠟ)
("c"  ?ꠌ)
("C"  ?ꠍ)
("`c" #x200C)  ; ZWNJ
("b"  ?ꠛ)
("B"  ?ꠜ)
("n"  ?ꠘ)
("m"  ?ꠝ)
("M"  ?ꠋ)
)

;;; indian.el ends here
