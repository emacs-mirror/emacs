;;; indian.el --- Quail packages for inputting Indian  -*- lexical-binding: t; -*-

;; Copyright (C) 2000-2024 Free Software Foundation, Inc.

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

(require 'pcase)
(require 'seq)
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

;; This is needed since the Unicode codepoint order does not reflect
;; the actual order in the Tamil language.
(defvar quail-tamil-itrans--consonant-order
  '("க" "ங" "ச" "ஞ" "ட" "ண"
    "த" "ந" "ப" "ம" "ய" "ர"
    "ல" "வ" "ழ" "ள" "ற" "ன"
    "ஜ" "ஸ" "ஷ" "ஹ" "க்ஷ"
    "க்‌ஷ" "ஶ"))

(defun quail-tamil-itrans-compute-syllable-table (vowels consonants)
  "Return the syllable table for the input method as a string.
VOWELS is a list of (VOWEL SIGN INPUT-SEQ) where VOWEL is the
Tamil vowel character, SIGN is the vowel sign corresponding to
that vowel character or nil for none, and INPUT-SEQ is the input
sequence to insert VOWEL.

CONSONANTS is a list of (CONSONANT INPUT-SEQ...) where CONSONANT
is the Tamil consonant character, and INPUT-SEQ is one or more
strings that describe how to insert CONSONANT."
  (setq vowels (sort vowels
                     (lambda (x y)
                       (string-lessp (car x) (car y)))))
  (setq consonants
     (sort consonants
         (lambda (x y)
           (or (seq-position (car x) quail-tamil-itrans--consonant-order) 1000)
           (or (seq-position (car y) quail-tamil-itrans--consonant-order) 1000))))
  (let ((virama #x0BCD)
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
        (dolist (ct (cdr c))
	  (insert "\n" (or ct "")
		  (propertize "\t" 'display '(space :align-to 4))
		  "|")
	  (setq clm 6)
          (dolist (v vowels)
	    (apply #'insert (propertize "\t" 'display (list 'space :align-to clm))
		   (if ct (list ct (nth 2 v)) (list "")))
	    (setq clm (+ clm 6)))))
      (insert "\n")
      (insert "----+")
      (insert-char ?- 74)
      (insert "\n")
      (buffer-string))))

(defvar quail-tamil-itrans-syllable-table
  (quail-tamil-itrans-compute-syllable-table
   (let ((vowels (car indian-tml-base-table))
         trans v ret)
     (dotimes (i (length vowels))
       (when (setq v (nth i vowels))
         (when (characterp (car v))
           (setcar v (string (car v))))
         (setq trans (nth i (car indian-itrans-v5-table-for-tamil)))
         (push (append v (list (if (listp trans) (car trans) trans)))
               ret)))
     ret)
   (let ((consonants (cadr indian-tml-base-table))
         trans c ret)
     (dotimes (i (length consonants))
       (when (setq c (nth i consonants))
         (when (characterp c)
           (setq c (string c)))
         (setq trans (nth i (cadr indian-itrans-v5-table-for-tamil)))
         (push (cons c (if (listp trans) trans (list trans)))
               ret)))
     (setq ret (nreverse ret))
     ret)))

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

(defun quail-tamil-itrans-compute-signs-table (digitp various)
  "Compute the signs table for the tamil-itrans input method.
If DIGITP is non-nil, include the digits translation as well.
If VARIOUS is non-nil, then it should a list of (CHAR TRANS)
where CHAR is the character/string to translate and TRANS is
CHAR's translation."
  (let ((digits "௦௧௨௩௪௫௬௭௮௯")
	(width 6) clm)
    (with-temp-buffer
      (insert "\n" (make-string 18 ?-))
      (when digitp
        (insert "+" (make-string 60 ?-)))
      (insert "\n")
      (insert
       (propertize "\t" 'display '(space :align-to 5)) "various"
       (propertize "\t" 'display '(space :align-to 18)))
      (when digitp
        (insert
          "|" (propertize "\t" 'display '(space :align-to 45)) "digits"))
      (insert "\n" (make-string 18 ?-))
      (when digitp
        (insert "+" (make-string 60 ?-)))
      (insert "\n")
      (setq clm 0)

      (dotimes (i (length various))
	(insert (propertize "\t" 'display (list 'space :align-to clm))
		(car (nth i various)))
	(setq clm (+ clm width)))
      (when digitp
        (insert (propertize "\t" 'display '(space :align-to 18)) "|"))
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
		(or (cadr (nth i various)) ""))
	(setq clm (+ clm width)))
      (when digitp
        (insert (propertize "\t" 'display '(space :align-to 18)) "|"))
      (setq clm 20)
      (when digitp
        (dotimes (i 10)
	  (insert (propertize "\t" 'display (list 'space :align-to clm))
		  (format "%d" i))
	  (setq clm (+ clm width))))
      (insert "\n" (make-string 18 ?-))
      (when digitp
        (insert "+" (make-string 60 ?-) "\n"))
      (buffer-string))))

(defvar quail-tamil-itrans-various-signs-and-digits-table
  (quail-tamil-itrans-compute-signs-table
   t '((?ஃ "H") ("ஸ்ரீ" "srii") (?ௐ "OM"))))

(defvar quail-tamil-itrans-various-signs-table
  (quail-tamil-itrans-compute-signs-table
   nil '((?ஃ "H") ("ஸ்ரீ" "srii") (?ௐ "OM"))))

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
;;; Tamil phonetic input method
;;;

;; Define the input method straight away.
(quail-define-package "tamil-phonetic" "Tamil" "ழ" t
 "Customizable Tamil phonetic input method.
To change the translation rules of the input method, customize
`tamil-translation-rules'.

To use native Tamil digits, customize `tamil-translation-rules'
accordingly.

To end the current translation process, say \\<quail-translation-keymap>\\[quail-select-current] (defined in
`quail-translation-keymap').  This is useful when there's a
conflict between two possible translation.

The current input scheme is:

### Basic syllables (உயிர்மெய் எழுத்துக்கள்) ###
\\=\\<tamil--syllable-table>

### Miscellaneous ####
\\=\\<tamil--signs-table>

The following characters have NO input sequence associated with
them by default.  Their descriptions are included for easy
reference.
\\=\\<quail-tamil-itrans-numerics-and-symbols-table>

Full key sequences are listed below:"
 nil nil nil nil nil nil t)

(defvar tamil--syllable-table nil)
(defvar tamil--signs-table nil)
(defvar tamil--hashtables
  (cons (make-hash-table :test #'equal)
        (make-hash-table :test #'equal)))
(defvar tamil--vowel-signs
  '(("அ" . t) ("ஆ" . ?ா) ("இ" . ?ி) ("ஈ" . ?ீ)
    ("உ" . ?ு) ("ஊ" . ?ூ) ("எ" . ?ெ) ("ஏ" . ?ே)
    ("ஐ" . ?ை) ("ஒ" . ?ொ) ("ஓ" . ?ோ) ("ஔ" . ?ௌ)))

(defun tamil--setter (sym val)
  (set-default sym val)
  (tamil--update-quail-rules val))

(defun tamil--make-tables (rules)
  (let (v v-table v-trans
          c-table c-trans
          m-table m-trans)
    (dolist (ch rules)
      (cond
       ;; Vowel.
       ((setq v (assoc-default (car ch) tamil--vowel-signs))
        (push (list (car ch) (and (characterp v) v)) v-table)
        (push (cdr ch) v-trans))
       ;; Consonant.  It needs to end with pulli.
       ((string-suffix-p "்" (car ch))
        ;; Strip the pulli now.
        (push (substring (car ch) 0 -1) c-table)
        (push (cdr ch) c-trans))
       ;; If nothing else, then consider it a misc character.
       (t (push (car ch) m-table)
          (push (cdr ch) m-trans))))
    (list v-table v-trans c-table c-trans m-table m-trans)))

(defun tamil--update-quail-rules (rules &optional name)
  ;; This function does pretty much what `indian-make-hash' does
  ;; except that we don't try to copy the structure of
  ;; `indian-tml-base-table' which leads to less code hassle.
  (let* ((quail-current-package (assoc (or name "tamil-phonetic") quail-package-alist))
         (tables (tamil--make-tables rules))
         (v (nth 0 tables))
         (v-trans (nth 1 tables))
         (c (nth 2 tables))
         (c-trans (nth 3 tables))
         (m (nth 4 tables))
         (m-trans (nth 5 tables))
         (pulli (string #x0BCD)))
    (clrhash (car tamil--hashtables))
    (clrhash (cdr tamil--hashtables))
    (indian--puthash-v v v-trans tamil--hashtables)
    (indian--puthash-c c c-trans pulli tamil--hashtables)
    (indian--puthash-cv c c-trans v v-trans tamil--hashtables)
    (indian--puthash-m m m-trans tamil--hashtables)
    ;; Now override the current translation rules.
    ;; Empty quail map is '(list nil)'.
    (setf (nth 2 quail-current-package) '(nil))
    (maphash (lambda (k v)
               (quail-defrule k (if (length= v 1)
                                    (string-to-char v)
                                  (vector v))))
             (cdr tamil--hashtables))
    (setq  tamil--syllable-table
           (quail-tamil-itrans-compute-syllable-table
            (mapcar (lambda (ch) (append ch (pop v-trans))) v)
            (mapcar (lambda (ch) (cons ch (pop c-trans))) c))
           tamil--signs-table
           (quail-tamil-itrans-compute-signs-table
            nil
            (append (mapcar (lambda (ch) (cons ch (pop m-trans))) m)
                    (and (gethash "ஸ்" (car tamil--hashtables))
                         `(("ஸ்ரீ" ,(concat (gethash "ஸ்" (car tamil--hashtables))
                                          (gethash "ரீ" (car tamil--hashtables)))))))))))

(defgroup tamil-input nil
  "Translation rules for the Tamil input method."
  :prefix "tamil-"
  :group 'leim)

(defcustom tamil-translation-rules
  ;; Vowels.
  '(("அ" "a") ("ஆ" "aa") ("இ" "i") ("ஈ" "ii")
    ("உ" "u") ("ஊ" "uu") ("எ" "e") ("ஏ" "ee")
    ("ஐ" "ai") ("ஒ" "o") ("ஓ" "oo") ("ஔ" "au" "ow")

    ;; Consonants.
    ("க்" "k" "g") ("ங்" "ng") ("ச்" "ch" "s") ("ஞ்" "nj") ("ட்" "t" "d")
    ("ண்" "N") ("த்" "th" "dh") ("ந்" "nh") ("ப்" "p" "b") ("ம்" "m")
    ("ய்" "y") ("ர்" "r") ("ல்" "l") ("வ்" "v") ("ழ்" "z" "zh")
    ("ள்" "L") ("ற்" "rh") ("ன்" "n")
    ;; Sanskrit.
    ("ஜ்" "j") ("ஸ்" "S") ("ஷ்" "sh") ("ஹ்" "h")
    ("க்‌ஷ்" "ksh") ("க்ஷ்" "ksH") ("ஶ்" "Z")

    ;; Misc.  ஃ is neither a consonant nor a vowel.
    ("ஃ" "F" "q")
    ("ௐ" "OM"))
  "List of input sequences to translate to Tamil characters.
Each element should be (CHARACTER INPUT-SEQUENCES...) where
CHARACTER is the Tamil character, and INPUT-SEQUENCES are one
or more input sequences which produce that character.

CHARACTER is considered as a consonant (மெய் எழுத்து) if it ends
with a pulli (virama).

CHARACTER that is neither a vowel nor a consonant is inserted as
is."
  :group 'tamil-input
  :type '(alist :key-type string :value-type (repeat string))
  :set #'tamil--setter
  :version "29.1"
  :options
  (delq nil
        (append (mapcar #'car tamil--vowel-signs)
                (mapcar (lambda (x) (if (characterp x)
                                        (string x #x0BCD)
                                      (and x (concat x "்"))))
                        (nth 1 indian-tml-base-table))
                '("ஃ" "ௐ")
                ;; Digits.
                (mapcar #'string (nth 3 indian-tml-base-digits-table)))))

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

;; Tamil99 input method
;;
;; Tamil99 is a keyboard layout and input method that is specifically
;; designed for the Tamil language.  Vowels and vowel modifiers are
;; input with your left hand, and consonants are input with your right
;; hand. See https://en.wikipedia.org/wiki/Tamil_99
;;
;; தமிழ்99 உள்ளீட்டு முறை
;;
;; தமிழ்99 தமிழுக்கென்றே உருவாக்கப்பட்ட விசைப்பலகை அமைப்பும் உள்ளீட்டு முறையும்
;; ஆகும். உயிர்களை இடக்கையுடனும் மெய்களை வலக்கையுடனும் தட்டச்சிடும்படி
;; அமைக்கப்பட்டது. https://ta.wikipedia.org/wiki/%E0%AE%A4%E0%AE%AE%E0%AE%BF%E0%AE%B4%E0%AF%8D_99
;; காண்க.

(quail-define-package
 "tamil99" "Tamil" "தமிழ்99"
 t "Tamil99 input method"
 nil t t t t nil nil nil nil nil t)

(defconst tamil99-vowels
  '(("q" "ஆ")
    ("w" "ஈ")
    ("e" "ஊ")
    ("r" "ஐ")
    ("t" "ஏ")
    ("a" "அ")
    ("s" "இ")
    ("d" "உ")
    ("g" "எ")
    ("z" "ஔ")
    ("x" "ஓ")
    ("c" "ஒ"))
  "Mapping for vowels.")

(defconst tamil99-vowel-modifiers
  '(("q" "ா")
    ("w" "ீ")
    ("e" "ூ")
    ("r" "ை")
    ("t" "ே")
    ("a" "")
    ("s" "ி")
    ("d" "ு")
    ("g" "ெ")
    ("z" "ௌ")
    ("x" "ோ")
    ("c" "ொ")
    ("f" "்"))
  "Mapping for vowel modifiers.")

(defconst tamil99-hard-consonants
  '(("h" "க")
    ("[" "ச")
    ("o" "ட")
    ("l" "த")
    ("j" "ப")
    ("u" "ற"))
  "Mapping for hard consonants (வல்லினம்).")

(defconst tamil99-soft-consonants
  '(("b" "ங")
    ("]" "ஞ")
    ("p" "ண")
    (";" "ந")
    ("k" "ம")
    ("i" "ன"))
  "Mapping for soft consonants (மெல்லினம்).")

(defconst tamil99-medium-consonants
  '(("'" "ய")
    ("m" "ர")
    ("n" "ல")
    ("v" "வ")
    ("/" "ழ")
    ("y" "ள"))
  "Mapping for medium consonants (இடையினம்).")

(defconst tamil99-grantham-consonants
  '(("Q" "ஸ")
    ("W" "ஷ")
    ("E" "ஜ")
    ("R" "ஹ"))
  "Mapping for grantham consonants (கிரந்தம்).")

(defconst tamil99-consonants
  (append tamil99-hard-consonants
          tamil99-soft-consonants
          tamil99-medium-consonants
          tamil99-grantham-consonants)
  "Mapping for all consonants.")

(defconst tamil99-other
  `(("T" ,(vector "க்ஷ"))
    ("Y" ,(vector "ஶஂரீ"))
    ("O" "[")
    ("P" "]")
    ("A" "௹")
    ("S" "௺")
    ("D" "௸")
    ("F" "ஃ")
    ("K" "\"")
    ("L" ":")
    (":" ";")
    ("\"" "'")
    ("Z" "௳")
    ("X" "௴")
    ("C" "௵")
    ("V" "௶")
    ("B" "௷")
    ("M" "/"))
  "Mapping for miscellaneous characters.")

;; உயிர்
;; vowel
(mapc (pcase-lambda (`(,vowel-key ,vowel))
        (quail-defrule vowel-key vowel))
      tamil99-vowels)

(mapc (pcase-lambda (`(,consonant-key ,consonant))
        ;; அகர உயிர்மெய்
        ;; consonant symbol (consonant combined with the first vowel அ)
        (quail-defrule consonant-key consonant)
        ;; மெய்யொற்று பின் அகர உயிர்மெய்
        ;; pulli on double consonant
        (quail-defrule (concat consonant-key consonant-key)
                       (vector (concat consonant "்" consonant)))
        (mapc (pcase-lambda (`(,vowel-key ,vowel-modifier))
                ;; உயிர்மெய்
                ;; vowelised consonant
                (quail-defrule (concat consonant-key vowel-key)
                               (vector (concat consonant vowel-modifier)))
                ;; மெய்யொற்று பின் பிற உயிர்மெய்
                ;; vowelised consonant after double consonant
                (quail-defrule (concat consonant-key consonant-key vowel-key)
                               (vector (concat consonant "்" consonant vowel-modifier))))
              tamil99-vowel-modifiers))
      tamil99-consonants)

(seq-mapn (pcase-lambda (`(,soft-consonant-key ,soft-consonant)
                         `(,hard-consonant-key ,hard-consonant))
            ;; மெல்லினம் பின் வல்லினம்
            ;; hard consonant after soft consonant
            (quail-defrule (concat soft-consonant-key hard-consonant-key)
                           (vector (concat soft-consonant "்" hard-consonant)))
            (mapc (pcase-lambda (`(,vowel-key ,vowel-modifier))
                    ;; மெல்லின ஒற்றொட்டிய வல்லினம் பின் உயிர்மெய்
                    ;; vowelised consonant after soft-hard consonant pair
                    (quail-defrule (concat soft-consonant-key hard-consonant-key vowel-key)
                                   (vector (concat soft-consonant "்" hard-consonant vowel-modifier))))
                  tamil99-vowel-modifiers))
          tamil99-soft-consonants
          tamil99-hard-consonants)

;; பிற வரியுருக்கள்
;; other characters
(mapc (pcase-lambda (`(,key ,translation))
        (quail-defrule key translation))
      tamil99-other)

;; Probhat Input Method
(quail-define-package
 "bengali-probhat" "Bengali" "BngPB" t
 "Probhat keyboard for Bengali/Bangla" nil t nil t t nil nil nil nil nil t)

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
                      #'indian-mlm-mozhi-update-translation nil t)

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

(quail-define-package
 "modi" "Modi" "𑘦𑘻" t "Modi phonetic input method.

 `\\=`' is used to switch levels instead of Alt-Gr.
" nil t t t t nil nil nil nil nil t)

(quail-define-rules
("``" ?₹)
("1"  ?𑙑)
("`1" ?1)
("2"  ?𑙒)
("`2" ?2)
("3"  ?𑙓)
("`3" ?3)
("4"  ?𑙔)
("`4" ?4)
("5"  ?𑙕)
("`5" ?5)
("6"  ?𑙖)
("`6" ?6)
("7"  ?𑙗)
("`7" ?7)
("8"  ?𑙘)
("`8" ?8)
("9"  ?𑙙)
("`9" ?9)
("0"  ?𑙐)
("`0" ?0)
("`)" ?𑙃)
("`\\" ?𑙁)
("`|" ?𑙂)
("`"  ?𑘘)
("q"  ?𑘘)
("Q"  ?𑘙)
("`q" ?𑙄)
("w"  ?𑘚)
("W"  ?𑘛)
("e"  ?𑘹)
("E"  ?𑘺)
("`e" ?𑘊)
("`E" ?𑘋)
("r"  ?𑘨)
("R"  ?𑘵)
("`r" ?𑘆)
("t"  ?𑘝)
("T"  ?𑘞)
("y"  ?𑘧)
("u"  ?𑘳)
("U"  ?𑘴)
("`u" ?𑘄)
("`U" ?𑘅)
("i"  ?𑘱)
("I"  ?𑘲)
("`i" ?𑘂)
("`I" ?𑘃)
("o"  ?𑘻)
("O"  ?𑘼)
("`o" ?𑘌)
("`O" ?𑘍)
("p"  ?𑘢)
("P"  ?𑘣)
("a"  ?𑘰)
("A"  ?𑘁)
("`a" ?𑘀)
("s"  ?𑘭)
("S"  ?𑘫)
("d"  ?𑘟)
("D"  ?𑘠)
("f"  ?𑘿)
("F"  ?𑘶)
("`f" ?𑘇)
("g"  ?𑘐)
("G"  ?𑘑)
("h"  ?𑘮)
("H"  ?𑘾)
("j"  ?𑘕)
("J"  ?𑘖)
("k"  ?𑘎)
("K"  ?𑘏)
("l"  ?𑘩)
("L"  ?𑘯)
("`l" ?𑘷)
("`L" ?𑘈)
("z"  ?𑘗)
("Z"  ?𑘒)
("`z" ?𑘸)
("`Z" ?𑘉)
("x"  ?𑘬)
("X"  ?𑙀)
("c"  ?𑘓)
("C"  ?𑘔)
("`c" #x200C)  ; ZWNJ
("v"  ?𑘪)
("b"  ?𑘤)
("B"  ?𑘥)
("n"  ?𑘡)
("N"  ?𑘜)
("m"  ?𑘦)
("M"  ?𑘽)
)

(quail-define-package
 "odia" "Odia" "ଓ" t "Odia phonetic input method.

 `\\=`' is used to switch levels instead of Alt-Gr.
" nil t t t t nil nil nil nil nil t)

(quail-define-rules
("``" ?₹)
("1"  ?୧)
("`1" ?1)
("`!" ?୲)
("2"  ?୨)
("`2" ?2)
("`@" ?୳)
("3"  ?୩)
("`3" ?3)
("`#" ?୴)
("4"  ?୪)
("`4" ?4)
("`$" ?୵)
("5"  ?୫)
("`5" ?5)
("`%" ?୶)
("6"  ?୬)
("`6" ?6)
("`^" ?୷)
("7"  ?୭)
("`7" ?7)
("8"  ?୮)
("`8" ?8)
("9"  ?୯)
("`9" ?9)
("0"  ?୦)
("`0" ?0)
("`\\" ?।)
("`|" ?॥)
("`"  ?ଟ)
("q"  ?ଟ)
("Q"  ?ଠ)
("`q" ?୰)
("`Q" ?୕)
("w"  ?ଡ)
("W"  ?ଢ)
("`w" ?ଡ଼)
("`W" ?ଢ଼)
("e"  ?େ)
("E"  ?ୈ)
("`e" ?ଏ)
("`E" ?ଐ)
("r"  ?ର)
("R"  ?ୃ)
("`r" ?ଋ)
("t"  ?ତ)
("T"  ?ଥ)
("`t" ?ୖ)
("`T" ?ୗ)
("y"  ?ଯ)
("Y"  ?ୟ)
("u"  ?ୁ)
("U"  ?ୂ)
("`u" ?ଉ)
("`U" ?ଊ)
("i"  ?ି)
("I"  ?ୀ)
("`i" ?ଇ)
("`I" ?ଈ)
("o"  ?ୋ)
("O"  ?ୌ)
("`o" ?ଓ)
("`O" ?ଔ)
("p"  ?ପ)
("P"  ?ଫ)
("a"  ?ା)
("A"  ?ଆ)
("`a" ?ଅ)
("s"  ?ସ)
("S"  ?ଶ)
("d"  ?ଦ)
("D"  ?ଧ)
("f"  ?୍)
("F"  ?ୄ)
("`f" ?ୠ)
("g"  ?ଗ)
("G"  ?ଘ)
("h"  ?ହ)
("H"  ?ଃ)
("j"  ?ଜ)
("J"  ?ଝ)
("k"  ?କ)
("K"  ?ଖ)
("l"  ?ଲ)
("L"  ?ଳ)
("`l" ?ୢ)
("`L" ?ଌ)
("z"  ?ଞ)
("Z"  ?ଙ)
("`z" ?ୣ)
("`Z" ?ୡ)
("x"  ?ଷ)
("X"  ?଼)
("c"  ?ଚ)
("C"  ?ଛ)
("`c" #x200C)  ; ZWNJ
("`C" #x200D)  ; ZWJ
("v"  ?ଵ)
("V"  ?ୱ)
("b"  ?ବ)
("B"  ?ଭ)
("n"  ?ନ)
("N"  ?ଣ)
("m"  ?ମ)
("M"  ?ଂ)
("`m" ?ଁ)
("`M" ?ଽ)
)

(quail-define-package
 "limbu" "Limbu" "ᤕ" t "Limbu phonetic input method.

 `\\=`' is used to switch levels instead of Alt-Gr.
" nil t t t t nil nil nil nil nil t)

(quail-define-rules
("``" ?₹)
("1"  ?᥇)
("`1" ?1)
("`!" ?᥄)
("2"  ?᥈)
("`2" ?2)
("3"  ?᥉)
("`3" ?3)
("4"  ?᥊)
("`4" ?4)
("5"  ?᥋)
("`5" ?5)
("6"  ?᥌)
("`6" ?6)
("7"  ?᥍)
("`7" ?7)
("8"  ?᥎)
("`8" ?8)
("9"  ?᥏)
("`9" ?9)
("0"  ?᥆)
("`0" ?0)
("`\\" ?।)
("`|" ?॥)
("`"  ?ᤘ)
("q"  ?ᤧ)
("Q"  ?ᤨ)
("`q" ?᥀)
("w"  ?ᤘ)
("W"  ?ᤫ)
("e"  ?ᤣ)
("E"  ?ᤤ)
("r"  ?ᤖ)
("R"  ?ᤷ)
("`r" ?ᤪ)
("t"  ?ᤋ)
("T"  ?ᤌ)
("`t" ?ᤳ)
("`T" ?ᤞ)
("y"  ?ᤕ)
("Y"  ?ᤩ)
("u"  ?ᤢ)
("i"  ?ᤡ)
("o"  ?ᤥ)
("O"  ?ᤦ)
("p"  ?ᤐ)
("P"  ?ᤑ)
("`p" ?ᤵ)
("a"  ?ᤠ)
("A"  ?ᤀ)
("s"  ?ᤛ)
("S"  ?ᤙ)
("d"  ?ᤍ)
("D"  ?ᤎ)
("f"  ?᤻)
("g"  ?ᤃ)
("G"  ?ᤄ)
("`g" ?ᤝ)
("h"  ?ᤜ)
("j"  ?ᤈ)
("J"  ?ᤉ)
("k"  ?ᤁ)
("K"  ?ᤂ)
("`k" ?ᤰ)
("l"  ?ᤗ)
("L"  ?ᤸ)
("z"  ?ᤊ)
("Z"  ?ᤅ)
("x"  ?ᤚ)
("X"  ?᤹)
("c"  ?ᤆ)
("C"  ?ᤇ)
("`c" #x200C)  ; ZWNJ
("v"  ?᤺)
("b"  ?ᤒ)
("B"  ?ᤓ)
("n"  ?ᤏ)
("N"  ?ᤴ)
("m"  ?ᤔ)
("M"  ?ᤱ)
("`m" ?ᤲ)
("`?" ?᥅)
)

(quail-define-package
 "grantha" "Grantha" "𑌗𑍍𑌰" t "Grantha phonetic input method.

 `\\=`' is used to switch levels instead of Alt-Gr."
 nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ("``" ?₹)
 ("1"  ?௧)
 ("`1" ?1)
 ("`!" ?𑍧)
 ("2"  ?௨)
 ("`2" ?2)
 ("`@" ?𑍨)
 ("3"  ?௩)
 ("`3" ?3)
 ("`#" ?𑍩)
 ("4"  ?௪)
 ("`4" ?4)
 ("`$" ?𑍪)
 ("5"  ?௫)
 ("`5" ?5)
 ("`%" ?𑍫)
 ("6"  ?௬)
 ("`6" ?6)
 ("`^" ?𑍬)
 ("7"  ?௭)
 ("`7" ?7)
 ("8"  ?௮)
 ("`8" ?8)
 ("9"  ?௯)
 ("`9" ?9)
 ("0"  ?௦)
 ("`0" ?0)
 ("q"  ?𑌟)
 ("Q"  ?𑌠)
 ("`q" ?𑍐)
 ("`Q" ?𑍝)
 ("w"  ?𑌡)
 ("W"  ?𑌢)
 ("`w" ?𑍞)
 ("`W" ?𑍟)
 ("e"  ?𑍇)
 ("E"  ?𑍈)
 ("`e" ?𑌏)
 ("`E" ?𑌐)
 ("r"  ?𑌰)
 ("R"  ?𑍃)
 ("`r" ?𑌋)
 ("t"  ?𑌤)
 ("T"  ?𑌥)
 ("`t" ?𑍗)
 ("y"  ?𑌯)
 ("u"  ?𑍁)
 ("U"  ?𑍂)
 ("`u" ?𑌉)
 ("`U" ?𑌊)
 ("i"  ?𑌿)
 ("I"  ?𑍀)
 ("`i" ?𑌇)
 ("`I" ?𑌈)
 ("o"  ?𑍋)
 ("O"  ?𑍌)
 ("`o" ?𑌓)
 ("`O" ?𑌔)
 ("p"  ?𑌪)
 ("P"  ?𑌫)
 ("`p" ?𑍴)
 ("a"  ?𑌾)
 ("A"  ?𑌆)
 ("`a" ?𑌅)
 ("`A" ?𑍰)
 ("s"  ?𑌸)
 ("S"  ?𑌶)
 ("d"  ?𑌦)
 ("D"  ?𑌧)
 ("f"  ?𑍍)
 ("F"  ?𑍄)
 ("`f" ?𑍠)
 ("g"  ?𑌗)
 ("G" ?𑌘)
 ("h"  ?𑌹)
 ("H"  ?𑌃)
 ("j"  ?𑌜)
 ("J"  ?𑌝)
 ("k"  ?𑌕)
 ("K"  ?𑌖)
 ("`k" ?𑍱)
 ("l"  ?𑌲)
 ("L"  ?𑌳)
 ("`l" ?𑍢)
 ("`L" ?𑌌)
 ("z"  ?𑌞)
 ("Z"  ?𑌙)
 ("`z" ?𑍣)
 ("`Z" ?𑍡)
 ("x"  ?𑌷)
 ("X"  ?𑌼)
 ("`x" ?𑌻)
 ("c"  ?𑌚)
 ("C"  ?𑌛)
 ("`c" #x200C)  ; ZWNJ
 ("v"  ?𑌵)
 ("V"  ?𑌽)
 ("`v" ?𑍳)
 ("b"  ?𑌬)
 ("B"  ?𑌭)
 ("n"  ?𑌨)
 ("N"  ?𑌣)
 ("`n" ?𑍲)
 ("m"  ?𑌮)
 ("M"  ?𑌂)
 ("`m" ?𑌁)
 ("`M" ?𑌀))

(quail-define-package
 "lepcha" "Lepcha" "ᰛᰩᰵ" t "Lepcha phonetic input method.

 `\\=`' is used to switch levels instead of Alt-Gr."
 nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ("``" ?₹)
 ("1"  ?᱁)
 ("`1" ?1)
 ("2"  ?᱂)
 ("`2" ?2)
 ("3"  ?᱃)
 ("`3" ?3)
 ("4"  ?᱄)
 ("`4" ?4)
 ("5"  ?᱅)
 ("`5" ?5)
 ("6"  ?᱆)
 ("`6" ?6)
 ("7"  ?᱇)
 ("`7" ?7)
 ("8"  ?᱈)
 ("`8" ?8)
 ("9"  ?᱉)
 ("`9" ?9)
 ("0"  ?᱀)
 ("`0" ?0)
 ("`\\" ?᰻)
 ("`|" ?᰼)
 ("`"  ?ᱍ)
 ("q"  ?ᱍ)
 ("Q"  ?ᱎ)
 ("`q" ?᰽)
 ("`Q" ?᰾)
 ("w"  ?ᰢ)
 ("W"  ?ᱏ)
 ("`w" ?᰿)
 ("e"  ?ᰬ)
 ("r"  ?ᰛ)
 ("R"  ?ᰥ)
 ("`r" ?ᰲ)
 ("t"  ?ᰊ)
 ("T"  ?ᰋ)
 ("`t" ?ᰳ)
 ("y"  ?ᰚ)
 ("Y"  ?ᰤ)
 ("u"  ?ᰪ)
 ("U"  ?ᰫ)
 ("i"  ?ᰧ)
 ("o"  ?ᰨ)
 ("O"  ?ᰩ)
 ("p"  ?ᰎ)
 ("P"  ?ᰏ)
 ("`p" ?ᰐ)
 ("`P" ?ᰱ)
 ("a"  ?ᰦ)
 ("A"  ?ᰣ)
 ("s"  ?ᰠ)
 ("S"  ?ᰡ)
 ("d"  ?ᰌ)
 ("f"  ?ᰑ)
 ("F"  ?ᰒ)
 ("g"  ?ᰃ)
 ("G"  ?ᰄ)
 ("h"  ?ᰝ)
 ("H"  ?ᰞ)
 ("j"  ?ᰈ)
 ("k"  ?ᰀ)
 ("K"  ?ᰁ)
 ("`k" ?ᰂ)
 ("`K" ?ᰭ)
 ("l"  ?ᰜ)
 ("L"  ?ᰯ)
 ("z"  ?ᰉ)
 ("Z"  ?ᰅ)
 ("`z" ?ᰴ)
 ("`Z" ?ᰵ)
 ("x"  ?ᰶ)
 ("X"  ?᰷)
 ("c"  ?ᰆ)
 ("C"  ?ᰇ)
 ("`c" #x200C)  ; ZWNJ
 ("v"  ?ᰟ)
 ("b"  ?ᰓ)
 ("B"  ?ᰔ)
 ("n"  ?ᰍ)
 ("N"  ?ᰰ)
 ("m"  ?ᰕ)
 ("M"  ?ᰖ)
 ("`m"  ?ᰮ))

(quail-define-package
 "meetei-mayek" "Meetei Mayek" "ꯃꯤ" t "Meetei Mayek phonetic input method.

 `\\=`' is used to switch levels instead of Alt-Gr."
 nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ("``" ?₹)
 ("1"  ?꯱)
 ("`1" ?1)
 ("2"  ?꯲)
 ("`2" ?2)
 ("3"  ?꯳)
 ("`3" ?3)
 ("4"  ?꯴)
 ("`4" ?4)
 ("5"  ?꯵)
 ("`5" ?5)
 ("6"  ?꯶)
 ("`6" ?6)
 ("7"  ?꯷)
 ("`7" ?7)
 ("8"  ?꯸)
 ("`8" ?8)
 ("9"  ?꯹)
 ("`9" ?9)
 ("0"  ?꯰)
 ("`0" ?0)
 ("`\\" ?꫰)
 ("`|" ?꯫)
 ("`"  ?ꫤ)
 ("q"  ?ꫤ)
 ("Q"  ?ꫥ)
 ("w"  ?ꯋ)
 ("W"  ?ꫦ)
 ("`w" ?ꫧ)
 ("e"  ?ꯦ)
 ("E"  ?ꯩ)
 ("`e" ?ꫠ)
 ("r"  ?ꯔ)
 ("t"  ?ꯇ)
 ("T"  ?ꯊ)
 ("`t" ?ꯠ)
 ("y"  ?ꯌ)
 ("u"  ?ꯨ)
 ("U"  ?ꯎ)
 ("`u" ?ꫬ)
 ("i"  ?ꯤ)
 ("I"  ?ꯏ)
 ("`i" ?ꯢ)
 ("`I" ?ꫫ)
 ("o"  ?ꯣ)
 ("O"  ?ꯧ)
 ("`o" ?ꫡ)
 ("`O" ?ꫮ)
 ("p"  ?ꯄ)
 ("P"  ?ꯐ)
 ("`p" ?ꯞ)
 ("a"  ?ꯥ)
 ("A"  ?ꯑ)
 ("`a" ?ꫭ)
 ("`A" ?ꫯ)
 ("s"  ?ꯁ)
 ("S"  ?ꫩ)
 ("`s" ?ꫪ)
 ("d"  ?ꯗ)
 ("D"  ?ꯙ)
 ("f"  ?꯭)
 ("F"  ?꫶)
 ("g"  ?ꯒ)
 ("G"  ?ꯘ)
 ("h"  ?ꯍ)
 ("H"  ?ꫵ)
 ("j"  ?ꯖ)
 ("J"  ?ꯓ)
 ("k"  ?ꯀ)
 ("K"  ?ꯈ)
 ("`k" ?ꯛ)
 ("l"  ?ꯂ)
 ("L"  ?ꯜ)
 ("z"  ?ꯉ)
 ("Z"  ?ꯡ)
 ("`z" ?ꫣ)
 ("x"  ?ꯪ)
 ("c"  ?ꯆ)
 ("C"  ?ꫢ)
 ("v"  ?꯬)
 ("V"  ?ꫳ)
 ("`v" ?ꫴ)
 ("b"  ?ꯕ)
 ("B"  ?ꯚ)
 ("n"  ?ꯅ)
 ("N"  ?ꯟ)
 ("`n" ?ꫨ)
 ("m"  ?ꯃ)
 ("M"  ?ꯝ)
 ("`m" ?ꫲ)
 ("`?" ?꫱))

(quail-define-package
 "wancho" "Wancho" "𞋒" t "Wancho phonetic input method.

 `\\=`' is used to switch levels instead of Alt-Gr."
 nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ("``" ?𞋿)
 ("1"  ?𞋱)
 ("`1" ?1)
 ("2"  ?𞋲)
 ("`2" ?2)
 ("3"  ?𞋳)
 ("`3" ?3)
 ("4"  ?𞋴)
 ("`4" ?4)
 ("5"  ?𞋵)
 ("`5" ?5)
 ("6"  ?𞋶)
 ("`6" ?6)
 ("7"  ?𞋷)
 ("`7" ?7)
 ("8"  ?𞋸)
 ("`8" ?8)
 ("9"  ?𞋹)
 ("`9" ?9)
 ("0"  ?𞋰)
 ("`0" ?0)
 ("q"  ?𞋠)
 ("Q"  ?𞋡)
 ("w"  ?𞋒)
 ("e"  ?𞋛)
 ("E"  ?𞋧)
 ("r"  ?𞋗)
 ("t"  ?𞋋)
 ("T"  ?𞋌)
 ("y"  ?𞋆)
 ("Y"  ?𞋫)
 ("u"  ?𞋞)
 ("U"  ?𞋪)
 ("i"  ?𞋜)
 ("I"  ?𞋥)
 ("o"  ?𞋕)
 ("O"  ?𞋖)
 ("`o" ?𞋢)
 ("`O" ?𞋦)
 ("p"  ?𞋊)
 ("P"  ?𞋇)
 ("a"  ?𞋁)
 ("A"  ?𞋀)
 ("`a" ?𞋤)
 ("`A" ?𞋣)
 ("s"  ?𞋎)
 ("S"  ?𞋏)
 ("d"  ?𞋄)
 ("f"  ?𞋍)
 ("g"  ?𞋅)
 ("h"  ?𞋚)
 ("j"  ?𞋐)
 ("k"  ?𞋔)
 ("K"  ?𞋙)
 ("l"  ?𞋈)
 ("L"  ?𞋟)
 ("z"  ?𞋑)
 ("x"  ?𞋩)
 ("X"  ?𞋝)
 ("c"  ?𞋃)
 ("C"  ?𞋬)
 ("v"  ?𞋓)
 ("V"  ?𞋭)
 ("b"  ?𞋂)
 ("B"  ?𞋮)
 ("n"  ?𞋉)
 ("N"  ?𞋯)
 ("m"  ?𞋘)
 ("M"  ?𞋨))

(quail-define-package
 "toto" "Toto" "𞊒𞊪" nil "Toto script phonetic input method."
 nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ("q"  ?𞊫)
 ("Q"  ?𞊬)
 ("w"  ?𞊜)
 ("e"  ?𞊦)
 ("E"  ?𞊧)
 ("r"  ?𞊟)
 ("t"  ?𞊒)
 ("y"  ?𞊛)
 ("u"  ?𞊥)
 ("i"  ?𞊡)
 ("I"  ?𞊢)
 ("o"  ?𞊪)
 ("p"  ?𞊐)
 ("a"  ?𞊭)
 ("s"  ?𞊙)
 ("d"  ?𞊓)
 ("f"  ?𞊮)
 ("g"  ?𞊕)
 ("h"  ?𞊞)
 ("j"  ?𞊝)
 ("k"  ?𞊔)
 ("l"  ?𞊠)
 ("z"  ?𞊣)
 ("Z"  ?𞊤)
 ("x"  ?𞊨)
 ("X"  ?𞊩)
 ("c"  ?𞊚)
 ("b"  ?𞊑)
 ("n"  ?𞊗)
 ("N"  ?𞊘)
 ("m"  ?𞊖))

(provide 'indian)
;;; indian.el ends here
