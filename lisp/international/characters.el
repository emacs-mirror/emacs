;;; characters.el --- set syntax and category for multibyte characters

;; Copyright (C) 1995 Free Software Foundation, Inc.
;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.

;; Keywords: multibyte character, character set, syntax, category

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This file contains multibyte characters.  Save this file always in
;; `coding-system-iso-2022-7'.

;;; Predefined categories.

;; For each character set.

(define-category ?a "ASCII")
(define-category ?l "Latin")
(define-category ?t "Thai")
(define-category ?g "Greek")
(define-category ?b "Arabic")
(define-category ?w "Hebrew")
(define-category ?y "Cyrillic")
(define-category ?k "Japanese katakana")
(define-category ?r "Japanese roman")
(define-category ?c "Chinese")
(define-category ?j "Japanese")
(define-category ?h "Korean")
(define-category ?e "Ethiopic (Ge'ez)")
(define-category ?v "Vietnamese")
(define-category ?i "Indian")

;; For each group (row) of 2-byte character sets.

(define-category ?A "Alpha numeric characters of 2-byte character sets")
(define-category ?C "Chinese (Han) characters of 2-byte character sets")
(define-category ?G "Greek characters of 2-byte characters sets")
(define-category ?H "Japanese Hiragana characters of 2-byte character sets")
(define-category ?K "Japanese Katakana characters of 2-byte character sets")
(define-category ?N "Korean Hangul characters of 2-byte character sets")
(define-category ?Y "Cyrillic character of 2-byte character sets")
(define-category ?I "Indian Glyphs")

;; For phonetic classifications.

(define-category ?0 "consonant")
(define-category ?1 "base vowel")
(define-category ?2 "upper diacritical mark (including upper vowel)")
(define-category ?3 "lower diacritical mark (including lower vowel)")
(define-category ?4 "tone mark")
(define-category ?5 "vowel")
(define-category ?6 "digit")
(define-category ?7 "vowel-modifying diacritical mark")
(define-category ?8 "vowel-signs.")

;; For filling.
(define-category ?| "While filling, we can break a line at this character.")

;; Keep the followings for `kinsoku' processing.  See comments in
;; kinsoku.el.
(define-category ?> "A character which can't be placed at beginning of line.")
(define-category ?< "A character which can't be placed at end of line.")


;;; Setting syntax and category.

;; ASCII

(let ((ch 32))
  (while (< ch 127)			; All ASCII characters have
    (modify-category-entry ch ?a)	; the category `a' (ASCII)
    (modify-category-entry ch ?l)	; and `l' (Latin).
    (setq ch (1+ ch))))

;; Arabic character set

(let ((charsets '(arabic-iso8859-6
		  arabic-digit
		  arabic-1-column
		  arabic-2-column)))
  (while charsets
    (modify-syntax-entry (make-char (car charsets)) "w")
    (modify-category-entry (make-char (car charsets)) ?b)
    (setq charsets (cdr charsets))))

;; Chinese character set (GB2312)

(modify-syntax-entry (make-char 'chinese-gb2312) "w")
(modify-syntax-entry (make-char 'chinese-gb2312 33) "_")
(modify-syntax-entry (make-char 'chinese-gb2312 34) "_")
(modify-syntax-entry (make-char 'chinese-gb2312 41) "_")
(modify-syntax-entry ?\$A!2(B "($A!3(B")
(modify-syntax-entry ?\$A!4(B "($A!5(B")
(modify-syntax-entry ?\$A!6(B "($A!7(B")
(modify-syntax-entry ?\$A!8(B "($A!9(B")
(modify-syntax-entry ?\$A!:(B "($A!;(B")
(modify-syntax-entry ?\$A!<(B "($A!=(B")
(modify-syntax-entry ?\$A!>(B "($A!?(B")
(modify-syntax-entry ?\$A!3(B ")$A!2(B")
(modify-syntax-entry ?\$A!5(B ")$A!4(B")
(modify-syntax-entry ?\$A!7(B ")$A!6(B")
(modify-syntax-entry ?\$A!9(B ")$A!8(B")
(modify-syntax-entry ?\$A!;(B ")$A!:(B")
(modify-syntax-entry ?\$A!=(B ")$A!<(B")
(modify-syntax-entry ?\$A!?(B ")$A!>(B")

(modify-category-entry (make-char 'chinese-gb2312) ?c)
(modify-category-entry (make-char 'chinese-gb2312) ?\|)
(modify-category-entry (make-char 'chinese-gb2312 35) ?A)
(modify-category-entry (make-char 'chinese-gb2312 36) ?H)
(modify-category-entry (make-char 'chinese-gb2312 37) ?K)
(modify-category-entry (make-char 'chinese-gb2312 38) ?G)
(modify-category-entry (make-char 'chinese-gb2312 39) ?Y)
(modify-category-entry (make-char 'chinese-gb2312 35) ?A)
(let ((row 48))
  (while (< row 127)
    (modify-category-entry (make-char 'chinese-gb2312 row) ?C)
    (setq row (1+ row))))

;; Chinese character set (BIG5)

(let ((generic-big5-1-char (make-char 'chinese-big5-1))
      (generic-big5-2-char (make-char 'chinese-big5-2)))
  (modify-syntax-entry generic-big5-1-char "w")
  (modify-syntax-entry generic-big5-2-char "w")

  (modify-category-entry generic-big5-1-char ?c)
  (modify-category-entry generic-big5-2-char ?c)

  (modify-category-entry generic-big5-1-char ?C)
  (modify-category-entry generic-big5-2-char ?C)

  (modify-category-entry generic-big5-1-char ?\|)
  (modify-category-entry generic-big5-2-char ?\|))


;; Chinese character set (CNS11643)

(let ((cns-list '(chinese-cns11643-1
		  chinese-cns11643-2
		  chinese-cns11643-3
		  chinese-cns11643-4
		  chinese-cns11643-5
		  chinese-cns11643-6
		  chinese-cns11643-7))
      generic-char)
  (while cns-list
    (setq generic-char (make-char (car cns-list)))
    (modify-syntax-entry generic-char "w")
    (modify-category-entry generic-char ?c)
    (modify-category-entry generic-char ?C)
    (modify-category-entry generic-char ?|)
    (setq cns-list (cdr cns-list))))

;; Cyrillic character set (ISO-8859-5)

(modify-category-entry (make-char 'cyrillic-iso8859-5) ?y)

(let ((c 160))
  (while (< c 256)
    (modify-syntax-entry (make-char 'cyrillic-iso8859-5 c) "w")
    (setq c (1+ c))))
(modify-syntax-entry ?,L-(B ".")
(modify-syntax-entry ?,Lp(B ".")
(modify-syntax-entry ?,L}(B ".")

;; Ethiopic character set

(modify-category-entry (make-char 'ethiopic) ?e)

;; European character set (Latin-1,2,3,4,5)

(modify-category-entry (make-char 'latin-iso8859-1) ?l)
(modify-category-entry (make-char 'latin-iso8859-2) ?l)
(modify-category-entry (make-char 'latin-iso8859-3) ?l)
(modify-category-entry (make-char 'latin-iso8859-4) ?l)
(modify-category-entry (make-char 'latin-iso8859-9) ?l)

;; ISO-8859-1 (Latin-1)
(let ((c 64))
  (while (< c 128)			; from ',A@(B' to ',A(B'
    (modify-syntax-entry (make-char 'latin-iso8859-1 c) "w")
    (setq c (1+ c)))
  (modify-syntax-entry (make-char 'latin-iso8859-1 32) "w") ; NBSP
  (modify-syntax-entry ?,AW(B "_")
  (modify-syntax-entry ?,Aw(B "_")
  )

;; ISO-8859-2 (Latin-2)
(let ((c 190))
  (while (< c 255)
    (modify-syntax-entry (make-char 'latin-iso8859-2 c) "w")
    (setq c (1+ c))))
(let ((chars '(?,B!(B ?,B#(B ?,B%(B ?,B&(B ?,B)(B ?,B*(B ?,B+(B ?,B,(B ?,B.(B ?,B/(B ?,B1(B ?,B3(B ?,B5(B ?,B6(B ?,B9(B ?,B:(B ?,B;(B ?,B<(B)))
  (while chars
    (modify-syntax-entry (car chars) "w")
    (setq chars (cdr chars))))
(modify-syntax-entry (make-char 'latin-iso8859-2 160) "w") ; NBSP
(modify-syntax-entry ?,BW(B ".")
(modify-syntax-entry ?,Bw(B ".")

;; Greek character set (ISO-8859-7)

(modify-category-entry (make-char 'greek-iso8859-7) ?g)

(let ((c 182))
  (while (< c 255)
    (modify-syntax-entry (make-char 'greek-iso8859-7 c) "w")
    (setq c (1+ c))))
(modify-syntax-entry (make-char 'greek-iso8859-7 160) "w") ; NBSP
(modify-syntax-entry ?,F7(B ".")
(modify-syntax-entry ?,F;(B ".")
(modify-syntax-entry ?,F=(B ".")

;; Hebrew character set (ISO-8859-8)

(modify-category-entry (make-char 'hebrew-iso8859-8) ?w)

(let ((c 224))
  (while (< c 251)
    (modify-syntax-entry (make-char 'hebrew-iso8859-8 c) "w")
    (setq c (1+ c))))
(modify-syntax-entry (make-char 'hebrew-iso8859-8 160) "w") ; NBSP

;; Indian character set (IS 13194 and other Emacs original Indian charsets)

(modify-category-entry (make-char 'indian-is13194) ?i)
(modify-category-entry (make-char 'indian-2-column) ?I)
(modify-category-entry (make-char 'indian-1-column) ?I)

;; Japanese character set (JISX0201-kana, JISX0201-roman, JISX0208, JISX0212)

(modify-category-entry (make-char 'katakana-jisx0201) ?k)
(modify-category-entry (make-char 'latin-jisx0201) ?r)
(modify-category-entry (make-char 'japanese-jisx0208) ?j)
(modify-category-entry (make-char 'japanese-jisx0212) ?j)
(modify-category-entry (make-char 'japanese-jisx0208) ?\|)

;; JISX0208
(modify-syntax-entry (make-char 'japanese-jisx0208) "w")
(modify-syntax-entry (make-char 'japanese-jisx0208 33) "_")
(modify-syntax-entry (make-char 'japanese-jisx0208 34) "_")
(modify-syntax-entry (make-char 'japanese-jisx0208 40) "_")
(let ((chars '(?$B!<(B ?$B!+(B ?$B!,(B ?$B!3(B ?$B!4(B ?$B!5(B ?$B!6(B ?$B!7(B ?$B!8(B ?$B!9(B ?$B!:(B ?$B!;(B)))
  (while chars
    (modify-syntax-entry (car chars) "w")
    (setq chars (cdr chars))))
(modify-syntax-entry ?\$B!J(B "($B!K(B")
(modify-syntax-entry ?\$B!N(B "($B!O(B")
(modify-syntax-entry ?\$B!P(B "($B!Q(B")
(modify-syntax-entry ?\$B!V(B "($B!W(B")
(modify-syntax-entry ?\$B!X(B "($B!Y(B")
(modify-syntax-entry ?\$B!K(B ")$B!J(B")
(modify-syntax-entry ?\$B!O(B ")$B!N(B")
(modify-syntax-entry ?\$B!Q(B ")$B!P(B")
(modify-syntax-entry ?\$B!W(B ")$B!V(B")
(modify-syntax-entry ?\$B!Y(B ")$B!X(B")

(modify-category-entry (make-char 'japanese-jisx0208 35) ?A)
(modify-category-entry (make-char 'japanese-jisx0208 36) ?H)
(modify-category-entry (make-char 'japanese-jisx0208 37) ?K)
(modify-category-entry (make-char 'japanese-jisx0208 38) ?G)
(modify-category-entry (make-char 'japanese-jisx0208 39) ?Y)
(let ((row 48))
  (while (< row 127)
    (modify-category-entry (make-char 'japanese-jisx0208 row) ?C)
    (setq row (1+ row))))
(let ((chars '(?$B!<(B ?$B!+(B ?$B!,(B)))
  (while chars
    (modify-category-entry (car chars) ?K)
    (modify-category-entry (car chars) ?H)
    (setq chars (cdr chars))))
(let ((chars '(?$B!3(B ?$B!4(B ?$B!5(B ?$B!6(B ?$B!7(B ?$B!8(B ?$B!9(B ?$B!:(B ?$B!;(B)))
  (while chars
    (modify-category-entry (car chars) ?C)
    (setq chars (cdr chars))))

;; JISX0212
(modify-syntax-entry (make-char 'japanese-jisx0212) "w")
(modify-syntax-entry (make-char 'japanese-jisx0212 33) "_")
(modify-syntax-entry (make-char 'japanese-jisx0212 34) "_")
(modify-syntax-entry (make-char 'japanese-jisx0212 35) "_")

(modify-category-entry (make-char 'japanese-jisx0212 ) ?C)

;; JISX0201-Kana
(modify-syntax-entry (make-char 'katakana-jisx0201) "w")
(let ((chars '(?(I!(B ?(I"(B ?(I#(B ?(I$(B ?(I%(B)))
  (while chars
    (modify-syntax-entry (car chars) ".")
    (setq chars (cdr chars))))

;; Korean character set (KSC5601)

(modify-syntax-entry (make-char 'korean-ksc5601) "w")
(modify-syntax-entry (make-char 'korean-ksc5601 33) "_")
(modify-syntax-entry (make-char 'korean-ksc5601 34) "_")
(modify-syntax-entry (make-char 'korean-ksc5601 38) "_")
(modify-syntax-entry (make-char 'korean-ksc5601 39) "_")
(modify-syntax-entry (make-char 'korean-ksc5601 40) "_")
(modify-syntax-entry (make-char 'korean-ksc5601 41) "_")

(modify-category-entry (make-char 'korean-ksc5601) ?h)
(modify-category-entry (make-char 'korean-ksc5601 35) ?A)
(modify-category-entry (make-char 'korean-ksc5601 37) ?G)
(modify-category-entry (make-char 'korean-ksc5601 42) ?H)
(modify-category-entry (make-char 'korean-ksc5601 43) ?K)
(modify-category-entry (make-char 'korean-ksc5601 44) ?Y)

;; Thai character set (TIS620)

(modify-category-entry (make-char 'thai-tis620) ?t)

(let ((deflist	'(;; chars	syntax	category
		  (",T!(B-,TCEG(B-,TN(B"	"w"	?0) ; consonant
		  (",TDFPRS`(B-,Te(B"	"w"	?1) ; vowel base
		  (",TQT(B-,TWgn(B"	"w"	?2) ; vowel upper
		  (",TX(B-,TZ(B"	"w"	?3) ; vowel lower
		  (",Th(B-,Tm(B"	"w"	?4) ; tone mark 
		  (",TOfp(B-,Ty(B"	"w"	?0) ; digit and misc
		  (",T_oz{(B"	"_"	?0) ; symbol
		  ))
      elm chars len syntax category to ch i)
  (while deflist
    (setq elm (car deflist))
    (setq chars (car elm)
	  len (length chars)
	  syntax (nth 1 elm)
	  category (nth 2 elm)
	  i 0)
    (while (< i len)
      (if (= (aref chars i) ?-)
	  (setq i (1+ i)
		to (sref chars i))
	(setq ch (sref chars i)
	      to ch))
      (while (<= ch to)
	(modify-syntax-entry ch syntax)
	(modify-category-entry ch category)
	(setq ch (1+ ch)))
      (setq i (+ i (char-bytes to))))
    (setq deflist (cdr deflist))))

;; Vietnamese character set

(let ((lower (make-char 'vietnamese-viscii-lower))
      (upper (make-char 'vietnamese-viscii-upper)))
  (modify-syntax-entry lower "w")
  (modify-syntax-entry upper "w")
  (modify-category-entry lower ?v)
  (modify-category-entry upper ?v)
  (modify-category-entry lower ?l)	; To make a word with
  (modify-category-entry upper ?l)	; latin characters.
  )


;;; Setting word boundary.

(setq word-combining-categories
      '((?l . ?l)))

(setq word-separating-categories	;  (2-byte character sets)
      '((?A . ?K)			; Alpha numeric - Katakana
	(?A . ?C)			; Alpha numeric - Chinese
	(?H . ?A)			; Hiragana - Alpha numeric
	(?H . ?K)			; Hiragana - Katakana
	(?H . ?C)			; Hiragana - Chinese
	(?K . ?A)			; Katakana - Alpha numeric
	(?K . ?C)			; Katakana - Chinese
	(?C . ?A)			; Chinese - Alpha numeric
	(?C . ?K)			; Chinese - Katakana
	))
