;;; quail/thai.el --- Quail package for inputting Thai characters

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.

;; Keywords: multilingual, input method, Thai

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

(require 'quail)
(require 'thai-util)

(eval-and-compile

(defvar thai-keyboard-mapping-alist
  '((kesmanee
     [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0	; control codes
      0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0	; control codes
      0 "#" ",TF(B" ",Tr(B" ",Ts(B" ",Tt(B" "0,TQi1(B" ",T'(B"	; SPC .. '
      ",Tv(B" ",Tw(B" ",Tu(B" ",Ty(B" ",TA(B" ",T"(B" ",Tc(B" ",T=(B"	; ( .. /
      ",T((B" ",TE(B" "/" "_" ",T@(B" ",T6(B" ",TX(B" ",TV(B"	; 0 .. 7
      ",T$(B" ",T5(B" ",T+(B" ",TG(B" ",T2(B" ",T*(B" ",TL(B" ",TF(B"	; 8 .. ?
      ",Tq(B" ",TD(B" ",TZ(B" ",T)(B" ",T/(B" ",T.(B" ",Tb(B" ",T,(B"	; @ .. G
      ",Tg(B" ",T3(B" ",Tk(B" ",TI(B" ",TH(B" ",Tn(B" ",Tl(B" ",TO(B"	; H .. O
      ",T-(B" ",Tp(B" ",T1(B" ",T&(B" ",T8(B" ",Tj(B" ",TN(B" "\""	; P .. W
      ")" ",Tm(B" "(" ",T:(B" ",T_(B" ",TE(B" ",TY(B" ",Tx(B"	; X .. _
      ",T#(B" ",T?(B" ",TT(B" ",Ta(B" ",T!(B" ",TS(B" ",T4(B" ",T`(B"	; ` .. g
      ",Ti(B" ",TC(B" ",Th(B" ",TR(B" ",TJ(B" ",T7(B" ",TW(B" ",T9(B"	; h .. o
      ",TB(B" ",Tf(B" ",T>(B" ",TK(B" ",TP(B" ",TU(B" ",TM(B" ",Td(B"	; p .. w
      ",T;(B" ",TQ(B" ",T<(B" ",T0(B" ",To(B" "." ",T%(B" 0]	; x .. DEL
     nil nil)

    (pattachote
     [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0	; control codes
      0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0	; control codes
      0 "+" ",T1(B" "/" "," "?" "_" ",T"(B"	; SPC .. '
      "(" ")" "." "%" ",TP(B" ",Tq(B" ",T((B" ",T>(B"	; ( .. /
      ",Tp(B" "=" ",Tr(B" ",Ts(B" ",Tt(B" ",Tu(B" ",TY(B" ",Tw(B"	; 0 .. 7
      ",Tx(B" ",Ty(B" ",T&(B" ",Td(B" ",T?(B" ",Tv(B" ",T2(B" ",TL(B"	; 8 .. ?
      "\"" ",Tk(B" ",TQ(B" ",T0(B" ",TS(B" ",Tf(B" ",T3(B" ",Tl(B"	; @ .. G
      ",TW(B" ",T+(B" ",T<(B" ",T*(B" ",Tb(B" ",TN(B" ",TH(B" ",T6(B"	; H .. O
      ",T2(B" ",Tj(B" ",T-(B" ",T8(B" ",TI(B" ",T=(B" ",T@(B" ",TD(B"	; P .. W
      ",T.(B" ",TV(B" ",T.(B" ",Tc(B" ",TZ(B" ",T2(B" ",TX(B" "-"	; X .. _
      ",T#(B" ",Ti(B" ",TT(B" ",TE(B" ",T'(B" ",TB(B" ",T!(B" ",TQ(B"	; ` .. g
      ",TU(B" ",TA(B" ",TR(B" ",T9(B" ",T`(B" ",TJ(B" ",T$(B" ",TG(B"	; h .. o
      ",Ta(B" ",Tg(B" ",TM(B" ",T7(B" ",TC(B" ",T4(B" ",TK(B" ",T5(B"	; p .. w
      ",T;(B" ",Th(B" ",T:(B" ",TO(B" ",Tm(B" ",TF(B" ",T%(B" 0]	; x .. DEL
     nil nil)
    )
  "Alist of Thai keyboard types vs. corresponding mapping tables.
Each element is a list of:
  KEYBOARD-TYPE, ASCII-THAI-TABLE, CONSONANT-MAP-TEMPLATE,
  and VOWEL-UPPER-LOWER-TEMPLATE.

KEYBOARD-TYPE is a symbol, one of kesmanee or pattachote.

ASCII-THAI-TABLE is a vector indexed by an ASCII key code
and the value is the one-char string of Thai character
assigned at the location of ASCII key on the specific Thai keyboard.
The value is 0 if no Thai character is assigned at the location.

CONSONANT-MAP-TEMPLATE is a template of a cdr part of a Quail map when
a consonant is entered.

VOWEL-UPPER-LOWER-TEMPLATE is a template of a cdr part of a Quail map
when a vowel upper or a vowel lower is entered.")

(defmacro thai-keyboard-info (keyboard-type)
  `(assq ,keyboard-type thai-keyboard-mapping-alist))

)

(defvar thai-current-keyboard-type nil
  "Thai Keyboard type which Quail is assuming currently.
This variable is used in Quail internally only.")

;; Template of a cdr part of a Quail map when a consonant is entered.
(defvar thai-consonant-alist nil)
;; Template of a cdr part of a Quail map when a vowel upper or a vowel
;; lower is entered.
(defvar thai-vowel-upper-lower-alist nil)

;; Return a Quail map corresponding to KEY of length LEN.
;; The car part of the map is a translation generated automatically.
;; The cdr part of the map is a copy of ALIST.
(defun thai-generate-quail-map (key len alist)
  (let ((str "")
	(idx 0))
    (while (< idx len)
      (setq str (concat str (aref (nth 1 (thai-keyboard-info
					  thai-current-keyboard-type))
				  (aref key idx)))
	    idx (1+ idx)))
    (cons (string-to-char (compose-string str)) (copy-alist alist))))

;; Return a Quail map corresponding to KEY of length LEN when Thai
;; tone mark is entered.
(defun thai-tone-input (key len)
  (thai-generate-quail-map key len nil))

;; Return a Quail map corresponding to KEY of length LEN when Thai
;; vowel upper or vowel lower is entered.
(defun thai-vowel-upper-lower-input (key len)
  (thai-generate-quail-map
   key len
   (nth 3 (thai-keyboard-info thai-current-keyboard-type))))

;; Define RULES in Quail map.  In addition, create
;; `thai-consonant-alist-KEYBOARD-TYPE' and
;; `thai-vowel-upper-lower-alist-KEYBOARD-TYPE'.

;; The general composing rules are as follows:
;;
;;                          T
;;       V        T         V                  T
;; CV -> C, CT -> C, CVT -> C, Cv -> C, CvT -> C
;;                                   v         v
;;
;; where C: consonant, V: vowel upper, v: vowel lower, T: tone mark.

(defmacro thai-quail-define-rules (keyboard-type &rest rules)
  (let ((l rules)
	consonant-alist
	vowel-upper-lower-alist
	rule trans ch c-set)
    (while l
      (setq rule (car l))
      (setq trans (nth 1 rule))
      (if (consp trans)
	  (setq trans (car trans)))
      (setq c-set (char-category-set (string-to-char trans)))
      (cond ((or (aref c-set ?2)
		 (aref c-set ?3))
	     (setq consonant-alist
		   (cons (cons (string-to-char (car rule))
			       'thai-vowel-upper-lower-input)
			 consonant-alist)))
	    ((aref c-set ?4)
	     (setq consonant-alist
		   (cons (cons (string-to-char (car rule))
			       'thai-tone-input)
			 consonant-alist)
		   vowel-upper-lower-alist
		   (cons (cons (string-to-char (car rule))
			       'thai-tone-input)
			 vowel-upper-lower-alist))))
      (setq l (cdr l)))
    `(progn
	  (quail-define-rules ,@rules)
	  (setcar (nthcdr 2 (thai-keyboard-info ,keyboard-type))
		  ',consonant-alist)
	  (setcar (nthcdr 3 (thai-keyboard-info ,keyboard-type))
		  ',vowel-upper-lower-alist))))

;; Return an alist which can be a cdr part of a Quail map
;; corresponding to the current key when Thai consonant is entered.
(defun thai-consonant-input (key len)
  (let ((quail-package-name (quail-name)))
    (setq thai-current-keyboard-type
	  (cond ((string= quail-package-name "thai-pattachote") 'pattachote)
		((string= quail-package-name "thai-kesmanee") 'kesmanee)
		(t (error "Invalid Quail package %s" quail-package-name))))
    (copy-alist (nth 2 (thai-keyboard-info thai-current-keyboard-type)))))

;; Thai Kesmanee keyboard support.

(quail-define-package
 "thai-kesmanee" "Thai" ",T!!(B>" t
 "Thai Kesmanee input method with TIS620 keyboard layout

The difference from the ordinal Thai keyboard:
    ',T_(B' and ',To(B' are assigned to '\\' and '|' respectively,
    ',T#(B' and ',T%(B' are assigned to '`' and '~' respectively,
    Don't know where to assign characters ',Tz(B' and ',T{(B'."
 nil t t nil t)

(thai-quail-define-rules 'kesmanee
 ("1" (",TE(B" . thai-consonant-input))
 ("!" "#")
 ("2" "/")
 ("@" ",Tq(B")
 ("3" "_")
 ("#" ",Tr(B")
 ("4" (",T@(B" . thai-consonant-input))
 ("$" ",Ts(B")
 ("5" (",T6(B" . thai-consonant-input))
 ("%" ",Tt(B")
 ("6" ",TX(B")
 ("^" ",TY(B")
 ("7" ",TV(B")
 ("&" "0,TQi1(B")
 ("8" (",T$(B" . thai-consonant-input))
 ("*" ",Tu(B")
 ("9" (",T5(B" . thai-consonant-input))
 ("\(" ",Tv(B")
 ("0" (",T((B" . thai-consonant-input))
 ("\)" ",Tw(B")
 ("-" (",T"(B" . thai-consonant-input))
 ("_" ",Tx(B")
 ("=" (",T*(B" . thai-consonant-input))
 ("+" ",Ty(B")
 ("\\" ",T_(B")
 ("|" ",To(B")
 ("`" (",T#(B" . thai-consonant-input))
 ("~" (",T%(B" . thai-consonant-input))
 ("q" ",Tf(B")
 ("Q" ",Tp(B")
 ("w" ",Td(B")
 ("W" "\"")
 ("e" ",TS(B")
 ("E" (",T.(B" . thai-consonant-input))
 ("r" (",T>(B" . thai-consonant-input))
 ("R" (",T1(B" . thai-consonant-input))
 ("t" ",TP(B")
 ("T" (",T8(B" . thai-consonant-input))
 ("y" ",TQ(B")
 ("Y" ",Tm(B")
 ("u" ",TU(B")
 ("U" ",Tj(B")
 ("i" (",TC(B" . thai-consonant-input))
 ("I" (",T3(B" . thai-consonant-input))
 ("o" (",T9(B" . thai-consonant-input))
 ("O" ",TO(B")
 ("p" (",TB(B" . thai-consonant-input))
 ("P" (",T-(B" . thai-consonant-input))
 ("\[" (",T:(B" . thai-consonant-input))
 ("{" (",T0(B" . thai-consonant-input))
 ("\]" (",TE(B" . thai-consonant-input))
 ("}" ",")

 ("a" (",T?(B" . thai-consonant-input))
 ("A" ",TD(B")
 ("s" (",TK(B" . thai-consonant-input))
 ("S" (",T&(B" . thai-consonant-input))
 ("d" (",T!(B" . thai-consonant-input))
 ("D" (",T/(B" . thai-consonant-input))
 ("f" (",T4(B" . thai-consonant-input))
 ("F" ",Tb(B")
 ("g" ",T`(B")
 ("G" (",T,(B" . thai-consonant-input))
 ("h" ",Ti(B")
 ("H" ",Tg(B")
 ("j" ",Th(B")
 ("J" ",Tk(B")
 ("k" ",TR(B")
 ("K" (",TI(B" . thai-consonant-input))
 ("l" (",TJ(B" . thai-consonant-input))
 ("L" (",TH(B" . thai-consonant-input))
 ("\;" (",TG(B" . thai-consonant-input))
 (":" (",T+(B" . thai-consonant-input))
 ("'" (",T'(B" . thai-consonant-input))
 ("\"" ".")

 ("z" (",T<(B" . thai-consonant-input))
 ("Z" "(")
 ("x" (",T;(B" . thai-consonant-input))
 ("X" ")")
 ("c" ",Ta(B")
 ("C" (",T)(B" . thai-consonant-input))
 ("v" (",TM(B" . thai-consonant-input))
 ("V" (",TN(B" . thai-consonant-input))
 ("b" ",TT(B")
 ("B" ",TZ(B")
 ("n" ",TW(B")
 ("N" ",Tl(B")
 ("m" (",T7(B" . thai-consonant-input))
 ("M" ",Tn(B")
 ("," (",TA(B" . thai-consonant-input))
 ("<" (",T2(B" . thai-consonant-input))
 ("." ",Tc(B")
 (">" (",TL(B" . thai-consonant-input))
 ("/" (",T=(B" . thai-consonant-input))
 ("?" ",TF(B")
 )


;; Thai Pattachote keyboard support.

(quail-define-package
 "thai-pattachote" "Thai" ",T!;(B>" t
 "Thai Pattachote input method with TIS620 keyboard layout"
 nil t t nil t)

(thai-quail-define-rules 'pattachote
 ("1" "=")
 ("!" "+")
 ("2" ",Tr(B")
 ("@" "\"")
 ("3" ",Ts(B")
 ("#" "/")
 ("4" ",Tt(B")
 ("$" ",")
 ("5" ",Tu(B")
 ("%" "?")
 ("6" ",TY(B")
 ("^" ",TX(B")
 ("7" ",Tw(B")
 ("&" "_")
 ("8" ",Tx(B")
 ("*" ".")
 ("9" ",Ty(B")
 ("(" "(")
 ("0" ",Tp(B")
 (")" ")")
 ("-" ",Tq(B")
 ("_" "-")
 ("=" ",Tv(B")
 ("+" "%")
 ("\\" ",TZ(B")
 ("|" ",Tm(B")
 ("`" (",T#(B" . thai-consonant-input))
 ("~" (",T%(B" . thai-consonant-input))

 ("q" ",Tg(B")
 ("Q" ",Tj(B")
 ("w" (",T5(B" . thai-consonant-input))
 ("W" ",TD(B")
 ("e" (",TB(B" . thai-consonant-input))
 ("E" ",Tf(B")
 ("r" (",TM(B" . thai-consonant-input))
 ("R" (",T-(B" . thai-consonant-input))
 ("t" (",TC(B" . thai-consonant-input))
 ("T" (",TI(B" . thai-consonant-input))
 ("y" ",Th(B")
 ("Y" ",TV(B")
 ("u" (",T4(B" . thai-consonant-input))
 ("U" (",T=(B" . thai-consonant-input))
 ("i" (",TA(B" . thai-consonant-input))
 ("I" (",T+(B" . thai-consonant-input))
 ("o" (",TG(B" . thai-consonant-input))
 ("O" (",T6(B" . thai-consonant-input))
 ("p" ",Ta(B")
 ("P" (",T2(B" . thai-consonant-input))
 ("\[" ",Tc(B")
 ("{" ",TO(B")
 ("\]" (",T2(B" . thai-consonant-input))
 ("}" ",TF(B")

 ("a" ",Ti(B")
 ("A" ",Tk(B")
 ("s" (",T7(B" . thai-consonant-input))
 ("S" (",T8(B" . thai-consonant-input))
 ("d" (",T'(B" . thai-consonant-input))
 ("D" ",TS(B")
 ("f" (",T!(B" . thai-consonant-input))
 ("F" (",T3(B" . thai-consonant-input))
 ("g" ",TQ(B")
 ("G" ",Tl(B")
 ("h" ",TU(B")
 ("H" ",TW(B")
 ("j" ",TR(B")
 ("J" (",T<(B" . thai-consonant-input))
 ("k" (",T9(B" . thai-consonant-input))
 ("K" (",T*(B" . thai-consonant-input))
 ("l" ",T`(B")
 ("L" ",Tb(B")
 (";" ",Td(B")
 (":" (",T&(B" . thai-consonant-input))
 ("'" (",T"(B" . thai-consonant-input))
 ("\"" (",T1(B" . thai-consonant-input))

 ("z" (",T:(B" . thai-consonant-input))
 ("Z" (",T.(B" . thai-consonant-input))
 ("x" (",T;(B" . thai-consonant-input))
 ("X" (",T.(B" . thai-consonant-input))
 ("c" (",TE(B" . thai-consonant-input))
 ("C" (",T0(B" . thai-consonant-input))
 ("v" (",TK(B" . thai-consonant-input))
 ("V" (",T@(B" . thai-consonant-input))
 ("b" ",TT(B")
 ("B" ",TQ(B")
 ("n" (",T$(B" . thai-consonant-input))
 ("N" (",TH(B" . thai-consonant-input))
 ("m" (",TJ(B" . thai-consonant-input))
 ("M" (",TN(B" . thai-consonant-input))
 ("," ",TP(B")
 ("<" (",T?(B" . thai-consonant-input))
 ("." (",T((B" . thai-consonant-input))
 (">" (",T2(B" . thai-consonant-input))
 ("/" (",T>(B" . thai-consonant-input))
 ("?" (",TL(B" . thai-consonant-input))
 )

;;; quail/thai.el ends here
