;;; quail/lao.el --- Quail package for inputting Lao characters

;; Copyright (C) 1997 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.

;; Keywords: multilingual, input method, Lao

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
(require 'lao-util)

(eval-and-compile

(defconst lao-keyboard-mapping
  [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0	; control codes
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0	; control codes
   0 "1" "=" "3" "4" "(1l(B" "5" "(1'(B"          ; SPC .. '
   "7" "8" "6" "(1mh(B" "(1A(B" "(1*(B" "(1c(B" "(1=(B"	; ( .. /
   "(1"(B" "(1B(B" "(1?(B" "(1b(B" "(16(B" "(1X(B" "(1Y(B" "(1$(B"	; 0 .. 7
   "(15(B" "(1((B" "%" "(1G(B" "(1}(B" "(1m(B" "$" "\)"	; 8 .. ?
   "2" "(1Qi(B" "(1Vi(B" "(1O(B" "." "(1Si(B" "," ":"	; @ .. G
   "(1j(B" "(1N(B" "(1k(B" "!" "?" "(1f(B" "(1Wi(B" "(1|(B" 	; H .. O
   "(1](B" "(1[i(B" "_" ";" "+" "(1Ui(B" "x" "0"	; P .. W
   "\(" "(1Ti(B" "\"" "(1:(B" 0 "(1E(B" "(1\(B" "9"     ; X .. _
   "(1'(B" "(1Q(B" "(1V(B" "(1a(B" "(1!(B" "(1S(B" "(14(B" "(1`(B"	; ` .. g
   "(1i(B" "(1C(B" "(1h(B" "(1R(B" "(1J(B" "(17(B" "(1W(B" "(19(B"	; h .. o
   "(1-(B" "(1[(B" "(1>(B" "(1K(B" "(1P(B" "(1U(B" "(1M(B" "(1d(B"	; p .. w
   "(1;(B" "(1T(B" "(1<(B" "-" "(1K\(B" "/" "~" 0]    	; x .. DEL
  "A table which maps ASCII key codes to corresponding Lao characters."
  )

)

;; Template of a cdr part of a Quail map when a consonant is entered.
(defvar lao-consonant-alist nil)
;; Template of a cdr part of a Quail map when a vowel upper is entered.
(defvar lao-vowel-upper-alist nil)
;; Template of a cdr part of a Quail map when a vowel lower is entered.
(defvar lao-vowel-lower-alist nil)
;; Template of a cdr part of a Quail map when a semivowel lower is entered.
(defvar lao-semivowel-lower-alist nil)

;; Return a Quail map corresponding to KEY of length LEN.
;; The car part of the map is a translation generated automatically.
;; The cdr part of the map is a copy of ALIST.
(defun lao-generate-quail-map (key len alist)
  (let ((str "")
	(idx 0))
    (while (< idx len)
      (setq str (concat str (aref lao-keyboard-mapping (aref key idx)))
	    idx (1+ idx)))
    (cons (string-to-char (compose-string str)) (copy-alist alist))))

;; Return a Quail map corresponding to KEY of length LEN when Lao
;; tone mark is entered.
(defun lao-tone-input (key len)
  (lao-generate-quail-map key len nil))

;; Return a Quail map corresponding to KEY of length LEN when Lao
;; vowel upper is entered.
(defun lao-vowel-upper-input (key len)
  (lao-generate-quail-map key len lao-vowel-upper-alist))

;; Return a Quail map corresponding to KEY of length LEN when Lao
;; vowel lower is entered.
(defun lao-vowel-lower-input (key len)
  (lao-generate-quail-map key len lao-vowel-lower-alist))

;; Return a Quail map corresponding to KEY of length LEN when Lao
;; semivowel lower is entered.
(defun lao-semivowel-lower-input (key len)
  (lao-generate-quail-map key len lao-semivowel-lower-alist))

;; Return an alist which can be a cdr part of a Quail map
;; corresponding to the current key when Lao consonant is entered.
(defun lao-consonant-input (key len)
  (copy-alist lao-consonant-alist))

(quail-define-package
 "lao" "Lao" "(1E(B" t
 "Lao input method simulating Lao keyboard layout based on Thai TIS620"
 nil t t t t nil nil nil nil nil t)

(defmacro lao-quail-define-rules (&rest rules)
  (let ((l rules)
	consonant-alist
	vowel-upper-alist
	vowel-lower-alist
	semivowel-lower-alist
	rule trans ch c-set)
    (while l
      (setq rule (car l))
      (setq trans (nth 1 rule))
      (if (consp trans)
	  (setq trans (car trans)))
      (setq c-set (char-category-set (string-to-char trans)))
      (cond ((aref c-set ?2)		; vowel upper
	     (setq consonant-alist
		   (cons (cons (string-to-char (car rule))
			       'lao-vowel-upper-input)
			 consonant-alist)))
	    ((aref c-set ?3)		; vowel lower
	     (setq consonant-alist
		   (cons (cons (string-to-char (car rule))
			       'lao-vowel-lower-input)
			 consonant-alist)
		   semivowel-lower-alist
		   (cons (cons (string-to-char (car rule))
			       'lao-vowel-lower-input)
			 semivowel-lower-alist)))
	    ((aref c-set ?4)		; tone
	     (setq consonant-alist
		   (cons (cons (string-to-char (car rule))
			       'lao-tone-input)
			 consonant-alist)
		   vowel-upper-alist
		   (cons (cons (string-to-char (car rule))
			       'lao-tone-input)
			 vowel-upper-alist)
		   vowel-lower-alist
		   (cons (cons (string-to-char (car rule))
			       'lao-tone-input)
			 vowel-lower-alist)))
	    ((aref c-set ?9)		; semivowel lower
	     (setq consonant-alist
		   (cons (cons (string-to-char (car rule))
			       'lao-semivowel-lower-input)
			 consonant-alist)
		   vowel-upper-alist
		   (cons (cons (string-to-char (car rule))
			       'lao-semivowel-lower-input)
			 vowel-upper-alist))))
      (setq l (cdr l)))
    (list 'progn
	  (cons 'quail-define-rules rules)
	  `(setq lao-consonant-alist ',consonant-alist
		 lao-vowel-upper-alist ',vowel-upper-alist
		 lao-vowel-lower-alist ',vowel-lower-alist
		 lao-semivowel-lower-alist ',semivowel-lower-alist))))

(lao-quail-define-rules
 ("!" "1")
 ("\"" "=")
 ("#" "3")
 ("$" "4")
 ("&" "5")
 ("%" "(1l(B")
 ("'" ("(1'(B" . lao-consonant-input))
 ("(" "7")
 (")" "8")
 ("*" "6")
 ("+" "0(1mh1(B")
 ("," ("(1A(B" . lao-consonant-input))
 ("-" ("(1*(B" . lao-consonant-input))
 ("." "(1c(B")
 ("/" ("(1=(B" . lao-consonant-input))
 ("0" ("(1"(B" . lao-consonant-input))
 ("1" ("(1B(B" . lao-consonant-input))
 ("2" ("(1?(B" . lao-consonant-input))
 ("3" "(1b(B")
 ("4" ("(16(B" . lao-consonant-input))
 ("5" "(1X(B")
 ("6" "(1Y(B")
 ("7" ("(1$(B" . lao-consonant-input))
 ("8" ("(15(B" . lao-consonant-input))
 ("9" ("(1((B" . lao-consonant-input))
 (":" "%")
 (";" ("(1G(B" . lao-consonant-input))
 ("<" ("(1}(B" . lao-consonant-input))
 ("=" "(1m(B")
 (">" "$")
 ("?" ")")
 ("@" "2")
 ("A" "0(1Qi1(B")
 ("B" "0(1Vi1(B")
 ("C" "(1O(B")
 ("D" ".")
 ("E" "0(1Si1(B")
 ("F" ",")
 ("G" ":")
 ("H" "(1j(B")
 ("I" ("(1N(B" . lao-consonant-input))
 ("J" "(1k(B")
 ("K" "!")
 ("L" "?")
 ("M" "(1f(B")
 ("N" "0(1Wi1(B")
 ("O" ("(1|(B" . lao-consonant-input))
 ("P" "(1](B")
 ("Q" "0(1[i1(B")
 ("R" "_")
 ("S" ";")
 ("T" "+")
 ("U" "0(1Ui1(B")
 ("V" "x")
 ("W" "0")
 ("X" "(")
 ("Y" "0(1Ti1(B")
 ("Z" "\"")
 ("[" ("(1:(B" . lao-consonant-input))
 ("]" ("(1E(B" . lao-consonant-input))
 ("^" "(1\(B")
 ("_" "9")
 ("`" ("(1'(B" . lao-consonant-input))
 ("a" "(1Q(B")
 ("b" "(1V(B")
 ("c" "(1a(B")
 ("d" ("(1!(B" . lao-consonant-input))
 ("e" "(1S(B")
 ("f" ("(14(B" . lao-consonant-input))
 ("g" "(1`(B")
 ("h" "(1i(B")
 ("i" ("(1C(B" . lao-consonant-input))
 ("j" "(1h(B")
 ("k" "(1R(B")
 ("l" ("(1J(B" . lao-consonant-input))
 ("m" ("(17(B" . lao-consonant-input))
 ("n" "(1W(B")
 ("o" ("(19(B" . lao-consonant-input))
 ("p" ("(1-(B" . lao-consonant-input))
 ("q" "(1[(B")
 ("r" ("(1>(B" . lao-consonant-input))
 ("s" ("(1K(B" . lao-consonant-input))
 ("t" "(1P(B")
 ("u" "(1U(B")
 ("v" ("(1M(B" . lao-consonant-input))
 ("w" "(1d(B")
 ("x" ("(1;(B" . lao-consonant-input))
 ("y" "(1T(B")
 ("z" ("(1<(B" . lao-consonant-input))
 ("{" "-")
 ("|" ("0(1K\1(B" . lao-consonant-input))
 ("}" "/")
 ("~" "(1l(B")
 ("\\0" "(1p(B")
 ("\\1" "(1q(B")
 ("\\2" "(1r(B")
 ("\\3" "(1s(B")
 ("\\4" "(1t(B")
 ("\\5" "(1u(B")
 ("\\6" "(1v(B")
 ("\\7" "(1w(B")
 ("\\8" "(1x(B")
 ("\\9" "(1y(B")
 )


;;; quail/lao.el ends here
