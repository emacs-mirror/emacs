;;; indian.el --- Support for Indian Languages -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1995 Free Software Foundation, Inc.

;; Author: KAWABATA, Taichi <kawabata@is.s.u-tokyo.ac.jp>

;; Keywords: multilingual, Indian

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

;;; Commentary:

;; History:
;; 1996.10.18 written by KAWABATA, Taichi <kawabata@is.s.u-tokyo.ac.jp>

;; For Indian, the character set IS 13194 is supported.
;;
;; IS 13194 does not specifically assign glyphs for each characters.
;; Following code is not specific to each Indian language.
;;
;; Eventually, this code will support generic information about
;; following scripts.
;;
;;    Devanagari
;;    Bengali
;;    Gurmukhi
;;    Gujarati
;;    Oriya
;;    Tamil
;;    Telgu
;;    Kannada
;;    Malayalam
;;
;; In this file, charsets other than charset-ascii and charset-indian-is13194
;; should not be used except in the comment.

;;; Code:

;;  Followings are what you see when you refer to the Emacs
;;  representations of IS 13194 charcters.  However, this is merely
;;  tentative apperance, and you must convert them by
;;  indian-to-xxxxxx(specific script) function to use them.
;;  Devanagari is not an exception of this rule.

;;     0xa0 //(5!"#$%&'()*+,-./(B
;;     0xb0 (50123456789:;<=>?(B
;;     0xc0 (5@ABCDEFGHIJKLMNO(B
;;     0xd0 (5PQRSTUVWXYZ[\]^_(B
;;     0xe0 (5`abcdefghijklmno(B
;;     0xf0 (5pqrstuvwxyz{|}~(B//

;; Note - In IS 13194, several symbols are obtained by special
;; combination of several characters and Nukta sign.
;;
;;   Sanskrit Vowel R  -> (5*(B + (5i(B
;;   Sanskrit Vowel L  -> (5&(B + (5i(B
;;   Sanskrit Vowel LL -> (5'(B + (5i(B
;;   Sanskrit Avagrah  -> (5j(B + (5i(B
;;   OM                -> (5!(B + (5i(B
;;
;; Note - IS 13194 defines ATR(0xEF) and EXT(0xF0), but they are
;; not used in Emacs.
;;
;; Note - the above characters DO NOT represent any script.  For
;; example, if you want to obtain Devanagari character, you must do
;; something like the following.
;;
;;   (char-to-string (indian-to-devanagari ?(5$(B))
;;   "$(5!$(B"

;;; ITRANS
;;
;; ITRANS is one of the most popular method to exchange indian scripts
;; electronically.  Here is the table to convert between ITRANS code and
;; IS 13194 code.

(defvar indian-itrans-consonant-alist
  '(
    ("k" . "(53(B")
    ("kh" . "(54(B")
    ("g" . "(55(B")
    ("gh" . "(56(B")
    ("N^" . "(57(B")
    ("ch" . "(58(B")
    ("chh" . "(59(B")
    ("j" . "(5:(B")
    ("jh" . "(5;(B")
    ("JN" . "(5<(B")
    ("T" . "(5=(B")
    ("Th" . "(5>(B")
    ("D" . "(5?(B")
    ("Dh" . "(5@(B")
    ("N" . "(5A(B")
    ("t" . "(5B(B")
    ("th" . "(5C(B")
    ("d" . "(5D(B")
    ("dh" . "(5E(B")
    ("n" . "(5F(B")
    ("nh" . "(5G(B")     ; For transcription of non-Devanagari Languages.
    ("p" . "(5H(B")
    ("ph" . "(5I(B")
    ("b" . "(5J(B")
    ("bh" . "(5K(B")
    ("m" . "(5L(B")
    ("y" . "(5M(B")
    ("yh" . "(5N(B")      ; For transcription of non-Devanagari Languages.
    ("r" . "(5O(B")
    ("rh" . "(5P(B")      ; For transcription of non-Devanagari Languages.
    ("l" . "(5Q(B")
    ("v" . "(5T(B")
    ("sh" . "(5U(B")
    ("shh" . "(5V(B")
    ("s" . "(5W(B")
    ("h" . "(5X(B")
    ("ld" . "(5R(B")
    ("L" . "(5R(B")
    ("ksh" . "$(5!3!h!V(B")
    ("GY" . "***GY***")  ; Must check out later.
    ;; special consonants
    ("q" . "(53i(B")
    ("K" . "(54i(B")
    ("G" . "(55i(B")
    ("z" . "(5:i(B")
    ("f" . "(5Ii(B")
    (".D" . "(5?i(B")
    (".Dh" . "(5@i(B")
  ))

(defvar indian-itrans-vowel-sign-alist
  '(
    ;; Special treatment unique to IS 13194 Transliteration
    ("" . "(5h(B")
    ("a" . "")
    ;; Matra (Vowel Sign)
    ("aa" . "(5Z(B")
    ("A" . "(5Z(B")
    ("i" . "(5[(B")
    ("ii" . "(5\(B")
    ("I" . "(5\(B")
    ("u" . "(5](B")
    ("uu" . "(5^(B")
    ("U" . "(5^(B")
    ("R^i" . "(5_(B")     ; These must be checked out later.
    ("R^I" . "(5_i(B")
    ("L^i" . "(5[i(B")
    ("L^I" . "(5\i(B")
    ("E" . "(5`(B")       ; For transcription of non-Devanangri Languages.
    ("e" . "(5a(B")
    ("ai" . "(5b(B") 
    ;; ("e.c" . "(5c(B")     ; Tentatively suppressed.
    ("O" . "(5d(B")       ; For transcription of non-Devanagari Languages.
    ("o" . "(5e(B")
    ("au" . "(5f(B")
    ;; ("o.c" . "(5g(B")     ; Tentatively suppressed.
    ))

;;
;; Independent vowels and other signs.
;;

(defvar indian-itrans-other-letters-alist
  '(
    ("a" . "(5$(B")
    ("aa" . "(5%(B")
    ("A" . "(5%(B")
    ("i" . "(5&(B")
    ("ii" . "(5'(B")
    ("I" . "(5'(B")
    ("u" . "(5((B")
    ("uu" . "(5)(B")
    ("U" . "(5)(B")
    ("R^i" . "(5*(B")
    ("R^I" . "(5*i(B")
    ("L^i" . "(5&i(B")
    ("L^I" . "(5'i(B")
    ("E" . "(5+(B")	; For transcription of non-Devanagari Languages.
    ("e" . "(5,(B")
    ("ai" . "(5-(B")
    ;; ("e.c" . "(5.(B")	; Candra E
    ("O" . "(5/(B")	; For transcription of non-Devanagari Languages.
    ("o" . "(50(B")
    ("au" . "(51(B")
    ;; ("o.c" . "(52(B")	; Candra O
    ("M" . "(5$(B")
    ("H" . "(5#(B")
    ("AUM" . "(5!i(B")
    ("OM" . "(5!i(B")
    (".r" . "(5Oh(B")
    (".n" . "(5"(B")
    (".N" . "(5!(B")
    (".h" . "(5h(B")        ; Halant
    (".." . "(5j(B")
    (".a" . "(5ji(B")      ; Avagrah
    ("0" . "(5q(B")
    ("1" . "(5r(B")
    ("2" . "(5s(B")
    ("3" . "(5t(B")
    ("4" . "(5u(B")
    ("5" . "(5v(B")
    ("6" . "(5w(B")
    ("7" . "(5x(B")
    ("8" . "(5y(B")
    ("9" . "(5z(B")
    ))

;; Regular expression matching single Indian character represented
;; by ITRANS.

(defvar indian-itrans-regexp
  (let ((consonant "\\([cs]hh?\\)\\|[kgjTDnpbyr]h?\\|\\(N\\^?\\)\\|\\(jN\\)\\|[mvqKGzfs]\\|\\(ld?\\)\\|\\(ksh\\)\\|\\(GY\\)\\|\\(\\.Dh?\\)")
	(vowel "\\(a[aiu]\\)\\|\\(ii\\)\\|\\(uu\\)\\|\\([RL]\\^[iI]\\)\\|[AIEOeoaiu]")
	(misc "[MH0-9]\\|\\(AUM\\)\\|\\(OM\\)\\|\\(\\.[rnNh\\.a]\\)")
	(lpre "\\(") (rpre "\\)") (orre "\\|"))
    (concat lpre misc rpre orre
	    lpre lpre consonant rpre "?" lpre vowel rpre rpre orre
	    lpre consonant rpre )))

;;
;; Regular expression matching single ITRANS unit for IS 13194 characters.
;;

(defvar itrans-indian-regexp
  (let ((vowel "[(5$(B-(52(B]")
	(consonant "[(53(B-(5X(B]")
	(matra "[(5Z(B-(5g(B]")
	(misc "[(5q(B-(5z(B]")
	(lpre "\\(") (rpre "\\)") (orre "\\|"))
    (concat misc orre
	    lpre consonant matra "?" rpre orre
	    vowel)))

;;
;; IS13194 - ITRANS conversion table for string matching above regexp.
;;

(defvar indian-itrans-alist
  (let ((cl indian-itrans-consonant-alist)
	(ml indian-itrans-other-letters-alist) rules)
	  (while cl
	    (let ((vl indian-itrans-vowel-sign-alist))
	      (while vl
		(setq rules 
		      (cons (cons (concat (car (car cl)) (car (car vl)))
				  (concat (cdr (car cl)) (cdr (car vl))))
			    rules))
		(setq vl (cdr vl))))
	    (setq cl (cdr cl)))
	  (while ml
	    (setq rules (cons (cons (car (car ml)) 
				    (cdr (car ml)))
			      rules))
	    (setq ml (cdr ml)))
	  rules))

;;
;; Utility program to convert from ITRANS to IS 13194 in specified region.
;;

(defun indian-decode-itrans-region (from to)
  "Convert `ITRANS' mnemonics of the current region to Indian characters.
When called from a program, expects two arguments,
positions (integers or markers) specifying the stretch of the region."
  (interactive "r")
  (save-restriction
    (narrow-to-region from to)
    (goto-char (point-min))
    (while (re-search-forward indian-itrans-regexp nil t)
      (let* ((itrans (buffer-substring (match-beginning 0) (match-end 0)))
	     (ch (cdr (assoc itrans indian-itrans-alist))))
	(if ch
	    (progn
	      (delete-region (match-beginning 0) (match-end 0))
	      (insert ch)))))
    (goto-char (point-min))
    (while (re-search-forward "\\((5h(B\\)[^\\c0]" nil t)
      (delete-region (match-beginning 1) (match-end 1)))))

;;
;; Utility program to convert from IS 13194 to ITRANS in specified region.
;;

(defun indian-encode-itrans-region (from to)
  "Convert indian region to ITRANS mnemonics."
  (interactive "r")
  (save-restriction
    (narrow-to-region from to)
    (goto-char (point-min))
    (while (re-search-forward itrans-indian-regexp nil t)
      (let* ((indian (buffer-substring (match-beginning 0) (match-end 0)))
	     (ch (car (rassoc indian indian-itrans-alist))))
	(if ch
	    (progn
	      (delete-region (match-beginning 0) (match-end 0))
	      (insert ch)))))
    (goto-char (point-min))))

(provide 'indian)
  
;;; indian.el ends here
