;;; devanagari.el --- Quail packages for inputting Devanagari  -*-coding: iso-2022-7bit;-*-

;; Copyright (C) 1997 Free Software Foundation, Inc.

;; Author: KAWABATA, Taichi <kawabata@is.s.u-tokyo.ac.jp>

;; Keywords: multilingual, input method, Indian, Devanagari

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
;; 1996.10.10 written by KAWABATA, Taichi <kawabata@is.s.u-tokyo.ac.jp>
;; 1997.03.21 fixed by KAWABATA, Taichi

;; I'm not sure if this keyboard layout is REALLY an ISCII keyboard layout.
;; Please let me know if it is not.

;;; Code:

(require 'quail)
(require 'devan-util)

;; This function does nothing for now.  For future use.
(defun quail-devanagari-update-translation (control-flag)
  (cond ((eq control-flag t) ; terminate translation with the whole key.
	 (insert quail-current-str)
	 (quail-terminate-translation))
	((null control-flag) ; proceed translation with more keys.
	 (insert (or quail-current-str quail-current-key)))
	(t            ; control-flag is the number of keys to be translated.
	 (insert (aref quail-current-key 0))
	 (setq unread-command-events
	       (list (aref quail-current-key control-flag))))))

(defun quail-devanagari-compose-characters ()
  (interactive)
  (if (quail-point-in-conversion-region)
      (let* ((from (overlay-start quail-conv-overlay))
	     (to   (overlay-end quail-conv-overlay))
	     (dstr (buffer-substring from to)))
	(setq overriding-terminal-local-map nil
	      quail-converting nil
	      quail-conversion-str
	      (concat (devanagari-compose-string dstr 'sanskrit)
		      (list (if (numberp last-command-char)
				last-command-char
			      (get last-command-char 'ascii-character))))))))

(quail-define-package 
 "devanagari-keyboard-a" "Devanagari" "DevK" t
 "Devanagari input method with ISCII format

 `$(5!\(B !$(5!.(B @$(5!c(B #x  $x  %x  ^x  &$(5")(B *$(5"((B (   )   _$(5!#(B +$(5!*(B
 ~$(5!/(B 1$(5!r(B 2$(5!s(B 3$(5!t(B 4$(5!u(B 5$(5!v(B 6$(5!w(B 7$(5!x(B 8$(5!y(B 9$(5!z(B 0$(5!q(B -   =$(5!_(B

   Q$(5!f(B W$(5!-(B E$(5!%(B R$(5!'(B T$(5!)(B Y$(5!K(B U$(5!7(B I$(5!E(B O$(5!C(B P$(5!;(B {$(5!@(B }$(5!<(B |$(5!2(B
   q$(5!f(B w$(5!b(B e$(5!Z(B r$(5!\(B t$(5!^(B y$(5!J(B u$(5!X(B i$(5!5(B o$(5!D(B p$(5!:(B [$(5!?(B ]$(5!i(B \\$(5!g(B

     A$(5!0(B S$(5!,(B D$(5!$(B F$(5!&(B G$(5!((B H$(5!I(B J$(5!P(B K$(5!4(B l$(5!C(B :$(5!8(B \"$(5!>(B
     a$(5!e(B s$(5!a(B d$(5!h(B f$(5![(B g$(5!](B h$(5!H(B j$(5!O(B k$(5!3(B l$(5!B(B ;$(5!9(B '$(5!=(B

       Z$(5!+(B X$(5!!(B C$(5!A(B V$(5!G(B B$(5!S(B N$(5!R(B M$(5!U(B <$(5!V(B >$(5!j(B ?$(5!N(B
       z$(5!`(B x$(5!"(B c$(5!L(B v$(5!F(B b$(5!T(B n$(5!Q(B m$(5!W(B ,   .   /$(5!M(B
"
		      nil t t nil nil nil nil nil 
		      ;;'quail-devanagari-update-translation
		      nil
		      '((" "      . quail-devanagari-compose-characters)
			("-"      . quail-devanagari-compose-characters)
			(","      . quail-devanagari-compose-characters)
			("\C-m"   . quail-devanagari-compose-characters)
			([return] . quail-devanagari-compose-characters))
		      )

;;   ..... (not prepared yet)
;;   I forgot where I got this keymap from.  
;;   Please let me know if you know what this keymap is.

(quail-define-rules
 ("`" ?$(5!\(B)
 ("~" ?$(5!/(B)
 ("1" ?$(5!r(B)
 ("!" ?$(5!.(B)
 ("2" ?$(5!s(B)
 ("@" ?$(5!c(B)
 ("3" ?$(5!t(B)
 ("#" ?# ) ; following "r" in keymap...
 ("4" ?$(5!u(B)
 ("$" ?$ ) ; preceding "r" in keymap...
 ("5" ?$(5!v(B)
 ("%" ?x ) ; ??
 ("6" ?$(5!w(B)
 ("^" ?x ) ; %tra
 ("7" ?$(5!x(B)
 ("&" ?$(5")(B) ; % special
 ("8" ?$(5!y(B)
 ("*" ?$(5"((B) ; % special
 ("9" ?$(5!z(B)
 ("(" ?\()
 ("0" ?$(5!q(B)
 (")" ?\))
 ("-" ?-)
 ("_" ?$(5!#(B)
 ("=" ?$(5!_(B)
 ("+" ?$(5!*(B)
 ("q" ?$(5!f(B)
 ("Q" ?$(5!1(B)
 ("w" ?$(5!b(B)
 ("W" ?$(5!-(B)
 ("e" ?$(5!Z(B)
 ("E" ?$(5!%(B)
 ("r" ?$(5!\(B)
 ("R" ?$(5!'(B)
 ("t" ?$(5!^(B)
 ("T" ?$(5!)(B)
 ("y" ?$(5!J(B)
 ("Y" ?$(5!K(B)
 ("u" ?$(5!X(B)
 ("U" ?$(5!7(B)
 ("i" ?$(5!5(B)
 ("I" ?$(5!E(B)
 ("o" ?$(5!D(B)
 ("O" ?$(5!C(B)
 ("p" ?$(5!:(B)
 ("P" ?$(5!;(B)
 ("[" ?$(5!?(B)
 ("{" ?$(5!@(B)
 ("]" ?$(5!i(B)
 ("}" ?$(5!<(B)
 ("\\" ?$(5!g(B)
 ("|" ?$(5!2(B)
 ("a" ?$(5!e(B)
 ("A" ?$(5!0(B)
 ("s" ?$(5!a(B)
 ("S" ?$(5!,(B)
 ("d" ?$(5!h(B)
 ("D" ?$(5!$(B)
 ("f" ?$(5![(B)
 ("F" ?$(5!&(B)
 ("g" ?$(5!](B)
 ("G" ?$(5!((B)
 ("h" ?$(5!H(B)
 ("H" ?$(5!I(B)
 ("j" ?$(5!O(B)
 ("J" ?$(5!P(B)
 ("k" ?$(5!3(B)
 ("K" ?$(5!4(B)
 ("l" ?$(5!B(B)
 ("L" ?$(5!C(B)
 (";" ?$(5!8(B)
 (":" ?$(5!9(B)
 ("'" ?$(5!=(B)
 ("\"" ?$(5!>(B)
 ("z" ?$(5!`(B)
 ("Z" ?$(5!+(B)
 ("x" ?$(5!"(B)
 ("X" ?$(5!!(B)
 ("c" ?$(5!L(B)
 ("C" ?$(5!A(B)
 ("v" ?$(5!F(B)
 ("V" ?$(5!G(B)
 ("b" ?$(5!T(B)
 ("B" ?$(5!S(B)
 ("n" ?$(5!Q(B)
 ("N" ?$(5!R(B)
 ("m" ?$(5!W(B)
 ("M" ?$(5!U(B)
 ;; ("," ?,)
 ("<" ?$(5!V(B)
 ;; ("." ?.)
 (">" ?$(5!j(B)
 ("/" ?$(5!M(B)
 ("?" ?$(5!N(B)
 )


;;
;; Quail Devanagari Input By Transliteration
;;

(eval-when-compile

(defvar devanagari-consonant-transliteration-alist
  '(
    ; GUTTURALS
    ("k" . "$(5!3(B")
    ("k." . "$(5!3!i(B")
    ("kh" . "$(5!4(B")
    ("kh." . "$(5!4!i(B")
    ("g" . "$(5!5(B")
    ("g." . "$(5!5!i(B")
    ("gh" . "$(5!6(B")
    ("G" . "$(5!7(B")
    ; PALATALS
    ("c" . "$(5!8(B")
    ("ch" . "$(5!9(B")
    ("j" . "$(5!:(B")
    ("j." . "$(5!:!i(B")
    ("Z" . "$(5!:!i(B")
    ("jh" . "$(5!;(B")
    ("J" . "$(5!<(B")
    ; CEREBRALS
    ("T" . "$(5!=(B")
    ("Th" . "$(5!>(B")
    ("D" . "$(5!?(B")
    ("D." . "$(5!?!i(B")
    ("Dh" . "$(5!@(B")
    ("Dh." . "$(5!@!i(B")
    ("N" . "$(5!A(B")
    ; DENTALS
    ("t" . "$(5!B(B")
    ("th" . "$(5!C(B")
    ("d" . "$(5!D(B")
    ("dh" . "$(5!E(B")
    ("n" . "$(5!F(B")
    ("N." . "$(5!G(B")
    ; LABIALS
    ("p" . "$(5!H(B")
    ("ph" . "$(5!I(B")
    ("ph." . "$(5!I!i(B")
    ("f" . "$(5!I(B")
    ("f." . "$(5!I!i(B")
    ("b" . "$(5!J(B")
    ("bh" . "$(5!K(B")
    ("m" . "$(5!L(B")
    ; SEMIVOWELS
    ("y" . "$(5!M(B")
    ("y." . "$(5!N(B")
    ("Y" . "$(5!N(B")
    ("r" . "$(5!O(B")
    ("r." . "$(5!P(B")
    ("l" . "$(5!Q(B")
    ("W" . "$(5!R(B")
    ("W." . "$(5!S(B")
    ("v" . "$(5!T(B")
    ("w" . "$(5!T(B")
    ; SIBILANTS
    ("z" . "$(5!U(B")
    ("S" . "$(5!V(B")
    ("s" . "$(5!W(B")
    ("h" . "$(5!X(B")
  ))

(defvar devanagari-vowel-transliteration-alist
  '(
    ;; Special treatment unique to IS 13194 Transliteration
    ("" . "$(5!h(B")
    ("a" . "")
    ; Matra (Vowel Sign)
    ("A" . "$(5!Z(B")
    ("i" . "$(5![(B")
    ("I" . "$(5!\(B")
    ("u" . "$(5!](B")
    ("U" . "$(5!^(B")
    ("R" . "$(5!_(B")
    ;; ("RR" . "x")	; not specified in ordinary IS 13194.(but in Unicode??)
    ("q" . "$(5#K(B")  ; "$(5#K(B" = "$(5!_!i(B" in IS 13194.
    ("L" . "$(5#L(B")  ; "$(5#L(B" = "$(5![!i(B" in IS 13194.
    ("E" . "$(5#M(B")  ; "$(5#M(B" = "$(5!\!i(B" in IS 13194.
    ("E" . "$(5!`(B")  ; only for transcription of other scripts.
    ("e" . "$(5!a(B")
    ("ai" . "$(5!b(B")
    ("ae" . "$(5!b(B") ; variation of transliteration.
    ("EE" . "$(5!c(B") ; only for transcription of other scripts. (Candra E)
    ("O" . "$(5!d(B")  ; only for transcription of other scripts.
    ("o" . "$(5!e(B")
    ("au" . "$(5!f(B")
    ("ao" . "$(5!f(B") ; variation of transliteration.
    ("OO" . "$(5!g(B") ; only for transcription of other scripts. (Candra O)
    ))

;;
;; Independent vowels and other signs.
;;

(defvar devanagari-other-letters-alist
  '(
    ("a" . "$(5!$(B")
    ("A" . "$(5!%(B")
    ("i" . "$(5!&(B")
    ("I" . "$(5!'(B")
    ("u" . "$(5!((B")
    ("U" . "$(5!)(B")
    ("R" . "$(5!*(B")
    ;; ("RR" . "x")	; not specified in IS 13194. (but in Unicode??)
    ("q" . "$(5#*(B")  ; "$(5#*(B" = "$(5!*!i(B" in IS 13194.
    ("L" . "$(5#&(B")  ; "$(5#&(B" = "$(5!&!i(B" in IS 13194.
    ("E" . "$(5#'(B")  ; "$(5#'(B" = "$(5!'!i(B" in IS 13194.
    ("Ex" . "$(5!+(B") ; only for transcription of other scripts.
    ("e" . "$(5!,(B")
    ("ai" . "$(5!-(B")
    ("EE" . "$(5!.(B") ; only for transcription of other scripts. (Candra E)
    ("O" . "$(5!/(B")  ; only for transcription of other scripts.
    ("o" . "$(5!0(B")
    ("au" . "$(5!1(B")
    ("ao" . "$(5!1(B") ; variation of transliteration.
    ("OO" . "$(5!2(B") ; only for transcription of other scripts. (Candra O)
    ("'" . "$(5#J(B") ; avagraha
    ("@" . "$(5#!(B") ; OM
    ("/" . "$(5!j(B")
    ("M" . "$(5!"(B")
    ("&" . "$(5!!(B")
    ("H" . "$(5!#(B")
    ("." . "$(5!i(B")        ; Nukta
    ("0" . "$(5!q(B")
    ("1" . "$(5!r(B")
    ("2" . "$(5!s(B")
    ("3" . "$(5!t(B")
    ("4" . "$(5!u(B")
    ("5" . "$(5!v(B")
    ("6" . "$(5!w(B")
    ("7" . "$(5!x(B")
    ("8" . "$(5!y(B")
    ("9" . "$(5!z(B")
    ))
)

(defmacro devanagari-transliteration-quail-define-rules ()
  (cons 'quail-define-rules
	(let ((cl devanagari-consonant-transliteration-alist)
	      (ml devanagari-other-letters-alist) rules)
	  (while cl
	    (let ((vl devanagari-vowel-transliteration-alist))
	      (while vl
		(setq rules 
		      (cons (list (concat (car (car cl)) (car (car vl)))
				  (make-vector 1
				   (concat (cdr (car cl)) (cdr (car vl)))))
			    rules))
		(setq vl (cdr vl))))
	    (setq cl (cdr cl)))
	  (while ml
	    (setq rules (cons (list (car (car ml)) 
				    (make-vector 1 (cdr (car ml))))
			      rules))
	    (setq ml (cdr ml)))
	  rules)))

(quail-define-package
 "devanagari-transliteration" "Devanagari" "DEVt" t
 "Devanagari input method by transliteration
VOWELS     :   a  $(5!$(B   A  $(5!%(B   i  $(5!&(B   I  $(5!'(B   u  $(5!((B   U  $(5!)(B
               R  $(5!*(B   q  $(5#*(B   L  $(5#&(B   E  $(5#'(B   Ex $(5!+(B   e  $(5!,(B
               ai $(5!-(B   EE $(5!.(B   O  $(5!/(B   o  $(5!0(B   au $(5!1(B   OO $(5!2(B
GRUTTALS   :   k  $(5!3(B   kh $(5!4(B   g  $(5!5(B   gh $(5!6(B   G  $(5!7(B
PALATALS   :   c  $(5!8(B   ch $(5!9(B   j  $(5!:(B   jh $(5!;(B   J  $(5!<(B   (Z  $(5!:!i(B)
CEREBRALS  :   T  $(5!=(B   Th $(5!>(B   D  $(5!?(B   Dh $(5!@(B   N  $(5!A(B
DENTALS    :   t  $(5!B(B   th $(5!C(B   d  $(5!D(B   dh $(5!E(B   n  $(5!F(B   (Nq $(5!G(B)
LABIALS    :   p  $(5!H(B   ph $(5!I(B   b  $(5!J(B   bh $(5!K(B   m  $(5!L(B   (f  $(5!I(B)
SEMIVOWELS :   y  $(5!M(B   Y  $(5!N(B   r  $(5!O(B   Rq $(5!P(B   
               l  $(5!Q(B   W  $(5!R(B   W. $(5!S(B   v  $(5!T(B   w  $(5!T(B
SIBILANTS  :   z  $(5!U(B   S  $(5!V(B   s  $(5!W(B   h  $(5!X(B

Specials   :   Anuswar       M  $(5!"(B     Visarg        H  $(5!#(B
               Chandrabindu  &  $(5!!(B     Nukta         .  $(5!i(B
               Danda         /   $(5!j(B    Avagrah       '  $(5#J(B
               OM            @   $(5#!(B
" 
		      nil t t nil nil nil nil nil 
		      ;; 'quail-devanagari-update-translation
		      nil
		      '((" "      . quail-devanagari-compose-characters)
			("-"      . quail-devanagari-compose-characters)
			(","      . quail-devanagari-compose-characters)
			("\C-m"   . quail-devanagari-compose-characters)
			([return] . quail-devanagari-compose-characters))
		      )

(devanagari-transliteration-quail-define-rules)

;;
;;  ITRANS - Indian Script Translation
;;

(eval-and-compile

(defun rule-indian-to-devanagari (alist)
  (if (null alist) nil
    (cons (cons (car (car alist)) 
		(indian-to-devanagari-string (cdr (car alist))))
	  (rule-indian-to-devanagari (cdr alist)))))
)

(eval-when-compile

(defvar devanagari-consonant-itrans-alist
  (rule-indian-to-devanagari indian-itrans-consonant-alist))

(defvar devanagari-vowel-itrans-alist
  (rule-indian-to-devanagari indian-itrans-vowel-sign-alist))

(defvar devanagari-other-letters-itrans-alist
  (rule-indian-to-devanagari indian-itrans-other-letters-alist))

)

(defmacro devanagari-itrans-quail-define-rules ()
  (cons 'quail-define-rules
	(let ((cl devanagari-consonant-itrans-alist)
	      (ml devanagari-other-letters-itrans-alist) rules)
	  (while cl
	    (let ((vl devanagari-vowel-itrans-alist))
	      (while vl
		(setq rules 
		      (cons (list (concat (car (car cl)) (car (car vl)))
				  (make-vector 1
				   (concat (cdr (car cl)) (cdr (car vl)))))
			    rules))
		(setq vl (cdr vl))))
	    (setq cl (cdr cl)))
	  (while ml
	    (setq rules (cons (list (car (car ml)) 
				    (make-vector 1 (cdr (car ml))))
			      rules))
	    (setq ml (cdr ml)))
	  rules)))

(quail-define-package
 "devanagari-itrans" "Devanagari" "DEVi" t
 "Devanagari input method by ITRANS
Special Keys : Anuswar         n'
               Chandrabindu    nn'
               Visarg          nh
               Nukta           type capital letter for first character.
               $(5!7(B(ng) $(5!<(B(ny) $(5!A(B(nn) $(5!F(B(n) $(5!G(B(nnn)
" 
		      nil t t nil nil nil nil nil 
		      ;; 'quail-devanagari-update-translation
		      nil
		      '((" "      . quail-devanagari-compose-characters)
			("-"      . quail-devanagari-compose-characters)
			(","      . quail-devanagari-compose-characters)
			("\C-m"   . quail-devanagari-compose-characters)
			([return] . quail-devanagari-compose-characters))
		      )

(devanagari-itrans-quail-define-rules)


;;
;; Quail Hindi Input By Transliteration
;;

(defun quail-devanagari-hindi-compose-characters ()
  (interactive)
  (if (quail-point-in-conversion-region)
      (let* ((from (overlay-start quail-conv-overlay))
	     (to   (overlay-end quail-conv-overlay))
	     (dstr (buffer-substring from to)))
	(setq overriding-terminal-local-map nil
	      quail-converting nil
	      quail-conversion-str
	      (concat (devanagari-compose-string dstr)
		      (list (if (numberp last-command-char)
				last-command-char
			      (get last-command-char 'ascii-character))))))))

(eval-when-compile

(defvar devanagari-hindi-consonant-transliteration-alist
  '(
    ; GUTTURALS
    ("k" . "$(5!3(B")
    ("ks" . "$(5$.(B")
    ("k." . "$(5!3!i(B")
    ("kh" . "$(5!4(B")
    ("kh." . "$(5!4!i(B")
    ("g" . "$(5!5(B")
    ("g." . "$(5!5!i(B")
    ("gh" . "$(5!6(B")
    ("G" . "$(5!7(B")
    ; PALATALS
    ("ch" . "$(5!8(B")
    ("chh" . "$(5!9(B")
    ("j" . "$(5!:(B")
    ("j." . "$(5!:!i(B")
    ("z" . "$(5!:!i(B")
    ("jh" . "$(5!;(B")
    ("J" . "$(5!<(B")
    ; CEREBRALS
    ("T" . "$(5!=(B")
    ("Th" . "$(5!>(B")
    ("D" . "$(5!?(B")
    ("D." . "$(5!?!i(B")
    ("Dh" . "$(5!@(B")
    ("Dh." . "$(5!@!i(B")
    ("N" . "$(5!A(B")
    ; DENTALS
    ("t" . "$(5!B(B")
    ("th" . "$(5!C(B")
    ("d" . "$(5!D(B")
    ("dh" . "$(5!E(B")
    ("n" . "$(5!F(B")
    ("N." . "$(5!G(B")
    ; LABIALS
    ("p" . "$(5!H(B")
    ("ph" . "$(5!I(B")
    ("ph." . "$(5!I!i(B")
    ("f" . "$(5!I(B")
    ("f." . "$(5!I!i(B")
    ("b" . "$(5!J(B")
    ("bh" . "$(5!K(B")
    ("m" . "$(5!L(B")
    ; SEMIVOWELS
    ("y" . "$(5!M(B")
    ("y." . "$(5!N(B")
    ("Y" . "$(5!N(B")
    ("r" . "$(5!O(B")
    ("r." . "$(5!P(B")
    ("l" . "$(5!Q(B")
    ("W" . "$(5!R(B")
    ("W." . "$(5!S(B")
    ("v" . "$(5!T(B")
    ("w" . "$(5!T(B")
    ; SIBILANTS
    ("sh" . "$(5!U(B")
    ("S" . "$(5!V(B")
    ("s" . "$(5!W(B")
    ("h" . "$(5!X(B")
    ; Special for Hindi
    ("ks" . "$(5$.(B")
    ("tr" . "$(5"%(B")
    ("xn" . "$(5$E(B")
  ))

(defvar devanagari-hindi-vowel-transliteration-alist
  '(
    ; In hindi, halant sign is rarely used so should explicity typed in.
    ("" . "")
    ("~" . "$(5!h(B")
    ; Matra (Vowel Sign)
    ("a" . "$(5!Z(B")
    ("i" . "$(5![(B")
    ("I" . "$(5!\(B")
    ("u" . "$(5!](B")
    ("U" . "$(5!^(B")
    ("R" . "$(5!_(B")
    ;; ("RR" . "x")	; not specified in ordinary IS 13194.(but in Unicode??)
    ("q" . "$(5#K(B")  ; "$(5#K(B" = "$(5!_!i(B" in IS 13194.
    ("L" . "$(5#L(B")  ; "$(5#L(B" = "$(5![!i(B" in IS 13194.
    ("E" . "$(5#M(B")  ; "$(5#M(B" = "$(5!\!i(B" in IS 13194.
    ("E" . "$(5!`(B")  ; only for transcription of other scripts.
    ("e" . "$(5!a(B")
    ("ai" . "$(5!b(B")
    ("ae" . "$(5!b(B") ; variation of transliteration.
    ("EE" . "$(5!c(B") ; only for transcription of other scripts. (Candra E)
    ("O" . "$(5!d(B")  ; only for transcription of other scripts.
    ("o" . "$(5!e(B")
    ("au" . "$(5!f(B")
    ("ao" . "$(5!f(B") ; variation of transliteration.
    ("OO" . "$(5!g(B") ; only for transcription of other scripts. (Candra O)
    ))

;;
;; Independent vowels and other signs.
;;

(defvar devanagari-hindi-other-letters-alist
  '(
    ("a" . "$(5!$(B")
    ("A" . "$(5!%(B")
    ("i" . "$(5!&(B")
    ("I" . "$(5!'(B")
    ("u" . "$(5!((B")
    ("U" . "$(5!)(B")
    ("R" . "$(5!*(B")
    ;; ("RR" . "x")	; not specified in IS 13194. (but in Unicode??)
    ("q" . "$(5#*(B")  ; "$(5#*(B" = "$(5!*!i(B" in IS 13194.
    ("L" . "$(5#&(B")  ; "$(5#&(B" = "$(5!&!i(B" in IS 13194.
    ("E" . "$(5#'(B")  ; "$(5#'(B" = "$(5!'!i(B" in IS 13194.
    ("Ex" . "$(5!+(B") ; only for transcription of other scripts.
    ("e" . "$(5!,(B")
    ("ai" . "$(5!-(B")
    ("EE" . "$(5!.(B") ; only for transcription of other scripts. (Candra E)
    ("O" . "$(5!/(B")  ; only for transcription of other scripts.
    ("o" . "$(5!0(B")
    ("au" . "$(5!1(B")
    ("ao" . "$(5!1(B") ; variation of transliteration.
    ("OO" . "$(5!2(B") ; only for transcription of other scripts. (Candra O)
    ("'" . "$(5#J(B") ; avagraha
    ("@" . "$(5#!(B") ; OM
    ("/" . "$(5!j(B")
    ("M" . "$(5!"(B")
    ("&" . "$(5!!(B")
    ("H" . "$(5!#(B")
    ("." . "$(5!i(B")        ; Nukta
    ("0" . "$(5!q(B")
    ("1" . "$(5!r(B")
    ("2" . "$(5!s(B")
    ("3" . "$(5!t(B")
    ("4" . "$(5!u(B")
    ("5" . "$(5!v(B")
    ("6" . "$(5!w(B")
    ("7" . "$(5!x(B")
    ("8" . "$(5!y(B")
    ("9" . "$(5!z(B")
    ))
)

(defmacro devanagari-hindi-transliteration-quail-define-rules ()
  (cons 'quail-define-rules
	(let ((cl devanagari-hindi-consonant-transliteration-alist)
	      (ml devanagari-hindi-other-letters-alist) rules)
	  (while cl
	    (let ((vl devanagari-hindi-vowel-transliteration-alist))
	      (while vl
		(setq rules 
		      (cons (list (concat (car (car cl)) (car (car vl)))
				  (make-vector 1
				   (concat (cdr (car cl)) (cdr (car vl)))))
			    rules))
		(setq vl (cdr vl))))
	    (setq cl (cdr cl)))
	  (while ml
	    (setq rules (cons (list (car (car ml)) 
				    (make-vector 1 (cdr (car ml))))
			      rules))
	    (setq ml (cdr ml)))
	  rules)))

(quail-define-package
 "devanagari-hindi-transliteration" "Hindi" "HINt" t
 "Devanagari-Hindi input method by transliteration
VOWELS     :   a  $(5!$(B   A  $(5!%(B   i  $(5!&(B   I  $(5!'(B   u  $(5!((B   U  $(5!)(B
               R  $(5!*(B   q  $(5#*(B   L  $(5#&(B   E  $(5#'(B   Ex $(5!+(B   e  $(5!,(B
               ai $(5!-(B   EE $(5!.(B   O  $(5!/(B   o  $(5!0(B   au $(5!1(B   OO $(5!2(B
GRUTTALS   :   k  $(5!3(B   kh $(5!4(B   g  $(5!5(B   gh $(5!6(B   G  $(5!7(B
PALATALS   :   c  $(5!8(B   ch $(5!9(B   j  $(5!:(B   jh $(5!;(B   J  $(5!<(B   z  $(5!:!i(B
CEREBRALS  :   T  $(5!=(B   Th $(5!>(B   D  $(5!?(B   Dh $(5!@(B   N  $(5!A(B
DENTALS    :   t  $(5!B(B   th $(5!C(B   d  $(5!D(B   dh $(5!E(B   n  $(5!F(B   (Nq $(5!G(B)
LABIALS    :   p  $(5!H(B   ph $(5!I(B   b  $(5!J(B   bh $(5!K(B   m  $(5!L(B   (f  $(5!I(B)
SEMIVOWELS :   y  $(5!M(B   Y  $(5!N(B   r  $(5!O(B   Rq $(5!P(B   
               l  $(5!Q(B   W  $(5!R(B   W. $(5!S(B   v  $(5!T(B   w  $(5!T(B
SIBILANTS  :   sh $(5!U(B   S  $(5!V(B   s  $(5!W(B   h  $(5!X(B
OTHERS     :   ks $(5$.(B   tr $(5"%(B   xn $(5$E(B

Specials   :   Anuswar       M  $(5!"(B     Visarg        H  $(5!#(B
               Chandrabindu  &  $(5!!(B     Nukta         .  $(5!i(B
               Danda         /   $(5!j(B    Avagrah       '  $(5#J(B
               OM            @   $(5#!(B    Halant        ~  $(5!h(B
" 
		      nil t t nil nil nil nil nil 
		      ;; 'quail-devanagari-update-translation
		      nil
		      '((" "      . quail-devanagari-hindi-compose-characters)
			("-"      . quail-devanagari-hindi-compose-characters)
			(","      . quail-devanagari-hindi-compose-characters)
			("\C-m"   . quail-devanagari-hindi-compose-characters)
			([return] . quail-devanagari-hindi-compose-characters))
		      )

(devanagari-hindi-transliteration-quail-define-rules)

;;; devanagari.el ends here
