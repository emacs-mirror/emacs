;;; thai-util.el --- utilities for Thai -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1995, 1997, 1998, 1999, 2000, 2001, 2005
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021
;; Copyright (C) 2000, 2001 Free Software Foundation, Inc.

;; Keywords: mule, multilingual, thai

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

;; Setting information of Thai characters.

(defconst thai-category-table (make-category-table))
(define-category ?c "Thai consonant" thai-category-table)
(define-category ?v "Thai upper/lower vowel" thai-category-table)
(define-category ?t "Thai tone mark" thai-category-table)
(define-category ?u "Thai tone mark and upper sign" thai-category-table)
(define-category ?I "THAI CHARACTER SARA I" thai-category-table)
(define-category ?U "THAI CHARACTER THANTHAKHAT" thai-category-table)

;; The general composing rules are as follows:
;;
;;                          T
;;       V        U         V                  U
;; CV -> C, CU -> C, CVT -> C, Cv -> C, CvU -> C
;;                                   v         v
;;
;; where C: consonant, V: vowel upper, v: vowel lower,
;;       T: tone mark, U: tone mark and upper sign.
;; Special rule: The sign `,Tl(B' can be put on the vowel `,TT(B'.


(defvar thai-composition-pattern
  "\\cc\\(\\cu\\|\\cI\\cU\\|\\cv\\ct?\\)\\|\\cv\\ct\\|\\cI\\cU"
  "Regular expression matching a Thai composite sequence.")

(defun thai-self-insert-command (&optional n)
  "Insert the Thai character you type.
The character will be composed with the surrounding Thai character
if necessary."
  (interactive "*p")
  (let ((pos (point))
	category-set ch)
    (self-insert-command n)
    (or thai-auto-composition-mode
	(thai-auto-composition (1- (point)) (point) 0))))

(let ((l '((?,T!(B consonant "LETTER KO KAI")				; 0xA1
	   (?,T"(B consonant "LETTER KHO KHAI")				; 0xA2
	   (?,T#(B consonant "LETTER KHO KHUAT")				; 0xA3
	   (?,T$(B consonant "LETTER KHO KHWAI")				; 0xA4
	   (?,T%(B consonant "LETTER KHO KHON")				; 0xA5
	   (?,T&(B consonant "LETTER KHO RAKHANG")				; 0xA6
	   (?,T'(B consonant "LETTER NGO NGU")				; 0xA7
	   (?,T((B consonant "LETTER CHO CHAN")				; 0xA8
	   (?,T)(B consonant "LETTER CHO CHING")				; 0xA9
	   (?,T*(B consonant "LETTER CHO CHANG")				; 0xAA
	   (?,T+(B consonant "LETTER SO SO")				; 0xAB
	   (?,T,(B consonant "LETTER CHO CHOE")				; 0xAC
	   (?,T-(B consonant "LETTER YO YING")				; 0xAD
	   (?,T.(B consonant "LETTER DO CHADA")				; 0xAE
	   (?,T/(B consonant "LETTER TO PATAK")				; 0xAF
	   (?,T0(B consonant "LETTER THO THAN")				; 0xB0
	   (?,T1(B consonant "LETTER THO NANGMONTHO")			; 0xB1
	   (?,T2(B consonant "LETTER THO PHUTHAO")				; 0xB2
	   (?,T3(B consonant "LETTER NO NEN")				; 0xB3
	   (?,T4(B consonant "LETTER DO DEK")				; 0xB4
	   (?,T5(B consonant "LETTER TO TAO")				; 0xB5
	   (?,T6(B consonant "LETTER THO THUNG")				; 0xB6
	   (?,T7(B consonant "LETTER THO THAHAN")				; 0xB7
	   (?,T8(B consonant "LETTER THO THONG")				; 0xB8
	   (?,T9(B consonant "LETTER NO NU")				; 0xB9
	   (?,T:(B consonant "LETTER BO BAIMAI")				; 0xBA
	   (?,T;(B consonant "LETTER PO PLA")				; 0xBB
	   (?,T<(B consonant "LETTER PHO PHUNG")				; 0xBC
	   (?,T=(B consonant "LETTER FO FA")				; 0xBD
	   (?,T>(B consonant "LETTER PHO PHAN")				; 0xBE
	   (?,T?(B consonant "LETTER FO FAN")				; 0xBF
	   (?,T@(B consonant "LETTER PHO SAMPHAO")				; 0xC0
	   (?,TA(B consonant "LETTER MO MA")				; 0xC1
	   (?,TB(B consonant "LETTER YO YAK")				; 0xC2
	   (?,TC(B consonant "LETTER RO RUA")				; 0xC3
	   (?,TD(B vowel-base "LETTER RU (Pali vowel letter)")		; 0xC4
	   (?,TE(B consonant "LETTER LO LING")				; 0xC5
	   (?,TF(B vowel-base "LETTER LU (Pali vowel letter)")		; 0xC6
	   (?,TG(B consonant "LETTER WO WAEN")				; 0xC7
	   (?,TH(B consonant "LETTER SO SALA")				; 0xC8
	   (?,TI(B consonant "LETTER SO RUSI")				; 0xC9
	   (?,TJ(B consonant "LETTER SO SUA")				; 0xCA
	   (?,TK(B consonant "LETTER HO HIP")				; 0xCB
	   (?,TL(B consonant "LETTER LO CHULA")				; 0xCC
	   (?,TM(B consonant "LETTER O ANG")				; 0xCD
	   (?,TN(B consonant "LETTER HO NOK HUK")				; 0xCE
	   (?,TO(B special "PAI YAN NOI (abbreviation)")			; 0xCF
	   (?,TP(B vowel-base "VOWEL SIGN SARA A")				; 0xD0
	   (?,TQ(B vowel-upper "VOWEL SIGN MAI HAN-AKAT N/S-T")		; 0xD1
	   (?,TR(B vowel-base "VOWEL SIGN SARA AA")				; 0xD2
	   (?,TS(B vowel-base "VOWEL SIGN SARA AM")				; 0xD3
	   (?,TT(B vowel-upper "VOWEL SIGN SARA I N/S-T")			; 0xD4
	   (?,TU(B vowel-upper "VOWEL SIGN SARA II N/S-T")			; 0xD5
	   (?,TV(B vowel-upper "VOWEL SIGN SARA UE N/S-T")			; 0xD6
	   (?,TW(B vowel-upper "VOWEL SIGN SARA UEE N/S-T")			; 0xD7
	   (?,TX(B vowel-lower "VOWEL SIGN SARA U N/S-B")			; 0xD8
	   (?,TY(B vowel-lower "VOWEL SIGN SARA UU N/S-B")			; 0xD9
	   (?,TZ(B vowel-lower "VOWEL SIGN PHINTHU N/S-B (Pali virama)")	; 0xDA
	   (?,T[(B invalid nil)						; 0xDA
	   (?,T\(B invalid nil)						; 0xDC
	   (?,T](B invalid nil)						; 0xDC
	   (?,T^(B invalid nil)						; 0xDC
	   (?,T_(B special "BAHT SIGN (currency symbol)")			; 0xDF
	   (?,T`(B vowel-base "VOWEL SIGN SARA E")				; 0xE0
	   (?,Ta(B vowel-base "VOWEL SIGN SARA AE")				; 0xE1
	   (?,Tb(B vowel-base "VOWEL SIGN SARA O")				; 0xE2
	   (?,Tc(B vowel-base "VOWEL SIGN SARA MAI MUAN")			; 0xE3
	   (?,Td(B vowel-base "VOWEL SIGN SARA MAI MALAI")			; 0xE4
	   (?,Te(B vowel-base "LAK KHANG YAO")				; 0xE5
	   (?,Tf(B special "MAI YAMOK (repetion)")				; 0xE6
	   (?,Tg(B sign-upper "VOWEL SIGN MAI TAI KHU N/S-T")		; 0xE7
	   (?,Th(B tone "TONE MAI EK N/S-T")				; 0xE8
	   (?,Ti(B tone "TONE MAI THO N/S-T")				; 0xE9
	   (?,Tj(B tone "TONE MAI TRI N/S-T")				; 0xEA
	   (?,Tk(B tone "TONE MAI CHATTAWA N/S-T")				; 0xEB
	   (?,Tl(B sign-upper "THANTHAKHAT N/S-T (cancellation mark)")	; 0xEC
	   (?,Tm(B sign-upper "NIKKHAHIT N/S-T (final nasal)")		; 0xED
	   (?,Tn(B sign-upper "YAMAKKAN N/S-T")				; 0xEE
	   (?,To(B special "FONRMAN")					; 0xEF
	   (?,Tp(B special "DIGIT ZERO")					; 0xF0
	   (?,Tq(B special "DIGIT ONE")					; 0xF1
	   (?,Tr(B special "DIGIT TWO")					; 0xF2
	   (?,Ts(B special "DIGIT THREE")					; 0xF3
	   (?,Tt(B special "DIGIT FOUR")					; 0xF4
	   (?,Tu(B special "DIGIT FIVE")					; 0xF5
	   (?,Tv(B special "DIGIT SIX")					; 0xF6
	   (?,Tw(B special "DIGIT SEVEN")					; 0xF7
	   (?,Tx(B special "DIGIT EIGHT")					; 0xF8
	   (?,Ty(B special "DIGIT NINE")					; 0xF9
	   (?,Tz(B special "ANGKHANKHU (ellipsis)")				; 0xFA
	   (?,T{(B special "KHOMUT (beginning of religious texts)")		; 0xFB
	   (?,T|(B invalid nil)						; 0xFC
	   (?,T}(B invalid nil)						; 0xFD
	   (?,T~(B invalid nil)						; 0xFE

	   ;; Unicode equivalents
	   (?$,1Ba(B consonant "LETTER KO KAI")
	   (?$,1Bb(B consonant "LETTER KHO KHAI")
	   (?$,1Bc(B consonant "LETTER KHO KHUAT")
	   (?$,1Bd(B consonant "LETTER KHO KHWAI")
	   (?$,1Be(B consonant "LETTER KHO KHON")
	   (?$,1Bf(B consonant "LETTER KHO RAKHANG")
	   (?$,1Bg(B consonant "LETTER NGO NGU")
	   (?$,1Bh(B consonant "LETTER CHO CHAN")
	   (?$,1Bi(B consonant "LETTER CHO CHING")
	   (?$,1Bj(B consonant "LETTER CHO CHANG")
	   (?$,1Bk(B consonant "LETTER SO SO")
	   (?$,1Bl(B consonant "LETTER CHO CHOE")
	   (?$,1Bm(B consonant "LETTER YO YING")
	   (?$,1Bn(B consonant "LETTER DO CHADA")
	   (?$,1Bo(B consonant "LETTER TO PATAK")
	   (?$,1Bp(B consonant "LETTER THO THAN")
	   (?$,1Bq(B consonant "LETTER THO NANGMONTHO")
	   (?$,1Br(B consonant "LETTER THO PHUTHAO")
	   (?$,1Bs(B consonant "LETTER NO NEN")
	   (?$,1Bt(B consonant "LETTER DO DEK")
	   (?$,1Bu(B consonant "LETTER TO TAO")
	   (?$,1Bv(B consonant "LETTER THO THUNG")
	   (?$,1Bw(B consonant "LETTER THO THAHAN")
	   (?$,1Bx(B consonant "LETTER THO THONG")
	   (?$,1By(B consonant "LETTER NO NU")
	   (?$,1Bz(B consonant "LETTER BO BAIMAI")
	   (?$,1B{(B consonant "LETTER PO PLA")
	   (?$,1B|(B consonant "LETTER PHO PHUNG")
	   (?$,1B}(B consonant "LETTER FO FA")
	   (?$,1B~(B consonant "LETTER PHO PHAN")
	   (?$,1B(B consonant "LETTER FO FAN")
	   (?$,1C (B consonant "LETTER PHO SAMPHAO")
	   (?$,1C!(B consonant "LETTER MO MA")
	   (?$,1C"(B consonant "LETTER YO YAK")
	   (?$,1C#(B consonant "LETTER RO RUA")
	   (?$,1C$(B vowel-base "LETTER RU (Pali vowel letter)")
	   (?$,1C%(B consonant "LETTER LO LING")
	   (?$,1C&(B vowel-base "LETTER LU (Pali vowel letter)")
	   (?$,1C'(B consonant "LETTER WO WAEN")
	   (?$,1C((B consonant "LETTER SO SALA")
	   (?$,1C)(B consonant "LETTER SO RUSI")
	   (?$,1C*(B consonant "LETTER SO SUA")
	   (?$,1C+(B consonant "LETTER HO HIP")
	   (?$,1C,(B consonant "LETTER LO CHULA")
	   (?$,1C-(B consonant "LETTER O ANG")
	   (?$,1C.(B consonant "LETTER HO NOK HUK")
	   (?$,1C/(B special "PAI YAN NOI (abbreviation)")
	   (?$,1C0(B vowel-base "VOWEL SIGN SARA A")
	   (?$,1C1(B vowel-upper "VOWEL SIGN MAI HAN-AKAT N/S-T")
	   (?$,1C2(B vowel-base "VOWEL SIGN SARA AA")
	   (?$,1C3(B vowel-base "VOWEL SIGN SARA AM")
	   (?$,1C4(B vowel-upper "VOWEL SIGN SARA I N/S-T")
	   (?$,1C5(B vowel-upper "VOWEL SIGN SARA II N/S-T")
	   (?$,1C6(B vowel-upper "VOWEL SIGN SARA UE N/S-T")
	   (?$,1C7(B vowel-upper "VOWEL SIGN SARA UEE N/S-T")
	   (?$,1C8(B vowel-lower "VOWEL SIGN SARA U N/S-B")
	   (?$,1C9(B vowel-lower "VOWEL SIGN SARA UU N/S-B")
	   (?$,1C:(B vowel-lower "VOWEL SIGN PHINTHU N/S-B (Pali virama)")
	   (?$,1C?(B special "BAHT SIGN (currency symbol)")
	   (?$,1C@(B vowel-base "VOWEL SIGN SARA E")
	   (?$,1CA(B vowel-base "VOWEL SIGN SARA AE")
	   (?$,1CB(B vowel-base "VOWEL SIGN SARA O")
	   (?$,1CC(B vowel-base "VOWEL SIGN SARA MAI MUAN")
	   (?$,1CD(B vowel-base "VOWEL SIGN SARA MAI MALAI")
	   (?$,1CE(B vowel-base "LAK KHANG YAO")
	   (?$,1CF(B special "MAI YAMOK (repetion)")
	   (?$,1CG(B sign-upper "VOWEL SIGN MAI TAI KHU N/S-T")
	   (?$,1CH(B tone "TONE MAI EK N/S-T")
	   (?$,1CI(B tone "TONE MAI THO N/S-T")
	   (?$,1CJ(B tone "TONE MAI TRI N/S-T")
	   (?$,1CK(B tone "TONE MAI CHATTAWA N/S-T")
	   (?$,1CL(B sign-upper "THANTHAKHAT N/S-T (cancellation mark)")
	   (?$,1CM(B sign-upper "NIKKHAHIT N/S-T (final nasal)")
	   (?$,1CN(B sign-upper "YAMAKKAN N/S-T")
	   (?$,1CO(B special "FONRMAN")
	   (?$,1CP(B special "DIGIT ZERO")
	   (?$,1CQ(B special "DIGIT ONE")
	   (?$,1CR(B special "DIGIT TWO")
	   (?$,1CS(B special "DIGIT THREE")
	   (?$,1CT(B special "DIGIT FOUR")
	   (?$,1CU(B special "DIGIT FIVE")
	   (?$,1CV(B special "DIGIT SIX")
	   (?$,1CW(B special "DIGIT SEVEN")
	   (?$,1CX(B special "DIGIT EIGHT")
	   (?$,1CY(B special "DIGIT NINE")
	   (?$,1CZ(B special "ANGKHANKHU (ellipsis)")
	   (?$,1C[(B special "KHOMUT (beginning of religious texts)")
	   ))
      elm)
  (while l
    (setq elm (car l) l (cdr l))
    (let ((char (car elm))
	  (ptype (nth 1 elm)))
      (put-char-code-property char 'phonetic-type ptype)
      (cond ((eq ptype 'consonant)
	     (modify-category-entry char ?c thai-category-table)
	     (global-set-key (vector char) 'thai-self-insert-command))
	    ((memq ptype '(vowel-upper vowel-lower))
	     (modify-category-entry char ?v thai-category-table)
	     (if (or (= char ?,TT(B) (= char ?$,1C4(B))
		 ;; Give category `I' to "SARA I".
		 (modify-category-entry char ?I thai-category-table))
	     (global-set-key (vector char) 'thai-self-insert-command))
	    ((eq ptype 'tone)
	     (modify-category-entry char ?t thai-category-table)
	     (modify-category-entry char ?u thai-category-table)
	     (global-set-key (vector char) 'thai-self-insert-command))
	    ((eq ptype 'sign-upper)
	     (modify-category-entry char ?u thai-category-table)
	     (if (or (= char ?,Tl(B) (= char ?$,1CL(B))
		 ;; Give category `U' to "THANTHAKHAT".
		 (modify-category-entry char ?U thai-category-table))
	     (global-set-key (vector char) 'thai-self-insert-command)))
      (put-char-code-property char 'name (nth 2 elm)))))

(defun thai-compose-syllable (beg end &optional category-set string)
  (or category-set
      (setq category-set
	    (char-category-set (if string (aref string beg) (char-after beg)))))
  (if (aref category-set ?c)
      ;; Starting with a consonant.  We do relative composition.
      (if string
	  (compose-string string beg end)
	(compose-region beg end))
    ;; Vowel tone sequence.
    (if string
	(compose-string string beg end (list (aref string beg) '(Bc . Bc)
					     (aref string (1+ beg))))
      (compose-region beg end (list (char-after beg) '(Bc . Bc)
				    (char-after (1+ beg))))))
  (- end beg))

;;;###autoload
(defun thai-compose-region (beg end)
  "Compose Thai characters in the region.
When called from a program, expects two arguments,
positions (integers or markers) specifying the region."
  (interactive "r")
  (let ((pos (point)))
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (with-category-table thai-category-table
	(while (re-search-forward thai-composition-pattern nil t)
	  (setq beg (match-beginning 0) end (match-end 0))
	  (if (and (> pos beg) (< pos end))
	      (setq pos end))
	  (thai-compose-syllable beg end
				 (char-category-set (char-after beg))))))
    (goto-char pos)))

;;;###autoload
(defun thai-compose-string (string)
  "Compose Thai characters in STRING and return the resulting string."
  (with-category-table thai-category-table
    (let ((idx 0))
      (while (setq idx (string-match thai-composition-pattern string idx))
	(thai-compose-syllable idx (match-end 0) nil string)
	(setq idx (match-end 0)))))
  string)

;;;###autoload
(defun thai-compose-buffer ()
  "Compose Thai characters in the current buffer."
  (interactive)
  (thai-compose-region (point-min) (point-max)))

;;;###autoload
(defun thai-post-read-conversion (len)
  (thai-compose-region (point) (+ (point) len))
  len)

;;;###autoload
(defun thai-composition-function (from to pattern &optional string)
  "Compose Thai text in the region FROM and TO.
The text matches the regular expression PATTERN.
Optional 4th argument STRING, if non-nil, is a string containing text
to compose.

The return value is number of composed characters."
  (when (and (not thai-auto-composition-mode)
	     (< (1+ from) to))
    (with-category-table thai-category-table
      (if string
	  (if (eq (string-match thai-composition-pattern string from) from)
	      (thai-compose-syllable from (match-end 0) nil string))
	(if (save-excursion
	      (goto-char from)
	      (and (looking-at thai-composition-pattern)
		   (setq to (match-end 0))))
	    (thai-compose-syllable from to))))))

(defun thai-auto-composition (beg end len)
  (with-category-table thai-category-table
    (let (category-set)
      (while (and (> beg (point-min))
		  (setq category-set (char-category-set (char-after (1- beg))))
		  (or (aref category-set ?v) (aref category-set ?u)))
	  (setq beg (1- beg)))
      (if (and (> beg (point-min))
	       (aref (char-category-set (char-after (1- beg))) ?c))
	  (setq beg (1- beg)))
      (while (and (< end (point-max))
		  (setq category-set (char-category-set (char-after end)))
		  (or (aref category-set ?v) (aref category-set ?u)))
	(setq end (1+ end)))
      (if (< beg end)
	  (thai-compose-region beg end)))))

(put 'thai-auto-composition-mode 'permanent-local t)

;;;###autoload
(define-minor-mode thai-auto-composition-mode
  "Minor mode for automatically correct Thai character composition."
  :group 'mule
  (cond ((null thai-auto-composition-mode)
	 (remove-hook 'after-change-functions 'thai-auto-composition))
	(t
	 (add-hook 'after-change-functions 'thai-auto-composition))))

;; Thai-word-mode requires functions in the feature `thai-word'.
(require 'thai-word)

(defvar thai-word-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap forward-word] 'thai-forward-word)
    (define-key map [remap backward-word] 'thai-backward-word)
    (define-key map [remap kill-word] 'thai-kill-word)
    (define-key map [remap backward-kill-word] 'thai-backward-kill-word)
    (define-key map [remap transpose-words] 'thai-transpose-words)
    map)
  "Keymap for `thai-word-mode'.")

(define-minor-mode thai-word-mode
  "Minor mode to make word-oriented commands aware of Thai words.
The commands affected are \\[forward-word], \\[backward-word], \\[kill-word], \\[backward-kill-word], \\[transpose-words], and \\[fill-paragraph]."
  :global t :group 'mule
  (cond (thai-word-mode
	 ;; This enables linebreak between Thai characters.
	 (modify-category-entry (make-char 'thai-tis620) ?|)
	 ;; This enables linebreak at a Thai word boundary.
	 (put-charset-property 'thai-tis620 'fill-find-break-point-function
			       'thai-fill-find-break-point))
	(t
	 (modify-category-entry (make-char 'thai-tis620) ?| nil t)
	 (put-charset-property 'thai-tis620 'fill-find-break-point-function
			       nil))))

;; Function to call on entering the Thai language environment.
(defun setup-thai-language-environment-internal ()
  (thai-word-mode 1))

;; Function to call on exiting the Thai language environment.
(defun exit-thai-language-environment-internal ()
  (thai-word-mode -1))

;;
(provide 'thai-util)

;;; arch-tag: 59425d6a-8cf9-4e06-a6ab-8ab7dc7a7a97
;;; thai-util.el ends here
