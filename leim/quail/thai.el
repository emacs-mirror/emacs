;;; thai.el --- Quail package for inputting Thai characters -*-coding: iso-2022-7bit;-*-

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

;;; Commentary:

;;; Code:

(require 'quail)
(require 'thai-util)

(defun quail-thai-update-translation (control-flag)
  (if (integerp control-flag)
      ;; Non-composable character typed.
      (setq quail-current-str
	    (buffer-substring (overlay-start quail-overlay)
			      (overlay-end quail-overlay))
	    unread-command-events
	    (string-to-list
	     (substring quail-current-key control-flag)))
    (setq quail-current-str
	  (compose-string (quail-lookup-map-and-concat quail-current-key))))
  control-flag)

(defun thai-generate-quail-map (translation-table)
  (let ((i 0)
	consonant vowel tone voweltone others)
    ;; Categorize Thai characters into one of above.
    (while (< i 128)
      (let ((trans (aref translation-table i))
	    ptype)
	(if (eq trans 0)
	    nil
	  (if (> (length trans) 1)
	      (setq ptype 'voweltone
		    trans (vector (compose-string trans)))
	    (setq ptype (get-char-code-property (aref trans 0) 'phonetic-type))
	    (cond ((memq ptype '(vowel-upper vowel-lower))
		   (setq ptype 'vowel))
		  ((not (memq ptype '(consonant tone)))
		   (setq ptype 'others))))
	  (set ptype (cons (cons (char-to-string i) trans)
			   (symbol-value ptype)))))
      (setq i (1+ i)))

    (quail-map-from-table
     '((base-state (consonant . vt-state)
		   vowel tone voweltone others)
       (vt-state (vowel . t-state)
		 voweltone tone)
       (t-state tone)))))

;; Thai Kesmanee keyboard support.

(quail-define-package
 "thai-kesmanee" "Thai" ",T!!(B>" t
 "Thai Kesmanee input method with TIS620 keyboard layout

The difference from the ordinal Thai keyboard:
    ',T_(B' and ',To(B' are assigned to '\\' and '|' respectively,
    ',T#(B' and ',T%(B' are assigned to '`' and '~' respectively,
    Don't know where to assign characters ',Tz(B' and ',T{(B'."
 nil t t t t nil nil nil 'quail-thai-update-translation nil t)

(quail-install-map 
 (thai-generate-quail-map
  [
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0	; control codes
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0	; control codes
   0   "#" ",TF(B" ",Tr(B" ",Ts(B" ",Tt(B" ",TQi(B" ",T'(B"	; SPC .. '
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
   ",T;(B" ",TQ(B" ",T<(B" ",T0(B" ",To(B" "." ",T%(B" 0	; x .. DEL
   ]))



;; Thai Pattachote keyboard support.

(quail-define-package
 "thai-pattachote" "Thai" ",T!;(B>" t
 "Thai Pattachote input method with TIS620 keyboard layout"
 nil t t t t nil nil nil 'quail-thai-update-translation nil t)

(quail-install-map 
 (thai-generate-quail-map
  [ 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0	; control codes
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
    ",T;(B" ",Th(B" ",T:(B" ",TO(B" ",Tm(B" ",TF(B" ",T%(B" 0	; x .. DEL
    ]))

;;; thai.el ends here
