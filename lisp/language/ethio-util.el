;;; ethio-util.el --- utilities for Ethiopic -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1997, 1998, 2002
;;   Free Software Foundation, Inc.
;; Copyright (C) 1997, 1998, 1999, 2001, 2004, 2005
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021

;; Keywords: mule, multilingual, Ethiopic

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

;; Author: TAKAHASHI Naoto <ntakahas@m17n.org>

;;; Commentary:

;;; Code:

(defvar rmail-current-message)
(defvar rmail-message-vector)

;; Information for exiting Ethiopic environment.
(defvar exit-ethiopic-environment-data nil)

;;;###autoload
(defun setup-ethiopic-environment-internal ()
  (let ((key-bindings '((" " . ethio-insert-space)
			([?\S- ] . ethio-insert-ethio-space)
			([?\C-'] . ethio-gemination)

			;; these old bindings conflict
			;; with Emacs' binding policy

			;; ([f2] . ethio-toggle-space)
			;; ([S-f2] . ethio-replace-space) ; as requested
			;; ([f3] . ethio-toggle-punctuation)
			;; ([f4] . ethio-sera-to-fidel-buffer)
			;; ([S-f4] . ethio-sera-to-fidel-region)
			;; ([C-f4] . ethio-sera-to-fidel-mail-or-marker)
			;; ([f5] . ethio-fidel-to-sera-buffer)
			;; ([S-f5] . ethio-fidel-to-sera-region)
			;; ([C-f5] . ethio-fidel-to-sera-mail-or-marker)
			;; ([f6] . ethio-modify-vowel)
			;; ([f7] . ethio-replace-space)
			;; ([f8] . ethio-input-special-character)

			;; this is the rewritten bindings

			([f3] . ethio-fidel-to-sera-buffer)
			([S-f3] . ethio-fidel-to-sera-region)
			([C-f3] . ethio-fidel-to-sera-mail-or-marker)
			([f4] . ethio-sera-to-fidel-buffer)
			([S-f4] . ethio-sera-to-fidel-region)
			([C-f4] . ethio-sera-to-fidel-mail-or-marker)
			([S-f5] . ethio-toggle-punctuation)
			([S-f6] . ethio-modify-vowel)
			([S-f7] . ethio-replace-space)
			([S-f8] . ethio-input-special-character)
			([C-f9] . ethio-toggle-space)
			([S-f9] . ethio-replace-space) ; as requested
			))
	kb)
    (while key-bindings
      (setq kb (car (car key-bindings)))
      (setq exit-ethiopic-environment-data
	    (cons (cons kb (global-key-binding kb))
		  exit-ethiopic-environment-data))
      (global-set-key kb (cdr (car key-bindings)))
      (setq key-bindings (cdr key-bindings))))

  (add-hook 'quail-activate-hook 'ethio-select-a-translation)
  (add-hook 'find-file-hook 'ethio-find-file)
  (add-hook 'write-file-functions 'ethio-write-file)
  (add-hook 'after-save-hook 'ethio-find-file))

(defun exit-ethiopic-environment ()
  "Exit Ethiopic language environment."
  (while exit-ethiopic-environment-data
    (global-set-key (car (car exit-ethiopic-environment-data))
		    (cdr (car exit-ethiopic-environment-data)))
    (setq exit-ethiopic-environment-data
	  (cdr exit-ethiopic-environment-data)))

  (remove-hook 'quail-activate-hook 'ethio-select-a-translation)
  (remove-hook 'find-file-hook 'ethio-find-file)
  (remove-hook 'write-file-functions 'ethio-write-file)
  (remove-hook 'after-save-hook 'ethio-find-file))

;;
;; ETHIOPIC UTILITY FUNCTIONS
;;

;; If the filename ends in ".sera", editing is done in fidel
;; but file I/O is done in SERA.
;;
;; If the filename ends in ".java", editing is done in fidel
;; but file I/O is done in the \uXXXX style, where XXXX is
;; the Unicode codepoint for the Ethiopic character.
;;
;; If the filename ends in ".tex", editing is done in fidel
;; but file I/O is done in EthioTeX format.
;;
;; To automatically convert Ethiopic text to SERA format when sending mail,
;;   (add-hook 'mail-send-hook 'ethio-fidel-to-sera-mail)
;;
;; To automatically convert SERA format to Ethiopic when receiving mail,
;;   (add-hook 'rmail-show-message-hook 'ethio-sera-to-fidel-mail)
;;
;; To automatically convert Ethiopic text to SERA format when posting news,
;;   (add-hook 'news-inews-hook 'ethio-fidel-to-sera-mail)

;;
;; users' preference
;;

(defvar ethio-primary-language 'tigrigna
  "*Symbol that defines the primary language in SERA --> FIDEL conversion.
The value should be one of: `tigrigna', `amharic' or `english'.")

(defvar ethio-secondary-language 'english
  "*Symbol that defines the secondary language in SERA --> FIDEL conversion.
The value should be one of: `tigrigna', `amharic' or `english'.")

(defvar ethio-use-colon-for-colon nil
  "*Non-nil means associate ASCII colon with Ethiopic colon.
If nil, associate ASCII colon with Ethiopic word separator, i.e., two
vertically stacked dots.  All SERA <--> FIDEL converters refer this
variable.")

(defvar ethio-use-three-dot-question nil
  "*Non-nil means associate ASCII question mark with Ethiopic old style question mark (three vertically stacked dots).
If nil, associate ASCII question mark with Ethiopic stylized question
mark.  All SERA <--> FIDEL converters refer this variable.")

(defvar ethio-quote-vowel-always nil
  "*Non-nil means always put an apostrophe before an isolated vowel (except at word initial) in FIDEL --> SERA conversion.
If nil, put an apostrophe only between a sixth-form consonant and an
isolated vowel.")

(defvar ethio-W-sixth-always nil
  "*Non-nil means convert the Wu-form of a 12-form consonant to \"W'\" instead of \"Wu\" in FIDEL --> SERA conversion.")

(defvar ethio-numeric-reduction 0
  "*Degree of reduction in converting Ethiopic digits into Arabic digits.
Should be 0, 1 or 2.
For example, ({10}{9}{100}{80}{7}) is converted into:
    `10`9`100`80`7  if `ethio-numeric-reduction' is 0,
    `109100807	    if `ethio-numeric-reduction' is 1,
    `10900807	    if `ethio-numeric-reduction' is 2.")

(defvar ethio-implicit-period-conversion t
  "*Non-nil means replacing the Ethiopic dot at the end of an Ethiopic sentence
with an Ethiopic full stop.")

(defvar ethio-java-save-lowercase nil
  "*Non-nil means save Ethiopic characters in lowercase hex numbers to Java files.
If nil, use uppercases.")

;;
;; SERA to FIDEL
;;

(defconst ethio-sera-to-fidel-table
  [
   nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
;;; SP
   (" "
    (?: (if ethio-use-colon-for-colon " $(3$l(B" "$(3$h(B")
	(32 (if ethio-use-colon-for-colon " $(3$l(B " "$(3$h(B"))
	(?- " $(3$m(B")
	(?: " $(3$i(B")
	(?| (if ethio-use-colon-for-colon " $(3$l(B|" " $(3$h(B|")
	    (?: " $(3$o(B"))))

;;; !   "   #   $   %   &    '
   nil nil nil nil nil nil ("" (?' "$(3%s(B"))
;;; (   )   *   +    ,      -               .
   nil nil nil nil ("$(3$j(B") ("-" (?: "$(3$l(B")) ("$(3%u(B")
;;;  /   0   1   2   3   4   5   6   7   8   9
    nil nil nil nil nil nil nil nil nil nil nil
;;; :
   ((if ethio-use-colon-for-colon "$(3$l(B" "$(3$h(B")
    (32 (if ethio-use-colon-for-colon "$(3$l(B " "$(3$h(B"))
    (?- "$(3$m(B")
    (?: "$(3$i(B")
    (?| (if ethio-use-colon-for-colon "$(3$l(B|" "$(3$h(B|")
	(?: "$(3$o(B")))
;;;  ;      <              =    >
   ("$(3$k(B") ("<" (?< "$(3%v(B")) nil (">" (?> "$(3%w(B"))
;;; ?
   ((if ethio-use-three-dot-question "$(3$n(B" "$(3%x(B"))
;;; @
    nil
;;; A
   ("$(3"f(B" (?2 "$(3#8(B"))
;;; B
   ("$(3"((B" (?e "$(3"#(B") (?u "$(3"$(B") (?i "$(3"%(B") (?a "$(3"&(B") (?E "$(3"'(B") (?o "$(3")(B")
         (?W "$(3%b(B" (?e "$(3%2(B") (?u "$(3%b(B") (?i "$(3%B(B") (?a "$(3"*(B") (?E "$(3%R(B")))
;;; C
   ("$(3$4(B" (?e "$(3$/(B") (?u "$(3$0(B") (?i "$(3$1(B") (?a "$(3$2(B") (?E "$(3$3(B") (?o "$(3$5(B")
         (?W "$(3$6(B" (?a "$(3$6(B")
                  (?e "$(3$4%n(B") (?u "$(3$4%r(B") (?i "$(3$4%o(B") (?E "$(3$4%q(B")))
;;; D
   ("$(3#b(B" (?e "$(3#](B") (?u "$(3#^(B") (?i "$(3#_(B") (?a "$(3#`(B") (?E "$(3#a(B") (?o "$(3#c(B")
         (?W "$(3#d(B" (?a "$(3#d(B")
                  (?e "$(3#b%n(B") (?u "$(3#b%r(B") (?i "$(3#b%o(B") (?E "$(3#b%q(B")))
;;; E
   ("$(3"g(B" (?2 "$(3#9(B"))
;;; F
   ("$(3$T(B" (?e "$(3$O(B") (?u "$(3$P(B") (?i "$(3$Q(B") (?a "$(3$R(B") (?E "$(3$S(B") (?o "$(3$U(B")
         (?W "$(3%d(B" (?e "$(3%4(B") (?u "$(3%d(B") (?i "$(3%D(B") (?a "$(3$V(B") (?E "$(3%T(B"))
	 (?Y "$(3$a(B" (?a "$(3$a(B")))
;;; G
   ("$(3$$(B" (?e "$(3#}(B") (?u "$(3#~(B") (?i "$(3$!(B") (?a "$(3$"(B") (?E "$(3$#(B") (?o "$(3$%(B")
         (?W "$(3%c(B" (?e "$(3%3(B") (?u "$(3%c(B") (?i "$(3%C(B") (?a "$(3$&(B") (?E "$(3%S(B")))
;;; H
   ("$(3!6(B" (?e "$(3!1(B") (?u "$(3!2(B") (?i "$(3!3(B") (?a "$(3!4(B") (?E "$(3!5(B") (?o "$(3!7(B")
         (?W "$(3!8(B" (?a "$(3!8(B")
                  (?e "$(3!6%n(B") (?u "$(3!6%r(B") (?i "$(3!6%o(B") (?E "$(3!6%q(B")))
;;; I
   ("$(3"h(B" (?2 "$(3#:(B"))
;;; J
   ("$(3#j(B" (?e "$(3#e(B") (?u "$(3#f(B") (?i "$(3#g(B") (?a "$(3#h(B") (?E "$(3#i(B") (?o "$(3#k(B")
         (?W "$(3#l(B" (?a "$(3#l(B")
	          (?e "$(3#j%n(B") (?u "$(3#j%r(B") (?i "$(3#j%o(B") (?E "$(3#j%q(B")))
;;; K
   ("$(3#"(B" (?e "$(3"{(B") (?u "$(3"|(B") (?i "$(3"}(B") (?a "$(3"~(B") (?E "$(3#!(B") (?o "$(3##(B")
         (?W "$(3#*(B" (?e "$(3#%(B") (?u "$(3#*(B") (?i "$(3#'(B") (?a "$(3#((B") (?E "$(3#)(B")))
;;; L
   ("$(3!.(B" (?e "$(3!)(B") (?u "$(3!*(B") (?i "$(3!+(B") (?a "$(3!,(B") (?E "$(3!-(B") (?o "$(3!/(B")
         (?W "$(3!0(B" (?a "$(3!0(B")
                  (?e "$(3!.%n(B") (?u "$(3!.%r(B") (?i "$(3!.%o(B") (?E "$(3!.%q(B")))
;;; M
   ("$(3!>(B" (?e "$(3!9(B") (?u "$(3!:(B") (?i "$(3!;(B") (?a "$(3!<(B") (?E "$(3!=(B") (?o "$(3!?(B")
         (?W "$(3%a(B" (?e "$(3%1(B") (?u "$(3%a(B") (?i "$(3%A(B") (?a "$(3!@(B") (?E "$(3%Q(B"))
	 (?Y "$(3$_(B" (?a "$(3$_(B")))
;;; N
   ("$(3"`(B" (?e "$(3"[(B") (?u "$(3"\(B") (?i "$(3"](B") (?a "$(3"^(B") (?E "$(3"_(B") (?o "$(3"a(B")
         (?W "$(3"b(B" (?a "$(3"b(B")
                  (?e "$(3"`%n(B") (?u "$(3"`%r(B") (?i "$(3"`%o(B") (?E "$(3"`%q(B")))
;;; O
   ("$(3"i(B" (?2 "$(3#;(B"))
;;; P
   ("$(3$<(B" (?e "$(3$7(B") (?u "$(3$8(B") (?i "$(3$9(B") (?a "$(3$:(B") (?E "$(3$;(B") (?o "$(3$=(B")
         (?W "$(3$>(B" (?a "$(3$>(B")
                  (?e "$(3$<%n(B") (?u "$(3$<%r(B") (?i "$(3$<%o(B") (?E "$(3$<%q(B")))
;;; Q
   ("$(3!v(B" (?e "$(3!q(B") (?u "$(3!r(B") (?i "$(3!s(B") (?a "$(3!t(B") (?E "$(3!u(B") (?o "$(3!w(B")
         (?W "$(3!~(B" (?e "$(3!y(B") (?u "$(3!~(B") (?i "$(3!{(B") (?a "$(3!|(B") (?E "$(3!}(B")))
;;; R
   ("$(3!N(B" (?e "$(3!I(B") (?u "$(3!J(B") (?i "$(3!K(B") (?a "$(3!L(B") (?E "$(3!M(B") (?o "$(3!O(B")
         (?W "$(3!P(B" (?a "$(3!P(B")
	          (?e "$(3!N%n(B") (?u "$(3!N%r(B") (?i "$(3!N%o(B") (?E "$(3!N%q(B"))
         (?Y "$(3$`(B" (?a "$(3$`(B")))
;;; S
   ("$(3$D(B" (?e "$(3$?(B") (?u "$(3$@(B") (?i "$(3$A(B") (?a "$(3$B(B") (?E "$(3$C(B") (?o "$(3$E(B")
         (?W "$(3$F(B" (?a "$(3$F(B")
                  (?e "$(3$D%n(B") (?u "$(3$D%r(B") (?i "$(3$D%o(B") (?E "$(3$D%q(B"))
	 (?2 "$(3$L(B"
	     (?e "$(3$G(B") (?u "$(3$H(B") (?i "$(3$I(B") (?a "$(3$J(B") (?E "$(3$K(B") (?o "$(3$M(B")
	     (?W "$(3$F(B" (?a "$(3$F(B")
		 (?e "$(3$L%n(B") (?u "$(3$L%r(B") (?i "$(3$L%o(B") (?E "$(3$L%q(B"))))
;;; T
   ("$(3$,(B" (?e "$(3$'(B") (?u "$(3$((B") (?i "$(3$)(B") (?a "$(3$*(B") (?E "$(3$+(B") (?o "$(3$-(B")
         (?W "$(3$.(B" (?a "$(3$.(B")
	          (?e "$(3$,%n(B") (?u "$(3$,%r(B") (?i "$(3$,%o(B") (?E "$(3$,%q(B")))
;;; U
   ("$(3"d(B" (?2 "$(3#6(B"))
;;; V
   ("$(3"0(B" (?e "$(3"+(B") (?u "$(3",(B") (?i "$(3"-(B") (?a "$(3".(B") (?E "$(3"/(B") (?o "$(3"1(B")
         (?W "$(3"2(B" (?a "$(3"2(B")
	          (?e "$(3"0%n(B") (?u "$(3"0%r(B") (?i "$(3"0%o(B") (?E "$(3"0%q(B")))
;;; W
   ("$(3%r(B" (?e "$(3%n(B") (?u "$(3%r(B") (?i "$(3%o(B") (?a "$(3%p(B") (?E "$(3%q(B"))
;;; X
   ("$(3%N(B" (?e "$(3%I(B") (?u "$(3%J(B") (?i "$(3%K(B") (?a "$(3%L(B") (?E "$(3%M(B") (?o "$(3%O(B"))
;;; Y
   ("$(3#R(B" (?e "$(3#M(B") (?u "$(3#N(B") (?i "$(3#O(B") (?a "$(3#P(B") (?E "$(3#Q(B") (?o "$(3#S(B")
         (?W "$(3#T(B" (?a "$(3#T(B")
	          (?e "$(3#R%n(B") (?u "$(3#R%r(B") (?i "$(3#R%o(B") (?E "$(3#R%q(B")))
;;; Z
   ("$(3#J(B" (?e "$(3#E(B") (?u "$(3#F(B") (?i "$(3#G(B") (?a "$(3#H(B") (?E "$(3#I(B") (?o "$(3#K(B")
         (?W "$(3#L(B" (?a "$(3#L(B")
	          (?e "$(3#J%n(B") (?u "$(3#J%r(B") (?i "$(3#J%o(B") (?E "$(3#J%q(B")))
;;; [   \   ]   ^   _
   nil nil nil nil nil
;;; `
   (""
    (?: "$(3$h(B")
    (?? (if ethio-use-three-dot-question "$(3%x(B" "$(3$n(B"))
    (?! "$(3%t(B")
    (?e "$(3#5(B") (?u "$(3#6(B") (?U "$(3#6(B") (?i "$(3#7(B") (?a "$(3#8(B") (?A "$(3#8(B")
        (?E "$(3#9(B") (?I "$(3#:(B") (?o "$(3#;(B") (?O "$(3#;(B")
    (?g "$(3%^(B"
        (?e "$(3%Y(B") (?u "$(3%Z(B") (?i "$(3%[(B") (?a "$(3%\(B") (?E "$(3%](B") (?o "$(3%_(B"))
    (?h "$(3"H(B"
        (?e "$(3"C(B") (?u "$(3"D(B") (?i "$(3"E(B") (?a "$(3"F(B") (?E "$(3"G(B") (?o "$(3"I(B")
	          (?W "$(3"P(B" (?e "$(3"K(B") (?u "$(3"P(B") (?i "$(3"M(B") (?a "$(3"N(B") (?E "$(3"O(B")))
    (?k "$(3%>(B"
        (?e "$(3%9(B") (?u "$(3%:(B") (?i "$(3%;(B") (?a "$(3%<(B") (?E "$(3%=(B") (?o "$(3%?(B"))
    (?s "$(3!F(B"
        (?e "$(3!A(B") (?u "$(3!B(B") (?i "$(3!C(B") (?a "$(3!D(B") (?E "$(3!E(B") (?o "$(3!G(B")
	(?W "$(3!H(B" (?a "$(3!H(B")
		   (?e "$(3!F%n(B") (?u "$(3!F%r(B") (?i "$(3!F%o(B") (?E "$(3!F%q(B")))
    (?S "$(3$L(B"
	(?e "$(3$G(B") (?u "$(3$H(B") (?i "$(3$I(B") (?a "$(3$J(B") (?E "$(3$K(B") (?o "$(3$M(B")
	(?W "$(3$F(B" (?a "$(3$F(B")
	         (?e "$(3$L%n(B") (?u "$(3$L%r(B") (?i "$(3$L%o(B") (?E "$(3$L%q(B")))
    (?q "$(3%.(B" (?e "$(3%)(B") (?u "$(3%*(B") (?i "$(3%+(B") (?a "$(3%,(B") (?E "$(3%-(B") (?o "$(3%/(B")))
;;; a
   ("$(3"f(B" (?2 "$(3#8(B"))
;;; b
   ("$(3"((B" (?e "$(3"#(B") (?u "$(3"$(B") (?i "$(3"%(B") (?a "$(3"&(B") (?E "$(3"'(B") (?o "$(3")(B")
         (?W "$(3%b(B" (?e "$(3%2(B") (?u "$(3%b(B") (?i "$(3%B(B") (?a "$(3"*(B") (?E "$(3%R(B")))
;;; c
   ("$(3"@(B" (?e "$(3";(B") (?u "$(3"<(B") (?i "$(3"=(B") (?a "$(3">(B") (?E "$(3"?(B") (?o "$(3"A(B")
         (?W "$(3"B(B" (?a "$(3"B(B")
	          (?e "$(3"@%n(B") (?u "$(3"@%r(B") (?i "$(3"@%o(B") (?E "$(3"@%q(B")))
;;; d
   ("$(3#Z(B" (?e "$(3#U(B") (?u "$(3#V(B") (?i "$(3#W(B") (?a "$(3#X(B") (?E "$(3#Y(B") (?o "$(3#[(B")
         (?W "$(3#\(B" (?a "$(3#\(B")
	          (?e "$(3#Z%o(B") (?u "$(3#Z%r(B") (?i "$(3#Z%p(B") (?E "$(3#Z%q(B")))
;;; e
   ("$(3"c(B" (?2 "$(3#5(B") (?a "$(3"j(B"))
;;; f
   ("$(3$T(B" (?e "$(3$O(B") (?u "$(3$P(B") (?i "$(3$Q(B") (?a "$(3$R(B") (?E "$(3$S(B") (?o "$(3$U(B")
         (?W "$(3%d(B" (?e "$(3%4(B") (?u "$(3%d(B") (?i "$(3%D(B") (?a "$(3$V(B") (?E "$(3%T(B"))
	 (?Y "$(3$a(B" (?a "$(3$a(B")))
;;; g
   ("$(3#r(B" (?e "$(3#m(B") (?u "$(3#n(B") (?i "$(3#o(B") (?a "$(3#p(B") (?E "$(3#q(B") (?o "$(3#s(B")
         (?W "$(3#z(B" (?e "$(3#u(B") (?u "$(3#z(B") (?i "$(3#w(B") (?a "$(3#x(B") (?E "$(3#y(B"))
         (?2 "$(3%^(B" (?e "$(3%Y(B") (?u "$(3%Z(B") (?i "$(3%[(B") (?a "$(3%\(B") (?E "$(3%](B") (?o "$(3%_(B")))
;;; h
   ("$(3!&(B" (?e "$(3!!(B") (?u "$(3!"(B") (?i "$(3!#(B") (?a "$(3!$(B") (?E "$(3!%(B") (?o "$(3!'(B")
         (?W "$(3"P(B" (?e "$(3"K(B") (?u "$(3"P(B") (?i "$(3"M(B") (?a "$(3"N(B") (?E "$(3"O(B"))
	 (?2 "$(3"H(B" (?e "$(3"C(B") (?u "$(3"D(B") (?i "$(3"E(B") (?a "$(3"F(B") (?E "$(3"G(B") (?o "$(3"I(B")
	          (?W "$(3"P(B" (?e "$(3"K(B") (?u "$(3"P(B") (?i "$(3"M(B") (?a "$(3"N(B") (?E "$(3"O(B"))))
;;; i
   ("$(3"e(B" (?2 "$(3#7(B"))
;;; j
   ("$(3#j(B" (?e "$(3#e(B") (?u "$(3#f(B") (?i "$(3#g(B") (?a "$(3#h(B") (?E "$(3#i(B") (?o "$(3#k(B")
         (?W "$(3#l(B" (?a "$(3#l(B")
	          (?e "$(3#j%n(B") (?u "$(3#j%r(B") (?i "$(3#j%o(B") (?E "$(3#j%q(B")))
;;; k
   ("$(3"p(B" (?e "$(3"k(B") (?u "$(3"l(B") (?i "$(3"m(B") (?a "$(3"n(B") (?E "$(3"o(B") (?o "$(3"q(B")
         (?W "$(3"x(B" (?e "$(3"s(B") (?u "$(3"x(B") (?i "$(3"u(B") (?a "$(3"v(B") (?E "$(3"w(B"))
	 (?2 "$(3%>(B" (?e "$(3%9(B") (?u "$(3%:(B") (?i "$(3%;(B") (?a "$(3%<(B") (?E "$(3%=(B") (?o "$(3%?(B")))
;;; l
   ("$(3!.(B" (?e "$(3!)(B") (?u "$(3!*(B") (?i "$(3!+(B") (?a "$(3!,(B") (?E "$(3!-(B") (?o "$(3!/(B")
         (?W "$(3!0(B" (?a "$(3!0(B")
                  (?e "$(3!.%n(B") (?u "$(3!.%r(B") (?i "$(3!.%o(B") (?E "$(3!.%q(B")))
;;; m
   ("$(3!>(B" (?e "$(3!9(B") (?u "$(3!:(B") (?i "$(3!;(B") (?a "$(3!<(B") (?E "$(3!=(B") (?o "$(3!?(B")
         (?W "$(3%a(B" (?e "$(3%1(B") (?u "$(3%a(B") (?i "$(3%A(B") (?a "$(3!@(B") (?E "$(3%Q(B"))
	 (?Y "$(3$_(B" (?a "$(3$_(B")))
;;; n
   ("$(3"X(B" (?e "$(3"S(B") (?u "$(3"T(B") (?i "$(3"U(B") (?a "$(3"V(B") (?E "$(3"W(B") (?o "$(3"Y(B")
         (?W "$(3"Z(B" (?a "$(3"Z(B")
	          (?e "$(3"X%n(B") (?u "$(3"X%r(B") (?i "$(3"X%o(B") (?E "$(3"X%q(B")))
;;; o
   ("$(3"i(B" (?2 "$(3#;(B"))
;;; p
   ("$(3$\(B" (?e "$(3$W(B") (?u "$(3$X(B") (?i "$(3$Y(B") (?a "$(3$Z(B") (?E "$(3$[(B") (?o "$(3$](B")
         (?W "$(3%e(B" (?e "$(3%5(B") (?u "$(3%e(B") (?i "$(3%E(B") (?a "$(3$^(B") (?E "$(3%U(B")))
;;; q
   ("$(3!f(B" (?e "$(3!a(B") (?u "$(3!b(B") (?i "$(3!c(B") (?a "$(3!d(B") (?E "$(3!e(B") (?o "$(3!g(B")
         (?W "$(3!n(B" (?e "$(3!i(B") (?u "$(3!n(B") (?i "$(3!k(B") (?a "$(3!l(B") (?E "$(3!m(B"))
         (?2 "$(3%.(B" (?e "$(3%)(B") (?u "$(3%*(B") (?i "$(3%+(B") (?a "$(3%,(B") (?E "$(3%-(B") (?o "$(3%/(B")))
;;; r
   ("$(3!N(B" (?e "$(3!I(B") (?u "$(3!J(B") (?i "$(3!K(B") (?a "$(3!L(B") (?E "$(3!M(B") (?o "$(3!O(B")
         (?W "$(3!P(B" (?a "$(3!P(B")
	          (?e "$(3!N%n(B") (?u "$(3!N%r(B") (?i "$(3!N%o(B") (?E "$(3!N%q(B"))
         (?Y "$(3$`(B" (?a "$(3$`(B")))
;;; s
   ("$(3!V(B" (?e "$(3!Q(B") (?u "$(3!R(B") (?i "$(3!S(B") (?a "$(3!T(B") (?E "$(3!U(B") (?o "$(3!W(B")
         (?W "$(3!X(B" (?a "$(3!X(B")
	          (?e "$(3!V%n(B") (?u "$(3!V%r(B") (?i "$(3!V%o(B") (?E "$(3!V%q(B"))
	 (?2 "$(3!F(B" (?e "$(3!A(B") (?u "$(3!B(B") (?i "$(3!C(B") (?a "$(3!D(B") (?E "$(3!E(B") (?o "$(3!G(B")
		  (?W "$(3!H(B" (?a "$(3!H(B")
		           (?e "$(3!F%n(B") (?u "$(3!F%r(B") (?i "$(3!F%o(B") (?E "$(3!F%q(B"))))
;;; t
   ("$(3"8(B" (?e "$(3"3(B") (?u "$(3"4(B") (?i "$(3"5(B") (?a "$(3"6(B") (?E "$(3"7(B") (?o "$(3"9(B")
         (?W "$(3":(B" (?a "$(3":(B")
	          (?e "$(3"8%n(B") (?u "$(3"8%r(B") (?i "$(3"8%o(B") (?E "$(3"8%q(B")))
;;; u
   ("$(3"d(B" (?2 "$(3#6(B"))
;;; v
   ("$(3"0(B" (?e "$(3"+(B") (?u "$(3",(B") (?i "$(3"-(B") (?a "$(3".(B") (?E "$(3"/(B") (?o "$(3"1(B")
         (?W "$(3"2(B" (?a "$(3"2(B")
	          (?e "$(3"0%n(B") (?u "$(3"0%r(B") (?i "$(3"0%o(B") (?E "$(3"0%q(B")))
;;; w
   ("$(3#2(B" (?e "$(3#-(B") (?u "$(3#.(B") (?i "$(3#/(B") (?a "$(3#0(B") (?E "$(3#1(B") (?o "$(3#3(B")
         (?W "$(3%p(B" (?e "$(3%n(B") (?u "$(3%r(B") (?i "$(3%o(B") (?a "$(3%p(B") (?E "$(3%q(B")))
;;; x
   ("$(3!^(B" (?e "$(3!Y(B") (?u "$(3!Z(B") (?i "$(3![(B") (?a "$(3!\(B") (?E "$(3!](B") (?o "$(3!_(B")
         (?W "$(3!`(B" (?a "$(3!`(B")
	          (?e "$(3!^%n(B") (?u "$(3!^%r(B") (?i "$(3!^%o(B") (?E "$(3!^%q(B")))
;;; y
   ("$(3#R(B" (?e "$(3#M(B") (?u "$(3#N(B") (?i "$(3#O(B") (?a "$(3#P(B") (?E "$(3#Q(B") (?o "$(3#S(B")
         (?W "$(3#T(B" (?a "$(3#T(B")
	          (?e "$(3#R%n(B") (?u "$(3#R%r(B") (?i "$(3#R%o(B") (?E "$(3#R%q(B")))
;;; z
   ("$(3#B(B" (?e "$(3#=(B") (?u "$(3#>(B") (?i "$(3#?(B") (?a "$(3#@(B") (?E "$(3#A(B") (?o "$(3#C(B")
         (?W "$(3#D(B" (?a "$(3#D(B")
	          (?e "$(3#B%n(B") (?u "$(3#B%r(B") (?i "$(3#B%o(B") (?E "$(3#B%q(B")))
;;; {   |   }   ~  DEL
   nil nil nil nil nil
   ])

;; To avoid byte-compiler warnings.  It should never be set globally.
(defvar ethio-sera-being-called-by-w3)
;; This variable will be bound by some third-party package.
(defvar sera-being-called-by-w3)

;;;###autoload
(defun ethio-sera-to-fidel-region (beg end &optional secondary force)
  "Convert the characters in region from SERA to FIDEL.
The variable `ethio-primary-language' specifies the primary language
and `ethio-secondary-language' specifies the secondary.

If the 3rd parameter SECONDARY is given and non-nil, assume the region
begins with the secondary language; otherwise with the primary
language.

If the 4th parameter FORCE is given and non-nil, perform conversion
even if the buffer is read-only.

See also the descriptions of the variables
`ethio-use-colon-for-colon' and
`ethio-use-three-dot-question'."

  (interactive "r\nP")
  (save-restriction
    (narrow-to-region beg end)
    (ethio-sera-to-fidel-buffer secondary force)))

;;;###autoload
(defun ethio-sera-to-fidel-buffer (&optional secondary force)
  "Convert the current buffer from SERA to FIDEL.

The variable `ethio-primary-language' specifies the primary
language and `ethio-secondary-language' specifies the secondary.

If the 1st optional parameter SECONDARY is non-nil, assume the buffer
begins with the secondary language; otherwise with the primary
language.

If the 2nd optional parametr FORCE is non-nil, perform conversion even if the
buffer is read-only.

See also the descriptions of the variables
`ethio-use-colon-for-colon' and
`ethio-use-three-dot-question'."

  (interactive "P")

  (if (and buffer-read-only
	   (not force)
	   (not (y-or-n-p "Buffer is read-only.  Force to convert? ")))
      (error ""))

  (let ((ethio-primary-language ethio-primary-language)
	(ethio-secondary-language ethio-secondary-language)
	(ethio-use-colon-for-colon ethio-use-colon-for-colon)
	(ethio-use-three-dot-question ethio-use-three-dot-question)
	;; The above four variables may be changed temporary
	;; by tilde escapes during conversion.  So we bind them to other
	;; variables but of the same names.
	(buffer-read-only nil)
	(case-fold-search nil)
	current-language
	next-language)

    (setq current-language
	  (if secondary
	      ethio-secondary-language
	    ethio-primary-language))

    (goto-char (point-min))

    (while (not (eobp))
      (setq next-language
	    (cond
	     ((eq current-language 'english)
	      (ethio-sera-to-fidel-english))
	     ((eq current-language 'amharic)
	      (ethio-sera-to-fidel-ethio 'amharic))
	     ((eq current-language 'tigrigna)
	      (ethio-sera-to-fidel-ethio 'tigrigna))
	     (t				; we don't know what to do
	      (ethio-sera-to-fidel-english))))

      (setq current-language
	    (cond

	     ;; when language tag is explicitly specified
	     ((not (eq next-language 'toggle))
	      next-language)

	     ;; found a toggle in a primary language section
	     ((eq current-language ethio-primary-language)
	      ethio-secondary-language)

	     ;; found a toggle in a secondary, third, fourth, ...
	     ;; language section
	     (t
	      ethio-primary-language))))

    ;; If ethio-implicit-period-conversion is non-nil, the
    ;; Ethiopic dot "$(3%u(B" at the end of an Ethiopic sentence is
    ;; replaced with the Ethiopic full stop "$(3$i(B".
    (if ethio-implicit-period-conversion
	(progn
	  (goto-char (point-min))
	  (while (re-search-forward "\\([$(3!!(B-$(3$a%)(B-$(3%e%n(B-$(3%r%s(B]\\)$(3%u(B\\([ \t]\\)"
				    nil t)
	    (replace-match "\\1$(3$i(B\\2"))
	  (goto-char (point-min))
	  (while (re-search-forward "\\([$(3!!(B-$(3$a%)(B-$(3%e%n(B-$(3%r%s(B]\\)$(3%u(B$" nil t)
	    (replace-match "\\1$(3$i(B"))))

    ;; gemination
    (goto-char (point-min))
    (while (re-search-forward "\\ce$(3%s(B" nil 0)
      (compose-region
       (save-excursion (backward-char 2) (point))
       (point)))
    ))

(defun ethio-sera-to-fidel-english nil
  "Handle English section in SERA to FIDEL conversion.
Conversion stops when a language switch is found.  Then delete that
switch and return the name of the new language as a symbol."
  (let ((new-language nil))

    (while (and (not (eobp)) (null new-language))
      (cond

       ;; if no more "\", nothing to do.
       ((not (search-forward "\\" nil 0)))

       ;; hereafter point is put after a "\".
       ;; first delete that "\", then check the following chars

       ;; "\\" :  leave the second "\"
       ((progn
	  (delete-backward-char 1)
	  (= (following-char) ?\\ ))
	(forward-char 1))

       ;; "\ " :  delete the following " "
       ((= (following-char) 32)
	(delete-char 1)
	(setq new-language 'toggle))

       ;; a language flag
       ((setq new-language (ethio-process-language-flag)))

       ;; just a "\" :  not special sequence.
       (t
	(setq new-language 'toggle))))

    new-language))

(defun ethio-sera-to-fidel-ethio (lang)
  "Handle Ethiopic section in SERA to FIDEL conversion.
Conversion stops when a language switch is found.  Then delete that
switch and return the name of the new language as a symbol.

The parameter LANG (symbol, either `amharic' or `tigrigna') affects
the conversion of \"a\"."

  (let ((new-language nil)
	(verbatim nil)
	start table table2 ch)

    (setcar (aref ethio-sera-to-fidel-table ?a)
	    (if (eq lang 'tigrigna) "$(3"f(B" "$(3"c(B"))

    (while (and (not (eobp)) (null new-language))
      (setq ch (following-char))
      (cond

       ;; skip from "<" to ">" (or from "&" to ";") if in w3-mode
       ((and (or (= ch ?<) (= ch ?&))
	     (or (and (boundp 'ethio-sera-being-called-by-w3)
		      ethio-sera-being-called-by-w3)
		 (and (boundp 'sera-being-called-by-w3)
		      sera-being-called-by-w3)))
	(search-forward (if (= ch ?<) ">" ";")
			nil 0))

       ;; leave non-ASCII characters as they are
       ((>= ch 128)
	(forward-char 1))

       ;; ethiopic digits
       ((looking-at "`[1-9][0-9]*")
	(delete-char 1)
	(ethio-convert-digit))

       ;; if not seeing a "\", do sera to fidel conversion
       ((/= ch ?\\ )
	(setq start (point))
	(forward-char 1)
	(setq table (aref ethio-sera-to-fidel-table ch))
	(while (setq table2 (cdr (assoc (following-char) table)))
	  (setq table table2)
	  (forward-char 1))
	(if (setq ch (car table))
	    (progn
	      (delete-region start (point))
	      (if (stringp ch)
		  (insert ch)
		(insert (eval ch))))))

       ;; if control reaches here, we must be looking at a "\"

       ;; verbatim mode
       (verbatim
	(if (looking-at "\\\\~! ?")

	    ;; "\~!" or "\~! ".  switch to non-verbatim mode
	    (progn
	      (replace-match "")
	      (setq verbatim nil))

	  ;; "\" but not "\~!" nor "\~! ".  skip the current "\".
	  (forward-char 1)))

       ;; hereafter, non-verbatim mode and looking at a "\"
       ;; first delete that "\", then check the following chars.

       ;; "\ " : delete the following " "
       ((progn
	  (delete-char 1)
	  (setq ch (following-char))
	  (= ch 32))
	(delete-char 1)
	(setq new-language 'toggle))

       ;; "\~!" or "\~! " : switch to verbatim mode
       ((looking-at "~! ?")
	(replace-match "")
	(setq verbatim t))

       ;; a language flag
       ((setq new-language (ethio-process-language-flag)))

       ;; "\~" but not "\~!" nor a language flag
       ((= ch ?~)
	(delete-char 1)
	(ethio-tilde-escape))

       ;; ASCII punctuation escape.  skip
       ((looking-at "\\(,\\|\\.\\|;\\|:\\|'\\|`\\|\?\\|\\\\\\)+")
	(goto-char (match-end 0)))

       ;; "\", but not special sequence
       (t
	(setq new-language 'toggle))))

    new-language))

(defun ethio-process-language-flag nil
  "Process a language flag of the form \"~lang\" or \"~lang1~lang2\".

If looking at \"~lang1~lang2\", set `ethio-primary-language' and
`ethio-une-secondary-language' based on \"lang1\" and \"lang2\".
Then delete the language flag \"~lang1~lang2\" from the buffer.
Return value is the new primary language.

If looking at \"~lang\", delete that language flag \"~lang\" from the
buffer and return that language.  In this case
`ethio-primary-language' and `ethio-uni-secondary-language'
are left unchanged.

If an unsupported language flag is found, just return nil without
changing anything."

  (let (lang1 lang2)
    (cond

     ;; ~lang1~lang2
     ((and (looking-at
	    "~\\([a-z][a-z][a-z]?\\)~\\([a-z][a-z][a-z]?\\)[ \t\n\\]")
	   (setq lang1
		 (ethio-flag-to-language
		  (buffer-substring (match-beginning 1) (match-end 1))))
	   (setq lang2
		 (ethio-flag-to-language
		  (buffer-substring (match-beginning 2) (match-end 2)))))
      (setq ethio-primary-language lang1
	    ethio-secondary-language lang2)
      (delete-region (point) (match-end 2))
      (if (= (following-char) 32)
	  (delete-char 1))
      ethio-primary-language)

     ;; ~lang
     ((and (looking-at "~\\([a-z][a-z][a-z]?\\)[ \t\n\\]")
	   (setq lang1
		 (ethio-flag-to-language
		  (buffer-substring (match-beginning 1) (match-end 1)))))
      (delete-region (point) (match-end 1))
      (if (= (following-char) 32)
	  (delete-char 1))
      lang1)

     ;; otherwise
     (t
      nil))))

(defun ethio-tilde-escape nil
  "Handle a SERA tilde escape in Ethiopic section and delete it.
Delete the escape even it is not recognized."

  (let ((p (point)) command)
    (skip-chars-forward "^ \t\n\\\\")
    (setq command (buffer-substring p (point)))
    (delete-region p (point))
    (if (= (following-char) 32)
	(delete-char 1))

    (cond

     ;; \~-:
     ((string= command "-:")
      (setq ethio-use-colon-for-colon t))

     ;; \~`:
     ((string= command "`:")
      (setq ethio-use-colon-for-colon nil))

     ;; \~?
     ((string= command "?")
      (setq ethio-use-three-dot-question nil))

     ;; \~`|
     ((string= command "`|")
      (setq ethio-use-three-dot-question t))

     ;; \~e
     ((string= command "e")
      (insert "$(3%j(B"))

     ;; \~E
     ((string= command "E")
      (insert "$(3%k(B"))

     ;; \~a
     ((string= command "a")
      (insert "$(3%l(B"))

     ;; \~A
     ((string= command "A")
      (insert "$(3%m(B"))

     ;; \~X
     ((string= command "X")
      (insert "$(3%i(B"))

     ;; unsupported tilde escape
     (t
      nil))))

(defun ethio-flag-to-language (flag)
  (cond
   ((or (string= flag "en") (string= flag "eng")) 'english)
   ((or (string= flag "ti") (string= flag "tir")) 'tigrigna)
   ((or (string= flag "am") (string= flag "amh")) 'amharic)
   (t nil)))

(defun ethio-convert-digit nil
  "Convert Arabic digits to Ethiopic digits."
  (let (ch z)
    (while (and (>= (setq ch (following-char)) ?1)
		(<= ch ?9))
      (delete-char 1)

      ;; count up following zeros
      (setq z 0)
      (while (= (following-char) ?0)
	(delete-char 1)
	(setq z (1+ z)))

      (cond

       ;; first digit is 10, 20, ..., or 90
       ((= (mod z 2) 1)
	(insert (aref [?$(3$y(B ?$(3$z(B ?$(3${(B ?$(3$|(B ?$(3$}(B ?$(3$~(B ?$(3%!(B ?$(3%"(B ?$(3%#(B] (- ch ?1)))
	(setq z (1- z)))

       ;; first digit is 2, 3, ..., or 9
       ((/= ch ?1)
	(insert (aref [?$(3$q(B ?$(3$r(B ?$(3$s(B ?$(3$t(B ?$(3$u(B ?$(3$v(B ?$(3$w(B ?$(3$x(B] (- ch ?2))))

       ;; single 1
       ((= z 0)
	(insert "$(3$p(B")))

      ;; 100
      (if (= (mod z 4) 2)
	  (insert "$(3%$(B"))

      ;; 10000
      (insert-char ?$(3%%(B (/ z 4)))))

;;;###autoload
(defun ethio-sera-to-fidel-mail-or-marker (&optional arg)
  "Execute ethio-sera-to-fidel-mail or ethio-sera-to-fidel-marker depending on the current major mode.
If in rmail-mode or in mail-mode, execute the former; otherwise latter."

  (interactive "P")
  (if (or (eq major-mode 'rmail-mode)
	  (eq major-mode 'mail-mode))
      (ethio-sera-to-fidel-mail (prefix-numeric-value arg))
    (ethio-sera-to-fidel-marker arg)))

;;;###autoload
(defun ethio-sera-to-fidel-mail (&optional arg)
  "Convert SERA to FIDEL to read/write mail and news.

If the buffer contains the markers \"<sera>\" and \"</sera>\",
convert the segments between them into FIDEL.

If invoked interactively and there is no marker, convert the subject field
and the body into FIDEL using `ethio-sera-to-fidel-region'."

  (interactive "p")
  (let ((buffer-read-only nil)
	border)
    (save-excursion

      ;; follow RFC822 rules instead of looking for a fixed separator
      (rfc822-goto-eoh)
      (forward-line 1)
      (setq border (point))

      ;; note that the point is placed at the border
      (if (or (re-search-forward "^<sera>$" nil t)
	      (progn
		(goto-char (point-min))
		(re-search-forward "^Subject: <sera>" border t)))

	  ;; there are markers
	  (progn
	    ;; we start with the body so that the border will not change
	    ;; use "^<sera>\n" instead of "^<sera>$" not to leave a blank line
	    (goto-char border)
	    (while (re-search-forward "^<sera>\n" nil t)
	      (replace-match "")
	      (ethio-sera-to-fidel-region
	       (point)
	       (progn
		 (if (re-search-forward "^</sera>\n" nil 0)
		     (replace-match ""))
		 (point))))
	    ;; now process the subject
	    (goto-char (point-min))
	    (if (re-search-forward "^Subject: <sera>" border t)
		(ethio-sera-to-fidel-region
		 (progn (delete-backward-char 6) (point))
		 (progn
		   (if (re-search-forward "</sera>$" (line-end-position) 0)
		       (replace-match ""))
		   (point)))))

	;; in case there are no marks but invoked interactively
	(if arg
	    (progn
	      (ethio-sera-to-fidel-region border (point-max))
	      (goto-char (point-min))
	      (if (re-search-forward "^Subject: " border t)
		  (ethio-sera-to-fidel-region (point) (line-end-position))))))

      ;; adjust the rmail marker
      (if (eq major-mode 'rmail-mode)
	  (set-marker
	   (aref rmail-message-vector (1+ rmail-current-message))
	   (point-max))))))

;;;###autoload
(defun ethio-sera-to-fidel-marker (&optional force)
  "Convert the regions surrounded by \"<sera>\" and \"</sera>\" from SERA to FIDEL.
Assume that each region begins with `ethio-primary-language'.
The markers \"<sera>\" and \"</sera>\" themselves are not deleted."
  (interactive "P")
  (if (and buffer-read-only
	   (not force)
	   (not (y-or-n-p "Buffer is read-only.  Force to convert? ")))
      (error ""))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "<sera>" nil t)
      (ethio-sera-to-fidel-region
       (point)
       (if (re-search-forward "</sera>" nil t)
	   (match-beginning 0)
	 (point-max))
       nil
       'force))))

;;
;; FIDEL to SERA
;;

(defconst ethio-fidel-to-sera-map
 [ "he"  "hu"  "hi"  "ha"  "hE"  "h"  "ho"    ""       ;;   0 - 7
   "le"  "lu"  "li"  "la"  "lE"  "l"  "lo"  "lWa"      ;;   8
   "He"  "Hu"  "Hi"  "Ha"  "HE"  "H"  "Ho"  "HWa"      ;;  16
   "me"  "mu"  "mi"  "ma"  "mE"  "m"  "mo"  "mWa"      ;;  24
  "`se" "`su" "`si" "`sa" "`sE" "`s" "`so" "`sWa"      ;;  32
   "re"  "ru"  "ri"  "ra"  "rE"  "r"  "ro"  "rWa"      ;;  40
   "se"  "su"  "si"  "sa"  "sE"  "s"  "so"  "sWa"      ;;  48
   "xe"  "xu"  "xi"  "xa"  "xE"  "x"  "xo"  "xWa"      ;;  56
   "qe"  "qu"  "qi"  "qa"  "qE"  "q"  "qo"    ""       ;;  64
  "qWe"   ""  "qWi" "qWa" "qWE"  "qW'" ""     ""       ;;  72
   "Qe"  "Qu"  "Qi"  "Qa"  "QE"  "Q"  "Qo"    ""       ;;  80
  "QWe"   ""  "QWi" "QWa" "QWE"  "QW'" ""     ""       ;;  88
   "be"  "bu"  "bi"  "ba"  "bE"  "b"  "bo"  "bWa"      ;;  96
   "ve"  "vu"  "vi"  "va"  "vE"  "v"  "vo"  "vWa"      ;; 104
   "te"  "tu"  "ti"  "ta"  "tE"  "t"  "to"  "tWa"      ;; 112
   "ce"  "cu"  "ci"  "ca"  "cE"  "c"  "co"  "cWa"      ;; 120
  "`he" "`hu" "`hi" "`ha" "`hE" "`h" "`ho"    ""       ;; 128
  "hWe"   ""  "hWi" "hWa"  "hWE" "hW'" ""     ""       ;; 136
   "ne"  "nu"  "ni"  "na"  "nE"  "n"  "no"  "nWa"      ;; 144
   "Ne"  "Nu"  "Ni"  "Na"  "NE"  "N"  "No"  "NWa"      ;; 152
    "e"   "u"   "i"   "A"   "E"  "I"   "o"   "ea"      ;; 160
   "ke"  "ku"  "ki"  "ka"  "kE"  "k"  "ko"    ""       ;; 168
  "kWe"   ""  "kWi" "kWa" "kWE"  "kW'" ""     ""       ;; 176
   "Ke"  "Ku"  "Ki"  "Ka"  "KE"  "K"  "Ko"    ""       ;; 184
  "KWe"   ""  "KWi" "KWa" "KWE"  "KW'" ""     ""       ;; 192
   "we"  "wu"  "wi"  "wa"  "wE"  "w"  "wo"    ""       ;; 200
   "`e"  "`u"  "`i"  "`a"  "`E" "`I"  "`o"    ""       ;; 208
   "ze"  "zu"  "zi"  "za"  "zE"  "z"  "zo"  "zWa"      ;; 216
   "Ze"  "Zu"  "Zi"  "Za"  "ZE"  "Z"  "Zo"  "ZWa"      ;; 224
   "ye"  "yu"  "yi"  "ya"  "yE"  "y"  "yo"  "yWa"      ;; 232
   "de"  "du"  "di"  "da"  "dE"  "d"  "do"  "dWa"      ;; 240
   "De"  "Du"  "Di"  "Da"  "DE"  "D"  "Do"  "DWa"      ;; 248
   "je"  "ju"  "ji"  "ja"  "jE"  "j"  "jo"  "jWa"      ;; 256
   "ge"  "gu"  "gi"  "ga"  "gE"  "g"  "go"    ""       ;; 264
  "gWe"   ""  "gWi" "gWa" "gWE" "gW'"  ""     ""       ;; 272
   "Ge"  "Gu"  "Gi"  "Ga"  "GE"  "G"  "Go"  "GWa"      ;; 280
   "Te"  "Tu"  "Ti"  "Ta"  "TE"  "T"  "To"  "TWa"      ;; 288
   "Ce"  "Cu"  "Ci"  "Ca"  "CE"  "C"  "Co"  "CWa"      ;; 296
   "Pe"  "Pu"  "Pi"  "Pa"  "PE"  "P"  "Po"  "PWa"      ;; 304
   "Se"  "Su"  "Si"  "Sa"  "SE"  "S"  "So"  "SWa"      ;; 312
  "`Se" "`Su" "`Si" "`Sa" "`SE" "`S" "`So"    ""       ;; 320
   "fe"  "fu"  "fi"  "fa"  "fE"  "f"  "fo"  "fWa"      ;; 328
   "pe"  "pu"  "pi"  "pa"  "pE"  "p"  "po"  "pWa"      ;; 336
  "mYa" "rYa" "fYa"   ""    ""   ""    ""     ""       ;; 344
   " "  " : "  "::"  ","   ";"  "-:"  ":-"   "`?"      ;; 352
  ":|:"  "1"   "2"   "3"   "4"   "5"   "6"   "7"       ;; 360
   "8"   "9"   "10"  "20"  "30"  "40" "50"   "60"      ;; 368
   "70"  "80"  "90" "100" "10000" ""   ""     ""       ;; 376
  "`qe" "`qu" "`qi" "`qa" "`qE" "`q" "`qo"    ""       ;; 384
  "mWe" "bWe" "GWe" "fWe" "pWe"  ""    ""     ""       ;; 392
  "`ke" "`ku" "`ki" "`ka" "`kE" "`k" "`ko"    ""       ;; 400
  "mWi" "bWi" "GWi" "fWi" "pWi"  ""    ""     ""       ;; 408
   "Xe"  "Xu"  "Xi"  "Xa"  "XE"  "X"  "Xo"    ""       ;; 416
  "mWE" "bWE" "GWE" "fWE" "pWE"  ""    ""     ""       ;; 424
  "`ge" "`gu" "`gi" "`ga" "`gE" "`g" "`go"    ""       ;; 432
  "mW'" "bW'" "GW'" "fW'" "pW'"  ""    ""     ""       ;; 440
  "\\~X " "\\~e " "\\~E " "\\~a " "\\~A " "wWe" "wWi" "wWa" ;; 448
  "wWE" "wW'"  "''"  "`!"  "."  "<<"  ">>"   "?" ])    ;; 456

(defun ethio-prefer-amharic-p nil
  (or (eq ethio-primary-language 'amharic)
      (and (not (eq ethio-primary-language 'tigrigna))
	   (eq ethio-secondary-language 'amharic))))

(defun ethio-language-to-flag (lang)
  (cond
   ((eq lang 'english) "eng")
   ((eq lang 'tigrigna) "tir")
   ((eq lang 'amharic) "amh")
   (t "")))

;;;###autoload
(defun ethio-fidel-to-sera-region (begin end &optional secondary force)
  "Replace all the FIDEL characters in the region to the SERA format.
The variable `ethio-primary-language' specifies the primary
language and `ethio-secondary-language' specifies the secondary.

If the 3dr parameter SECONDARY is given and non-nil, try to convert
the region so that it begins in the secondary language; otherwise with
the primary language.

If the 4th parameter FORCE is given and non-nil, convert even if the
buffer is read-only.

See also the descriptions of the variables
`ethio-use-colon-for-colon', `ethio-use-three-dot-question',
`ethio-quote-vowel-always' and `ethio-numeric-reduction'."

  (interactive "r\nP")
  (save-restriction
    (narrow-to-region begin end)
    (ethio-fidel-to-sera-buffer secondary force)))

;;;###autoload
(defun ethio-fidel-to-sera-buffer (&optional secondary force)
  "Replace all the FIDEL characters in the current buffer to the SERA format.
The variable `ethio-primary-language' specifies the primary
language and `ethio-secondary-language' specifies the secondary.

If the 1st optional parameter SECONDARY is non-nil, try to convert the
region so that it begins in the secondary language; otherwise with the
primary language.

If the 2nd optional parameter FORCE is non-nil, convert even if the
buffer is read-only.

See also the descriptions of the variables
`ethio-use-colon-for-colon', `ethio-use-three-dot-question',
`ethio-quote-vowel-always' and `ethio-numeric-reduction'."

  (interactive "P")
  (if (and buffer-read-only
	   (not force)
	   (not (y-or-n-p "Buffer is read-only.  Force to convert? ")))
      (error ""))

  (let ((buffer-read-only nil)
	(case-fold-search nil)
	(lonec nil) ;; t means previous char was a lone consonant
	(fidel nil) ;; t means previous char was a FIDEL
	(digit nil) ;; t means previous char was an Ethiopic digit
	(flag (if (ethio-prefer-amharic-p) "\\~amh " "\\~tir "))
	mode ch)

    ;; user's preference in transcription
    (if ethio-use-colon-for-colon
	(progn
	  (aset ethio-fidel-to-sera-map 353 "`:")
	  (aset ethio-fidel-to-sera-map 357 ":"))
      (aset ethio-fidel-to-sera-map 353 " : ")
      (aset ethio-fidel-to-sera-map 357 "-:"))

    (if ethio-use-three-dot-question
	(progn
	  (aset ethio-fidel-to-sera-map 359 "?")
	  (aset ethio-fidel-to-sera-map 463 "`?"))
      (aset ethio-fidel-to-sera-map 359 "`?")
      (aset ethio-fidel-to-sera-map 463 "?"))

    (mapcar
     '(lambda (x)
	(aset (aref ethio-fidel-to-sera-map x)
	      2
	      (if ethio-W-sixth-always ?' ?u)))
     '(77 93 141 181 197 277 440 441 442 443 444 457))

    (if (ethio-prefer-amharic-p)
	(aset ethio-fidel-to-sera-map 160 "a")
      (aset ethio-fidel-to-sera-map 160 "e"))
    ;; end of user's preference

    ;; first, decompose geminated characters
    (decompose-region (point-min) (point-max))

    ;; main conversion routine
    (goto-char (point-min))
    (while (not (eobp))
      (setq ch (following-char))

      (cond				; ethiopic, english, neutral

       ;; ethiopic character.  must go to ethiopic mode, if not in it.
       ((eq (char-charset ch) 'ethiopic)
	(setq ch (ethio-char-to-ethiocode ch))
	(delete-char 1)
	(if (not (eq mode 'ethiopic))
	    (progn
	      (insert flag)
	      (setq mode 'ethiopic)))

	(cond				; fidel, punc, digit

	 ;; fidels
	 ((or (<= ch 346)		;  he - fYa
	      (and (>= ch 384) (<= ch 444)) ; `qe - pw
	      (and (>= ch 453) (<= ch 457))) ; wWe - wW
	  (if (and (memq ch '(160 161 162 163 164 166 167)) ; (e - ea)
		   (or lonec
		       (and ethio-quote-vowel-always
			    fidel)))
	      (insert "'"))
	  (insert (aref ethio-fidel-to-sera-map ch))
	  (setq lonec (ethio-lone-consonant-p ch)
		fidel t
		digit nil))

	 ;; punctuations or icons
	 ((or (and (>= ch 353) (<= ch 360)) ;  : - :|:
	      (>= ch 458)		;  '' -  ?
	      (and (>= ch 448) (<= ch 452))) ;  \~X \~e \~E \~a \~A
	  (insert (aref ethio-fidel-to-sera-map ch))
	  (setq lonec nil
		fidel nil
		digit nil))

	 ;; now CH must be an ethiopic digit

	 ;; reduction = 0 or not preceded by Ethiopic number(s)
	 ((or (= ethio-numeric-reduction 0)
	      (not digit))
	  (insert "`" (aref ethio-fidel-to-sera-map ch))
	  (setq lonec nil
		fidel nil
		digit t))

	 ;; reduction = 2 and following 10s, 100s, 10000s
	 ((and (= ethio-numeric-reduction 2)
	       (memq ch '(370 379 380)))
	  (insert (substring (aref ethio-fidel-to-sera-map ch) 1))
	  (setq lonec nil
		fidel nil
		digit t))

	 ;; ordinary following digits
	 (t
	  (insert (aref ethio-fidel-to-sera-map ch))
	  (setq lonec nil
		fidel nil
		digit t))))

       ;; english character.  must go to english mode, if not in it.
       ((or (and (>= ch ?a) (<= ch ?z))
	    (and (>= ch ?A) (<= ch ?Z)))
	(if (not (eq mode 'english))
	    (insert "\\~eng "))
	(forward-char 1)
	(setq mode 'english
	      lonec nil
	      fidel nil
	      digit nil))

       ;; ch can appear both in ethiopic section and in english section.
       (t

	;; we must decide the mode, if not decided yet
	(if (null mode)
	    (progn
	      (setq mode
		    (if secondary
			ethio-secondary-language
		      ethio-primary-language))
	      (if (eq mode 'english)
		  (insert "\\~eng ")
		(insert flag)
		(setq mode 'ethiopic)))) ; tigrigna & amharic --> ethiopic

	(cond				; \ , eng-mode , punc , w3 , other

	 ;; backslash is always quoted
	 ((= ch ?\\ )
	  (insert "\\")
	  (forward-char 1))

	 ;; nothing to do if in english mode
	 ((eq mode 'english)
	  (forward-char 1))

	 ;; now we must be in ethiopic mode and seeing a non-"\"

	 ;; ascii punctuations in ethiopic mode
	 ((looking-at "[,.;:'`?]+")
	  (insert "\\")
	  (goto-char (1+ (match-end 0)))) ; because we inserted one byte (\)

	 ;; skip from "<" to ">" (or from "&" to ";") if called from w3
	 ((and (or (= ch ?<) (= ch ?&))
	       (or (and (boundp 'ethio-sera-being-called-by-w3)
			ethio-sera-being-called-by-w3)
		   (and (boundp 'sera-being-called-by-w3)
			sera-being-called-by-w3)))
	  (search-forward (if (= ch ?<) ">" ";")
			  nil 0))

	 ;; neutral character.  no need to quote.  just skip it.
	 (t
	  (forward-char 1)))

	(setq lonec nil
	      fidel nil
	      digit nil)))
    ;; end of main conversion routine
    )))

(defun ethio-lone-consonant-p (ethiocode)
  "If ETHIOCODE is an Ethiopic lone consonant, return t."
  (or (and (< ethiocode 344) (= (% ethiocode 8) 5))

      ;;                     `q  `k   X  `g  mW  bW  GW  fW  pW  wW
      (memq ethiocode '(389 405 421 437 440 441 442 443 444 457))))

;;;###autoload
(defun ethio-fidel-to-sera-mail-or-marker (&optional arg)
  "Execute ethio-fidel-to-sera-mail or ethio-fidel-to-sera-marker depending on the current major mode.
If in rmail-mode or in mail-mode, execute the former; otherwise latter."

  (interactive "P")
  (if (or (eq major-mode 'rmail-mode)
	  (eq major-mode 'mail-mode))
      (ethio-fidel-to-sera-mail)
    (ethio-fidel-to-sera-marker arg)))

;;;###autoload
(defun ethio-fidel-to-sera-mail nil
  "Convert FIDEL to SERA to read/write mail and news.

If the body contains at least one Ethiopic character,
 1) insert the string \"<sera>\" at the beginning of the body,
 2) insert \"</sera>\" at the end of the body, and
 3) convert the body into SERA.

The very same procedure applies to the subject field, too."

  (interactive)
  (let ((buffer-read-only nil)
	border)
    (save-excursion

      ;; follow RFC822 rules instead of looking for a fixed separator
      (rfc822-goto-eoh)
      (forward-line 1)
      (setq border (point))

      ;; process body first not to change the border
      ;; note that the point is already at the border
      (if (re-search-forward "\\ce" nil t)
	  (progn
	    (ethio-fidel-to-sera-region border (point-max))
	    (goto-char border)
	    (insert "<sera>")
	    (goto-char (point-max))
	    (insert "</sera>")))

      ;; process subject
      (goto-char (point-min))
      (if (re-search-forward "^Subject: " border t)
	  (let ((beg (point))
		(end (line-end-position)))
	    (if (re-search-forward "\\ce" end t)
		(progn
		  (ethio-fidel-to-sera-region beg end)
		  (goto-char beg)
		  (insert "<sera>")
		  (end-of-line)
		  (insert "</sera>")))))

      ;; adjust the rmail marker
      (if (eq major-mode 'rmail-mode)
	  (set-marker
	   (aref rmail-message-vector (1+ rmail-current-message))
	   (point-max))))))

;;;###autoload
(defun ethio-fidel-to-sera-marker (&optional force)
  "Convert the regions surrounded by \"<sera>\" and \"</sera>\" from FIDEL to SERA.
The markers \"<sera>\" and \"</sera>\" themselves are not deleted."

  (interactive "P")
  (if (and buffer-read-only
	   (not force)
	   (not (y-or-n-p "Buffer is read-only.  Force to convert? ")))
      (error ""))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "<sera>" nil t)
      (ethio-fidel-to-sera-region
       (point)
       (if (re-search-forward "</sera>" nil t)
	   (match-beginning 0)
	 (point-max))
       nil
       'force))))

;;
;; vowel modification
;;

;;;###autoload
(defun ethio-modify-vowel nil
  "Modify the vowel of the FIDEL that is under the cursor."
  (interactive)
  (let ((ch (following-char))
	(composite nil)			; geminated or not
	newch base vowel modulo)

    (cond
     ;; in case of gemination
     ((eq (char-charset ch) 'composition)
      (setq ch (string-to-char (char-to-string ch))
	    composite t))
     ;; neither gemination nor fidel
     ((not (eq (char-charset ch) 'ethiopic))
      (error "Not a valid character")))

    ;; set frequently referred character features
    (setq ch     (ethio-char-to-ethiocode ch)
	  base   (* (/ ch 8) 8)
	  modulo (% ch 8))

    (if (or (and (>= ch 344) (<= ch 380)) ;; mYa - `10000
	    (and (>= ch 448) (<= ch 452)) ;; \~X - \~A
	    (>= ch 458))		  ;; private punctuations
	(error "Not a valid character"))

    (setq
     newch
     (cond

      ;; first standalone vowels
      ((= base 160)
       (if (ethio-prefer-amharic-p)
	   (message "Modify vowel to: [auiAEIoW\"] ")
	 (message "Modify vowel to: [euiAEIoW\"] "))
       (setq vowel (read-char))
       (cond
	((= vowel ?e) 160)
	((= vowel ?u) 161)
	((= vowel ?i) 162)
	((= vowel ?A) 163)
	((= vowel ?E) 164)
	((= vowel ?I) 165)
	((= vowel ?o) 166)
	((= vowel ?W) 167)
	((= vowel ?a) (if (ethio-prefer-amharic-p) 160 163))
	((= vowel ?\") (setq composite t) ch)
	(t nil)))

      ;; second standalone vowels
      ((= base 208)
       (message "Modify vowel to: [euiaEIo\"] ")
       (setq vowel (read-char))
       (cond
	((= vowel ?e) 208)
	((= vowel ?u) 209)
	((= vowel ?i) 210)
	((= vowel ?a) 211)
	((= vowel ?E) 212)
	((= vowel ?I) 213)
	((= vowel ?o) 214)
	((= vowel ?\") (setq composite t) ch)
	(t nil)))

      ;; 12-form consonants, *W* form
      ((memq base '(72 88 136 176 192 272)) ; qW QW hW kW KW gW
       (message "Modify vowel to: [euiaE'\"] ")
       (setq vowel (read-char))
       (cond
	((= vowel ?e) base)
	((= vowel ?u) (+ base 5))
	((= vowel ?i) (+ base 2))
	((= vowel ?a) (+ base 3))
	((= vowel ?E) (+ base 4))
	((= vowel ?') (+ base 5))
	((= vowel ?\") (setq composite t) ch)
	(t nil)))

      ;; extended 12-form consonants, mWa bWa GWa fWa pWa
      ((= ch 31)			; mWa
       (message "Modify vowel to: [euiaE'\"] ")
       (setq vowel (read-char))
       (cond
	((= vowel ?e) 392)
	((= vowel ?u) 440)
	((= vowel ?i) 408)
	((= vowel ?a) ch)
	((= vowel ?E) 424)
	((= vowel ?') 440)
	((= vowel ?\") (setq composite t) ch)
	(t nil)))
      ((= ch 103)			; bWa
       (message "Modify vowel to: [euiaE'\"] ")
       (setq vowel (read-char))
       (cond
	((= vowel ?e) 393)
	((= vowel ?u) 441)
	((= vowel ?i) 409)
	((= vowel ?a) ch)
	((= vowel ?E) 425)
	((= vowel ?') 441)
	((= vowel ?\") (setq composite t) ch)
	(t nil)))
      ((= ch 287)			; GWa
       (message "Modify vowel to: [euiaE'\"] ")
       (setq vowel (read-char))
       (cond
	((= vowel ?e) 394)
	((= vowel ?u) 442)
	((= vowel ?i) 410)
	((= vowel ?a) ch)
	((= vowel ?E) 426)
	((= vowel ?') 442)
	((= vowel ?\") (setq composite t) ch)
	(t nil)))
      ((= ch 335)			; fWa
       (message "Modify vowel to: [euiaE'\"] ")
       (setq vowel (read-char))
       (cond
	((= vowel ?e) 395)
	((= vowel ?u) 443)
	((= vowel ?i) 411)
	((= vowel ?a) ch)
	((= vowel ?E) 427)
	((= vowel ?') 443)
	((= vowel ?\") (setq composite t) ch)
	(t nil)))
      ((= ch 343)			; pWa
       (message "Modify vowel to: [euiaE'\"] ")
       (setq vowel (read-char))
       (cond
	((= vowel ?e) 396)
	((= vowel ?u) 444)
	((= vowel ?i) 412)
	((= vowel ?a) ch)
	((= vowel ?E) 428)
	((= vowel ?') 444)
	((= vowel ?\") (setq composite t) ch)
	(t nil)))

      ;; extended 12-form consonatns, mW* bW* GW* fW* pW*
      ((memq base '(392 408 424 440))	; *We *Wi *WE *W
       (message "Modify vowel to: [eiEau'\"] ")
       (setq vowel (read-char))
       (cond
	((= vowel ?e) (+ 392 modulo))
	((= vowel ?i) (+ 408 modulo))
	((= vowel ?E) (+ 424 modulo))
	((= vowel ?a) (cond
		       ((= modulo 0)  31) ; mWa
		       ((= modulo 1) 103) ; bWa
		       ((= modulo 2) 287) ; GWa
		       ((= modulo 3) 335) ; fWa
		       ((= modulo 4) 343) ; pWa
		       (t nil)))	; never reach here
	((= vowel ?') (+ 440 modulo))
	((= vowel ?u) (+ 440 modulo))
	((= vowel ?\") (setq composite t) ch)
	(t nil)))

      ((and (>= ch 453) (<= ch 457))	; wWe wWi wWa wWE wW
       (message "Modify vowel to: [eiaE'u\"] ")
       (setq vowel (read-char))
       (cond
	((= vowel ?e) 453)
	((= vowel ?i) 454)
	((= vowel ?a) 455)
	((= vowel ?E) 456)
	((= vowel ?') 457)
	((= vowel ?u) 457)
	((= vowel ?\") (setq composite t) ch)
	(t nil)))

      ;; 7-form consonants, or
      ;; first 7 of 8-form consonants
      ((<= modulo 6)
       (message "Modify vowel to: [euiaE'o\"] ")
       (setq vowel (read-char))
       (cond
	((= vowel ?e) base)
	((= vowel ?u) (+ base 1))
	((= vowel ?i) (+ base 2))
	((= vowel ?a) (+ base 3))
	((= vowel ?E) (+ base 4))
	((= vowel ?') (+ base 5))
	((= vowel ?o) (+ base 6))
	((= vowel ?\") (setq composite t) ch)
	(t nil)))

      ;; otherwise
      (t
       nil)))

    (cond

     ;; could not get new character
     ((null newch)
      (error "Invalid vowel"))

     ;; vowel changed on a composite Fidel
     (composite
      (delete-char 1)
      (insert
       (compose-string
	(concat (char-to-string (ethio-ethiocode-to-char newch))	"$(3%s(B"))))

     ;; simple vowel modification
     (t
      (delete-char 1)
      (insert (ethio-ethiocode-to-char newch))))))

(defun ethio-ethiocode-to-char (ethiocode)
  (make-char
   'ethiopic
   (+ (/ ethiocode 94) 33)
   (+ (mod ethiocode 94) 33)))

(defun ethio-char-to-ethiocode (ch)
  (and (eq (char-charset ch) 'ethiopic)
       (let ((char-components (split-char ch)))
	 (+ (* (- (nth 1 char-components) 33) 94)
	    (- (nth 2 char-components) 33)))))

;;
;; space replacement
;;

;;;###autoload
(defun ethio-replace-space (ch begin end)
  "Replace ASCII spaces with Ethiopic word separators in the region.

In the specified region, replace word separators surrounded by two
Ethiopic characters, depending on the first parameter CH, which should
be 1, 2, or 3.

If CH = 1, word separator will be replaced with an ASCII space.
If CH = 2, with two ASCII spaces.
If CH = 3, with the Ethiopic colon-like word separator.

The second and third parameters BEGIN and END specify the region."

  (interactive "*cReplace spaces to: 1 (sg col), 2 (dbl col), 3 (Ethiopic)\nr")
  (if (not (memq ch '(?1 ?2 ?3)))
      (error ""))
  (save-excursion
    (save-restriction
      (narrow-to-region begin end)

      (cond
       ((= ch ?1)
	;; an Ethiopic word separator --> an ASCII space
	(goto-char (point-min))
	(while (search-forward "$(3$h(B" nil t)
	  (replace-match " " nil t))

	;; two ASCII spaces between Ethiopic characters --> an ASCII space
	(goto-char (point-min))
	(while (re-search-forward "\\(\\ce\\)  \\(\\ce\\)" nil t)
	  (replace-match "\\1 \\2")
	  (goto-char (match-beginning 2))))

       ((= ch ?2)
	;; An Ethiopic word separator --> two ASCII spaces
	(goto-char (point-min))
	(while (search-forward "$(3$h(B" nil t)
	  (replace-match "  "))

	;; An ASCII space between Ethiopic characters --> two ASCII spaces
	(goto-char (point-min))
	(while (re-search-forward "\\(\\ce\\) \\(\\ce\\)" nil t)
	  (replace-match "\\1  \\2")
	  (goto-char (match-beginning 2))))

       (t
	;; One or two ASCII spaces between Ethiopic characters
	;;   --> An Ethiopic word separator
	(goto-char (point-min))
	(while (re-search-forward "\\(\\ce\\)  ?\\(\\ce\\)" nil t)
	  (replace-match "\\1$(3$h(B\\2")
	  (goto-char (match-beginning 2)))

	;; Three or more ASCII spaces between Ethiopic characters
	;;   --> An Ethiopic word separator + (N - 2) ASCII spaces
	(goto-char (point-min))
	(while (re-search-forward "\\(\\ce\\)  \\( *\\ce\\)" nil t)
	  (replace-match "\\1$(3$h(B\\2")
	  (goto-char (match-beginning 2))))))))

;;
;; special icons
;;

;;;###autoload
(defun ethio-input-special-character (arg)
  "Allow the user to input special characters."
  (interactive "*cInput number: 1.$(3%j(B  2.$(3%k(B  3.$(3%l(B  4.$(3%m(B  5.$(3%i(B")
  (cond
   ((= arg ?1)
    (insert "$(3%j(B"))
   ((= arg ?2)
    (insert "$(3%k(B"))
   ((= arg ?3)
    (insert "$(3%l(B"))
   ((= arg ?4)
    (insert "$(3%m(B"))
   ((= arg ?5)
    (insert "$(3%i(B"))
   (t
    (error ""))))

;;
;; TeX support
;;

(defconst ethio-fidel-to-tex-map
 [ "heG"  "huG"  "hiG"  "haG"  "hEG"   "hG"  "hoG"      ""     ;;   0 - 7
   "leG"  "luG"  "liG"  "laG"  "lEG"   "lG"  "loG"  "lWaG"     ;;   8
   "HeG"  "HuG"  "HiG"  "HaG"  "HEG"   "HG"  "HoG"  "HWaG"     ;;  16
   "meG"  "muG"  "miG"  "maG"  "mEG"   "mG"  "moG"  "mWaG"     ;;  24
  "sseG" "ssuG" "ssiG" "ssaG" "ssEG"  "ssG" "ssoG" "ssWaG"     ;;  32
   "reG"  "ruG"  "riG"  "raG"  "rEG"   "rG"  "roG"  "rWaG"     ;;  40
   "seG"  "suG"  "siG"  "saG"  "sEG"   "sG"  "soG"  "sWaG"     ;;  48
   "xeG"  "xuG"  "xiG"  "xaG"  "xEG"   "xG"  "xoG"  "xWaG"     ;;  56
   "qeG"  "quG"  "qiG"  "qaG"  "qEG"   "qG"  "qoG"      ""     ;;  64
  "qWeG"     "" "qWiG" "qWaG" "qWEG"  "qWG"     ""      ""     ;;  72
   "QeG"  "QuG"  "QiG"  "QaG"  "QEG"   "QG"  "QoG"      ""     ;;  80
  "QWeG"     "" "QWiG" "QWaG" "QWEG"  "QWG"     ""      ""     ;;  88
   "beG"  "buG"  "biG"  "baG"  "bEG"   "bG"  "boG"  "bWaG"     ;;  96
   "veG"  "vuG"  "viG"  "vaG"  "vEG"   "vG"  "voG"  "vWaG"     ;; 104
   "teG"  "tuG"  "tiG"  "taG"  "tEG"   "tG"  "toG"  "tWaG"     ;; 112
   "ceG"  "cuG"  "ciG"  "caG"  "cEG"   "cG"  "coG"  "cWaG"     ;; 120
  "hheG" "hhuG" "hhiG" "hhaG" "hhEG"  "hhG" "hhoG"      ""     ;; 128
  "hWeG"     "" "hWiG" "hWaG" "hWEG"  "hWG"     ""      ""     ;; 136
   "neG"  "nuG"  "niG"  "naG"  "nEG"   "nG"  "noG"  "nWaG"     ;; 144
   "NeG"  "NuG"  "NiG"  "NaG"  "NEG"   "NG"  "NoG"  "NWaG"     ;; 152
    "eG"   "uG"   "iG"   "AG"   "EG"   "IG"   "oG"   "eaG"     ;; 160
   "keG"  "kuG"  "kiG"  "kaG"  "kEG"   "kG"  "koG"      ""     ;; 168
  "kWeG"     "" "kWiG" "kWaG" "kWEG"  "kWG"     ""      ""     ;; 176
   "KeG"  "KuG"  "KiG"  "KaG"  "KEG"   "KG"  "KoG"      ""     ;; 184
  "KWeG"     "" "KWiG" "KWaG" "KWEG"  "KWG"     ""      ""     ;; 192
   "weG"  "wuG"  "wiG"  "waG"  "wEG"   "wG"  "woG"      ""     ;; 200
   "eeG"  "uuG"  "iiG"  "aaG"  "EEG"  "IIG"  "ooG"      ""     ;; 208
   "zeG"  "zuG"  "ziG"  "zaG"  "zEG"   "zG"  "zoG"  "zWaG"     ;; 216
   "ZeG"  "ZuG"  "ZiG"  "ZaG"  "ZEG"   "ZG"  "ZoG"  "ZWaG"     ;; 224
   "yeG"  "yuG"  "yiG"  "yaG"  "yEG"   "yG"  "yoG"  "yWaG"     ;; 232
   "deG"  "duG"  "diG"  "daG"  "dEG"   "dG" "doG"   "dWaG"     ;; 240
   "DeG"  "DuG"  "DiG"  "DaG"  "DEG"   "DG"  "DoG"  "DWaG"     ;; 248
   "jeG"  "juG"  "jiG"  "jaG"  "jEG"   "jG"  "joG"  "jWaG"     ;; 256
   "geG"  "guG"  "giG"  "gaG"  "gEG"   "gG"  "goG"     ""      ;; 264
  "gWeG"     "" "gWiG" "gWaG" "gWEG"  "gWG"     ""     ""      ;; 272
   "GeG"  "GuG"  "GiG"  "GaG"  "GEG"   "GG"  "GoG"  "GWaG"     ;; 280
   "TeG"  "TuG"  "TiG"  "TaG"  "TEG"   "TG"  "ToG"  "TWaG"     ;; 288
   "CeG"  "CuG"  "CiG"  "CaG"  "CEG"   "CG"  "CoG"  "CWaG"     ;; 296
   "PeG"  "PuG"  "PiG"  "PaG"  "PEG"   "PG"  "PoG"  "PWaG"     ;; 304
   "SeG"  "SuG"  "SiG"  "SaG"  "SEG"   "SG"  "SoG"  "SWaG"     ;; 312
  "SSeG" "SSuG" "SSiG" "SSaG" "SSEG"  "SSG" "SSoG"      ""     ;; 320
   "feG"  "fuG"  "fiG"  "faG"  "fEG"   "fG"  "foG"  "fWaG"     ;; 328
   "peG"  "puG"  "piG"  "paG"  "pEG"   "pG"  "poG"  "pWaG"     ;; 336
  "mYaG" "rYaG" "fYaG"     ""     ""     ""     ""      ""     ;; 344
      "" "spaceG" "periodG" "commaG"                           ;; 352
  "semicolonG" "colonG" "precolonG" "oldqmarkG"                ;; 356
  "pbreakG" "andG" "huletG" "sostG" "aratG" "amstG" "sadstG" "sabatG"  ;; 360
  "smntG" "zeteNG" "asrG" "heyaG" "selasaG" "arbaG" "hemsaG" "slsaG"   ;; 368
  "sebaG" "semanyaG" "zeTanaG" "metoG" "asrxiG" "" "" ""               ;; 376
  "qqeG" "qquG" "qqiG" "qqaG" "qqEG" "qqG" "qqoG"    ""      ;; 384
  "mWeG" "bWeG" "GWeG" "fWeG" "pWeG"    ""     ""    ""      ;; 392
  "kkeG" "kkuG" "kkiG" "kkaG" "kkEG" "kkG" "kkoG"    ""      ;; 400
  "mWiG" "bWiG" "GWiG" "fWiG" "pWiG"    ""     ""    ""      ;; 408
   "XeG"  "XuG" "GXiG"  "XaG"  "XEG"  "XG"  "XoG"    ""      ;; 416
  "mWEG" "bWEG" "GWEG" "fWEG" "pWEG"    ""     ""    ""      ;; 424
  "ggeG" "gguG" "ggiG" "ggaG" "ggEG" "ggG" "ggoG"    ""      ;; 432
   "mWG" "bWG"   "GWG"  "fWG"  "pWG"    ""     ""    ""      ;; 440
  "ornamentG" "flandG" "iflandG" "africaG"	                 ;; 448
  "iafricaG" "wWeG" "wWiG" "wWaG"                            ;; 452
  "wWEG"  "wWG" "" "slaqG" "dotG" "lquoteG" "rquoteG" "qmarkG" ])  ;; 456

;;
;; To make tex-to-fidel mapping.
;; The following code makes
;;     (get 'ethio-tex-command-he 'ethio-fidel-char)  ==>  ?$(3!!(B
;; etc.
;;

(let ((i 0) str)
  (while (< i (length ethio-fidel-to-tex-map))
    (setq str (aref ethio-fidel-to-tex-map i))
    (if (not (string= str ""))
	(put
	 (intern (concat "ethio-tex-command-" (aref ethio-fidel-to-tex-map i)))
	 'ethio-fidel-char
	 (ethio-ethiocode-to-char i)))
    (setq i (1+ i))))

;;;###autoload
(defun ethio-fidel-to-tex-buffer nil
  "Convert each fidel characters in the current buffer into a fidel-tex command.
Each command is always surrounded by braces."
  (interactive)
  (let ((buffer-read-only nil))

    ;; Isolated gemination marks need special treatement
    (goto-char (point-min))
    (while (search-forward "$(3%s(B" nil t)
      (replace-match "\\geminateG{}" t t))

    ;; First, decompose geminations
    ;; Here we assume that each composed character consists of
    ;; one Ethiopic character and the Ethiopic gemination mark.
    (decompose-region (point-min) (point-max))

    ;; Special treatment for geminated characters
    ;; The geminated character (la'') will be "\geminateG{\la}".
    (goto-char (point-min))
    (while (search-forward "$(3%s(B" nil t)
      (delete-backward-char 1)
      (backward-char 1)
      (insert "\\geminateG")
      (forward-char 1))

    ;; Ethiopic characters to TeX macros
    (goto-char (point-min))
    (while (re-search-forward "\\ce" nil t)
      (insert
       "{\\"
       (aref ethio-fidel-to-tex-map
	     (prog1 (ethio-char-to-ethiocode (preceding-char))
	       (backward-delete-char 1)))
       "}"))
    (goto-char (point-min))
    (set-buffer-modified-p nil)))

;;;###autoload
(defun ethio-tex-to-fidel-buffer nil
  "Convert fidel-tex commands in the current buffer into fidel chars."
  (interactive)
  (let ((buffer-read-only nil)
	(p) (ch))

    ;; Special treatment for gemination
    ;; "\geminateG{\la}" or "\geminateG{{\la}}" will be "\la$(3%s(B"
    ;; "\geminateG{}" remains unchanged.
    (goto-char (point-min))
    (while (re-search-forward "\\\\geminateG{\\(\\\\[a-zA-Z]+\\)}" nil t)
      (replace-match "\\1$(3%s(B"))

    ;; TeX macros to Ethiopic characters
    (goto-char (point-min))
    (while (search-forward "\\" nil t)
      (setq p (point))
      (skip-chars-forward "a-zA-Z")
      (setq ch
	    (get (intern (concat "ethio-tex-command-"
				 (buffer-substring p (point))))
		 'ethio-fidel-char))
      (if ch
	  (progn
	    (delete-region (1- p) (point)) ; don't forget the preceding "\"
	    (if (and (= (preceding-char) ?{)
		     (= (following-char) ?}))
		(progn
		  (backward-delete-char 1)
		  (delete-char 1)))
	    (insert ch))))

    ;; compose geminated characters
    (goto-char (point-min))
    (while (re-search-forward "\\ce$(3%s(B" nil 0)
      (compose-region
       (save-excursion (backward-char 2) (point))
       (point)))

    ;; Now it's time to convert isolated gemination marks.
    (goto-char (point-min))
    (while (search-forward "\\geminateG{}" nil t)
      (replace-match "$(3%s(B"))

    (goto-char (point-min))
    (set-buffer-modified-p nil)))

;;
;; Java support
;;

;;;###autoload
(defun ethio-fidel-to-java-buffer nil
  "Convert Ethiopic characters into the Java escape sequences.

Each escape sequence is of the form \uXXXX, where XXXX is the
character's codepoint (in hex) in Unicode.

If `ethio-java-save-lowercase' is non-nil, use [0-9a-f].
Otherwise, [0-9A-F]."
  (let ((ucode))

    ;; first, decompose geminations
    (decompose-region (point-min) (point-max))

    (goto-char (point-min))
    (while (re-search-forward "\\ce" nil t)
      (setq ucode (+ ?\x1200 (ethio-char-to-ethiocode (preceding-char))))
      (if (> ucode ?\x13bc)
	  (setq ucode (+ ucode 59952)))
      (delete-backward-char 1)
      (if ethio-java-save-lowercase
	  (insert (format "\\u%4x" ucode))
	(insert (upcase (format "\\u%4x" ucode)))))))

;;;###autoload
(defun ethio-java-to-fidel-buffer nil
  "Convert the Java escape sequences into corresponding Ethiopic characters."
  (let ((ucode))
    (goto-char (point-min))
    (while (re-search-forward "\\\\u\\([0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]\\)" nil t)
      (setq ucode
	    (read
	     (concat
	      "?\\x"
	      (buffer-substring (match-beginning 1) (match-end 1)))))
      (cond
       ((and (>= ucode ?\x1200) (<= ucode ?\x13bc))
	(replace-match "")
	(insert (ethio-ethiocode-to-char (- ucode ?\x1200))))
       ((and (>= ucode ?\xfdf1) (<= ucode ?\xfdff))
	(replace-match "")
	(insert (ethio-ethiocode-to-char (- ucode 64560))))
       (t
	nil)))

    ;; gemination
    (goto-char (point-min))
    (while (re-search-forward "\\ce$(3%s(B" nil 0)
      (compose-region
       (save-excursion (backward-char 2) (point))
       (point)))
    ))

;;
;; file I/O hooks
;;

;;;###autoload
(defun ethio-find-file nil
  "Transcribe file content into Ethiopic depending on filename suffix."
  (cond

   ((string-match "\\.sera$" (buffer-file-name))
    (save-excursion
      (ethio-sera-to-fidel-buffer nil 'force)
      (set-buffer-modified-p nil)))

   ((string-match "\\.html$" (buffer-file-name))
    (let ((ethio-sera-being-called-by-w3 t))
      (save-excursion
	(ethio-sera-to-fidel-marker 'force)
	(goto-char (point-min))
	(while (re-search-forward "&[lr]aquote;" nil t)
	  (if (= (char-after (1+ (match-beginning 0))) ?l)
	      (replace-match "$(3%v(B")
	    (replace-match "$(3%w(B")))
	(set-buffer-modified-p nil))))

   ((string-match "\\.tex$" (buffer-file-name))
    (save-excursion
      (ethio-tex-to-fidel-buffer)
      (set-buffer-modified-p nil)))

   ((string-match "\\.java$" (buffer-file-name))
    (save-excursion
      (ethio-java-to-fidel-buffer)
      (set-buffer-modified-p nil)))

   (t
    nil)))

;;;###autoload
(defun ethio-write-file nil
  "Transcribe Ethiopic characters in ASCII depending on the file extension."
  (cond

   ((string-match "\\.sera$" (buffer-file-name))
    (save-excursion
      (ethio-fidel-to-sera-buffer nil 'force)
      (goto-char (point-min))
      (ethio-record-user-preference)
      (set-buffer-modified-p nil)))

   ((string-match "\\.html$" (buffer-file-name))
    (save-excursion
      (let ((ethio-sera-being-called-by-w3 t)
	    (lq (aref ethio-fidel-to-sera-map 461))
	    (rq (aref ethio-fidel-to-sera-map 462)))
	(aset ethio-fidel-to-sera-map 461 "&laquote;")
	(aset ethio-fidel-to-sera-map 462 "&raquote;")
	(ethio-fidel-to-sera-marker 'force)
	(goto-char (point-min))
	(if (search-forward "<sera>" nil t)
	    (ethio-record-user-preference))
	(aset ethio-fidel-to-sera-map 461 lq)
	(aset ethio-fidel-to-sera-map 462 rq)
	(set-buffer-modified-p nil))))

   ((string-match "\\.tex$" (buffer-file-name))
    (save-excursion
      (ethio-fidel-to-tex-buffer)
      (set-buffer-modified-p nil)))

   ((string-match "\\.java$" (buffer-file-name))
    (save-excursion
      (ethio-fidel-to-java-buffer)
      (set-buffer-modified-p nil)))

   (t
    nil)))

(defun ethio-record-user-preference nil
  (if (looking-at "\\\\~\\(tir?\\|amh?\\) ")
      (goto-char (match-end 0))
    (insert (if (ethio-prefer-amharic-p) "\\~amh " "\\~tir ")))
  (insert (if ethio-use-colon-for-colon "\\~-: " "\\~`: ")
	  (if ethio-use-three-dot-question "\\~`| " "\\~`? ")))

;;
;; Ethiopic word separator vs. ASCII space
;;

(defvar ethio-prefer-ascii-space t)
(make-variable-buffer-local 'ethio-prefer-ascii-space)

(defun ethio-toggle-space nil
  "Toggle ASCII space and Ethiopic separator for keyboard input."
  (interactive)
  (setq ethio-prefer-ascii-space
	(not ethio-prefer-ascii-space))
  (if (equal current-input-method "ethiopic")
      (setq current-input-method-title (quail-title)))
  (force-mode-line-update))

(defun ethio-insert-space (arg)
  "Insert ASCII spaces or Ethiopic word separators depending on context.

If the current word separator (indicated in mode-line) is the ASCII space,
insert an ASCII space.  With ARG, insert that many ASCII spaces.

If the current word separator is the colon-like Ethiopic word
separator and the point is preceded by `an Ethiopic punctuation mark
followed by zero or more ASCII spaces', then insert also an ASCII
space.  With ARG, insert that many ASCII spaces.

Otherwise, insert a colon-like Ethiopic word separator.  With ARG, insert that
many Ethiopic word separators."

  (interactive "*p")
  (cond
   (ethio-prefer-ascii-space
    (insert-char 32 arg))
   ((save-excursion
      (skip-chars-backward " ")
      (memq (preceding-char)
	    '(?$(3$h(B ?$(3$i(B ?$(3$j(B ?$(3$k(B ?$(3$l(B ?$(3$m(B ?$(3$n(B ?$(3$o(B ?$(3%t(B ?$(3%u(B ?$(3%v(B ?$(3%w(B ?$(3%x(B)))
    (insert-char 32 arg))
   (t
    (insert-char ?$(3$h(B arg))))

(defun ethio-insert-ethio-space (arg)
  "Insert the Ethiopic word delimiter (the colon-like character).
With ARG, insert that many delimiters."
  (interactive "*p")
  (insert-char ?$(3$h(B arg))

;;
;; Ethiopic punctuation vs. ASCII punctuation
;;

(defvar ethio-prefer-ascii-punctuation nil)
(make-variable-buffer-local 'ethio-prefer-ascii-punctuation)

(defun ethio-toggle-punctuation nil
  "Toggle Ethiopic punctuations and ASCII punctuations for keyboard input."
  (interactive)
  (setq ethio-prefer-ascii-punctuation
	(not ethio-prefer-ascii-punctuation))
  (let* ((keys '("." ".." "..." "," ",," ";" ";;" ":" "::" ":::" "*" "**"))
	 (puncs
	  (if ethio-prefer-ascii-punctuation
	      '(?. [".."] ["..."] ?, [",,"] ?\; [";;"] ?: ["::"] [":::"] ?* ["**"])
	    '(?$(3$i(B ?$(3%u(B ?. ?$(3$j(B ?, ?$(3$k(B ?\; ?$(3$h(B ?$(3$i(B ?: ?* ?$(3$o(B))))
    (while keys
      (quail-defrule (car keys) (car puncs) "ethiopic")
      (setq keys (cdr keys)
	    puncs (cdr puncs)))
    (if (equal current-input-method "ethiopic")
	(setq current-input-method-title (quail-title)))
    (force-mode-line-update)))

;;
;; Gemination
;;

(defun ethio-gemination nil
  "Compose the character before the point with the Ethiopic gemination mark.
If the character is already composed, decompose it and remove the gemination
mark."
  (interactive "*")
  (cond
   ((eq (char-charset (preceding-char)) 'ethiopic)
    (insert "$(3%s(B")
    (compose-region
     (save-excursion (backward-char 2) (point))
     (point))
    (forward-char 1))
   ((eq (char-charset (preceding-char)) 'leading-code-composition)
    (decompose-region
     (save-excursion (backward-char 1) (point))
     (point))
    (delete-backward-char 1))
   (t
    (error ""))))

;;
(provide 'ethio-util)

;;; arch-tag: c8feb3d6-39bf-4b0a-b6ef-26f03fbc8140
;;; ethio-util.el ends here
