;;; indian.el --- Quail packages for inputting Indian

;; Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007
;;   Free Software Foundation, Inc.

;; Author: KAWABATA, Taichi <kawabata@m17n.org>

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; History:

;; 2000.12.12
;; Totally re-written from devanagari.el to handle multiple Indian Scripts.

;;; Code:

(require 'quail)
(require 'devan-util)
(require 'ind-util)

(defun quail-indian-preceding-char-position (position)
  "Return the position of preceding composite character."
  (let (prec-composed)
    (if (char-valid-p (char-before position)) ;; range o.k.
        (if (setq prec-composed (find-composition (1- position)))
            (car prec-composed)
          (1- position))
      nil)))

(defvar quail-indian-update-preceding-char nil)
(make-variable-frame-local 'quail-indian-update-preceding-char)

;; Input value ::
;;   CONTROL-FLAG is integer `n'
;;     quail-current-key :: keyboard input.
;;                          Only first `n' can be translated.
;;     quail-current-str :: corresonding string. 
;;     jobs :: (1) put last (len-n) char to unrread-command-event.
;;             (2) put translated string to  quail-current-str.
;;
;;   CONTROL-FLAG is t (terminate) or nil (proceed the translation)
;;     quail-current-key :: keyboard input.
;;     quail-current-str :: corresponding string.
;;     jobs :: (1) put modified translated string to quail-current-str.
;;
;; When non-nil value is returned from quail-translation-update-function, 
;; the quail-current-str is split to characters and put into event queue, 
;; with `compose-last-char' event with composition info at the end.

(defun quail-indian-update-translation (control-flag)
  ;; make quail-current-str string when possible.
  (if (char-valid-p quail-current-str)
      (setq quail-current-str (char-to-string quail-current-str)))
  ;(message "\n input control-flag=%s, str=%s, key=%s q-ind-upd-prec-char=%s"
  ;         control-flag quail-current-str quail-current-key
  ;         quail-indian-update-preceding-char)
  ;; reset quail-indian-update-preceding-char if it's initial.
  (if (= (overlay-start quail-overlay) (overlay-end quail-overlay))
      (setq quail-indian-update-preceding-char nil))
  ;; Check the preceding character of the quail region.  If the
  ;; preceding character can be composed with quail-current-str, then
  ;; grab that preceding character into the quail-current-str and
  ;; remove that char from the region.  
  (let* (prec-char-position composition-regexp
         prec-char-str candidate-str match-pos match-end)
    (when (and quail-current-str
               (null quail-indian-update-preceding-char)
               (null input-method-use-echo-area)
               (null input-method-exit-on-first-char)
               (setq prec-char-position
                     (quail-indian-preceding-char-position
                      (overlay-start quail-overlay)))
               (setq composition-regexp
                     (if prec-char-position
                         (caar (elt composition-function-table
                                    (char-after prec-char-position)))))
               (setq prec-char-str
                     (buffer-substring prec-char-position
                                       (overlay-start quail-overlay))
                     candidate-str (concat prec-char-str quail-current-str)
                     match-pos (string-match composition-regexp candidate-str)
                     match-end (match-end 0))
               (> match-end (length prec-char-str)))
      (setq quail-indian-update-preceding-char prec-char-str)
      (delete-region prec-char-position
                     (overlay-start quail-overlay))))
  (setq quail-current-str 
        (indian-compose-string
         (concat quail-indian-update-preceding-char 
                 quail-current-str)))
  (if (numberp control-flag)
      (setq unread-command-events
            (string-to-list
             (substring quail-current-key control-flag))))
  (when control-flag
    (setq quail-indian-update-preceding-char nil))
  ;(message "output control-flag=%s, str=%s, key=%s q-ind-upd-prec-char=%s"
  ;         control-flag quail-current-str quail-current-key
  ;         quail-indian-update-preceding-char)
  control-flag)

;;;
;;; Input by transliteration
;;;

(defun quail-define-indian-trans-package (hashtbls pkgname
						   lang title doc)
  (funcall 'quail-define-package pkgname lang title t doc
	   nil nil nil nil nil nil t nil
	   'quail-indian-update-translation)
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

(if nil
    (quail-define-package "malayalam-itrans" "Malayalam" "MlmIT" t "Malayalam ITRANS"))
(quail-define-indian-trans-package
 indian-mlm-itrans-v5-hash "malayalam-itrans" "Malayalam" "MlmIT"
 "Malayalam transliteration by ITRANS method.")

(if nil
    (quail-define-package "tamil-itrans" "Tamil" "TmlIT" t "Tamil ITRANS"))
(quail-define-indian-trans-package
 indian-tml-itrans-v5-hash "tamil-itrans" "Tamil" "TmlIT"
 "Tamil transliteration by ITRANS method.")


;;;
;;; Input by Inscript
;;;

(defun quail-indian-flatten-list (lst)
  "Flatten the nested LIST so that there would be no innner list."
  (if (listp lst)
      (apply 'append (mapcar 'quail-indian-flatten-list lst))
    (list lst)))

(defun quail-define-inscript-package (char-table key-table pkgname lang title
						 docstring)
  (setq char-table (quail-indian-flatten-list char-table))
  (setq key-table (quail-indian-flatten-list key-table))
  (funcall 'quail-define-package pkgname lang title nil docstring
	   nil nil nil nil nil nil nil nil
	   'quail-indian-update-translation)
  (dolist (key key-table)
    (let ((val (pop char-table)))
      (if (and key val)
	  (quail-defrule
	    (if (char-valid-p key) (char-to-string key) key)
	    (if (stringp val) (vector val) val))))))

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
 indian-dev-base-table inscript-dev-keytable
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
 indian-mlm-base-table inscript-dev-keytable
 "malayalam-inscript" "Malayalam" "MlmIS"
 "Malayalam keyboard Inscript.")

(if nil
    (quail-define-package "tamil-inscript" "Tamil" "TmlIS" t "Tamil keyboard Inscript"))
(quail-define-inscript-package
 indian-tml-base-table inscript-dev-keytable
 "tamil-inscript" "Tamil" "TmlIS"
 "Tamil keyboard Inscript.")

;;; arch-tag: 9e5a621e-f7d5-4fce-9543-0a51b407c940
;;; indian.el ends here
