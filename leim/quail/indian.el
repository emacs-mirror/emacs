;;; indian.el --- Quail packages for inputting Indian

;; Copyright (C) 2000, 2001 Free Software Foundation, Inc.

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

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

;;; update function

;; CONTROL-FLAG is integer (n)
;;     quail-current-key :: keyboard input.
;;                          Only first n can be translated.
;;     quail-current-string :: corresonding string.  Translated when last
;;                             time CONTROL-FLAG is nil.
;;     todo :: (1) put last (len-n) char to unrread-command-event.
;;             (2) put translated string to  quail-current-string.
;;
;; CONTROL-FLAG is t (terminate) or nil (proceed the translation)
;;     quail-current-key :: keyboard input.
;;     quail-current-string :: corresponding string.  Created by database.
;;     todo :: (1) put modified translated string to quail-current-string.

(defun quail-indian-update-translation (control-flag)
  ;;(message "input control-flag=%s, string=%s, key=%s"
  ;;         control-flag quail-current-str quail-current-key)
  ;; make quail-current-str string when possible.
  (if (char-valid-p quail-current-str)
      (setq quail-current-str (char-to-string quail-current-str)))
  ;; reset quail-indian-update-preceding-char if it's initial.
  (if (= (overlay-start quail-overlay) (overlay-end quail-overlay))
      (setq quail-indian-update-preceding-char nil))
  ;; set quial-indian-update-preceding-char if appropriate.
  (let* (prec-char-position composition-regexp
         prec-char-str candidate-str match-pos match-end)
    (when (and quail-current-str
               (null input-method-use-echo-area)
               (null input-method-exit-on-first-char)
               (setq prec-char-position
                     (quail-indian-preceding-char-position
                      (overlay-start quail-overlay)))
               (setq composition-regexp
                     (if prec-char-position
                         (caar (elt composition-function-table
                                    (char-after prec-char-position)))))
               ;; (null quail-indian-update-preceding-char)
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
  ;; make quail-current-str string when possible.
  (if (null quail-current-str)
      (setq quail-current-str ""))
  ;; set quail-current-str unless control-flag is number.
  (if (numberp control-flag)
      (setq quail-indian-update-preceding-char nil
            quail-current-str
            (if (equal quail-current-str "")
                (substring quail-current-key 0 control-flag)
              (indian-compose-string quail-current-str))
            unread-command-events
            (string-to-list
             (substring quail-current-key control-flag)))
    (if quail-indian-update-preceding-char
        (setq quail-current-str
              (concat quail-indian-update-preceding-char
                      quail-current-str)))
    (setq quail-current-str
          (indian-compose-string quail-current-str)))
  (when (eq t control-flag)
    ;; reset preceding-char if translation is terminated.
    (setq quail-indian-update-preceding-char nil))
    ;; compose to previous char if it looks possible.
  ;;(message "  out control-flag=%s, string=%s, key=%s"
  ;;         control-flag quail-current-str quail-current-key)
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

;;; indian.el ends here
