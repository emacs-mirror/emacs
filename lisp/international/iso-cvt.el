;; iso-cvt.el -- translate to ISO 8859-1 from/to net/TeX conventions
;; Copyright © 1993 Free Software Foundation, Inc.
;; Was formerly called gm-lingo.el.

;; Author: Michael Gschwind <mike@vlsivie.tuwien.ac.at>
;; Keywords: tex, iso, latin, i18n

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

; Calling `iso-german' will turn the net convention f or umlauts ("a etc.) 
; into ISO latin umlaute for easy reading.
; hooks change TeX files to Latin-1 for editing and back to TeX sequences 
; for calling TeX. An alternative is a TeX style that handles 
; 8 bit ISO files (available on ftp.vlsivie.tuwien.ac.at in /pub/8bit) 
; - but these files are difficult to transmit ... so while the net is  
; still @ 7 bit this may be useful

; to do: translate buffer when displaying from GNUS, 
; use function 'german which does the Right Thing
;
; to do: use more general regular expressions for (g)tex2iso tables, to 
; cover more cases for translation.

;;; Code:

(provide 'iso-cvt)

(defvar iso-spanish-trans-tab
  '(
    ("~n" "ñ")
    ("\([a-zA-Z]\)#" "\\1ñ")
    ("~N" "Ñ")
    ("\\([-a-zA-Z\"`]\\)\"u" "\\1ü")
    ("\\([-a-zA-Z\"`]\\)\"U" "\\1Ü")
    ("\\([-a-zA-Z]\\)'o" "\\1ó")
    ("\\([-a-zA-Z]\\)'O" "\\Ó")
    ("\\([-a-zA-Z]\\)'e" "\\1é")
    ("\\([-a-zA-Z]\\)'E" "\\1É")
    ("\\([-a-zA-Z]\\)'a" "\\1á")
    ("\\([-a-zA-Z]\\)'A" "\\1A")
    ("\\([-a-zA-Z]\\)'i" "\\1í")
    ("\\([-a-zA-Z]\\)'I" "\\1Í")
    )
  "Spanish translation table.")

(defun iso-translate-conventions (trans-tab)
  "Use the translation table argument to translate the current buffer."
  (interactive)
  (save-excursion
    (widen)
    (goto-char (point-min))
    (let ((work-tab trans-tab)
	  (buffer-read-only nil))
      (while work-tab
	(save-excursion
	  (let ((trans-this (car work-tab)))
	    (while (re-search-forward (car trans-this) nil t)
	      (replace-match (car (cdr trans-this)) nil nil)))
	  (setq work-tab (cdr work-tab)))))))

(defun iso-spanish ()
  "Translate net conventions for Spanish to ISO 8859-1."
  (interactive)
  (iso-translate-conventions iso-spanish-trans-tab))

(defvar iso-aggressive-german-trans-tab
  '(
    ("\"a" "ä")
    ("\"A" "Ä")
    ("\"o" "ö")
    ("\"O" "Ö")
    ("\"u" "ü")
    ("\"U" "Ü")
    ("\"s" "ß")
    ("\\\\3" "ß")
    )
  "German translation table. 
This table uses an aggressive translation approach and may translate 
erroneously translate too much.")

(defvar iso-conservative-german-trans-tab
  '(
    ("\\([-a-zA-Z\"`]\\)\"a" "\\1ä")
    ("\\([-a-zA-Z\"`]\\)\"A" "\\1Ä")
    ("\\([-a-zA-Z\"`]\\)\"o" "\\1ö")
    ("\\([-a-zA-Z\"`]\\)\"O" "\\1Ö")
    ("\\([-a-zA-Z\"`]\\)\"u" "\\1ü")
    ("\\([-a-zA-Z\"`]\\)\"U" "\\1Ü")
    ("\\([-a-zA-Z\"`]\\)\"s" "\\1ß")
    ("\\([-a-zA-Z\"`]\\)\\\\3" "\\1ß")
    )
  "German translation table.
This table uses a conservative translation approach and may translate too 
little.")


(defvar iso-german-trans-tab iso-aggressive-german-trans-tab 
  "Currently active translation table for German.")

(defun iso-german ()
 "Translate net conventions for German to ISO 8859-1."
 (interactive)
 (iso-translate-conventions iso-german-trans-tab))
 
(defvar iso-iso2tex-trans-tab
  '(
    ("ä" "{\\\\\"a}")
    ("à" "{\\\\`a}")
    ("á" "{\\\\'a}")
    ("ã" "{\\\\~a}")
    ("â" "{\\\\^a}")
    ("ë" "{\\\\\"e}")
    ("è" "{\\\\`e}")
    ("é" "{\\\\'e}")
    ("ê" "{\\\\^e}")
    ("ï" "{\\\\\"\\\\i}")
    ("ì" "{\\\\`\\\\i}")
    ("í" "{\\\\'\\\\i}")
    ("î" "{\\\\^\\\\i}")
    ("ö" "{\\\\\"o}")
    ("ò" "{\\\\`o}")
    ("ó" "{\\\\'o}")
    ("õ" "{\\\\~o}")
    ("ô" "{\\\\^o}")
    ("ü" "{\\\\\"u}")
    ("ù" "{\\\\`u}")
    ("ú" "{\\\\'u}")
    ("û" "{\\\\^u}")
    ("Ä" "{\\\\\"A}")
    ("À" "{\\\\`A}")
    ("Á" "{\\\\'A}")
    ("Ã" "{\\\\~A}")
    ("Â" "{\\\\^A}")
    ("Ë" "{\\\\\"E}")
    ("È" "{\\\\`E}")
    ("É" "{\\\\'E}")
    ("Ê" "{\\\\^E}")
    ("Ï" "{\\\\\"I}")
    ("Ì" "{\\\\`I}")
    ("Í" "{\\\\'I}")
    ("Î" "{\\\\^I}")
    ("Ö" "{\\\\\"O}")
    ("Ò" "{\\\\`O}")
    ("Ó" "{\\\\'O}")
    ("Õ" "{\\\\~O}")
    ("Ô" "{\\\\^O}")
    ("Ü" "{\\\\\"U}")
    ("Ù" "{\\\\`U}")
    ("Ú" "{\\\\'U}")
    ("Û" "{\\\\^U}")
    ("ñ" "{\\\\~n}")
    ("Ñ" "{\\\\~N}")
    ("ç" "{\\\\c c}")
    ("Ç" "{\\\\c C}")
    ("ß" "{\\\\ss}")
    ("¿" "{?`}")
    ("¡" "{!`}")
    )
  "Translation table for translating ISO 8859-1 characters to TeX sequences.")




(defun iso-iso2tex ()
 "Translate ISO 8859-1 characters to TeX sequences."
 (interactive)
 (iso-translate-conventions iso-iso2tex-trans-tab))


(defvar iso-tex2iso-trans-tab
  '(
    ("{\\\\\"a}" "ä")
    ("{\\\\`a}" "à")
    ("{\\\\'a}" "á")
    ("{\\\\~a}" "ã")
    ("{\\\\^a}" "â")
    ("{\\\\\"e}" "ë")
    ("{\\\\`e}" "è")
    ("{\\\\'e}" "é")
    ("{\\\\^e}" "ê")
    ("{\\\\\"\\\\i}" "ï")
    ("{\\\\`\\\\i}" "ì")
    ("{\\\\'\\\\i}" "í")
    ("{\\\\^\\\\i}" "î")
    ("{\\\\\"i}" "ï")
    ("{\\\\`i}" "ì")
    ("{\\\\'i}" "í")
    ("{\\\\^i}" "î")
    ("{\\\\\"o}" "ö")
    ("{\\\\`o}" "ò")
    ("{\\\\'o}" "ó")
    ("{\\\\~o}" "õ")
    ("{\\\\^o}" "ô")
    ("{\\\\\"u}" "ü")
    ("{\\\\`u}" "ù")
    ("{\\\\'u}" "ú")
    ("{\\\\^u}" "û")
    ("{\\\\\"A}" "Ä")
    ("{\\\\`A}" "À")
    ("{\\\\'A}" "Á")
    ("{\\\\~A}" "Ã")
    ("{\\\\^A}" "Â")
    ("{\\\\\"E}" "Ë")
    ("{\\\\`E}" "È")
    ("{\\\\'E}" "É")
    ("{\\\\^E}" "Ê")
    ("{\\\\\"I}" "Ï")
    ("{\\\\`I}" "Ì")
    ("{\\\\'I}" "Í")
    ("{\\\\^I}" "Î")
    ("{\\\\\"O}" "Ö")
    ("{\\\\`O}" "Ò")
    ("{\\\\'O}" "Ó")
    ("{\\\\~O}" "Õ")
    ("{\\\\^O}" "Ô")
    ("{\\\\\"U}" "Ü")
    ("{\\\\`U}" "Ù")
    ("{\\\\'U}" "Ú")
    ("{\\\\^U}" "Û")
    ("{\\\\~n}" "ñ")
    ("{\\\\~N}" "Ñ")
    ("{\\\\c c}" "ç")
    ("{\\\\c C}" "Ç")
    ("\\\\\"{a}" "ä")
    ("\\\\`{a}" "à")
    ("\\\\'{a}" "á")
    ("\\\\~{a}" "ã")
    ("\\\\^{a}" "â")
    ("\\\\\"{e}" "ë")
    ("\\\\`{e}" "è")
    ("\\\\'{e}" "é")
    ("\\\\^{e}" "ê")
    ("\\\\\"{\\\\i}" "ï")
    ("\\\\`{\\\\i}" "ì")
    ("\\\\'{\\\\i}" "í")
    ("\\\\^{\\\\i}" "î")
    ("\\\\\"{i}" "ï")
    ("\\\\`{i}" "ì")
    ("\\\\'{i}" "í")
    ("\\\\^{i}" "î")
    ("\\\\\"{o}" "ö")
    ("\\\\`{o}" "ò")
    ("\\\\'{o}" "ó")
    ("\\\\~{o}" "õ")
    ("\\\\^{o}" "ô")
    ("\\\\\"{u}" "ü")
    ("\\\\`{u}" "ù")
    ("\\\\'{u}" "ú")
    ("\\\\^{u}" "û")
    ("\\\\\"{A}" "Ä")
    ("\\\\`{A}" "À")
    ("\\\\'{A}" "Á")
    ("\\\\~{A}" "Ã")
    ("\\\\^{A}" "Â")
    ("\\\\\"{E}" "Ë")
    ("\\\\`{E}" "È")
    ("\\\\'{E}" "É")
    ("\\\\^{E}" "Ê")
    ("\\\\\"{I}" "Ï")
    ("\\\\`{I}" "Ì")
    ("\\\\'{I}" "Í")
    ("\\\\^{I}" "Î")
    ("\\\\\"{O}" "Ö")
    ("\\\\`{O}" "Ò")
    ("\\\\'{O}" "Ó")
    ("\\\\~{O}" "Õ")
    ("\\\\^{O}" "Ô")
    ("\\\\\"{U}" "Ü")
    ("\\\\`{U}" "Ù")
    ("\\\\'{U}" "Ú")
    ("\\\\^{U}" "Û")
    ("\\\\~{n}" "ñ")
    ("\\\\~{N}" "Ñ")
    ("\\\\c{c}" "ç")
    ("\\\\c{C}" "Ç")
    ("{\\\\ss}" "ß")
    ("?`" "¿")
    ("!`" "¡")
    ("{?`}" "¿")
    ("{!`}" "¡")
    )
  "Translation table for translating TeX sequences to ISO 8859-1 characters. 
This table is not exhaustive (and due to TeX's power can never be). It only
contains commonly used sequences.")

(defun iso-tex2iso ()
 "Translate TeX sequences to ISO 8859-1 characters."
 (interactive)
 (iso-translate-conventions iso-tex2iso-trans-tab))

(defvar iso-gtex2iso-trans-tab
  '(
    ("\"a" "ä")
    ("\"A" "Ä")
    ("\"o" "ö")
    ("\"O" "Ö")
    ("\"u" "ü")
    ("\"U" "Ü")
    ("\"s" "ß")
    ("\\\\3" "ß")
    ("{\\\\\"a}" "ä")
    ("{\\\\`a}" "à")
    ("{\\\\'a}" "á")
    ("{\\\\~a}" "ã")
    ("{\\\\^a}" "â")
    ("{\\\\\"e}" "ë")
    ("{\\\\`e}" "è")
    ("{\\\\'e}" "é")
    ("{\\\\^e}" "ê")
    ("{\\\\\"\\\\i}" "ï")
    ("{\\\\`\\\\i}" "ì")
    ("{\\\\'\\\\i}" "í")
    ("{\\\\^\\\\i}" "î")
    ("{\\\\\"i}" "ï")
    ("{\\\\`i}" "ì")
    ("{\\\\'i}" "í")
    ("{\\\\^i}" "î")
    ("{\\\\\"o}" "ö")
    ("{\\\\`o}" "ò")
    ("{\\\\'o}" "ó")
    ("{\\\\~o}" "õ")
    ("{\\\\^o}" "ô")
    ("{\\\\\"u}" "ü")
    ("{\\\\`u}" "ù")
    ("{\\\\'u}" "ú")
    ("{\\\\^u}" "û")
    ("{\\\\\"A}" "Ä")
    ("{\\\\`A}" "À")
    ("{\\\\'A}" "Á")
    ("{\\\\~A}" "Ã")
    ("{\\\\^A}" "Â")
    ("{\\\\\"E}" "Ë")
    ("{\\\\`E}" "È")
    ("{\\\\'E}" "É")
    ("{\\\\^E}" "Ê")
    ("{\\\\\"I}" "Ï")
    ("{\\\\`I}" "Ì")
    ("{\\\\'I}" "Í")
    ("{\\\\^I}" "Î")
    ("{\\\\\"O}" "Ö")
    ("{\\\\`O}" "Ò")
    ("{\\\\'O}" "Ó")
    ("{\\\\~O}" "Õ")
    ("{\\\\^O}" "Ô")
    ("{\\\\\"U}" "Ü")
    ("{\\\\`U}" "Ù")
    ("{\\\\'U}" "Ú")
    ("{\\\\^U}" "Û")
    ("{\\\\~n}" "ñ")
    ("{\\\\~N}" "Ñ")
    ("{\\\\c c}" "ç")
    ("{\\\\c C}" "Ç")
    ("\\\\\"{a}" "ä")
    ("\\\\`{a}" "à")
    ("\\\\'{a}" "á")
    ("\\\\~{a}" "ã")
    ("\\\\^{a}" "â")
    ("\\\\\"{e}" "ë")
    ("\\\\`{e}" "è")
    ("\\\\'{e}" "é")
    ("\\\\^{e}" "ê")
    ("\\\\\"{\\\\i}" "ï")
    ("\\\\`{\\\\i}" "ì")
    ("\\\\'{\\\\i}" "í")
    ("\\\\^{\\\\i}" "î")
    ("\\\\\"{i}" "ï")
    ("\\\\`{i}" "ì")
    ("\\\\'{i}" "í")
    ("\\\\^{i}" "î")
    ("\\\\\"{o}" "ö")
    ("\\\\`{o}" "ò")
    ("\\\\'{o}" "ó")
    ("\\\\~{o}" "õ")
    ("\\\\^{o}" "ô")
    ("\\\\\"{u}" "ü")
    ("\\\\`{u}" "ù")
    ("\\\\'{u}" "ú")
    ("\\\\^{u}" "û")
    ("\\\\\"{A}" "Ä")
    ("\\\\`{A}" "À")
    ("\\\\'{A}" "Á")
    ("\\\\~{A}" "Ã")
    ("\\\\^{A}" "Â")
    ("\\\\\"{E}" "Ë")
    ("\\\\`{E}" "È")
    ("\\\\'{E}" "É")
    ("\\\\^{E}" "Ê")
    ("\\\\\"{I}" "Ï")
    ("\\\\`{I}" "Ì")
    ("\\\\'{I}" "Í")
    ("\\\\^{I}" "Î")
    ("\\\\\"{O}" "Ö")
    ("\\\\`{O}" "Ò")
    ("\\\\'{O}" "Ó")
    ("\\\\~{O}" "Õ")
    ("\\\\^{O}" "Ô")
    ("\\\\\"{U}" "Ü")
    ("\\\\`{U}" "Ù")
    ("\\\\'{U}" "Ú")
    ("\\\\^{U}" "Û")
    ("\\\\~{n}" "ñ")
    ("\\\\~{N}" "Ñ")
    ("\\\\c{c}" "ç")
    ("\\\\c{C}" "Ç")
    ("{\\\\ss}" "ß")
    ("?`" "¿")
    ("!`" "¡")
    ("{?`}" "¿")
    ("{!`}" "¡")
    )
  "Translation table for translating German TeX sequences to ISO 8859-1.
This table is not exhaustive (and due to TeX's power can never be).  It only
contains commonly used sequences.")

(defvar iso-iso2gtex-trans-tab
  '(
    ("ä" "\"a")
    ("à" "{\\\\`a}")
    ("á" "{\\\\'a}")
    ("ã" "{\\\\~a}")
    ("â" "{\\\\^a}")
    ("ë" "{\\\\\"e}")
    ("è" "{\\\\`e}")
    ("é" "{\\\\'e}")
    ("ê" "{\\\\^e}")
    ("ï" "{\\\\\"\\\\i}")
    ("ì" "{\\\\`\\\\i}")
    ("í" "{\\\\'\\\\i}")
    ("î" "{\\\\^\\\\i}")
    ("ö" "\"o")
    ("ò" "{\\\\`o}")
    ("ó" "{\\\\'o}")
    ("õ" "{\\\\~o}")
    ("ô" "{\\\\^o}")
    ("ü" "\"u")
    ("ù" "{\\\\`u}")
    ("ú" "{\\\\'u}")
    ("û" "{\\\\^u}")
    ("Ä" "\"A")
    ("À" "{\\\\`A}")
    ("Á" "{\\\\'A}")
    ("Ã" "{\\\\~A}")
    ("Â" "{\\\\^A}")
    ("Ë" "{\\\\\"E}")
    ("È" "{\\\\`E}")
    ("É" "{\\\\'E}")
    ("Ê" "{\\\\^E}")
    ("Ï" "{\\\\\"I}")
    ("Ì" "{\\\\`I}")
    ("Í" "{\\\\'I}")
    ("Î" "{\\\\^I}")
    ("Ö" "\"O")
    ("Ò" "{\\\\`O}")
    ("Ó" "{\\\\'O}")
    ("Õ" "{\\\\~O}")
    ("Ô" "{\\\\^O}")
    ("Ü" "\"U")
    ("Ù" "{\\\\`U}")
    ("Ú" "{\\\\'U}")
    ("Û" "{\\\\^U}")
    ("ñ" "{\\\\~n}")
    ("Ñ" "{\\\\~N}")
    ("ç" "{\\\\c c}")
    ("Ç" "{\\\\c C}")
    ("ß" "\"s")
    ("¿" "{?`}")
    ("¡" "{!`}")
    )
  "Translation table for translating ISO 8859-1 characters to German TeX.")

(defun iso-gtex2iso ()
 "Translate German TeX sequences to ISO 8859-1 characters."
 (interactive)
 (iso-translate-conventions iso-gtex2iso-trans-tab))


(defun iso-iso2gtex ()
 "Translate ISO 8859-1 characters to German TeX sequences."
 (interactive)
 (iso-translate-conventions iso-iso2gtex-trans-tab))


(defun iso-german-tex-p ()
 "Check if tex buffer is German LaTeX."
 (save-excursion
   (widen)
   (goto-char (point-min))
   (re-search-forward "\\\\documentstyle\\[.*german.*\\]" nil t)))

(defun iso-fix-iso2tex ()
  "Turn ISO 8859-1 (aka. ISO Latin-1) buffer into TeX sequences.
If German TeX is used, German TeX sequences are generated."
  (if (or (equal major-mode 'latex-mode)
	  (equal major-mode 'LaTeX-mode)) ; AucTeX wants this
      (if (iso-german-tex-p)
	  (iso-iso2gtex)
	(iso-iso2tex)))
  (if (or (equal major-mode 'tex-mode)
	  (equal major-mode 'TeX-mode)) ; AucTeX wants this
      (iso-iso2tex)))

(defun iso-fix-tex2iso ()
  "Turn TeX sequences into ISO 8859-1 (aka. ISO Latin-1) characters.
This function recognices German TeX buffers."
  (if (or (equal major-mode 'latex-mode)
	  (equal major-mode 'Latex-mode)) ; AucTeX wants this
      (if (iso-german-tex-p)
	  (iso-gtex2iso)
	(iso-tex2iso)))
  (if (or (equal major-mode 'tex-mode)
	  (equal major-mode 'TeX-mode))  ;; AucTeX wants this
      (iso-tex2iso)))

(defun iso-cvt-ffh ()
  "find-file-hook for iso-cvt-cvt.el."
  (iso-fix-tex2iso)
  (set-buffer-modified-p nil))

(defun iso-cvt-wfh ()
  "write file hook for iso-cvt-cvt.el."
  (iso-fix-iso2tex))

(defun iso-cvt-ash ()
  "after save hook for iso-cvt-cvt.el."
  (iso-fix-tex2iso)
  (set-buffer-modified-p nil))

(add-hook 'find-file-hooks 'iso-cvt-ffh)
(add-hook 'write-file-hooks 'iso-cvt-wfh)
(add-hook 'after-save-hook 'iso-cvt-ash)

;;; iso-cvt.el ends here
