;;; iso-transl.el --- keyboard input definitions for ISO 8859-1  -*- coding: iso-8859-1 -*-

;; Copyright (C) 1987, 1993, 1994, 1995, 2001 Free Software Foundation, Inc.

;; Author: Howard Gayle
;; Maintainer: FSF
;; Keywords: i18n

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

;; Loading this package defines three ways of entering the non-ASCII
;; printable characters with codes above 127: the prefix C-x 8, or the
;; Alt key, or a dead accent key.  For example, you can enter uppercase
;; A-umlaut as `C-x 8 " A' or `Alt-" A' (if you have an Alt key) or
;; `umlaut A' (if you have an umlaut/diaeresis key).

;; C-x 8 is set up to autoload this package,
;; but Alt keys and dead accent keys are only defined
;; once you have loaded the package.  It is nontrivial
;; to make all of the Alt keys autoload, and it is not clear
;; that the dead accent keys SHOULD autoload this package.

;;; Code:

;;; Provide some binding for startup:
;;;###autoload (or key-translation-map (setq key-translation-map (make-sparse-keymap)))
;;;###autoload (define-key key-translation-map "\C-x8" 'iso-transl-ctl-x-8-map)
;;;###autoload (autoload 'iso-transl-ctl-x-8-map "iso-transl" "Keymap for C-x 8 prefix." t 'keymap)
  
(defvar iso-transl-dead-key-alist
  '((?\' . mute-acute)
    (?\` . mute-grave)
    (?\" . mute-diaeresis)
    (?^ . mute-asciicircum)
    (?\~ . mute-asciitilde)
    (?\' . dead-acute)
    (?\` . dead-grave)
    (?\" . dead-diaeresis)
    (?^ . dead-asciicircum)
    (?\~ . dead-asciitilde)
    (?^ . dead-circum)
    (?^ . dead-circumflex)
    (?\~ . dead-tilde)
    ;; Someone reports that these keys don't work if shifted.
    ;; This might fix it--no word yet.
    (?\' . S-dead-acute)
    (?\` . S-dead-grave)
    (?\" . S-dead-diaeresis)
    (?^ . S-dead-asciicircum)
    (?\~ . S-dead-asciitilde)
    (?^ . S-dead-circum)
    (?^ . S-dead-circumflex)
    (?\~ . S-dead-tilde))
  "Mapping of ASCII characters to their corresponding dead-key symbols.")

;; The two-character mnemonics are intended to be available in all languages.
;; The ones beginning with `*' have one-character synonyms, but a
;; language-specific table might override the short form for its own use.

;; When a translation is non-ASCII, we use a symbol name
;; whose "function definition" is a translation, rather than
;; writing the translation directly here.
;; That is for the sake of C-x 8 C-h.
(defvar iso-transl-char-map
  '(("* "   . iso-transl-no-break-space)
    (" "    . iso-transl-no-break-space)
    ("*!"   . iso-transl-inverted-exclamation-mark)
    ("!"    . iso-transl-inverted-exclamation-mark)
    ("\"\"" . iso-transl-diaeresis)
    ("\"A"  . iso-transl-A-umlaut)
    ("\"E"  . iso-transl-E-umlaut)
    ("\"I"  . iso-transl-I-umlaut)
    ("\"O"  . iso-transl-O-umlaut)
    ("\"U"  . iso-transl-U-umlaut)
    ("\"a"  . iso-transl-a-umlaut)
    ("\"e"  . iso-transl-e-umlaut)
    ("\"i"  . iso-transl-i-umlaut)
    ("\"o"  . iso-transl-o-umlaut)
    ("\"s"  . iso-transl-ss)
    ("\"u"  . iso-transl-u-umlaut)
    ("\"y"  . iso-transl-y-umlaut)
    ("''"   . iso-transl-acute-accent)
    ("'A"   . iso-transl-A-acute)
    ("'E"   . iso-transl-E-acute)
    ("'I"   . iso-transl-I-acute)
    ("'O"   . iso-transl-O-acute)
    ("'U"   . iso-transl-U-acute)
    ("'Y"   . iso-transl-Y-acute)
    ("'a"   . iso-transl-a-acute)
    ("'e"   . iso-transl-e-acute)
    ("'i"   . iso-transl-i-acute)
    ("'o"   . iso-transl-o-acute)
    ("'u"   . iso-transl-u-acute)
    ("'y"   . iso-transl-y-acute)
    ("*$"   . iso-transl-general-currency-sign)
    ("$"    . iso-transl-general-currency-sign)
    ("*+"   . iso-transl-plus-or-minus-sign)
    ("+"    . iso-transl-plus-or-minus-sign)
    (",,"   . iso-transl-cedilla)
    (",C"   . iso-transl-C-cedilla)
    (",c"   . iso-transl-c-cedilla)
    ("*-"   . iso-transl-soft-hyphen)
    ("-"    . iso-transl-soft-hyphen)
    ("*."   . iso-transl-middle-dot)
    ("."    . iso-transl-middle-dot)
    ("//"   . iso-transl-division-sign)
    ("/A"   . iso-transl-A-ring)
    ("/E"   . iso-transl-AE)
    ("/O"   . iso-transl-O-slash)
    ("/a"   . iso-transl-a-ring)
    ("/e"   . iso-transl-ae)
    ("/o"   . iso-transl-o-slash)
    ("1/2"  . iso-transl-one-half)
    ("1/4"  . iso-transl-one-quarter)
    ("3/4"  . iso-transl-three-quarters)
    ("*<"   . iso-transl-angle-left)
    ("<"    . iso-transl-angle-left)
    ("*="   . iso-transl-macron)
    ("="    . iso-transl-macron)
    ("*>"   . iso-transl-angle-right)
    (">"    . iso-transl-angle-right)
    ("*?"   . iso-transl-inverted-question-mark)
    ("?"    . iso-transl-inverted-question-mark)
    ("*C"   . iso-transl-copyright-sign)
    ("C"    . iso-transl-copyright-sign)
    ("*L"   . iso-transl-pound-sign)
    ("L"    . iso-transl-pound-sign)
    ("*P"   . iso-transl-pilcrow)
    ("P"    . iso-transl-pilcrow)
    ("*R"   . iso-transl-registered-sign)
    ("R"    . iso-transl-registered-sign)
    ("*S"   . iso-transl-section-sign)
    ("S"    . iso-transl-section-sign)
    ("*Y"   . iso-transl-yen-sign)
    ("Y"    . iso-transl-yen-sign)
    ("^1"   . iso-transl-superscript-1)
    ("^2"   . iso-transl-superscript-2)
    ("^3"   . iso-transl-superscript-3)
    ("^A"   . iso-transl-A-caret)
    ("^E"   . iso-transl-E-caret)
    ("^I"   . iso-transl-I-caret)
    ("^O"   . iso-transl-O-caret)
    ("^U"   . iso-transl-U-caret)
    ("^a"   . iso-transl-a-caret)
    ("^e"   . iso-transl-e-caret)
    ("^i"   . iso-transl-i-caret)
    ("^o"   . iso-transl-o-caret)
    ("^u"   . iso-transl-u-caret)
    ("_a"   . iso-transl-ordinal-indicator-feminine)
    ("_o"   . iso-transl-ordinal-indicator-masculine)
    ("`A"   . iso-transl-A-grave)
    ("`E"   . iso-transl-E-grave)
    ("`I"   . iso-transl-I-grave)
    ("`O"   . iso-transl-O-grave)
    ("`U"   . iso-transl-U-grave)
    ("`a"   . iso-transl-a-grave)
    ("`e"   . iso-transl-e-grave)
    ("`i"   . iso-transl-i-grave)
    ("`o"   . iso-transl-o-grave)
    ("`u"   . iso-transl-u-grave)
    ("*c"   . iso-transl-cent-sign)
    ("c"    . iso-transl-cent-sign)
    ("*o"   . iso-transl-degree-sign)
    ("o"    . iso-transl-degree-sign)
    ("*u"   . iso-transl-micro-sign)
    ("u"    . iso-transl-micro-sign)
    ("*m"   . iso-transl-micro-sign)
    ("m"    . iso-transl-micro-sign)
    ("*x"   . iso-transl-multiplication-sign)
    ("x"    . iso-transl-multiplication-sign)
    ("*|"   . iso-transl-broken-vertical-line)
    ("|"    . iso-transl-broken-vertical-line)
    ("~A"   . iso-transl-A-tilde)
    ("~D"   . iso-transl-D-stroke)
    ("~N"   . iso-transl-N-tilde)
    ("~O"   . iso-transl-O-tilde)
    ("~T"   . iso-transl-THORN)
    ("~a"   . iso-transl-a-tilde)
    ("~d"   . iso-transl-d-stroke)
    ("~n"   . iso-transl-n-tilde)
    ("~o"   . iso-transl-o-tilde)
    ("~t"   . iso-transl-thorn)
    ("~~"   . iso-transl-not-sign)
    ("' "   . "'")
    ("` "   . "`")
    ("\" "  . "\"")
    ("^ "   . "^")
    ("~ "   . "~"))
  "Alist of character translations for entering ISO characters.
Each element has the form (STRING . VECTOR).
The sequence STRING of ASCII chars translates into the
sequence VECTOR.  (VECTOR is normally one character long.)")

(defalias 'iso-transl-no-break-space [? ])
(defalias 'iso-transl-inverted-exclamation-mark [?¡])
(defalias 'iso-transl-cent-sign [?¢])
(defalias 'iso-transl-pound-sign [?£])
(defalias 'iso-transl-general-currency-sign [?¤])
(defalias 'iso-transl-yen-sign [?¥])
(defalias 'iso-transl-broken-vertical-line [?¦])
(defalias 'iso-transl-section-sign [?§])
(defalias 'iso-transl-diaeresis [?¨])
(defalias 'iso-transl-copyright-sign [?©])
(defalias 'iso-transl-ordinal-indicator-feminine [?ª])
(defalias 'iso-transl-angle-left [?«])
(defalias 'iso-transl-not-sign [?¬])
(defalias 'iso-transl-soft-hyphen [?­])
(defalias 'iso-transl-registered-sign [?®])
(defalias 'iso-transl-macron [?¯])
(defalias 'iso-transl-degree-sign [?°])
(defalias 'iso-transl-plus-or-minus-sign [?±])
(defalias 'iso-transl-superscript-2 [?²])
(defalias 'iso-transl-superscript-3 [?³])
(defalias 'iso-transl-acute-accent [?´])
(defalias 'iso-transl-micro-sign [?µ])
(defalias 'iso-transl-pilcrow [?¶])
(defalias 'iso-transl-middle-dot [?·])
(defalias 'iso-transl-cedilla [?¸])
(defalias 'iso-transl-superscript-1 [?¹])
(defalias 'iso-transl-ordinal-indicator-masculine [?º])
(defalias 'iso-transl-angle-right [?»])
(defalias 'iso-transl-one-quarter [?¼])
(defalias 'iso-transl-one-half [?½])
(defalias 'iso-transl-three-quarters [?¾])
(defalias 'iso-transl-inverted-question-mark [?¿])
(defalias 'iso-transl-A-grave [?À])
(defalias 'iso-transl-A-acute [?Á])
(defalias 'iso-transl-A-caret [?Â])
(defalias 'iso-transl-A-tilde [?Ã])
(defalias 'iso-transl-A-umlaut [?Ä])
(defalias 'iso-transl-A-ring [?Å])
(defalias 'iso-transl-AE [?Æ])
(defalias 'iso-transl-C-cedilla [?Ç])
(defalias 'iso-transl-E-grave [?È])
(defalias 'iso-transl-E-acute [?É])
(defalias 'iso-transl-E-caret [?Ê])
(defalias 'iso-transl-E-umlaut [?Ë])
(defalias 'iso-transl-I-grave [?Ì])
(defalias 'iso-transl-I-acute [?Í])
(defalias 'iso-transl-I-caret [?Î])
(defalias 'iso-transl-I-umlaut [?Ï])
(defalias 'iso-transl-D-stroke [?Ð])
(defalias 'iso-transl-N-tilde [?Ñ])
(defalias 'iso-transl-O-grave [?Ò])
(defalias 'iso-transl-O-acute [?Ó])
(defalias 'iso-transl-O-caret [?Ô])
(defalias 'iso-transl-O-tilde [?Õ])
(defalias 'iso-transl-O-umlaut [?Ö])
(defalias 'iso-transl-multiplication-sign [?×])
(defalias 'iso-transl-O-slash [?Ø])
(defalias 'iso-transl-U-grave [?Ù])
(defalias 'iso-transl-U-acute [?Ú])
(defalias 'iso-transl-U-caret [?Û])
(defalias 'iso-transl-U-umlaut [?Ü])
(defalias 'iso-transl-Y-acute [?Ý])
(defalias 'iso-transl-THORN [?Þ])
(defalias 'iso-transl-ss [?ß])
(defalias 'iso-transl-a-grave [?à])
(defalias 'iso-transl-a-acute [?á])
(defalias 'iso-transl-a-caret [?â])
(defalias 'iso-transl-a-tilde [?ã])
(defalias 'iso-transl-a-umlaut [?ä])
(defalias 'iso-transl-a-ring [?å])
(defalias 'iso-transl-ae [?æ])
(defalias 'iso-transl-c-cedilla [?ç])
(defalias 'iso-transl-e-grave [?è])
(defalias 'iso-transl-e-acute [?é])
(defalias 'iso-transl-e-caret [?ê])
(defalias 'iso-transl-e-umlaut [?ë])
(defalias 'iso-transl-i-grave [?ì])
(defalias 'iso-transl-i-acute [?í])
(defalias 'iso-transl-i-caret [?î])
(defalias 'iso-transl-i-umlaut [?ï])
(defalias 'iso-transl-d-stroke [?ð])
(defalias 'iso-transl-n-tilde [?ñ])
(defalias 'iso-transl-o-grave [?ò])
(defalias 'iso-transl-o-acute [?ó])
(defalias 'iso-transl-o-caret [?ô])
(defalias 'iso-transl-o-tilde [?õ])
(defalias 'iso-transl-o-umlaut [?ö])
(defalias 'iso-transl-division-sign [?÷])
(defalias 'iso-transl-o-slash [?ø])
(defalias 'iso-transl-u-grave [?ù])
(defalias 'iso-transl-u-acute [?ú])
(defalias 'iso-transl-u-caret [?û])
(defalias 'iso-transl-u-umlaut [?ü])
(defalias 'iso-transl-y-acute [?ý])
(defalias 'iso-transl-thorn [?þ])
(defalias 'iso-transl-y-umlaut [?ÿ])

;; Language-specific translation lists.
(defvar iso-transl-language-alist
  '(("Esperanto"
     ("C"  . [?Æ])
     ("G"  . [?Ø])
     ("H"  . [?¦])
     ("J"  . [?¬])
     ("S"  . [?Þ])
     ("U"  . [?Ý])
     ("c"  . [?æ])
     ("g"  . [?ø])
     ("h"  . [?¶])
     ("j"  . [?¼])
     ("s"  . [?þ])
     ("u"  . [?ý]))
    ("French"
     ("C"  . [?Ç])
     ("c"  . [?ç]))
    ("German"
     ("A"  . [?Ä])
     ("O"  . [?Ö]) 
     ("U"  . [?Ü])
     ("a"  . [?ä])
     ("o"  . [?ö])
     ("s"  . [?ß])
     ("u"  . [?ü]))
    ("Portuguese"
     ("C"  . [?Ç])
     ("c"  . [?ç]))
    ("Spanish"
     ("!"  . [?¡])
     ("?"  . [?¿])
     ("N"  . [?Ñ])
     ("n"  . [?ñ]))))

(defvar iso-transl-ctl-x-8-map nil
  "Keymap for C-x 8 prefix.")
(or iso-transl-ctl-x-8-map
    (fset 'iso-transl-ctl-x-8-map
	  (setq iso-transl-ctl-x-8-map (make-sparse-keymap))))
(or key-translation-map
    (setq key-translation-map (make-sparse-keymap)))
(define-key key-translation-map "\C-x8" iso-transl-ctl-x-8-map)

;; For each entry in the alist, we'll make up to three ways to generate
;; the character in question: the prefix `C-x 8'; the ALT modifier on
;; the first key of the sequence; and (if applicable) replacing the first
;; key of the sequence with the corresponding dead key.  For example, a
;; character associated with the string "~n" can be input with `C-x 8 ~ n'
;; or `Alt-~ n' or `mute-asciitilde n'.
(defun iso-transl-define-keys (alist)
  (while alist
    (let ((translated-vec (cdr (car alist))))
      (define-key iso-transl-ctl-x-8-map (car (car alist)) translated-vec)
      (let ((inchar (aref (car (car alist)) 0))
	    (vec (vconcat (car (car alist))))
	    (tail iso-transl-dead-key-alist))
	(aset vec 0 (logior (aref vec 0) ?\A-\^@))
	(define-key key-translation-map vec translated-vec)
	(define-key isearch-mode-map (vector (aref vec 0)) nil)
	(while tail
	  (if (eq (car (car tail)) inchar)
	      (let ((deadvec (copy-sequence vec))
		    (deadkey (cdr (car tail))))
		(aset deadvec 0 deadkey)
		(define-key isearch-mode-map (vector deadkey) nil)
		(define-key key-translation-map deadvec translated-vec)))
	  (setq tail (cdr tail)))))
    (setq alist (cdr alist))))

(defun iso-transl-set-language (lang)
  (interactive (list (let ((completion-ignore-case t))
		       (completing-read "Set which language? "
					iso-transl-language-alist nil t))))
  (iso-transl-define-keys (cdr (assoc lang iso-transl-language-alist))))


;; The standard mapping comes automatically.  You can partially overlay it
;; with a language-specific mapping by using `M-x iso-transl-set-language'.
(iso-transl-define-keys iso-transl-char-map)

(define-key isearch-mode-map "\C-x" nil)
(define-key isearch-mode-map [?\C-x t] 'isearch-other-control-char)
(define-key isearch-mode-map "\C-x8" nil)


(provide 'iso-transl)

;;; iso-transl.el ends here
