;;; iso-transl.el --- keyboard input definitions for ISO 8859/1.

;; Copyright (C) 1987, 1993, 1994, 1995 Free Software Foundation, Inc.

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Loading this package defines three ways of entering the non-ASCII
;; printable characters with codes above 127: the prefix C-x 8, or the
;; Alt key, or a dead accent key.  For example, you can enter uppercase
;; A-umlaut as `C-x 8 " A' or `Alt-" A' (if you have an Alt key) or
;; `umlaut A' (if you have an umlaut/diaeresis key).

;;; Code:

(defvar iso-transl-dead-key-alist
  '((?\' . mute-acute)
    (?\` . mute-grave)
    (?\" . mute-diaeresis)
    (?\^ . mute-asciicircum)
    (?\~ . mute-asciitilde))
  "Mapping of ASCII characters to their corresponding dead-key symbols.")

;; The two-character mnemonics are intended to be available in all languages.
;; The ones beginning with `*' have one-character synonyms, but a
;; language-specific table might override the short form for its own use.
(defvar iso-transl-char-map
  '(("* "   . [160])(" "   . [160])
    ("*!"   . [161])("!"   . [161])
    ("\"\"" . [168])
    ("\"A"  . [196])
    ("\"E"  . [203])
    ("\"I"  . [207])
    ("\"O"  . [214])
    ("\"U"  . [220])
    ("\"a"  . [228])
    ("\"e"  . [235])
    ("\"i"  . [239])
    ("\"o"  . [246])
    ("\"u"  . [252])
    ("\"y"  . [255])
    ("''"   . [180])
    ("'A"   . [193])
    ("'E"   . [201])
    ("'I"   . [205])
    ("'O"   . [211])
    ("'U"   . [218])
    ("'Y"   . [221])
    ("'a"   . [225])
    ("'e"   . [233])
    ("'i"   . [237])
    ("'o"   . [243])
    ("'u"   . [250])
    ("'y"   . [253])
    ("*$"   . [164])("$"   . [164])
    ("*+"   . [177])("+"   . [177])
    (",,"   . [184])
    (",C"   . [199])
    (",c"   . [231])
    ("*-"   . [173])("-"   . [173])
    ("*."   . [183])("."   . [183])
    ("//"   . [247])
    ("/O"   . [216])
    ("/o"   . [248])
    ("1/2"  . [189])
    ("1/4"  . [188])
    ("3/4"  . [190])
    ("*<"   . [171])("<"   . [171])
    ("*="   . [175])("="   . [175])
    ("*>"   . [187])(">"   . [187])
    ("*?"   . [191])("?"   . [191])
    ("*A"   . [197])("A"   . [197])
    ("*E"   . [198])("E"   . [198])
    ("*C"   . [169])("C"   . [169])
    ("*D"   . [208])("D"   . [208])
    ("*L"   . [163])("L"   . [163])
    ("*P"   . [182])("P"   . [182])
    ("*R"   . [174])("R"   . [174])
    ("*S"   . [167])("S"   . [167])
    ("*T"   . [222])("T"   . [222])
    ("*Y"   . [165])("Y"   . [165])
    ("^1"   . [185])
    ("^2"   . [178])
    ("^3"   . [179])
    ("^A"   . [194])
    ("^E"   . [202])
    ("^I"   . [206])
    ("^O"   . [212])
    ("^U"   . [219])
    ("^a"   . [226])
    ("^e"   . [234])
    ("^i"   . [238])
    ("^o"   . [244])
    ("^u"   . [251])
    ("_a"   . [170])
    ("_o"   . [186])
    ("`A"   . [192])
    ("`E"   . [200])
    ("`I"   . [204])
    ("`O"   . [210])
    ("`U"   . [217])
    ("`a"   . [224])
    ("`e"   . [232])
    ("`i"   . [236])
    ("`o"   . [242])
    ("`u"   . [249])
    ("*a"   . [229])("a"   . [229])
    ("*e"   . [230])("e"   . [230])
    ("*c"   . [162])("c"   . [162])
    ("*d"   . [240])("d"   . [240])
    ("*o"   . [176])("o"   . [176])
    ("*s"   . [223])("s"   . [223])
    ("*t"   . [254])("t"   . [254])
    ("*u"   . [181])("u"   . [181])
    ("*x"   . [215])("x"   . [215])
    ("*|"   . [166])("|"   . [166])
    ("~A"   . [195])
    ("~N"   . [209])
    ("~O"   . [213])
    ("~a"   . [227])
    ("~n"   . [241])
    ("~o"   . [245])
    ("~~"   . [172]))
  "Alist of character translations for entering ISO characters.
Each element has the form (STRING . VECTOR).
The sequence STRING of ASCII chars translates into the
sequence VECTOR.  (VECTOR is normally one character long.)")

;; Language-specific translation lists.
(defvar iso-transl-language-alist
  '(("German"
     ("A"  . [196])
     ("O"  . [214]) 
     ("U"  . [220])
     ("a"  . [228])
     ("o"  . [246])
     ("s"  . [223])
     ("u"  . [252]))
    ("Spanish"
     ("!"  . [161])
     ("?"  . [191])
     ("N"  . [241])
     ("n"  . [209]))
    ("Esperanto"
     ("C"  . [198])
     ("G"  . [216])
     ("H"  . [166])
     ("J"  . [172])
     ("S"  . [222])
     ("U"  . [221])
     ("c"  . [230])
     ("g"  . [248])
     ("h"  . [182])
     ("j"  . [188])
     ("s"  . [254])
     ("u"  . [253]))))

(defvar iso-transl-ctl-x-8-map nil
  "Keymap for C-x 8 prefix.")
(or iso-transl-ctl-x-8-map
    (setq iso-transl-ctl-x-8-map (make-sparse-keymap)))
(or key-translation-map
    (setq key-translation-map (make-sparse-keymap)))
(define-key key-translation-map "\C-x8" iso-transl-ctl-x-8-map)

;; For each entry in the alist, we'll make up to three ways to generate
;; the character in question: the prefix `C-x 8'; the ALT modifier on
;; the first key of the sequence; and (if applicable) replacing the first
;; key of the sequence with the corresponding dead key.  For example, a
;; character associated with the string "~n" can be input with `C-x 8 ~ n'
;; or `Alt-~ c' or `mute-asciitilde c'.
(defun iso-transl-define-keys (alist)
    (while alist
      (define-key iso-transl-ctl-x-8-map (car (car alist)) (cdr (car alist)))
      (let ((vec      (vconcat (car (car alist))))
	    (deadpair (assq (aref (car (car alist)) 0)
			    iso-transl-dead-key-alist)))
	(aset vec 0 (logior (aref vec 0) ?\A-\^@))
	(define-key key-translation-map vec (cdr (car alist)))
	(define-key isearch-mode-map (vector (aref vec 0)) nil)
	(if deadpair
	    (let ((deadvec (copy-sequence vec)))
	      (aset deadvec 0 (cdr deadpair))
	      (define-key isearch-mode-map (vector (aref deadvec 0)) nil)
	      (define-key key-translation-map deadvec (cdr (car alist))))))
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
