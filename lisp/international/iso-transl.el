;;; iso-transl.el --- keyboard input definitions for ISO 8859/1.

;; Copyright (C) 1987, 1993, 1994 Free Software Foundation, Inc.

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

;; Loading this package defines two ways of entering the ISO Latin 1 characters
;; with codes above 127.

;; One way is to type C-x 8 followed by a special chaacter sequence.
;; For example, C-x 8 " A enters an upper-case A-umlaut.

;; The other way is to type the same special sequence
;; but hold down Alt for the first character in it.

;;; Code:

(defvar iso-transl-char-map
  '((" "    . [160])
    ("!"    . [161])
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
    ("$"    . [164])
    ("+"    . [177])
    (",,"   . [184])
    (",C"   . [199])
    (",c"   . [231])
    ("-"    . [173])
    ("."    . [183])
    ("//"   . [247])
    ("/O"   . [216])
    ("/o"   . [248])
    ("1/2"  . [189])
    ("1/4"  . [188])
    ("3/4"  . [190])
    ("<"    . [171])
    ("="    . [175])
    (">"    . [187])
    ("?"    . [191])
    ("A"    . [197])
    ("E"    . [198])
    ("C"    . [169])
    ("D"    . [208])
    ("L"    . [163])
    ("P"    . [182])
    ("R"    . [174])
    ("S"    . [167])
    ("T"    . [222])
    ("Y"    . [165])
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
    ("a"    . [229])
    ("e"    . [230])
    ("c"    . [162])
    ("d"    . [240])
    ("o"    . [176])
    ("s"    . [223])
    ("t"    . [254])
    ("u"    . [181])
    ("x"    . [215])
    ("|"    . [166])
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


(let ((map (make-sparse-keymap))
      table)
  (setq table iso-transl-char-map)
;;;  ;; Create all the prefixes we need.
;;;  (define-key map "\""   (make-sparse-keymap))
;;;  (define-key map "'"    (make-sparse-keymap))
;;;  (define-key map ","    (make-sparse-keymap))
;;;  (define-key map "/"    (make-sparse-keymap))
;;;  (define-key map "1"    (make-sparse-keymap))
;;;  (define-key map "1/"   (make-sparse-keymap))
;;;  (define-key map "3"    (make-sparse-keymap))
;;;  (define-key map "3/"   (make-sparse-keymap))
;;;  (define-key map "A"    (make-sparse-keymap))
;;;  (define-key map "^"    (make-sparse-keymap))
;;;  (define-key map "_"    (make-sparse-keymap))
;;;  (define-key map "`"    (make-sparse-keymap))
;;;  (define-key map "a"    (make-sparse-keymap))
;;;  (define-key map "~"    (make-sparse-keymap))

  ;; Enter the individual sequences.
  (setq table iso-transl-char-map)
  (while table
    (define-key map (car (car table)) (cdr (car table)))
    (setq table (cdr table)))

  (or key-translation-map
      (setq key-translation-map (make-sparse-keymap)))
  (define-key key-translation-map "\C-x8" map)

  ;; Enter the individual sequences, this time with Alt as a modifier
  ;; on the first character, instead of with C-x 8 as a prefix.
  (setq table iso-transl-char-map)
  (while table
    (let ((string (vconcat (car (car table)))))
      (aset string 0 (+ (aref string 0) 262144))
      (define-key key-translation-map string (cdr (car table))))
    (setq table (cdr table)))

  ;; Enter the individual sequences, this time with
  ;; certain special function keys replacing the punctuation characters.
  (setq table iso-transl-char-map)
  (while table
    (let ((mapping
	   (assq (aref (car (car table)) 0) '((?\' . mute-acute)
					      (?\` . mute-grave)
					      (?\" . mute-diaeresis)
					      (?\^ . mute-asciicircum)
					      (?\~ . mute-asciitilde)))))
      (if mapping
	  (let ((string (vector (cdr mapping) (aref (car (car table)) 1))))
	    (define-key key-translation-map string (cdr (car table))))))
    (setq table (cdr table)))

  (define-key isearch-mode-map "\C-x" nil)
  (define-key isearch-mode-map [?\C-x t] 'isearch-other-control-char)
  (define-key isearch-mode-map "\C-x8" nil))

(provide 'iso-transl)

;;; iso-transl.el ends here
