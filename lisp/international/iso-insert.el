;;; iso-insert.el --- insert functions for ISO 8859/1.

;; Copyright (C) 1987, 1994 Free Software Foundation, Inc.

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

;; Written by Howard Gayle.  See case-table.el for details.

;;; Code:

(defun insert-no-break-space ()
   (interactive "*")
   (insert 160)
)

(defun insert-inverted-exclamation-mark ()
   (interactive "*")
   (insert 161)
)

(defun insert-cent-sign ()
   (interactive "*")
   (insert 162)
)

(defun insert-pound-sign ()
   (interactive "*")
   (insert 163)
)

(defun insert-general-currency-sign ()
   (interactive "*")
   (insert 164)
)

(defun insert-yen-sign ()
   (interactive "*")
   (insert 165)
)

(defun insert-broken-vertical-line ()
   (interactive "*")
   (insert 166)
)

(defun insert-section-sign ()
   (interactive "*")
   (insert 167)
)

(defun insert-diaeresis ()
   (interactive "*")
   (insert 168)
)

(defun insert-copyright-sign ()
   (interactive "*")
   (insert 169)
)

(defun insert-ordinal-indicator-feminine ()
   (interactive "*")
   (insert 170)
)

(defun insert-angle-quotation-mark-left ()
   (interactive "*")
   (insert 171)
)

(defun insert-not-sign ()
   (interactive "*")
   (insert 172)
)

(defun insert-soft-hyphen ()
   (interactive "*")
   (insert 173)
)

(defun insert-registered-sign ()
   (interactive "*")
   (insert 174)
)

(defun insert-macron ()
   (interactive "*")
   (insert 175)
)

(defun insert-degree-sign ()
   (interactive "*")
   (insert 176)
)

(defun insert-plus-or-minus-sign ()
   (interactive "*")
   (insert 177)
)

(defun insert-superscript-two ()
   (interactive "*")
   (insert 178)
)

(defun insert-superscript-three ()
   (interactive "*")
   (insert 179)
)

(defun insert-acute-accent ()
   (interactive "*")
   (insert 180)
)

(defun insert-micro-sign ()
   (interactive "*")
   (insert 181)
)

(defun insert-pilcrow ()
   (interactive "*")
   (insert 182)
)

(defun insert-middle-dot ()
   (interactive "*")
   (insert 183)
)

(defun insert-cedilla ()
   (interactive "*")
   (insert 184)
)

(defun insert-superscript-one ()
   (interactive "*")
   (insert 185)
)

(defun insert-ordinal-indicator-masculine ()
   (interactive "*")
   (insert 186)
)

(defun insert-angle-quotation-mark-right ()
   (interactive "*")
   (insert 187)
)

(defun insert-fraction-one-quarter ()
   (interactive "*")
   (insert 188)
)

(defun insert-fraction-one-half ()
   (interactive "*")
   (insert 189)
)

(defun insert-fraction-three-quarters ()
   (interactive "*")
   (insert 190)
)

(defun insert-inverted-question-mark ()
   (interactive "*")
   (insert 191)
)

(defun insert-A-grave ()
   (interactive "*")
   (insert 192)
)

(defun insert-A-acute ()
   (interactive "*")
   (insert 193)
)

(defun insert-A-circumflex ()
   (interactive "*")
   (insert 194)
)

(defun insert-A-tilde ()
   (interactive "*")
   (insert 195)
)

(defun insert-A-umlaut ()
   (interactive "*")
   (insert 196)
)

(defun insert-A-ring ()
   (interactive "*")
   (insert 197)
)

(defun insert-AE ()
   (interactive "*")
   (insert 198)
)

(defun insert-C-cedilla ()
   (interactive "*")
   (insert 199)
)

(defun insert-E-grave ()
   (interactive "*")
   (insert 200)
)

(defun insert-E-acute ()
   (interactive "*")
   (insert 201)
)

(defun insert-E-circumflex ()
   (interactive "*")
   (insert 202)
)

(defun insert-E-umlaut ()
   (interactive "*")
   (insert 203)
)

(defun insert-I-grave ()
   (interactive "*")
   (insert 204)
)

(defun insert-I-acute ()
   (interactive "*")
   (insert 205)
)

(defun insert-I-circumflex ()
   (interactive "*")
   (insert 206)
)

(defun insert-I-umlaut ()
   (interactive "*")
   (insert 207)
)

(defun insert-D-stroke ()
   (interactive "*")
   (insert 208)
)

(defun insert-N-tilde ()
   (interactive "*")
   (insert 209)
)

(defun insert-O-grave ()
   (interactive "*")
   (insert 210)
)

(defun insert-O-acute ()
   (interactive "*")
   (insert 211)
)

(defun insert-O-circumflex ()
   (interactive "*")
   (insert 212)
)

(defun insert-O-tilde ()
   (interactive "*")
   (insert 213)
)

(defun insert-O-umlaut ()
   (interactive "*")
   (insert 214)
)

(defun insert-multiplication-sign ()
   (interactive "*")
   (insert 215)
)

(defun insert-O-slash ()
   (interactive "*")
   (insert 216)
)

(defun insert-U-grave ()
   (interactive "*")
   (insert 217)
)

(defun insert-U-acute ()
   (interactive "*")
   (insert 218)
)

(defun insert-U-circumflex ()
   (interactive "*")
   (insert 219)
)

(defun insert-U-umlaut ()
   (interactive "*")
   (insert 220)
)

(defun insert-Y-acute ()
   (interactive "*")
   (insert 221)
)

(defun insert-THORN ()
   (interactive "*")
   (insert 222)
)

(defun insert-ss ()
   (interactive "*")
   (insert 223)
)

(defun insert-a-grave ()
   (interactive "*")
   (insert 224)
)

(defun insert-a-acute ()
   (interactive "*")
   (insert 225)
)

(defun insert-a-circumflex ()
   (interactive "*")
   (insert 226)
)

(defun insert-a-tilde ()
   (interactive "*")
   (insert 227)
)

(defun insert-a-umlaut ()
   (interactive "*")
   (insert 228)
)

(defun insert-a-ring ()
   (interactive "*")
   (insert 229)
)

(defun insert-ae ()
   (interactive "*")
   (insert 230)
)

(defun insert-c-cedilla ()
   (interactive "*")
   (insert 231)
)

(defun insert-e-grave ()
   (interactive "*")
   (insert 232)
)

(defun insert-e-acute ()
   (interactive "*")
   (insert 233)
)

(defun insert-e-circumflex ()
   (interactive "*")
   (insert 234)
)

(defun insert-e-umlaut ()
   (interactive "*")
   (insert 235)
)

(defun insert-i-grave ()
   (interactive "*")
   (insert 236)
)

(defun insert-i-acute ()
   (interactive "*")
   (insert 237)
)

(defun insert-i-circumflex ()
   (interactive "*")
   (insert 238)
)

(defun insert-i-umlaut ()
   (interactive "*")
   (insert 239)
)

(defun insert-d-stroke ()
   (interactive "*")
   (insert 240)
)

(defun insert-n-tilde ()
   (interactive "*")
   (insert 241)
)

(defun insert-o-grave ()
   (interactive "*")
   (insert 242)
)

(defun insert-o-acute ()
   (interactive "*")
   (insert 243)
)

(defun insert-o-circumflex ()
   (interactive "*")
   (insert 244)
)

(defun insert-o-tilde ()
   (interactive "*")
   (insert 245)
)

(defun insert-o-umlaut ()
   (interactive "*")
   (insert 246)
)

(defun insert-division-sign ()
   (interactive "*")
   (insert 247)
)

(defun insert-o-slash ()
   (interactive "*")
   (insert 248)
)

(defun insert-u-grave ()
   (interactive "*")
   (insert 249)
)

(defun insert-u-acute ()
   (interactive "*")
   (insert 250)
)

(defun insert-u-circumflex ()
   (interactive "*")
   (insert 251)
)

(defun insert-u-umlaut ()
   (interactive "*")
   (insert 252)
)

(defun insert-y-acute ()
   (interactive "*")
   (insert 253)
)

(defun insert-thorn ()
   (interactive "*")
   (insert 254)
)

(defun insert-y-umlaut ()
   (interactive "*")
   (insert 255)
)

(defvar 8859-1-map nil "Keymap for ISO 8859/1 character insertion.")
(if 8859-1-map nil
   (setq 8859-1-map (make-keymap))
   (define-key 8859-1-map " "    'insert-no-break-space)
   (define-key 8859-1-map "!"    'insert-inverted-exclamation-mark)
   (define-key 8859-1-map "\""   (make-sparse-keymap))
   (define-key 8859-1-map "\"\"" 'insert-diaeresis)
   (define-key 8859-1-map "\"A"  'insert-A-umlaut)
   (define-key 8859-1-map "\"E"  'insert-E-umlaut)
   (define-key 8859-1-map "\"I"  'insert-I-umlaut)
   (define-key 8859-1-map "\"O"  'insert-O-umlaut)
   (define-key 8859-1-map "\"U"  'insert-U-umlaut)
   (define-key 8859-1-map "\"a"  'insert-a-umlaut)
   (define-key 8859-1-map "\"e"  'insert-e-umlaut)
   (define-key 8859-1-map "\"i"  'insert-i-umlaut)
   (define-key 8859-1-map "\"o"  'insert-o-umlaut)
   (define-key 8859-1-map "\"u"  'insert-u-umlaut)
   (define-key 8859-1-map "\"y"  'insert-y-umlaut)
   (define-key 8859-1-map "'"    (make-sparse-keymap))
   (define-key 8859-1-map "''"   'insert-acute-accent)
   (define-key 8859-1-map "'A"   'insert-A-acute)
   (define-key 8859-1-map "'E"   'insert-E-acute)
   (define-key 8859-1-map "'I"   'insert-I-acute)
   (define-key 8859-1-map "'O"   'insert-O-acute)
   (define-key 8859-1-map "'U"   'insert-U-acute)
   (define-key 8859-1-map "'Y"   'insert-Y-acute)
   (define-key 8859-1-map "'a"   'insert-a-acute)
   (define-key 8859-1-map "'e"   'insert-e-acute)
   (define-key 8859-1-map "'i"   'insert-i-acute)
   (define-key 8859-1-map "'o"   'insert-o-acute)
   (define-key 8859-1-map "'u"   'insert-u-acute)
   (define-key 8859-1-map "'y"   'insert-y-acute)
   (define-key 8859-1-map "$"    'insert-general-currency-sign)
   (define-key 8859-1-map "+"    'insert-plus-or-minus-sign)
   (define-key 8859-1-map ","    (make-sparse-keymap))
   (define-key 8859-1-map ",,"   'insert-cedilla)
   (define-key 8859-1-map ",C"   'insert-C-cedilla)
   (define-key 8859-1-map ",c"   'insert-c-cedilla)
   (define-key 8859-1-map "-"    'insert-soft-hyphen)
   (define-key 8859-1-map "."    'insert-middle-dot)
   (define-key 8859-1-map "/"    (make-sparse-keymap))
   (define-key 8859-1-map "//"   'insert-division-sign)
   (define-key 8859-1-map "/O"   'insert-O-slash)
   (define-key 8859-1-map "/o"   'insert-o-slash)
   (define-key 8859-1-map "1"    (make-sparse-keymap))
   (define-key 8859-1-map "1/"   (make-sparse-keymap))
   (define-key 8859-1-map "1/2"  'insert-fraction-one-half)
   (define-key 8859-1-map "1/4"  'insert-fraction-one-quarter)
   (define-key 8859-1-map "3"    (make-sparse-keymap))
   (define-key 8859-1-map "3/"   (make-sparse-keymap))
   (define-key 8859-1-map "3/4"  'insert-fraction-three-quarters)
   (define-key 8859-1-map "<"    'insert-angle-quotation-mark-left)
   (define-key 8859-1-map "="    'insert-macron)
   (define-key 8859-1-map ">"    'insert-angle-quotation-mark-right)
   (define-key 8859-1-map "?"    'insert-inverted-question-mark)
   (define-key 8859-1-map "A"    'insert-A-ring)
   (define-key 8859-1-map "E"    'insert-AE)
   (define-key 8859-1-map "C"    'insert-copyright-sign)
   (define-key 8859-1-map "D"    'insert-D-stroke)
   (define-key 8859-1-map "L"    'insert-pound-sign)
   (define-key 8859-1-map "P"    'insert-pilcrow)
   (define-key 8859-1-map "R"    'insert-registered-sign)
   (define-key 8859-1-map "S"    'insert-section-sign)
   (define-key 8859-1-map "T"    'insert-THORN)
   (define-key 8859-1-map "Y"    'insert-yen-sign)
   (define-key 8859-1-map "^"    (make-sparse-keymap))
   (define-key 8859-1-map "^1"   'insert-superscript-one)
   (define-key 8859-1-map "^2"   'insert-superscript-two)
   (define-key 8859-1-map "^3"   'insert-superscript-three)
   (define-key 8859-1-map "^A"   'insert-A-circumflex)
   (define-key 8859-1-map "^E"   'insert-E-circumflex)
   (define-key 8859-1-map "^I"   'insert-I-circumflex)
   (define-key 8859-1-map "^O"   'insert-O-circumflex)
   (define-key 8859-1-map "^U"   'insert-U-circumflex)
   (define-key 8859-1-map "^a"   'insert-a-circumflex)
   (define-key 8859-1-map "^e"   'insert-e-circumflex)
   (define-key 8859-1-map "^i"   'insert-i-circumflex)
   (define-key 8859-1-map "^o"   'insert-o-circumflex)
   (define-key 8859-1-map "^u"   'insert-u-circumflex)
   (define-key 8859-1-map "_"    (make-sparse-keymap))
   (define-key 8859-1-map "_a"   'insert-ordinal-indicator-feminine)
   (define-key 8859-1-map "_o"   'insert-ordinal-indicator-masculine)
   (define-key 8859-1-map "`"    (make-sparse-keymap))
   (define-key 8859-1-map "`A"   'insert-A-grave)
   (define-key 8859-1-map "`E"   'insert-E-grave)
   (define-key 8859-1-map "`I"   'insert-I-grave)
   (define-key 8859-1-map "`O"   'insert-O-grave)
   (define-key 8859-1-map "`U"   'insert-U-grave)
   (define-key 8859-1-map "`a"   'insert-a-grave)
   (define-key 8859-1-map "`e"   'insert-e-grave)
   (define-key 8859-1-map "`i"   'insert-i-grave)
   (define-key 8859-1-map "`o"   'insert-o-grave)
   (define-key 8859-1-map "`u"   'insert-u-grave)
   (define-key 8859-1-map "a"    'insert-a-ring)
   (define-key 8859-1-map "e"    'insert-ae)
   (define-key 8859-1-map "c"    'insert-cent-sign)
   (define-key 8859-1-map "d"    'insert-d-stroke)
   (define-key 8859-1-map "o"    'insert-degree-sign)
   (define-key 8859-1-map "s"    'insert-ss)
   (define-key 8859-1-map "t"    'insert-thorn)
   (define-key 8859-1-map "u"    'insert-micro-sign)
   (define-key 8859-1-map "x"    'insert-multiplication-sign)
   (define-key 8859-1-map "|"    'insert-broken-vertical-line)
   (define-key 8859-1-map "~"    (make-sparse-keymap))
   (define-key 8859-1-map "~A"   'insert-A-tilde)
   (define-key 8859-1-map "~N"   'insert-N-tilde)
   (define-key 8859-1-map "~O"   'insert-O-tilde)
   (define-key 8859-1-map "~a"   'insert-a-tilde)
   (define-key 8859-1-map "~n"   'insert-n-tilde)
   (define-key 8859-1-map "~o"   'insert-o-tilde)
   (define-key 8859-1-map "~~"   'insert-not-sign)
   (if (not (lookup-key global-map "\C-x8"))
      (define-key global-map "\C-x8" 8859-1-map))
)

(provide 'iso-insert)

;;; iso-insert.el ends here
