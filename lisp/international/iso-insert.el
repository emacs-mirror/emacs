;;; iso-insert.el --- insert functions for ISO 8859/1

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Written by Howard Gayle.  See case-table.el for details.

;;; Code:

(defun insert-no-break-space ()
   (interactive "*")
   (insert (string 160))
)

(defun insert-inverted-exclamation-mark ()
   (interactive "*")
   (insert (string 161))
)

(defun insert-cent-sign ()
   (interactive "*")
   (insert (string 162))
)

(defun insert-pound-sign ()
   (interactive "*")
   (insert (string 163))
)

(defun insert-general-currency-sign ()
   (interactive "*")
   (insert (string 164))
)

(defun insert-yen-sign ()
   (interactive "*")
   (insert (string 165))
)

(defun insert-broken-vertical-line ()
   (interactive "*")
   (insert (string 166))
)

(defun insert-section-sign ()
   (interactive "*")
   (insert (string 167))
)

(defun insert-diaeresis ()
   (interactive "*")
   (insert (string 168))
)

(defun insert-copyright-sign ()
   (interactive "*")
   (insert (string 169))
)

(defun insert-ordinal-indicator-feminine ()
   (interactive "*")
   (insert (string 170))
)

(defun insert-angle-quotation-mark-left ()
   (interactive "*")
   (insert (string 171))
)

(defun insert-not-sign ()
   (interactive "*")
   (insert (string 172))
)

(defun insert-soft-hyphen ()
   (interactive "*")
   (insert (string 173))
)

(defun insert-registered-sign ()
   (interactive "*")
   (insert (string 174))
)

(defun insert-macron ()
   (interactive "*")
   (insert (string 175))
)

(defun insert-degree-sign ()
   (interactive "*")
   (insert (string 176))
)

(defun insert-plus-or-minus-sign ()
   (interactive "*")
   (insert (string 177))
)

(defun insert-superscript-two ()
   (interactive "*")
   (insert (string 178))
)

(defun insert-superscript-three ()
   (interactive "*")
   (insert (string 179))
)

(defun insert-acute-accent ()
   (interactive "*")
   (insert (string 180))
)

(defun insert-micro-sign ()
   (interactive "*")
   (insert (string 181))
)

(defun insert-pilcrow ()
   (interactive "*")
   (insert (string 182))
)

(defun insert-middle-dot ()
   (interactive "*")
   (insert (string 183))
)

(defun insert-cedilla ()
   (interactive "*")
   (insert (string 184))
)

(defun insert-superscript-one ()
   (interactive "*")
   (insert (string 185))
)

(defun insert-ordinal-indicator-masculine ()
   (interactive "*")
   (insert (string 186))
)

(defun insert-angle-quotation-mark-right ()
   (interactive "*")
   (insert (string 187))
)

(defun insert-fraction-one-quarter ()
   (interactive "*")
   (insert (string 188))
)

(defun insert-fraction-one-half ()
   (interactive "*")
   (insert (string 189))
)

(defun insert-fraction-three-quarters ()
   (interactive "*")
   (insert (string 190))
)

(defun insert-inverted-question-mark ()
   (interactive "*")
   (insert (string 191))
)

(defun insert-A-grave ()
   (interactive "*")
   (insert (string 192))
)

(defun insert-A-acute ()
   (interactive "*")
   (insert (string 193))
)

(defun insert-A-circumflex ()
   (interactive "*")
   (insert (string 194))
)

(defun insert-A-tilde ()
   (interactive "*")
   (insert (string 195))
)

(defun insert-A-umlaut ()
   (interactive "*")
   (insert (string 196))
)

(defun insert-A-ring ()
   (interactive "*")
   (insert (string 197))
)

(defun insert-AE ()
   (interactive "*")
   (insert (string 198))
)

(defun insert-C-cedilla ()
   (interactive "*")
   (insert (string 199))
)

(defun insert-E-grave ()
   (interactive "*")
   (insert (string 200))
)

(defun insert-E-acute ()
   (interactive "*")
   (insert (string 201))
)

(defun insert-E-circumflex ()
   (interactive "*")
   (insert (string 202))
)

(defun insert-E-umlaut ()
   (interactive "*")
   (insert (string 203))
)

(defun insert-I-grave ()
   (interactive "*")
   (insert (string 204))
)

(defun insert-I-acute ()
   (interactive "*")
   (insert (string 205))
)

(defun insert-I-circumflex ()
   (interactive "*")
   (insert (string 206))
)

(defun insert-I-umlaut ()
   (interactive "*")
   (insert (string 207))
)

(defun insert-D-stroke ()
   (interactive "*")
   (insert (string 208))
)

(defun insert-N-tilde ()
   (interactive "*")
   (insert (string 209))
)

(defun insert-O-grave ()
   (interactive "*")
   (insert (string 210))
)

(defun insert-O-acute ()
   (interactive "*")
   (insert (string 211))
)

(defun insert-O-circumflex ()
   (interactive "*")
   (insert (string 212))
)

(defun insert-O-tilde ()
   (interactive "*")
   (insert (string 213))
)

(defun insert-O-umlaut ()
   (interactive "*")
   (insert (string 214))
)

(defun insert-multiplication-sign ()
   (interactive "*")
   (insert (string 215))
)

(defun insert-O-slash ()
   (interactive "*")
   (insert (string 216))
)

(defun insert-U-grave ()
   (interactive "*")
   (insert (string 217))
)

(defun insert-U-acute ()
   (interactive "*")
   (insert (string 218))
)

(defun insert-U-circumflex ()
   (interactive "*")
   (insert (string 219))
)

(defun insert-U-umlaut ()
   (interactive "*")
   (insert (string 220))
)

(defun insert-Y-acute ()
   (interactive "*")
   (insert (string 221))
)

(defun insert-THORN ()
   (interactive "*")
   (insert (string 222))
)

(defun insert-ss ()
   (interactive "*")
   (insert (string 223))
)

(defun insert-a-grave ()
   (interactive "*")
   (insert (string 224))
)

(defun insert-a-acute ()
   (interactive "*")
   (insert (string 225))
)

(defun insert-a-circumflex ()
   (interactive "*")
   (insert (string 226))
)

(defun insert-a-tilde ()
   (interactive "*")
   (insert (string 227))
)

(defun insert-a-umlaut ()
   (interactive "*")
   (insert (string 228))
)

(defun insert-a-ring ()
   (interactive "*")
   (insert (string 229))
)

(defun insert-ae ()
   (interactive "*")
   (insert (string 230))
)

(defun insert-c-cedilla ()
   (interactive "*")
   (insert (string 231))
)

(defun insert-e-grave ()
   (interactive "*")
   (insert (string 232))
)

(defun insert-e-acute ()
   (interactive "*")
   (insert (string 233))
)

(defun insert-e-circumflex ()
   (interactive "*")
   (insert (string 234))
)

(defun insert-e-umlaut ()
   (interactive "*")
   (insert (string 235))
)

(defun insert-i-grave ()
   (interactive "*")
   (insert (string 236))
)

(defun insert-i-acute ()
   (interactive "*")
   (insert (string 237))
)

(defun insert-i-circumflex ()
   (interactive "*")
   (insert (string 238))
)

(defun insert-i-umlaut ()
   (interactive "*")
   (insert (string 239))
)

(defun insert-d-stroke ()
   (interactive "*")
   (insert (string 240))
)

(defun insert-n-tilde ()
   (interactive "*")
   (insert (string 241))
)

(defun insert-o-grave ()
   (interactive "*")
   (insert (string 242))
)

(defun insert-o-acute ()
   (interactive "*")
   (insert (string 243))
)

(defun insert-o-circumflex ()
   (interactive "*")
   (insert (string 244))
)

(defun insert-o-tilde ()
   (interactive "*")
   (insert (string 245))
)

(defun insert-o-umlaut ()
   (interactive "*")
   (insert (string 246))
)

(defun insert-division-sign ()
   (interactive "*")
   (insert (string 247))
)

(defun insert-o-slash ()
   (interactive "*")
   (insert (string 248))
)

(defun insert-u-grave ()
   (interactive "*")
   (insert (string 249))
)

(defun insert-u-acute ()
   (interactive "*")
   (insert (string 250))
)

(defun insert-u-circumflex ()
   (interactive "*")
   (insert (string 251))
)

(defun insert-u-umlaut ()
   (interactive "*")
   (insert (string 252))
)

(defun insert-y-acute ()
   (interactive "*")
   (insert (string 253))
)

(defun insert-thorn ()
   (interactive "*")
   (insert (string 254))
)

(defun insert-y-umlaut ()
   (interactive "*")
   (insert (string 255))
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
(defalias '8859-1-map 8859-1-map)

(provide 'iso-insert)

;;; iso-insert.el ends here
