;;; iso-transl.el --- keyboard input definitions for ISO 8859/1.

;; Copyright (C) 1987, 1993 Free Software Foundation, Inc.

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

;;; Code:

(let ((map (make-sparse-keymap)))
  (define-key map " "    [160])
  (define-key map "!"    [161])
  (define-key map "\""   (make-sparse-keymap))
  (define-key map "\"\"" [168])
  (define-key map "\"A"  [196])
  (define-key map "\"E"  [203])
  (define-key map "\"I"  [207])
  (define-key map "\"O"  [214])
  (define-key map "\"U"  [220])
  (define-key map "\"a"  [228])
  (define-key map "\"e"  [235])
  (define-key map "\"i"  [239])
  (define-key map "\"o"  [246])
  (define-key map "\"u"  [252])
  (define-key map "\"y"  [255])
  (define-key map "'"    (make-sparse-keymap))
  (define-key map "''"   [180])
  (define-key map "'A"   [193])
  (define-key map "'E"   [201])
  (define-key map "'I"   [205])
  (define-key map "'O"   [211])
  (define-key map "'U"   [218])
  (define-key map "'Y"   [221])
  (define-key map "'a"   [225])
  (define-key map "'e"   [233])
  (define-key map "'i"   [237])
  (define-key map "'o"   [243])
  (define-key map "'u"   [250])
  (define-key map "'y"   [253])
  (define-key map "$"    [164])
  (define-key map "+"    [177])
  (define-key map ","    (make-sparse-keymap))
  (define-key map ",,"   [184])
  (define-key map ",C"   [199])
  (define-key map ",c"   [231])
  (define-key map "-"    [173])
  (define-key map "."    [183])
  (define-key map "/"    (make-sparse-keymap))
  (define-key map "//"   [247])
  (define-key map "/O"   [216])
  (define-key map "/o"   [248])
  (define-key map "1"    (make-sparse-keymap))
  (define-key map "1/"   (make-sparse-keymap))
  (define-key map "1/2"  [189])
  (define-key map "1/4"  [188])
  (define-key map "3"    (make-sparse-keymap))
  (define-key map "3/"   (make-sparse-keymap))
  (define-key map "3/4"  [190])
  (define-key map "<"    [171])
  (define-key map "="    [175])
  (define-key map ">"    [187])
  (define-key map "?"    [191])
  (define-key map "A"    (make-sparse-keymap))
  (define-key map "AA"   [197])
  (define-key map "AE"   [198])
  (define-key map "C"    [169])
  (define-key map "D"    [208])
  (define-key map "L"    [163])
  (define-key map "P"    [182])
  (define-key map "R"    [174])
  (define-key map "S"    [167])
  (define-key map "T"    [222])
  (define-key map "Y"    [165])
  (define-key map "^"    (make-sparse-keymap))
  (define-key map "^1"   [185])
  (define-key map "^2"   [178])
  (define-key map "^3"   [179])
  (define-key map "^A"   [194])
  (define-key map "^E"   [202])
  (define-key map "^I"   [206])
  (define-key map "^O"   [212])
  (define-key map "^U"   [219])
  (define-key map "^a"   [226])
  (define-key map "^e"   [234])
  (define-key map "^i"   [238])
  (define-key map "^o"   [244])
  (define-key map "^u"   [251])
  (define-key map "_"    (make-sparse-keymap))
  (define-key map "_a"   [170])
  (define-key map "_o"   [186])
  (define-key map "`"    (make-sparse-keymap))
  (define-key map "`A"   [192])
  (define-key map "`E"   [200])
  (define-key map "`I"   [204])
  (define-key map "`O"   [210])
  (define-key map "`U"   [217])
  (define-key map "`a"   [224])
  (define-key map "`e"   [232])
  (define-key map "`i"   [236])
  (define-key map "`o"   [242])
  (define-key map "`u"   [249])
  (define-key map "a"    (make-sparse-keymap))
  (define-key map "aa"   [229])
  (define-key map "ae"   [230])
  (define-key map "c"    [162])
  (define-key map "d"    [240])
  (define-key map "o"    [176])
  (define-key map "s"    [223])
  (define-key map "t"    [254])
  (define-key map "u"    [181])
  (define-key map "x"    [215])
  (define-key map "|"    [166])
  (define-key map "~"    (make-sparse-keymap))
  (define-key map "~A"   [195])
  (define-key map "~N"   [209])
  (define-key map "~O"   [213])
  (define-key map "~a"   [227])
  (define-key map "~n"   [241])
  (define-key map "~o"   [245])
  (define-key map "~~"   [172])
  (or key-translation-map
      (setq key-translation-map (make-sparse-keymap)))
  (define-key key-translation-map "\C-x8" map)
  (define-key isearch-mode-map "\C-x" nil)
  (define-key isearch-mode-map [?\C-x t] 'isearch-other-control-char)
  (define-key isearch-mode-map "\C-x8" nil))

(provide 'iso-transl)

;;; iso-transl.el ends here
