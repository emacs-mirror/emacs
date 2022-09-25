;;; cham.el --- Quail package for inputting Cham characters  -*- coding: utf-8; lexical-binding:t -*-

;; Copyright (C) 2021-2022 Free Software Foundation, Inc.

;; Author: Eli Zaretskii <eliz@gnu.org>
;; Keywords: i18n

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file defines the following Cham keyboards:
;;
;; - QWERTY-based Cham.

;;; Code:

(require 'quail)

(quail-define-package
 "cham" "Cham" "ꨌꩌ" t
 "A QWERTY-based Cham input method."
 nil t nil nil t nil nil nil nil nil t)

(quail-define-rules
 ("a" ?ꨀ)
 ("A" ?ꨄ)
 ("i" ?ꨁ)
 ("u" ?ꨂ)
 ("e" ?ꨃ)
 ("o" ?ꨅ)
 ("k" ?ꨆ)
 ("K" ?ꨇ)
 ("g" ?ꨈ)
 ("G" ?ꨉ)
 ("q" ?ꨊ)
 ("Q" ?ꨋ)
 ("c" ?ꨌ)
 ("C" ?ꨍ)
 ("j" ?ꨎ)
 ("J" ?ꨏ)
 ("z" ?ꨐ)
 ("Z" ?ꨑ)
 ("zz" ?ꨒ)
 ("t" ?ꨓ)
 ("T" ?ꨔ)
 ("d" ?ꨕ)
 ("D" ?ꨖ)
 ("n" ?ꨗ)
 ("N" ?ꨘ)
 ("p" ?ꨚ)
 ("P" ?ꨛ)
 ("f" ?ꨜ)
 ("b" ?ꨝ)
 ("B" ?ꨞ)
 ("m" ?ꨟ)
 ("M" ?ꨠ)
 ("mm" ?ꨡ)
 ("y" ?ꨢ)
 ("r" ?ꨣ)
 ("l" ?ꨤ)
 ("w" ?ꨥ)
 ("v" ?ꨥ)
 ("x" ?ꨦ)
 ("s" ?ꨧ)
 ("h" ?ꨨ)
 ("kk" ?ꩀ)
 ("ww" ?ꩁ)
 ("vv" ?ꩁ)
 ("qq" ?ꩂ)
 ("cc" ?ꩄ)
 ("tt" ?ꩅ)
 ("nn" ?ꩆ)
 ("pp" ?ꩇ)
 ("yy" ?ꩈ)
 ("rr" ?ꩉ)
 ("ll" ?ꩊ)
 ("gg" ?ꩊ)
 ("xx" ?ꩋ)
 ("." ?ꩌ)
 ("H" ?ꩍ)
 ("0" ?꩐)
 ("1" ?꩑)
 ("2" ?꩒)
 ("3" ?꩓)
 ("4" ?꩔)
 ("5" ?꩕)
 ("6" ?꩖)
 ("7" ?꩗)
 ("8" ?꩘)
 ("9" ?꩙)
 ("!" ?ꨩ)
 ("#" ?ꨪ)
 ("$" ?ꨫ)
 ("^" ?ꨬ)
 ("&" ?ꨮ)
 ("`" ?꩜)
 ("=" ?ꨱ)
 ("-" ?ꩃ)
 ("~" ?꩟)
 )

;;; cham.el ends here
