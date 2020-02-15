;;; georgian.el --- Quail package for inputting Georgian characters  -*-coding: utf-8;-*-

;; Copyright (C) 2001-2020 Free Software Foundation, Inc.

;; Author: Dave Love <fx@gnu.org>
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

;; This file defines the following Georgian keyboards:
;;
;; - Georgian input following the Yudit map from Mark Leisher
;;   <mleisher@crl.nmsu.edu>.
;;
;; - QWERTY-based Georgian.
;;
;; - QWERTY-based Nuskhuri script.

;;; Code:

(require 'quail)

(quail-define-package
 "georgian" "Georgian" "გ" t
 "A common Georgian transliteration (using Unicode)"
 nil t nil nil t nil nil nil nil nil t)

(quail-define-rules
 ("a" ?ა)
 ("b" ?ბ)
 ("g" ?გ)
 ("d" ?დ)
 ("e" ?ე)
 ("v" ?ვ)
 ("z" ?ზ)
 ("t" ?თ)
 ("i" ?ი)
 (".k" ?კ)
 ("l" ?ლ)
 ("m" ?მ)
 ("n" ?ნ)
 ("o" ?ო)
 (".p" ?პ)
 ("+z" ?ჟ)
 ("r" ?რ)
 ("s" ?ს)
 (".t" ?ტ)
 ("u" ?უ)
 ("p" ?ფ)
 ("k" ?ქ)
 (".g" ?ღ)
 ("q" ?ყ)
 ("+s" ?შ)
 ("+c" ?ჩ)
 ("c" ?ც)
 ("j" ?ძ)
 (".c" ?წ)
 (".+c" ?ჭ)
 ("x" ?ხ)
 ("+j" ?ჯ)
 ("h" ?ჰ)
 ("q1" ?ჴ)
 ("e0" ?ჱ)
 ("o1" ?ჵ)
 ("i1" ?ჲ)
 ("w" ?ჳ)
 ("f" ?ჶ)
 ("y" ?ჷ)
 ("e1" ?ჸ)
 )

(quail-define-package
 "georgian-qwerty" "Georgian" "ქ" t
 "Georgian input based on QWERTY keyboard."
 nil t nil nil t nil nil nil nil nil t)

(quail-define-rules
 ("a" ?ა)
 ("b" ?ბ)
 ("g" ?გ)
 ("d" ?დ)
 ("e" ?ე)
 ("v" ?ვ)
 ("z" ?ზ)
 ("T" ?თ)
 ("i" ?ი)
 ("k" ?კ)
 ("l" ?ლ)
 ("m" ?მ)
 ("n" ?ნ)
 ("o" ?ო)
 ("p" ?პ)
 ("J" ?ჟ)
 ("r" ?რ)
 ("s" ?ს)
 ("t" ?ტ)
 ("u" ?უ)
 ("f" ?ფ)
 ("q" ?ქ)
 ("R" ?ღ)
 ("y" ?ყ)
 ("S" ?შ)
 ("C" ?ჩ)
 ("c" ?ც)
 ("Z" ?ძ)
 ("w" ?წ)
 ("W" ?ჭ)
 ("x" ?ხ)
 ("j" ?ჯ)
 ("h" ?ჰ)
 ("X" ?ჴ)
 ("H" ?ჱ)
 ("K" ?ჵ)
 ("I" ?ჲ)
 ("V" ?ჳ)
 ("F" ?ჶ)
 ("Y" ?ჸ)
 ("G" ?ჷ)
 )

(quail-define-package
 "georgian-nuskhuri" "Georgian" "ⴌ" t
 "Nuskhuri Georgian (QWERTY-based)."
 nil t nil nil t nil nil nil nil nil t)

(quail-define-rules
 ("a" ?ⴀ)
 ("b" ?ⴁ)
 ("g" ?ⴂ)
 ("d" ?ⴃ)
 ("e" ?ⴄ)
 ("v" ?ⴅ)
 ("z" ?ⴆ)
 ("T" ?ⴇ)
 ("i" ?ⴈ)
 ("k" ?ⴉ)
 ("l" ?ⴊ)
 ("m" ?ⴋ)
 ("n" ?ⴌ)
 ("o" ?ⴍ)
 ("p" ?ⴎ)
 ("J" ?ⴏ)
 ("r" ?ⴐ)
 ("s" ?ⴑ)
 ("t" ?ⴒ)
 ("u" ?ⴓ)
 ("f" ?ⴔ)
 ("q" ?ⴕ)
 ("R" ?ⴖ)
 ("y" ?ⴗ)
 ("S" ?ⴘ)
 ("C" ?ⴙ)
 ("c" ?ⴚ)
 ("Z" ?ⴛ)
 ("w" ?ⴜ)
 ("W" ?ⴝ)
 ("x" ?ⴞ)
 ("j" ?ⴟ)
 ("h" ?ⴠ)
 ("X" ?ⴤ)
 ("H" ?ⴡ)
 ("K" ?ⴥ)
 ("I" ?ⴢ)
 ("V" ?ⴣ)
 )

;;; georgian.el ends here
