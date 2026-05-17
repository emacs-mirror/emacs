;;; syriac.el --- Quail package for inputting Syriac	-*- coding: utf-8; lexical-binding:t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Lee Thompson <lee.p.thomp@gmail.com>
;; Keywords: multilingual, input method, Syriac

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

;;; Code:

(require 'quail)

(quail-define-package
 "syriac" "Syriac" "ܣ" nil "Syriac input method.

Based on Syriac table in X Keyboard Configuration DB
" nil t t t t nil nil nil nil nil t)

;;  ܏̮ 1! 2̊ 3̥ 4݉ 5♰ 6♱ 7܊ 8» 9) 0( -« =+
;;      ܔܰ ܨܳ ܖܶ ܩܺ ܦܽ ܜ݀ ܥ݁ ܗ̈ ܞ̄ ܚ̇ ܓ̃ ܕ݊ <>
;;       ܫܱ ܣܴ ܝܷ ܒܻ ܠܾ ܐܑ ܬـ ܢ̤ ܡ̱ ܟ̣ ܛ̰ ܆:
;;        ]ܲ [ܵ ܤܸ ܪܼ ܧܿ ܀ܹ .݂ ܘ، ܙ؛ ܇؟
;;

(quail-define-rules
 ("`" ?܏)
 ("~" ?̮)

 ("!" ?!)
 ("@" ?̊)
 ("#" ?̥)
 ("$" ?݉)
 ("%" ?♰)
 ("^" ?♱)
 ("&" ?܊)
 ("*" ?»)
 ("(" ?\))
 (")" ?\()
 ("_" ?«)
 ("+" ?+)

 ("Q" ?ܰ)
 ("W" ?ܳ)
 ("E" ?ܶ)
 ("R" ?ܺ)
 ("T" ?ܽ)
 ("Y" ?݀)
 ("U" ?݁)
 ("I" ?̈)
 ("O" ?̄)
 ("P" ?̇)
 ("{" ?̃)
 ("}" ?݊)

 ("A" ?ܱ)
 ("S" ?ܴ)
 ("D" ?ܷ)
 ("F" ?ܻ )
 ("G" ?ܾ)
 ("H" ?ܑ)
 ("J" ?ـ)
 ("K" ?̤)
 ("L" ?̱)
 (":" ?̣)
 ("\"" ?̰)
 ("|" ?:)


 ("Z" ?ܲ)
 ("X" ?ܵ)
 ("C" ?ܸ)
 ("V" ?ܼ)
 ("B" ?ܿ)
 ("N" ?ܹ)
 ("M" ?݂)
 ("<" ?،)
 (">" ?؛)
 ("?" ?؟)

 ("q" ?ܔ)
 ("w" ?ܨ)
 ("e" ?ܖ)
 ("r" ?ܩ)
 ("t" ?ܦ)
 ("y" ?ܜ)
 ("u" ?ܥ)
 ("i" ?ܗ)
 ("o" ?ܞ)
 ("p" ?ܚ)
 ("[" ?ܓ)
 ("]" ?ܕ)

 ("a" ?ܫ)
 ("s" ?ܣ)
 ("d" ?ܝ)
 ("f" ?ܒ)
 ("g" ?ܠ)
 ("h" ?ܐ)
 ("j" ?ܬ)
 ("k" ?ܢ)
 ("l" ?ܡ)
 (";" ?ܟ)
 ("'" ?ܛ)
 ("\\" ?܆)

 ("z" ?\])
 ("x" ?\[)
 ("c" ?ܤ)
 ("v" ?ܪ)
 ("b" ?ܧ)
 ("n" ?܀)
 ("m" ?.)
 ("," ?ܘ)
 ("." ?ܙ)
 ("/" ?܇))

(quail-define-package
 "syriac-phonetic" "Syriac" "ܦ" nil "Phonetic input method for Syriac.

Based on Syriac (phonetic) table in X Keyboard Configuration DB
" nil t t t t nil nil nil nil nil t)

;;  ܏̮ 1! 2̊ 3̥ 4݉ 5♰ 6♱ 7܊ 8» 9) 0( -« =+
;;     ܩܰ ܘܳ ܖܶ ܪܺ ܬܽ ܝ݀ ܜ݁ ܥ̈ ܧ̄ ܦ̇ ]̃ [݊
;;      ܐܱ ܣܴ ܕܷ ܔܻ ܓܾ ܗܑ ܛـ ܟ̤ ܠ̱ ܚ̣ ܞ̰ ܆:
;;       ܙܲ ܨܵ ܤܸ ܫܼ ܒܿ ܢܹ ܡ݂ ܀، .؛ ܇؟

(quail-define-rules
 ("`" ?܏)
 ("~" ?̮)

 ("!" ?!)
 ("@" ?̊)
 ("#" ?̥)
 ("$" ?݉)
 ("%" ?♰)
 ("^" ?♱)
 ("&" ?܊)
 ("*" ?»)
 ("(" ?\))
 (")" ?\()
 ("_" ?«)
 ("+" ?+)

 ("Q" ?ܰ)
 ("W" ?ܳ)
 ("E" ?ܶ)
 ("R" ?ܺ)
 ("T" ?ܽ)
 ("Y" ?݀)
 ("U" ?݁)
 ("I" ?̈)
 ("O" ?̄)
 ("P" ?̇)
 ("{" ?̃)
 ("}" ?݊)

 ("A" ?ܱ)
 ("S" ?ܴ)
 ("D" ?ܷ)
 ("F" ?ܻ)
 ("G" ?ܾ)
 ("H" ?ܑ)
 ("J" ?ـ)
 ("K" ?̤)
 ("L" ?̱)
 (":" ?̣)
 ("\"" ?̰)
 ("|" ?:)

 ("Z" ?ܲ)
 ("X" ?ܵ)
 ("C" ?ܸ)
 ("V" ?ܼ)
 ("B" ?ܿ)
 ("N" ?ܹ)
 ("M" ?݂)
 ("<" ?،)
 (">" ?؛)
 ("?" ?؟)

 ("q" ?ܩ)
 ("w" ?ܘ)
 ("e" ?ܖ)
 ("r" ?ܪ)
 ("t" ?ܬ)
 ("y" ?ܝ)
 ("u" ?ܜ)
 ("i" ?ܥ)
 ("o" ?ܧ)
 ("p" ?ܦ)
 ("[" ?\])
 ("]" ?\[)

 ("a" ?ܐ)
 ("s" ?ܣ)
 ("d" ?ܕ)
 ("f" ?ܔ)
 ("g" ?ܓ)
 ("h" ?ܗ)
 ("j" ?ܛ)
 ("k" ?ܟ)
 ("l" ?ܠ)
 (";" ?ܚ)
 ("\\" ?܆)
 ("'" ?ܞ)

 ("z" ?ܙ)
 ("x" ?ܨ)
 ("c" ?ܤ)
 ("v" ?ܫ)
 ("b" ?ܒ)
 ("n" ?ܢ)
 ("m" ?ܡ)
 ("," ?܀)
 ("." ?.)
 ("/" ?܇)
 )

;;; syriac.el ends here
