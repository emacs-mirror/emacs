;;; tifinagh.el --- Quail package for inputting Tifinagh	-*- coding: utf-8; lexical-binding:t -*-

;; Copyright (C) 2024-2026 Free Software Foundation, Inc.

;; Author: Adam Oudad <adam.oudad@gmail.com>
;; Keywords: mule, input method, Tifinagh

(require 'quail)

(quail-define-package
 "tifinagh" "Tininagh" "ⵣ" nil "Tifinagh input method.

Based on Tifinagh table in X Keyboard Configuration DB.
" nil t t t t nil nil nil nil nil t)

;; FIXME: This doesn't cover all of the codepoints that Unicode has
;; defined for the Tifinagh script.
(quail-define-rules
 ("Q" ?ⵈ)
 ("W" ?ⵯ)
 ("R" ?ⵕ)
 ("T" ?ⵟ)
 ("P" ?ⵒ)

 ("S" ?ⵚ)
 ("D" ?ⴹ)
 ("G" ?ⴶ)
 ("H" ?ⵂ)
 ("J" ?ⵌ)
 ("K" ?ⴾ)

 ("Z" ?ⵥ)
 ("X" ?ⵝ)
 ("C" ?ⵞ)
 ("V" ?ⵗ)

 ("q" ?ⵇ)
 ("w" ?ⵡ)
 ("e" ?ⴻ)
 ("r" ?ⵔ)
 ("t" ?ⵜ)
 ("y" ?ⵢ)
 ("u" ?ⵓ)
 ("i" ?ⵉ)
 ("o" ?ⵄ)
 ("p" ?ⵃ)

 ("a" ?ⴰ)
 ("s" ?ⵙ)
 ("d" ?ⴷ)
 ("f" ?ⴼ)
 ("g" ?ⴳ)
 ("h" ?ⵀ)
 ("j" ?ⵊ)
 ("k" ?ⴽ)
 ("l" ?ⵍ)

 ("z" ?ⵣ)
 ("x" ?ⵅ)
 ("c" ?ⵛ)
 ("v" ?ⵖ)
 ("b" ?ⴱ)
 ("n" ?ⵏ)
 ("m" ?ⵎ)
 )

;;; tifinagh.el ends here
