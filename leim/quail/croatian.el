;;; quail/croatian.el -- Quail package for inputing Croatian  -*-coding: iso-8859-2;-*-

;; Copyright (C) 2002 Free Software Foundation.

;; Author: Hrvoje Nik¹iæ <hniksic@xemacs.org>,
;;         modeled after czech.el by Milan Zamazal.
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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'quail)

(quail-define-package
 "croatian" "Croatian" "HR" nil
 "\"Standard\" Croatian keyboard."
  nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("@" ?\")
 ("^" ?&)
 ("&" ?/)
 ("*" ?\()
 ("(" ?\))
 (")" ?=)
 ("-" ?\')
 ("_" ??)
 ("=" ?+)
 ("+" ?*)
 ("[" ?¹)
 ("{" ?©)
 ("]" ?ð)
 ("}" ?Ð)
 (";" ?è)
 (":" ?È)
 ("'" ?æ)
 ("\"" ?Æ)
 ("\\" ?¾)
 ("|" ?®)
 ("<" ?\;)
 (">" ?:)
 ("/" ?-)
 ("?" ?_)
 ("y" ?z)
 ("Y" ?Z)
 ("z" ?y)
 ("Z" ?Y))

(quail-define-package
 "croatian-qwerty" "Croatian" "HR" nil
 "Croatian keyboard without the y/z swap."
 nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("@" ?\")
 ("^" ?&)
 ("&" ?/)
 ("*" ?\()
 ("(" ?\))
 (")" ?=)
 ("-" ?\')
 ("_" ??)
 ("=" ?+)
 ("+" ?*)
 ("[" ?¹)
 ("{" ?©)
 ("]" ?ð)
 ("}" ?Ð)
 (";" ?è)
 (":" ?È)
 ("'" ?æ)
 ("\"" ?Æ)
 ("\\" ?¾)
 ("|" ?®)
 ("<" ?\;)
 (">" ?:)
 ("/" ?-)
 ("?" ?_))

(quail-define-package
 "croatian-prefix" "Croatian" "HR" nil
 "Croatian input method, postfix.

\"c -> è
'c -> æ
\"s -> ¹
\"z -> ¾
/d -> ð"
 nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("\"c" ?è)
 ("\"C" ?È)
 ("'c" ?æ)
 ("'C" ?Æ)
 ("\"s" ?¹)
 ("\"S" ?©)
 ("\"z" ?¾)
 ("\"Z" ?®)
 ("/d" ?ð)
 ("/D" ?Ð))

(quail-define-package
 "croatian-postfix" "Croatian" "HR" nil
 "Croatian input method, postfix.

c\" -> è
c' -> æ
s\" -> ¹
z\" -> ¾
d/ -> ð"
 nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("c\"" ?è)
 ("C\"" ?È)
 ("c'" ?æ)
 ("C'" ?Æ)
 ("s\"" ?¹)
 ("S\"" ?©)
 ("z\"" ?¾)
 ("Z\"" ?®)
 ("d/" ?ð)
 ("D/" ?Ð))

(quail-define-package
 "croatian-xy" "Croatian" "HR" nil
 "An alternative Croatian input method.

cx -> è
cy -> æ
sx -> ¹
zx -> ¾
dy -> ð"
 nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("cx" ?è)
 ("CX" ?È)
 ("Cx" ?È)
 ("cy" ?æ)
 ("CY" ?Æ)
 ("Cy" ?Æ)
 ("sx" ?¹)
 ("SX" ?©)
 ("Sx" ?©)
 ("zx" ?¾)
 ("ZX" ?®)
 ("Zx" ?®)
 ("dy" ?ð)
 ("DY" ?Ð)
 ("Dy" ?Ð))

(quail-define-package
 "croatian-cc" "Croatian" "HR" nil
 "Another alternative Croatian input method.

cc -> è
ch -> æ
ss -> ¹
zz -> ¾
dd -> ð"
 nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("cc" ?è)
 ("CC" ?È)
 ("Cc" ?È)
 ("ch" ?æ)
 ("CH" ?Æ)
 ("Ch" ?Æ)
 ("ss" ?¹)
 ("SS" ?©)
 ("Ss" ?©)
 ("zz" ?¾)
 ("ZZ" ?®)
 ("Zz" ?®)
 ("dd" ?ð)
 ("DD" ?Ð)
 ("Dd" ?Ð))

;;; arch-tag: 8cb44078-9c51-4e81-9ef8-7d5b89f62e31
