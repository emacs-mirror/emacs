;;; quail/greek.el -- Quail package for inputting Greek

;; Copyright (C) 1997 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.

;; Keywords: multilingual, input method, Greek

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

;;; Code:

(require 'quail)

(quail-define-package
 "greek-jis" "Greek" "Ω" nil
 "Ελληνικα: Greek keyboard layout (JIS X0208.1983)

The layout is same as greek, but uses JIS characters.
Sorry, accents and terminal sigma are not supported in JIS."
 nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ("1" ?１)
 ("2" ?２)
 ("3" ?３)
 ("4" ?４)
 ("5" ?５)
 ("6" ?６)
 ("7" ?７)
 ("8" ?８)
 ("9" ?９)
 ("0" ?０)
 ("-" ?−)
 ("=" ?＝)
 ("`" ?‘)
 ("q" ?・)
 ("w" ?σ)
 ("e" ?ε)
 ("r" ?ρ)
 ("t" ?τ)
 ("y" ?υ)
 ("u" ?θ)
 ("i" ?ι)
 ("o" ?ο)
 ("p" ?π)
 ("[" ?\［)
 ("]" ?\］)
 ("a" ?α)
 ("s" ?σ)
 ("d" ?δ)
 ("f" ?φ)
 ("g" ?γ)
 ("h" ?η)
 ("j" ?ξ)
 ("k" ?κ)
 ("l" ?λ)
 (";" ?’)
 ("'" ?’)
 ("\\" ?＼)
 ("z" ?ζ)
 ("x" ?χ)
 ("c" ?ψ)
 ("v" ?ω)
 ("b" ?β)
 ("n" ?ν)
 ("m" ?μ)
 ("," ?, )
 ("." ?. )
 ("/" ?／)
  
 ("!" ?！)
 ("@" ?＠)
 ("#" ?＃)
 ("$" ?＃)
 ("%" ?％)
 ("^" ?＾)
 ("&" ?＆)
 ("*" ?＊)
 ("(" ?\（)
 (")" ?\）)
 ("_" ?＿)
 ("+" ?＋)
 ("~" ?￣)
 ("Q" ?−)
 ("W" ?Σ)
 ("E" ?Ε)
 ("R" ?Ρ)
 ("T" ?Τ)
 ("Y" ?Υ)
 ("U" ?Θ)
 ("I" ?Ι)
 ("O" ?Ο)
 ("P" ?Ρ)
 ("{" ?\｛)
 ("}" ?\｝)
 ("A" ?Α)
 ("S" ?Σ)
 ("D" ?Δ)
 ("F" ?Φ)
 ("G" ?Γ)
 ("H" ?Η)
 ("J" ?Ξ)
 ("K" ?Κ)
 ("L" ?Λ)
 (":" ?”)
 ("\"" ?”)
 ("|" ?｜)
 ("Z" ?Ζ)
 ("X" ?Χ)
 ("C" ?Ψ)
 ("V" ?Ω)
 ("B" ?Β)
 ("N" ?Ν)
 ("M" ?Μ)
 ("<" ?；)
 (">" ?：)
 ("?" ?？))

;;

(quail-define-package
 "greek" "Greek" ",FY" nil
 ",FEkkgmij\: Greek keyboard layout (ISO 8859-7)
--------------

In the right of ,Fk key is a combination key, where
 ,F4 acute
 ,F( diaresis

e.g.
 ,Fa + ,F4 -> ,F\
 ,Fi + ,F( -> ,Fz
 ,Fi + ,F( + ,F4 -> ,F@"
 nil t t t t nil nil nil nil nil t)

;; 1!  2@  3#  4$  5%  6^  7&  8*  9(  0)  -_  =+  `~
;;  ,F7/  ,FrS  ,FeE  ,FqQ  ,FtT  ,FuU  ,FhH  ,FiI  ,FoO  ,FpP  [{  ]}
;;   ,FaA  ,FsS  ,FdD  ,FvV  ,FcC  ,FgG  ,FnN  ,FjJ  ,FkK  ,F4(  '"  \|
;;    ,FfF  ,FwW  ,FxX  ,FyY  ,FbB  ,FmM  ,FlL  ,;  .:  /?  

(quail-define-rules
 ("1" ?1)
 ("2" ?2)
 ("3" ?3)
 ("4" ?4)
 ("5" ?5)
 ("6" ?6)
 ("7" ?7)
 ("8" ?8)
 ("9" ?9)
 ("0" ?0)
 ("-" ?-)
 ("=" ?=)
 ("`" ?`)
 ("q" ?,F7)
 ("w" ?,Fr)
 ("e" ?,Fe)
 ("r" ?,Fq)
 ("t" ?,Ft)
 ("y" ?,Fu)
 ("u" ?,Fh)
 ("i" ?,Fi)
 ("o" ?,Fo)
 ("p" ?,Fp)
 ("[" ?\[)
 ("]" ?\])
 ("a" ?,Fa)
 ("s" ?,Fs)
 ("d" ?,Fd)
 ("f" ?,Fv)
 ("g" ?,Fc)
 ("h" ?,Fg)
 ("j" ?,Fn)
 ("k" ?,Fj)
 ("l" ?,Fk)
 (";" ?,F4)
 ("'" ?')
 ("\\" ?\\)
 ("z" ?,Ff)
 ("x" ?,Fw)
 ("c" ?,Fx)
 ("v" ?,Fy)
 ("b" ?,Fb)
 ("n" ?,Fm)
 ("m" ?,Fl)
 ("," ?,)
 ("." ?.)
 ("/" ?/)
 
 ("!" ?!)
 ("@" ?@)
 ("#" ?#)
 ("$" ?$)
 ("%" ?%)
 ("^" ?^)
 ("&" ?&)
 ("*" ?*)
 ("(" ?\()
 (")" ?\))
 ("_" ?_)
 ("+" ?+)
 ("~" ?~)
 ("Q" ?,F/)
 ("W" ?,FS)
 ("E" ?,FE)
 ("R" ?,FQ)
 ("T" ?,FT)
 ("Y" ?,FU)
 ("U" ?,FH)
 ("I" ?,FI)
 ("O" ?,FO)
 ("P" ?,FP)
 ("{" ?{)
 ("}" ?})
 ("A" ?,FA)
 ("S" ?,FS)
 ("D" ?,FD)
 ("F" ?,FV)
 ("G" ?,FC)
 ("H" ?,FG)
 ("J" ?,FN)
 ("K" ?,FJ)
 ("L" ?,FK)
 (":" ?,F()
 ("\"" ?\")
 ("|" ?|)
 ("Z" ?,FF)
 ("X" ?,FW)
 ("C" ?,FX)
 ("V" ?,FY)
 ("B" ?,FB)
 ("N" ?,FM)
 ("M" ?,FL)
 ("<" ?\;)
 (">" ?:)
 ("?" ??)
 
 ("a;" ?,F\)
 ("e;" ?,F])
 ("h;" ?,F^)
 ("i;" ?,F_)
 ("o;" ?,F|)
 ("y;" ?,F})
 ("v;" ?,F~)
 ("A;" ?,F6)
 ("E;" ?,F8)
 ("H;" ?,F9)
 ("I;" ?,F:)
 ("O;" ?,F<)
 ("Y;" ?,F>)
 ("V;" ?,F?)
 ("i:" ?,Fz)
 ("y:" ?,F{)
 ("I:" ?,FZ)
 ("Y:" ?,F[)
 ("i:;" ?,F@)
 ("y:;" ?,F`))
