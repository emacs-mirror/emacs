;;; hebrew.el --- Quail package for inputting Hebrew characters -*-coding: iso-2022-7bit;-*-

;; Copyright (C) 1998 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.

;; Keywords: mule, input method, Hebrew

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

;;; Code:

(require 'quail)

(quail-define-package
 "hebrew" "Hebrew" ",Hr(B" nil "Hebrew (ISO 8859-8) input method.

Based on Hebrew typewriter keys.
Hebrew letters are assigned to lowercases.
" nil t t t t nil nil nil nil nil t)

;;  1! 2@ 3# 4$ 5% 6^ 7& 8* 9( 0) _- += ~;
;;   /Q 'W ,Hw(BE ,Hx(BR ,H`(BT ,Hh(BY ,He(BU ,Ho(BI ,Hm(BO ,Ht(BP {[ {]
;;    ,Hy(BA ,Hc(BS ,Hb(BD ,Hk(BF ,Hr(BG ,Hi(BH ,Hg(BJ ,Hl(BK ,Hj(BL :,Hs(B ", |\
;;     ,Hf(BZ ,Hq(BX ,Ha(BC ,Hd(BV ,Hp(BB ,Hn(BN ,Hv(BM <,Hz(B >,Hu(B ?.
;;		         

(quail-define-rules
 ("`" ?\;)
 ("q" ?/)
 ("w" ?')
 ("e" ?,Hw(B)
 ("r" ?,Hx(B)
 ("t" ?,H`(B)
 ("y" ?,Hh(B)
 ("u" ?,He(B)
 ("i" ?,Ho(B)
 ("o" ?,Hm(B)
 ("p" ?,Ht(B)
 ("a" ?,Hy(B)
 ("s" ?,Hc(B)
 ("d" ?,Hb(B)
 ("f" ?,Hk(B)
 ("g" ?,Hr(B)
 ("h" ?,Hi(B)
 ("j" ?,Hg(B)
 ("k" ?,Hl(B)
 ("l" ?,Hj(B)
 (";" ?,Hs(B)
 ("z" ?,Hf(B)
 ("x" ?,Hq(B)
 ("c" ?,Ha(B)
 ("v" ?,Hd(B)
 ("b" ?,Hp(B)
 ("n" ?,Hn(B)
 ("m" ?,Hv(B)
 ("," ?,Hz(B)
 ("." ?,Hu(B)
 ("/" ?.)
 ("'" ?,))

;;; hebrew.el ends here
