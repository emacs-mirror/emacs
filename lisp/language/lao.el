;;; lao.el --- Support for Lao

;; Copyright (C) 1997 Free Software Foundation, Inc.
;; Copyright (C) 1997 Electrotechnical Laboratory, JAPAN.

;; Keywords: multilingual, Lao

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

(make-coding-system
 'lao 2 ?T
 "Coding-system used for ASCII(MSB=0) & LAO(MSB=1)."
 '((ascii t) (lao t) nil nil
   nil ascii-eol))

(register-input-method
 "Lao" '("quail-lao" quail-use-package "quail/lao"))

(register-input-method
 "Lao" '("quail-lrt" quail-use-package "quail/lrt"))

(defun setup-lao-environment ()
  (setq coding-category-iso-8-1 'lao)

  (set-coding-priority
   '(coding-category-iso-7
     coding-category-iso-8-1))

  (set-default buffer-file-coding-system 'iso-2022-7)

  (setq default-input-method '("Lao" . "quail-lao")))

(set-language-info-alist
 "Lao" '((setup-function . setup-lao-environment)
	 (charset . (lao))
	 (coding-system . (lao))
	 (documentation . t)
	 (sample-text . "Lao((1>RJRERG(B)            (1JP:R-04U1(B, 0(1"i1M-`0;Q190$[19ERG(B")))

(aset use-default-ascent ?(1;(B t)
(aset use-default-ascent ?(1=(B t)
(aset use-default-ascent ?(1?(B t)
(aset use-default-ascent ?(1B(B t)

;;; lao.el ends here
