;;; pypunct-b5.el --- Quail packages for Chinese (pinyin + extra symbols) -*-coding: iso-2022-7bit;-*-

;; Copyright (C) 1997 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.

;; Author: Ken'ichi HANDA <handa@etl.go.jp>

;; Keywords: multilingual, input method, Chienese

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

(load "quail/PY-b5")
(load "quail/Punct-b5")

(quail-define-package
 "chinese-py-punct-b5" "Chinese-BIG5" "$AF47{(B"
 t
 "$(0&d'GTT&,!J3<5x!K(B and `v' for $(0O:X5>KHATT&,(B

This is the combination of the input method `chinese-py-b5' and
`chinese-punct-b5'.

You can enter normal Chinese characters by the same way as `chinese-py-b5'.
And, you can enter symbols by typing `v' followed by any key sequences
defined in `chinese-punct-b5'.

For instance, typing `v' and `%' insert `$(0"h(B'.
")

(setcar (nthcdr 2 quail-current-package)
	(nth 2 (assoc "chinese-py-b5" quail-package-alist)))

(quail-defrule "v" (nth 2 (assoc "chinese-punct-b5" quail-package-alist)))

;;; pypunct-b5.el ends here
