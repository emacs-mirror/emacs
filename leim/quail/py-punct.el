;;; py-punct.el --- Quail packages for Chinese (pinyin + extra symbols)  -*-coding: iso-2022-7bit;-*-

;; Copyright (C) 1997 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 2001 Free Software Foundation, Inc.

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'quail)

(load "quail/PY")
(load "quail/Punct")

(quail-define-package
 "chinese-py-punct" "Chinese-GB" "拼符"
 t
 "汉字输入 拼音方案 and `v' for 标点符号输入

This is the combination of the input methods `chinese-py' and `chinese-punct'.
You can enter normal Chinese characters by the same way as `chinese-py'.
And, you can enter symbols by typing `v' followed by any key sequences
defined in `chinese-punct'.

For instance, typing `v' and `%' insert `％'.
")

(setcar (nthcdr 2 quail-current-package)
	(copy-sequence (nth 2 (assoc "chinese-py" quail-package-alist))))

(quail-defrule "v" (nth 2 (assoc "chinese-punct" quail-package-alist)))

(load "quail/TONEPY")

(quail-define-package
 "chinese-tonepy-punct" "Chinese-GB" "拼符"
 t
 "汉字输入 带调拼音方案 and `v' for 标点符号输入

This is the combination of the input methods `chinese-tonepy' and
`chinese-punct'.

You can enter normal Chinese characters by the same way as
`chinese-tonepy'.  And, you can enter symbols by typing `v' followed
by any key sequences defined in `chinese-punct'.

For instance, typing `v' and `%' insert `％'.
")

(setcar (nthcdr 2 quail-current-package)
	(copy-sequence (nth 2 (assoc "chinese-tonepy" quail-package-alist))))

(quail-defrule "v" (nth 2 (assoc "chinese-punct" quail-package-alist)))

;;; arch-tag: 941fd2ac-2d26-4b0e-8458-b5e485c5aa7d
;;; py-punct.el ends here
