;;; japanese.el --- Japanese support

;; Copyright (C) 1995 Free Software Foundation, Inc.
;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.

;; Keywords: multilingual, Japanese

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

;; For Japanese, character sets JISX0201, JISX0208, JISX0212 are
;; supported.

;;; Code:

(make-coding-system
 'coding-system-sjis 1 ?S
 "Coding-system of Shift-JIS used in Japan." t)

;; ISO-2022-JP and JUNET are aliases for ISO-2022-7.
(put 'coding-system-iso-2022-jp 'coding-system 'coding-system-iso-2022-7)
(put 'coding-system-junet 'coding-system 'coding-system-iso-2022-7)

(make-coding-system
 'coding-system-old-jis 2 ?J
 "Coding-system used for old jis terminal."
 '((ascii t) nil nil nil
   short ascii-eol ascii-cntl seven nil nil use-roman use-oldjis))

(make-coding-system
 'coding-system-euc-japan 2 ?E
 "Coding-system of Japanese EUC (Extended Unix Code)."
 '(ascii japanese-jisx0208 katakana-jisx0201 japanese-jisx0212
	 short ascii-eol ascii-cntl nil nil single-shift))

(register-input-method
 "Japanese" '("quail-ja" quail-use-package "quail/japanese"))

(register-input-method
 "Japanese" '("quail-ja-hiragana" quail-use-package "quail/japanese"))

(defun setup-japanese-environment ()
  (interactive)
  (setq coding-category-iso-8-2 'coding-system-euc-japan)

  (set-coding-priority
   '(coding-category-iso-7
     coding-category-iso-8-2
     coding-category-sjis
     coding-category-iso-8-1
     coding-category-iso-else
     coding-category-internal))

  (if (eq system-type 'ms-dos)
      (progn
	(setq-default buffer-file-coding-system 'coding-system-sjis)
	(set-terminal-coding-system 'coding-system-sjis)
	(set-keyboard-coding-system 'coding-system-sjis)
	(setq default-process-coding-system
	      '(coding-system-sjis-dos . coding-system-sjis-dos)))
    (setq-default buffer-file-coding-system 'coding-system-iso-2022-jp)
    (set-terminal-coding-system 'coding-system-iso-2022-jp)
    (set-keyboard-coding-system 'coding-system-iso-2022-jp))

  (set-default-input-method "Japanese" "quail-ja")
  )

(set-language-info-alist
 "Japanese" '((setup-function . setup-japanese-environment)
	      (tutorial . "TUTORIAL.jp")
	      (charset . (japanese-jisx0208 japanese-jisx0208-1978
			  japanese-jisx0212 latin-jisx0201
			  katakana-jisx0201))
	      (coding-system . (coding-system-euc-japan
				coding-system-sjis
				coding-system-old-jis
				coding-system-iso-2022-jp))
	      (documentation . t)
	      (sample-text . "Japanese (日本語)		こんにちは, ｺﾝﾆﾁﾊ")))

;;; japanese.el ends here
