;;; korean.el --- Support for Korean

;; Copyright (C) 1995 Free Software Foundation, Inc.
;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.

;; Keywords: multilingual, Korean

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; For Korean, the character set KSC5601 is supported.

;;; Code:

(make-coding-system
 'coding-system-euc-korea 2 ?K
 "Coding-system of Korean EUC (Extended Unix Code)."
 '((ascii t) korean-ksc5601 nil nil
   nil ascii-eol ascii-cntl))

;; EUC-KR is an alias for EUC-KOREA.
(put 'coding-system-euc-kr 'coding-system 'coding-system-euc-korea)

(make-coding-system
 'coding-system-iso-2022-kr 2 ?k
 "Coding-System used for communication with mail in Korea."
 '(ascii (nil korean-ksc5601) nil nil
	 nil ascii-eol ascii-cntl seven locking-shift))

(register-input-method
 "Korean" '("quail-hangul" quail-use-package "quail/hangul"))
(register-input-method
 "Korean" '("quail-hangul3" quail-use-package "quail/hangul3"))
(register-input-method
 "Korean" '("quail-hanja" quail-use-package "quail/hanja"))
(register-input-method
 "Korean" '("quail-symbol-ksc" quail-use-package "quail/symbol-ksc"))
(register-input-method
 "Korean" '("quail-hanja-jis" quail-use-package "quail/hanja-jis"))

(defun setup-korean-environment ()
  (setq coding-category-iso-8-2 'coding-system-euc-korea)

  (set-coding-priority
   '(coding-category-iso-7
     coding-category-iso-8-2
     coding-category-iso-8-1))

  (setq-default buffer-file-coding-system 'coding-system-euc-korea)

  (setq default-input-method '("Korean" . "quail-hangul"))
  )

(set-language-info-alist
 "Korean" '((setup-function . setup-korean-environment)
	    (tutorial . "TUTORIAL.kr")
	    (charset . (korean-ksc5601))
	    (coding-system . (coding-system-euc-korea
			      coding-system-iso-2022-kr))
	    (documentation . t)
	    (sample-text . "Hangul (한글)	안녕하세요, 안녕하십니까")))

;;; korean.el ends here
