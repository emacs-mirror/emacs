;;; korean.el --- Support for Korean

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; For Korean, the character set KSC5601 is supported.

;;; Code:

(make-coding-system
 'korean-iso-8bit 2 ?K
 "ISO 2022 based EUC encoding for Korean KSC5601 (MIME:EUC-KR)"
 '((ascii t) korean-ksc5601 nil nil
   nil ascii-eol ascii-cntl))

(define-coding-system-alias 'euc-kr 'korean-iso-8bit)
(define-coding-system-alias 'euc-korea 'korean-iso-8bit)

(make-coding-system
 'korean-iso-7bit-lock 2 ?k
 "ISO 2022 based 7-bit encoding for Korean KSC5601 (MIME:ISO-2022-KR)."
 '(ascii (nil korean-ksc5601) nil nil
	 nil ascii-eol ascii-cntl seven locking-shift nil nil nil nil nil
	 designation-bol))

(define-coding-system-alias 'iso-2022-kr 'korean-iso-7bit-lock)

(defun setup-korean-environment ()
  "Setup multilingual environment (MULE) for Korean."
  (interactive)
  (setup-english-environment)
  (setq coding-category-iso-8-2 'korean-iso-8bit)

  (set-coding-priority
   '(coding-category-iso-7
     coding-category-iso-8-2
     coding-category-iso-8-1))

  (set-default-coding-systems 'korean-iso-8bit)

  (setq default-input-method "korean-hangul"))

(set-language-info-alist
 "Korean" '((setup-function . setup-korean-environment)
	    (tutorial . "TUTORIAL.kr")
	    (charset . (korean-ksc5601))
	    (coding-system . (korean-iso-7bit-lock korean-iso-8bit))
	    (sample-text . "Hangul (한글)	안녕하세요, 안녕하십니까")
	    (documentation . t)))

;;; korean.el ends here
