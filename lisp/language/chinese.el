;;; chinese.el --- Support for Chinese

;; Copyright (C) 1995 Free Software Foundation, Inc.
;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.

;; Keywords: multilingual, Chinese

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

;; For Chinese, three character sets GB2312, BIG5, and CNS11643 are
;; supported.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Chinese (general)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(make-coding-system
 'coding-system-iso-2022-cn 2 ?C
 "Coding system ISO-2022-CN for Chinese (GB and CNS character sets)."
 '(ascii
   (nil chinese-gb2312 chinese-cns11643-1)
   (nil chinese-cns11643-2)
   (nil chinese-cns11643-3 chinese-cns11643-4 chinese-cns11643-5
	chinese-cns11643-6 chinese-cns11643-7)
   nil ascii-eol ascii-cntl seven locking-shift single-shift))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Chinese GB2312 (simplified) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(make-coding-system
 'coding-system-euc-china 2 ?C
 "Coding-system of Chinese EUC (so called GB Encoding)."
 '((ascii t) chinese-gb2312 chinese-sisheng nil
   nil ascii-eol ascii-cntl nil nil single-shift nil))

(make-coding-system
 'coding-system-hz 0 ?z
 "Codins-system of Hz/ZW used for Chinese (GB)."
 nil)
(put 'coding-system-hz 'post-read-conversion 'post-read-decode-hz)
(put 'coding-system-hz 'pre-write-conversion 'pre-write-encode-hz)

(defun post-read-decode-hz (len)
  (let ((pos (point)))
    (decode-hz-region pos (+ pos len))))

(defun pre-write-encode-hz (from to)
  (let ((buf (current-buffer))
	(work (get-buffer-create " *pre-write-encoding-work*")))
    (set-buffer work)
    (widen)
    (erase-buffer)
    (insert-buffer-substring buf from to)
    (encode-hz-region 1 (point-max))
    nil))

(register-input-method
 "Chinese-GB" '("quail-ccdospy" quail-use-package "quail/ccdospy"))
(register-input-method
 "Chinese-GB" '("quail-ctlau" quail-use-package "quail/ctlau"))
(register-input-method
 "Chinese-GB" '("quail-punct" quail-use-package "quail/punct"))
(register-input-method
 "Chinese-GB" '("quail-qj" quail-use-package "quail/qj"))
(register-input-method
 "Chinese-GB" '("quail-sw" quail-use-package "quail/sw"))
(register-input-method
 "Chinese-GB" '("quail-ziranma" quail-use-package "quail/ziranma"))
(register-input-method
 "Chinese-GB" '("quail-tonepy" quail-use-package "quail/tonepy"))
(register-input-method
 "Chinese-GB" '("quail-py" quail-use-package "quail/py"))

(defun setup-chinese-gb-environment ()
  (setq primary-language "Chinese-GB")

  (setq coding-category-iso-8-2 'coding-system-euc-china)
  (setq coding-category-iso-else 'coding-system-iso-2022-cn)

  (set-coding-priority
   '(coding-category-iso-7
     coding-category-iso-else
     coding-category-iso-8-2
     coding-category-big5
     coding-category-iso-8-1
     coding-category-internal
     ))

  (setq-default buffer-file-coding-system 'coding-system-euc-china)
  (set-terminal-coding-system 'coding-system-euc-china)
  (set-keyboard-coding-system 'coding-system-euc-china)

  (setq default-input-method '("Chinese-GB" . "quail-py"))
  )

(set-language-info-alist
 "Chinese" '((documentation . 
"Emacs provides three kinds of Chinese support: Chinese-GB,
Chinese-BIG5, and Chinese-CNS.  Please specify one of them to get more
information.")
	     (setup-function . setup-chinese-gb-environment)
	     (charset . (chinese-gb2312 chinese-sisheng))
	     (coding-system . (coding-system-euc-china
			       coding-system-hz
			       coding-system-iso-2022-cn))
	     (documentation . t)
	     (sample-text . "Chinese ($AVPND(B,$AFUM(;0(B,$A::So(B)	$ADc:C(B")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chinese BIG5 (traditional)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(make-coding-system
 'coding-system-big5 3 ?B
 "Coding-system of BIG5.")

;; Big5 font requires special encoding.
(define-ccl-program ccl-encode-big5-font
  `(0
    ;; In:  R0:chinese-big5-1 or chinese-big5-2
    ;;      R1:position code 1
    ;;      R2:position code 2
    ;; Out: R1:font code point 1
    ;;      R2:font code point 2
    ((r2 = ((((r1 - ?\x21) * 94) + r2) - ?\x21))
     (if (r0 == ,(charset-id 'chinese-big5-2)) (r2 += 6280))
     (r1 = ((r2 / 157) + ?\xA1))
     (r2 %= 157)
     (if (r2 < ?\x3F) (r2 += ?\x40) (r2 += ?\x62))))
  "CCL program to encode a Big5 code to code point of Big5 font.")

(setq font-ccl-encoder-alist
      (cons (cons "big5" ccl-encode-big5-font) font-ccl-encoder-alist))

(register-input-method
 "Chinese-BIG5" '("quail-qj-b5" quail-use-package "quail/qj-b5"))
(register-input-method
 "Chinese-BIG5" '("quail-zozy" quail-use-package "quail/zozy"))
(register-input-method
 "Chinese-BIG5" '("quail-tsangchi-b5" quail-use-package "quail/tsangchi-b5"))
(register-input-method
 "Chinese-BIG5" '("quail-py-b5" quail-use-package "quail/py-b5"))
(register-input-method
 "Chinese-BIG5" '("quail-quick-b5" quail-use-package "quail/quick-bt"))
(register-input-method
 "Chinese-BIG5" '("quail-etzy" quail-use-package "quail/etzy"))
(register-input-method
 "Chinese-BIG5" '("quail-ecdict" quail-use-package "quail/ecdict"))
(register-input-method
 "Chinese-BIG5" '("quail-ctlaub" quail-use-package "quail/ctlaub"))
(register-input-method
 "Chinese-BIG5" '("quail-array30" quail-use-package "quail/array30"))
(register-input-method
 "Chinese-BIG5" '("quail-4corner" quail-use-package "quail/4corner"))

(defun setup-chinese-big5-environment ()
  (setq primary-language "Chinese-BIG5")

  (setq coding-category-big5 'coding-system-big5)
  (setq coding-category-iso-else 'coding-system-iso-2022-cn)

  (set-coding-priority
   '(coding-category-iso-7
     coding-category-iso-else
     coding-category-big5))

  (setq-default buffer-file-coding-system 'coding-system-big5)
  (set-terminal-coding-system 'coding-system-big5)
  (set-keyboard-coding-system 'coding-system-big5)

  (setq default-input-method '("Chinese-BIG5" . "quail-py-b5"))
  )

(set-language-info-alist
 "Chinese-BIG5" '((setup-function . setup-chinese-big5-environment)
		  (charset . (chinese-big5-1 chinese-big5-2))
		  (coding-system . (coding-system-big5
				    coding-system-iso-2022-cn))
		  (documentation . t)
		  (sample-text . "Cantonese ($(0GnM$(B,$(0N]0*Hd(B)	$(0*/=((B, $(0+$)p(B")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chinese CNS11643 (traditional)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(register-input-method
 "Chinese-CNS" '("quail-quick-cns" quail-use-package "quail/quick-cns"))
(register-input-method
 "Chinese-CNS" '("quail-tsangchi-cns" quail-use-package "quail/tsangchi-cns"))

(defun setup-chinese-cns-environment ()
  (setq primary-language "Chinese-CNS")

  (setq coding-category-iso-else 'coding-system-iso-2022-cn)

  (set-coding-priority
   '(coding-category-iso-7
     coding-category-iso-else
     coding-category-big5))

  (setq-default buffer-file-coding-system 'coding-system-iso-2022-7)
  (set-terminal-coding-system 'coding-system-iso-2022-7)
  (set-keyboard-coding-system 'coding-system-iso-2022-7)

  (setq default-input-method '("Chinese-CNS" . "quail-py-cns"))
  )

(set-language-info-alist
 "Chinese-CNS" '((setup-function . setup-chinese-cns-environment)
		 (charset . (chinese-cns11643-1 chinese-cns11643-2
			     chinese-cns11643-3 chinese-cns11643-4
			     chinese-cns11643-5 chinese-cns11643-6
			     chinese-cns11643-7))
		 (coding-system . (coding-system-iso-2022-cn))
		 (documentation . t)))

;;; chinese.el ends here
