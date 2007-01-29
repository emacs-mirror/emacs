;;; china-util.el --- utilities for Chinese  -*- coding: iso-2022-7bit -*-

;; Copyright (C) 1995, 2001, 2002, 2003, 2004, 2005, 2006, 2007
;;   Free Software Foundation, Inc.
;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021

;; Keywords: mule, multilingual, Chinese

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

;; Hz/ZW/EUC-TW encoding stuff

;; HZ is an encoding method for Chinese character set GB2312 used
;; widely in Internet.  It is very similar to 7-bit environment of
;; ISO-2022.  The difference is that HZ uses the sequence "~{" and
;; "~}" for designating GB2312 and ASCII respectively, hence, it
;; doesn't uses ESC (0x1B) code.

;; ZW is another encoding method for Chinese character set GB2312.  It
;; encodes Chinese characters line by line by starting each line with
;; the sequence "zW".  It also uses only 7-bit as HZ.

;; EUC-TW is similar to EUC-KS or EUC-JP.  Its main character set is
;; plane 1 of CNS 11643; characters of planes 2 to 7 are accessed with
;; a single shift escape followed by three bytes: the first gives the
;; plane, the second and third the character code.  Note that characters
;; of plane 1 are (redundantly) accessible with a single shift escape
;; also.

;; ISO-2022 escape sequence to designate GB2312.
(defvar iso2022-gb-designation "\e$A")
;; HZ escape sequence to designate GB2312.
(defvar hz-gb-designnation "~{")
;; ISO-2022 escape sequence to designate ASCII.
(defvar iso2022-ascii-designation "\e(B")
;; HZ escape sequence to designate ASCII.
(defvar hz-ascii-designnation "~}")
;; Regexp of ZW sequence to start GB2312.
(defvar zw-start-gb "^zW")
;; Regexp for start of GB2312 in an encoding mixture of HZ and ZW.
(defvar hz/zw-start-gb
  (concat hz-gb-designnation "\\|" zw-start-gb "\\|[^\0-\177]"))

(defvar decode-hz-line-continuation nil
  "Flag to tell if we should care line continuation convention of Hz.")

(defconst hz-set-msb-table
  (eval-when-compile
    (let ((chars nil)
	  (i 0))
      (while (< i 33)
	(push i chars)
	(setq i (1+ i)))
      (while (< i 127)
	(push (+ i 128) chars)
	(setq i (1+ i)))
      (apply 'string (nreverse chars)))))

;;;###autoload
(defun decode-hz-region (beg end)
  "Decode HZ/ZW encoded text in the current region.
Return the length of resulting text."
  (interactive "r")
  (save-excursion
    (save-restriction
      (let (pos ch)
	(narrow-to-region beg end)

	;; We, at first, convert HZ/ZW to `euc-china',
	;; then decode it.

	;; "~\n" -> "\n", "~~" -> "~"
	(goto-char (point-min))
	(while (search-forward "~" nil t)
	  (setq ch (following-char))
	  (if (or (= ch ?\n) (= ch ?~)) (delete-char -1)))

	;; "^zW...\n" -> Chinese GB2312
	;; "~{...~}"  -> Chinese GB2312
	(goto-char (point-min))
	(setq beg nil)
	(while (re-search-forward hz/zw-start-gb nil t)
	  (setq pos (match-beginning 0)
		ch (char-after pos))
	  ;; Record the first position to start conversion.
	  (or beg (setq beg pos))
	  (end-of-line)
	  (setq end (point))
	  (if (>= ch 128)		; 8bit GB2312
	      nil
	    (goto-char pos)
	    (delete-char 2)
	    (setq end (- end 2))
	    (if (= ch ?z)			; ZW -> euc-china
		(progn
		  (translate-region (point) end hz-set-msb-table)
		  (goto-char end))
	      (if (search-forward hz-ascii-designnation
				  (if decode-hz-line-continuation nil end)
				  t)
		  (delete-char -2))
	      (setq end (point))
	      (translate-region pos (point) hz-set-msb-table))))
	(if beg
	    (decode-coding-region beg end 'euc-china)))
      (- (point-max) (point-min)))))

;;;###autoload
(defun decode-hz-buffer ()
  "Decode HZ/ZW encoded text in the current buffer."
  (interactive)
  (decode-hz-region (point-min) (point-max)))

;;;###autoload
(defun encode-hz-region (beg end)
  "Encode the text in the current region to HZ.
Return the length of resulting text."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)

      ;; "~" -> "~~"
      (goto-char (point-min))
      (while (search-forward "~" nil t)	(insert ?~))

      ;; Chinese GB2312 -> "~{...~}"
      (goto-char (point-min))
      (if (re-search-forward "\\cc" nil t)
	  (let (pos)
	    (goto-char (setq pos (match-beginning 0)))
	    (encode-coding-region pos (point-max) 'iso-2022-7bit)
	    (goto-char pos)
	    (while (search-forward iso2022-gb-designation nil t)
	      (delete-char -3)
	      (insert hz-gb-designnation))
	    (goto-char pos)
	    (while (search-forward iso2022-ascii-designation nil t)
	      (delete-char -3)
	      (insert hz-ascii-designnation))))
      (- (point-max) (point-min)))))

;;;###autoload
(defun encode-hz-buffer ()
  "Encode the text in the current buffer to HZ."
  (interactive)
  (encode-hz-region (point-min) (point-max)))

;; The following sets up a translation table (big5-to-cns) from Big 5
;; to CNS encoding, using some auxiliary functions to make the code
;; more readable.

;; Many kudos to Himi!  The used code has been adapted from his
;; mule-ucs package.

(eval-when-compile
(defun big5-to-flat-code (num)
  "Convert NUM in Big 5 encoding to a `flat code'.
0xA140 will be mapped to position 0, 0xA141 to position 1, etc.
There are no gaps in the flat code."

  (let ((hi (/ num 256))
        (lo (% num 256)))
    (+ (* 157 (- hi #xa1))
       (- lo (if (>= lo #xa1) 98 64)))))

(defun flat-code-to-big5 (num)
  "Convert NUM from a `flat code' to Big 5 encoding.
This is the inverse function of `big5-to-flat-code'."

  (let ((hi (/ num 157))
        (lo (% num 157)))
    (+ (* 256 (+ hi #xa1))
       (+ lo (if (< lo 63) 64 98)))))

(defun euc-to-flat-code (num)
  "Convert NUM in EUC encoding (in GL representation) to a `flat code'.
0x2121 will be mapped to position 0, 0x2122 to position 1, etc.
There are no gaps in the flat code."

  (let ((hi (/ num 256))
        (lo (% num 256)))
    (+ (* 94 (- hi #x21))
       (- lo #x21))))

(defun flat-code-to-euc (num)
  "Convert NUM from a `flat code' to EUC encoding (in GL representation).
The inverse function of `euc-to-flat-code'.  The high and low bytes are
returned in a list."

  (let ((hi (/ num 94))
        (lo (% num 94)))
    (list (+ hi #x21) (+ lo #x21))))

(defun expand-euc-big5-alist (alist)
  "Create a translation table and fills it with data given in ALIST.
Elements of ALIST can be either given as

  ((euc-charset . startchar) . (big5-range-begin . big5-range-end))

or as

  (euc-character . big5-charcode)

The former maps a range of glyphs in an EUC charset (where STARTCHAR
is in GL representation) to a certain range of Big 5 encoded
characters, the latter maps a single glyph.  Glyphs which can't be
mapped will be represented with the byte 0xFF.

The return value is the filled translation table."

  (let ((chartable (make-char-table 'translation-table #xFF))
        char
        big5
        i
        end
        codepoint
        charset)
    (dolist (elem alist)
      (setq char (car elem)
            big5 (cdr elem))
      (cond ((and (consp char)
                  (consp big5))
	     (setq i (big5-to-flat-code (car big5))
		   end (big5-to-flat-code (cdr big5))
		   codepoint (euc-to-flat-code (cdr char))
		   charset (car char))
	     (while (>= end i)
	       (aset chartable
		     (decode-big5-char (flat-code-to-big5 i))
		     (apply (function make-char)
			    charset
			    (flat-code-to-euc codepoint)))
	       (setq i (1+ i)
		     codepoint (1+ codepoint))))
            ((and (char-valid-p char)
                  (numberp big5))
	     (setq i (decode-big5-char big5))
	     (aset chartable i char))
            (t
             (error "Unknown slot type: %S" elem))))
    ;; the return value
    chartable)))

;; All non-CNS encodings are commented out.

(define-translation-table 'big5-to-cns
  (eval-when-compile
  (expand-euc-big5-alist
   '(
     ;; Symbols
     ((chinese-cns11643-1 . #x2121) . (#xA140 . #xA1F5))
     (?$(G"X(B . #xA1F6)
     (?$(G"W(B . #xA1F7)
     ((chinese-cns11643-1 . #x2259) . (#xA1F8 . #xA2AE))
     ((chinese-cns11643-1 . #x2421) . (#xA2AF . #xA3BF))
     ;; Control codes (vendor dependent)
     ((chinese-cns11643-1 . #x4221) . (#xA3C0 . #xA3E0))
     ;; Level 1 Ideographs
     ((chinese-cns11643-1 . #x4421) . (#xA440 . #xACFD))
     (?$(GWS(B . #xACFE)
     ((chinese-cns11643-1 . #x5323) . (#xAD40 . #xAFCF))
     ((chinese-cns11643-1 . #x5754) . (#xAFD0 . #xBBC7))
     ((chinese-cns11643-1 . #x6B51) . (#xBBC8 . #xBE51))
     (?$(GkP(B . #xBE52)
     ((chinese-cns11643-1 . #x6F5C) . (#xBE53 . #xC1AA))
     ((chinese-cns11643-1 . #x7536) . (#xC1AB . #xC2CA))
     (?$(Gu5(B . #xC2CB)
     ((chinese-cns11643-1 . #x7737) . (#xC2CC . #xC360))
     ((chinese-cns11643-1 . #x782E) . (#xC361 . #xC3B8))
     (?$(Gxe(B . #xC3B9)
     (?$(Gxd(B . #xC3BA)
     ((chinese-cns11643-1 . #x7866) . (#xC3BB . #xC455))
     (?$(Gx-(B . #xC456)
     ((chinese-cns11643-1 . #x7962) . (#xC457 . #xC67E))
     ;; Symbols
     ((chinese-cns11643-1 . #x2621) . (#xC6A1 . #xC6BE))
     ;; Radicals
     (?$(G'#(B . #xC6BF)
     (?$(G'$(B . #xC6C0)
     (?$(G'&(B . #xC6C1)
     (?$(G'((B . #xC6C2)
     (?$(G'-(B . #xC6C3)
     (?$(G'.(B . #xC6C4)
     (?$(G'/(B . #xC6C5)
     (?$(G'4(B . #xC6C6)
     (?$(G'7(B . #xC6C7)
     (?$(G':(B . #xC6C8)
     (?$(G'<(B . #xC6C9)
     (?$(G'B(B . #xC6CA)
     (?$(G'G(B . #xC6CB)
     (?$(G'N(B . #xC6CC)
     (?$(G'S(B . #xC6CD)
     (?$(G'T(B . #xC6CE)
     (?$(G'U(B . #xC6CF)
     (?$(G'Y(B . #xC6D0)
     (?$(G'Z(B . #xC6D1)
     (?$(G'a(B . #xC6D2)
     (?$(G'f(B . #xC6D3)
     (?$(G()(B . #xC6D4)
     (?$(G(*(B . #xC6D5)
     (?$(G(c(B . #xC6D6)
     (?$(G(l(B . #xC6D7)
     ;; Diacritical Marks
     ; ((japanese-jisx0208 . #x212F) . (#xC6D8 . #xC6D9))
     ;; Japanese Kana Supplement
     ; ((japanese-jisx0208 . #x2133) . (#xC6DA . #xC6E3))
     ;; Japanese Hiragana
     ; ((japanese-jisx0208 . #x2421) . (#xC6E7 . #xC77A))
     ;; Japanese Katakana
     ; ((japanese-jisx0208 . #x2521) . (#xC77B . #xC7F2))
     ;; Cyrillic Characters
     ; ((japanese-jisx0208 . #x2721) . (#xC7F3 . #xC854))
     ; ((japanese-jisx0208 . #x2751) . (#xC855 . #xC875))
     ;; Special Chinese Characters
     (?$(J!#(B . #xC879)
     (?$(J!$(B . #xC87B)
     (?$(J!*(B . #xC87D)
     (?$(J!R(B . #xC8A2)

     ;; JIS X 0208 NOT SIGN (cf. U+00AC)
     ; (?$B"L(B . #xC8CD)
     ;; JIS X 0212 BROKEN BAR (cf. U+00A6)
     ; (?$(D"C(B . #xC8CE)

     ;; GB 2312 characters
     ; (?$A!d(B . #xC8CF)
     ; (?$A!e(B . #xC8D0)
        ;;;;; C8D1 - Japanese `($B3t(B)'
     ; (?$A!m(B . #xC8D2)
        ;;;;; C8D2 - Tel.

     ;; Level 2 Ideographs
     ((chinese-cns11643-2 . #x2121) . (#xC940 . #xC949))
     (?$(GDB(B . #xC94A);; a duplicate of #xA461
     ((chinese-cns11643-2 . #x212B) . (#xC94B . #xC96B))
     ((chinese-cns11643-2 . #x214D) . (#xC96C . #xC9BD))
     (?$(H!L(B . #xC9BE)
     ((chinese-cns11643-2 . #x217D) . (#xC9BF . #xC9EC))
     ((chinese-cns11643-2 . #x224E) . (#xC9ED . #xCAF6))
     (?$(H"M(B . #xCAF7)
     ((chinese-cns11643-2 . #x2439) . (#xCAF8 . #xD6CB))
     (?$(H>c(B . #xD6CC)
     ((chinese-cns11643-2 . #x3770) . (#xD6CD . #xD779))
     (?$(H?j(B . #xD77A)
     ((chinese-cns11643-2 . #x387E) . (#xD77B . #xDADE))
     (?$(H7o(B . #xDADF)
     ((chinese-cns11643-2 . #x3E64) . (#xDAE0 . #xDBA6))
     ((chinese-cns11643-2 . #x3F6B) . (#xDBA7 . #xDDFB))
     (?$(HAv(B . #xDDFC);; a duplicate of #xDCD1
     ((chinese-cns11643-2 . #x4424) . (#xDDFD . #xE8A2))
     ((chinese-cns11643-2 . #x554C) . (#xE8A3 . #xE975))
     ((chinese-cns11643-2 . #x5723) . (#xE976 . #xEB5A))
     ((chinese-cns11643-2 . #x5A29) . (#xEB5B . #xEBF0))
     (?$(HUK(B . #xEBF1)
     ((chinese-cns11643-2 . #x5B3F) . (#xEBF2 . #xECDD))
     (?$(HW"(B . #xECDE)
     ((chinese-cns11643-2 . #x5C6A) . (#xECDF . #xEDA9))
     ((chinese-cns11643-2 . #x5D75) . (#xEDAA . #xEEEA))
     (?$(Hd/(B . #xEEEB)
     ((chinese-cns11643-2 . #x6039) . (#xEEEC . #xF055))
     (?$(H]t(B . #xF056)
     ((chinese-cns11643-2 . #x6243) . (#xF057 . #xF0CA))
     (?$(HZ((B . #xF0CB)
     ((chinese-cns11643-2 . #x6337) . (#xF0CC . #xF162))
     ((chinese-cns11643-2 . #x6430) . (#xF163 . #xF16A))
     (?$(Hga(B . #xF16B)
     ((chinese-cns11643-2 . #x6438) . (#xF16C . #xF267))
     (?$(Hi4(B . #xF268)
     ((chinese-cns11643-2 . #x6573) . (#xF269 . #xF2C2))
     ((chinese-cns11643-2 . #x664E) . (#xF2C3 . #xF374))
     ((chinese-cns11643-2 . #x6762) . (#xF375 . #xF465))
     ((chinese-cns11643-2 . #x6935) . (#xF466 . #xF4B4))
     (?$(HfM(B . #xF4B5)
     ((chinese-cns11643-2 . #x6962) . (#xF4B6 . #xF4FC))
     ((chinese-cns11643-2 . #x6A4C) . (#xF4FD . #xF662))
     (?$(HjK(B . #xF663)
     ((chinese-cns11643-2 . #x6C52) . (#xF664 . #xF976))
     ((chinese-cns11643-2 . #x7167) . (#xF977 . #xF9C3))
     (?$(Hqf(B . #xF9C4)
     (?$(Hr4(B . #xF9C5)
     (?$(Hr@(B . #xF9C6)
     ((chinese-cns11643-2 . #x7235) . (#xF9C7 . #xF9D1))
     ((chinese-cns11643-2 . #x7241) . (#xF9D2 . #xF9D5))

     ;; Additional Ideographs
     (?$(IC7(B . #xF9D6)
     (?$(IOP(B . #xF9D7)
     (?$(IDN(B . #xF9D8)
     (?$(IPJ(B . #xF9D9)
     (?$(I,](B . #xF9DA)
     (?$(I=~(B . #xF9DB)
     (?$(IK\(B . #xF9DC)
    )
  ))
)

;;
(provide 'china-util)

;;; arch-tag: 5a47b084-b9ac-420e-8191-70c5b3a14836
;;; china-util.el ends here
