;;; china-util.el --- utilities for Chinese  -*- coding: utf-8 -*-

;; Copyright (C) 1995, 2001-2017 Free Software Foundation, Inc.
;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021
;; Copyright (C) 2003
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H13PRO009

;; Keywords: mule, multilingual, Chinese

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

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
(defvar hz-gb-designation "~{")
;; ISO-2022 escape sequence to designate ASCII.
(defvar iso2022-ascii-designation "\e(B")
;; HZ escape sequence to designate ASCII.
(defvar hz-ascii-designation "~}")
;; Regexp of ZW sequence to start GB2312.
(defvar zw-start-gb "^zW")
;; Regexp for start of GB2312 in an encoding mixture of HZ and ZW.
(defvar hz/zw-start-gb
  (concat hz-gb-designation "\\|" zw-start-gb "\\|[^\0-\177]"))

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
	(push (decode-char 'eight-bit (+ i 128)) chars)
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

	;; We, at first, convert HZ/ZW to `iso-2022-7bit',
	;; then decode it.

	;; "~\n" -> "", "~~" -> "~"
	(goto-char (point-min))
	(while (search-forward "~" nil t)
	  (setq ch (following-char))
	  (cond ((= ch ?{)
		 (delete-region (1- (point)) (1+ (point)))
		 (setq pos (point))
		 (insert iso2022-gb-designation)
		 (if (looking-at "\\([!-}][!-~]\\)*")
		     (goto-char (match-end 0)))
		 (if (looking-at hz-ascii-designation)
		     (delete-region (match-beginning 0) (match-end 0)))
		 (insert iso2022-ascii-designation)
		 (decode-coding-region pos (point) 'iso-2022-7bit))

		((= ch ?~)
		 (delete-char 1))

		((and (= ch ?\n)
		      decode-hz-line-continuation)
		 (delete-region (1- (point)) (1+ (point))))

		(t
		 (forward-char 1)))))

      (- (point-max) (point-min)))))

;;;###autoload
(defun decode-hz-buffer ()
  "Decode HZ/ZW encoded text in the current buffer."
  (interactive)
  (decode-hz-region (point-min) (point-max)))

(defvar hz-category-table nil)

;;;###autoload
(defun encode-hz-region (beg end)
  "Encode the text in the current region to HZ.
Return the length of resulting text."
  (interactive "r")
  (unless hz-category-table
    (setq hz-category-table (make-category-table))
    (with-category-table hz-category-table
      (define-category ?c "hz encodable")
      (map-charset-chars #'modify-category-entry 'ascii ?c)
      (map-charset-chars #'modify-category-entry 'chinese-gb2312 ?c)))
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (with-category-table hz-category-table
	;; ~ -> ~~
	(goto-char (point-min))
	(while (search-forward "~" nil t) (insert ?~))

	;; ESC -> ESC ESC
	(goto-char (point-min))
	(while (search-forward "\e" nil t) (insert ?\e))

	;; Non-ASCII-GB2312 -> \uXXXX
	(goto-char (point-min))
	(while (re-search-forward "\\Cc" nil t)
	  (let ((ch (preceding-char)))
	    (delete-char -1)
	    (insert (format (if (< ch #x10000) "\\u%04X" "\\U%08X") ch))))

	;; Prefer chinese-gb2312 for Chinese characters.
	(put-text-property (point-min) (point-max) 'charset 'chinese-gb2312)
	(encode-coding-region (point-min) (point-max) 'iso-2022-7bit)

	;; ESC $ B ... ESC ( B  -> ~{ ... ~}
	;; ESC ESC -> ESC
	(goto-char (point-min))
	(while (search-forward "\e" nil t)
	  (if (= (following-char) ?\e)
	      ;; ESC ESC -> ESC
	      (delete-char 1)
	    (forward-char -1)
	    (if (looking-at iso2022-gb-designation)
		(progn
		  (delete-region (match-beginning 0) (match-end 0))
		  (insert hz-gb-designation)
		  (search-forward iso2022-ascii-designation nil 'move)
		  (delete-region (match-beginning 0) (match-end 0))
		  (insert hz-ascii-designation))))))
      (- (point-max) (point-min)))))

;;;###autoload
(defun encode-hz-buffer ()
  "Encode the text in the current buffer to HZ."
  (interactive)
  (encode-hz-region (point-min) (point-max)))

;;;###autoload
(defun post-read-decode-hz (len)
  (let ((pos (point))
	(buffer-modified-p (buffer-modified-p))
	last-coding-system-used)
    (prog1
	(decode-hz-region pos (+ pos len))
      (set-buffer-modified-p buffer-modified-p))))

;;;###autoload
(defun pre-write-encode-hz (from to)
  (let ((buf (current-buffer)))
    (set-buffer (generate-new-buffer " *temp*"))
    (if (stringp from)
	(insert from)
      (insert-buffer-substring buf from to))
    (let (last-coding-system-used)
      (encode-hz-region 1 (point-max)))
    nil))
;;
(provide 'china-util)

;;; china-util.el ends here
