;; china-util.el -- utilities for Chinese

;; Copyright (C) 1995 Free Software Foundation, Inc.
;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

;; Hz/ZW encoding stuffs

;; HZ is an encoding method for Chinese character set GB2312 used
;; widely in Internet.  It is very similar to 7-bit environment of
;; ISO-2022.  The difference is that HZ uses the sequence "~{" and
;; "~}" for designating GB2312 and ASCII respectively, hence, it
;; doesn't uses ESC (0x1B) code.

;; ZW is another encoding method for Chinese character set GB2312.  It
;; encodes Chinese characters line by line by starting each line with
;; the sequence "zW".  It also uses only 7-bit as HZ.

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
(defvar hz/zw-start-gb (concat hz-gb-designnation "\\|" zw-start-gb))

(defvar decode-hz-line-continuation nil
  "Flag to tell if we should care line continuation convention of Hz.")

;;;###autoload
(defun decode-hz-region (beg end)
  "Decode HZ/ZW encoded text in the current region.
Return the length of resulting text."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)

      ;; We, at first, convert HZ/ZW to `coding-system-iso-2022-7',
      ;; then decode it.

      ;; "~\n" -> "\n"
      (goto-char (point-min))
      (while (search-forward "~" nil t)
	(if (= (following-char) ?\n) (delete-char -1))
	(if (not (eobp)) (forward-char 1)))

      ;; "^zW...\n" -> Chinese GB2312
      ;; "~{...~}"  -> Chinese GB2312
      (goto-char (point-min))
      (let ((chinese-found nil))
	(while (re-search-forward hz/zw-start-gb nil t)
	  (if (= (char-after (match-beginning 0)) ?z)
	      ;; ZW -> coding-system-iso-20227-7
	      (progn
		(delete-char -2)
		(insert iso2022-gb-designation)
		(end-of-line)
		(insert iso2022-ascii-designation))
	    ;; HZ -> coding-system-iso-20227-7
	    (delete-char -2)
	    (insert iso2022-gb-designation)
	    (let ((pos (save-excursion (end-of-line) (point))))
	      (if (search-forward hz-ascii-designnation pos t)
		  (replace-match iso2022-ascii-designation)
		(if (not decode-hz-line-continuation)
		    (insert iso2022-ascii-designation)))))
	  (setq chinese-found t))
	(if (or chinese-found
		(let ((enable-multibyte-characters nil))
		  ;; Here we check if the text contains EUC (China) codes.
		  ;; If any, we had better decode them also.
		  (goto-char (point-min))
		  (re-search-forward "[\240-\377]" nil t))) 
	    (decode-coding-region (point-min) (point-max)
				  'coding-system-euc-china)))

      ;; "~~" -> "~"
      (goto-char (point-min))
      (while (search-forward "~~" nil t) (delete-char -1))
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
	  (let ((enable-multibyte-characters nil)
		pos)
	    (goto-char (setq pos (match-beginning 0)))
	    (encode-coding-region pos (point-max) 'coding-system-iso-2022-7)
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

;;
(provide 'language/china-util)

;;; Local Variables:
;;; generated-autoload-file: "../loaddefs.el"
;;; End:
;;; china-util.el ends here
