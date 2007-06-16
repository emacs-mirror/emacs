;;; tv-util.el --- support for Tai Viet			-*- coding: utf-8 -*-

;; Copyright (C) 2007
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H13PRO009

;; Keywords: multilingual, Tai Viet, i18n

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

;;; Code

;; Regexp matching with a sequence of Tai Viet characters.
(defconst tai-viet-re
  (format "[\xaa80-\xaac2\xaadb-\xaadf]+"))

;; Char-table of information about glyph type of Tai Viet characters.
(defconst tai-viet-glyph-info
  (let ((table (make-char-table nil))
	(specials '((right-overhang . "ꪊꪋꪌꪍꪏꪓꪖꪜꪞꪡꪤꪨ")
		    (left-overhang . "ꫂ")
		    (combining-vowel . "ꪴꪰꪲꪳꪷꪸꪾ")
		    (combining-tone . "꪿꫁"))))
    ;; Set all TaiViet characters to `t'.
    (set-char-table-range table (cons #xaa80 #xaac2) t)
    (set-char-table-range table (cons #xaadb #xaadf) t)
    ;; Overwrite it for special characters.
    (dolist (elt specials)
      (let ((category (car elt))
	    (chars (cdr elt)))
	(dotimes (i (length chars))
	  (aset table (aref chars i) category))))
    table))

(defun tai-viet-compose-string (from to string)
  "Compose Tai Viet characters in STRING between indices FROM and TO."
  (let* ((ch (aref string from))
	 (info (aref tai-viet-glyph-info ch))
	 prev-info)
    (if (eq info 'non-spacing)
	(compose-string string from (1+ from) (string ch ?\t)))
    (setq from (1+ from) prev-info info)
    (while (and (< from to)
		(tai-viet-char-p (setq ch (aref string from))))
      (setq info (aref tai-viet-glyph-info ch))
      (if (and (eq info 'non-spacing)
	       (eq prev-info 'non-spacing))
	  (compose-string from (1+ from) (string ?\t ch)))
      (setq from (1+ from) prev-info info))
    (if (eq info 'right-overhang)
	(compose-string string (1- from) from (string ch ?\t)))
    from))

(defun tai-viet-compose-region (from to)
  "Compose Tai Viet characters in the region between FROM and TO."
  (decompose-region from to)
  (let ((normal-rule '(Br . Bl))
	(tone-rule '(tr . bl))
	(prev-viet nil)
	ch info pos components overhang)
    (while (< from to) 
      (or ch
	  (setq ch (char-after from)
		info (aref tai-viet-glyph-info ch)))
      (setq from (1+ from))
      (if (not info)
	  (setq prev-viet nil
		ch nil)
	(if (memq info '(combining-vowel combining-tone))
	    (progn
	      ;; Display this as a spacing glyph.
	      (compose-region (1- from) from (string ?\t ch))
	      (setq prev-viet t
		    ch nil))
	  (setq pos (1- from)
		components ch
		overhang (if (eq info 'right-overhang)
			     'right-overhang
			   (if (and (not prev-viet) (eq info 'left-overhang))
			       'left-overhang))
		prev-viet t
		ch nil)
	  (if (and (< from to)
		   (setq ch (char-after from)
			 info (aref tai-viet-glyph-info ch)))
	      (if (memq info '(combining-vowel combining-tone))
		  (progn
		    (setq components
			  (list components normal-rule ch)
			  from (1+ from)
			  ch nil)
		    (if (and (< from to)
			     (setq ch (char-after from)
				   info (aref tai-viet-glyph-info ch))
			     (eq info 'combining-tone))
			(setq components (nconc components
						(list tone-rule ch))
			      from (1+ from)))
		    (if (eq overhang 'left-overhang)
			(setq components (cons ?\t
					       (cons normal-rule components)))
		      (if (and (eq overhang 'right-overhang)
			       (>= from to))
			  (setq components (nconc components
						  (list normal-rule ?\t)))))
		    (compose-region pos from components))
		(if (eq overhang 'left-overhang)
		    (compose-region pos from (string ?\t components))))
	    (if (eq overhang 'left-overhang)
		(compose-region pos from (string ?\t components))
	      (if (and (eq overhang 'right-overhang) (>= from to))
		  (compose-region pos from (string components ?\t))))))))
    from))


;;;###autoload
(defun tai-viet-composition-function (pos &optional string)
  (let (to)
    (if string
	(progn
	  (if (string-match tai-viet-re string pos)
	      (tai-viet-compose-string pos (match-end 0) string)
	    (1+ pos)))
      (goto-char pos)
      (if (looking-at tai-viet-re)
	  (tai-viet-compose-region pos (match-end 0))
	(1+ pos)))))

;;
(provide 'tai-viet-util)
