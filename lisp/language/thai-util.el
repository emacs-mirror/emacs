;;; thai-util.el --- utilities for Thai -*- coding: utf-8; -*-

;; Copyright (C) 2000-2020 Free Software Foundation, Inc.
;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021

;; Keywords: mule, multilingual, Thai, i18n

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

(defvar thai-auto-composition-mode)

;; Setting information of Thai characters.

(defconst thai-category-table (make-category-table))
(define-category ?c "Thai consonant" thai-category-table)
(define-category ?v "Thai upper/lower vowel" thai-category-table)
(define-category ?t "Thai tone mark" thai-category-table)
(define-category ?u "Thai tone mark and upper sign" thai-category-table)
(define-category ?I "THAI CHARACTER SARA I" thai-category-table)
(define-category ?U "THAI CHARACTER THANTHAKHAT" thai-category-table)

;; The general composing rules are as follows:
;;
;;                          T
;;       V        U         V                  U
;; CV -> C, CU -> C, CVT -> C, Cv -> C, CvU -> C
;;                                   v         v
;;
;; where C: consonant, V: vowel upper, v: vowel lower,
;;       T: tone mark, U: tone mark and upper sign.
;; Special rule: The sign `์' can be put on the vowel `ิ'.


(defvar thai-composition-pattern
  "\\cc\\(\\cu\\|\\cI\\cU\\|\\cv\\ct?\\)\\|\\cv\\ct\\|\\cI\\cU"
  "Regular expression matching a Thai composite sequence.")

(let ((l '((?ก consonant)	; 0xA1
	   (?ข consonant)	; 0xA2
	   (?ฃ consonant)	; 0xA3
	   (?ค consonant)	; 0xA4
	   (?ฅ consonant)	; 0xA5
	   (?ฆ consonant)	; 0xA6
	   (?ง consonant)	; 0xA7
	   (?จ consonant)	; 0xA8
	   (?ฉ consonant)	; 0xA9
	   (?ช consonant)	; 0xAA
	   (?ซ consonant)	; 0xAB
	   (?ฌ consonant)	; 0xAC
	   (?ญ consonant)	; 0xAD
	   (?ฎ consonant)	; 0xAE
	   (?ฏ consonant)	; 0xAF
	   (?ฐ consonant)	; 0xB0
	   (?ฑ consonant)	; 0xB1
	   (?ฒ consonant)	; 0xB2
	   (?ณ consonant)	; 0xB3
	   (?ด consonant)	; 0xB4
	   (?ต consonant)	; 0xB5
	   (?ถ consonant)	; 0xB6
	   (?ท consonant)	; 0xB7
	   (?ธ consonant)	; 0xB8
	   (?น consonant)	; 0xB9
	   (?บ consonant)	; 0xBA
	   (?ป consonant)	; 0xBB
	   (?ผ consonant)	; 0xBC
	   (?ฝ consonant)	; 0xBD
	   (?พ consonant)	; 0xBE
	   (?ฟ consonant)	; 0xBF
	   (?ภ consonant)	; 0xC0
	   (?ม consonant)	; 0xC1
	   (?ย consonant)	; 0xC2
	   (?ร consonant)	; 0xC3
	   (?ฤ vowel-base)	; 0xC4
	   (?ล consonant)	; 0xC5
	   (?ฦ vowel-base)	; 0xC6
	   (?ว consonant)	; 0xC7
	   (?ศ consonant)	; 0xC8
	   (?ษ consonant)	; 0xC9
	   (?ส consonant)	; 0xCA
	   (?ห consonant)	; 0xCB
	   (?ฬ consonant)	; 0xCC
	   (?อ consonant)	; 0xCD
	   (?ฮ consonant)	; 0xCE
	   (?ฯ special)	; 0xCF
	   (?ะ vowel-base)	; 0xD0
	   (?ั vowel-upper)	; 0xD1
	   (?า vowel-base)	; 0xD2
	   (?ำ vowel-base)	; 0xD3
	   (?ิ vowel-upper)	; 0xD4
	   (?ี vowel-upper)	; 0xD5
	   (?ึ vowel-upper)	; 0xD6
	   (?ื vowel-upper)	; 0xD7
	   (?ุ vowel-lower)	; 0xD8
	   (?ู vowel-lower)	; 0xD9
	   (?ฺ vowel-lower)	; 0xDA
	   (?฻ invalid)	; 0xDA
	   (?฼ invalid)	; 0xDC
	   (?฽ invalid)	; 0xDC
	   (?฾ invalid)	; 0xDC
	   (?฿ special)	; 0xDF
	   (?เ vowel-base)	; 0xE0
	   (?แ vowel-base)	; 0xE1
	   (?โ vowel-base)	; 0xE2
	   (?ใ vowel-base)	; 0xE3
	   (?ไ vowel-base)	; 0xE4
	   (?ๅ vowel-base)	; 0xE5
	   (?ๆ special)	; 0xE6
	   (?็ sign-upper)	; 0xE7
	   (?่ tone)		; 0xE8
	   (?้ tone)		; 0xE9
	   (?๊ tone)		; 0xEA
	   (?๋ tone)		; 0xEB
	   (?์ sign-upper)	; 0xEC
	   (?ํ sign-upper)	; 0xED
	   (?๎ sign-upper)	; 0xEE
	   (?๏ special)	; 0xEF
	   (?๐ special)	; 0xF0
	   (?๑ special)	; 0xF1
	   (?๒ special)	; 0xF2
	   (?๓ special)	; 0xF3
	   (?๔ special)	; 0xF4
	   (?๕ special)	; 0xF5
	   (?๖ special)	; 0xF6
	   (?๗ special)	; 0xF7
	   (?๘ special)	; 0xF8
	   (?๙ special)	; 0xF9
	   (?๚ special)	; 0xFA
	   (?๛ special)	; 0xFB
	   (?๜ invalid)	; 0xFC
	   (?๝ invalid)	; 0xFD
	   (?๞ invalid)	; 0xFE
	   ))
      elm)
  (while l
    (setq elm (car l) l (cdr l))
    (let ((char (car elm))
	  (ptype (nth 1 elm)))
      (put-char-code-property char 'phonetic-type ptype)
      (cond ((eq ptype 'consonant)
	     (modify-category-entry char ?c thai-category-table))
	    ((memq ptype '(vowel-upper vowel-lower))
	     (modify-category-entry char ?v thai-category-table)
	     (if (= char ?ิ)
		 ;; Give category `I' to "SARA I".
		 (modify-category-entry char ?I thai-category-table)))
	    ((eq ptype 'tone)
	     (modify-category-entry char ?t thai-category-table)
	     (modify-category-entry char ?u thai-category-table))
	    ((eq ptype 'sign-upper)
	     (modify-category-entry char ?u thai-category-table)
	     (if (= char ?์)
		 ;; Give category `U' to "THANTHAKHAT".
		 (modify-category-entry char ?U thai-category-table)))))))

(defun thai-compose-syllable (beg end &optional category-set string)
  (or category-set
      (setq category-set
	    (char-category-set (if string (aref string beg) (char-after beg)))))
  (if (aref category-set ?c)
      ;; Starting with a consonant.  We do relative composition.
      (if string
	  (compose-string string beg end)
	(compose-region beg end))
    ;; Vowel tone sequence.
    (if string
	(compose-string string beg end (list (aref string beg) '(Bc . Bc)
					     (aref string (1+ beg))))
      (compose-region beg end (list (char-after beg) '(Bc . Bc)
				    (char-after (1+ beg))))))
  (- end beg))

;;;###autoload
(defun thai-compose-region (beg end)
  "Compose Thai characters in the region.
When called from a program, expects two arguments,
positions (integers or markers) specifying the region."
  (interactive "r")
  (let ((pos (point)))
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (with-category-table thai-category-table
	(while (re-search-forward thai-composition-pattern nil t)
	  (setq beg (match-beginning 0) end (match-end 0))
	  (if (and (> pos beg) (< pos end))
	      (setq pos end))
	  (thai-compose-syllable beg end
				 (char-category-set (char-after beg))))))
    (goto-char pos)))

;;;###autoload
(defun thai-compose-string (string)
  "Compose Thai characters in STRING and return the resulting string."
  (with-category-table thai-category-table
    (let ((idx 0))
      (while (setq idx (string-match thai-composition-pattern string idx))
	(thai-compose-syllable idx (match-end 0) nil string)
	(setq idx (match-end 0)))))
  string)

;;;###autoload
(defun thai-compose-buffer ()
  "Compose Thai characters in the current buffer."
  (interactive)
  (thai-compose-region (point-min) (point-max)))

;;;###autoload
(defun thai-composition-function (gstring direction)
  (if (= (lgstring-char-len gstring) 1)
      (compose-gstring-for-graphic gstring direction)
    (or (font-shape-gstring gstring direction)
	(let ((glyph-len (lgstring-glyph-len gstring))
	      (last-char (lgstring-char gstring
					(1- (lgstring-char-len gstring))))
	      (i 0)
	      glyph)
	  (while (and (< i glyph-len)
		      (setq glyph (lgstring-glyph gstring i)))
	    (setq i (1+ i)))
	  (if (= last-char ?ำ)
	      (setq i (1- i)))
	  (compose-glyph-string-relative gstring 0 i 0.1)))))

;; Thai-word-mode requires functions in the feature `thai-word'.
(require 'thai-word)

(defvar thai-word-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap forward-word] 'thai-forward-word)
    (define-key map [remap backward-word] 'thai-backward-word)
    (define-key map [remap kill-word] 'thai-kill-word)
    (define-key map [remap backward-kill-word] 'thai-backward-kill-word)
    (define-key map [remap transpose-words] 'thai-transpose-words)
    map)
  "Keymap for `thai-word-mode'.")

(define-minor-mode thai-word-mode
  "Minor mode to make word-oriented commands aware of Thai words.

The commands affected are \\[forward-word], \\[backward-word],
\\[kill-word], \\[backward-kill-word], \\[transpose-words], and
\\[fill-paragraph]."
  :global t :group 'mule
  (cond (thai-word-mode
	 ;; This enables linebreak between Thai characters.
	 (modify-category-entry (make-char 'thai-tis620) ?|)
	 ;; This enables linebreak at a Thai word boundary.
	 (put-charset-property 'thai-tis620 'fill-find-break-point-function
			       'thai-fill-find-break-point))
	(t
	 (modify-category-entry (make-char 'thai-tis620) ?| nil t)
	 (put-charset-property 'thai-tis620 'fill-find-break-point-function
			       nil))))

;; Function to call on entering the Thai language environment.
(defun setup-thai-language-environment-internal ()
  (thai-word-mode 1))

;; Function to call on exiting the Thai language environment.
(defun exit-thai-language-environment-internal ()
  (thai-word-mode -1))

;;
(provide 'thai-util)

;;; thai-util.el ends here
