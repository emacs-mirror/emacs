;;; codepage.el --- MS-DOS/MS-Windows specific coding systems

;; Copyright (C) 1998 Free Software Foundation, Inc.

;; Author: Eli Zaretskii
;; Maintainer: FSF
;; Keywords: i18n ms-dos ms-windows codepage obsolete

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

;; Special coding systems for DOS/Windows codepage support.
;; Obsolete.

;;; Code:

;; I doubt we need compatibility versions of any of these autoloaded
;; functions apart from codepage-setup, which users may call.

;; ;;;###autoload
;; (defun cp-make-coding-systems-for-codepage (codepage iso-name offset)
;;   "Create a coding system to convert IBM CODEPAGE into charset ISO-NAME
;; whose first character is at offset OFFSET from the beginning of 8-bit
;; ASCII table.

;; The created coding system has the usual 3 subsidiary systems: for Unix-,
;; DOS- and Mac-style EOL conversion.  However, unlike built-in coding
;; systems, the Mac-style EOL conversion is currently not supported by the
;; decoder and encoder created by this function."
;;   (let* ((decode-table (intern (format "%s-decode-table" codepage)))
;; 	 (nonascii-table
;; 	  (intern (format "%s-nonascii-translation-table" codepage)))
;; 	 (decode-translation
;; 	  (intern (format "%s-decode-translation-table" codepage)))
;; 	 (encode-translation
;; 	  (intern (format "%s-encode-translation-table" codepage))))
;;     (set nonascii-table
;; 	 (make-translation-table-from-vector
;; 	  (cp-decoding-vector-for-codepage
;; 	   (symbol-value decode-table) iso-name offset)))
;;     (define-translation-table encode-translation
;;       (char-table-extra-slot (symbol-value nonascii-table) 0))
;;     ;; For charsets other than ascii, eight-bit-* and ISO-NAME, set
;;     ;; `?' for one-column charsets, and some Japanese character for
;;     ;; wide-column charsets.  CCL encoder convert that Japanese
;;     ;; character to either dos-unsupported-char-glyph or "??".
;;     (let ((tbl (char-table-extra-slot (symbol-value nonascii-table) 0))
;; 	  (undef (if (eq system-type 'ms-dos)
;; 		     (if dos-unsupported-char-glyph
;; 			 (logand dos-unsupported-char-glyph 255)
;; 		       127)
;; 		   ??))
;; 	  (charsets (delq 'ascii
;; 			  (delq 'eight-bit-control
;; 				(delq 'eight-bit-graphic
;; 				      (delq iso-name
;; 					    (copy-sequence charset-list))))))
;; 	  (wide-column-char (make-char 'japanese-jisx0208 32 32)))
;;       (while charsets
;; 	(aset tbl (make-char (car charsets))
;; 	      (if (= (charset-width (car charsets)) 1) undef wide-column-char))
;; 	(setq charsets (cdr charsets))))
;;     (define-translation-table decode-translation
;;       (symbol-value nonascii-table))
;;     (cp-coding-system-for-codepage-1
;;      (intern codepage) ?D iso-name decode-translation encode-translation)
;;     ))

;; ;;;###autoload
;; (defun cp-charset-for-codepage (codepage)
;;   "Return the charset for which there is a translation table to DOS CODEPAGE.
;; CODEPAGE must be the name of a DOS codepage, a string."
;;   (let ((cp-decoder (cp-codepage-decoder codepage)))
;;     (if (null cp-decoder)
;; 	(error "Unsupported codepage %s" codepage)
;;       (get cp-decoder 'charset))))

;; ;;;###autoload
;; (defun cp-language-for-codepage (codepage)
;;   "Return the name of the MULE language environment for CODEPAGE.
;; CODEPAGE must be the name of a DOS codepage, a string."
;;   (let ((cp-decoder (cp-codepage-decoder codepage)))
;;     (if (null cp-decoder)
;; 	(error "Unsupported codepage %s" codepage)
;;       (get cp-decoder 'language))))

;; ;;;###autoload
;; (defun cp-offset-for-codepage (codepage)
;;   "Return the offset to be used in setting up coding systems for CODEPAGE.
;; CODEPAGE must be the name of a DOS codepage, a string."
;;   (let ((cp-decoder (cp-codepage-decoder codepage)))
;;     (if (null cp-decoder)
;; 	(error "Unsupported codepage %s" codepage)
;;       (get cp-decoder 'offset))))

;; ;;;###autoload
;; (defun cp-supported-codepages ()
;;   "Return an alist of supported codepages.

;; Each association in the alist has the form (NNN . CHARSET), where NNN is the
;; codepage number, and CHARSET is the MULE charset which is the closest match
;; for the character set supported by that codepage.

;; A codepage NNN is supported if a variable called `cpNNN-decode-table' exists,
;; is a vector, and has a charset property."
;;   (save-match-data
;;     (let (alist chset sname)
;;       (mapatoms
;;        (function
;; 	(lambda (sym)
;; 	  (if (and (boundp sym)
;; 		   (string-match "\\`cp\\([1-9][0-9][0-9][0-9]?\\)-decode-table\\'"
;; 				 (setq sname (symbol-name sym)))
;; 		   (vectorp (symbol-value sym))
;; 		   (setq chset (get sym 'charset)))
;; 	      (setq alist
;; 		    (cons (cons (match-string 1 sname) chset) alist))))))
;;       alist)))

;;;###autoload
(defun codepage-setup (&optional codepage)
  "Obsolete.  All coding systems are set up initially."
  (interactive))
(make-obsolete 'codepage-setup "no longer relevant" "22.1")

(provide 'codepage)

;;; codepage.el ends here
