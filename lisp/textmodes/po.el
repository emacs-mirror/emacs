;;; po.el --- basic support of PO translation files -*- coding: latin-1; -*-

;; Copyright (C) 1995-1998, 2000-2002 Free Software Foundation, Inc.

;; Authors: François Pinard <pinard@iro.umontreal.ca>,
;;          Greg McGary <gkm@magilla.cichlid.com>,
;;          Bruno Haible <bruno@clisp.org>.
;; Keywords: i18n, files

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

;; This package makes sure visiting PO files decodes them correctly,
;; according to the Charset= header in the PO file.  For more support
;; for editing PO files, see po-mode.el.

;;; Code:

; Make the cpnnn codesets available.
(if (not (string-match "XEmacs\\|Lucid" emacs-version))
  (mapc #'codepage-setup (mapcar #'car (cp-supported-codepages))))

(defconst po-content-type-charset-alist
  '(; Note: Emacs 21 doesn't support all encodings, thus the missing entries.
    (ASCII . undecided)
    (ANSI_X3.4-1968 . undecided)
    (US-ASCII . undecided)
    (ISO-8859-1 . iso-8859-1)
    (ISO_8859-1 . iso-8859-1)
    (ISO-8859-2 . iso-8859-2)
    (ISO_8859-2 . iso-8859-2)
    (ISO-8859-3 . iso-8859-3)
    (ISO_8859-3 . iso-8859-3)
    (ISO-8859-4 . iso-8859-4)
    (ISO_8859-4 . iso-8859-4)
    (ISO-8859-5 . iso-8859-5)
    (ISO_8859-5 . iso-8859-5)
    ;(ISO-8859-6 . ??)
    ;(ISO_8859-6 . ??)
    (ISO-8859-7 . iso-8859-7)
    (ISO_8859-7 . iso-8859-7)
    (ISO-8859-8 . iso-8859-8)
    (ISO_8859-8 . iso-8859-8)
    (ISO-8859-9 . iso-8859-9)
    (ISO_8859-9 . iso-8859-9)
    ;(ISO-8859-13 . ??)
    ;(ISO_8859-13 . ??)
    (ISO-8859-15 . iso-8859-15) ; requires Emacs 21
    (ISO_8859-15 . iso-8859-15) ; requires Emacs 21
    (KOI8-R . koi8-r)
    ;(KOI8-U . ??)
    (CP437 . cp437) ; requires Emacs 20
    (CP775 . cp775) ; requires Emacs 20
    (CP850 . cp850) ; requires Emacs 20
    (CP852 . cp852) ; requires Emacs 20
    (CP855 . cp855) ; requires Emacs 20
    ;(CP856 . ??)
    (CP857 . cp857) ; requires Emacs 20
    (CP861 . cp861) ; requires Emacs 20
    (CP862 . cp862) ; requires Emacs 20
    (CP864 . cp864) ; requires Emacs 20
    (CP865 . cp865) ; requires Emacs 20
    (CP866 . cp866) ; requires Emacs 21
    (CP869 . cp869) ; requires Emacs 20
    ;(CP874 . ??)
    ;(CP922 . ??)
    ;(CP932 . ??)
    ;(CP943 . ??)
    ;(CP949 . ??)
    ;(CP950 . ??)
    ;(CP1046 . ??)
    ;(CP1124 . ??)
    ;(CP1129 . ??)
    (CP1250 . cp1250) ; requires Emacs 20
    (CP1251 . cp1251) ; requires Emacs 20
    (CP1252 . iso-8859-1) ; approximation
    (CP1253 . cp1253) ; requires Emacs 20
    (CP1254 . iso-8859-9) ; approximation
    (CP1255 . iso-8859-8) ; approximation
    ;(CP1256 . ??)
    (CP1257 . cp1257) ; requires Emacs 20
    (GB2312 . cn-gb-2312)  ; also named 'gb2312' in XEmacs 21 or Emacs 21
                           ; also named 'euc-cn' in Emacs 20 or Emacs 21
    (EUC-JP . euc-jp)
    (EUC-KR . euc-kr)
    ;(EUC-TW . ??)
    (BIG5 . big5)
    ;(BIG5-HKSCS . ??)
    ;(GBK . ??)
    ;(GB18030 . ??)
    (SHIFT_JIS . shift_jis)
    ;(JOHAB . ??)
    (TIS-620 . tis-620)    ; requires Emacs 20 or Emacs 21
    (VISCII . viscii)      ; requires Emacs 20 or Emacs 21
    (UTF-8 . utf-8)        ; requires Mule-UCS in Emacs 20, or Emacs 21
    )
  "How to convert a GNU libc/libiconv canonical charset name as seen in
Content-Type into a Mule coding system.")

(defun po-find-charset (filename)
  "Return PO file charset value."
  (interactive)
  (let ((charset-regexp
	 "^\"Content-Type: text/plain;[ \t]*charset=\\(.*\\)\\\\n\"")
	(short-read nil))
    ;; Try the first 4096 bytes.  In case we cannot find the charset value
    ;; within the first 4096 bytes (the PO file might start with a long
    ;; comment) try the next 4096 bytes repeatedly until we'll know for sure
    ;; we've checked the empty header entry entirely.
    (while (not (or short-read (re-search-forward "^msgid" nil t)))
      (save-excursion
        (goto-char (point-max))
	(let ((pair (insert-file-contents-literally filename nil
						    (1- (point))
						    (1- (+ (point) 4096)))))
	  (setq short-read (< (nth 1 pair) 4096)))))
    (cond ((re-search-forward charset-regexp nil t) (match-string 1))
	  (short-read nil)
	  ;; We've found the first msgid; maybe, only a part of the msgstr
	  ;; value was loaded.  Load the next 1024 bytes; if charset still
	  ;; isn't available, give up.
	  (t (save-excursion
	       (goto-char (point-max))
	       (insert-file-contents-literally filename nil
					       (1- (point))
					       (1- (+ (point) 1024))))
	     (if (re-search-forward charset-regexp nil t)
		 (match-string 1))))))

(defun po-find-file-coding-system-guts (operation filename)
  "\
Return a Mule (DECODING . ENCODING) pair, according to PO file charset.
Called through file-coding-system-alist, before the file is visited for real."
  (and (eq operation 'insert-file-contents)
       (file-exists-p filename)
       (with-temp-buffer
	 (let* ((coding-system-for-read 'no-conversion)
                (charset (or (po-find-charset filename) "ascii"))
                (charset-upper (intern (upcase charset)))
                (charset-lower (intern (downcase charset))))
           (list (or (cdr (assq charset-upper po-content-type-charset-alist))
                     (if (memq charset-lower (coding-system-list))
                       charset-lower
                      'no-conversion)))))))

;;;###autoload
(defun po-find-file-coding-system (arg-list)
  "\
Return a Mule (DECODING . ENCODING) pair, according to PO file charset.
Called through file-coding-system-alist, before the file is visited for real."
  (po-find-file-coding-system-guts (car arg-list) (car (cdr arg-list))))
;; This is for XEmacs.
;(defun po-find-file-coding-system (operation filename)
;  "\
;Return a Mule (DECODING . ENCODING) pair, according to PO file charset.
;Called through file-coding-system-alist, before the file is visited for real."
;  (po-find-file-coding-system-guts operation filename))
