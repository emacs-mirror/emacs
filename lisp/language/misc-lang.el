;;; misc-lang.el --- support for miscellaneous languages (characters)

;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021

;; Keywords: multilingual, character set, coding system

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IPA (International Phonetic Alphabet)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-language-info-alist
 "IPA" '((charset . (ipa))
	 (coding-priority utf-8)
	 (coding-system utf-8)
	 (input-method . "ipa")
	 (nonascii-translation . ipa)
	 (documentation . "\
IPA is International Phonetic Alphabet for English, French, German
and Italian.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Arabic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-coding-system 'iso-8859-6
  "ISO-8859-6 based encoding (MIME:ISO-8859-6)."
  :coding-type 'charset
  :mnemonic ?6
  :charset-list '(iso-8859-6)
  :mime-charset 'iso-8859-6)

(define-coding-system 'windows-1256
  "windows-1256 (Arabic) encoding (MIME: WINDOWS-1256)"
  :coding-type 'charset
  :mnemonic ?A
  :charset-list '(windows-1256)
  :mime-charset 'windows-1256)
(define-coding-system-alias 'cp1256 'windows-1256)

(set-language-info-alist
 "Arabic" '((charset unicode)
	    (coding-system utf-8 iso-8859-6 windows-1256)
	    (coding-priority utf-8 iso-8859-6 windows-1256)
	    (input-method . "arabic")
	    (sample-text . "Arabic	السّلام عليكم")
	    (documentation . "Bidirectional editing is supported.")))

(set-language-info-alist
 "Persian" '((charset unicode)
	    (coding-system utf-8 iso-8859-6 windows-1256)
	    (coding-priority utf-8 iso-8859-6 windows-1256)
	    (input-method . "farsi-transliterate-banan")
	    (sample-text . "Persian	فارسی")
	    (documentation . "Bidirectional editing is supported.")))

(defcustom arabic-shaper-ZWNJ-handling nil
  "How to handle ZWMJ in Arabic text rendering.
This variable controls the way to handle a glyph for ZWNJ
returned by the underling shaping engine.

The default value is nil, which means that the ZWNJ glyph is
displayed as is.

If the value is `absorb', ZWNJ is absorbed into the previous
grapheme cluster, and not displayed.

If the value is `as-space', the glyph is displayed by a
thin (i.e. 1-dot width) space."
  :group 'mule
  :version "26.1"
  :type '(choice
          (const :tag "default" nil)
          (const :tag "as space" as-space)
          (const :tag "absorb" absorb))
  :set (lambda (sym val)
         (set-default sym val)
         (clear-composition-cache)))

;; Record error in arabic-change-gstring.
(defvar arabic-shape-log nil)

(defun arabic-shape-gstring (gstring direction)
  (setq gstring (font-shape-gstring gstring direction))
  (condition-case err
      (when arabic-shaper-ZWNJ-handling
        (let ((font (lgstring-font gstring))
              (i 1)
              (len (lgstring-glyph-len gstring))
              (modified nil))
          (while (< i len)
            (let ((glyph (lgstring-glyph gstring i)))
              (when (eq (lglyph-char glyph) #x200c)
                (cond
                 ((eq arabic-shaper-ZWNJ-handling 'as-space)
                  (if (> (- (lglyph-rbearing glyph) (lglyph-lbearing glyph)) 0)
                      (let ((space-glyph (aref (font-get-glyphs font 0 1 " ") 0)))
                        (when space-glyph
                          (lglyph-set-code glyph (aref space-glyph 3))
                          (lglyph-set-width glyph (aref space-glyph 4)))))
                  (lglyph-set-adjustment glyph 0 0 1)
                  (setq modified t))
                 ((eq arabic-shaper-ZWNJ-handling 'absorb)
                  (let ((prev (lgstring-glyph gstring (1- i))))
                    (lglyph-set-from-to prev (lglyph-from prev) (lglyph-to glyph))
                    (setq gstring (lgstring-remove-glyph gstring i))
                    (setq len (1- len)))
                  (setq modified t)))))
            (setq i (1+ i)))
          (if modified
              (lgstring-set-id gstring nil))))
    (error (push err arabic-shape-log)))
  gstring)

(set-char-table-range
 composition-function-table
 '(#x600 . #x74F)
 (list (vector "[\u0600-\u074F\u200C\u200D]+" 0
               'arabic-shape-gstring)
       (vector "[\u200C\u200D][\u0600-\u074F\u200C\u200D]+" 1
               'arabic-shape-gstring)))

(provide 'misc-lang)

;;; misc-lang.el ends here
