;;; misc-lang.el --- support for miscellaneous languages (characters)  -*- lexical-binding: t; -*-

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
 (list (vector "[\u200C\u200D][\u0600-\u074F\u200C\u200D]+"
               1 #'arabic-shape-gstring)
       (vector "[\u0600-\u074F\u200C\u200D]+"
               0 #'arabic-shape-gstring)))

;; The Egyptian Hieroglyph Format Controls were introduced in Unicode
;; Standard v12.0.  Apparently, they are not yet well supported in
;; existing fonts, as of late 2020.  But there's no reason for us not
;; to be ready for when they will be!
;; The below is needed to support the arrangement of the Egyptian
;; Hieroglyphs in "quadrats", as directed by the format controls,
;; which specify how the hieroglyphs should be joined horizontally and
;; vertically.
(defun egyptian-shape-grouping (gstring direction)
  (if (= (lgstring-char gstring 0) #x13437)
      (let ((nchars (lgstring-char-len gstring))
            (i 1)
            (nesting 1)
            ch)
        ;; Find where this group ends.
        (while (and (< i nchars) (> nesting 0))
          (setq ch (lgstring-char gstring i))
          (cond
           ((= ch #x13437)
            (setq nesting (1+ nesting)))
           ((= ch #x13438)
            (setq nesting (1- nesting))))
          (setq i (1+ i)))
        (when (zerop nesting)
          ;; Make a new gstring from the characters that constitute a
          ;; complete nested group.
          (let ((new-header (make-vector (1+ i) nil))
                (new-gstring (make-vector (+ i 2) nil)))
            (aset new-header 0 (lgstring-font gstring))
            (dotimes (j i)
              (aset new-header (1+ j) (lgstring-char gstring j))
              (lgstring-set-glyph new-gstring j (lgstring-glyph gstring j)))
            (lgstring-set-header new-gstring new-header)
            (font-shape-gstring new-gstring direction))))))

(let ((hieroglyph "[\U00013000-\U0001342F]"))
  ;; HORIZONTAL/VERTICAL JOINER and INSERT AT.../OVERLAY controls
  (set-char-table-range
   composition-function-table
   '(#x13430 . #x13436)
   (list (vector (concat hieroglyph "[\U00013430-\U00013436]" hieroglyph)
                 ;; We use font-shape-gstring so that, if the font
                 ;; doesn't support these controls, the glyphs are
                 ;; displayed individually, and not as a single
                 ;; grapheme cluster.
                 1 #'font-shape-gstring)))
  ;; Grouping controls
  (set-char-table-range
   composition-function-table
   #x13437
   (list (vector "\U00013437[\U00013000-\U0001343F]+"
                 0 #'egyptian-shape-grouping)))
  ;; "Normal" hieroglyphs, for fonts that don't support the above
  ;; controls, but do shape sequences of hieroglyphs without the
  ;; controls.
  ;; FIXME: As of late 2021, Egyptian Hieroglyph Format Controls are
  ;; not yet supported in existing fonts and/or shaping engines, but
  ;; some fonts do provide ligatures with which texts in Egyptian
  ;; Hieroglyphs are correctly displayed.  If and when these format
  ;; controls are supported, as described in section 11.4 "Egyptian
  ;; Hieroglyphs" of the Unicode Standard, the five lines below (which
  ;; allow composition of hieroglyphs without formatting controls
  ;; around) can be removed, and the entry in etc/HELLO can be
  ;; restored to:
  ;; Egyptian Hieroglyphs (𓂋𓐰𓏤𓈖𓆎𓅓𓏏𓐰𓊖) 𓅓𓊵𓐰𓐷𓏏𓊪𓐸, 𓇍𓇋𓂻𓍘𓇋
  (set-char-table-range
   composition-function-table
   '(#x13000 . #x1342E)
   (list (vector "[\U00013000-\U0001342E]+"
                 0 #'font-shape-gstring))))

(provide 'misc-lang)

;;; misc-lang.el ends here
