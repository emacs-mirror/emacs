;;; ind-util.el --- Transliteration and Misc. Tools for Indian Languages -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001 Free Software Foundation, Inc.

;; Maintainer:  KAWABATA, Taichi <batta@beige.ocn.ne.jp>
;; Keywords: multilingual, Indian, Devanagari

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

;; This file provides conversion between UCS and various
;; transliteration schemes, such as ITRANS, kyoto-harvard and aiba
;; methods.  It also provides conversion between IS 13194 and UCS.
;; Finally, this program provides the compatibility support with
;; old implementation of Devanagari script.

;;; Code:

;;; Transliteration

;; The followings provide the various transliteration schemes (such as
;; ITRANS, kyoto-harvard, and Aiba) of Indian scripts.  They are also
;; used in quail/indian.el for typing Indian script in Emacs.

(eval-and-compile

(defun indian-regexp-of-hashtbl-keys (hashtbl)
  "Returns the regular expression of hashtable keys."
  (let ((max-specpdl-size 1000))
    (regexp-opt
     (sort 
      (let (dummy)
	(maphash (function (lambda (key val) (setq dummy (cons key dummy))))
		 hashtbl)
	dummy)
      (function (lambda (x y) (> (length x) (length y))))))))

(defvar indian-dev-base-table
  '(
    (;; VOWELS  (18)
     (?$,15E(B nil) (?$,15F(B ?$,15~(B) (?$,15G(B ?$,15(B) (?$,15H(B ?$,16 (B) (?$,15I(B ?$,16!(B) (?$,15J(B ?$,16"(B)
     (?$,15K(B ?$,16#(B) (?$,15L(B ?$,16$(B) (?$,15M(B ?$,16%(B) (?$,15N(B ?$,16&(B) (?$,15O(B ?$,16'(B) (?$,15P(B ?$,16((B)
     (?$,15Q(B ?$,16)(B) (?$,15R(B ?$,16*(B) (?$,15S(B ?$,16+(B) (?$,15T(B ?$,16,(B) (?$,16@(B ?$,16B(B) (?$,16A(B ?$,16C(B))
    (;; CONSONANTS (currently 42, including special cases)
     ?$,15U(B ?$,15V(B ?$,15W(B ?$,15X(B ?$,15Y(B                  ;; GUTTRULS
     ?$,15Z(B ?$,15[(B ?$,15\(B ?$,15](B ?$,15^(B                  ;; PALATALS  
     ?$,15_(B ?$,15`(B ?$,15a(B ?$,15b(B ?$,15c(B                  ;; CEREBRALS 
     ?$,15d(B ?$,15e(B ?$,15f(B ?$,15g(B ?$,15h(B ?$,15i(B              ;; DENTALS   
     ?$,15j(B ?$,15k(B ?$,15l(B ?$,15m(B ?$,15n(B                  ;; LABIALS   
     ?$,15o(B ?$,15p(B ?$,15q(B ?$,15r(B ?$,15s(B ?$,15t(B ?$,15u(B          ;; SEMIVOWELS
     ?$,15v(B ?$,15w(B ?$,15x(B ?$,15y(B                    ;; SIBILANTS 
     ?$,168(B ?$,169(B ?$,16:(B ?$,16;(B ?$,16<(B ?$,16=(B ?$,16>(B ?$,16?(B      ;; NUKTAS    
     "$,15\6-5^(B" "$,15U6-5w(B")
    (;; Misc Symbols (7)  
     ?$,15A(B ?$,15B(B ?$,15C(B ?$,15}(B ?$,16-(B ?$,160(B ?$,16D(B)
    (;; Digits (10)
     ?$,16F(B ?$,16G(B ?$,16H(B ?$,16I(B ?$,16J(B ?$,16K(B ?$,16L(B ?$,16M(B ?$,16N(B ?$,16O(B)
    (;; Inscript-extra (4)  (#, $, ^, *, ])
     "$,16-5p(B" "$,15p6-(B" "$,15d6-5p(B" "$,15v6-5p(B" "$,15|(B")))

(defvar indian-pnj-base-table nil)
(defvar indian-ori-base-table nil)
(defvar indian-bng-base-table nil)
(defvar indian-asm-base-table nil)
(defvar indian-tlg-base-table nil)
(defvar indian-knd-base-table nil)
(defvar indian-mlm-base-table nil)
(defvar indian-tml-base-table nil)

(defvar indian-base-table-to-language-alist
  '((indian-dev-base-table . "Devanagari")
    (indian-pnj-base-table . "Punjabi")    
    (indian-ori-base-table . "Oriya")
    (indian-bng-base-table . "Bengali")
    (indian-asm-base-table . "Assamese")
    (indian-tlg-base-table . "Telugu")
    (indian-knd-base-table . "Kannada")
    (indian-mlm-base-table . "Malayalam")
    (indian-tml-base-table . "Tamil")))

(defvar indian-itrans-v5-table
  '(;; for encode/decode
    (;; vowels -- 18
     "a" ("aa" "A") "i" ("ii" "I") "u" ("uu" "U")
     ("RRi" "R^i") ("LLi" "L^i") (".c" "e.c") nil "e" "ai"
     "o.c"  nil   "o"   "au"  ("RRI" "R^I") ("LLI" "L^I"))
    (;; consonants -- 40 
     "k"   "kh"  "g"   "gh"  ("~N" "N^")
     "ch" ("Ch" "chh") "j" "jh" ("~n" "JN")
     "T"   "Th"  "D"   "Dh"  "N"
     "t"   "th"  "d"   "dh"  "n"   "nh" 
     "p"   "ph"  "b"   "bh"  "m"
     "y"   "r"   "rh"  "l"   ("L" "ld") nil  ("v" "w")
     "sh" ("Sh" "shh") "s" "h"
     "q" "K" "G" ("J" "z") ".D" ".Dh" "f" ("Y" "yh")
     ("GY" "dny") "x")
    (;; misc -- 7
     ".N" (".n" "M") "H" ".a" ".h" ("AUM" "OM") "..")))

(defvar indian-kyoto-harvard-table
  '(;; for encode/decode
    (;; vowel
     "a"   ("A" "aa")  "i"   ("I" "ii")  "u"   ("U" "uu")
     "R"   ("L" "lR")  nil   nil   "e"   "ai"
     nil   nil   "o"   "au"  ("q" "RR" "Q")   ("E" "LL" "lRR"))
    (;; consonant
     "k"   "kh"  "g"   "gh"  "G"
     "c"   "ch"  "j"   "jh"  "J"
     "T"   "Th"  "D"   "Dh"  "N"
     "t"   "th"  "d"   "dh"  "n"   nil
     "p"   "ph"  "b"   "bh"  "m"
     "y"   "r"   nil   "l"   "L"   nil   "v"
     ("z" "Z")   "S"   "s"   "h"
     nil   nil   nil   nil   nil   nil   nil   nil
     nil   nil)
    (;; misc
     nil   "M"   "H"   "'"   nil   "." nil)))

(defvar indian-harvard-table
  '(;; for encode/decode
    (;; vowel
     "a"   ("A" "aa")  "i"   ("I" "ii")  "u"   ("U" "uu")
     "R"   ("L" "lR")  nil   nil   "e"   "ai"
     nil   nil   "o"   "au"  ("RR" "q" "Q")   ("LL" "E" "lRR"))
    (;; consonant
     "k"   "kh"  "g"   "gh"  "G"
     "c"   "ch"  "j"   "jh"  "J"
     "T"   "Th"  "D"   "Dh"  "N"
     "t"   "th"  "d"   "dh"  "n"   nil
     "p"   "ph"  "b"   "bh"  "m"
     "y"   "r"   nil   "l"   "L"   nil   "v"
     ("z" "Z")   "S"   "s"   "h"
     nil   nil   nil   nil   nil   nil   nil   nil
     nil   nil)
    (;; misc
     nil   "M"   "H"   "'"   nil   "." nil)))

(defvar indian-tokyo-table
  '(;; for encode/decode
    (;; vowel
     "a"   ("A" "aa")  "i"   ("I" "ii")  "u"   ("U" "uu")
     "R"   ("L" "lR")  nil   nil   "e"   "ai"
     nil   nil   "o"   "au"  ("Q" "RR" "q")   ("E" "LL" "lRR"))
    (;; consonant
     "k"   "kh"  "g"   "gh"  "G"
     "c"   "ch"  "j"   "jh"  "J"
     "T"   "Th"  "D"   "Dh"  "N"
     "t"   "th"  "d"   "dh"  "n"   nil
     "p"   "ph"  "b"   "bh"  "m"
     "y"   "r"   nil   "l"   "L"   nil   "v"
     ("Z" "z")   "S"   "s"   "h"
     nil   nil   nil   nil   nil   nil   nil   nil
     nil   nil)
    (;; misc
     nil   "M"   "H"   "'"   nil   "." nil)))

(defvar indian-aiba-table
  '(;; for encode/decode
    (;; vowel
     "a"   "aa"  "i"   "ii"  "u"   "uu"
     ".r"  ".l"   nil   nil  "e"   "ai"
     nil   nil   "o"   "au"  "~r"  "~l")
    (;; consonant
     "k"   "kh"  "g"   "gh"  "^n"
     "c"   "ch"  "j"   "jh"  "~n"
     ".t"  ".th" ".d"  ".dh" ".n"
     "t"   "th"  "d"   "dh"  "n"   nil
     "p"   "ph"  "b"   "bh"  "m"
     "y"   "r"   nil   "l"   nil  nil  "v"
     "^s"  ".s"  "s"   "h"
     nil   nil   nil   nil   nil   nil   nil   nil
     nil   nil)
    (;; misc
     nil   ".m"  ".h"  "'"   nil   "." nil)))

(defun mapthread (function seq1 &rest seqrest)
  "Apply FUNCTION to each element of SEQ1 and return result list.
If there are several SEQRESTs, FUNCTION is called with that many
arguments, with all possible combinations of these multiple SEQUENCES.
Thus, if SEQ1 contains 3 elements and SEQ2 contains 5 elements, then
FUNCTION will be called 15 times."
  (if seqrest
      (mapcar 
       (lambda (x)
         (apply 
          'mapthread 
          `(lambda (&rest y) (apply ',function x y))
          seqrest))
       seq1)
  (mapcar function seq1)))

(defun indian--puthash-char (char trans-char hashtbls)
  (let ((encode-hash (car hashtbls))  ;; char -> trans
	(decode-hash (cdr hashtbls))  ;; trans -> char
	)
    ;; char -- nil / char / string (/ list of vowel & matra)
    ;; trans-char -- nil / string / list of strings
    (when (and char trans-char)
      (if (stringp trans-char) (setq trans-char (list trans-char)))
      (if (characterp char) (setq char (char-to-string char)))
      (puthash char (car trans-char) encode-hash)
      (mapc
       (lambda (trans)
	 (puthash trans char decode-hash))
       trans-char))))

(defun indian--map (f l1 l2)
  (while l1
    (funcall f (pop l1) (pop l2))))

(defun indian--puthash-v (v trans-v hashtbls)
  (indian--map 
   (lambda (v trans-v)
     (indian--puthash-char (car v) trans-v hashtbls))
   v trans-v))

(defun indian--puthash-c (c trans-c halant hashtbls)
  (indian--map
   (lambda (c trans-c)
     (if (characterp c) (setq c (char-to-string c)))
     (indian--puthash-char (concat c halant) trans-c hashtbls))
   c trans-c))

(defun indian--puthash-m (m trans-m hashtbls)
  (indian--map
   (lambda (m trans-m)
     (indian--puthash-char m trans-m hashtbls))
   m trans-m))

(defun indian--puthash-cv (c trans-c v trans-v hashtbls)
  (indian--map
   (lambda (c trans-c)
     (indian--map
      (lambda (v trans-v)
	(when (and c trans-c  v trans-v)
	  (if (characterp c) (setq c (char-to-string c)))
	  (setq v (if (characterp (cadr v)) (char-to-string (cadr v)) ""))
	  (if (stringp trans-c) (setq trans-c (list trans-c)))
	  (if (stringp trans-v) (setq trans-v (list trans-v)))
	  (indian--puthash-char 
	   (concat c v)
	   (apply 'append
		  (mapthread 'concat trans-c trans-v))
	   hashtbls)))
      v trans-v))
   c trans-c))

(defun indian-make-hash (table trans-table)
  "Indian Transliteration Hash for decode/encode"
  (let* ((encode-hash (makehash 'equal))
	 (decode-hash (makehash 'equal))
	 (hashtbls (cons encode-hash decode-hash))
	 (vowels     (elt table 0))
	 (consonants (elt table 1))
	 (misc       (elt table 2))
	 (digits     (elt table 3))
	 (halant     (char-to-string (elt misc  4)))
	 (trans-vowels     (elt trans-table 0))
	 (trans-consonants (elt trans-table 1))
	 (trans-misc       (elt trans-table 2))
	 (trans-digits  '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9")))
    (indian--puthash-v vowels trans-vowels hashtbls)
    (indian--puthash-c consonants trans-consonants halant hashtbls)
    (indian--puthash-cv consonants trans-consonants 
			      vowels trans-vowels hashtbls)
    (indian--puthash-m misc trans-misc hashtbls)
    (indian--puthash-m digits trans-digits hashtbls)
    hashtbls))

(defvar indian-dev-itrans-v5-hash
  (indian-make-hash indian-dev-base-table
			  indian-itrans-v5-table))
(defvar indian-dev-kyoto-harvard-hash
  (indian-make-hash indian-dev-base-table
			  indian-kyoto-harvard-table))
(defvar indian-dev-aiba-hash
  (indian-make-hash indian-dev-base-table
			  indian-aiba-table))

)

(defmacro indian-translate-region (from to hashtable encode-p)
  `(save-excursion
     (save-restriction
       (let ((regexp ,(indian-regexp-of-hashtbl-keys 
		       (if encode-p (car (eval hashtable)) 
			 (cdr (eval hashtable))))))
	 (narrow-to-region from to)
	 (goto-char (point-min))
	 (while (re-search-forward regexp nil t)
	   (let ((matchstr (gethash (match-string 0) 
				    (if ,encode-p
					(car ,hashtable)
				      (cdr ,hashtable)))))
	     (if matchstr (replace-match matchstr))))))))

;;;

(defun indian-dev-itrans-v5-encode-region (from to)
  (interactive "r")
  (indian-translate-region 
   from to indian-dev-itrans-v5-hash t))

(defun indian-dev-itrans-v5-decode-region (from to)
  (interactive "r")
  (indian-translate-region
   from to indian-dev-itrans-v5-hash nil))

(defun indian-dev-kyoto-harvard-encode-region (from to)
  (interactive "r")
  (indian-translate-region 
   from to indian-dev-kyoto-harvard-hash t))

(defun indian-dev-kyoto-harvard-decode-region (from to)
  (interactive "r")
  (indian-translate-region
   from to indian-dev-kyoto-harvard-hash nil))

(defun indian-dev-aiba-encode-region (from to)
  (interactive "r")
  (indian-translate-region 
   from to indian-dev-aiba-hash t))

(defun indian-dev-aiba-decode-region (from to)
  (interactive "r")
  (indian-translate-region
   from to indian-dev-aiba-hash nil))




;;; IS 13194 utilities

;; The followings provide conversion between IS 13194 (ISCII) and UCS.

(defvar ucs-devanagari-to-is13194-alist
  '(;;Unicode vs IS13194  ;; only Devanagari is supported now.
    (?\x0900 . "[U+0900]")
    (?\x0901 . "(5!(B")
    (?\x0902 . "(5"(B")
    (?\x0903 . "(5#(B")
    (?\x0904 . "[U+0904]")
    (?\x0905 . "(5$(B")
    (?\x0906 . "(5%(B")
    (?\x0907 . "(5&(B")
    (?\x0908 . "(5'(B")
    (?\x0909 . "(5((B")
    (?\x090a . "(5)(B")
    (?\x090b . "(5*(B")
    (?\x090c . "(5&i(B")
    (?\x090d . "(5.(B")
    (?\x090e . "(5+(B")
    (?\x090f . "(5,(B")
    (?\x0910 . "(5-(B")
    (?\x0911 . "(52(B")
    (?\x0912 . "(5/(B")
    (?\x0913 . "(50(B")
    (?\x0914 . "(51(B")
    (?\x0915 . "(53(B")
    (?\x0916 . "(54(B")
    (?\x0917 . "(55(B")
    (?\x0918 . "(56(B")
    (?\x0919 . "(57(B")
    (?\x091a . "(58(B")
    (?\x091b . "(59(B")
    (?\x091c . "(5:(B")
    (?\x091d . "(5;(B")
    (?\x091e . "(5<(B")
    (?\x091f . "(5=(B")
    (?\x0920 . "(5>(B")
    (?\x0921 . "(5?(B")
    (?\x0922 . "(5@(B")
    (?\x0923 . "(5A(B")
    (?\x0924 . "(5B(B")
    (?\x0925 . "(5C(B")
    (?\x0926 . "(5D(B")
    (?\x0927 . "(5E(B")
    (?\x0928 . "(5F(B")
    (?\x0929 . "(5G(B")
    (?\x092a . "(5H(B")
    (?\x092b . "(5I(B")
    (?\x092c . "(5J(B")
    (?\x092d . "(5K(B")
    (?\x092e . "(5L(B")
    (?\x092f . "(5M(B")
    (?\x0930 . "(5O(B")
    (?\x0931 . "(5P(B")
    (?\x0932 . "(5Q(B")
    (?\x0933 . "(5R(B")
    (?\x0934 . "(5S(B")
    (?\x0935 . "(5T(B")
    (?\x0936 . "(5U(B")
    (?\x0937 . "(5V(B")
    (?\x0938 . "(5W(B")
    (?\x0939 . "(5X(B")
    (?\x093a . "[U+093a]")
    (?\x093b . "[U+093b]")
    (?\x093c . "(5i(B")
    (?\x093d . "(5ji(B")
    (?\x093e . "(5Z(B")
    (?\x093f . "(5[(B")
    (?\x0940 . "(5\(B")
    (?\x0941 . "(5](B")
    (?\x0942 . "(5^(B")
    (?\x0943 . "(5_(B")
    (?\x0944 . "(5_i(B")
    (?\x0945 . "(5c(B")
    (?\x0946 . "(5`(B")
    (?\x0947 . "(5a(B")
    (?\x0948 . "(5b(B")
    (?\x0949 . "(5g(B")
    (?\x094a . "(5d(B")
    (?\x094b . "(5e(B")
    (?\x094c . "(5f(B")
    (?\x094d . "(5h(B")
    (?\x094e . "[U+094e]")
    (?\x094f . "[U+094f]")
    (?\x0950 . "(5!i(B")
    (?\x0951 . "(5p5(B")
    (?\x0952 . "(5p8(B")
    (?\x0953 . "[DEVANAGARI GRAVE ACCENT]")
    (?\x0954 . "[DEVANAGARI ACUTE ACCENT]")
    (?\x0955 . "[U+0955]")
    (?\x0956 . "[U+0956]")
    (?\x0957 . "[U+0957]")
    (?\x0958 . "(53i(B")
    (?\x0959 . "(54i(B")
    (?\x095a . "(55i(B")
    (?\x095b . "(5:i(B")
    (?\x095c . "(5?i(B")
    (?\x095d . "(5@i(B")
    (?\x095e . "(5Ii(B")
    (?\x095f . "(5N(B")
    (?\x0960 . "(5*i(B")
    (?\x0961 . "(5'i(B")
    (?\x0962 . "(5[i(B")
    (?\x0963 . "(5ei(B")
    (?\x0964 . "(5j(B")
    (?\x0965 . "(5jj(B")
    (?\x0966 . "(5q(B")
    (?\x0967 . "(5r(B")
    (?\x0968 . "(5s(B")
    (?\x0969 . "(5t(B")
    (?\x096a . "(5u(B")
    (?\x096b . "(5v(B")
    (?\x096c . "(5w(B")
    (?\x096d . "(5x(B")
    (?\x096e . "(5y(B")
    (?\x096f . "(5z(B")
    (?\x0970 . "[U+0970]")
    (?\x0971 . "[U+0971]")
    (?\x0972 . "[U+0972]")
    (?\x0973 . "[U+0973]")
    (?\x0974 . "[U+0974]")
    (?\x0975 . "[U+0975]")
    (?\x0976 . "[U+0976]")
    (?\x0977 . "[U+0977]")
    (?\x0978 . "[U+0978]")
    (?\x0979 . "[U+0979]")
    (?\x097a . "[U+097a]")
    (?\x097b . "[U+097b]")
    (?\x097c . "[U+097c]")
    (?\x097d . "[U+097d]")
    (?\x097e . "[U+097e]")
    (?\x097f . "[U+097f]")))

(defvar ucs-bengali-to-is13194-alist nil)
(defvar ucs-assamese-to-is13194-alist nil)
(defvar ucs-gurmukhi-to-is13194-alist nil)
(defvar ucs-gujarati-to-is13194-alist nil)
(defvar ucs-oriya-to-is13194-alist nil)
(defvar ucs-tamil-to-is13194-alist nil)
(defvar ucs-telugu-to-is13194-alist nil)
(defvar ucs-malayalam-to-is13194-alist nil)

(defvar is13194-default-repartory 'devanagari)

(defvar is13194-repertory-to-ucs-script
  `((DEF ?\x40 ,is13194-default-repartory)
    (RMN ?\x41 ,is13194-default-repartory)
    (DEV ?\x42 devanagari)
    (BNG ?\x43 bengali)
    (TML ?\x44 tamil)
    (TLG ?\x45 telugu)
    (ASM ?\x46 bengali)
    (ORI ?\x47 oriya)
    (KND ?\x48 kannada)
    (MLM ?\x49 malayalam)
    (GJR ?\x4a gujarati)
    (PNJ ?\x4b gurmukhi)))

;; for guiding find-variable function.
(defvar is13194-to-ucs-devanagari-hashtbl nil)
(defvar is13194-to-ucs-devanagari-regexp nil)
(defvar is13194-to-ucs-bengali-hashtbl nil)
(defvar is13194-to-ucs-bengali-regexp nil)
(defvar is13194-to-ucs-assamese-hashtbl nil)
(defvar is13194-to-ucs-assamese-regexp nil)
(defvar is13194-to-ucs-gurmukhi-hashtbl nil)
(defvar is13194-to-ucs-gurmukhi-regexp nil)
(defvar is13194-to-ucs-gujarati-hashtbl nil)
(defvar is13194-to-ucs-gujarati-regexp nil)
(defvar is13194-to-ucs-oriya-hashtbl nil)
(defvar is13194-to-ucs-oriya-regexp nil)
(defvar is13194-to-ucs-tamil-hashtbl nil)
(defvar is13194-to-ucs-tamil-regexp nil)
(defvar is13194-to-ucs-telugu-hashtbl nil)
(defvar is13194-to-ucs-telugu-regexp nil)
(defvar is13194-to-ucs-malayalam-hashtbl nil)
(defvar is13194-to-ucs-malayalam-regexp nil)

(mapc 
 (function (lambda (script) 
   (let ((hashtable (intern (concat "is13194-to-ucs-" 
                                    (symbol-name script) "-hashtbl" )))
         (regexp    (intern (concat "is13194-to-ucs-"
                                    (symbol-name script) "-regexp"))))
     (set hashtable (make-hash-table :test 'equal :size 128))
     (mapc
      (function (lambda (x)
        (put-char-code-property (car x) 'script script)
        (put-char-code-property (car x) 'iscii (cdr x))
        (puthash (cdr x) (char-to-string (car x))
                 (eval hashtable))))
      (eval (intern (concat "ucs-" (symbol-name script)
                            "-to-is13194-alist"))))
     (set regexp (indian-regexp-of-hashtbl-keys (eval hashtable))))))
 '(devanagari bengali assamese gurmukhi gujarati
   oriya tamil telugu malayalam))

(defvar ucs-to-is13194-regexp
  ;; only Devanagari is supported now.
  (concat "[" (char-to-string #x0900)
          "-" (char-to-string #x097f) "]")
  "Regexp that matches to conversion")

(defun ucs-to-iscii-region (from to)
  "Converts the indian UCS characters in the region to ISCII.  
Returns new end position."
  (interactive "r")
  ;; only Devanagari is supported now.
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char (point-min))
      (let* ((current-repertory is13194-default-repartory))
        (while (re-search-forward ucs-to-is13194-regexp nil t)
          (replace-match 
           (get-char-code-property (string-to-char (match-string 0))
                                   'iscii))))
      (point-max))))

(defun iscii-to-ucs-region (from to)
  "Converts the ISCII characters in the region to UCS.  
Returns new end position."
  (interactive "r")
  ;; only Devanagari is supported now.
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char (point-min))
      (let* ((current-repertory is13194-default-repartory)
             (current-hashtable
              (intern (concat "is13194-to-ucs-" 
                              (symbol-name current-repertory) "-hashtbl")))
             (current-regexp
              (intern (concat "is13194-to-ucs-" 
                              (symbol-name current-repertory) "-regexp"))))
        (while (re-search-forward (eval current-regexp) nil t)
          (replace-match 
           (gethash (match-string 0) (eval current-hashtable) ""))))
      (point-max))))

;;;###autoload
(defun indian-compose-region (from to)
  "Compose the region according to `composition-function-table'. "
  (interactive "r")
  (save-excursion
    (save-restriction
      (let ((pos from) chars (max to))
        (narrow-to-region from to)
        (while (< pos max)
          (setq chars (compose-chars-after pos))
          (if chars (setq pos (+ pos chars)) (setq pos (1+ pos))))))))

;;;###autoload
(defun indian-compose-string (string)
  (with-temp-buffer 
    (insert string)
    (indian-compose-region (point-min) (point-max))
    (buffer-string)))

;;;###autoload
(defun in-is13194-post-read-conversion (len)
  (let ((pos (point)) endpos)
    (setq endpos (iscii-to-ucs-region pos (+ pos len)))
    (indian-compose-region pos endpos)
    (- endpos pos)))

;;;###autoload
(defun in-is13194-pre-write-conversion (from to)
  (let ((buf (current-buffer)))
    (set-buffer (generate-new-buffer " *temp*"))
    (if (stringp from)
	(insert from)
      (insert-buffer-substring buf from to))
    (ucs-to-iscii-region (point-min) (point-max))
    nil))




;;; Backward Compatibility support programs

;; The followings provides the conversion from old-implementation of
;; Emacs Devanagari script to UCS.

(defconst indian-2-colum-to-ucs
  '(
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2120   $(5!!!"!#!$!%!&!'!(!)!*!+!,!-!.!/(B
  ("$(5!!(B" . "$,15A(B")
  ("$(5!"(B" . "$,15B(B")
  ("$(5!#(B" . "$,15C(B")
  ("$(5!$(B" . "$,15E(B")
  ("$(5!%(B" . "$,15F(B")
  ("$(5!&(B" . "$,15G(B")
  ("$(5!'(B" . "$,15H(B")
  ("$(5!((B" . "$,15I(B")
  ("$(5!)(B" . "$,15J(B")
  ("$(5!*(B" . "$,15K(B")
  ("$(5!*"p(B" . "$,15p6#(B")
  ("$(5!+(B" . "$,15N(B")
  ("$(5!,(B" . "$,15O(B")
  ("$(5!-(B" . "$,15P(B")
  ("$(5!.(B" . "$,15M(B")
  ("$(5!/(B" . "$,15R(B")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2130 $(5!0!1!2!3!4!5!6!7!8!9!:!;!<!=!>!?(B
  ("$(5!0(B" . "$,15S(B")
  ("$(5!1(B" . "$,15T(B")
  ("$(5!2(B" . "$,15Q(B")
  ("$(5!3(B" . "$,15U(B")
  ("$(5!4(B" . "$,15V(B")
  ("$(5!5(B" . "$,15W(B")
  ("$(5!6(B" . "$,15X(B")
  ("$(5!7(B" . "$,15Y(B")
  ("$(5!8(B" . "$,15Z(B")
  ("$(5!9(B" . "$,15[(B")
  ("$(5!:(B" . "$,15\(B")
  ("$(5!;(B" . "$,15](B")
  ("$(5!<(B" . "$,15^(B")
  ("$(5!=(B" . "$,15_(B")
  ("$(5!>(B" . "$,15`(B")
  ("$(5!?(B" . "$,15a(B")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2140 $(5!@!A!B!C!D!E!F!G!H!I!J!K!L!M!N!O(B
  ("$(5!@(B" . "$,15b(B")
  ("$(5!A(B" . "$,15c(B")
  ("$(5!B(B" . "$,15d(B")
  ("$(5!C(B" . "$,15e(B")
  ("$(5!D(B" . "$,15f(B")
  ("$(5!E(B" . "$,15g(B")
  ("$(5!F(B" . "$,15h(B")
  ("$(5!G(B" . "$,15i(B")
  ("$(5!H(B" . "$,15j(B")
  ("$(5!I(B" . "$,15k(B")
  ("$(5!J(B" . "$,15l(B")
  ("$(5!K(B" . "$,15m(B")
  ("$(5!L(B" . "$,15n(B")
  ("$(5!M(B" . "$,15o(B")
  ("$(5!N(B" . "$,16?(B")
  ("$(5!O(B" . "$,15p(B")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2150 $(5!P!Q!R!S!T!U!V!W!X!Y!Z![!\!]!^!_(B
  ("$(5!P(B" . "$,15q(B")
  ("$(5!Q(B" . "$,15r(B")
  ("$(5!R(B" . "$,15s(B")
  ("$(5!S(B" . "$,15t(B")
  ("$(5!T(B" . "$,15u(B")
  ("$(5!U(B" . "$,15v(B")
  ("$(5!V(B" . "$,15w(B")
  ("$(5!W(B" . "$,15x(B")
  ("$(5!X(B" . "$,15y(B")
  ("$(5!Z(B" . "$,15~(B")
  ("$(5![(B" . "$,15(B")
  ("$(5!\(B" . "$,16 (B")
  ("$(5!](B" . "$,16!(B")
  ("$(5!^(B" . "$,16"(B")
  ("$(5!_(B" . "$,16#(B")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2160 $(5!`!a!b!c!d!e!f!g!h!i!j!k!l!m!n!o(B
  ("$(5!`(B" . "$,16&(B")
  ("$(5!a(B" . "$,16'(B")
  ("$(5!b(B" . "$,16((B")
  ("$(5!c(B" . "$,16%(B")
  ("$(5!d(B" . "$,16*(B")
  ("$(5!e(B" . "$,16+(B")
  ("$(5!f(B" . "$,16,(B")
  ("$(5!g(B" . "$,16)(B")
  ("$(5!h(B" . "$,16-(B")
  ("$(5!i(B" . "$,15|(B")
  ("$(5!j(B" . "$,16D(B")
  ("$(5!j!j(B" . "$,16E(B")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2170 $(5!p!q!r!s!t!u!v!w!x!y!z!{!|!}!~(B
  ("$(5!q(B" . "$,16F(B")
  ("$(5!r(B" . "$,16G(B")
  ("$(5!s(B" . "$,16H(B")
  ("$(5!t(B" . "$,16I(B")
  ("$(5!u(B" . "$,16J(B")
  ("$(5!v(B" . "$,16K(B")
  ("$(5!w(B" . "$,16L(B")
  ("$(5!x(B" . "$,16M(B")
  ("$(5!y(B" . "$,16N(B")
  ("$(5!z(B" . "$,16O(B")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2220   $(5"!"""#"$"%"&"'"(")"*"+","-"."/(B
  ("$(5"!(B" . "$,16;6-5p(B")
  ("$(5""(B" . "$,16>6-5p(B")
  ("$(5"#(B" . "$,15U6-5p(B")
  ("$(5"$(B" . "$,15W6-5p(B")
  ("$(5"%(B" . "$,15d6-5p(B")
  ("$(5"&(B" . "$,15j6-5p(B")
  ("$(5"'(B" . "$,15k6-5p(B")
  ("$(5")(B" . "$,15v6-5p(B")
  ("$(5",(B" . "$,15p6!(B")
  ("$(5"-(B" . "$,15p6"(B")
  ("$(5".(B" . "$,15q6!(B")
  ("$(5"/(B" . "$,15q6"(B")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2230 $(5"0"1"2"3"4"5"6"7"8"9":";"<"=">"?(B
  ("$(5"3(B" . "$,15U6-(B")
  ("$(5"4(B" . "$,15V6-(B")
  ("$(5"5(B" . "$,15W6-(B")
  ("$(5"6(B" . "$,15X6-(B")
  ("$(5"8(B" . "$,15Z6-(B")
  ("$(5"8"q(B" . "$,15Z6-5p6-(B")
  ("$(5":(B" . "$,15\6-(B")
  ("$(5";(B" . "$,15]6-(B")
  ("$(5"<(B" . "$,15^6-(B")
  ("$(5"<(B" . "$,15^6-(B")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2240 $(5"@"A"B"C"D"E"F"G"H"I"J"K"L"M"N"O(B
  ("$(5"A(B" . "$,15c6-(B")
  ("$(5"B(B" . "$,15d6-(B")
  ("$(5"C(B" . "$,15e6-(B")
  ("$(5"E(B" . "$,15g6-(B")
  ("$(5"F(B" . "$,15h6-(B")
  ("$(5"G(B" . "$,15i6-(B")
  ("$(5"H(B" . "$,15j6-(B")
  ("$(5"I(B" . "$,15k6-(B")
  ("$(5"J(B" . "$,15l6-(B")
  ("$(5"J(B" . "$,15l6-(B")
  ("$(5"K(B" . "$,15m6-(B")
  ("$(5"L(B" . "$,15n6-(B")
  ("$(5"M(B" . "$,15o6-(B")
  ("$(5"N(B" . "$,16?6-(B")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2250 $(5"P"Q"R"S"T"U"V"W"X"Y"Z"["\"]"^"_(B
  ("$(5"Q(B" . "$,15r6-(B")
  ("$(5"R(B" . "$,15s6-(B")
  ("$(5"S(B" . "$,15t6-(B")
  ("$(5"T(B" . "$,15u6-(B")
  ("$(5"U(B" . "$,15v6-(B")
  ("$(5"V(B" . "$,15w6-(B")
  ("$(5"W(B" . "$,15x6-(B")
  ("$(5"](B" . "$,16-5o(B")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2260 $(5"`"a"b"c"d"e"f"g"h"i"j"k"l"m"n"o(B
  ("$(5"`(B" . "$,15W6-5p6-(B")
  ("$(5"a(B" . "$,15X6-5h6-(B")
  ("$(5"c(B" . "$,15d6-5d6-(B")
  ("$(5"d(B" . "$,15d6-5p6-(B")
  ("$(5"e(B" . "$,15g6-5h6-(B")
  ("$(5"f(B" . "$,15g6-5p6-(B")
  ("$(5"g(B" . "$,15j6-5d6-(B")
  ("$(5"h(B" . "$,15v6-5Z6-(B")
  ("$(5"i(B" . "$,15v6-5p6-(B")
  ("$(5"j(B" . "$,15v6-5u6-(B")
  ("$(5"k(B" . "$,15h6-5h6-(B")
  ("$(5"l(B" . "$,15U6-5w6-(B")
  ("$(5"m(B" . "$,15\6-5^6-(B")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2270 $(5"p"q"r"s"t"u"v"w"x"y"z"{"|"}"~(B
  ("$(5"p(B" . "$,15p6-(B")
  ("$(5"q(B" . "$,16-5p(B")
  ("$(5"r(B" . "$,16-5p(B")
  ("$(5"s(B" . "$,1686-(B")
  ("$(5"t(B" . "$,1696-(B")
  ("$(5"u(B" . "$,16:6-(B")
  ("$(5"y(B" . "$,16>6-(B")
  ("$(5"z(B" . "$,16;6-(B")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2320   $(5#!#"###$#%#&#'#(#)#*#+#,#-#.#/(B
  ("$(5#!(B" . "$,160(B")
  ("$(5#&(B" . "$,15L(B")
  ("$(5#&"p(B" . "$,15p6$(B")
  ("$(5#'(B" . "$,16A(B")
  ("$(5#'"p(B" . "$,15p6C(B")
  ("$(5#*(B" . "$,16@(B")
  ("$(5#*"p(B" . "$,15p6B(B")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2330 $(5#0#1#2#3#4#5#6#7#8#9#:#;#<#=#>#?(B
  ("$(5#3(B" . "$,168(B")
  ("$(5#4(B" . "$,169(B")
  ("$(5#5(B" . "$,16:(B")
  ("$(5#:(B" . "$,16;(B")
  ("$(5#?(B" . "$,16<(B")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2340 $(5#@#A#B#C#D#E#F#G#H#I#J#K#L#M#N#O(B
  ("$(5#@(B" . "$,16=(B")
  ("$(5#I(B" . "$,16>(B")
  ("$(5#J(B" . "$,15}(B")
  ("$(5#K(B" . "$,16$(B")
  ("$(5#L(B" . "$,16B(B")
  ("$(5#M(B" . "$,16C(B")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2350 $(5#P#Q#R#S#T#U#V#W#X#Y#Z#[#\#]#^#_(B
  ("$(5#P(B" . "$,15n6-5h(B")
  ("$(5#Q(B" . "$,15n6-5r(B")
  ("$(5#R(B" . "$,15y6#(B")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2360 $(5#`#a#b#c#d#e#f#g#h#i#j#k#l#m#n#o(B
  ("$(5#`(B" . "$,15r6-5r(B")
  ("$(5#a(B" . "$,15u6-5h(B")
  ("$(5#b(B" . "$,15u6-5u(B")
  ("$(5#c(B" . "$,15v6-5Z(B")
  ("$(5#d(B" . "$,15v6-5h(B")
  ("$(5#e(B" . "$,15v6-5l(B")
  ("$(5#f(B" . "$,15v6-5r(B")
  ("$(5#g(B" . "$,15v6-5u(B")
  ("$(5#h(B" . "$,15w6-5_6-5p6-5o(B")
  ("$(5#i(B" . "$,15w6-5_6-5o(B")
  ("$(5#j(B" . "$,15w6-5_6-5u(B")
  ("$(5#k(B" . "$,15w6-5_(B")
  ("$(5#l(B" . "$,15w6-5`(B")
  ("$(5#m(B" . "$,15x6-5h(B")
  ("$(5#n(B" . "$,15x6-5p(B")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2370 $(5#p#q#r#s#t#u#v#w#x#y#z#{#|#}#~(B
  ("$(5#p(B" . "$,15y6-5c(B")
  ("$(5#q(B" . "$,15y6-5h(B")
  ("$(5#r(B" . "$,15y6-5n(B")
  ("$(5#s(B" . "$,15y6-5o(B")
  ("$(5#t(B" . "$,15y6-5p(B")
  ("$(5#u(B" . "$,15y6-5r(B")
  ("$(5#v(B" . "$,15y6-5u(B")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2420   $(5$!$"$#$$$%$&$'$($)$*$+$,$-$.$/(B
  ("$(5$!(B" . "$,15U6-5d6-5p6-5o(B")
  ("$(5$"(B" . "$,15U6-5d6-5u(B")
  ("$(5$#(B" . "$,15U6-5d6-5o(B")
  ("$(5$$(B" . "$,15U6-5h6-5o(B")
  ("$(5$%(B" . "$,15U6-5p6-5o(B")
  ("$(5$&(B" . "$,15U6-5u6-5o(B")
  ("$(5$'(B" . "$,15U6-5U(B")
  ("$(5$((B" . "$,15U6-5d(B")
  ("$(5$)(B" . "$,15U6-5h(B")
  ("$(5$*(B" . "$,15U6-5n(B")
  ("$(5$+(B" . "$,15U6-5o(B")
  ("$(5$,(B" . "$,15U6-5r(B")
  ("$(5$-(B" . "$,15U6-5u(B")
  ("$(5$.(B" . "$,15U6-5w(B")
  ("$(5$/(B" . "$,15X6-5h(B")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2430 $(5$0$1$2$3$4$5$6$7$8$9$:$;$<$=$>$?(B
  ("$(5$0(B" . "$,15Y6-5U6-5d6-5o(B")
  ("$(5$1(B" . "$,15Y6-5U6-5w6-5u(B")
  ("$(5$2(B" . "$,15Y6-5U6-5d(B")
  ("$(5$3(B" . "$,15Y6-5U6-5w(B")
  ("$(5$4(B" . "$,15Y6-5X6-5p(B")
  ("$(5$5(B" . "$,15Y6-5U6-5o(B")
  ("$(5$6(B" . "$,15Y6-5V6-5o(B")
  ("$(5$7(B" . "$,15Y6-5W6-5o(B")
  ("$(5$8(B" . "$,15Y6-5X6-5o(B")
  ("$(5$9(B" . "$,15Y6-5U(B")
  ("$(5$:(B" . "$,15Y6-5V(B")
  ("$(5$;(B" . "$,15Y6-5W(B")
  ("$(5$<(B" . "$,15Y6-5X(B")
  ("$(5$=(B" . "$,15Y6-5Y(B")
  ("$(5$>(B" . "$,15Y6-5h(B")
  ("$(5$?(B" . "$,15Y6-5n(B")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2440 $(5$@$A$B$C$D$E$F$G$H$I$J$K$L$M$N$O(B
  ("$(5$@(B" . "$,15Y6-5o(B")
  ("$(5$A(B" . "$,15Z6-5Z(B")
  ("$(5$B(B" . "$,15Z6-5^(B")
  ("$(5$C(B" . "$,15[6-5o(B")
  ("$(5$D(B" . "$,15\6-5p(B")
  ("$(5$E(B" . "$,15\6-5^(B")
  ("$(5$F(B" . "$,15^6-5Z(B")
  ("$(5$G(B" . "$,15^6-5\(B")
  ("$(5$H(B" . "$,15_6-5U(B")
  ("$(5$I(B" . "$,15_6-5_(B")
  ("$(5$J(B" . "$,15_6-5`(B")
  ("$(5$K(B" . "$,15_6-5o(B")
  ("$(5$L(B" . "$,15`6-5o(B")
  ("$(5$M(B" . "$,15a6-5W6-5o(B")
  ("$(5$N(B" . "$,15a6-5X6-5p(B")
  ("$(5$O(B" . "$,15a6-5p6-5o(B")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2450 $(5$P$Q$R$S$T$U$V$W$X$Y$Z$[$\$]$^$_(B
  ("$(5$P(B" . "$,15a6-5W(B")
  ("$(5$Q(B" . "$,15a6-5X(B")
  ("$(5$R(B" . "$,15a6-5a(B")
  ("$(5$S(B" . "$,15a6-5n(B")
  ("$(5$T(B" . "$,15a6-5o(B")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2460 $(5$`$a$b$c$d$e$f$g$h$i$j$k$l$m$n$o(B
  ("$(5$`(B" . "$,15b6-5o(B")
  ("$(5$a(B" . "$,15d6-5d(B")
  ("$(5$b(B" . "$,15d6-5h(B")
  ("$(5$c(B" . "$,15f6-5f6-5o(B")
  ("$(5$d(B" . "$,15f6-5g6-5o(B")
  ("$(5$e(B" . "$,15f6-5m6-5o(B")
  ("$(5$f(B" . "$,15f6-5p6-5o(B")
  ("$(5$g(B" . "$,15f6-5u6-5o(B")
  ("$(5$h(B" . "$,15f6-5W6-5p(B")
  ("$(5$i(B" . "$,15f6-5X6-5p(B")
  ("$(5$j(B" . "$,15f6-5f6-5u(B")
  ("$(5$k(B" . "$,15f6-5g6-5u(B")
  ("$(5$l(B" . "$,15f6-5W(B")
  ("$(5$m(B" . "$,15f6-5X(B")
  ("$(5$n(B" . "$,15f6-5f(B")
  ("$(5$o(B" . "$,15f6-5g(B")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2470 $(5$p$q$r$s$t$u$v$w$x$y$z${$|$}$~(B
  ("$(5$p(B" . "$,15f6-5h(B")
  ("$(5$q(B" . "$,15f6-5l(B")
  ("$(5$r(B" . "$,15f6-5m(B")
  ("$(5$s(B" . "$,15f6-5n(B")
  ("$(5$t(B" . "$,15f6-5o(B")
  ("$(5$u(B" . "$,15f6-5u(B")
  ("$(5$v(B" . "$,15g6-5h(B")
  ("$(5$w(B" . "$,15h6-5h(B")
  ("$(5$x(B" . "$,15j6-5d(B")
  ("$(5$y(B" . "$,15j6-5h(B")
  ("$(5$z(B" . "$,15j6-5r(B")
  ("$(5${(B" . "$,15l6-5h(B")
  ("$(5$|(B" . "$,15l6-5l(B")
  ("$(5$}(B" . "$,15l6-5u(B")
  ("$(5$~(B" . "$,15m6-5h(B")))

(defconst indian-2-column-to-ucs-regexp
  "$(5!j!j(B\\|$(5"8"q(B\\|[$(5#&#'!*#*(B]$(5"p(B\\|[$(5!!(B-$(5$~(B]")

(put 'indian-2-column-to-ucs-chartable 'char-table-extra-slots 1)
(defconst indian-2-column-to-ucs-chartable
  (let ((table (make-char-table 'indian-2-column-to-ucs-chartable))
        (alist nil))
    (dolist (elt indian-2-colum-to-ucs)
      (if (= (length (car elt)) 1)
          (aset table (aref (car elt) 0) (cdr elt))
        (setq alist (cons elt alist))))
    (set-char-table-extra-slot table 0 alist)
    table))

(defun indian-2-column-to-ucs-region (from to)
  "Convert old Emacs Devanagari characters to UCS."
  (interactive "r")
  (save-excursion
    (save-restriction
      (let ((pos from)
            (alist (char-table-extra-slot indian-2-column-to-ucs-chartable 0)))
        (narrow-to-region from to)
        (decompose-region from to)
        (goto-char (point-min))
        (while (re-search-forward indian-2-column-to-ucs-regexp nil t)
          (let ((len (- (match-end 0) (match-beginning 0)))
                subst)
            (if (= len 1)
                (setq subst (aref indian-2-column-to-ucs-chartable
				  (char-after (match-beginning 0))))
              (setq subst (assoc (match-string 0) alist)))
            (replace-match (if subst subst "?"))))
        (indian-compose-region (point-min) (point-max))))))

;;;###autoload
(defun indian-glyph-char (index &optional script)
  "Return character of charset `indian-glyph' made from glyph index INDEX.
The variable `indian-default-script' specifies the script of the glyph.
Optional argument SCRIPT, if non-nil, overrides `indian-default-script'.
See also the function `indian-char-glyph'."
  (or script
      (setq script indian-default-script))
  (let ((offset (get script 'indian-glyph-code-offset)))
    (or (integerp offset)
	(error "Invalid script name: %s" script))
    (or (and (>= index 0) (< index 256))
	(error "Invalid glyph index: %d" index))
    (setq index (+ offset index))
    (make-char 'indian-glyph (+ (/ index 96) 32) (+ (% index 96) 32))))

(defvar indian-glyph-max-char
  (indian-glyph-char
   255 (aref indian-script-table (1- (length indian-script-table))))
  "The maximum valid code of characters in the charset `indian-glyph'.")

;;;###autoload
(defun indian-char-glyph (char)
  "Return information about the glyph code for CHAR of `indian-glyph' charset.
The value is (INDEX . SCRIPT), where INDEX is the glyph index
in the font that Indian script name SCRIPT specifies.
See also the function `indian-glyph-char'."
  (let ((split (split-char char))
	code)
    (or (eq (car split) 'indian-glyph)
	(error "Charset of `%c' is not indian-glyph" char))
    (or (<= char indian-glyph-max-char)
	(error "Invalid indian-glyph char: %d" char))
    (setq code (+ (* (- (nth 1 split) 32) 96) (nth 2 split) -32))
    (cons (% code 256) (aref indian-script-table (/ code 256)))))
    
(provide 'ind-util)
 
;;; ind-util.el ends here
