;;; vietnamese.el --- support for Vietnamese -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.

;; Keywords: multilingual, Vietnamese

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

;; For Vietnames, the character sets VISCII, VSCII and TCVN-5712 are
;; supported.

;;; Code:

(defvar viet-viscii-decode-table
  [;; VISCII is a full 8-bit code.
   0 1 ?,2F(B 3 4 ?,2G(B ?,2g(B 7 8 9 10 11 12 13 14 15
   16 17 18 19 ?,2V(B 21 22 23 24 ?,2[(B 26 27 28 29 ?,2\(B 31
   32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47
   48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63
   64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79
   80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95
   96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111
   112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 
   ?,2U(B ?,2!(B ?,2"(B ?,2#(B ?,2$(B ?,2%(B ?,2&(B ?,2'(B ?,2((B ?,2)(B ?,2*(B ?,2+(B ?,2,(B ?,2-(B ?,2.(B ?,2/(B
   ?,20(B ?,21(B ?,22(B ?,25(B ?,2~(B ?,2>(B ?,26(B ?,27(B ?,28(B ?,2v(B ?,2w(B ?,2o(B ?,2|(B ?,2{(B ?,2x(B ?,2O(B
   ?,2u(B ?,1!(B ?,1"(B ?,1#(B ?,1$(B ?,1%(B ?,1&(B ?,1'(B ?,1((B ?,1)(B ?,1*(B ?,1+(B ?,1,(B ?,1-(B ?,1.(B ?,1/(B
   ?,10(B ?,11(B ?,12(B ?,2^(B ?,2=(B ?,15(B ?,16(B ?,17(B ?,18(B ?,2q(B ?,2Q(B ?,2W(B ?,2X(B ?,1=(B ?,1>(B ?,2_(B
   ?,2`(B ?,2a(B ?,2b(B ?,2c(B ?,2d(B ?,2e(B ?,1F(B ?,1G(B ?,2h(B ?,2i(B ?,2j(B ?,2k(B ?,2l(B ?,2m(B ?,2n(B ?,1O(B
   ?,2p(B ?,1Q(B ?,2r(B ?,2s(B ?,2t(B ?,1U(B ?,1V(B ?,1W(B ?,1X(B ?,2y(B ?,2z(B ?,1[(B ?,1\(B ?,2}(B ?,1^(B ?,1_(B
   ?,1`(B ?,1a(B ?,1b(B ?,1c(B ?,1d(B ?,1e(B ?,1f(B ?,1g(B ?,1h(B ?,1i(B ?,1j(B ?,1k(B ?,1l(B ?,1m(B ?,1n(B ?,1o(B
   ?,1p(B ?,1q(B ?,1r(B ?,1s(B ?,1t(B ?,1u(B ?,1v(B ?,1w(B ?,1x(B ?,1y(B ?,1z(B ?,1{(B ?,1|(B ?,1}(B ?,1~(B ?,2f(B ]
  "Vietnamese VISCII decoding table.")

(let ((table (make-translation-table-from-vector viet-viscii-decode-table)))
  (define-translation-table 'viet-viscii-nonascii-translation-table table)
  (define-translation-table 'viet-viscii-encode-table
    (char-table-extra-slot table 0)))

;;;
;;; VSCII is a pre-version of TCVN-5712 and deprecated
;;;
(defvar viet-vscii-decode-table
  [;; VSCII is a full 8-bit code.
   0 ?,2z(B ?,2x(B 3 ?,2W(B ?,2X(B ?,2f(B 7 8 9 10 11 12 13 14 15
   16 ?,2Q(B ?,2_(B ?,2O(B ?,2V(B ?,2[(B ?,2}(B ?,2\(B 24 25 26 27 28 29 30 31
   32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47
   48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63
   64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79
   80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95
   96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111
   112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 
   ?,2`(B ?,2d(B ?,2c(B ?,2a(B ?,2U(B ?,2#(B ?,2'(B ?,2h(B ?,2k(B ?,2((B ?,2i(B ?,2)(B ?,2.(B ?,2l(B ?,2o(B ?,2n(B
   ?,2m(B ?,28(B ?,2r(B ?,2v(B ?,2u(B ?,2s(B ?,2w(B ?,25(B ?,26(B ?,27(B ?,2^(B ?,2>(B ?,2~(B ?,2y(B ?,2|(B ?,2{(B
   160 ?,2e(B ?,2b(B ?,2j(B ?,2t(B ?,2=(B ?,2_(B ?,2p(B ?,1e(B ?,1b(B ?,1j(B ?,1t(B ?,1=(B ?,1y(B ?,1p(B ?,2"(B
   192 193 194 195 196 ?,1`(B ?,1d(B ?,1c(B ?,1a(B ?,1U(B ?,2F(B ?,1"(B ?,1F(B ?,1G(B ?,1!(B ?,2G(B
   ?,2!(B ?,2%(B ?,2&(B ?,2g(B ?,2%(B ?,2+(B ?,1#(B ?,1%(B ?,1&(B ?,1g(B ?,1$(B ?,1'(B ?,1h(B ?,2,(B ?,1k(B ?,1((B
   ?,1i(B ?,1)(B ?,1+(B ?,1,(B ?,1-(B ?,1*(B ?,1.(B ?,1l(B ?,1o(B ?,2-(B ?,2*(B ?,20(B ?,1n(B ?,1m(B ?,18(B ?,1r(B
   ?,21(B ?,1v(B ?,1u(B ?,1s(B ?,1w(B ?,10(B ?,11(B ?,12(B ?,1/(B ?,15(B ?,16(B ?,17(B ?,1^(B ?,1>(B ?,1~(B ?,1y(B
   ?,22(B ?,1|(B ?,1{(B ?,1z(B ?,1x(B ?,1W(B ?,1X(B ?,1f(B ?,1Q(B ?,1q(B ?,1O(B ?,1V(B ?,1[(B ?,1}(B ?,1\(B ?,2/(B]
  "Vietnamese VSCII decoding table.")

(let ((table (make-translation-table-from-vector viet-vscii-decode-table)))
  (define-translation-table 'viet-vscii-nonascii-translation-table table)
  (define-translation-table 'viet-vscii-encode-table
    (char-table-extra-slot table 0)))

;; Does not support combining characters in the range [176, 180]
(defvar viet-tcvn-decode-table
  [;; TCVN is a full 8-bit code.
   0 ?,2z(B ?,2x(B 3 ?,2W(B ?,2X(B ?,2f(B 7 8 9 10 11 12 13 14 15
   16 ?,2Q(B ?,2q(B ?,2O(B ?,2V(B ?,2[(B ?,2}(B ?,2\(B 24 25 26 27 28 29 30 31
   32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47
   48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63
   64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79
   80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95
   96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111
   112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 
   ?,2`(B ?,2d(B ?,2c(B ?,2a(B ?,2U(B ?,2#(B ?,2'(B ?,2h(B ?,2k(B ?,2((B ?,2i(B ?,2)(B ?,2.(B ?,2l(B ?,2o(B ?,2n(B
   ?,2m(B ?,28(B ?,2r(B ?,2v(B ?,2u(B ?,2s(B ?,2w(B ?,25(B ?,26(B ?,27(B ?,2^(B ?,2>(B ?,2~(B ?,2y(B ?,2|(B ?,2{(B
   160 ?,2e(B ?,2b(B ?,2j(B ?,2t(B ?,2=(B ?,2_(B ?,2p(B ?,1e(B ?,1b(B ?,1j(B ?,1t(B ?,1=(B ?,1_(B ?,1p(B ?,2"(B
   176 177 178 179 180 ?,1`(B ?,1d(B ?,1c(B ?,1a(B ?,1U(B ?,2F(B ?,1"(B ?,1F(B ?,1G(B ?,1!(B ?,2G(B
   ?,2!(B ?,2%(B ?,2&(B ?,2g(B ?,2$(B ?,2+(B ?,1#(B ?,1%(B ?,1&(B ?,1g(B ?,1$(B ?,1'(B ?,1h(B ?,2,(B ?,1k(B ?,1((B
   ?,1i(B ?,1)(B ?,1+(B ?,1,(B ?,1-(B ?,1*(B ?,1.(B ?,1l(B ?,1o(B ?,2-(B ?,2*(B ?,20(B ?,1n(B ?,1m(B ?,18(B ?,1r(B
   ?,21(B ?,1v(B ?,1u(B ?,1s(B ?,1w(B ?,10(B ?,11(B ?,12(B ?,1/(B ?,15(B ?,16(B ?,17(B ?,1^(B ?,1>(B ?,1~(B ?,1y(B
   ?,22(B ?,1|(B ?,1{(B ?,1z(B ?,1x(B ?,1W(B ?,1X(B ?,1f(B ?,1Q(B ?,1q(B ?,1O(B ?,1V(B ?,1[(B ?,1}(B ?,1\(B ?,2/(B]
  "Vietnamese TCVN-5712 decoding table.")

(let ((table (make-translation-table-from-vector viet-tcvn-decode-table)))
  (define-translation-table 'viet-tcvn-nonascii-translation-table table)
  (define-translation-table 'viet-tcvn-encode-table
    (char-table-extra-slot table 0)))

;; (defvar viet-vps-decode-table
;;   [;; VPS is a full 8-bit code.
;;    0 1 ?,2U(B ?,2'(B ?,2#(B ?,2)(B ?,2.(B 7 8 9 10 11 12 13 14 15
;;    ?,28(B ?,2w(B ?,25(B ?,2~(B ?,2x(B ?,2q(B 22 23 24 ?,2\(B 26 27 ?,2g(B ?,2f(B 30 31
;;    32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47
;;    48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63
;;    64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79
;;    80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95
;;    96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111
;;    112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127
;;    ?,2`(B ?,2d(B ?,2c(B ?,2$(B ?,2%(B ?,2&(B ?,1w(B ?,12(B ?,2e(B ?,1*(B ?,1+(B ?,1,(B ?,1.(B ?,2!(B ?,2"(B ?,2F(B
;;    ?,2*(B 145 146 ?,2+(B ?,2,(B ?,2-(B ?,2/(B ?,20(B ?,21(B ?,22(B ?,1}(B ?,1V(B ?,1\(B ?,2>(B ?,26(B ?,27(B
;;    160 ?,1!(B ?,1"(B ?,1F(B ?,1G(B ?,1#(B ?,2^(B ?,1>(B ?,2y(B ?,16(B ?,17(B ?,1^(B ?,2{(B ?,2Q(B ?,1~(B ?,2W(B
;;    ?,11(B ?,2X(B ?,2O(B ?,2[(B ?,2m(B ?,2l(B ?,15(B ?,2o(B ?,2n(B ?,2s(B ?,1X(B ?,1f(B ?,2r(B ?,2v(B ?,2u(B ?,1q(B
;;    ?,1%(B ?,2a(B ?,2b(B ?,1$(B ?,1&(B ?,1g(B ?,1'(B ?,1p(B ?,1k(B ?,2i(B ?,2j(B ?,1)(B ?,1o(B ?,1-(B ?,18(B ?,1[(B
;;    ?,2_(B ?,2|(B ?,10(B ?,1/(B ?,2t(B ?,1v(B ?,1=(B ?,2h(B ?,1W(B ?,1Q(B ?,2z(B ?,1{(B ?,1_(B ?,2}(B ?,2k(B 223
;;    ?,1`(B ?,1a(B ?,1b(B ?,1c(B ?,1d(B ?,1U(B ?,1e(B 231 ?,1h(B ?,1i(B ?,1j(B ?,1((B ?,1l(B ?,1m(B 238 ?,1n(B
;;    ?,2G(B ?,2p(B ?,1r(B ?,1s(B ?,1t(B ?,1u(B 246 ?,2=(B ?,1x(B ?,1y(B ?,1z(B ?,1|(B 252 ?,2V(B ?,2((B ?,1O(B]
;;   "Vietnamese VPS decoding table.")
;; 
;; (let ((table (make-translation-table-from-vector viet-vps-decode-table)))
;;   (define-translation-table 'viet-vps-nonascii-translation-table table)
;;   (define-translation-table 'viet-vps-encode-table
;;     (char-table-extra-slot table 0)))

(define-ccl-program ccl-decode-viscii
  `(3
    ((loop
      (r0 = 0)
      (read r1)
      (translate-character viet-viscii-nonascii-translation-table r0 r1)
      (write-multibyte-character r0 r1)
      (repeat))))
  "CCL program to decode VISCII 1.1")

(define-ccl-program ccl-encode-viscii
  `(1
    ((loop
      (read-multibyte-character r0 r1)
      (translate-character viet-viscii-encode-table r0 r1)
      (write-repeat r1))))
  "CCL program to encode VISCII 1.1")

(define-ccl-program ccl-encode-viscii-font
  `(0
    ;; In:  R0:vietnamese-viscii-lower/vietnamese-viscii-upper
    ;;      R1:position code
    ;; Out: R1:font code point
    (translate-character viet-viscii-encode-table r0 r1))
  "CCL program to encode Vietnamese chars to VISCII 1.1 font")

(define-ccl-program ccl-decode-vscii
  `(3
    ((loop
      (r0 = 0)
      (read r1)
      (translate-character viet-vscii-nonascii-translation-table r0 r1)
      (write-multibyte-character r0 r1)
      (repeat))))
  "CCL program to decode VSCII-1.")

(define-ccl-program ccl-encode-vscii
  `(1
    ((loop
      (read-multibyte-character r0 r1)
      (translate-character viet-vscii-encode-table r0 r1)
      (write-repeat r1))))
  "CCL program to encode VSCII-1.")

(define-ccl-program ccl-encode-vscii-font
  `(0
    ;; In:  R0:vietnamese-viscii-lower/vietnamese-viscii-upper
    ;;      R1:position code
    ;; Out: R1:font code point
    (translate-character viet-vscii-encode-table r0 r1))
  "CCL program to encode Vietnamese chars to VSCII-1 font.")

(define-ccl-program ccl-decode-tcvn
  `(3
    ((loop
      (r0 = 0)
      (read r1)
      (translate-character viet-tcvn-nonascii-translation-table r0 r1)
      (write-multibyte-character r0 r1)
      (repeat))))
  "CCL program to decode TCVN-5712.")

(define-ccl-program ccl-encode-tcvn
  `(1
    ((loop
      (read-multibyte-character r0 r1)
      (translate-character viet-tcvn-encode-table r0 r1)
      (write-repeat r1))))
  "CCL program to encode TCVN-5712.")

(define-ccl-program ccl-encode-tcvn-font
  `(0
    ;; In:  R0:vietnamese-viscii-lower/vietnamese-viscii-upper
    ;;      R1:position code
    ;; Out: R1:font code point
    (translate-character viet-tcvn-encode-table r0 r1))
  "CCL program to encode Vietnamese chars to TCVN-5712 font.")

;; (define-ccl-program ccl-decode-vps
;;   `(3
;;     ((loop
;; 	 (r0 = 0)
;; 	 (read r1)
;; 	 (translate-character viet-vps-nonascii-translation-table r0 r1)
;; 	 (write-multibyte-character r0 r1)
;; 	 (repeat))))
;;   "CCL program to decode VPS.")
;; 
;; (define-ccl-program ccl-encode-vps
;;   `(1
;;     ((loop
;; 	 (read-multibyte-character r0 r1)
;; 	 (translate-character viet-vps-encode-table r0 r1)
;; 	 (write-repeat r1))))
;;   "CCL program to encode VPS.")
;; 
;; (define-ccl-program ccl-encode-vps-font
;;   `(0
;;     ;; In:  R0:vietnamese-viscii-lower/vietnamese-viscii-upper
;;     ;;      R1:position code
;;     ;; Out: R1:font code point
;;     (translate-character viet-vps-encode-table r0 r1))
;;   "CCL program to encode Vietnamese chars to VPS font.")

(make-coding-system
 'vietnamese-viscii 4 ?V
 "8-bit encoding for Vietnamese VISCII 1.1 (MIME:VISCII)"
 '(ccl-decode-viscii . ccl-encode-viscii)
 '((safe-charsets ascii vietnamese-viscii-lower vietnamese-viscii-upper)
   (mime-charset . viscii)
   (valid-codes (0 . 255))))

(define-coding-system-alias 'viscii 'vietnamese-viscii)

(make-coding-system
 'vietnamese-vscii 4 ?v
 "8-bit encoding for Vietnamese VSCII-1"
 '(ccl-decode-vscii . ccl-encode-vscii)
 '((safe-charsets ascii vietnamese-viscii-lower vietnamese-viscii-upper)
   (valid-codes (0 . 255))))

(define-coding-system-alias 'vscii 'vietnamese-vscii)

(make-coding-system
 'vietnamese-tcvn 4 ?t
 "8-bit encoding for Vietnamese TCVN-5712"
 '(ccl-decode-tcvn . ccl-encode-tcvn)
 '((safe-charsets ascii vietnamese-viscii-lower vietnamese-viscii-upper)
   (valid-codes (0 . 255))))

(define-coding-system-alias 'tcvn 'vietnamese-tcvn)

;; (make-coding-system
;;  'vietnamese-vps 4 ?p
;;  "8-bit encoding for Vietnamese VPS"
;;  '(ccl-decode-vps . ccl-encode-vps)
;;  '((safe-charsets ascii vietnamese-viscii-lower vietnamese-viscii-upper)
;;    (valid-codes (0 . 255))))
;; 
;; (define-coding-system-alias 'vps 'vietnamese-vps)

(make-coding-system
 'vietnamese-viqr 0 ?q
 "Vietnamese latin transcription (VIQR)"
 nil
 '((safe-charsets ascii vietnamese-viscii-lower vietnamese-viscii-upper)
   (post-read-conversion . viqr-post-read-conversion)
   (pre-write-conversion . viqr-pre-write-conversion)
   (charset-origin-alist
    (vietnamese-viscii-lower "VISCII" viet-encode-viscii-char)
    (vietnamese-viscii-upper "VISCII" viet-encode-viscii-char))))


(define-coding-system-alias 'viqr 'vietnamese-viqr)

(setq font-ccl-encoder-alist
      (cons '("viscii" . ccl-encode-viscii-font) font-ccl-encoder-alist))

(setq font-ccl-encoder-alist
      (cons '("vscii" . ccl-encode-vscii-font) font-ccl-encoder-alist))

(setq font-ccl-encoder-alist
      (cons '("tcvn" . ccl-encode-tcvn-font) font-ccl-encoder-alist))

(set-language-info-alist
 "Vietnamese" `((charset vietnamese-viscii-lower vietnamese-viscii-upper)
		(nonascii-translation
		 . ,(get 'viet-viscii-nonascii-translation-table
			 'translation-table))
		(coding-system vietnamese-viscii vietnamese-vscii vietnamese-tcvn
			       vietnamese-viqr)
		(coding-priority vietnamese-viscii)
		(input-method . "vietnamese-viqr")
		(unibyte-display . vietnamese-viscii)
		(features viet-util)
		(sample-text . "Vietnamese (Ti,1*(Bng Vi,1.(Bt)	Ch,1`(Bo b,1U(Bn")
		(documentation . "\
For Vietnamese, Emacs uses special charasets internally.
They can be decoded from and encoded to VISCII, VSCII, TCVN-5712, and
VIQR.  VSCII is deprecated in favour of TCVN-5712.  Current setting
put higher priority to the coding system VISCII than TCVN-5712.  If
you prefer TCVN-5712, please do: (prefer-coding-system 'vietnamese-tcvn).
There are two Vietnamese input methods: VIQR and Telex, VIQR is the
default setting.")
		))

(provide 'vietnamese)

;;; vietnamese.el ends here
