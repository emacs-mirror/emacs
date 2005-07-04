;;; tml-util.el --- support for composing tamil characters  -*-coding: iso-2022-7bit;-*-

;; Copyright (C) 2001 Free Software Foundation, Inc.

;; Maintainer: KAWABATA, Taichi <kawabata@m17n.org>
;; Keywords: multilingual, Indian, Tamil

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

;; Created: Nov. 08. 2002

;;; Commentary:

;; This file provides character(Unicode) to glyph(CDAC) conversion and
;; composition of Tamil script characters.

;;; Code:

;; Tamil Composable Pattern
;;    C .. Consonants
;;    V .. Vowel
;;    H .. Pulli
;;    M .. Matra
;;    V .. Vowel
;;    A .. Anuswar
;;    D .. Chandrabindu
;; 1. vowel
;;  V
;; 2. syllable : only ligature-formed pattern forms composition.
;;  (CkHCs|C)(H|M)?
;; 3. sri special
;;  (CsHCrVi)

;;  oririnal
;;  ((CH)?(CH)?(CH)?CH)?C(H|M?(A|D)?)?

(defconst tamil-consonant
  "[$,1<5(B-$,1<Y(B]")

(defconst tamil-composable-pattern
  (concat
   "\\([$,1<%(B-$,1<4(B]\\)\\|"
   "[$,1<"<#(B]\\|" ;; vowel modifier considered independent
   "\\(\\(?:\\(?:$,1<5<m<W(B\\)\\|[$,1<5(B-$,1<Y(B]\\)[$,1<m<^(B-$,1<l(B]?\\)\\|"
   "\\($,1<W<m<P<`(B\\)")
  "Regexp matching a composable sequence of Tamil characters.")

;;;###autoload
(defun tamil-compose-region (from to)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char (point-min))
      (while (re-search-forward tamil-composable-pattern nil t)
	(tamil-compose-syllable-region (match-beginning 0)
					    (match-end 0))))))
(defun tamil-compose-string (string)
  (with-temp-buffer
    (insert (decompose-string string))
    (tamil-compose-region (point-min) (point-max))
    (buffer-string)))

;;;###autoload
(defun tamil-post-read-conversion (len)
  (save-excursion
    (save-restriction
      (let ((buffer-modified-p (buffer-modified-p)))
	(narrow-to-region (point) (+ (point) len))
	(tamil-compose-region (point-min) (point-max))
	(set-buffer-modified-p buffer-modified-p)
	(- (point-max) (point-min))))))

(defun tamil-range (from to)
  "Make the list of the integers of range FROM to TO."
  (let (result)
    (while (<= from to) (setq result (cons to result) to (1- to))) result))

(defun tamil-regexp-of-hashtbl-keys (hashtbl)
  "Return a regular expression that matches all keys in hashtable HASHTBL."
  (let ((max-specpdl-size 1000))
    (regexp-opt
     (sort
      (let (dummy)
	(maphash (function (lambda (key val) (setq dummy (cons key dummy)))) hashtbl)
	dummy)
      (function (lambda (x y) (> (length x) (length y))))))))


;;;###autoload
(defun tamil-composition-function (from to pattern  &optional string)
  "Compose Tamil characters in REGION, or STRING if specified.
Assume that the REGION or STRING must fully match the composable 
PATTERN regexp."
  (if string (tamil-compose-syllable-string string)
    (tamil-compose-syllable-region from to))
  (- to from))

;; Register a function to compose Tamil characters.
(mapc
 (function (lambda (ucs)
   (aset composition-function-table (decode-char 'ucs ucs)
	 (list (cons tamil-composable-pattern
                     'tamil-composition-function)))))
 (nconc '(#x0b82 #x0b83) (tamil-range #x0b85 #x0bb9)))

;; Notes on conversion steps.

;; 1. chars to glyphs
;; Simple replacement of characters to glyphs is done.

;; 2. glyphs reordering.
;; following "$,4)j(B", "$,4)k(B", "$,4)l(B" goes to the front.

;; 3. glyphs to glyphs
;; reordered vowels are ligatured to consonants.

;; 4. Composition.
;; left modifiers will be attached at the left.
;; others will be attached right.

(defvar tml-char-glyph
  '(;; various signs
    ;;("$,1<"(B" . "")
    ("$,1<#(B" . "$,4*G(B")
    ;; Independent Vowels
    ("$,1<%(B" . "$,4*<(B")
    ("$,1<&(B" . "$,4*=(B")
    ("$,1<'(B" . "$,4*>(B")
    ("$,1<((B" . "$,4*?(B")
    ("$,1<)(B" . "$,4*@(B")
    ("$,1<*(B" . "$,4*A(B")
    ("$,1<.(B" . "$,4*B(B")
    ("$,1</(B" . "$,4*C(B")
    ("$,1<0(B" . "$,4*D(B")
    ("$,1<2(B" . "$,4*E(B")
    ("$,1<3(B" . "$,4*F(B")
    ("$,1<4(B" . "$,4*E*W(B")
    ;; Consonants
    ("$,1<5<m<W<m(B" . "$,4):(B")	; ks.
    ("$,1<5<m<W(B" . "$,4*^(B")	; ks
    ("$,1<5(B" . "$,4*H(B")

    ("$,1<9(B" . "$,4*I(B")
    ("$,1<:(B" . "$,4*J(B")
    ("$,1<<(B" . "$,4*\(B")
    ("$,1<<<m(B" . "$,4)8(B")
    ("$,1<>(B" . "$,4*K(B")
    ("$,1<?(B" . "$,4*L(B")
    ("$,1<C(B" . "$,4*M(B")
    ("$,1<D(B" . "$,4*N(B")
    ("$,1<H(B" . "$,4*O(B")
    ("$,1<I(B" . "$,4*Y(B")
    ("$,1<I<m(B" . "$,4)a(B")
    ("$,1<J(B" . "$,4*P(B")
    ("$,1<N(B" . "$,4*Q(B")
    ("$,1<O(B" . "$,4*R(B")
    ("$,1<P(B" . "$,4*S(B")
    ("$,1<Q(B" . "$,4*X(B")
    ("$,1<R(B" . "$,4*T(B")
    ("$,1<S(B" . "$,4*W(B")
    ("$,1<T(B" . "$,4*V(B")
    ("$,1<U(B" . "$,4*U(B")
    ("$,1<W(B" . "$,4*[(B")
    ("$,1<W<m(B" . "$,4)7(B")
    ("$,1<W<m<P<`(B" . "$,4*_(B")
    ("$,1<X(B" . "$,4*Z(B")
    ("$,1<X<m(B" . "$,4)6(B")
    ("$,1<Y(B" . "$,4*](B")
    ("$,1<Y<m(B" . "$,4)9(B")

    ;; Dependent vowel signs
    ("$,1<^(B" . "$,4)c(B")
    ("$,1<_(B" . "$,4)d(B")
    ("$,1<`(B" . "$,4)f(B")
    ("$,1<a(B" . "$,4)g(B")
    ("$,1<b(B" . "$,4)h(B")
    ("$,1<f(B" . "$,4)j(B")
    ("$,1<g(B" . "$,4)k(B")
    ("$,1<h(B" . "$,4)l(B")
    ("$,1<j(B" . "$,4)j)c(B")
    ("$,1<k(B" . "$,4)k)c(B")
    ("$,1<l(B" . "$,4)j*W(B")

    ;; Various signs
    ("$,1<m(B" . "$,4)b(B")
    ("$,1<w(B" . "nil") ;; not supported?
    ))

(defvar tml-char-glyph-hash
  (let* ((hash (make-hash-table :test 'equal)))
    (mapc (function (lambda (x) (puthash (car x) (cdr x) hash)))
	  tml-char-glyph)
    hash))

(defvar tml-char-glyph-regexp
  (tamil-regexp-of-hashtbl-keys tml-char-glyph-hash))

;; Tamil languages needed to be reordered.

(defvar tml-consonants-regexp
  "[$,4*H*^*I*J*\*K*L*M*N*O*Y*P*Q*R*S*X*T*W*V*U*[*Z*](B]")

(defvar tml-glyph-reorder-key-glyphs "[$,4)j)k)l(B]")

(defvar tml-glyph-reordering-regexp-list
  (cons
   (concat "\\(" tml-consonants-regexp "\\)\\([$,4)j)k)l(B]\\)") "\\2\\1"))

;; Tamil vowel modifiers to be ligatured.
(defvar tml-glyph-glyph
  '(
    ("$,4*H)d(B" . "$,4(a(B")	; ki
    ("$,4*^)d(B" . "$,4(v(B")	; ksi
    ("$,4*^)f(B" . "$,4)2(B")	; ksi~
    ("$,4*I)d(B" . "$,4(b(B")	; n^i
    ("$,4*J)d(B" . "$,4(c(B")	; ci
    ("$,4*K)d(B" . "$,4(d(B")	; n~i
    ("$,4*L)d(B" . "$,4)n(B")	; t.i
    ("$,4*M)d(B" . "$,4(e(B")	; n.i
    ("$,4*N)d(B" . "$,4(f(B")	; ti
    ("$,4*O)d(B" . "$,4(g(B")	; ni
    ("$,4*P)d(B" . "$,4(h(B")	; pi
    ("$,4*Q)d(B" . "$,4(i(B")	; mi
    ("$,4*R)d(B" . "$,4(j(B")	; yi
    ("$,4*S)d(B" . "$,4(k(B")	; ri
    ("$,4*T)d(B" . "$,4(l(B")	; li
    ("$,4*U)d(B" . "$,4(m(B")	; vi
    ("$,4*V)d(B" . "$,4(n(B")	; l_i
    ("$,4*W)d(B" . "$,4(o(B")	; l.i
    ("$,4*X)d(B" . "$,4(p(B")	; r_i
    ("$,4*Y)d(B" . "$,4(q(B")	; n_i
    ("$,4*Z)d(B" . "$,4(r(B")	; si
    ("$,4*[)d(B" . "$,4(s(B")	; s'i
    ("$,4*\)d(B" . "$,4(t(B")	; ji
    ("$,4*])d(B" . "$,4(u(B")	; hi

    ("$,4*H)f(B" . "$,4(w(B")	; ki~
    ("$,4*I)f(B" . "$,4(x(B")	; n^i~
    ("$,4*J)f(B" . "$,4(y(B")	; ci~
    ("$,4*K)f(B" . "$,4(z(B")	; n~i~
    ("$,4*L)f(B" . "$,4)o(B")	; t.i~
    ("$,4*M)f(B" . "$,4)!(B")	; n.i~
    ("$,4*N)f(B" . "$,4)"(B")	; ti~
    ("$,4*O)f(B" . "$,4)#(B")	; ni~
    ("$,4*P)f(B" . "$,4)$(B")	; pi~
    ("$,4*Q)f(B" . "$,4)%(B")	; mi~
    ("$,4*R)f(B" . "$,4)&(B")	; yi~
    ("$,4*S)f(B" . "$,4)'(B")	; ri~
    ("$,4*T)f(B" . "$,4)((B")	; li~
    ("$,4*U)f(B" . "$,4))(B")	; vi~
    ("$,4*V)f(B" . "$,4)*(B")	; l_i~
    ("$,4*W)f(B" . "$,4)+(B")	; l.i~
    ("$,4*X)f(B" . "$,4),(B")	; r_i~
    ("$,4*Y)f(B" . "$,4)-(B")	; n_i~
    ("$,4*Z)f(B" . "$,4).(B")	; si~
    ("$,4*[)f(B" . "$,4)/(B")	; s'i~
    ("$,4*\)f(B" . "$,4)0(B")	; ji~
    ("$,4*])f(B" . "$,4)1(B")	; hi~

    ("$,4*H)g(B" . "$,4)p(B")	; ku
    ("$,4*I)g(B" . "$,4)q(B")	; n^u
    ("$,4*J)g(B" . "$,4)r(B")	; cu
    ("$,4*K)g(B" . "$,4)s(B")	; n~u
    ("$,4*L)g(B" . "$,4)t(B")	; t.u
    ("$,4*M)g(B" . "$,4)u(B")	; n.u
    ("$,4*N)g(B" . "$,4)v(B")	; tu
    ("$,4*O)g(B" . "$,4)x(B")	; nu
    ("$,4*P)g(B" . "$,4)y(B")	; pu
    ("$,4*Q)g(B" . "$,4)z(B")	; mu
    ("$,4*R)g(B" . "$,4){(B")	; yu
    ("$,4*S)g(B" . "$,4)|(B")	; ru
    ("$,4*T)g(B" . "$,4)}(B")	; lu
    ("$,4*U)g(B" . "$,4)~(B")	; vu
    ("$,4*V)g(B" . "$,4)(B")	; l_u
    ("$,4*W)g(B" . "$,4* (B")	; l.u
    ("$,4*X)g(B" . "$,4*!(B")	; r_u
    ("$,4*Y)g(B" . "$,4*"(B")	; n_u

    ("$,4*H)h(B" . "$,4*#(B")	; ku~
    ("$,4*I)h(B" . "$,4*$(B")	; n^u~
    ("$,4*J)h(B" . "$,4*%(B")	; cu~
    ("$,4*K)h(B" . "$,4*&(B")	; n~u~
    ("$,4*L)h(B" . "$,4*'(B")	; t.u~
    ("$,4*M)h(B" . "$,4*((B")	; n.u~
    ("$,4*N)h(B" . "$,4*)(B")	; tu~
    ("$,4*O)h(B" . "$,4*+(B")	; nu~
    ("$,4*P)h(B" . "$,4*,(B")	; pu~
    ("$,4*Q)h(B" . "$,4*-(B")	; mu~
    ("$,4*R)h(B" . "$,4*.(B")	; yu~
    ("$,4*S)h(B" . "$,4*/(B")	; ru~
    ("$,4*T)h(B" . "$,4*6(B")	; lu~
    ("$,4*U)h(B" . "$,4*7(B")	; vu~
    ("$,4*V)h(B" . "$,4*8(B")	; l_u~
    ("$,4*W)h(B" . "$,4*9(B")	; l.u~
    ("$,4*X)h(B" . "$,4*:(B")	; r_u~
    ("$,4*Y)h(B" . "$,4*;(B")	; n_u~
    ))

(defvar tml-glyph-glyph-hash
  (let* ((hash (make-hash-table :test 'equal)))
    (mapc (function (lambda (x) (puthash (car x) (cdr x) hash)))
	  tml-glyph-glyph)
    hash))

(defvar tml-glyph-glyph-regexp
  (tamil-regexp-of-hashtbl-keys tml-glyph-glyph-hash))

(defun tamil-compose-syllable-string (string)
  (with-temp-buffer
    (insert (decompose-string string))
    (tamil-compose-syllable-region (point-min) (point-max))
    (buffer-string)))

(defun tamil-compose-syllable-region (from to)
  "Compose tamil syllable in region FROM to TO."
  (let (glyph-str match-str glyph-reorder-regexps)
    (save-excursion
      (save-restriction
        (narrow-to-region from to)
        (goto-char (point-min))
        ;; char-glyph-conversion
        (while (re-search-forward tml-char-glyph-regexp nil t)
          (setq match-str (match-string 0))
          (setq glyph-str
                (concat glyph-str (gethash match-str tml-char-glyph-hash))))
        ;; glyph reordering
        (when (string-match tml-glyph-reorder-key-glyphs glyph-str)
          (if (string-match (car tml-glyph-reordering-regexp-list)
                            glyph-str)
              (setq glyph-str
                    (replace-match (cdr tml-glyph-reordering-regexp-list)
                                   nil nil glyph-str))))
        ;; glyph-glyph-conversion
        (when (string-match tml-glyph-glyph-regexp glyph-str)
          (setq match-str (match-string 0 glyph-str))
          (setq glyph-str 
                (replace-match (gethash match-str tml-glyph-glyph-hash)
                               nil nil glyph-str)))
        ;; concatenate and attach reference-points.
        (setq glyph-str
              (cdr
               (apply
                'nconc
                (mapcar
                 (function 
                  (lambda (x) (list '(5 . 3) x))) ;; default ref. point.
                 glyph-str))))
        (compose-region from to glyph-str)))))

(provide 'tml-util)

;;; arch-tag: 4d1c9737-e7b1-44cf-a040-4f64c50e773e
;;; tml-util.el ends here
