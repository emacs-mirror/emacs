;;; mlm-util.el --- support for composing malayalam characters  -*-coding: iso-2022-7bit;-*-

;; Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008
;;   Free Software Foundation, Inc.

;; Maintainer:  KAWABATA, Taichi <kawabata@m17n.org>
;; Keywords: multilingual, Malayalam

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; Created: Feb. 11. 2003

;;; Commentary:

;; This file provides character(Unicode) to glyph(CDAC) conversion and
;; composition of Malayalam script characters.

;;; Code:

;; Malayalam Composable Pattern
;;    C .. Consonants
;;    V .. Vowel
;;    H .. Halant
;;    M .. Matra
;;    V .. Vowel
;;    A .. Anuswar
;;    D .. Chandrabindu
;;    (N .. Zerowidth Non Joiner)
;;    (J .. Zerowidth Joiner.  )
;; 1. vowel
;;  V(A|visargam)?
;; 2. syllable : maximum of 5 consecutive consonants.  (e.g. kartsnya)
;;  ((CH)?(CH)?(CH)?CH)?C(H|M?(A|D)?)?

(defconst malayalam-consonant
  "[$,1@5(B-$,1@Y(B]")

(defconst malayalam-composable-pattern
  (concat
   "\\([$,1@%(B-$,1@4(B][$,1@"(B]?\\)\\|$,1@#(B"
   "\\|\\("
   "\\(?:\\(?:[$,1@5(B-$,1@Y(B]$,1@m(B\\)?\\(?:[$,1@5(B-$,1@Y(B]$,1@m(B\\)?\\(?:[$,1@5(B-$,1@Y(B]$,1@m(B\\)?[$,1@5(B-$,1@Y(B]$,1@m(B\\)?"
   "[$,1@5(B-$,1@Y(B]\\(?:$,1@m(B\\|[$,1@^(B-$,1@c@f@g@h@j@j@k@l(B]?[$,1@"@m(B]?\\)?"
   "\\)")
  "Regexp matching a composable sequence of Malayalam characters.")

;;;###autoload
(defun malayalam-compose-region (from to)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char (point-min))
      (while (re-search-forward malayalam-composable-pattern nil t)
	(malayalam-compose-syllable-region (match-beginning 0)
					    (match-end 0))))))
(defun malayalam-compose-string (string)
  (with-temp-buffer
    (insert (decompose-string string))
    (malayalam-compose-region (point-min) (point-max))
    (buffer-string)))

;;;###autoload
(defun malayalam-post-read-conversion (len)
  (save-excursion
    (save-restriction
      (let ((buffer-modified-p (buffer-modified-p)))
	(narrow-to-region (point) (+ (point) len))
	(malayalam-compose-region (point-min) (point-max))
	(set-buffer-modified-p buffer-modified-p)
	(- (point-max) (point-min))))))

(defun malayalam-range (from to)
  "Make the list of the integers of range FROM to TO."
  (let (result)
    (while (<= from to) (setq result (cons to result) to (1- to))) result))

(defun malayalam-regexp-of-hashtbl-keys (hashtbl)
  "Return a regular expression that matches all keys in hashtable HASHTBL."
  (let ((max-specpdl-size 1000))
    (regexp-opt
     (sort
      (let (dummy)
	(maphash (function (lambda (key val) (setq dummy (cons key dummy)))) hashtbl)
	dummy)
      (function (lambda (x y) (> (length x) (length y))))))))


;;;###autoload
(defun malayalam-composition-function (pos  &optional string)
  "Compose Malayalam characters after the position POS.
If STRING is not nil, it is a string, and POS is an index to the string.
In this case, compose characters after POS of the string."
  (if string
      (if auto-compose-current-font
	  (if (eq (string-match "[$,1@ (B-$,1A?(B]+" pos) pos)
	      (or (font-shape-text 0 (match-end 0) auto-compose-current-font
				   string)
		  pos)))
    (goto-char pos)
    (if auto-compose-current-font
	(if (looking-at  "[$,1@ (B-$,1A?(B]+")
	    (or (font-shape-text pos (match-end 0) auto-compose-current-font)
		pos)
	  (if (looking-at malayalam-composable-pattern)
	      (prog1 (match-end 0)
		(malayalam-compose-syllable-region pos (match-end 0))))))))

;; Notes on conversion steps.

;; 1. chars to glyphs
;;
;; Simple replacement of characters to glyphs is done.

;; 2. glyphs reordering.
;;
;; Two special reordering rule takes place.
;; a. following "$,46[(B" goes to the front.
;; b. following "$,46S6S(B", "$,46S(B" or "$,46T(B" goes to the front.
;; This reordering occurs only to the last cluster of consonants.
;; Preceding consonants with halant characters are not affected.

;; 3. Composition.
;;
;; left modifiers will be attached at the left.
;; others will be attached right.

(defvar mlm-char-glyph
  '(;; various signs
    ("$,1@"(B" . "$,46W(B")
    ("$,1@#(B" . "$,46X(B")
    ;; Independent Vowels
    ("$,1@%(B" . "$,46!(B")
    ("$,1@&(B" . "$,46"(B")
    ("$,1@'(B" . "$,46#(B")
    ("$,1@((B" . "$,46#6U(B")
    ("$,1@)(B" . "$,46$(B")
    ("$,1@*(B" . "$,46$6U(B")
    ("$,1@+(B" . "$,46%(B")
    ("$,1@,(B" . "nil") ;; not in present use, not supported.
    ("$,1@.(B" . "$,46&(B")
    ("$,1@/(B" . "$,46'(B")
    ("$,1@0(B" . "$,46S6&(B")
    ("$,1@2(B" . "$,46((B")
    ("$,1@3(B" . "$,46(6M(B")
    ("$,1@4(B" . "$,46(6U(B")
    ;; Consonants
    ("$,1@5(B" . "$,46)(B")
    ("$,1@5@m@5(B" . "$,47!(B")
    ("$,1@5@m@S(B" . "$,47"(B")
    ("$,1@5@m@W(B" . "$,47#(B")
    ("$,1@5@m@?(B" . "$,47N(B")
    ("$,1@5@m@D(B" . "$,47`(B")
    ("$,1@5@a(B" . "$,47f(B")
    ("$,1@5@m@5@a(B" . "$,47g(B")
    ("$,1@5@a(B" . "$,47f(B")
    ("$,1@5@m@5@a(B" . "$,47g(B")

    ("$,1@6(B" . "$,46*(B")

    ("$,1@7(B" . "$,46+(B")
    ("$,1@7@m@7(B" . "$,47$(B")
    ("$,1@7@m@R(B" . "$,47%(B")
    ("$,1@7@m@N(B" . "$,47\(B")
    ("$,1@7@m@H(B" . "$,47a(B")

    ("$,1@8(B" . "$,46,(B")

    ("$,1@9(B" . "$,46-(B")
    ("$,1@9@m@5(B" . "$,47&(B")
    ("$,1@9@m@9(B" . "$,47'(B")
    ("$,1@9@m@5@a(B" . "$,47h(B")

    ("$,1@:(B" . "$,46.(B")
    ("$,1@:@m@:(B" . "$,47((B") ;; duplicate
    ("$,1@:@m@;(B" . "$,47Q(B")

    ("$,1@;(B" . "$,46/(B")

    ("$,1@<(B" . "$,460(B")
    ("$,1@<@m@<(B" . "$,47V(B")
    ("$,1@<@m@>(B" . "$,47Z(B")

    ("$,1@=(B" . "$,461(B")

    ("$,1@>(B" . "$,462(B")
    ("$,1@>@m@:(B" . "$,47)(B")
    ("$,1@>@m@>(B" . "$,47*(B")

    ("$,1@?(B" . "$,463(B")
    ("$,1@?@m@?(B" . "$,47+(B")

    ("$,1@@(B" . "$,464(B")
    ("$,1@A(B" . "$,465(B")
    ("$,1@A@m@A(B" . "$,47M(B")
    ("$,1@B(B" . "$,466(B")

    ("$,1@C(B" . "$,467(B")
    ("$,1@C@a@m(B" . "$,47,(B") ;; half consonant
    ("$,1@C@m@?(B" . "$,47-(B")
    ("$,1@C@m@C(B" . "$,47.(B")
    ("$,1@C@m@N(B" . "$,47W(B")
    ("$,1@C@m@A(B" . "$,47^(B")
    ("$,1@C@a(B" . "$,47i(B")

    ("$,1@D(B" . "$,468(B")
    ("$,1@D@m@D(B" . "$,47/(B")
    ("$,1@D@m@E(B" . "$,470(B")
    ("$,1@D@m@X(B" . "$,47U(B")
    ("$,1@D@m@M(B" . "$,47[(B")
    ("$,1@D@m@N(B" . "$,47_(B")

    ("$,1@E(B" . "$,469(B")

    ("$,1@F(B" . "$,46:(B")
    ("$,1@F@m@F(B" . "$,471(B")
    ("$,1@F@m@G(B" . "$,472(B")

    ("$,1@G(B" . "$,46;(B")

    ("$,1@H(B" . "$,46<(B")
    ("$,1@H@a@m(B" . "$,473(B") ;; half consonant
    ("$,1@H@m@D(B" . "$,474(B")
    ("$,1@H@m@F(B" . "$,475(B")
    ("$,1@H@m@H(B" . "$,476(B")
    ("$,1@H@m@N(B" . "$,477(B")
    ("$,1@H@m@G(B" . "$,47T(B")
    ("$,1@H@m@E(B" . "$,47Y(B")
    ("$,1@H@m@Q(B" . "$,47b(B")
    ("$,1@H@a(B" . "$,47k(B")
    ("$,1@H@m@H@a(B" . "$,47l(B")

    ("$,1@J(B" . "$,46=(B")
    ("$,1@J@m@J(B" . "$,478(B") ;; duplicate
    ("$,1@J@m@R(B" . "$,479(B") ;; lakar

    ("$,1@K(B" . "$,46>(B")

    ("$,1@L(B" . "$,46?(B")
    ("$,1@L@m@L(B" . "$,47:(B") ;; duplicate
    ("$,1@L@m@R(B" . "$,47;(B") ;; lakar
    ("$,1@L@m@G(B" . "$,47O(B")
    ("$,1@L@m@F(B" . "$,47P(B")

    ("$,1@M(B" . "$,46@(B")

    ("$,1@N(B" . "$,46A(B")
    ("$,1@N@m@J(B" . "$,47<(B")
    ("$,1@N@m@N(B" . "$,47=(B")
    ("$,1@N@m@R(B" . "$,47>(B") ;; lakar

    ("$,1@O(B" . "$,46B(B")
    ("$,1@O@m@O(B" . "$,47?(B") ;; duplicate
    ("$,1@O@m@5@m@5(B" . "$,47m(B")

    ("$,1@P(B" . "$,46C(B")
    ("$,1@P@a@m(B" . "$,47@(B")
    ("$,1@P@a(B" . "$,47j(B")

    ("$,1@Q(B" . "$,46D(B")
    ("$,1@Q@m(B" . "$,47@(B") ;; same glyph as "$,1@P@m(B"
    ("$,1@Q@a@m(B" . "$,47@(B") ;; same glyph as "$,1@P@m(B"
    ;;("$,1@Q@m@Q(B" . "$,47A(B")
    ("$,1@Q@m@Q(B" . "$,47d(B")

    ("$,1@R(B" . "$,46E(B")
    ("$,1@R@a@m(B" . "$,47B(B")
    ("$,1@R@m@R(B" . "$,47C(B") ;; lakar
    ("$,1@R@m@J(B" . "$,47e(B")

    ("$,1@S(B" . "$,46F(B")
    ("$,1@S@a@m(B" . "$,47D(B")
    ("$,1@S@m@S(B" . "$,47E(B")

    ("$,1@T(B" . "$,46G(B")

    ("$,1@U(B" . "$,46H(B")
    ("$,1@U@m@U(B" . "$,47F(B")

    ("$,1@V(B" . "$,46I(B")
    ("$,1@V@m@R(B" . "$,47G(B")
    ("$,1@V@m@V(B" . "$,47H(B")
    ("$,1@V@m@:(B" . "$,47](B")

    ("$,1@W(B" . "$,46J(B")
    ("$,1@W@m@?(B" . "$,47c(B")

    ("$,1@X(B" . "$,46K(B")
    ("$,1@X@m@R(B" . "$,47I(B")
    ("$,1@X@m@X(B" . "$,47J(B")
    ("$,1@X@m@Q@m@Q(B" . "$,47L(B")
    ("$,1@X@m@E(B" . "$,47X(B")

    ("$,1@Y(B" . "$,46L(B")
    ("$,1@Y@m@R(B" . "$,47K(B")
    ("$,1@Y@m@N(B" . "$,47R(B")
    ("$,1@Y@m@H(B" . "$,47S(B")

    ;; Dependent vowel signs
    ("$,1@^(B" . "$,46M(B")
    ("$,1@_(B" . "$,46N(B")
    ("$,1@`(B" . "$,46O(B")
    ("$,1@a(B" . "$,46P(B")
    ("$,1@b(B" . "$,46Q(B")
    ("$,1@c(B" . "$,46R(B")
    ("$,1@f(B" . "$,46S(B")
    ("$,1@g(B" . "$,46T(B")
    ("$,1@h(B" . "$,46S6S(B")
    ("$,1@j(B" . "$,46S6M(B")
    ("$,1@k(B" . "$,46T6M(B")
    ("$,1@l(B" . "$,46U(B")
    ;; Various signs
    ("$,1@m(B" . "$,46V(B")
    ("$,1@m@O(B" . "$,46Y(B") ;; yakar
    ("$,1@m@O@a(B" . "$,46\(B") ;; yakar + u
    ("$,1@m@O@b(B" . "$,46](B") ;; yakar + uu
    ("$,1@m@U(B" . "$,46Z(B") ;; vakar modifier
    ("$,1@m@P(B" . "$,46[(B") ;; rakar modifier is the same to rra modifier.
    ("$,1@m@P@m(B" . "$,46R(B") ;; halant + rakar + halant
    ("$,1@m@Q(B" . "$,46[(B") ;; rrakar modifier
    ("$,1@m@Q@m(B" . "$,46R(B") ;; halant + rrakar + halant
    ("$,1@m@m(B" . "$,46V(B") ;; double omission sign to stop forming half consonant.
    ("$,1@w(B" . "$,46U(B") ;; not in present use, already at 0D4C.
    ))

(defvar mlm-char-glyph-hash
  (let* ((hash (make-hash-table :test 'equal)))
    (mapc (function (lambda (x) (puthash (car x) (cdr x) hash)))
	  mlm-char-glyph)
    hash))

(defvar mlm-char-glyph-regexp
  (malayalam-regexp-of-hashtbl-keys mlm-char-glyph-hash))

;; Malayalam languages needed to be reordered in a complex mannar.

(defvar mlm-consonants
  (concat
  "$,46)6*6+6,6-6.6/606162636465666768696:6;6<6=6>6?6@6A6B6C6D6E6F6G6H6I6J6K6L(B"
  "$,47!7"7#7$7%7&7'7(7)7*7+7,7-7.7/707172737475767778797:7;7<7=7>7?7@7A7B7C7D7E7F7G7H7I7J7K7L7M7N7O7P7Q7R7S7T7U7V7W7X7Y7Z7[7\7]7^7_7`7a7b7c7d7e(B"
  ))

(defvar mlm-consonants-regexp
  (concat "\\($,46[(B?[" mlm-consonants "][$,46Y6Z(B]?\\)"))

(defvar mlm-glyph-reorder-key-glyphs "[$,46[6S6T(B]")

(defvar mlm-glyph-reordering-regexp-list
  `((,(concat "\\([" mlm-consonants "][$,46Y6Z(B]?\\)$,46[(B") . "$,46[(B\\1")
    (,(concat mlm-consonants-regexp "$,46S6S(B") . "$,46S6S(B\\1")
    (,(concat mlm-consonants-regexp "$,46S(B") . "$,46S(B\\1")
    (,(concat mlm-consonants-regexp "$,46T(B") . "$,46T(B\\1")))

(defun malayalam-compose-syllable-string (string)
  (with-temp-buffer
    (insert (decompose-string string))
    (malayalam-compose-syllable-region (point-min) (point-max))
    (buffer-string)))

(defun malayalam-compose-syllable-region (from to)
  "Compose malayalam syllable in region FROM to TO."
  (let (glyph-str
	match-str
	glyph-reorder-regexps
	glyph-reorder-replace
	glyph-reorder-regexp)
    (save-excursion
      (save-restriction
        (narrow-to-region from to)
        (goto-char (point-min))
        ;; char-glyph-conversion
        (while (not (eobp))
	  (if (looking-at mlm-char-glyph-regexp)
	      (progn
		(setq match-str (match-string 0)
		      glyph-str
		      (concat glyph-str
			      (gethash match-str mlm-char-glyph-hash)))
		(goto-char (match-end 0)))
	    (setq glyph-str (concat glyph-str (string (following-char))))
	    (forward-char 1)))
        (when (string-match mlm-glyph-reorder-key-glyphs glyph-str)
          ;; glyph reordering
          (setq glyph-reorder-regexps mlm-glyph-reordering-regexp-list)
          (while glyph-reorder-regexps
            (setq glyph-reorder-regexp (caar glyph-reorder-regexps))
            (setq glyph-reorder-replace (cdar glyph-reorder-regexps))
            (setq glyph-reorder-regexps (cdr glyph-reorder-regexps))
            (if (string-match glyph-reorder-regexp glyph-str)
                (setq glyph-str
                      (replace-match glyph-reorder-replace nil nil
                                     glyph-str)))))
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

(provide 'mlm-util)

;;; arch-tag: 7f25ee67-8f9d-49f2-837b-35c412c00eba
;;; devan-util.el ends here
