;;; quail/lrt.el --- Quail package for inputting Lao characters by LRT method

;; Copyright (C) 1997 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.

;; Keywords: multilingual, input method, Lao, LRT.

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

;;; Code:

(require 'quail)
(require 'lao-util)

;; LRT (Lao Roman Transcription) input method accepts the following
;; key sequence:
;;	consonant [+ semi-vowel-sign-lo ] + vowel [+ maa-sakod ] [+ tone-mark ]

(eval-and-compile

;; Upper vowels and tone-marks are put on the letter.
;; Semi-vowel-sign-lo and lower vowels are put under the letter.
(defconst lrt-single-consonant-table
  '(("k" . ?(1!(B)
    ("kh" . ?(1"(B)
    ("qh" . ?(1$(B)
    ("ng" . ?(1'(B)
    ("j" . ?(1((B)
    ("s" . ?(1J(B)
    ("x" . ?(1*(B)
    ("y" . ?(1-(B)
    ("d" . ?(14(B)
    ("t" . ?(15(B)
    ("th" . ?(16(B)
    ("dh" . ?(17(B)
    ("n" . ?(19(B)
    ("b" . ?(1:(B)
    ("p" . ?(1;(B)
    ("hp" . ?(1<(B)
    ("fh" . ?(1=(B)
    ("ph" . ?(1>(B)
    ("f" . ?(1?(B)
    ("m" . ?(1A(B)
    ("gn" . ?(1B(B)
    ("l" . ?(1E(B)
    ("r" . ?(1C(B)
    ("v" . ?(1G(B)
    ("w" . ?(1G(B)
    ("hh" . ?(1K(B)
    ("O" . ?(1M(B)
    ("h" . ?(1N(B)
    ("nh" . ?(1|(B)
    ("mh" . ?(1}(B)
    ("lh" . ?0(1K\1(B)
    ))

;; Semi-vowel-sign-lo is put under the first letter.
;; Lower vowels are put under the last letter.
;; Upper vowels and tone-marks are put on the last letter.
(defconst lrt-double-consonant-table
  '(("ngh" . "(1K'(B")
    ("yh" . "(1K](B")
    ("wh" . "(1KG(B")
    ("hl" . "(1KE(B")
    ("hy" . "(1K-(B")         
    ("hn" . "(1K9(B")
    ("hm" . "(1KA(B")
    ))

(defconst lrt-semi-vowel-sign-lo
  '("r" . ?(1\(B))

(defconst lrt-vowel-table
  '(("a" "(1P(B" (0 ?(1P(B) (0 ?(1Q(B))
    ("ar" "(1R(B" (0 ?(1R(B))
    ("i" "(1T(B" (0 ?(1T(B))
    ("ii" "(1U(B" (0 ?(1U(B))
    ("eu" "(1V(B" (0 ?(1V(B))
    ("ur" "(1W(B" (0 ?(1W(B))
    ("u" "(1X(B" (0 ?(1X(B))
    ("uu" "(1Y(B" (0 ?(1Y(B))
    ("e" "(1`(B (1P(B" (?(1`(B 0 ?(1P(B) (?(1`(B 0 ?(1Q(B))
    ("ee" "(1`(B" (?(1`(B 0))
    ("ae" "(1a(B (1P(B" (?(1a(B 0 ?(1P(B) (?(1a(B 0 ?(1Q(B))
    ("aa" "(1a(B" (?(1a(B 0))
    ("o" "(1b(B (1P(B" (?(1b(B 0 ?(1P(B) (0 ?(1[(B) (?(1-(B ?(1b(B 0 ?(1Q(B) (?(1G(B ?(1b(B 0 ?(1Q(B))
    ("oo" "(1b(B" (?(1b(B 0))
    ("oe" "(1`(B (1RP(B" (?(1`(B 0 ?(1R(B ?(1P(B) (0 ?(1Q(B ?(1M(B))
    ("or" "(1m(B" (0 ?(1m(B) (0 ?(1M(B))
    ("er" "(1`(B (1T(B" (?(1`(B 0 ?(1T(B))
    ("ir" "(1`(B (1U(B" (?(1`(B 0 ?(1U(B))
    ("oua" "(1[GP(B" (0 ?(1[(B ?(1G(B ?(1P(B) (0 ?(1Q(B ?(1G(B))
    ("ua" "(1[G(B" (0 ?(1[(B ?(1G(B) (0 ?(1G(B))
    ("ie" "(1`Q]P(B" (?(1`(B 0 ?(1Q(B ?(1](B ?(1P(B) (0 ?(1Q(B ?(1](B))
    ("ia" "(1`Q](B" (?(1`(B 0 ?(1Q(B ?(1](B) (0 ?(1](B))
    ("eua" "(1`VM(B" (?(1`(B 0 ?(1V(B ?(1M(B))
    ("ea" "(1`WM(B" (?(1`(B 0 ?(1W(B ?(1M(B))
    ("ai" "(1d(B" (?(1d(B 0))
    ("ei" "(1c(B" (?(1c(B 0))
    ("ao" "(1`[R(B" (?(1`(B 0 ?(1[(B ?(1R(B))
    ("arm" "(1S(B" (?(1S(B 0))))

;; Maa-sakod is put at the tail.
(defconst lrt-maa-sakod-table
  '((?k . ?(1!(B)
    (?g . ?(1'(B)
    (?y . ?(1-(B)
    (?d . ?(14(B)
    (?n . ?(19(B)
    (?b . ?(1:(B)
    (?m . ?(1A(B)
    (?v . ?(1G(B)
    (?w . ?(1G(B)
    ))

(defconst lrt-tone-mark-table
  '(("'" . ?(1h(B)
    ("\"" . ?(1i(B)
    ("^" . ?(1j(B)
    ("+" . ?(1k(B)
    ("~" . ?(1l(B)))

;; Return list of composing patterns for normal (without maa-sakod)
;; key sequence and with-maa-sakod key sequence starting with single
;; consonant C and optional SEMI-VOWEL.
(defun lrt-composing-pattern-single-c (c semi-vowel vowel-pattern)
  (let* ((patterns (copy-sequence vowel-pattern))
	 (tail patterns)
	 place)
    ;; Embed C and SEMI-VOWEL (if any) at the place of 0.
    (while tail
      ;; At first, make a copy.
      (setcar tail (copy-sequence (car tail)))
      ;; Then, do embedding.
      (setq place (memq 0 (car tail)))
      (setcar place c)
      (if semi-vowel
	  (setcdr place (cons semi-vowel (cdr place))))
      (setq tail (cdr tail)))
    patterns))

;; Return list of composing patterns for normal (without maa-sakod)
;; key sequence and with-maa-sakod key sequence starting with double
;; consonant STR and optional SEMI-VOWEL.
(defun lrt-composing-pattern-double-c (str semi-vowel vowel-pattern)
  (let* ((patterns (copy-sequence vowel-pattern))
	 (tail patterns)
	 (chars (string-to-list str))
	 place)
    ;; Embed C and SEMI-VOWEL (if any) at the place of 0.
    (while tail
      ;; At first, make a copy.
      (setcar tail (copy-sequence (car tail)))
      ;; Then, do embedding.
      (setq place (memq 0 (car tail)))
      (setcar place (car chars))
      (setcdr place (cons (nth 1 chars) (cdr place)))
      (if semi-vowel
	  ;; Embed SEMI-VOWEL in between CHARS.
	  (setcdr place (cons semi-vowel (cdr place))))
      (setq tail (cdr tail)))
    patterns))

;; Return a string made of characters in CHAR-LIST while composing
;; such characters as vowel-upper, vowel-lower, semi-vowel(lower),
;; and tone-mark with the preceding base character.
(defun lrt-compose-string (char-list)
  ;; Make a copy because the following work alters it.
  (setq char-list (copy-sequence char-list))
  (let ((i -1)
	(l char-list))
    (while l
      (if (memq (get-char-code-property (car l) 'phonetic-type)
		'(vowel-upper vowel-lower semivowel-lower tone))
	  (let (composed-char)
	    (if (< i 0)
		;; No preceding base character.
		(error "Invalid CHAR-LIST: %s" char-list))
	    (setq composed-char
		  (string-to-char (compose-chars (nth i char-list) (car l))))
	    (setcar (nthcdr i char-list) composed-char)
	    (setq l (cdr l))
	    (setcdr (nthcdr i char-list) l))
	(setq l (cdr l))
	(setq i (1+ i))))
    (concat (apply 'vector char-list))))

(defun lrt-compose-c-s-v (consonant semi-vowel vowel-pattern)
  (let ((pattern-list
	 (if (integerp consonant)
	     (lrt-composing-pattern-single-c
	      consonant semi-vowel vowel-pattern)
	   (lrt-composing-pattern-double-c
	    consonant semi-vowel vowel-pattern))))
    (cons (vector (lrt-compose-string (car pattern-list)))
	  (cons t pattern-list))))

)

(defun lrt-handle-maa-sakod ()
  (interactive)
  (if (or (= (length quail-current-key) 0)
	  (not quail-current-data))
      (quail-self-insert-command)
    (if (not (car quail-current-data))
	(progn
	  (setq quail-current-data nil)
	  (setq unread-command-events
		(cons last-command-event unread-command-events))
	  (quail-terminate-translation))
      (if (not (integerp last-command-event))
	  (error "Bogus calling sequence"))
      (let* ((maa-sakod (cdr (assq last-command-event lrt-maa-sakod-table)))
	     (maa-sakod-pattern (append
				 (or (cdr (assq maa-sakod
						(nthcdr 3 quail-current-data)))
				     (nth 2 quail-current-data)
				     (nth 1 quail-current-data))
				 (list maa-sakod))))
	(quail-delete-region)
	(setq quail-current-str (lrt-compose-string maa-sakod-pattern))
	(insert quail-current-str)
	(setq quail-current-key " ")
	(quail-show-translations)
	(setq quail-current-data (list nil maa-sakod-pattern))))))

(defun lrt-handle-tone-mark ()
  (interactive)
  (if (= (length quail-current-key) 0)
      (quail-self-insert-command)
    (if (not quail-current-data)
	(progn
	  (setq unread-command-events
		(cons last-command-event unread-command-events))
	  (quail-terminate-translation))
      (if (not (integerp last-command-event))
	  (error "Bogus calling sequence"))
      (let* ((tone-mark (cdr (assoc (char-to-string last-command-event)
				    lrt-tone-mark-table)))
	     (tone-mark-pattern
	      (if (car quail-current-data)
		  (copy-sequence (nth 1 quail-current-data))
		;; No need of copy because lrt-handle-maa-sakod should
		;; have already done it.
		(nth 1 quail-current-data)))
	     (tail tone-mark-pattern)
	     (double-consonant-tail '(?(1'(B ?(1](B ?(1G(B ?(1E(B ?(1-(B ?(19(B ?(1A(B))
	     place)
	;; Find a place to embed TONE-MARK.  It should be after a
	;; single or double consonant and following vowels.
	(while (and tail (not place))
	  (if (and
	       (eq (get-char-code-property (car tail) 'phonetic-type)
		   'consonant)
	       ;; Skip `(1K(B' if it is the first letter of double consonant.
	       (or (/= (car tail) ?(1K(B)
		   (not (cdr tail))
		   (not
		    (if (= (car (cdr tail)) ?(1\(B)
			(and (cdr (cdr tail))
			     (memq (car (cdr (cdr tail))) double-consonant-tail))
		      (memq (car (cdr tail)) double-consonant-tail)))))
	      (progn
		(setq place tail)
		(setq tail (cdr tail))
		(while (and tail
			    (memq (get-char-code-property (car tail)
							  'phonetic-type)
				  '(vowel-upper vowel-lower semivowel-lower)))
		  (setq place tail tail (cdr tail))))
	    (setq tail (cdr tail))))
	;; Embed TONE-MARK.
	(setcdr place (cons tone-mark (cdr place)))
	(quail-delete-region)
	(insert (lrt-compose-string tone-mark-pattern))
	(setq quail-current-data nil)
	(quail-terminate-translation)))))

(defmacro lrt-generate-quail-map ()
  `(quail-install-map
    ',(let ((map (list nil))
	    (semi-vowel-key (car lrt-semi-vowel-sign-lo))
	    (semi-vowel-char (cdr lrt-semi-vowel-sign-lo))
	    l1 e1 l2 e2 pattern key)
	;; Single consonants.
	(setq l1 lrt-single-consonant-table)
	(while l1
	  (setq e1 (car l1))
	  (quail-defrule-internal (car e1) (cdr e1) map)
	  (quail-defrule-internal
	   (concat (car e1) semi-vowel-key)
	   (compose-string (format "%c%c" (cdr e1) semi-vowel-char))
	   map)
	  (setq l2 lrt-vowel-table)
	  (while l2
	    (setq e2 (car l2))
	    (setq key (concat (car e1) (car e2))
		  pattern (lrt-compose-c-s-v (cdr e1) nil (nthcdr 2 e2)))
	    (quail-defrule-internal key pattern map)
	    (quail-defrule-internal
	     (concat key " ")
	     (vector (concat (aref (car pattern) 0) " "))  map)
	    (setq key (concat (car e1) semi-vowel-key (car e2))
		  pattern (lrt-compose-c-s-v (cdr e1) semi-vowel-char
					     (nthcdr 2 e2)))
	    (quail-defrule-internal key pattern map)
	    (quail-defrule-internal
	     (concat key " ")
	     (vector (concat (aref (car pattern) 0) " "))  map)
	    (setq l2 (cdr l2)))
	  (setq l1 (cdr l1)))

	;; Double consonants.
	(setq l1 lrt-double-consonant-table)
	(while l1
	  (setq e1 (car l1))
	  (quail-defrule-internal (car e1) (vector (cdr e1)) map)
	  (quail-defrule-internal
	   (concat (car e1) semi-vowel-key)
	   (vector (concat (compose-string
			    (format "%c%c" (sref (cdr e1) 0) semi-vowel-char))
			   (substring (cdr e1) (charset-bytes 'lao))))
	   map)
	  (setq l2 lrt-vowel-table)
	  (while l2
	    (setq e2 (car l2))
	    (setq key (concat (car e1) (car e2))
		  pattern (lrt-compose-c-s-v (cdr e1) nil (nthcdr 2 e2)))
	    (quail-defrule-internal key pattern map)
	    (quail-defrule-internal
	     (concat key " ")
	     (vector (concat (aref (car pattern) 0) " "))  map)
	    (setq key (concat (car e1) semi-vowel-key (car e2))
		  pattern (lrt-compose-c-s-v (cdr e1) semi-vowel-char
					     (nthcdr 2 e2)))
	    (quail-defrule-internal key pattern map)
	    (quail-defrule-internal
	     (concat key " ")
	     (vector (concat (aref (car pattern) 0) " "))  map)
	    (setq l2 (cdr l2)))
	  (setq l1 (cdr l1)))

	;; Vowels.
	(setq l1 lrt-vowel-table)
	(while l1
	  (setq e1 (car l1) l1 (cdr l1))
	  (quail-defrule-internal (car e1) (vector (nth 1 e1)) map))

	;; Tone-marks.
	(setq l1 lrt-tone-mark-table)
	(while l1
	  (setq e1 (car l1) l1 (cdr l1))
	  (quail-defrule-internal (car e1) (cdr e1) map))

	map)))

(quail-define-package
 "lao-lrt" "Lao" "(1"(BR" t
 "Lao input method using LRT (Lao Roman Transcription)"
 '(("k" . lrt-handle-maa-sakod)
   ("g" . lrt-handle-maa-sakod)
   ("y" . lrt-handle-maa-sakod)
   ("d" . lrt-handle-maa-sakod)
   ("n" . lrt-handle-maa-sakod)
   ("b" . lrt-handle-maa-sakod)
   ("m" . lrt-handle-maa-sakod)
   ("v" . lrt-handle-maa-sakod)
   ("w" . lrt-handle-maa-sakod)
   ("'" . lrt-handle-tone-mark)
   ("\"" . lrt-handle-tone-mark)
   ("^" . lrt-handle-tone-mark)
   ("+" . lrt-handle-tone-mark)
   ("~" . lrt-handle-tone-mark))
 'forget-last-selection 'deterministic 'kbd-translate 'show-layout)

(lrt-generate-quail-map)
