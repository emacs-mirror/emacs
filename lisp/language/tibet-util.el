;;; tibet-util.el --- Support for inputting Tibetan characters

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.

;; Keywords: multilingual, Tibetan

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

;; Author: Toru TOMABECHI, <Toru.Tomabechi@orient.unil.ch>

;; Created: Feb. 17. 1997

;; History:
;; 1997.03.13 Modification in treatment of text properties;
;;            Support for some special signs and punctuations.

;;; Code:

;;;###autoload
(defun setup-tibetan-environment ()
  (interactive)
  (setup-english-environment)
  (setq coding-category-iso-8-2 'tibetan)

  (setq-default buffer-file-coding-system 'iso-2022-7bit)

  (setq default-input-method  "tibetan-wylie"))

;;; This function makes a transcription string for
;;; re-composing a character.

;;;###autoload
(defun tibetan-tibetan-to-transcription (ch)
  "Return a transcription string of Tibetan character CH"
  (let ((char ch)
	(l (append tibetan-consonant-transcription-alist
		   tibetan-vowel-transcription-alist
		   tibetan-precomposed-transcription-alist
		   tibetan-subjoined-transcription-alist)) 
	decomp-l t-char trans str result)
    (if (eq (char-charset char) 'composition)
	(setq decomp-l (decompose-composite-char char 'list nil))
      (setq decomp-l (cons char nil)))
    (setq str "")
    (while decomp-l
      (setq t-char (char-to-string (car decomp-l)))
      (setq trans (car (rassoc t-char l)))
      (setq str (concat str trans))
      (setq decomp-l (cdr decomp-l)))
    (setq result str)))

;;; This function translates transcription string into a string of
;;; Tibetan characters.

;;;###autoload
(defun tibetan-transcription-to-tibetan (transcription)
  "Translate Roman transcription into a sequence of Tibetan components."
  (let ((trans transcription)
	(lp tibetan-precomposed-transcription-alist)
	(l (append tibetan-consonant-transcription-alist
		   tibetan-vowel-transcription-alist
		   tibetan-subjoined-transcription-alist))
	(case-fold-search nil)
	substr t-char p-str t-str result)
    (setq substr "")
    (setq p-str "")
    (setq t-str "")
    (cond ((string-match tibetan-precomposed-regexp trans)
	   (setq substr (substring trans (match-beginning 0) (match-end 0)))
	   (setq trans (substring trans (match-end 0)))
	   (setq t-char (cdr (assoc substr lp)))
	   (setq p-str t-char)))
    (while (string-match tibetan-regexp trans)
      (setq substr (substring trans (match-beginning 0) (match-end 0)))
      (setq trans (substring trans 0 (match-beginning 0)))
      (setq t-char
	    (cdr (assoc substr l)))
      (setq t-str (concat t-char t-str)))
    (setq result (concat p-str t-str))))


;;;
;;; Functions for composing Tibetan character.
;;;
;;; A Tibetan syllable is typically structured as follows:
;;;
;;;      [Prefix] C [C+] V [M] [Suffix [Post suffix]]
;;;
;;; where C's are all vertically stacked, V appears below or above
;;; consonant cluster and M is always put above the C[C+]V combination.
;;; (Sanskrit visarga, though it is a vowel modifier, is considered
;;;  to be a punctuation.)
;;;
;;; Here are examples of the words "bsgrubs" and "h'uM"
;;;
;;;            $(7"72%q`"U1"7"G(B         2$(7"H`#A`"U0"_1(B        
;;;
;;;                             M
;;;             b s b s         h
;;;               g             '
;;;               r             u
;;;               u
;;;
;;; Consonants ''', 'w', 'y', 'r' take special forms when they are used
;;; as subjoined consonant. Consonant 'r' takes another special form
;;; when used as superjoined as in "rka", and so on, while it does not
;;; change its form when conjoined with subjoined ''', 'w' or 'y'
;;; as in "rwa", "rya".
;;;
;;;
;;; As a Tibetan input method should avoid using conversion key,
;;; we use a "Tibetan glyph -> transcription -> Tibetan glyph"
;;; translation at each key input.
;;;
;;; 1st stage -  Check the preceding char.
;;;              If the preceding char is Tibetan and composable, then
;;;
;;; 2nd stage -  Translate the preceding char into transcription
;;;
;;; 3rd stage -  Concatenate the transcription of preceding char
;;;              and the current input key.
;;;
;;; 4th stage -  Re-translate the concatenated transcription into
;;;              a sequence of Tibetan letters.
;;;
;;; 5th stage -  Convert leading consonants into one single precomposed char
;;;              if possible.
;;;
;;; 6th stage -  Compose the consonants into one composite glyph.
;;;
;;; (If the current input is a vowel sign or a vowel modifier,
;;;  then it is composed with preceding char without checking
;;;  except when the preceding char is a punctuation or a digit.)
;;;
;;;

;;; This function is used to avoid composition
;;;      between Tibetan and non-Tibetan chars.

;;;###autoload
(defun tibetan-char-examin (ch)
  "Check if char CH is Tibetan character.
Returns non-nil if CH is Tibetan. Otherwise, returns nil."
  (let ((chr ch))
    (if (eq (char-charset chr) 'composition)
	(string-match "\\cq+" (decompose-composite-char chr))
      (string-match "\\cq" (char-to-string chr)))))

;;; This is used to avoid composition between digits, signs, punctuations
;;; and word constituents.

;;;###autoload
(defun tibetan-composable-examin (ch)
  "Check if Tibetan char CH is composable.
Returns t if CH is a composable char \(i.e. neither punctuation nor digit)."
  (let ((chr ch)
	chstr)
    (if (eq (char-charset chr) 'composition)
	(setq chstr (decompose-composite-char chr))
      (setq chstr (char-to-string chr)))
    (not (string-match "[$(7!1(B-$(7!o"f$(8!;!=!?!@!A!D"`(B]" chstr))))


;;; This checks if a character to be composed contains already
;;; one or more vowels / vowel modifiers. If the character contains
;;; them, then no more consonant should be added.

;;;###autoload
(defun tibetan-complete-char-examin (ch)
  "Check if composite char CH contains one or more vowel/vowel modifiers.
Returns non-nil, if CH contains vowel/vowel modifiers."
  (let ((chr ch)
	chstr)
    (if (eq (char-charset chr) 'composition)
	(setq chstr (decompose-composite-char chr))
      (setq chstr (char-to-string chr)))
    (string-match "[$(7!g!e"Q(B-$(7"^"_(B-$(7"l(B]" chstr)))

;;; This function makes a composite character consisting of two characters
;;; vertically stacked.

;;;###autoload
(defun tibetan-vertical-stacking (first second upward)
  "Return a vertically stacked composite char consisting of FIRST and SECOND.
If UPWARD is non-nil, then SECOND is put above FIRST."
  (if upward
      (compose-chars first '(tc . bc) second)
    (compose-chars first '(bc . tc) second)))

;;; This function makes a composite char from a string.
;;; Note that this function returns a string, not a char.

;;;###autoload
(defun tibetan-compose-string (str)
  "Compose a sequence of Tibetan character components into a composite character.
Returns a string containing a composite character."
  (let ((t-str str)
	f-str s-str f-ch s-ch rest composed result)
    ;;Make sure no redundant vowel sign is present.  
    (if (string-match
	 "^\\(.+\\)\\($(7"Q(B\\)\\([$(7!I!g!e"Q(B-$(7"^"_(B-$(7"l(B]\\)" t-str)
	(setq t-str (concat
		     (match-string 1 t-str)
		     (match-string 3 t-str))))
    (if (string-match
	 "^\\(.+\\)\\([$(7!I!g!e"Q(B-$(7"^"_(B-$(7"l(B]\\)\\($(7"Q(B\\)" t-str)
	(setq t-str (concat
		     (match-string 1 t-str)
		     (match-string 2 t-str))))
    ;;Start conversion.
    (setq result "")
    ;; Consecutive base/precomposed consonants are reduced to the last one.
    (while (string-match "^\\([$(7"!(B-$(7"J$!(B-$(7%u(B]\\)\\([$(7"!(B-$(7"@"B(B-$(7"J$!(B-$(7%u(B].*\\)" t-str)
      (setq result (concat result (match-string 1 t-str)))
      (setq t-str (match-string 2 t-str)))
    ;; Vowel/vowel modifier, subjoined consonants are added one by one
    ;; to the preceding element.
    (while
	(string-match "^\\(.\\)\\([$(7"A#!(B-$(7#J!I!g!e"Q(B-$(7"^"_(B-$(7"l(B]\\)\\(.*\\)" t-str)
      (setq f-str (match-string 1 t-str))
      (setq f-ch (string-to-char f-str))
      (setq s-str (match-string 2 t-str))
      ;;Special treatment for 'a chung.
      ;;If 'a follows a consonant, then turned into its subjoined form.
      (if (and (string-match "$(7"A(B" s-str)
	       (not (tibetan-complete-char-examin f-ch)))
	  (setq s-str "$(7#A(B"))
      (setq s-ch (string-to-char s-str))
      (setq rest (match-string 3 t-str))
      (cond ((string-match "\\c2" s-str);; upper vowel sign
	     (setq composed
		   (tibetan-vertical-stacking f-ch s-ch t)))
	    ((string-match "\\c3" s-str);; lower vowel sign
	     (setq composed
		   (tibetan-vertical-stacking f-ch s-ch nil)))
	    ;;Automatic conversion of ra-mgo (superscribed r).
	    ;;'r' is converted if followed by a subjoined consonant
	    ;;other than w, ', y, r.
	    ((and (string-match "$(7"C(B" f-str)
		  (not (string-match "[$(7#>#A#B#C(B]" s-str)))
	     (setq f-ch ?$(7#P(B)
	     (setq composed
		   (tibetan-vertical-stacking f-ch s-ch nil)))
	    ((not (tibetan-complete-char-examin f-ch))
	     ;;Initial base consonant is tranformed, if followed by
	     ;;a subjoined consonant, except when it is followed
	     ;;by a subscribed 'a.
	     (if (and (string-match "[$(7"!(B-$(7"="?"@"D(B-$(7"J(B]" f-str)
		      (not (string-match "$(7#A(B" s-str)))
		 (setq f-ch
		       (string-to-char
			(cdr (assoc f-str tibetan-base-to-subjoined-alist)))))
	     (setq composed
		   (tibetan-vertical-stacking f-ch s-ch nil)))
	    (t
	     (setq composed s-str)
	     (setq result (concat result f-str))))
      (setq t-str (concat composed rest)))
    (setq result (concat result t-str))))

;;; quail <-> conversion interface.

;;;###autoload
(defun tibetan-composition (pc key)
  "Interface to quail input method.
Takes two arguments: char PC and string KEY, where PC is the preceding
character to be composed with current input KEY.
Returns a string which is the result of composition."
  (let (trans cur-ch t-str result)
    ;; Make a tibetan character corresponding to current input key.
    (setq cur-ch (tibetan-transcription-to-tibetan key))
    ;; Check if the preceding character is Tibetan and composable.
    (cond ((and (tibetan-char-examin pc)
		(tibetan-composable-examin pc))
	   ;;If Tibetan char corresponding to the current input key exists,
	   (cond (cur-ch
		  ;; Then,
		  ;; Convert the preceding character into transcription,
		  ;; and concatenate it with the current input key,
		  (setq trans (tibetan-tibetan-to-transcription pc))
		  (setq trans (concat trans key))
		  ;; Concatenated transcription is converted to
		  ;; a sequence of Tibetan characters,
		  (setq t-str (tibetan-transcription-to-tibetan trans))
		  ;; And it is composed into a composite character.
		  (setq result (tibetan-compose-string t-str)))
		 ;; Else,
		 (t
		  ;; Simply concatenate the preceding character and
		  ;; the current input key.
		  (setq result (char-to-string pc))
		  (setq result (concat result key)))))
	  ;; If the preceding char is not Tibetan or not composable,
	  (t
	   ;; pc = 0 means the point is at the beginning of buffer.
	   (if (not (eq pc 0))
	       (setq result (char-to-string pc)))
	   (if cur-ch
	       (setq result (concat result cur-ch))
	     (setq result (concat result key))))
	  )))


;;;###autoload
(defun tibetan-decompose-region (beg end)
  "Decompose Tibetan characters in the region BEG END into their components.
Components are: base and subjoined consonants, vowel signs, vowel modifiers.
One column punctuations are converted to their 2 column equivalents."
  (interactive "r")
  (let (ch-str ch-beg ch-end)
    (save-excursion
      (save-restriction
	(narrow-to-region beg end)
	(goto-char (point-min))
	;; \\cq = Tibetan character
	(while (re-search-forward "\\cq" nil t)
	  (setq ch-str (buffer-substring-no-properties
			(match-beginning 0) (match-end 0)))
	  ;; Save the points. Maybe, using save-match-data is preferable.
	  ;; But in order not to lose the trace(because the body is too long),
	  ;; we save the points in variables.
	  (setq ch-beg (match-beginning 0))
	  (setq ch-end (match-end 0))
	  ;; Here starts the decomposition.
	  (cond
	   ;; 1 column punctuations -> 2 column equivalent
	   ((string-match "[$(8!D!;!=!?!@!A"`(B]" ch-str)
	    (setq ch-str
		  (car (rassoc ch-str tibetan-precomposition-rule-alist))))
	   ;; Decomposition of composite character.
	   ((eq (char-charset (string-to-char ch-str)) 'composition)
	    ;; Make a string which consists of a sequence of
	    ;; components.
	    (setq ch-str (decompose-composite-char (string-to-char ch-str)))
	    ;; Converts nyi zla into base elements.
	    (cond ((string= ch-str "$(7#R#S#S#S(B")
		   (setq ch-str "$(7!4!5!5(B"))
		  ((string= ch-str "$(7#R#S#S(B")
		   (setq ch-str "$(7!4!5(B"))
		  ((string= ch-str "$(7#R#S!I(B")
		   (setq ch-str "$(7!6(B"))
		  ((string= ch-str "$(7#R#S(B")
		   (setq ch-str "$(7!4(B")))))
	  ;; If the sequence of components starts with a subjoined consonants,
	  (if (string-match "^\\([$(7#!(B-$(7#J(B]\\)\\(.*\\)$" ch-str)
	      ;; then the first components is converted to its base form.
	      (setq ch-str
		    (concat (car (rassoc (match-string 1 ch-str)
					 tibetan-base-to-subjoined-alist))
			    (match-string 2 ch-str))))
	  ;; If the sequence of components starts with a precomposed character,
	  (if (string-match "^\\([$(7$!(B-$(7%u(B]\\)\\(.*\\)$" ch-str)
	      ;; then it is converted into a sequence of components.
	      (setq ch-str
		    (concat (car (rassoc (match-string 1 ch-str)
					 tibetan-precomposition-rule-alist))
			    (match-string 2 ch-str))))
	  ;; Special treatment for superscribed r.
	  (if (string-match "^$(7#P(B\\(.*\\)$" ch-str)
	      (setq ch-str (concat "$(7"C(B" (match-string 1 ch-str))))
	  ;; Finally, the result of decomposition is inserted, and
	  ;; the composite character is deleted.
	  (insert-and-inherit ch-str)
	  (delete-region ch-beg ch-end))))))

;;;###autoload
(defun tibetan-compose-region (beg end)
  "Make composite chars from Tibetan character components in the region BEG END.
Two column punctuations are converted to their 1 column equivalents."
  (interactive "r")
  (let (str result)
    (save-excursion
      (save-restriction
	(narrow-to-region beg end)
	(goto-char (point-min))
	;; First, sequence of components which has a precomposed equivalent
	;; is converted.
	(while (re-search-forward
		tibetan-precomposition-rule-regexp nil t)
	  (setq str (buffer-substring-no-properties
		     (match-beginning 0) (match-end 0)))
	  (save-match-data
	    (insert-and-inherit
	     (cdr (assoc str tibetan-precomposition-rule-alist))))
	  (delete-region (match-beginning 0) (match-end 0)))
	(goto-char (point-min))
	;; Then, composable elements are put into a composite character. 
	(while (re-search-forward
		"[$(7"!(B-$(7"J$!(B-$(7%u(B]+[$(7#!(B-$(7#J!I!g!e"Q(B-$(7"^"_(B-$(7"l(B]+"
		nil t)
	  (setq str (buffer-substring-no-properties
		     (match-beginning 0) (match-end 0)))
	  (save-match-data
	    (setq result (tibetan-compose-string str))
	    (insert-and-inherit result))
	  (delete-region (match-beginning 0) (match-end 0)))))))

;;;
;;; This variable is used to avoid repeated decomposition.
;;;
(setq-default tibetan-decomposed nil)

;;;###autoload
(defun tibetan-decompose-buffer ()
  "Decomposes Tibetan characters in the buffer into their components.
See also docstring of the function tibetan-decompose-region."
  (interactive)
  (make-local-variable 'tibetan-decomposed)
  (cond ((not tibetan-decomposed)
	 (tibetan-decompose-region (point-min) (point-max))
	 (setq tibetan-decomposed t))))

;;;###autoload
(defun tibetan-compose-buffer ()
  "Composes Tibetan character components in the buffer.
See also docstring of the function tibetan-compose-region."
  (interactive)
  (make-local-variable 'tibetan-decomposed)
  (tibetan-compose-region (point-min) (point-max))
  (setq tibetan-decomposed nil))

;;;###autoload
(defun tibetan-post-read-conversion (len)
  (save-excursion
    (save-restriction
      (let ((buffer-modified-p (buffer-modified-p)))
	(narrow-to-region (point) (+ (point) len))
	(tibetan-compose-region (point-min) (point-max))
	(set-buffer-modified-p buffer-modified-p)
	(point-max))))
  (make-local-variable 'tibetan-decomposed)
  (setq tibetan-decomposed nil))


;;;###autoload
(defun tibetan-pre-write-conversion (from to)
  (setq tibetan-decomposed-temp tibetan-decomposed)
  (let ((old-buf (current-buffer))
	(work-buf (get-buffer-create " *tibetan-work*")))
    (set-buffer work-buf)
    (erase-buffer)
    (if (stringp from)
	(insert from)
      (insert-buffer-substring old-buf from to))
    (if (not tibetan-decomposed-temp)
	(tibetan-decompose-region (point-min) (point-max)))
    ;; Should return nil as annotations.
    nil))

(provide 'tibet-util)

;;; language/tibet-util.el ends here.
