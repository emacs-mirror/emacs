;;; footnote.el --- footnote support for message mode  -*- lexical-binding:t -*-

;; Copyright (C) 1997, 2000-2018 Free Software Foundation, Inc.

;; Author: Steven L Baur <steve@xemacs.org> (1997-2011)
;;         Boruch Baum <boruch_baum@gmx.com> (2017-)
;; Keywords: mail, news
;; Version: 0.19

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

;; This file provides footnote[1] support for message-mode in emacsen.
;; footnote-mode is implemented as a minor mode.

;; [1] Footnotes look something like this.  Along with some decorative
;; stuff.

;;;; TODO:
;; + Reasonable Undo support.
;;   - could use an `apply' entry in the buffer-undo-list to be warned when
;;     a footnote we inserted is removed via undo.
;;   - should try to handle the more general problem of deleting/removing
;;     footnotes via standard editing commands rather than via footnote
;;     commands.
;; + more language styles.
;; + The key sequence 'C-c ! a C-y C-c ! b' should auto-fill the
;;   footnote in adaptive fill mode. This does not seem to be a bug in
;;   `adaptive-fill' because it behaves that way on all point movements
;; + Handle footmode mode elegantly in all modes, even if that means refuses to
;;   accept the burden. For example, in a programming language mode, footnotes
;;   should be commented.
;; + Manually autofilling the a first footnote should not cause it to
;;   wrap into the footnote section tag
;;   + Current solution adds a second newline after the section tag, so it is
;;     clearly a separate paragraph. There may be stylistic objections to this.
;; + Footnotes with multiple paragraphs should not have their first
;;   line out-dented.
;; + Upon leaving footnote area, perform an auto-fill on an entire
;;   footnote (including multiple paragraphs), or on entire footnote area.
;;   + fill-paragraph takes arg REGION, but seemingly only when called
;;     interactively.
;; + At some point, it became necessary to change `footnote-section-tag-regexp'
;;   to remove its trailing space. (Adaptive fill side-effect?)
;; + useful for lazy testing
;;   (setq footnote-narrow-to-footnotes-when-editing t)
;;   (setq footnote-section-tag "Footnotes: ")
;;   (setq footnote-section-tag-regexp "Footnotes\\(\\[.\\]\\)?:")

;;; Code:

(eval-when-compile (require 'cl-lib))
(defvar filladapt-token-table)

(defgroup footnote nil
  "Support for footnotes in mail and news messages."
  :version "21.1"
  :group 'message)

(defcustom footnote-mode-line-string " FN"
  "String to display in modes section of the mode-line."
  :type 'string
  :group 'footnote)

(defcustom footnote-mode-hook nil
  "Hook functions run when footnote-mode is activated."
  :type 'hook
  :group 'footnote)

(defcustom footnote-narrow-to-footnotes-when-editing nil
  "If non-nil, narrow to footnote text body while editing a footnote."
  :type 'boolean
  :group 'footnote)

(defcustom footnote-prompt-before-deletion t
  "If non-nil, prompt before deleting a footnote.
There is currently no way to undo deletions."
  :type 'boolean
  :group 'footnote)

(defcustom footnote-spaced-footnotes t
  "If non-nil, insert an empty line between footnotes.
Customizing this variable has no effect on buffers already
displaying footnotes."
  :type 'boolean
  :group 'footnote)

(defcustom footnote-use-message-mode t ; Nowhere used.
  "If non-nil, assume Footnoting will be done in `message-mode'."
  :type 'boolean
  :group 'footnote)

(defcustom footnote-body-tag-spacing 2
  "Number of spaces separating a footnote body tag and its text.
Customizing this variable has no effect on buffers already
displaying footnotes."
  :type 'integer
  :group 'footnote)

(defcustom footnote-prefix [(control ?c) ?!]
  "Prefix key to use for Footnote command in Footnote minor mode.
The value of this variable is checked as part of loading Footnote mode.
After that, changing the prefix key requires manipulating keymaps."
  :type 'key-sequence
  :group 'footnote)

;;; Interface variables that probably shouldn't be changed

(defcustom footnote-section-tag "Footnotes:"
  "Tag inserted at beginning of footnote section.
If you set this to the empty string, no tag is inserted and the
value of `footnote-section-tag-regexp' is ignored.  Customizing
this variable has no effect on buffers already displaying
footnotes."
  :version "27.1"
  :type 'string
  :group 'footnote)

(defcustom footnote-section-tag-regexp
  ;; Even if `footnote-section-tag' has a trailing space, let's not require it
  ;; here, since it might be trimmed by various commands.
  "Footnotes\\(\\[.\\]\\)?:"
  "Regexp which indicates the start of a footnote section.
This variable is disregarded when `footnote-section-tag' is the
empty string.  Customizing this variable has no effect on buffers
already displaying footnotes."
  :version "27.1"
  :type 'regexp
  :group 'footnote)

;; The following three should be consumed by footnote styles.
(defcustom footnote-start-tag "["
  "String used to denote start of numbered footnote.
Should not be set to the empty string.  Customizing this variable
has no effect on buffers already displaying footnotes."
  :type 'string
  :group 'footnote)

(defcustom footnote-end-tag "]"
  "String used to denote end of numbered footnote.
Should not be set to the empty string.  Customizing this variable
has no effect on buffers already displaying footnotes."
  :type 'string
  :group 'footnote)

(defcustom footnote-signature-separator
  (if (boundp 'message-signature-separator)
      message-signature-separator
    "^-- $")
  "Regexp used by Footnote mode to recognize signatures."
  :type 'regexp
  :group 'footnote)

(defcustom footnote-align-to-fn-text t
  "How to left-align footnote text.
If nil, footnote text is to be aligned flush left with left side
of the footnote number. If non-nil, footnote text is to be aligned
left with the first character of footnote text."
  :type  'boolean)

;;; Private variables

(defvar footnote-style-number nil
  "Footnote style represented as an index into footnote-style-alist.")
(make-variable-buffer-local 'footnote-style-number)

(defvar footnote-text-marker-alist nil
  "List of markers pointing to text of footnotes in message buffer.")
(make-variable-buffer-local 'footnote-text-marker-alist)

(defvar footnote-pointer-marker-alist nil
  "List of markers pointing to footnote pointers in message buffer.")
(make-variable-buffer-local 'footnote-pointer-marker-alist)

(defvar footnote-mouse-highlight 'highlight
  "Text property name to enable mouse over highlight.")

(defvar footnote-mode)

;;; Default styles
;;; NUMERIC
(defconst footnote-numeric-regexp "[0-9]+"
  "Regexp for digits.")

(defun footnote--numeric (n)
  "Numeric footnote style.
Use Arabic numerals for footnoting."
  (int-to-string n))

;;; ENGLISH UPPER
(defconst footnote-english-upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "Upper case English alphabet.")

(defconst footnote-english-upper-regexp "[A-Z]+"
  "Regexp for upper case English alphabet.")

(defun footnote--english-upper (n)
  "Upper case English footnoting.
Wrapping around the alphabet implies successive repetitions of letters."
  (let* ((ltr (mod (1- n) (length footnote-english-upper)))
	 (rep (/ (1- n) (length footnote-english-upper)))
	 (chr (char-to-string (aref footnote-english-upper ltr)))
	 rc)
    (while (>= rep 0)
      (setq rc (concat rc chr))
      (setq rep (1- rep)))
    rc))

;;; ENGLISH LOWER
(defconst footnote-english-lower "abcdefghijklmnopqrstuvwxyz"
  "Lower case English alphabet.")

(defconst footnote-english-lower-regexp "[a-z]+"
  "Regexp of lower case English alphabet.")

(defun footnote--english-lower (n)
  "Lower case English footnoting.
Wrapping around the alphabet implies successive repetitions of letters."
  (let* ((ltr (mod (1- n) (length footnote-english-lower)))
	 (rep (/ (1- n) (length footnote-english-lower)))
	 (chr (char-to-string (aref footnote-english-lower ltr)))
	 rc)
    (while (>= rep 0)
      (setq rc (concat rc chr))
      (setq rep (1- rep)))
    rc))

;;; ROMAN LOWER
(defconst footnote-roman-lower-list
  '((1 . "i") (5 . "v") (10 . "x")
    (50 . "l") (100 . "c") (500 . "d") (1000 . "m"))
  "List of roman numerals with their values.")

(defconst footnote-roman-lower-regexp
  (concat "[" (mapconcat #'cdr footnote-roman-lower-list "") "]+")
  "Regexp of roman numerals.")

(defun footnote--roman-lower (n)
  "Generic Roman number footnoting."
  (footnote--roman-common n footnote-roman-lower-list))

;;; ROMAN UPPER
(defconst footnote-roman-upper-list
  (mapcar (lambda (x) (cons (car x) (upcase (cdr x))))
          footnote-roman-lower-list)
  "List of roman numerals with their values.")

(defconst footnote-roman-upper-regexp (upcase footnote-roman-lower-regexp)
  "Regexp of roman numerals.  Not complete")

(defun footnote--roman-upper (n)
  "Generic Roman number footnoting."
  (footnote--roman-common n footnote-roman-upper-list))

(defun footnote--roman-common (n footnote-roman-list)
  "Lower case Roman footnoting."
  (let* ((our-list footnote-roman-list)
	 (rom-lngth (length our-list))
	 (rom-high 0)
	 (rom-low 0)
	 (rom-div -1)
	 (count-high 0)
	 (count-low 0))
    ;; find surrounding numbers
    (while (and (<= count-high (1- rom-lngth))
		(>= n (car (nth count-high our-list))))
      ;; (message "Checking %d" (car (nth count-high our-list)))
      (setq count-high (1+ count-high)))
    (setq rom-high count-high)
    (setq rom-low (1- count-high))
    ;; find the appropriate divisor (if it exists)
    (while (and (= rom-div -1)
		(< count-low rom-high))
      (when (or (> n (- (car (nth rom-high our-list))
			(/ (car (nth count-low our-list))
			   2)))
		(= n (- (car (nth rom-high our-list))
			(car (nth count-low our-list)))))
	(setq rom-div count-low))
      ;; (message "Checking %d and %d in div loop" rom-high count-low)
      (setq count-low (1+ count-low)))
    ;;(message "We now have high: %d, low: %d, div: %d, n: %d"
    ;;	       rom-high rom-low (if rom-div rom-div -1) n)
    (let ((rom-low-pair (nth rom-low our-list))
	  (rom-high-pair (nth rom-high our-list))
	  (rom-div-pair (if (not (= rom-div -1)) (nth rom-div our-list) nil)))
      ;; (message "pairs are: rom-low: %S, rom-high: %S, rom-div: %S"
      ;;	  rom-low-pair rom-high-pair rom-div-pair)
      (cond
       ((< n 0) (error "footnote--roman-common called with n < 0"))
       ((= n 0) "")
       ((= n (car rom-low-pair)) (cdr rom-low-pair))
       ((= n (car rom-high-pair)) (cdr rom-high-pair))
       ((= (car rom-low-pair) (car rom-high-pair))
	(concat (cdr rom-low-pair)
		(footnote--roman-common
		 (- n (car rom-low-pair))
		 footnote-roman-list)))
       ((>= rom-div 0) (concat (cdr rom-div-pair) (cdr rom-high-pair)
			       (footnote--roman-common
				(- n (- (car rom-high-pair)
					(car rom-div-pair)))
				footnote-roman-list)))
       (t (concat (cdr rom-low-pair)
		  (footnote--roman-common
		   (- n (car rom-low-pair))
		   footnote-roman-list)))))))

;; Latin-1

(defconst footnote-latin-string "¹²³ºª§¶"
  "String of Latin-1 footnoting characters.")

;; Note not [...]+, because this style cycles.
(defconst footnote-latin-regexp (concat "[" footnote-latin-string "]")
  "Regexp for Latin-1 footnoting characters.")

(defun footnote--latin (n)
  "Latin-1 footnote style.
Use a range of Latin-1 non-ASCII characters for footnoting."
  (string (aref footnote-latin-string
		(mod (1- n) (length footnote-latin-string)))))

;; Unicode

(defconst footnote-unicode-string "⁰¹²³⁴⁵⁶⁷⁸⁹"
  "String of Unicode footnoting characters.")

(defconst footnote-unicode-regexp (concat "[" footnote-unicode-string "]+")
  "Regexp for Unicode footnoting characters.")

(defun footnote--unicode (n)
  "Unicode footnote style.
Use Unicode characters for footnoting."
  (let (modulus result done)
    (while (not done)
      (setq modulus (mod n 10)
            n (truncate n 10))
      (and (zerop n) (setq done t))
      (push (aref footnote-unicode-string modulus) result))
    (apply #'string result)))

;; Hebrew

(defconst footnote-hebrew-numeric
  '(
    ("א" "ב" "ג" "ד" "ה" "ו" "ז" "ח" "ט")
    ("י" "כ" "ל" "מ" "נ" "ס" "ע" "פ" "צ")
    ("ק" "ר" "ש" "ת" "תק" "תר" "תש" "תת" "תתק")))

(defconst footnote-hebrew-numeric-regex
  (concat "[" (apply #'concat (apply #'append footnote-hebrew-numeric)) "']+"))
;; (defconst footnote-hebrew-numeric-regex "\\([אבגדהוזחט]'\\)?\\(ת\\)?\\(ת\\)?\\([קרשת]\\)?\\([טיכלמנסעפצ]\\)?\\([אבגדהוזחט]\\)?")

(defun footnote--hebrew-numeric (n)
  "Supports 9999 footnotes, then rolls over."
  (let* ((n (+ (mod n 10000) (/ n 10000)))
         (thousands (/ n 1000))
         (hundreds (/ (mod n 1000) 100))
         (tens (/ (mod n 100) 10))
         (units (mod n 10))
         (special (cond
                   ((not (= tens 1)) nil)
                   ((= units 5) "טו")
                   ((= units 6) "טז"))))
    (concat
     (when (/= 0 thousands)
       (concat (nth (1- thousands) (nth 0 footnote-hebrew-numeric)) "'"))
     (when (/= 0 hundreds)
       (nth (1- hundreds) (nth 2 footnote-hebrew-numeric)))
     (or special
         (concat
          (when (/= 0 tens) (nth (1- tens) (nth 1 footnote-hebrew-numeric)))
          (when (/= 0 units) (nth (1- units) (nth 0 footnote-hebrew-numeric))))))))

(defconst footnote-hebrew-symbolic
  '(
    "א" "ב" "ג" "ד" "ה" "ו" "ז" "ח" "ט" "י" "כ" "ל" "מ" "נ" "ס" "ע" "פ" "צ" "ק" "ר" "ש" "ת"))

(defconst footnote-hebrew-symbolic-regex
  (concat "[" (apply #'concat footnote-hebrew-symbolic) "]"))

(defun footnote--hebrew-symbolic (n)
  "Only 22 elements, per the style of eg. 'פירוש שפתי חכמים על רש״י'.
Proceeds from `י' to `כ', from `צ' to `ק'. After `ת', rolls over to `א'."
  (nth (mod (1- n) 22) footnote-hebrew-symbolic))

;;; list of all footnote styles
(defvar footnote-style-alist
  `((numeric footnote--numeric ,footnote-numeric-regexp)
    (english-lower footnote--english-lower ,footnote-english-lower-regexp)
    (english-upper footnote--english-upper ,footnote-english-upper-regexp)
    (roman-lower footnote--roman-lower ,footnote-roman-lower-regexp)
    (roman-upper footnote--roman-upper ,footnote-roman-upper-regexp)
    (latin footnote--latin ,footnote-latin-regexp)
    (unicode footnote--unicode ,footnote-unicode-regexp)
    (hebrew-numeric footnote--hebrew-numeric ,footnote-hebrew-numeric-regex)
    (hebrew-symbolic footnote--hebrew-symbolic ,footnote-hebrew-symbolic-regex))
  "Styles of footnote tags available.
By default, Arabic numbers, English letters, Roman Numerals,
Latin and Unicode superscript characters, and Hebrew numerals
are available.
Each element of the list should be of the form (NAME FUNCTION REGEXP)
where NAME is a symbol, FUNCTION takes a footnote number and
returns the corresponding representation in that style as a string,
and REGEXP should be a regexp that matches any output of FUNCTION.")

(defcustom footnote-style 'numeric
  "Default style used for footnoting.
numeric == 1, 2, 3, ...
english-lower == a, b, c, ...
english-upper == A, B, C, ...
roman-lower == i, ii, iii, iv, v, ...
roman-upper == I, II, III, IV, V, ...
latin == ¹ ² ³ º ª § ¶
unicode == ¹, ², ³, ...
hebrew-numeric == א, ב, ..., יא, ..., תקא...
hebrew-symbolic == א, ב, ..., י, כ, ..., צ, ק, ..., ת, א
See also variables `footnote-start-tag' and `footnote-end-tag'.

Note: some characters in the unicode style may not show up
properly if the default font does not contain those characters.

Customizing this variable has no effect on buffers already
displaying footnotes.  To change the style of footnotes in such a
buffer use the command `footnote-set-style'."
  :type (cons 'choice (mapcar (lambda (x) (list 'const (car x)))
			      footnote-style-alist))
  :group 'footnote)

;;; Style utilities & functions
(defun footnote--style-p (style)
  "Return non-nil if style is a valid style known to `footnote-mode'."
  (assq style footnote-style-alist))

(defun footnote--index-to-string (index)
  "Convert a binary index into a string to display as a footnote.
Conversion is done based upon the current selected style."
  (let ((alist (if (footnote--style-p footnote-style)
		   (assq footnote-style footnote-style-alist)
		 (nth 0 footnote-style-alist))))
    (funcall (nth 1 alist) index)))

(defun footnote--current-regexp ()
  "Return the regexp of the index of the current style."
  (concat (nth 2 (or (assq footnote-style footnote-style-alist)
		     (nth 0 footnote-style-alist)))
	  "*"))

(defun footnote--refresh-footnotes (&optional index-regexp)
  "Redraw all footnotes.
You must call this or arrange to have this called after changing footnote
styles."
  (unless index-regexp
    (setq index-regexp (footnote--current-regexp)))
  (save-excursion
    ;; Take care of the pointers first
    (let ((i 0) locn alist)
      (while (setq alist (nth i footnote-pointer-marker-alist))
	(setq locn (cdr alist))
	(while locn
	  (goto-char (car locn))
	  ;; Try to handle the case where `footnote-start-tag' and
	  ;; `footnote-end-tag' are the same string.
	  (when (looking-back (concat
			       (regexp-quote footnote-start-tag)
			       "\\(" index-regexp "+\\)"
			       (regexp-quote footnote-end-tag))
			      (line-beginning-position))
	    (replace-match
	     (propertize
	      (concat
	       footnote-start-tag
	       (footnote--index-to-string (1+ i))
	       footnote-end-tag)
	      'footnote-number (1+ i) footnote-mouse-highlight t)
	     nil "\\1"))
	  (setq locn (cdr locn)))
	(setq i (1+ i))))

    ;; Now take care of the text section
    (let ((i 0) alist)
      (while (setq alist (nth i footnote-text-marker-alist))
	(goto-char (cdr alist))
	(when (looking-at (concat
			   (regexp-quote footnote-start-tag)
			   "\\(" index-regexp "+\\)"
			   (regexp-quote footnote-end-tag)))
	  (replace-match
	   (propertize
	    (concat
	     footnote-start-tag
	     (footnote--index-to-string (1+ i))
	     footnote-end-tag)
	    'footnote-number (1+ i))
	   nil "\\1"))
	(setq i (1+ i))))))

(defun footnote--assoc-index (key alist)
  "Give index of key in alist."
  (let ((i 0) (max (length alist)) rc)
    (while (and (null rc)
		(< i max))
      (when (eq key (car (nth i alist)))
	(setq rc i))
      (setq i (1+ i)))
    rc))

(defun footnote-cycle-style ()
  "Select next defined footnote style."
  (interactive)
  (let ((old (footnote--assoc-index footnote-style footnote-style-alist))
	(max (length footnote-style-alist))
	idx)
    (setq idx (1+ old))
    (when (>= idx max)
      (setq idx 0))
    (setq footnote-style (car (nth idx footnote-style-alist)))
    (footnote--refresh-footnotes (nth 2 (nth old footnote-style-alist)))))

(defun footnote-set-style (&optional style)
  "Select a specific style."
  (interactive
   (list (intern (completing-read
		  "Footnote Style: "
		  obarray #'footnote--style-p 'require-match))))
  (let ((old (footnote--assoc-index footnote-style footnote-style-alist)))
    (setq footnote-style style)
    (footnote--refresh-footnotes (nth 2 (nth old footnote-style-alist)))))

;; Internal functions
(defun footnote--insert-numbered-footnote (arg &optional mousable)
  "Insert numbered footnote at (point)."
  (let ((string (concat footnote-start-tag
			(footnote--index-to-string arg)
			footnote-end-tag)))
    (insert-before-markers
     (if mousable
	 (propertize
	  string 'footnote-number arg footnote-mouse-highlight t)
       (propertize string 'footnote-number arg)))))

(defun footnote--renumber (_from to pointer-alist text-alist)
  "Renumber a single footnote."
  (let* ((posn-list (cdr pointer-alist)))
    (setcar pointer-alist to)
    (setcar text-alist to)
    (while posn-list
      (goto-char (car posn-list))
      (when (looking-back (concat (regexp-quote footnote-start-tag)
				  (footnote--current-regexp)
				  (regexp-quote footnote-end-tag))
			  (line-beginning-position))
	(replace-match
	 (propertize
	  (concat footnote-start-tag
		  (footnote--index-to-string to)
		  footnote-end-tag)
	  'footnote-number to footnote-mouse-highlight t)))
      (setq posn-list (cdr posn-list)))
    (goto-char (cdr text-alist))
    (when (looking-at (concat (regexp-quote footnote-start-tag)
			      (footnote--current-regexp)
			      (regexp-quote footnote-end-tag)))
      (replace-match
       (propertize
	(concat footnote-start-tag
		(footnote--index-to-string to)
		footnote-end-tag)
	'footnote-number to)))))

(defun footnote--narrow-to-footnotes ()
  "Restrict text in buffer to show only text of footnotes."
  (interactive) ; testing
  (narrow-to-region (footnote--get-area-point-min)
                    (footnote--get-area-point-max)))

(defun footnote--goto-char-point-max ()
  "Move to end of buffer or prior to start of .signature."
  (goto-char (point-max))
  (or (re-search-backward footnote-signature-separator nil t)
      (point)))

(defun footnote--insert-text-marker (arg locn)
  "Insert a marker pointing to footnote ARG, at buffer location LOCN."
  (let ((marker (make-marker)))
    (unless (assq arg footnote-text-marker-alist)
      (set-marker marker locn)
      (setq footnote-text-marker-alist
	    (cons (cons arg marker) footnote-text-marker-alist))
      (setq footnote-text-marker-alist
	    (footnote--sort footnote-text-marker-alist)))))

(defun footnote--insert-pointer-marker (arg locn)
  "Insert a marker pointing to footnote ARG, at buffer location LOCN."
  (let ((marker (make-marker))
	alist)
    (set-marker marker locn)
    (if (setq alist (assq arg footnote-pointer-marker-alist))
	(setf alist
	      (cons marker (cdr alist)))
      (setq footnote-pointer-marker-alist
	    (cons (cons arg (list marker)) footnote-pointer-marker-alist))
      (setq footnote-pointer-marker-alist
	    (footnote--sort footnote-pointer-marker-alist)))))

(defun footnote--insert-footnote (arg)
  "Insert a footnote numbered ARG, at (point)."
  (push-mark)
  (footnote--insert-pointer-marker arg (point))
  (footnote--insert-numbered-footnote arg t)
  (footnote--goto-char-point-max)
  (if (cond
       ((not (string-equal footnote-section-tag ""))
	(re-search-backward (concat "^" footnote-section-tag-regexp) nil t))
       (footnote-text-marker-alist
	(goto-char (cdar footnote-text-marker-alist))))
      (save-restriction
	(when footnote-narrow-to-footnotes-when-editing
	  (footnote--narrow-to-footnotes))
	(footnote-goto-footnote (1- arg)) ; evil, FIXME (less evil now)
	;; (message "Inserting footnote %d" arg)
	(unless
	    (or (eq arg 1)
		(when (re-search-forward
		       (if footnote-spaced-footnotes
			   "\n\n"
			 (concat "\n"
				 (regexp-quote footnote-start-tag)
				 (footnote--current-regexp)
				 (regexp-quote footnote-end-tag)))
		       nil t)
		  (unless (beginning-of-line) t))
		(footnote--goto-char-point-max)
		(cond
		 ((not (string-equal footnote-section-tag ""))
		  (re-search-backward
		   (concat "^" footnote-section-tag-regexp) nil t))
		 (footnote-text-marker-alist
		  (goto-char (cdar footnote-text-marker-alist)))))))
    (unless (looking-at "^$")
      (insert "\n"))
    (when (eobp)
      (insert "\n"))
    (unless (string-equal footnote-section-tag "")
      (insert footnote-section-tag "\n")))
  (let ((old-point (point)))
    (footnote--insert-numbered-footnote arg nil)
    (footnote--insert-text-marker arg old-point)))

(defun footnote--sort (list)
  (sort list (lambda (e1 e2)
	       (< (car e1) (car e2)))))

(defun footnote--text-under-cursor ()
  "Return the number of the current footnote if in footnote text.
Return nil if the cursor is not positioned over the text of
a footnote."
  (when (and footnote-text-marker-alist
             (<= (footnote--get-area-point-min)
                 (point)
                 (footnote--get-area-point-max)))
    (let ((i 1) alist-txt result)
      (while (and (setq alist-txt (nth i footnote-text-marker-alist))
                  (null result))
        (when (< (point) (cdr alist-txt))
          (setq result (car (nth (1- i) footnote-text-marker-alist))))
        (setq i (1+ i)))
      (when (and (null result) (null alist-txt))
        (setq result (car (nth (1- i) footnote-text-marker-alist))))
      result)))

(defun footnote--under-cursor ()
  "Return the number of the footnote underneath the cursor.
Return nil if the cursor is not over a footnote."
  (or (get-text-property (point) 'footnote-number)
      (footnote--text-under-cursor)))

(defun footnote--calc-fn-alignment-column ()
  "Calculate the left alignment for footnote text."
  ;; FIXME: Maybe it would be better to go to the footnote's beginning and
  ;; see at which column it starts.
  (+ footnote-body-tag-spacing
     (string-width
      (concat footnote-start-tag  footnote-end-tag
              (footnote--index-to-string
               (caar (last footnote-text-marker-alist)))))))

(defun footnote--fill-prefix-string ()
  "Return the fill prefix to be used by footnote mode."
  ;; TODO: Prefix to this value other prefix strings, such as those
  ;; designating a comment line, a message response, or a boxquote.
  (make-string (footnote--calc-fn-alignment-column) ?\s))

(defun footnote--point-in-body-p ()
  "Return non-nil if point is in the buffer text area,
i.e. before the beginning of the footnote area."
  (< (point) (footnote--get-area-point-min)))

(defun footnote--get-area-point-min (&optional before-tag)
  "Return start of the first footnote.
If there is no footnote area, returns `point-max'.
With optional arg BEFORE-TAG, return position of the `footnote-section-tag'
instead, if applicable."
  (cond
   ;; FIXME: Shouldn't we use `footnote--get-area-point-max' instead?
   ((not footnote-text-marker-alist) (point-max))
   ((not before-tag) (cdr (car footnote-text-marker-alist)))
   ((string-equal footnote-section-tag "")
    (cdr (car footnote-text-marker-alist)))
   (t
    (save-excursion
      (goto-char (cdr (car footnote-text-marker-alist)))
      (if (re-search-backward (concat "^" footnote-section-tag-regexp) nil t)
          (match-beginning 0)
        (message "Footnote section tag not found!")
        ;; This `else' should never happen, and indicates an error,
        ;; ie. footnotes already exist and a footnote-section-tag is defined,
        ;; but the section tag hasn't been found. We choose to assume that the
        ;; user deleted it intentionally and wants us to behave in this buffer
        ;; as if the section tag was set "", so we do that, now.
        ;;(setq footnote-section-tag "")
        ;;
        ;; HOWEVER: The rest of footnote mode does not currently honor or
        ;; account for this.
        ;;
        ;; To illustrate the difference in behavior, create a few footnotes,
        ;; delete the section tag, and create another footnote. Then undo,
        ;; comment the above line (that sets the tag to ""), re-evaluate this
        ;; function, and repeat.
        ;;
        ;; TODO: integrate sanity checks at reasonable operational points.
        (cdr (car footnote-text-marker-alist)))))))

(defun footnote--get-area-point-max ()
  "Return the end of footnote area.
This is either `point-max' or the start of a `.signature' string, as
defined by variable `footnote-signature-separator'. If there is no
footnote area, returns `point-max'."
  (save-excursion (footnote--goto-char-point-max)))

(defun footnote--adaptive-fill-function (orig-fun)
  (or
   (and
    footnote-mode
    footnote-align-to-fn-text
    (footnote--text-under-cursor)
    ;; (not (footnote--point-in-body-p))
    ;; (< (point) (footnote--signature-area-start-point))
    (footnote--fill-prefix-string))
   ;; If not within a footnote's text, fallback to the default.
   (funcall orig-fun)))

;;; User functions

(defun footnote--make-hole ()
  (save-excursion
    (let ((i 0)
	  (notes (length footnote-pointer-marker-alist))
	  alist-ptr alist-txt rc)
      (while (< i notes)
	(setq alist-ptr (nth i footnote-pointer-marker-alist))
	(setq alist-txt (nth i footnote-text-marker-alist))
	(when (< (point) (- (cadr alist-ptr) 3))
	  (unless rc
	    (setq rc (car alist-ptr)))
	  (save-excursion
	    (message "Renumbering from %s to %s"
		     (footnote--index-to-string (car alist-ptr))
		     (footnote--index-to-string
		      (1+ (car alist-ptr))))
	    (footnote--renumber (car alist-ptr)
			       (1+ (car alist-ptr))
			       alist-ptr
			       alist-txt)))
	(setq i (1+ i)))
      rc)))

(defun footnote-add-footnote ()
  "Add a numbered footnote.
The number the footnote receives is dependent upon the relative location
of any other previously existing footnotes.
If the variable `footnote-narrow-to-footnotes-when-editing' is set,
the buffer is narrowed to the footnote body.  The restriction is removed
by using `footnote-back-to-message'."
  (interactive "*")
  (let ((num
         (if footnote-text-marker-alist
             (if (< (point) (cl-cadar (last footnote-pointer-marker-alist)))
                 (footnote--make-hole)
               (1+ (caar (last footnote-text-marker-alist))))
           1)))
    (message "Adding footnote %d" num)
    (footnote--insert-footnote num)
    (insert-before-markers (make-string footnote-body-tag-spacing ? ))
    (let ((opoint (point)))
      (save-excursion
	(insert-before-markers
	 (if footnote-spaced-footnotes
	     "\n\n"
	   "\n"))
	(when footnote-narrow-to-footnotes-when-editing
	  (footnote--narrow-to-footnotes)))
      ;; Emacs/XEmacs bug?  save-excursion doesn't restore point when using
      ;; insert-before-markers.
      (goto-char opoint))))

(defun footnote-delete-footnote (&optional arg)
  "Delete a numbered footnote.
With no parameter, delete the footnote under (point).  With ARG specified,
delete the footnote with that number."
  (interactive "*P")
  (unless arg
    (setq arg (footnote--under-cursor)))
  (when (and arg
	     (or (not footnote-prompt-before-deletion)
		 (y-or-n-p (format "Really delete footnote %d?" arg))))
    (let (alist-ptr alist-txt locn)
      (setq alist-ptr (assq arg footnote-pointer-marker-alist))
      (setq alist-txt (assq arg footnote-text-marker-alist))
      (unless (and alist-ptr alist-txt)
	(error "Can't delete footnote %d" arg))
      (setq locn (cdr alist-ptr))
      (while (car locn)
	(save-excursion
	  (goto-char (car locn))
	  (when (looking-back (concat (regexp-quote footnote-start-tag)
				      (footnote--current-regexp)
				      (regexp-quote footnote-end-tag))
			      (line-beginning-position))
	    (delete-region (match-beginning 0) (match-end 0))))
	(setq locn (cdr locn)))
      (save-excursion
	(goto-char (cdr alist-txt))
	(delete-region
	 (point)
	 (if footnote-spaced-footnotes
	     (search-forward "\n\n" nil t)
           (save-restriction ; <= 2017-12 Boruch: WHY?? I see no narrowing / widening here.
	     (end-of-line)
	     (next-single-char-property-change
	      (point) 'footnote-number nil (footnote--goto-char-point-max))))))
      (setq footnote-pointer-marker-alist
	    (delq alist-ptr footnote-pointer-marker-alist))
      (setq footnote-text-marker-alist
	    (delq alist-txt footnote-text-marker-alist))
      (footnote-renumber-footnotes)
      (when (and (null footnote-text-marker-alist)
		 (null footnote-pointer-marker-alist))
	(save-excursion
	  (if (not (string-equal footnote-section-tag ""))
	      (let* ((end (footnote--goto-char-point-max))
		     (start (1- (re-search-backward
				 (concat "^" footnote-section-tag-regexp)
				 nil t))))
		(forward-line -1)
		(when (looking-at "\n")
		  (kill-line))
		(delete-region start (if (< end (point-max))
					 end
				       (point-max))))
	    (footnote--goto-char-point-max)
	    (when (looking-back "\n\n" (- (point) 2))
	      (kill-line -1))))))))

(defun footnote-renumber-footnotes ()
  "Renumber footnotes, starting from 1."
  (interactive "*")
  (save-excursion
    (let ((i 0)
	  (notes (length footnote-pointer-marker-alist))
	  alist-ptr alist-txt)
      (while (< i notes)
	(setq alist-ptr (nth i footnote-pointer-marker-alist))
	(setq alist-txt (nth i footnote-text-marker-alist))
	(unless (= (1+ i) (car alist-ptr))
	  (footnote--renumber (car alist-ptr) (1+ i) alist-ptr alist-txt))
	(setq i (1+ i))))))

(defun footnote-goto-footnote (&optional arg)
  "Jump to the text of a footnote.
With no parameter, jump to the text of the footnote under (point).  With ARG
specified, jump to the text of that footnote."
  (interactive "P")
  (unless arg
    (setq arg (footnote--under-cursor)))
  (let ((footnote (assq arg footnote-text-marker-alist)))
    (cond
     (footnote
      (goto-char (cdr footnote)))
     ((eq arg 0)
      (goto-char (point-max))
      (cond
       ((not (string-equal footnote-section-tag ""))
	(re-search-backward (concat "^" footnote-section-tag-regexp))
	(forward-line 1))
       (footnote-text-marker-alist
	(goto-char (cdar footnote-text-marker-alist)))))
     (t
      (error "I don't see a footnote here")))))

(defun footnote-back-to-message ()
  "Move cursor back to footnote referent.
If the cursor is not over the text of a footnote, point is not changed.
If the buffer was narrowed due to `footnote-narrow-to-footnotes-when-editing'
being set it is automatically widened."
  (interactive)
  (let ((note (footnote--text-under-cursor)))
    (when note
      (when footnote-narrow-to-footnotes-when-editing
	(widen))
      (goto-char (cadr (assq note footnote-pointer-marker-alist))))))

(defvar footnote-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" 'footnote-add-footnote)
    (define-key map "b" 'footnote-back-to-message)
    (define-key map "c" 'footnote-cycle-style)
    (define-key map "d" 'footnote-delete-footnote)
    (define-key map "g" 'footnote-goto-footnote)
    (define-key map "r" 'footnote-renumber-footnotes)
    (define-key map "s" 'footnote-set-style)
    map))

(defvar footnote-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map footnote-prefix footnote-mode-map)
    map)
  "Keymap used for binding footnote minor mode.")

;;;###autoload
(define-minor-mode footnote-mode
  "Toggle Footnote mode.
With a prefix argument ARG, enable Footnote mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

Footnote mode is a buffer-local minor mode.  If enabled, it
provides footnote support for `message-mode'.  To get started,
play around with the following keys:
\\{footnote-minor-mode-map}"
  :lighter    footnote-mode-line-string
  :keymap     footnote-minor-mode-map
  ;; (filladapt-mode t)
  (unless adaptive-fill-function
    ;; nil and `ignore' have the same semantics for adaptive-fill-function,
    ;; but only `ignore' behaves correctly with add/remove-function.
    (setq adaptive-fill-function #'ignore))
  (remove-function (local 'adaptive-fill-function)
                   #'footnote--adaptive-fill-function)
  (when footnote-mode
    ;; (footnote-setup-keybindings)
    (make-local-variable 'footnote-style)
    (make-local-variable 'footnote-body-tag-spacing)
    (make-local-variable 'footnote-spaced-footnotes)
    (make-local-variable 'footnote-section-tag)
    (make-local-variable 'footnote-section-tag-regexp)
    (make-local-variable 'footnote-start-tag)
    (make-local-variable 'footnote-end-tag)
    (make-local-variable 'adaptive-fill-function)
    (add-function :around (local 'adaptive-fill-function)
                  #'footnote--adaptive-fill-function)

    ;; filladapt is an XEmacs package which AFAIK has never been ported
    ;; to Emacs.
    (when (boundp 'filladapt-token-table)
      ;; add tokens to filladapt to match footnotes
      ;; 1] xxxxxxxxxxx x x x or [1] x x x x x x x
      ;;    xxx x xx xxx xxxx	     x x x xxxxxxxxxx
      (let ((bullet-regexp (concat (regexp-quote footnote-start-tag)
				   "?[0-9a-zA-Z]+"
				   (regexp-quote footnote-end-tag)
				   "[ \t]")))
	(unless (assoc bullet-regexp filladapt-token-table)
	  (setq filladapt-token-table
		(append filladapt-token-table
			(list (list bullet-regexp 'bullet)))))))))

(provide 'footnote)

;;; footnote.el ends here
