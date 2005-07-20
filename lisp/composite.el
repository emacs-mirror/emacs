;;; composite.el --- support character composition

;; Copyright (C) 1999 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.

;; Keywords: mule, multilingual, character composition

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

;;; Commentary:

;;; Code:

;;;###autoload
(defconst reference-point-alist
  '((tl . 0) (tc . 1) (tr . 2)
    (Bl . 3) (Bc . 4) (Br . 5)
    (bl . 6) (bc . 7) (br . 8)
    (cl . 9) (cc . 10) (cr . 11)
    (top-left . 0) (top-center . 1) (top-right . 2)
    (base-left . 3) (base-center . 4) (base-right . 5)
    (bottom-left . 6) (bottom-center . 7) (bottom-right . 8)
    (center-left . 9) (center-center . 10) (center-right . 11)
    ;; For backward compatibility...
    (ml . 3) (mc . 10) (mr . 5)
    (mid-left . 3) (mid-center . 10) (mid-right . 5))
  "Alist of symbols vs integer codes of glyph reference points.
A glyph reference point symbol is to be used to specify a composition
rule in COMPONENTS argument to such functions as `compose-region' and
`make-composition'.

Meanings of glyph reference point codes are as follows:

    0----1----2 <---- ascent	0:tl or top-left
    |         |			1:tc or top-center
    |         |			2:tr or top-right
    |         |			3:Bl or base-left     9:cl or center-left
    9   10   11 <---- center	4:Bc or base-center  10:cc or center-center
    |         |			5:Br or base-right   11:cr or center-right
  --3----4----5-- <-- baseline	6:bl or bottom-left
    |         |			7:bc or bottom-center
    6----7----8 <---- descent	8:br or bottom-right

Glyph reference point symbols are to be used to specify composition
rule of the form \(GLOBAL-REF-POINT . NEW-REF-POINT), where
GLOBAL-REF-POINT is a reference point in the overall glyphs already
composed, and NEW-REF-POINT is a reference point in the new glyph to
be added.

For instance, if GLOBAL-REF-POINT is `br' (bottom-right) and
NEW-REF-POINT is `tc' (top-center), the overall glyph is updated as
follows (the point `*' corresponds to both reference points):

    +-------+--+ <--- new ascent
    |       |  |
    | global|  |
    | glyph |  |
 -- |       |  |-- <--- baseline \(doesn't change)
    +----+--*--+
    |    | new |
    |    |glyph|
    +----+-----+ <--- new descent
")


;;;###autoload
(defun encode-composition-rule (rule)
  "Encode composition rule RULE into an integer value.
RULE is a cons of global and new reference point symbols
\(see `reference-point-alist')."

  ;; This must be compatible with C macro COMPOSITION_ENCODE_RULE
  ;; defined in composite.h.

  (if (and (integerp rule) (< rule 144))
      ;; Already encoded.
      rule
    (or (consp rule)
	(error "Invalid composition rule: %S" rule))
    (let ((gref (car rule))
	  (nref (cdr rule)))
      (or (integerp gref)
	  (setq gref (cdr (assq gref reference-point-alist))))
      (or (integerp nref)
	  (setq nref (cdr (assq nref reference-point-alist))))
      (or (and (>= gref 0) (< gref 12) (>= nref 0) (< nref 12))
	  (error "Invalid composition rule: %S" rule))
      (+ (* gref 12) nref))))

;; Decode encoded composition rule RULE-CODE.  The value is a cons of
;; global and new reference point symbols.
;; This must be compatible with C macro COMPOSITION_DECODE_RULE
;; defined in composite.h.

(defun decode-composition-rule (rule-code)
  (or (and (natnump rule-code) (< rule-code 144))
      (error "Invalid encoded composition rule: %S" rule-code))
  (let ((gref (car (rassq (/ rule-code 12) reference-point-alist)))
	(nref (car (rassq (% rule-code 12) reference-point-alist))))
    (or (and gref (symbolp gref) nref (symbolp nref))
	(error "Invalid composition rule code: %S" rule-code))
    (cons gref nref)))

;; Encode composition rules in composition components COMPONENTS.  The
;; value is a copy of COMPONENTS, where composition rules (cons of
;; global and new glyph reference point symbols) are replaced with
;; encoded composition rules.  Optional 2nd argument NOCOPY non-nil
;; means don't make a copy but modify COMPONENTS directly.

(defun encode-composition-components (components &optional nocopy)
  (or nocopy
      (setq components (copy-sequence components)))
  (if (vectorp components)
      (let ((len (length components))
	    (i 1))
	(while (< i len)
	  (aset components i
		(encode-composition-rule (aref components i)))
	  (setq i (+ i 2))))
    (let ((tail (cdr components)))
      (while tail
	(setcar tail
		(encode-composition-rule (car tail)))
	(setq tail (nthcdr 2 tail)))))
  components)

;; Decode composition rule codes in composition components COMPONENTS.
;; The value is a copy of COMPONENTS, where composition rule codes are
;; replaced with composition rules (cons of global and new glyph
;; reference point symbols).  Optional 2nd argument NOCOPY non-nil
;; means don't make a copy but modify COMPONENTS directly.
;; It is assumed that COMPONENTS is a vector and is for rule-base
;; composition, thus (2N+1)th elements are rule codes.

(defun decode-composition-components (components &optional nocopy)
  (or nocopy
      (setq components (copy-sequence components)))
  (let ((len (length components))
	(i 1))
    (while (< i len)
      (aset components i
	    (decode-composition-rule (aref components i)))
      (setq i (+ i 2))))
  components)

;;;###autoload
(defun compose-region (start end &optional components modification-func)
  "Compose characters in the current region.

Characters are composed relatively, i.e. composed by overstricking or
stacking depending on ascent, descent and other properties.

When called from a program, expects these four arguments.

First two arguments START and END are positions (integers or markers)
specifying the region.

Optional 3rd argument COMPONENTS, if non-nil, is a character or a
sequence (vector, list, or string) of integers.  In this case,
characters are composed not relatively but according to COMPONENTS.

If it is a character, it is an alternate character to display instead
of the text in the region.

If it is a string, the elements are alternate characters.

If it is a vector or list, it is a sequence of alternate characters and
composition rules, where (2N)th elements are characters and (2N+1)th
elements are composition rules to specify how to compose (2N+2)th
elements with previously composed N glyphs.

A composition rule is a cons of global and new glyph reference point
symbols.  See the documentation of `reference-point-alist' for more
detail.

Optional 4th argument MODIFICATION-FUNC is a function to call to
adjust the composition when it gets invalid because of a change of
text in the composition."
  (interactive "r")
  (let ((modified-p (buffer-modified-p))
	(buffer-read-only nil))
    (if (or (vectorp components) (listp components))
	(setq components (encode-composition-components components)))
    (compose-region-internal start end components modification-func)
    (restore-buffer-modified-p modified-p)))

;;;###autoload
(defun decompose-region (start end)
  "Decompose text in the current region.

When called from a program, expects two arguments,
positions (integers or markers) specifying the region."
  (interactive "r")
  (let ((modified-p (buffer-modified-p))
	(buffer-read-only nil))
    (remove-text-properties start end '(composition nil))
    (set-buffer-modified-p modified-p)))

;;;###autoload
(defun compose-string (string &optional start end components modification-func)
  "Compose characters in string STRING.

The return value is STRING where `composition' property is put on all
the characters in it.

Optional 2nd and 3rd arguments START and END specify the range of
STRING to be composed.  They default to the beginning and the end of
STRING respectively.

Optional 4th argument COMPONENTS, if non-nil, is a character or a
sequence (vector, list, or string) of integers.  See the function
`compose-region' for more detail.

Optional 5th argument MODIFICATION-FUNC is a function to call to
adjust the composition when it gets invalid because of a change of
text in the composition."
  (if (or (vectorp components) (listp components))
      (setq components (encode-composition-components components)))
  (or start (setq start 0))
  (or end (setq end (length string)))
  (compose-string-internal string start end components modification-func)
  string)

;;;###autoload
(defun decompose-string (string)
  "Return STRING where `composition' property is removed."
  (remove-text-properties 0 (length string) '(composition nil) string)
  string)

;;;###autoload
(defun compose-chars (&rest args)
  "Return a string from arguments in which all characters are composed.
For relative composition, arguments are characters.
For rule-based composition, Mth \(where M is odd) arguments are
characters, and Nth \(where N is even) arguments are composition rules.
A composition rule is a cons of glyph reference points of the form
\(GLOBAL-REF-POINT . NEW-REF-POINT).  See the documentation of
`reference-point-alist' for more detail."
  (let (str components)
    (if (consp (car (cdr args)))
	;; Rule-base composition.
	(let ((len (length args))
	      (tail (encode-composition-components args 'nocopy)))

	  (while tail
	    (setq str (cons (car tail) str))
	    (setq tail (nthcdr 2 tail)))
	  (setq str (concat (nreverse str))
		components args))
      ;; Relative composition.
      (setq str (concat args)))
    (compose-string-internal str 0 (length str) components)))

;;;###autoload
(defun find-composition (pos &optional limit string detail-p)
  "Return information about a composition at or nearest to buffer position POS.

If the character at POS has `composition' property, the value is a list
of FROM, TO, and VALID-P.

FROM and TO specify the range of text that has the same `composition'
property, VALID-P is non-nil if and only if this composition is valid.

If there's no composition at POS, and the optional 2nd argument LIMIT
is non-nil, search for a composition toward LIMIT.

If no composition is found, return nil.

Optional 3rd argument STRING, if non-nil, is a string to look for a
composition in; nil means the current buffer.

If a valid composition is found and the optional 4th argument DETAIL-P
is non-nil, the return value is a list of FROM, TO, COMPONENTS,
RELATIVE-P, MOD-FUNC, and WIDTH.

COMPONENTS is a vector of integers, the meaning depends on RELATIVE-P.

RELATIVE-P is t if the composition method is relative, else nil.

If RELATIVE-P is t, COMPONENTS is a vector of characters to be
composed.  If RELATIVE-P is nil, COMPONENTS is a vector of characters
and composition rules as described in `compose-region'.

MOD-FUNC is a modification function of the composition.

WIDTH is a number of columns the composition occupies on the screen."
  (let ((result (find-composition-internal pos limit string detail-p)))
    (if (and detail-p result (nth 2 result) (not (nth 3 result)))
	;; This is a valid rule-base composition.
	(decode-composition-components (nth 2 result) 'nocopy))
    result))


;;;###autoload
(defun compose-chars-after (pos &optional limit object)
  "Compose characters in current buffer after position POS.

It looks up the char-table `composition-function-table' (which see) by
a character after POS.  If non-nil value is found, the format of the
value should be an alist of PATTERNs vs FUNCs, where PATTERNs are
regular expressions and FUNCs are functions.  If the text after POS
matches one of PATTERNs, call the corresponding FUNC with three
arguments POS, TO, and PATTERN, where TO is the end position of text
matching PATTERN, and return what FUNC returns.  Otherwise, return
nil.

FUNC is responsible for composing the text properly.  The return value
is:
  nil -- if no characters were composed.
  CHARS (integer) -- if CHARS characters were composed.

Optional 2nd arg LIMIT, if non-nil, limits the matching of text.

Optional 3rd arg OBJECT, if non-nil, is a string that contains the
text to compose.  In that case, POS and LIMIT index to the string.

This function is the default value of `compose-chars-after-function'."
  (let ((tail (aref composition-function-table (char-after pos)))
	pattern func result)
    (when tail
      (save-match-data
	(save-excursion
	  (while (and tail (not func))
	    (setq pattern (car (car tail))
		  func (cdr (car tail)))
	    (goto-char pos)
	    (if (if limit
		    (and (re-search-forward pattern limit t)
			 (= (match-beginning 0) pos))
		  (looking-at pattern))
		(setq result (funcall func pos (match-end 0) pattern nil))
	      (setq func nil tail (cdr tail)))))))
      result))

;;;###autoload
(defun compose-last-chars (args)
  "Compose last characters.
The argument is a parameterized event of the form
	\(compose-last-chars N COMPONENTS),
where N is the number of characters before point to compose,
COMPONENTS, if non-nil, is the same as the argument to `compose-region'
\(which see).  If it is nil, `compose-chars-after' is called,
and that function finds a proper rule to compose the target characters.
This function is intended to be used from input methods.
The global keymap binds special event `compose-last-chars' to this
function.  Input method may generate an event (compose-last-chars N COMPONENTS)
after a sequence of character events."
  (interactive "e")
  (let ((chars (nth 1 args)))
    (if (and (numberp chars)
	     (>= (- (point) (point-min)) chars))
	(if (nth 2 args)
	    (compose-region (- (point) chars) (point) (nth 2 args))
	  (compose-chars-after (- (point) chars) (point))))))

;;;###autoload(global-set-key [compose-last-chars] 'compose-last-chars)


;;; The following codes are only for backward compatibility with Emacs
;;; 20.4 and earlier.

;;;###autoload
(defun decompose-composite-char (char &optional type with-composition-rule)
  "Convert CHAR to string.

If optional 2nd arg TYPE is non-nil, it is `string', `list', or
`vector'.  In this case, CHAR is converted to string, list of CHAR, or
vector of CHAR respectively.
Optional 3rd arg WITH-COMPOSITION-RULE is ignored."
  (cond ((or (null type) (eq type 'string)) (char-to-string char))
	((eq type 'list) (list char))
	(t (vector char))))

;;;###autoload
(make-obsolete 'decompose-composite-char 'char-to-string "21.1")



;;; arch-tag: ee703d77-1723-45d4-a31f-e9f0f867aa33
;;; composite.el ends here
