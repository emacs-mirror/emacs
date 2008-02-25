;;; composite.el --- support character composition

;; Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
;;   2008
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021

;; Keywords: mule, multilingual, character composition

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

;;; Commentary:

;;; Code:

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
rule in COMPONENTS argument to such functions as `compose-region'.

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

A composition rule may have the form \(GLOBAL-REF-POINT
NEW-REF-POINT XOFF YOFF), where XOFF and YOFF specifies how much
to shift NEW-REF-POINT from GLOBAL-REF-POINT.  In this case, XOFF
and YOFF are integers in the range -100..100 representing the
shifting percentage against the font size.")


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
    (if (consp rule)
	(let ((gref (car rule))
	      (nref (cdr rule))
	      xoff yoff)
	  (if (consp nref)		; (GREF NREF XOFF YOFF)
	      (progn
		(setq xoff (nth 1 nref)
		      yoff (nth 2 nref)
		      nref (car nref))
		(or (and (>= xoff -100) (<= xoff 100)
			 (>= yoff -100) (<= yoff 100))
		    (error "Invalid composition rule: %s" rule))
		(setq xoff (+ xoff 128) yoff (+ yoff 128)))
	    ;; (GREF . NREF)
	    (setq xoff 0 yoff 0))
	  (or (integerp gref)
	      (setq gref (cdr (assq gref reference-point-alist))))
	  (or (integerp nref)
	      (setq nref (cdr (assq nref reference-point-alist))))
	  (or (and (>= gref 0) (< gref 12) (>= nref 0) (< nref 12))
	      (error "Invalid composition rule: %S" rule))
	  (logior (lsh xoff 16) (lsh yoff 8) (+ (* gref 12) nref)))
      (error "Invalid composition rule: %S" rule))))

;; Decode encoded composition rule RULE-CODE.  The value is a cons of
;; global and new reference point symbols.
;; This must be compatible with C macro COMPOSITION_DECODE_RULE
;; defined in composite.h.

(defun decode-composition-rule (rule-code)
  (or (and (natnump rule-code) (< rule-code #x1000000))
      (error "Invalid encoded composition rule: %S" rule-code))
  (let ((xoff (lsh rule-code -16))
	(yoff (logand (lsh rule-code -8) #xFF))
	gref nref)
    (setq rule-code (logand rule-code #xFF)
	  gref (car (rassq (/ rule-code 12) reference-point-alist))
	  nref (car (rassq (% rule-code 12) reference-point-alist)))
    (or (and gref (symbolp gref) nref (symbolp nref))
	(error "Invalid composition rule code: %S" rule-code))
    (if (and (= xoff 0) (= yoff 0))
	(cons gref nref)
      (setq xoff (- xoff 128) yoff (- yoff 128))
      (list gref xoff yoff nref))))

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

(defun compose-region (start end &optional components modification-func)
  "Compose characters in the current region.

Characters are composed relatively, i.e. composed by overstricking or
stacking depending on ascent, descent and other properties.

When called from a program, expects these four arguments.

First two arguments START and END are positions (integers or markers)
specifying the region.

Optional 3rd argument COMPONENTS, if non-nil, is a character, a string
or a vector or list of integers and rules.

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
	(inhibit-read-only t))
    (if (or (vectorp components) (listp components))
	(setq components (encode-composition-components components)))
    (compose-region-internal start end components modification-func)
    (restore-buffer-modified-p modified-p)))

(defun decompose-region (start end)
  "Decompose text in the current region.

When called from a program, expects two arguments,
positions (integers or markers) specifying the region."
  (interactive "r")
  (let ((modified-p (buffer-modified-p))
	(inhibit-read-only t))
    (remove-text-properties start end '(composition nil))
    (restore-buffer-modified-p modified-p)))

(defun compose-string (string &optional start end components modification-func)
  "Compose characters in string STRING.

The return value is STRING with the `composition' property put on all
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

(defun decompose-string (string)
  "Return STRING where `composition' property is removed."
  (remove-text-properties 0 (length string) '(composition nil) string)
  string)

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


(defun compose-chars-after (pos &optional limit object)
  "Compose characters in current buffer after position POS.

It looks up the char-table `composition-function-table' (which
see) by a character at POS, and compose characters after POS
according to the contents of `composition-function-table'.

Optional 2nd arg LIMIT, if non-nil, limits characters to compose.

Optional 3rd arg OBJECT, if non-nil, is a string that contains the
text to compose.  In that case, POS and LIMIT index into the string.

This function is the default value of `compose-chars-after-function'."
  (let ((tail (aref composition-function-table (char-after pos)))
	(font-obj (and (display-multi-font-p)
		       (and (not (stringp object))
			    (font-at pos (selected-window)))))
	pattern func result)
    (or limit
	(setq limit (if (stringp object) (length object) (point-max))))
    (when tail
      (save-match-data
	(save-excursion
	  (while tail
	    (if (functionp (car tail))
		(setq pattern nil func (car tail))
	      (setq pattern (car (car tail))
		    func (cdr (car tail))))
	    (goto-char pos)
	    (if pattern
		(if (and (if (stringp object)
			     (eq (string-match pattern object) 0)
			   (looking-at pattern))
			 (<= (match-end 0) limit))
		    (setq result
			  (funcall func pos (match-end 0) font-obj object)))
	      (setq result (funcall func pos limit font-obj  object)))
	    (if result (setq tail nil))))))
    result))

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

(global-set-key [compose-last-chars] 'compose-last-chars)


;;; Automatic character composition.

(defvar composition-function-table
  (make-char-table nil)
  "Char table of functions for automatic character composition.
For each character that has to be composed automatically with
preceding and/or following characters, this char table contains
a function to call to compose that character.

An element, if non-nil, is FUNC or an alist of PATTERNs vs FUNCs,
where PATTERNs are regular expressions and FUNCs are functions.
If the element is FUNC, FUNC itself determines the region to
compose.

Each function is called with 4 arguments, FROM, TO, FONT-OBJECT,
and STRING.

If STRING is nil, FROM and TO are positions specifying the region
matching with PATTERN in the current buffer, and the function has
to compose character in that region (possibly with characters
preceding FROM).  FONT-OBJECT may be nil if not
available (e.g. for the case of terminal).  The return value of
the function is the end position where characters are composed,
or nil if no composition is made.

Otherwise, STRING is a string, and FROM and TO are indices into
the string.  In this case, the function has to compose a
character in the string.  The others are the same as above.

See also the documentation of `auto-composition-mode'.")

;; Copied from font-lock.el.
(eval-when-compile
  ;; Borrowed from lazy-lock.el.
  ;; We use this to preserve or protect things when modifying text properties.
  (defmacro save-buffer-state (varlist &rest body)
    "Bind variables according to VARLIST and eval BODY restoring buffer state."
    `(let* ,(append varlist
		    '((modified (buffer-modified-p)) (buffer-undo-list t)
		      (inhibit-read-only t) (inhibit-point-motion-hooks t)
		      (inhibit-modification-hooks t)
		      deactivate-mark buffer-file-name buffer-file-truename))
       ,@body
       (unless modified
	 (restore-buffer-modified-p nil))))
  ;; Fixme: This makes bootstrapping fail with this error.
  ;;   Symbol's function definition is void: eval-defun
  ;;(def-edebug-spec save-buffer-state let)
  )

(put 'save-buffer-state 'lisp-indent-function 1)

(defun terminal-composition-function (from to font-object string)
  "General composition function used on terminal.
Non-spacing characters are composed with the preceding spacing
character.  All non-spacing characters has this function in
`terminal-composition-function-table'."
  (let ((pos (1+ from)))
    (if string
	(progn
	  (while (and (< pos to)
		      (= (aref char-width-table (aref string pos)) 0))
	    (setq pos (1+ pos)))
	  (if (> from 0)
	      (compose-string string (1- from) pos)
	    (compose-string string from pos
			    (concat " " (buffer-substring from pos)))))
      (while (and (< pos to)
		  (= (aref char-width-table (char-after pos)) 0))
	(setq pos (1+ pos)))
      (if (> from (point-min))
	  (compose-region (1- from) pos (buffer-substring from pos))
	(compose-region from pos
			(concat " " (buffer-substring from pos)))))
    pos))

(defvar terminal-composition-function-table
  (let ((table (make-char-table nil)))
    (map-char-table
     #'(lambda (key val)
	 (if (= val 0) (set-char-table-range table key
					     'terminal-composition-function)))
     char-width-table)
    table)
  "Char table of functions for automatic character composition on terminal.
This is like `composition-function-table' but used when Emacs is running
on a terminal.")

(defun auto-compose-chars (from to window string)
  "Compose characters in the region between FROM and TO.
WINDOW is a window displaying the current buffer.
If STRING is non-nil, it is a string, and FROM and TO are indices
into the string.  In that case, compose characters in the string.

This function is the default value of `auto-composition-function' (which see)."
  (save-buffer-state nil
    (save-excursion
      (save-restriction
	(save-match-data
	  (let ((table (if (display-graphic-p)
			   composition-function-table
			 terminal-composition-function-table))
		(start from))
	    (setq to (or (text-property-any (1+ from) to 'auto-composed t
					    string)
			 to))
	    (if string
		(while (< from to)
		  (let* ((ch (aref string from))
			 (elt (aref table ch))
			 font-obj newpos)
		    (when elt
		      (if window
			  (setq font-obj (font-at from window string)))
		      (if (functionp elt)
			  (setq newpos (funcall elt from to font-obj string))
			(while (and elt
				    (or (not (eq (string-match (caar elt) string
							       from)
						 from))
					(not (setq newpos
						   (funcall (cdar elt) from
							    (match-end 0)
							    font-obj string)))))
			  (setq elt (cdr elt)))))
		    (if (and newpos (> newpos from))
			(setq from newpos)
		      (setq from (1+ from)))))
	      (narrow-to-region from to)
	      (while (< from to)
		  (let* ((ch (char-after from))
			 (elt (aref table ch))
			 func pattern font-obj newpos)
		    (when elt
		      (if window
			  (setq font-obj (font-at from window)))
		      (if (functionp elt)
			  (setq newpos (funcall elt from to font-obj nil))
			(goto-char from)
			(while (and elt
				    (or (not (looking-at (caar elt)))
					(not (setq newpos
						   (funcall (cdar elt) from
							    (match-end 0)
							    font-obj nil)))))
			  (setq elt (cdr elt)))))
		    (if (and newpos (> newpos from))
			(setq from newpos)
		      (setq from (1+ from))))))
	    (put-text-property start to 'auto-composed t string)))))))

(make-variable-buffer-local 'auto-composition-function)

;;;###autoload
(define-minor-mode auto-composition-mode
  "Toggle Auto Composition mode.
With ARG, turn Auto Composition mode off if and only if ARG is a non-positive
number; if ARG is nil, toggle Auto Composition mode; anything else turns Auto
Composition on.

When Auto Composition is enabled, text characters are automatically composed
by functions registered in `composition-function-table' (which see).

You can use `global-auto-composition-mode' to turn on
Auto Composition mode in all buffers (this is the default)."
  nil nil nil
  (if noninteractive
      (setq auto-composition-mode nil))
  (cond (auto-composition-mode
	 (add-hook 'after-change-functions 'auto-composition-after-change nil t)
	 (setq auto-composition-function 'auto-compose-chars))
	(t
	 (remove-hook 'after-change-functions 'auto-composition-after-change t)
	 (setq auto-composition-function nil)))
  (save-buffer-state nil
    (save-restriction
      (widen)
      (remove-text-properties (point-min) (point-max) '(auto-composed nil))
      (decompose-region (point-min) (point-max)))))

(defun auto-composition-after-change (start end old-len)
  (save-buffer-state nil
    (if (< start (point-min))
	(setq start (point-min)))
    (if (> end (point-max))
	(setq end (point-max)))
    (when (and auto-composition-mode (not memory-full))
      (let (func1 func2)
	(when (and (> start (point-min))
		   (setq func2 (aref composition-function-table
				     (char-after (1- start))))
		   (or (= start (point-max))
		       (not (setq func1 (aref composition-function-table
					      (char-after start))))
		       (eq func1 func2)))
	  (setq start (1- start)
		func1 func2)
	  (while (eq func1 func2)
	    (if (> start (point-min))
		(setq start (1- start)
		      func2 (aref composition-function-table
				  (char-after start)))
	      (setq func2 nil))))
	(when (and (< end (point-max))
		   (setq func2 (aref composition-function-table
				     (char-after end)))
		   (or (= end (point-min))
		       (not (setq func1 (aref composition-function-table
					      (char-after (1- end)))))
		       (eq func1 func2)))
	  (setq end (1+ end)
		func1 func2)
	  (while (eq func1 func2)
	    (if (< end (point-max))
		(setq func2 (aref composition-function-table
				  (char-after end))
		      end (1+ end))
	      (setq func2 nil))))
	(if (< start end)
	    (remove-text-properties start end '(auto-composed nil)))))))

(defun turn-on-auto-composition-if-enabled ()
  (if enable-multibyte-characters
      (auto-composition-mode 1)))

;;;###autoload
(define-global-minor-mode global-auto-composition-mode
  auto-composition-mode turn-on-auto-composition-if-enabled
  :extra-args (dummy)
  :initialize 'custom-initialize-safe-default
  :init-value (not noninteractive)
  :group 'auto-composition
  :version "23.1")

(defun toggle-auto-composition (&optional arg)
  "Change whether automatic character composition is enabled in this buffer.
With arg, enable it iff arg is positive."
  (interactive "P")
  (let ((enable (if (null arg) (not auto-composition-function)
		  (> (prefix-numeric-value arg) 0))))
    (if enable
	(kill-local-variable 'auto-composition-function)
      (make-local-variable 'auto-composition-function)
      (setq auto-composition-function nil)
      (save-buffer-state nil
	(save-restriction
	  (widen)
	  (decompose-region (point-min) (point-max)))))

    (save-buffer-state nil
      (save-restriction
	(widen)
	(remove-text-properties (point-min) (point-max)
				'(auto-composed nil))))))

(defun auto-compose-region (from to)
  "Force automatic character composition on the region FROM and TO."
  (save-excursion
    (if (get-text-property from 'auto-composed)
	(setq from (next-single-property-change from 'auto-composed nil to)))
    (goto-char from)
    (let ((modified-p (buffer-modified-p))
	  (inhibit-read-only '(composition auto-composed))
	  (stop (next-single-property-change (point) 'auto-composed nil to)))
      (while (< (point) to)
	(if (= (point) stop)
	    (progn
	      (goto-char (next-single-property-change (point)
						      'auto-composed nil to))
	      (setq stop (next-single-property-change (point)
						      'auto-composed nil to)))
	  (let ((func (aref composition-function-table (following-char)))
		(font-obj (and (display-multi-font-p)
			       (font-at (point) (selected-window))))
		(pos (point)))
	    (if (functionp func)
		(goto-char (funcall func (point) to font-obj nil)))
	    (if (<= (point) pos)
		(forward-char 1)))))
      (put-text-property from to 'auto-composed t)
      (set-buffer-modified-p modified-p))))


;; The following codes are only for backward compatibility with Emacs
;; 20.4 and earlier.

(defun decompose-composite-char (char &optional type with-composition-rule)
  "Convert CHAR to string.

If optional 2nd arg TYPE is non-nil, it is `string', `list', or
`vector'.  In this case, CHAR is converted to string, list of CHAR, or
vector of CHAR respectively.
Optional 3rd arg WITH-COMPOSITION-RULE is ignored."
  (cond ((or (null type) (eq type 'string)) (char-to-string char))
	((eq type 'list) (list char))
	(t (vector char))))

(make-obsolete 'decompose-composite-char 'char-to-string "21.1")



;; arch-tag: ee703d77-1723-45d4-a31f-e9f0f867aa33
;;; composite.el ends here
