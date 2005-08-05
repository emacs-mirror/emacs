;;; cc-engine.el --- core syntax guessing engine for CC mode

;; Copyright (C) 1985,1987,1992-2003, 2004, 2005 Free Software Foundation, Inc.

;; Authors:    1998- Martin Stjernholm
;;             1992-1999 Barry A. Warsaw
;;             1987 Dave Detlefs and Stewart Clamen
;;             1985 Richard M. Stallman
;; Maintainer: bug-cc-mode@gnu.org
;; Created:    22-Apr-1997 (split from cc-mode.el)
;; Version:    See cc-mode.el
;; Keywords:   c languages oop

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
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; The functions which have docstring documentation can be considered
;; part of an API which other packages can use in CC Mode buffers.
;; Otoh, undocumented functions and functions with the documentation
;; in comments are considered purely internal and can change semantics
;; or even disappear in the future.
;;
;; (This policy applies to CC Mode as a whole, not just this file.  It
;; probably also applies to many other Emacs packages, but here it's
;; clearly spelled out.)

;; Hidden buffer changes
;;
;; Various functions in CC Mode use text properties for caching and
;; syntactic markup purposes, and those of them that might modify such
;; properties are said to do "hidden buffer changes".  They should be
;; used within `c-save-buffer-state' or a similar function that saves
;; and restores buffer modifiedness etc.
;;
;; Interactive functions are assumed to not do hidden buffer changes
;; (this isn't applicable in the specific parts of them that do real
;; changes, though).
;;
;; All other functions are assumed to do hidden buffer changes and
;; must thus be wrapped inside `c-save-buffer-state' if they're used
;; from any function that does not do hidden buffer changes.
;;
;; Every function, except the interactive ones, that doesn't do hidden
;; buffer changes have that explicitly stated in their docstring or
;; comment.

;; Use of text properties
;;
;; CC Mode uses several text properties internally to mark up various
;; positions, e.g. to improve speed and to eliminate glitches in
;; interactive refontification.
;;
;; Note: This doc is for internal use only.  Other packages should not
;; assume that these text properties are used as described here.
;;
;; 'syntax-table
;;   Used to modify the syntax of some characters.  Currently used to
;;   mark the "<" and ">" of angle bracket parens with paren syntax.
;;
;;   This property is used on single characters and is therefore
;;   always treated as front and rear nonsticky (or start and end open
;;   in XEmacs vocabulary).  It's therefore installed on
;;   `text-property-default-nonsticky' if that variable exists (Emacs
;;   >= 21).
;;
;; 'c-is-sws and 'c-in-sws
;;   Used by `c-forward-syntactic-ws' and `c-backward-syntactic-ws' to
;;   speed them up.  See the comment blurb before `c-put-is-sws'
;;   below for further details.
;;
;; 'c-type
;;   This property is used on single characters to mark positions with
;;   special syntactic relevance of various sorts.  It's primary use
;;   is to avoid glitches when multiline constructs are refontified
;;   interactively (on font lock decoration level 3).  It's cleared in
;;   a region before it's fontified and is then put on relevant chars
;;   in that region as they are encountered during the fontification.
;;   The value specifies the kind of position:
;;
;;     'c-decl-arg-start
;;  	 Put on the last char of the token preceding each declaration
;;  	 inside a declaration style arglist (typically in a function
;;  	 prototype).
;;
;;     'c-decl-end
;;  	 Put on the last char of the token preceding a declaration.
;;  	 This is used in cases where declaration boundaries can't be
;;  	 recognized simply by looking for a token like ";" or "}".
;;  	 `c-type-decl-end-used' must be set if this is used (see also
;;  	 `c-find-decl-spots').
;;
;;     'c-<>-arg-sep
;;  	 Put on the commas that separate arguments in angle bracket
;;  	 arglists like C++ template arglists.
;;
;;     'c-decl-id-start and 'c-decl-type-start
;;  	 Put on the last char of the token preceding each declarator
;;  	 in the declarator list of a declaration.  They are also used
;;  	 between the identifiers cases like enum declarations.
;;  	 'c-decl-type-start is used when the declarators are types,
;;  	 'c-decl-id-start otherwise.
;;
;; 'c-awk-NL-prop
;;   Used in AWK mode to mark the various kinds of newlines.  See
;;   cc-awk.el.

;;; Code:

(eval-when-compile
  (let ((load-path
	 (if (and (boundp 'byte-compile-dest-file)
		  (stringp byte-compile-dest-file))
	     (cons (file-name-directory byte-compile-dest-file) load-path)
	   load-path)))
    (load "cc-bytecomp" nil t)))

(cc-require 'cc-defs)
(cc-require-when-compile 'cc-langs)
(cc-require 'cc-vars)

;; Some functions/constants in cc-awk.el that are called/referenced here.
;; (Can't use cc-require due to cyclicity.)
(cc-bytecomp-defun c-awk-unstick-NL-prop)
(cc-bytecomp-defun c-awk-clear-NL-props)
(cc-bytecomp-defvar awk-mode-syntax-table)
(cc-bytecomp-defun c-awk-backward-syntactic-ws)
(cc-bytecomp-defun c-awk-after-logical-semicolon)
(cc-bytecomp-defun c-awk-NL-prop-not-set)
(cc-bytecomp-defun c-awk-completed-stmt-ws-ends-line-p)
(cc-bytecomp-defun c-awk-completed-stmt-ws-ends-prev-line-p)
(cc-bytecomp-defun c-awk-prev-line-incomplete-p)
(cc-bytecomp-defun c-awk-after-change)

;; Silence the compiler.
(cc-bytecomp-defun buffer-syntactic-context) ; XEmacs


;; Make declarations for all the `c-lang-defvar' variables in cc-langs.

(defmacro c-declare-lang-variables ()
  `(progn
     ,@(apply 'nconc
	      (mapcar (lambda (init)
			`(,(if (elt init 2)
			       `(defvar ,(car init) nil ,(elt init 2))
			     `(defvar ,(car init) nil))
			  (make-variable-buffer-local ',(car init))))
		      (cdr c-lang-variable-inits)))))
(c-declare-lang-variables)


;;; Internal state variables.

;; Internal state of hungry delete key feature
(defvar c-hungry-delete-key nil)
(make-variable-buffer-local 'c-hungry-delete-key)

;; Internal state of auto newline feature.
(defvar c-auto-newline nil)
(make-variable-buffer-local 'c-auto-newline)

;; Internal auto-newline/hungry-delete designation string for mode line.
(defvar c-auto-hungry-string nil)
(make-variable-buffer-local 'c-auto-hungry-string)

(defun c-calculate-state (arg prevstate)
  ;; Calculate the new state of PREVSTATE, t or nil, based on arg. If
  ;; arg is nil or zero, toggle the state. If arg is negative, turn
  ;; the state off, and if arg is positive, turn the state on
  (if (or (not arg)
	  (zerop (setq arg (prefix-numeric-value arg))))
      (not prevstate)
    (> arg 0)))

;; Dynamically bound cache for `c-in-literal'.
(defvar c-in-literal-cache t)

;; Must be set in buffers where the `c-type' text property might be used
;; with the value `c-decl-end'.
(defvar c-type-decl-end-used nil)
(make-variable-buffer-local 'c-type-decl-end-used)


;; Basic handling of preprocessor directives.

;; This is a dynamically bound cache used together with
;; `c-query-macro-start' and `c-query-and-set-macro-start'.  It only
;; works as long as point doesn't cross a macro boundary.
(defvar c-macro-start 'unknown)

(defsubst c-query-and-set-macro-start ()
  ;; This function does not do any hidden buffer changes.
  (if (symbolp c-macro-start)
      (setq c-macro-start (save-excursion
			    (and (c-beginning-of-macro)
				 (point))))
    c-macro-start))

(defsubst c-query-macro-start ()
  ;; This function does not do any hidden buffer changes.
  (if (symbolp c-macro-start)
      (save-excursion
	(and (c-beginning-of-macro)
	     (point)))
    c-macro-start))

(defun c-beginning-of-macro (&optional lim)
  "Go to the beginning of a preprocessor directive.
Leave point at the beginning of the directive and return t if in one,
otherwise return nil and leave point unchanged.

This function does not do any hidden buffer changes."
  (when c-opt-cpp-prefix
    (let ((here (point)))
      (save-restriction
	(if lim (narrow-to-region lim (point-max)))
	(beginning-of-line)
	(while (eq (char-before (1- (point))) ?\\)
	  (forward-line -1))
	(back-to-indentation)
	(if (and (<= (point) here)
		 (looking-at c-opt-cpp-start))
	    t
	  (goto-char here)
	  nil)))))

(defun c-end-of-macro ()
  "Go to the end of a preprocessor directive.
More accurately, move point to the end of the closest following line
that doesn't end with a line continuation backslash.

This function does not do any hidden buffer changes."
  (while (progn
	   (end-of-line)
	   (when (and (eq (char-before) ?\\)
		      (not (eobp)))
	     (forward-char)
	     t))))

(defun c-forward-to-cpp-define-body ()
  ;; Assuming point is at the "#" that introduces a preprocessor
  ;; directive, it's moved forward to the start of the definition body
  ;; if it's a "#define".  Non-nil is returned in this case, in all
  ;; other cases nil is returned and point isn't moved.
  (when (and (looking-at
	      (concat "#[ \t]*"
		      "define[ \t]+\\(\\sw\\|_\\)+\\(\([^\)]*\)\\)?"
		      "\\([ \t]\\|\\\\\n\\)*"))
	     (not (= (match-end 0) (c-point 'eol))))
    (goto-char (match-end 0))))


;;; Basic utility functions.

(defun c-syntactic-content (from to)
  ;; Return the given region as a string where all syntactic
  ;; whitespace is removed or, where necessary, replaced with a single
  ;; space.
  (save-excursion
    (goto-char from)
    (let* ((parts (list nil)) (tail parts) pos)
      (while (re-search-forward c-syntactic-ws-start to t)
	(goto-char (setq pos (match-beginning 0)))
	(c-forward-syntactic-ws to)
	(if (= (point) pos)
	    (forward-char)
	  (if (and (> pos from)
		   (< (point) to)
		   (looking-at "\\w\\|\\s_")
		   (save-excursion
		     (goto-char (1- pos))
		     (looking-at "\\w\\|\\s_")))
	      (progn
		(setcdr tail (list (buffer-substring-no-properties from pos)
				   " "))
		(setq tail (cddr tail)))
	    (setcdr tail (list (buffer-substring-no-properties from pos)))
	    (setq tail (cdr tail)))
	  (setq from (point))))
      (setcdr tail (list (buffer-substring-no-properties from to)))
      (apply 'concat (cdr parts)))))

(defsubst c-keyword-sym (keyword)
  ;; Return non-nil if the string KEYWORD is a known keyword.  More
  ;; precisely, the value is the symbol for the keyword in
  ;; `c-keywords-obarray'.
  (intern-soft keyword c-keywords-obarray))

(defsubst c-keyword-member (keyword-sym lang-constant)
  ;; Return non-nil if the symbol KEYWORD-SYM, as returned by
  ;; `c-keyword-sym', is a member of LANG-CONSTANT, which is the name
  ;; of a language constant that ends with "-kwds".  If KEYWORD-SYM is
  ;; nil then the result is nil.
  (get keyword-sym lang-constant))

;; String syntax chars, suitable for skip-syntax-(forward|backward).
(defconst c-string-syntax (if (memq 'gen-string-delim c-emacs-features)
                              "\"|"
                            "\""))

;; Regexp matching string start syntax.
(defconst c-string-limit-regexp (if (memq 'gen-string-delim c-emacs-features)
                                    "\\s\"\\|\\s|"
                                  "\\s\""))

;; Holds formatted error strings for the few cases where parse errors
;; are reported.
(defvar c-parsing-error nil)
(make-variable-buffer-local 'c-parsing-error)

(defun c-echo-parsing-error (&optional quiet)
  ;; This function does not do any hidden buffer changes.
  (when (and c-report-syntactic-errors c-parsing-error (not quiet))
    (c-benign-error "%s" c-parsing-error))
  c-parsing-error)

;; Faces given to comments and string literals.  This is used in some
;; situations to speed up recognition; it isn't mandatory that font
;; locking is in use.  This variable is extended with the face in
;; `c-doc-face-name' when fontification is activated in cc-fonts.el.
(defvar c-literal-faces
  '(font-lock-comment-face font-lock-string-face
			   font-lock-comment-delimiter-face))

(defun c-shift-line-indentation (shift-amt)
  ;; This function does not do any hidden buffer changes.
  (let ((pos (- (point-max) (point)))
	(c-macro-start c-macro-start)
	tmp-char-inserted)
    (if (zerop shift-amt)
	nil
      (when (and (c-query-and-set-macro-start)
		 (looking-at "[ \t]*\\\\$")
		 (save-excursion
		   (skip-chars-backward " \t")
		   (bolp)))
	(insert ?x)
	(backward-char)
	(setq tmp-char-inserted t))
      (unwind-protect
	  (let ((col (current-indentation)))
	    (delete-region (c-point 'bol) (c-point 'boi))
	    (beginning-of-line)
	    (indent-to (+ col shift-amt)))
	(when tmp-char-inserted
	  (delete-char 1))))
    ;; If initial point was within line's indentation and we're not on
    ;; a line with a line continuation in a macro, position after the
    ;; indentation.  Else stay at same point in text.
    (if (and (< (point) (c-point 'boi))
	     (not tmp-char-inserted))
	(back-to-indentation)
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos))))))


;; Some debug tools to visualize various special positions.  This
;; debug code isn't as portable as the rest of CC Mode.

(cc-bytecomp-defun overlays-in)
(cc-bytecomp-defun overlay-get)
(cc-bytecomp-defun overlay-start)
(cc-bytecomp-defun overlay-end)
(cc-bytecomp-defun delete-overlay)
(cc-bytecomp-defun overlay-put)
(cc-bytecomp-defun make-overlay)

(defun c-debug-add-face (beg end face)
  (c-save-buffer-state ((overlays (overlays-in beg end)) overlay)
    (while overlays
      (setq overlay (car overlays)
	    overlays (cdr overlays))
      (when (eq (overlay-get overlay 'face) face)
	(setq beg (min beg (overlay-start overlay))
	      end (max end (overlay-end overlay)))
	(delete-overlay overlay)))
    (overlay-put (make-overlay beg end) 'face face)))

(defun c-debug-remove-face (beg end face)
  (c-save-buffer-state ((overlays (overlays-in beg end)) overlay
			(ol-beg beg) (ol-end end))
    (while overlays
      (setq overlay (car overlays)
	    overlays (cdr overlays))
      (when (eq (overlay-get overlay 'face) face)
	(setq ol-beg (min ol-beg (overlay-start overlay))
	      ol-end (max ol-end (overlay-end overlay)))
	(delete-overlay overlay)))
    (when (< ol-beg beg)
      (overlay-put (make-overlay ol-beg beg) 'face face))
    (when (> ol-end end)
      (overlay-put (make-overlay end ol-end) 'face face))))


;; `c-beginning-of-statement-1' and accompanying stuff.

;; KLUDGE ALERT: c-maybe-labelp is used to pass information between
;; c-crosses-statement-barrier-p and c-beginning-of-statement-1.  A
;; better way should be implemented, but this will at least shut up
;; the byte compiler.
(defvar c-maybe-labelp nil)

;; New awk-compatible version of c-beginning-of-statement-1, ACM 2002/6/22

;; Macros used internally in c-beginning-of-statement-1 for the
;; automaton actions.
(defmacro c-bos-push-state ()
  '(setq stack (cons (cons state saved-pos)
		     stack)))
(defmacro c-bos-pop-state (&optional do-if-done)
  `(if (setq state (car (car stack))
	     saved-pos (cdr (car stack))
	     stack (cdr stack))
       t
     ,do-if-done
     (throw 'loop nil)))
(defmacro c-bos-pop-state-and-retry ()
  '(throw 'loop (setq state (car (car stack))
		      saved-pos (cdr (car stack))
		      ;; Throw nil if stack is empty, else throw non-nil.
		      stack (cdr stack))))
(defmacro c-bos-save-pos ()
  '(setq saved-pos (vector pos tok ptok pptok)))
(defmacro c-bos-restore-pos ()
  '(unless (eq (elt saved-pos 0) start)
     (setq pos (elt saved-pos 0)
	   tok (elt saved-pos 1)
	   ptok (elt saved-pos 2)
	   pptok (elt saved-pos 3))
     (goto-char pos)
     (setq sym nil)))
(defmacro c-bos-save-error-info (missing got)
  `(setq saved-pos (vector pos ,missing ,got)))
(defmacro c-bos-report-error ()
  '(unless noerror
     (setq c-parsing-error
	   (format "No matching `%s' found for `%s' on line %d"
		   (elt saved-pos 1)
		   (elt saved-pos 2)
		   (1+ (count-lines (point-min)
				    (c-point 'bol (elt saved-pos 0))))))))

(defun c-beginning-of-statement-1 (&optional lim ignore-labels
					     noerror comma-delim)
  "Move to the start of the current statement or declaration, or to
the previous one if already at the beginning of one.  Only
statements/declarations on the same level are considered, i.e. don't
move into or out of sexps (not even normal expression parentheses).

Stop at statement continuation tokens like \"else\", \"catch\",
\"finally\" and the \"while\" in \"do ... while\" if the start point
is within the continuation.  If starting at such a token, move to the
corresponding statement start.  If at the beginning of a statement,
move to the closest containing statement if there is any.  This might
also stop at a continuation clause.

Labels are treated as separate statements if IGNORE-LABELS is non-nil.
The function is not overly intelligent in telling labels from other
uses of colons; if used outside a statement context it might trip up
on e.g. inherit colons, so IGNORE-LABELS should be used then.  There
should be no such mistakes in a statement context, however.

Macros are ignored unless point is within one, in which case the
content of the macro is treated as normal code.  Aside from any normal
statement starts found in it, stop at the first token of the content
in the macro, i.e. the expression of an \"#if\" or the start of the
definition in a \"#define\".  Also stop at start of macros before
leaving them.

Return 'label if stopped at a label, 'same if stopped at the beginning
of the current statement, 'up if stepped to a containing statement,
'previous if stepped to a preceding statement, 'beginning if stepped
from a statement continuation clause to its start clause, or 'macro if
stepped to a macro start.  Note that 'same and not 'label is returned
if stopped at the same label without crossing the colon character.

LIM may be given to limit the search.  If the search hits the limit,
point will be left at the closest following token, or at the start
position if that is less ('same is returned in this case).

NOERROR turns off error logging to `c-parsing-error'.

Normally only ';' is considered to delimit statements, but if
COMMA-DELIM is non-nil then ',' is treated likewise."

  ;; The bulk of this function is a pushdown automaton that looks at statement
  ;; boundaries and the tokens (such as "while") in c-opt-block-stmt-key.  Its
  ;; purpose is to keep track of nested statements, ensuring that such
  ;; statments are skipped over in their entirety (somewhat akin to what C-M-p
  ;; does with nested braces/brackets/parentheses).
  ;;
  ;; Note: The position of a boundary is the following token.
  ;;
  ;; Beginning with the current token (the one following point), move back one
  ;; sexp at a time (where a sexp is, more or less, either a token or the
  ;; entire contents of a brace/bracket/paren pair).  Each time a statement
  ;; boundary is crossed or a "while"-like token is found, update the state of
  ;; the PDA.  Stop at the beginning of a statement when the stack (holding
  ;; nested statement info) is empty and the position has been moved.
  ;;
  ;; The following variables constitute the PDA:
  ;;
  ;; sym:    This is either the "while"-like token (e.g. 'for) we've just
  ;;         scanned back over, 'boundary if we've just gone back over a
  ;;         statement boundary, or nil otherwise.
  ;; state:  takes one of the values (nil else else-boundary while
  ;;         while-boundary catch catch-boundary).
  ;;         nil means "no "while"-like token yet scanned".
  ;;         'else, for example, means "just gone back over an else".
  ;;         'else-boundary means "just gone back over a statement boundary
  ;;         immediately after having gone back over an else".
  ;; saved-pos: A vector of either saved positions (tok ptok pptok, etc.) or
  ;;         of error reporting information.
  ;; stack:  The stack onto which the PDA pushes its state.  Each entry
  ;;         consists of a saved value of state and saved-pos.  An entry is
  ;;         pushed when we move back over a "continuation" token (e.g. else)
  ;;         and popped when we encounter the corresponding opening token
  ;;         (e.g. if).
  ;;
  ;;
  ;; The following diagram briefly outlines the PDA.  
  ;;
  ;; Common state:
  ;;   "else": Push state, goto state `else'.
  ;;   "while": Push state, goto state `while'.
  ;;   "catch" or "finally": Push state, goto state `catch'.
  ;;   boundary: Pop state.
  ;;   other: Do nothing special.
  ;;
  ;; State `else':
  ;;   boundary: Goto state `else-boundary'.
  ;;   other: Error, pop state, retry token.
  ;;
  ;; State `else-boundary':
  ;;   "if": Pop state.
  ;;   boundary: Error, pop state.
  ;;   other: See common state.
  ;;
  ;; State `while':
  ;;   boundary: Save position, goto state `while-boundary'.
  ;;   other: Pop state, retry token.
  ;;
  ;; State `while-boundary':
  ;;   "do": Pop state.
  ;;   boundary: Restore position if it's not at start, pop state. [*see below]
  ;;   other: See common state.
  ;;
  ;; State `catch':
  ;;   boundary: Goto state `catch-boundary'.
  ;;   other: Error, pop state, retry token.
  ;;
  ;; State `catch-boundary':
  ;;   "try": Pop state.
  ;;   "catch": Goto state `catch'.
  ;;   boundary: Error, pop state.
  ;;   other: See common state.
  ;;
  ;; [*] In the `while-boundary' state, we had pushed a 'while state, and were
  ;; searching for a "do" which would have opened a do-while.  If we didn't
  ;; find it, we discard the analysis done since the "while", go back to this
  ;; token in the buffer and restart the scanning there, this time WITHOUT
  ;; pushing the 'while state onto the stack.
  ;;
  ;; In addition to the above there is some special handling of labels
  ;; and macros.

  (let ((case-fold-search nil)
	(start (point))
	macro-start
	(delims (if comma-delim '(?\; ?,) '(?\;)))
	(c-stmt-delim-chars (if comma-delim
				c-stmt-delim-chars-with-comma
			      c-stmt-delim-chars))
	pos				; Current position.
	boundary-pos      ; Position of last stmt boundary character (e.g. ;).
	after-labels-pos		; Value of tok after first found colon.
	last-label-pos			; Value of tok after last found colon.
	sym         ; Symbol just scanned back over (e.g. 'while or
		    ; 'boundary). See above
	state                     ; Current state in the automaton. See above.
	saved-pos			; Current saved positions. See above
	stack				; Stack of conses (state . saved-pos).
	(cond-key (or c-opt-block-stmt-key ; regexp which matches "for", "if", etc.
		      "\\<\\>"))	; Matches nothing.
	(ret 'same)                     ; Return value.
	tok ptok pptok			; Pos of last three sexps or bounds.
	c-in-literal-cache c-maybe-labelp saved)

    (save-restriction
      (if lim (narrow-to-region lim (point-max)))

      (if (save-excursion
	    (and (c-beginning-of-macro)
		 (/= (point) start)))
	  (setq macro-start (point)))

      ;; Try to skip back over unary operator characters, to register
      ;; that we've moved.
      (while (progn
	       (setq pos (point))
               (if (c-mode-is-new-awk-p)
                   (c-awk-backward-syntactic-ws)
                 (c-backward-syntactic-ws))
	       (/= (skip-chars-backward "-+!*&~@`#") 0))) ; ACM, 2002/5/31;
							  ; Make a variable in
							  ; cc-langs.el, maybe

      ;; Skip back over any semicolon here.  If it was a bare semicolon, we're
      ;; done.  Later on we ignore the boundaries for statements that doesn't
      ;; contain any sexp.  The only thing that is affected is that the error
      ;; checking is a little less strict, and we really don't bother.
      (if (and (memq (char-before) delims)
	       (progn (forward-char -1)
		      (setq saved (point))
		      (if (c-mode-is-new-awk-p)
                          (c-awk-backward-syntactic-ws)
                        (c-backward-syntactic-ws))
		      (or (memq (char-before) delims)
			  (memq (char-before) '(?: nil))
			  (eq (char-syntax (char-before)) ?\()
                          (and (c-mode-is-new-awk-p)
                               (c-awk-after-logical-semicolon))))) ; ACM 2002/6/22
          ;; ACM, 2002/7/20: What about giving a limit to the above function?
          ;; ACM, 2003/6/16: The above two lines (checking for
          ;; awk-logical-semicolon) are probably redundant after rewriting
          ;; c-awk-backward-syntactic-ws.
	  (setq ret 'previous
		pos saved)

	;; Begin at start and not pos to detect macros if we stand
	;; directly after the #.
	(goto-char start)
	(if (looking-at "\\<\\|\\W")
	    ;; Record this as the first token if not starting inside it.
	    (setq tok start))

        ;; The following while loop goes back one sexp (balanced parens,
        ;; etc. with contents, or symbol or suchlike) each iteration.  This
        ;; movement is accomplished with a call to scan-sexps approx 130 lines
        ;; below.
	(while
	    (catch 'loop ;; Throw nil to break, non-nil to continue.
	      (cond
	       ;; Check for macro start.  Take this out for AWK Mode (ACM, 2002/5/31)
               ;; NO!! just make sure macro-start is nil in AWK Mode (ACM, 2002/6/22)
               ;; It always is (ACM, 2002/6/23)
	       ((save-excursion
		  (and macro-start
		       (progn (skip-chars-backward " \t")
			      (eq (char-before) ?#))
		       (progn (setq saved (1- (point)))
			      (beginning-of-line)
			      (not (eq (char-before (1- (point))) ?\\)))
		       (looking-at c-opt-cpp-start)
		       (progn (skip-chars-forward " \t")
			      (eq (point) saved))))
		(goto-char saved)
		(if (and (c-forward-to-cpp-define-body)
			 (progn (c-forward-syntactic-ws start)
				(< (point) start)))
		    ;; Stop at the first token in the content of the macro.
		    (setq pos (point)
			  ignore-labels t) ; Avoid the label check on exit.
		  (setq pos saved
			ret 'macro
			ignore-labels t))
		(throw 'loop nil))

	       ;; Do a round through the automaton if we've just passed a
	       ;; statement boundary or passed a "while"-like token.
	       ((or sym
		    (and (looking-at cond-key)
			 (setq sym (intern (match-string 1)))))

		(when (and (< pos start) (null stack))
		  (throw 'loop nil))

		;; The PDA state handling.
                ;;
                ;; Refer to the description of the PDA in the opening
                ;; comments.  In the following OR form, the first leaf
                ;; attempts to handles one of the specific actions detailed
                ;; (e.g., finding token "if" whilst in state `else-boundary').
                ;; We drop through to the second leaf (which handles common
                ;; state) if no specific handler is found in the first cond.
                ;; If a parsing error is detected (e.g. an "else" with no
                ;; preceding "if"), we throw to the enclosing catch.
                ;;
                ;; Note that the (eq state 'else) means
		;; "we've just passed an else", NOT "we're looking for an
		;; else".
		(or (cond
		     ((eq state 'else)
		      (if (eq sym 'boundary)
			  (setq state 'else-boundary)
			(c-bos-report-error)
			(c-bos-pop-state-and-retry)))

		     ((eq state 'else-boundary)
		      (cond ((eq sym 'if)
			     (c-bos-pop-state (setq ret 'beginning)))
			    ((eq sym 'boundary)
			     (c-bos-report-error)
			     (c-bos-pop-state))))

		     ((eq state 'while)
		      (if (and (eq sym 'boundary)
			       ;; Since this can cause backtracking we do a
			       ;; little more careful analysis to avoid it:
			       ;; If there's a label in front of the while
			       ;; it can't be part of a do-while.
			       (not after-labels-pos))
			  (progn (c-bos-save-pos)
				 (setq state 'while-boundary))
			(c-bos-pop-state-and-retry))) ; Can't be a do-while

		     ((eq state 'while-boundary)
		      (cond ((eq sym 'do)
			     (c-bos-pop-state (setq ret 'beginning)))
			    ((eq sym 'boundary) ; isn't a do-while
			     (c-bos-restore-pos) ; the position of the while
			     (c-bos-pop-state)))) ; no longer searching for do.

		     ((eq state 'catch)
		      (if (eq sym 'boundary)
			  (setq state 'catch-boundary)
			(c-bos-report-error)
			(c-bos-pop-state-and-retry)))

		     ((eq state 'catch-boundary)
		      (cond
		       ((eq sym 'try)
			(c-bos-pop-state (setq ret 'beginning)))
		       ((eq sym 'catch)
			(setq state 'catch))
		       ((eq sym 'boundary)
			(c-bos-report-error)
			(c-bos-pop-state)))))

		    ;; This is state common.  We get here when the previous
		    ;; cond statement found no particular state handler.
		    (cond ((eq sym 'boundary)
			   ;; If we have a boundary at the start
			   ;; position we push a frame to go to the
			   ;; previous statement.
			   (if (>= pos start)
			       (c-bos-push-state)
			     (c-bos-pop-state)))
			  ((eq sym 'else)
			   (c-bos-push-state)
			   (c-bos-save-error-info 'if 'else)
			   (setq state 'else))
			  ((eq sym 'while)
			   (when (or (not pptok)
				     (memq (char-after pptok) delims)
                                     (and (c-mode-is-new-awk-p)
                                          (or
                                        ;; might we be calling this from
                                        ;; c-awk-after-if-do-for-while-condition-p?
                                        ;; If so, avoid infinite recursion.
                                           (and (eq (point) start)
                                                (c-awk-NL-prop-not-set))
                                           ;; The following may recursively
                                           ;; call this function.
                                           (c-awk-completed-stmt-ws-ends-line-p pptok))))
			     ;; Since this can cause backtracking we do a
			     ;; little more careful analysis to avoid it: If
			     ;; the while isn't followed by a semicolon it
			     ;; can't be a do-while.
                             ;; ACM, 2002/5/31;  IT CAN IN AWK Mode. ;-(
			     (c-bos-push-state)
			     (setq state 'while)))
			  ((memq sym '(catch finally))
			   (c-bos-push-state)
			   (c-bos-save-error-info 'try sym)
			   (setq state 'catch))))

		(when c-maybe-labelp
		  ;; We're either past a statement boundary or at the
		  ;; start of a statement, so throw away any label data
		  ;; for the previous one.
		  (setq after-labels-pos nil
			last-label-pos nil
			c-maybe-labelp nil))))

	      ;; Step to the previous sexp, but not if we crossed a
	      ;; boundary, since that doesn't consume an sexp.
	      (if (eq sym 'boundary)
		  (setq ret 'previous)

                ;; HERE IS THE SINGLE PLACE INSIDE THE PDA LOOP WHERE WE MOVE
                ;; BACKWARDS THROUGH THE SOURCE. The following loop goes back
                ;; one sexp and then only loops in special circumstances (line
                ;; continuations and skipping past entire macros).
		(while
		    (progn
		      (or (c-safe (goto-char (scan-sexps (point) -1)) t)
			  ;; Give up if we hit an unbalanced block.
			  ;; Since the stack won't be empty the code
			  ;; below will report a suitable error.
			  (throw 'loop nil))
		      (cond ((looking-at "\\\\$")
			     ;; Step again if we hit a line continuation.
			     t)
			    (macro-start
			     ;; If we started inside a macro then this
			     ;; sexp is always interesting.
			     nil)
			    ((not (c-mode-is-new-awk-p)) ; Changed from t, ACM 2002/6/25
			     ;; Otherwise check that we didn't step
			     ;; into a macro from the end.
			     (let ((macro-start
				    (save-excursion
				      (and (c-beginning-of-macro)
					   (point)))))
			       (when macro-start
				 (goto-char macro-start)
				 t))))))

		;; Did the last movement by a sexp cross a statement boundary?
		(when (save-excursion
			(if (if (eq (char-after) ?{)
				(c-looking-at-inexpr-block lim nil)
			      (looking-at "\\s\("))

			    ;; Should not include the paren sexp we've
			    ;; passed over in the boundary check.
			    (if (> (point) (- pos 100))
				(c-forward-sexp 1)

			      ;; Find its end position this way instead of
			      ;; moving forward if the sexp is large.
			      (goto-char pos)
			      (while
				  (progn
				    (goto-char (1+ (c-down-list-backward)))
				    (unless macro-start
				      ;; Check that we didn't step into
				      ;; a macro from the end.
				      (let ((macro-start
					     (save-excursion
					       (and (c-beginning-of-macro)
						    (point)))))
					(when macro-start
					  (goto-char macro-start)
					  t)))))))

			(setq boundary-pos (c-crosses-statement-barrier-p
					    (point) pos)))

		  (setq pptok ptok
			ptok tok
			tok boundary-pos
			sym 'boundary)
		  (throw 'loop t))) ; like a C "continue".  Analyze the next sexp.

	      (when (and (numberp c-maybe-labelp)
			 (not ignore-labels)
			 (not (looking-at "\\s\(")))
		;; c-crosses-statement-barrier-p has found a colon, so
		;; we might be in a label now.
		(if (not after-labels-pos)
		    (setq after-labels-pos tok))
		(setq last-label-pos tok
		      c-maybe-labelp t))

	      ;; ObjC method def?
	      (when (and c-opt-method-key
			 (setq saved (c-in-method-def-p)))
		(setq pos saved
		      ignore-labels t)	; Avoid the label check on exit.
		(throw 'loop nil))

              ;; We've moved back by a sexp, so update the token positions. 
	      (setq sym nil
		    pptok ptok
		    ptok tok
		    tok (point)
		    pos tok)))		; Not nil (for the while loop).

	;; If the stack isn't empty there might be errors to report.
	(while stack
	  (if (and (vectorp saved-pos) (eq (length saved-pos) 3))
	      (c-bos-report-error))
	  (setq saved-pos (cdr (car stack))
		stack (cdr stack)))

	(when (and (eq ret 'same)
		   (not (memq sym '(boundary ignore nil))))
	  ;; Need to investigate closer whether we've crossed
	  ;; between a substatement and its containing statement.
	  (if (setq saved (if (looking-at c-block-stmt-1-key)
			      ptok
			    pptok))
	      (cond ((> start saved) (setq pos saved))
		    ((= start saved) (setq ret 'up)))))

	(when (and c-maybe-labelp
		   (not ignore-labels)
		   (not (eq ret 'beginning))
		   after-labels-pos)
	  ;; We're in a label.  Maybe we should step to the statement
	  ;; after it.
	  (if (< after-labels-pos start)
	      (setq pos after-labels-pos)
	    (setq ret 'label)
	    (if (< last-label-pos start)
		(setq pos last-label-pos)))))

      ;; Skip over the unary operators that can start the statement.
      (goto-char pos)
      (while (progn
	       (if (c-mode-is-new-awk-p)
                   (c-awk-backward-syntactic-ws)
                 (c-backward-syntactic-ws))
	       (/= (skip-chars-backward "-+!*&~@`#") 0)) ; Hopefully the # won't hurt awk.
	(setq pos (point)))
      (goto-char pos)
      ret)))

(defun c-crosses-statement-barrier-p (from to)
  "Return non-nil if buffer positions FROM to TO cross one or more
statement or declaration boundaries.  The returned value is actually
the position of the earliest boundary char.  FROM must not be within
a string or comment.

The variable `c-maybe-labelp' is set to the position of the first `:' that
might start a label (i.e. not part of `::' and not preceded by `?').  If a
single `?' is found, then `c-maybe-labelp' is cleared."
  (let ((skip-chars c-stmt-delim-chars)
	lit-range)
    (save-excursion
      (catch 'done
	(goto-char from)
	(while (progn (skip-chars-forward skip-chars to)
		      (< (point) to))
	  (if (setq lit-range (c-literal-limits from)) ; Have we landed in a string/comment?
	      (progn (goto-char (setq from (cdr lit-range)))
                     (if (and (c-mode-is-new-awk-p) (bolp)) ; ACM 2002/7/17. Make sure we
                         (backward-char))) ; don't skip over a virtual semi-colon after an awk comment.  :-(
	    (cond ((eq (char-after) ?:)
		   (forward-char)
		   (if (and (eq (char-after) ?:)
			    (< (point) to))
		       ;; Ignore scope operators.
		       (forward-char)
		     (setq c-maybe-labelp (1- (point)))))
		  ((eq (char-after) ??)
		   ;; A question mark.  Can't be a label, so stop
		   ;; looking for more : and ?.
		   (setq c-maybe-labelp nil
			 skip-chars (substring c-stmt-delim-chars 0 -2)))
                  ((and (eolp)  ; Can only happen in AWK Mode
                        (not (c-awk-completed-stmt-ws-ends-line-p)))
                   (forward-char))
                  ((and (c-mode-is-new-awk-p)
                        (bolp) lit-range ; awk: comment/string ended prev line.
                        (not (c-awk-completed-stmt-ws-ends-prev-line-p))))
		  (t (throw 'done (point))))))
	nil))))


;; A set of functions that covers various idiosyncrasies in
;; implementations of `forward-comment'.

;; Note: Some emacsen considers incorrectly that any line comment
;; ending with a backslash continues to the next line.  I can't think
;; of any way to work around that in a reliable way without changing
;; the buffer, though.  Suggestions welcome. ;) (No, temporarily
;; changing the syntax for backslash doesn't work since we must treat
;; escapes in string literals correctly.)

(defun c-forward-single-comment ()
  "Move forward past whitespace and the closest following comment, if any.
Return t if a comment was found, nil otherwise.  In either case, the
point is moved past the following whitespace.  Line continuations,
i.e. a backslashes followed by line breaks, are treated as whitespace.
The line breaks that end line comments are considered to be the
comment enders, so the point will be put on the beginning of the next
line if it moved past a line comment.

This function does not do any hidden buffer changes."

  (let ((start (point)))
    (when (looking-at "\\([ \t\n\r\f\v]\\|\\\\[\n\r]\\)+")
      (goto-char (match-end 0)))

    (when (forward-comment 1)
      (if (eobp)
	  ;; Some emacsen (e.g. XEmacs 21) return t when moving
	  ;; forwards at eob.
	  nil

	;; Emacs includes the ending newline in a b-style (c++)
	;; comment, but XEmacs doesn't.  We depend on the Emacs
	;; behavior (which also is symmetric).
	(if (and (eolp) (elt (parse-partial-sexp start (point)) 7))
	    (condition-case nil (forward-char 1)))

	t))))

(defsubst c-forward-comments ()
  "Move forward past all following whitespace and comments.
Line continuations, i.e. a backslashes followed by line breaks, are
treated as whitespace.

This function does not do any hidden buffer changes."

  (while (or
	  ;; If forward-comment in at least XEmacs 21 is given a large
	  ;; positive value, it'll loop all the way through if it hits
	  ;; eob.
	  (and (forward-comment 5)
	       ;; Some emacsen (e.g. XEmacs 21) return t when moving
	       ;; forwards at eob.
	       (not (eobp)))

	  (when (looking-at "\\\\[\n\r]")
	    (forward-char 2)
	    t))))

(defun c-backward-single-comment ()
  "Move backward past whitespace and the closest preceding comment, if any.
Return t if a comment was found, nil otherwise.  In either case, the
point is moved past the preceding whitespace.  Line continuations,
i.e. a backslashes followed by line breaks, are treated as whitespace.
The line breaks that end line comments are considered to be the
comment enders, so the point cannot be at the end of the same line to
move over a line comment.

This function does not do any hidden buffer changes."

  (let ((start (point)))
    ;; When we got newline terminated comments, forward-comment in all
    ;; supported emacsen so far will stop at eol of each line not
    ;; ending with a comment when moving backwards.  This corrects for
    ;; that, and at the same time handles line continuations.
    (while (progn
	     (skip-chars-backward " \t\n\r\f\v")
	     (and (looking-at "[\n\r]")
		  (eq (char-before) ?\\)
		  (< (point) start)))
      (backward-char))

    (if (bobp)
	;; Some emacsen (e.g. Emacs 19.34) return t when moving
	;; backwards at bob.
	nil

      ;; Leave point after the closest following newline if we've
      ;; backed up over any above, since forward-comment won't move
      ;; backward over a line comment if point is at the end of the
      ;; same line.
      (re-search-forward "\\=\\s *[\n\r]" start t)

      (if (if (forward-comment -1)
	      (if (eolp)
		  ;; If forward-comment above succeeded and we're at eol
		  ;; then the newline we moved over above didn't end a
		  ;; line comment, so we give it another go.
		  (forward-comment -1)
		t))

	  ;; Emacs <= 20 and XEmacs move back over the closer of a
	  ;; block comment that lacks an opener.
	  (if (looking-at "\\*/")
	      (progn (forward-char 2) nil)
	    t)))))

(defsubst c-backward-comments ()
  "Move backward past all preceding whitespace and comments.
Line continuations, i.e. a backslashes followed by line breaks, are
treated as whitespace.  The line breaks that end line comments are
considered to be the comment enders, so the point cannot be at the end
of the same line to move over a line comment.

This function does not do any hidden buffer changes."

  (let ((start (point)))
    (while (and
	    ;; `forward-comment' in some emacsen (e.g. Emacs 19.34)
	    ;; return t when moving backwards at bob.
	    (not (bobp))

	    (if (forward-comment -1)
		(if (looking-at "\\*/")
		    ;; Emacs <= 20 and XEmacs move back over the
		    ;; closer of a block comment that lacks an opener.
		    (progn (forward-char 2) nil)
		  t)

	      ;; XEmacs treats line continuations as whitespace but
	      ;; only in the backward direction, which seems a bit
	      ;; odd.  Anyway, this is necessary for Emacs.
	      (when (and (looking-at "[\n\r]")
			 (eq (char-before) ?\\)
			 (< (point) start))
		(backward-char)
		t))))))


;; Tools for skipping over syntactic whitespace.

;; The following functions use text properties to cache searches over
;; large regions of syntactic whitespace.  It works as follows:
;;
;; o  If a syntactic whitespace region contains anything but simple
;;    whitespace (i.e. space, tab and line breaks), the text property
;;    `c-in-sws' is put over it.  At places where we have stopped
;;    within that region there's also a `c-is-sws' text property.
;;    That since there typically are nested whitespace inside that
;;    must be handled separately, e.g. whitespace inside a comment or
;;    cpp directive.  Thus, from one point with `c-is-sws' it's safe
;;    to jump to another point with that property within the same
;;    `c-in-sws' region.  It can be likened to a ladder where
;;    `c-in-sws' marks the bars and `c-is-sws' the rungs.
;;
;; o  The `c-is-sws' property is put on the simple whitespace chars at
;;    a "rung position" and also maybe on the first following char.
;;    As many characters as can be conveniently found in this range
;;    are marked, but no assumption can be made that the whole range
;;    is marked (it could be clobbered by later changes, for
;;    instance).
;;
;;    Note that some part of the beginning of a sequence of simple
;;    whitespace might be part of the end of a preceding line comment
;;    or cpp directive and must not be considered part of the "rung".
;;    Such whitespace is some amount of horizontal whitespace followed
;;    by a newline.  In the case of cpp directives it could also be
;;    two newlines with horizontal whitespace between them.
;;
;;    The reason to include the first following char is to cope with
;;    "rung positions" that doesn't have any ordinary whitespace.  If
;;    `c-is-sws' is put on a token character it does not have
;;    `c-in-sws' set simultaneously.  That's the only case when that
;;    can occur, and the reason for not extending the `c-in-sws'
;;    region to cover it is that the `c-in-sws' region could then be
;;    accidentally merged with a following one if the token is only
;;    one character long.
;;
;; o  On buffer changes the `c-in-sws' and `c-is-sws' properties are
;;    removed in the changed region.  If the change was inside
;;    syntactic whitespace that means that the "ladder" is broken, but
;;    a later call to `c-forward-sws' or `c-backward-sws' will use the
;;    parts on either side and use an ordinary search only to "repair"
;;    the gap.
;;
;;    Special care needs to be taken if a region is removed: If there
;;    are `c-in-sws' on both sides of it which do not connect inside
;;    the region then they can't be joined.  If e.g. a marked macro is
;;    broken, syntactic whitespace inside the new text might be
;;    marked.  If those marks would become connected with the old
;;    `c-in-sws' range around the macro then we could get a ladder
;;    with one end outside the macro and the other at some whitespace
;;    within it.
;;
;; The main motivation for this system is to increase the speed in
;; skipping over the large whitespace regions that can occur at the
;; top level in e.g. header files that contain a lot of comments and
;; cpp directives.  For small comments inside code it's probably
;; slower than using `forward-comment' straightforwardly, but speed is
;; not a significant factor there anyway.

; (defface c-debug-is-sws-face
;   '((t (:background "GreenYellow")))
;   "Debug face to mark the `c-is-sws' property.")
; (defface c-debug-in-sws-face
;   '((t (:underline t)))
;   "Debug face to mark the `c-in-sws' property.")

; (defun c-debug-put-sws-faces ()
;   ;; Put the sws debug faces on all the `c-is-sws' and `c-in-sws'
;   ;; properties in the buffer.
;   (interactive)
;   (save-excursion
;     (let (in-face)
;       (goto-char (point-min))
;       (setq in-face (if (get-text-property (point) 'c-is-sws)
; 			(point)))
;       (while (progn
; 	       (goto-char (next-single-property-change
; 			   (point) 'c-is-sws nil (point-max)))
; 	       (if in-face
; 		   (progn
; 		     (c-debug-add-face in-face (point) 'c-debug-is-sws-face)
; 		     (setq in-face nil))
; 		 (setq in-face (point)))
; 	       (not (eobp))))
;       (goto-char (point-min))
;       (setq in-face (if (get-text-property (point) 'c-in-sws)
; 			(point)))
;       (while (progn
; 	       (goto-char (next-single-property-change
; 			   (point) 'c-in-sws nil (point-max)))
; 	       (if in-face
; 		   (progn
; 		     (c-debug-add-face in-face (point) 'c-debug-in-sws-face)
; 		     (setq in-face nil))
; 		 (setq in-face (point)))
; 	       (not (eobp)))))))

(defmacro c-debug-sws-msg (&rest args)
  ;;`(message ,@args)
  )

(defmacro c-put-is-sws (beg end)
  `(let ((beg ,beg) (end ,end))
     (put-text-property beg end 'c-is-sws t)
     ,@(when (facep 'c-debug-is-sws-face)
	 `((c-debug-add-face beg end 'c-debug-is-sws-face)))))

(defmacro c-put-in-sws (beg end)
  `(let ((beg ,beg) (end ,end))
     (put-text-property beg end 'c-in-sws t)
     ,@(when (facep 'c-debug-is-sws-face)
	 `((c-debug-add-face beg end 'c-debug-in-sws-face)))))

(defmacro c-remove-is-sws (beg end)
  `(let ((beg ,beg) (end ,end))
     (remove-text-properties beg end '(c-is-sws nil))
     ,@(when (facep 'c-debug-is-sws-face)
	 `((c-debug-remove-face beg end 'c-debug-is-sws-face)))))

(defmacro c-remove-in-sws (beg end)
  `(let ((beg ,beg) (end ,end))
     (remove-text-properties beg end '(c-in-sws nil))
     ,@(when (facep 'c-debug-is-sws-face)
	 `((c-debug-remove-face beg end 'c-debug-in-sws-face)))))

(defmacro c-remove-is-and-in-sws (beg end)
  `(let ((beg ,beg) (end ,end))
     (remove-text-properties beg end '(c-is-sws nil c-in-sws nil))
     ,@(when (facep 'c-debug-is-sws-face)
	 `((c-debug-remove-face beg end 'c-debug-is-sws-face)
	   (c-debug-remove-face beg end 'c-debug-in-sws-face)))))

(defsubst c-invalidate-sws-region-after (beg end)
  ;; Called from `after-change-functions'.  Note that if
  ;; `c-forward-sws' or `c-backward-sws' are used outside
  ;; `c-save-buffer-state' or similar then this will remove the cache
  ;; properties right after they're added.

  (save-excursion
    ;; Adjust the end to remove the properties in any following simple
    ;; ws up to and including the next line break, if there is any
    ;; after the changed region. This is necessary e.g. when a rung
    ;; marked empty line is converted to a line comment by inserting
    ;; "//" before the line break. In that case the line break would
    ;; keep the rung mark which could make a later `c-backward-sws'
    ;; move into the line comment instead of over it.
    (goto-char end)
    (skip-chars-forward " \t\f\v")
    (when (and (eolp) (not (eobp)))
      (setq end (1+ (point)))))

  (when (and (= beg end)
	     (get-text-property beg 'c-in-sws)
	     (> beg (point-min))
	     (get-text-property (1- beg) 'c-in-sws))
    ;; Ensure that an `c-in-sws' range gets broken.  Note that it isn't
    ;; safe to keep a range that was continuous before the change.  E.g:
    ;;
    ;;    #define foo
    ;;         \
    ;;    bar
    ;;
    ;; There can be a "ladder" between "#" and "b".  Now, if the newline
    ;; after "foo" is removed then "bar" will become part of the cpp
    ;; directive instead of a syntactically relevant token.  In that
    ;; case there's no longer syntactic ws from "#" to "b".
    (setq beg (1- beg)))

  (c-debug-sws-msg "c-invalidate-sws-region-after [%s..%s]" beg end)
  (c-remove-is-and-in-sws beg end))

(defun c-forward-sws ()
  ;; Used by `c-forward-syntactic-ws' to implement the unbounded search.

  (let (;; `rung-pos' is set to a position as early as possible in the
	;; unmarked part of the simple ws region.
	(rung-pos (point)) next-rung-pos rung-end-pos last-put-in-sws-pos
	rung-is-marked next-rung-is-marked simple-ws-end
	;; `safe-start' is set when it's safe to cache the start position.
	;; It's not set if we've initially skipped over comments and line
	;; continuations since we might have gone out through the end of a
	;; macro then.  This provision makes `c-forward-sws' not populate the
	;; cache in the majority of cases, but otoh is `c-backward-sws' by far
	;; more common.
	safe-start)

    ;; Skip simple ws and do a quick check on the following character to see
    ;; if it's anything that can't start syntactic ws, so we can bail out
    ;; early in the majority of cases when there just are a few ws chars.
    (skip-chars-forward " \t\n\r\f\v")
    (when (looking-at c-syntactic-ws-start)

      (setq rung-end-pos (min (1+ (point)) (point-max)))
      (if (setq rung-is-marked (text-property-any rung-pos rung-end-pos
						  'c-is-sws t))
	  ;; Find the last rung position to avoid setting properties in all
	  ;; the cases when the marked rung is complete.
	  ;; (`next-single-property-change' is certain to move at least one
	  ;; step forward.)
	  (setq rung-pos (1- (next-single-property-change
			      rung-is-marked 'c-is-sws nil rung-end-pos)))
	;; Got no marked rung here.  Since the simple ws might have started
	;; inside a line comment or cpp directive we must set `rung-pos' as
	;; high as possible.
	(setq rung-pos (point)))

      (while
	  (progn
	    (while
		(when (and rung-is-marked
			   (get-text-property (point) 'c-in-sws))

		  ;; The following search is the main reason that `c-in-sws'
		  ;; and `c-is-sws' aren't combined to one property.
		  (goto-char (next-single-property-change
			      (point) 'c-in-sws nil (point-max)))
		  (unless (get-text-property (point) 'c-is-sws)
		    ;; If the `c-in-sws' region extended past the last
		    ;; `c-is-sws' char we have to go back a bit.
		    (or (get-text-property (1- (point)) 'c-is-sws)
			(goto-char (previous-single-property-change
				    (point) 'c-is-sws)))
		    (backward-char))

		  (c-debug-sws-msg
		   "c-forward-sws cached move %s -> %s (max %s)"
		   rung-pos (point) (point-max))

		  (setq rung-pos (point))
		  (and (> (skip-chars-forward " \t\n\r\f\v") 0)
		       (not (eobp))))

	      ;; We'll loop here if there is simple ws after the last rung.
	      ;; That means that there's been some change in it and it's
	      ;; possible that we've stepped into another ladder, so extend
	      ;; the previous one to join with it if there is one, and try to
	      ;; use the cache again.
	      (c-debug-sws-msg
	       "c-forward-sws extending rung with [%s..%s] (max %s)"
	       (1+ rung-pos) (1+ (point)) (point-max))
	      (unless (get-text-property (point) 'c-is-sws)
		;; Remove any `c-in-sws' property from the last char of
		;; the rung before we mark it with `c-is-sws', so that we
		;; won't connect with the remains of a broken "ladder".
		(c-remove-in-sws (point) (1+ (point))))
	      (c-put-is-sws (1+ rung-pos)
			    (1+ (point)))
	      (c-put-in-sws rung-pos
			    (setq rung-pos (point)
				  last-put-in-sws-pos rung-pos)))

	    (setq simple-ws-end (point))
	    (c-forward-comments)

	    (cond
	     ((/= (point) simple-ws-end)
	      ;; Skipped over comments.  Don't cache at eob in case the buffer
	      ;; is narrowed.
	      (not (eobp)))

	     ((save-excursion
		(and c-opt-cpp-prefix
		     (looking-at c-opt-cpp-start)
		     (progn (skip-chars-backward " \t")
			    (bolp))
		     (or (bobp)
			 (progn (backward-char)
				(not (eq (char-before) ?\\))))))
	      ;; Skip a preprocessor directive.
	      (end-of-line)
	      (while (and (eq (char-before) ?\\)
			  (= (forward-line 1) 0))
		(end-of-line))
	      (forward-line 1)
	      (setq safe-start t)
	      ;; Don't cache at eob in case the buffer is narrowed.
	      (not (eobp)))))

	;; We've searched over a piece of non-white syntactic ws.  See if this
	;; can be cached.
	(setq next-rung-pos (point))
	(skip-chars-forward " \t\n\r\f\v")
	(setq rung-end-pos (min (1+ (point)) (point-max)))

	(if (or
	     ;; Cache if we haven't skipped comments only, and if we started
	     ;; either from a marked rung or from a completely uncached
	     ;; position.
	     (and safe-start
		  (or rung-is-marked
		      (not (get-text-property simple-ws-end 'c-in-sws))))

	     ;; See if there's a marked rung in the encountered simple ws.  If
	     ;; so then we can cache, unless `safe-start' is nil.  Even then
	     ;; we need to do this to check if the cache can be used for the
	     ;; next step.
	     (and (setq next-rung-is-marked
			(text-property-any next-rung-pos rung-end-pos
					   'c-is-sws t))
		  safe-start))

	    (progn
	      (c-debug-sws-msg
	       "c-forward-sws caching [%s..%s] - [%s..%s] (max %s)"
	       rung-pos (1+ simple-ws-end) next-rung-pos rung-end-pos
	       (point-max))

	      ;; Remove the properties for any nested ws that might be cached.
	      ;; Only necessary for `c-is-sws' since `c-in-sws' will be set
	      ;; anyway.
	      (c-remove-is-sws (1+ simple-ws-end) next-rung-pos)
	      (unless (and rung-is-marked (= rung-pos simple-ws-end))
		(c-put-is-sws rung-pos
			      (1+ simple-ws-end))
		(setq rung-is-marked t))
	      (c-put-in-sws rung-pos
			    (setq rung-pos (point)
				  last-put-in-sws-pos rung-pos))
	      (unless (get-text-property (1- rung-end-pos) 'c-is-sws)
		;; Remove any `c-in-sws' property from the last char of
		;; the rung before we mark it with `c-is-sws', so that we
		;; won't connect with the remains of a broken "ladder".
		(c-remove-in-sws (1- rung-end-pos) rung-end-pos))
	      (c-put-is-sws next-rung-pos
			    rung-end-pos))

	  (c-debug-sws-msg
	   "c-forward-sws not caching [%s..%s] - [%s..%s] (max %s)"
	   rung-pos (1+ simple-ws-end) next-rung-pos rung-end-pos
	   (point-max))

	  ;; Set `rung-pos' for the next rung.  It's the same thing here as
	  ;; initially, except that the rung position is set as early as
	  ;; possible since we can't be in the ending ws of a line comment or
	  ;; cpp directive now.
	  (if (setq rung-is-marked next-rung-is-marked)
	      (setq rung-pos (1- (next-single-property-change
				  rung-is-marked 'c-is-sws nil rung-end-pos)))
	    (setq rung-pos next-rung-pos))
	  (setq safe-start t)))

      ;; Make sure that the newly marked `c-in-sws' region doesn't connect to
      ;; another one after the point (which might occur when editing inside a
      ;; comment or macro).
      (when (eq last-put-in-sws-pos (point))
	(cond ((< last-put-in-sws-pos (point-max))
	       (c-debug-sws-msg
		"c-forward-sws clearing at %s for cache separation"
		last-put-in-sws-pos)
	       (c-remove-in-sws last-put-in-sws-pos
				(1+ last-put-in-sws-pos)))
	      (t
	       ;; If at eob we have to clear the last character before the end
	       ;; instead since the buffer might be narrowed and there might
	       ;; be a `c-in-sws' after (point-max).  In this case it's
	       ;; necessary to clear both properties.
	       (c-debug-sws-msg
		"c-forward-sws clearing thoroughly at %s for cache separation"
		(1- last-put-in-sws-pos))
	       (c-remove-is-and-in-sws (1- last-put-in-sws-pos)
				       last-put-in-sws-pos))))
      )))

(defun c-backward-sws ()
  ;; Used by `c-backward-syntactic-ws' to implement the unbounded search.

  (let (;; `rung-pos' is set to a position as late as possible in the unmarked
	;; part of the simple ws region.
	(rung-pos (point)) next-rung-pos last-put-in-sws-pos
	rung-is-marked simple-ws-beg cmt-skip-pos)

    ;; Skip simple horizontal ws and do a quick check on the preceding
    ;; character to see if it's anying that can't end syntactic ws, so we can
    ;; bail out early in the majority of cases when there just are a few ws
    ;; chars.  Newlines are complicated in the backward direction, so we can't
    ;; skip over them.
    (skip-chars-backward " \t\f")
    (when (and (not (bobp))
	       (save-excursion
		 (backward-char)
		 (looking-at c-syntactic-ws-end)))

      ;; Try to find a rung position in the simple ws preceding point, so that
      ;; we can get a cache hit even if the last bit of the simple ws has
      ;; changed recently.
      (setq simple-ws-beg (point))
      (skip-chars-backward " \t\n\r\f\v")
      (if (setq rung-is-marked (text-property-any
				(point) (min (1+ rung-pos) (point-max))
				'c-is-sws t))
	  ;; `rung-pos' will be the earliest marked position, which means that
	  ;; there might be later unmarked parts in the simple ws region.
	  ;; It's not worth the effort to fix that; the last part of the
	  ;; simple ws is also typically edited often, so it could be wasted.
	  (goto-char (setq rung-pos rung-is-marked))
	(goto-char simple-ws-beg))

      (while
	  (progn
	    (while
		(when (and rung-is-marked
			   (not (bobp))
			   (get-text-property (1- (point)) 'c-in-sws))

		  ;; The following search is the main reason that `c-in-sws'
		  ;; and `c-is-sws' aren't combined to one property.
		  (goto-char (previous-single-property-change
			      (point) 'c-in-sws nil (point-min)))
		  (unless (get-text-property (point) 'c-is-sws)
		    ;; If the `c-in-sws' region extended past the first
		    ;; `c-is-sws' char we have to go forward a bit.
		    (goto-char (next-single-property-change
				(point) 'c-is-sws)))

		  (c-debug-sws-msg
		   "c-backward-sws cached move %s <- %s (min %s)"
		   (point) rung-pos (point-min))

		  (setq rung-pos (point))
		  (if (and (< (min (skip-chars-backward " \t\f\v")
				   (progn
				     (setq simple-ws-beg (point))
				     (skip-chars-backward " \t\n\r\f\v")))
			      0)
			   (setq rung-is-marked
				 (text-property-any (point) rung-pos
						    'c-is-sws t)))
		      t
		    (goto-char simple-ws-beg)
		    nil))

	      ;; We'll loop here if there is simple ws before the first rung.
	      ;; That means that there's been some change in it and it's
	      ;; possible that we've stepped into another ladder, so extend
	      ;; the previous one to join with it if there is one, and try to
	      ;; use the cache again.
	      (c-debug-sws-msg
	       "c-backward-sws extending rung with [%s..%s] (min %s)"
	       rung-is-marked rung-pos (point-min))
	      (unless (get-text-property (1- rung-pos) 'c-is-sws)
		;; Remove any `c-in-sws' property from the last char of
		;; the rung before we mark it with `c-is-sws', so that we
		;; won't connect with the remains of a broken "ladder".
		(c-remove-in-sws (1- rung-pos) rung-pos))
	      (c-put-is-sws rung-is-marked
			    rung-pos)
	      (c-put-in-sws rung-is-marked
			    (1- rung-pos))
	      (setq rung-pos rung-is-marked
		    last-put-in-sws-pos rung-pos))

	    (c-backward-comments)
	    (setq cmt-skip-pos (point))

	    (cond
	     ((and c-opt-cpp-prefix
		   (/= cmt-skip-pos simple-ws-beg)
		   (c-beginning-of-macro))
	      ;; Inside a cpp directive.  See if it should be skipped over.
	      (let ((cpp-beg (point)))

		;; Move back over all line continuations in the region skipped
		;; over by `c-backward-comments'.  If we go past it then we
		;; started inside the cpp directive.
		(goto-char simple-ws-beg)
		(beginning-of-line)
		(while (and (> (point) cmt-skip-pos)
			    (progn (backward-char)
				   (eq (char-before) ?\\)))
		  (beginning-of-line))

		(if (< (point) cmt-skip-pos)
		    ;; Don't move past the cpp directive if we began inside
		    ;; it.  Note that the position at the end of the last line
		    ;; of the macro is also considered to be within it.
		    (progn (goto-char cmt-skip-pos)
			   nil)

		  ;; It's worthwhile to spend a little bit of effort on finding
		  ;; the end of the macro, to get a good `simple-ws-beg'
		  ;; position for the cache.  Note that `c-backward-comments'
		  ;; could have stepped over some comments before going into
		  ;; the macro, and then `simple-ws-beg' must be kept on the
		  ;; same side of those comments.
		  (goto-char simple-ws-beg)
		  (skip-chars-backward " \t\n\r\f\v")
		  (if (eq (char-before) ?\\)
		      (forward-char))
		  (forward-line 1)
		  (if (< (point) simple-ws-beg)
		      ;; Might happen if comments after the macro were skipped
		      ;; over.
		      (setq simple-ws-beg (point)))

		  (goto-char cpp-beg)
		  t)))

	     ((/= (save-excursion
		    (skip-chars-forward " \t\n\r\f\v" simple-ws-beg)
		    (setq next-rung-pos (point)))
		  simple-ws-beg)
	      ;; Skipped over comments.  Must put point at the end of
	      ;; the simple ws at point since we might be after a line
	      ;; comment or cpp directive that's been partially
	      ;; narrowed out, and we can't risk marking the simple ws
	      ;; at the end of it.
	      (goto-char next-rung-pos)
	      t)))

	;; We've searched over a piece of non-white syntactic ws.  See if this
	;; can be cached.
	(setq next-rung-pos (point))
	(skip-chars-backward " \t\f\v")

	(if (or
	     ;; Cache if we started either from a marked rung or from a
	     ;; completely uncached position.
	     rung-is-marked
	     (not (get-text-property (1- simple-ws-beg) 'c-in-sws))

	     ;; Cache if there's a marked rung in the encountered simple ws.
	     (save-excursion
	       (skip-chars-backward " \t\n\r\f\v")
	       (text-property-any (point) (min (1+ next-rung-pos) (point-max))
				  'c-is-sws t)))

	    (progn
	      (c-debug-sws-msg
	       "c-backward-sws caching [%s..%s] - [%s..%s] (min %s)"
	       (point) (1+ next-rung-pos)
	       simple-ws-beg (min (1+ rung-pos) (point-max))
	       (point-min))

	      ;; Remove the properties for any nested ws that might be cached.
	      ;; Only necessary for `c-is-sws' since `c-in-sws' will be set
	      ;; anyway.
	      (c-remove-is-sws (1+ next-rung-pos) simple-ws-beg)
	      (unless (and rung-is-marked (= simple-ws-beg rung-pos))
		(let ((rung-end-pos (min (1+ rung-pos) (point-max))))
		  (unless (get-text-property (1- rung-end-pos) 'c-is-sws)
		    ;; Remove any `c-in-sws' property from the last char of
		    ;; the rung before we mark it with `c-is-sws', so that we
		    ;; won't connect with the remains of a broken "ladder".
		    (c-remove-in-sws (1- rung-end-pos) rung-end-pos))
		  (c-put-is-sws simple-ws-beg
				rung-end-pos)
		  (setq rung-is-marked t)))
	      (c-put-in-sws (setq simple-ws-beg (point)
				  last-put-in-sws-pos simple-ws-beg)
			    rung-pos)
	      (c-put-is-sws (setq rung-pos simple-ws-beg)
			    (1+ next-rung-pos)))

	  (c-debug-sws-msg
	   "c-backward-sws not caching [%s..%s] - [%s..%s] (min %s)"
	   (point) (1+ next-rung-pos)
	   simple-ws-beg (min (1+ rung-pos) (point-max))
	   (point-min))
	  (setq rung-pos next-rung-pos
		simple-ws-beg (point))
	  ))

      ;; Make sure that the newly marked `c-in-sws' region doesn't connect to
      ;; another one before the point (which might occur when editing inside a
      ;; comment or macro).
      (when (eq last-put-in-sws-pos (point))
	(cond ((< (point-min) last-put-in-sws-pos)
	       (c-debug-sws-msg
		"c-backward-sws clearing at %s for cache separation"
		(1- last-put-in-sws-pos))
	       (c-remove-in-sws (1- last-put-in-sws-pos)
				last-put-in-sws-pos))
	      ((> (point-min) 1)
	       ;; If at bob and the buffer is narrowed, we have to clear the
	       ;; character we're standing on instead since there might be a
	       ;; `c-in-sws' before (point-min).  In this case it's necessary
	       ;; to clear both properties.
	       (c-debug-sws-msg
		"c-backward-sws clearing thoroughly at %s for cache separation"
		last-put-in-sws-pos)
	       (c-remove-is-and-in-sws last-put-in-sws-pos
				       (1+ last-put-in-sws-pos)))))
      )))


;; A system for handling noteworthy parens before the point.

(defvar c-state-cache nil)
(make-variable-buffer-local 'c-state-cache)
;; The state cache used by `c-parse-state' to cut down the amount of
;; searching.  It's the result from some earlier `c-parse-state' call.
;; The use of the cached info is more effective if the next
;; `c-parse-state' call is on a line close by the one the cached state
;; was made at; the cache can actually slow down a little if the
;; cached state was made very far back in the buffer.  The cache is
;; most effective if `c-parse-state' is used on each line while moving
;; forward.

(defvar c-state-cache-start 1)
(make-variable-buffer-local 'c-state-cache-start)
;; This is (point-min) when `c-state-cache' was calculated, since a
;; change of narrowing is likely to affect the parens that are visible
;; before the point.

(defsubst c-invalidate-state-cache (pos)
  ;; Invalidate all info on `c-state-cache' that applies to the buffer
  ;; at POS or higher.  This is much like `c-whack-state-after', but
  ;; it never changes a paren pair element into an open paren element.
  ;; Doing that would mean that the new open paren wouldn't have the
  ;; required preceding paren pair element.
  ;;
  ;; This function does not do any hidden buffer changes.
  (while (and c-state-cache
	      (let ((elem (car c-state-cache)))
		(if (consp elem)
		    (or (<= pos (car elem))
			(< pos (cdr elem)))
		  (<= pos elem))))
    (setq c-state-cache (cdr c-state-cache))))

(defun c-parse-state ()
  ;; Finds and records all noteworthy parens between some good point
  ;; earlier in the file and point.  That good point is at least the
  ;; beginning of the top-level construct we are in, or the beginning
  ;; of the preceding top-level construct if we aren't in one.
  ;;
  ;; The returned value is a list of the noteworthy parens with the
  ;; last one first.  If an element in the list is an integer, it's
  ;; the position of an open paren which has not been closed before
  ;; the point.  If an element is a cons, it gives the position of a
  ;; closed brace paren pair; the car is the start paren position and
  ;; the cdr is the position following the closing paren.  Only the
  ;; last closed brace paren pair before each open paren is recorded,
  ;; and thus the state never contains two cons elements in
  ;; succession.
  ;;
  ;; Currently no characters which are given paren syntax with the
  ;; syntax-table property are recorded, i.e. angle bracket arglist
  ;; parens are never present here.  Note that this might change.
  ;;
  ;; This function does not do any hidden buffer changes.

  (save-restriction
    (let* ((here (point))
	   (c-macro-start (c-query-macro-start))
	   (in-macro-start (or c-macro-start (point)))
	   old-state last-pos pairs pos save-pos)
      (c-invalidate-state-cache (point))

      ;; If the minimum position has changed due to narrowing then we
      ;; have to fix the tail of `c-state-cache' accordingly.
      (unless (= c-state-cache-start (point-min))
	(if (> (point-min) c-state-cache-start)
	    ;; If point-min has moved forward then we just need to cut
	    ;; off a bit of the tail.
	    (let ((ptr (cons nil c-state-cache)) elem)
	      (while (and (setq elem (car-safe (cdr ptr)))
			  (>= (if (consp elem) (car elem) elem)
			      (point-min)))
		(setq ptr (cdr ptr)))
	      (when (consp ptr)
		(if (eq (cdr ptr) c-state-cache)
		    (setq c-state-cache nil)
		  (setcdr ptr nil))))
	  ;; If point-min has moved backward then we drop the state
	  ;; completely.  It's possible to do a better job here and
	  ;; recalculate the top only.
	  (setq c-state-cache nil))
	(setq c-state-cache-start (point-min)))

      ;; Get the latest position we know are directly inside the
      ;; closest containing paren of the cached state.
      (setq last-pos (and c-state-cache
			  (if (consp (car c-state-cache))
			      (cdr (car c-state-cache))
			    (1+ (car c-state-cache)))))

      ;; Check if the found last-pos is in a macro.  If it is, and
      ;; we're not in the same macro, we must discard everything on
      ;; c-state-cache that is inside the macro before using it.
      (when last-pos
	(save-excursion
	  (goto-char last-pos)
	  (when (and (c-beginning-of-macro)
		     (/= (point) in-macro-start))
	    (c-invalidate-state-cache (point))
	    ;; Set last-pos again, just like above.
	    (setq last-pos (and c-state-cache
				(if (consp (car c-state-cache))
				    (cdr (car c-state-cache))
				  (1+ (car c-state-cache))))))))

      (setq pos
	    ;; Find the start position for the forward search.  (Can't
	    ;; search in the backward direction since point might be
	    ;; in some kind of literal.)
	    (or (when last-pos

		  ;; There's a cached state with a containing paren.  Pop
		  ;; off the stale containing sexps from it by going
		  ;; forward out of parens as far as possible.
		  (narrow-to-region (point-min) here)
		  (let (placeholder pair-beg)
		    (while (and c-state-cache
				(setq placeholder
				      (c-up-list-forward last-pos)))
		      (setq last-pos placeholder)
		      (if (consp (car c-state-cache))
			  (setq pair-beg (car-safe (cdr c-state-cache))
				c-state-cache (cdr-safe (cdr c-state-cache)))
			(setq pair-beg (car c-state-cache)
			      c-state-cache (cdr c-state-cache))))

		    (when (and pair-beg (eq (char-after pair-beg) ?{))
		      ;; The last paren pair we moved out from was a brace
		      ;; pair.  Modify the state to record this as a closed
		      ;; pair now.
		      (if (consp (car-safe c-state-cache))
			  (setq c-state-cache (cdr c-state-cache)))
		      (setq c-state-cache (cons (cons pair-beg last-pos)
						c-state-cache))))

		  ;; Check if the preceding balanced paren is within a
		  ;; macro; it should be ignored if we're outside the
		  ;; macro.  There's no need to check any further upwards;
		  ;; if the macro contains an unbalanced opening paren then
		  ;; we're smoked anyway.
		  (when (and (<= (point) in-macro-start)
			     (consp (car c-state-cache)))
		    (save-excursion
		      (goto-char (car (car c-state-cache)))
		      (when (c-beginning-of-macro)
			(setq here (point)
			      c-state-cache (cdr c-state-cache)))))

		  (when c-state-cache
		    (setq old-state c-state-cache)
		    last-pos))

		(save-excursion
		  ;; go back 2 bods, but ignore any bogus positions
		  ;; returned by beginning-of-defun (i.e. open paren in
		  ;; column zero)
		  (goto-char here)
		  (let ((cnt 2))
		    (while (not (or (bobp) (zerop cnt)))
		      (c-beginning-of-defun-1)
		      (if (eq (char-after) ?\{)
			  (setq cnt (1- cnt)))))
		  (point))))

      (narrow-to-region (point-min) here)

      (while pos
	;; Find the balanced brace pairs.
	(setq save-pos pos
	      pairs nil)
	(while (and (setq last-pos (c-down-list-forward pos))
		    (setq pos (c-up-list-forward last-pos)))
	  (if (eq (char-before last-pos) ?{)
	      (setq pairs (cons (cons last-pos pos) pairs))))

	;; Should ignore any pairs that are in a macro, providing
	;; we're not in the same one.
	(when (and pairs (< (car (car pairs)) in-macro-start))
	  (while (and (save-excursion
			(goto-char (car (car pairs)))
			(c-beginning-of-macro))
		      (setq pairs (cdr pairs)))))

	;; Record the last brace pair.
	(when pairs
	  (if (and (eq c-state-cache old-state)
		   (consp (car-safe c-state-cache)))
	      ;; There's a closed pair on the cached state but we've
	      ;; found a later one, so remove it.
	      (setq c-state-cache (cdr c-state-cache)))
	  (setq pairs (car pairs))
	  (setcar pairs (1- (car pairs)))
	  (when (consp (car-safe c-state-cache))
	    ;; There could already be a cons first in `c-state-cache'
	    ;; if we've e.g. jumped over an unbalanced open paren in a
	    ;; macro below.
	    (setq c-state-cache (cdr c-state-cache)))
	  (setq c-state-cache (cons pairs c-state-cache)))

	(if last-pos
	    ;; Prepare to loop, but record the open paren only if it's
	    ;; outside a macro or within the same macro as point, and
	    ;; if it is a legitimate open paren and not some character
	    ;; that got an open paren syntax-table property.
	    (progn
	      (setq pos last-pos)
	      (if (and (or (>= last-pos in-macro-start)
			   (save-excursion
			     (goto-char last-pos)
			     (not (c-beginning-of-macro))))
		       ;; Check for known types of parens that we want
		       ;; to record.  The syntax table is not to be
		       ;; trusted here since the caller might be using
		       ;; e.g. `c++-template-syntax-table'.
		       (memq (char-before last-pos) '(?{ ?\( ?\[)))
		  (setq c-state-cache (cons (1- last-pos) c-state-cache))))

	  (if (setq last-pos (c-up-list-forward pos))
	      ;; Found a close paren without a corresponding opening
	      ;; one.  Maybe we didn't go back far enough, so try to
	      ;; scan backward for the start paren and then start over.
	      (progn
		(setq pos (c-up-list-backward pos)
		      c-state-cache nil)
		(when (or (not pos)
			  ;; Emacs (up to at least 21.2) can get confused by
			  ;; open parens in column zero inside comments: The
			  ;; sexp functions can then misbehave and bring us
			  ;; back to the same point again.  Check this so that
			  ;; we don't get an infinite loop.
			  (>= pos save-pos))
		  (setq pos last-pos
			c-parsing-error
			(format "Unbalanced close paren at line %d"
				(1+ (count-lines (point-min)
						 (c-point 'bol last-pos)))))))
	    (setq pos nil))))

      c-state-cache)))

;; Debug tool to catch cache inconsistencies.
(defvar c-debug-parse-state nil)
(unless (fboundp 'c-real-parse-state)
  (fset 'c-real-parse-state (symbol-function 'c-parse-state)))
(cc-bytecomp-defun c-real-parse-state)
(defun c-debug-parse-state ()
  (let ((res1 (c-real-parse-state)) res2)
    (let ((c-state-cache nil))
      (setq res2 (c-real-parse-state)))
    (unless (equal res1 res2)
      (error "c-parse-state inconsistency: using cache: %s, from scratch: %s"
	     res1 res2))
    res1))
(defun c-toggle-parse-state-debug (&optional arg)
  (interactive "P")
  (setq c-debug-parse-state (c-calculate-state arg c-debug-parse-state))
  (fset 'c-parse-state (symbol-function (if c-debug-parse-state
					    'c-debug-parse-state
					  'c-real-parse-state)))
  (c-keep-region-active))

(defun c-whack-state-before (bufpos paren-state)
  ;; Whack off any state information from PAREN-STATE which lies
  ;; before BUFPOS.  Not destructive on PAREN-STATE.
  ;;
  ;; This function does not do any hidden buffer changes.
  (let* ((newstate (list nil))
	 (ptr newstate)
	 car)
    (while paren-state
      (setq car (car paren-state)
	    paren-state (cdr paren-state))
      (if (< (if (consp car) (car car) car) bufpos)
	  (setq paren-state nil)
	(setcdr ptr (list car))
	(setq ptr (cdr ptr))))
    (cdr newstate)))

(defun c-whack-state-after (bufpos paren-state)
  ;; Whack off any state information from PAREN-STATE which lies at or
  ;; after BUFPOS.  Not destructive on PAREN-STATE.
  ;;
  ;; This function does not do any hidden buffer changes.
  (catch 'done
    (while paren-state
      (let ((car (car paren-state)))
	(if (consp car)
	    ;; just check the car, because in a balanced brace
	    ;; expression, it must be impossible for the corresponding
	    ;; close brace to be before point, but the open brace to
	    ;; be after.
	    (if (<= bufpos (car car))
		nil			; whack it off
	      (if (< bufpos (cdr car))
		  ;; its possible that the open brace is before
		  ;; bufpos, but the close brace is after.  In that
		  ;; case, convert this to a non-cons element.  The
		  ;; rest of the state is before bufpos, so we're
		  ;; done.
		  (throw 'done (cons (car car) (cdr paren-state)))
		;; we know that both the open and close braces are
		;; before bufpos, so we also know that everything else
		;; on state is before bufpos.
		(throw 'done paren-state)))
	  (if (<= bufpos car)
	      nil			; whack it off
	    ;; it's before bufpos, so everything else should too.
	    (throw 'done paren-state)))
	(setq paren-state (cdr paren-state)))
      nil)))

(defun c-most-enclosing-brace (paren-state &optional bufpos)
  ;; Return the bufpos of the innermost enclosing open paren before
  ;; bufpos that hasn't been narrowed out, or nil if none was found.
  ;;
  ;; This function does not do any hidden buffer changes.
  (let (enclosingp)
    (or bufpos (setq bufpos 134217727))
    (while paren-state
      (setq enclosingp (car paren-state)
	    paren-state (cdr paren-state))
      (if (or (consp enclosingp)
	      (>= enclosingp bufpos))
	  (setq enclosingp nil)
	(if (< enclosingp (point-min))
	    (setq enclosingp nil))
	(setq paren-state nil)))
    enclosingp))

(defun c-least-enclosing-brace (paren-state &optional bufpos)
  ;; Return the bufpos of the outermost enclosing open paren before
  ;; bufpos that hasn't been narrowed out, or nil if none was found.
  ;;
  ;; This function does not do any hidden buffer changes.
  (let (pos elem)
    (or bufpos (setq bufpos 134217727))
    (while paren-state
      (setq elem (car paren-state)
	    paren-state (cdr paren-state))
      (unless (or (consp elem)
		  (>= elem bufpos))
	(if (>= elem (point-min))
	    (setq pos elem))))
    pos))

(defun c-safe-position (bufpos paren-state)
  ;; Return the closest known safe position higher up than BUFPOS, or
  ;; nil if PAREN-STATE doesn't contain one.  Return nil if BUFPOS is
  ;; nil, which is useful to find the closest limit before a given
  ;; limit that might be nil.
  ;;
  ;; This function does not do any hidden buffer changes.
  (when bufpos
    (let (elem)
      (catch 'done
	(while paren-state
	  (setq elem (car paren-state))
	  (if (consp elem)
	      (cond ((< (cdr elem) bufpos)
		     (throw 'done (cdr elem)))
		    ((< (car elem) bufpos)
		     ;; See below.
		     (throw 'done (min (1+ (car elem)) bufpos))))
	    (if (< elem bufpos)
		;; elem is the position at and not after the opening paren, so
		;; we can go forward one more step unless it's equal to
		;; bufpos.  This is useful in some cases avoid an extra paren
		;; level between the safe position and bufpos.
		(throw 'done (min (1+ elem) bufpos))))
	  (setq paren-state (cdr paren-state)))))))

(defun c-beginning-of-syntax ()
  ;; This is used for `font-lock-beginning-of-syntax-function'.  It
  ;; goes to the closest previous point that is known to be outside
  ;; any string literal or comment.  `c-state-cache' is used if it has
  ;; a position in the vicinity.
  (let* ((paren-state c-state-cache)
	 elem

	 (pos (catch 'done
		;; Note: Similar code in `c-safe-position'.  The
		;; difference is that we accept a safe position at
		;; the point and don't bother to go forward past open
		;; parens.
		(while paren-state
		  (setq elem (car paren-state))
		  (if (consp elem)
		      (cond ((<= (cdr elem) (point))
			     (throw 'done (cdr elem)))
			    ((<= (car elem) (point))
			     (throw 'done (car elem))))
		    (if (<= elem (point))
			(throw 'done elem)))
		  (setq paren-state (cdr paren-state)))
		(point-min))))

    (if (> pos (- (point) 4000))
	(goto-char pos)
      ;; The position is far back.  Try `c-beginning-of-defun-1'
      ;; (although we can't be entirely sure it will go to a position
      ;; outside a comment or string in current emacsen).  FIXME:
      ;; Consult `syntax-ppss' here.
      (c-beginning-of-defun-1)
      (if (< (point) pos)
	  (goto-char pos)))))


;; Tools for scanning identifiers and other tokens.

(defun c-on-identifier ()
  "Return non-nil if the point is on or directly after an identifier.
Keywords are recognized and not considered identifiers.  If an
identifier is detected, the returned value is its starting position.
If an identifier both starts and stops at the point \(can only happen
in Pike) then the point for the preceding one is returned.

This function does not do any hidden buffer changes."

  (save-excursion
    (if (zerop (skip-syntax-backward "w_"))

	(when (c-major-mode-is 'pike-mode)
	  ;; Handle the `<operator> syntax in Pike.
	  (let ((pos (point)))
	    (skip-chars-backward "-!%&*+/<=>^|~[]()")
	    (and (if (< (skip-chars-backward "`") 0)
		     t
		   (goto-char pos)
		   (eq (char-after) ?\`))
		 (looking-at c-symbol-key)
		 (>= (match-end 0) pos)
		 (point))))

      (and (not (looking-at c-keywords-regexp))
	   (point)))))

(defsubst c-simple-skip-symbol-backward ()
  ;; If the point is at the end of a symbol then skip backward to the
  ;; beginning of it.  Don't move otherwise.  Return non-nil if point
  ;; moved.
  (or (< (skip-syntax-backward "w_") 0)
      (and (c-major-mode-is 'pike-mode)
	   ;; Handle the `<operator> syntax in Pike.
	   (let ((pos (point)))
	     (if (and (< (skip-chars-backward "-!%&*+/<=>^|~[]()") 0)
		      (< (skip-chars-backward "`") 0)
		      (looking-at c-symbol-key)
		      (>= (match-end 0) pos))
		 t
	       (goto-char pos)
	       nil)))))

(defsubst c-beginning-of-current-token (&optional back-limit)
  ;; Move to the beginning of the current token.  Do not move if not
  ;; in the middle of one.  BACK-LIMIT may be used to bound the
  ;; backward search; if given it's assumed to be at the boundary
  ;; between two tokens.
  (if (looking-at "\\w\\|\\s_")
      (skip-syntax-backward "w_" back-limit)
    (let ((start (point)))
      (when (< (skip-syntax-backward ".()" back-limit) 0)
	(while (let ((pos (or (and (looking-at c-nonsymbol-token-regexp)
				   (match-end 0))
			      ;; `c-nonsymbol-token-regexp' should always match
			      ;; since we've skipped backward over punctuator
			      ;; or paren syntax, but consume one char in case
			      ;; it doesn't so that we don't leave point before
			      ;; some earlier incorrect token.
			      (1+ (point)))))
		 (if (<= pos start)
		     (goto-char pos))
		 (< pos start)))))))

(defun c-end-of-current-token (&optional back-limit)
  ;; Move to the end of the current token.  Do not move if not in the
  ;; middle of one.  BACK-LIMIT may be used to bound the backward
  ;; search; if given it's assumed to be at the boundary between two
  ;; tokens.  Return non-nil if the point is moved, nil otherwise.
  (let ((start (point)))
    (cond ((< (skip-syntax-backward "w_" (1- start)) 0)
	   (skip-syntax-forward "w_"))
	  ((< (skip-syntax-backward ".()" back-limit) 0)
	   (while (progn
		    (if (looking-at c-nonsymbol-token-regexp)
			(goto-char (match-end 0))
		      ;; `c-nonsymbol-token-regexp' should always match since
		      ;; we've skipped backward over punctuator or paren
		      ;; syntax, but move forward in case it doesn't so that
		      ;; we don't leave point earlier than we started with.
		      (forward-char))
		    (< (point) start)))))
    (> (point) start)))

(defconst c-jump-syntax-balanced
  (if (memq 'gen-string-delim c-emacs-features)
      "\\w\\|\\s_\\|\\s\(\\|\\s\)\\|\\s\"\\|\\s|"
    "\\w\\|\\s_\\|\\s\(\\|\\s\)\\|\\s\""))

(defconst c-jump-syntax-unbalanced
  (if (memq 'gen-string-delim c-emacs-features)
      "\\w\\|\\s_\\|\\s\"\\|\\s|"
    "\\w\\|\\s_\\|\\s\""))

(defun c-forward-token-2 (&optional count balanced limit)
  "Move forward by tokens.
A token is defined as all symbols and identifiers which aren't
syntactic whitespace \(note that multicharacter tokens like \"==\" are
treated properly).  Point is always either left at the beginning of a
token or not moved at all.  COUNT specifies the number of tokens to
move; a negative COUNT moves in the opposite direction.  A COUNT of 0
moves to the next token beginning only if not already at one.  If
BALANCED is true, move over balanced parens, otherwise move into them.
Also, if BALANCED is true, never move out of an enclosing paren.

LIMIT sets the limit for the movement and defaults to the point limit.
The case when LIMIT is set in the middle of a token, comment or macro
is handled correctly, i.e. the point won't be left there.

Return the number of tokens left to move \(positive or negative).  If
BALANCED is true, a move over a balanced paren counts as one.  Note
that if COUNT is 0 and no appropriate token beginning is found, 1 will
be returned.  Thus, a return value of 0 guarantees that point is at
the requested position and a return value less \(without signs) than
COUNT guarantees that point is at the beginning of some token."

  (or count (setq count 1))
  (if (< count 0)
      (- (c-backward-token-2 (- count) balanced limit))

    (let ((jump-syntax (if balanced
			   c-jump-syntax-balanced
			 c-jump-syntax-unbalanced))
	  (last (point))
	  (prev (point)))

      (if (zerop count)
	  ;; If count is zero we should jump if in the middle of a token.
	  (c-end-of-current-token))

      (save-restriction
	(if limit (narrow-to-region (point-min) limit))
	(if (/= (point)
		(progn (c-forward-syntactic-ws) (point)))
	    ;; Skip whitespace.  Count this as a move if we did in
	    ;; fact move.
	    (setq count (max (1- count) 0)))

	(if (eobp)
	    ;; Moved out of bounds.  Make sure the returned count isn't zero.
	    (progn
	      (if (zerop count) (setq count 1))
	      (goto-char last))

	  ;; Use `condition-case' to avoid having the limit tests
	  ;; inside the loop.
	  (condition-case nil
	      (while (and
		      (> count 0)
		      (progn
			(setq last (point))
			(cond ((looking-at jump-syntax)
			       (goto-char (scan-sexps (point) 1))
			       t)
			      ((looking-at c-nonsymbol-token-regexp)
			       (goto-char (match-end 0))
			       t)
			      ;; `c-nonsymbol-token-regexp' above should always
			      ;; match if there are correct tokens.  Try to
			      ;; widen to see if the limit was set in the
			      ;; middle of one, else fall back to treating
			      ;; the offending thing as a one character token.
			      ((and limit
				    (save-restriction
				      (widen)
				      (looking-at c-nonsymbol-token-regexp)))
			       nil)
			      (t
			       (forward-char)
			       t))))
		(c-forward-syntactic-ws)
		(setq prev last
		      count (1- count)))
	    (error (goto-char last)))

	  (when (eobp)
	    (goto-char prev)
	    (setq count (1+ count)))))

      count)))

(defun c-backward-token-2 (&optional count balanced limit)
  "Move backward by tokens.
See `c-forward-token-2' for details."

  (or count (setq count 1))
  (if (< count 0)
      (- (c-forward-token-2 (- count) balanced limit))

    (or limit (setq limit (point-min)))
    (let ((jump-syntax (if balanced
			   c-jump-syntax-balanced
			 c-jump-syntax-unbalanced))
	  (last (point)))

      (if (zerop count)
	  ;; The count is zero so try to skip to the beginning of the
	  ;; current token.
	  (if (> (point)
		 (progn (c-beginning-of-current-token) (point)))
	      (if (< (point) limit)
		  ;; The limit is inside the same token, so return 1.
		  (setq count 1))

	    ;; We're not in the middle of a token.  If there's
	    ;; whitespace after the point then we must move backward,
	    ;; so set count to 1 in that case.
	    (and (looking-at c-syntactic-ws-start)
		 ;; If we're looking at a '#' that might start a cpp
		 ;; directive then we have to do a more elaborate check.
		 (or (/= (char-after) ?#)
		     (not c-opt-cpp-prefix)
		     (save-excursion
		       (and (= (point)
			       (progn (beginning-of-line)
				      (looking-at "[ \t]*")
				      (match-end 0)))
			    (or (bobp)
				(progn (backward-char)
				       (not (eq (char-before) ?\\)))))))
		 (setq count 1))))

      ;; Use `condition-case' to avoid having to check for buffer
      ;; limits in `backward-char', `scan-sexps' and `goto-char' below.
      (condition-case nil
	  (while (and
		  (> count 0)
		  (progn
		    (c-backward-syntactic-ws)
		    (backward-char)
		    (if (looking-at jump-syntax)
			(goto-char (scan-sexps (1+ (point)) -1))
		      ;; This can be very inefficient if there's a long
		      ;; sequence of operator tokens without any separation.
		      ;; That doesn't happen in practice, anyway.
		      (c-beginning-of-current-token))
		    (>= (point) limit)))
	    (setq last (point)
		  count (1- count)))
	(error (goto-char last)))

      (if (< (point) limit)
	  (goto-char last))

      count)))

(defun c-forward-token-1 (&optional count balanced limit)
  "Like `c-forward-token-2' but doesn't treat multicharacter operator
tokens like \"==\" as single tokens, i.e. all sequences of symbol
characters are jumped over character by character.  This function is
for compatibility only; it's only a wrapper over `c-forward-token-2'."
  (let ((c-nonsymbol-token-regexp "\\s.\\|\\s\(\\|\\s\)"))
    (c-forward-token-2 count balanced limit)))

(defun c-backward-token-1 (&optional count balanced limit)
  "Like `c-backward-token-2' but doesn't treat multicharacter operator
tokens like \"==\" as single tokens, i.e. all sequences of symbol
characters are jumped over character by character.  This function is
for compatibility only; it's only a wrapper over `c-backward-token-2'."
  (let ((c-nonsymbol-token-regexp "\\s.\\|\\s\(\\|\\s\)"))
    (c-backward-token-2 count balanced limit)))


;; Tools for doing searches restricted to syntactically relevant text.

(defun c-syntactic-re-search-forward (regexp &optional bound noerror
				      paren-level not-inside-token
				      lookbehind-submatch)
  "Like `re-search-forward', but only report matches that are found
in syntactically significant text.  I.e. matches in comments, macros
or string literals are ignored.  The start point is assumed to be
outside any comment, macro or string literal, or else the content of
that region is taken as syntactically significant text.

If PAREN-LEVEL is non-nil, an additional restriction is added to
ignore matches in nested paren sexps.  The search will also not go
outside the current list sexp, which has the effect that if the point
should be moved to BOUND when no match is found \(i.e. NOERROR is
neither nil nor t), then it will be at the closing paren if the end of
the current list sexp is encountered first.

If NOT-INSIDE-TOKEN is non-nil, matches in the middle of tokens are
ignored.  Things like multicharacter operators and special symbols
\(e.g. \"`()\" in Pike) are handled but currently not floating point
constants.

If LOOKBEHIND-SUBMATCH is non-nil, it's taken as a number of a
subexpression in REGEXP.  The end of that submatch is used as the
position to check for syntactic significance.  If LOOKBEHIND-SUBMATCH
isn't used or if that subexpression didn't match then the start
position of the whole match is used instead.  The \"look behind\"
subexpression is never tested before the starting position, so it
might be a good idea to include \\=\\= as a match alternative in it.

Optimization note: Matches might be missed if the \"look behind\"
subexpression can match the end of nonwhite syntactic whitespace,
i.e. the end of comments or cpp directives.  This since the function
skips over such things before resuming the search.  It's on the other
hand not safe to assume that the \"look behind\" subexpression never
matches syntactic whitespace.

Bug: Unbalanced parens inside cpp directives are currently not handled
correctly \(i.e. they don't get ignored as they should) when
PAREN-LEVEL is set."

  (or bound (setq bound (point-max)))
  (if paren-level (setq paren-level -1))

  ;;(message "c-syntactic-re-search-forward %s %s %S" (point) bound regexp)

  (let ((start (point))
	tmp
	;; Start position for the last search.
	search-pos
	;; The `parse-partial-sexp' state between the start position
	;; and the point.
	state
	;; The current position after the last state update.  The next
	;; `parse-partial-sexp' continues from here.
	(state-pos (point))
	;; The position at which to check the state and the state
	;; there.  This is separate from `state-pos' since we might
	;; need to back up before doing the next search round.
	check-pos check-state
	;; Last position known to end a token.
	(last-token-end-pos (point-min))
	;; Set when a valid match is found.
	found)

    (condition-case err
	(while
	    (and
	     (progn
	       (setq search-pos (point))
	       (re-search-forward regexp bound noerror))

	     (progn
	       (setq state (parse-partial-sexp
			    state-pos (match-beginning 0) paren-level nil state)
		     state-pos (point))
	       (if (setq check-pos (and lookbehind-submatch
					(or (not paren-level)
					    (>= (car state) 0))
					(match-end lookbehind-submatch)))
		   (setq check-state (parse-partial-sexp
				      state-pos check-pos paren-level nil state))
		 (setq check-pos state-pos
		       check-state state))

	       ;; NOTE: If we got a look behind subexpression and get
	       ;; an insignificant match in something that isn't
	       ;; syntactic whitespace (i.e. strings or in nested
	       ;; parentheses), then we can never skip more than a
	       ;; single character from the match start position
	       ;; (i.e. `state-pos' here) before continuing the
	       ;; search.  That since the look behind subexpression
	       ;; might match the end of the insignificant region in
	       ;; the next search.

	       (cond
		((elt check-state 7)
		 ;; Match inside a line comment.  Skip to eol.  Use
		 ;; `re-search-forward' instead of `skip-chars-forward' to get
		 ;; the right bound behavior.
		 (re-search-forward "[\n\r]" bound noerror))

		((elt check-state 4)
		 ;; Match inside a block comment.  Skip to the '*/'.
		 (search-forward "*/" bound noerror))

		((and (not (elt check-state 5))
		      (eq (char-before check-pos) ?/)
		      (not (c-get-char-property (1- check-pos) 'syntax-table))
		      (memq (char-after check-pos) '(?/ ?*)))
		 ;; Match in the middle of the opener of a block or line
		 ;; comment.
		 (if (= (char-after check-pos) ?/)
		     (re-search-forward "[\n\r]" bound noerror)
		   (search-forward "*/" bound noerror)))

		;; The last `parse-partial-sexp' above might have
		;; stopped short of the real check position if the end
		;; of the current sexp was encountered in paren-level
		;; mode.  The checks above are always false in that
		;; case, and since they can do better skipping in
		;; lookbehind-submatch mode, we do them before
		;; checking the paren level.

		((and paren-level
		      (/= (setq tmp (car check-state)) 0))
		 ;; Check the paren level first since we're short of the
		 ;; syntactic checking position if the end of the
		 ;; current sexp was encountered by `parse-partial-sexp'.
		 (if (> tmp 0)

		     ;; Inside a nested paren sexp.
		     (if lookbehind-submatch
			 ;; See the NOTE above.
			 (progn (goto-char state-pos) t)
		       ;; Skip out of the paren quickly.
		       (setq state (parse-partial-sexp state-pos bound 0 nil state)
			     state-pos (point)))

		   ;; Have exited the current paren sexp.
		   (if noerror
		       (progn
			 ;; The last `parse-partial-sexp' call above
			 ;; has left us just after the closing paren
			 ;; in this case, so we can modify the bound
			 ;; to leave the point at the right position
			 ;; upon return.
			 (setq bound (1- (point)))
			 nil)
		     (signal 'search-failed (list regexp)))))

		((setq tmp (elt check-state 3))
		 ;; Match inside a string.
		 (if (or lookbehind-submatch
			 (not (integerp tmp)))
		     ;; See the NOTE above.
		     (progn (goto-char state-pos) t)
		   ;; Skip to the end of the string before continuing.
		   (let ((ender (make-string 1 tmp)) (continue t))
		     (while (if (search-forward ender bound noerror)
				(progn
				  (setq state (parse-partial-sexp
					       state-pos (point) nil nil state)
					state-pos (point))
				  (elt state 3))
			      (setq continue nil)))
		     continue)))

		((save-excursion
		   (save-match-data
		     (c-beginning-of-macro start)))
		 ;; Match inside a macro.  Skip to the end of it.
		 (c-end-of-macro)
		 (cond ((<= (point) bound) t)
		       (noerror nil)
		       (t (signal 'search-failed (list regexp)))))

		((and not-inside-token
		      (or (< check-pos last-token-end-pos)
			  (< check-pos
			     (save-excursion
			       (goto-char check-pos)
			       (save-match-data
				 (c-end-of-current-token last-token-end-pos))
			       (setq last-token-end-pos (point))))))
		 ;; Inside a token.
		 (if lookbehind-submatch
		     ;; See the NOTE above.
		     (goto-char state-pos)
		   (goto-char (min last-token-end-pos bound))))

		(t
		 ;; A real match.
		 (setq found t)
		 nil)))

	     ;; Should loop to search again, but take care to avoid
	     ;; looping on the same spot.
	     (or (/= search-pos (point))
		 (if (= (point) bound)
		     (if noerror
			 nil
		       (signal 'search-failed (list regexp)))
		   (forward-char)
		   t))))

      (error
       (goto-char start)
       (signal (car err) (cdr err))))

    ;;(message "c-syntactic-re-search-forward done %s" (or (match-end 0) (point)))

    (if found
	(progn
	  (goto-char (match-end 0))
	  (match-end 0))

      ;; Search failed.  Set point as appropriate.
      (if (eq noerror t)
	  (goto-char start)
	(goto-char bound))
      nil)))

(defun c-syntactic-skip-backward (skip-chars &optional limit)
  "Like `skip-chars-backward' but only look at syntactically relevant chars,
i.e. don't stop at positions inside syntactic whitespace or string
literals.  Preprocessor directives are also ignored, with the exception
of the one that the point starts within, if any.  If LIMIT is given,
it's assumed to be at a syntactically relevant position."

  (let ((start (point))
	;; A list of syntactically relevant positions in descending
	;; order.  It's used to avoid scanning repeatedly over
	;; potentially large regions with `parse-partial-sexp' to verify
	;; each position.
	safe-pos-list
	;; The result from `c-beginning-of-macro' at the start position or the
	;; start position itself if it isn't within a macro.  Evaluated on
	;; demand.
	start-macro-beg)

    (while (progn
	     (while (and
		     (< (skip-chars-backward skip-chars limit) 0)

		     ;; Use `parse-partial-sexp' from a safe position down to
		     ;; the point to check if it's outside comments and
		     ;; strings.
		     (let ((pos (point)) safe-pos state)
		       ;; Pick a safe position as close to the point as
		       ;; possible.
		       ;;
		       ;; FIXME: Consult `syntax-ppss' here if our
		       ;; cache doesn't give a good position.
		       (while (and safe-pos-list
				   (> (car safe-pos-list) (point)))
			 (setq safe-pos-list (cdr safe-pos-list)))
		       (unless (setq safe-pos (car-safe safe-pos-list))
			 (setq safe-pos (max (or (c-safe-position
						  (point) (or c-state-cache
							      (c-parse-state)))
						 0)
					     (point-min))
			       safe-pos-list (list safe-pos)))

		       (while (progn
				(setq state (parse-partial-sexp
					     safe-pos pos 0))
				(< (point) pos))
			 ;; Cache positions along the way to use if we have to
			 ;; back up more.  Every closing paren on the same
			 ;; level seems like fairly well spaced positions.
			 (setq safe-pos (point)
			       safe-pos-list (cons safe-pos safe-pos-list)))

		       (cond
			((or (elt state 3) (elt state 4))
			 ;; Inside string or comment.  Continue search at the
			 ;; beginning of it.
			 (if (setq pos (nth 8 state))
			     ;; It's an emacs where `parse-partial-sexp'
			     ;; supplies the starting position.
			     (goto-char pos)
			   (goto-char (car (c-literal-limits safe-pos))))
			 t)

			((c-beginning-of-macro limit)
			 ;; Inside a macro.
			 (if (< (point)
				(or start-macro-beg
				    (setq start-macro-beg
					  (save-excursion
					    (goto-char start)
					    (c-beginning-of-macro limit)
					    (point)))))
			     t
			   ;; It's inside the same macro we started in so it's
			   ;; a relevant match.
			   (goto-char pos)
			   nil))))))

	     (> (point)
		(progn
		  ;; Skip syntactic ws afterwards so that we don't stop at the
		  ;; end of a comment if `skip-chars' is something like "^/".
		  (c-backward-syntactic-ws)
		  (point)))))

    (- (point) start)))


;; Tools for handling comments and string literals.

(defun c-slow-in-literal (&optional lim detect-cpp)
  "Return the type of literal point is in, if any.
The return value is `c' if in a C-style comment, `c++' if in a C++
style comment, `string' if in a string literal, `pound' if DETECT-CPP
is non-nil and in a preprocessor line, or nil if somewhere else.
Optional LIM is used as the backward limit of the search.  If omitted,
or nil, `c-beginning-of-defun' is used.

The last point calculated is cached if the cache is enabled, i.e. if
`c-in-literal-cache' is bound to a two element vector.

This function does not do any hidden buffer changes."
  (if (and (vectorp c-in-literal-cache)
	   (= (point) (aref c-in-literal-cache 0)))
      (aref c-in-literal-cache 1)
    (let ((rtn (save-excursion
		 (let* ((pos (point))
			(lim (or lim (progn
				       (c-beginning-of-syntax)
				       (point))))
			(state (parse-partial-sexp lim pos)))
		   (cond
		    ((elt state 3) 'string)
		    ((elt state 4) (if (elt state 7) 'c++ 'c))
		    ((and detect-cpp (c-beginning-of-macro lim)) 'pound)
		    (t nil))))))
      ;; cache this result if the cache is enabled
      (if (not c-in-literal-cache)
	  (setq c-in-literal-cache (vector (point) rtn)))
      rtn)))

;; XEmacs has a built-in function that should make this much quicker.
;; I don't think we even need the cache, which makes our lives more
;; complicated anyway.  In this case, lim is only used to detect
;; cpp directives.
;;
;; Note that there is a bug in Xemacs's buffer-syntactic-context when used in
;; conjunction with syntax-table-properties.  The bug is present in, e.g.,
;; Xemacs 21.4.4.  It manifested itself thus:
;;
;; Starting with an empty AWK Mode buffer, type
;; /regexp/ {<C-j>
;; Point gets wrongly left at column 0, rather than being indented to tab-width.
;;
;; AWK Mode is designed such that when the first / is typed, it gets the
;; syntax-table property "string fence".  When the second / is typed, BOTH /s
;; are given the s-t property "string".  However, buffer-syntactic-context
;; fails to take account of the change of the s-t property on the opening / to
;; "string", and reports that the { is within a string started by the second /.
;;
;; The workaround for this is for the AWK Mode initialisation to switch the
;; defalias for c-in-literal to c-slow-in-literal.  This will slow down other
;; cc-modes in Xemacs whenever an awk-buffer has been initialised.
;; 
;; (Alan Mackenzie, 2003/4/30).

(defun c-fast-in-literal (&optional lim detect-cpp)
  (let ((context (buffer-syntactic-context)))
    (cond
     ((eq context 'string) 'string)
     ((eq context 'comment) 'c++)
     ((eq context 'block-comment) 'c)
     ((and detect-cpp (save-excursion (c-beginning-of-macro lim))) 'pound))))

(defalias 'c-in-literal
  (if (fboundp 'buffer-syntactic-context)
    'c-fast-in-literal                  ; XEmacs
    'c-slow-in-literal))                ; GNU Emacs

;; The defalias above isn't enough to shut up the byte compiler.
(cc-bytecomp-defun c-in-literal)

(defun c-literal-limits (&optional lim near not-in-delimiter)
  "Return a cons of the beginning and end positions of the comment or
string surrounding point (including both delimiters), or nil if point
isn't in one.  If LIM is non-nil, it's used as the \"safe\" position
to start parsing from.  If NEAR is non-nil, then the limits of any
literal next to point is returned.  \"Next to\" means there's only
spaces and tabs between point and the literal.  The search for such a
literal is done first in forward direction.  If NOT-IN-DELIMITER is
non-nil, the case when point is inside a starting delimiter won't be
recognized.  This only has effect for comments, which have starting
delimiters with more than one character.

This function does not do any hidden buffer changes."

  (save-excursion
    (let* ((pos (point))
	   (lim (or lim (progn
			  (c-beginning-of-syntax)
			  (point))))
	   (state (parse-partial-sexp lim pos)))

      (cond ((elt state 3)
	     ;; String.  Search backward for the start.
	     (while (elt state 3)
	       (search-backward (make-string 1 (elt state 3)))
	       (setq state (parse-partial-sexp lim (point))))
	     (cons (point) (or (c-safe (c-forward-sexp 1) (point))
			       (point-max))))

	    ((elt state 7)
	     ;; Line comment.  Search from bol for the comment starter.
	     (beginning-of-line)
	     (setq state (parse-partial-sexp lim (point))
		   lim (point))
	     (while (not (elt state 7))
	       (search-forward "//")	; Should never fail.
	       (setq state (parse-partial-sexp
			    lim (point) nil nil state)
		     lim (point)))
	     (backward-char 2)
	     (cons (point) (progn (c-forward-single-comment) (point))))

	    ((elt state 4)
	     ;; Block comment.  Search backward for the comment starter.
	     (while (elt state 4)
	       (search-backward "/*")	; Should never fail.
	       (setq state (parse-partial-sexp lim (point))))
	     (cons (point) (progn (c-forward-single-comment) (point))))

	    ((and (not not-in-delimiter)
		  (not (elt state 5))
		  (eq (char-before) ?/)
		  (looking-at "[/*]"))
	     ;; We're standing in a comment starter.
	     (backward-char 1)
	     (cons (point) (progn (c-forward-single-comment) (point))))

	    (near
	     (goto-char pos)

	     ;; Search forward for a literal.
	     (skip-chars-forward " \t")

	     (cond
	      ((looking-at c-string-limit-regexp) ; String.
	       (cons (point) (or (c-safe (c-forward-sexp 1) (point))
				 (point-max))))

	      ((looking-at c-comment-start-regexp) ; Line or block comment.
	       (cons (point) (progn (c-forward-single-comment) (point))))

	      (t
	       ;; Search backward.
	       (skip-chars-backward " \t")

	       (let ((end (point)) beg)
		 (cond
		  ((save-excursion
		     (< (skip-syntax-backward c-string-syntax) 0)) ; String.
		   (setq beg (c-safe (c-backward-sexp 1) (point))))

		  ((and (c-safe (forward-char -2) t)
			(looking-at "*/"))
		   ;; Block comment.  Due to the nature of line
		   ;; comments, they will always be covered by the
		   ;; normal case above.
		   (goto-char end)
		   (c-backward-single-comment)
		   ;; If LIM is bogus, beg will be bogus.
		   (setq beg (point))))

		 (if beg (cons beg end))))))
	    ))))

(defun c-literal-limits-fast (&optional lim near not-in-delimiter)
  ;; Like c-literal-limits, but for emacsen whose `parse-partial-sexp'
  ;; returns the pos of the comment start.

  "Return a cons of the beginning and end positions of the comment or
string surrounding point (including both delimiters), or nil if point
isn't in one.  If LIM is non-nil, it's used as the \"safe\" position
to start parsing from.  If NEAR is non-nil, then the limits of any
literal next to point is returned.  \"Next to\" means there's only
spaces and tabs between point and the literal.  The search for such a
literal is done first in forward direction.  If NOT-IN-DELIMITER is
non-nil, the case when point is inside a starting delimiter won't be
recognized.  This only has effect for comments, which have starting
delimiters with more than one character.

This function does not do any hidden buffer changes."

  (save-excursion
    (let* ((pos (point))
	   (lim (or lim (progn
			  (c-beginning-of-syntax)
			  (point))))
	   (state (parse-partial-sexp lim pos)))

      (cond ((elt state 3)		; String.
	     (goto-char (elt state 8))
	     (cons (point) (or (c-safe (c-forward-sexp 1) (point))
			       (point-max))))

	    ((elt state 4)		; Comment.
	     (goto-char (elt state 8))
	     (cons (point) (progn (c-forward-single-comment) (point))))

	    ((and (not not-in-delimiter)
		  (not (elt state 5))
		  (eq (char-before) ?/)
		  (looking-at "[/*]"))
	     ;; We're standing in a comment starter.
	     (backward-char 1)
	     (cons (point) (progn (c-forward-single-comment) (point))))

	    (near
	     (goto-char pos)

	     ;; Search forward for a literal.
	     (skip-chars-forward " \t")

	     (cond
	      ((looking-at c-string-limit-regexp) ; String.
	       (cons (point) (or (c-safe (c-forward-sexp 1) (point))
				 (point-max))))

	      ((looking-at c-comment-start-regexp) ; Line or block comment.
	       (cons (point) (progn (c-forward-single-comment) (point))))

	      (t
	       ;; Search backward.
	       (skip-chars-backward " \t")

	       (let ((end (point)) beg)
		 (cond
		  ((save-excursion
		     (< (skip-syntax-backward c-string-syntax) 0)) ; String.
		   (setq beg (c-safe (c-backward-sexp 1) (point))))

		  ((and (c-safe (forward-char -2) t)
			(looking-at "*/"))
		   ;; Block comment.  Due to the nature of line
		   ;; comments, they will always be covered by the
		   ;; normal case above.
		   (goto-char end)
		   (c-backward-single-comment)
		   ;; If LIM is bogus, beg will be bogus.
		   (setq beg (point))))

		 (if beg (cons beg end))))))
	    ))))

(if (memq 'pps-extended-state c-emacs-features)
    (defalias 'c-literal-limits 'c-literal-limits-fast))

(defun c-collect-line-comments (range)
  "If the argument is a cons of two buffer positions (such as returned by
`c-literal-limits'), and that range contains a C++ style line comment,
then an extended range is returned that contains all adjacent line
comments (i.e. all comments that starts in the same column with no
empty lines or non-whitespace characters between them).  Otherwise the
argument is returned.

This function does not do any hidden buffer changes."
  (save-excursion
    (condition-case nil
	(if (and (consp range) (progn
				 (goto-char (car range))
				 (looking-at "//")))
	    (let ((col (current-column))
		  (beg (point))
		  (bopl (c-point 'bopl))
		  (end (cdr range)))
	      ;; Got to take care in the backward direction to handle
	      ;; comments which are preceded by code.
	      (while (and (c-backward-single-comment)
			  (>= (point) bopl)
			  (looking-at "//")
			  (= col (current-column)))
		(setq beg (point)
		      bopl (c-point 'bopl)))
	      (goto-char end)
	      (while (and (progn (skip-chars-forward " \t")
				 (looking-at "//"))
			  (= col (current-column))
			  (prog1 (zerop (forward-line 1))
			    (setq end (point)))))
	      (cons beg end))
	  range)
      (error range))))

(defun c-literal-type (range)
  "Convenience function that given the result of `c-literal-limits',
returns nil or the type of literal that the range surrounds.  It's
much faster than using `c-in-literal' and is intended to be used when
you need both the type of a literal and its limits.

This function does not do any hidden buffer changes."
  (if (consp range)
      (save-excursion
	(goto-char (car range))
	(cond ((looking-at c-string-limit-regexp) 'string)
	      ((or (looking-at "//") ; c++ line comment
		   (and (looking-at "\\s<") ; comment starter
			(looking-at "#"))) ; awk comment.
               'c++)
	      (t 'c)))			; Assuming the range is valid.
    range))


;; `c-find-decl-spots' and accompanying stuff.

;; Variables used in `c-find-decl-spots' to cache the search done for
;; the first declaration in the last call.  When that function starts,
;; it needs to back up over syntactic whitespace to look at the last
;; token before the region being searched.  That can sometimes cause
;; moves back and forth over a quite large region of comments and
;; macros, which would be repeated for each changed character when
;; we're called during fontification, since font-lock refontifies the
;; current line for each change.  Thus it's worthwhile to cache the
;; first match.
;;
;; `c-find-decl-syntactic-pos' is a syntactically relevant position in
;; the syntactic whitespace less or equal to some start position.
;; There's no cached value if it's nil.
;;
;; `c-find-decl-match-pos' is the match position if
;; `c-find-decl-prefix-search' matched before the syntactic whitespace
;; at `c-find-decl-syntactic-pos', or nil if there's no such match.
(defvar c-find-decl-syntactic-pos nil)
(make-variable-buffer-local 'c-find-decl-syntactic-pos)
(defvar c-find-decl-match-pos nil)
(make-variable-buffer-local 'c-find-decl-match-pos)

(defsubst c-invalidate-find-decl-cache (change-min-pos)
  (and c-find-decl-syntactic-pos
       (< change-min-pos c-find-decl-syntactic-pos)
       (setq c-find-decl-syntactic-pos nil)))

; (defface c-debug-decl-spot-face
;   '((t (:background "Turquoise")))
;   "Debug face to mark the spots where `c-find-decl-spots' stopped.")
; (defface c-debug-decl-sws-face
;   '((t (:background "Khaki")))
;   "Debug face to mark the syntactic whitespace between the declaration
; spots and the preceding token end.")

(defmacro c-debug-put-decl-spot-faces (match-pos decl-pos)
  (when (facep 'c-debug-decl-spot-face)
    `(let ((match-pos ,match-pos) (decl-pos ,decl-pos))
       (c-debug-add-face (max match-pos (point-min)) decl-pos
			 'c-debug-decl-sws-face)
       (c-debug-add-face decl-pos (min (1+ decl-pos) (point-max))
			 'c-debug-decl-spot-face))))
(defmacro c-debug-remove-decl-spot-faces (beg end)
  (when (facep 'c-debug-decl-spot-face)
    `(progn
       (c-debug-remove-face ,beg ,end 'c-debug-decl-spot-face)
       (c-debug-remove-face ,beg ,end 'c-debug-decl-sws-face))))

(defmacro c-find-decl-prefix-search ()
  ;; Macro used inside `c-find-decl-spots'.  It ought to be a defun,
  ;; but it contains lots of free variables that refer to things
  ;; inside `c-find-decl-spots'.  The point is left at `cfd-match-pos'
  ;; if there is a match, otherwise at `cfd-limit'.

  '(progn
     ;; Find the next property match position if we haven't got one already.
     (unless cfd-prop-match
       (save-excursion
	 (while (progn
		  (goto-char (next-single-property-change
			      (point) 'c-type nil cfd-limit))
		  (and (< (point) cfd-limit)
		       (not (eq (c-get-char-property (1- (point)) 'c-type)
				'c-decl-end)))))
	 (setq cfd-prop-match (point))))

     ;; Find the next `c-decl-prefix-re' match if we haven't got one already.
     (unless cfd-re-match
       (while (and (setq cfd-re-match
			 (re-search-forward c-decl-prefix-re cfd-limit 'move))
		   (c-got-face-at (1- (setq cfd-re-match (match-end 1)))
				  c-literal-faces))
	 ;; Search again if the match is within a comment or a string literal.
	 (while (progn
		  (goto-char (next-single-property-change
			      cfd-re-match 'face nil cfd-limit))
		  (and (< (point) cfd-limit)
		       (c-got-face-at (point) c-literal-faces)))
	   (setq cfd-re-match (point))))
       (unless cfd-re-match
	 (setq cfd-re-match cfd-limit)))

     ;; Choose whichever match is closer to the start.
     (if (< cfd-re-match cfd-prop-match)
	 (setq cfd-match-pos cfd-re-match
	       cfd-re-match nil)
       (setq cfd-match-pos cfd-prop-match
	     cfd-prop-match nil))

     (goto-char cfd-match-pos)

     (when (< cfd-match-pos cfd-limit)
       ;; Skip forward past comments only so we don't skip macros.
       (c-forward-comments)
       ;; Set the position to continue at.  We can avoid going over
       ;; the comments skipped above a second time, but it's possible
       ;; that the comment skipping has taken us past `cfd-prop-match'
       ;; since the property might be used inside comments.
       (setq cfd-continue-pos (if cfd-prop-match
				  (min cfd-prop-match (point))
				(point))))))

(defun c-find-decl-spots (cfd-limit cfd-decl-re cfd-face-checklist cfd-fun)
  ;; Call CFD-FUN for each possible spot for a declaration from the
  ;; point to CFD-LIMIT.  A spot for a declaration is the first token
  ;; in the buffer and each token after the ones matched by
  ;; `c-decl-prefix-re' and after the occurrences of the `c-type'
  ;; property with the value `c-decl-end' (if `c-type-decl-end-used'
  ;; is set).  Only a spot that match CFD-DECL-RE and whose face is in
  ;; the CFD-FACE-CHECKLIST list causes CFD-FUN to be called.  The
  ;; face check is disabled if CFD-FACE-CHECKLIST is nil.
  ;;
  ;; If the match is inside a macro then the buffer is narrowed to the
  ;; end of it, so that CFD-FUN can investigate the following tokens
  ;; without matching something that begins inside a macro and ends
  ;; outside it.  It's to avoid this work that the CFD-DECL-RE and
  ;; CFD-FACE-CHECKLIST checks exist.
  ;;
  ;; CFD-FUN is called with point at the start of the spot.  It's
  ;; passed two arguments: The first is the end position of the token
  ;; that `c-decl-prefix-re' matched, or 0 for the implicit match at
  ;; bob.  The second is a flag that is t when the match is inside a
  ;; macro.
  ;;
  ;; It's assumed that comment and strings are fontified in the
  ;; searched range.
  ;;
  ;; This is mainly used in fontification, and so has an elaborate
  ;; cache to handle repeated calls from the same start position; see
  ;; the variables above.
  ;;
  ;; All variables in this function begin with `cfd-' to avoid name
  ;; collision with the (dynamically bound) variables used in CFD-FUN.

  (let ((cfd-buffer-end (point-max))
	;; The last regexp match found by `c-find-decl-prefix-search'.
	cfd-re-match
	;; The last `c-decl-end' found by `c-find-decl-prefix-search'.
	;; If searching for the property isn't needed then we disable
	;; it by faking a first match at the limit.
	(cfd-prop-match (unless c-type-decl-end-used cfd-limit))
	;; The position of the last match found by
	;; `c-find-decl-prefix-search'.  For regexp matches it's the
	;; end of the matched token, for property matches it's the end
	;; of the property.  0 for the implicit match at bob.
	;; `cfd-limit' if there's no match.
	(cfd-match-pos cfd-limit)
	;; The position to continue searching at.
	cfd-continue-pos
	;; The position of the last "real" token we've stopped at.
	;; This can be greater than `cfd-continue-pos' when we get
	;; hits inside macros or at `c-decl-end' positions inside
	;; comments.
	(cfd-token-pos 0)
	;; The end position of the last entered macro.
	(cfd-macro-end 0))

    ;; Initialize by finding a syntactically relevant start position
    ;; before the point, and do the first `c-decl-prefix-re' search
    ;; unless we're at bob.

    (let ((start-pos (point)) syntactic-pos)
      ;; Must back up a bit since we look for the end of the previous
      ;; statement or declaration, which is earlier than the first
      ;; returned match.

      (when (c-got-face-at (point) c-literal-faces)
	;; But first we need to move to a syntactically relevant
	;; position.  Use the faces to back up to the start of the
	;; comment or string literal.
	(when (and (not (bobp))
		   (c-got-face-at (1- (point)) c-literal-faces))
	  (while (progn
		   (goto-char (previous-single-property-change
			       (point) 'face nil (point-min)))
		   (and (> (point) (point-min))
			(c-got-face-at (point) c-literal-faces)))))

	;; XEmacs doesn't fontify the quotes surrounding string
	;; literals.
	(and (featurep 'xemacs)
	     (eq (get-text-property (point) 'face)
		 'font-lock-string-face)
	     (not (bobp))
	     (progn (backward-char)
		    (not (looking-at c-string-limit-regexp)))
	     (forward-char))

	;; The font lock package might not have fontified the start of
	;; the literal at all so check that we have arrived at
	;; something that looks like a start or else resort to
	;; `c-literal-limits'.
	(unless (looking-at c-literal-start-regexp)
	  (let ((range (c-literal-limits)))
	    (if range (goto-char (car range))))))

      ;; Must back out of any macro so that we don't miss any
      ;; declaration that could follow after it, unless the limit is
      ;; inside the macro.  We only check that for the current line to
      ;; save some time; it's enough for the by far most common case
      ;; when font-lock refontifies the current line only.
      (when (save-excursion
	      (and (= (forward-line 1) 0)
		   (bolp)		; forward-line has funny behavior at eob.
		   (or (< (c-point 'eol) cfd-limit)
		       (progn (backward-char)
			      (not (eq (char-before) ?\\))))))
	(c-beginning-of-macro))

      ;; Clear the cache if it applied further down.
      (c-invalidate-find-decl-cache start-pos)

      (setq syntactic-pos (point))
      (c-backward-syntactic-ws c-find-decl-syntactic-pos)

      ;; If we hit `c-find-decl-syntactic-pos' and
      ;; `c-find-decl-match-pos' is set then we install the cached
      ;; values.  If we hit `c-find-decl-syntactic-pos' and
      ;; `c-find-decl-match-pos' is nil then we know there's no decl
      ;; prefix in the whitespace before `c-find-decl-syntactic-pos'
      ;; and so we can continue the search from this point.  If we
      ;; didn't hit `c-find-decl-syntactic-pos' then we're now in the
      ;; right spot to begin searching anyway.
      (if (and (eq (point) c-find-decl-syntactic-pos)
	       c-find-decl-match-pos)

	  (progn
	    ;; The match is always outside macros and comments so we
	    ;; start at the next token.  The loop below will later go
	    ;; back using `cfd-continue-pos' to fix declarations inside
	    ;; the syntactic ws.
	    (goto-char syntactic-pos)
	    (c-forward-syntactic-ws)
	    (setq cfd-match-pos c-find-decl-match-pos
		  cfd-continue-pos syntactic-pos)
	    (if (< cfd-continue-pos (point))
		(setq cfd-token-pos (point))))

	(setq c-find-decl-syntactic-pos syntactic-pos)

	(when (if (bobp)
		  ;; Always consider bob a match to get the first declaration
		  ;; in the file.  Do this separately instead of letting
		  ;; `c-decl-prefix-re' match bob, so that it always can
		  ;; consume at least one character to ensure that we won't
		  ;; get stuck in an infinite loop.
		  (setq cfd-re-match 0)
		(backward-char)
		(c-beginning-of-current-token)
		(< (point) cfd-limit))
	  ;; Do an initial search now.  In the bob case above it's only done
	  ;; to search for the `c-type' property.
	  (c-find-decl-prefix-search))

	;; Advance `cfd-continue-pos' if we got a hit before the start
	;; position.  The earliest position that could affect after
	;; the start position is the char before the preceding
	;; comments.
	(when (and cfd-continue-pos (< cfd-continue-pos start-pos))
	  (goto-char syntactic-pos)
	  (c-backward-comments)
	  (unless (bobp)
	    (backward-char)
	    (c-beginning-of-current-token))
	  (setq cfd-continue-pos (max cfd-continue-pos (point))))

	;; If we got a match it's always outside macros and comments so
	;; advance to the next token and set `cfd-token-pos'.  The loop
	;; below will later go back using `cfd-continue-pos' to fix
	;; declarations inside the syntactic ws.
	(when (and (< cfd-match-pos cfd-limit) (< (point) syntactic-pos))
	  (goto-char syntactic-pos)
	  (c-forward-syntactic-ws)
	  (and cfd-continue-pos
	       (< cfd-continue-pos (point))
	       (setq cfd-token-pos (point))))

	(setq c-find-decl-match-pos (and (< cfd-match-pos start-pos)
					 cfd-match-pos))))

    ;; Now loop.  We already got the first match.

    (while (progn
	     (while (and
		     (< cfd-match-pos cfd-limit)

		     (or
		      ;; Kludge to filter out matches on the "<" that
		      ;; aren't open parens, for the sake of languages
		      ;; that got `c-recognize-<>-arglists' set.
		      (and (eq (char-before cfd-match-pos) ?<)
			   (not (c-get-char-property (1- cfd-match-pos)
						     'syntax-table)))

		      ;; If `cfd-continue-pos' is less or equal to
		      ;; `cfd-token-pos', we've got a hit inside a macro
		      ;; that's in the syntactic whitespace before the last
		      ;; "real" declaration we've checked.  If they're equal
		      ;; we've arrived at the declaration a second time, so
		      ;; there's nothing to do.
		      (= cfd-continue-pos cfd-token-pos)

		      (progn
			;; If `cfd-continue-pos' is less than `cfd-token-pos'
			;; we're still searching for declarations embedded in
			;; the syntactic whitespace.  In that case we need
			;; only to skip comments and not macros, since they
			;; can't be nested, and that's already been done in
			;; `c-find-decl-prefix-search'.
			(when (> cfd-continue-pos cfd-token-pos)
			  (c-forward-syntactic-ws)
			  (setq cfd-token-pos (point)))

			;; Continue if the following token fails the
			;; CFD-DECL-RE and CFD-FACE-CHECKLIST checks.
			(when (or (>= (point) cfd-limit)
				  (not (looking-at cfd-decl-re))
				  (and cfd-face-checklist
				       (not (c-got-face-at
					     (point) cfd-face-checklist))))
			  (goto-char cfd-continue-pos)
			  t)))

		     (< (point) cfd-limit))
	       (c-find-decl-prefix-search))

	     (< (point) cfd-limit))

      (when (progn
	      ;; Narrow to the end of the macro if we got a hit inside
	      ;; one, to avoid recognizing things that start inside
	      ;; the macro and end outside it.
	      (when (> cfd-match-pos cfd-macro-end)
		;; Not in the same macro as in the previous round.
		(save-excursion
		  (goto-char cfd-match-pos)
		  (setq cfd-macro-end
			(if (save-excursion (and (c-beginning-of-macro)
						 (< (point) cfd-match-pos)))
			    (progn (c-end-of-macro)
				   (point))
			  0))))

	      (if (zerop cfd-macro-end)
		  t
		(if (> cfd-macro-end (point))
		    (progn (narrow-to-region (point-min) cfd-macro-end)
			   t)
		  ;; The matched token was the last thing in the
		  ;; macro, so the whole match is bogus.
		  (setq cfd-macro-end 0)
		  nil)))

	(c-debug-put-decl-spot-faces cfd-match-pos (point))
	(funcall cfd-fun cfd-match-pos (/= cfd-macro-end 0))

	(when (/= cfd-macro-end 0)
	  ;; Restore limits if we did macro narrowment above.
	  (narrow-to-region (point-min) cfd-buffer-end)))

      (goto-char cfd-continue-pos)
      (if (= cfd-continue-pos cfd-limit)
	  (setq cfd-match-pos cfd-limit)
	(c-find-decl-prefix-search)))))


;; A cache for found types.

;; Buffer local variable that contains an obarray with the types we've
;; found.  If a declaration is recognized somewhere we record the
;; fully qualified identifier in it to recognize it as a type
;; elsewhere in the file too.  This is not accurate since we do not
;; bother with the scoping rules of the languages, but in practice the
;; same name is seldom used as both a type and something else in a
;; file, and we only use this as a last resort in ambiguous cases (see
;; `c-font-lock-declarations').
(defvar c-found-types nil)
(make-variable-buffer-local 'c-found-types)

(defsubst c-clear-found-types ()
  ;; Clears `c-found-types'.
  ;;
  ;; This function does not do any hidden buffer changes.
  (setq c-found-types (make-vector 53 0)))

(defun c-add-type (from to)
  ;; Add the given region as a type in `c-found-types'.  If the region
  ;; doesn't match an existing type but there is a type which is equal
  ;; to the given one except that the last character is missing, then
  ;; the shorter type is removed.  That's done to avoid adding all
  ;; prefixes of a type as it's being entered and font locked.  This
  ;; doesn't cover cases like when characters are removed from a type
  ;; or added in the middle.  We'd need the position of point when the
  ;; font locking is invoked to solve this well.
  (unless (and c-recognize-<>-arglists
	       (save-excursion
		 (goto-char from)
		 (c-syntactic-re-search-forward "<" to t)))
    ;; To avoid storing very long strings, do not add a type that
    ;; contains '<' in languages with angle bracket arglists, since
    ;; the type then probably contains a C++ template spec and those
    ;; can be fairly sized programs in themselves.
    (let ((type (c-syntactic-content from to)))
      (unless (intern-soft type c-found-types)
	(unintern (substring type 0 -1) c-found-types)
	(intern type c-found-types)))))

(defsubst c-check-type (from to)
  ;; Return non-nil if the given region contains a type in
  ;; `c-found-types'.
  (intern-soft (c-syntactic-content from to) c-found-types))

(defun c-list-found-types ()
  ;; Return all the types in `c-found-types' as a sorted list of
  ;; strings.
  (let (type-list)
    (mapatoms (lambda (type)
		(setq type-list (cons (symbol-name type)
				      type-list)))
	      c-found-types)
    (sort type-list 'string-lessp)))


;; Handling of small scale constructs like types and names.

(defun c-remove-<>-arglist-properties (from to)
  ;; Remove all the properties put by `c-forward-<>-arglist' in the
  ;; specified region.  Point is clobbered.
  (goto-char from)
  (while (progn (skip-chars-forward "^<>," to)
		(< (point) to))
    (if (eq (char-after) ?,)
	(when (eq (c-get-char-property (point) 'c-type) 'c-<>-arg-sep)
	  (c-clear-char-property (point) 'c-type))
      (c-clear-char-property (point) 'syntax-table))
    (forward-char)))

;; Dynamically bound variable that instructs `c-forward-type' to also
;; treat possible types (i.e. those that it normally returns 'maybe or
;; 'found for) as actual types (and always return 'found for them).
;; This means that it records them in `c-record-type-identifiers' if
;; that is set, and that it adds them to `c-found-types'.
(defvar c-promote-possible-types nil)

;; Dynamically bound variable that instructs `c-forward-<>-arglist' to
;; not accept arglists that contain binary operators.
;;
;; This is primarily used to handle C++ template arglists.  C++
;; disambiguates them by checking whether the preceding name is a
;; template or not.  We can't do that, so we assume it is a template
;; if it can be parsed as one.  That usually works well since
;; comparison expressions on the forms "a < b > c" or "a < b, c > d"
;; in almost all cases would be pointless.
;;
;; However, in function arglists, e.g. in "foo (a < b, c > d)", we
;; should let the comma separate the function arguments instead.  And
;; in a context where the value of the expression is taken, e.g. in
;; "if (a < b || c > d)", it's probably not a template.
(defvar c-restricted-<>-arglists nil)

;; Dynamically bound variables that instructs `c-forward-name',
;; `c-forward-type' and `c-forward-<>-arglist' to record the ranges of
;; all the type and reference identifiers they encounter.  They will
;; build lists on these variables where each element is a cons of the
;; buffer positions surrounding each identifier.  This recording is
;; only activated when `c-record-type-identifiers' is non-nil.
;;
;; All known types that can't be identifiers are recorded, and also
;; other possible types if `c-promote-possible-types' is set.
;; Recording is however disabled inside angle bracket arglists that
;; are encountered inside names and other angle bracket arglists.
;; Such occurences are taken care of by `c-font-lock-<>-arglists'
;; instead.
;;
;; Only the names in C++ template style references (e.g. "tmpl" in
;; "tmpl<a,b>::foo") are recorded as references, other references
;; aren't handled here.
(defvar c-record-type-identifiers nil)
(defvar c-record-ref-identifiers nil)

;; If `c-record-type-identifiers' is set, this will receive a cons
;; cell of the range of the last single identifier symbol stepped over
;; by `c-forward-name' if it's successful.  This is the range that
;; should be put on one of the record lists by the caller.  It's
;; assigned nil if there's no such symbol in the name.
(defvar c-last-identifier-range nil)

(defmacro c-record-type-id (range)
  (if (eq (car-safe range) 'cons)
      ;; Always true.
      `(setq c-record-type-identifiers
	     (cons ,range c-record-type-identifiers))
    `(let ((range ,range))
       (if range
	   (setq c-record-type-identifiers
		 (cons range c-record-type-identifiers))))))

(defmacro c-record-ref-id (range)
  (if (eq (car-safe range) 'cons)
      ;; Always true.
      `(setq c-record-ref-identifiers
	     (cons ,range c-record-ref-identifiers))
    `(let ((range ,range))
       (if range
	   (setq c-record-ref-identifiers
		 (cons range c-record-ref-identifiers))))))

;; Dynamically bound variable that instructs `c-forward-type' to
;; record the ranges of types that only are found.  Behaves otherwise
;; like `c-record-type-identifiers'.
(defvar c-record-found-types nil)

(defmacro c-forward-keyword-prefixed-id (type)
  ;; Used internally in `c-forward-keyword-clause' to move forward
  ;; over a type (if TYPE is 'type) or a name (otherwise) which
  ;; possibly is prefixed by keywords and their associated clauses.
  ;; Try with a type/name first to not trip up on those that begin
  ;; with a keyword.  Return t if a known or found type is moved
  ;; over.  The point is clobbered if nil is returned.  If range
  ;; recording is enabled, the identifier is recorded on as a type
  ;; if TYPE is 'type or as a reference if TYPE is 'ref.
  `(let (res)
     (while (if (setq res ,(if (eq type 'type)
			       `(c-forward-type)
			     `(c-forward-name)))
		nil
	      (and (looking-at c-keywords-regexp)
		   (c-forward-keyword-clause))))
     (when (memq res '(t known found prefix))
       ,(when (eq type 'ref)
	  `(when c-record-type-identifiers
	     (c-record-ref-id c-last-identifier-range)))
       t)))

(defmacro c-forward-id-comma-list (type)
  ;; Used internally in `c-forward-keyword-clause' to move forward
  ;; over a comma separated list of types or names using
  ;; `c-forward-keyword-prefixed-id'.
  `(while (and (progn
		 (setq safe-pos (point))
		 (eq (char-after) ?,))
	       (progn
		 (forward-char)
		 (c-forward-syntactic-ws)
		 (c-forward-keyword-prefixed-id ,type)))))

(defun c-forward-keyword-clause ()
  ;; The first submatch in the current match data is assumed to
  ;; surround a token.  If it's a keyword, move over it and any
  ;; following clauses associated with it, stopping at the next
  ;; following token.  t is returned in that case, otherwise the point
  ;; stays and nil is returned.  The kind of clauses that are
  ;; recognized are those specified by `c-type-list-kwds',
  ;; `c-ref-list-kwds', `c-colon-type-list-kwds',
  ;; `c-paren-nontype-kwds', `c-paren-type-kwds', `c-<>-type-kwds',
  ;; and `c-<>-arglist-kwds'.

  (let ((kwd-sym (c-keyword-sym (match-string 1))) safe-pos pos)
    (when kwd-sym
      (goto-char (match-end 1))
      (c-forward-syntactic-ws)
      (setq safe-pos (point))

      (cond
       ((and (c-keyword-member kwd-sym 'c-type-list-kwds)
	     (c-forward-keyword-prefixed-id type))
	;; There's a type directly after a keyword in `c-type-list-kwds'.
	(c-forward-id-comma-list type))

       ((and (c-keyword-member kwd-sym 'c-ref-list-kwds)
	     (c-forward-keyword-prefixed-id ref))
	;; There's a name directly after a keyword in `c-ref-list-kwds'.
	(c-forward-id-comma-list ref))

       ((and (c-keyword-member kwd-sym 'c-paren-any-kwds)
	     (eq (char-after) ?\())
	;; There's an open paren after a keyword in `c-paren-any-kwds'.

	(forward-char)
	(when (and (setq pos (c-up-list-forward))
		   (eq (char-before pos) ?\)))
	  (when (and c-record-type-identifiers
		     (c-keyword-member kwd-sym 'c-paren-type-kwds))
	    ;; Use `c-forward-type' on every identifier we can find
	    ;; inside the paren, to record the types.
	    (while (c-syntactic-re-search-forward c-symbol-start pos t)
	      (goto-char (match-beginning 0))
	      (unless (c-forward-type)
		(looking-at c-symbol-key) ; Always matches.
		(goto-char (match-end 0)))))

	  (goto-char pos)
	  (c-forward-syntactic-ws)
	  (setq safe-pos (point))))

       ((and (c-keyword-member kwd-sym 'c-<>-sexp-kwds)
	     (eq (char-after) ?<)
	     (c-forward-<>-arglist (c-keyword-member kwd-sym 'c-<>-type-kwds)
				   (or c-record-type-identifiers
				       c-restricted-<>-arglists)))
	(c-forward-syntactic-ws)
	(setq safe-pos (point)))

       ((and (c-keyword-member kwd-sym 'c-nonsymbol-sexp-kwds)
	     (not (looking-at c-symbol-start))
	     (c-safe (c-forward-sexp) t))
	(c-forward-syntactic-ws)
	(setq safe-pos (point))))

      (when (and (c-keyword-member kwd-sym 'c-colon-type-list-kwds)
		 (progn
		   ;; If a keyword matched both one of the types above and
		   ;; this one, we match `c-colon-type-list-re' after the
		   ;; clause matched above.
		   (goto-char safe-pos)
		   (looking-at c-colon-type-list-re))
		 (progn
		   (goto-char (match-end 0))
		   (c-forward-syntactic-ws)
		   (c-forward-keyword-prefixed-id type)))
	;; There's a type after the `c-colon-type-list-re'
	;; match after a keyword in `c-colon-type-list-kwds'.
	(c-forward-id-comma-list type))

      (goto-char safe-pos)
      t)))

(defun c-forward-<>-arglist (all-types reparse)
  ;; The point is assumed to be at a '<'.  Try to treat it as the open
  ;; paren of an angle bracket arglist and move forward to the the
  ;; corresponding '>'.  If successful, the point is left after the
  ;; '>' and t is returned, otherwise the point isn't moved and nil is
  ;; returned.  If ALL-TYPES is t then all encountered arguments in
  ;; the arglist that might be types are treated as found types.
  ;;
  ;; The surrounding '<' and '>' are given syntax-table properties to
  ;; make them behave like parentheses.  Each argument separating ','
  ;; is also set to `c-<>-arg-sep' in the `c-type' property.  These
  ;; properties are also cleared in a relevant region forward from the
  ;; point if they seems to be set and it turns out to not be an
  ;; arglist.
  ;;
  ;; If the arglist has been successfully parsed before then paren
  ;; syntax properties will be exploited to quickly jump to the end,
  ;; but that can be disabled by setting REPARSE to t.  That is
  ;; necessary if the various side effects, e.g. recording of type
  ;; ranges, are important.  Setting REPARSE to t only applies
  ;; recursively to nested angle bracket arglists if
  ;; `c-restricted-<>-arglists' is set.

  (let ((start (point))
	;; If `c-record-type-identifiers' is set then activate
	;; recording of any found types that constitute an argument in
	;; the arglist.
	(c-record-found-types (if c-record-type-identifiers t)))
    (if (catch 'angle-bracket-arglist-escape
	  (setq c-record-found-types
		(c-forward-<>-arglist-recur all-types reparse)))
	(progn
	  (when (consp c-record-found-types)
	    (setq c-record-type-identifiers
		  ;; `nconc' doesn't mind that the tail of
		  ;; `c-record-found-types' is t.
		  (nconc c-record-found-types c-record-type-identifiers)))
	  t)

      (goto-char start)
      nil)))

(defun c-forward-<>-arglist-recur (all-types reparse)
  ;; Recursive part of `c-forward-<>-arglist'.

  (let ((start (point)) res pos tmp
	;; Cover this so that any recorded found type ranges are
	;; automatically lost if it turns out to not be an angle
	;; bracket arglist.  It's propagated through the return value
	;; on successful completion.
	(c-record-found-types c-record-found-types)
	;; List that collects the positions after the argument
	;; separating ',' in the arglist.
	arg-start-pos)

    ;; If the '<' has paren open syntax then we've marked it as an
    ;; angle bracket arglist before, so try to skip to the end and see
    ;; that the close paren matches.
    (if (and (c-get-char-property (point) 'syntax-table)
	     (progn
	       (forward-char)
	       (if (and (not (looking-at c-<-op-cont-regexp))
			(if (c-parse-sexp-lookup-properties)
			    (c-go-up-list-forward)
			  (catch 'at-end
			    (let ((depth 1))
			      (while (c-syntactic-re-search-forward
				      "[<>]" nil t t)
				(when (c-get-char-property (1- (point))
							   'syntax-table)
				  (if (eq (char-before) ?<)
				      (setq depth (1+ depth))
				    (setq depth (1- depth))
				    (when (= depth 0) (throw 'at-end t)))))
			      nil)))
			(not (looking-at c->-op-cont-regexp))
			(save-excursion
			  (backward-char)
			  (= (point)
			     (progn (c-beginning-of-current-token)
				    (point)))))

		   ;; Got an arglist that appears to be valid.
		   (if reparse
		       ;; Reparsing is requested, so zap the properties in the
		       ;; region and go on to redo it.  It's done here to
		       ;; avoid leaving it behind if we exit through
		       ;; `angle-bracket-arglist-escape' below.
		       (progn
			 (c-remove-<>-arglist-properties start (point))
			 (goto-char start)
			 nil)
		     t)

		 ;; Got unmatched paren brackets or either paren was
		 ;; actually some other token.  Recover by clearing the
		 ;; syntax properties on all the '<' and '>' in the
		 ;; range where we'll search for the arglist below.
		 (goto-char start)
		 (while (progn (skip-chars-forward "^<>,;{}")
			       (looking-at "[<>,]"))
		   (if (eq (char-after) ?,)
		       (when (eq (c-get-char-property (point) 'c-type)
				 'c-<>-arg-sep)
			 (c-clear-char-property (point) 'c-type))
		     (c-clear-char-property (point) 'syntax-table))
		   (forward-char))
		 (goto-char start)
		 nil)))
	t

      (forward-char)
      (unless (looking-at c-<-op-cont-regexp)
	(while (and
		(progn

		  (when c-record-type-identifiers
		    (if all-types

			;; All encountered identifiers are types, so set the
			;; promote flag and parse the type.
			(progn
			  (c-forward-syntactic-ws)
			  (when (looking-at c-identifier-start)
			    (let ((c-promote-possible-types t))
			      (c-forward-type))))

		      ;; Check if this arglist argument is a sole type.  If
		      ;; it's known then it's recorded in
		      ;; `c-record-type-identifiers'.  If it only is found
		      ;; then it's recorded in `c-record-found-types' which we
		      ;; might roll back if it turns out that this isn't an
		      ;; angle bracket arglist afterall.
		      (when (memq (char-before) '(?, ?<))
			(let ((orig-record-found-types c-record-found-types))
			  (c-forward-syntactic-ws)
			  (and (memq (c-forward-type) '(known found))
			       (not (looking-at "[,>]"))
			       ;; A found type was recorded but it's not the
			       ;; only thing in the arglist argument, so reset
			       ;; `c-record-found-types'.
			       (setq c-record-found-types
				     orig-record-found-types))))))

		  (setq pos (point))
		  (or (when (eq (char-after) ?>)
			;; Must check for '>' at the very start separately,
			;; since the regexp below has to avoid ">>" without
			;; using \\=.
			(forward-char)
			t)

		      ;; Note: These regexps exploit the match order in \| so
		      ;; that "<>" is matched by "<" rather than "[^>:-]>".
		      (c-syntactic-re-search-forward
		       (if c-restricted-<>-arglists
			   ;; Stop on ',', '|', '&', '+' and '-' to catch
			   ;; common binary operators that could be between
			   ;; two comparison expressions "a<b" and "c>d".
			   "[<;{},|&+-]\\|\\([^>:-]>\\)"
			 ;; Otherwise we still stop on ',' to find the
			 ;; argument start positions.
			 "[<;{},]\\|\\([^>:-]>\\)")
		       nil 'move t t 1)

		      ;; If the arglist starter has lost its open paren
		      ;; syntax but not the closer, we won't find the
		      ;; closer above since we only search in the
		      ;; balanced sexp.  In that case we stop just short
		      ;; of it so check if the following char is the closer.
		      (when (eq (char-after) ?>)
			;; Remove its syntax so that we don't enter the
			;; recovery code below.  That's not necessary
			;; since there's no real reason to suspect that
			;; things inside the arglist are unbalanced.
			(c-clear-char-property (point) 'syntax-table)
			(forward-char)
			t)))

		(cond
		 ((eq (char-before) ?>)
		  ;; Either an operator starting with '>' or the end of
		  ;; the angle bracket arglist.

		  (if (and (/= (1- (point)) pos)
			   (c-get-char-property (1- (point)) 'syntax-table)
			   (progn
			     (c-clear-char-property (1- (point)) 'syntax-table)
			     (c-parse-sexp-lookup-properties)))

		      ;; We've skipped past a list that ended with '>'.  It
		      ;; must be unbalanced since nested arglists are handled
		      ;; in the case below.  Recover by removing all paren
		      ;; properties on '<' and '>' in the searched region and
		      ;; redo the search.
		      (progn
			(c-remove-<>-arglist-properties pos (point))
			(goto-char pos)
			t)

		    (if (looking-at c->-op-cont-regexp)
			(progn
			  (when (text-property-not-all
				 (1- (point)) (match-end 0) 'syntax-table nil)
			    (c-remove-<>-arglist-properties (1- (point))
							    (match-end 0)))
			  (goto-char (match-end 0))
			  t)

		      ;; The angle bracket arglist is finished.
		      (while arg-start-pos
			(c-put-char-property (1- (car arg-start-pos))
					     'c-type 'c-<>-arg-sep)
			(setq arg-start-pos (cdr arg-start-pos)))
		      (c-mark-<-as-paren start)
		      (c-mark->-as-paren (1- (point)))
		      (setq res t)
		      nil)))

		 ((eq (char-before) ?<)
		  ;; Either an operator starting with '<' or a nested arglist.

		  (setq pos (point))
		  (let (id-start id-end subres keyword-match)
		    (if (if (looking-at c-<-op-cont-regexp)
			    (setq tmp (match-end 0))
			  (setq tmp pos)
			  (backward-char)
			  (not
			   (and

			    (save-excursion
			      ;; There's always an identifier before a angle
			      ;; bracket arglist, or a keyword in
			      ;; `c-<>-type-kwds' or `c-<>-arglist-kwds'.
			      (c-backward-syntactic-ws)
			      (setq id-end (point))
			      (c-simple-skip-symbol-backward)
			      (when (or (setq keyword-match
					      (looking-at c-opt-<>-sexp-key))
					(not (looking-at c-keywords-regexp)))
				(setq id-start (point))))

			    (setq subres
				  (let ((c-record-type-identifiers nil)
					(c-record-found-types nil))
				    (c-forward-<>-arglist-recur
				     (and keyword-match
					  (c-keyword-member
					   (c-keyword-sym (match-string 1))
					   'c-<>-type-kwds))
				     (and reparse
					  c-restricted-<>-arglists))))
			    )))

			;; It was not an angle bracket arglist.
			(progn
			  (when (text-property-not-all
				 (1- pos) tmp 'syntax-table nil)
			    (if (c-parse-sexp-lookup-properties)
				;; Got an invalid open paren syntax on this
				;; '<'.  We'll probably get an unbalanced '>'
				;; further ahead if we just remove the syntax
				;; here, so recover by removing all paren
				;; properties up to and including the
				;; balancing close paren.
				(parse-partial-sexp pos (point-max) -1)
			      (goto-char tmp))
			    (c-remove-<>-arglist-properties pos (point)))
			  (goto-char tmp))

		      ;; It was an angle bracket arglist.
		      (setq c-record-found-types subres)

		      ;; Record the identifier before the template as a type
		      ;; or reference depending on whether the arglist is last
		      ;; in a qualified identifier.
		      (when (and c-record-type-identifiers
				 (not keyword-match))
			(if (and c-opt-identifier-concat-key
				 (progn
				   (c-forward-syntactic-ws)
				   (looking-at c-opt-identifier-concat-key)))
			    (c-record-ref-id (cons id-start id-end))
			  (c-record-type-id (cons id-start id-end))))))
		  t)

		 ((and (eq (char-before) ?,)
		       (not c-restricted-<>-arglists))
		  ;; Just another argument.  Record the position.  The
		  ;; type check stuff that made us stop at it is at
		  ;; the top of the loop.
		  (setq arg-start-pos (cons (point) arg-start-pos)))

		 (t
		  ;; Got a character that can't be in an angle bracket
		  ;; arglist argument.  Abort using `throw', since
		  ;; it's useless to try to find a surrounding arglist
		  ;; if we're nested.
		  (throw 'angle-bracket-arglist-escape nil))))))

      (if res
	  (or c-record-found-types t)))))

(defun c-forward-name ()
  ;; Move forward over a complete name if at the beginning of one,
  ;; stopping at the next following token.  If the point is not at
  ;; something that are recognized as name then it stays put.  A name
  ;; could be something as simple as "foo" in C or something as
  ;; complex as "X<Y<class A<int>::B, BIT_MAX >> b>, ::operator<> ::
  ;; Z<(a>b)> :: operator const X<&foo>::T Q::G<unsigned short
  ;; int>::*volatile const" in C++ (this function is actually little
  ;; more than a `looking-at' call in all modes except those that,
  ;; like C++, have `c-recognize-<>-arglists' set).  Return nil if no
  ;; name is found, 'template if it's an identifier ending with an
  ;; angle bracket arglist, 'operator of it's an operator identifier,
  ;; or t if it's some other kind of name.

  (let ((pos (point)) res id-start id-end
	;; Turn off `c-promote-possible-types' here since we might
	;; call `c-forward-<>-arglist' and we don't want it to promote
	;; every suspect thing in the arglist to a type.  We're
	;; typically called from `c-forward-type' in this case, and
	;; the caller only wants the top level type that it finds to
	;; be promoted.
	c-promote-possible-types)
    (while
	(and
	 (looking-at c-identifier-key)

	 (progn
	   ;; Check for keyword.  We go to the last symbol in
	   ;; `c-identifier-key' first.
	   (if (eq c-identifier-key c-symbol-key)
	       (setq id-start (point)
		     id-end (match-end 0))
	     (goto-char (setq id-end (match-end 0)))
	     (c-simple-skip-symbol-backward)
	     (setq id-start (point)))

	   (if (looking-at c-keywords-regexp)
	       (when (and (c-major-mode-is 'c++-mode)
			  (looking-at
			   (cc-eval-when-compile
			     (concat "\\(operator\\|\\(template\\)\\)"
				     "\\(" (c-lang-const c-nonsymbol-key c++)
				     "\\|$\\)")))
			  (if (match-beginning 2)
			      ;; "template" is only valid inside an
			      ;; identifier if preceded by "::".
			      (save-excursion
				(c-backward-syntactic-ws)
				(and (c-safe (backward-char 2) t)
				     (looking-at "::")))
			    t))

		 ;; Handle a C++ operator or template identifier.
		 (goto-char id-end)
		 (c-forward-syntactic-ws)
		 (cond ((eq (char-before id-end) ?e)
			;; Got "... ::template".
			(let ((subres (c-forward-name)))
			  (when subres
			    (setq pos (point)
				  res subres))))

		       ((looking-at c-identifier-start)
			;; Got a cast operator.
			(when (c-forward-type)
			  (setq pos (point)
				res 'operator)
			  ;; Now we should match a sequence of either
			  ;; '*', '&' or a name followed by ":: *",
			  ;; where each can be followed by a sequence
			  ;; of `c-opt-type-modifier-key'.
			  (while (cond ((looking-at "[*&]")
					(goto-char (match-end 0))
					t)
				       ((looking-at c-identifier-start)
					(and (c-forward-name)
					     (looking-at "::")
					     (progn
					       (goto-char (match-end 0))
					       (c-forward-syntactic-ws)
					       (eq (char-after) ?*))
					     (progn
					       (forward-char)
					       t))))
			    (while (progn
				     (c-forward-syntactic-ws)
				     (setq pos (point))
				     (looking-at c-opt-type-modifier-key))
			      (goto-char (match-end 1))))))

		       ((looking-at c-overloadable-operators-regexp)
			;; Got some other operator.
			(when c-record-type-identifiers
			  (setq c-last-identifier-range
				(cons (point) (match-end 0))))
			(goto-char (match-end 0))
			(c-forward-syntactic-ws)
			(setq pos (point)
			      res 'operator)))

		 nil)

	     (when c-record-type-identifiers
	       (setq c-last-identifier-range
		     (cons id-start id-end)))
	     (goto-char id-end)
	     (c-forward-syntactic-ws)
	     (setq pos (point)
		   res t)))

	 (progn
	   (goto-char pos)
	   (when (or c-opt-identifier-concat-key
		     c-recognize-<>-arglists)

	     (cond
	      ((and c-opt-identifier-concat-key
		    (looking-at c-opt-identifier-concat-key))
	       ;; Got a concatenated identifier.  This handles the
	       ;; cases with tricky syntactic whitespace that aren't
	       ;; covered in `c-identifier-key'.
	       (goto-char (match-end 0))
	       (c-forward-syntactic-ws)
	       t)

	      ((and c-recognize-<>-arglists
		    (eq (char-after) ?<))
	       ;; Maybe an angle bracket arglist.
	       (when (let ((c-record-type-identifiers nil)
			   (c-record-found-types nil))
		       (c-forward-<>-arglist
			nil c-restricted-<>-arglists))
		 (c-forward-syntactic-ws)
		 (setq pos (point))
		 (if (and c-opt-identifier-concat-key
			  (looking-at c-opt-identifier-concat-key))
		     ;; Continue if there's an identifier concatenation
		     ;; operator after the template argument.
		     (progn
		       (when c-record-type-identifiers
			 (c-record-ref-id (cons id-start id-end))
			 (setq c-last-identifier-range nil))
		       (forward-char 2)
		       (c-forward-syntactic-ws)
		       t)
		   ;; `c-add-type' isn't called here since we don't
		   ;; want to add types containing angle bracket
		   ;; arglists.
		   (when c-record-type-identifiers
		     (c-record-type-id (cons id-start id-end))
		     (setq c-last-identifier-range nil))
		   (setq res 'template)
		   nil)))
	      )))))

    (goto-char pos)
    res))

(defun c-forward-type ()
  ;; Move forward over a type spec if at the beginning of one,
  ;; stopping at the next following token.  Return t if it's a known
  ;; type that can't be a name or other expression, 'known if it's an
  ;; otherwise known type (according to `*-font-lock-extra-types'),
  ;; 'prefix if it's a known prefix of a type, 'found if it's a type
  ;; that matches one in `c-found-types', 'maybe if it's an identfier
  ;; that might be a type, or nil if it can't be a type (the point
  ;; isn't moved then).  The point is assumed to be at the beginning
  ;; of a token.
  ;;
  ;; Note that this function doesn't skip past the brace definition
  ;; that might be considered part of the type, e.g.
  ;; "enum {a, b, c} foo".
  (let ((start (point)) pos res res2 id-start id-end id-range)

    ;; Skip leading type modifiers.  If any are found we know it's a
    ;; prefix of a type.
    (when c-opt-type-modifier-key
      (while (looking-at c-opt-type-modifier-key)
	(goto-char (match-end 1))
	(c-forward-syntactic-ws)
	(setq res 'prefix)))

    (cond
     ((looking-at c-type-prefix-key)
      ;; Looking at a keyword that prefixes a type identifier,
      ;; e.g. "class".
      (goto-char (match-end 1))
      (c-forward-syntactic-ws)
      (setq pos (point))
      (if (memq (setq res2 (c-forward-name)) '(t template))
	  (progn
	    (when (eq res2 t)
	      ;; In many languages the name can be used without the
	      ;; prefix, so we add it to `c-found-types'.
	      (c-add-type pos (point))
	      (when c-record-type-identifiers
		(c-record-type-id c-last-identifier-range)))
	    (setq res t))
	;; Invalid syntax.
	(goto-char start)
	(setq res nil)))

     ((progn
	(setq pos nil)
	(if (looking-at c-identifier-start)
	    (save-excursion
	      (setq id-start (point)
		    res2 (c-forward-name))
	      (when res2
		(setq id-end (point)
		      id-range c-last-identifier-range))))
	(and (cond ((looking-at c-primitive-type-key)
		    (setq res t))
		   ((c-with-syntax-table c-identifier-syntax-table
		      (looking-at c-known-type-key))
		    (setq res 'known)))
	     (or (not id-end)
		 (>= (save-excursion
		       (save-match-data
			 (goto-char (match-end 1))
			 (c-forward-syntactic-ws)
			 (setq pos (point))))
		     id-end)
		 (setq res nil))))
      ;; Looking at a primitive or known type identifier.  We've
      ;; checked for a name first so that we don't go here if the
      ;; known type match only is a prefix of another name.

      (setq id-end (match-end 1))

      (when (and c-record-type-identifiers
		 (or c-promote-possible-types (eq res t)))
	(c-record-type-id (cons (match-beginning 1) (match-end 1))))

      (if (and c-opt-type-component-key
	       (save-match-data
		 (looking-at c-opt-type-component-key)))
	  ;; There might be more keywords for the type.
	  (let (safe-pos)
	    (c-forward-keyword-clause)
	    (while (progn
		     (setq safe-pos (point))
		     (looking-at c-opt-type-component-key))
	      (when (and c-record-type-identifiers
			 (looking-at c-primitive-type-key))
		(c-record-type-id (cons (match-beginning 1)
					(match-end 1))))
	      (c-forward-keyword-clause))
	    (if (looking-at c-primitive-type-key)
		(progn
		  (when c-record-type-identifiers
		    (c-record-type-id (cons (match-beginning 1)
					    (match-end 1))))
		  (c-forward-keyword-clause)
		  (setq res t))
	      (goto-char safe-pos)
	      (setq res 'prefix)))
	(unless (save-match-data (c-forward-keyword-clause))
	  (if pos
	      (goto-char pos)
	    (goto-char (match-end 1))
	    (c-forward-syntactic-ws)))))

     (res2
      (cond ((eq res2 t)
	     ;; A normal identifier.
	     (goto-char id-end)
	     (if (or res c-promote-possible-types)
		 (progn
		   (c-add-type id-start id-end)
		   (when c-record-type-identifiers
		     (c-record-type-id id-range))
		   (unless res
		     (setq res 'found)))
	       (setq res (if (c-check-type id-start id-end)
			     ;; It's an identifier that has been used as
			     ;; a type somewhere else.
			     'found
			   ;; It's an identifier that might be a type.
			   'maybe))))
	    ((eq res2 'template)
	     ;; A template is a type.
	     (goto-char id-end)
	     (setq res t))
	    (t
	     ;; Otherwise it's an operator identifier, which is not a type.
	     (goto-char start)
	     (setq res nil)))))

    (when res
      ;; Skip trailing type modifiers.  If any are found we know it's
      ;; a type.
      (when c-opt-type-modifier-key
	(while (looking-at c-opt-type-modifier-key)
	  (goto-char (match-end 1))
	  (c-forward-syntactic-ws)
	  (setq res t)))

      ;; Step over any type suffix operator.  Do not let the existence
      ;; of these alter the classification of the found type, since
      ;; these operators typically are allowed in normal expressions
      ;; too.
      (when c-opt-type-suffix-key
	(while (looking-at c-opt-type-suffix-key)
	  (goto-char (match-end 1))
	  (c-forward-syntactic-ws)))

      (when c-opt-type-concat-key
	;; Look for a trailing operator that concatenate the type with
	;; a following one, and if so step past that one through a
	;; recursive call.
	(setq pos (point))
	(let* ((c-promote-possible-types (or (memq res '(t known))
					     c-promote-possible-types))
	       ;; If we can't promote then set `c-record-found-types' so that
	       ;; we can merge in the types from the second part afterwards if
	       ;; it turns out to be a known type there.
	       (c-record-found-types (and c-record-type-identifiers
					  (not c-promote-possible-types))))
	  (if (and (looking-at c-opt-type-concat-key)

		   (progn
		     (goto-char (match-end 1))
		     (c-forward-syntactic-ws)
		     (setq res2 (c-forward-type))))

	      (progn
		;; If either operand certainly is a type then both are, but we
		;; don't let the existence of the operator itself promote two
		;; uncertain types to a certain one.
		(cond ((eq res t))
		      ((eq res2 t)
		       (c-add-type id-start id-end)
		       (when c-record-type-identifiers
			 (c-record-type-id id-range))
		       (setq res t))
		      ((eq res 'known))
		      ((eq res2 'known)
		       (setq res 'known))
		      ((eq res 'found))
		      ((eq res2 'found)
		       (setq res 'found))
		      (t
		       (setq res 'maybe)))

		(when (and (eq res t)
			   (consp c-record-found-types))
		  ;; Merge in the ranges of any types found by the second
		  ;; `c-forward-type'.
		  (setq c-record-type-identifiers
			;; `nconc' doesn't mind that the tail of
			;; `c-record-found-types' is t.
			(nconc c-record-found-types
			       c-record-type-identifiers))))

	    (goto-char pos))))

      (when (and c-record-found-types (memq res '(known found)) id-range)
	(setq c-record-found-types
	      (cons id-range c-record-found-types))))

    ;;(message "c-forward-type %s -> %s: %s" start (point) res)

    res))


;; Handling of large scale constructs like statements and declarations.

(defun c-beginning-of-inheritance-list (&optional lim)
  ;; Go to the first non-whitespace after the colon that starts a
  ;; multiple inheritance introduction.  Optional LIM is the farthest
  ;; back we should search.
  (let* ((lim (or lim (save-excursion
			(c-beginning-of-syntax)
			(point)))))
    (c-with-syntax-table c++-template-syntax-table
      (c-backward-token-2 0 t lim)
      (while (and (or (looking-at c-symbol-start)
		      (looking-at "[<,]\\|::"))
		  (zerop (c-backward-token-2 1 t lim))))
      (skip-chars-forward "^:"))))

(defun c-in-method-def-p ()
  ;; Return nil if we aren't in a method definition, otherwise the
  ;; position of the initial [+-].
  (save-excursion
    (beginning-of-line)
    (and c-opt-method-key
	 (looking-at c-opt-method-key)
	 (point))
    ))

;; Contributed by Kevin Ryde <user42@zip.com.au>.
(defun c-in-gcc-asm-p ()
  ;; Return non-nil if point is within a gcc \"asm\" block.
  ;;
  ;; This should be called with point inside an argument list.
  ;;
  ;; Only one level of enclosing parentheses is considered, so for
  ;; instance `nil' is returned when in a function call within an asm
  ;; operand.

  (and c-opt-asm-stmt-key
       (save-excursion
	 (beginning-of-line)
	 (backward-up-list 1)
	 (c-beginning-of-statement-1 (point-min) nil t)
	 (looking-at c-opt-asm-stmt-key))))

(defun c-at-toplevel-p ()
  "Return a determination as to whether point is at the `top-level'.
Being at the top-level means that point is either outside any
enclosing block (such function definition), or only inside a class,
namespace or other block that contains another declaration level.

If point is not at the top-level (e.g. it is inside a method
definition), then nil is returned.  Otherwise, if point is at a
top-level not enclosed within a class definition, t is returned.
Otherwise, a 2-vector is returned where the zeroth element is the
buffer position of the start of the class declaration, and the first
element is the buffer position of the enclosing class's opening
brace."
  (let ((paren-state (c-parse-state)))
    (or (not (c-most-enclosing-brace paren-state))
	(c-search-uplist-for-classkey paren-state))))

(defun c-just-after-func-arglist-p (&optional lim)
  ;; Return non-nil if we are between a function's argument list closing
  ;; paren and its opening brace.  Note that the list close brace
  ;; could be followed by a "const" specifier or a member init hanging
  ;; colon.  LIM is used as bound for some backward buffer searches;
  ;; the search might continue past it.
  ;;
  ;; Note: This test is easily fooled.  It only works reasonably well
  ;; in the situations where `c-guess-basic-syntax' uses it.
  (save-excursion
    (if (c-mode-is-new-awk-p)
        (c-awk-backward-syntactic-ws lim)
      (c-backward-syntactic-ws lim))
    (let ((checkpoint (point)))
      ;; could be looking at const specifier
      (if (and (eq (char-before) ?t)
	       (forward-word -1)
	       (looking-at "\\<const\\>[^_]"))
	  (c-backward-syntactic-ws lim)
	;; otherwise, we could be looking at a hanging member init
	;; colon
	(goto-char checkpoint)
	(while (and
		(eq (char-before) ?,)
		;; this will catch member inits with multiple
		;; line arglists
		(progn
		  (forward-char -1)
		  (c-backward-syntactic-ws (c-point 'bol))
		  (c-safe (c-backward-sexp 1) t))
		(or (not (looking-at "\\s\("))
		    (c-safe (c-backward-sexp 1) t)))
	  (c-backward-syntactic-ws lim))
	(if (and (eq (char-before) ?:)
		 (progn
		   (forward-char -1)
		   (c-backward-syntactic-ws lim)
		   (looking-at "\\([ \t\n]\\|\\\\\n\\)*:\\([^:]+\\|$\\)")))
	    nil
	  (goto-char checkpoint))
	)
      (setq checkpoint (point))
      (and (eq (char-before) ?\))
	   ;; Check that it isn't a cpp expression, e.g. the
	   ;; expression of an #if directive or the "function header"
	   ;; of a #define.
	   (or (not (c-beginning-of-macro))
	       (and (c-forward-to-cpp-define-body)
		    (< (point) checkpoint)))
	   ;; Check if we are looking at an ObjC method def or a class
	   ;; category.
	   (not (and c-opt-method-key
		     (progn
		       (goto-char checkpoint)
		       (c-safe (c-backward-sexp) t))
		     (progn
		       (c-backward-syntactic-ws lim)
		       (or (memq (char-before) '(?- ?+))
			   (and (c-safe (c-forward-sexp -2) t)
				(looking-at c-class-key))))))
	   ;; Pike has compound types that include parens,
	   ;; e.g. "array(string)".  Check that we aren't after one.
	   (not (and (c-major-mode-is 'pike-mode)
		     (progn
		       (goto-char checkpoint)
		       (c-safe (c-backward-sexp 2) t))
		     (looking-at c-primitive-type-key)))
	   ))))

(defun c-in-knr-argdecl (&optional lim)
  ;; Return the position of the first argument declaration if point is
  ;; inside a K&R style argument declaration list, nil otherwise.
  ;; `c-recognize-knr-p' is not checked.  If LIM is non-nil, it's a
  ;; position that bounds the backward search for the argument list.
  ;;
  ;; Note: A declaration level context is assumed; the test can return
  ;; false positives for statements.

  (save-excursion
    (save-restriction

      ;; Go back to the closest preceding normal parenthesis sexp.  We
      ;; take that as the argument list in the function header.  Then
      ;; check that it's followed by some symbol before the next ';'
      ;; or '{'.  If it does, it's the header of the K&R argdecl we're
      ;; in.
      (if lim (narrow-to-region lim (c-point 'eol)))
      (let ((outside-macro (not (c-query-macro-start)))
	    paren-end)

	(catch 'done
	  (while (if (and (setq paren-end (c-down-list-backward (point)))
			  (eq (char-after paren-end) ?\)))
		     (progn
		       (goto-char (1+ paren-end))
		       (if outside-macro
			   (c-beginning-of-macro)))
		   (throw 'done nil))))

	(and (progn
	       (c-forward-syntactic-ws)
	       (looking-at "\\w\\|\\s_"))

	     (save-excursion
	       ;; The function header in a K&R declaration should only
	       ;; contain identifiers separated by comma.  It should
	       ;; also contain at least one identifier since there
	       ;; wouldn't be anything to declare in the K&R region
	       ;; otherwise.
	       (when (c-go-up-list-backward paren-end)
		 (forward-char)
		 (catch 'knr-ok
		   (while t
		     (c-forward-syntactic-ws)
		     (if (or (looking-at c-known-type-key)
			     (looking-at c-keywords-regexp))
			 (throw 'knr-ok nil))
		     (c-forward-token-2)
		     (if (eq (char-after) ?,)
			 (forward-char)
		       (throw 'knr-ok (and (eq (char-after) ?\))
					   (= (point) paren-end))))))))

	     (save-excursion
	       ;; If it's a K&R declaration then we're now at the
	       ;; beginning of the function arglist.  Check that there
	       ;; isn't a '=' before it in this statement since that
	       ;; means it some kind of initialization instead.
	       (c-syntactic-skip-backward "^;=}{")
	       (not (eq (char-before) ?=)))

	     (point))))))

(defun c-skip-conditional ()
  ;; skip forward over conditional at point, including any predicate
  ;; statements in parentheses. No error checking is performed.
  (c-forward-sexp (cond
		   ;; else if()
		   ((looking-at (concat "\\<else"
					"\\([ \t\n]\\|\\\\\n\\)+"
					"if\\>\\([^_]\\|$\\)"))
		    3)
		   ;; do, else, try, finally
		   ((looking-at (concat "\\<\\("
					"do\\|else\\|try\\|finally"
					"\\)\\>\\([^_]\\|$\\)"))
		    1)
		   ;; for, if, while, switch, catch, synchronized, foreach
		   (t 2))))

(defun c-after-conditional (&optional lim)
  ;; If looking at the token after a conditional then return the
  ;; position of its start, otherwise return nil.
  (save-excursion
    (and (zerop (c-backward-token-2 1 t lim))
	 (or (looking-at c-block-stmt-1-key)
	     (and (eq (char-after) ?\()
		  (zerop (c-backward-token-2 1 t lim))
		  (looking-at c-block-stmt-2-key)))
	 (point))))

(defsubst c-backward-to-block-anchor (&optional lim)
  ;; Assuming point is at a brace that opens a statement block of some
  ;; kind, move to the proper anchor point for that block.  It might
  ;; need to be adjusted further by c-add-stmt-syntax, but the
  ;; position at return is suitable as start position for that
  ;; function.
  (unless (= (point) (c-point 'boi))
    (let ((start (c-after-conditional lim)))
      (if start
	  (goto-char start)))))

(defsubst c-backward-to-decl-anchor (&optional lim)
  ;; Assuming point is at a brace that opens the block of a top level
  ;; declaration of some kind, move to the proper anchor point for
  ;; that block.
  (unless (= (point) (c-point 'boi))
    (c-beginning-of-statement-1 lim)))

(defun c-search-decl-header-end ()
  ;; Search forward for the end of the "header" of the current
  ;; declaration.  That's the position where the definition body
  ;; starts, or the first variable initializer, or the ending
  ;; semicolon.  I.e. search forward for the closest following
  ;; (syntactically relevant) '{', '=' or ';' token.  Point is left
  ;; _after_ the first found token, or at point-max if none is found.

  (let ((base (point)))
    (if (c-major-mode-is 'c++-mode)

	;; In C++ we need to take special care to handle operator
	;; tokens and those pesky template brackets.
	(while (and
		(c-syntactic-re-search-forward "[;{<=]" nil 'move t t)
		(or
		 (c-end-of-current-token base)
		 ;; Handle operator identifiers, i.e. ignore any
		 ;; operator token preceded by "operator".
		 (save-excursion
		   (and (c-safe (c-backward-sexp) t)
			(looking-at "operator\\>\\([^_]\\|$\\)")))
		 (and (eq (char-before) ?<)
		      (c-with-syntax-table c++-template-syntax-table
			(if (c-safe (goto-char (c-up-list-forward (point))))
			    t
			  (goto-char (point-max))
			  nil)))))
	  (setq base (point)))

      (while (and
	      (c-syntactic-re-search-forward "[;{=]" nil 'move t t)
	      (c-end-of-current-token base))
	(setq base (point))))))

(defun c-beginning-of-decl-1 (&optional lim)
  ;; Go to the beginning of the current declaration, or the beginning
  ;; of the previous one if already at the start of it.  Point won't
  ;; be moved out of any surrounding paren.  Return a cons cell on the
  ;; form (MOVE . KNR-POS).  MOVE is like the return value from
  ;; `c-beginning-of-statement-1'.  If point skipped over some K&R
  ;; style argument declarations (and they are to be recognized) then
  ;; KNR-POS is set to the start of the first such argument
  ;; declaration, otherwise KNR-POS is nil.  If LIM is non-nil, it's a
  ;; position that bounds the backward search.
  ;;
  ;; NB: Cases where the declaration continues after the block, as in
  ;; "struct foo { ... } bar;", are currently recognized as two
  ;; declarations, e.g. "struct foo { ... }" and "bar;" in this case.
  (catch 'return
    (let* ((start (point))
	   (last-stmt-start (point))
	   (move (c-beginning-of-statement-1 lim t t)))

      ;; `c-beginning-of-statement-1' stops at a block start, but we
      ;; want to continue if the block doesn't begin a top level
      ;; construct, i.e. if it isn't preceded by ';', '}', ':', bob,
      ;; or an open paren.
      (let ((beg (point)) tentative-move)
	(while (and
		;; Must check with c-opt-method-key in ObjC mode.
		(not (and c-opt-method-key
			  (looking-at c-opt-method-key)))
		(/= last-stmt-start (point))
		(progn
		  (c-backward-syntactic-ws lim)
		  (not (memq (char-before) '(?\; ?} ?: nil))))
		(save-excursion
		  (backward-char)
		  (not (looking-at "\\s(")))
		;; Check that we don't move from the first thing in a
		;; macro to its header.
		(not (eq (setq tentative-move
			       (c-beginning-of-statement-1 lim t t))
			 'macro)))
	  (setq last-stmt-start beg
		beg (point)
		move tentative-move))
	(goto-char beg))

      (when c-recognize-knr-p
	(let ((fallback-pos (point)) knr-argdecl-start)
	  ;; Handle K&R argdecls.  Back up after the "statement" jumped
	  ;; over by `c-beginning-of-statement-1', unless it was the
	  ;; function body, in which case we're sitting on the opening
	  ;; brace now.  Then test if we're in a K&R argdecl region and
	  ;; that we started at the other side of the first argdecl in
	  ;; it.
	  (unless (eq (char-after) ?{)
	    (goto-char last-stmt-start))
	  (if (and (setq knr-argdecl-start (c-in-knr-argdecl lim))
		   (< knr-argdecl-start start)
		   (progn
		     (goto-char knr-argdecl-start)
		     (not (eq (c-beginning-of-statement-1 lim t t) 'macro))))
	      (throw 'return
		     (cons (if (eq (char-after fallback-pos) ?{)
			       'previous
			     'same)
			   knr-argdecl-start))
	    (goto-char fallback-pos))))

      (when c-opt-access-key
	;; Might have ended up before a protection label.  This should
	;; perhaps be checked before `c-recognize-knr-p' to be really
	;; accurate, but we know that no language has both.
	(while (looking-at c-opt-access-key)
	  (goto-char (match-end 0))
	  (c-forward-syntactic-ws)
	  (when (>= (point) start)
	    (goto-char start)
	    (throw 'return (cons 'same nil)))))

      ;; `c-beginning-of-statement-1' counts each brace block as a
      ;; separate statement, so the result will be 'previous if we've
      ;; moved over any.  If they were brace list initializers we might
      ;; not have moved over a declaration boundary though, so change it
      ;; to 'same if we've moved past a '=' before '{', but not ';'.
      ;; (This ought to be integrated into `c-beginning-of-statement-1',
      ;; so we avoid this extra pass which potentially can search over a
      ;; large amount of text.)
      (if (and (eq move 'previous)
	       (c-with-syntax-table (if (c-major-mode-is 'c++-mode)
					c++-template-syntax-table
				      (syntax-table))
		 (save-excursion
		   (and (c-syntactic-re-search-forward "[;={]" start t t t)
			(eq (char-before) ?=)
			(c-syntactic-re-search-forward "[;{]" start t t)
			(eq (char-before) ?{)
			(c-safe (goto-char (c-up-list-forward (point))) t)
			(not (c-syntactic-re-search-forward ";" start t t))))))
	  (cons 'same nil)
	(cons move nil)))))

(defun c-end-of-decl-1 ()
  ;; Assuming point is at the start of a declaration (as detected by
  ;; e.g. `c-beginning-of-decl-1'), go to the end of it.  Unlike
  ;; `c-beginning-of-decl-1', this function handles the case when a
  ;; block is followed by identifiers in e.g. struct declarations in C
  ;; or C++.  If a proper end was found then t is returned, otherwise
  ;; point is moved as far as possible within the current sexp and nil
  ;; is returned.  This function doesn't handle macros; use
  ;; `c-end-of-macro' instead in those cases.
  (let ((start (point))
	(decl-syntax-table (if (c-major-mode-is 'c++-mode)
			       c++-template-syntax-table
			     (syntax-table))))
    (catch 'return
      (c-search-decl-header-end)

      (when (and c-recognize-knr-p
		 (eq (char-before) ?\;)
		 (c-in-knr-argdecl start))
	;; Stopped at the ';' in a K&R argdecl section which is
	;; detected using the same criteria as in
	;; `c-beginning-of-decl-1'.  Move to the following block
	;; start.
	(c-syntactic-re-search-forward "{" nil 'move t))

      (when (eq (char-before) ?{)
	;; Encountered a block in the declaration.  Jump over it.
	(condition-case nil
	    (goto-char (c-up-list-forward (point)))
	  (error (goto-char (point-max))
		 (throw 'return nil)))
	(if (or (not c-opt-block-decls-with-vars-key)
		(save-excursion
		  (c-with-syntax-table decl-syntax-table
		    (let ((lim (point)))
		      (goto-char start)
		      (not (and
			    ;; Check for `c-opt-block-decls-with-vars-key'
			    ;; before the first paren.
			    (c-syntactic-re-search-forward
			     (concat "[;=\(\[{]\\|\\("
				     c-opt-block-decls-with-vars-key
				     "\\)")
			     lim t t t)
			    (match-beginning 1)
			    (not (eq (char-before) ?_))
			    ;; Check that the first following paren is
			    ;; the block.
			    (c-syntactic-re-search-forward "[;=\(\[{]"
							   lim t t t)
			    (eq (char-before) ?{)))))))
	    ;; The declaration doesn't have any of the
	    ;; `c-opt-block-decls-with-vars' keywords in the
	    ;; beginning, so it ends here at the end of the block.
	    (throw 'return t)))

      (c-with-syntax-table decl-syntax-table
	(while (progn
		 (if (eq (char-before) ?\;)
		     (throw 'return t))
		 (c-syntactic-re-search-forward ";" nil 'move t))))
      nil)))

(defun c-beginning-of-member-init-list (&optional limit)
  ;; Go to the beginning of a member init list (i.e. just after the
  ;; ':') if inside one.  Returns t in that case, nil otherwise.
  (or limit
      (setq limit (point-min)))
  (skip-chars-forward " \t")

  (if (eq (char-after) ?,)
      (forward-char 1)
    (c-backward-syntactic-ws limit))

  (catch 'exit
    (while (and (< limit (point))
		(eq (char-before) ?,))

      ;; this will catch member inits with multiple
      ;; line arglists
      (forward-char -1)
      (c-backward-syntactic-ws limit)
      (if (eq (char-before) ?\))
	  (unless (c-safe (c-backward-sexp 1))
	    (throw 'exit nil)))
      (c-backward-syntactic-ws limit)

      ;; Skip over any template arg to the class.  This way with a
      ;; syntax table is bogus but it'll have to do for now.
      (if (and (eq (char-before) ?>)
	       (c-major-mode-is 'c++-mode))
	  (c-with-syntax-table c++-template-syntax-table
	    (unless (c-safe (c-backward-sexp 1))
	      (throw 'exit nil))))
      (c-safe (c-backward-sexp 1))
      (c-backward-syntactic-ws limit)

      ;; Skip backwards over a fully::qualified::name.
      (while (and (eq (char-before) ?:)
		  (save-excursion
		    (forward-char -1)
		    (eq (char-before) ?:)))
	(backward-char 2)
	(c-safe (c-backward-sexp 1)))

      ;; If we've stepped over a number then this is a bitfield.
      (when (and c-opt-bitfield-key
		 (looking-at "[0-9]"))
	(throw 'exit nil))

      ;; now continue checking
      (c-backward-syntactic-ws limit))

    (and (< limit (point))
	 (eq (char-before) ?:))))

(defun c-search-uplist-for-classkey (paren-state)
  ;; search for the containing class, returning a 2 element vector if
  ;; found. aref 0 contains the bufpos of the boi of the class key
  ;; line, and aref 1 contains the bufpos of the open brace.
  (if (null paren-state)
      ;; no paren-state means we cannot be inside a class
      nil
    (let ((carcache (car paren-state))
	  search-start search-end)
      (if (consp carcache)
	  ;; a cons cell in the first element means that there is some
	  ;; balanced sexp before the current bufpos. this we can
	  ;; ignore. the nth 1 and nth 2 elements define for us the
	  ;; search boundaries
	  (setq search-start (nth 2 paren-state)
		search-end (nth 1 paren-state))
	;; if the car was not a cons cell then nth 0 and nth 1 define
	;; for us the search boundaries
	(setq search-start (nth 1 paren-state)
	      search-end (nth 0 paren-state)))
      ;; if search-end is nil, or if the search-end character isn't an
      ;; open brace, we are definitely not in a class
      (if (or (not search-end)
	      (< search-end (point-min))
	      (not (eq (char-after search-end) ?{)))
	  nil
	;; now, we need to look more closely at search-start.  if
	;; search-start is nil, then our start boundary is really
	;; point-min.
	(if (not search-start)
	    (setq search-start (point-min))
	  ;; if search-start is a cons cell, then we can start
	  ;; searching from the end of the balanced sexp just ahead of
	  ;; us
	  (if (consp search-start)
	      (setq search-start (cdr search-start))
	    ;; Otherwise we start searching within the surrounding paren sexp.
	    (setq search-start (1+ search-start))))
	;; now we can do a quick regexp search from search-start to
	;; search-end and see if we can find a class key.  watch for
	;; class like strings in literals
	(save-excursion
	  (save-restriction
	    (goto-char search-start)
	    (let (foundp class match-end)
	      (while (and (not foundp)
			  (progn
			    (c-forward-syntactic-ws search-end)
			    (> search-end (point)))
			  ;; Add one to the search limit, to allow
			  ;; matching of the "{" in the regexp.
			  (re-search-forward c-decl-block-key
					     (1+ search-end)
					     t))
		(setq class (match-beginning 0)
		      match-end (match-end 0))
		(goto-char class)
		(if (c-in-literal search-start)
		    (goto-char match-end) ; its in a comment or string, ignore
		  (c-skip-ws-forward)
		  (setq foundp (vector (c-point 'boi) search-end))
		  (cond
		   ;; check for embedded keywords
		   ((let ((char (char-after (1- class))))
		      (and char
			   (memq (char-syntax char) '(?w ?_))))
		    (goto-char match-end)
		    (setq foundp nil))
		   ;; make sure we're really looking at the start of a
		   ;; class definition, and not an ObjC method.
		   ((and c-opt-method-key
			 (re-search-forward c-opt-method-key search-end t)
			 (not (c-in-literal class)))
		    (setq foundp nil))
		   ;; Check if this is an anonymous inner class.
		   ((and c-opt-inexpr-class-key
			 (looking-at c-opt-inexpr-class-key))
		    (while (and (zerop (c-forward-token-2 1 t))
				(looking-at "(\\|\\w\\|\\s_\\|\\.")))
		    (if (eq (point) search-end)
			;; We're done.  Just trap this case in the cond.
			nil
		      ;; False alarm; all conditions aren't satisfied.
		      (setq foundp nil)))
		   ;; Its impossible to define a regexp for this, and
		   ;; nearly so to do it programmatically.
		   ;;
		   ;; ; picks up forward decls
		   ;; = picks up init lists
		   ;; ) picks up return types
		   ;; > picks up templates, but remember that we can
		   ;;   inherit from templates!
		   ((let ((skipchars "^;=)"))
		      ;; try to see if we found the `class' keyword
		      ;; inside a template arg list
		      (save-excursion
			(skip-chars-backward "^<>" search-start)
			(if (eq (char-before) ?<)
			    (setq skipchars (concat skipchars ">"))))
		      (while (progn
			       (skip-chars-forward skipchars search-end)
			       (c-in-literal class))
			(forward-char))
		      (/= (point) search-end))
		    (setq foundp nil))
		   )))
	      foundp))
	  )))))

(defun c-inside-bracelist-p (containing-sexp paren-state)
  ;; return the buffer position of the beginning of the brace list
  ;; statement if we're inside a brace list, otherwise return nil.
  ;; CONTAINING-SEXP is the buffer pos of the innermost containing
  ;; paren.  BRACE-STATE is the remainder of the state of enclosing
  ;; braces
  ;;
  ;; N.B.: This algorithm can potentially get confused by cpp macros
  ;; places in inconvenient locations.  Its a trade-off we make for
  ;; speed.
  (or
   ;; This will pick up brace list declarations.
   (c-safe
    (save-excursion
      (goto-char containing-sexp)
      (c-forward-sexp -1)
      (let (bracepos)
	(if (and (or (looking-at c-brace-list-key)
		     (progn (c-forward-sexp -1)
			    (looking-at c-brace-list-key)))
		 (setq bracepos (c-down-list-forward (point)))
		 (not (c-crosses-statement-barrier-p (point)
						     (- bracepos 2))))
	    (point)))))
   ;; this will pick up array/aggregate init lists, even if they are nested.
   (save-excursion
     (let ((class-key
	    ;; Pike can have class definitions anywhere, so we must
	    ;; check for the class key here.
	    (and (c-major-mode-is 'pike-mode)
		 c-decl-block-key))
	   bufpos braceassignp lim next-containing)
       (while (and (not bufpos)
		   containing-sexp)
	   (when paren-state
	     (if (consp (car paren-state))
		 (setq lim (cdr (car paren-state))
		       paren-state (cdr paren-state))
	       (setq lim (car paren-state)))
	     (when paren-state
	       (setq next-containing (car paren-state)
		     paren-state (cdr paren-state))))
	   (goto-char containing-sexp)
	   (if (c-looking-at-inexpr-block next-containing next-containing)
	       ;; We're in an in-expression block of some kind.  Do not
	       ;; check nesting.  We deliberately set the limit to the
	       ;; containing sexp, so that c-looking-at-inexpr-block
	       ;; doesn't check for an identifier before it.
	       (setq containing-sexp nil)
	     ;; see if the open brace is preceded by = or [...] in
	     ;; this statement, but watch out for operator=
	     (setq braceassignp 'dontknow)
	     (c-backward-token-2 1 t lim)
	     ;; Checks to do only on the first sexp before the brace.
	     (when (and c-opt-inexpr-brace-list-key
			(eq (char-after) ?\[))
	       ;; In Java, an initialization brace list may follow
	       ;; directly after "new Foo[]", so check for a "new"
	       ;; earlier.
	       (while (eq braceassignp 'dontknow)
		 (setq braceassignp
		       (cond ((/= (c-backward-token-2 1 t lim) 0) nil)
			     ((looking-at c-opt-inexpr-brace-list-key) t)
			     ((looking-at "\\sw\\|\\s_\\|[.[]")
			      ;; Carry on looking if this is an
			      ;; identifier (may contain "." in Java)
			      ;; or another "[]" sexp.
			      'dontknow)
			     (t nil)))))
	     ;; Checks to do on all sexps before the brace, up to the
	     ;; beginning of the statement.
	     (while (eq braceassignp 'dontknow)
	       (cond ((eq (char-after) ?\;)
		      (setq braceassignp nil))
		     ((and class-key
			   (looking-at class-key))
		      (setq braceassignp nil))
		     ((eq (char-after) ?=)
		      ;; We've seen a =, but must check earlier tokens so
		      ;; that it isn't something that should be ignored.
		      (setq braceassignp 'maybe)
		      (while (and (eq braceassignp 'maybe)
				  (zerop (c-backward-token-2 1 t lim)))
			(setq braceassignp
			      (cond
			       ;; Check for operator =
			       ((looking-at "operator\\>[^_]") nil)
			       ;; Check for `<opchar>= in Pike.
			       ((and (c-major-mode-is 'pike-mode)
				     (or (eq (char-after) ?`)
					 ;; Special case for Pikes
					 ;; `[]=, since '[' is not in
					 ;; the punctuation class.
					 (and (eq (char-after) ?\[)
					      (eq (char-before) ?`))))
				nil)
			       ((looking-at "\\s.") 'maybe)
			       ;; make sure we're not in a C++ template
			       ;; argument assignment
			       ((and
				 (c-major-mode-is 'c++-mode)
				 (save-excursion
				   (let ((here (point))
					 (pos< (progn
						 (skip-chars-backward "^<>")
						 (point))))
				     (and (eq (char-before) ?<)
					  (not (c-crosses-statement-barrier-p
						pos< here))
					  (not (c-in-literal))
					  ))))
				nil)
			       (t t))))))
	       (if (and (eq braceassignp 'dontknow)
			(/= (c-backward-token-2 1 t lim) 0))
		   (setq braceassignp nil)))
	     (if (not braceassignp)
		 (if (eq (char-after) ?\;)
		     ;; Brace lists can't contain a semicolon, so we're done.
		     (setq containing-sexp nil)
		   ;; Go up one level.
		   (setq containing-sexp next-containing
			 lim nil
			 next-containing nil))
	       ;; we've hit the beginning of the aggregate list
	       (c-beginning-of-statement-1
		(c-most-enclosing-brace paren-state))
	       (setq bufpos (point))))
	   )
       bufpos))
   ))

(defun c-looking-at-special-brace-list (&optional lim)
  ;; If we're looking at the start of a pike-style list, ie `({ })',
  ;; `([ ])', `(< >)' etc, a cons of a cons of its starting and ending
  ;; positions and its entry in c-special-brace-lists is returned, nil
  ;; otherwise.  The ending position is nil if the list is still open.
  ;; LIM is the limit for forward search.  The point may either be at
  ;; the `(' or at the following paren character.  Tries to check the
  ;; matching closer, but assumes it's correct if no balanced paren is
  ;; found (i.e. the case `({ ... } ... )' is detected as _not_ being
  ;; a special brace list).
  (if c-special-brace-lists
      (condition-case ()
	  (save-excursion
	    (let ((beg (point))
		  inner-beg end type)
	      (c-forward-syntactic-ws)
	      (if (eq (char-after) ?\()
		  (progn
		    (forward-char 1)
		    (c-forward-syntactic-ws)
		    (setq inner-beg (point))
		    (setq type (assq (char-after) c-special-brace-lists)))
		(if (setq type (assq (char-after) c-special-brace-lists))
		    (progn
		      (setq inner-beg (point))
		      (c-backward-syntactic-ws)
		      (forward-char -1)
		      (setq beg (if (eq (char-after) ?\()
				    (point)
				  nil)))))
	      (if (and beg type)
		  (if (and (c-safe
			     (goto-char beg)
			     (c-forward-sexp 1)
			     (setq end (point))
			     (= (char-before) ?\)))
			   (c-safe
			     (goto-char inner-beg)
			     (if (looking-at "\\s(")
				 ;; Check balancing of the inner paren
				 ;; below.
				 (progn
				   (c-forward-sexp 1)
				   t)
			       ;; If the inner char isn't a paren then
			       ;; we can't check balancing, so just
			       ;; check the char before the outer
			       ;; closing paren.
			       (goto-char end)
			       (backward-char)
			       (c-backward-syntactic-ws)
			       (= (char-before) (cdr type)))))
		      (if (or (/= (char-syntax (char-before)) ?\))
			      (= (progn
				   (c-forward-syntactic-ws)
				   (point))
				 (1- end)))
			  (cons (cons beg end) type))
		    (cons (list beg) type)))))
	(error nil))))

(defun c-looking-at-bos (&optional lim)
  ;; Return non-nil if between two statements or declarations, assuming
  ;; point is not inside a literal or comment.
  (save-excursion
    (c-backward-syntactic-ws lim)
    (or (bobp)
	;; Return t if at the start inside some parenthesis expression
	;; too, to catch macros that have statements as arguments.
	(memq (char-before) '(?\; ?} ?\())
	(and (eq (char-before) ?{)
	     (not (and c-special-brace-lists
		       (progn (backward-char)
			      (c-looking-at-special-brace-list))))))))

(defun c-looking-at-inexpr-block (lim containing-sexp)
  ;; Returns non-nil if we're looking at the beginning of a block
  ;; inside an expression.  The value returned is actually a cons of
  ;; either 'inlambda, 'inexpr-statement or 'inexpr-class and the
  ;; position of the beginning of the construct.  LIM limits the
  ;; backward search.  CONTAINING-SEXP is the start position of the
  ;; closest containing list.  If it's nil, the containing paren isn't
  ;; used to decide whether we're inside an expression or not.  If
  ;; both LIM and CONTAINING-SEXP is used, LIM needs to be farther
  ;; back.
  (save-excursion
    (let ((res 'maybe) passed-bracket
	  (closest-lim (or containing-sexp lim (point-min)))
	  ;; Look at the character after point only as a last resort
	  ;; when we can't disambiguate.
	  (block-follows (and (eq (char-after) ?{) (point))))
      (while (and (eq res 'maybe)
		  (progn (c-backward-syntactic-ws)
			 (> (point) closest-lim))
		  (not (bobp))
		  (progn (backward-char)
			 (looking-at "[\]\).]\\|\\w\\|\\s_"))
		  (progn (forward-char)
			 (goto-char (scan-sexps (point) -1))))
	(setq res
	      (cond
	       ((and block-follows
		     c-opt-inexpr-class-key
		     (looking-at c-opt-inexpr-class-key))
		(and (not passed-bracket)
		     (or (not (looking-at c-class-key))
			 ;; If the class definition is at the start of
			 ;; a statement, we don't consider it an
			 ;; in-expression class.
			 (let ((prev (point)))
			   (while (and
				   (= (c-backward-token-2 1 nil closest-lim) 0)
				   (eq (char-syntax (char-after)) ?w))
			     (setq prev (point)))
			   (goto-char prev)
			   (not (c-looking-at-bos)))
			 ;; Also, in Pike we treat it as an
			 ;; in-expression class if it's used in an
			 ;; object clone expression.
			 (save-excursion
			   (and (c-major-mode-is 'pike-mode)
				(progn (goto-char block-follows)
				       (zerop (c-forward-token-2 1 t)))
				(eq (char-after) ?\())))
		     (cons 'inexpr-class (point))))
	       ((and c-opt-inexpr-block-key
		     (looking-at c-opt-inexpr-block-key))
		(cons 'inexpr-statement (point)))
	       ((and c-opt-lambda-key
		     (looking-at c-opt-lambda-key))
		(cons 'inlambda (point)))
	       ((and c-opt-block-stmt-key
		     (looking-at c-opt-block-stmt-key))
		nil)
	       (t
		(if (eq (char-after) ?\[)
		    (setq passed-bracket t))
		'maybe))))
      (if (eq res 'maybe)
	  (when (and block-follows
		     containing-sexp
		     (eq (char-after containing-sexp) ?\())
	    (goto-char containing-sexp)
	    (if (or (save-excursion
		      (c-backward-syntactic-ws lim)
		      (and (> (point) (or lim (point-min)))
			   (c-on-identifier)))
		    (and c-special-brace-lists
			 (c-looking-at-special-brace-list)))
		nil
	      (cons 'inexpr-statement (point))))
	res))))

(defun c-looking-at-inexpr-block-backward (paren-state)
  ;; Returns non-nil if we're looking at the end of an in-expression
  ;; block, otherwise the same as `c-looking-at-inexpr-block'.
  ;; PAREN-STATE is the paren state relevant at the current position.
  (save-excursion
    ;; We currently only recognize a block.
    (let ((here (point))
	  (elem (car-safe paren-state))
	  containing-sexp)
      (when (and (consp elem)
		 (progn (goto-char (cdr elem))
			(c-forward-syntactic-ws here)
			(= (point) here)))
	(goto-char (car elem))
	(if (setq paren-state (cdr paren-state))
	    (setq containing-sexp (car-safe paren-state)))
	(c-looking-at-inexpr-block (c-safe-position containing-sexp
						    paren-state)
				   containing-sexp)))))

(defun c-narrow-out-enclosing-class (paren-state lim)
  ;; Narrow the buffer so that the enclosing class is hidden.  Uses
  ;; and returns the value from c-search-uplist-for-classkey.
  (setq paren-state (c-whack-state-after (point) paren-state))
  (let (inclass-p)
    (and paren-state
	 (setq inclass-p (c-search-uplist-for-classkey paren-state))
	 (narrow-to-region
	  (progn
	    (goto-char (1+ (aref inclass-p 1)))
	    (c-skip-ws-forward lim)
	    ;; if point is now left of the class opening brace, we're
	    ;; hosed, so try a different tact
	    (if (<= (point) (aref inclass-p 1))
		(progn
		  (goto-char (1+ (aref inclass-p 1)))
		  (c-forward-syntactic-ws lim)))
	    (point))
	  ;; end point is the end of the current line
	  (progn
	    (goto-char lim)
	    (c-point 'eol))))
    ;; return the class vector
    inclass-p))


;; `c-guess-basic-syntax' and the functions that precedes it below
;; implements the main decision tree for determining the syntactic
;; analysis of the current line of code.

;; Dynamically bound to t when `c-guess-basic-syntax' is called during
;; auto newline analysis.
(defvar c-auto-newline-analysis nil)

(defsubst c-add-syntax (symbol &rest args)
  ;; A simple function to prepend a new syntax element to
  ;; `c-syntactic-context'.  Using `setq' on it is unsafe since it
  ;; should always be dynamically bound but since we read it first
  ;; we'll fail properly anyway if this function is misused.
  (setq c-syntactic-context (cons (cons symbol args)
				  c-syntactic-context)))

(defsubst c-append-syntax (symbol &rest args)
  ;; Like `c-add-syntax' but appends to the end of the syntax list.
  ;; (Normally not necessary.)
  (setq c-syntactic-context (nconc c-syntactic-context
				   (list (cons symbol args)))))

(defun c-add-stmt-syntax (syntax-symbol
			  syntax-extra-args
			  stop-at-boi-only
			  at-block-start
			  containing-sexp
			  paren-state)
  ;; Do the generic processing to anchor the given syntax symbol on
  ;; the preceding statement: Skip over any labels and containing
  ;; statements on the same line, and then search backward until we
  ;; find a statement or block start that begins at boi without a
  ;; label or comment.
  ;;
  ;; Point is assumed to be at the prospective anchor point for the
  ;; given SYNTAX-SYMBOL.  More syntax entries are added if we need to
  ;; skip past open parens and containing statements.  All the added
  ;; syntax elements will get the same anchor point.
  ;;
  ;; SYNTAX-EXTRA-ARGS are a list of the extra arguments for the
  ;; syntax symbol.  They are appended after the anchor point.
  ;;
  ;; If STOP-AT-BOI-ONLY is nil, we might stop in the middle of the
  ;; line if another statement precedes the current one on this line.
  ;;
  ;; If AT-BLOCK-START is non-nil, point is taken to be at the
  ;; beginning of a block or brace list, which then might be nested
  ;; inside an expression.  If AT-BLOCK-START is nil, this is found
  ;; out by checking whether the character at point is "{" or not.
  (if (= (point) (c-point 'boi))
      ;; This is by far the most common case, so let's give it special
      ;; treatment.
      (apply 'c-add-syntax syntax-symbol (point) syntax-extra-args)

    (let ((savepos (point))
	  (syntax-last c-syntactic-context)
	  (boi (c-point 'boi))
	  (prev-paren (if at-block-start ?{ (char-after)))
	  step-type step-tmp at-comment special-list)
      (apply 'c-add-syntax syntax-symbol nil syntax-extra-args)

      ;; Begin by skipping any labels and containing statements that
      ;; are on the same line.
      (while (and (/= (point) boi)
		  (if (memq (setq step-tmp
				  (c-beginning-of-statement-1 boi nil t))
			    '(up label))
		      t
		    (goto-char savepos)
		    nil)
		  (/= (point) savepos))
	(setq savepos (point)
	      step-type step-tmp))

      (catch 'done
	  ;; Loop if we have to back out of the containing block.
	  (while
	    (progn

	      ;; Loop if we have to back up another statement.
	      (while
		  (progn

		    ;; Always start by skipping over any comments that
		    ;; stands between the statement and boi.
		    (while (and (/= (setq savepos (point)) boi)
				(c-backward-single-comment))
		      (setq at-comment t
			    boi (c-point 'boi)))
		    (goto-char savepos)

		    (and
		     (or at-comment
			 (eq step-type 'label)
			 (/= savepos boi))

		     (let ((save-step-type step-type))
		       ;; Current position might not be good enough;
		       ;; skip backward another statement.
		       (setq step-type (c-beginning-of-statement-1
					containing-sexp))

		       (if (and (not stop-at-boi-only)
				(/= savepos boi)
				(memq step-type '(up previous)))
			   ;; If stop-at-boi-only is nil, we shouldn't
			   ;; back up over previous or containing
			   ;; statements to try to reach boi, so go
			   ;; back to the last position and exit.
			   (progn
			     (goto-char savepos)
			     nil)
			 (if (and (not stop-at-boi-only)
				  (memq step-type '(up previous beginning)))
			     ;; If we've moved into another statement
			     ;; then we should no longer try to stop
			     ;; after boi.
			     (setq stop-at-boi-only t))

			 ;; Record this a substatement if we skipped up
			 ;; one level, but not if we're still on the
			 ;; same line.  This so e.g. a sequence of "else
			 ;; if" clauses won't indent deeper and deeper.
			 (when (and (eq step-type 'up)
				    (< (point) boi))
			   (c-add-syntax 'substatement nil))

			 (setq boi (c-point 'boi))
			 (if (= (point) savepos)
			     (progn
			       (setq step-type save-step-type)
			       nil)
			   t)))))

		(setq savepos (point)
		      at-comment nil))
	      (setq at-comment nil)

	      (when (and containing-sexp
			 (if (memq step-type '(nil same))
			     (/= (point) boi)
			   (eq step-type 'label)))
		(goto-char containing-sexp)

		;; Don't stop in the middle of a special brace list opener
		;; like "({".
		(when (and c-special-brace-lists
			   (setq special-list
				 (c-looking-at-special-brace-list)))
		  (setq containing-sexp (car (car special-list)))
		  (goto-char containing-sexp))

		(setq paren-state (c-whack-state-after containing-sexp
						       paren-state)
		      containing-sexp (c-most-enclosing-brace paren-state)
		      savepos (point)
		      boi (c-point 'boi))

		(if (eq (setq prev-paren (char-after)) ?\()
		    (progn
		      (c-backward-syntactic-ws containing-sexp)
		      (when (/= savepos boi)
			(if (and (or (not (looking-at "\\>"))
				     (not (c-on-identifier)))
				 (not special-list)
				 (save-excursion
				   (c-forward-syntactic-ws)
				   (forward-char)
				   (c-forward-syntactic-ws)
				   (eq (char-after) ?{)))
			    ;; We're in an in-expression statement.
			    ;; This syntactic element won't get an anchor pos.
			    (c-add-syntax 'inexpr-statement)
			  (c-add-syntax 'arglist-cont-nonempty nil savepos)))
		      (goto-char (max boi
				      (if containing-sexp
					  (1+ containing-sexp)
					(point-min))))
		      (setq step-type 'same))
		  (setq step-type
			(c-beginning-of-statement-1 containing-sexp)))

		(let ((at-bod (and (eq step-type 'same)
				   (/= savepos (point))
				   (eq prev-paren ?{))))

		  (when (= savepos boi)
		    ;; If the open brace was at boi, we're always
		    ;; done.  The c-beginning-of-statement-1 call
		    ;; above is necessary anyway, to decide the type
		    ;; of block-intro to add.
		    (goto-char savepos)
		    (setq savepos nil))

		  (when (eq prev-paren ?{)
		    (c-add-syntax (if at-bod
				      'defun-block-intro
				    'statement-block-intro)
				  nil))

		  (when (and (not at-bod) savepos)
		    ;; Loop if the brace wasn't at boi, and we didn't
		    ;; arrive at a defun block.
		    (if (eq step-type 'same)
			;; Avoid backing up another sexp if the point
			;; we're at now is found to be good enough in
			;; the loop above.
			(setq step-type nil))
		    (if (and (not stop-at-boi-only)
			     (memq step-type '(up previous beginning)))
			(setq stop-at-boi-only t))
		    (setq boi (c-point 'boi)))))
	      )))

      ;; Fill in the current point as the anchor for all the symbols
      ;; added above.
      (let ((p c-syntactic-context))
	(while (not (eq p syntax-last))
	  (if (cdr (car p))
	      (setcar (cdr (car p)) (point)))
	  (setq p (cdr p))))

      )))

(defun c-add-class-syntax (symbol classkey paren-state)
  ;; The inclass and class-close syntactic symbols are added in
  ;; several places and some work is needed to fix everything.
  ;; Therefore it's collected here.
  (save-restriction
    (widen)
    (let (inexpr anchor containing-sexp)
      (goto-char (aref classkey 1))
      (if (and (eq symbol 'inclass) (= (point) (c-point 'boi)))
	  (c-add-syntax symbol (setq anchor (point)))
	(c-add-syntax symbol (setq anchor (aref classkey 0)))
	(if (and c-opt-inexpr-class-key
		 (setq containing-sexp (c-most-enclosing-brace paren-state
							       (point))
		       inexpr (cdr (c-looking-at-inexpr-block
				    (c-safe-position containing-sexp
						     paren-state)
				    containing-sexp)))
		 (/= inexpr (c-point 'boi inexpr)))
	    (c-add-syntax 'inexpr-class)))
      anchor)))

(defun c-guess-continued-construct (indent-point
				    char-after-ip
				    beg-of-same-or-containing-stmt
				    containing-sexp
				    paren-state)
  ;; This function contains the decision tree reached through both
  ;; cases 18 and 10.  It's a continued statement or top level
  ;; construct of some kind.

  (let (special-brace-list)
    (goto-char indent-point)
    (skip-chars-forward " \t")

    (cond
     ;; (CASE A removed.)
     ;; CASE B: open braces for class or brace-lists
     ((setq special-brace-list
	    (or (and c-special-brace-lists
		     (c-looking-at-special-brace-list))
		(eq char-after-ip ?{)))

      (cond
       ;; CASE B.1: class-open
       ((save-excursion
	  (skip-chars-forward "{")
	  (let ((decl (c-search-uplist-for-classkey (c-parse-state))))
	    (and decl
		 (setq beg-of-same-or-containing-stmt (aref decl 0)))
	    ))
	(c-add-syntax 'class-open beg-of-same-or-containing-stmt))

       ;; CASE B.2: brace-list-open
       ((or (consp special-brace-list)
	    (save-excursion
	      (goto-char beg-of-same-or-containing-stmt)
	      (c-syntactic-re-search-forward "=\\([^=]\\|$\\)"
					     indent-point t t t)))
	;; The most semantically accurate symbol here is
	;; brace-list-open, but we normally report it simply as a
	;; statement-cont.  The reason is that one normally adjusts
	;; brace-list-open for brace lists as top-level constructs,
	;; and brace lists inside statements is a completely different
	;; context.  C.f. case 5A.3.
	(c-beginning-of-statement-1 containing-sexp)
	(c-add-stmt-syntax (if c-auto-newline-analysis
			       ;; Turn off the dwim above when we're
			       ;; analyzing the nature of the brace
			       ;; for the auto newline feature.
			       'brace-list-open
			     'statement-cont)
			   nil nil nil
			   containing-sexp paren-state))

       ;; CASE B.3: The body of a function declared inside a normal
       ;; block.  Can occur e.g. in Pike and when using gcc
       ;; extensions, but watch out for macros followed by blocks.
       ;; C.f. cases E, 16F and 17G.
       ((and (not (c-looking-at-bos))
	     (eq (c-beginning-of-statement-1 containing-sexp nil nil t)
		 'same)
	     (save-excursion
	       ;; Look for a type followed by a symbol, i.e. the start of a
	       ;; function declaration.  Doesn't work for declarations like
	       ;; "int *foo() ..."; we'd need to refactor the more competent
	       ;; analysis in `c-font-lock-declarations' for that.
	       (and (c-forward-type)
		    (progn
		      (c-forward-syntactic-ws)
		      (looking-at c-symbol-start)))))
	(c-add-stmt-syntax 'defun-open nil t nil
			   containing-sexp paren-state))

       ;; CASE B.4: Continued statement with block open.  The most
       ;; accurate analysis is perhaps `statement-cont' together with
       ;; `block-open' but we play DWIM and use `substatement-open'
       ;; instead.  The rationaly is that this typically is a macro
       ;; followed by a block which makes it very similar to a
       ;; statement with a substatement block.
       (t
	(c-add-stmt-syntax 'substatement-open nil nil nil
			   containing-sexp paren-state))
       ))

     ;; CASE C: iostream insertion or extraction operator
     ((and (looking-at "\\(<<\\|>>\\)\\([^=]\\|$\\)")
	   (save-excursion
	     (goto-char beg-of-same-or-containing-stmt)
	     ;; If there is no preceding streamop in the statement
	     ;; then indent this line as a normal statement-cont.
	     (when (c-syntactic-re-search-forward
		    "\\(<<\\|>>\\)\\([^=]\\|$\\)" indent-point 'move t t)
	       (c-add-syntax 'stream-op (c-point 'boi))
	       t))))

     ;; CASE E: In the "K&R region" of a function declared inside a
     ;; normal block.  C.f. case B.3.
     ((and (save-excursion
	     ;; Check that the next token is a '{'.  This works as
	     ;; long as no language that allows nested function
	     ;; definitions allows stuff like member init lists, K&R
	     ;; declarations or throws clauses there.
	     ;;
	     ;; Note that we do a forward search for something ahead
	     ;; of the indentation line here.  That's not good since
	     ;; the user might not have typed it yet.  Unfortunately
	     ;; it's exceedingly tricky to recognize a function
	     ;; prototype in a code block without resorting to this.
	     (c-forward-syntactic-ws)
	     (eq (char-after) ?{))
	   (not (c-looking-at-bos))
	   (eq (c-beginning-of-statement-1 containing-sexp nil nil t)
	       'same)
	   (save-excursion
	     ;; Look for a type followed by a symbol, i.e. the start of a
	     ;; function declaration.  Doesn't work for declarations like "int
	     ;; *foo() ..."; we'd need to refactor the more competent analysis
	     ;; in `c-font-lock-declarations' for that.
	     (and (c-forward-type)
		  (progn
		    (c-forward-syntactic-ws)
		    (looking-at c-symbol-start)))))
      (c-add-stmt-syntax 'func-decl-cont nil t nil
			 containing-sexp paren-state))

     ;; CASE D: continued statement.
     (t
      (c-beginning-of-statement-1 containing-sexp)
      (c-add-stmt-syntax 'statement-cont nil nil nil
			 containing-sexp paren-state))
     )))

(defun c-guess-basic-syntax ()
  "Return the syntactic context of the current line.
This function does not do any hidden buffer changes."
  (save-excursion
    (save-restriction
      (beginning-of-line)
      (c-save-buffer-state
	  ((indent-point (point))
	   (case-fold-search nil)
	   (paren-state (c-parse-state))
	   literal containing-sexp char-before-ip char-after-ip lim
	   c-syntactic-context placeholder c-in-literal-cache step-type
	   tmpsymbol keyword injava-inher special-brace-list
	   ;; narrow out any enclosing class or extern "C" block
	   (inclass-p (c-narrow-out-enclosing-class paren-state
						    indent-point))
	   ;; `c-state-cache' is shadowed here so that we don't
	   ;; throw it away due to the narrowing that might be done
	   ;; by the function above.  That means we must not do any
	   ;; changes during the execution of this function, since
	   ;; `c-invalidate-state-cache' then would change this local
	   ;; variable and leave a bogus value in the global one.
	   (c-state-cache (if inclass-p
			      (c-whack-state-before (point-min) paren-state)
			    paren-state))
	   (c-state-cache-start (point-min))
	   inenclosing-p macro-start in-macro-expr
	   ;; There's always at most one syntactic element which got
	   ;; a relpos.  It's stored in syntactic-relpos.
	   syntactic-relpos
	   (c-stmt-delim-chars c-stmt-delim-chars))
	;; Check for meta top-level enclosing constructs such as
	;; extern language definitions.
	(save-excursion
	  (save-restriction
	    (widen)
	    (when (and inclass-p
		       (progn
			 (goto-char (aref inclass-p 0))
			 (looking-at c-other-decl-block-key)))
	      (setq inenclosing-p (match-string 1))
	      (if (string-equal inenclosing-p "extern")
		  ;; Compatibility with legacy choice of name for the
		  ;; extern-lang syntactic symbols.
		  (setq inenclosing-p "extern-lang")))))

	;; Init some position variables:
	;;
	;; containing-sexp is the open paren of the closest
	;; surrounding sexp or nil if there is none that hasn't been
	;; narrowed out.
	;;
	;; lim is the position after the closest preceding brace sexp
	;; (nested sexps are ignored), or the position after
	;; containing-sexp if there is none, or (point-min) if
	;; containing-sexp is nil.
	;;
	;; c-state-cache is the state from c-parse-state at
	;; indent-point, without any parens outside the region
	;; narrowed by c-narrow-out-enclosing-class.
	;;
	;; paren-state is the state from c-parse-state outside
	;; containing-sexp, or at indent-point if containing-sexp is
	;; nil.  paren-state is not limited to the narrowed region, as
	;; opposed to c-state-cache.
	(if c-state-cache
	    (progn
	      (setq containing-sexp (car paren-state)
		    paren-state (cdr paren-state))
	      (if (consp containing-sexp)
		  (progn
		    (setq lim (cdr containing-sexp))
		    (if (cdr c-state-cache)
			;; Ignore balanced paren.  The next entry
			;; can't be another one.
			(setq containing-sexp (car (cdr c-state-cache))
			      paren-state (cdr paren-state))
		      ;; If there is no surrounding open paren then
		      ;; put the last balanced pair back on paren-state.
		      (setq paren-state (cons containing-sexp paren-state)
			    containing-sexp nil)))
		(setq lim (1+ containing-sexp))))
	  (setq lim (point-min)))

	;; If we're in a parenthesis list then ',' delimits the
	;; "statements" rather than being an operator (with the
	;; exception of the "for" clause).  This difference is
	;; typically only noticeable when statements are used in macro
	;; arglists.
	(when (and containing-sexp
		   (eq (char-after containing-sexp) ?\())
	  (setq c-stmt-delim-chars c-stmt-delim-chars-with-comma))

	;; cache char before and after indent point, and move point to
	;; the most likely position to perform the majority of tests
	(goto-char indent-point)
	(c-backward-syntactic-ws lim)
	(setq char-before-ip (char-before))
	(goto-char indent-point)
	(skip-chars-forward " \t")
	(setq char-after-ip (char-after))

	;; are we in a literal?
	(setq literal (c-in-literal lim))

	;; now figure out syntactic qualities of the current line
	(cond
	 ;; CASE 1: in a string.
	 ((eq literal 'string)
	  (c-add-syntax 'string (c-point 'bopl)))
	 ;; CASE 2: in a C or C++ style comment.
	 ((and (memq literal '(c c++))
	       ;; This is a kludge for XEmacs where we use
	       ;; `buffer-syntactic-context', which doesn't correctly
	       ;; recognize "\*/" to end a block comment.
	       ;; `parse-partial-sexp' which is used by
	       ;; `c-literal-limits' will however do that in most
	       ;; versions, which results in that we get nil from
	       ;; `c-literal-limits' even when `c-in-literal' claims
	       ;; we're inside a comment.
	       (setq placeholder (c-literal-limits lim)))
	  (c-add-syntax literal (car placeholder)))
	 ;; CASE 3: in a cpp preprocessor macro continuation.
	 ((and (save-excursion
		 (when (c-beginning-of-macro)
		   (setq macro-start (point))))
	       (/= macro-start (c-point 'boi))
	       (progn
		 (setq tmpsymbol 'cpp-macro-cont)
		 (or (not c-syntactic-indentation-in-macros)
		     (save-excursion
		       (goto-char macro-start)
		       ;; If at the beginning of the body of a #define
		       ;; directive then analyze as cpp-define-intro
		       ;; only.  Go on with the syntactic analysis
		       ;; otherwise.  in-macro-expr is set if we're in a
		       ;; cpp expression, i.e. before the #define body
		       ;; or anywhere in a non-#define directive.
		       (if (c-forward-to-cpp-define-body)
			   (let ((indent-boi (c-point 'boi indent-point)))
			     (setq in-macro-expr (> (point) indent-boi)
				   tmpsymbol 'cpp-define-intro)
			     (= (point) indent-boi))
			 (setq in-macro-expr t)
			 nil)))))
	  (c-add-syntax tmpsymbol macro-start)
	  (setq macro-start nil))
	 ;; CASE 11: an else clause?
	 ((looking-at "else\\>[^_]")
	  (c-beginning-of-statement-1 containing-sexp)
	  (c-add-stmt-syntax 'else-clause nil t nil
			     containing-sexp paren-state))
	 ;; CASE 12: while closure of a do/while construct?
	 ((and (looking-at "while\\>[^_]")
	       (save-excursion
		 (prog1 (eq (c-beginning-of-statement-1 containing-sexp)
			    'beginning)
		   (setq placeholder (point)))))
	  (goto-char placeholder)
	  (c-add-stmt-syntax 'do-while-closure nil t nil
			     containing-sexp paren-state))
	 ;; CASE 13: A catch or finally clause?  This case is simpler
	 ;; than if-else and do-while, because a block is required
	 ;; after every try, catch and finally.
	 ((save-excursion
	    (and (cond ((c-major-mode-is 'c++-mode)
			(looking-at "catch\\>[^_]"))
		       ((c-major-mode-is 'java-mode)
			(looking-at "\\(catch\\|finally\\)\\>[^_]")))
		 (and (c-safe (c-backward-syntactic-ws)
			      (c-backward-sexp)
			      t)
		      (eq (char-after) ?{)
		      (c-safe (c-backward-syntactic-ws)
			      (c-backward-sexp)
			      t)
		      (if (eq (char-after) ?\()
			  (c-safe (c-backward-sexp) t)
			t))
		 (looking-at "\\(try\\|catch\\)\\>[^_]")
		 (setq placeholder (point))))
	  (goto-char placeholder)
	  (c-add-stmt-syntax 'catch-clause nil t nil
			     containing-sexp paren-state))
	 ;; CASE 18: A substatement we can recognize by keyword.
	 ((save-excursion
	    (and c-opt-block-stmt-key
		 (if (c-mode-is-new-awk-p)
                     (c-awk-prev-line-incomplete-p containing-sexp) ; ACM 2002/3/29
                   (not (eq char-before-ip ?\;)))
		 (not (memq char-after-ip '(?\) ?\] ?,)))
		 (or (not (eq char-before-ip ?}))
		     (c-looking-at-inexpr-block-backward c-state-cache))
		 (> (point)
		    (progn
		      ;; Ought to cache the result from the
		      ;; c-beginning-of-statement-1 calls here.
		      (setq placeholder (point))
		      (while (eq (setq step-type
				       (c-beginning-of-statement-1 lim))
				 'label))
		      (if (eq step-type 'previous)
			  (goto-char placeholder)
			(setq placeholder (point))
			(if (and (eq step-type 'same)
				 (not (looking-at c-opt-block-stmt-key)))
			    ;; Step up to the containing statement if we
			    ;; stayed in the same one.
			    (let (step)
			      (while (eq
				      (setq step
					    (c-beginning-of-statement-1 lim))
				      'label))
			      (if (eq step 'up)
				  (setq placeholder (point))
				;; There was no containing statement afterall.
				(goto-char placeholder)))))
		      placeholder))
		 (if (looking-at c-block-stmt-2-key)
		     ;; Require a parenthesis after these keywords.
		     ;; Necessary to catch e.g. synchronized in Java,
		     ;; which can be used both as statement and
		     ;; modifier.
		     (and (zerop (c-forward-token-2 1 nil))
			  (eq (char-after) ?\())
		   (looking-at c-opt-block-stmt-key))))
	  (if (eq step-type 'up)
	      ;; CASE 18A: Simple substatement.
	      (progn
		(goto-char placeholder)
		(cond
		 ((eq char-after-ip ?{)
		  (c-add-stmt-syntax 'substatement-open nil nil nil
				     containing-sexp paren-state))
		 ((save-excursion
		    (goto-char indent-point)
		    (back-to-indentation)
		    (looking-at c-label-key))
		  (c-add-stmt-syntax 'substatement-label nil nil nil
				     containing-sexp paren-state))
		 (t
		  (c-add-stmt-syntax 'substatement nil nil nil
				     containing-sexp paren-state))))
	    ;; CASE 18B: Some other substatement.  This is shared
	    ;; with case 10.
	    (c-guess-continued-construct indent-point
					 char-after-ip
					 placeholder
					 lim
					 paren-state)))
	 ;; CASE 4: In-expression statement.  C.f. cases 7B, 16A and
	 ;; 17E.
	 ((and (or c-opt-inexpr-class-key
		   c-opt-inexpr-block-key
		   c-opt-lambda-key)
	       (setq placeholder (c-looking-at-inexpr-block
				  (c-safe-position containing-sexp paren-state)
				  containing-sexp)))
	  (setq tmpsymbol (assq (car placeholder)
				'((inexpr-class . class-open)
				  (inexpr-statement . block-open))))
	  (if tmpsymbol
	      ;; It's a statement block or an anonymous class.
	      (setq tmpsymbol (cdr tmpsymbol))
	    ;; It's a Pike lambda.  Check whether we are between the
	    ;; lambda keyword and the argument list or at the defun
	    ;; opener.
	    (setq tmpsymbol (if (eq char-after-ip ?{)
				'inline-open
			      'lambda-intro-cont)))
	  (goto-char (cdr placeholder))
	  (back-to-indentation)
	  (c-add-stmt-syntax tmpsymbol nil t nil
			     (c-most-enclosing-brace c-state-cache (point))
			     (c-whack-state-after (point) paren-state))
	  (unless (eq (point) (cdr placeholder))
	    (c-add-syntax (car placeholder))))
	 ;; CASE 5: Line is at top level.
	 ((null containing-sexp)
	  (cond
	   ;; CASE 5A: we are looking at a defun, brace list, class,
	   ;; or inline-inclass method opening brace
	   ((setq special-brace-list
		  (or (and c-special-brace-lists
			   (c-looking-at-special-brace-list))
		      (eq char-after-ip ?{)))
	    (cond
	     ;; CASE 5A.1: Non-class declaration block open.
	     ((save-excursion
		(goto-char indent-point)
		(skip-chars-forward " \t")
		(and (c-safe (c-backward-sexp 2) t)
		     (looking-at c-other-decl-block-key)
		     (setq keyword (match-string 1)
			   placeholder (point))
		     (if (string-equal keyword "extern")
			 ;; Special case for extern-lang-open.  The
			 ;; check for a following string is disabled
			 ;; since it doesn't disambiguate anything.
			 (and ;;(progn
			      ;;  (c-forward-sexp 1)
			      ;;  (c-forward-syntactic-ws)
			      ;;  (eq (char-after) ?\"))
			      (setq tmpsymbol 'extern-lang-open))
		       (setq tmpsymbol (intern (concat keyword "-open"))))
		     ))
	      (goto-char placeholder)
	      (c-add-syntax tmpsymbol (c-point 'boi)))
	     ;; CASE 5A.2: we are looking at a class opening brace
	     ((save-excursion
		(goto-char indent-point)
		(skip-chars-forward " \t{")
		(let ((decl (c-search-uplist-for-classkey (c-parse-state))))
		  (and decl
		       (setq placeholder (aref decl 0)))
		  ))
	      (c-add-syntax 'class-open placeholder))
	     ;; CASE 5A.3: brace list open
	     ((save-excursion
		(c-beginning-of-decl-1 lim)
		(while (looking-at c-specifier-key)
		  (goto-char (match-end 1))
		  (c-forward-syntactic-ws indent-point))
		(setq placeholder (c-point 'boi))
		(or (consp special-brace-list)
		    (and (or (save-excursion
			       (goto-char indent-point)
			       (setq tmpsymbol nil)
			       (while (and (> (point) placeholder)
					   (zerop (c-backward-token-2 1 t))
					   (/= (char-after) ?=))
				 (and c-opt-inexpr-brace-list-key
				      (not tmpsymbol)
				      (looking-at c-opt-inexpr-brace-list-key)
				      (setq tmpsymbol 'topmost-intro-cont)))
			       (eq (char-after) ?=))
			     (looking-at c-brace-list-key))
			 (save-excursion
			   (while (and (< (point) indent-point)
				       (zerop (c-forward-token-2 1 t))
				       (not (memq (char-after) '(?\; ?\()))))
			   (not (memq (char-after) '(?\; ?\()))
			   ))))
	      (if (and (not c-auto-newline-analysis)
		       (c-major-mode-is 'java-mode)
		       (eq tmpsymbol 'topmost-intro-cont))
		  ;; We're in Java and have found that the open brace
		  ;; belongs to a "new Foo[]" initialization list,
		  ;; which means the brace list is part of an
		  ;; expression and not a top level definition.  We
		  ;; therefore treat it as any topmost continuation
		  ;; even though the semantically correct symbol still
		  ;; is brace-list-open, on the same grounds as in
		  ;; case B.2.
		  (progn
		    (c-beginning-of-statement-1 lim)
		    (c-add-syntax 'topmost-intro-cont (c-point 'boi)))
		(c-add-syntax 'brace-list-open placeholder)))
	     ;; CASE 5A.4: inline defun open
	     ((and inclass-p (not inenclosing-p))
	      (c-add-syntax 'inline-open)
	      (c-add-class-syntax 'inclass inclass-p paren-state))
	     ;; CASE 5A.5: ordinary defun open
	     (t
	      (goto-char placeholder)
	      (if (or inclass-p macro-start)
		  (c-add-syntax 'defun-open (c-point 'boi))
		;; Bogus to use bol here, but it's the legacy.
		(c-add-syntax 'defun-open (c-point 'bol)))
	      )))
	   ;; CASE 5B: first K&R arg decl or member init
	   ((c-just-after-func-arglist-p lim)
	    (cond
	     ;; CASE 5B.1: a member init
	     ((or (eq char-before-ip ?:)
		  (eq char-after-ip ?:))
	      ;; this line should be indented relative to the beginning
	      ;; of indentation for the topmost-intro line that contains
	      ;; the prototype's open paren
	      ;; TBD: is the following redundant?
	      (if (eq char-before-ip ?:)
		  (forward-char -1))
	      (c-backward-syntactic-ws lim)
	      ;; TBD: is the preceding redundant?
	      (if (eq (char-before) ?:)
		  (progn (forward-char -1)
			 (c-backward-syntactic-ws lim)))
	      (if (eq (char-before) ?\))
		  (c-backward-sexp 1))
	      (setq placeholder (point))
	      (save-excursion
		(and (c-safe (c-backward-sexp 1) t)
		     (looking-at "throw[^_]")
		     (c-safe (c-backward-sexp 1) t)
		     (setq placeholder (point))))
	      (goto-char placeholder)
	      (c-add-syntax 'member-init-intro (c-point 'boi))
	      ;; we don't need to add any class offset since this
	      ;; should be relative to the ctor's indentation
	      )
	     ;; CASE 5B.2: K&R arg decl intro
	     ((and c-recognize-knr-p
		   (c-in-knr-argdecl lim))
	      (c-beginning-of-statement-1 lim)
	      (c-add-syntax 'knr-argdecl-intro (c-point 'boi))
	      (if inclass-p
		  (c-add-class-syntax 'inclass inclass-p paren-state)))
	     ;; CASE 5B.3: Inside a member init list.
	     ((c-beginning-of-member-init-list lim)
	      (c-forward-syntactic-ws)
	      (c-add-syntax 'member-init-cont (point)))
	     ;; CASE 5B.4: Nether region after a C++ or Java func
	     ;; decl, which could include a `throws' declaration.
	     (t
	      (c-beginning-of-statement-1 lim)
	      (c-add-syntax 'func-decl-cont (c-point 'boi))
	      )))
	   ;; CASE 5C: inheritance line. could be first inheritance
	   ;; line, or continuation of a multiple inheritance
	   ((or (and (c-major-mode-is 'c++-mode)
		     (progn
		       (when (eq char-after-ip ?,)
			 (skip-chars-forward " \t")
			 (forward-char))
		       (looking-at c-opt-postfix-decl-spec-key)))
		(and (or (eq char-before-ip ?:)
			 ;; watch out for scope operator
			 (save-excursion
			   (and (eq char-after-ip ?:)
				(c-safe (forward-char 1) t)
				(not (eq (char-after) ?:))
				)))
		     (save-excursion
		       (c-backward-syntactic-ws lim)
		       (if (eq char-before-ip ?:)
			   (progn
			     (forward-char -1)
			     (c-backward-syntactic-ws lim)))
		       (back-to-indentation)
		       (looking-at c-class-key)))
		;; for Java
		(and (c-major-mode-is 'java-mode)
		     (let ((fence (save-excursion
				    (c-beginning-of-statement-1 lim)
				    (point)))
			   cont done)
		       (save-excursion
			 (while (not done)
			   (cond ((looking-at c-opt-postfix-decl-spec-key)
				  (setq injava-inher (cons cont (point))
					done t))
				 ((or (not (c-safe (c-forward-sexp -1) t))
				      (<= (point) fence))
				  (setq done t))
				 )
			   (setq cont t)))
		       injava-inher)
		     (not (c-crosses-statement-barrier-p (cdr injava-inher)
							 (point)))
		     ))
	    (cond
	     ;; CASE 5C.1: non-hanging colon on an inher intro
	     ((eq char-after-ip ?:)
	      (c-beginning-of-statement-1 lim)
	      (c-add-syntax 'inher-intro (c-point 'boi))
	      ;; don't add inclass symbol since relative point already
	      ;; contains any class offset
	      )
	     ;; CASE 5C.2: hanging colon on an inher intro
	     ((eq char-before-ip ?:)
	      (c-beginning-of-statement-1 lim)
	      (c-add-syntax 'inher-intro (c-point 'boi))
	      (if inclass-p
		  (c-add-class-syntax 'inclass inclass-p paren-state)))
	     ;; CASE 5C.3: in a Java implements/extends
	     (injava-inher
	      (let ((where (cdr injava-inher))
		    (cont (car injava-inher)))
		(goto-char where)
		(cond ((looking-at "throws\\>[^_]")
		       (c-add-syntax 'func-decl-cont
				     (progn (c-beginning-of-statement-1 lim)
					    (c-point 'boi))))
		      (cont (c-add-syntax 'inher-cont where))
		      (t (c-add-syntax 'inher-intro
				       (progn (goto-char (cdr injava-inher))
					      (c-beginning-of-statement-1 lim)
					      (point))))
		      )))
	     ;; CASE 5C.4: a continued inheritance line
	     (t
	      (c-beginning-of-inheritance-list lim)
	      (c-add-syntax 'inher-cont (point))
	      ;; don't add inclass symbol since relative point already
	      ;; contains any class offset
	      )))
	   ;; CASE 5D: this could be a top-level initialization, a
	   ;; member init list continuation, or a template argument
	   ;; list continuation.
	   ((c-with-syntax-table (if (c-major-mode-is 'c++-mode)
				     c++-template-syntax-table
				   (syntax-table))
	      (save-excursion
		;; Note: We use the fact that lim is always after any
		;; preceding brace sexp.
		(while (and (zerop (c-backward-token-2 1 t lim))
			    (or (not (looking-at "[;<,=]"))
				(and c-overloadable-operators-regexp
				     (looking-at c-overloadable-operators-regexp)
				     (save-excursion
				       (c-backward-token-2 1 nil lim)
				       (looking-at "operator\\>[^_]"))))))
		(or (memq (char-after) '(?, ?=))
		    (and (c-major-mode-is 'c++-mode)
			 (zerop (c-backward-token-2 1 nil lim))
			 (eq (char-after) ?<)))))
	    (goto-char indent-point)
	    (setq placeholder
		  (c-beginning-of-member-init-list lim))
	    (cond
	     ;; CASE 5D.1: hanging member init colon, but watch out
	     ;; for bogus matches on access specifiers inside classes.
	     ((and placeholder
		   (save-excursion
		     (setq placeholder (point))
		     (c-backward-token-2 1 t lim)
		     (and (eq (char-after) ?:)
			  (not (eq (char-before) ?:))))
		   (save-excursion
		     (goto-char placeholder)
		     (back-to-indentation)
		     (or
		      (/= (car (save-excursion
				 (parse-partial-sexp (point) placeholder)))
			  0)
		      (and
		       (if c-opt-access-key
			   (not (looking-at c-opt-access-key)) t)
		       (not (looking-at c-class-key))
		       (if c-opt-bitfield-key
			   (not (looking-at c-opt-bitfield-key)) t))
		      )))
	      (goto-char placeholder)
	      (c-forward-syntactic-ws)
	      (c-add-syntax 'member-init-cont (point))
	      ;; we do not need to add class offset since relative
	      ;; point is the member init above us
	      )
	     ;; CASE 5D.2: non-hanging member init colon
	     ((progn
		(c-forward-syntactic-ws indent-point)
		(eq (char-after) ?:))
	      (skip-chars-forward " \t:")
	      (c-add-syntax 'member-init-cont (point)))
	     ;; CASE 5D.3: perhaps a template list continuation?
	     ((and (c-major-mode-is 'c++-mode)
		   (save-excursion
		     (save-restriction
		       (c-with-syntax-table c++-template-syntax-table
			 (goto-char indent-point)
			 (setq placeholder (c-up-list-backward (point)))
			 (and placeholder
			      (eq (char-after placeholder) ?<))))))
	      ;; we can probably indent it just like an arglist-cont
	      (goto-char placeholder)
	      (c-beginning-of-statement-1 lim t)
	      (c-add-syntax 'template-args-cont (c-point 'boi)))
	     ;; CASE 5D.4: perhaps a multiple inheritance line?
	     ((and (c-major-mode-is 'c++-mode)
		   (save-excursion
		     (c-beginning-of-statement-1 lim)
		     (setq placeholder (point))
		     (if (looking-at "static\\>[^_]")
			 (c-forward-token-2 1 nil indent-point))
		     (and (looking-at c-class-key)
			  (zerop (c-forward-token-2 2 nil indent-point))
			  (if (eq (char-after) ?<)
			      (c-with-syntax-table c++-template-syntax-table
				(zerop (c-forward-token-2 1 t indent-point)))
			    t)
			  (eq (char-after) ?:))))
	      (goto-char placeholder)
	      (c-add-syntax 'inher-cont (c-point 'boi)))
	     ;; CASE 5D.5: Continuation of the "expression part" of a
	     ;; top level construct.
	     (t
	      (while (and (eq (car (c-beginning-of-decl-1 containing-sexp))
			      'same)
			  (save-excursion
			    (c-backward-syntactic-ws)
			    (eq (char-before) ?}))))
	      (c-add-stmt-syntax
	       (if (eq char-before-ip ?,)
		   ;; A preceding comma at the top level means that a
		   ;; new variable declaration starts here.  Use
		   ;; topmost-intro-cont for it, for consistency with
		   ;; the first variable declaration.  C.f. case 5N.
		   'topmost-intro-cont
		 'statement-cont)
	       nil nil nil containing-sexp paren-state))
	     ))
	   ;; CASE 5E: we are looking at a access specifier
	   ((and inclass-p
		 c-opt-access-key
		 (looking-at c-opt-access-key))
	    (setq placeholder (c-add-class-syntax 'inclass inclass-p
						  paren-state))
	    ;; Append access-label with the same anchor point as inclass gets.
	    (c-append-syntax 'access-label placeholder))
	   ;; CASE 5F: Close of a non-class declaration level block.
	   ((and inenclosing-p
		 (eq char-after-ip ?}))
	    (c-add-syntax (intern (concat inenclosing-p "-close"))
			  (aref inclass-p 0)))
	   ;; CASE 5G: we are looking at the brace which closes the
	   ;; enclosing nested class decl
	   ((and inclass-p
		 (eq char-after-ip ?})
		 (save-excursion
		   (save-restriction
		     (widen)
		     (forward-char 1)
		     (and (c-safe (c-backward-sexp 1) t)
			  (= (point) (aref inclass-p 1))
			  ))))
	    (c-add-class-syntax 'class-close inclass-p paren-state))
	   ;; CASE 5H: we could be looking at subsequent knr-argdecls
	   ((and c-recognize-knr-p
		 (not (eq char-before-ip ?}))
		 (save-excursion
		   (setq placeholder (cdr (c-beginning-of-decl-1 lim)))
		   (and placeholder
			;; Do an extra check to avoid tripping up on
			;; statements that occur in invalid contexts
			;; (e.g. in macro bodies where we don't really
			;; know the context of what we're looking at).
			(not (and c-opt-block-stmt-key
				  (looking-at c-opt-block-stmt-key)))))
		 (< placeholder indent-point))
	    (goto-char placeholder)
	    (c-add-syntax 'knr-argdecl (point)))
	   ;; CASE 5I: ObjC method definition.
	   ((and c-opt-method-key
		 (looking-at c-opt-method-key))
	    (c-beginning-of-statement-1 nil t)
	    (if (= (point) indent-point)
		;; Handle the case when it's the first (non-comment)
		;; thing in the buffer.  Can't look for a 'same return
		;; value from cbos1 since ObjC directives currently
		;; aren't recognized fully, so that we get 'same
		;; instead of 'previous if it moved over a preceding
		;; directive.
		(goto-char (point-min)))
	    (c-add-syntax 'objc-method-intro (c-point 'boi)))
           ;; CASE 5P: AWK pattern or function or continuation
           ;; thereof.
           ((c-mode-is-new-awk-p)
            (setq placeholder (point))
            (c-add-stmt-syntax
             (if (and (eq (c-beginning-of-statement-1) 'same)
                      (/= (point) placeholder))
                 'topmost-intro-cont
               'topmost-intro)
             nil nil nil
             containing-sexp paren-state))
	   ;; CASE 5N: At a variable declaration that follows a class
	   ;; definition or some other block declaration that doesn't
	   ;; end at the closing '}'.  C.f. case 5D.5.
	   ((progn
	      (c-backward-syntactic-ws lim)
	      (and (eq (char-before) ?})
		   (save-excursion
		     (let ((start (point)))
		       (if paren-state
			   ;; Speed up the backward search a bit.
			   (goto-char (car (car paren-state))))
		       (c-beginning-of-decl-1 containing-sexp)
		       (setq placeholder (point))
		       (if (= start (point))
			   ;; The '}' is unbalanced.
			   nil
			 (c-end-of-decl-1)
			 (>= (point) indent-point))))))
	    (goto-char placeholder)
	    (c-add-stmt-syntax 'topmost-intro-cont nil nil nil
			       containing-sexp paren-state))
	   ;; CASE 5J: we are at the topmost level, make
	   ;; sure we skip back past any access specifiers
	   ((progn
	      (while (and inclass-p
			  c-opt-access-key
			  (not (bobp))
			  (save-excursion
			    (c-safe (c-backward-sexp 1) t)
			    (looking-at c-opt-access-key)))
		(c-backward-sexp 1)
		(c-backward-syntactic-ws lim))
	      (or (bobp)
                  (if (c-mode-is-new-awk-p)
                      (not (c-awk-prev-line-incomplete-p))
                    (memq (char-before) '(?\; ?})))
		  (and (c-major-mode-is 'objc-mode)
		       (progn
			 (c-beginning-of-statement-1 lim)
			 (eq (char-after) ?@)))))
	    ;; real beginning-of-line could be narrowed out due to
	    ;; enclosure in a class block
	    (save-restriction
	      (widen)
	      (c-add-syntax 'topmost-intro (c-point 'bol))
	      ;; Using bol instead of boi above is highly bogus, and
	      ;; it makes our lives hard to remain compatible. :P
	      (if inclass-p
		  (progn
		    (goto-char (aref inclass-p 1))
		    (or (= (point) (c-point 'boi))
			(goto-char (aref inclass-p 0)))
		    (if inenclosing-p
			(c-add-syntax (intern (concat "in" inenclosing-p))
				      (c-point 'boi))
		      (c-add-class-syntax 'inclass inclass-p paren-state))
		    ))
	      (when (and c-syntactic-indentation-in-macros
			 macro-start
			 (/= macro-start (c-point 'boi indent-point)))
		(c-add-syntax 'cpp-define-intro)
		(setq macro-start nil))
	      ))
	   ;; CASE 5K: we are at an ObjC method definition
	   ;; continuation line.
	   ((and c-opt-method-key
		 (save-excursion
		   (goto-char indent-point)
		   (c-beginning-of-statement-1 lim)
		   (beginning-of-line)
		   (when (looking-at c-opt-method-key)
		     (setq placeholder (point)))))
	    (c-add-syntax 'objc-method-args-cont placeholder))
	   ;; CASE 5L: we are at the first argument of a template
	   ;; arglist that begins on the previous line.
	   ((eq (char-before) ?<)
	    (c-beginning-of-statement-1 (c-safe-position (point) paren-state))
	    (c-add-syntax 'template-args-cont (c-point 'boi)))
	   ;; CASE 5M: we are at a topmost continuation line
	   (t
	    (c-beginning-of-statement-1 (c-safe-position (point) paren-state))
	    (c-add-syntax 'topmost-intro-cont (c-point 'boi)))
	   ))
	 ;; (CASE 6 has been removed.)
	 ;; CASE 7: line is an expression, not a statement.  Most
	 ;; likely we are either in a function prototype or a function
	 ;; call argument list
	 ((not (or (and c-special-brace-lists
			(save-excursion
			  (goto-char containing-sexp)
			  (c-looking-at-special-brace-list)))
		   (eq (char-after containing-sexp) ?{)))
	  (cond
	   ;; CASE 7A: we are looking at the arglist closing paren.
	   ;; C.f. case 7F.
	   ((memq char-after-ip '(?\) ?\]))
	    (goto-char containing-sexp)
	    (setq placeholder (c-point 'boi))
	    (if (and (c-safe (backward-up-list 1) t)
		     (>= (point) placeholder))
		(progn
		  (forward-char)
		  (skip-chars-forward " \t"))
	      (goto-char placeholder))
	    (c-add-stmt-syntax 'arglist-close (list containing-sexp) t nil
			       (c-most-enclosing-brace paren-state (point))
			       (c-whack-state-after (point) paren-state)))
	   ;; CASE 7B: Looking at the opening brace of an
	   ;; in-expression block or brace list.  C.f. cases 4, 16A
	   ;; and 17E.
	   ((and (eq char-after-ip ?{)
		 (progn
		   (setq placeholder (c-inside-bracelist-p (point)
							   c-state-cache))
		   (if placeholder
		       (setq tmpsymbol '(brace-list-open . inexpr-class))
		     (setq tmpsymbol '(block-open . inexpr-statement)
			   placeholder
			   (cdr-safe (c-looking-at-inexpr-block
				      (c-safe-position containing-sexp
						       paren-state)
				      containing-sexp)))
		     ;; placeholder is nil if it's a block directly in
		     ;; a function arglist.  That makes us skip out of
		     ;; this case.
		     )))
	    (goto-char placeholder)
	    (back-to-indentation)
	    (c-add-stmt-syntax (car tmpsymbol) nil t nil
			       (c-most-enclosing-brace paren-state (point))
			       (c-whack-state-after (point) paren-state))
	    (if (/= (point) placeholder)
		(c-add-syntax (cdr tmpsymbol))))
	   ;; CASE 7C: we are looking at the first argument in an empty
	   ;; argument list. Use arglist-close if we're actually
	   ;; looking at a close paren or bracket.
	   ((memq char-before-ip '(?\( ?\[))
	    (goto-char containing-sexp)
	    (setq placeholder (c-point 'boi))
	    (when (and (c-safe (backward-up-list 1) t)
		       (>= (point) placeholder))
	      (forward-char)
	      (skip-chars-forward " \t")
	      (setq placeholder (point)))
	    (c-add-syntax 'arglist-intro placeholder))
	   ;; CASE 7D: we are inside a conditional test clause. treat
	   ;; these things as statements
	   ((progn
	      (goto-char containing-sexp)
	      (and (c-safe (c-forward-sexp -1) t)
		   (looking-at "\\<for\\>[^_]")))
	    (goto-char (1+ containing-sexp))
	    (c-forward-syntactic-ws indent-point)
	    (if (eq char-before-ip ?\;)
		(c-add-syntax 'statement (point))
	      (c-add-syntax 'statement-cont (point))
	      ))
	   ;; CASE 7E: maybe a continued ObjC method call. This is the
	   ;; case when we are inside a [] bracketed exp, and what
	   ;; precede the opening bracket is not an identifier.
	   ((and c-opt-method-key
		 (eq (char-after containing-sexp) ?\[)
		 (progn
		   (goto-char (1- containing-sexp))
		   (c-backward-syntactic-ws (c-point 'bod))
		   (if (not (looking-at c-symbol-key))
		       (c-add-syntax 'objc-method-call-cont containing-sexp))
		   )))
	   ;; CASE 7F: we are looking at an arglist continuation line,
	   ;; but the preceding argument is on the same line as the
	   ;; opening paren.  This case includes multi-line
	   ;; mathematical paren groupings, but we could be on a
	   ;; for-list continuation line.  C.f. case 7A.
	   ((progn
	      (goto-char (1+ containing-sexp))
	      (skip-chars-forward " \t")
	      (and (not (eolp))
		   (not (looking-at "\\\\$"))))
	    (goto-char containing-sexp)
	    (setq placeholder (c-point 'boi))
	    (if (and (c-safe (backward-up-list 1) t)
		     (>= (point) placeholder))
		(progn
		  (forward-char)
		  (skip-chars-forward " \t"))
	      (goto-char placeholder))
	    (c-add-stmt-syntax 'arglist-cont-nonempty (list containing-sexp)
			       t nil
			       (c-most-enclosing-brace c-state-cache (point))
			       (c-whack-state-after (point) paren-state)))
	   ;; CASE 7G: we are looking at just a normal arglist
	   ;; continuation line
	   (t (c-forward-syntactic-ws indent-point)
	      (c-add-syntax 'arglist-cont (c-point 'boi)))
	   ))
	 ;; CASE 8: func-local multi-inheritance line
	 ((and (c-major-mode-is 'c++-mode)
	       (save-excursion
		 (goto-char indent-point)
		 (skip-chars-forward " \t")
		 (looking-at c-opt-postfix-decl-spec-key)))
	  (goto-char indent-point)
	  (skip-chars-forward " \t")
	  (cond
	   ;; CASE 8A: non-hanging colon on an inher intro
	   ((eq char-after-ip ?:)
	    (c-backward-syntactic-ws lim)
	    (c-add-syntax 'inher-intro (c-point 'boi)))
	   ;; CASE 8B: hanging colon on an inher intro
	   ((eq char-before-ip ?:)
	    (c-add-syntax 'inher-intro (c-point 'boi)))
	   ;; CASE 8C: a continued inheritance line
	   (t
	    (c-beginning-of-inheritance-list lim)
	    (c-add-syntax 'inher-cont (point))
	    )))
	 ;; CASE 9: we are inside a brace-list
	 ((and (not (c-mode-is-new-awk-p))  ; Maybe this isn't needed (ACM, 2002/3/29)
               (setq special-brace-list
                     (or (and c-special-brace-lists
                              (save-excursion
                                (goto-char containing-sexp)
                                (c-looking-at-special-brace-list)))
                         (c-inside-bracelist-p containing-sexp paren-state))))
	  (cond
	   ;; CASE 9A: In the middle of a special brace list opener.
	   ((and (consp special-brace-list)
		 (save-excursion
		   (goto-char containing-sexp)
		   (eq (char-after) ?\())
		 (eq char-after-ip (car (cdr special-brace-list))))
	    (goto-char (car (car special-brace-list)))
	    (skip-chars-backward " \t")
	    (if (and (bolp)
		     (assoc 'statement-cont
			    (setq placeholder (c-guess-basic-syntax))))
		(setq c-syntactic-context placeholder)
	      (c-beginning-of-statement-1
	       (c-safe-position (1- containing-sexp) paren-state))
	      (c-forward-token-2 0)
	      (while (looking-at c-specifier-key)
		(goto-char (match-end 1))
		(c-forward-syntactic-ws))
	      (c-add-syntax 'brace-list-open (c-point 'boi))))
	   ;; CASE 9B: brace-list-close brace
	   ((if (consp special-brace-list)
		;; Check special brace list closer.
		(progn
		  (goto-char (car (car special-brace-list)))
		  (save-excursion
		    (goto-char indent-point)
		    (back-to-indentation)
		    (or
		     ;; We were between the special close char and the `)'.
		     (and (eq (char-after) ?\))
			  (eq (1+ (point)) (cdr (car special-brace-list))))
		     ;; We were before the special close char.
		     (and (eq (char-after) (cdr (cdr special-brace-list)))
			  (zerop (c-forward-token-2))
			  (eq (1+ (point)) (cdr (car special-brace-list)))))))
	      ;; Normal brace list check.
	      (and (eq char-after-ip ?})
		   (c-safe (goto-char (c-up-list-backward (point))) t)
		   (= (point) containing-sexp)))
	    (if (eq (point) (c-point 'boi))
		(c-add-syntax 'brace-list-close (point))
	      (setq lim (c-most-enclosing-brace c-state-cache (point)))
	      (c-beginning-of-statement-1 lim)
	      (c-add-stmt-syntax 'brace-list-close nil t t lim
				 (c-whack-state-after (point) paren-state))))
	   (t
	    ;; Prepare for the rest of the cases below by going to the
	    ;; token following the opening brace
	    (if (consp special-brace-list)
		(progn
		  (goto-char (car (car special-brace-list)))
		  (c-forward-token-2 1 nil indent-point))
	      (goto-char containing-sexp))
	    (forward-char)
	    (let ((start (point)))
	      (c-forward-syntactic-ws indent-point)
	      (goto-char (max start (c-point 'bol))))
	    (c-skip-ws-forward indent-point)
	    (cond
	     ;; CASE 9C: we're looking at the first line in a brace-list
	     ((= (point) indent-point)
	      (if (consp special-brace-list)
		  (goto-char (car (car special-brace-list)))
		(goto-char containing-sexp))
	      (if (eq (point) (c-point 'boi))
		  (c-add-syntax 'brace-list-intro (point))
		(setq lim (c-most-enclosing-brace c-state-cache (point)))
		(c-beginning-of-statement-1 lim)
		(c-add-stmt-syntax 'brace-list-intro nil t t lim
				   (c-whack-state-after (point) paren-state))))
	     ;; CASE 9D: this is just a later brace-list-entry or
	     ;; brace-entry-open
	     (t (if (or (eq char-after-ip ?{)
			(and c-special-brace-lists
			     (save-excursion
			       (goto-char indent-point)
			       (c-forward-syntactic-ws (c-point 'eol))
			       (c-looking-at-special-brace-list (point)))))
		    (c-add-syntax 'brace-entry-open (point))
		  (c-add-syntax 'brace-list-entry (point))
		  ))
	     ))))
	 ;; CASE 10: A continued statement or top level construct.
	 ((and (if (c-mode-is-new-awk-p)
                   (c-awk-prev-line-incomplete-p containing-sexp) ; ACM 2002/3/29
                 (and (not (memq char-before-ip '(?\; ?:)))
                      (or (not (eq char-before-ip ?}))
                          (c-looking-at-inexpr-block-backward c-state-cache))))
	       (> (point)
		  (save-excursion
		    (c-beginning-of-statement-1 containing-sexp)
		    (setq placeholder (point))))
	       (/= placeholder containing-sexp))
	  ;; This is shared with case 18.
	  (c-guess-continued-construct indent-point
				       char-after-ip
				       placeholder
				       containing-sexp
				       paren-state))
	 ;; CASE 14: A case or default label
	 ((looking-at c-label-kwds-regexp)
	  (goto-char containing-sexp)
	  (setq lim (c-most-enclosing-brace c-state-cache containing-sexp))
	  (c-backward-to-block-anchor lim)
	  (c-add-stmt-syntax 'case-label nil t nil
			     lim paren-state))
	 ;; CASE 15: any other label
	 ((looking-at c-label-key)
	  (goto-char containing-sexp)
	  (setq lim (c-most-enclosing-brace c-state-cache containing-sexp))
	  (save-excursion
	    (setq tmpsymbol
		  (if (and (eq (c-beginning-of-statement-1 lim) 'up)
			   (looking-at "switch\\>[^_]"))
		      ;; If the surrounding statement is a switch then
		      ;; let's analyze all labels as switch labels, so
		      ;; that they get lined up consistently.
		      'case-label
		    'label)))
	  (c-backward-to-block-anchor lim)
	  (c-add-stmt-syntax tmpsymbol nil t nil
			     lim paren-state))
	 ;; CASE 16: block close brace, possibly closing the defun or
	 ;; the class
	 ((eq char-after-ip ?})
	  ;; From here on we have the next containing sexp in lim.
	  (setq lim (c-most-enclosing-brace paren-state))
	  (goto-char containing-sexp)
	    (cond
	     ;; CASE 16E: Closing a statement block?  This catches
	     ;; cases where it's preceded by a statement keyword,
	     ;; which works even when used in an "invalid" context,
	     ;; e.g. a macro argument.
	     ((c-after-conditional)
	      (c-backward-to-block-anchor lim)
	      (c-add-stmt-syntax 'block-close nil t nil
				 lim paren-state))
	     ;; CASE 16A: closing a lambda defun or an in-expression
	     ;; block?  C.f. cases 4, 7B and 17E.
	     ((setq placeholder (c-looking-at-inexpr-block
				 (c-safe-position containing-sexp paren-state)
				 nil))
	      (setq tmpsymbol (if (eq (car placeholder) 'inlambda)
				  'inline-close
				'block-close))
	      (goto-char containing-sexp)
	      (back-to-indentation)
	      (if (= containing-sexp (point))
		  (c-add-syntax tmpsymbol (point))
		(goto-char (cdr placeholder))
		(back-to-indentation)
		(c-add-stmt-syntax tmpsymbol nil t nil
				   (c-most-enclosing-brace paren-state (point))
				   (c-whack-state-after (point) paren-state))
		(if (/= (point) (cdr placeholder))
		    (c-add-syntax (car placeholder)))))
	     ;; CASE 16B: does this close an inline or a function in
	     ;; a non-class declaration level block?
	     ((setq placeholder (c-search-uplist-for-classkey paren-state))
	      (c-backward-to-decl-anchor lim)
	      (back-to-indentation)
	      (if (save-excursion
		    (goto-char (aref placeholder 0))
		    (looking-at c-other-decl-block-key))
		  (c-add-syntax 'defun-close (point))
		(c-add-syntax 'inline-close (point))))
	     ;; CASE 16F: Can be a defun-close of a function declared
	     ;; in a statement block, e.g. in Pike or when using gcc
	     ;; extensions, but watch out for macros followed by
	     ;; blocks.  Let it through to be handled below.
	     ;; C.f. cases B.3 and 17G.
	     ((and (not inenclosing-p)
		   lim
		   (save-excursion
		     (and (not (c-looking-at-bos))
			  (eq (c-beginning-of-statement-1 lim nil nil t) 'same)
			  (setq placeholder (point))
			  ;; Look for a type or identifier followed by a
			  ;; symbol, i.e. the start of a function declaration.
			  ;; Doesn't work for declarations like "int *foo()
			  ;; ..."; we'd need to refactor the more competent
			  ;; analysis in `c-font-lock-declarations' for that.
			  (c-forward-type)
			  (progn
			    (c-forward-syntactic-ws)
			    (looking-at c-symbol-start)))))
	      (back-to-indentation)
	      (if (/= (point) containing-sexp)
		  (goto-char placeholder))
	      (c-add-stmt-syntax 'defun-close nil t nil
				 lim paren-state))
	     ;; CASE 16C: if there an enclosing brace that hasn't
	     ;; been narrowed out by a class, then this is a
	     ;; block-close.  C.f. case 17H.
	     ((and (not inenclosing-p) lim)
	      ;; If the block is preceded by a case/switch label on
	      ;; the same line, we anchor at the first preceding label
	      ;; at boi.  The default handling in c-add-stmt-syntax is
	      ;; really fixes it better, but we do like this to keep
	      ;; the indentation compatible with version 5.28 and
	      ;; earlier.
	      (while (and (/= (setq placeholder (point)) (c-point 'boi))
			  (eq (c-beginning-of-statement-1 lim) 'label)))
	      (goto-char placeholder)
	      (if (looking-at c-label-kwds-regexp)
		  (c-add-syntax 'block-close (point))
		(goto-char containing-sexp)
		;; c-backward-to-block-anchor not necessary here; those
		;; situations are handled in case 16E above.
		(c-add-stmt-syntax 'block-close nil t nil
				   lim paren-state)))
	     ;; CASE 16D: find out whether we're closing a top-level
	     ;; class or a defun
	     (t
	      (save-restriction
		(narrow-to-region (point-min) indent-point)
		(let ((decl (c-search-uplist-for-classkey (c-parse-state))))
		  (if decl
		      (c-add-class-syntax 'class-close decl paren-state)
		    (goto-char containing-sexp)
		    (c-backward-to-decl-anchor lim)
		    (back-to-indentation)
		    (c-add-syntax 'defun-close (point)))))
	      )))
	 ;; CASE 17: Statement or defun catchall.
	 (t
	  (goto-char indent-point)
	  ;; Back up statements until we find one that starts at boi.
	  (while (let* ((prev-point (point))
			(last-step-type (c-beginning-of-statement-1
					 containing-sexp)))
		   (if (= (point) prev-point)
		       (progn
			 (setq step-type (or step-type last-step-type))
			 nil)
		     (setq step-type last-step-type)
		     (/= (point) (c-point 'boi)))))
	  (cond
	   ;; CASE 17B: continued statement
	   ((and (eq step-type 'same)
		 (/= (point) indent-point))
	    (c-add-stmt-syntax 'statement-cont nil nil nil
			       containing-sexp paren-state))
	   ;; CASE 17A: After a case/default label?
	   ((progn
	      (while (and (eq step-type 'label)
			  (not (looking-at c-label-kwds-regexp)))
		(setq step-type
		      (c-beginning-of-statement-1 containing-sexp)))
	      (eq step-type 'label))
	    (c-add-stmt-syntax (if (eq char-after-ip ?{)
				   'statement-case-open
				 'statement-case-intro)
			       nil t nil containing-sexp paren-state))
	   ;; CASE 17D: any old statement
	   ((progn
	      (while (eq step-type 'label)
		(setq step-type
		      (c-beginning-of-statement-1 containing-sexp)))
	      (eq step-type 'previous))
	    (c-add-stmt-syntax 'statement nil t nil
			       containing-sexp paren-state)
	    (if (eq char-after-ip ?{)
		(c-add-syntax 'block-open)))
	   ;; CASE 17I: Inside a substatement block.
	   ((progn
	      ;; The following tests are all based on containing-sexp.
	      (goto-char containing-sexp)
	      ;; From here on we have the next containing sexp in lim.
	      (setq lim (c-most-enclosing-brace paren-state containing-sexp))
	      (c-after-conditional))
	    (c-backward-to-block-anchor lim)
	    (c-add-stmt-syntax 'statement-block-intro nil t nil
			       lim paren-state)
	    (if (eq char-after-ip ?{)
		(c-add-syntax 'block-open)))
	   ;; CASE 17E: first statement in an in-expression block.
	   ;; C.f. cases 4, 7B and 16A.
	   ((setq placeholder (c-looking-at-inexpr-block
			       (c-safe-position containing-sexp paren-state)
			       nil))
	    (setq tmpsymbol (if (eq (car placeholder) 'inlambda)
				'defun-block-intro
			      'statement-block-intro))
	    (back-to-indentation)
	    (if (= containing-sexp (point))
		(c-add-syntax tmpsymbol (point))
	      (goto-char (cdr placeholder))
	      (back-to-indentation)
	      (c-add-stmt-syntax tmpsymbol nil t nil
				 (c-most-enclosing-brace c-state-cache (point))
				 (c-whack-state-after (point) paren-state))
	      (if (/= (point) (cdr placeholder))
		  (c-add-syntax (car placeholder))))
	    (if (eq char-after-ip ?{)
		(c-add-syntax 'block-open)))
	   ;; CASE 17F: first statement in an inline, or first
	   ;; statement in a top-level defun. we can tell this is it
	   ;; if there are no enclosing braces that haven't been
	   ;; narrowed out by a class (i.e. don't use bod here).
	   ((save-excursion
	      (save-restriction
		(widen)
		(c-narrow-out-enclosing-class paren-state containing-sexp)
		(not (c-most-enclosing-brace paren-state))))
	    (c-backward-to-decl-anchor lim)
	    (back-to-indentation)
	    (c-add-syntax 'defun-block-intro (point)))
	   ;; CASE 17G: First statement in a function declared inside
	   ;; a normal block.  This can occur in Pike and with
	   ;; e.g. the gcc extensions, but watch out for macros
	   ;; followed by blocks.  C.f. cases B.3 and 16F.
	   ((save-excursion
	      (and (not (c-looking-at-bos))
		   (eq (c-beginning-of-statement-1 lim nil nil t) 'same)
		   (setq placeholder (point))
		   ;; Look for a type or identifier followed by a
		   ;; symbol, i.e. the start of a function declaration.
		   ;; Doesn't work for declarations like "int *foo()
		   ;; ..."; we'd need to refactor the more competent
		   ;; analysis in `c-font-lock-declarations' for that.
		   (c-forward-type)
		   (progn
		     (c-forward-syntactic-ws)
		     (looking-at c-symbol-start))))
	    (back-to-indentation)
	    (if (/= (point) containing-sexp)
		(goto-char placeholder))
	    (c-add-stmt-syntax 'defun-block-intro nil t nil
			       lim paren-state))
	   ;; CASE 17H: First statement in a block.  C.f. case 16C.
	   (t
	    ;; If the block is preceded by a case/switch label on the
	    ;; same line, we anchor at the first preceding label at
	    ;; boi.  The default handling in c-add-stmt-syntax is
	    ;; really fixes it better, but we do like this to keep the
	    ;; indentation compatible with version 5.28 and earlier.
	    (while (and (/= (setq placeholder (point)) (c-point 'boi))
			(eq (c-beginning-of-statement-1 lim) 'label)))
	    (goto-char placeholder)
	    (if (looking-at c-label-kwds-regexp)
		(c-add-syntax 'statement-block-intro (point))
	      (goto-char containing-sexp)
	      ;; c-backward-to-block-anchor not necessary here; those
	      ;; situations are handled in case 17I above.
	      (c-add-stmt-syntax 'statement-block-intro nil t nil
				 lim paren-state))
	    (if (eq char-after-ip ?{)
		(c-add-syntax 'block-open)))
	   ))
	 )
	;; now we need to look at any modifiers
	(goto-char indent-point)
	(skip-chars-forward " \t")
	;; are we looking at a comment only line?
	(when (and (looking-at c-comment-start-regexp)
		   (/= (c-forward-token-2 0 nil (c-point 'eol)) 0))
	  (c-append-syntax 'comment-intro))
	;; we might want to give additional offset to friends (in C++).
	(when (and c-opt-friend-key
		   (looking-at c-opt-friend-key))
	  (c-append-syntax 'friend))

	;; Set syntactic-relpos.
	(let ((p c-syntactic-context))
	  (while (and p
		      (if (integerp (car-safe (cdr-safe (car p))))
			  (progn
			    (setq syntactic-relpos (car (cdr (car p))))
			    nil)
			t))
	    (setq p (cdr p))))

	;; Start of or a continuation of a preprocessor directive?
	(if (and macro-start
		 (eq macro-start (c-point 'boi))
		 (not (and (c-major-mode-is 'pike-mode)
			   (eq (char-after (1+ macro-start)) ?\"))))
	    (c-append-syntax 'cpp-macro)
	  (when (and c-syntactic-indentation-in-macros macro-start)
	    (if in-macro-expr
		(when (or
		       (< syntactic-relpos macro-start)
		       (not (or
			     (assq 'arglist-intro c-syntactic-context)
			     (assq 'arglist-cont c-syntactic-context)
			     (assq 'arglist-cont-nonempty c-syntactic-context)
			     (assq 'arglist-close c-syntactic-context))))
		  ;; If inside a cpp expression, i.e. anywhere in a
		  ;; cpp directive except a #define body, we only let
		  ;; through the syntactic analysis that is internal
		  ;; in the expression.  That means the arglist
		  ;; elements, if they are anchored inside the cpp
		  ;; expression.
		  (setq c-syntactic-context nil)
		  (c-add-syntax 'cpp-macro-cont macro-start))
	      (when (and (eq macro-start syntactic-relpos)
			 (not (assq 'cpp-define-intro c-syntactic-context))
			 (save-excursion
			   (goto-char macro-start)
			   (or (not (c-forward-to-cpp-define-body))
			       (<= (point) (c-point 'boi indent-point)))))
		;; Inside a #define body and the syntactic analysis is
		;; anchored on the start of the #define.  In this case
		;; we add cpp-define-intro to get the extra
		;; indentation of the #define body.
		(c-add-syntax 'cpp-define-intro)))))
	;; return the syntax
	c-syntactic-context))))


;; Indentation calculation.

(defun c-evaluate-offset (offset langelem symbol)
  ;; offset can be a number, a function, a variable, a list, or one of
  ;; the symbols + or -
  (cond
   ((eq offset '+)         c-basic-offset)
   ((eq offset '-)         (- c-basic-offset))
   ((eq offset '++)        (* 2 c-basic-offset))
   ((eq offset '--)        (* 2 (- c-basic-offset)))
   ((eq offset '*)         (/ c-basic-offset 2))
   ((eq offset '/)         (/ (- c-basic-offset) 2))
   ((numberp offset)       offset)
   ((functionp offset)     (c-evaluate-offset
			    (funcall offset
				     (cons (car langelem)
					   (car-safe (cdr langelem))))
			    langelem symbol))
   ((vectorp offset)       offset)
   ((null offset)          nil)
   ((listp offset)
    (if (eq (car offset) 'quote)
	(error
"Setting in c-offsets-alist element \"(%s . '%s)\" was mistakenly quoted"
         symbol (cadr offset)))
    (let (done)
      (while (and (not done) offset)
	(setq done (c-evaluate-offset (car offset) langelem symbol)
	      offset (cdr offset)))
      (if (and c-strict-syntax-p (not done))
	  (c-benign-error "No offset found for syntactic symbol %s" symbol))
      done))
   (t (symbol-value offset))
   ))

(defun c-calc-offset (langelem)
  ;; Get offset from LANGELEM which is a list beginning with the
  ;; syntactic symbol and followed by any analysis data it provides.
  ;; That data may be zero or more elements, but if at least one is
  ;; given then the first is the relpos (or nil).  The symbol is
  ;; matched against `c-offsets-alist' and the offset calculated from
  ;; that is returned.
  (let* ((symbol (car langelem))
	 (match  (assq symbol c-offsets-alist))
	 (offset (cdr-safe match)))
    (if match
	(setq offset (c-evaluate-offset offset langelem symbol))
      (if c-strict-syntax-p
	  (c-benign-error "No offset found for syntactic symbol %s" symbol))
      (setq offset 0))
    (if (vectorp offset)
	offset
      (or (and (numberp offset) offset)
	  (and (symbolp offset) (symbol-value offset))
	  0))
    ))

(defun c-get-offset (langelem)
  ;; This is a compatibility wrapper for `c-calc-offset' in case
  ;; someone is calling it directly.  It takes an old style syntactic
  ;; element on the form (SYMBOL . RELPOS) and converts it to the new
  ;; list form.
  (if (cdr langelem)
      (c-calc-offset (list (car langelem) (cdr langelem)))
    (c-calc-offset langelem)))

(defun c-get-syntactic-indentation (langelems)
  ;; Calculate the syntactic indentation from a syntactic description
  ;; as returned by `c-guess-syntax'.
  ;;
  ;; Note that topmost-intro always has a relpos at bol, for
  ;; historical reasons.  It's often used together with other symbols
  ;; that has more sane positions.  Since we always use the first
  ;; found relpos, we rely on that these other symbols always precede
  ;; topmost-intro in the LANGELEMS list.
  (let ((indent 0) anchor)

    (while langelems
      (let* ((c-syntactic-element (car langelems))
	     (res (c-calc-offset c-syntactic-element)))

	(if (vectorp res)
	    ;; Got an absolute column that overrides any indentation
	    ;; we've collected so far, but not the relative
	    ;; indentation we might get for the nested structures
	    ;; further down the langelems list.
	    (setq indent (elt res 0)
		  anchor (point-min))	; A position at column 0.

	  ;; Got a relative change of the current calculated
	  ;; indentation.
	  (setq indent (+ indent res))

	  ;; Use the anchor position from the first syntactic
	  ;; element with one.
	  (unless anchor
	    (let ((relpos (car-safe (cdr (car langelems)))))
	      (if relpos
		  (setq anchor relpos)))))

	(setq langelems (cdr langelems))))

    (if anchor
	(+ indent (save-excursion
		    (goto-char anchor)
		    (current-column)))
      indent)))


(cc-provide 'cc-engine)

;;; arch-tag: 149add18-4673-4da5-ac47-6805e4eae089
;;; cc-engine.el ends here
