;;; cc-styles.el --- support for styles in CC Mode

;; Copyright (C) 1985,87,92,93,94,95,96,97,98 Free Software Foundation, Inc.

;; Authors:    1998 Barry A. Warsaw and Martin Stjernholm
;;             1992-1997 Barry A. Warsaw
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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;; explicit compile-time dependencies
(eval-when-compile
  (require 'cc-defs))


;; Warning: don't eval-defun this constant or you'll break style inheritance.
(defconst c-style-alist
  '(("gnu"
     (c-basic-offset . 2)
     (c-comment-only-line-offset . (0 . 0))
     (c-offsets-alist . ((statement-block-intro . +)
			 (knr-argdecl-intro . 5)
			 (substatement-open . +)
			 (label . 0)
			 (statement-case-open . +)
			 (statement-cont . +)
			 (arglist-intro . c-lineup-arglist-intro-after-paren)
			 (arglist-close . c-lineup-arglist)
			 (inline-open . 0)
			 ))
     (c-special-indent-hook . c-gnu-impose-minimum)
     (c-comment-continuation-stars . "")
     (c-hanging-comment-ender-p . t)
     )
    ("k&r"
     (c-basic-offset . 5)
     (c-comment-only-line-offset . 0)
     (c-offsets-alist . ((statement-block-intro . +)
			 (knr-argdecl-intro . 0)
			 (substatement-open . 0)
			 (label . 0)
			 (statement-cont . +)
			 ))
     )
    ("bsd"
     (c-basic-offset . 4)
     (c-comment-only-line-offset . 0)
     (c-offsets-alist . ((statement-block-intro . +)
			 (knr-argdecl-intro . +)
			 (substatement-open . 0)
			 (label . 0)
			 (statement-cont . +)
			 ))
     )
    ("stroustrup"
     (c-basic-offset . 4)
     (c-comment-only-line-offset . 0)
     (c-offsets-alist . ((statement-block-intro . +)
			 (substatement-open . 0)
			 (label . 0)
			 (statement-cont . +)
			 ))
     )
    ("whitesmith"
     (c-basic-offset . 4)
     (c-comment-only-line-offset . 0)
     (c-offsets-alist . ((statement-block-intro . +)
			 (knr-argdecl-intro . +)
			 (substatement-open . 0)
			 (label . 0)
			 (statement-cont . +)
			 ))

     )
    ("ellemtel"
     (c-basic-offset . 3)
     (c-comment-only-line-offset . 0)
     (c-hanging-braces-alist     . ((substatement-open before after)))
     (c-offsets-alist . ((topmost-intro        . 0)
                         (topmost-intro-cont   . 0)
                         (substatement         . +)
			 (substatement-open    . 0)
                         (case-label           . +)
                         (access-label         . -)
                         (inclass              . ++)
                         (inline-open          . 0)
                         ))
     )
    ("linux"
     (c-basic-offset  . 8)
     (c-comment-only-line-offset . 0)
     (c-hanging-braces-alist . ((brace-list-open)
				(brace-entry-open)
				(substatement-open after)
				(block-close . c-snug-do-while)))
     (c-cleanup-list . (brace-else-brace))
     (c-offsets-alist . ((statement-block-intro . +)
			 (knr-argdecl-intro     . 0)
			 (substatement-open     . 0)
			 (label                 . 0)
			 (statement-cont        . +)
			 ))
     )
    ("python"
     (indent-tabs-mode . t)
     (fill-column      . 72)
     (c-basic-offset   . 8)
     (c-offsets-alist  . ((substatement-open . 0)
			  (inextern-lang . 0)
			  (arglist-intro . +)
			  (knr-argdecl-intro . +)
			  ))
     (c-hanging-braces-alist . ((brace-list-open)
				(brace-list-intro)
				(brace-list-close)
				(brace-entry-open)
				(substatement-open after)
				(block-close . c-snug-do-while)
				))
     (c-comment-continuation-stars . "")
     (c-hanging-comment-ender-p . nil)
     (fill-column . 78)
     )
    ("java"
     (c-basic-offset . 4)
     (c-comment-only-line-offset . (0 . 0))
     ;; the following preserves Javadoc starter lines
     (c-hanging-comment-starter-p . nil)
     (c-offsets-alist . ((inline-open . 0)
			 (topmost-intro-cont    . +)
			 (statement-block-intro . +)
 			 (knr-argdecl-intro     . 5)
 			 (substatement-open     . +)
 			 (label                 . 0)
 			 (statement-case-open   . +)
 			 (statement-cont        . +)
 			 (arglist-intro  . c-lineup-arglist-intro-after-paren)
 			 (arglist-close  . c-lineup-arglist)
 			 (access-label   . 0)
			 (inher-cont     . c-lineup-java-inher)
			 (func-decl-cont . c-lineup-java-throws)
			 ))

     )
    )
  "Styles of indentation.
Elements of this alist are of the form:

  (STYLE-STRING [BASE-STYLE] (VARIABLE . VALUE) [(VARIABLE . VALUE) ...])

where STYLE-STRING is a short descriptive string used to select a
style, VARIABLE is any Emacs variable, and VALUE is the intended value
for that variable when using the selected style.

Optional BASE-STYLE if present, is a string and must follow
STYLE-STRING.  BASE-STYLE names a style that this style inherits from.
By default, all styles inherit from the \"cc-mode\" style, which is
computed at run time.  Style loops generate errors.

Two variables are treated specially.  When VARIABLE is
`c-offsets-alist', the VALUE is a list containing elements of the
form:

  (SYNTACTIC-SYMBOL . OFFSET)

as described in `c-offsets-alist'.  These are passed directly to
`c-set-offset' so there is no need to set every syntactic symbol in
your style, only those that are different from the default.

When VARIABLE is `c-special-indent-hook', its VALUE is added to
`c-special-indent-hook' using `add-hook'.  If VALUE is a list, each
element of the list is added with `add-hook'.

Do not change this variable directly.  Use the function `c-add-style'
to add new styles or modify existing styles (it is not a good idea to
modify existing styles -- you should create a new style that inherits
the existing style.")


;; Functions that manipulate styles
(defun c-set-style-1 (conscell)
  ;; Set the style for one variable
  (let ((attr (car conscell))
	(val  (cdr conscell)))
    (cond
     ;; first special variable
     ((eq attr 'c-offsets-alist)
      (mapcar
       (function
	(lambda (langentry)
	  (let ((langelem (car langentry))
		(offset (cdr langentry)))
	    (c-set-offset langelem offset)
	    )))
       val))
     ;; second special variable
     ((eq attr 'c-special-indent-hook)
      (if (listp val)
	  (while val
	    (add-hook 'c-special-indent-hook (car val))
	    (setq val (cdr val)))
	(add-hook 'c-special-indent-hook val)))
     ;; all other variables
     (t (set attr val)))
    ))

(defun c-set-style-2 (style basestyles)
  ;; Recursively set the base style.  If no base style is given, the
  ;; default base style is "user" (a.k.a. "cc-mode") and the recursion
  ;; stops.  Be sure to detect loops.
  (let ((vars (cdr (or (assoc (downcase style) c-style-alist)
		       (assoc (upcase style) c-style-alist)
		       (assoc style c-style-alist)
		       (error "Undefined style: %s" style)))))
    (if (not (string-equal style "user"))
	(let ((base (if (stringp (car vars))
			(prog1
			    (downcase (car vars))
			  (setq vars (cdr vars)))
		      "user")))
	  (if (memq base basestyles)
	      (error "Style loop detected: %s in %s" base basestyles))
	  (c-set-style-2 base (cons base basestyles))))
    (mapcar 'c-set-style-1 vars)))
    
(defvar c-set-style-history nil)

;;;###autoload
(defun c-set-style (stylename)
  "Set CC Mode variables to use one of several different indentation styles.
STYLENAME is a string representing the desired style from the list of
styles described in the variable `c-style-alist'.  See that variable
for details of setting up styles.

The variable `c-indentation-style' always contains the buffer's current
style name."
  (interactive (list (let ((completion-ignore-case t)
			   (prompt (format "Which %s indentation style? "
					   mode-name)))
		       (completing-read prompt c-style-alist nil t
					(cons c-indentation-style 0)
					'c-set-style-history))))
  (c-initialize-builtin-style)
  (c-set-style-2 stylename nil)
  (setq c-indentation-style stylename)
  (c-keep-region-active))

;;;###autoload
(defun c-add-style (style descrip &optional set-p)
  "Adds a style to `c-style-alist', or updates an existing one.
STYLE is a string identifying the style to add or update.  DESCRIP is
an association list describing the style and must be of the form:

  ([BASESTYLE] (VARIABLE . VALUE) [(VARIABLE . VALUE) ...])

See the variable `c-style-alist' for the semantics of BASESTYLE,
VARIABLE and VALUE.  This function also sets the current style to
STYLE using `c-set-style' if the optional SET-P flag is non-nil."
  (interactive
   (let ((stylename (completing-read "Style to add: " c-style-alist
				     nil nil nil 'c-set-style-history))
	 (description (eval-minibuffer "Style description: ")))
     (list stylename description
	   (y-or-n-p "Set the style too? "))))
  (setq style (downcase style))
  (let ((s (assoc style c-style-alist)))
    (if s
	(setcdr s (copy-alist descrip))	; replace
      (setq c-style-alist (cons (cons style descrip) c-style-alist))))
  (and set-p (c-set-style style)))



(defconst c-offsets-alist
  '((string                . c-lineup-dont-change)
    (c                     . c-lineup-C-comments)
    (defun-open            . 0)
    (defun-close           . 0)
    (defun-block-intro     . +)
    (class-open            . 0)
    (class-close           . 0)
    (inline-open           . +)
    (inline-close          . 0)
    (func-decl-cont        . +)
    (knr-argdecl-intro     . +)
    (knr-argdecl           . 0)
    (topmost-intro         . 0)
    (topmost-intro-cont    . 0)
    (member-init-intro     . +)
    (member-init-cont      . 0)
    (inher-intro           . +)
    (inher-cont            . c-lineup-multi-inher)
    (block-open            . 0)
    (block-close           . 0)
    (brace-list-open       . 0)
    (brace-list-close      . 0)
    (brace-list-intro      . +)
    (brace-list-entry      . 0)
    (brace-entry-open      . 0)
    (statement             . 0)
    ;; some people might prefer
    ;;(statement             . c-lineup-runin-statements)
    (statement-cont        . +)
    ;; some people might prefer
    ;;(statement-cont        . c-lineup-math)
    (statement-block-intro . +)
    (statement-case-intro  . +)
    (statement-case-open   . 0)
    (substatement          . +)
    (substatement-open     . +)
    (case-label            . 0)
    (access-label          . -)
    (label                 . 2)
    (do-while-closure      . 0)
    (else-clause           . 0)
    (catch-clause          . 0)
    (comment-intro         . c-lineup-comment)
    (arglist-intro         . +)
    (arglist-cont          . 0)
    (arglist-cont-nonempty . c-lineup-arglist)
    (arglist-close         . +)
    (stream-op             . c-lineup-streamop)
    (inclass               . +)
    (cpp-macro             . -1000)
    (cpp-macro-cont        . c-lineup-dont-change)
    (friend                . 0)
    (objc-method-intro     . -1000)
    (objc-method-args-cont . c-lineup-ObjC-method-args)
    (objc-method-call-cont . c-lineup-ObjC-method-call)
    (extern-lang-open      . 0)
    (extern-lang-close     . 0)
    (inextern-lang         . +)
    (namespace-open        . 0)
    (namespace-close       . 0)
    (innamespace           . +)
    (template-args-cont    . +)
    (inlambda              . c-lineup-inexpr-block)
    (lambda-intro-cont     . +)
    (inexpr-statement      . 0)
    (inexpr-class          . +)
    )
  "Association list of syntactic element symbols and indentation offsets.
As described below, each cons cell in this list has the form:

    (SYNTACTIC-SYMBOL . OFFSET)

When a line is indented, CC Mode first determines the syntactic
context of the line by generating a list of symbols called syntactic
elements.  This list can contain more than one syntactic element and
the global variable `c-syntactic-context' contains the context list
for the line being indented.  Each element in this list is actually a
cons cell of the syntactic symbol and a buffer position.  This buffer
position is called the relative indent point for the line.  Some
syntactic symbols may not have a relative indent point associated with
them.

After the syntactic context list for a line is generated, CC Mode
calculates the absolute indentation for the line by looking at each
syntactic element in the list.  First, it compares the syntactic
element against the SYNTACTIC-SYMBOL's in `c-offsets-alist'.  When it
finds a match, it adds the OFFSET to the column of the relative indent
point.  The sum of this calculation for each element in the syntactic
list is the absolute offset for line being indented.

If the syntactic element does not match any in the `c-offsets-alist',
an error is generated if `c-strict-syntax-p' is non-nil, otherwise the
element is ignored.

Actually, OFFSET can be an integer, a function, a variable, or one of
the following symbols: `+', `-', `++', `--', `*', or `/'.  These
latter designate positive or negative multiples of `c-basic-offset',
respectively: 1, -1, 2, -2, 0.5, and -0.5. If OFFSET is a function, it
is called with a single argument containing the cons of the syntactic
element symbol and the relative indent point.  The function should
return an integer offset.

OFFSET can also be a list, in which case it is recursively evaluated
using the semantics described above.  The first element of the list to 
return a non-nil value succeeds.  If none of the elements returns a
non-nil value, then what happends depends on the value of
`c-strict-syntax-p'.  When `c-strict-syntax-p' is nil, then an offset
of zero is used, otherwise an error is generated.

Here is the current list of valid syntactic element symbols:

 string                 -- inside multi-line string
 c                      -- inside a multi-line C style block comment
 defun-open             -- brace that opens a function definition
 defun-close            -- brace that closes a function definition
 defun-block-intro      -- the first line in a top-level defun
 class-open             -- brace that opens a class definition
 class-close            -- brace that closes a class definition
 inline-open            -- brace that opens an in-class inline method
 inline-close           -- brace that closes an in-class inline method
 func-decl-cont         -- the region between a function definition's
                           argument list and the function opening brace
                           (excluding K&R argument declarations). In C, you
                           cannot put anything but whitespace and comments
                           between them; in C++ and Java, throws declarations
                           and other things can appear in this context.
 knr-argdecl-intro      -- first line of a K&R C argument declaration
 knr-argdecl            -- subsequent lines in a K&R C argument declaration
 topmost-intro          -- the first line in a topmost construct definition
 topmost-intro-cont     -- topmost definition continuation lines
 member-init-intro      -- first line in a member initialization list
 member-init-cont       -- subsequent member initialization list lines
 inher-intro            -- first line of a multiple inheritance list
 inher-cont             -- subsequent multiple inheritance lines
 block-open             -- statement block open brace
 block-close            -- statement block close brace
 brace-list-open        -- open brace of an enum or static array list
 brace-list-close       -- close brace of an enum or static array list
 brace-list-intro       -- first line in an enum or static array list
 brace-list-entry       -- subsequent lines in an enum or static array list
 brace-entry-open       -- subsequent lines in an enum or static array
                           list that start with an open brace.
 statement              -- a C (or like) statement
 statement-cont         -- a continuation of a C (or like) statement
 statement-block-intro  -- the first line in a new statement block
 statement-case-intro   -- the first line in a case \"block\"
 statement-case-open    -- the first line in a case block starting with brace
 substatement           -- the first line after an if/while/for/do/else
 substatement-open      -- the brace that opens a substatement block
 case-label             -- a `case' or `default' label
 access-label           -- C++ private/protected/public access label
 label                  -- any ordinary label
 do-while-closure       -- the `while' that ends a do/while construct
 else-clause            -- the `else' of an if/else construct
 catch-clause           -- the `catch' or `finally' of a try/catch construct
 comment-intro          -- a line containing only a comment introduction
 arglist-intro          -- the first line in an argument list
 arglist-cont           -- subsequent argument list lines when no
                           arguments follow on the same line as the
                           arglist opening paren
 arglist-cont-nonempty  -- subsequent argument list lines when at
                           least one argument follows on the same
                           line as the arglist opening paren
 arglist-close          -- the solo close paren of an argument list
 stream-op              -- lines continuing a stream operator construct
 inclass                -- the construct is nested inside a class definition
 cpp-macro              -- the start of a C preprocessor macro definition
 cpp-macro-cont         -- the second and subsequent lines in a
                           multi-line C preprocessor macro definition
 friend                 -- a C++ friend declaration
 objc-method-intro      -- the first line of an Objective-C method definition
 objc-method-args-cont  -- lines continuing an Objective-C method definition
 objc-method-call-cont  -- lines continuing an Objective-C method call
 extern-lang-open       -- brace that opens an external language block
 extern-lang-close      -- brace that closes an external language block
 inextern-lang          -- analogous to `inclass' syntactic symbol,
                           but used inside, e.g. extern \"C\" constructs
 namespace-open         -- brace that opens a C++ namespace block
 namespace-close        -- brace that closes a C++ namespace block
 innamespace            -- analogous to `inextern-lang' syntactic
                           symbol, but used inside C++ namespace constructs
 template-args-cont     -- C++ template argument list continuations
 inlambda               -- in the header or body of a lambda function
 lambda-intro-cont      -- continuation of the header of a lambda function
 inexpr-statement       -- the statement is inside an expression
 inexpr-class           -- the class is inside an expression
")

(defun c-evaluate-offset (offset langelem symbol)
  ;; offset can be a number, a function, a variable, a list, or one of
  ;; the symbols + or -
  (cond
   ((eq offset '+)         (setq offset c-basic-offset))
   ((eq offset '-)         (setq offset (- c-basic-offset)))
   ((eq offset '++)        (setq offset (* 2 c-basic-offset)))
   ((eq offset '--)        (setq offset (* 2 (- c-basic-offset))))
   ((eq offset '*)         (setq offset (/ c-basic-offset 2)))
   ((eq offset '/)         (setq offset (/ (- c-basic-offset) 2)))
   ((functionp offset)     (setq offset (funcall offset langelem)))
   ((listp offset)
    (setq offset
	  (let (done)
	    (while (and (not done) offset)
	      (setq done (c-evaluate-offset (car offset) langelem symbol)
		    offset (cdr offset)))
	    (if (not done)
		(if c-strict-syntax-p
		    (error "No offset found for syntactic symbol %s" symbol)
		  0)
	      done))))
   ((not (numberp offset)) (setq offset (symbol-value offset)))
   )
  offset)

(defun c-get-offset (langelem)
  ;; Get offset from LANGELEM which is a cons cell of the form:
  ;; (SYMBOL . RELPOS).  The symbol is matched against
  ;; c-offsets-alist and the offset found there is either returned,
  ;; or added to the indentation at RELPOS.  If RELPOS is nil, then
  ;; the offset is simply returned.
  (let* ((symbol (car langelem))
	 (relpos (cdr langelem))
	 (match  (assq symbol c-offsets-alist))
	 (offset (cdr-safe match)))
    (if (not match)
	(if c-strict-syntax-p
	    (error "No offset found for syntactic symbol %s" symbol)
	  (setq offset 0
		relpos 0))
      (setq offset (c-evaluate-offset offset langelem symbol)))
    (+ (if (and relpos
		(< relpos (c-point 'bol)))
	   (save-excursion
	     (goto-char relpos)
	     (current-column))
	 0)
       (c-evaluate-offset offset langelem symbol))
    ))


(defvar c-read-offset-history nil)

(defun c-read-offset (langelem)
  ;; read new offset value for LANGELEM from minibuffer. return a
  ;; legal value only
  (let* ((oldoff  (cdr-safe (assq langelem c-offsets-alist)))
	 (symname (symbol-name langelem))
	 (defstr  (format "(default %s): " oldoff))
	 (errmsg  (concat "Offset must be int, func, var, list, "
			  "or [+,-,++,--,*,/] "
			  defstr))
	 (prompt (concat symname " offset " defstr))
	 offset input interned raw)
    (while (not offset)
      (setq input (completing-read prompt obarray 'fboundp nil nil
				   'c-read-offset-history)
	    offset (cond ((string-equal "" input) oldoff)  ; default
			 ((string-equal "+" input) '+)
			 ((string-equal "-" input) '-)
			 ((string-equal "++" input) '++)
			 ((string-equal "--" input) '--)
			 ((string-equal "*" input) '*)
			 ((string-equal "/" input) '/)
			 ((string-match "^-?[0-9]+$" input)
			  (string-to-int input))
			 ;; a symbol with a function binding
			 ((fboundp (setq interned (intern input)))
			  interned)
			 ;; a lambda function
			 ((c-safe (functionp (setq raw (read input))))
			  raw)
			 ;; a symbol with variable binding
			 ((boundp interned) interned)
			 ;; error, but don't signal one, keep trying
			 ;; to read an input value
			 (t (ding)
			    (setq prompt errmsg)
			    nil))))
    offset))

;;;###autoload
(defun c-set-offset (symbol offset &optional add-p)
  "Change the value of a syntactic element symbol in `c-offsets-alist'.
SYMBOL is the syntactic element symbol to change and OFFSET is the new
offset for that syntactic element.  Optional ADD says to add SYMBOL to
`c-offsets-alist' if it doesn't already appear there."
  (interactive
   (let* ((langelem
	   (intern (completing-read
		    (concat "Syntactic symbol to change"
			    (if current-prefix-arg " or add" "")
			    ": ")
		    (mapcar
		     #'(lambda (langelem)
			 (cons (format "%s" (car langelem)) nil))
		     c-offsets-alist)
		    nil (not current-prefix-arg)
		    ;; initial contents tries to be the last element
		    ;; on the syntactic analysis list for the current
		    ;; line
		    (let* ((syntax (c-guess-basic-syntax))
			   (len (length syntax))
			   (ic (format "%s" (car (nth (1- len) syntax)))))
		      (cons ic 0))
		    )))
	  (offset (c-read-offset langelem)))
     (list langelem offset current-prefix-arg)))
  ;; sanity check offset
  (or (eq offset '+)
      (eq offset '-)
      (eq offset '++)
      (eq offset '--)
      (eq offset '*)
      (eq offset '/)
      (integerp offset)
      (functionp offset)
      (listp offset)
      (boundp offset)
      (error "Offset must be int, func, var, list, or in [+,-,++,--,*,/]: %s"
	     offset))
  (let ((entry (assq symbol c-offsets-alist)))
    (if entry
	(setcdr entry offset)
      (if add-p
	  (setq c-offsets-alist (cons (cons symbol offset) c-offsets-alist))
	(error "%s is not a valid syntactic symbol." symbol))))
  (c-keep-region-active))


(defun c-copy-tree (tree)
  (if (consp tree)
      (cons (c-copy-tree (car tree))
            (c-copy-tree (cdr tree)))
    tree))

(defun c-initialize-builtin-style ()
  ;; Dynamically append the default value of most variables. This is
  ;; crucial because future c-set-style calls will always reset the
  ;; variables first to the `cc-mode' style before instituting the new
  ;; style.  Only do this once!
  (unless (get 'c-initialize-builtin-style 'is-run)
    (put 'c-initialize-builtin-style 'is-run t)
    (c-initialize-cc-mode)
    (or (assoc "cc-mode" c-style-alist)
	(assoc "user" c-style-alist)
	(progn
	  (c-add-style "user"
		       (mapcar
			(function
			 (lambda (var)
			   (let ((val (symbol-value var)))
			     (cons var (c-copy-tree val)))))
			'(c-backslash-column
			  c-basic-offset
			  c-cleanup-list
			  c-comment-only-line-offset
			  c-electric-pound-behavior
			  c-hanging-braces-alist
			  c-hanging-colons-alist
			  c-hanging-comment-starter-p
			  c-hanging-comment-ender-p
			  c-offsets-alist
			  )))
	  (c-add-style "cc-mode" '("user"))
	  ))
    (if c-style-variables-are-local-p
	(c-make-styles-buffer-local))))


(defun c-make-styles-buffer-local (&optional this-buf-only-p)
  "Make all CC Mode style variables buffer local.
If you edit primarily one style of C (or C++, Objective-C, Java) code,
you probably want style variables to be global.  This is the default.

If you edit many different styles of C (or C++, Objective-C, Java) at
the same time, you probably want the CC Mode style variables to be
buffer local.  If you do, then you will need to set any CC Mode style
variables in a hook function (e.g. off of c-mode-common-hook), instead
of at the top level of your ~/.emacs file.

This function makes all the CC Mode style variables buffer local.
Call it after CC Mode is loaded into your Emacs environment.
Conversely, set the variable `c-style-variables-are-local-p' to t in
your .emacs file, before CC Mode is loaded, and this function will be
automatically called when CC Mode is loaded.

Optional argument, when non-nil, means use `make-local-variable'
instead of `make-variable-buffer-local'."
  ;; style variables
  (let ((func (if this-buf-only-p
		  'make-local-variable
		'make-variable-buffer-local))
	(varsyms '(c-offsets-alist
		   c-basic-offset
		   c-file-style
		   c-file-offsets
		   c-comment-only-line-offset
		   c-cleanup-list
		   c-hanging-braces-alist
		   c-hanging-colons-alist
		   c-hanging-comment-starter-p
		   c-hanging-comment-ender-p
		   c-backslash-column
		   c-label-minimum-indentation
		   c-indentation-style)))
    (mapcar func varsyms)
    ;; Hooks must be handled specially
    (if this-buf-only-p
	(make-local-hook 'c-special-indent-hook)
      (make-variable-buffer-local 'c-special-indent-hook))
    ))



(provide 'cc-styles)
;;; cc-styles.el ends here
