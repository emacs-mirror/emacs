;;; cc-align.el --- custom indentation functions for CC Mode

;; Copyright (C) 1985,1987,1992-2001 Free Software Foundation, Inc.

;; Authors:    2000- Martin Stjernholm
;;	       1998-1999 Barry A. Warsaw and Martin Stjernholm
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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(eval-when-compile
  (let ((load-path
	 (if (and (boundp 'byte-compile-dest-file)
		  (stringp byte-compile-dest-file))
	     (cons (file-name-directory byte-compile-dest-file) load-path)
	   load-path)))
    (require 'cc-bytecomp)))

(cc-require 'cc-defs)
(cc-require 'cc-vars)
(cc-require 'cc-langs)
(cc-require 'cc-engine)


;; Standard indentation line-ups

(defun c-lineup-topmost-intro-cont (langelem)
  "Line up declaration continuation lines zero or one indentation step.
For lines in the \"header\" of a definition, zero is used.  For other
lines, `c-basic-offset' is added to the indentation.  E.g:

int
neg (int i)           <- c-lineup-topmost-intro-cont
{
    return -i;
}

struct
larch                 <- c-lineup-topmost-intro-cont
{
    double height;
}
    the_larch,        <- c-lineup-topmost-intro-cont
    another_larch;    <- c-lineup-topmost-intro-cont
<--> c-basic-offset

struct larch
the_larch,            <- c-lineup-topmost-intro-cont
    another_larch;    <- c-lineup-topmost-intro-cont

\(This function is mainly provided to mimic the behavior of CC Mode
5.28 and earlier where this case wasn't handled consistently so that
these lines could be analyzed as either topmost-intro-cont or
statement-cont.)

Works with: topmost-intro-cont."
  (save-excursion
    (beginning-of-line)
    (c-backward-syntactic-ws (cdr langelem))
    (if (memq (char-before) '(?} ?,))
	c-basic-offset)))

(defun c-lineup-arglist (langelem)
  "Line up the current argument line under the first argument.

Works with: arglist-cont-nonempty, arglist-close."
  (save-excursion
    (beginning-of-line)
    (let ((containing-sexp (c-most-enclosing-brace (c-parse-state))))
      (goto-char (1+ containing-sexp))
      (let ((eol (c-point 'eol)))
	(c-forward-syntactic-ws)
	(when (< (point) eol)
	  (goto-char (1+ containing-sexp))
	  (skip-chars-forward " \t")))
      (vector (current-column)))))

;; Contributed by Kevin Ryde <user42@zip.com.au>.
(defun c-lineup-argcont (elem)
  "Line up a continued argument.

foo (xyz, aaa + bbb + ccc
          + ddd + eee + fff);    <- c-lineup-argcont

Only continuation lines like this are touched, `nil' is returned on lines
which are the start of an argument.

Within a gcc asm block, \":\" is recognised as an argument separator,
but of course only between operand specifications, not in the expressions
for the operands.

Works with: arglist-cont, arglist-cont-nonempty."

  (save-excursion
    (beginning-of-line)
    (let ((bol (point)))

      ;; Previous line ending in a comma means we're the start of an
      ;; argument.  This should quickly catch most cases not for us.
      (c-backward-syntactic-ws)
      (let ((c (char-before)))
	(unless (eq c ?,)

	  ;; In a gcc asm, ":" on the previous line means the start of an
	  ;; argument.  And lines starting with ":" are not for us, don't
	  ;; want them to indent to the preceding operand.
	  (let ((gcc-asm (save-excursion
			   (goto-char bol)
			   (c-in-gcc-asm-p))))
	    (unless (and gcc-asm
			 (or (eq c ?:)
			     (save-excursion
			       (goto-char bol)
			       (looking-at "[ \t]*:"))))

	      (c-lineup-argcont-scan (if gcc-asm ?:))
	      (vector (current-column)))))))))

(defun c-lineup-argcont-scan (&optional other-match)
  ;; Find the start of an argument, for `c-lineup-argcont'.
  (when (eq 0 (c-backward-token-1 1 t))
    (let ((c (char-after)))
      (if (or (eq c ?,) (eq c other-match))
	  (progn
	    (forward-char)
	    (c-forward-syntactic-ws))
	(c-lineup-argcont-scan other-match)))))

(defun c-lineup-arglist-intro-after-paren (langelem)
  "Line up a line just after the open paren of the surrounding paren or
brace block.

Works with: defun-block-intro, brace-list-intro,
statement-block-intro, statement-case-intro, arglist-intro."
  (save-excursion
    (beginning-of-line)
    (backward-up-list 1)
    (skip-chars-forward " \t" (c-point 'eol))
    (vector (1+ (current-column)))))

(defun c-lineup-arglist-close-under-paren (langelem)
  "Line up a closing paren line under the corresponding open paren.

Works with: defun-close, class-close, inline-close, block-close,
brace-list-close, arglist-close, extern-lang-close, namespace-close
\(for most of these, a zero offset will normally produce the same
result, though)."
  (save-excursion
    (beginning-of-line)
    (backward-up-list 1)
    (vector (current-column))))

(defun c-lineup-close-paren (langelem)
  "Line up the closing paren under its corresponding open paren if the
open paren is followed by code.  If the open paren ends its line, no
indentation is added.  E.g:

main (int,              main (
      char **               int, char **
     )           <->    )                 <- c-lineup-close-paren

Works with: defun-close, class-close, inline-close, block-close,
brace-list-close, arglist-close, extern-lang-close, namespace-close."
  (save-excursion
    (condition-case nil
	(let (opencol spec)
	  (beginning-of-line)
	  (backward-up-list 1)
	  (setq spec (c-looking-at-special-brace-list))
	  (if spec (goto-char (car (car spec))))
	  (setq opencol (current-column))
	  (forward-char 1)
	  (if spec (progn
		     (c-forward-syntactic-ws)
		     (forward-char 1)))
	  (c-forward-syntactic-ws (c-point 'eol))
	  (if (eolp)
	      0
	    (vector opencol)))
      (error nil))))

(defun c-lineup-streamop (langelem)
  "Line up C++ stream operators under each other.

Works with: stream-op."
  (save-excursion
    (goto-char (cdr langelem))
    (re-search-forward "<<\\|>>" (c-point 'eol) 'move)
    (goto-char (match-beginning 0))
    (vector (current-column))))

(defun c-lineup-multi-inher (langelem)
  "Line up the classes in C++ multiple inheritance clauses and member
initializers under each other.  E.g:

class Foo:                Foo::Foo (int a, int b):
    public Cyphr,             Cyphr (a),
    public Bar       <->      Bar (b)               <- c-lineup-multi-inher

class Foo                 Foo::Foo (int a, int b)
    : public Cyphr,           : Cyphr (a),
      public Bar     <->        Bar (b)             <- c-lineup-multi-inher

class Foo                 Foo::Foo (int a, int b)
    : public Cyphr            : Cyphr (a)
    , public Bar     <->      , Bar (b)             <- c-lineup-multi-inher

Works with: inher-cont, member-init-cont."
  (save-excursion
    (let* ((eol (c-point 'eol))
	   (here (point))
	   (char-after-ip (progn
			    (skip-chars-forward " \t")
			    (char-after))))
      (if (cdr langelem) (goto-char (cdr langelem)))

      ;; This kludge is necessary to support both inher-cont and
      ;; member-init-cont, since they have different anchor positions.
      (c-backward-syntactic-ws)
      (when (eq (char-before) ?:)
	(backward-char)
	(c-backward-syntactic-ws))

      (skip-chars-forward "^:" eol)
      (if (eq char-after-ip ?,)
	  (skip-chars-forward " \t" eol)
	(skip-chars-forward " \t:" eol))
      (if (or (eolp)
	      (looking-at c-comment-start-regexp))
	  (c-forward-syntactic-ws here))
      (vector (current-column))
      )))

(defun c-lineup-java-inher (langelem)
  "Line up Java implements and extends declarations.
If class names follows on the same line as the implements/extends
keyword, they are lined up under each other.  Otherwise, they are
indented by adding `c-basic-offset' to the column of the keyword.
E.g:

class Foo             class Foo
    extends               extends Cyphr,
        Bar    <->                Bar     <- c-lineup-java-inher
    <--> c-basic-offset

Works with: inher-cont."
  (save-excursion
    (goto-char (cdr langelem))
    (forward-word 1)
    (if (looking-at "[ \t]*$")
	c-basic-offset
      (c-forward-syntactic-ws)
      (vector (current-column)))))

(defun c-lineup-java-throws (langelem)
  "Line up Java throws declarations.
If exception names follows on the same line as the throws keyword,
they are lined up under each other.  Otherwise, they are indented by
adding `c-basic-offset' to the column of the throws keyword.  The
throws keyword itself is also indented by `c-basic-offset' from the
function declaration start if it doesn't hang.  E.g:

int foo()           int foo() throws Cyphr,
    throws     <->                   Bar,    <- c-lineup-java-throws
        Bar    <->                   Vlod    <- c-lineup-java-throws
<--><--> c-basic-offset

Works with: func-decl-cont."
  (save-excursion
    (let* ((lim (1- (c-point 'bol)))
	   (throws (catch 'done
		     (goto-char (cdr langelem))
		     (while (zerop (c-forward-token-1 1 t lim))
		       (if (looking-at "throws\\>[^_]")
			   (throw 'done t))))))
      (if throws
	  (if (zerop (c-forward-token-1 1 nil (c-point 'eol)))
	      (vector (current-column))
	    (back-to-indentation)
	    (vector (+ (current-column) c-basic-offset)))
	c-basic-offset))))

(defun c-indent-one-line-block (langelem)
  "Indent a one line block `c-basic-offset' extra.
E.g:

if (n > 0)                 if (n > 0)
    {m+=n; n=0;}    <->    {               <- c-indent-one-line-block
<--> c-basic-offset            m+=n; n=0;
                           }

The block may use any kind of parenthesis character.  nil is returned
if the line doesn't start with a one line block, which makes the
function usable in list expressions.

Work with: Almost all syntactic symbols, but most useful on *-open."
  (save-excursion
    (let ((eol (c-point 'eol)))
      (back-to-indentation)
      (if (and (eq (char-syntax (char-after)) ?\()
	       (c-safe (progn (c-forward-sexp) t))
	       (<= (point) eol))
	  c-basic-offset
	nil))))

(defun c-indent-multi-line-block (langelem)
  "Indent a multi line block `c-basic-offset' extra.
E.g:

int *foo[] = {           int *foo[] = {
    NULL,                    NULL,
    {17},         <->            {       <- c-indent-multi-line-block
                                 17
                                 }
                             <--> c-basic-offset

The block may use any kind of parenthesis character.  nil is returned
if the line doesn't start with a multi line block, which makes the
function usable in list expressions.

Work with: Almost all syntactic symbols, but most useful on *-open."
  (save-excursion
    (let ((eol (c-point 'eol)))
      (back-to-indentation)
      (if (and (eq (char-syntax (char-after)) ?\()
	       (or (not (c-safe (progn (c-forward-sexp) t)))
		   (> (point) eol)))
	  c-basic-offset
	nil))))

(defun c-lineup-C-comments (langelem)
  "Line up C block comment continuation lines.
Various heuristics are used to handle many of the common comment
styles.  Some examples:

/*          /**         /*         /* text      /*          /**
 * text      * text       text        text      ** text      ** text
 */          */         */         */           */           */

/*********************************************************************
 * text
 ********************************************************************/

/*********************************************************************
    Free form text comments:
 In comments with a long delimiter line at the start, the indentation
 is kept unchanged for lines that start with an empty comment line
 prefix.  The delimiter line is whatever matches the
 `comment-start-skip' regexp.
*********************************************************************/

The variable `c-comment-prefix-regexp' is used to recognize the
comment line prefix, e.g. the `*' that usually starts every line
inside a comment.

Works with: The `c' syntactic symbol."
  (save-excursion
    (let* ((here (point))
	   (prefixlen (progn (back-to-indentation)
			     (if (looking-at c-current-comment-prefix)
				 (- (match-end 0) (point))
			       0)))
	   (starterlen
	    ;; Get the length of the comment starter, not including
	    ;; the first '/'. We check if the comment prefix matched
	    ;; on the current line matches the starter or if it
	    ;; matches comment-start-skip, and choose whichever is
	    ;; longest.
	    (max (save-excursion
		   (goto-char (1+ (cdr langelem)))
		   (if (and (match-string 0)
			    (looking-at (regexp-quote (match-string 0))))
		       (- (match-end 0) (match-beginning 0))
		     0))
		 (save-excursion
		   (goto-char (cdr langelem))
		   (looking-at comment-start-skip)
		   (- (or (match-end 1)
			  (save-excursion
			    (goto-char (match-end 0))
			    (skip-chars-backward " \t")
			    (point)))
		      (point)
		      1)))))
      (if (and (> starterlen 10) (zerop prefixlen))
	  ;; The comment has a long starter and the line doesn't have
	  ;; a nonempty comment prefix.  Treat it as free form text
	  ;; and don't change the indentation.
	  (vector (current-column))
	(forward-line -1)
	(back-to-indentation)
	(if (>= (cdr langelem) (point))
	    ;; On the second line in the comment.
	    (if (zerop prefixlen)
		;; No nonempty comment prefix. Align after comment
		;; starter.
		(progn
		  (goto-char (match-end 0))
		  ;; The following should not be necessary, since
		  ;; comment-start-skip should match everything (i.e.
		  ;; typically whitespace) that leads up to the text.
		  ;;(if (looking-at "\\([ \t]+\\).+$")
		  ;;    ;; Align with the text that hangs after the
		  ;;    ;; comment starter.
		  ;;    (goto-char (match-end 1)))
		  (vector (current-column)))
	      ;; How long is the comment starter?  if greater than the
	      ;; length of the comment prefix, align left.  if less
	      ;; than or equal, align right.  this should also pick up
	      ;; Javadoc style comments.
	      (if (> starterlen prefixlen)
		  (progn
		    (goto-char (cdr langelem))
		    (vector (1+ (current-column))))
		(goto-char (+ (cdr langelem) starterlen 1))
		(vector (- (current-column) prefixlen))))
	  ;; Not on the second line in the comment.  If the previous
	  ;; line has a nonempty comment prefix, align with it.
	  ;; Otherwise, align with the previous nonempty line, but
	  ;; align the comment ender with the starter.
	  (when (or (not (looking-at c-current-comment-prefix))
		    (eq (match-beginning 0) (match-end 0)))
	    (goto-char here)
	    (back-to-indentation)
	    (if (looking-at (concat "\\(" c-current-comment-prefix "\\)\\*/"))
		(goto-char (cdr langelem))
	      (while (and (zerop (forward-line -1))
			  (looking-at "^[ \t]*$")))
	      (back-to-indentation)
	      (if (< (point) (cdr langelem))
		  ;; Align with the comment starter rather than
		  ;; with the code before it.
		  (goto-char (cdr langelem)))))
	  (vector (current-column)))))))

(defun c-lineup-comment (langelem)
  "Line up a comment start according to `c-comment-only-line-offset'.
If the comment is lined up with a comment starter on the previous
line, that alignment is preserved.

Works with: comment-intro."
  (save-excursion
    (back-to-indentation)
    (let ((col (current-column)))
      (cond
       ;; CASE 1: preserve aligned comments
       ((save-excursion
	  (and (c-forward-comment -1)
	       (= col (current-column))))
	(vector col))			; Return an absolute column.
       ;; indent as specified by c-comment-only-line-offset
       ((not (bolp))
	(or (car-safe c-comment-only-line-offset)
	    c-comment-only-line-offset))
       (t
	(or (cdr-safe c-comment-only-line-offset)
	    (car-safe c-comment-only-line-offset)
	    -1000))			;jam it against the left side
       ))))

(defun c-lineup-knr-region-comment (langelem)
  "Line up a comment in the \"K&R region\" with the declaration.
That is the region between the function or class header and the
beginning of the block.  E.g:

int main()
/* This is the main function. */  <- c-lineup-knr-region-comment
{
  return 0;
}

Return nil if called in any other situation, to be useful in list
expressions.

Works with: comment-intro."
  (when (or (assq 'topmost-intro-cont c-syntactic-context)
	    (assq 'func-decl-cont c-syntactic-context)
	    (assq 'knr-argdecl-intro c-syntactic-context)
	    (assq 'lambda-intro-cont c-syntactic-context))
    (save-excursion
      (beginning-of-line)
      (c-beginning-of-statement-1)
      (vector (current-column)))))

(defun c-lineup-runin-statements (langelem)
  "Line up statements when the first statement is on the same line as
the block opening brace.  E.g:

int main()
{ puts (\"Hello world!\");
  return 0;                 <- c-lineup-runin-statements
}

If there is no statement after the opening brace to align with, nil is
returned.  This makes the function usable in list expressions.

Works with: The `statement' syntactic symbol."
  (if (eq (char-after (cdr langelem)) ?{)
      (save-excursion
	(if (cdr langelem) (goto-char (cdr langelem)))
	(forward-char 1)
	(skip-chars-forward " \t")
	(unless (eolp)
	  (vector (current-column))))))

(defun c-lineup-math (langelem)
  "Line up the current line after the equal sign on the first line in
the statement.  If there isn't any, indent with `c-basic-offset'.  If
the current line contains an equal sign too, try to align it with the
first one.

Works with: statement-cont, arglist-cont, arglist-cont-nonempty."
  (save-excursion
    (let ((equalp (save-excursion
		    (goto-char (c-point 'boi))
		    (let ((eol (c-point 'eol)))
		      (c-forward-token-1 0 t eol)
		      (while (and (not (eq (char-after) ?=))
				  (= (c-forward-token-1 1 t eol) 0))))
		    (and (eq (char-after) ?=)
			 (- (point) (c-point 'boi)))))
	  donep)
      (if (cdr langelem) (goto-char (cdr langelem)))
      (while (and (not donep)
		  (< (point) (c-point 'eol)))
	(skip-chars-forward "^=" (c-point 'eol))
	(if (c-in-literal (cdr langelem))
	    (forward-char 1)
	  (setq donep t)))
      (if (or (not (eq (char-after) ?=))
	      (save-excursion
		(forward-char 1)
		(c-forward-syntactic-ws (c-point 'eol))
		(eolp)))
	  ;; there's no equal sign on the line
	  c-basic-offset
	;; calculate indentation column after equals and ws, unless
	;; our line contains an equals sign
	(if (not equalp)
	    (progn
	      (forward-char 1)
	      (skip-chars-forward " \t")
	      (setq equalp 0)))
	(vector (- (current-column) equalp)))
      )))

(defun c-lineup-cascaded-calls (langelem)
  "Line up \"cascaded calls\" under each other.
If the line begins with \"->\" and the preceding line ends with one or
more function calls preceded by \"->\", then the arrow is lined up with
the first of those \"->\". E.g:

result = proc->add(17)->add(18)
             ->add(19) +           <- c-lineup-cascaded-calls
  offset;                          <- c-lineup-cascaded-calls (inactive)

In any other situation nil is returned to allow use in list
expressions.

Works with: statement-cont, arglist-cont, arglist-cont-nonempty."
  (save-excursion
    (let ((bopl (c-point 'bopl)) col)
      (back-to-indentation)
      (when (and (looking-at "->")
		 (= (c-backward-token-1 1 t bopl) 0)
		 (eq (char-after) ?\()
		 (= (c-backward-token-1 3 t bopl) 0)
		 (looking-at "->"))
	(setq col (current-column))
	(while (and (= (c-backward-token-1 1 t bopl) 0)
		    (eq (char-after) ?\()
		    (= (c-backward-token-1 3 t bopl) 0)
		    (looking-at "->"))
	  (setq col (current-column)))
	(vector col)))))

(defun c-lineup-template-args (langelem)
  "Line up template argument lines under the first argument.
To allow this function to be used in a list expression, nil is
returned if there's no template argument on the first line.

Works with: template-args-cont."
  (save-excursion
    (c-with-syntax-table c++-template-syntax-table
      (beginning-of-line)
      (backward-up-list 1)
      (if (and (eq (char-after) ?<)
	       (zerop (c-forward-token-1 1 nil (c-point 'eol))))
	  (vector (current-column))))))

(defun c-lineup-ObjC-method-call (langelem)
  "Line up selector args as elisp-mode does with function args:
Go to the position right after the message receiver, and if you are at
the end of the line, indent the current line c-basic-offset columns
from the opening bracket; otherwise you are looking at the first
character of the first method call argument, so lineup the current
line with it.

Works with: objc-method-call-cont."
  (save-excursion
    (let* ((extra (save-excursion
		    (back-to-indentation)
		    (c-backward-syntactic-ws (cdr langelem))
		    (if (eq (char-before) ?:)
			(- c-basic-offset)
		      0)))
	   (open-bracket-pos (cdr langelem))
           (open-bracket-col (progn
			       (goto-char open-bracket-pos)
			       (current-column)))
           (target-col (progn
			 (forward-char)
			 (c-forward-sexp)
			 (skip-chars-forward " \t")
			 (if (eolp)
			     (+ open-bracket-col c-basic-offset)
			   (current-column))))
	   )
      (- target-col open-bracket-col extra))))

(defun c-lineup-ObjC-method-args (langelem)
  "Line up the colons that separate args.
The colon on the current line is aligned with the one on the first
line.

Works with: objc-method-args-cont."
  (save-excursion
    (let* ((here (c-point 'boi))
	   (curcol (progn (goto-char here) (current-column)))
	   (eol (c-point 'eol))
	   (relpos (cdr langelem))
	   (first-col-column (progn
			       (goto-char relpos)
			       (skip-chars-forward "^:" eol)
			       (and (eq (char-after) ?:)
				    (current-column)))))
      (if (not first-col-column)
	  c-basic-offset
	(goto-char here)
	(skip-chars-forward "^:" eol)
	(if (eq (char-after) ?:)
	    (+ curcol (- first-col-column (current-column)))
	  c-basic-offset)))))

(defun c-lineup-ObjC-method-args-2 (langelem)
  "Line up the colons that separate args.
The colon on the current line is aligned with the one on the previous
line.

Works with: objc-method-args-cont."
  (save-excursion
    (let* ((here (c-point 'boi))
	   (curcol (progn (goto-char here) (current-column)))
	   (eol (c-point 'eol))
	   (relpos (cdr langelem))
	   (prev-col-column (progn
			      (skip-chars-backward "^:" relpos)
			      (and (eq (char-before) ?:)
				   (- (current-column) 1)))))
      (if (not prev-col-column)
	  c-basic-offset
	(goto-char here)
	(skip-chars-forward "^:" eol)
	(if (eq (char-after) ?:)
	    (+ curcol (- prev-col-column (current-column)))
	  c-basic-offset)))))

(defun c-lineup-inexpr-block (langelem)
  "Line up the block for constructs that use a block inside an expression,
e.g. anonymous classes in Java and lambda functions in Pike.  The body
is aligned with the start of the header, e.g. with the \"new\" or
\"lambda\" keyword.  Returns nil if the block isn't part of such a
construct.

Works with: inlambda, inexpr-statement, inexpr-class."
  (save-excursion
    (back-to-indentation)
    (let* ((paren-state (c-parse-state))
	   (containing-sexp (c-most-enclosing-brace paren-state))
	   (res (or (c-looking-at-inexpr-block
		     (c-safe-position containing-sexp paren-state)
		     containing-sexp)
		    (and containing-sexp
			 (progn (goto-char containing-sexp)
				(eq (char-after) ?{))
			 (progn (setq containing-sexp
				      (c-most-enclosing-brace paren-state
							      (point)))
				(c-looking-at-inexpr-block
				 (c-safe-position containing-sexp paren-state)
				 containing-sexp))))))
      (when res
	(goto-char (cdr res))
	(- (current-column)
	   (progn
	     (back-to-indentation)
	     (current-column)))))))

(defun c-lineup-whitesmith-in-block (langelem)
  "Line up lines inside a block in whitesmith style.
It's done in a way that works both when the opening brace hangs and
when it doesn't.  E.g:

something
    {                something {
    foo;     <->         foo;     <- c-lineup-whitesmith-in-block
    }                    }
                     <--> c-basic-offset

In the first case the indentation is kept unchanged, in the
second `c-basic-offset' is added.

Works with: defun-close, defun-block-intro, block-close,
brace-list-close, brace-list-intro, statement-block-intro, inclass,
inextern-lang, innamespace."
  (save-excursion
    (goto-char (cdr langelem))
    (back-to-indentation)
    (if (eq (char-syntax (char-after)) ?\()
	0
      c-basic-offset)))

(defun c-lineup-cpp-define (langelem)
  "Line up macro continuation lines according to the indentation of
the construct preceding the macro.  E.g:

v beg of preceding constr      v beg of preceding constr
                             int dribble() {
const char msg[] =             if (!running)
  \"Some text.\";	         error(\"Not running!\");

#define X(A, B)  \           #define X(A, B)    \
do {             \    <->      do {             \    <- c-lineup-cpp-define
  printf (A, B); \               printf (A, B); \
} while (0)                    } while (0)

If `c-syntactic-indentation-in-macros' is non-nil, the function
returns the relative indentation to the macro start line to allow
accumulation with other offsets.  E.g. in the following cases,
cpp-define-intro is combined with the statement-block-intro that comes
from the \"do {\" that hangs on the \"#define\" line:

                             int dribble() {
const char msg[] =             if (!running)
  \"Some text.\";	         error(\"Not running!\");

#define X(A, B) do { \       #define X(A, B) do { \
  printf (A, B);     \  <->      printf (A, B);   \  <- c-lineup-cpp-define
  this->refs++;      \           this->refs++;    \
} while (0)             <->    } while (0)           <- c-lineup-cpp-define

The relative indentation returned by `c-lineup-cpp-define' is zero and
two, respectively, in these two examples. They are then added to the
two column indentation that statement-block-intro gives in both cases
here.

If the relative indentation is zero, then nil is returned instead.
This useful in a list expression to specify the default indentation on
the top level.

If `c-syntactic-indentation-in-macros' is nil then this function keeps
the current indentation, except for empty lines \(ignoring the ending
backslash) where it takes the indentation from the closest preceding
nonempty line in the macro.  If there's no such line in the macro then
the indentation is taken from the construct preceding it, as described
above.

Works with: cpp-define-intro."
  (let (offset)
    (if c-syntactic-indentation-in-macros
	;; Go to the macro start and do a syntactic analysis of it.
	;; Then remove the cpp-macro element it should contain and
	;; calculate the indentation it then would get.
	(save-excursion
	  (c-beginning-of-macro)
	  (setq offset (- (c-get-syntactic-indentation
			   (delete '(cpp-macro) (c-guess-basic-syntax)))
			  (save-excursion
			    (back-to-indentation)
			    (current-column))))
	  (if (zerop offset)
	      nil
	    offset))
      ;; Do not indent syntactically inside the macro.
      (save-excursion
	(let ((macro-start-line (save-excursion
				  (goto-char (c-query-macro-start))
				  (beginning-of-line)
				  (point))))
	  (beginning-of-line)
	  ;; Check every line while inside the macro.
	  (while (and (> (point) macro-start-line)
		      (looking-at "[ \t]*\\\\?$")
		      (= (forward-line -1) 0)))
	  (if (<= (point) macro-start-line)
	      ;; If we've stepped out of the macro we take the
	      ;; syntactic offset.
	      (setq offset (c-get-syntactic-indentation
			    (delete '(cpp-macro) (c-guess-basic-syntax))))
	    (setq offset (current-indentation)))
	  (if (zerop offset)
	      nil
	    (vector offset)))))))

;; Contributed by Kevin Ryde <user42@zip.com.au>.
(defun c-lineup-gcc-asm-reg (elem)
  "Line up a gcc asm register under one on a previous line.

    asm (\"foo %1, %0\\n\"
         \"bar %0, %1\"
         : \"=r\" (w),
           \"=r\" (x)
         :  \"0\" (y),
            \"1\" (z));

The \"x\" line is aligned to the text after the \":\" on the \"w\" line, and
similarly \"z\" under \"y\".

This is done only in an \"asm\" or \"__asm__\" block, and only to those
lines mentioned.  Anywhere else `nil' is returned.  The usual arrangement is
to have this routine as an extra feature at the start of arglist lineups, e.g.

    (c-lineup-gcc-asm-reg c-lineup-arglist)

Works with: arglist-cont, arglist-cont-nonempty."

  (let ((orig-pos (point))
	alignto)
    (save-excursion
      (and
       c-opt-asm-stmt-key

       ;; Find the ":" to align to.  Look for this first so as to quickly
       ;; eliminate pretty much all cases which are not for us.
       (re-search-backward "^[ \t]*:[ \t]*\\(.\\)?" (cdr elem) t)

       ;; Must have something after the ":".
       (setq alignto (match-beginning 1))

       ;; Don't touch ":" lines themselves.
       (progn (goto-char orig-pos)
	      (beginning-of-line)
	      (not (looking-at "^[ \t]*:")))

       ;; Only operate in an asm statement.
       (progn (goto-char orig-pos)
	      (c-in-gcc-asm-p))

       (vector (progn (goto-char alignto) (current-column)))))))

(defun c-lineup-dont-change (langelem)
  "Do not change the indentation of the current line.

Works with: Any syntactic symbol."
  (save-excursion
    (back-to-indentation)
    (vector (current-column))))


(defun c-snug-do-while (syntax pos)
  "Dynamically calculate brace hanginess for do-while statements.
Using this function, `while' clauses that end a `do-while' block will
remain on the same line as the brace that closes that block.

See `c-hanging-braces-alist' for how to utilize this function as an
ACTION associated with `block-close' syntax."
  (save-excursion
    (let (langelem)
      (if (and (eq syntax 'block-close)
	       (setq langelem (assq 'block-close c-syntactic-context))
	       (progn (goto-char (cdr langelem))
		      (if (eq (char-after) ?{)
			  (c-safe (c-forward-sexp -1)))
		      (looking-at "\\<do\\>[^_]")))
	  '(before)
	'(before after)))))

(defun c-gnu-impose-minimum ()
  "Imposes a minimum indentation for lines inside a top-level construct.
The variable `c-label-minimum-indentation' specifies the minimum
indentation amount."
  (let ((non-top-levels '(defun-block-intro statement statement-cont
			   statement-block-intro statement-case-intro
			   statement-case-open substatement substatement-open
			   case-label label do-while-closure else-clause
			   ))
	(syntax c-syntactic-context)
	langelem)
    (while syntax
      (setq langelem (car (car syntax))
	    syntax (cdr syntax))
      ;; don't adjust macro or comment-only lines
      (cond ((memq langelem '(cpp-macro comment-intro))
	     (setq syntax nil))
	    ((memq langelem non-top-levels)
	     (save-excursion
	       (setq syntax nil)
	       (back-to-indentation)
	       (if (zerop (current-column))
		   (insert-char ?\  c-label-minimum-indentation t))
	       ))
	    ))))


;; Useful for c-hanging-semi&comma-criteria

(defun c-semi&comma-inside-parenlist ()
  "Controls newline insertion after semicolons in parenthesis lists.
If a comma was inserted, no determination is made.  If a semicolon was
inserted inside a parenthesis list, no newline is added otherwise a
newline is added.  In either case, checking is stopped.  This supports
exactly the old newline insertion behavior."
  ;; newline only after semicolon, but only if that semicolon is not
  ;; inside a parenthesis list (e.g. a for loop statement)
  (if (not (eq last-command-char ?\;))
      nil				; continue checking
    (if (condition-case nil
	    (save-excursion
	      (up-list -1)
	      (not (eq (char-after) ?\()))
	  (error t))
	t
      'stop)))

;; Suppresses newlines before non-blank lines
(defun c-semi&comma-no-newlines-before-nonblanks ()
  "Controls newline insertion after semicolons.
If a comma was inserted, no determination is made.  If a semicolon was
inserted, and the following line is not blank, no newline is inserted.
Otherwise, no determination is made."
  (save-excursion
    (if (and (= last-command-char ?\;)
	     ;;(/= (point-max)
	     ;;    (save-excursion (skip-syntax-forward " ") (point))
	     (zerop (forward-line 1))
	     (not (looking-at "^[ \t]*$")))
	'stop
      nil)))

;; Suppresses new lines after semicolons in one-liners methods
(defun c-semi&comma-no-newlines-for-oneline-inliners ()
  "Controls newline insertion after semicolons for some one-line methods.
If a comma was inserted, no determination is made.  Newlines are
suppressed in one-liners, if the line is an in-class inline function.
For other semicolon contexts, no determination is made."
  (let ((syntax (c-guess-basic-syntax))
        (bol (save-excursion
               (if (c-safe (up-list -1) t)
                   (c-point 'bol)
                 -1))))
    (if (and (eq last-command-char ?\;)
             (eq (car (car syntax)) 'inclass)
             (eq (car (car (cdr syntax))) 'topmost-intro)
             (= (c-point 'bol) bol))
        'stop
      nil)))


(cc-provide 'cc-align)

;;; cc-align.el ends here
