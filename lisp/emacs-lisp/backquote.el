;;; New backquote for GNU Emacs.
;;; Copyright (C) 1990, 1992, 1994 Free Software Foundation, Inc.

;; Author: Rick Sladkey <jrs@world.std.com>
;; Maintainer: FSF
;; Keywords: extensions, internal

;; This file is not part of GNU Emacs but is distributed under
;; the same conditions as GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; This is a new backquote for GNU Emacs written by
;; Rick Sladkey <jrs@world.std.com>.  It has the following
;; features compared to the version 18 backquote:

;; Correctly handles nested backquotes.
;; Correctly handles constants after a splice.
;; Correctly handles top-level atoms and unquotes.
;; Correctly handles unquote after dot.
;; Understands vectors.
;; Minimizes gratuitous consing.
;; Faster operation with simpler semantics.
;; Generates faster run-time expressions.
;; One third fewer calories than our regular beer.

;; This backquote will generate calls to the backquote-list* form.
;; Both a function version and a macro version are included.
;; The macro version is used by default because it is faster
;; and needs no run-time support.  It should really be a subr.

;;; Code:

(provide 'backquote)

;; function and macro versions of backquote-list*

(defun backquote-list*-function (first &rest list)
  "Like `list' but the last argument is the tail of the new list.

For example (backquote-list* 'a 'b 'c) => (a b . c)"
  (if list
      (let* ((rest list) (newlist (cons first nil)) (last newlist))
	(while (cdr rest)
	  (setcdr last (cons (car rest) nil))
	  (setq last (cdr last)
		rest (cdr rest)))
	(setcdr last (car rest))
	newlist)
    first))

(defmacro backquote-list*-macro (first &rest list)
  "Like `list' but the last argument is the tail of the new list.

For example (backquote-list* 'a 'b 'c) => (a b . c)"
  (setq list (reverse (cons first list))
	first (car list)
	list (cdr list))
  (if list
      (let* ((second (car list))
	     (rest (cdr list))
	     (newlist (list 'cons second first)))
	(while rest
	  (setq newlist (list 'cons (car rest) newlist)
		rest (cdr rest)))
	newlist)
    first))

(defalias 'backquote-list* (symbol-function 'backquote-list*-macro))

;; A few advertised variables that control which symbols are used
;; to represent the backquote, unquote, and splice operations.

(defvar backquote-backquote-symbol '\`
  "*Symbol used to represent a backquote or nested backquote (e.g. `).")

(defvar backquote-unquote-symbol ',
  "*Symbol used to represent an unquote (e.g. `,') inside a backquote.")

(defvar backquote-splice-symbol ',@
  "*Symbol used to represent a splice (e.g. `,@') inside a backquote.")

;;;###autoload
(defmacro backquote (arg)
  "Argument STRUCTURE describes a template to build.

The whole structure acts as if it were quoted except for certain
places where expressions are evaluated and inserted or spliced in.

For example:

b              => (ba bb bc)		; assume b has this value
`(a b c)       => (a b c)		; backquote acts like quote
`(a (, b) c)   => (a (ba bb bc) c)	; insert the value of b
`(a (,@ b) c)  => (a ba bb bc c)	; splice in the value of b

Vectors work just like lists.  Nested backquotes are permitted."
  (cdr (backquote-process arg)))

;; GNU Emacs has no reader macros

;;;###autoload
(defalias '\` (symbol-function 'backquote))

;; backquote-process returns a dotted-pair of a tag (0, 1, or 2) and
;; the backquote-processed structure.  0 => the structure is
;; constant, 1 => to be unquoted, 2 => to be spliced in.
;; The top-level backquote macro just discards the tag.

(defun backquote-process (s)
  (cond
   ((vectorp s)
    (let ((n (backquote-process (append s ()))))
      (if (= (car n) 0)
	  (cons 0 s)
	(cons 1 (cond
		 ((eq (nth 1 n) 'list)
		  (cons 'vector (nthcdr 2 n)))
		 ((eq (nth 1 n) 'append)
		  (cons 'vconcat (nthcdr 2 n)))
		 (t
		  (list 'apply '(function vector) (cdr n))))))))
   ((atom s)
    (cons 0 (if (or (null s) (eq s t) (not (symbolp s)))
		s
	      (list 'quote s))))
   ((eq (car s) backquote-unquote-symbol)
    (cons 1 (nth 1 s)))
   ((eq (car s) backquote-splice-symbol)
    (cons 2 (nth 1 s)))
   ((eq (car s) backquote-backquote-symbol)
    (backquote-process (cdr (backquote-process (nth 1 s)))))
   (t
    (let ((rest s)
	  item firstlist list lists expression)
      ;; Scan this list-level, setting LISTS to a list of forms,
      ;; each of which produces a list of elements
      ;; that should go in this level.
      ;; The order of LISTS is backwards. 
      ;; If there are non-splicing elements (constant or variable)
      ;; at the beginning, put them in FIRSTLIST,
      ;; as a list of tagged values (TAG . FORM).
      ;; If there are any at the end, they go in LIST, likewise.
      (while (consp rest)
	;; Turn . (, foo) into (,@ foo).
	(if (eq (car rest) backquote-unquote-symbol)
	    (setq rest (list (list backquote-splice-symbol (nth 1 rest)))))
	(setq item (backquote-process (car rest)))
	(cond
	 ((= (car item) 2)
	  ;; Put the nonspliced items before the first spliced item
	  ;; into FIRSTLIST.
	  (if (null lists)
	      (setq firstlist list
		    list nil))
	  ;; Otherwise, put any preceding nonspliced items into LISTS.
	  (if list
	      (setq lists (cons (backquote-listify list '(0 . nil)) lists)))
	  (setq lists (cons (cdr item) lists))
	  (setq list nil))
	 (t
	  (setq list (cons item list))))
	(setq rest (cdr rest)))
      ;; Handle nonsplicing final elements, and the tail of the list
      ;; (which remains in REST).
      (if (or rest list)
	  (setq lists (cons (backquote-listify list (backquote-process rest))
			    lists)))
      ;; Turn LISTS into a form that produces the combined list. 
      (setq expression
	    (if (or (cdr lists)
		    (eq (car-safe (car lists)) backquote-splice-symbol))
		(cons 'append (nreverse lists))
	      (car lists)))
      ;; Tack on any initial elements.
      (if firstlist
	  (setq expression (backquote-listify firstlist (cons 1 expression))))
      (if (eq (car-safe expression) 'quote)
	  (cons 0 (list 'quote s))
	(cons 1 expression))))))

;; backquote-listify takes (tag . structure) pairs from backquote-process
;; and decides between append, list, backquote-list*, and cons depending
;; on which tags are in the list.

(defun backquote-listify (list old-tail)
  (let ((heads nil) (tail (cdr old-tail)) (list-tail list) (item nil))
    (if (= (car old-tail) 0)
	(setq tail (eval tail)
	      old-tail nil))
    (while (consp list-tail)
      (setq item (car list-tail))
      (setq list-tail (cdr list-tail))
      (if (or heads old-tail (/= (car item) 0))
	  (setq heads (cons (cdr item) heads))
	(setq tail (cons (eval (cdr item)) tail))))
    (cond
     (tail
      (if (null old-tail)
	  (setq tail (list 'quote tail)))
      (if heads
	  (let ((use-list* (or (cdr heads)
			       (and (consp (car heads))
				    (eq (car (car heads))
					backquote-splice-symbol)))))
	    (cons (if use-list* 'backquote-list* 'cons)
		  (append heads (list tail))))
	tail))
     (t (cons 'list heads)))))

;; backquote.el ends here
