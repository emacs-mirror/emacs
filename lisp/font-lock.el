;; Electric Font Lock Mode
;; Copyright (C) 1992, 1993, 1994 Free Software Foundation, Inc.

;; Author: jwz, then rms
;; Maintainer: FSF
;; Keywords: languages, faces

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
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;;; Commentary:

;; Font-lock-mode is a minor mode that causes your comments to be 
;; displayed in one face, strings in another, reserved words in another,
;; documentation strings in another, and so on.
;;
;; Comments will be displayed in `font-lock-comment-face'.
;; Strings will be displayed in `font-lock-string-face'.
;; Doc strings will be displayed in `font-lock-doc-string-face'.
;; Function and variable names (in their defining forms) will be
;;  displayed in `font-lock-function-name-face'.
;; Reserved words will be displayed in `font-lock-keyword-face'.
;;
;; To make the text you type be fontified, use M-x font-lock-mode.
;; When this minor mode is on, the fonts of the current line are
;; updated with every insertion or deletion.
;;
;; To define new reserved words or other patterns to highlight, use
;; the `font-lock-keywords' variable.  This should be mode-local.
;;
;; To turn this on automatically, add this to your .emacs file:
;;
;;	(setq emacs-lisp-mode-hook '(lambda () (font-lock-mode 1)))
;;
;; On a Sparc2, the initial fontification takes about 12 seconds for a 120k
;; file of C code, using the default configuration.  You can speed this up
;; substantially by removing some of the patterns that are highlighted by
;; default.  Fontifying Lisp code is significantly faster, because Lisp has a
;; more regular syntax than C, so the expressions don't have to be as hairy.

;;; Code:

(or window-system
    (error "Can't fontify on an ASCII terminal"))

(defvar font-lock-comment-face
  'italic
  "Face to use for comments.")

(defvar font-lock-doc-string-face
  'italic
  "Face to use for documentation strings.")

(defvar font-lock-string-face
  'underline
  "Face to use for string constants.")

(defvar font-lock-function-name-face
  'bold-italic
  "Face to use for function names.")

(defvar font-lock-keyword-face
  'bold
  "Face to use for keywords.")

(defvar font-lock-type-face
  'italic
  "Face to use for data types.")

(defvar font-lock-no-comments nil
  "Non-nil means Font-Lock shouldn't check for comments or strings.")

(make-variable-buffer-local 'font-lock-keywords)
(defvar font-lock-keywords nil
  "*The keywords to highlight.
If this is a list, then elements may be of the forms:

  \"string\"			  ; A regexp to highlight in the 
				  ;  `font-lock-keyword-face'.
  (\"string\" . N)  	          ; Highlight subexpression N of the regexp.
  (\"string\" . face-name)	  ; Use the named face
  (\"string\" N face-name)        ; Both of the above
  (\"string\" N face-name t)      ; This allows highlighting to override
				  ;  already-highlighted regions.
  (\"string\" N face-name keep)   ; This allows highlighting to occur
				  ; even if some parts of what STRING matches
				  ; are already highlighted--but does not alter
				  ; the existing highlighting of those parts.

These regular expressions should not match text which spans lines.
While \\[font-lock-fontify-buffer] handles multi-line patterns correctly,
updating when you edit the buffer does not,
since it considers text one line at a time.

Be careful composing regexps for this list; the wrong pattern can dramatically
slow things down!")

(defvar font-lock-keywords-case-fold-search nil
  "*Non-nil means the patterns in `font-lock-keywords' are case-insensitive.")

(defvar font-lock-syntax-table nil
  "*Non-nil means use this syntax table for fontifying.
If this is nil, the major mode's syntax table is used.")

(defvar font-lock-verbose t
  "*Non-nil means `font-lock-fontify-buffer' should print status messages.")

;;;###autoload
(defvar font-lock-mode-hook nil
  "Function or functions to run on entry to Font Lock mode.")

;;; These variables record, for each buffer,
;;; the parse state at a particular position, always the start of a line.
;;; This is used to make font-lock-fontify-region faster.
(defvar font-lock-cache-position nil)
(defvar font-lock-cache-state nil)
(make-variable-buffer-local 'font-lock-cache-position)
(make-variable-buffer-local 'font-lock-cache-state)

(defun font-lock-fontify-region (start end)
  "Put proper face on each string and comment between START and END."
  (save-excursion
    (goto-char start)
    (beginning-of-line)
    (setq end (min end (point-max)))
    (let ((buffer-read-only nil)
	  state startline prev prevstate
	  (modified (buffer-modified-p)))
      ;; Find the state at the line-beginning before START.
      (setq startline (point))
      (if (eq (point) font-lock-cache-position)
	  (setq state font-lock-cache-state)
	;; Find outermost containing sexp.
	(beginning-of-defun)
	;; Find the state at STARTLINE.
	(while (< (point) startline)
	  (setq state (parse-partial-sexp (point) startline 0)))
	(setq font-lock-cache-state state
	      font-lock-cache-position (point)))
      ;; Now find the state precisely at START.
      (setq state (parse-partial-sexp (point) start nil nil state))
      ;; If the region starts inside a string, show the extent of it.
      (if (nth 3 state)
	  (let ((beg (point)))
	    (while (and (re-search-forward "\\s\"" end 'move)
			(nth 3 (parse-partial-sexp beg (point)
						   nil nil state))))
	    (put-text-property beg (point) 'face font-lock-string-face)
	    (setq state (parse-partial-sexp beg (point) nil nil state))))
      ;; Likewise for a comment.
      (if (or (nth 4 state) (nth 7 state))
	  (let ((beg (point)))
	    (while (and (re-search-forward (if comment-end
					       (concat "\\s>\\|"
						       (regexp-quote comment-end))
					     "\\s>")
					   end 'move)
			(nth 3 (parse-partial-sexp beg (point)
						   nil nil state))))
	    (put-text-property beg (point) 'face font-lock-comment-face)
	    (setq state (parse-partial-sexp beg (point) nil nil state))))
      ;; Find each interesting place between here and END.
      (while (and (< (point) end)
		  (setq prev (point) prevstate state)
		  (re-search-forward (if comment-start-skip
					 (concat "\\s\"\\|" comment-start-skip)
				       "\\s\"")
				     end t)
		  ;; Clear out the fonts of what we skip over.
		  (progn (remove-text-properties prev (point) '(face nil)) t)
		  ;; Verify the state at that place
		  ;; so we don't get fooled by \" or \;.
		  (setq state (parse-partial-sexp prev (point)
						  nil nil state)))
	(let ((here (point)))
	  (if (or (nth 4 state) (nth 7 state))
	      ;; We found a real comment start.
	      (let ((beg (match-beginning 0)))
		(goto-char beg)
		(save-restriction
		  (narrow-to-region (point-min) end)
		  (condition-case nil
		      (progn
			(forward-comment 1)
			;; forward-comment skips all whitespace,
			;; so go back to the real end of the comment.
			(skip-chars-backward " \t"))
		    (error (goto-char end))))
		(put-text-property beg (point) 'face font-lock-comment-face)
		(setq state (parse-partial-sexp here (point) nil nil state)))
	    (if (nth 3 state)
		(let ((beg (match-beginning 0)))
		  (while (and (re-search-forward "\\s\"" end 'move)
			      (nth 3 (parse-partial-sexp here (point)
							 nil nil state))))
		  (put-text-property beg (point) 'face font-lock-string-face)
		  (setq state (parse-partial-sexp here (point) nil nil state))))
	      ))
	;; Make sure PREV is non-nil after the loop
	;; only if it was set on the very last iteration.
	(setq prev nil))
      (and prev
	   (remove-text-properties prev end '(face nil)))
      (and (buffer-modified-p)
	   (not modified)
	   (set-buffer-modified-p nil)))))

;; This code used to be used to show a string on reaching the end of it.
;; It is probably not needed due to later changes to handle strings
;; starting before the region in question.
;;	    (if (and (null (nth 3 state))
;;		     (eq (char-syntax (preceding-char)) ?\")
;;		     (save-excursion
;;		       (nth 3 (parse-partial-sexp prev (1- (point))
;;						  nil nil prevstate))))
;;		;; We found the end of a string.
;;		(save-excursion
;;		  (setq foo2 (point))
;;		  (let ((ept (point)))
;;		    (forward-sexp -1)
;;		    ;; Highlight the string when we see the end.
;;		    ;; Doing it at the start leads to trouble:
;;		    ;; either it fails to handle multiline strings
;;		    ;; or it can run away when an unmatched " is inserted.
;;		    (put-text-property (point) ept 'face
;;				       (if (= (car state) 1)
;;					   font-lock-doc-string-face
;;					 font-lock-string-face)))))

(defun font-lock-unfontify-region (beg end)
  (let ((modified (buffer-modified-p))
	(buffer-read-only nil))
    (remove-text-properties beg end '(face nil))
    (set-buffer-modified-p modified)))

;; Called when any modification is made to buffer text.
(defun font-lock-after-change-function (beg end old-len)
  (save-excursion
    (save-match-data
      (goto-char beg)
      ;; Discard the cache info if text before it has changed.
      (and font-lock-cache-position
	   (> font-lock-cache-position beg)
	   (setq font-lock-cache-position nil))
      ;; Rescan till end of line.  yes!
      (goto-char end)
      (end-of-line)
      (setq end (point))
      (goto-char beg)
      (beginning-of-line)
      (setq beg (point))
      ;; First scan for strings and comments.
      ;; Must scan from line start in case of
      ;; inserting space into `intfoo () {}'.
      (if font-lock-no-comments
	  (remove-text-properties beg (min (1+ end) (point-max)) '(face nil))
	(font-lock-fontify-region beg (min (1+ end) (point-max))))
      ;; Now scan for keywords.
      (font-lock-hack-keywords beg end))))

;;; Fontifying arbitrary patterns

(defsubst font-lock-any-properties-p (start end)
  (or (get-text-property start 'face)
      (let ((next (next-single-property-change start 'face)))
	(and next (< next end)))))

(defun font-lock-hack-keywords (start end &optional loudly)
  (goto-char start)
  (let ((case-fold-search font-lock-keywords-case-fold-search)
	(rest font-lock-keywords)
	(count 0)
	(buffer-read-only nil)
	(modified (buffer-modified-p))
	first str match face s e allow-overlap-p
	(old-syntax (syntax-table)))
    (unwind-protect
	(progn
	  (if font-lock-syntax-table
	      (set-syntax-table font-lock-syntax-table))
	  (while rest
	    (setq first (car rest) rest (cdr rest))
	    (goto-char start)
	    (cond ((consp first)
		   (setq str (car first))
		   (cond ((consp (cdr first))
			  (setq match (nth 1 first)
				face (eval (nth 2 first))
				allow-overlap-p (nth 3 first)))
			 ((symbolp (cdr first))
			  (setq match 0 allow-overlap-p nil
				face (eval (cdr first))))
			 (t
			  (setq match (cdr first)
				allow-overlap-p nil
				face font-lock-keyword-face))))
		  (t
		   (setq str first match 0 allow-overlap-p nil
			 face font-lock-keyword-face)))
	    ;(message "regexp: %s" str)
	    (while (re-search-forward str end t)
	      (setq s (match-beginning match)
		    e (match-end match))
	      (or s (error "expression did not match subexpression %d" match))
	      ;; don't fontify this keyword if we're already in some other context.
	      (or (if allow-overlap-p nil (font-lock-any-properties-p s e))
		  (if (not (memq allow-overlap-p '(t nil)))
		      (save-excursion
			(goto-char s)
			(while (< (point) e)
			  (let ((next (next-single-property-change (point) 'face
								   nil e)))
			    (if (or (null next) (> next e))
				(setq next e))
			    (if (not (get-text-property (point) 'face))
				(put-text-property (point) next 'face face))
			    (goto-char next))))
		    (put-text-property s e 'face face))))
	    (if loudly (message "Fontifying %s... (regexps...%s)"
				(buffer-name)
				(make-string (setq count (1+ count)) ?.)))))
      (set-syntax-table old-syntax))
    (and (buffer-modified-p)
	 (not modified)
	 (set-buffer-modified-p nil))))

;; The user level functions

(defvar font-lock-mode nil) ; for modeline
(or (assq 'font-lock-mode minor-mode-alist)
    (setq minor-mode-alist
	  (append minor-mode-alist
		  '((font-lock-mode " Font")))))

(defvar font-lock-fontified nil) ; whether we have hacked this buffer
(put 'font-lock-fontified 'permanent-local t)

;;;###autoload
(defun font-lock-mode (&optional arg)
  "Toggle Font Lock mode.
With arg, turn Font Lock mode on if and only if arg is positive.

When Font Lock mode is enabled, text is fontified as you type it:

 - comments are displayed in `font-lock-comment-face';
     (That is a variable whose value should be a face name.)
 - strings are displayed in `font-lock-string-face';
 - documentation strings are displayed in `font-lock-doc-string-face';
 - function and variable names in their defining forms are displayed
   in `font-lock-function-name-face';
 - and certain other expressions are displayed in other faces
   according to the value of the variable `font-lock-keywords'.

When you turn Font Lock mode on/off, the buffer is fontified/defontified.
To fontify a buffer without having newly typed text become fontified, you
can use \\[font-lock-fontify-buffer]."
  (interactive "P")
  (let ((on-p (if (null arg)
		  (not font-lock-mode)
		(> (prefix-numeric-value arg) 0))))
    (if (equal (buffer-name) " *Compiler Input*") ; hack for bytecomp...
	(setq on-p nil))
    (make-local-variable 'after-change-functions)
    (if on-p
	(or (memq 'font-lock-after-change-function after-change-functions)
	    (setq after-change-functions (cons 'font-lock-after-change-function
					       after-change-functions)))
      (setq after-change-functions
	    (delq 'font-lock-after-change-function
		  (copy-sequence after-change-functions))))
    (set (make-local-variable 'font-lock-mode) on-p)
    (make-local-variable 'font-lock-no-comments)
    (cond (on-p
	   (font-lock-set-defaults)
	   (make-local-variable 'before-revert-hook)
	   (make-local-variable 'after-revert-hook)
	   ;; If buffer is reverted, must clean up the state.
	   (add-hook 'before-revert-hook 'font-lock-revert-setup)
	   (add-hook 'after-revert-hook 'font-lock-revert-cleanup)
	   (run-hooks 'font-lock-mode-hook)
	   (or font-lock-fontified (font-lock-fontify-buffer)))
	  (font-lock-fontified
	   (setq font-lock-fontified nil)
	   (remove-hook 'before-revert-hook 'font-lock-revert-setup)
	   (remove-hook 'after-revert-hook 'font-lock-revert-cleanup)
	   (font-lock-unfontify-region (point-min) (point-max))))
    (force-mode-line-update)))

;; If the buffer is about to be reverted, it won't be fontified.
(defun font-lock-revert-setup ()
  (setq font-lock-fontified nil))

;; If the buffer has just been reverted, we might not even be in font-lock
;; mode anymore, and if we are, the buffer may or may not have already been
;; refontified.  Refontify here if it looks like we need to.
(defun font-lock-revert-cleanup ()
  (and font-lock-mode
       (not font-lock-fontified)
       (font-lock-mode 1)))

(defun font-lock-fontify-buffer ()
  "Fontify the current buffer the way `font-lock-mode' would:

 - comments are displayed in `font-lock-comment-face';
 - strings are displayed in `font-lock-string-face';
 - documentation strings are displayed in `font-lock-doc-string-face';
 - function and variable names in their defining forms are displayed
   in `font-lock-function-name-face';
 - and certain other expressions are displayed in other faces
   according to the value of the variable `font-lock-keywords'.

This can take a while for large buffers."
  (interactive)
  (let ((was-on font-lock-mode)
	(font-lock-verbose (or font-lock-verbose (interactive-p))))
    (if font-lock-verbose (message "Fontifying %s..." (buffer-name)))
    ;; Turn it on to run hooks and get the right font-lock-keywords.
    (or was-on (font-lock-set-defaults))
    (font-lock-unfontify-region (point-min) (point-max))
    (if (and font-lock-verbose (not font-lock-no-comments))
	(message "Fontifying %s... (syntactically...)" (buffer-name)))
    (save-excursion
      (or font-lock-no-comments
	  (font-lock-fontify-region (point-min) (point-max)))
      (if font-lock-verbose (message "Fontifying %s... (regexps...)"
				     (buffer-name)))
      (font-lock-hack-keywords (point-min) (point-max) font-lock-verbose))
    (set (make-local-variable 'font-lock-fontified) t)
    (if font-lock-verbose (message "Fontifying %s... done." (buffer-name)))
    ))


;;; Various mode-specific information.

(defconst lisp-font-lock-keywords-1
 '(;;
   ;; highlight defining forms.  This doesn't work too nicely for
   ;; (defun (setf foo) ...) but it does work for (defvar foo) which
   ;; is more important.
   ("^(def[-a-z]+\\s +\\([^ \t\n\)]+\\)" 1 font-lock-function-name-face)
   ;;
   ;; highlight CL keywords
   ("\\s :\\(\\(\\sw\\|\\s_\\)+\\)\\>" . 1)
   ;;
   ;; this is highlights things like (def* (setf foo) (bar baz)), but may
   ;; be slower (I haven't really thought about it)
;   ("^(def[-a-z]+\\s +\\(\\s(\\S)*\\s)\\|\\S(\\S *\\)"
;    1 font-lock-function-name-face)
   )
 "For consideration as a value of `lisp-font-lock-keywords'.
This does fairly subdued highlighting.")

(defconst lisp-font-lock-keywords-2
  (append
   lisp-font-lock-keywords-1
   '(;;
     ;; Highlight control structures
     ("(\\(cond\\|if\\|when\\|unless\\|[ec]?\\(type\\)?case\\)[ \t\n]" . 1)
     ("(\\(while\\|do\\|let\\*?\\|flet\\|labels\\|prog[nv12*]?\\)[ \t\n]" . 1)
     ("(\\(catch\\|\\throw\\|block\\|return\\|return-from\\)[ \t\n]" . 1)
     ("(\\(save-restriction\\|save-window-restriction\\)[ \t\n]" . 1)
     ("(\\(save-excursion\\|unwind-protect\\|condition-case\\)[ \t\n]" . 1)
     ;;
     ;; highlight function names in emacs-lisp docstrings (in the syntax
     ;; that substitute-command-keys understands.)
     ("\\\\\\\\\\[\\([^]\\\n]+\\)]" 1 font-lock-keyword-face t)
     ;;
     ;; highlight words inside `' which tend to be function names
     ("`\\([-a-zA-Z0-9_][-a-zA-Z0-9_][-a-zA-Z0-9_.]+\\)'"
      1 font-lock-keyword-face t)
     ))
 "For consideration as a value of `lisp-font-lock-keywords'.
This does a lot more highlighting.")

;; default to the gaudier variety?
;(defvar lisp-font-lock-keywords lisp-font-lock-keywords-2
;  "Additional expressions to highlight in Lisp modes.")
(defvar lisp-font-lock-keywords lisp-font-lock-keywords-1
  "Additional expressions to highlight in Lisp modes.")


(defconst c-font-lock-keywords-1 nil
 "For consideration as a value of `c-font-lock-keywords'.
This does fairly subdued highlighting.")

(defconst c-font-lock-keywords-2 nil
 "For consideration as a value of `c-font-lock-keywords'.
This does a lot more highlighting.")

(defconst c++-font-lock-keywords-1 nil
 "For consideration as a value of `c++-font-lock-keywords'.
This does fairly subdued highlighting.")

(defconst c++-font-lock-keywords-2 nil
 "For consideration as a value of `c++-font-lock-keywords'.
This does a lot more highlighting.")

(let* ((storage "auto\\|extern\\|register\\|static\\|typedef")
       (struct "struct\\|union\\|enum")
       (prefixes "signed\\|unsigned\\|short\\|long")
       (types (concat prefixes "\\|int\\|char\\|float\\|double\\|void"))
       (ctoken "[a-zA-Z0-9_:~*]+")
       (c++-things (concat
		    "const\\|class\\|protected:\\|private:\\|public:\\|inline\\|"
		    "new\\|delete")))
 (setq c-font-lock-keywords-1
  (list
   ;; fontify preprocessor directives as comments.
   '("^#[ \t]*[a-z]+" . font-lock-comment-face)
   ;;
   ;; fontify names being defined.
   '("^#[ \t]*\\(define\\|undef\\)[ \t]+\\(\\(\\sw\\|\\s_\\)+\\)" 2
     font-lock-function-name-face)
   ;;
   ;; fontify other preprocessor lines.
   '("^#[ \t]*\\(if\\|elif\\|else\\|endif\\)[ \t]+\\([^\n]+\\)"
     2 font-lock-function-name-face keep)
   '("^#[ \t]*\\(ifn?def\\)[ \t]+\\([^ \t\n]+\\)"
     2 font-lock-function-name-face t)
   ;;
   ;; fontify the filename in #include <...>
   ;; don't need to do this for #include "..." because those were
   ;; already fontified as strings by the syntactic pass.
   '("^#[ \t]*include[ \t]+\\(<[^>\"\n]+>\\)" 1 font-lock-string-face)
   ;;
   ;; fontify the names of functions being defined.
   (list (concat
	  "^\\(" ctoken "[ \t]+\\)?"	; type specs; there can be no
	  "\\(" ctoken "[ \t]+\\)?"	; more than 3 tokens, right?
	  "\\(" ctoken "[ \t]+\\)?"
	  "\\([*&]+[ \t]*\\)?"		; pointer
	  "\\(" ctoken "\\)[ \t]*(")		; name
    5 'font-lock-function-name-face)
   ;;
   ;;
   ;; Fontify structure names (in structure definition form).
   (list (concat "^\\(" storage "\\)?[ \t]*\\<\\(" struct "\\)"
	  "[ \t]+\\(" ctoken "\\)[ \t]*\\(\{\\|$\\)")
    3 'font-lock-function-name-face)
   ;;
   ;; Fontify declarations of simple identifiers (including typedefs).
   ;; (Should this be in c-font-lock-keywords-2 instead?)
   (list (concat "^[ \t]*\\(\\(" storage "\\)[ \t]+\\)?\\(\\(\\(" prefixes
	  "\\)\\>[ \t]*\\)*\\(" types "\\)\\)[ \t]+\\(" ctoken
	  "\\)[ \t]*[=;]")
    7 'font-lock-function-name-face 'keep)
   ;;
   ;; And likewise for structs
   (list (concat "^[ \t]*\\(\\(" storage "\\)[ \t]+\\)?\\(" struct
	  "\\)[ \t]+" ctoken "[ \t]+\\(" ctoken "\\);")
    4 'font-lock-function-name-face 'keep)
   ;;
   ;; Fontify case clauses.  This is fast because its anchored on the left.
   '("case[ \t]+\\(\\(\\sw\\|\\s_\\)+\\):". 1)
   '("\\<\\(default\\):". 1)
   ))

 (setq c-font-lock-keywords-2
  (append c-font-lock-keywords-1
   (list
    ;;
    ;; fontify all storage classes and type specifiers
    (cons (concat "\\<\\(" storage "\\)\\>") 'font-lock-type-face)
    (cons (concat "\\<\\(" types "\\)\\>") 'font-lock-type-face)
    (cons (concat "\\<\\(\\(\\(" prefixes "\\)\\>[ \t]*\\)*\\(" types
	   "\\)\\)\\>")
     'font-lock-type-face)
    (list (concat "\\<\\(" struct "\\)[ \t]+" ctoken)
     0 'font-lock-type-face 'keep)
    ;;
    ;; fontify all builtin tokens
    (cons (concat
	   "[ \t]\\("
	   (mapconcat 'identity
	    '("for" "while" "do" "return" "goto" "case" "break" "switch"
	      "if" "else" "default" "continue" "default")
	    "\\|")
	   "\\)[ \t\n(){};,]")
     1)
    ;;
    ;; fontify case targets and goto-tags.  This is slow because the
    ;; expression is anchored on the right.
    '("[ \t\n]\\(\\(\\sw\\|\\s_\\)+\\):" . 1)
    ;;
    ;; Fontify variables declared with structures, or typedef names.
    '("}[ \t*]*\\(\\(\\sw\\|\\s_\\)+\\)[ \t]*[,;]"
      1 font-lock-function-name-face)
    ;;
    ;; Fontify global variables without a type.
;    '("^\\([_a-zA-Z0-9:~*]+\\)[ \t]*[[;={]" 1 font-lock-function-name-face)
    )))

 (setq c++-font-lock-keywords-1
       (cons 
	(concat "\\(" c++-things "\\)[ \t\n]")
	c-font-lock-keywords-1))
 (setq c++-font-lock-keywords-2
       (cons 
	(cons (concat "\\<\\(" c++-things "\\)\\>") 'font-lock-type-face)
	c-font-lock-keywords-2))
 )

; default to the gaudier variety?
(defvar c-font-lock-keywords c-font-lock-keywords-1
  "Additional expressions to highlight in C mode.")

(defvar c++-font-lock-keywords c++-font-lock-keywords-1
  "Additional expressions to highlight in C++ mode.")


(defvar perl-font-lock-keywords
  (list
   (cons (concat "[ \n\t{]*\\("
		 (mapconcat 'identity
			    '("if" "until" "while" "elsif" "else" "unless" "for"
			      "foreach" "continue" "exit" "die" "last" "goto" "next"
			      "redo" "return" "local" "exec")
			    "\\|")
		 "\\)[ \n\t;(]") 1)
   (mapconcat 'identity
	      '("#endif" "#else" "#ifdef" "#ifndef" "#if" "#include"
		"#define" "#undef")
	      "\\|")
   '("^[ \n\t]*sub[ \t]+\\([^ \t{]+\\)[ \t]*[{]" 1 font-lock-function-name-face)
   '("[ \n\t{]*\\(eval\\)[ \n\t(;]" 1 font-lock-function-name-face)
   '("\\(--- .* ---\\|=== .* ===\\)" . font-lock-doc-string-face)
   )
  "Additional expressions to highlight in Perl mode.")

(defvar tex-font-lock-keywords
  (list
   '("\\(\\\\\\([a-zA-Z@]+\\|.\\)\\)" 1 font-lock-keyword-face t)
   '("{\\\\em\\([^}]+\\)}" 1 font-lock-comment-face t)
   '("{\\\\bf\\([^}]+\\)}" 1 font-lock-keyword-face t)
   '("^[ \t\n]*\\\\def[\\\\@]\\(\\w+\\)" 1 font-lock-function-name-face t)
   '("\\\\\\(begin\\|end\\){\\([a-zA-Z0-9\\*]+\\)}"
     2 font-lock-function-name-face t)
   '("[^\\\\]\\$\\([^$]*\\)\\$" 1 font-lock-string-face t)
;   '("\\$\\([^$]*\\)\\$" 1 font-lock-string-face t)
   )
  "Additional expressions to highlight in TeX mode.")

(defvar texi-font-lock-keywords
  (list
   "@\\(@\\|[^}\t \n{]+\\)"					;commands
   '("^\\(@c\\|@comment\\)[ \t].*$" . font-lock-comment-face)	;comments
   '("^\\(*.*\\)[\t ]*$" 1 font-lock-function-name-face t)	;menu items
   '("@\\(emph\\|strong\\|b\\|i\\){\\([^}]+\\)" 2 font-lock-comment-face t)
   '("@\\(file\\|kbd\\|key\\){\\([^}]+\\)" 2 font-lock-string-face t)
   '("@\\(samp\\|code\\|var\\){\\([^}]+\\)" 2 font-lock-function-name-face t)
   '("@\\(xref\\|pxref\\){\\([^}]+\\)" 2 font-lock-keyword-face t)
   '("@end *\\([a-zA-Z0-9]+\\)[ \t]*$" 1 font-lock-function-name-face t)
   '("@item \\(.*\\)$" 1 font-lock-function-name-face t)
   '("\\$\\([^$]*\\)\\$" 1 font-lock-string-face t)
   )
  "Additional expressions to highlight in TeXinfo mode.")

(defvar shell-font-lock-keywords
  (list (cons shell-prompt-pattern 'font-lock-keyword-face)
	'("[ \t]\\([+-][^ \t\n]+\\)" 1 font-lock-comment-face)
	'("^[^ \t]+:.*$" . font-lock-string-face)
	'("^\\[[1-9][0-9]*\\]" . font-lock-string-face))
  "Additional expressions to highlight in Shell mode.")

(defvar dired-font-lock-keywords
  '(;; Put directory headers in italics.
    ("^  \\(/.+\\)$" 1 font-lock-type-face)
    ;; Put symlinks in bold italics.
    ("\\([^ ]+\\) -> [^ ]+$" . font-lock-function-name-face)
    ;; Put marks in bold.
    ("^\\([^ ]\\).*$" 1 font-lock-keyword-face t)
    ;; Put files that are subdirectories in bold.
    ("^..d.* \\([^ ]+\\)$" 1 font-lock-keyword-face))
  "Additional expressions to highlight in Dired mode.")

(defvar rmail-font-lock-keywords
  '(;; Put From field in bold.
    ("^From: \\(.*\\)$" 1 font-lock-keyword-face)
    ;; Put subject in bold italics
    ("^Subject: \\(.*\\)$" 1 font-lock-function-name-face))
  "Additional expressions to highlight in Rmail mode.")

(defvar rmail-summary-font-lock-keywords
  '(("^\\s *[0-9]+D.*$" . font-lock-doc-string-face)
    ("^\\s *[0-9]+-.*$" . font-lock-keyword-face))
  "Additional expressions to highlight in Rmail Summary mode.")

(defvar compilation-mode-font-lock-keywords
  '(("^\\([^\n:]*:\\([0-9]+:\\)+\\)\\(.*\\)$" 1 font-lock-function-name-face))
;;;  ("^\\([^\n:]*:\\([0-9]+:\\)+\\)\\(.*\\)$" 0 font-lock-keyword-face keep)
  "Additional expressions to highlight in Compilation mode.")

(defun font-lock-set-defaults ()
  "Set `font-lock-keywords' to something appropriate for this mode."
  (if (memq major-mode '(rmail-mode dired-mode compilation-mode shell-mode))
      (setq font-lock-no-comments t))
  (if (not font-lock-keywords)		; if not already set.
      (setq font-lock-keywords
	    (cond ((eq major-mode 'lisp-mode)	    lisp-font-lock-keywords)
		  ((eq major-mode 'emacs-lisp-mode) lisp-font-lock-keywords)
		  ((eq major-mode 'c-mode)
		   (make-local-variable 'font-lock-syntax-table)
		   (setq font-lock-syntax-table
			 (copy-syntax-table (syntax-table)))
		   (modify-syntax-entry ?_ "w" font-lock-syntax-table)
		   c-font-lock-keywords)
		  ((eq major-mode 'c++-c-mode)
		   (make-local-variable 'font-lock-syntax-table)
		   (setq font-lock-syntax-table
			 (copy-syntax-table (syntax-table)))
		   (modify-syntax-entry ?_ "w" font-lock-syntax-table)
		   c-font-lock-keywords)
		  ((eq major-mode 'c++-mode)	    c++-font-lock-keywords)
		  ((eq major-mode 'perl-mode) 	    perl-font-lock-keywords)
		  ((eq major-mode 'plain-tex-mode)  tex-font-lock-keywords)
		  ((eq major-mode 'latex-mode)      tex-font-lock-keywords)
		  ((eq major-mode 'slitex-mode)     tex-font-lock-keywords)
		  ((eq major-mode 'texinfo-mode)    texi-font-lock-keywords)
		  ((eq major-mode 'shell-mode)      shell-font-lock-keywords)
		  ((eq major-mode 'dired-mode)      dired-font-lock-keywords)
		  ((eq major-mode 'rmail-mode)      rmail-font-lock-keywords)
		  ((eq major-mode 'rmail-summary-mode)
		   rmail-summary-font-lock-keywords)
		  ((eq major-mode 'compilation-mode)
		   compilation-mode-font-lock-keywords)
		  (t nil)))))

(provide 'font-lock)

;;; font-lock.el ends here
