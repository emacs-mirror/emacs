;;; pascal.el  -  Major mode for editing pascal source in emacs.

;;; Copyright (C) 1993 Free Software Foundation, Inc.

;; Author: Espen Skoglund (espensk@stud.cs.uit.no)
;; Keywords: languages

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

;;; If you want to customize the pascal mode in your startup file, you
;;; can add these lines to your .emacs file (and remove the ;s at the
;;; beginning of the line):
;;;
;;; ;;; Pascal-mode custumization.
;;; (autoload 'pascal-mode "pascal-mode" nil t)
;;; (setq auto-mode-alist (append (list (cons "\\.p$" 'pascal-mode)
;;;                                     (cons "\\.pas$" 'pascal-mode))
;;;                       auto-mode-alist))
;;; (setq pascal-mode-hook '(lambda ()
;;;	                      ;; User specifications
;;;	                      (setq pascal-tab-always-indent t
;;;		                   pascal-auto-newline nil
;;;		                   pascal-auto-endcomments t
;;;		                   pascal-indent-level 3
;;;                                pascal-continued-expr 1
;;;		                   pascal-label-offset -2
;;;		                   pascal-case-offset 2
;;;		                   pascal-typedecl-indent 10
;;; 	 	                   pascal-vardecl-indent 20)))

;;; USAGE
;;; =====
;;; If you have modified your startup file as described above, emacs
;;; should enter pascal-mode when you load a pascal source into emacs.
;;; If not, you will have to start pascal-mode manually:
;;;    M-x load-library pascal-mode
;;;    M-x pascal-mode
;;; When you have entered pascal-mode, you may get more info by pressing
;;; C-h m. You may also get online help describing various functions by:
;;;   C-h d <Name of function you want described>

;;; KNOWN BUGS / BUGREPORTS
;;; =======================
;;; As far as I know, there are no bugs in the current version of this
;;; package. This may not be true however, since I never use this mode
;;; myself and therefore would never notice them anyway. But if you DO
;;; find any bugd, you may submitt them to: espensk@stud.cs.uit.no

;;; LCD Archive Entry:
;;; pascal-mode|Espen Skoglund|espensk@stud.cs.uit.no|
;;; Major mode for editing Pascal code|
;;; 14-Sep-93|$Revision: 1.3 $|~/modes/pascal-mode.el.Z|

(defconst pascal-mode-version "1.3"
  "Version of this pascal mode.")

(defvar pascal-mode-abbrev-table nil
  "Abbrev table in use in Pascal-mode buffers.")
(define-abbrev-table 'pascal-mode-abbrev-table ())

(defvar pascal-mode-map ()
  "Keymap used in Pascal mode.")
(if (null pascal-mode-map)
    (setq pascal-mode-map (make-sparse-keymap)))

(define-key pascal-mode-map ";" 'electric-pascal-semi)
(define-key pascal-mode-map "." 'electric-pascal-dot)
(define-key pascal-mode-map ":" 'electric-pascal-colon)
(define-key pascal-mode-map "=" 'electric-pascal-equal)
(define-key pascal-mode-map "\r" 'electric-pascal-terminate-line)
(define-key pascal-mode-map "\t" 'electric-pascal-tab)
(define-key pascal-mode-map "\177" 'backward-delete-char-untabify)
(define-key pascal-mode-map "\C-\M-a" 'pascal-backward-to-beginning-of-function)
(define-key pascal-mode-map "\C-\M-e" 'pascal-forward-to-end-of-function)
(define-key pascal-mode-map "\C-\M-h" 'pascal-mark-function)
(define-key pascal-mode-map "\C-c\C-b" 'pascal-insert-block)
(define-key pascal-mode-map "\C-c\C-c" 'pascal-comment-area)
(define-key pascal-mode-map "\C-c\C-u" 'pascal-uncomment-area)
(define-key pascal-mode-map "\M-*" 'pascal-star-comment)

;;; A command to change the whole buffer won't be used terribly
;;; often, so no need for a key binding.
;;;(define-key pascal-mode-map "\C-c\C-l" 'pascal-downcase-keywords)
;;;(define-key pascal-mode-map "\C-c\C-u" 'pascal-upcase-keywords)
;;;(define-key pascal-mode-map "\C-c\C-c" 'pascal-capitalize-keywords)

(defvar pascal-keywords '("and" "array" "begin" "case" "const" "div" "do" 
"downto" "else" "end" "file" "for" "function" "goto" "if" "in" "label" "mod" 
"nil" "not" "of" "or" "packed" "procedure" "program" "record" "repeat" "set" 
"then" "to" "type" "until" "var" "while" "with"
;; The following are not standard in pascal, but widely used.
"get" "put" "input" "output" "read" "readln" "reset" "rewrite" "write"
"writeln"))

(defvar pascal-mode-syntax-table nil
  "Syntax table in use in Pascal-mode buffers.")

(if pascal-mode-syntax-table
    ()
  (setq pascal-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\\ "\\" pascal-mode-syntax-table)
  (modify-syntax-entry ?( ". 1" pascal-mode-syntax-table)  
  (modify-syntax-entry ?) ". 4" pascal-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" pascal-mode-syntax-table)
  (modify-syntax-entry ?{ "<" pascal-mode-syntax-table)
  (modify-syntax-entry ?} ">" pascal-mode-syntax-table)
  (modify-syntax-entry ?+ "." pascal-mode-syntax-table)
  (modify-syntax-entry ?- "." pascal-mode-syntax-table)
  (modify-syntax-entry ?= "." pascal-mode-syntax-table)
  (modify-syntax-entry ?% "." pascal-mode-syntax-table)
  (modify-syntax-entry ?< "." pascal-mode-syntax-table)
  (modify-syntax-entry ?> "." pascal-mode-syntax-table)
  (modify-syntax-entry ?& "." pascal-mode-syntax-table)
  (modify-syntax-entry ?| "." pascal-mode-syntax-table)
  (modify-syntax-entry ?_ "w" pascal-mode-syntax-table)
  (modify-syntax-entry ?\' "\"" pascal-mode-syntax-table))

(defconst pascal-indent-level 3
  "*Indentation of Pascal statements with respect to containing block.")
(defconst pascal-continued-expr 1
  "*Indentation of line that is a continued expression.")
(defconst pascal-label-offset -1
  "*Offset of Pascal label lines, case statements and record lines.
This is relative to usual indentation.")
(defconst pascal-case-offset 2
  "*Indentation after case statements.")
(defconst pascal-vardecl-indent 15
  "*Indentation (from the beginning of line to `:' of the declaration.")
(defconst pascal-typedecl-indent 10
  "*Indentation (from the beginning of line to `=' of the declaration.")
(defconst pascal-auto-newline nil
  "*Non-nil means automatically newline after semicolons and `end'.")
(defconst pascal-tab-always-indent t
  "*Non-nil means TAB in Pascal mode should always reindent the current line.
It does so regardless of where in the line point is
when the TAB command is used.")
(defconst pascal-auto-endcomments t
  "*Non-nil means make a comment { ... } after the end for a case or function.
The name of the function or case is put between the braces.")

;;;###autoload
(defun pascal-mode ()
  "Major mode for editing Pascal code.
Tab indents for Pascal code.
Delete converts tabs to spaces as it moves back.
\\{pascal-mode-map}
Variables controlling indentation style:
 pascal-tab-always-indent (default t)
    Non-nil means TAB in Pascal mode should always reindent the current line,
    regardless of where in the line point is when the TAB command is used.
 pascal-auto-newline (default nil)
    Non-nil means automatically newline after semicolons and the punctation
    mark after an end.
 pascal-auto-endcomments (default t)
    Non-nil means automatically set name of function or `case' in braces after
    after the `end' if this end ends a function or a case block.
 pascal-indent-level (default 3)
    Indentation of Pascal statements within surrounding block.
 pascal-continued-expr (default 1)
    Indentation of a line that is a continued expression.
 pascal-typedecl-indent (default 10)
    Indentation to the `=' in type declarations. (Or constant declarations.)
 pascal-vardecl-indent (default 20)
    Indentation to the `:' in var declarations.
 pascal-label-offset (default -1)
    Extra indentation for line that is a label, case statement or part of
    a record block.
 pascal-case-offset (default 2)
    Extra indent to the `:' in case statements.

The only auto indention this mode doesn't fully support is if there is a
case within a type declaration.  However, this is seldom used.

When typing text, you should not worry about to get right indentions, they
will be set when you hit return. The mode will also automatically delete the
whitespaces between `*' and `)' when ending a starcomment.

Turning on Pascal mode calls the value of the variable pascal-mode-hook with
no args, if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map pascal-mode-map)
  (setq major-mode 'pascal-mode)
  (setq mode-name "Pascal")
  (setq local-abbrev-table pascal-mode-abbrev-table)
  (set-syntax-table pascal-mode-syntax-table)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'pascal-indent-line)
  (setq comment-indent-hook 'pascal-indent-within-comment)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (make-local-variable 'case-fold-search)
  (setq case-fold-search t)
  (run-hooks 'pascal-mode-hook))

;;;
;;;  Electric functions
;;;

(defun electric-pascal-terminate-line ()
  "Terminate line and indent next line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (if (looking-at "until\\b\\|end\\(\\b\\|;\\|\\.\\)\\|begin\\b\\|repeat\\b\\|else\\b")
	(pascal-indent-line)))
  (newline)
  (pascal-indent-line)
  ;; Maybe we should set some endcomments
  (if pascal-auto-endcomments
      (pascal-set-auto-comments))
  ;; Check if we shall indent inside comment
  (let ((setstar nil))
    (save-excursion
      (forward-line -1)
      (skip-chars-forward " \t")
      (cond ((looking-at "\\*[ \t]*)")
	     ;; Delete region between `*' and `)' if there is only whitespaces.
	     (forward-char 1)
	     (pascal-delete-whitespaces))
	    ((and (looking-at "(\\*\\|\\*[^)]")
		  (not (save-excursion
			 (search-forward "*)" (pascal-get-end-of-line) t))))
	     (setq setstar t))))
    ;; If last line was a star comment line then this one shall be too.
    (if setstar
	(progn
	  (insert "*")
	  (pascal-indent-command))
      (pascal-indent-line))))

(defun electric-pascal-semi ()
  "Insert ; character and correct this line's indention."
  (interactive)
  (insert last-command-char)
  (save-excursion
    (beginning-of-line)
    (pascal-indent-line))
  (if pascal-auto-newline
      (electric-pascal-terminate-line)))

(defun electric-pascal-dot ()
  "Insert a period and correct this line's indention."
  (interactive)
  (insert last-command-char)
  (save-excursion
    (beginning-of-line)
    (pascal-indent-line))
  (if pascal-auto-newline
      (electric-pascal-terminate-line)))

(defun electric-pascal-colon ()
  "Insert : and do all indentions except line indent on this line."
  (interactive)
  (insert last-command-char)
  ;; Do nothing of within string.
  (if (not (pascal-within-string))
      (progn
	(if (save-excursion
	      (backward-char 2)
	      (looking-at "[0-9]"))
	    (save-excursion
	      (beginning-of-line)
	      (pascal-indent-line)))
	(let ((pascal-tab-always-indent nil))
	  (pascal-indent-command)))))
  
(defun electric-pascal-equal ()
  "Insert = and do indention if within type declaration."
  (interactive)
  (insert last-command-char)
  (if (eq (nth 1 (pascal-calculate-indent t)) 'decl)
      (let ((pascal-tab-always-indent nil))
	(pascal-indent-command))))

(defun electric-pascal-tab ()
  "Function called when tab is pressed."
  (interactive)
  ;; Do nothing if within a string.
  (if (not (pascal-within-string))
      ;; If pascal-tab-always-indent is set then indent the beginning of
      ;; the line.
      (progn
	(if pascal-tab-always-indent
	    (save-excursion
	      (beginning-of-line)
	      (pascal-indent-line)))
	(pascal-indent-command))))

;;;
;;; Interactive functions
;;;
(defun pascal-insert-block ()
  "Insert begin ... end; block in the code with right indents."
  (interactive)
  (pascal-indent-line)
  (insert "begin")
  (electric-pascal-terminate-line)
  (save-excursion
    (electric-pascal-terminate-line)
    (insert "end;")
    (beginning-of-line)
    (pascal-indent-line)))

(defun pascal-star-comment ()
  "Insert star comment in the code."
  (interactive)
  (pascal-indent-line)
  (insert "(*")
  (electric-pascal-terminate-line)
  (save-excursion
    (electric-pascal-terminate-line)
    (pascal-delete-whitespaces)
    (insert ")")))

(defun pascal-mark-function ()
  "Mark the current pascal function (or procedure).
Put the mark at the end of the function, and point at the beginning."
  (interactive)
  (push-mark (point))
  (pascal-forward-to-end-of-function)
  (push-mark (point))
  (pascal-backward-to-beginning-of-function)
  (zmacs-activate-region))

(defun pascal-comment-area (start end)
  "Put the current region in a comment.
The comments that are in this area are
be changed so that `*)' becomes `!(*' and `}' becomes `!{'. These will
however be turned back to normal when the area is uncommented by pressing
\\[pascal-uncomment-area].
The commented area starts with: `{---\\/---EXCLUDED---\\/---' , and ends with:
` ---/\\---EXCLUDED---/\\---}'. If these texts are changed, uncomment-area
will not be able to recognize them."
  (interactive "r")
  (save-excursion
    ;; Insert start and endcomments
    (goto-char end)
    (if (and (save-excursion (skip-chars-forward " \t") (eolp))
	     (not (save-excursion (skip-chars-backward " \t") (bolp))))
	(forward-line 1)
      (beginning-of-line))
    (insert " ---/\\---EXCLUDED---/\\---}")
    (setq end (point))
    (newline)
    (goto-char start)
    (beginning-of-line)
    (insert "{---\\/---EXCLUDED---\\/--- ")
    (newline)
    ;; Replace end-comments within commented area
    (goto-char end)
    (save-excursion
      (while (re-search-backward "\\*)" start t)
	(replace-match "!(*" t t)))
    (save-excursion
      (while (re-search-backward "}" start t)
	(replace-match "!{" t t)))))

(defun pascal-uncomment-area ()
  "Uncomment a commented area.
Change all deformed comments in this area back to normal.
This function does nothing if the pointer is not in a commented
area.  See also `pascal-comment-area'."
  (interactive)
  (save-excursion
    (let ((start (point))
	  (end (point)))
      ;; Find the boundaries of the comment
      (save-excursion
	(setq start (progn (search-backward "{---\\/---EXCLUDED---\\/--" nil t)
			   (point)))
	(setq end (progn (search-forward "---/\\---EXCLUDED---/\\---}" nil t)
			 (point))))
      ;; Check if we're really inside a comment
      (if (or (equal start (point)) (<= end (point)))
	  (message "Not standing within commented area.")
	(progn
	  ;; Remove endcomment
	  (goto-char end)
	  (beginning-of-line)
	  (let ((pos (point)))
	    (end-of-line)
	    (delete-region pos (1+ (point))))
	  ;; Change comments back to normal
	  (save-excursion
	    (while (re-search-backward "!{" start t)
	      (replace-match "}" t t)))
	  (save-excursion
	    (while (re-search-backward "!(\\*" start t)
	      (replace-match "*)" t t)))
	  ;; Remove startcomment
	  (goto-char start)
	  (beginning-of-line)
	  (let ((pos (point)))
	    (end-of-line)
	    (delete-region pos (1+ (point)))))))))

(defun pascal-backward-to-beginning-of-function ()
  "Move backwards to the beginning of this function or procedure."
  (interactive)
  ;; Check if this is a 
  (if (save-excursion
	(re-search-backward "\\<end" nil t)
	(looking-at "end\\."))
      (beginning-of-buffer)
    (let ((nest-depth 0) (nest-max 0)
	  (nest-noexit 1))
      (beginning-of-line)
      ;; First we find the max depth of the nesting
      (save-excursion
	(while (not (or (bobp) (looking-at "function\\b\\|procedure\\b")))
	  (backward-sexp 1)
	  (cond ((looking-at "begin\\b\\|\\case\\b\\|record\\b")
		 (setq nest-depth (1+ nest-depth)))
		((looking-at "end\\(\\b\\|;\\|\\.\\)")
		 (setq nest-depth (1- nest-depth))))
	  (setq nest-max (max nest-depth nest-max))))
      ;; Then we can start searching
      (setq nest-depth 0)
      (while (not (or (bobp) (and (looking-at "function\\b\\|procedure\\b")
				  (zerop nest-noexit))))
	(backward-sexp 1)
	(cond ((looking-at "begin\\b\\|\\case\\b\\|record\\b")
	       (setq nest-depth (1+ nest-depth)))
	      ((looking-at "end\\(\\b\\|;\\|\\.\\)")
	       (if (equal nest-depth nest-max)
		   (setq nest-noexit (1+ nest-noexit)))
	       (setq nest-depth (1- nest-depth)))
	      ((looking-at "function\\b\\|procedure\\b")
	       (setq nest-noexit (1- nest-noexit))))))))

(defun pascal-forward-to-end-of-function ()
  "Moves the point to the end of the function."
  (interactive)
  (if (not (looking-at "function\\b\\|procedure\\b"))
      (pascal-backward-to-beginning-of-function))
  (if (bobp)
      (end-of-buffer)
    (progn
      (let ((nest-depth 0)
	    (func-depth 1))
	(while (not (or (and (zerop nest-depth) (zerop func-depth)) (eobp)))
	  (forward-sexp 2)
	  (if (not (eobp))
	      (progn
		(backward-sexp 1) ; Move to the beginning of the next sexp
		(cond ((looking-at "begin\\b\\|case\\b\\|record\\b")
		       (setq nest-depth (1+ nest-depth)))
		      ((looking-at "end\\(\\b\\|;\\|\\.\\)")
		       (setq nest-depth (1- nest-depth))
		       (if (zerop nest-depth)
			   (setq func-depth (1- func-depth))))
		      ((looking-at "function\\b\\|procedure\\b")
		       (setq func-depth (1+ func-depth)))))))
	(end-of-line)))))

(defun pascal-downcase-keywords ()
  "Makes all Pascal keywords in the buffer lowercase."
  (interactive)
  (pascal-change-keywords 'downcase-word))

(defun pascal-upcase-keywords ()
  "Makes all Pascal keywords in the buffer uppercase."
  (interactive)
  (pascal-change-keywords 'upcase-word))

(defun pascal-capitalize-keywords ()
  "Makes all Pascal keywords in the buffer uppercase."
  (interactive)
  (pascal-change-keywords 'capitalize-word))

(defun pascal-change-keywords (change-word)
  "Change the keywords according to argument."
  (save-excursion
    (beginning-of-buffer)
    (while (re-search-forward (mapconcat
			       'downcase pascal-keywords "\\>\\|\\<") nil t)
      (funcall change-word -1))))

;;;
;;; Other functions
;;;
(defun pascal-delete-whitespaces ()
  "Deletes the whitespaces around the current point."
  (interactive)
  (let ((pos (progn (skip-chars-backward " \t") (point))))
    (skip-chars-forward " \t")
    (delete-region pos (point))))

(defun pascal-get-beg-of-line ()
  (save-excursion
    (beginning-of-line)
    (point)))

(defun pascal-get-end-of-line ()
  (save-excursion
    (end-of-line)
    (point)))
  
(defun pascal-within-string ()
  "Return t if within string; nil otherwise."
  (and (save-excursion (search-backward "\"" (pascal-get-beg-of-line) t))
       (save-excursion (not (search-backward "\"" (pascal-get-beg-of-line) t 2)))))

(defun pascal-check-if-within-comment ()
  "If within a comment, return the correct indent.  Return nil otherwise."
  (let ((comstart (point))
	(comend (point)))
    (save-excursion
      (if (re-search-backward "(\\*\\|{" nil t)
	  (setq comstart (point))
	(setq comstart 0)))
    (save-excursion
      (if (re-search-backward "\\*)\\|}" nil t)
	  (setq comend (point))
	(setq comend 0)))
    (if (< comend comstart)
	(save-excursion
	  (goto-char comstart)
	  ;; Add 1 to indent if this is a starcomment
	  (if (looking-at "(\\*")
	      (1+ (current-column))
	    (current-column)))
      nil)))

(defun pascal-set-auto-comments ()
  "Put { case } or { FUNNAME } on this line if appropriate after `end'."
  (save-excursion
    (forward-line -1)
    (skip-chars-forward " \t")
    (if (and (looking-at "end\\(\>\\|;\\)")
	     (not (save-excursion
		    (end-of-line)
		    (search-backward "}" (pascal-get-beg-of-line) t))))
	(progn
	  (if (eq (nth 1 (pascal-calculate-indent)) 'case)
	      ;; This is a case block
	      (progn
		(end-of-line)
		(pascal-delete-whitespaces)
		(insert " { case }"))
	    (let ((nest 1))
	      ;; Check if this is the end of a function
	      (save-excursion
		(while (not (or (looking-at "function\\b\\|\\procedure\\b")
				(bobp)))
		  (backward-sexp 1)
		  (cond ((looking-at "begin\\b\\|case\\b")
			 (setq nest (1- nest)))
			((looking-at "end\\(\\b\\|;\\|\\.\\)")
			 (setq nest (1+ nest)))))
		(if (bobp)
		    (setq nest 1)))
	      (if (zerop nest)
		  (let ((last-command nil))
		    ;; Find the function name and put it in braces
		    (save-excursion
		      (pascal-backward-to-beginning-of-function)
		      (skip-chars-forward "^ \t")
		      (skip-chars-forward " \t")
		      (copy-region-as-kill (point)
					   (save-excursion
					     (skip-chars-forward "a-zA-Z0-9_")
					     (point))))
		    (end-of-line)
		    (pascal-delete-whitespaces)
		    (insert " { ")
		    ;; We've filled up the kill ring, but hey, who cares?
		    (yank) (rotate-yank-pointer 1)
		    (insert " }")))))))))

;;;
;;; Indent functions and calculation of indent
;;;	
(defun pascal-indent-command ()
  "Indent current line as Pascal code and/or indent within line."
  ;; Call pascal-indent-line. This does nothing if we're not at the
  ;; beginning of the line.
  (pascal-indent-line)
  (let ((indent (pascal-calculate-indent t))
	(pos 0))
    (save-excursion
      (cond ((or (eq (nth 1 indent) 'case)
		 (eq (nth 1 indent) 'record))
	     ;; Indent for case and record blocks
	     (beginning-of-line)
	     (if (search-forward ":" (pascal-get-end-of-line) t)
		 (progn
		   ;; Indent before colon
		   (backward-char 1)
		   (pascal-delete-whitespaces)
		   (indent-to (max (pascal-find-leading-case-colon)
				   (1+ (current-column))))
		   ;; Indent after colon
		   (forward-char 1)
		   (pascal-delete-whitespaces)
		   (indent-to (1+ (current-column))))
	       ;; Indent if there is no colon
	       (progn
		 (beginning-of-line)
		 (skip-chars-forward " \t")
		 (if (not (eolp))
		     (progn
		       (skip-chars-forward "0-9a-zA-Z\"\'_;")
		       (pascal-delete-whitespaces)
		       (indent-to (max (pascal-find-leading-case-colon)
				       (1+ (current-column)))))))))
	    ((eq (nth 1 indent) 'decl)
	     ;; Indent for declarations
	     (let ((posii (pascal-get-beg-of-line)))
	       (re-search-backward "\\<\\(var\\|type\\|const\\|label\\)\\>"
				   nil t)
	       (cond ((looking-at "var\\b")
		      (pascal-declindent-middle-of-line
		       ":" posii pascal-vardecl-indent))
		     ((looking-at "type\\b\\|const\\b")
		      (pascal-declindent-middle-of-line
		       "=" posii pascal-typedecl-indent)))))
	    ((eq (nth 1 indent) 'function)
	     ;; Indent for parameterlist
	     ;; Done twice in case something has changed
	     (pascal-indent-parameter-list)
	     (pascal-indent-parameter-list))))	     
    ;; Go to the end of a line if rest of line contains only whitespaces
    (if (save-excursion (skip-chars-forward " \t") (eolp))
	(end-of-line))))

(defun pascal-indent-line ()
  "Indent current line as Pascal code."
  (let ((indent (list 0 nil))
	(comindent 0)
	beg (point))
    (save-excursion
      (beginning-of-line)
      (setq indent (pascal-calculate-indent)))
    ;; If we are inside a comment, do special indent.
    (if (setq comindent (pascal-check-if-within-comment))
	(pascal-indent-within-comment comindent)
      ;; Skip the rest if we're not standing on the beginning of a line.
      (if (save-excursion (skip-chars-backward " \t") (bolp))
	  (progn
	    (beginning-of-line)
	    (pascal-delete-whitespaces)
	    ;; When to skip the ekstra indent:
	    ;; If we are standing at end or until.
	    ;; If we are in an if statement and standing at else,
	    ;;  begin or repeat
	    ;; If we are in a with, while or for statement and standing
	    ;;  at begin or end.
	    (cond ((or (or (looking-at "end\\b\\|until\\b")
			   (not (nth 1 indent)))
		       (and (eq (nth 1 indent) 'if)
			    (looking-at "begin\\b\\|\\repeat\\b\\|else\\b"))
		       (and (eq (nth 1 indent) 'whilewith)
			    (looking-at "begin\\b\\|\\repeat\\b")))
		   (indent-to (car indent)))
		  ;; Continued expression
		  ((eq (nth 1 indent) 'contexp)
		   (indent-to (+ (car indent) pascal-continued-expr)))
		  ;; If this is a part of a case or record block,
		  ;; then modify the indent level.
		  ((or (eq (nth 1 indent) 'case)
		       (eq (nth 1 indent) 'record))
		   (indent-to (+ (car indent) pascal-indent-level
				 pascal-label-offset)))
		  ;; If this is a label - don't indent.
		  ((looking-at "[0-9]*:")
		   (skip-chars-forward "0-9:")
		   (pascal-delete-whitespaces)
		   (indent-to (+ (car indent) pascal-indent-level)))
		  ;; If this is insde a parameter list, do special indent
		  ((eq (nth 1 indent) 'function)
		   (pascal-indent-parameter-list))
		  ;; All other indents are set normaly.
		  (t
		   (indent-to (+ (car indent) pascal-indent-level)))))))))
    
(defun pascal-calculate-indent (&optional arg)
  "Search backward in code to find the right indent level.
Return a list containing:
1. Indent level
2. The indent keyword (begin, case etc.), or nil if backtracking failed.
If arg is non-nil, we do not search for continued expressions."
  (let ((pascal-nest-depth 1)
	(oldpos (save-excursion (forward-line -1) (end-of-line) (point)))
	(samepos (point)) (if-is-set t)
	(return-struct (list 0 nil)) (pos 0)
	(contexpr nil) (after-contexpr (not arg))
	(case-fold-search t))
    (save-excursion
      (while (and (not (zerop pascal-nest-depth))
		  (not (bobp)))
	(progn
	  (backward-sexp 1)
	  (if (save-excursion
		(setq pos (point))
		(end-of-line)
		(search-backward ";" pos t))
	      (setq if-is-set nil
		    after-contexpr nil))
	  (if (looking-at "then\\b\\|end\\b\\|else\\b\\|do\\b")
	      (setq after-contexpr nil))

	  (cond ((looking-at "begin\\b\\|case\\b\\|record\\b\\|repeat\\b")
		 (setq pascal-nest-depth (1- pascal-nest-depth)))
		;;
		;; END | UNTIL
		((looking-at "end\\(\\b\\|;\\|\\.\\)\\|until\\b")
		 (setq if-is-set nil)
		 (if after-contexpr
		     (setq pascal-nest-depth 0
			   contexpr t)
		   (setq pascal-nest-depth (1+ pascal-nest-depth))))
		;;
		;; IF | ELSE | WITH | WHILE | FOR
		;; LABEL |  CONST | TYPE | FUNCTION | PROCEDURE
		((or (and (looking-at "if\\b\\|else\\b\\|with\\b\\|while\\b\\|for\\b")
			  if-is-set)
		     (looking-at "label\\b\\|const\\b\\|type\\b\\|function\\b\\|procedure\\b"))
		 (setq pascal-nest-depth 0))
		;;
		;; VAR
		((looking-at "var\\b")
		 ;; A `var' can be in a declaration part or parameter part
		 (let ((stpos 0) (edpos 0))
		   (save-excursion
		     (if (not (re-search-backward
			       "\\<\\(function\\|procedure\\)\\>" nil t))
			 (beginning-of-buffer))
		     (setq stpos (save-excursion
				   (search-forward "(" nil t) (point)))
		     (setq edpos (save-excursion
				   (search-forward ")" nil t) (point))))
		   (cond ((or (= stpos edpos) (< samepos stpos)
			      (and (> (point) edpos) (> edpos stpos)))
			  ;; This is really a declaration block!!
			  nil)
			 ((and (>= samepos stpos) (or (< samepos edpos)
						      (> stpos edpos)))
			  ;; Hmm... part of a parameter
			  (re-search-backward
			   "\\<\\(function\\|procedure\\)\\>" nil t))
			 (t
			  ;; This is just after a parameter declaration
			  (forward-char 1)))
		   ;; We'll quit anyway
		   (setq pascal-nest-depth 0)))
		;;
		;; CONTINUED EXPRESSIONS
		(after-contexpr
		 (save-excursion
		   ;; First, we have to be at the begining of a line
		   (if (and (progn (skip-chars-backward " \t") (bolp))
			    ;; Blank lines don't count
			    (not (progn (skip-chars-forward " \t") (eolp)))
			    ;; But nonblank without ';' do
			    (not (search-forward ";" (pascal-get-end-of-line) t)))
		       (save-excursion
			 (forward-line -1)
			 (end-of-line)
			 (backward-sexp 1)
			 (if (or (looking-at "\\(do\\|then\\|of\\\|begin\\|repeat\\|else\\)\\>")
				 (progn
				   (skip-chars-forward "^; " (pascal-get-end-of-line))
				   (equal (char-to-string (following-char))
					  ";")))
			     (setq pascal-nest-depth 0))
			 (setq contexpr t)))))
		)))
      (cond (contexpr
	     (setq return-struct (list (pascal-lstart-col) 'contexp)))
	    ((looking-at "begin\\b")
	     (setq return-struct (list (pascal-lstart-col) 'begin)))
	    ((looking-at "else\\b")
	     (setq return-struct (list (save-excursion
					 (re-search-backward "if\\b" nil t)
					 (pascal-lstart-col)) 'if))
	     ;; Indent line in case this is a multiple if
	     (beginning-of-line)
	     (pascal-delete-whitespaces)
	     (indent-to (car return-struct)))
	    ((looking-at "if\\b")
	     (if (save-excursion
		   (narrow-to-region (pascal-get-beg-of-line) (point))
		   (backward-sexp 1)
		   (widen)
		   (looking-at "else\\b"))
		 ;; Indent line if this is a multiple if
		 (progn
		   (beginning-of-line)
		   (pascal-delete-whitespaces)
		   (indent-to (save-excursion
				(re-search-backward "if\\b" nil t)
				(pascal-lstart-col)))))
	     ;; This could be a continued expression
	     (if (and after-contexpr
		      (not (save-excursion (re-search-forward
					    "then\\b" (pascal-get-end-of-line) t))))
		 (setq return-struct (list (pascal-lstart-col) 'contexp))
	       (setq return-struct (list (pascal-lstart-col) 'if))))
	    ((looking-at "repeat\\b")
	     (setq return-struct (list (pascal-lstart-col) 'repeat)))
	    ((looking-at "case\\b")
	     (setq return-struct (list (current-column) 'case)))
	    ((looking-at "record\\b")
	     (setq return-struct (list (current-column) 'record)))
	    ((looking-at "while\\b\\|with\\b\\|for\\b")
	     ;; This could ba a continued expression
	     (if (and after-contexpr
		      (not (save-excursion (re-search-forward
					    "do\\b" (pascal-get-end-of-line) t))))
		 (setq return-struct (list (pascal-lstart-col) 'contexp))
	       (setq return-struct (list (current-column) 'whilewith))))
	    ((looking-at "procedure\\b\\|function\\b")
	     ;; Make sure that this is a function with parameters, and
	     ;; that we are actually standing inside the paranthesis.
	     (let ((spos (save-excursion
			   (search-forward "(" samepos t) (point)))
		   (epos (save-excursion
			   (search-forward ")" samepos t) (point))))
	       (if (and (>= samepos spos) (or (< samepos epos)
					      (> spos epos)))
		   (setq return-struct (list 0 'function))
		 (setq return-struct (list 0 nil)))))
	    ((looking-at "var\\b\\|label\\b\\|const\\b\\|type\\b")
	     ;; Are we really in the declaration part?(Check for blank lines)
	     (if (< oldpos (point))
		 (setq return-struct (list 0 'decl))
	       (if (save-excursion
		     (not (re-search-forward "^[ \t]*$" oldpos t)))
		   (setq return-struct (list 0 'decl))
		 (setq return-struct (list 0 nil)))))
	    (t
	     (setq return-struct (list 0 nil))))
      return-struct)))

(defun pascal-lstart-col ()
  "Return the column of the beginning of the first command on the line."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward ":0-9")
    (skip-chars-forward " \t")
    (current-column)))

(defun pascal-indent-parameter-list ()
  "Indent this line as part of a parameter list in a function."
  (let ((indents (pascal-get-highest-indents-in-parameterlist))
	(pos 0))
    (if (not (progn (beginning-of-line)
		    (search-forward "(" (pascal-get-end-of-line) t)))
	(progn (beginning-of-line)
  	       (skip-chars-forward " \t")))
    ;; Indent region in front of var
    (skip-chars-forward " \t")
    (pascal-delete-whitespaces)
    (indent-to (nth 0 indents))
    (if (looking-at "var\\b")
	(forward-char 3))
    ;; Indent parameternames
    (pascal-delete-whitespaces)
    (indent-to (nth 1 indents))
    (if (not (save-excursion (skip-chars-forward " \t") (eolp)))
	(progn
	  ;; Indent colon
	  (if (search-forward ":" (pascal-get-end-of-line) t)
	      (backward-char 1)
	    (end-of-line))
	  (pascal-delete-whitespaces)
	  (indent-to (nth 2 indents))
	  ;; Indent after colon
	  (if (equal (following-char) ?:)
	      (progn
		(forward-char 1)
		(pascal-delete-whitespaces)
		(indent-to (+ 2 (nth 2 indents)))))))))

;; Get the indents to use in a parameterlist.
;; Returns:
;; 1. Indent to the beginning of the line.
;; 2. Indent to the beginning of the parameter names.
;; 3. Indent to the right colon position."
(defun pascal-get-highest-indents-in-parameterlist ()
  (save-excursion
    (let ((start (progn
		   (re-search-backward
		    "\\<\\(function\\|procedure\\)\\>" nil t)
		   (search-forward "(")
		   (current-column)))
	  (arglength 0) (vardecl nil) (done nil))
      (while (not (or done (eobp)))
	(beginning-of-line)
	(if (save-excursion
	      (re-search-forward "\\<var\\>" (pascal-get-end-of-line) t))
	      (setq vardecl t))
	(if (not (re-search-forward ":" (pascal-get-end-of-line) t))
	    (setq done t))
	(skip-chars-backward ": \t")
	(setq arglength (max arglength (current-column)))
	(forward-line 1))
      (if vardecl
	  (list start (+ start 4) (1+ arglength))
	(list start start (1+ arglength))))))

(defun pascal-declindent-middle-of-line (declkey endpos defaultindent)
  "Indent declaration line."
  (let ((decindent 0))
    (if (search-forward declkey endpos t)
	(setq decindent (1- (current-column)))
      (setq decindent defaultindent))
    (goto-char endpos)
    (end-of-line)
    (if (save-excursion (search-backward declkey endpos t))
	(progn (search-backward declkey) (skip-chars-backward " \t"))
      (skip-chars-backward " \t"))
    (pascal-delete-whitespaces)
    (indent-to (max decindent (1+ (current-column))))
    ;; Indent after `declkey'
    (if (looking-at declkey)
	(progn
	  (forward-char 1)
	  (pascal-delete-whitespaces)
	  (indent-to (1+ (current-column)))))))
  
(defun pascal-indent-within-comment (indent)
  "Indent comments and/or indent text within comment."
  (progn
    ;; If we are at the beginning of the line, then we indent this line.
    (if (save-excursion (skip-chars-backward " \t") (bolp))
	(progn
	  (beginning-of-line)
	  (pascal-delete-whitespaces)
	  (indent-to indent))
      ;; Do nothing if we're not in a star comment.
      (if (save-excursion
	    (beginning-of-line)
	    (skip-chars-forward " \t")
	    (looking-at "\\*\\|(\\*"))
	  (save-excursion
	    (beginning-of-line)
	    (search-forward "*")
	    (pascal-delete-whitespaces)
	    (indent-to (+ (current-column) 2)))))))

(defun pascal-find-leading-case-colon ()
  "Return hpos of first colon after the case-of or record line.
If there's no such line, use the place where it ought to be."
  (let ((pos (save-excursion
	       (beginning-of-line)
	       (skip-chars-forward " \t")
	       (point))))
    (save-excursion
      (re-search-backward "\\<\\(case\\|record\\)\\>")
      (forward-line 1)
      (skip-chars-forward " \t")
      (if (not (eq pos (point)))
	  (progn
	    (search-forward ":" (pascal-get-end-of-line) t)
	    (1- (current-column)))
	(+ (current-column) pascal-case-offset)))))

(provide 'pascal)

;; pascal.el ends here.
