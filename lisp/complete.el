;; complete.el -- partial completion mechanism plus other goodies.

;; Copyright (C) 1990, 1991, 1992, 1993 Free Software Foundation, Inc.

;; Author: Dave Gillespie <daveg@synaptics.com>
;; Version: 2.02
;; Special thanks to Hallvard Furuseth for his many ideas and contributions.

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

;; Extended completion for the Emacs minibuffer.
;;
;; The basic idea is that the command name or other completable text is
;; divided into words and each word is completed separately, so that
;; "M-x p-b" expands to "M-x print-buffer".  If the entry is ambiguous
;; each word is completed as much as possible and then the cursor is
;; left at the first position where typing another letter will resolve
;; the ambiguity.
;;
;; Word separators for this purpose are hyphen, space, and period.
;; These would most likely occur in command names, Info menu items,
;; and file names, respectively.  But all word separators are treated
;; alike at all times.
;;
;; This completion package replaces the old-style completer's key
;; bindings for TAB, SPC, RET, and `?'.  The old completer is still
;; available on the Meta versions of those keys.  If you set
;; PC-meta-flag to nil, the old completion keys will be left alone
;; and the partial completer will use the Meta versions of the keys.


;; Usage:  Load this file.  Now, during completable minibuffer entry,
;;
;;     TAB    means to do a partial completion;
;;     SPC    means to do a partial complete-word;
;;     RET    means to do a partial complete-and-exit;
;;     ?      means to do a partial completion-help.
;;
;; If you set PC-meta-flag to nil, then TAB, SPC, RET, and ? perform
;; original Emacs completions, and M-TAB etc. do partial completion.
;; To do this, put the command,
;;
;;       (setq PC-meta-flag nil)
;;
;; in your .emacs file.  To load partial completion automatically, put
;;
;;       (load "complete")
;;
;; in your .emacs file, too.  Things will be faster if you byte-compile
;; this file when you install it.
;;
;; As an extra feature, in cases where RET would not normally
;; complete (such as `C-x b'), the M-RET key will always do a partial
;; complete-and-exit.  Thus `C-x b f.c RET' will select or create a
;; buffer called "f.c", but `C-x b f.c M-RET' will select the existing
;; buffer whose name matches that pattern (perhaps "filing.c").
;; (PC-meta-flag does not affect this behavior; M-RET used to be
;; undefined in this situation.)
;;
;; The regular M-TAB (lisp-complete-symbol) command also supports
;; partial completion in this package.

;; This package also contains a wildcard feature for C-x C-f (find-file).
;; For example, `C-x C-f *.c RET' loads all .c files at once, exactly
;; as if you had typed C-x C-f separately for each file.  Completion
;; is supported in connection with wildcards.  Currently only the `*'
;; wildcard character works.

;; File name completion does not do partial completion of directories
;; on the path, e.g., "/u/b/f" will not complete to "/usr/bin/foo",
;; but you can put *'s in the path to accomplish this:  "/u*/b*/f".
;; Stars are required for performance reasons.

;; In addition, this package includes a feature for accessing include
;; files.  For example, `C-x C-f <sys/time.h> RET' reads the file
;; /usr/include/sys/time.h.  The variable PC-include-file-path is a
;; list of directories in which to search for include files.  Completion
;; is supported in include file names.


;;; Code:

(defvar PC-meta-flag t
  "*If nil, TAB does normal Emacs completion and M-TAB does Partial Completion.
If t, TAB does Partial Completion and M-TAB does normal completion.")


(defvar PC-word-delimiters "-_. "
  "*A string of characters which are to be treated as word delimiters
by the Partial Completion system.

Some arcane rules:  If `]' is in this string it must come first.
If `^' is in this string it must NOT come first.  If `-' is in this
string, it must come first or right after `]'.  In other words, if
S is this string, then `[S]' must be a legal Emacs regular expression
\(not containing character ranges like `a-z').")


(defvar PC-first-char 'x
  "*If t, first character of a string to be completed is always taken literally.
If nil, word delimiters are handled even if they appear as first character.
This controls whether \".e\" matches \".e*\" (t) or \"*.e*\" (nil).
If neither nil nor t, first char is literal only for filename completion.")


(defvar PC-include-file-path '("/usr/include")
  "*List of directories in which to look for include files.
If this is nil, uses the colon-separated path in $INCPATH instead.")


(defvar PC-disable-wildcards nil
  "Set this to non-nil to disable wildcard support in \\[find-file].")

(defvar PC-disable-includes nil
  "Set this to non-nil to disable include-file support in \\[find-file].")


(defvar PC-default-bindings t
  "Set this to nil to suppress the default partial completion key bindings.")

(if PC-default-bindings (progn
(define-key minibuffer-local-completion-map "\t" 'PC-complete)
(define-key minibuffer-local-completion-map " "  'PC-complete-word)
(define-key minibuffer-local-completion-map "?"  'PC-completion-help)

(define-key minibuffer-local-completion-map "\e\t" 'PC-complete)
(define-key minibuffer-local-completion-map "\e "  'PC-complete-word)
(define-key minibuffer-local-completion-map "\e\r" 'PC-force-complete-and-exit)
(define-key minibuffer-local-completion-map "\e\n" 'PC-force-complete-and-exit)
(define-key minibuffer-local-completion-map "\e?"  'PC-completion-help)

(define-key minibuffer-local-must-match-map "\t" 'PC-complete)
(define-key minibuffer-local-must-match-map " "  'PC-complete-word)
(define-key minibuffer-local-must-match-map "\r" 'PC-complete-and-exit)
(define-key minibuffer-local-must-match-map "\n" 'PC-complete-and-exit)
(define-key minibuffer-local-must-match-map "?"  'PC-completion-help)

(define-key minibuffer-local-must-match-map "\e\t" 'PC-complete)
(define-key minibuffer-local-must-match-map "\e "  'PC-complete-word)
(define-key minibuffer-local-must-match-map "\e\r" 'PC-complete-and-exit)
(define-key minibuffer-local-must-match-map "\e\n" 'PC-complete-and-exit)
(define-key minibuffer-local-must-match-map "\e?"  'PC-completion-help)

(define-key global-map "\e\t" 'PC-lisp-complete-symbol)
))


(defun PC-complete ()
  "Like minibuffer-complete, but allows \"b--di\"-style abbreviations.
For example, \"M-x b--di\" would match `byte-recompile-directory', or any
name which consists of three or more words, the first beginning with \"b\"
and the third beginning with \"di\".

The pattern \"b--d\" is ambiguous for `byte-recompile-directory' and
`beginning-of-defun', so this would produce a list of completions
just like when normal Emacs completions are ambiguous.

Word-delimiters for the purposes of Partial Completion are \"-\", \"_\",
\".\", and SPC."
  (interactive)
  (if (PC-was-meta-key)
      (minibuffer-complete)
    (PC-do-completion nil)))


(defun PC-complete-word ()
  "Like `minibuffer-complete-word', but allows \"b--di\"-style abbreviations.
See `PC-complete' for details.
This can be bound to other keys, like `-' and `.', if you wish."
  (interactive)
  (if (eq (PC-was-meta-key) PC-meta-flag)
      (if (eq last-command-char ? )
	  (minibuffer-complete-word)
	(self-insert-command 1))
    (self-insert-command 1)
    (if (eobp)
	(PC-do-completion 'word))))


(defun PC-complete-space ()
  "Like `minibuffer-complete-word', but allows \"b--di\"-style abbreviations.
See `PC-complete' for details.
This is suitable for binding to other keys which should act just like SPC."
  (interactive)
  (if (eq (PC-was-meta-key) PC-meta-flag)
      (minibuffer-complete-word)
    (insert " ")
    (if (eobp)
	(PC-do-completion 'word))))


(defun PC-complete-and-exit ()
  "Like `minibuffer-complete-and-exit', but allows \"b--di\"-style abbreviations.
See `PC-complete' for details."
  (interactive)
  (if (eq (PC-was-meta-key) PC-meta-flag)
      (minibuffer-complete-and-exit)
    (PC-do-complete-and-exit)))

(defun PC-force-complete-and-exit ()
  "Like `minibuffer-complete-and-exit', but allows \"b--di\"-style abbreviations.
See `PC-complete' for details."
  (interactive)
  (let ((minibuffer-completion-confirm nil))
    (PC-do-complete-and-exit)))

(defun PC-do-complete-and-exit ()
  (if (= (buffer-size) 0)  ; Duplicate the "bug" that Info-menu relies on...
      (exit-minibuffer)
    (let ((flag (PC-do-completion 'exit)))
      (and flag
	   (if (or (eq flag 'complete)
		   (not minibuffer-completion-confirm))
	       (exit-minibuffer)
	     (PC-temp-minibuffer-message " [Confirm]"))))))


(defun PC-completion-help ()
  "Like `minibuffer-completion-help', but allows \"b--di\"-style abbreviations.
See `PC-complete' for details."
  (interactive)
  (if (eq (PC-was-meta-key) PC-meta-flag)
      (minibuffer-completion-help)
    (PC-do-completion 'help)))

(defun PC-was-meta-key ()
  (or (/= (length (this-command-keys)) 1)
      (let ((key (aref (this-command-keys) 0)))
	(if (integerp key)
	    (>= key 128)
	  (not (null (memq 'meta (event-modifiers key))))))))


(defvar PC-ignored-extensions 'empty-cache)
(defvar PC-delims 'empty-cache)
(defvar PC-ignored-regexp nil)
(defvar PC-word-failed-flag nil)
(defvar PC-delim-regex nil)
(defvar PC-ndelims-regex nil)
(defvar PC-delims-list nil)

(defun PC-do-completion (&optional mode beg end)
  (or beg (setq beg (point-min)))
  (or end (setq end (point-max)))
  (let* ((table minibuffer-completion-table)
	 (pred minibuffer-completion-predicate)
	 (filename (memq table '(read-file-name-internal
				 read-directory-name-internal)))
	 (dirname nil)
	 dirlength
	 (str (buffer-substring beg end))
	 (incname (and filename (string-match "<\\([^\"<>]*\\)>?$" str)))
	 (ambig nil)
	 basestr
	 regex
	 p offset
	 (poss nil)
	 helpposs
	 (case-fold-search completion-ignore-case))

    ;; Check if buffer contents can already be considered complete
    (if (and (eq mode 'exit)
	     (PC-is-complete-p str table pred))
	'complete

      ;; Record how many characters at the beginning are not included
      ;; in completion.
      (setq dirlength
	    (if filename
		(length (file-name-directory str))
	      0))

      ;; Do substitutions in directory names
      (and filename
	   (not (equal str (setq p (substitute-in-file-name str))))
	   (progn
	     (delete-region beg end)
	     (insert p)
	     (setq str p end (+ beg (length str)))))

      ;; Prepare various delimiter strings
      (or (equal PC-word-delimiters PC-delims)
	  (setq PC-delims PC-word-delimiters
		PC-delim-regex (concat "[" PC-delims "]")
		PC-ndelims-regex (concat "[^" PC-delims "]*")
		PC-delims-list (append PC-delims nil)))

      ;; Look for wildcard expansions in directory name
      (and filename
	   (string-match "\\*.*/" str)
	   (let ((pat str)
		 files)
	     (setq p (1+ (string-match "/[^/]*\\'" pat)))
	     (while (setq p (string-match PC-delim-regex pat p))
	       (setq pat (concat (substring pat 0 p)
				 "*"
				 (substring pat p))
		     p (+ p 2)))
	     (setq files (PC-expand-many-files (concat pat "*")))
	     (if files
		 (let ((dir (file-name-directory (car files)))
		       (p files))
		   (while (and (setq p (cdr p))
			       (equal dir (file-name-directory (car p)))))
		   (if p
		       (setq filename nil table nil pred nil
			     ambig t)
		     (delete-region beg end)
		     (setq str (concat dir (file-name-nondirectory str)))
		     (insert str)
		     (setq end (+ beg (length str)))))
	       (setq filename nil table nil pred nil))))

      ;; Strip directory name if appropriate
      (if filename
	  (if incname
	      (setq basestr (substring str incname)
		    dirname (substring str 0 incname))
	    (setq basestr (file-name-nondirectory str)
		  dirname (file-name-directory str)))
	(setq basestr str))

      ;; Convert search pattern to a standard regular expression
      (setq regex (regexp-quote basestr)
	    offset (if (and (> (length regex) 0)
			    (not (eq (aref basestr 0) ?\*))
			    (or (eq PC-first-char t)
				(and PC-first-char filename))) 1 0)
	    p offset)
      (while (setq p (string-match PC-delim-regex regex p))
	(if (eq (aref regex p) ? )
	    (setq regex (concat (substring regex 0 p)
				PC-ndelims-regex
				PC-delim-regex
				(substring regex (1+ p)))
		  p (+ p (length PC-ndelims-regex) (length PC-delim-regex)))
	  (let ((bump (if (memq (aref regex p)
				'(?$ ?^ ?\. ?* ?+ ?? ?[ ?] ?\\))
			  -1 0)))
	    (setq regex (concat (substring regex 0 (+ p bump))
				PC-ndelims-regex
				(substring regex (+ p bump)))
		  p (+ p (length PC-ndelims-regex) 1)))))
      (setq p 0)
      (if filename
	  (while (setq p (string-match "\\\\\\*" regex p))
	    (setq regex (concat (substring regex 0 p)
				"[^/]*"
				(substring regex (+ p 2))))))
      ;;(setq the-regex regex)
      (setq regex (concat "\\`" regex))

      ;; Find an initial list of possible completions
      (if (not (setq p (string-match (concat PC-delim-regex
					     (if filename "\\|\\*" ""))
				     str
				     (+ (length dirname) offset))))

	  ;; Minibuffer contains no hyphens -- simple case!
	  (setq poss (all-completions str
				      table
				      pred))

	;; Use all-completions to do an initial cull.  This is a big win,
	;; since all-completions is written in C!
	(let ((compl (all-completions (substring str 0 p)
				      table
				      pred)))
	  (setq p compl)
	  (while p
	    (and (string-match regex (car p))
		 (setq poss (cons (car p) poss)))
	    (setq p (cdr p)))))

      ;; Now we have a list of possible completions
      (cond

       ;; No valid completions found
       ((null poss)
	(if (and (eq mode 'word)
		 (not PC-word-failed-flag))
	    (let ((PC-word-failed-flag t))
	      (delete-backward-char 1)
	      (PC-do-completion 'word))
	  (beep)
	  (PC-temp-minibuffer-message (if ambig
					  " [Ambiguous dir name]"
					(if (eq mode 'help)
					    " [No completions]"
					  " [No match]")))
	  nil))

       ;; More than one valid completion found
       ((or (cdr (setq helpposs poss))
	    (memq mode '(help word)))

	;; Handle completion-ignored-extensions
	(and filename
	     (not (eq mode 'help))
	     (let ((p2 poss))

	       ;; Build a regular expression representing the extensions list
	       (or (equal completion-ignored-extensions PC-ignored-extensions)
		   (setq PC-ignored-regexp
			 (concat "\\("
				 (mapconcat
				  'regexp-quote
				  (setq PC-ignored-extensions
					completion-ignored-extensions)
				  "\\|")
				 "\\)\\'")))

	       ;; Check if there are any without an ignored extension
	       (setq p nil)
	       (while p2
		 (or (string-match PC-ignored-regexp (car p2))
		     (setq p (cons (car p2) p)))
		 (setq p2 (cdr p2)))

	       ;; If there are "good" names, use them
	       (and p (setq poss p))))

	;; Is the actual string one of the possible completions?
	(setq p (and (not (eq mode 'help)) poss))
	(while (and p
		    (not (equal (car p) basestr)))
	  (setq p (cdr p)))
	(and p (null mode)
	     (PC-temp-minibuffer-message " [Complete, but not unique]"))
	(if (and p
		 (not (and (null mode)
			   (eq this-command last-command))))
	    t

	  ;; If ambiguous, try for a partial completion
	  (let ((improved nil)
		prefix
		(pt nil)
		(skip "\\`"))

	    ;; Check if next few letters are the same in all cases
	    (if (and (not (eq mode 'help))
		     (setq prefix (try-completion "" (mapcar 'list poss))))
		(let ((first t) i)
		  (if (eq mode 'word)
		      (setq prefix (PC-chop-word prefix basestr)))
		  (goto-char (+ beg (length dirname)))
		  (while (and (progn
				(setq i 0)
				(while (< i (length prefix))
				  (if (and (< (point) end)
					   (eq (aref prefix i)
					       (following-char)))
				      (forward-char 1)
				    (if (and (< (point) end)
					     (or (and (looking-at " ")
						      (memq (aref prefix i)
							    PC-delims-list))
						 (eq (downcase (aref prefix i))
						     (downcase
						      (following-char)))))
					(progn
					  (delete-char 1)
					  (setq end (1- end)))
				      (and filename (looking-at "\\*")
					   (progn
					     (delete-char 1)
					     (setq end (1- end))))
				      (setq improved t))
				    ;; Use format to discard text properties.
				    (insert (format "%s" (substring prefix i (1+ i))))
				    (setq end (1+ end)))
				  (setq i (1+ i)))
				(or pt (equal (point) beg)
				    (setq pt (point)))
				(looking-at PC-delim-regex))
			      (setq skip (concat skip
						 (regexp-quote prefix)
						 PC-ndelims-regex)
				    prefix (try-completion
					    ""
					    (mapcar
					     (function
					      (lambda (x)
						(list
						 (and (string-match skip x)
						      (substring
						       x
						       (match-end 0))))))
					     poss)))
			      (or (> i 0) (> (length prefix) 0))
			      (or (not (eq mode 'word))
				  (and first (> (length prefix) 0)
				       (setq first nil
					     prefix (substring prefix 0 1))))))
		  (goto-char (if (eq mode 'word) end
			       (or pt beg)))))

	    (if (and (eq mode 'word)
		     (not PC-word-failed-flag))

		(if improved

		    ;; We changed it... would it be complete without the space?
		    (if (PC-is-complete-p (buffer-substring 1 (1- end))
					  table pred)
			(delete-region (1- end) end)))

	      (if improved

		  ;; We changed it... enough to be complete?
		  (and (eq mode 'exit)
		       (PC-is-complete-p (buffer-string) table pred))

		;; If totally ambiguous, display a list of completions
		(if (or completion-auto-help
			(eq mode 'help))
		    (with-output-to-temp-buffer "*Completions*"
		      (display-completion-list (sort helpposs 'string-lessp))
		      (save-excursion
			(set-buffer standard-output)
			;; Record which part of the buffer we are completing
			;; so that choosing a completion from the list
			;; knows how much old text to replace.
			(setq completion-base-size dirlength)))
		  (PC-temp-minibuffer-message " [Next char not unique]"))
		nil)))))

       ;; Only one possible completion
       (t
	(if (equal basestr (car poss))
	    (if (null mode)
		(PC-temp-minibuffer-message " [Sole completion]"))
	  (delete-region beg end)
	  (insert (format "%s"
			  (if filename
			      (substitute-in-file-name (concat dirname (car poss)))
			    (car poss)))))
	t)))))


(defun PC-is-complete-p (str table pred)
  (let ((res (if (listp table)
		 (assoc str table)
	       (if (vectorp table)
		   (or (equal str "nil")   ; heh, heh, heh
		       (intern-soft str table))
		 (funcall table str pred 'lambda)))))
    (and res
	 (or (not pred)
	     (and (not (listp table)) (not (vectorp table)))
	     (funcall pred res))
	 res)))

(defun PC-chop-word (new old)
  (let ((i -1)
	(j -1))
    (while (and (setq i (string-match PC-delim-regex old (1+ i)))
		(setq j (string-match PC-delim-regex new (1+ j)))))
    (if (and j
	     (or (not PC-word-failed-flag)
		 (setq j (string-match PC-delim-regex new (1+ j)))))
	(substring new 0 (1+ j))
      new)))

(defvar PC-not-minibuffer nil)

(defun PC-temp-minibuffer-message (m)
  "A Lisp version of `temp_minibuffer_message' from minibuf.c."
  (if PC-not-minibuffer
      (progn
	(message m)
	(sit-for 2)
	(message ""))
    (if (fboundp 'temp-minibuffer-message)
	(temp-minibuffer-message m)
      (let ((savemax (point-max)))
	(save-excursion
	  (goto-char (point-max))
	  (insert m))
	(let ((inhibit-quit t))
	  (sit-for 2)
	  (delete-region savemax (point-max))
	  (if quit-flag
	      (setq quit-flag nil
		    unread-command-char 7)))))))


(defun PC-lisp-complete-symbol ()
  "Perform completion on Lisp symbol preceding point.
That symbol is compared against the symbols that exist
and any additional characters determined by what is there
are inserted.
If the symbol starts just after an open-parenthesis,
only symbols with function definitions are considered.
Otherwise, all symbols with function definitions, values
or properties are considered."
  (interactive)
  (let* ((end (point))
	 (buffer-syntax (syntax-table))
	 (beg (unwind-protect
		  (save-excursion
		    (if lisp-mode-syntax-table
			(set-syntax-table lisp-mode-syntax-table))
		    (backward-sexp 1)
		    (while (= (char-syntax (following-char)) ?\')
		      (forward-char 1))
		    (point))
		(set-syntax-table buffer-syntax)))
	 (minibuffer-completion-table obarray)
	 (minibuffer-completion-predicate
	  (if (eq (char-after (1- beg)) ?\()
	      'fboundp
	    (function (lambda (sym)
			(or (boundp sym) (fboundp sym)
			    (symbol-plist sym))))))
	 (PC-not-minibuffer t))
    (PC-do-completion nil beg end)))


;;; Wildcards in `C-x C-f' command.  This is independent from the main
;;; completion code, except for `PC-expand-many-files' which is called
;;; when "*"'s are found in the path during filename completion.  (The
;;; above completion code always understands "*"'s, except in file paths,
;;; without relying on the following code.)

(defvar PC-many-files-list nil)

(defun PC-try-load-many-files ()
  (if (string-match "\\*" buffer-file-name)
      (let* ((pat buffer-file-name)
	     (files (PC-expand-many-files pat))
	     (first (car files))
	     (next files))
	(kill-buffer (current-buffer))
	(or files
	    (error "No matching files"))
	(save-window-excursion
	  (while (setq next (cdr next))
	    (let ((buf (find-file-noselect (car next))))
	      (switch-to-buffer buf))))
	;; This modifies the "buf" variable inside find-file-noselect.
	(setq buf (get-file-buffer first))
	(if buf
	    nil   ; should do verify-visited-file-modtime stuff.
	  (setq filename first)
	  (setq buf (create-file-buffer filename))
	  (set-buffer buf)
	  (erase-buffer)
	  (insert-file-contents filename t))
	(if (cdr files)
	    (setq PC-many-files-list (mapconcat
				      (if (string-match "\\*.*/" pat)
					  'identity
					'file-name-nondirectory)
				      (cdr files) ", ")
		  find-file-hooks (cons 'PC-after-load-many-files
					find-file-hooks)))
	;; This modifies the "error" variable inside find-file-noselect.
	(setq error nil)
	t)
    nil))

(defun PC-after-load-many-files ()
  (setq find-file-hooks (delq 'PC-after-load-many-files find-file-hooks))
  (message "Also loaded %s." PC-many-files-list))

(defun PC-expand-many-files (name)
  (save-excursion
    (set-buffer (generate-new-buffer " *Glob Output*"))
    (erase-buffer)
    (shell-command (concat "echo " name) t)
    (goto-char (point-min))
    (if (looking-at ".*No match")
	nil
      (insert "(\"")
      (while (search-forward " " nil t)
	(delete-backward-char 1)
	(insert "\" \""))
      (goto-char (point-max))
      (delete-backward-char 1)
      (insert "\")")
      (goto-char (point-min))
      (let ((files (read (current-buffer))))
	(kill-buffer (current-buffer))
	files))))

(or PC-disable-wildcards
    (memq 'PC-try-load-many-files find-file-not-found-hooks)
    (setq find-file-not-found-hooks (cons 'PC-try-load-many-files
					  find-file-not-found-hooks)))



;;; Facilities for loading C header files.  This is independent from the
;;; main completion code.  See also the variable `PC-include-file-path'
;;; at top of this file.

(defun PC-look-for-include-file ()
  (if (string-match "[\"<]\\([^\"<>]*\\)[\">]?$" (buffer-file-name))
      (let ((name (substring (buffer-file-name)
			     (match-beginning 1) (match-end 1)))
	    (punc (aref (buffer-file-name) (match-beginning 0)))
	    (path nil)
	    new-buf)
	(kill-buffer (current-buffer))
	(if (equal name "")
	    (save-excursion
	      (set-buffer (car (buffer-list)))
	      (save-excursion
		(beginning-of-line)
		(if (looking-at
		     "[ \t]*#[ \t]*include[ \t]+[<\"]\\(.+\\)[>\"][ \t]*[\n/]")
		    (setq name (buffer-substring (match-beginning 1)
						 (match-end 1))
			  punc (char-after (1- (match-beginning 1))))
		  ;; Suggested by Frank Siebenlist:
		  (if (or (looking-at
			   "[ \t]*([ \t]*load[ \t]+\"\\([^\"]+\\)\"")
			  (looking-at
			   "[ \t]*([ \t]*load-library[ \t]+\"\\([^\"]+\\)\"")
			  (looking-at
			   "[ \t]*([ \t]*require[ \t]+'\\([^\t )]+\\)[\t )]"))
		      (progn
			(setq name (buffer-substring (match-beginning 1)
						     (match-end 1))
			      punc ?\<
			      path load-path)
			(if (string-match "\\.elc$" name)
			    (setq name (substring name 0 -1))
			  (or (string-match "\\.el$" name)
			      (setq name (concat name ".el")))))
		    (error "Not on an #include line"))))))
	(or (string-match "\\.[a-zA-Z0-9]+$" name)
	    (setq name (concat name ".h")))
	(if (eq punc ?\<)
	    (let ((path (or path (PC-include-file-path))))
	      (while (and path
			  (not (file-exists-p
				(concat (file-name-as-directory (car path))
					name))))
		(setq path (cdr path)))
	      (if path
		  (setq name (concat (file-name-as-directory (car path)) name))
		(error "No such include file: <%s>" name)))
	  (let ((dir (save-excursion
		       (set-buffer (car (buffer-list)))
		       default-directory)))
	    (if (file-exists-p (concat dir name))
		(setq name (concat dir name))
	      (error "No such include file: \"%s\"" name))))
	(setq new-buf (get-file-buffer name))
	(if new-buf
	    ;; no need to verify last-modified time for this!
	    (set-buffer new-buf)
	  (setq new-buf (create-file-buffer name))
	  (set-buffer new-buf)
	  (erase-buffer)
	  (insert-file-contents name t))
	(setq filename name
	      error nil
	      buf new-buf)
	t)
    nil))

(defun PC-include-file-path ()
  (or PC-include-file-path
      (let ((env (getenv "INCPATH"))
	    (path nil)
	    pos)
	(or env (error "No include file path specified"))
	(while (setq pos (string-match ":[^:]+$" env))
	  (setq path (cons (substring env (1+ pos)) path)
		env (substring env 0 pos)))
	path)))

;;; This is adapted from lib-complete.el, by Mike Williams.
(defun PC-include-file-all-completions (file search-path &optional full)
  "Return all completions for FILE in any directory on SEARCH-PATH.
If optional third argument FULL is non-nil, returned pathnames should be 
absolute rather than relative to some directory on the SEARCH-PATH."
  (setq search-path
	(mapcar '(lambda (dir)
		   (if dir (file-name-as-directory dir) default-directory))
		search-path))
  (if (file-name-absolute-p file)
      ;; It's an absolute file name, so don't need search-path
      (progn
	(setq file (expand-file-name file))
	(file-name-all-completions 
	 (file-name-nondirectory file) (file-name-directory file)))
    (let ((subdir (file-name-directory file))
	  (ndfile (file-name-nondirectory file))
	  file-lists)
      ;; Append subdirectory part to each element of search-path
      (if subdir
	  (setq search-path
		(mapcar '(lambda (dir) (concat dir subdir))
			search-path)
		file ))
      ;; Make list of completions in each directory on search-path
      (while search-path
	(let* ((dir (car search-path))
	       (subdir (if full dir subdir)))
	  (if (file-directory-p dir)
	      (progn
		(setq file-lists
		      (cons 
		       (mapcar '(lambda (file) (concat subdir file))
			       (file-name-all-completions ndfile 
							  (car search-path)))
		       file-lists))))
	  (setq search-path (cdr search-path))))
      ;; Compress out duplicates while building complete list (slloooow!)
      (let ((sorted (sort (apply 'nconc file-lists)
			  '(lambda (x y) (not (string-lessp x y)))))
	    compressed)
	(while sorted
	  (if (equal (car sorted) (car compressed)) nil
	    (setq compressed (cons (car sorted) compressed)))
	  (setq sorted (cdr sorted)))
	compressed))))

(defvar PC-old-read-file-name-internal nil)

(defun PC-read-include-file-name-internal (string dir action)
  (if (string-match "<\\([^\"<>]*\\)>?$" string)
      (let* ((name (substring string (match-beginning 1) (match-end 1)))
	     (str2 (substring string (match-beginning 0)))
	     (completion-table
	      (mapcar (function (lambda (x) (list (format "<%s>" x))))
		      (PC-include-file-all-completions
		       name (PC-include-file-path)))))
	(cond
	 ((not completion-table) nil)
	 ((eq action nil) (try-completion str2 completion-table nil))
	 ((eq action t) (all-completions str2 completion-table nil))
	 ((eq action 'lambda)
	  (eq (try-completion str2 completion-table nil) t))))
    (funcall PC-old-read-file-name-internal string dir action)))

(or PC-disable-includes
    (memq 'PC-look-for-include-file find-file-not-found-hooks)
    (setq find-file-not-found-hooks (cons 'PC-look-for-include-file
					  find-file-not-found-hooks)))

(or PC-disable-includes
    PC-old-read-file-name-internal
    (progn
      (setq PC-old-read-file-name-internal
	    (symbol-function 'read-file-name-internal))
      (fset 'read-file-name-internal 'PC-read-include-file-name-internal)))


(provide 'complete)

;;; End.
