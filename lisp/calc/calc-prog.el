;; Calculator for GNU Emacs, part II [calc-prog.el]
;; Copyright (C) 1990, 1991, 1992, 1993, 2001 Free Software Foundation, Inc.
;; Written by Dave Gillespie, daveg@synaptics.com.

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.



;; This file is autoloaded from calc-ext.el.
(require 'calc-ext)

(require 'calc-macs)

(defun calc-Need-calc-prog () nil)


(defun calc-equal-to (arg)
  (interactive "P")
  (calc-wrapper
   (if (and (integerp arg) (> arg 2))
       (calc-enter-result arg "eq" (cons 'calcFunc-eq (calc-top-list-n arg)))
     (calc-binary-op "eq" 'calcFunc-eq arg)))
)

(defun calc-remove-equal (arg)
  (interactive "P")
  (calc-wrapper
   (calc-unary-op "rmeq" 'calcFunc-rmeq arg))
)

(defun calc-not-equal-to (arg)
  (interactive "P")
  (calc-wrapper
   (if (and (integerp arg) (> arg 2))
       (calc-enter-result arg "neq" (cons 'calcFunc-neq (calc-top-list-n arg)))
     (calc-binary-op "neq" 'calcFunc-neq arg)))
)

(defun calc-less-than (arg)
  (interactive "P")
  (calc-wrapper
   (calc-binary-op "lt" 'calcFunc-lt arg))
)

(defun calc-greater-than (arg)
  (interactive "P")
  (calc-wrapper
   (calc-binary-op "gt" 'calcFunc-gt arg))
)

(defun calc-less-equal (arg)
  (interactive "P")
  (calc-wrapper
   (calc-binary-op "leq" 'calcFunc-leq arg))
)

(defun calc-greater-equal (arg)
  (interactive "P")
  (calc-wrapper
   (calc-binary-op "geq" 'calcFunc-geq arg))
)

(defun calc-in-set (arg)
  (interactive "P")
  (calc-wrapper
   (calc-binary-op "in" 'calcFunc-in arg))
)

(defun calc-logical-and (arg)
  (interactive "P")
  (calc-wrapper
   (calc-binary-op "land" 'calcFunc-land arg 1))
)

(defun calc-logical-or (arg)
  (interactive "P")
  (calc-wrapper
   (calc-binary-op "lor" 'calcFunc-lor arg 0))
)

(defun calc-logical-not (arg)
  (interactive "P")
  (calc-wrapper
   (calc-unary-op "lnot" 'calcFunc-lnot arg))
)

(defun calc-logical-if ()
  (interactive)
  (calc-wrapper
   (calc-enter-result 3 "if" (cons 'calcFunc-if (calc-top-list-n 3))))
)





(defun calc-timing (n)
  (interactive "P")
  (calc-wrapper
   (calc-change-mode 'calc-timing n nil t)
   (message (if calc-timing
		"Reporting timing of slow commands in Trail."
	      "Not reporting timing of commands.")))
)

(defun calc-pass-errors ()
  (interactive)
  ;; The following two cases are for the new, optimizing byte compiler
  ;; or the standard 18.57 byte compiler, respectively.
  (condition-case err
      (let ((place (aref (nth 2 (nth 2 (symbol-function 'calc-do))) 15)))
	(or (memq (car-safe (car-safe place)) '(error xxxerror))
	    (setq place (aref (nth 2 (nth 2 (symbol-function 'calc-do))) 27)))
	(or (memq (car (car place)) '(error xxxerror))
	    (error "foo"))
	(setcar (car place) 'xxxerror))
    (error (error "The calc-do function has been modified; unable to patch.")))
)

(defun calc-user-define ()
  (interactive)
  (message "Define user key: z-")
  (let ((key (read-char)))
    (if (= (calc-user-function-classify key) 0)
	(error "Can't redefine \"?\" key"))
    (let ((func (intern (completing-read (concat "Set key z "
						 (char-to-string key)
						 " to command: ")
					 obarray
					 'commandp
					 t
					 "calc-"))))
      (let* ((kmap (calc-user-key-map))
	     (old (assq key kmap)))
	(if old
	    (setcdr old func)
	  (setcdr kmap (cons (cons key func) (cdr kmap)))))))
)

(defun calc-user-undefine ()
  (interactive)
  (message "Undefine user key: z-")
  (let ((key (read-char)))
    (if (= (calc-user-function-classify key) 0)
	(error "Can't undefine \"?\" key"))
    (let* ((kmap (calc-user-key-map)))
      (delq (or (assq key kmap)
		(assq (upcase key) kmap)
		(assq (downcase key) kmap)
		(error "No such user key is defined"))
	    kmap)))
)

(defun calc-user-define-formula ()
  (interactive)
  (calc-wrapper
   (let* ((form (calc-top 1))
	  (arglist nil)
	  (is-lambda (and (eq (car-safe form) 'calcFunc-lambda)
			  (>= (length form) 2)))
	  odef key keyname cmd cmd-base func alist is-symb)
     (if is-lambda
	 (setq arglist (mapcar (function (lambda (x) (nth 1 x)))
			       (nreverse (cdr (reverse (cdr form)))))
	       form (nth (1- (length form)) form))
       (calc-default-formula-arglist form)
       (setq arglist (sort arglist 'string-lessp)))
     (message "Define user key: z-")
     (setq key (read-char))
     (if (= (calc-user-function-classify key) 0)
	 (error "Can't redefine \"?\" key"))
     (setq key (and (not (memq key '(13 32))) key)
	   keyname (and key
			(if (or (and (<= ?0 key) (<= key ?9))
				(and (<= ?a key) (<= key ?z))
				(and (<= ?A key) (<= key ?Z)))
			    (char-to-string key)
			  (format "%03d" key)))
	   odef (assq key (calc-user-key-map)))
     (while
	 (progn
	   (setq cmd (completing-read "Define M-x command name: "
				      obarray 'commandp nil
				      (if (and odef (symbolp (cdr odef)))
					  (symbol-name (cdr odef))
					"calc-"))
		 cmd-base (and (string-match "\\`calc-\\(.+\\)\\'" cmd)
			       (math-match-substring cmd 1))
		 cmd (and (not (or (string-equal cmd "")
				   (string-equal cmd "calc-")))
			  (intern cmd)))
	   (and cmd
		(fboundp cmd)
		odef
		(not
		 (y-or-n-p
		  (if (get cmd 'calc-user-defn)
		      (concat "Replace previous definition for "
			      (symbol-name cmd) "? ")
		    "That name conflicts with a built-in Emacs function.  Replace this function? "))))))
     (if (and key (not cmd))
	 (setq cmd (intern (concat "calc-User-" keyname))))
     (while
	 (progn
	   (setq func (completing-read "Define algebraic function name: "
				       obarray 'fboundp nil
				       (concat "calcFunc-"
					       (if cmd-base
						   (if (string-match
							"\\`User-.+" cmd-base)
						       (concat
							"User"
							(substring cmd-base 5))
						     cmd-base)
						 "")))
		 func (and (not (or (string-equal func "")
				    (string-equal func "calcFunc-")))
			   (intern func)))
	   (and func
		(fboundp func)
		(not (fboundp cmd))
		odef
		(not
		 (y-or-n-p
		  (if (get func 'calc-user-defn)
		      (concat "Replace previous definition for "
			      (symbol-name func) "? ")
		    "That name conflicts with a built-in Emacs function.  Replace this function? "))))))
     (if (not func)
	 (setq func (intern (concat "calcFunc-User"
				    (or keyname
					(and cmd (symbol-name cmd))
					(format "%05d" (% (random) 10000)))))))
     (if is-lambda
	 (setq alist arglist)
       (while
	   (progn
	     (setq alist (read-from-minibuffer "Function argument list: "
					       (if arglist
						   (prin1-to-string arglist)
						 "()")
					       minibuffer-local-map
					       t))
	     (and (not (calc-subsetp alist arglist))
		  (not (y-or-n-p
			"Okay for arguments that don't appear in formula to be ignored? "))))))
     (setq is-symb (and alist
			func
			(y-or-n-p
			 "Leave it symbolic for non-constant arguments? ")))
     (setq alist (mapcar (function (lambda (x)
				     (or (cdr (assq x '((nil . arg-nil)
							(t . arg-t))))
					 x))) alist))
     (if cmd
	 (progn
	   (calc-need-macros)
	   (fset cmd
		 (list 'lambda
		       '()
		       '(interactive)
		       (list 'calc-wrapper
			     (list 'calc-enter-result
				   (length alist)
				   (let ((name (symbol-name (or func cmd))))
				     (and (string-match
					   "\\([^-][^-]?[^-]?[^-]?\\)[^-]*\\'"
					   name)
					  (math-match-substring name 1)))
				   (list 'cons
					 (list 'quote func)
					 (list 'calc-top-list-n
					       (length alist)))))))
	   (put cmd 'calc-user-defn t)))
     (let ((body (list 'math-normalize (calc-fix-user-formula form))))
       (fset func
	     (append
	      (list 'lambda alist)
	      (and is-symb
		   (mapcar (function (lambda (v)
				       (list 'math-check-const v t)))
			   alist))
	      (list body))))
     (put func 'calc-user-defn form)
     (setq math-integral-cache-state nil)
     (if key
	 (let* ((kmap (calc-user-key-map))
		(old (assq key kmap)))
	   (if old
	       (setcdr old cmd)
	     (setcdr kmap (cons (cons key cmd) (cdr kmap)))))))
   (message ""))
)

(defun calc-default-formula-arglist (form)
  (if (consp form)
      (if (eq (car form) 'var)
	  (if (or (memq (nth 1 form) arglist)
		  (math-const-var form))
	      ()
	    (setq arglist (cons (nth 1 form) arglist)))
	(calc-default-formula-arglist-step (cdr form))))
)

(defun calc-default-formula-arglist-step (l)
  (and l
       (progn
	 (calc-default-formula-arglist (car l))
	 (calc-default-formula-arglist-step (cdr l))))
)

(defun calc-subsetp (a b)
  (or (null a)
      (and (memq (car a) b)
	   (calc-subsetp (cdr a) b)))
)

(defun calc-fix-user-formula (f)
  (if (consp f)
      (let (temp)
	(cond ((and (eq (car f) 'var)
		    (memq (setq temp (or (cdr (assq (nth 1 f) '((nil . arg-nil)
								(t . arg-t))))
					 (nth 1 f)))
			  alist))
	       temp)
	      ((or (math-constp f) (eq (car f) 'var))
	       (list 'quote f))
	      ((and (eq (car f) 'calcFunc-eval)
		    (= (length f) 2))
	       (list 'let '((calc-simplify-mode nil))
		     (list 'math-normalize (calc-fix-user-formula (nth 1 f)))))
	      ((and (eq (car f) 'calcFunc-evalsimp)
		    (= (length f) 2))
	       (list 'math-simplify (calc-fix-user-formula (nth 1 f))))
	      ((and (eq (car f) 'calcFunc-evalextsimp)
		    (= (length f) 2))
	       (list 'math-simplify-extended
		     (calc-fix-user-formula (nth 1 f))))
	      (t
	       (cons 'list
		     (cons (list 'quote (car f))
			   (mapcar 'calc-fix-user-formula (cdr f)))))))
    f)
)

(defun calc-user-define-composition ()
  (interactive)
  (calc-wrapper
   (if (eq calc-language 'unform)
       (error "Can't define formats for unformatted mode"))
   (let* ((comp (calc-top 1))
	  (func (intern (completing-read "Define format for which function: "
					 obarray 'fboundp nil "calcFunc-")))
	  (comps (get func 'math-compose-forms))
	  entry entry2
	  (arglist nil)
	  (alist nil))
     (if (math-zerop comp)
	 (if (setq entry (assq calc-language comps))
	     (put func 'math-compose-forms (delq entry comps)))
       (calc-default-formula-arglist comp)
       (setq arglist (sort arglist 'string-lessp))
       (while
	   (progn
	     (setq alist (read-from-minibuffer "Composition argument list: "
					       (if arglist
						   (prin1-to-string arglist)
						 "()")
					       minibuffer-local-map
					       t))
	     (and (not (calc-subsetp alist arglist))
		  (y-or-n-p
		   "Okay for arguments that don't appear in formula to be invisible? "))))
       (or (setq entry (assq calc-language comps))
	   (put func 'math-compose-forms
		(cons (setq entry (list calc-language)) comps)))
       (or (setq entry2 (assq (length alist) (cdr entry)))
	   (setcdr entry
		   (cons (setq entry2 (list (length alist))) (cdr entry))))
       (setcdr entry2 (list 'lambda alist (calc-fix-user-formula comp))))
     (calc-pop-stack 1)
     (calc-do-refresh)))
)


(defun calc-user-define-kbd-macro (arg)
  (interactive "P")
  (or last-kbd-macro
      (error "No keyboard macro defined"))
  (message "Define last kbd macro on user key: z-")
  (let ((key (read-char)))
    (if (= (calc-user-function-classify key) 0)
	(error "Can't redefine \"?\" key"))
    (let ((cmd (intern (completing-read "Full name for new command: "
					obarray
					'commandp
					nil
					(concat "calc-User-"
						(if (or (and (>= key ?a)
							     (<= key ?z))
							(and (>= key ?A)
							     (<= key ?Z))
							(and (>= key ?0)
							     (<= key ?9)))
						    (char-to-string key)
						  (format "%03d" key)))))))
      (and (fboundp cmd)
	   (not (let ((f (symbol-function cmd)))
		  (or (stringp f)
		      (and (consp f)
			   (eq (car-safe (nth 3 f))
			       'calc-execute-kbd-macro)))))
	   (error "Function %s is already defined and not a keyboard macro"
		  cmd))
      (put cmd 'calc-user-defn t)
      (fset cmd (if (< (prefix-numeric-value arg) 0)
		    last-kbd-macro
		  (list 'lambda
			'(arg)
			'(interactive "P")
			(list 'calc-execute-kbd-macro
			      (vector (key-description last-kbd-macro)
				      last-kbd-macro)
			      'arg
			      (format "z%c" key)))))
      (let* ((kmap (calc-user-key-map))
	     (old (assq key kmap)))
	(if old
	    (setcdr old cmd)
	  (setcdr kmap (cons (cons key cmd) (cdr kmap)))))))
)


(defun calc-edit-user-syntax ()
  (interactive)
  (calc-wrapper
   (let ((lang calc-language))
     (calc-edit-mode (list 'calc-finish-user-syntax-edit (list 'quote lang))
		     t
		     (format "Editing %s-Mode Syntax Table"
			     (cond ((null lang) "Normal")
				   ((eq lang 'tex) "TeX")
				   (t (capitalize (symbol-name lang))))))
     (calc-write-parse-table (cdr (assq lang calc-user-parse-tables))
			     lang)))
  (calc-show-edit-buffer)
)

(defun calc-finish-user-syntax-edit (lang)
  (let ((tab (calc-read-parse-table calc-original-buffer lang))
	(entry (assq lang calc-user-parse-tables)))
    (if tab
	(setcdr (or entry
		    (car (setq calc-user-parse-tables
			       (cons (list lang) calc-user-parse-tables))))
		tab)
      (if entry
	  (setq calc-user-parse-tables
		(delq entry calc-user-parse-tables)))))
  (switch-to-buffer calc-original-buffer)
)

(defun calc-write-parse-table (tab calc-lang)
  (let ((p tab))
    (while p
      (calc-write-parse-table-part (car (car p)))
      (insert ":= "
	      (let ((math-format-hash-args t))
		(math-format-flat-expr (cdr (car p)) 0))
	      "\n")
      (setq p (cdr p))))
)

(defun calc-write-parse-table-part (p)
  (while p
    (cond ((stringp (car p))
	   (let ((s (car p)))
	     (if (and (string-match "\\`\\\\dots\\>" s)
		      (not (eq calc-lang 'tex)))
		 (setq s (concat ".." (substring s 5))))
	     (if (or (and (string-match
			   "[a-zA-Z0-9\"{}]\\|\\`:=\\'\\|\\`#\\|\\`%%" s)
			  (string-match "[^a-zA-Z0-9\\]" s))
		     (and (assoc s '((")") ("]") (">")))
			  (not (cdr p))))
		 (insert (prin1-to-string s) " ")
	       (insert s " "))))
	  ((integerp (car p))
	   (insert "#")
	   (or (= (car p) 0)
	       (insert "/" (int-to-string (car p))))
	   (insert " "))
	  ((and (eq (car (car p)) '\?) (equal (car (nth 2 (car p))) "$$"))
	   (insert (car (nth 1 (car p))) " "))
	  (t
	   (insert "{ ")
	   (calc-write-parse-table-part (nth 1 (car p)))
	   (insert "}" (symbol-name (car (car p))))
	   (if (nth 2 (car p))
	       (calc-write-parse-table-part (list (car (nth 2 (car p)))))
	     (insert " "))))
    (setq p (cdr p)))
)

(defun calc-read-parse-table (calc-buf calc-lang)
  (let ((tab nil))
    (while (progn
	     (skip-chars-forward "\n\t ")
	     (not (eobp)))
      (if (looking-at "%%")
	  (end-of-line)
	(let ((pt (point))
	      (p (calc-read-parse-table-part ":=[\n\t ]+" ":=")))
	  (or (stringp (car p))
	      (and (integerp (car p))
		   (stringp (nth 1 p)))
	      (progn
		(goto-char pt)
		(error "Malformed syntax rule")))
	  (let ((pos (point)))
	    (end-of-line)
	    (let* ((str (buffer-substring pos (point)))
		   (exp (save-excursion
			  (set-buffer calc-buf)
			  (let ((calc-user-parse-tables nil)
				(calc-language nil)
				(math-expr-opers math-standard-opers)
				(calc-hashes-used 0))
			    (math-read-expr
			     (if (string-match ",[ \t]*\\'" str)
				 (substring str 0 (match-beginning 0))
			       str))))))
	      (if (eq (car-safe exp) 'error)
		  (progn
		    (goto-char (+ pos (nth 1 exp)))
		    (error (nth 2 exp))))
	      (setq tab (nconc tab (list (cons p exp)))))))))
    tab)
)

(defun calc-fix-token-name (name &optional unquoted)
  (cond ((string-match "\\`\\.\\." name)
	 (concat "\\dots" (substring name 2)))
	((and (equal name "{") (memq calc-lang '(tex eqn)))
	 "(")
	((and (equal name "}") (memq calc-lang '(tex eqn)))
	 ")")
	((and (equal name "&") (eq calc-lang 'tex))
	 ",")
	((equal name "#")
	 (search-backward "#")
	 (error "Token '#' is reserved"))
	((and unquoted (string-match "#" name))
	 (error "Tokens containing '#' must be quoted"))
	((not (string-match "[^ ]" name))
	 (search-backward "\"" nil t)
	 (error "Blank tokens are not allowed"))
	(t name))
)

(defun calc-read-parse-table-part (term eterm)
  (let ((part nil)
	(quoted nil))
    (while (progn
	     (skip-chars-forward "\n\t ")
	     (if (eobp) (error "Expected '%s'" eterm))
	     (not (looking-at term)))
      (cond ((looking-at "%%")
	     (end-of-line))
	    ((looking-at "{[\n\t ]")
	     (forward-char 2)
	     (let ((p (calc-read-parse-table-part "}" "}")))
	       (or (looking-at "[+*?]")
		   (error "Expected '+', '*', or '?'"))
	       (let ((sym (intern (buffer-substring (point) (1+ (point))))))
		 (forward-char 1)
		 (looking-at "[^\n\t ]*")
		 (let ((sep (buffer-substring (point) (match-end 0))))
		   (goto-char (match-end 0))
		   (and (eq sym '\?) (> (length sep) 0)
			(not (equal sep "$")) (not (equal sep "."))
			(error "Separator not allowed with { ... }?"))
		   (if (string-match "\\`\"" sep)
		       (setq sep (read-from-string sep)))
		   (setq sep (calc-fix-token-name sep))
		   (setq part (nconc part
				     (list (list sym p
						 (and (> (length sep) 0)
						      (cons sep p))))))))))
	    ((looking-at "}")
	     (error "Too many }'s"))
	    ((looking-at "\"")
	     (setq quoted (calc-fix-token-name (read (current-buffer)))
		   part (nconc part (list quoted))))
	    ((looking-at "#\\(\\(/[0-9]+\\)?\\)[\n\t ]")
	     (setq part (nconc part (list (if (= (match-beginning 1)
						 (match-end 1))
					      0
					    (string-to-int
					     (buffer-substring
					      (1+ (match-beginning 1))
					      (match-end 1)))))))
	     (goto-char (match-end 0)))
	    ((looking-at ":=[\n\t ]")
	     (error "Misplaced ':='"))
	    (t
	     (looking-at "[^\n\t ]*")
	     (let ((end (match-end 0)))
	       (setq part (nconc part (list (calc-fix-token-name
					     (buffer-substring
					      (point) end) t))))
	       (goto-char end)))))
    (goto-char (match-end 0))
    (let ((len (length part)))
      (while (and (> len 1)
		  (let ((last (nthcdr (setq len (1- len)) part)))
		    (and (assoc (car last) '((")") ("]") (">")))
			 (not (eq (car last) quoted))
			 (setcar last
				 (list '\? (list (car last)) '("$$"))))))))
    part)
)


(defun calc-user-define-invocation ()
  (interactive)
  (or last-kbd-macro
      (error "No keyboard macro defined"))
  (setq calc-invocation-macro last-kbd-macro)
  (message "Use `M-# Z' to invoke this macro")
)


(defun calc-user-define-edit (prefix)
  (interactive "P")  ; but no calc-wrapper!
  (message "Edit definition of command: z-")
  (let* ((key (read-char))
	 (def (or (assq key (calc-user-key-map))
		  (assq (upcase key) (calc-user-key-map))
		  (assq (downcase key) (calc-user-key-map))
		  (error "No command defined for that key")))
	 (cmd (cdr def)))
    (if (symbolp cmd)
	(setq cmd (symbol-function cmd)))
    (cond ((or (stringp cmd)
	       (and (consp cmd)
		    (eq (car-safe (nth 3 cmd)) 'calc-execute-kbd-macro)))
	   (if (and (>= (prefix-numeric-value prefix) 0)
		    (fboundp 'edit-kbd-macro)
		    (symbolp (cdr def))
		    (eq major-mode 'calc-mode))
	       (progn
		 (if (and (< (window-width) (frame-width))
			  calc-display-trail)
		     (let ((win (get-buffer-window (calc-trail-buffer))))
		       (if win
			   (delete-window win))))
		 (edit-kbd-macro (cdr def) prefix nil
				 (function
				  (lambda (x)
				    (and calc-display-trail
					 (calc-wrapper
					  (calc-trail-display 1 t)))))
				 (function
				  (lambda (cmd)
				    (if (stringp (symbol-function cmd))
					(symbol-function cmd)
				      (let ((mac (nth 1 (nth 3 (symbol-function
								cmd)))))
					(if (vectorp mac)
					    (aref mac 1)
					  mac)))))
				 (function
				  (lambda (new cmd)
				    (if (stringp (symbol-function cmd))
					(fset cmd new)
				      (let ((mac (cdr (nth 3 (symbol-function
							      cmd)))))
					(if (vectorp (car mac))
					    (progn
					      (aset (car mac) 0
						    (key-description new))
					      (aset (car mac) 1 new))
					  (setcar mac new))))))))
	     (let ((keys (progn (and (fboundp 'edit-kbd-macro)
				     (edit-kbd-macro nil))
				(fboundp 'MacEdit-parse-keys))))
	       (calc-wrapper
		(calc-edit-mode (list 'calc-finish-macro-edit
				      (list 'quote def)
				      keys)
				t)
		(if keys
		    (let (top
			  (fill-column 70)
			  (fill-prefix nil))
		      (insert "Notations: RET, SPC, TAB, DEL, LFD, NUL"
			      ", C-xxx, M-xxx.\n\n")
		      (setq top (point))
		      (insert (if (stringp cmd)
				  (key-description cmd)
				(if (vectorp (nth 1 (nth 3 cmd)))
				    (aref (nth 1 (nth 3 cmd)) 0)
				  (key-description (nth 1 (nth 3 cmd)))))
			      "\n")
		      (if (>= (prog2 (forward-char -1)
				     (current-column)
				     (forward-char 1))
			      (frame-width))
			  (fill-region top (point))))
		  (insert "Press C-q to quote control characters like RET"
			  " and TAB.\n"
			  (if (stringp cmd)
			      cmd
			    (if (vectorp (nth 1 (nth 3 cmd)))
				(aref (nth 1 (nth 3 cmd)) 1)
			      (nth 1 (nth 3 cmd)))))))
	       (calc-show-edit-buffer)
	       (forward-line (if keys 2 1)))))
	  (t (let* ((func (calc-stack-command-p cmd))
		    (defn (and func
			       (symbolp func)
			       (get func 'calc-user-defn))))
	       (if (and defn (calc-valid-formula-func func))
		   (progn
		     (calc-wrapper
		      (calc-edit-mode (list 'calc-finish-formula-edit
					    (list 'quote func)))
		      (insert (math-showing-full-precision
			       (math-format-nice-expr defn (frame-width)))
			      "\n"))
		     (calc-show-edit-buffer))
		 (error "That command's definition cannot be edited"))))))
)

(defun calc-finish-macro-edit (def keys)
  (forward-line 1)
  (if (and keys (looking-at "\n")) (forward-line 1))
  (let* ((true-str (buffer-substring (point) (point-max)))
	 (str true-str))
    (if keys (setq str (MacEdit-parse-keys str)))
    (if (symbolp (cdr def))
	(if (stringp (symbol-function (cdr def)))
	    (fset (cdr def) str)
	  (let ((mac (cdr (nth 3 (symbol-function (cdr def))))))
	    (if (vectorp (car mac))
		(progn
		  (aset (car mac) 0 (if keys true-str (key-description str)))
		  (aset (car mac) 1 str))
	      (setcar mac str))))
      (setcdr def str)))
)

;;; The following are hooks into the MacEdit package from macedit.el.
(put 'calc-execute-extended-command 'MacEdit-print
     (function (lambda ()
		 (setq macro-str (concat "\excalc-" macro-str))))
)

(put 'calcDigit-start 'MacEdit-print
     (function (lambda ()
		 (if calc-algebraic-mode
		     (calc-macro-edit-algebraic)
		   (MacEdit-unread-chars key-last)
		   (let ((str "")
			 (min-bsp 0)
			 ch last)
		     (while (and (setq ch (MacEdit-read-char))
				 (or (and (>= ch ?0) (<= ch ?9))
				     (memq ch '(?\. ?e ?\_ ?n ?\: ?\# ?M
						    ?o ?h ?\@ ?\"))
				     (and (memq ch '(?\' ?m ?s))
					  (string-match "[@oh]" str))
				     (and (or (and (>= ch ?a) (<= ch ?z))
					      (and (>= ch ?A) (<= ch ?Z)))
					  (string-match
					   "^[-+]?\\(1[1-9]\\|[2-9][0-9]\\)#"
					   str))
				     (and (memq ch '(?\177 ?\C-h))
					  (> (length str) 0))
				     (and (memq ch '(?+ ?-))
					  (> (length str) 0)
					  (eq (aref str (1- (length str)))
					      ?e))))
		       (if (or (and (>= ch ?0) (<= ch ?9))
			       (and (or (not (memq ch '(?\177 ?\C-h)))
					(<= (length str) min-bsp))
				    (setq min-bsp (1+ (length str)))))
			   (setq str (concat str (char-to-string ch)))
			 (setq str (substring str 0 -1))))
		     (if (memq ch '(32 10 13))
			 (setq str (concat str (char-to-string ch)))
		       (MacEdit-unread-chars ch))
		     (insert "type \"")
		     (MacEdit-insert-string str)
		     (insert "\"\n")))))
)

(defun calc-macro-edit-algebraic ()
  (MacEdit-unread-chars key-last)
  (let ((str "")
	(min-bsp 0))
    (while (progn
	     (MacEdit-lookup-key calc-alg-ent-map)
	     (or (and (memq key-symbol '(self-insert-command
					 calcAlg-previous))
		      (< (length str) 60))
		 (memq key-symbol
			    '(backward-delete-char
			      delete-backward-char
			      backward-delete-char-untabify))
		 (eq key-last 9)))
      (setq macro-str (substring macro-str (length key-str)))
      (if (or (eq key-symbol 'self-insert-command)
	      (and (or (not (memq key-symbol '(backward-delete-char
					       delete-backward-char
					       backward-delete-char-untabify)))
		       (<= (length str) min-bsp))
		   (setq min-bsp (+ (length str) (length key-str)))))
	  (setq str (concat str key-str))
	(setq str (substring str 0 -1))))
    (if (memq key-last '(10 13))
	(setq str (concat str key-str)
	      macro-str (substring macro-str (length key-str))))
    (if (> (length str) 0)
	(progn
	  (insert "type \"")
	  (MacEdit-insert-string str)
	  (insert "\"\n"))))
)
(put 'calc-algebraic-entry 'MacEdit-print 'calc-macro-edit-algebraic)
(put 'calc-auto-algebraic-entry 'MacEdit-print 'calc-macro-edit-algebraic)

(defun calc-macro-edit-variable (&optional no-cmd)
  (let ((str "") ch)
    (or no-cmd (insert (symbol-name key-symbol) "\n"))
    (if (memq (MacEdit-peek-char) '(?\+ ?\- ?\* ?\/ ?\^ ?\|))
	(setq str (char-to-string (MacEdit-read-char))))
    (if (and (setq ch (MacEdit-peek-char))
	     (>= ch ?0) (<= ch ?9))
	(insert "type \"" str
		(char-to-string (MacEdit-read-char)) "\"\n")
      (if (> (length str) 0)
	  (insert "type \"" str "\"\n"))
      (MacEdit-read-argument)))
)
(put 'calc-store 'MacEdit-print 'calc-macro-edit-variable)
(put 'calc-store-into 'MacEdit-print 'calc-macro-edit-variable)
(put 'calc-store-neg 'MacEdit-print 'calc-macro-edit-variable)
(put 'calc-store-plus 'MacEdit-print 'calc-macro-edit-variable)
(put 'calc-store-minus 'MacEdit-print 'calc-macro-edit-variable)
(put 'calc-store-times 'MacEdit-print 'calc-macro-edit-variable)
(put 'calc-store-div 'MacEdit-print 'calc-macro-edit-variable)
(put 'calc-store-power 'MacEdit-print 'calc-macro-edit-variable)
(put 'calc-store-concat 'MacEdit-print 'calc-macro-edit-variable)
(put 'calc-store-inv 'MacEdit-print 'calc-macro-edit-variable)
(put 'calc-store-decr 'MacEdit-print 'calc-macro-edit-variable)
(put 'calc-store-incr 'MacEdit-print 'calc-macro-edit-variable)
(put 'calc-store-exchange 'MacEdit-print 'calc-macro-edit-variable)
(put 'calc-unstore 'MacEdit-print 'calc-macro-edit-variable)
(put 'calc-recall 'MacEdit-print 'calc-macro-edit-variable)
(put 'calc-let 'MacEdit-print 'calc-macro-edit-variable)
(put 'calc-permanent-variable 'MacEdit-print 'calc-macro-edit-variable)

(defun calc-macro-edit-variable-2 ()
  (calc-macro-edit-variable)
  (calc-macro-edit-variable t)
)
(put 'calc-copy-variable 'MacEdit-print 'calc-macro-edit-variable-2)
(put 'calc-declare-variable 'MacEdit-print 'calc-macro-edit-variable-2)

(defun calc-macro-edit-quick-digit ()
  (insert "type \"" key-str "\"  # " (symbol-name key-symbol) "\n")
)
(put 'calc-store-quick 'MacEdit-print 'calc-macro-edit-quick-digit)
(put 'calc-store-into-quick 'MacEdit-print 'calc-macro-edit-quick-digit)
(put 'calc-recall-quick 'MacEdit-print 'calc-macro-edit-quick-digit)
(put 'calc-select-part 'MacEdit-print 'calc-macro-edit-quick-digit)
(put 'calc-clean-num 'MacEdit-print 'calc-macro-edit-quick-digit)


(defun calc-finish-formula-edit (func)
  (let ((buf (current-buffer))
	(str (buffer-substring (point) (point-max)))
	(start (point))
	(body (calc-valid-formula-func func)))
    (set-buffer calc-original-buffer)
    (let ((val (math-read-expr str)))
      (if (eq (car-safe val) 'error)
	  (progn
	    (set-buffer buf)
	    (goto-char (+ start (nth 1 val)))
	    (error (nth 2 val))))
      (setcar (cdr body)
	      (let ((alist (nth 1 (symbol-function func))))
		(calc-fix-user-formula val)))
      (put func 'calc-user-defn val)))
)

(defun calc-valid-formula-func (func)
  (let ((def (symbol-function func)))
    (and (consp def)
	 (eq (car def) 'lambda)
	 (progn
	   (setq def (cdr (cdr def)))
	   (while (and def
		       (not (eq (car (car def)) 'math-normalize)))
	     (setq def (cdr def)))
	   (car def))))
)


(defun calc-get-user-defn ()
  (interactive)
  (calc-wrapper
   (message "Get definition of command: z-")
   (let* ((key (read-char))
	  (def (or (assq key (calc-user-key-map))
		   (assq (upcase key) (calc-user-key-map))
		   (assq (downcase key) (calc-user-key-map))
		   (error "No command defined for that key")))
	  (cmd (cdr def)))
     (if (symbolp cmd)
	 (setq cmd (symbol-function cmd)))
     (cond ((stringp cmd)
	    (message "Keyboard macro: %s" cmd))
	   (t (let* ((func (calc-stack-command-p cmd))
		     (defn (and func
				(symbolp func)
				(get func 'calc-user-defn))))
		(if defn
		    (progn
		      (and (calc-valid-formula-func func)
			   (setq defn (append '(calcFunc-lambda)
					      (mapcar 'math-build-var-name
						      (nth 1 (symbol-function
							      func)))
					      (list defn))))
		      (calc-enter-result 0 "gdef" defn))
		  (error "That command is not defined by a formula")))))))
)


(defun calc-user-define-permanent ()
  (interactive)
  (calc-wrapper
   (message "Record in %s the command: z-" calc-settings-file)
   (let* ((key (read-char))
	  (def (or (assq key (calc-user-key-map))
		   (assq (upcase key) (calc-user-key-map))
		   (assq (downcase key) (calc-user-key-map))
		   (and (eq key ?\') 
			(cons nil
			      (intern (completing-read
				       (format "Record in %s the function: "
					       calc-settings-file)
				       obarray 'fboundp nil "calcFunc-"))))
		   (error "No command defined for that key"))))
     (set-buffer (find-file-noselect (substitute-in-file-name
				      calc-settings-file)))
     (goto-char (point-max))
     (let* ((cmd (cdr def))
	    (fcmd (and cmd (symbolp cmd) (symbol-function cmd)))
	    (func nil)
	    (pt (point))
	    (fill-column 70)
	    (fill-prefix nil)
	    str q-ok)
       (insert "\n;;; Definition stored by Calc on " (current-time-string)
	       "\n(put 'calc-define '"
	       (if (symbolp cmd) (symbol-name cmd) (format "key%d" key))
	       " '(progn\n")
       (if (and fcmd
		(eq (car-safe fcmd) 'lambda)
		(get cmd 'calc-user-defn))
	   (let ((pt (point)))
	     (and (eq (car-safe (nth 3 fcmd)) 'calc-execute-kbd-macro)
		  (vectorp (nth 1 (nth 3 fcmd)))
		  (progn (and (fboundp 'edit-kbd-macro)
			      (edit-kbd-macro nil))
			 (fboundp 'MacEdit-parse-keys))
		  (setq q-ok t)
		  (aset (nth 1 (nth 3 fcmd)) 1 nil))
	     (insert (setq str (prin1-to-string
				(cons 'defun (cons cmd (cdr fcmd)))))
		     "\n")
	     (or (and (string-match "\"" str) (not q-ok))
		 (fill-region pt (point)))
	     (indent-rigidly pt (point) 2)
	     (delete-region pt (1+ pt))
	     (insert " (put '" (symbol-name cmd)
		     " 'calc-user-defn '"
		     (prin1-to-string (get cmd 'calc-user-defn))
		     ")\n")
	     (setq func (calc-stack-command-p cmd))
	     (let ((ffunc (and func (symbolp func) (symbol-function func)))
		   (pt (point)))
	       (and ffunc
		    (eq (car-safe ffunc) 'lambda)
		    (get func 'calc-user-defn)
		    (progn
		      (insert (setq str (prin1-to-string
					 (cons 'defun (cons func
							    (cdr ffunc)))))
			      "\n")
		      (or (and (string-match "\"" str) (not q-ok))
			  (fill-region pt (point)))
		      (indent-rigidly pt (point) 2)
		      (delete-region pt (1+ pt))
		      (setq pt (point))
		      (insert "(put '" (symbol-name func)
			      " 'calc-user-defn '"
			      (prin1-to-string (get func 'calc-user-defn))
			      ")\n")
		      (fill-region pt (point))
		      (indent-rigidly pt (point) 2)
		      (delete-region pt (1+ pt))))))
	 (and (stringp fcmd)
	      (insert " (fset '" (prin1-to-string cmd)
		      " " (prin1-to-string fcmd) ")\n")))
       (or func (setq func (and cmd (symbolp cmd) (fboundp cmd) cmd)))
       (if (get func 'math-compose-forms)
	   (let ((pt (point)))
	     (insert "(put '" (symbol-name cmd)
		     " 'math-compose-forms '"
		     (prin1-to-string (get func 'math-compose-forms))
		     ")\n")
	     (fill-region pt (point))
	     (indent-rigidly pt (point) 2)
	     (delete-region pt (1+ pt))))
       (if (car def)
	   (insert " (define-key calc-mode-map "
		   (prin1-to-string (concat "z" (char-to-string key)))
		   " '"
		   (prin1-to-string cmd)
		   ")\n")))
     (insert "))\n")
     (save-buffer)))
)

(defun calc-stack-command-p (cmd)
  (if (and cmd (symbolp cmd))
      (and (fboundp cmd)
	   (calc-stack-command-p (symbol-function cmd)))
    (and (consp cmd)
	 (eq (car cmd) 'lambda)
	 (setq cmd (or (assq 'calc-wrapper cmd)
		       (assq 'calc-slow-wrapper cmd)))
	 (setq cmd (assq 'calc-enter-result cmd))
	 (memq (car (nth 3 cmd)) '(cons list))
	 (eq (car (nth 1 (nth 3 cmd))) 'quote)
	 (nth 1 (nth 1 (nth 3 cmd)))))
)


(defun calc-call-last-kbd-macro (arg)
  (interactive "P")
  (and defining-kbd-macro
       (error "Can't execute anonymous macro while defining one"))
  (or last-kbd-macro
      (error "No kbd macro has been defined"))
  (calc-execute-kbd-macro last-kbd-macro arg)
)

(defun calc-execute-kbd-macro (mac arg &rest prefix)
  (if (and (vectorp mac) (> (length mac) 0) (stringp (aref mac 0)))
      (setq mac (or (aref mac 1)
		    (aset mac 1 (progn (and (fboundp 'edit-kbd-macro)
					    (edit-kbd-macro nil))
				       (MacEdit-parse-keys (aref mac 0)))))))
  (if (< (prefix-numeric-value arg) 0)
      (execute-kbd-macro mac (- (prefix-numeric-value arg)))
    (if calc-executing-macro
	(execute-kbd-macro mac arg)
      (calc-slow-wrapper
       (let ((old-stack-whole (copy-sequence calc-stack))
	     (old-stack-top calc-stack-top)
	     (old-buffer-size (buffer-size))
	     (old-refresh-count calc-refresh-count))
	 (unwind-protect
	     (let ((calc-executing-macro mac))
	       (execute-kbd-macro mac arg))
	   (calc-select-buffer)
	   (let ((new-stack (reverse calc-stack))
		 (old-stack (reverse old-stack-whole)))
	     (while (and new-stack old-stack
			 (equal (car new-stack) (car old-stack)))
	       (setq new-stack (cdr new-stack)
		     old-stack (cdr old-stack)))
	     (or (equal prefix '(nil))
		 (calc-record-list (if (> (length new-stack) 1)
				       (mapcar 'car new-stack)
				     '(""))
				   (or (car prefix) "kmac")))
	     (calc-record-undo (list 'set 'saved-stack-top old-stack-top))
	     (and old-stack
		  (calc-record-undo (list 'pop 1 (mapcar 'car old-stack))))
	     (let ((calc-stack old-stack-whole)
		   (calc-stack-top 0))
	       (calc-cursor-stack-index (length old-stack)))
	     (if (and (= old-buffer-size (buffer-size))
		      (= old-refresh-count calc-refresh-count))
		 (let ((buffer-read-only nil))
		   (delete-region (point) (point-max))
		   (while new-stack
		     (calc-record-undo (list 'push 1))
		     (insert (math-format-stack-value (car new-stack)) "\n")
		     (setq new-stack (cdr new-stack)))
		   (calc-renumber-stack))
	       (while new-stack
		 (calc-record-undo (list 'push 1))
		 (setq new-stack (cdr new-stack)))
	       (calc-refresh))
	     (calc-record-undo (list 'set 'saved-stack-top 0))))))))
)

(defun calc-push-list-in-macro (vals m sels)
  (let ((entry (list (car vals) 1 (car sels)))
	(mm (+ (or m 1) calc-stack-top)))
    (if (> mm 1)
	(setcdr (nthcdr (- mm 2) calc-stack)
		(cons entry (nthcdr (1- mm) calc-stack)))
      (setq calc-stack (cons entry calc-stack))))
)

(defun calc-pop-stack-in-macro (n mm)
  (if (> mm 1)
      (setcdr (nthcdr (- mm 2) calc-stack)
	      (nthcdr (+ n mm -1) calc-stack))
    (setq calc-stack (nthcdr n calc-stack)))
)


(defun calc-kbd-if ()
  (interactive)
  (calc-wrapper
   (let ((cond (calc-top-n 1)))
     (calc-pop-stack 1)
     (if (math-is-true cond)
	 (if defining-kbd-macro
	     (message "If true..."))
       (if defining-kbd-macro
	   (message "Condition is false; skipping to Z: or Z] ..."))
       (calc-kbd-skip-to-else-if t))))
)

(defun calc-kbd-else-if ()
  (interactive)
  (calc-kbd-if)
)

(defun calc-kbd-skip-to-else-if (else-okay)
  (let ((count 0)
	ch)
    (while (>= count 0)
      (setq ch (read-char))
      (if (= ch -1)
	  (error "Unterminated Z[ in keyboard macro"))
      (if (= ch ?Z)
	  (progn
	    (setq ch (read-char))
	    (cond ((= ch ?\[)
		   (setq count (1+ count)))
		  ((= ch ?\])
		   (setq count (1- count)))
		  ((= ch ?\:)
		   (and (= count 0)
			else-okay
			(setq count -1)))
		  ((eq ch 7)
		   (keyboard-quit))))))
    (and defining-kbd-macro
	 (if (= ch ?\:)
	     (message "Else...")
	   (message "End-if..."))))
)

(defun calc-kbd-end-if ()
  (interactive)
  (if defining-kbd-macro
      (message "End-if..."))
)

(defun calc-kbd-else ()
  (interactive)
  (if defining-kbd-macro
      (message "Else; skipping to Z] ..."))
  (calc-kbd-skip-to-else-if nil)
)


(defun calc-kbd-repeat ()
  (interactive)
  (let (count)
    (calc-wrapper
     (setq count (math-trunc (calc-top-n 1)))
     (or (Math-integerp count)
	 (error "Count must be an integer"))
     (if (Math-integer-negp count)
	 (setq count 0))
     (or (integerp count)
	 (setq count 1000000))
     (calc-pop-stack 1))
    (calc-kbd-loop count))
)

(defun calc-kbd-for (dir)
  (interactive "P")
  (let (init final)
    (calc-wrapper
     (setq init (calc-top-n 2)
	   final (calc-top-n 1))
     (or (and (math-anglep init) (math-anglep final))
	 (error "Initial and final values must be real numbers"))
     (calc-pop-stack 2))
    (calc-kbd-loop nil init final (and dir (prefix-numeric-value dir))))
)

(defun calc-kbd-loop (rpt-count &optional initial final dir)
  (interactive "P")
  (setq rpt-count (if rpt-count (prefix-numeric-value rpt-count) 1000000))
  (let* ((count 0)
	 (parts nil)
	 (body "")
	 (open last-command-char)
	 (counter initial)
	 ch)
    (or executing-kbd-macro
	(message "Reading loop body..."))
    (while (>= count 0)
      (setq ch (read-char))
      (if (= ch -1)
	  (error "Unterminated Z%c in keyboard macro" open))
      (if (= ch ?Z)
	  (progn
	    (setq ch (read-char)
		  body (concat body "Z" (char-to-string ch)))
	    (cond ((memq ch '(?\< ?\( ?\{))
		   (setq count (1+ count)))
		  ((memq ch '(?\> ?\) ?\}))
		   (setq count (1- count)))
		  ((and (= ch ?/)
			(= count 0))
		   (setq parts (nconc parts (list (concat (substring body 0 -2)
							  "Z]")))
			 body ""))
		  ((eq ch 7)
		   (keyboard-quit))))
	(setq body (concat body (char-to-string ch)))))
    (if (/= ch (cdr (assq open '( (?\< . ?\>) (?\( . ?\)) (?\{ . ?\}) ))))
	(error "Mismatched Z%c and Z%c in keyboard macro" open ch))
    (or executing-kbd-macro
	(message "Looping..."))
    (setq body (concat (substring body 0 -2) "Z]"))
    (and (not executing-kbd-macro)
	 (= rpt-count 1000000)
	 (null parts)
	 (null counter)
	 (progn
	   (message "Warning: Infinite loop!  Not executing.")
	   (setq rpt-count 0)))
    (or (not initial) dir
	(setq dir (math-compare final initial)))
    (calc-wrapper
     (while (> rpt-count 0)
       (let ((part parts))
	 (if counter
	     (if (cond ((eq dir 0) (Math-equal final counter))
		       ((eq dir 1) (Math-lessp final counter))
		       ((eq dir -1) (Math-lessp counter final)))
		 (setq rpt-count 0)
	       (calc-push counter)))
	 (while (and part (> rpt-count 0))
	   (execute-kbd-macro (car part))
	   (if (math-is-true (calc-top-n 1))
	       (setq rpt-count 0)
	     (setq part (cdr part)))
	   (calc-pop-stack 1))
	 (if (> rpt-count 0)
	     (progn
	       (execute-kbd-macro body)
	       (if counter
		   (let ((step (calc-top-n 1)))
		     (calc-pop-stack 1)
		     (setq counter (calcFunc-add counter step)))
		 (setq rpt-count (1- rpt-count))))))))
    (or executing-kbd-macro
	(message "Looping...done")))
)

(defun calc-kbd-end-repeat ()
  (interactive)
  (error "Unbalanced Z> in keyboard macro")
)

(defun calc-kbd-end-for ()
  (interactive)
  (error "Unbalanced Z) in keyboard macro")
)

(defun calc-kbd-end-loop ()
  (interactive)
  (error "Unbalanced Z} in keyboard macro")
)

(defun calc-kbd-break ()
  (interactive)
  (calc-wrapper
   (let ((cond (calc-top-n 1)))
     (calc-pop-stack 1)
     (if (math-is-true cond)
	 (error "Keyboard macro aborted."))))
)


(defun calc-kbd-push (arg)
  (interactive "P")
  (calc-wrapper
   (let* ((defs (and arg (> (prefix-numeric-value arg) 0)))
	  (var-q0 (and (boundp 'var-q0) var-q0))
	  (var-q1 (and (boundp 'var-q1) var-q1))
	  (var-q2 (and (boundp 'var-q2) var-q2))
	  (var-q3 (and (boundp 'var-q3) var-q3))
	  (var-q4 (and (boundp 'var-q4) var-q4))
	  (var-q5 (and (boundp 'var-q5) var-q5))
	  (var-q6 (and (boundp 'var-q6) var-q6))
	  (var-q7 (and (boundp 'var-q7) var-q7))
	  (var-q8 (and (boundp 'var-q8) var-q8))
	  (var-q9 (and (boundp 'var-q9) var-q9))
	  (calc-internal-prec (if defs 12 calc-internal-prec))
	  (calc-word-size (if defs 32 calc-word-size))
	  (calc-angle-mode (if defs 'deg calc-angle-mode))
	  (calc-simplify-mode (if defs nil calc-simplify-mode))
	  (calc-algebraic-mode (if arg nil calc-algebraic-mode))
	  (calc-incomplete-algebraic-mode (if arg nil
					    calc-incomplete-algebraic-mode))
	  (calc-symbolic-mode (if defs nil calc-symbolic-mode))
	  (calc-matrix-mode (if defs nil calc-matrix-mode))
	  (calc-prefer-frac (if defs nil calc-prefer-frac))
	  (calc-complex-mode (if defs nil calc-complex-mode))
	  (calc-infinite-mode (if defs nil calc-infinite-mode))
	  (count 0)
	  (body "")
	  ch)
     (if (or executing-kbd-macro defining-kbd-macro)
	 (progn
	   (if defining-kbd-macro
	       (message "Reading body..."))
	   (while (>= count 0)
	     (setq ch (read-char))
	     (if (= ch -1)
		 (error "Unterminated Z` in keyboard macro"))
	     (if (= ch ?Z)
		 (progn
		   (setq ch (read-char)
			 body (concat body "Z" (char-to-string ch)))
		   (cond ((eq ch ?\`)
			  (setq count (1+ count)))
			 ((eq ch ?\')
			  (setq count (1- count)))
			 ((eq ch 7)
			  (keyboard-quit))))
	       (setq body (concat body (char-to-string ch)))))
	   (if defining-kbd-macro
	       (message "Reading body...done"))
	   (let ((calc-kbd-push-level 0))
	     (execute-kbd-macro (substring body 0 -2))))
       (let ((calc-kbd-push-level (1+ calc-kbd-push-level)))
	 (message "Saving modes; type Z' to restore")
	 (recursive-edit)))))
)
(setq calc-kbd-push-level 0)

(defun calc-kbd-pop ()
  (interactive)
  (if (> calc-kbd-push-level 0)
      (progn
	(message "Mode settings restored")
	(exit-recursive-edit))
    (error "Unbalanced Z' in keyboard macro"))
)


(defun calc-kbd-report (msg)
  (interactive "sMessage: ")
  (calc-wrapper
   (let ((executing-kbd-macro nil)
	 (defining-kbd-macro nil))
     (math-working msg (calc-top-n 1))))
)

(defun calc-kbd-query (msg)
  (interactive "sPrompt: ")
  (calc-wrapper
   (let ((executing-kbd-macro nil)
	 (defining-kbd-macro nil))
     (calc-alg-entry nil (and (not (equal msg "")) msg))))
)







;;;; Logical operations.

(defun calcFunc-eq (a b &rest more)
  (if more
      (let* ((args (cons a (cons b (copy-sequence more))))
	     (res 1)
	     (p args)
	     p2)
	(while (and (cdr p) (not (eq res 0)))
	  (setq p2 p)
	  (while (and (setq p2 (cdr p2)) (not (eq res 0)))
	    (setq res (math-two-eq (car p) (car p2)))
	    (if (eq res 1)
		(setcdr p (delq (car p2) (cdr p)))))
	  (setq p (cdr p)))
	(if (eq res 0)
	    0
	  (if (cdr args)
	      (cons 'calcFunc-eq args)
	    1)))
    (or (math-two-eq a b)
	(if (and (or (math-looks-negp a) (math-zerop a))
		 (or (math-looks-negp b) (math-zerop b)))
	    (list 'calcFunc-eq (math-neg a) (math-neg b))
	  (list 'calcFunc-eq a b))))
)

(defun calcFunc-neq (a b &rest more)
  (if more
      (let* ((args (cons a (cons b more)))
	     (res 0)
	     (all t)
	     (p args)
	     p2)
	(while (and (cdr p) (not (eq res 1)))
	  (setq p2 p)
	  (while (and (setq p2 (cdr p2)) (not (eq res 1)))
	    (setq res (math-two-eq (car p) (car p2)))
	    (or res (setq all nil)))
	  (setq p (cdr p)))
	(if (eq res 1)
	    0
	  (if all
	      1
	    (cons 'calcFunc-neq args))))
    (or (cdr (assq (math-two-eq a b) '((0 . 1) (1 . 0))))
	(if (and (or (math-looks-negp a) (math-zerop a))
		 (or (math-looks-negp b) (math-zerop b)))
	    (list 'calcFunc-neq (math-neg a) (math-neg b))
	  (list 'calcFunc-neq a b))))
)

(defun math-two-eq (a b)
  (if (eq (car-safe a) 'vec)
      (if (eq (car-safe b) 'vec)
	  (if (= (length a) (length b))
	      (let ((res 1))
		(while (and (setq a (cdr a) b (cdr b)) (not (eq res 0)))
		  (if res
		      (setq res (math-two-eq (car a) (car b)))
		    (if (eq (math-two-eq (car a) (car b)) 0)
			(setq res 0))))
		res)
	    0)
	(if (Math-objectp b)
	    0
	  nil))
    (if (eq (car-safe b) 'vec)
	(if (Math-objectp a)
	    0
	  nil)
      (let ((res (math-compare a b)))
	(if (= res 0)
	    1
	  (if (and (= res 2) (not (and (Math-scalarp a) (Math-scalarp b))))
	      nil
	    0)))))
)

(defun calcFunc-lt (a b)
  (let ((res (math-compare a b)))
    (if (= res -1)
	1
      (if (= res 2)
	  (if (and (or (math-looks-negp a) (math-zerop a))
		   (or (math-looks-negp b) (math-zerop b)))
	      (list 'calcFunc-gt (math-neg a) (math-neg b))
	    (list 'calcFunc-lt a b))
	0)))
)

(defun calcFunc-gt (a b)
  (let ((res (math-compare a b)))
    (if (= res 1)
	1
      (if (= res 2)
	  (if (and (or (math-looks-negp a) (math-zerop a))
		   (or (math-looks-negp b) (math-zerop b)))
	      (list 'calcFunc-lt (math-neg a) (math-neg b))
	    (list 'calcFunc-gt a b))
	0)))
)

(defun calcFunc-leq (a b)
  (let ((res (math-compare a b)))
    (if (= res 1)
	0
      (if (= res 2)
	  (if (and (or (math-looks-negp a) (math-zerop a))
		   (or (math-looks-negp b) (math-zerop b)))
	      (list 'calcFunc-geq (math-neg a) (math-neg b))
	    (list 'calcFunc-leq a b))
	1)))
)

(defun calcFunc-geq (a b)
  (let ((res (math-compare a b)))
    (if (= res -1)
	0
      (if (= res 2)
	  (if (and (or (math-looks-negp a) (math-zerop a))
		   (or (math-looks-negp b) (math-zerop b)))
	      (list 'calcFunc-leq (math-neg a) (math-neg b))
	    (list 'calcFunc-geq a b))
	1)))
)

(defun calcFunc-rmeq (a)
  (if (math-vectorp a)
      (math-map-vec 'calcFunc-rmeq a)
    (if (assq (car-safe a) calc-tweak-eqn-table)
	(if (and (eq (car-safe (nth 2 a)) 'var)
		 (math-objectp (nth 1 a)))
	    (nth 1 a)
	  (nth 2 a))
      (if (eq (car-safe a) 'calcFunc-assign)
	  (nth 2 a)
	(if (eq (car-safe a) 'calcFunc-evalto)
	    (nth 1 a)
	  (list 'calcFunc-rmeq a)))))
)

(defun calcFunc-land (a b)
  (cond ((Math-zerop a)
	 a)
	((Math-zerop b)
	 b)
	((math-is-true a)
	 b)
	((math-is-true b)
	 a)
	(t (list 'calcFunc-land a b)))
)

(defun calcFunc-lor (a b)
  (cond ((Math-zerop a)
	 b)
	((Math-zerop b)
	 a)
	((math-is-true a)
	 a)
	((math-is-true b)
	 b)
	(t (list 'calcFunc-lor a b)))
)

(defun calcFunc-lnot (a)
  (if (Math-zerop a)
      1
    (if (math-is-true a)
	0
      (let ((op (and (= (length a) 3)
		     (assq (car a) calc-tweak-eqn-table))))
	(if op
	    (cons (nth 2 op) (cdr a))
	  (list 'calcFunc-lnot a)))))
)

(defun calcFunc-if (c e1 e2)
  (if (Math-zerop c)
      e2
    (if (and (math-is-true c) (not (Math-vectorp c)))
	e1
      (or (and (Math-vectorp c)
	       (math-constp c)
	       (let ((ee1 (if (Math-vectorp e1)
			      (if (= (length c) (length e1))
				  (cdr e1)
				(calc-record-why "*Dimension error" e1))
			    (list e1)))
		     (ee2 (if (Math-vectorp e2)
			      (if (= (length c) (length e2))
				  (cdr e2)
				(calc-record-why "*Dimension error" e2))
			    (list e2))))
		 (and ee1 ee2
		      (cons 'vec (math-if-vector (cdr c) ee1 ee2)))))
	  (list 'calcFunc-if c e1 e2))))
)

(defun math-if-vector (c e1 e2)
  (and c
       (cons (if (Math-zerop (car c)) (car e2) (car e1))
	     (math-if-vector (cdr c)
			     (or (cdr e1) e1)
			     (or (cdr e2) e2))))
)

(defun math-normalize-logical-op (a)
  (or (and (eq (car a) 'calcFunc-if)
	   (= (length a) 4)
	   (let ((a1 (math-normalize (nth 1 a))))
	     (if (Math-zerop a1)
		 (math-normalize (nth 3 a))
	       (if (Math-numberp a1)
		   (math-normalize (nth 2 a))
		 (if (and (Math-vectorp (nth 1 a))
			  (math-constp (nth 1 a)))
		     (calcFunc-if (nth 1 a)
				  (math-normalize (nth 2 a))
				  (math-normalize (nth 3 a)))
		   (let ((calc-simplify-mode 'none))
		     (list 'calcFunc-if a1
			   (math-normalize (nth 2 a))
			   (math-normalize (nth 3 a)))))))))
      a)
)

(defun calcFunc-in (a b)
  (or (and (eq (car-safe b) 'vec)
	   (let ((bb b))
	     (while (and (setq bb (cdr bb))
			 (not (if (memq (car-safe (car bb)) '(vec intv))
				  (eq (calcFunc-in a (car bb)) 1)
				(Math-equal a (car bb))))))
	     (if bb 1 (and (math-constp a) (math-constp bb) 0))))
      (and (eq (car-safe b) 'intv)
	   (let ((res (math-compare a (nth 2 b))) res2)
	     (cond ((= res -1)
		    0)
		   ((and (= res 0)
			 (or (/= (nth 1 b) 2)
			     (Math-lessp (nth 2 b) (nth 3 b))))
		    (if (memq (nth 1 b) '(2 3)) 1 0))
		   ((= (setq res2 (math-compare a (nth 3 b))) 1)
		    0)
		   ((and (= res2 0)
			 (or (/= (nth 1 b) 1)
			     (Math-lessp (nth 2 b) (nth 3 b))))
		    (if (memq (nth 1 b) '(1 3)) 1 0))
		   ((/= res 1)
		    nil)
		   ((/= res2 -1)
		    nil)
		   (t 1))))
      (and (Math-equal a b)
	   1)
      (and (math-constp a) (math-constp b)
	   0)
      (list 'calcFunc-in a b))
)

(defun calcFunc-typeof (a)
  (cond ((Math-integerp a) 1)
	((eq (car a) 'frac) 2)
	((eq (car a) 'float) 3)
	((eq (car a) 'hms) 4)
	((eq (car a) 'cplx) 5)
	((eq (car a) 'polar) 6)
	((eq (car a) 'sdev) 7)
	((eq (car a) 'intv) 8)
	((eq (car a) 'mod) 9)
	((eq (car a) 'date) (if (Math-integerp (nth 1 a)) 10 11))
	((eq (car a) 'var)
	 (if (memq (nth 2 a) '(var-inf var-uinf var-nan)) 12 100))
	((eq (car a) 'vec) (if (math-matrixp a) 102 101))
	(t (math-calcFunc-to-var func)))
)

(defun calcFunc-integer (a)
  (if (Math-integerp a)
      1
    (if (Math-objvecp a)
	0
      (list 'calcFunc-integer a)))
)

(defun calcFunc-real (a)
  (if (Math-realp a)
      1
    (if (Math-objvecp a)
	0
      (list 'calcFunc-real a)))
)

(defun calcFunc-constant (a)
  (if (math-constp a)
      1
    (if (Math-objvecp a)
	0
      (list 'calcFunc-constant a)))
)

(defun calcFunc-refers (a b)
  (if (math-expr-contains a b)
      1
    (if (eq (car-safe a) 'var)
	(list 'calcFunc-refers a b)
      0))
)

(defun calcFunc-negative (a)
  (if (math-looks-negp a)
      1
    (if (or (math-zerop a)
	    (math-posp a))
	0
      (list 'calcFunc-negative a)))
)

(defun calcFunc-variable (a)
  (if (eq (car-safe a) 'var)
      1
    (if (Math-objvecp a)
	0
      (list 'calcFunc-variable a)))
)

(defun calcFunc-nonvar (a)
  (if (eq (car-safe a) 'var)
      (list 'calcFunc-nonvar a)
    1)
)

(defun calcFunc-istrue (a)
  (if (math-is-true a)
      1
    0)
)




;;;; User-programmability.

;;; Compiling Lisp-like forms to use the math library.

(defun math-do-defmath (func args body)
  (calc-need-macros)
  (let* ((fname (intern (concat "calcFunc-" (symbol-name func))))
	 (doc (if (stringp (car body)) (list (car body))))
	 (clargs (mapcar 'math-clean-arg args))
	 (body (math-define-function-body
		(if (stringp (car body)) (cdr body) body)
		clargs)))
    (list 'progn
	  (if (and (consp (car body))
		   (eq (car (car body)) 'interactive))
	      (let ((inter (car body)))
		(setq body (cdr body))
		(if (or (> (length inter) 2)
			(integerp (nth 1 inter)))
		    (let ((hasprefix nil) (hasmulti nil))
		      (if (stringp (nth 1 inter))
			  (progn
			    (cond ((equal (nth 1 inter) "p")
				   (setq hasprefix t))
				  ((equal (nth 1 inter) "m")
				   (setq hasmulti t))
				  (t (error
				      "Can't handle interactive code string \"%s\""
				      (nth 1 inter))))
			    (setq inter (cdr inter))))
		      (if (not (integerp (nth 1 inter)))
			  (error
			   "Expected an integer in interactive specification"))
		      (append (list 'defun
				    (intern (concat "calc-"
						    (symbol-name func)))
				    (if (or hasprefix hasmulti)
					'(&optional n)
				      ()))
			      doc
			      (if (or hasprefix hasmulti)
				  '((interactive "P"))
				'((interactive)))
			      (list
			       (append
				'(calc-slow-wrapper)
				(and hasmulti
				     (list
				      (list 'setq
					    'n
					    (list 'if
						  'n
						  (list 'prefix-numeric-value
							'n)
						  (nth 1 inter)))))
				(list
				 (list 'calc-enter-result
				       (if hasmulti 'n (nth 1 inter))
				       (nth 2 inter)
				       (if hasprefix
					   (list 'append
						 (list 'quote (list fname))
						 (list 'calc-top-list-n
						       (nth 1 inter))
						 (list 'and
						       'n
						       (list
							'list
							(list
							 'math-normalize
							 (list
							  'prefix-numeric-value
							  'n)))))
					 (list 'cons
					       (list 'quote fname)
					       (list 'calc-top-list-n
						     (if hasmulti
							 'n
						       (nth 1 inter)))))))))))
		  (append (list 'defun
				(intern (concat "calc-" (symbol-name func)))
				args)
			  doc
			  (list
			   inter
			   (cons 'calc-wrapper body))))))
	  (append (list 'defun fname clargs)
		  doc
		  (math-do-arg-list-check args nil nil)
		  body)))
)

(defun math-clean-arg (arg)
  (if (consp arg)
      (math-clean-arg (nth 1 arg))
    arg)
)

(defun math-do-arg-check (arg var is-opt is-rest)
  (if is-opt
      (let ((chk (math-do-arg-check arg var nil nil)))
	(list (cons 'and
		    (cons var
			  (if (cdr chk)
			      (setq chk (list (cons 'progn chk)))
			    chk)))))
    (and (consp arg)
	 (let* ((rest (math-do-arg-check (nth 1 arg) var is-opt is-rest))
		(qual (car arg))
		(qqual (list 'quote qual))
		(qual-name (symbol-name qual))
		(chk (intern (concat "math-check-" qual-name))))
	   (if (fboundp chk)
	       (append rest
		       (list
			(if is-rest
			    (list 'setq var
				  (list 'mapcar (list 'quote chk) var))
			  (list 'setq var (list chk var)))))
	     (if (fboundp (setq chk (intern (concat "math-" qual-name))))
		 (append rest
			 (list
			  (if is-rest
			      (list 'mapcar
				    (list 'function
					  (list 'lambda '(x)
						(list 'or
						      (list chk 'x)
						      (list 'math-reject-arg
							    'x qqual))))
				    var)
			    (list 'or
				  (list chk var)
				  (list 'math-reject-arg var qqual)))))
	       (if (and (string-match "\\`not-\\(.*\\)\\'" qual-name)
			(fboundp (setq chk (intern
					    (concat "math-"
						    (math-match-substring
						     qual-name 1))))))
		   (append rest
			   (list
			    (if is-rest
				(list 'mapcar
				      (list 'function
					    (list 'lambda '(x)
						  (list 'and
							(list chk 'x)
							(list 'math-reject-arg
							      'x qqual))))
				      var)
			      (list 'and
				    (list chk var)
				    (list 'math-reject-arg var qqual)))))
		 (error "Unknown qualifier `%s'" qual-name)))))))
)

(defun math-do-arg-list-check (args is-opt is-rest)
  (cond ((null args) nil)
	((consp (car args))
	 (append (math-do-arg-check (car args)
				    (math-clean-arg (car args))
				    is-opt is-rest)
		 (math-do-arg-list-check (cdr args) is-opt is-rest)))
	((eq (car args) '&optional)
	 (math-do-arg-list-check (cdr args) t nil))
	((eq (car args) '&rest)
	 (math-do-arg-list-check (cdr args) nil t))
	(t (math-do-arg-list-check (cdr args) is-opt is-rest)))
)

(defconst math-prim-funcs
  '( (~= . math-nearly-equal)
     (% . math-mod)
     (lsh . calcFunc-lsh)
     (ash . calcFunc-ash)
     (logand . calcFunc-and)
     (logandc2 . calcFunc-diff)
     (logior . calcFunc-or)
     (logxor . calcFunc-xor)
     (lognot . calcFunc-not)
     (equal . equal)   ; need to leave these ones alone!
     (eq . eq)
     (and . and)
     (or . or)
     (if . if)
     (^ . math-pow)
     (expt . math-pow)
   )
)

(defconst math-prim-vars
  '( (nil . nil)
     (t . t)
     (&optional . &optional)
     (&rest . &rest)
   )
)

(defun math-define-function-body (body env)
  (let ((body (math-define-body body env)))
    (if (math-body-refers-to body 'math-return)
	(list (cons 'catch (cons '(quote math-return) body)))
      body))
)

(defun math-define-body (body exp-env)
  (math-define-list body)
)

(defun math-define-list (body &optional quote)
  (cond ((null body)
	 nil)
	((and (eq (car body) ':)
	      (stringp (nth 1 body)))
	 (cons (let* ((math-read-expr-quotes t)
		      (exp (math-read-plain-expr (nth 1 body) t)))
		 (math-define-exp exp))
	       (math-define-list (cdr (cdr body)))))
	(quote
	 (cons (cond ((consp (car body))
		      (math-define-list (cdr body) t))
		     (t
		      (car body)))
	       (math-define-list (cdr body))))
	(t
	 (cons (math-define-exp (car body))
	       (math-define-list (cdr body)))))
)

(defun math-define-exp (exp)
  (cond ((consp exp)
	 (let ((func (car exp)))
	   (cond ((memq func '(quote function))
		  (if (and (consp (nth 1 exp))
			   (eq (car (nth 1 exp)) 'lambda))
		      (cons 'quote
			    (math-define-lambda (nth 1 exp) exp-env))
		    exp))
		 ((memq func '(let let* for foreach))
		  (let ((head (nth 1 exp))
			(body (cdr (cdr exp))))
		    (if (memq func '(let let*))
			()
		      (setq func (cdr (assq func '((for . math-for)
						   (foreach . math-foreach)))))
		      (if (not (listp (car head)))
			  (setq head (list head))))
		    (macroexpand
		     (cons func
			   (cons (math-define-let head)
				 (math-define-body body
						   (nconc
						    (math-define-let-env head)
						    exp-env)))))))
		 ((and (memq func '(setq setf))
		       (math-complicated-lhs (cdr exp)))
		  (if (> (length exp) 3)
		      (cons 'progn (math-define-setf-list (cdr exp)))
		    (math-define-setf (nth 1 exp) (nth 2 exp))))
		 ((eq func 'condition-case)
		  (cons func
			(cons (nth 1 exp)
			      (math-define-body (cdr (cdr exp))
						(cons (nth 1 exp)
						      exp-env)))))
		 ((eq func 'cond)
		  (cons func
			(math-define-cond (cdr exp))))
		 ((and (consp func)   ; ('spam a b) == force use of plain spam
		       (eq (car func) 'quote))
		  (cons func (math-define-list (cdr exp))))
		 ((symbolp func)
		  (let ((args (math-define-list (cdr exp)))
			(prim (assq func math-prim-funcs)))
		    (cond (prim
			   (cons (cdr prim) args))
			  ((eq func 'floatp)
			   (list 'eq (car args) '(quote float)))
			  ((eq func '+)
			   (math-define-binop 'math-add 0
					      (car args) (cdr args)))
			  ((eq func '-)
			   (if (= (length args) 1)
			       (cons 'math-neg args)
			     (math-define-binop 'math-sub 0
						(car args) (cdr args))))
			  ((eq func '*)
			   (math-define-binop 'math-mul 1
					      (car args) (cdr args)))
			  ((eq func '/)
			   (math-define-binop 'math-div 1
					      (car args) (cdr args)))
			  ((eq func 'min)
			   (math-define-binop 'math-min 0
					      (car args) (cdr args)))
			  ((eq func 'max)
			   (math-define-binop 'math-max 0
					      (car args) (cdr args)))
			  ((eq func '<)
			   (if (and (math-numberp (nth 1 args))
				    (math-zerop (nth 1 args)))
			       (list 'math-negp (car args))
			     (cons 'math-lessp args)))
			  ((eq func '>)
			   (if (and (math-numberp (nth 1 args))
				    (math-zerop (nth 1 args)))
			       (list 'math-posp (car args))
			     (list 'math-lessp (nth 1 args) (nth 0 args))))
			  ((eq func '<=)
			   (list 'not
				 (if (and (math-numberp (nth 1 args))
					  (math-zerop (nth 1 args)))
				     (list 'math-posp (car args))
				   (list 'math-lessp
					 (nth 1 args) (nth 0 args)))))
			  ((eq func '>=)
			   (list 'not
				 (if (and (math-numberp (nth 1 args))
					  (math-zerop (nth 1 args)))
				     (list 'math-negp (car args))
				   (cons 'math-lessp args))))
			  ((eq func '=)
			   (if (and (math-numberp (nth 1 args))
				    (math-zerop (nth 1 args)))
			       (list 'math-zerop (nth 0 args))
			     (if (and (integerp (nth 1 args))
				      (/= (% (nth 1 args) 10) 0))
				 (cons 'math-equal-int args)
			       (cons 'math-equal args))))
			  ((eq func '/=)
			   (list 'not
				 (if (and (math-numberp (nth 1 args))
					  (math-zerop (nth 1 args)))
				     (list 'math-zerop (nth 0 args))
				   (if (and (integerp (nth 1 args))
					    (/= (% (nth 1 args) 10) 0))
				       (cons 'math-equal-int args)
				     (cons 'math-equal args)))))
			  ((eq func '1+)
			   (list 'math-add (car args) 1))
			  ((eq func '1-)
			   (list 'math-add (car args) -1))
			  ((eq func 'not)   ; optimize (not (not x)) => x
			   (if (eq (car-safe args) func)
			       (car (nth 1 args))
			     (cons func args)))
			  ((and (eq func 'elt) (cdr (cdr args)))
			   (math-define-elt (car args) (cdr args)))
			  (t
			   (macroexpand
			    (let* ((name (symbol-name func))
				   (cfunc (intern (concat "calcFunc-" name)))
				   (mfunc (intern (concat "math-" name))))
			      (cond ((fboundp cfunc)
				     (cons cfunc args))
				    ((fboundp mfunc)
				     (cons mfunc args))
				    ((or (fboundp func)
					 (string-match "\\`calcFunc-.*" name))
				     (cons func args))
				    (t
				     (cons cfunc args)))))))))
		 (t (cons func args)))))
	((symbolp exp)
	 (let ((prim (assq exp math-prim-vars))
	       (name (symbol-name exp)))
	   (cond (prim
		  (cdr prim))
		 ((memq exp exp-env)
		  exp)
		 ((string-match "-" name)
		  exp)
		 (t
		  (intern (concat "var-" name))))))
	((integerp exp)
	 (if (or (<= exp -1000000) (>= exp 1000000))
	     (list 'quote (math-normalize exp))
	   exp))
	(t exp))
)

(defun math-define-cond (forms)
  (and forms
       (cons (math-define-list (car forms))
	     (math-define-cond (cdr forms))))
)

(defun math-complicated-lhs (body)
  (and body
       (or (not (symbolp (car body)))
	   (math-complicated-lhs (cdr (cdr body)))))
)

(defun math-define-setf-list (body)
  (and body
       (cons (math-define-setf (nth 0 body) (nth 1 body))
	     (math-define-setf-list (cdr (cdr body)))))
)

(defun math-define-setf (place value)
  (setq place (math-define-exp place)
	value (math-define-exp value))
  (cond ((symbolp place)
	 (list 'setq place value))
	((eq (car-safe place) 'nth)
	 (list 'setcar (list 'nthcdr (nth 1 place) (nth 2 place)) value))
	((eq (car-safe place) 'elt)
	 (list 'setcar (list 'nthcdr (nth 2 place) (nth 1 place)) value))
	((eq (car-safe place) 'car)
	 (list 'setcar (nth 1 place) value))
	((eq (car-safe place) 'cdr)
	 (list 'setcdr (nth 1 place) value))
	(t
	 (error "Bad place form for setf: %s" place)))
)

(defun math-define-binop (op ident arg1 rest)
  (if rest
      (math-define-binop op ident
			 (list op arg1 (car rest))
			 (cdr rest))
    (or arg1 ident))
)

(defun math-define-let (vlist)
  (and vlist
       (cons (if (consp (car vlist))
		 (cons (car (car vlist))
		       (math-define-list (cdr (car vlist))))
	       (car vlist))
	     (math-define-let (cdr vlist))))
)

(defun math-define-let-env (vlist)
  (and vlist
       (cons (if (consp (car vlist))
		 (car (car vlist))
	       (car vlist))
	     (math-define-let-env (cdr vlist))))
)

(defun math-define-lambda (exp exp-env)
  (nconc (list (nth 0 exp)   ; 'lambda
	       (nth 1 exp))  ; arg list
	 (math-define-function-body (cdr (cdr exp))
				    (append (nth 1 exp) exp-env)))
)

(defun math-define-elt (seq idx)
  (if idx
      (math-define-elt (list 'elt seq (car idx)) (cdr idx))
    seq)
)



;;; Useful programming macros.

(defmacro math-while (head &rest body)
  (let ((body (cons 'while (cons head body))))
    (if (math-body-refers-to body 'math-break)
	(cons 'catch (cons '(quote math-break) (list body)))
      body))
)


(defmacro math-for (head &rest body)
  (let ((body (if head
		  (math-handle-for head body)
		(cons 'while (cons t body)))))
    (if (math-body-refers-to body 'math-break)
	(cons 'catch (cons '(quote math-break) (list body)))
      body))
)

(defun math-handle-for (head body)
  (let* ((var (nth 0 (car head)))
	 (init (nth 1 (car head)))
	 (limit (nth 2 (car head)))
	 (step (or (nth 3 (car head)) 1))
	 (body (if (cdr head)
		   (list (math-handle-for (cdr head) body))
		 body))
	 (all-ints (and (integerp init) (integerp limit) (integerp step)))
	 (const-limit (or (integerp limit)
			  (and (eq (car-safe limit) 'quote)
			       (math-realp (nth 1 limit)))))
	 (const-step (or (integerp step)
			 (and (eq (car-safe step) 'quote)
			      (math-realp (nth 1 step)))))
	 (save-limit (if const-limit limit (make-symbol "<limit>")))
	 (save-step (if const-step step (make-symbol "<step>"))))
    (cons 'let
	  (cons (append (if const-limit nil (list (list save-limit limit)))
			(if const-step nil (list (list save-step step)))
			(list (list var init)))
		(list
		 (cons 'while
		       (cons (if all-ints
				 (if (> step 0)
				     (list '<= var save-limit)
				   (list '>= var save-limit))
			       (list 'not
				     (if const-step
					 (if (or (math-posp step)
						 (math-posp
						  (cdr-safe step)))
					     (list 'math-lessp
						   save-limit
						   var)
					   (list 'math-lessp
						 var
						 save-limit))
				       (list 'if
					     (list 'math-posp
						   save-step)
					     (list 'math-lessp
						   save-limit
						   var)
					     (list 'math-lessp
						   var
						   save-limit)))))
			     (append body
				     (list (list 'setq
						 var
						 (list (if all-ints
							   '+
							 'math-add)
						       var
						       save-step))))))))))
)


(defmacro math-foreach (head &rest body)
  (let ((body (math-handle-foreach head body)))
    (if (math-body-refers-to body 'math-break)
	(cons 'catch (cons '(quote math-break) (list body)))
      body))
)


(defun math-handle-foreach (head body)
  (let ((var (nth 0 (car head)))
	(data (nth 1 (car head)))
	(body (if (cdr head)
		  (list (math-handle-foreach (cdr head) body))
		body)))
    (cons 'let
	  (cons (list (list var data))
		(list
		 (cons 'while
		       (cons var
			     (append body
				     (list (list 'setq
						 var
						 (list 'cdr var))))))))))
)


(defun math-body-refers-to (body thing)
  (or (equal body thing)
      (and (consp body)
	   (or (math-body-refers-to (car body) thing)
	       (math-body-refers-to (cdr body) thing))))
)

(defun math-break (&optional value)
  (throw 'math-break value)
)

(defun math-return (&optional value)
  (throw 'math-return value)
)





(defun math-composite-inequalities (x op)
  (if (memq (nth 1 op) '(calcFunc-eq calcFunc-neq))
      (if (eq (car x) (nth 1 op))
	  (append x (list (math-read-expr-level (nth 3 op))))
	(throw 'syntax "Syntax error"))
    (list 'calcFunc-in
	  (nth 2 x)
	  (if (memq (nth 1 op) '(calcFunc-lt calcFunc-leq))
	      (if (memq (car x) '(calcFunc-lt calcFunc-leq))
		  (math-make-intv
		   (+ (if (eq (car x) 'calcFunc-leq) 2 0)
		      (if (eq (nth 1 op) 'calcFunc-leq) 1 0))
		   (nth 1 x) (math-read-expr-level (nth 3 op)))
		(throw 'syntax "Syntax error"))
	    (if (memq (car x) '(calcFunc-gt calcFunc-geq))
		(math-make-intv
		 (+ (if (eq (nth 1 op) 'calcFunc-geq) 2 0)
		    (if (eq (car x) 'calcFunc-geq) 1 0))
		 (math-read-expr-level (nth 3 op)) (nth 1 x))
	      (throw 'syntax "Syntax error")))))
)

