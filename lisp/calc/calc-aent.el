;; Calculator for GNU Emacs, part I [calc-aent.el]
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



;; This file is autoloaded from calc.el.
(require 'calc)

(require 'calc-macs)
(eval-when-compile '(require calc-macs))

(defun calc-Need-calc-aent () nil)


(defun calc-do-quick-calc ()
  (calc-check-defines)
  (if (eq major-mode 'calc-mode)
      (calc-algebraic-entry t)
    (let (buf shortbuf)
      (save-excursion
	(calc-create-buffer)
	(let* ((calc-command-flags nil)
	       (calc-dollar-values calc-quick-prev-results)
	       (calc-dollar-used 0)
	       (enable-recursive-minibuffers t)
	       (calc-language (if (memq calc-language '(nil big))
				  'flat calc-language))
	       (entry (calc-do-alg-entry "" "Quick calc: " t))
	       (alg-exp (mapcar (function
				 (lambda (x)
				   (if (and (not calc-extensions-loaded)
					    calc-previous-alg-entry
					    (string-match
					     "\\`[-0-9._+*/^() ]+\\'"
					     calc-previous-alg-entry))
				       (calc-normalize x)
				     (calc-extensions)
				     (math-evaluate-expr x))))
				entry)))
	  (if (and (= (length alg-exp) 1)
		   (eq (car-safe (car alg-exp)) 'calcFunc-assign)
		   (= (length (car alg-exp)) 3)
		   (eq (car-safe (nth 1 (car alg-exp))) 'var))
	      (progn
		(calc-extensions)
		(set (nth 2 (nth 1 (car alg-exp))) (nth 2 (car alg-exp)))
		(calc-refresh-evaltos (nth 2 (nth 1 (car alg-exp))))
		(setq alg-exp (list (nth 2 (car alg-exp))))))
	  (setq calc-quick-prev-results alg-exp
		buf (mapconcat (function (lambda (x)
					   (math-format-value x 1000)))
			       alg-exp
			       " ")
		shortbuf buf)
	  (if (and (= (length alg-exp) 1)
		   (memq (car-safe (car alg-exp)) '(nil bigpos bigneg))
		   (< (length buf) 20)
		   (= calc-number-radix 10))
	      (setq buf (concat buf "  ("
				(let ((calc-number-radix 16))
				  (math-format-value (car alg-exp) 1000))
				", "
				(let ((calc-number-radix 8))
				  (math-format-value (car alg-exp) 1000))
				(if (and (integerp (car alg-exp))
					 (> (car alg-exp) 0)
					 (< (car alg-exp) 127))
				    (format ", \"%c\"" (car alg-exp))
				  "")
				")")))
	  (if (and (< (length buf) (frame-width)) (= (length entry) 1)
		   calc-extensions-loaded)
	      (let ((long (concat (math-format-value (car entry) 1000)
				  " =>  " buf)))
		(if (<= (length long) (- (frame-width) 8))
		    (setq buf long))))
	  (calc-handle-whys)
	  (message "Result: %s" buf)))
      (if (eq last-command-char 10)
	  (insert shortbuf)
	(setq kill-ring (cons shortbuf kill-ring))
	(if (> (length kill-ring) kill-ring-max)
	    (setcdr (nthcdr (1- kill-ring-max) kill-ring) nil))
	(setq kill-ring-yank-pointer kill-ring)))))

(defun calc-do-calc-eval (str separator args)
  (calc-check-defines)
  (catch 'calc-error
    (save-excursion
      (calc-create-buffer)
      (cond
       ((and (consp str) (not (symbolp (car str))))
	(let ((calc-language nil)
	      (math-expr-opers math-standard-opers)
	      (calc-internal-prec 12)
	      (calc-word-size 32)
	      (calc-symbolic-mode nil)
	      (calc-matrix-mode nil)
	      (calc-angle-mode 'deg)
	      (calc-number-radix 10)
	      (calc-leading-zeros nil)
	      (calc-group-digits nil)
	      (calc-point-char ".")
	      (calc-frac-format '(":" nil))
	      (calc-prefer-frac nil)
	      (calc-hms-format "%s@ %s' %s\"")
	      (calc-date-format '((H ":" mm C SS pp " ")
				  Www " " Mmm " " D ", " YYYY))
	      (calc-float-format '(float 0))
	      (calc-full-float-format '(float 0))
	      (calc-complex-format nil)
	      (calc-matrix-just nil)
	      (calc-full-vectors t)
	      (calc-break-vectors nil)
	      (calc-vector-commas ",")
	      (calc-vector-brackets "[]")
	      (calc-matrix-brackets '(R O))
	      (calc-complex-mode 'cplx)
	      (calc-infinite-mode nil)
	      (calc-display-strings nil)
	      (calc-simplify-mode nil)
	      (calc-display-working-message 'lots)
	      (strp (cdr str)))
	  (while strp
	    (set (car strp) (nth 1 strp))
	    (setq strp (cdr (cdr strp))))
	  (calc-do-calc-eval (car str) separator args)))
       ((eq separator 'eval)
	(eval str))
       ((eq separator 'macro)
	(calc-extensions)
	(let* ((calc-buffer (current-buffer))
	       (calc-window (get-buffer-window calc-buffer))
	       (save-window (selected-window)))
	  (if calc-window
	      (unwind-protect
		  (progn
		    (select-window calc-window)
		    (calc-execute-kbd-macro str nil (car args)))
		(and (window-point save-window)
		     (select-window save-window)))
	    (save-window-excursion
	      (select-window (get-largest-window))
	      (switch-to-buffer calc-buffer)
	      (calc-execute-kbd-macro str nil (car args)))))
	nil)
       ((eq separator 'pop)
	(or (not (integerp str))
	    (= str 0)
	    (calc-pop (min str (calc-stack-size))))
	(calc-stack-size))
       ((eq separator 'top)
	(and (integerp str)
	     (> str 0)
	     (<= str (calc-stack-size))
	     (math-format-value (calc-top-n str (car args)) 1000)))
       ((eq separator 'rawtop)
	(and (integerp str)
	     (> str 0)
	     (<= str (calc-stack-size))
	     (calc-top-n str (car args))))
       (t
	(let* ((calc-command-flags nil)
	       (calc-next-why nil)
	       (calc-language (if (memq calc-language '(nil big))
				  'flat calc-language))
	       (calc-dollar-values (mapcar
				    (function
				     (lambda (x)
				       (if (stringp x)
					   (progn
					     (setq x (math-read-exprs x))
					     (if (eq (car-safe x)
						     'error)
						 (throw 'calc-error
							(calc-eval-error
							 (cdr x)))
					       (car x)))
					 x)))
				    args))
	       (calc-dollar-used 0)
	       (res (if (stringp str)
			(math-read-exprs str)
		      (list str)))
	       buf)
	  (if (eq (car res) 'error)
	      (calc-eval-error (cdr res))
	    (setq res (mapcar 'calc-normalize res))
	    (and (memq 'clear-message calc-command-flags)
		 (message ""))
	    (cond ((eq separator 'pred)
		   (calc-extensions)
		   (if (= (length res) 1)
		       (math-is-true (car res))
		     (calc-eval-error '(0 "Single value expected"))))
		  ((eq separator 'raw)
		   (if (= (length res) 1)
		       (car res)
		     (calc-eval-error '(0 "Single value expected"))))
		  ((eq separator 'list)
		   res)
		  ((memq separator '(num rawnum))
		   (if (= (length res) 1)
		       (if (math-constp (car res))
			   (if (eq separator 'num)
			       (math-format-value (car res) 1000)
			     (car res))
			 (calc-eval-error
			  (list 0
				(if calc-next-why
				    (calc-explain-why (car calc-next-why))
				  "Number expected"))))
		     (calc-eval-error '(0 "Single value expected"))))
		  ((eq separator 'push)
		   (calc-push-list res)
		   nil)
		  (t (while res
		       (setq buf (concat buf
					 (and buf (or separator ", "))
					 (math-format-value (car res) 1000))
			     res (cdr res)))
		     buf)))))))))

(defun calc-eval-error (msg)
  (if (and (boundp 'calc-eval-error)
	   calc-eval-error)
      (if (eq calc-eval-error 'string)
	  (nth 1 msg)
	(error "%s" (nth 1 msg)))
    msg))


;;;; Reading an expression in algebraic form.

(defun calc-auto-algebraic-entry (&optional prefix)
  (interactive "P")
  (calc-algebraic-entry prefix t))

(defun calc-algebraic-entry (&optional prefix auto)
  (interactive "P")
  (calc-wrapper
   (let ((calc-language (if prefix nil calc-language))
	 (math-expr-opers (if prefix math-standard-opers math-expr-opers)))
     (calc-alg-entry (and auto (char-to-string last-command-char))))))

(defun calc-alg-entry (&optional initial prompt)
  (let* ((sel-mode nil)
	 (calc-dollar-values (mapcar 'calc-get-stack-element
				     (nthcdr calc-stack-top calc-stack)))
	 (calc-dollar-used 0)
	 (calc-plain-entry t)
	 (alg-exp (calc-do-alg-entry initial prompt t)))
    (if (stringp alg-exp)
	(progn
	  (calc-extensions)
	  (calc-alg-edit alg-exp))
      (let* ((calc-simplify-mode (if (eq last-command-char ?\C-j)
				     'none
				   calc-simplify-mode))
	     (nvals (mapcar 'calc-normalize alg-exp)))
	(while alg-exp
	  (calc-record (if calc-extensions-loaded (car alg-exp) (car nvals))
		       "alg'")
	  (calc-pop-push-record-list calc-dollar-used
				     (and (not (equal (car alg-exp)
						      (car nvals)))
					  calc-extensions-loaded
					  "")
				     (list (car nvals)))
	  (setq alg-exp (cdr alg-exp)
		nvals (cdr nvals)
		calc-dollar-used 0)))
      (calc-handle-whys))))

(defun calc-do-alg-entry (&optional initial prompt no-normalize)
  (let* ((calc-buffer (current-buffer))
	 (blink-paren-function 'calcAlg-blink-matching-open)
	 (alg-exp 'error))
    (if (boundp 'calc-alg-ent-map)
	()
      (setq calc-alg-ent-map (copy-keymap minibuffer-local-map))
      (define-key calc-alg-ent-map "'" 'calcAlg-previous)
      (define-key calc-alg-ent-map "`" 'calcAlg-edit)
      (define-key calc-alg-ent-map "\C-m" 'calcAlg-enter)
      (define-key calc-alg-ent-map "\C-j" 'calcAlg-enter)
      (or calc-emacs-type-19
	  (let ((i 33))
	    (setq calc-alg-ent-esc-map (copy-sequence esc-map))
	    (while (< i 127)
	      (aset calc-alg-ent-esc-map i 'calcAlg-escape)
	      (setq i (1+ i))))))
    (or calc-emacs-type-19
	(define-key calc-alg-ent-map "\e" nil))
    (if (eq calc-algebraic-mode 'total)
	(define-key calc-alg-ent-map "\e" calc-alg-ent-esc-map)
      (define-key calc-alg-ent-map "\ep" 'calcAlg-plus-minus)
      (define-key calc-alg-ent-map "\em" 'calcAlg-mod)
      (define-key calc-alg-ent-map "\e=" 'calcAlg-equals)
      (define-key calc-alg-ent-map "\e\r" 'calcAlg-equals)
      (define-key calc-alg-ent-map "\e%" 'self-insert-command))
    (setq calc-aborted-prefix nil)
    (let ((buf (read-from-minibuffer (or prompt "Algebraic: ")
				     (or initial "")
				     calc-alg-ent-map nil)))
      (if (eq alg-exp 'error)
	  (if (eq (car-safe (setq alg-exp (math-read-exprs buf))) 'error)
	      (setq alg-exp nil)))
      (setq calc-aborted-prefix "alg'")
      (or no-normalize
	  (and alg-exp (setq alg-exp (mapcar 'calc-normalize alg-exp))))
      alg-exp)))

(defun calcAlg-plus-minus ()
  (interactive)
  (if (calc-minibuffer-contains ".* \\'")
      (insert "+/- ")
    (insert " +/- ")))

(defun calcAlg-mod ()
  (interactive)
  (if (not (calc-minibuffer-contains ".* \\'"))
      (insert " "))
  (if (calc-minibuffer-contains ".* mod +\\'")
      (if calc-previous-modulo
	  (insert (math-format-flat-expr calc-previous-modulo 0))
	(beep))
    (insert "mod ")))

(defun calcAlg-previous ()
  (interactive)
  (if (calc-minibuffer-contains "\\`\\'")
      (if calc-previous-alg-entry
	  (insert calc-previous-alg-entry)
	(beep))
    (insert "'")))

(defun calcAlg-equals ()
  (interactive)
  (unwind-protect
      (calcAlg-enter)
    (if (consp alg-exp)
	(progn (setq prefix-arg (length alg-exp))
	       (calc-unread-command ?=)))))

(defun calcAlg-escape ()
  (interactive)
  (calc-unread-command)
  (save-excursion
    (calc-select-buffer)
    (use-local-map calc-mode-map))
  (calcAlg-enter))

(defun calcAlg-edit ()
  (interactive)
  (if (or (not calc-plain-entry)
	  (calc-minibuffer-contains
	   "\\`\\([^\"]*\"[^\"]*\"\\)*[^\"]*\"[^\"]*\\'"))
      (insert "`")
    (setq alg-exp (minibuffer-contents))
    (and (> (length alg-exp) 0) (setq calc-previous-alg-entry alg-exp))
    (exit-minibuffer)))
(setq calc-plain-entry nil)

(defun calcAlg-enter ()
  (interactive)
  (let* ((str (minibuffer-contents))
	 (exp (and (> (length str) 0)
		   (save-excursion
		     (set-buffer calc-buffer)
		     (math-read-exprs str)))))
    (if (eq (car-safe exp) 'error)
	(progn
	  (goto-char (minibuffer-prompt-end))
	  (forward-char (nth 1 exp))
	  (beep)
	  (calc-temp-minibuffer-message
	   (concat " [" (or (nth 2 exp) "Error") "]"))
	  (calc-clear-unread-commands))
      (setq alg-exp (if (calc-minibuffer-contains "\\` *\\[ *\\'")
			'((incomplete vec))
		      exp))
      (and (> (length str) 0) (setq calc-previous-alg-entry str))
      (exit-minibuffer))))

(defun calcAlg-blink-matching-open ()
  (let ((oldpos (point))
	(blinkpos nil))
    (save-excursion
      (condition-case ()
	  (setq blinkpos (scan-sexps oldpos -1))
	(error nil)))
    (if (and blinkpos
	     (> oldpos (1+ (point-min)))
	     (or (and (= (char-after (1- oldpos)) ?\))
		      (= (char-after blinkpos) ?\[))
		 (and (= (char-after (1- oldpos)) ?\])
		      (= (char-after blinkpos) ?\()))
	     (save-excursion
	       (goto-char blinkpos)
	       (looking-at ".+\\(\\.\\.\\|\\\\dots\\|\\\\ldots\\)")))
	(let ((saved (aref (syntax-table) (char-after blinkpos))))
	  (unwind-protect
	      (progn
		(aset (syntax-table) (char-after blinkpos)
		      (+ (logand saved 255)
			 (lsh (char-after (1- oldpos)) 8)))
		(blink-matching-open))
	    (aset (syntax-table) (char-after blinkpos) saved)))
      (blink-matching-open))))


(defun calc-alg-digit-entry ()
  (calc-alg-entry 
   (cond ((eq last-command-char ?e)
	  (if (> calc-number-radix 14) (format "%d.^" calc-number-radix) "1e"))
	 ((eq last-command-char ?#) (format "%d#" calc-number-radix))
	 ((eq last-command-char ?_) "-")
	 ((eq last-command-char ?@) "0@ ")
	 (t (char-to-string last-command-char)))))

(defun calcDigit-algebraic ()
  (interactive)
  (if (calc-minibuffer-contains ".*[@oh] *[^'m ]+[^'m]*\\'")
      (calcDigit-key)
    (setq calc-digit-value (minibuffer-contents))
    (exit-minibuffer)))

(defun calcDigit-edit ()
  (interactive)
  (calc-unread-command)
  (setq calc-digit-value (minibuffer-contents))
  (exit-minibuffer))


;;; Algebraic expression parsing.   [Public]

(defun math-read-exprs (exp-str)
  (let ((exp-pos 0)
	(exp-old-pos 0)
	(exp-keep-spaces nil)
	exp-token exp-data)
    (if calc-language-input-filter
	(setq exp-str (funcall calc-language-input-filter exp-str)))
    (while (setq exp-token (string-match "\\.\\.\\([^.]\\|.[^.]\\)" exp-str))
      (setq exp-str (concat (substring exp-str 0 exp-token) "\\dots"
			    (substring exp-str (+ exp-token 2)))))
    (math-build-parse-table)
    (math-read-token)
    (let ((val (catch 'syntax (math-read-expr-list))))
      (if (stringp val)
	  (list 'error exp-old-pos val)
	(if (equal exp-token 'end)
	    val
	  (list 'error exp-old-pos "Syntax error"))))))

(defun math-read-expr-list ()
  (let* ((exp-keep-spaces nil)
	 (val (list (math-read-expr-level 0)))
	 (last val))
    (while (equal exp-data ",")
      (math-read-token)
      (let ((rest (list (math-read-expr-level 0))))
	(setcdr last rest)
	(setq last rest)))
    val))

(setq calc-user-parse-table nil)
(setq calc-last-main-parse-table nil)
(setq calc-last-lang-parse-table nil)
(setq calc-user-tokens nil)
(setq calc-user-token-chars nil)

(defun math-build-parse-table ()
  (let ((mtab (cdr (assq nil calc-user-parse-tables)))
	(ltab (cdr (assq calc-language calc-user-parse-tables))))
    (or (and (eq mtab calc-last-main-parse-table)
	     (eq ltab calc-last-lang-parse-table))
	(let ((p (append mtab ltab))
	      (toks nil))
	  (setq calc-user-parse-table p)
	  (setq calc-user-token-chars nil)
	  (while p
	    (math-find-user-tokens (car (car p)))
	    (setq p (cdr p)))
	  (setq calc-user-tokens (mapconcat 'identity
					    (sort (mapcar 'car toks)
						  (function (lambda (x y)
							      (> (length x)
								 (length y)))))
					    "\\|")
		calc-last-main-parse-table mtab
		calc-last-lang-parse-table ltab)))))

(defun math-find-user-tokens (p)   ; uses "toks"
  (while p
    (cond ((and (stringp (car p))
		(or (> (length (car p)) 1) (equal (car p) "$")
		    (equal (car p) "\""))
		(string-match "[^a-zA-Z0-9]" (car p)))
	   (let ((s (regexp-quote (car p))))
	     (if (string-match "\\`[a-zA-Z0-9]" s)
		 (setq s (concat "\\<" s)))
	     (if (string-match "[a-zA-Z0-9]\\'" s)
		 (setq s (concat s "\\>")))
	     (or (assoc s toks)
		 (progn
		   (setq toks (cons (list s) toks))
		   (or (memq (aref (car p) 0) calc-user-token-chars)
		       (setq calc-user-token-chars
			     (cons (aref (car p) 0)
				   calc-user-token-chars)))))))
	  ((consp (car p))
	   (math-find-user-tokens (nth 1 (car p)))
	   (or (eq (car (car p)) '\?)
	       (math-find-user-tokens (nth 2 (car p))))))
    (setq p (cdr p))))

(defun math-read-token ()
  (if (>= exp-pos (length exp-str))
      (setq exp-old-pos exp-pos
	    exp-token 'end
	    exp-data "\000")
    (let ((ch (aref exp-str exp-pos)))
      (setq exp-old-pos exp-pos)
      (cond ((memq ch '(32 10 9))
	     (setq exp-pos (1+ exp-pos))
	     (if exp-keep-spaces
		 (setq exp-token 'space
		       exp-data " ")
	       (math-read-token)))
	    ((and (memq ch calc-user-token-chars)
		  (let ((case-fold-search nil))
		    (eq (string-match calc-user-tokens exp-str exp-pos)
			exp-pos)))
	     (setq exp-token 'punc
		   exp-data (math-match-substring exp-str 0)
		   exp-pos (match-end 0)))
	    ((or (and (>= ch ?a) (<= ch ?z))
		 (and (>= ch ?A) (<= ch ?Z)))
	     (string-match (if (memq calc-language '(c fortran pascal maple))
			       "[a-zA-Z0-9_#]*"
			     "[a-zA-Z0-9'#]*")
			   exp-str exp-pos)
	     (setq exp-token 'symbol
		   exp-pos (match-end 0)
		   exp-data (math-restore-dashes
			     (math-match-substring exp-str 0)))
	     (if (eq calc-language 'eqn)
		 (let ((code (assoc exp-data math-eqn-ignore-words)))
		   (cond ((null code))
			 ((null (cdr code))
			  (math-read-token))
			 ((consp (nth 1 code))
			  (math-read-token)
			  (if (assoc exp-data (cdr code))
			      (setq exp-data (format "%s %s"
						     (car code) exp-data))))
			 ((eq (nth 1 code) 'punc)
			  (setq exp-token 'punc
				exp-data (nth 2 code)))
			 (t
			  (math-read-token)
			  (math-read-token))))))
	    ((or (and (>= ch ?0) (<= ch ?9))
		 (and (eq ch '?\.)
		      (eq (string-match "\\.[0-9]" exp-str exp-pos) exp-pos))
		 (and (eq ch '?_)
		      (eq (string-match "_\\.?[0-9]" exp-str exp-pos) exp-pos)
		      (or (eq exp-pos 0)
			  (and (memq calc-language '(nil flat big unform
							 tex eqn))
			       (eq (string-match "[^])}\"a-zA-Z0-9'$]_"
						 exp-str (1- exp-pos))
				   (1- exp-pos))))))
	     (or (and (eq calc-language 'c)
		      (string-match "0[xX][0-9a-fA-F]+" exp-str exp-pos))
		 (string-match "_?\\([0-9]+.?0*@ *\\)?\\([0-9]+.?0*' *\\)?\\(0*\\([2-9]\\|1[0-4]\\)\\(#\\|\\^\\^\\)[0-9a-dA-D.]+[eE][-+_]?[0-9]+\\|0*\\([2-9]\\|[0-2][0-9]\\|3[0-6]\\)\\(#\\|\\^\\^\\)[0-9a-zA-Z:.]+\\|[0-9]+:[0-9:]+\\|[0-9.]+\\([eE][-+_]?[0-9]+\\)?\"?\\)?" exp-str exp-pos))
	     (setq exp-token 'number
		   exp-data (math-match-substring exp-str 0)
		   exp-pos (match-end 0)))
	    ((eq ch ?\$)
	     (if (and (eq calc-language 'pascal)
		      (eq (string-match
			   "\\(\\$[0-9a-fA-F]+\\)\\($\\|[^0-9a-zA-Z]\\)"
			   exp-str exp-pos)
			  exp-pos))
		 (setq exp-token 'number
		       exp-data (math-match-substring exp-str 1)
		       exp-pos (match-end 1))
	       (if (eq (string-match "\\$\\([1-9][0-9]*\\)" exp-str exp-pos)
		       exp-pos)
		   (setq exp-data (- (string-to-int (math-match-substring
						     exp-str 1))))
		 (string-match "\\$+" exp-str exp-pos)
		 (setq exp-data (- (match-end 0) (match-beginning 0))))
	       (setq exp-token 'dollar
		     exp-pos (match-end 0))))
	    ((eq ch ?\#)
	     (if (eq (string-match "#\\([1-9][0-9]*\\)" exp-str exp-pos)
		     exp-pos)
		 (setq exp-data (string-to-int
				 (math-match-substring exp-str 1))
		       exp-pos (match-end 0))
	       (setq exp-data 1
		     exp-pos (1+ exp-pos)))
	     (setq exp-token 'hash))
	    ((eq (string-match "~=\\|<=\\|>=\\|<>\\|/=\\|\\+/-\\|\\\\dots\\|\\\\ldots\\|\\*\\*\\|<<\\|>>\\|==\\|!=\\|&&&\\||||\\|!!!\\|&&\\|||\\|!!\\|:=\\|::\\|=>"
			       exp-str exp-pos)
		 exp-pos)
	     (setq exp-token 'punc
		   exp-data (math-match-substring exp-str 0)
		   exp-pos (match-end 0)))
	    ((and (eq ch ?\")
		  (string-match "\\(\"\\([^\"\\]\\|\\\\.\\)*\\)\\(\"\\|\\'\\)" exp-str exp-pos))
	     (if (eq calc-language 'eqn)
		 (progn
		   (setq exp-str (copy-sequence exp-str))
		   (aset exp-str (match-beginning 1) ?\{)
		   (if (< (match-end 1) (length exp-str))
		       (aset exp-str (match-end 1) ?\}))
		   (math-read-token))
	       (setq exp-token 'string
		     exp-data (math-match-substring exp-str 1)
		     exp-pos (match-end 0))))
	    ((and (= ch ?\\) (eq calc-language 'tex)
		  (< exp-pos (1- (length exp-str))))
	     (or (string-match "\\\\hbox *{\\([a-zA-Z0-9]+\\)}" exp-str exp-pos)
		 (string-match "\\(\\\\\\([a-zA-Z]+\\|[^a-zA-Z]\\)\\)" exp-str exp-pos))
	     (setq exp-token 'symbol
		   exp-pos (match-end 0)
		   exp-data (math-restore-dashes
			     (math-match-substring exp-str 1)))
	     (let ((code (assoc exp-data math-tex-ignore-words)))
	       (cond ((null code))
		     ((null (cdr code))
		      (math-read-token))
		     ((eq (nth 1 code) 'punc)
		      (setq exp-token 'punc
			    exp-data (nth 2 code)))
		     ((and (eq (nth 1 code) 'mat)
			   (string-match " *{" exp-str exp-pos))
		      (setq exp-pos (match-end 0)
			    exp-token 'punc
			    exp-data "[")
		      (let ((right (string-match "}" exp-str exp-pos)))
			(and right
			     (setq exp-str (copy-sequence exp-str))
			     (aset exp-str right ?\])))))))
	    ((and (= ch ?\.) (eq calc-language 'fortran)
		  (eq (string-match "\\.[a-zA-Z][a-zA-Z][a-zA-Z]?\\."
				    exp-str exp-pos) exp-pos))
	     (setq exp-token 'punc
		   exp-data (upcase (math-match-substring exp-str 0))
		   exp-pos (match-end 0)))
	    ((and (eq calc-language 'math)
		  (eq (string-match "\\[\\[\\|->\\|:>" exp-str exp-pos)
		      exp-pos))
	     (setq exp-token 'punc
		   exp-data (math-match-substring exp-str 0)
		   exp-pos (match-end 0)))
	    ((and (eq calc-language 'eqn)
		  (eq (string-match "->\\|<-\\|+-\\|\\\\dots\\|~\\|\\^"
				    exp-str exp-pos)
		      exp-pos))
	     (setq exp-token 'punc
		   exp-data (math-match-substring exp-str 0)
		   exp-pos (match-end 0))
	     (and (eq (string-match "\\\\dots\\." exp-str exp-pos) exp-pos)
		  (setq exp-pos (match-end 0)))
	     (if (memq (aref exp-data 0) '(?~ ?^))
		 (math-read-token)))
	    ((eq (string-match "%%.*$" exp-str exp-pos) exp-pos)
	     (setq exp-pos (match-end 0))
	     (math-read-token))
	    (t
	     (if (and (eq ch ?\{) (memq calc-language '(tex eqn)))
		 (setq ch ?\())
	     (if (and (eq ch ?\}) (memq calc-language '(tex eqn)))
		 (setq ch ?\)))
	     (if (and (eq ch ?\&) (eq calc-language 'tex))
		 (setq ch ?\,))
	     (setq exp-token 'punc
		   exp-data (char-to-string ch)
		   exp-pos (1+ exp-pos)))))))


(defun math-read-expr-level (exp-prec &optional exp-term)
  (let* ((x (math-read-factor)) (first t) op op2)
    (while (and (or (and calc-user-parse-table
			 (setq op (calc-check-user-syntax x exp-prec))
			 (setq x op
			       op '("2x" ident 999999 -1)))
		    (and (setq op (assoc exp-data math-expr-opers))
			 (/= (nth 2 op) -1)
			 (or (and (setq op2 (assoc
					     exp-data
					     (cdr (memq op math-expr-opers))))
				  (eq (= (nth 3 op) -1)
				      (/= (nth 3 op2) -1))
				  (eq (= (nth 3 op2) -1)
				      (not (math-factor-after)))
				  (setq op op2))
			     t))
		    (and (or (eq (nth 2 op) -1)
			     (memq exp-token '(symbol number dollar hash))
			     (equal exp-data "(")
			     (and (equal exp-data "[")
				  (not (eq calc-language 'math))
				  (not (and exp-keep-spaces
					    (eq (car-safe x) 'vec)))))
			 (or (not (setq op (assoc exp-data math-expr-opers)))
			     (/= (nth 2 op) -1))
			 (or (not calc-user-parse-table)
			     (not (eq exp-token 'symbol))
			     (let ((p calc-user-parse-table))
			       (while (and p
					   (or (not (integerp
						     (car (car (car p)))))
					       (not (equal
						     (nth 1 (car (car p)))
						     exp-data))))
				 (setq p (cdr p)))
			       (not p)))
			 (setq op (assoc "2x" math-expr-opers))))
		(not (and exp-term (equal exp-data exp-term)))
		(>= (nth 2 op) exp-prec))
      (if (not (equal (car op) "2x"))
	  (math-read-token))
      (and (memq (nth 1 op) '(sdev mod))
	   (calc-extensions))
      (setq x (cond ((consp (nth 1 op))
		     (funcall (car (nth 1 op)) x op))
		    ((eq (nth 3 op) -1)
		     (if (eq (nth 1 op) 'ident)
			 x
		       (if (eq (nth 1 op) 'closing)
			   (if (eq (nth 2 op) exp-prec)
			       (progn
				 (setq exp-prec 1000)
				 x)
			     (throw 'syntax "Mismatched delimiters"))
			 (list (nth 1 op) x))))
		    ((and (not first)
			  (memq (nth 1 op) math-alg-inequalities)
			  (memq (car-safe x) math-alg-inequalities))
		     (calc-extensions)
		     (math-composite-inequalities x op))
		    (t (list (nth 1 op)
			     x
			     (math-read-expr-level (nth 3 op) exp-term))))
	    first nil))
    x))

(defun calc-check-user-syntax (&optional x prec)
  (let ((p calc-user-parse-table)
	(matches nil)
	match rule)
    (while (and p
		(or (not (progn
			   (setq rule (car (car p)))
			   (if x
			       (and (integerp (car rule))
				    (>= (car rule) prec)
				    (equal exp-data
					   (car (setq rule (cdr rule)))))
			     (equal exp-data (car rule)))))
		    (let ((save-exp-pos exp-pos)
			  (save-exp-old-pos exp-old-pos)
			  (save-exp-token exp-token)
			  (save-exp-data exp-data))
		      (or (not (listp
				(setq matches (calc-match-user-syntax rule))))
			  (let ((args (progn
					(calc-extensions)
					calc-arg-values))
				(conds nil)
				temp)
			    (if x
				(setq matches (cons x matches)))
			    (setq match (cdr (car p)))
			    (while (and (eq (car-safe match)
					    'calcFunc-condition)
					(= (length match) 3))
			      (setq conds (append (math-flatten-lands
						   (nth 2 match))
						  conds)
				    match (nth 1 match)))
			    (while (and conds match)
			      (calc-extensions)
			      (cond ((eq (car-safe (car conds))
					 'calcFunc-let)
				     (setq temp (car conds))
				     (or (= (length temp) 3)
					 (and (= (length temp) 2)
					      (eq (car-safe (nth 1 temp))
						  'calcFunc-assign)
					      (= (length (nth 1 temp)) 3)
					      (setq temp (nth 1 temp)))
					 (setq match nil))
				     (setq matches (cons
						    (math-normalize
						     (math-multi-subst
						      (nth 2 temp)
						      args matches))
						    matches)
					   args (cons (nth 1 temp)
						      args)))
				    ((and (eq (car-safe (car conds))
					      'calcFunc-matches)
					  (= (length (car conds)) 3))
				     (setq temp (calcFunc-vmatches
						 (math-multi-subst
						  (nth 1 (car conds))
						  args matches)
						 (nth 2 (car conds))))
				     (if (eq temp 0)
					 (setq match nil)
				       (while (setq temp (cdr temp))
					 (setq matches (cons (nth 2 (car temp))
							     matches)
					       args (cons (nth 1 (car temp))
							  args)))))
				    (t
				     (or (math-is-true (math-simplify
							(math-multi-subst
							 (car conds)
							 args matches)))
					 (setq match nil))))
			      (setq conds (cdr conds)))
			    (if match
				(not (setq match (math-multi-subst
						  match args matches)))
			      (setq exp-old-pos save-exp-old-pos
				    exp-token save-exp-token
				    exp-data save-exp-data
				    exp-pos save-exp-pos)))))))
      (setq p (cdr p)))
    (and p match)))

(defun calc-match-user-syntax (p &optional term)
  (let ((matches nil)
	(save-exp-pos exp-pos)
	(save-exp-old-pos exp-old-pos)
	(save-exp-token exp-token)
	(save-exp-data exp-data))
    (while (and p
		(cond ((stringp (car p))
		       (and (equal exp-data (car p))
			    (progn
			      (math-read-token)
			      t)))
		      ((integerp (car p))
		       (and (setq m (catch 'syntax
				      (math-read-expr-level
				       (car p)
				       (if (cdr p)
					   (if (consp (nth 1 p))
					       (car (nth 1 (nth 1 p)))
					     (nth 1 p))
					 term))))
			    (not (stringp m))
			    (setq matches (nconc matches (list m)))))
		      ((eq (car (car p)) '\?)
		       (setq m (calc-match-user-syntax (nth 1 (car p))))
		       (or (nth 2 (car p))
			   (setq matches
				 (nconc matches
					(list
					 (cons 'vec (and (listp m) m))))))
		       (or (listp m) (not (nth 2 (car p)))
			   (not (eq (aref (car (nth 2 (car p))) 0) ?\$))
			   (eq exp-token 'end)))
		      (t
		       (setq m (calc-match-user-syntax (nth 1 (car p))
						       (car (nth 2 (car p)))))
		       (if (listp m)
			   (let ((vec (cons 'vec m))
				 opos mm)
			     (while (and (listp
					  (setq opos exp-pos
						mm (calc-match-user-syntax
						    (or (nth 2 (car p))
							(nth 1 (car p)))
						    (car (nth 2 (car p))))))
					 (> exp-pos opos))
			       (setq vec (nconc vec mm)))
			     (setq matches (nconc matches (list vec))))
			 (and (eq (car (car p)) '*)
			      (setq matches (nconc matches (list '(vec)))))))))
      (setq p (cdr p)))
    (if p
	(setq exp-pos save-exp-pos
	      exp-old-pos save-exp-old-pos
	      exp-token save-exp-token
	      exp-data save-exp-data
	      matches "Failed"))
    matches))

(defconst math-alg-inequalities
  '(calcFunc-lt calcFunc-gt calcFunc-leq calcFunc-geq
		calcFunc-eq calcFunc-neq))

(defun math-remove-dashes (x)
  (if (string-match "\\`\\(.*\\)-\\(.*\\)\\'" x)
      (math-remove-dashes
       (concat (math-match-substring x 1) "#" (math-match-substring x 2)))
    x))

(defun math-restore-dashes (x)
  (if (string-match "\\`\\(.*\\)[#_]\\(.*\\)\\'" x)
      (math-restore-dashes
       (concat (math-match-substring x 1) "-" (math-match-substring x 2)))
    x))

(defun math-read-if (cond op)
  (let ((then (math-read-expr-level 0)))
    (or (equal exp-data ":")
	(throw 'syntax "Expected ':'"))
    (math-read-token)
    (list 'calcFunc-if cond then (math-read-expr-level (nth 3 op)))))

(defun math-factor-after ()
  (let ((exp-pos exp-pos)
	exp-old-pos exp-token exp-data)
    (math-read-token)
    (or (memq exp-token '(number symbol dollar hash string))
	(and (assoc exp-data '(("-") ("+") ("!") ("|") ("/")))
	     (assoc (concat "u" exp-data) math-expr-opers))
	(eq (nth 2 (assoc exp-data math-expr-opers)) -1)
	(assoc exp-data '(("(") ("[") ("{"))))))

(defun math-read-factor ()
  (let (op)
    (cond ((eq exp-token 'number)
	   (let ((num (math-read-number exp-data)))
	     (if (not num)
		 (progn
		   (setq exp-old-pos exp-pos)
		   (throw 'syntax "Bad format")))
	     (math-read-token)
	     (if (and math-read-expr-quotes
		      (consp num))
		 (list 'quote num)
	       num)))
	  ((and calc-user-parse-table
		(setq op (calc-check-user-syntax)))
	   op)
	  ((or (equal exp-data "-")
	       (equal exp-data "+")
	       (equal exp-data "!")
	       (equal exp-data "|")
	       (equal exp-data "/"))
	   (setq exp-data (concat "u" exp-data))
	   (math-read-factor))
	  ((and (setq op (assoc exp-data math-expr-opers))
		(eq (nth 2 op) -1))
	   (if (consp (nth 1 op))
	       (funcall (car (nth 1 op)) op)
	     (math-read-token)
	     (let ((val (math-read-expr-level (nth 3 op))))
	       (cond ((eq (nth 1 op) 'ident)
		      val)
		     ((and (Math-numberp val)
			   (equal (car op) "u-"))
		      (math-neg val))
		     (t (list (nth 1 op) val))))))
	  ((eq exp-token 'symbol)
	   (let ((sym (intern exp-data)))
	     (math-read-token)
	     (if (equal exp-data calc-function-open)
		 (let ((f (assq sym math-expr-function-mapping)))
		   (math-read-token)
		   (if (consp (cdr f))
		       (funcall (car (cdr f)) f sym)
		     (let ((args (if (or (equal exp-data calc-function-close)
					 (eq exp-token 'end))
				     nil
				   (math-read-expr-list))))
		       (if (not (or (equal exp-data calc-function-close)
				    (eq exp-token 'end)))
			   (throw 'syntax "Expected `)'"))
		       (math-read-token)
		       (if (and (eq calc-language 'fortran) args
				(calc-extensions)
				(let ((calc-matrix-mode 'scalar))
				  (math-known-matrixp
				   (list 'var sym
					 (intern
					  (concat "var-"
						  (symbol-name sym)))))))
			   (math-parse-fortran-subscr sym args)
			 (if f
			     (setq sym (cdr f))
			   (and (= (aref (symbol-name sym) 0) ?\\)
				(< (prefix-numeric-value calc-language-option)
				   0)
				(setq sym (intern (substring (symbol-name sym)
							     1))))
			   (or (string-match "-" (symbol-name sym))
			       (setq sym (intern
					  (concat "calcFunc-"
						  (symbol-name sym))))))
			 (cons sym args)))))
	       (if math-read-expr-quotes
		   sym
		 (let ((val (list 'var
				  (intern (math-remove-dashes
					   (symbol-name sym)))
				  (if (string-match "-" (symbol-name sym))
				      sym
				    (intern (concat "var-"
						    (symbol-name sym)))))))
		   (let ((v (assq (nth 1 val) math-expr-variable-mapping)))
		     (and v (setq val (if (consp (cdr v))
					  (funcall (car (cdr v)) v val)
					(list 'var
					      (intern
					       (substring (symbol-name (cdr v))
							  4))
					      (cdr v))))))
		   (while (and (memq calc-language '(c pascal maple))
			       (equal exp-data "["))
		     (math-read-token)
		     (setq val (append (list 'calcFunc-subscr val)
				       (math-read-expr-list)))
		     (if (equal exp-data "]")
			 (math-read-token)
		       (throw 'syntax "Expected ']'")))
		   val)))))
	  ((eq exp-token 'dollar)
	   (let ((abs (if (> exp-data 0) exp-data (- exp-data))))
	     (if (>= (length calc-dollar-values) abs)
		 (let ((num exp-data))
		   (math-read-token)
		   (setq calc-dollar-used (max calc-dollar-used num))
		   (math-check-complete (nth (1- abs) calc-dollar-values)))
	       (throw 'syntax (if calc-dollar-values
				  "Too many $'s"
				"$'s not allowed in this context")))))
	  ((eq exp-token 'hash)
	   (or calc-hashes-used
	       (throw 'syntax "#'s not allowed in this context"))
	   (calc-extensions)
	   (if (<= exp-data (length calc-arg-values))
	       (let ((num exp-data))
		 (math-read-token)
		 (setq calc-hashes-used (max calc-hashes-used num))
		 (nth (1- num) calc-arg-values))
	     (throw 'syntax "Too many # arguments")))
	  ((equal exp-data "(")
	   (let* ((exp (let ((exp-keep-spaces nil))
			 (math-read-token)
			 (if (or (equal exp-data "\\dots")
				 (equal exp-data "\\ldots"))
			     '(neg (var inf var-inf))
			   (math-read-expr-level 0)))))
	     (let ((exp-keep-spaces nil))
	       (cond
		((equal exp-data ",")
		 (progn
		   (math-read-token)
		   (let ((exp2 (math-read-expr-level 0)))
		     (setq exp
			   (if (and exp2 (Math-realp exp) (Math-realp exp2))
			       (math-normalize (list 'cplx exp exp2))
			     (list '+ exp (list '* exp2 '(var i var-i))))))))
		((equal exp-data ";")
		 (progn
		   (math-read-token)
		   (let ((exp2 (math-read-expr-level 0)))
		     (setq exp (if (and exp2 (Math-realp exp)
					(Math-anglep exp2))
				   (math-normalize (list 'polar exp exp2))
				 (calc-extensions)
				 (list '* exp
				       (list 'calcFunc-exp
					     (list '*
						   (math-to-radians-2 exp2)
						   '(var i var-i)))))))))
		((or (equal exp-data "\\dots")
		     (equal exp-data "\\ldots"))
		 (progn
		   (math-read-token)
		   (let ((exp2 (if (or (equal exp-data ")")
				       (equal exp-data "]")
				       (eq exp-token 'end))
				   '(var inf var-inf)
				 (math-read-expr-level 0))))
		     (setq exp
			   (list 'intv
				 (if (equal exp-data ")") 0 1)
				 exp
				 exp2)))))))
	     (if (not (or (equal exp-data ")")
			  (and (equal exp-data "]") (eq (car-safe exp) 'intv))
			  (eq exp-token 'end)))
		 (throw 'syntax "Expected `)'"))
	     (math-read-token)
	     exp))
	  ((eq exp-token 'string)
	   (calc-extensions)
	   (math-read-string))
	  ((equal exp-data "[")
	   (calc-extensions)
	   (math-read-brackets t "]"))
	  ((equal exp-data "{")
	   (calc-extensions)
	   (math-read-brackets nil "}"))
	  ((equal exp-data "<")
	   (calc-extensions)
	   (math-read-angle-brackets))
	  (t (throw 'syntax "Expected a number")))))

;;; calc-aent.el ends here
