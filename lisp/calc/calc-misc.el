;;; calc-misc.el --- miscellaenous functions for Calc

;; Copyright (C) 1990, 1991, 1992, 1993, 2001 Free Software Foundation, Inc.

;; Author: David Gillespie <daveg@synaptics.com>
;; Maintainer: Colin Walters <walters@debian.org>

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

;;; Commentary:

;;; Code:

;; This file is autoloaded from calc.el.
(require 'calc)

(require 'calc-macs)

(defun calc-Need-calc-misc () nil)


(defun calc-dispatch-help (arg)
  "M-# is a prefix key; follow it with one of these letters:

For turning Calc on and off:
  C  calc.  Start the Calculator in a window at the bottom of the screen.
  O  calc-other-window.  Start the Calculator but don't select its window.
  B  calc-big-or-small.  Control whether to use the full Emacs screen for Calc.
  Q  quick-calc.  Use the Calculator in the minibuffer.
  K  calc-keypad.  Start the Calculator in keypad mode (X window system only).
  E  calc-embedded.  Use the Calculator on a formula in this editing buffer.
  J  calc-embedded-select.  Like E, but select appropriate half of => or :=.
  W  calc-embedded-word.  Like E, but activate a single word, i.e., a number.
  Z  calc-user-invocation.  Invoke Calc in the way you defined with `Z I' cmd.
  X  calc-quit.  Turn Calc off.

For moving data into and out of Calc:
  G  calc-grab-region.  Grab the region defined by mark and point into Calc.
  R  calc-grab-rectangle.  Grab the rectangle defined by mark, point into Calc.
  :  calc-grab-sum-down.  Grab a rectangle and sum the columns.
  _  calc-grab-sum-across.  Grab a rectangle and sum the rows.
  Y  calc-copy-to-buffer.  Copy a value from the stack into the editing buffer.

For use with Embedded mode:
  A  calc-embedded-activate.  Find and activate all :='s and =>'s in buffer.
  D  calc-embedded-duplicate.  Make a copy of this formula and select it.
  F  calc-embedded-new-formula.  Insert a new formula at current point.
  N  calc-embedded-next.  Advance cursor to next known formula in buffer.
  P  calc-embedded-previous.  Advance cursor to previous known formula.
  U  calc-embedded-update-formula.  Re-evaluate formula at point.
  `  calc-embedded-edit.  Use calc-edit to edit formula at point.

Documentation:
  I  calc-info.  Read the Calculator manual in the Emacs Info system.
  T  calc-tutorial.  Run the Calculator Tutorial using the Emacs Info system.
  S  calc-summary.  Read the Summary from the Calculator manual in Info.

Miscellaneous:
  L  calc-load-everything.  Load all parts of the Calculator into memory.
  M  read-kbd-macro.  Read a region of keystroke names as a keyboard macro.
  0  (zero) calc-reset.  Reset Calc stack and modes to default state.

Press twice (`M-# M-#' or `M-# #') to turn Calc on or off using the same
Calc user interface as before (either M-# C or M-# K; initially M-# C)."
  (interactive "P")
  (calc-check-defines)
  (if calc-dispatch-help
      (progn
	(save-window-excursion
	  (describe-function 'calc-dispatch-help)
	  (let ((win (get-buffer-window "*Help*")))
	    (if win
		(let (key)
		  (select-window win)
		  (while (progn
			   (message "Calc options: Calc, Keypad, ...  %s"
				    "press SPC, DEL to scroll, C-g to cancel")
			   (memq (car (setq key (calc-read-key t)))
				 '(?  ?\C-h ?\C-? ?\C-v ?\M-v)))
		    (condition-case err
			(if (memq (car key) '(?  ?\C-v))
			    (scroll-up)
			  (scroll-down))
		      (error (beep))))
		      (calc-unread-command (cdr key))))))
	(calc-do-dispatch nil))
    (let ((calc-dispatch-help t))
      (calc-do-dispatch arg))))


(defun calc-big-or-small (arg)
  "Toggle Calc between full-screen and regular mode."
  (interactive "P")
  (let ((cwin (get-buffer-window "*Calculator*"))
	(twin (get-buffer-window "*Calc Trail*"))
	(kwin (get-buffer-window "*Calc Keypad*")))
    (if cwin
	(setq calc-full-mode
	      (if kwin
		  (and twin (eq (window-width twin) (frame-width)))
		(eq (window-height cwin) (1- (frame-height))))))
    (setq calc-full-mode (if arg
			     (> (prefix-numeric-value arg) 0)
			   (not calc-full-mode)))
    (if kwin
	(progn
	  (calc-quit)
	  (calc-do-keypad calc-full-mode nil))
      (if cwin
	  (progn
	    (calc-quit)
	    (calc nil calc-full-mode nil))))
    (message (if calc-full-mode
		 "Now using full screen for Calc"
	       "Now using partial screen for Calc"))))

(defun calc-other-window ()
  "Invoke the Calculator in another window."
  (interactive)
  (if (memq major-mode '(calc-mode calc-trail-mode))
      (progn
	(other-window 1)
	(if (memq major-mode '(calc-mode calc-trail-mode))
	    (other-window 1)))
    (if (get-buffer-window "*Calculator*")
	(calc-quit)
      (let ((win (selected-window)))
	(calc nil win (interactive-p))))))

(defun another-calc ()
  "Create another, independent Calculator buffer."
  (interactive)
  (if (eq major-mode 'calc-mode)
      (mapcar (function
	       (lambda (v)
		 (set-default v (symbol-value v)))) calc-local-var-list))
  (set-buffer (generate-new-buffer "*Calculator*"))
  (pop-to-buffer (current-buffer))
  (calc-mode))

(defun calc-info ()
  "Run the Emacs Info system on the Calculator documentation."
  (interactive)
  (select-window (get-largest-window))
  (info "Calc"))

(defun calc-tutorial ()
  "Run the Emacs Info system on the Calculator Tutorial."
  (interactive)
  (if (get-buffer-window "*Calculator*")
      (calc-quit))
  (calc-info)
  (Info-goto-node "Interactive Tutorial")
  (calc-other-window)
  (message "Welcome to the Calc Tutorial!"))

(defun calc-info-summary ()
  "Run the Emacs Info system on the Calculator Summary."
  (interactive)
  (calc-info)
  (Info-goto-node "Summary"))

(defun calc-help ()
  (interactive)
  (let ((msgs (append
	 '("Press `h' for complete help; press `?' repeatedly for a summary"
	   "Letter keys: Negate; Precision; Yank; Why; Xtended cmd; Quit"
	   "Letter keys: SHIFT + Undo, reDo; Keep-args; Inverse, Hyperbolic"
	   "Letter keys: SHIFT + sQrt; Sin, Cos, Tan; Exp, Ln, logB"
	   "Letter keys: SHIFT + Floor, Round; Abs, conJ, arG; Pi"
	   "Letter keys: SHIFT + Num-eval; More-recn; eXec-kbd-macro"
	   "Other keys: +, -, *, /, ^, \\ (int div), : (frac div)"
	   "Other keys: & (1/x), | (concat), % (modulo), ! (factorial)"
	   "Other keys: ' (alg-entry), = (eval), ` (edit); M-RET (last-args)"
	   "Other keys: SPC/RET (enter/dup), LFD (over); < > (scroll horiz)"
	   "Other keys: DEL (drop), M-DEL (drop-above); { } (scroll vert)"
	   "Other keys: TAB (swap/roll-dn), M-TAB (roll-up)"
	   "Other keys: [ , ; ] (vector), ( , ) (complex), ( ; ) (polar)"
	   "Prefix keys: Algebra, Binary/business, Convert, Display"
	   "Prefix keys: Functions, Graphics, Help, J (select)"
	   "Prefix keys: Kombinatorics/statistics, Modes, Store/recall"
	   "Prefix keys: Trail/time, Units/statistics, Vector/matrix"
	   "Prefix keys: Z (user), SHIFT + Z (define)"
	   "Prefix keys: prefix + ? gives further help for that prefix")
	 (list (format
		"  Calc %s by Dave Gillespie, daveg@synaptics.com"
		calc-version)))))
    (if calc-full-help-flag
	msgs
      (if (or calc-inverse-flag calc-hyperbolic-flag)
	  (if calc-inverse-flag
	      (if calc-hyperbolic-flag
		  (calc-inv-hyp-prefix-help)
		(calc-inverse-prefix-help))
	    (calc-hyperbolic-prefix-help))
	(setq calc-help-phase
	      (if (eq this-command last-command)
		  (% (1+ calc-help-phase) (1+ (length msgs)))
		0))
	(let ((msg (nth calc-help-phase msgs)))
	  (message "%s" (if msg
			    (concat msg ":"
				    (make-string (- (apply 'max
							   (mapcar 'length
								   msgs))
						    (length msg)) 32)
				    "  [?=MORE]")
			  "")))))))




;;;; Stack and buffer management.


(defun calc-do-handle-whys ()
  (setq calc-why (sort calc-next-why
		       (function
			(lambda (x y)
			  (and (eq (car x) '*) (not (eq (car y) '*))))))
	calc-next-why nil)
  (if (and calc-why (or (eq calc-auto-why t)
			(and (eq (car (car calc-why)) '*)
			     calc-auto-why)))
      (progn
	(calc-extensions)
	(calc-explain-why (car calc-why)
			  (if (eq calc-auto-why t)
			      (cdr calc-why)
			    (if calc-auto-why
				(eq (car (nth 1 calc-why)) '*))))
	(setq calc-last-why-command this-command)
	(calc-clear-command-flag 'clear-message))))

(defun calc-record-why (&rest stuff)
  (if (eq (car stuff) 'quiet)
      (setq stuff (cdr stuff))
    (if (and (symbolp (car stuff))
	     (cdr stuff)
	     (or (Math-objectp (nth 1 stuff))
		 (and (Math-vectorp (nth 1 stuff))
		      (math-constp (nth 1 stuff)))
		 (math-infinitep (nth 1 stuff))))
	(setq stuff (cons '* stuff))
      (if (and (stringp (car stuff))
	       (string-match "\\`\\*" (car stuff)))
	  (setq stuff (cons '* (cons (substring (car stuff) 1)
				     (cdr stuff)))))))
  (setq calc-next-why (cons stuff calc-next-why))
  nil)

;;; True if A is a constant or vector of constants.  [P x] [Public]
(defun math-constp (a)
  (or (Math-scalarp a)
      (and (memq (car a) '(sdev intv mod vec))
	   (progn
	     (while (and (setq a (cdr a))
			 (or (Math-scalarp (car a))  ; optimization
			     (math-constp (car a)))))
	     (null a)))))


(defun calc-roll-down-stack (n &optional m)
  (if (< n 0)
      (calc-roll-up-stack (- n) m)
    (if (or (= n 0) (> n (calc-stack-size))) (setq n (calc-stack-size)))
    (or m (setq m 1))
    (and (> n 1)
	 (< m n)
	 (if (and calc-any-selections
		  (not calc-use-selections))
	     (calc-roll-down-with-selections n m)
	   (calc-pop-push-list n
			       (append (calc-top-list m 1)
				       (calc-top-list (- n m) (1+ m))))))))

(defun calc-roll-up-stack (n &optional m)
  (if (< n 0)
      (calc-roll-down-stack (- n) m)
    (if (or (= n 0) (> n (calc-stack-size))) (setq n (calc-stack-size)))
    (or m (setq m 1))
    (and (> n 1)
	 (< m n)
	 (if (and calc-any-selections
		  (not calc-use-selections))
	     (calc-roll-up-with-selections n m)
	   (calc-pop-push-list n
			       (append (calc-top-list (- n m) 1)
				       (calc-top-list m (- n m -1))))))))


(defun calc-do-refresh ()
  (if calc-hyperbolic-flag
      (progn
	(setq calc-display-dirty t)
	nil)
    (calc-refresh)
    t))


(defun calc-record-list (vals &optional prefix)
  (while vals
    (or (eq (car vals) 'top-of-stack)
	(progn
	  (calc-record (car vals) prefix)
	  (setq prefix "...")))
    (setq vals (cdr vals))))


(defun calc-last-args-stub (arg)
  (interactive "p")
  (calc-extensions)
  (calc-last-args arg))


(defun calc-power (arg)
  (interactive "P")
  (calc-slow-wrapper
   (if (and calc-extensions-loaded
	    (calc-is-inverse))
       (calc-binary-op "root" 'calcFunc-nroot arg nil nil)
     (calc-binary-op "^" 'calcFunc-pow arg nil nil '^))))

(defun calc-mod (arg)
  (interactive "P")
  (calc-slow-wrapper
   (calc-binary-op "%" 'calcFunc-mod arg nil nil '%)))

(defun calc-inv (arg)
  (interactive "P")
  (calc-slow-wrapper
   (calc-unary-op "inv" 'calcFunc-inv arg)))

(defun calc-percent ()
  (interactive)
  (calc-slow-wrapper
   (calc-pop-push-record-list
    1 "%" (list (list 'calcFunc-percent (calc-top-n 1))))))


(defun calc-over (n)
  (interactive "P")
  (if n
      (calc-enter (- (prefix-numeric-value n)))
    (calc-enter -2)))


(defun calc-pop-above (n)
  (interactive "P")
  (if n
      (calc-pop (- (prefix-numeric-value n)))
    (calc-pop -2)))

(defun calc-roll-down (n)
  (interactive "P")
  (calc-wrapper
   (let ((nn (prefix-numeric-value n)))
     (cond ((null n)
	    (calc-roll-down-stack 2))
	   ((> nn 0)
	    (calc-roll-down-stack nn))
	   ((= nn 0)
	    (calc-pop-push-list (calc-stack-size)
				(reverse
				 (calc-top-list (calc-stack-size)))))
	   (t
	    (calc-roll-down-stack (calc-stack-size) (- nn)))))))

(defun calc-roll-up (n)
  (interactive "P")
  (calc-wrapper
   (let ((nn (prefix-numeric-value n)))
     (cond ((null n)
	    (calc-roll-up-stack 3))
	   ((> nn 0)
	    (calc-roll-up-stack nn))
	   ((= nn 0)
	    (calc-pop-push-list (calc-stack-size)
				(reverse
				 (calc-top-list (calc-stack-size)))))
	   (t
	    (calc-roll-up-stack (calc-stack-size) (- nn)))))))




;;; Other commands.

(defun calc-num-prefix-name (n)
  (cond ((eq n '-) "- ")
	((equal n '(4)) "C-u ")
	((consp n) (format "%d " (car n)))
	((integerp n) (format "%d " n))
	(t "")))

(defun calc-missing-key (n)
  "This is a placeholder for a command which needs to be loaded from calc-ext.
When this key is used, calc-ext (the Calculator extensions module) will be
loaded and the keystroke automatically re-typed."
  (interactive "P")
  (calc-extensions)
  (if (keymapp (key-binding (char-to-string last-command-char)))
      (message "%s%c-" (calc-num-prefix-name n) last-command-char))
  (calc-unread-command)
  (setq prefix-arg n))

(defun calc-shift-Y-prefix-help ()
  (interactive)
  (calc-extensions)
  (calc-do-prefix-help calc-Y-help-msgs "other" ?Y))




(defun calcDigit-letter ()
  (interactive)
  (if (calc-minibuffer-contains "[-+]?\\(1[1-9]\\|[2-9][0-9]\\)#.*")
      (progn
	(setq last-command-char (upcase last-command-char))
	(calcDigit-key))
    (calcDigit-nondigit)))


;; A Lisp version of temp_minibuffer_message from minibuf.c.
(defun calc-temp-minibuffer-message (m)
  (let ((savemax (point-max)))
    (save-excursion
      (goto-char (point-max))
      (insert m))
    (let ((okay nil))
      (unwind-protect
	  (progn
	    (sit-for 2)
	    (identity 1)   ; this forces a call to QUIT; in bytecode.c.
	    (setq okay t))
	(progn
	  (delete-region savemax (point-max))
	  (or okay (abort-recursive-edit)))))))


(put 'math-with-extra-prec 'lisp-indent-hook 1)


;;; Concatenate two vectors, or a vector and an object.  [V O O] [Public]
(defun math-concat (v1 v2)
  (if (stringp v1)
      (concat v1 v2)
    (calc-extensions)
    (if (and (or (math-objvecp v1) (math-known-scalarp v1))
	     (or (math-objvecp v2) (math-known-scalarp v2)))
	(append (if (and (math-vectorp v1)
			 (or (math-matrixp v1)
			     (not (math-matrixp v2))))
		    v1
		  (list 'vec v1))
		(if (and (math-vectorp v2)
			 (or (math-matrixp v2)
			     (not (math-matrixp v1))))
		    (cdr v2)
		  (list v2)))
      (list '| v1 v2))))


;;; True if A is zero.  Works for un-normalized values.  [P n] [Public]
(defun math-zerop (a)
  (if (consp a)
      (cond ((memq (car a) '(bigpos bigneg))
	     (while (eq (car (setq a (cdr a))) 0))
	     (null a))
	    ((memq (car a) '(frac float polar mod))
	     (math-zerop (nth 1 a)))
	    ((eq (car a) 'cplx)
	     (and (math-zerop (nth 1 a)) (math-zerop (nth 2 a))))
	    ((eq (car a) 'hms)
	     (and (math-zerop (nth 1 a))
		  (math-zerop (nth 2 a))
		  (math-zerop (nth 3 a)))))
    (eq a 0)))


;;; True if A is real and negative.  [P n] [Public]

(defun math-negp (a)
  (if (consp a)
      (cond ((eq (car a) 'bigpos) nil)
	    ((eq (car a) 'bigneg) (cdr a))
	    ((memq (car a) '(float frac))
	     (Math-integer-negp (nth 1 a)))
	    ((eq (car a) 'hms)
	     (if (math-zerop (nth 1 a))
		 (if (math-zerop (nth 2 a))
		     (math-negp (nth 3 a))
		   (math-negp (nth 2 a)))
	       (math-negp (nth 1 a))))
	    ((eq (car a) 'date)
	     (math-negp (nth 1 a)))
	    ((eq (car a) 'intv)
	     (or (math-negp (nth 3 a))
		 (and (math-zerop (nth 3 a))
		      (memq (nth 1 a) '(0 2)))))
	    ((equal a '(neg (var inf var-inf))) t))
    (< a 0)))

;;; True if A is a negative number or an expression the starts with '-'.
(defun math-looks-negp (a)   ; [P x] [Public]
  (or (Math-negp a)
      (eq (car-safe a) 'neg)
      (and (memq (car-safe a) '(* /))
	   (or (math-looks-negp (nth 1 a))
	       (math-looks-negp (nth 2 a))))
      (and (eq (car-safe a) '-)
	   (math-looks-negp (nth 1 a)))))


;;; True if A is real and positive.  [P n] [Public]
(defun math-posp (a)
  (if (consp a)
      (cond ((eq (car a) 'bigpos) (cdr a))
	    ((eq (car a) 'bigneg) nil)
	    ((memq (car a) '(float frac))
	     (Math-integer-posp (nth 1 a)))
	    ((eq (car a) 'hms)
	     (if (math-zerop (nth 1 a))
		 (if (math-zerop (nth 2 a))
		     (math-posp (nth 3 a))
		   (math-posp (nth 2 a)))
	       (math-posp (nth 1 a))))
	    ((eq (car a) 'date)
	     (math-posp (nth 1 a)))
	    ((eq (car a) 'mod)
	     (not (math-zerop (nth 1 a))))
	    ((eq (car a) 'intv)
	     (or (math-posp (nth 2 a))
		 (and (math-zerop (nth 2 a))
		      (memq (nth 1 a) '(0 1)))))
	    ((equal a '(var inf var-inf)) t))
    (> a 0)))

(defalias 'math-fixnump 'integerp)
(defalias 'math-fixnatnump 'natnump)


;;; True if A is an even integer.  [P R R] [Public]
(defun math-evenp (a)
  (if (consp a)
      (and (memq (car a) '(bigpos bigneg))
	   (= (% (nth 1 a) 2) 0))
    (= (% a 2) 0)))

;;; Compute A / 2, for small or big integer A.  [I i]
;;; If A is negative, type of truncation is undefined.
(defun math-div2 (a)
  (if (consp a)
      (if (cdr a)
	  (math-normalize (cons (car a) (math-div2-bignum (cdr a))))
	0)
    (/ a 2)))

(defun math-div2-bignum (a)   ; [l l]
  (if (cdr a)
      (cons (+ (/ (car a) 2) (* (% (nth 1 a) 2) 500))
	    (math-div2-bignum (cdr a)))
    (list (/ (car a) 2))))


;;; Reject an argument to a calculator function.  [Public]
(defun math-reject-arg (&optional a p option)
  (if option
      (calc-record-why option p a)
    (if p
	(calc-record-why p a)))
  (signal 'wrong-type-argument (and a (if p (list p a) (list a)))))


;;; Coerce A to be an integer (by truncation toward zero).  [I N] [Public]
(defun math-trunc (a &optional prec)
  (cond (prec
	 (calc-extensions)
	 (math-trunc-special a prec))
	((Math-integerp a) a)
	((Math-looks-negp a)
	 (math-neg (math-trunc (math-neg a))))
	((eq (car a) 'float)
	 (math-scale-int (nth 1 a) (nth 2 a)))
	(t (calc-extensions)
	   (math-trunc-fancy a))))
(defalias 'calcFunc-trunc 'math-trunc)

;;; Coerce A to be an integer (by truncation toward minus infinity).  [I N]
(defun math-floor (a &optional prec)    ;  [Public]
  (cond (prec
	 (calc-extensions)
	 (math-floor-special a prec))
	((Math-integerp a) a)
	((Math-messy-integerp a) (math-trunc a))
	((Math-realp a)
	 (if (Math-negp a)
	     (math-add (math-trunc a) -1)
	   (math-trunc a)))
	(t (calc-extensions)
	   (math-floor-fancy a))))
(defalias 'calcFunc-floor 'math-floor)


(defun math-imod (a b)   ; [I I I] [Public]
  (if (and (not (consp a)) (not (consp b)))
      (if (= b 0)
	  (math-reject-arg a "*Division by zero")
	(% a b))
    (cdr (math-idivmod a b))))


(defun calcFunc-inv (m)
  (if (Math-vectorp m)
      (progn
	(calc-extensions)
	(if (math-square-matrixp m)
	    (or (math-with-extra-prec 2 (math-matrix-inv-raw m))
		(math-reject-arg m "*Singular matrix"))
	  (math-reject-arg m 'square-matrixp)))
    (math-div 1 m)))


(defun math-do-working (msg arg)
  (or executing-kbd-macro
      (progn
	(calc-set-command-flag 'clear-message)
	(if math-working-step
	    (if math-working-step-2
		(setq msg (format "[%d/%d] %s"
				  math-working-step math-working-step-2 msg))
	      (setq msg (format "[%d] %s" math-working-step msg))))
	(message "Working... %s = %s" msg
		 (math-showing-full-precision (math-format-number arg))))))


;;; Compute A modulo B, defined in terms of truncation toward minus infinity.
(defun math-mod (a b)   ; [R R R] [Public]
  (cond ((and (Math-zerop a) (not (eq (car-safe a) 'mod))) a)
	((Math-zerop b)
	 (math-reject-arg a "*Division by zero"))
	((and (Math-natnump a) (Math-natnump b))
	 (math-imod a b))
	((and (Math-anglep a) (Math-anglep b))
	 (math-sub a (math-mul (math-floor (math-div a b)) b)))
	(t (calc-extensions)
	   (math-mod-fancy a b))))



;;; General exponentiation.

(defun math-pow (a b)   ; [O O N] [Public]
  (cond ((equal b '(var nan var-nan))
	 b)
	((Math-zerop a)
	 (if (and (Math-scalarp b) (Math-posp b))
	     (if (math-floatp b) (math-float a) a)
	   (calc-extensions)
	   (math-pow-of-zero a b)))
	((or (eq a 1) (eq b 1)) a)
	((or (equal a '(float 1 0)) (equal b '(float 1 0))) a)
	((Math-zerop b)
	 (if (Math-scalarp a)
	     (if (or (math-floatp a) (math-floatp b))
		 '(float 1 0) 1)
	   (calc-extensions)
	   (math-pow-zero a b)))
	((and (Math-integerp b) (or (Math-numberp a) (Math-vectorp a)))
	 (if (and (equal a '(float 1 1)) (integerp b))
	     (math-make-float 1 b)
	   (math-with-extra-prec 2
	     (math-ipow a b))))
	(t
	 (calc-extensions)
	 (math-pow-fancy a b))))

(defun math-ipow (a n)   ; [O O I] [Public]
  (cond ((Math-integer-negp n)
	 (math-ipow (math-div 1 a) (Math-integer-neg n)))
	((not (consp n))
	 (if (and (Math-ratp a) (> n 20))
	     (math-iipow-show a n)
	   (math-iipow a n)))
	((math-evenp n)
	 (math-ipow (math-mul a a) (math-div2 n)))
	(t
	 (math-mul a (math-ipow (math-mul a a)
				(math-div2 (math-add n -1)))))))

(defun math-iipow (a n)   ; [O O S]
  (cond ((= n 0) 1)
	((= n 1) a)
	((= (% n 2) 0) (math-iipow (math-mul a a) (/ n 2)))
	(t (math-mul a (math-iipow (math-mul a a) (/ n 2))))))

(defun math-iipow-show (a n)   ; [O O S]
  (math-working "pow" a)
  (let ((val (cond
	      ((= n 0) 1)
	      ((= n 1) a)
	      ((= (% n 2) 0) (math-iipow-show (math-mul a a) (/ n 2)))
	      (t (math-mul a (math-iipow-show (math-mul a a) (/ n 2)))))))
    (math-working "pow" val)
    val))


(defun math-read-radix-digit (dig)   ; [D S; Z S]
  (if (> dig ?9)
      (if (< dig ?A)
	  nil
	(- dig 55))
    (if (>= dig ?0)
	(- dig ?0)
      nil)))


;;; Bug reporting

(defun report-calc-bug ()
  "Report a bug in Calc, the GNU Emacs calculator.
Prompts for bug subject.  Leaves you in a mail buffer."
  (interactive)
  (let ((reporter-prompt-for-summary-p t))
    (reporter-submit-bug-report calc-bug-address "Calc" '(calc-version)
				nil nil
				"Please describe exactly what actions triggered the bug and the
precise symptoms of the bug.  If possible, include a backtrace by
doing 'M-x toggle-debug-on-error', then reproducing the bug.
" )))
(defalias 'calc-report-bug 'report-calc-bug)

;;; calc-misc.el ends here
