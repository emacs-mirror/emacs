;;; calc-graph.el --- graph output functions for Calc

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

;; This file is autoloaded from calc-ext.el.
(require 'calc-ext)

(require 'calc-macs)

(defun calc-Need-calc-graph () nil)


;;; Graphics

;;; Note that some of the following initial values also occur in calc.el.
(defvar calc-gnuplot-tempfile (expand-file-name "calc" temporary-file-directory))

(defvar calc-gnuplot-default-device "default")
(defvar calc-gnuplot-default-output "STDOUT")
(defvar calc-gnuplot-print-device "postscript")
(defvar calc-gnuplot-print-output "auto")
(defvar calc-gnuplot-keep-outfile nil)
(defvar calc-gnuplot-version nil)

(defvar calc-gnuplot-display (getenv "DISPLAY"))
(defvar calc-gnuplot-geometry nil)

(defvar calc-graph-default-resolution 15)
(defvar calc-graph-default-resolution-3d 5)
(defvar calc-graph-default-precision 5)

(defvar calc-gnuplot-buffer nil)
(defvar calc-gnuplot-input nil)

(defvar calc-gnuplot-last-error-pos 1)
(defvar calc-graph-last-device nil)
(defvar calc-graph-last-output nil)
(defvar calc-graph-file-cache nil)
(defvar calc-graph-var-cache nil)
(defvar calc-graph-data-cache nil)
(defvar calc-graph-data-cache-limit 10)
(defvar calc-graph-no-auto-view nil)
(defvar calc-graph-no-wait nil)

(defun calc-graph-fast (many)
  (interactive "P")
  (let ((calc-graph-no-auto-view t))
    (calc-graph-delete t)
    (calc-graph-add many)
    (calc-graph-plot nil)))

(defun calc-graph-fast-3d (many)
  (interactive "P")
  (let ((calc-graph-no-auto-view t))
    (calc-graph-delete t)
    (calc-graph-add-3d many)
    (calc-graph-plot nil)))

(defun calc-graph-delete (all)
  (interactive "P")
  (calc-wrapper
   (calc-graph-init)
   (save-excursion
     (set-buffer calc-gnuplot-input)
     (and (calc-graph-find-plot t all)
	  (progn
	    (if (looking-at "s?plot")
		(progn
		  (setq calc-graph-var-cache nil)
		  (delete-region (point) (point-max)))
	      (delete-region (point) (1- (point-max)))))))
   (calc-graph-view-commands)))

(defun calc-graph-find-plot (&optional before all)
  (goto-char (point-min))
  (and (re-search-forward "^s?plot[ \t]+" nil t)
       (let ((beg (point)))
	 (goto-char (point-max))
	 (if (or all
		 (not (search-backward "," nil t))
		 (< (point) beg))
	     (progn
	       (goto-char beg)
	       (if before
		   (beginning-of-line)))
	   (or before
	       (re-search-forward ",[ \t]+")))
	 t)))

(defun calc-graph-add (many)
  (interactive "P")
  (calc-wrapper
   (calc-graph-init)
   (cond ((null many)
	  (calc-graph-add-curve (calc-graph-lookup (calc-top-n 2))
				(calc-graph-lookup (calc-top-n 1))))
	 ((or (consp many) (eq many 0))
	  (let ((xdata (calc-graph-lookup (calc-top-n 2)))
		(ylist (calc-top-n 1)))
	    (or (eq (car-safe ylist) 'vec)
		(error "Y argument must be a vector"))
	    (while (setq ylist (cdr ylist))
	      (calc-graph-add-curve xdata (calc-graph-lookup (car ylist))))))
	 ((> (setq many (prefix-numeric-value many)) 0)
	  (let ((xdata (calc-graph-lookup (calc-top-n (1+ many)))))
	    (while (> many 0)
	      (calc-graph-add-curve xdata
				    (calc-graph-lookup (calc-top-n many)))
	      (setq many (1- many)))))
	 (t
	  (let (pair)
	    (setq many (- many))
	    (while (> many 0)
	      (setq pair (calc-top-n many))
	      (or (and (eq (car-safe pair) 'vec)
		       (= (length pair) 3))
		  (error "Argument must be an [x,y] vector"))
	      (calc-graph-add-curve (calc-graph-lookup (nth 1 pair))
				    (calc-graph-lookup (nth 2 pair)))
	      (setq many (1- many))))))
   (calc-graph-view-commands)))

(defun calc-graph-add-3d (many)
  (interactive "P")
  (calc-wrapper
   (calc-graph-init)
   (cond ((null many)
	  (calc-graph-add-curve (calc-graph-lookup (calc-top-n 3))
				(calc-graph-lookup (calc-top-n 2))
				(calc-graph-lookup (calc-top-n 1))))
	 ((or (consp many) (eq many 0))
	  (let ((xdata (calc-graph-lookup (calc-top-n 3)))
		(ydata (calc-graph-lookup (calc-top-n 2)))
		(zlist (calc-top-n 1)))
	    (or (eq (car-safe zlist) 'vec)
		(error "Z argument must be a vector"))
	    (while (setq zlist (cdr zlist))
	      (calc-graph-add-curve xdata ydata
				    (calc-graph-lookup (car zlist))))))
	 ((> (setq many (prefix-numeric-value many)) 0)
	  (let ((xdata (calc-graph-lookup (calc-top-n (+ many 2))))
		(ydata (calc-graph-lookup (calc-top-n (+ many 1)))))
	    (while (> many 0)
	      (calc-graph-add-curve xdata ydata
				    (calc-graph-lookup (calc-top-n many)))
	      (setq many (1- many)))))
	 (t
	  (let (curve)
	    (setq many (- many))
	    (while (> many 0)
	      (setq curve (calc-top-n many))
	      (or (and (eq (car-safe curve) 'vec)
		       (= (length curve) 4))
		  (error "Argument must be an [x,y,z] vector"))
	      (calc-graph-add-curve (calc-graph-lookup (nth 1 curve))
				    (calc-graph-lookup (nth 2 curve))
				    (calc-graph-lookup (nth 3 curve)))
	      (setq many (1- many))))))
   (calc-graph-view-commands)))

(defun calc-graph-add-curve (xdata ydata &optional zdata)
  (let ((num (calc-graph-count-curves))
	(pstyle (calc-var-value 'var-PointStyles))
	(lstyle (calc-var-value 'var-LineStyles)))
    (save-excursion
      (set-buffer calc-gnuplot-input)
      (goto-char (point-min))
      (if (re-search-forward (if zdata "^plot[ \t]" "^splot[ \t]")
			     nil t)
	  (error "Can't mix 2d and 3d curves on one graph"))
      (if (re-search-forward "^s?plot[ \t]" nil t)
	  (progn
	    (end-of-line)
	    (insert ", "))
	(goto-char (point-max))
	(or (eq (preceding-char) ?\n)
	    (insert "\n"))
	(insert (if zdata "splot" "plot") " \n")
	(forward-char -1))
      (insert "{" (symbol-name (nth 1 xdata))
	      ":" (symbol-name (nth 1 ydata)))
      (if zdata
	  (insert ":" (symbol-name (nth 1 zdata))))
      (insert "} "
	      "title \"" (symbol-name (nth 1 ydata)) "\" "
	      "with dots")
      (setq pstyle (and (eq (car-safe pstyle) 'vec) (nth (1+ num) pstyle)))
      (setq lstyle (and (eq (car-safe lstyle) 'vec) (nth (1+ num) lstyle)))
      (calc-graph-set-styles
       (or (and (Math-num-integerp lstyle) (math-trunc lstyle))
	   0)
       (or (and (Math-num-integerp pstyle) (math-trunc pstyle))
	   (if (eq (car-safe (calc-var-value (nth 2 ydata))) 'vec)
	       0 -1))))))

(defun calc-graph-lookup (thing)
  (if (and (eq (car-safe thing) 'var)
	   (calc-var-value (nth 2 thing)))
      thing
    (let ((found (assoc thing calc-graph-var-cache)))
      (or found
	  (progn
	    (setq varname (concat "PlotData"
				  (int-to-string
				   (1+ (length calc-graph-var-cache))))
		  var (list 'var (intern varname)
			    (intern (concat "var-" varname)))
		  found (cons thing var)
		  calc-graph-var-cache (cons found calc-graph-var-cache))
	    (set (nth 2 var) thing)))
      (cdr found))))

(defun calc-graph-juggle (arg)
  (interactive "p")
  (calc-graph-init)
  (save-excursion
    (set-buffer calc-gnuplot-input)
    (if (< arg 0)
	(let ((num (calc-graph-count-curves)))
	  (if (> num 0)
	      (while (< arg 0)
		(setq arg (+ arg num))))))
    (while (>= (setq arg (1- arg)) 0)
      (calc-graph-do-juggle))))

(defun calc-graph-count-curves ()
  (save-excursion
    (set-buffer calc-gnuplot-input)
    (if (re-search-forward "^s?plot[ \t]" nil t)
	(let ((num 1))
	  (goto-char (point-min))
	  (while (search-forward "," nil t)
	    (setq num (1+ num)))
	  num)
      0)))

(defun calc-graph-do-juggle ()
  (let (base)
    (and (calc-graph-find-plot t t)
	 (progn
	   (setq base (point))
	   (calc-graph-find-plot t nil)
	   (or (eq base (point))
	       (let ((str (buffer-substring (+ (point) 2) (1- (point-max)))))
		 (delete-region (point) (1- (point-max)))
		 (goto-char (+ base 5))
		 (insert str ", ")))))))

(defun calc-graph-print (flag)
  (interactive "P")
  (calc-graph-plot flag t))

(defun calc-graph-plot (flag &optional printing)
  (interactive "P")
  (calc-slow-wrapper
   (let ((calcbuf (current-buffer))
	 (tempbuf (get-buffer-create "*Gnuplot Temp-2*"))
	 (tempbuftop 1)
	 (tempoutfile nil)
	 (curve-num 0)
	 (refine (and flag (> (prefix-numeric-value flag) 0)))
	 (recompute (and flag (< (prefix-numeric-value flag) 0)))
	 (surprise-splot nil)
	 (tty-output nil)
	 cache-env is-splot device output resolution precision samples-pos)
     (or (boundp 'calc-graph-prev-kill-hook)
	 (if calc-emacs-type-19
	     (progn
	       (setq calc-graph-prev-kill-hook nil)
	       (add-hook 'kill-emacs-hook 'calc-graph-kill-hook))
	   (setq calc-graph-prev-kill-hook kill-emacs-hook)
	   (setq kill-emacs-hook 'calc-graph-kill-hook)))
     (save-excursion
       (calc-graph-init)
       (set-buffer tempbuf)
       (erase-buffer)
       (set-buffer calc-gnuplot-input)
       (goto-char (point-min))
       (setq is-splot (re-search-forward "^splot[ \t]" nil t))
       (let ((str (buffer-string))
	     (ver calc-gnuplot-version))
	 (set-buffer (get-buffer-create "*Gnuplot Temp*"))
	 (erase-buffer)
	 (insert "# (Note: This is a temporary copy---do not edit!)\n")
	 (if (>= ver 2)
	     (insert "set noarrow\nset nolabel\n"
		     "set autoscale xy\nset nologscale xy\n"
		     "set xlabel\nset ylabel\nset title\n"
		     "set noclip points\nset clip one\nset clip two\n"
		     "set format \"%g\"\nset tics\nset xtics\nset ytics\n"
		     "set data style linespoints\n"
		     "set nogrid\nset nokey\nset nopolar\n"))
	 (if (>= ver 3)
	     (insert "set surface\nset nocontour\n"
		     "set " (if is-splot "" "no") "parametric\n"
		     "set notime\nset border\nset ztics\nset zeroaxis\n"
		     "set view 60,30,1,1\nset offsets 0,0,0,0\n"))
	 (setq samples-pos (point))
	 (insert "\n\n" str))
       (goto-char (point-min))
       (if is-splot
	   (if refine
	       (error "This option works only for 2d plots")
	     (setq recompute t)))
       (let ((calc-gnuplot-input (current-buffer))
	     (calc-graph-no-auto-view t))
	 (if printing
	     (setq device calc-gnuplot-print-device
		   output calc-gnuplot-print-output)
	   (setq device (calc-graph-find-command "terminal")
		 output (calc-graph-find-command "output"))
	   (or device
	       (setq device calc-gnuplot-default-device))
	   (if output
	       (setq output (car (read-from-string output)))
	     (setq output calc-gnuplot-default-output)))
	 (if (or (equal device "") (equal device "default"))
	     (setq device (if printing
			      "postscript"
			    (if (or (eq window-system 'x) (getenv "DISPLAY"))
				"x11"
			      (if (>= calc-gnuplot-version 3)
				  "dumb" "postscript")))))
	 (if (equal device "dumb")
	     (setq device (format "dumb %d %d"
				  (1- (frame-width)) (1- (frame-height)))))
	 (if (equal device "big")
	     (setq device (format "dumb %d %d"
				  (* 4 (- (frame-width) 3))
				  (* 4 (- (frame-height) 3)))))
	 (if (stringp output)
	     (if (or (equal output "auto")
		     (and (equal output "tty") (setq tty-output t)))
		 (setq tempoutfile (calc-temp-file-name -1)
		       output tempoutfile))
	   (setq output (eval output)))
	 (or (equal device calc-graph-last-device)
	     (progn
	       (setq calc-graph-last-device device)
	       (calc-gnuplot-command "set terminal" device)))
	 (or (equal output calc-graph-last-output)
	     (progn
	       (setq calc-graph-last-output output)
	       (calc-gnuplot-command "set output"
				     (if (equal output "STDOUT")
					 ""
				       (prin1-to-string output)))))
	 (setq resolution (calc-graph-find-command "samples"))
	 (if resolution
	     (setq resolution (string-to-int resolution))
	   (setq resolution (if is-splot
				calc-graph-default-resolution-3d
			      calc-graph-default-resolution)))
	 (setq precision (calc-graph-find-command "precision"))
	 (if precision
	     (setq precision (string-to-int precision))
	   (setq precision calc-graph-default-precision))
	 (calc-graph-set-command "terminal")
	 (calc-graph-set-command "output")
	 (calc-graph-set-command "samples")
	 (calc-graph-set-command "precision"))
       (goto-char samples-pos)
       (insert "set samples " (int-to-string (max (if is-splot 20 200)
						  (+ 5 resolution))) "\n")
       (while (re-search-forward "{\\*[^}]+}[^,\n]*" nil t)
	 (delete-region (match-beginning 0) (match-end 0))
	 (if (looking-at ",")
	     (delete-char 1)
	   (while (memq (preceding-char) '(?\ ?\t))
	     (forward-char -1))
	   (if (eq (preceding-char) ?\,)
	       (delete-backward-char 1))))
       (save-excursion
	 (set-buffer calcbuf)
	 (setq cache-env (list calc-angle-mode
			       calc-complex-mode
			       calc-simplify-mode
			       calc-infinite-mode
			       calc-word-size
			       precision is-splot))
	 (if (and (not recompute)
		  (equal (cdr (car calc-graph-data-cache)) cache-env))
	     (while (> (length calc-graph-data-cache)
		       calc-graph-data-cache-limit)
	       (setcdr calc-graph-data-cache
		       (cdr (cdr calc-graph-data-cache))))
	   (setq calc-graph-data-cache (list (cons nil cache-env)))))
       (calc-graph-find-plot t t)
       (while (re-search-forward
	       (if is-splot
		   "{\\([^{}:\n]+\\):\\([^{}:\n]+\\):\\([^{}:\n]+\\)}"
		 "{\\([^{}:\n]+\\)\\(:\\)\\([^{}:\n]+\\)}")
	       nil t)
	 (setq curve-num (1+ curve-num))
	 (let* ((xname (buffer-substring (match-beginning 1) (match-end 1)))
		(xvar (intern (concat "var-" xname)))
		(xvalue (math-evaluate-expr (calc-var-value xvar)))
		(y3name (and is-splot
			     (buffer-substring (match-beginning 2)
					       (match-end 2))))
		(y3var (and is-splot (intern (concat "var-" y3name))))
		(y3value (and is-splot (calc-var-value y3var)))
		(yname (buffer-substring (match-beginning 3) (match-end 3)))
		(yvar (intern (concat "var-" yname)))
		(yvalue (calc-var-value yvar))
		filename)
	   (delete-region (match-beginning 0) (match-end 0))
	   (setq filename (calc-temp-file-name curve-num))
	   (save-excursion
	     (set-buffer calcbuf)
	     (let (tempbuftop
		   (xp xvalue)
		   (yp yvalue)
		   (zp nil)
		   (xlow nil) (xhigh nil) (y3low nil) (y3high nil)
		   xvec xval xstep var-DUMMY
		   y3vec y3val y3step var-DUMMY2 (zval nil)
		   yvec yval ycache ycacheptr yvector
		   numsteps numsteps3
		   (keep-file (and (not is-splot) (file-exists-p filename)))
		   (stepcount 0)
		   (calc-symbolic-mode nil)
		   (calc-prefer-frac nil)
		   (calc-internal-prec (max 3 precision))
		   (calc-simplify-mode (and (not (memq calc-simplify-mode
						       '(none num)))
					    calc-simplify-mode))
		   (blank t)
		   (non-blank nil)
		   (math-working-step 0)
		   (math-working-step-2 nil))
	       (save-excursion
		 (if is-splot
		     (calc-graph-compute-3d)
		   (calc-graph-compute-2d))
		 (set-buffer tempbuf)
		 (goto-char (point-max))
		 (insert "\n" xname)
		 (if is-splot
		     (insert ":" y3name))
		 (insert ":" yname "\n\n")
		 (setq tempbuftop (point))
		 (let ((calc-group-digits nil)
		       (calc-leading-zeros nil)
		       (calc-number-radix 10)
		       (entry (and (not is-splot)
				   (list xp yp xhigh numsteps))))
		   (or (equal entry
			      (nth 1 (nth (1+ curve-num)
					  calc-graph-file-cache)))
		       (setq keep-file nil))
		   (setcar (cdr (nth (1+ curve-num) calc-graph-file-cache))
			   entry)
		   (or keep-file
		       (calc-graph-format-data)))
		 (or keep-file
		     (progn
		       (or non-blank
			   (error "No valid data points for %s:%s"
				  xname yname))
		       (write-region tempbuftop (point-max) filename
				     nil 'quiet))))))
	   (insert (prin1-to-string filename))))
       (if surprise-splot
	   (setcdr cache-env nil))
       (if (= curve-num 0)
	   (progn
	     (calc-gnuplot-command "clear")
	     (calc-clear-command-flag 'clear-message)
	     (message "No data to plot!"))
	 (setq calc-graph-data-cache-limit (max curve-num
						calc-graph-data-cache-limit)
	       filename (calc-temp-file-name 0))
	 (write-region (point-min) (point-max) filename nil 'quiet)
	 (calc-gnuplot-command "load" (prin1-to-string filename))
	 (or (equal output "STDOUT")
	     calc-gnuplot-keep-outfile
	     (progn   ; need to close the output file before printing/plotting
	       (setq calc-graph-last-output "STDOUT")
	       (calc-gnuplot-command "set output")))
	 (let ((command (if printing
			    calc-gnuplot-print-command
			  (or calc-gnuplot-plot-command
			      (and (string-match "^dumb" device)
				   'calc-graph-show-dumb)
			      (and tty-output
				   'calc-graph-show-tty)))))
	   (if command
	       (if (stringp command)
		   (calc-gnuplot-command
		    "!" (format command
				(or tempoutfile
				    calc-gnuplot-print-output)))
		 (if (symbolp command)
		     (funcall command output)
		   (eval command))))))))))

(defun calc-graph-compute-2d ()
  (if (setq yvec (eq (car-safe yvalue) 'vec))
      (if (= (setq numsteps (1- (length yvalue))) 0)
	  (error "Can't plot an empty vector")
	(if (setq xvec (eq (car-safe xvalue) 'vec))
	    (or (= (1- (length xvalue)) numsteps)
		(error "%s and %s have different lengths" xname yname))
	  (if (and (eq (car-safe xvalue) 'intv)
		   (math-constp xvalue))
	      (setq xstep (math-div (math-sub (nth 3 xvalue)
					      (nth 2 xvalue))
				    (1- numsteps))
		    xvalue (nth 2 xvalue))
	    (if (math-realp xvalue)
		(setq xstep 1)
	      (error "%s is not a suitable basis for %s" xname yname)))))
    (or (math-realp yvalue)
	(let ((arglist nil))
	  (setq yvalue (math-evaluate-expr yvalue))
	  (calc-default-formula-arglist yvalue)
	  (or arglist
	      (error "%s does not contain any unassigned variables" yname))
	  (and (cdr arglist)
	       (error "%s contains more than one variable: %s"
		      yname arglist))
	  (setq yvalue (math-expr-subst yvalue
					(math-build-var-name (car arglist))
					'(var DUMMY var-DUMMY)))))
    (setq ycache (assoc yvalue calc-graph-data-cache))
    (delq ycache calc-graph-data-cache)
    (nconc calc-graph-data-cache
	   (list (or ycache (setq ycache (list yvalue)))))
    (if (and (not (setq xvec (eq (car-safe xvalue) 'vec)))
	     refine (cdr (cdr ycache)))
	(calc-graph-refine-2d)
      (calc-graph-recompute-2d))))

(defun calc-graph-refine-2d ()
  (setq keep-file nil
	ycacheptr (cdr ycache))
  (if (and (setq xval (calc-graph-find-command "xrange"))
	   (string-match "\\`\\[\\([0-9.eE+-]*\\):\\([0-9.eE+-]*\\)\\]\\'"
			 xval))
      (let ((b2 (match-beginning 2))
	    (e2 (match-end 2)))
	(setq xlow (math-read-number (substring xval
						(match-beginning 1)
						(match-end 1)))
	      xhigh (math-read-number (substring xval b2 e2))))
    (if xlow
	(while (and (cdr ycacheptr)
		    (Math-lessp (car (nth 1 ycacheptr)) xlow))
	  (setq ycacheptr (cdr ycacheptr)))))
  (setq math-working-step-2 (1- (length ycacheptr)))
  (while (and (cdr ycacheptr)
	      (or (not xhigh)
		  (Math-lessp (car (car ycacheptr)) xhigh)))
    (setq var-DUMMY (math-div (math-add (car (car ycacheptr))
					(car (nth 1 ycacheptr)))
			      2)
	  math-working-step (1+ math-working-step)
	  yval (math-evaluate-expr yvalue))
    (setcdr ycacheptr (cons (cons var-DUMMY yval)
			    (cdr ycacheptr)))
    (setq ycacheptr (cdr (cdr ycacheptr))))
  (setq yp ycache
	numsteps 1000000))

(defun calc-graph-recompute-2d ()
  (setq ycacheptr ycache)
  (if xvec
      (setq numsteps (1- (length xvalue))
	    yvector nil)
    (if (and (eq (car-safe xvalue) 'intv)
	     (math-constp xvalue))
	(setq numsteps resolution
	      yp nil
	      xlow (nth 2 xvalue)
	      xhigh (nth 3 xvalue)
	      xstep (math-div (math-sub xhigh xlow)
			      (1- numsteps))
	      xvalue (nth 2 xvalue))
      (error "%s is not a suitable basis for %s"
	     xname yname)))
  (setq math-working-step-2 numsteps)
  (while (>= (setq numsteps (1- numsteps)) 0)
    (setq math-working-step (1+ math-working-step))
    (if xvec
	(progn
	  (setq xp (cdr xp)
		xval (car xp))
	  (and (not (eq ycacheptr ycache))
	       (consp (car ycacheptr))
	       (not (Math-lessp (car (car ycacheptr)) xval))
	       (setq ycacheptr ycache)))
      (if (= numsteps 0)
	  (setq xval xhigh)   ; avoid cumulative roundoff
	(setq xval xvalue
	      xvalue (math-add xvalue xstep))))
    (while (and (cdr ycacheptr)
		(Math-lessp (car (nth 1 ycacheptr)) xval))
      (setq ycacheptr (cdr ycacheptr)))
    (or (and (cdr ycacheptr)
	     (Math-equal (car (nth 1 ycacheptr)) xval))
	(progn
	  (setq keep-file nil
		var-DUMMY xval)
	  (setcdr ycacheptr (cons (cons xval (math-evaluate-expr yvalue))
				  (cdr ycacheptr)))))
    (setq ycacheptr (cdr ycacheptr))
    (if xvec
	(setq yvector (cons (cdr (car ycacheptr)) yvector))
      (or yp (setq yp ycacheptr))))
  (if xvec
      (setq xp xvalue
	    yvec t
	    yp (cons 'vec (nreverse yvector))
	    numsteps (1- (length xp)))
    (setq numsteps 1000000)))

(defun calc-graph-compute-3d ()
  (if (setq yvec (eq (car-safe yvalue) 'vec))
      (if (math-matrixp yvalue)
	  (progn
	    (setq numsteps (1- (length yvalue))
		  numsteps3 (1- (length (nth 1 yvalue))))
	    (if (eq (car-safe xvalue) 'vec)
		(or (= (1- (length xvalue)) numsteps)
		    (error "%s has wrong length" xname))
	      (if (and (eq (car-safe xvalue) 'intv)
		       (math-constp xvalue))
		  (setq xvalue (calcFunc-index numsteps
					       (nth 2 xvalue)
					       (math-div
						(math-sub (nth 3 xvalue)
							  (nth 2 xvalue))
						(1- numsteps))))
		(if (math-realp xvalue)
		    (setq xvalue (calcFunc-index numsteps xvalue 1))
		  (error "%s is not a suitable basis for %s" xname yname))))
	    (if (eq (car-safe y3value) 'vec)
		(or (= (1- (length y3value)) numsteps3)
		    (error "%s has wrong length" y3name))
	      (if (and (eq (car-safe y3value) 'intv)
		       (math-constp y3value))
		  (setq y3value (calcFunc-index numsteps3
						(nth 2 y3value)
						(math-div
						 (math-sub (nth 3 y3value)
							   (nth 2 y3value))
						 (1- numsteps3))))
		(if (math-realp y3value)
		    (setq y3value (calcFunc-index numsteps3 y3value 1))
		  (error "%s is not a suitable basis for %s" y3name yname))))
	    (setq xp nil
		  yp nil
		  zp nil
		  xvec t)
	    (while (setq xvalue (cdr xvalue) yvalue (cdr yvalue))
	      (setq xp (nconc xp (make-list (1+ numsteps3) (car xvalue)))
		    yp (nconc yp (cons 0 (copy-sequence (cdr y3value))))
		    zp (nconc zp (cons '(skip)
				       (copy-sequence (cdr (car yvalue)))))))
	    (setq numsteps (1- (* numsteps (1+ numsteps3)))))
	(if (= (setq numsteps (1- (length yvalue))) 0)
	    (error "Can't plot an empty vector"))
	(or (and (eq (car-safe xvalue) 'vec)
		 (= (1- (length xvalue)) numsteps))
	    (error "%s is not a suitable basis for %s" xname yname))
	(or (and (eq (car-safe y3value) 'vec)
		 (= (1- (length y3value)) numsteps))
	    (error "%s is not a suitable basis for %s" y3name yname))
	(setq xp xvalue
	      yp y3value
	      zp yvalue
	      xvec t))
    (or (math-realp yvalue)
	(let ((arglist nil))
	  (setq yvalue (math-evaluate-expr yvalue))
	  (calc-default-formula-arglist yvalue)
	  (setq arglist (sort arglist 'string-lessp))
	  (or (cdr arglist)
	      (error "%s does not contain enough unassigned variables" yname))
	  (and (cdr (cdr arglist))
	       (error "%s contains too many variables: %s" yname arglist))
	  (setq yvalue (math-multi-subst yvalue
					 (mapcar 'math-build-var-name
						 arglist)
					 '((var DUMMY var-DUMMY)
					   (var DUMMY2 var-DUMMY2))))))
    (if (setq xvec (eq (car-safe xvalue) 'vec))
	(setq numsteps (1- (length xvalue)))
      (if (and (eq (car-safe xvalue) 'intv)
	       (math-constp xvalue))
	  (setq numsteps resolution
		xvalue (calcFunc-index numsteps
				       (nth 2 xvalue)
				       (math-div (math-sub (nth 3 xvalue)
							   (nth 2 xvalue))
						 (1- numsteps))))
	(error "%s is not a suitable basis for %s"
	       xname yname)))
    (if (setq y3vec (eq (car-safe y3value) 'vec))
	(setq numsteps3 (1- (length y3value)))
      (if (and (eq (car-safe y3value) 'intv)
	       (math-constp y3value))
	  (setq numsteps3 resolution
		y3value (calcFunc-index numsteps3
					(nth 2 y3value)
					(math-div (math-sub (nth 3 y3value)
							    (nth 2 y3value))
						  (1- numsteps3))))
	(error "%s is not a suitable basis for %s"
	       y3name yname)))
    (setq xp nil
	  yp nil
	  zp nil
	  xvec t)
    (setq math-working-step 0)
    (while (setq xvalue (cdr xvalue))
      (setq xp (nconc xp (make-list (1+ numsteps3) (car xvalue)))
	    yp (nconc yp (cons 0 (copy-sequence (cdr y3value))))
	    zp (cons '(skip) zp)
	    y3step y3value
	    var-DUMMY (car xvalue)
	    math-working-step-2 0
	    math-working-step (1+ math-working-step))
      (while (setq y3step (cdr y3step))
	(setq math-working-step-2 (1+ math-working-step-2)
	      var-DUMMY2 (car y3step)
	      zp (cons (math-evaluate-expr yvalue) zp))))
    (setq zp (nreverse zp)
	  numsteps (1- (* numsteps (1+ numsteps3))))))

(defun calc-graph-format-data ()
  (while (<= (setq stepcount (1+ stepcount)) numsteps)
    (if xvec
	(setq xp (cdr xp)
	      xval (car xp)
	      yp (cdr yp)
	      yval (car yp)
	      zp (cdr zp)
	      zval (car zp))
      (if yvec
	  (setq xval xvalue
		xvalue (math-add xvalue xstep)
		yp (cdr yp)
		yval (car yp))
	(setq xval (car (car yp))
	      yval (cdr (car yp))
	      yp (cdr yp))
	(if (or (not yp)
		(and xhigh (equal xval xhigh)))
	    (setq numsteps 0))))
    (if is-splot
	(if (and (eq (car-safe zval) 'calcFunc-xyz)
		 (= (length zval) 4))
	    (setq xval (nth 1 zval)
		  yval (nth 2 zval)
		  zval (nth 3 zval)))
      (if (and (eq (car-safe yval) 'calcFunc-xyz)
	       (= (length yval) 4))
	  (progn
	    (or surprise-splot
		(save-excursion
		  (set-buffer (get-buffer-create "*Gnuplot Temp*"))
		  (save-excursion
		    (goto-char (point-max))
		    (re-search-backward "^plot[ \t]")
		    (insert "set parametric\ns")
		    (setq surprise-splot t))))
	    (setq xval (nth 1 yval)
		  zval (nth 3 yval)
		  yval (nth 2 yval)))
	(if (and (eq (car-safe yval) 'calcFunc-xy)
		 (= (length yval) 3))
	    (setq xval (nth 1 yval)
		  yval (nth 2 yval)))))
    (if (and (Math-realp xval)
	     (Math-realp yval)
	     (or (not zval) (Math-realp zval)))
	(progn
	  (setq blank nil
		non-blank t)
	  (if (Math-integerp xval)
	      (insert (math-format-number xval))
	    (if (eq (car xval) 'frac)
		(setq xval (math-float xval)))
	    (insert (math-format-number (nth 1 xval))
		    "e" (int-to-string (nth 2 xval))))
	  (insert " ")
	  (if (Math-integerp yval)
	      (insert (math-format-number yval))
	    (if (eq (car yval) 'frac)
		(setq yval (math-float yval)))
	    (insert (math-format-number (nth 1 yval))
		    "e" (int-to-string (nth 2 yval))))
	  (if zval
	      (progn
		(insert " ")
		(if (Math-integerp zval)
		    (insert (math-format-number zval))
		  (if (eq (car zval) 'frac)
		      (setq zval (math-float zval)))
		  (insert (math-format-number (nth 1 zval))
			  "e" (int-to-string (nth 2 zval))))))
	  (insert "\n"))
      (and (not (equal zval '(skip)))
	   (boundp 'var-PlotRejects)
	   (eq (car-safe var-PlotRejects) 'vec)
	   (nconc var-PlotRejects
		  (list (list 'vec
			      curve-num
			      stepcount
			      xval yval)))
	   (calc-refresh-evaltos 'var-PlotRejects))
      (or blank
	  (progn
	    (insert "\n")
	    (setq blank t))))))

(defun calc-temp-file-name (num)
  (while (<= (length calc-graph-file-cache) (1+ num))
    (setq calc-graph-file-cache (nconc calc-graph-file-cache (list nil))))
  (car (or (nth (1+ num) calc-graph-file-cache)
	   (setcar (nthcdr (1+ num) calc-graph-file-cache)
		   (list (make-temp-name
			  (concat calc-gnuplot-tempfile
				  (if (<= num 0)
				      (char-to-string (- ?A num))
				    (int-to-string num))))
			 nil)))))

(defun calc-graph-delete-temps ()
  (while calc-graph-file-cache
    (and (car calc-graph-file-cache)
	 (file-exists-p (car (car calc-graph-file-cache)))
	 (condition-case err
	     (delete-file (car (car calc-graph-file-cache)))
	   (error nil)))
    (setq calc-graph-file-cache (cdr calc-graph-file-cache))))

(defun calc-graph-kill-hook ()
  (calc-graph-delete-temps)
  (if calc-graph-prev-kill-hook
      (funcall calc-graph-prev-kill-hook)))

(defun calc-graph-show-tty (output)
  "Default calc-gnuplot-plot-command for \"tty\" output mode.
This is useful for tek40xx and other graphics-terminal types."
  (call-process-region 1 1 shell-file-name
		       nil calc-gnuplot-buffer nil
		       "-c" (format "cat %s >/dev/tty; rm %s" output output)))

(defun calc-graph-show-dumb (&optional output)
  "Default calc-gnuplot-plot-command for Pinard's \"dumb\" terminal type.
This \"dumb\" driver will be present in Gnuplot 3.0."
  (interactive)
  (save-window-excursion
    (switch-to-buffer calc-gnuplot-buffer)
    (delete-other-windows)
    (goto-char calc-gnuplot-trail-mark)
    (or (search-forward "\f" nil t)
	(sleep-for 1))
    (goto-char (point-max))
    (re-search-backward "\f\\|^[ \t]+\\^$\\|G N U P L O T")
    (setq found-pt (point))
    (if (looking-at "\f")
	(progn
	  (forward-char 1)
	  (if (eolp) (forward-line 1))
	  (or (calc-graph-find-command "time")
	      (calc-graph-find-command "title")
	      (calc-graph-find-command "ylabel")
	      (let ((pt (point)))
		(insert-before-markers (format "(%s)" (current-time-string)))
		(goto-char pt)))
	  (set-window-start (selected-window) (point))
	  (goto-char (point-max)))
      (end-of-line)
      (backward-char 1)
      (recenter '(4)))
    (or (boundp 'calc-dumb-map)
	(progn
	  (setq calc-dumb-map (make-sparse-keymap))
	  (define-key calc-dumb-map "\n" 'scroll-up)
	  (define-key calc-dumb-map " " 'scroll-up)
	  (define-key calc-dumb-map "\177" 'scroll-down)
	  (define-key calc-dumb-map "<" 'scroll-left)
	  (define-key calc-dumb-map ">" 'scroll-right)
	  (define-key calc-dumb-map "{" 'scroll-down)
	  (define-key calc-dumb-map "}" 'scroll-up)
	  (define-key calc-dumb-map "q" 'exit-recursive-edit)
	  (define-key calc-dumb-map "\C-c\C-c" 'exit-recursive-edit)))
    (use-local-map calc-dumb-map)
    (setq truncate-lines t)
    (message "Type `q'%s to return to Calc"
	     (if (eq (lookup-key (current-global-map) "\e#") 'calc-dispatch)
		    " or `M-# M-#'" ""))
    (recursive-edit)
    (bury-buffer "*Gnuplot Trail*")))

(defun calc-graph-clear ()
  (interactive)
  (if calc-graph-last-device
      (if (or (equal calc-graph-last-device "x11")
	      (equal calc-graph-last-device "X11"))
	  (calc-gnuplot-command "set output"
				(if (equal calc-graph-last-output "STDOUT")
				    ""
				  (prin1-to-string calc-graph-last-output)))
	(calc-gnuplot-command "clear"))))

(defun calc-graph-title-x (title)
  (interactive "sX axis title: ")
  (calc-graph-set-command "xlabel" (if (not (equal title ""))
				       (prin1-to-string title))))

(defun calc-graph-title-y (title)
  (interactive "sY axis title: ")
  (calc-graph-set-command "ylabel" (if (not (equal title ""))
				       (prin1-to-string title))))

(defun calc-graph-title-z (title)
  (interactive "sZ axis title: ")
  (calc-graph-set-command "zlabel" (if (not (equal title ""))
				       (prin1-to-string title))))

(defun calc-graph-range-x (range)
  (interactive "sX axis range: ")
  (calc-graph-set-range "xrange" range))

(defun calc-graph-range-y (range)
  (interactive "sY axis range: ")
  (calc-graph-set-range "yrange" range))

(defun calc-graph-range-z (range)
  (interactive "sZ axis range: ")
  (calc-graph-set-range "zrange" range))

(defun calc-graph-set-range (cmd range)
  (if (equal range "$")
      (calc-wrapper
       (let ((val (calc-top-n 1)))
	 (if (and (eq (car-safe val) 'intv) (math-constp val))
	     (setq range (concat
			  (math-format-number (math-float (nth 2 val))) ":"
			  (math-format-number (math-float (nth 3 val)))))
	   (if (and (eq (car-safe val) 'vec)
		    (= (length val) 3))
	       (setq range (concat
			    (math-format-number (math-float (nth 1 val))) ":"
			    (math-format-number (math-float (nth 2 val)))))
	     (error "Range specification must be an interval or 2-vector")))
	 (calc-pop-stack 1))))
  (if (string-match "\\[.+\\]" range)
      (setq range (substring range 1 -1)))
  (if (and (not (string-match ":" range))
	   (or (string-match "," range)
	       (string-match " " range)))
      (aset range (match-beginning 0) ?\:))
  (calc-graph-set-command cmd (if (not (equal range ""))
				  (concat "[" range "]"))))

(defun calc-graph-log-x (flag)
  (interactive "P")
  (calc-graph-set-log flag 0 0))

(defun calc-graph-log-y (flag)
  (interactive "P")
  (calc-graph-set-log 0 flag 0))

(defun calc-graph-log-z (flag)
  (interactive "P")
  (calc-graph-set-log 0 0 flag))

(defun calc-graph-set-log (xflag yflag zflag)
  (let* ((old (or (calc-graph-find-command "logscale") ""))
	 (xold (string-match "x" old))
	 (yold (string-match "y" old))
	 (zold (string-match "z" old))
	 str)
    (setq str (concat (if (if xflag
			      (if (eq xflag 0) xold
				(> (prefix-numeric-value xflag) 0))
			    (not xold)) "x" "")
		      (if (if yflag
			      (if (eq yflag 0) yold
				(> (prefix-numeric-value yflag) 0))
			    (not yold)) "y" "")
		      (if (if zflag
			      (if (eq zflag 0) zold
				(> (prefix-numeric-value zflag) 0))
			    (not zold)) "z" "")))
    (calc-graph-set-command "logscale" (if (not (equal str "")) str))))

(defun calc-graph-line-style (style)
  (interactive "P")
  (calc-graph-set-styles (and style (prefix-numeric-value style)) t))

(defun calc-graph-point-style (style)
  (interactive "P")
  (calc-graph-set-styles t (and style (prefix-numeric-value style))))

(defun calc-graph-set-styles (lines points)
  (calc-graph-init)
  (save-excursion
    (set-buffer calc-gnuplot-input)
    (or (calc-graph-find-plot nil nil)
	(error "No data points have been set!"))
    (let ((base (point))
	  (mode nil) (lstyle nil) (pstyle nil)
	  start end lenbl penbl)
      (re-search-forward "[,\n]")
      (forward-char -1)
      (setq end (point) start end)
      (goto-char base)
      (if (looking-at "[^,\n]*[^,\n \t]\\([ \t]+with\\)")
	  (progn
	    (setq start (match-beginning 1))
	    (goto-char (match-end 0))
	    (if (looking-at "[ \t]+\\([a-z]+\\)")
		(setq mode (buffer-substring (match-beginning 1)
					     (match-end 1))))
	    (if (looking-at "[ \ta-z]+\\([0-9]+\\)")
		(setq lstyle (string-to-int
			      (buffer-substring (match-beginning 1)
						(match-end 1)))))
	    (if (looking-at "[ \ta-z]+[0-9]+[ \t]+\\([0-9]+\\)")
		(setq pstyle (string-to-int
			      (buffer-substring (match-beginning 1)
						(match-end 1)))))))
      (setq lenbl (or (equal mode "lines") (equal mode "linespoints"))
	    penbl (or (equal mode "points") (equal mode "linespoints")))
      (if lines
	  (or (eq lines t)
	      (setq lstyle lines
		    lenbl (>= lines 0)))
	(setq lenbl (not lenbl)))
      (if points
	  (or (eq points t)
	      (setq pstyle points
		    penbl (>= points 0)))
	(setq penbl (not penbl)))
      (delete-region start end)
      (goto-char start)
      (insert " with "
	      (if lenbl
		  (if penbl "linespoints" "lines")
		(if penbl "points" "dots")))
      (if (and pstyle (> pstyle 0))
	  (insert " " (if (and lstyle (> lstyle 0)) (int-to-string lstyle) "1")
		  " " (int-to-string pstyle))
	(if (and lstyle (> lstyle 0))
	    (insert " " (int-to-string lstyle))))))
  (calc-graph-view-commands))

(defun calc-graph-zero-x (flag)
  (interactive "P")
  (calc-graph-set-command "noxzeroaxis"
			  (and (if flag
				   (<= (prefix-numeric-value flag) 0)
				 (not (calc-graph-find-command "noxzeroaxis")))
			       " ")))

(defun calc-graph-zero-y (flag)
  (interactive "P")
  (calc-graph-set-command "noyzeroaxis"
			  (and (if flag
				   (<= (prefix-numeric-value flag) 0)
				 (not (calc-graph-find-command "noyzeroaxis")))
			       " ")))

(defun calc-graph-name (name)
  (interactive "sTitle for current curve: ")
  (calc-graph-init)
  (save-excursion
    (set-buffer calc-gnuplot-input)
    (or (calc-graph-find-plot nil nil)
	(error "No data points have been set!"))
    (let ((base (point))
	  start)
      (re-search-forward "[,\n]\\|[ \t]+with")
      (setq end (match-beginning 0))
      (goto-char base)
      (if (looking-at "[^,\n]*[^,\n \t]\\([ \t]+title\\)")
	  (progn
	    (goto-char (match-beginning 1))
	    (delete-region (point) end))
	(goto-char end))
      (insert " title " (prin1-to-string name))))
  (calc-graph-view-commands))

(defun calc-graph-hide (flag)
  (interactive "P")
  (calc-graph-init)
  (and (calc-graph-find-plot nil nil)
       (progn
	 (or (looking-at "{")
	     (error "Can't hide this curve (wrong format)"))
	 (forward-char 1)
	 (if (looking-at "*")
	     (if (or (null flag) (<= (prefix-numeric-value flag) 0))
		 (delete-char 1))
	   (if (or (null flag) (> (prefix-numeric-value flag) 0))
	       (insert "*"))))))

(defun calc-graph-header (title)
  (interactive "sTitle for entire graph: ")
  (calc-graph-set-command "title" (if (not (equal title ""))
				      (prin1-to-string title))))

(defun calc-graph-border (flag)
  (interactive "P")
  (calc-graph-set-command "noborder"
			  (and (if flag
				   (<= (prefix-numeric-value flag) 0)
				 (not (calc-graph-find-command "noborder")))
			       " ")))

(defun calc-graph-grid (flag)
  (interactive "P")
  (calc-graph-set-command "grid" (and (if flag
					  (> (prefix-numeric-value flag) 0)
					(not (calc-graph-find-command "grid")))
				      " ")))

(defun calc-graph-key (flag)
  (interactive "P")
  (calc-graph-set-command "key" (and (if flag
					 (> (prefix-numeric-value flag) 0)
				       (not (calc-graph-find-command "key")))
				     " ")))

(defun calc-graph-num-points (res flag)
  (interactive "sNumber of data points: \nP")
  (if flag
      (if (> (prefix-numeric-value flag) 0)
	  (if (equal res "")
	      (message "Default resolution is %d"
		       calc-graph-default-resolution)
	    (setq calc-graph-default-resolution (string-to-int res)))
	(if (equal res "")
	    (message "Default 3D resolution is %d"
		     calc-graph-default-resolution-3d)
	  (setq calc-graph-default-resolution-3d (string-to-int res))))
    (calc-graph-set-command "samples" (if (not (equal res "")) res))))

(defun calc-graph-device (name flag)
  (interactive "sDevice name: \nP")
  (if (equal name "?")
      (progn
	(calc-gnuplot-command "set terminal")
	(calc-graph-view-trail))
    (if flag
	(if (> (prefix-numeric-value flag) 0)
	    (if (equal name "")
		(message "Default GNUPLOT device is \"%s\""
			 calc-gnuplot-default-device)
	      (setq calc-gnuplot-default-device name))
	  (if (equal name "")
	      (message "GNUPLOT device for Print command is \"%s\""
		       calc-gnuplot-print-device)
	    (setq calc-gnuplot-print-device name)))
      (calc-graph-set-command "terminal" (if (not (equal name ""))
					     name)))))

(defun calc-graph-output (name flag)
  (interactive "FOutput file name: \np")
  (cond ((string-match "\\<[aA][uU][tT][oO]$" name)
	 (setq name "auto"))
	((string-match "\\<[tT][tT][yY]$" name)
	 (setq name "tty"))
	((string-match "\\<[sS][tT][dD][oO][uU][tT]$" name)
	 (setq name "STDOUT"))
	((equal (file-name-nondirectory name) "")
	 (setq name ""))
	(t (setq name (expand-file-name name))))
  (if flag
      (if (> (prefix-numeric-value flag) 0)
	  (if (equal name "")
	      (message "Default GNUPLOT output file is \"%s\""
		       calc-gnuplot-default-output)
	    (setq calc-gnuplot-default-output name))
	(if (equal name "")
	    (message "GNUPLOT output file for Print command is \"%s\""
		     calc-gnuplot-print-output)
	  (setq calc-gnuplot-print-output name)))
    (calc-graph-set-command "output" (if (not (equal name ""))
					 (prin1-to-string name)))))

(defun calc-graph-display (name)
  (interactive "sX display name: ")
  (if (equal name "")
      (message "Current X display is \"%s\""
	       (or calc-gnuplot-display "<none>"))
    (setq calc-gnuplot-display name)
    (if (calc-gnuplot-alive)
	(calc-gnuplot-command "exit"))))

(defun calc-graph-geometry (name)
  (interactive "sX geometry spec (or \"default\"): ")
  (if (equal name "")
      (message "Current X geometry is \"%s\""
	       (or calc-gnuplot-geometry "default"))
    (setq calc-gnuplot-geometry (and (not (equal name "default")) name))
    (if (calc-gnuplot-alive)
	(calc-gnuplot-command "exit"))))

(defun calc-graph-find-command (cmd)
  (calc-graph-init)
  (save-excursion
    (set-buffer calc-gnuplot-input)
    (goto-char (point-min))
    (if (re-search-forward (concat "^set[ \t]+" cmd "[ \t]*\\(.*\\)$") nil t)
	(buffer-substring (match-beginning 1) (match-end 1)))))

(defun calc-graph-set-command (cmd &rest args)
  (calc-graph-init)
  (save-excursion
    (set-buffer calc-gnuplot-input)
    (goto-char (point-min))
    (if (re-search-forward (concat "^set[ \t]+" cmd "[ \t\n]") nil t)
	(progn
	  (forward-char -1)
	  (end-of-line)
	  (let ((end (point)))
	    (beginning-of-line)
	    (delete-region (point) (1+ end))))
      (if (calc-graph-find-plot t t)
	  (if (eq (preceding-char) ?\n)
	      (forward-char -1))
	(goto-char (1- (point-max)))))
    (if (and args (car args))
	(progn
	  (or (bolp)
	      (insert "\n"))
	  (insert "set " (mapconcat 'identity (cons cmd args) " ") "\n"))))
  (calc-graph-view-commands))

(defun calc-graph-command (cmd)
  (interactive "sGNUPLOT command: ")
  (calc-wrapper
   (calc-graph-init)
   (calc-graph-view-trail)
   (calc-gnuplot-command cmd)
   (accept-process-output)
   (calc-graph-view-trail)))

(defun calc-graph-kill (&optional no-view)
  (interactive)
  (calc-graph-delete-temps)
  (if (calc-gnuplot-alive)
      (calc-wrapper
       (or no-view (calc-graph-view-trail))
       (let ((calc-graph-no-wait t))
	 (calc-gnuplot-command "exit"))
       (sit-for 1)
       (if (process-status calc-gnuplot-process)
	   (delete-process calc-gnuplot-process))
       (setq calc-gnuplot-process nil))))

(defun calc-graph-quit ()
  (interactive)
  (if (get-buffer-window calc-gnuplot-input)
      (calc-graph-view-commands t))
  (if (get-buffer-window calc-gnuplot-buffer)
      (calc-graph-view-trail t))
  (calc-graph-kill t))

(defun calc-graph-view-commands (&optional no-need)
  (interactive "p")
  (or calc-graph-no-auto-view (calc-graph-init-buffers))
  (calc-graph-view calc-gnuplot-input calc-gnuplot-buffer (null no-need)))

(defun calc-graph-view-trail (&optional no-need)
  (interactive "p")
  (or calc-graph-no-auto-view (calc-graph-init-buffers))
  (calc-graph-view calc-gnuplot-buffer calc-gnuplot-input (null no-need)))

(defun calc-graph-view (buf other-buf need)
  (let (win)
    (or calc-graph-no-auto-view
	(if (setq win (get-buffer-window buf))
	    (or need
		(and (eq buf calc-gnuplot-buffer)
		     (save-excursion
		       (set-buffer buf)
		       (not (pos-visible-in-window-p (point-max) win))))
		(progn
		  (bury-buffer buf)
		  (bury-buffer other-buf)
		  (let ((curwin (selected-window)))
		    (select-window win)
		    (switch-to-buffer nil)
		    (select-window curwin))))
	  (if (setq win (get-buffer-window other-buf))
	      (set-window-buffer win buf)
	    (if (eq major-mode 'calc-mode)
		(if (or need
			(< (window-height) (1- (frame-height))))
		    (display-buffer buf))
	      (switch-to-buffer buf)))))
    (save-excursion
      (set-buffer buf)
      (if (and (eq buf calc-gnuplot-buffer)
	       (setq win (get-buffer-window buf))
	       (not (pos-visible-in-window-p (point-max) win)))
	  (progn
	    (goto-char (point-max))
	    (vertical-motion (- 6 (window-height win)))
	    (set-window-start win (point))
	    (goto-char (point-max)))))
    (or calc-graph-no-auto-view (sit-for 0))))

(defun calc-gnuplot-check-for-errors ()
  (if (save-excursion
	(prog2
	 (progn
	   (set-buffer calc-gnuplot-buffer)
	   (goto-char calc-gnuplot-last-error-pos))
	 (re-search-forward "^[ \t]+\\^$" nil t)
	 (goto-char (point-max))
	 (setq calc-gnuplot-last-error-pos (point-max))))
      (calc-graph-view-trail)))

(defun calc-gnuplot-command (&rest args)
  (calc-graph-init)
  (let ((cmd (concat (mapconcat 'identity args " ") "\n")))
    (accept-process-output)
    (save-excursion
      (set-buffer calc-gnuplot-buffer)
      (calc-gnuplot-check-for-errors)
      (goto-char (point-max))
      (setq calc-gnuplot-trail-mark (point))
      (or (>= calc-gnuplot-version 3)
	  (insert cmd))
      (set-marker (process-mark calc-gnuplot-process) (point))
      (process-send-string calc-gnuplot-process cmd)
      (if (get-buffer-window calc-gnuplot-buffer)
	  (calc-graph-view-trail))
      (accept-process-output (and (not calc-graph-no-wait)
				  calc-gnuplot-process))
      (calc-gnuplot-check-for-errors)
      (if (get-buffer-window calc-gnuplot-buffer)
	  (calc-graph-view-trail)))))

(defun calc-graph-init-buffers ()
  (or (and calc-gnuplot-buffer
	   (buffer-name calc-gnuplot-buffer))
      (setq calc-gnuplot-buffer (get-buffer-create "*Gnuplot Trail*")))
  (or (and calc-gnuplot-input
	   (buffer-name calc-gnuplot-input))
      (setq calc-gnuplot-input (get-buffer-create "*Gnuplot Commands*"))))

(defun calc-graph-init ()
  (or (calc-gnuplot-alive)
      (let ((process-connection-type t)
	    origin)
	(if calc-gnuplot-process
	    (progn
	      (delete-process calc-gnuplot-process)
	      (setq calc-gnuplot-process nil)))
	(calc-graph-init-buffers)
	(save-excursion
	  (set-buffer calc-gnuplot-buffer)
	  (insert "\nStarting gnuplot...\n")
	  (setq origin (point)))
	(setq calc-graph-last-device nil)
	(setq calc-graph-last-output nil)
	(condition-case err
	    (let ((args (append (and calc-gnuplot-display
				     (not (equal calc-gnuplot-display
						 (getenv "DISPLAY")))
				     (list "-display"
					   calc-gnuplot-display))
				(and calc-gnuplot-geometry
				     (list "-geometry"
					   calc-gnuplot-geometry)))))
	      (setq calc-gnuplot-process 
		    (apply 'start-process
			   "gnuplot"
			   calc-gnuplot-buffer
			   calc-gnuplot-name
			   args))
	      (process-kill-without-query calc-gnuplot-process))
	  (file-error
	   (error "Sorry, can't find \"%s\" on your system"
		  calc-gnuplot-name)))
	(save-excursion
	  (set-buffer calc-gnuplot-buffer)
	  (while (and (not (save-excursion
			     (goto-char origin)
			     (search-forward "gnuplot> " nil t)))
		      (memq (process-status calc-gnuplot-process) '(run stop)))
	    (accept-process-output calc-gnuplot-process))
	  (or (memq (process-status calc-gnuplot-process) '(run stop))
	      (error "Unable to start GNUPLOT process"))
	  (if (save-excursion
		(goto-char origin)
		(re-search-forward
		 "G N U P L O T.*\n.*version \\([0-9]+\\)\\." nil t))
	      (setq calc-gnuplot-version (string-to-int (buffer-substring
							 (match-beginning 1)
							 (match-end 1))))
	    (setq calc-gnuplot-version 1))
	  (goto-char (point-max)))))
  (save-excursion
    (set-buffer calc-gnuplot-input)
    (if (= (buffer-size) 0)
	(insert "# Commands for running gnuplot\n\n\n")
      (or calc-graph-no-auto-view
	  (eq (char-after (1- (point-max))) ?\n)
	  (progn
	    (goto-char (point-max))
	    (insert "\n"))))))

;;; calc-graph.el ends here
