;;; cmacexp.el --- expand C macros in a region

;; Copyright (C) 1992 Free Software Foundation, Inc.

;; Author: Francesco Potorti` <pot@cnuce.cnr.it>
;; Version: $Id: cmacexp.el 1.2 1992/09/15 11:34:56 pot Exp $
;; Adapted-By: ESR
;; Keywords: c

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

;; In C mode C-M-x is bound to c-macro-expand.  The result of the
;; expansion is put in a separate buffer.  The buffer is put in
;; view-mode if the Inge Frick's view.el is installed.  A user option
;; allows the window displaying the buffer to be optimally sized.
;;
;; When called with a C-u prefix, c-macro-expand replaces the selected
;; region with the expansion.  With two C-u's the user is offered to
;; change the flags to the preprocessor (while the results of the
;; expansion go to a separate buffer).  Preprocessor arguments default
;; to the last ones entered.  Both the preprocessor name and the
;; initial flag defaults can be set by the user.  Setting
;; c-macro-always-prompt to a non-nil value allows one to be always
;; prompted for the flags, regardless of the prefix used.

;; A c-macro-expansion function is provided for non-interactive use.
;; A still experimental function c-macro-eval is provided.  It aims at
;; evaluating the contents of a region by using calc (by Dave
;; Gillespie).  Select a region and type C-x C-e (if you followed the
;; suggestions in the INSTALLATION section) or type M-x c-ma RET v
;; RET.  If you have calc installed, the computed value of the
;; expression will appear in the message area.  If you give an
;; interactive C-u prefix the computed value will be shown in signed,
;; unsigned, hex and boolean representations.  Two C-u's allow to
;; change the preprocessor flags via prompt.  c-macro-eval works well
;; for constant expressions, but see the BUG section.

;; A patch to calc 2.02 has been written by Dave Gillespie.  It can
;; be downloaded via anonymous ftp at fly.cnuce.cnr.it:pub/calc.diff.

;; INSTALLATION ======================================================

;; Put this file on your load-path, byte compile it for increased
;; speed and put part or all of the following in your ~/.emacs file.

;; To make a directory ~/emacs be in front of your load-path:
;;(setq load-path (cons (expand-file-name "~/emacs") load-path))
;;
;; Suggested keybindings (work only in c-mode):
;;(define-key c-mode-map "\C-\M-x" 'c-macro-expand)
;;(define-key c-mode-map "\C-x\C-e" 'c-macro-eval)
;;
;; If you want the *Macroexpansion* window to be not higher than
;; necessary: 
;;(setq c-macro-shrink-window-p t)
;;
;; If you use a preprocessor other than /lib/cpp (be careful to set a
;; -C option or equivalent in order to make the preprocessor not to
;; strip the comments):
;;(setq c-macro-preprocessor "gpp -C")
;;
;; If you often use a particular set of flags, and want them to be
;; the default:
;;(setq c-macro-default-cppflags "-I /usr/include/local -DDEBUG"
;;
;; If you always want the "Preprocessor arguments: " prompt,
;; regardless of the arguments provided:
;;(setq c-macro-always-prompt-p t)
;;
;; If you want to experiment with the C constant expressions
;; evaluation feature:
;;(autoload 'c-macro-eval "cmacexp"
;;  "C constant expressions evaluation.  Requires calc.  Experimental." t)

;; BUG REPORTS =======================================================

;; Please report bugs, suggestions, complaints and so on to
;; pot@cnuce.cnr.it (Francesco Potorti`).

;; IMPROVEMENTS OVER emacs 18.xx cmacexp.el ==========================

;; - A lot of user visible changes.  See above.
;; - #line directives are inserted, so __LINE__ and __FILE__ are
;;   correctly expanded.  Works even with START inside a string, a
;;   comment or a region #ifdef'd away by cpp. cpp is invoked with -C,
;;   making comments visible in the expansion.
;; - All work is done in core memory, no need for temporary files.
;; - The /lib/cpp process is run synchronously.  This fixes an
;;   infinite loop bug on Motorola Delta (cpp waiting forever for
;;   end-of-file, don't know why).  Fixes a similar intermittent
;;   problem on SunOS 4.1.

;; ACKNOWLEDGEMENTS ==================================================

;; A lot of thanks to Don Maszle who did a great work of testing, bug
;; reporting and suggestion of new features, to Inge Fricks for her
;; help with view.el and to Dave Gillespie for his suggestions on
;; calc's use.  This work has been partially inspired by Don Maszle
;; and Jonathan Segal's.

;; By the way, I recommend you Inge Frick's view.el.  It works like
;; the standard view, but *it is not recursive* and has some more
;; commands.  Moreover it is a minor mode, so you preserve all your
;; major mode keybindings (well, not always :).  Mail me to obtain a
;; copy, or get it by anonymous ftp in fly.cnuce.cnr.it:pub/view.el.

;; BUGS ==============================================================

;; calc 2.02 does not handle the C operators "->", ".", "*" (as a
;; prefix), the composite assignement operators "+=" etc.  It cannot
;; handle the "," operator and will be confused by ";".  Almost all
;; these can be defined as no-ops using the Calc's Syntax Tables
;; feature.  The built-in calc functions will cause problems in
;; certain circumstances.  c-macro-eval behaves correctly only on
;; expressions not containing such operators.  Does not distinguish
;; among integer and real division.

;; If the start point of the region is inside a macro definition the
;; macro expansion is often inaccurate.

;;; Code:

(defvar c-macro-shrink-window-p nil
  "*Non-nil means shrink the *Macroexpansion* window to fit its contents.")

(defvar c-macro-always-prompt-p nil
  "*Non-nil means always prompt for preprocessor arguments.")

(defvar c-macro-preprocessor "/lib/cpp -C" "\
The preprocessor used by the cmacexp package.

If you change this, be sure to preserve the -C (don't strip comments)
option, or to set an equivalent one.")

(defvar c-macro-default-cppflags ""
  "Default cpp flags used by c-macro-expand.")

(defconst c-macro-buffer-name "*Macroexpansion*")

(defun c-macro-expand (start end &optional flag) "\
Expand all C macros occurring in the region using c-macro-preprocessor.
Normally display output in temp buffer.
Prefix arg means replace the region with it.
Prompt for a string of arguments to the preprocessor, (e.g.
-DDEBUG -I ./include) when prefixed with two C-u's.

It is intended for interactive use only.
For non interactive use, see the c-macro-expansion function."

  (interactive "r\nP")
  (let* ((subst (and flag (not (equal flag '(16)))))
	 (inbuf (current-buffer))
	 (displaybuf (if subst
			 (get-buffer c-macro-buffer-name)
		       (get-buffer-create c-macro-buffer-name)))
	 (expansion ""))
    ;; Build the command string.
    (if (or c-macro-always-prompt-p (equal flag '(16)))
	(setq c-macro-default-cppflags
	      (read-string "Preprocessor arguments: "
			   c-macro-default-cppflags)))
    ;; Decide where to display output.
    (if (and subst
	     buffer-read-only
	     (not (eq inbuf displaybuf)))
	(progn
	  (message
	   "Buffer is read only: displaying expansion in alternate window")
	  (sit-for 2)
	  (setq subst nil)
	  (or displaybuf
	      (setq displaybuf (get-buffer-create c-macro-buffer-name)))))
    ;; Expand the macro and output it.
    (if (interactive-p) (message (c-macro-default-message)))
    (setq expansion
	  (c-macro-expansion start end
			     (concat c-macro-preprocessor " "
				     c-macro-default-cppflags)))
    (message (concat (c-macro-default-message) "done"))
    (if subst
	(let ((exchange (= (point) start)))
	  (delete-region start end)
	  (insert expansion)
	  (if exchange
	      (exchange-point-and-mark)))
      (set-buffer displaybuf)
      (setq buffer-read-only nil)
      (buffer-flush-undo displaybuf)
      (erase-buffer)
      (insert expansion)
      (set-buffer-modified-p nil)
      (if (string= "" expansion)
	  (message "Null expansion")
	(c-macro-display-buffer inbuf))
      (setq buffer-read-only t)
      (bury-buffer displaybuf))))


;; Display the current buffer in a window which is either just large
;; enough to contain the entire buffer, or half the size of the
;; screen, whichever is smaller.  Put the current buffer in view-mode
;; if the Inge Frick's view-mode is installed, with buffer to return
;; to set to RETBUF (if sensible). Do not select the new window.
;;
;; Several factors influence window resizing so that the window is
;; sized optimally if it is created anew, and so that it is messed
;; with minimally if it has been created by the user.  If the window
;; chosen for display exists already but contains something else, the
;; window is not re-sized.  If the window already contains the current
;; buffer, it is never shrunk, but possibly expanded.  Finally, if the
;; variable c-macro-shrink-window-p is nil the window size is *never*
;; changed.
(defun c-macro-display-buffer (retbuf)

  (goto-char (point-min))
  (c-mode)
  (require 'view)			;load view.el
  (let ((oldwinheight (window-height))
	(alreadythere			;the window was already there
	 (get-buffer-window (current-buffer)))
	(popped nil)			;the window popped changing the layout 
	(niceview			;is this Inge Fricks's view.el?
	 (boundp 'view-kill-when-finished)))

    (or alreadythere
	(progn
	  (display-buffer (current-buffer) t)
	  (setq popped (/= oldwinheight (window-height)))))
    (if niceview
	(view-mode 1))			;set view mode
    (if (and c-macro-shrink-window-p	;user wants fancy shrinking :\)
	     (or alreadythere popped))
	;; Enlarge up to half screen, or shrink properly.
	(let ((oldwin (selected-window))
	      (minheight 0)
	      (maxheight 0))
	  (save-excursion
	    (select-window (get-buffer-window (current-buffer)))
	    (setq minheight (if alreadythere
				(window-height)
			      window-min-height))
	    (setq maxheight (/ (screen-height) 2))
	    (enlarge-window (- (min maxheight
				    (max minheight
					 (+ 2 (vertical-motion 1000000))))
			       (window-height)))
	    (goto-char (point-min))
	    (select-window oldwin))))))


(defun c-macro-expansion (start end cppcommand) "\
Expands the region between START and END in the current buffer using
the shell command CPPCOMMAND (e.g. \"/lib/cpp -C -DDEBUG\").  Be sure
to use a -C (don't strip comments) or equivalent option.
Returns the output as a string."

;; Copy the current buffer's contents to a temporary hidden buffer.
;; Delete from END to end of buffer.  Insert a preprocessor #line
;; directive at START and after each #endif following START that are
;; not inside a comment or a string.  Put all the strings thus
;; inserted (without the "line" substring) in a list named linelist.
;; If START is inside a comment, prepend "*/" and append "/*" to the
;; #line directive.  If inside a string, prepend and append "\"".
;; Preprocess the buffer contents, then look for all the lines stored
;; in linelist starting from end of buffer.  The last line so found is
;; where START was, so return the substring from point to end of
;; buffer. 
  (let ((inbuf (current-buffer))
	(outbuf (get-buffer-create " *C Macro Expansion*"))
	(filename (if (and buffer-file-name
			   (string-match (regexp-quote default-directory)
					 buffer-file-name))
		      (substring buffer-file-name (match-end 0))
		    (buffer-name)))
	(linenum 0)
	(linelist ()))
    (unwind-protect
	(save-excursion
	  (save-restriction
	    (widen)
	    (set-buffer outbuf)
	    (setq buffer-read-only nil)
	    (erase-buffer)
	    (set-syntax-table c-mode-syntax-table)
	    (insert-buffer-substring inbuf 1 end))

	  ;; We have copied inbuf to outbuf.  Point is at end of
	  ;; outbuf.  Insert a space at the end, so cpp can correctly
	  ;; parse a token ending at END. 

	  (insert " ")

	  ;; Now we insert the #line directives after all #endif or
	  ;; #else following START. 
	  ;(switch-to-buffer outbuf) (debug)	;debugging instructions
	  (while (re-search-backward "\n#\\(endif\\|else\\)\\>" start 'move)
	    (if (equal (nthcdr 3 (parse-partial-sexp 1 (point)))
		       '(nil nil nil 0)) ;neither in string nor in
					 ;comment nor after quote
		(progn
		  (goto-char (match-end 0))
		  (setq linenum (count-lines 1 (point)))
		  (setq linelist
			(cons (format "\n# %d \"%s\"\n" linenum filename)
			      linelist))
		  (insert (car linelist))
		  (skip-chars-backward "^#")
		  (insert "line")
	      (goto-char (match-beginning 0)))))

	  ;; We are at START.  Insert the first #line directive.  This
	  ;; must work even inside a string or comment, or after a
	  ;; quote.
	  (setq linenum (+ (count-lines 1 (point))
			   (if (bolp) 1 0)))
	  (setq linelist
		(cons
		 (let* ((startstat (parse-partial-sexp 1 start))
			(startinstring (nth 3 startstat))
			(startincomment (nth 4 startstat))
			(startafterquote (nth 5 startstat)))
		   (concat (if startafterquote " ")
			   (cond (startinstring "\"") (startincomment "*/"))
			   (format "\n# %d \"%s\"\n" linenum filename)
			   (cond (startinstring "\"") (startincomment "/*"))
			   (if startafterquote "\\")))
		 linelist))
	  (insert (car linelist))
	  (skip-chars-backward "^#")
	  (insert "line")

	  ;; Call the preprocessor.
	  (call-process-region 1 (point-max) "sh" t t nil "-c"
			       (concat cppcommand " 2>/dev/null"))

	  ;; Look for the `# nn "file.c"' lines from the last to the first
	  ;; and delete them.
	  (setq linelist (reverse linelist))
	  (while (progn
		   (if (search-backward (car linelist) nil t)
		       (replace-match ""))
		   (setq linelist (cdr linelist))))
	  
	  ;; Compute the return value, keeping in account the space
	  ;; inserted at the end of the buffer.
	  (buffer-substring (point) (max (point) (- (point-max) 1))))

      ;; Cleanup.
      (kill-buffer outbuf))))


;; Experimental.  With an argument, print signed, unsigned, hex and
;; boolean representations.
(defun c-macro-eval (start end &optional flag) "\
Expand region using cpp and evaluate it using calc.
Interactively print value in minibuffer and push it on the kill ring.
With a C-u argument shows the evaluation in a variety of formats.
With two C-u's prompts the user for a string of flags to the preprocessor.

Non interactively returns value of region between START and END
as a string.  Several formats are used if optional FLAG is non-nil."

  (interactive "r\nP")
  (or (fboundp 'calc-eval)
      (require 'calc))
  (if (or c-macro-always-prompt-p (equal flag '(16)))
      (setq c-macro-default-cppflags
	    (read-string "Preprocessor arguments: "
			 c-macro-default-cppflags)))

  ;; Expand the region.
  (if (interactive-p) (message (c-macro-default-message)))
  (let ((evaluation
	 (c-macro-expansion start end
			    (concat c-macro-preprocessor " "
				    c-macro-default-cppflags)))
	(evalbuf (get-buffer-create " *Macro Evaluation*")))
    (unwind-protect
	(save-excursion
	  (set-buffer evalbuf)
	  (setq buffer-read-only nil)
	  (erase-buffer)
	  (insert evaluation)

	  ;; Evaluate expression(s).
	  (if (interactive-p)
	      (message "Invoking calc..."))
	  (setq evaluation
		(let ((calc-eval-error t))
		  (calc-eval (list (buffer-string) 'calc-language 'c))))
	  (erase-buffer)
	  (cond
	   (flag
	    (insert (calc-eval (list evaluation
				     'calc-language 'c
				     'calc-simplify-mode 'binary))
		    "(u)" " == "
		    (calc-eval (list evaluation
				     'calc-language 'c
				     'calc-word-size (- calc-word-size)
				     'calc-simplify-mode 'binary))
		    "(d)" " == "
		    (calc-eval (list evaluation
				     'calc-language 'c
				     'calc-number-radix 16
				     'calc-simplify-mode 'binary))
		    "(x)")
	    (save-excursion
	      (insert " == " (calc-eval (list evaluation
					      'calc-language 'c
					      'calc-number-radix 16
					      'calc-simplify-mode 'binary))))
	    (while (re-search-forward "0x\\([^,]+\\)\\(, \\|\\'\\)" nil t)
	      (if (string= "0"
			   (buffer-substring (match-beginning 1)
					     (match-end 1)))
		  (replace-match "FALSE\\2")
		(replace-match "TRUE\\2"))))
	   (t
	    (insert evaluation)))

	  ;; Output the evaluation.
	  (if (interactive-p)
	      (progn
		(copy-region-as-kill 1 (point-max))
		(message (buffer-string)))
	    (buffer-string)))
      (kill-buffer evalbuf))))

(defun c-macro-default-message ()
  (format "Invoking %s%s%s on region..."
	  c-macro-preprocessor
	  (if (string= "" c-macro-default-cppflags) "" " ")
	  c-macro-default-cppflags))

(provide 'cmacexp)

;;; cmacexp.el ends here.
