;;; cmacexp.el --- expand C macros in a region

;; Copyright (C) 1992 Free Software Foundation, Inc.

;; Author: Francesco Potorti` <pot@cnuce.cnr.it>
;; Version: $Id: cmacexp.el,v 1.10 1994/02/25 06:27:24 rms Exp rms $
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

;; USAGE =============================================================

;; In C mode C-M-x is bound to c-macro-expand.  The result of the
;; expansion is put in a separate buffer.  The buffer is put in
;; view-mode if the Inge Frick's view.el is installed.  A user option
;; allows the window displaying the buffer to be optimally sized.
;;
;; When called with a C-u prefix, c-macro-expand replaces the selected
;; region with the expansion.  Both the preprocessor name and the
;; initial flag can be set by the user.  If c-macro-prompt-p
;; is set to a non-nil value the user is offered to change the flags
;; to the preprocessor each time c-macro-expand is invoked.
;; Preprocessor arguments default to the last ones entered.
;; If c-macro-prompt is nil, one must use M-x set-variable to set a
;; different value for c-macro-cppflags.

;; A c-macro-expansion function is provided for non-interactive use.

;; INSTALLATION ======================================================

;; Put this file on your load-path, byte compile it for increased
;; speed and put part or all of the following in your ~/.emacs file.

;; To make a directory ~/emacs be in front of your load-path:
;;(setq load-path (cons (expand-file-name "~/emacs") load-path))
;;
;; Suggested keybinding (work only in c-mode):
;;(if (boundp 'c-mode-map)
;;  (define-key c-mode-map "\C-x\C-e" 'c-macro-expand))
;;(if (boundp 'c++-mode-map)
;;  (define-key c++-mode-map "\C-x\C-e" 'c-macro-expand))
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
;; If you often use a particular set of flags:
;;(setq c-macro-cppflags "-I /usr/include/local -DDEBUG"
;;
;; If you want the "Preprocessor arguments: " prompt:
;;(setq c-macro-prompt-p t)

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
;; reporting and suggestion of new features and to Inge Fricks for her
;; help with view.el.  This work has been partially inspired by Don
;; Maszle and Jonathan Segal's.

;; By the way, I recommend you Inge Frick's view.el.  It works like
;; the standard view, but *it is not recursive* and has some more
;; commands.  Moreover it is a minor mode, so you preserve all your
;; major mode keybindings (well, not always :).  Mail me to obtain a
;; copy, or get it by anonymous ftp in fly.cnuce.cnr.it:pub/view.el.

;; BUGS ==============================================================

;; If the start point of the region is inside a macro definition the
;; macro expansion is often inaccurate.


(provide 'cmacexp)

(defvar c-macro-shrink-window-p nil
  "*Non-nil means shrink the *Macroexpansion* window to fit its contents.")

(defvar c-macro-prompt-p nil
  "*Non-nil makes c-macro-expand prompt for preprocessor arguments.")

(defvar c-macro-preprocessor "/lib/cpp -C" "\
The preprocessor used by the cmacexp package.

If you change this, be sure to preserve the -C (don't strip comments)
option, or to set an equivalent one.")

(defvar c-macro-cppflags ""
  "*Preprocessor flags used by c-macro-expand.")

(defconst c-macro-buffer-name "*Macroexpansion*")

(defun c-macro-expand (start end subst) "\
Expand all C macros occurring in the region using c-macro-preprocessor.
Normally display output in temp buffer.
Prefix arg means replace the region with it.
Prompt for a string of arguments to the preprocessor
\(e.g. -DDEBUG -I ./include) if the user option c-macro-prompt-p is non-nil.

Noninteractive args are START, END, SUBST.
For use inside programs see also c-macro-expansion."

  (interactive "r\nP")
  (let ((inbuf (current-buffer))
	(displaybuf (if subst
			(get-buffer c-macro-buffer-name)
		      (get-buffer-create c-macro-buffer-name)))
	(expansion "")
	(mymsg ""))
    ;; Build the command string.
    (if c-macro-prompt-p
	(setq c-macro-cppflags
	      (read-string "Preprocessor arguments: "
			   c-macro-cppflags)))
    (setq mymsg (format "Invoking %s%s%s on region..."
			c-macro-preprocessor
			(if (string= "" c-macro-cppflags) "" " ")
			c-macro-cppflags))
    ;; Decide where to display output.
    (if (and subst
	     (and buffer-read-only (not inhibit-read-only))
	     (not (eq inbuf displaybuf)))
	(progn
	  (message
	   "Buffer is read only: displaying expansion in alternate window")
	  (sit-for 2)
	  (setq subst nil)
	  (or displaybuf
	      (setq displaybuf (get-buffer-create c-macro-buffer-name)))))
    ;; Expand the macro and output it.
    (message mymsg)
    (setq expansion (c-macro-expansion start end
				       (concat c-macro-preprocessor " "
					       c-macro-cppflags)))
    (message (concat mymsg "done"))
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
	(c-macro-display-buffer))
      (setq buffer-read-only t)
      (setq buffer-auto-save-file-name nil)
      (bury-buffer displaybuf))))


;; Display the current buffer in a window which is either just large
;; enough to contain the entire buffer, or half the size of the
;; screen, whichever is smaller.  Do not select the new
;; window.
;;
;; Several factors influence window resizing so that the window is
;; sized optimally if it is created anew, and so that it is messed
;; with minimally if it has been created by the user.  If the window
;; chosen for display exists already but contains something else, the
;; window is not re-sized.  If the window already contains the current
;; buffer, it is never shrunk, but possibly expanded.  Finally, if the
;; variable c-macro-shrink-window-p is nil the window size is *never*
;; changed.
(defun c-macro-display-buffer ()

  (goto-char (point-min))
  (c-mode)
  (let ((oldwinheight (window-height))
	(alreadythere			;the window was already there
	 (get-buffer-window (current-buffer)))
	(popped nil))			;the window popped changing the layout 
    (or alreadythere
	(progn
	  (display-buffer (current-buffer) t)
	  (setq popped (/= oldwinheight (window-height)))))
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
					 (+ 2 (vertical-motion (point-max)))))
			       (window-height)))
	    (goto-char (point-min))
	    (select-window oldwin))))))


(defun c-macro-expansion (start end cppcommand) "\
Run a preprocessor on region and return the output as a string.
Expand the region between START and END in the current buffer using
the shell command CPPCOMMAND (e.g. \"/lib/cpp -C -DDEBUG\").
Be sure to use a -C (don't strip comments) or equivalent option."

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
	(start-state)
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

	  (save-excursion
	    (goto-char start)
	    (setq start-state (parse-partial-sexp 1 (point))))
	  ;; Now we insert the #line directives after all #endif or
	  ;; #else following START. 
	  ;(switch-to-buffer outbuf) (debug)	;debugging instructions
	  (while (re-search-backward "\n#\\(endif\\|else\\)\\>" start 'move)
	    (if (equal (nthcdr 3 (parse-partial-sexp start (point) start-state))
		       '(nil nil nil 0)) ;neither in string nor in
					 ;comment nor after quote
		(progn
		  (goto-char (match-end 0))
;;;		  (setq linenum (count-lines 1 (point)))
		  (setq linelist
			;; This used to be a #line command
			;; but it's not guaranteed that the output
			;; will have properly matching commands.
			;; Only the *line numbers* have to agree!
			(cons (format "\n???!!!???!!!!\n")
			      linelist))
		  (insert (car linelist))
		  (skip-chars-backward "^#")
		  (insert "line")
	      (goto-char (match-beginning 0)))))

	  ;; We are at START.  Insert the first #line directive.  This
	  ;; must work even inside a string or comment, or after a
	  ;; quote.
;;;	  (setq linenum (+ (count-lines 1 (point))
;;;			   (if (bolp) 1 0)))
	  (setq linelist
		(cons
		 (let* ((startstat (parse-partial-sexp 1 start))
			(startinstring (nth 3 startstat))
			(startincomment (nth 4 startstat))
			(startafterquote (nth 5 startstat)))
		   (concat (if startafterquote " ")
			   (cond (startinstring (char-to-string startinstring))
				 (startincomment "*/"))
			   (format "\n???!!!???!!!!")
			   (cond (startinstring (char-to-string startinstring))
				 (startincomment "/*"))
			   (if startafterquote "\\")))
		 linelist))
	  (insert (car linelist))
	  (skip-chars-backward "^#")
	  (insert "line")

	  ;; Call the preprocessor.
	  (call-process-region 1 (point-max) "sh" t t nil "-c"
			       (concat cppcommand " 2>/dev/null"))

	  (while (search-backward "\n???!!!???!!!!" nil t)
	    (replace-match ""))
	  
	  ;; Compute the return value, keeping in account the space
	  ;; inserted at the end of the buffer.
	  (buffer-substring (point) (max (point) (- (point-max) 1))))

      ;; Cleanup.
      (kill-buffer outbuf))))

;;; cmacexp.el ends here
