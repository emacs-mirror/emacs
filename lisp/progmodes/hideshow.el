;;; hideshow.el --- minor mode cmds to selectively display blocks of code

;; Copyright (C) 1994,1995,1996,1997 Free Software Foundation

;; Author: Thien-Thi Nguyen <ttn@netcom.com>
;; Maintainer: Dan Nicolaescu <done@ece.arizona.edu>
;; Version: 4.0
;; Keywords: C C++ java lisp tools editing comments blocks hiding
;; Time-of-Day-Author-Most-Likely-to-be-Recalcitrant: early morning

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; LCD Archive Entry:
;; hideshow|Thien-Thi Nguyen|ttn@netcom.com|
;; minor mode commands to selectively display blocks of code|
;; 18-Oct-1994|3.4|~/modes/hideshow.el.Z|

;;; Commentary:

;; This file provides `hs-minor-mode'.  When active, six commands:
;;   hs-{hide,show}-{all,block}, hs-show-region and hs-minor-mode
;; are available.  They implement block hiding and showing.  Blocks are
;; defined in mode-specific way.  In c-mode or c++-mode, they are simply
;; curly braces, while in lisp-ish modes they are parens.  Multi-line
;; comments (c-mode) can also be hidden.  The command M-x hs-minor-mode
;; toggles the minor mode or sets it (similar to outline minor mode).
;; See documentation for each command for more info.
;;
;; The variable `hs-unbalance-handler-method' controls hideshow's behavior
;; in the case of "unbalanced parentheses".  See doc for more info.

;; Suggested usage:

;; (load-library "hideshow")
;; (add-hook 'X-mode-hook 'hs-minor-mode)   ; other modes similarly
;;
;; where X = {emacs-lisp,c,c++,perl,...}.  See the doc for the variable
;; `hs-special-modes-alist' if you'd like to use hideshow w/ other modes.

;; Etc:

;; Bug reports and fixes welcome (comments, too).  Thanks go to
;;	Dean Andrews <adahome@ix.netcom.com>
;;	Preston F. Crow <preston.f.crow@dartmouth.edu>
;;	Gael Marziou <gael@gnlab030.grenoble.hp.com>
;;	Keith Sheffield <sheff@edcsgw2.cr.usgs.gov>
;;	Jan Djarv <jan.djarv@sa.erisoft.se>
;;	Lars Lindberg <qhslali@aom.ericsson.se>
;;	Alf-Ivar Holm <alfh@ifi.uio.no>
;; for valuable feedback, code and bug reports.

;;; Code:


;;;----------------------------------------------------------------------------
;;; user-configurable variables

(defvar hs-unbalance-handler-method 'top-level
  "*Symbol representing how \"unbalanced parentheses\" should be handled.
This error is usually signaled by `hs-show-block'.  One of four values:
`top-level', `next-line', `signal' or `ignore'.  Default is `top-level'.

- `top-level' -- Show top-level block containing the currently troublesome
                 block.
- `next-line' -- Use the fact that, for an already hidden block, its end
		 will be on the next line.  Attempt to show this block.
- `signal' -- Pass the error through, stopping execution.
- `ignore' -- Ignore the error, continuing execution.

Values other than these four will be interpreted as `signal'.") 

;;;#autoload
(defvar hs-special-modes-alist 
  '((c-mode "{" "}")
    (c++-mode "{" "}")
    (java-mode "\\(\\(public\\|private\\|protected\\|static\\|\\s-\\)+\\([a-zA-Z0-9_:]+[ \t]+\\)\\([a-zA-Z0-9_:]+\\)[ \t]*([^)]*)[ \t\n]*\\([ \t\n]throws[ \t]+[^{]+\\)*[ \t]*\\){" "}" java-hs-forward-sexp))
  "*Alist of the form (MODE START-RE END-RE FORWARD-SEXP-FUNC).
If present, hideshow will use these values for the start and end regexps,
respectively.  Since Algol-ish languages do not have single-character
block delimiters, the function `forward-sexp' which is used by hideshow
doesn't work.  In this case, if a similar function is provided, you can
register it and have hideshow use it instead of `forward-sexp'.  To add
more values, use

\t(pushnew '(new-mode st-re end-re function-name)
\t	hs-special-modes-alist :test 'equal)

For example:

\t(pushnew '(simula-mode \"begin\" \"end\" simula-next-statement)
\t	hs-special-modes-alist :test 'equal)

Note that the regexps should not contain leading or trailing whitespace.")

(defvar hs-minor-mode-hook 'hs-hide-initial-comment-block
  "Hook called when `hs-minor-mode' is installed.
A good value for this would be `hs-hide-initial-comment-block' to
hide all the comments at the beginning of the file.")

(defvar hs-hide-hook nil
  "*Hooks called at the end of `hs-hide-all' and `hs-hide-block'.")

(defvar hs-show-hook nil
  "*Hooks called at the end of commands to show text.
These commands include `hs-show-all', `hs-show-block' and `hs-show-region'.")

(defvar hs-minor-mode-prefix "\C-c"
  "*Prefix key to use for hideshow commands in hideshow minor mode.")


;;;----------------------------------------------------------------------------
;;; internal variables

(defvar hs-minor-mode nil
  "Non-nil if using hideshow mode as a minor mode of some other mode.
Use the command `hs-minor-mode' to toggle this variable.")

(defvar hs-minor-mode-map nil
  "Mode map for hideshow minor mode.")

(defvar hs-menu-bar nil
  "Menu bar for hideshow minor mode (Xemacs only).")

(defvar hs-c-start-regexp nil
  "Regexp for beginning of comments.  
Differs from mode-specific comment regexps in that 
surrounding whitespace is stripped.")

(defvar hs-c-end-regexp nil
  "Regexp for end of comments.
See `hs-c-start-regexp'.")

(defvar hs-block-start-regexp nil
  "Regexp for beginning of block.")

(defvar hs-block-end-regexp nil
  "Regexp for end of block.")

(defvar hs-forward-sexp-func 'forward-sexp
  "Function used to do a forward-sexp.
Should change for Algol-ish modes.  For single-character block
delimiters -- ie, the syntax table regexp for the character is
either `(' or `)' -- `hs-forward-sexp-func' would just be `forward-sexp'.
For other modes such as simula, a more specialized function
is necessary.")

(defvar hs-hide-comments-when-hiding-all t 
  "Hide the comments too when you do an `hs-hide-all'." )

;(defvar hs-emacs-type 'fsf
;  "Used to support both Emacs and Xemacs.")

;(eval-when-compile
;  (if (string-match "xemacs\\|lucid" emacs-version)
;      (progn
;	(defvar current-menubar nil "")
;	(defun set-buffer-menubar (arg1))
;	(defun add-menu (arg1 arg2 arg3)))))

;;;----------------------------------------------------------------------------
;;; support funcs

;; snarfed from noutline.el;
(defun hs-flag-region (from to flag)
  "Hides or shows lines from FROM to TO, according to FLAG.
If FLAG is nil then text is shown, while if FLAG is t the text is hidden."
    (save-excursion
      (goto-char from)
      (end-of-line)
      (hs-discard-overlays (point) to 'invisible 'hs)
      (if flag
	  (let ((overlay (make-overlay (point) to)))
	    ;; Make overlay hidden and intangible.
	    (overlay-put overlay 'invisible 'hs)
	    (overlay-put overlay 'hs t)
	    (overlay-put overlay 'intangible t)))))

;; Remove from the region BEG ... END all overlays
;; with a PROP property equal to VALUE.
;; Overlays with a PROP property different from VALUE are not touched.
(defun hs-discard-overlays (beg end prop value)
  (if (< end beg)
      (setq beg (prog1 end (setq end beg))))
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (let ((overlays (overlays-at (point))))
	(while overlays
	  (let ((o (car overlays)))
	    (if (eq (overlay-get o prop) value)
		(if (or 
		     (and (> (overlay-end o) beg) (< (overlay-end o) end))
		     (and (< (overlay-start o) beg) (< (overlay-start o) end)))
		    (delete-overlay o))))
	  (setq overlays (cdr overlays))))
      (goto-char (next-overlay-change (point))))))

(defun hs-hide-block-at-point (&optional end comment c-reg)
  "Hide block iff on block beginning, optional END means reposition at end.
COMMENT true means that it should hide a comment block, C-REG is a list
of the forme (BEGIN . END) and specifies the limits of the comment." 
  (if comment
      (let ((reg (if c-reg  c-reg (hs-inside-comment-p))))
	(goto-char (nth 1 reg))
	(forward-line -1)
	(end-of-line)
	(hs-flag-region (car reg)  (point) t)
	(goto-char (if end (nth 1 reg) (car reg)))
	)
      (if (looking-at hs-block-start-regexp)
	  (let* ((p (point))
		 (q (progn (funcall hs-forward-sexp-func 1) (point))))
	    (forward-line -1) (end-of-line)
	    (if (and (< p (point)) (> (count-lines p q) 1))
	    (hs-flag-region p (point) t))
	(goto-char (if end q p))))))

(defun hs-show-block-at-point (&optional end)
  "Show block iff on block beginning.  Optional END means reposition at end."
  (if (looking-at hs-block-start-regexp)
      (let* ((p (point))
	     (q
	      (condition-case error	; probably unbalanced paren
		  (progn
		    (funcall hs-forward-sexp-func 1)
		    (point))
		(error
		 (cond
		  ((eq hs-unbalance-handler-method 'ignore)
		   ;; just ignore this block
		   (point))
		  ((eq hs-unbalance-handler-method 'top-level)
		   ;; try to get out of rat's nest and expose the whole func
		   (if (/= (current-column) 0) (beginning-of-defun))
		   (setq p (point))
		   (re-search-forward (concat "^" hs-block-start-regexp)
				      (point-max) t 2)
		   (point))
		  ((eq hs-unbalance-handler-method 'next-line)
		   ;; assumption is that user knows what s/he's doing
		   (beginning-of-line) (setq p (point))
		   (end-of-line 2) (point))
		  (t
		   ;; pass error through -- this applies to `signal', too
		   (signal (car error) (cdr error))))))))
	(hs-flag-region p q nil)
	(goto-char (if end (1+ (point)) p)))))

(defun hs-safety-is-job-n ()
  "Warn `buffer-invisibility-spec' does not contain hs."
    (if (or buffer-invisibility-spec (assq hs buffer-invisibility-spec) )
	nil
      (message "Warning: `buffer-invisibility-spec' does not contain hs!!")
      (sit-for 2)))

(defun hs-hide-initial-comment-block ()
  (interactive)
  "Hides the first block of comments in a file.
The best usage is in `hs-minor-mode-hook', it hides all the comments at the
file beginning, so if you have huge RCS logs you won't see them!"
      (let ((p (point))
	    c-reg)
	(goto-char (point-min))
	(skip-chars-forward " \t\n")
	(setq c-reg (hs-inside-comment-p))
	(if (and c-reg (> (count-lines (car c-reg) (nth 1 c-reg)) 2))
	    (hs-hide-block)
	  (goto-char p))))

(defun hs-inside-single-line-comment-p ()
  "Look to see if we are on a single line comment."
  (save-excursion
    (beginning-of-line)
    (looking-at (concat "^[ \t]*" hs-c-start-regexp))))

(defun hs-inside-comment-p ()
  "Returns non-nil if point is inside a comment, otherwise nil.
Actually, returns a list containing the buffer position of the start 
and the end of the comment."
  (save-excursion
    (let ((p (point))
	  q
	  p-aux)
      (if (string= comment-end "")	; single line
	    (if (not (hs-inside-single-line-comment-p))
		nil
	      ;;find-beginning-of-the-chained-single-line-comments
	      (beginning-of-line)
	      (forward-comment (- (buffer-size)))
	      (skip-chars-forward " \t\n")
	      (beginning-of-line)
	      (setq q (point))
	      (goto-char p)
	      ;;find-end-of-the-chained-single-line-comments
	      (forward-comment (buffer-size))
	      (skip-chars-backward " \t\n")
	      (list q   (point)))
	(re-search-forward hs-c-end-regexp (point-max) 1)
	(forward-comment (buffer-size))
	(skip-chars-backward " \t\n")
	(end-of-line)
	(setq q (point))
	(forward-comment (- 0 (buffer-size)))
	(re-search-forward hs-c-start-regexp (point-max) 1)
	(setq p-aux (- (point) (length comment-start)))
	(if (and (>= p-aux 0) (< p-aux p))
	    (list (match-beginning 0) q))))))

(defun hs-grok-mode-type ()
  "Setup variables for new buffers where applicable."
  (if (and (boundp 'comment-start)
	   (boundp 'comment-end))
      (progn
	(setq hs-c-start-regexp (regexp-quote comment-start))
	(if (string-match " +$" hs-c-start-regexp)
	    (setq hs-c-start-regexp
		  (substring hs-c-start-regexp 0 (1- (match-end 0)))))
	(setq hs-c-end-regexp (if (string= "" comment-end) "\n"
				(regexp-quote comment-end)))
	(if (string-match "^ +" hs-c-end-regexp)
	    (setq hs-c-end-regexp
		  (substring hs-c-end-regexp (match-end 0))))
	(let ((lookup (assoc major-mode hs-special-modes-alist)))
	  (setq hs-block-start-regexp (or (nth 1 lookup) "\\s\(")
		hs-block-end-regexp (or (nth 2 lookup) "\\s\)")
		hs-forward-sexp-func (or (nth 3 lookup) 'forward-sexp))))))

(defun hs-find-block-beginning ()
  "Repositions point at block-start.  
Return point, or nil if top-level." 
  (let (done
	(here (point))
	(both-regexps (concat "\\(" hs-block-start-regexp "\\)\\|\\("
			      hs-block-end-regexp "\\)")))
    (while (and (not done)
		(re-search-backward both-regexps (point-min) t))
      (if (match-beginning 1)		; start of start-regexp
	  (setq done (match-beginning 1))
	(goto-char (match-end 2))	; end of end-regexp
	(funcall hs-forward-sexp-func -1)))
    (goto-char (or done here))
    done))

(defmacro hs-life-goes-on (&rest body)
  "Executes optional BODY iff variable `hs-minor-mode' is non-nil."
  (list 'if 'hs-minor-mode (cons 'progn body)))

(defun hs-already-hidden-p ()
  "Return non-nil if point is in an already-hidden block otherwise nil."
  (save-excursion
    (end-of-line)
    (let ((overlays (overlays-at (point)))
	  (found nil))
      (while (and (not found) (overlayp (car overlays)))
	   (setq found (overlay-get (car overlays) 'hs)
		 overlays (cdr overlays)))
      found)))

(defun java-hs-forward-sexp (arg)
  "Function used by `hs-minor-mode' for `forward-sexp' in Java mode."
  (if (< arg 0)
      (backward-sexp 1)
    (if (looking-at hs-block-start-regexp)
	(progn
	  (goto-char (match-end 0))
	  (forward-char -1)
	  (forward-sexp 1))
      (forward-sexp 1))))

;;;----------------------------------------------------------------------------
;;; commands

;;;###autoload
(defun hs-hide-all ()
  "Hides all top-level blocks, displaying only first and last lines.
It moves point to the beginning of the line, and it runs the normal hook
`hs-hide-hook'.  See documentation for `run-hooks'. 
If `hs-hide-comments-when-hiding-all' is t also hides the comments."
  (interactive)
  (hs-life-goes-on
   (message "Hiding all blocks ...")
   (save-excursion
     (hs-flag-region (point-min) (point-max) nil) ; eliminate weirdness
     (goto-char (point-min))
     (if hs-hide-comments-when-hiding-all 
	 (let ((count 0)
	       (block-and-comment-re  ;; this should match 
		(concat "\\(^"        ;; the block beginning and comment start
			hs-block-start-regexp  
			"\\)\\|\\(" hs-c-start-regexp "\\)")))
	   (while (re-search-forward block-and-comment-re (point-max) t)
	     (if (match-beginning 1) ;; we have found a block beginning
		 (progn
		   (goto-char (match-beginning 1))
		   (hs-hide-block-at-point t)
		   (message "Hiding ... %d" (setq count (1+ count))))
	       ;;found a comment
	       (setq c-reg (hs-inside-comment-p))
	       (if c-reg 
		   (progn 
		     (goto-char (nth 1 c-reg))
		     (if (> (count-lines (car c-reg) (nth 1 c-reg)) 2)
			 (progn
			   (hs-hide-block-at-point t t c-reg)
			   (message "Hiding ... %d" 
				    (setq count (1+ count))))))))))
       (let ((count 0)
	     (top-level-re (concat "^" hs-block-start-regexp)))
	 (while 
	     (progn
	       (forward-comment (buffer-size))
	       (re-search-forward top-level-re (point-max) t))
	   (goto-char (match-beginning 0))
	   (hs-hide-block-at-point t)
	   (message "Hiding ... %d" (setq count (1+ count))))))
   (hs-safety-is-job-n))
   (beginning-of-line)
   (message "Hiding all blocks ... done")
   (run-hooks 'hs-hide-hook)))

(defun hs-show-all ()
  "Shows all top-level blocks.
This does not change point; it runs the normal hook `hs-show-hook'.
See documentation for `run-hooks'."
  (interactive)
  (hs-life-goes-on
   (message "Showing all blocks ...")
   (hs-flag-region (point-min) (point-max) nil)
   (message "Showing all blocks ... done")
   (run-hooks 'hs-show-hook)))

(defun hs-hide-block (&optional end)
  "Selects a block and hides it.  
With prefix arg, reposition at end.  Block is defined as a sexp for
lispish modes, mode-specific otherwise.  Comments are blocks, too.  
Upon completion, point is at repositioned and the normal hook 
`hs-hide-hook' is run.  See documentation for `run-hooks'."
  (interactive "P")
  (hs-life-goes-on
   (let ((c-reg (hs-inside-comment-p)))
     (if c-reg
	 (cond 
	  ((<= (count-lines (car c-reg) (nth 1 c-reg)) 2)
	   (message "Not enough comment lines to hide!"))
	  (t
	   (goto-char (nth 1 c-reg))
	   (hs-hide-block-at-point end t c-reg)
	   (hs-safety-is-job-n)
	   (run-hooks 'hs-hide-hook)))
       (if (or (looking-at hs-block-start-regexp)
	       (hs-find-block-beginning))
	   (progn
	     (hs-hide-block-at-point end)
	     (hs-safety-is-job-n)
	     (run-hooks 'hs-hide-hook)))))))

(defun hs-show-block (&optional end)
  "Selects a block and shows it.
With prefix arg, reposition at end.  Upon completion, point is 
repositioned and the normal hook `hs-show-hook' is run.  
See documentation for `hs-hide-block' and `run-hooks'."
  (interactive "P")
  (hs-life-goes-on
   (let ((c-reg (hs-inside-comment-p)))
     (if c-reg
	 (progn
	   (hs-flag-region (car c-reg) (nth 1 c-reg) nil)
	   (hs-safety-is-job-n)
	   (goto-char (if end (nth 1 c-reg) (car c-reg))))
       (if (or (looking-at hs-block-start-regexp)
	       (hs-find-block-beginning))
	   (progn
	     (hs-show-block-at-point end)
	     (hs-safety-is-job-n)
	     (run-hooks 'hs-show-hook)))))))

(defun hs-show-region (beg end)
  "Shows all lines from BEG to END, without doing any block analysis.
Note:`hs-show-region' is intended for use when `hs-show-block' signals
`unbalanced parentheses' and so is an emergency measure only.  You may
become very confused if you use this command indiscriminately."
  (interactive "r")
  (hs-life-goes-on
   (hs-flag-region beg end nil)
   (hs-safety-is-job-n)
   (run-hooks 'hs-show-hook)))

;;;###autoload
(defun hs-mouse-toggle-hiding (e)
  "Toggles hiding/showing of a block. 
Should be bound to a mouse key."
  (interactive "@e")
  (mouse-set-point e)
  (if (hs-already-hidden-p)
      (hs-show-block)
    (hs-hide-block)))

;;;###autoload
(defun hs-minor-mode (&optional arg)
  "Toggle hideshow minor mode.
With ARG, turn hideshow minor mode on if ARG is positive, off otherwise.
When hideshow minor mode is on, the menu bar is augmented with hideshow
commands and the hideshow commands are enabled.  The variables
`selective-display' and `selective-display-ellipses' are set to t.
Last, the normal hook `hs-minor-mode-hook' is run; see the doc 
for `run-hooks'.

Turning hideshow minor mode off reverts the menu bar and the
variables to default values and disables the hideshow commands."
  (interactive "P")
  (setq hs-minor-mode
        (if (null arg)
	    (not hs-minor-mode)
          (> (prefix-numeric-value arg) 0)))
  (if hs-minor-mode
      (progn
; 	(if (eq hs-emacs-type 'lucid)
;	    (progn
;	      (set-buffer-menubar (copy-sequence current-menubar))
;	      (add-menu nil (car hs-menu-bar) (cdr hs-menu-bar))))
	(make-variable-buffer-local 'line-move-ignore-invisible)
	(setq  line-move-ignore-invisible t)
	(add-to-invisibility-spec '(hs . t)) ;;hs invisible
	(hs-grok-mode-type)
	(run-hooks 'hs-minor-mode-hook))
;    (if (eq hs-emacs-type 'lucid)
;	(set-buffer-menubar (delete hs-menu-bar current-menubar)))
    (remove-from-invisibility-spec '(hs . t))))


;;;----------------------------------------------------------------------------
;;; load-time setup routines

;; which emacs being used?
;(setq hs-emacs-type
;      (if (string-match "xemacs\\|lucid" emacs-version)
;	  'lucid
;	'fsf))

;; keymaps and menus
(if hs-minor-mode-map
    nil
  (setq hs-minor-mode-map (make-sparse-keymap))
  ;; I beleive there is nothing bound on this keys
  (define-key hs-minor-mode-map "\C-ch" 'hs-hide-block)
  (define-key hs-minor-mode-map "\C-cs" 'hs-show-block)
  (define-key hs-minor-mode-map "\C-cH" 'hs-hide-all)
  (define-key hs-minor-mode-map "\C-cS" 'hs-show-all)
  (define-key hs-minor-mode-map "\C-cR" 'hs-show-region)
  
  (define-key hs-minor-mode-map [S-mouse-2] 'hs-mouse-toggle-hiding)

  ;; should we use easymenu here?
  (define-key hs-minor-mode-map [menu-bar Hide/Show]
    (cons "Hide/Show" (make-sparse-keymap "Hide/Show")))
  (define-key hs-minor-mode-map [menu-bar Hide/Show  hs-show-region]
    '("Show Region" . hs-show-region))
  (define-key hs-minor-mode-map [menu-bar Hide/Show hs-show-all]
    '("Show All" . hs-show-all))
  (define-key hs-minor-mode-map [menu-bar Hide/Show hs-hide-all]
    '("Hide All" . hs-hide-all))
  (define-key hs-minor-mode-map [menu-bar Hide/Show hs-show-block]
    '("Show Block" . hs-show-block))
  (define-key hs-minor-mode-map [menu-bar Hide/Show hs-hide-block]
    '("Hide Block" . hs-hide-block))
  )

;; some housekeeping
(or (assq 'hs-minor-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
          (cons (cons 'hs-minor-mode hs-minor-mode-map)
                minor-mode-map-alist)))
(or (assq 'hs-minor-mode minor-mode-alist)
    (setq minor-mode-alist (append minor-mode-alist
                                   (list '(hs-minor-mode " hs")))))

;; make some variables buffer-local
(make-variable-buffer-local 'hs-minor-mode)
(make-variable-buffer-local 'hs-c-start-regexp)
(make-variable-buffer-local 'hs-c-end-regexp)
(make-variable-buffer-local 'hs-block-start-regexp)
(make-variable-buffer-local 'hs-block-end-regexp)
(make-variable-buffer-local 'hs-forward-sexp-func)
(put 'hs-minor-mode 'permanent-local t)
(put 'hs-c-start-regexp 'permanent-local t)
(put 'hs-c-end-regexp 'permanent-local t)
(put 'hs-block-start-regexp 'permanent-local t)
(put 'hs-block-end-regexp 'permanent-local t)
(put 'hs-forward-sexp-func 'permanent-local t)


;;;----------------------------------------------------------------------------
;;; that's it

(provide 'hideshow)

;;; hideshow.el ends here
