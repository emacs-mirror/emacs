;;; hideshow.el --- minor mode cmds to selectively display blocks of code

;;; Copyright (C) 1994,1995 Free Software Foundation

;;; Author: Thien-Thi Nguyen <ttn@netcom.com>
;;; Version: 3.4
;;; Keywords: C C++ lisp tools editing
;;; Time-of-Day-Author-Most-Likely-to-be-Recalcitrant: early morning

;;; This file is part of GNU Emacs.

;;; GNU Emacs is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by the
;;; Free Software Foundation; either version 2 of the License, or (at your
;;; option) any later version.
;;; 
;;; GNU Emacs is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;;; for more details.
;;; 
;;; You should have received a copy of the GNU General Public License along
;;; with this program; if not, write to the Free Software Foundation, Inc.,
;;; 675 Mass Ave, Cambridge, MA 02139, USA.

;;; LCD Archive Entry:
;;; hideshow|Thien-Thi Nguyen|ttn@netcom.com|
;;; minor mode commands to selectively display blocks of code|
;;; 18-Oct-1994|3.4|~/modes/hideshow.el.Z|

;;; Commentary:

;;; This file provides `hs-minor-mode'.  When active, six commands:
;;;   hs-{hide,show}-{all,block}, hs-show-region and hs-minor-mode
;;; are available.  They implement block hiding and showing.  Blocks are
;;; defined in mode-specific way.  In c-mode or c++-mode, they are simply
;;; curly braces, while in lisp-ish modes they are parens.  Multi-line
;;; comments (c-mode) can also be hidden.  The command M-x hs-minor-mode
;;; toggles the minor mode or sets it (similar to outline minor mode).
;;; See documentation for each command for more info.
;;;
;;; The variable `hs-unbalance-handler-method' controls hideshow's behavior
;;; in the case of "unbalanced parentheses".  See doc for more info.

;;; Suggested usage:

;;; (load-library "hideshow")
;;; (defun my-hs-setup () "enables hideshow and binds some commands"
;;;   (hs-minor-mode 1)
;;;   (define-key hs-minor-mode-map "\C-ch" 'hs-hide-block)
;;;   (define-key hs-minor-mode-map "\C-cs" 'hs-show-block)
;;;   (define-key hs-minro-mode-map "\C-cH" 'hs-hide-all)
;;;   (define-key hs-minro-mode-map "\C-cS" 'hs-show-all)
;;;   (define-key hs-minor-mode-map "\C-cR" 'hs-show-region))
;;; (add-hook 'X-mode-hook 'my-hs-setup t)   ; other modes similarly
;;;
;;; where X = {emacs-lisp,c,c++,perl,...}.  See the doc for the variable
;;; `hs-special-modes-alist' if you'd like to use hideshow w/ other modes.

;;; Etc:

;;; Bug reports and fixes welcome (comments, too).  Thanks go to
;;;	Dean Andrews <adahome@ix.netcom.com>
;;;	Preston F. Crow <preston.f.crow@dartmouth.edu>
;;;	Gael Marziou <gael@gnlab030.grenoble.hp.com>
;;;	Keith Sheffield <sheff@edcsgw2.cr.usgs.gov>
;;;	Jan Djarv <jan.djarv@sa.erisoft.se>
;;;	Lars Lindberg <qhslali@aom.ericsson.se>
;;;	Alf-Ivar Holm <alfh@ifi.uio.no>
;;; for valuable feedback, code and bug reports.

;;; Code:


;;;----------------------------------------------------------------------------
;;; user-configurable variables

(defvar hs-unbalance-handler-method 'top-level
  "*Symbol representing how \"unbalanced parentheses\" should be handled.
This error is usually signalled by hs-show-block.  One of four values:
`top-level', `next-line', `signal' or `ignore'.  Default is `top-level'.

- `top-level' -- Show top-level block containing the currently troublesome
block.
- `next-line' -- Use the fact that, for an already hidden block, its end
will be on the next line.  Attempt to show this block.
- `signal' -- Pass the error through, stopping execution.
- `ignore' -- Ignore the error, continuing execution.

Values other than these four will be interpreted as `signal'.")

(defvar hs-special-modes-alist '((c-mode "{" "}")
				 (c++-mode "{" "}"))
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

(defvar hs-hide-hooks nil
  "*Hooks called at the end of hs-hide-all and hs-hide-block.")

(defvar hs-show-hooks nil
  "*Hooks called at the end of hs-show-all, hs-show-block and hs-show-region.")

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
  "Regexp for beginning of comments.  Buffer-local.
Differs from mode-specific comment regexps in that surrounding
whitespace is stripped.")

(defvar hs-c-end-regexp nil
  "Regexp for end of comments.  Buffer-local.
See `hs-c-start-regexp'.")

(defvar hs-block-start-regexp nil
  "Regexp for beginning of block.  Buffer-local.")

(defvar hs-block-end-regexp nil
  "Regexp for end of block.  Buffer-local.")

(defvar hs-forward-sexp-func 'forward-sexp
  "Function used to do a forward-sexp.  Should change for Algol-ish modes.
For single-character block delimiters -- ie, the syntax table regexp for the
character is either `(' or `)' -- `hs-forward-sexp-func' would just be
`forward-sexp'.  For other modes such as simula, a more specialized function
is necessary.")

(defvar hs-emacs-type 'fsf
  "Used to support both FSF Emacs and Xemacs.")

(eval-when-compile
  (if (string-match "xemacs\\|lucid" emacs-version)
      (progn
	(defvar current-menubar nil "")
	(defun set-buffer-menubar (arg1))
	(defun add-menu (arg1 arg2 arg3)))))


;;;----------------------------------------------------------------------------
;;; support funcs

;; snarfed from outline.el, but added buffer-read-only
(defun hs-flag-region (from to flag)
  "Hides or shows lines from FROM to TO, according to FLAG.
If FLAG is \\n (newline character) then text is shown, while if FLAG
is \\^M \(control-M) the text is hidden."
  (let ((modp (buffer-modified-p))
	buffer-read-only)		; nothing is immune
    (unwind-protect (progn
		      (subst-char-in-region
		       from to
		       (if (= flag ?\n) ?\C-m ?\n)
		       flag t))
      (set-buffer-modified-p modp))))

(defun hs-hide-block-at-point (&optional end)
  "Hide block iff on block beginning, optional END means reposition at end." 
  (if (looking-at hs-block-start-regexp)
      (let* ((p (point))
	     (q (progn (funcall hs-forward-sexp-func 1) (point))))
	(forward-line -1) (end-of-line)
	(if (and (< p (point)) (> (count-lines p q) 1))
	    (hs-flag-region p (point) ?\C-m))
	(goto-char (if end q p)))))

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
	(hs-flag-region p q ?\n)
	(goto-char (if end (1+ (point)) p)))))

(defun hs-safety-is-job-n ()
  "Warns if selective-display or selective-display-ellipses is nil."
  (let ((str ""))
    (or selective-display
	(setq str "selective-display nil "))
    (or selective-display-ellipses
	(setq str (concat str "selective-display-ellipses nil")))
    (if (= (length str) 0)
	nil
      (message "warning: %s" str)
      (sit-for 2))))

(defun hs-inside-comment-p ()
  "Returns non-nil if point is inside a comment, otherwise nil.
Actually, for multi-line-able comments, returns a list containing
the buffer position of the start and the end of the comment."
  ;; is it single-line-only or multi-line-able?
  (save-excursion
    (let ((p (point))
	  q)
      (if (string= comment-end "")	; single line
	  (let (found)
	    (beginning-of-line)
	    (setq found (re-search-forward hs-c-start-regexp p t))
	    (and found (not (search-forward "\"" p t))))
	(re-search-forward hs-c-end-regexp (point-max) 1)
	(setq q (point))
	(forward-comment -1)
	(re-search-forward hs-c-start-regexp (point-max) 1)
	(if (< (- (point) (length comment-start)) p)
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
  "Repositions point at block-start.  Return point, or nil if top-level." 
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


;;;----------------------------------------------------------------------------
;;; commands

;;;###autoload
(defun hs-hide-all ()
  "Hides all top-level blocks, displaying only first and last lines.
When done, point is repositioned at the beginning of the line, and
hs-hide-hooks is called.  See documentation for `run-hooks'."
  (interactive)
  (hs-life-goes-on
   (message "hiding all blocks ...")
   (save-excursion
     (hs-flag-region (point-min) (point-max) ?\n) ; eliminate weirdness
     (goto-char (point-min))
     (let ((count 0)
	   (top-level-re (concat "^" hs-block-start-regexp)))
       (while (progn
		(forward-comment (buffer-size))
		(re-search-forward top-level-re (point-max) t))
	 (goto-char (match-beginning 0))
	 (hs-hide-block-at-point t)
	 (message "hiding ... %d" (setq count (1+ count)))))
     (hs-safety-is-job-n))
   (beginning-of-line)
   (message "hiding all blocks ... done")
   (run-hooks 'hs-hide-hooks)))

(defun hs-show-all ()
  "Shows all top-level blocks.
When done, point is unchanged, and hs-show-hooks is called.  See
documentation for `run-hooks'."
  (interactive)
  (hs-life-goes-on
   (message "showing all blocks ...")
   (hs-flag-region (point-min) (point-max) ?\n)
   (message "showing all blocks ... done")
   (run-hooks 'hs-show-hooks)))

;;;###autoload
(defun hs-hide-block (&optional end)
  "Selects a block and hides it.  With prefix arg, reposition at end.
Block is defined as a sexp for lispish modes, mode-specific otherwise.
Comments are blocks, too.  Upon completion, point is at repositioned and
hs-hide-hooks is called.  See documentation for `run-hooks'."
  (interactive "P")
  (hs-life-goes-on
   (let ((c-reg (hs-inside-comment-p)))
     (if c-reg
	 (cond ((string= comment-end "")
		(message "can't hide a single-line comment"))
	       ((< (count-lines (car c-reg) (nth 1 c-reg)) 2)
		(message "not enougn comment lines to hide"))
	       (t
		(goto-char (nth 1 c-reg))
		(forward-line -1)
		(hs-flag-region (car c-reg) (point) ?\C-m)
		(goto-char (if end (nth 1 c-reg) (car c-reg)))
		(hs-safety-is-job-n)
		(run-hooks 'hs-hide-hooks)))
       (if (or (looking-at hs-block-start-regexp)
	       (hs-find-block-beginning))
	   (progn
	     (hs-hide-block-at-point end)
	     (hs-safety-is-job-n)
	     (run-hooks 'hs-hide-hooks)))))))

(defun hs-show-block (&optional end)
  "Selects a block and shows it.  With prefix arg, reposition at end.
Upon completion, point is repositioned hs-show-hooks are called.  See
documetation for `hs-hide-block' and `run-hooks'."
  (interactive "P")
  (hs-life-goes-on
   (let ((c-reg (hs-inside-comment-p)))
     (if c-reg
	 (cond ((string= comment-end "")
		(message "already looking at the entire comment"))
	       (t
		(hs-flag-region (car c-reg) (nth 1 c-reg) ?\n)
		(goto-char (if end (nth 1 c-reg) (car c-reg)))))
       (if (or (looking-at hs-block-start-regexp)
	       (hs-find-block-beginning))
	   (progn
	     (hs-show-block-at-point end)
	     (hs-safety-is-job-n)
	     (run-hooks 'hs-show-hooks)))))))

(defun hs-show-region (beg end)
  "Shows all lines from BEG to END, without doing any block analysis.
Note: hs-show-region is intended for use when when hs-show-block signals
`unbalanced parentheses' and so is an emergency measure only.  You may
become very confused if you use this command indiscriminately."
  (interactive "r")
  (hs-life-goes-on
   (hs-flag-region beg end ?\n)
   (hs-safety-is-job-n)
   (run-hooks 'hs-show-hooks)))

;;;###autoload
(defun hs-minor-mode (&optional arg)
  "Toggle hideshow minor mode.
With ARG, turn hideshow minor mode on if ARG is positive, off otherwise.
When hideshow minor mode is on, the menu bar is augmented with hideshow
commands and the hideshow commands are enabled.  The variables\n
\tselective-display\n\tselective-display-ellipses\n
are set to t.  Lastly, the hooks set in hs-minor-mode-hook are called.
See documentation for `run-hooks'.\n
Turning hideshow minor mode off reverts the menu bar and the
variables to default values and disables the hideshow commands."
  (interactive "P")
  (setq hs-minor-mode
        (if (null arg)
	    (not hs-minor-mode)
          (> (prefix-numeric-value arg) 0)))
  (if hs-minor-mode
      (progn
 	(if (eq hs-emacs-type 'lucid)
	    (progn
	      (set-buffer-menubar (copy-sequence current-menubar))
	      (add-menu nil (car hs-menu-bar) (cdr hs-menu-bar))))
	(setq selective-display t
	      selective-display-ellipses t)
	(hs-grok-mode-type)
	(run-hooks 'hs-minor-mode-hook))
    (if (eq hs-emacs-type 'lucid)
	(set-buffer-menubar (delete hs-menu-bar current-menubar)))
    (kill-local-variable 'selective-display)
    (kill-local-variable 'selective-display-ellipses)))


;;;----------------------------------------------------------------------------
;;; load-time setup routines

;; which emacs being used?
(setq hs-emacs-type
      (if (string-match "xemacs\\|lucid" emacs-version)
	  'lucid
	'fsf))

;; keymaps and menus
(if (not hs-minor-mode-map)
  (setq hs-minor-mode-map (make-sparse-keymap))
  (cond
   ((eq hs-emacs-type 'lucid)
    (setq hs-menu-bar			; build top down for lucid
	  '("hideshow"
	    ["hide block" hs-hide-block t]
	    ["show block" hs-show-block t]
	    ["hide all" hs-hide-all t]
	    ["show all" hs-show-all t]
	    ["show region" hs-show-region t])))
   (t					; build bottom up for others
    (define-key hs-minor-mode-map [menu-bar hideshow]
      (cons "hideshow" (make-sparse-keymap "hideshow")))
    (define-key hs-minor-mode-map [menu-bar hideshow hs-show-region]
      '("show region" . hs-show-region))
    (define-key hs-minor-mode-map [menu-bar hideshow hs-show-all]
      '("show all" . hs-show-all))
    (define-key hs-minor-mode-map [menu-bar hideshow hs-hide-all]
      '("hide all" . hs-hide-all))
    (define-key hs-minor-mode-map [menu-bar hideshow hs-show-block]
      '("show block" . hs-show-block))
    (define-key hs-minor-mode-map [menu-bar hideshow hs-hide-block]
      '("hide block" . hs-hide-block)))))

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
