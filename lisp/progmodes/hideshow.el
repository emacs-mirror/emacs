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

;;; this file provides `hs-minor-mode'.  when active, six commands:
;;;   hs-{hide,show}-{all,block}, hs-show-region and hs-minor-mode
;;; are available.  they implement block hiding and showing.  blocks are
;;; defined in mode-specific way.  in c-mode or c++-mode, they are simply
;;; curly braces, while in lisp-ish modes they are parens.  multi-line
;;; comments (c-mode) can also be hidden.  the command M-x hs-minor-mode
;;; toggles the minor mode or sets it (similar to outline minor mode).
;;; see documentation for each command for more info.
;;;
;;; the variable `hs-unbalance-handler-method' controls hideshow's behavior
;;; in the case of "unbalanced parentheses".  see doc for more info.

;;; suggested usage:

;;; (load-library "hideshow")
;;; (defun my-hs-setup () "enables hideshow and binds some commands"
;;;   (hs-minor-mode 1)
;;;   (define-key hs-minor-mode-map "\C-ch" 'hs-hide-block)
;;;   (define-key hs-minor-mode-map "\C-cs" 'hs-show-block)
;;;   (define-key hs-minor-mode-map "\C-cr" 'hs-show-region))
;;; (add-hook 'X-mode-hook 'my-hs-setup t)   ; other modes similarly
;;;
;;; where X = {emacs-lisp,c,c++,perl,...}.  see the doc for the variable
;;; `hs-special-modes-alist' if you'd like to use hideshow w/ other modes.

;;; etc:

;;; bug reports and fixes welcome (comments, too).  thanks go to
;;;	Dean Andrews <adahome@ix.netcom.com>
;;;	Preston F. Crow <preston.f.crow@dartmouth.edu>
;;;	Gael Marziou <gael@gnlab030.grenoble.hp.com>
;;;	Keith Sheffield <sheff@edcsgw2.cr.usgs.gov>
;;;	Jan Djarv <jan.djarv@sa.erisoft.se>
;;;	Lars Lindberg <qhslali@aom.ericsson.se>
;;;	Alf-Ivar Holm <alfh@ifi.uio.no>
;;; for valuable feedback and bug reports.

;;; Code:


;;;----------------------------------------------------------------------------
;;; dependencies

; (require 'emacs-vers)			; support different emacs flavors
(require 'cl)				; common lisp package


;;;----------------------------------------------------------------------------
;;; user-configurable variables

(defvar hs-unbalance-handler-method 'top-level
  "*symbol representing how \"unbalanced parentheses\" should be handled.
this error is usually signalled by hs-show-block.  one of four values:
`top-level', `next-line', `signal' or `ignore'.  default is `top-level'.

- `top-level' -- show top-level block containing the currently troublesome
block.
- `next-line' -- use the fact that, for an already hidden block, its end
will be on the next line.  attempt to show this block.
- `signal' -- pass the error through, stopping execution.
- `ignore' -- ignore the error, continuing execution.

values other than these four will be interpreted as `signal'.")

(defvar hs-special-modes-alist '((c-mode "{" "}")
				 (c++-mode "{" "}"))
  "*alist of the form (MODE START-RE END-RE FORWARD-SEXP-FUNC).
if present, hideshow will use these values for the start and end regexps,
respectively.  since algol-ish languages do not have single-character
block delimiters, the function `forward-sexp' which is used by hideshow
doesn't work.  in this case, if a similar function is provided, you can
register it and have hideshow use it instead of `forward-sexp'.  to add
more values, use

\t(pushnew '(new-mode st-re end-re function-name)
\t	hs-special-modes-alist :test 'equal)

for example:

\t(pushnew '(simula-mode \"begin\" \"end\" simula-next-statement)
\t	hs-special-modes-alist :test 'equal)

note that the regexps should not contain leading or trailing whitespace.")

(defvar hs-hide-hooks nil
  "*hooks called at the end of hs-hide-all and hs-hide-block.")

(defvar hs-show-hooks nil
  "*hooks called at the end of hs-show-all, hs-show-block and hs-show-region.")

(defvar hs-minor-mode-prefix "\C-c"
  "*prefix key to use for hideshow commands in hideshow minor mode.")


;;;----------------------------------------------------------------------------
;;; internal variables

(defvar hs-minor-mode nil
  "non-nil if using hideshow mode as a minor mode of some other mode.
use the command `hs-minor-mode' to toggle this variable.")

(defvar hs-minor-mode-map nil
  "mode map for hideshow minor mode.")

(defvar hs-menu-bar nil
  "menu bar for hideshow minor mode (xemacs only).")

(defvar hs-c-start-regexp nil
  "regexp for beginning of comments.  buffer-local.
differs from mode-specific comment regexps in that surrounding
whitespace is stripped.")

(defvar hs-c-end-regexp nil
  "regexp for end of comments.  buffer-local.
see `hs-c-start-regexp'.")

(defvar hs-block-start-regexp nil
  "regexp for beginning of block.  buffer-local.")

(defvar hs-block-end-regexp nil
  "regexp for end of block.  buffer-local.")

(defvar hs-forward-sexp-func 'forward-sexp
  "function used to do a forward-sexp.  should change for algol-ish modes.
for single-character block delimiters -- ie, the syntax table regexp for the
character is either ( or ) -- `hs-forward-sexp-func' would just be
`forward-sexp'.  for other modes such as simula, a more specialized function
is necessary.")

; (eval-when-compile			; lint free!
;   (unless (emacs-type-eq 'lucid)
;     (defvar current-menubar nil "")
;     (defun set-buffer-menubar (arg1))
;     (defun add-menu (arg1 arg2 arg3))))


;;;----------------------------------------------------------------------------
;;; support funcs

;; snarfed from outline.el, but added buffer-read-only
(defun hs-flag-region (from to flag)
  "hides or shows lines from FROM to TO, according to FLAG.
if FLAG is \\n (newline character) then text is shown, while if FLAG
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
  "hide block iff on block beginning, optional END means reposition at end." 
  (when (looking-at hs-block-start-regexp)
    (let* ((p (point))
	   (q (progn (funcall hs-forward-sexp-func 1) (point))))
      (forward-line -1) (end-of-line)
      (when (and (< p (point)) (> (count-lines p q) 1))
	(hs-flag-region p (point) ?\C-m))
      (goto-char (if end q p)))))

(defun hs-show-block-at-point (&optional end)
  "show block iff on block beginning.  optional END means reposition at end."
  (when (looking-at hs-block-start-regexp)
    (let* ((p (point))
	   (q
	    (condition-case error	; probably unbalanced paren
		(progn
		  (funcall hs-forward-sexp-func 1)
		  (point))
	      (error
	       (case hs-unbalance-handler-method
		 ('ignore
		  ;; just ignore this block
		  (point))
		 ('top-level
		  ;; try to get out of rat's nest and expose the whole func
		  (unless (= (current-column) 0) (beginning-of-defun))
		  (setq p (point))
		  (re-search-forward (concat "^" hs-block-start-regexp)
				     (point-max) t 2)
		  (point))
		 ('next-line
		  ;; assumption is that user knows what s/he's doing
		  (beginning-of-line) (setq p (point))
		  (end-of-line 2) (point))
		 (t
		  ;; pass error through -- this applies to `signal', too
		  (signal (car error) (cdr error))))))))
      (hs-flag-region p q ?\n)
      (goto-char (if end (1+ (point)) p)))))

(defun hs-safety-is-job-n ()
  "warns if selective-display or selective-display-ellipses is nil."
  (let ((str ""))
    (unless selective-display
      (setq str "selective-display nil "))
    (unless selective-display-ellipses
      (setq str (concat str "selective-display-ellipses nil")))
    (when (/= (length str) 0)
      (message "warning: %s" str)
      (sit-for 2))))

(defun hs-inside-comment-p ()
  "returns non-nil if point is inside a comment, otherwise nil.
actually, for multi-line-able comments, returns a list containing
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
	(when (< (- (point) (length comment-start)) p)
	  (list (match-beginning 0) q)))))) 

(defun hs-grok-mode-type ()
  "setup variables for new buffers where applicable."
  (when (and (boundp 'comment-start)
	     (boundp 'comment-end))
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
      (setq hs-block-start-regexp (or (cadr lookup) "\\s\(")
	    hs-block-end-regexp (or (caddr lookup) "\\s\)")
	    hs-forward-sexp-func (or (cadddr lookup) 'forward-sexp)))))

(defun hs-find-block-beginning ()
  "repositions point at block-start.  return point, or nil if top-level." 
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
  "executes optional BODY iff variable hs-minor-mode is non-nil."
  (list 'if 'hs-minor-mode (cons 'progn body)))


;;;----------------------------------------------------------------------------
;;; commands

;;;###autoload
(defun hs-hide-all ()
  "hides all top-level blocks, displaying only first and last lines.
when done, point is repositioned at the beginning of the line, and
hs-hide-hooks is called.  see documentation for `run-hooks'."
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
	 (message "hiding ... %d" (incf count))))
     (hs-safety-is-job-n))
   (beginning-of-line)
   (message "hiding all blocks ... done")
   (run-hooks 'hs-hide-hooks)))

(defun hs-show-all ()
  "shows all top-level blocks.
when done, point is unchanged, and hs-show-hooks is called.  see
documentation for `run-hooks'."
  (interactive)
  (hs-life-goes-on
   (message "showing all blocks ...")
   (hs-flag-region (point-min) (point-max) ?\n)
   (message "showing all blocks ... done")
   (run-hooks 'hs-show-hooks)))

;;;###autoload
(defun hs-hide-block (&optional end)
  "selects a block and hides it.  with prefix arg, reposition at end.
block is defined as a sexp for lispish modes, mode-specific otherwise.
comments are blocks, too.  upon completion, point is at repositioned and
hs-hide-hooks is called.  see documentation for `run-hooks'."
  (interactive "P")
  (hs-life-goes-on
   (let ((c-reg (hs-inside-comment-p)))
     (if c-reg
	 (cond ((string= comment-end "")
		(message "can't hide a single-line comment"))
	       ((< (count-lines (car c-reg) (cadr c-reg)) 2)
		(message "not enougn comment lines to hide"))
	       (t
		(goto-char (cadr c-reg))
		(forward-line -1)
		(hs-flag-region (car c-reg) (point) ?\C-m)
		(goto-char (if end (cadr c-reg) (car c-reg)))
		(hs-safety-is-job-n)
		(run-hooks 'hs-hide-hooks)))
       (when (or (looking-at hs-block-start-regexp)
		 (hs-find-block-beginning))
	 (hs-hide-block-at-point end)
	 (hs-safety-is-job-n)
	 (run-hooks 'hs-hide-hooks))))))

(defun hs-show-block (&optional end)
  "selects a block and shows it.  with prefix arg, reposition at end.
upon completion, point is repositioned hs-show-hooks are called.  see
documetation for `hs-hide-block' and `run-hooks'."
  (interactive "P")
  (hs-life-goes-on
   (let ((c-reg (hs-inside-comment-p)))
     (if c-reg
	 (cond ((string= comment-end "")
		(message "already looking at the entire comment"))
	       (t
		(hs-flag-region (car c-reg) (cadr c-reg) ?\n)
		(goto-char (if end (cadr c-reg) (car c-reg)))))
       (when (or (looking-at hs-block-start-regexp)
		 (hs-find-block-beginning))
	 (hs-show-block-at-point end)
	 (hs-safety-is-job-n)
	 (run-hooks 'hs-show-hooks))))))

(defun hs-show-region (beg end)
  "shows all lines from BEG to END, without doing any block analysis.
note: hs-show-region is intended for use when when hs-show-block signals
`unbalanced parentheses' and so is an emergency measure only.  you may
become very confused if you use this command indiscriminately."
  (interactive "r")
  (hs-life-goes-on
   (hs-flag-region beg end ?\n)
   (hs-safety-is-job-n)
   (run-hooks 'hs-show-hooks)))

;;;###autoload
(defun hs-minor-mode (&optional arg)
  "toggle hideshow minor mode.
with ARG, turn hideshow minor mode on if ARG is positive, off otherwise.
when hideshow minor mode is on, the menu bar is augmented with hideshow
commands and the hideshow commands are enabled.  the variables\n
\tselective-display\n\tselective-display-ellipses\n
are set to t.  lastly, the hooks set in hs-minor-mode-hook are called.
see documentation for `run-hooks'.\n
turning hideshow minor mode off reverts the menu bar and the
variables to default values and disables the hideshow commands."
  (interactive "P")
  (setq hs-minor-mode
        (if (null arg)
	    (not hs-minor-mode)
          (> (prefix-numeric-value arg) 0)))
  (if hs-minor-mode
      (progn
; 	(when (emacs-type-eq 'lucid)
;	  (set-buffer-menubar (copy-sequence current-menubar))
;	  (add-menu nil (car hs-menu-bar) (cdr hs-menu-bar)))
	(setq selective-display t
	      selective-display-ellipses t)
	(hs-grok-mode-type)
	(run-hooks 'hs-minor-mode-hook))
;    (when (emacs-type-eq 'lucid)
;      (set-buffer-menubar (delete hs-menu-bar current-menubar)))
    (kill-local-variable 'selective-display)
    (kill-local-variable 'selective-display-ellipses)))


;;;----------------------------------------------------------------------------
;;; load-time setup routines

;; keymaps and menus
(unless hs-minor-mode-map
  (setq hs-minor-mode-map (make-sparse-keymap))
  (cond
;	 ((emacs-type-eq 'lucid)
;	 (setq hs-menu-bar		; build top down for lucid
;	       '("hideshow"
;		 ["hide block" hs-hide-block t]
;		 ["show block" hs-show-block t]
;		 ["hide all" hs-hide-all t]
;		 ["show all" hs-show-all t]
;		 ["show region" hs-show-region t])))
	(t				; build bottom up for others
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
(pushnew (cons 'hs-minor-mode hs-minor-mode-map)
	 minor-mode-map-alist
	 :test 'equal)
(pushnew '(hs-minor-mode " hs") minor-mode-alist :test 'equal)

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
