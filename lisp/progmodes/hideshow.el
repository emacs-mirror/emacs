;;; hideshow.el --- minor mode cmds to selectively display blocks of code

;; Copyright (C) 1994, 1995, 1996, 1997 Free Software Foundation

;; Author: Thien-Thi Nguyen <ttn@netcom.com>
;; Maintainer: Dan Nicolaescu <done@ece.arizona.edu>
;; Version: 4.0
;; Keywords: C C++ java lisp tools editing comments blocks hiding outlines
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

(defgroup hideshow nil
  "Minor mode for hiding and showing program and comment blocks."
  :prefix "hs-"
  :group 'languages)

;;;###autoload
(defcustom hs-hide-comments-when-hiding-all t 
  "Hide the comments too when you do an `hs-hide-all'." 
  :type 'boolean
  :group 'hideshow)

;;;###autoload
(defcustom hs-show-hidden-short-form t
  "Leave only the first line visible in a hidden block.
If t only the first line is visible when a block is in the hidden state, 
else both the first line and the last line are showed. Also if t and 
`hs-adjust-block-beginning' is set, it is used also.

An example of how this works: (in c-mode)
original:
                      
/* My function main
   some more stuff about main
*/
int
main(void)
{
  int x=0;
  return 0;
}


hidden and hs-show-hidden-short-form is nil
/* My function main...
*/                        
int
main(void)
{...
}

hidden and hs-show-hidden-short-form is t
/* My function main...
int                   
main(void)...

For latest you have to be on the line containing the ellipsis when 
you do `hs-show-block'."
  :type 'boolean
  :group 'hideshow)

(defcustom hs-minor-mode-hook 'hs-hide-initial-comment-block
  "Hook called when `hs-minor-mode' is installed.
A good value for this would be `hs-hide-initial-comment-block' to
hide all the comments at the beginning of the file."
  :type 'hook
  :group 'hideshow)

(defcustom hs-isearch-open 'block
"What kind of hidden blocks to open when doing `isearch'.
It can have the following values:
    `block'     open only  blocks
    `comment'   open only comments
    t           open all of them
    nil         don't open any.
This only has effect iff `search-invisible' is set to `open'."
  :type '(choice (const :tag "open only  blocks" block)
	         (const :tag "open only comments" comment)
	         (const :tag "open both blocks and comments" t)
	         (const :tag "don't open any of them" nil))
  :group 'hideshow)

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

;;;###autoload
(defvar hs-special-modes-alist 
  '((c-mode "{" "}" nil nil hs-c-like-adjust-block-beginning)
    (c++-mode "{" "}" "/[*/]" nil hs-c-like-adjust-block-beginning)
    (java-mode   "\\(\\(\\([ \t]*\\(\\(abstract\\|final\\|p\\(r\\(ivate\\|otected\\)\\|ublic\\)\\|static\\)[ \t\n]+\\)+\\(synchronized[ \t\n]*\\)?[a-zA-Z0-9_:]+[ \t\n]*\\(\\[[ \t\n]*\\][ \t\n]*\\)?\\([a-zA-Z0-9_:]+[ \t\n]*\\)([^)]*)\\([ \n\t]+throws[ \t\n][^{]+\\)?\\)\\|\\([ \t]*static[^{]*\\)\\)[ \t\n]*{\\)" java-hs-forward-sexp hs-c-like-adjust-block-beginning))
; I tested the java regexp using the following:
;(defvar hsj-public)
;(defvar hsj-syncronised)
;(defvar hsj-type)
;(defvar hsj-fname)
;(defvar hsj-par)
;(defvar hsj-throws)
;(defvar hsj-static)

;(setq hsj-public 
;      (concat "[ \t]*\\("
;	      (regexp-opt '("public" "private" "protected" "abstract" 
;			    "static" "final") 1)
;	      "[ \t\n]+\\)+"))

;(setq hsj-syncronised "\\(synchronized[ \t\n]*\\)?")
;(setq hsj-type "[a-zA-Z0-9_:]+[ \t\n]*\\(\\[[ \t\n]*\\][ \t\n]*\\)?")
;(setq hsj-fname "\\([a-zA-Z0-9_:]+[ \t\n]*\\)")
;(setq hsj-par "([^)]*)")
;(setq hsj-throws "\\([ \n\t]+throws[ \t\n][^{]+\\)?")

;(setq hsj-static "[ \t]*static[^{]*")


;(setq hs-block-start-regexp (concat
;			     "\\("
;			         "\\("
;				      "\\("
;				         hsj-public
;				         hsj-syncronised
;					 hsj-type
;					 hsj-fname
;					 hsj-par
;					 hsj-throws
;				      "\\)"
;				      "\\|"
;				      "\\("
;				         hsj-static
;				      "\\)"
;				 "\\)"
;				    "[ \t\n]*{"
;			      "\\)"  
;				    ))

  "*Alist for initializing the hideshow variables for different modes.
It has the form 
(MODE START-RE END-RE COMMENT-START-RE FORWARD-SEXP-FUNC ADJUST-BEG-FUNC).
If present, hideshow will use these values for the start and end regexps,
respectively.  Since Algol-ish languages do not have single-character
block delimiters, the function `forward-sexp' which is used by hideshow
doesn't work.  In this case, if a similar function is provided, you can
register it and have hideshow use it instead of `forward-sexp'.  To add
more values, use

\t(pushnew '(new-mode st-re end-re function-name)
\t	hs-special-modes-alist :test 'equal)

For example:

\t(pushnew '(simula-mode \"begin\" \"end\" \"!\" simula-next-statement)
\t	hs-special-modes-alist :test 'equal)

See the documentation for `hs-adjust-block-beginning' to see what
is the use of ADJUST-BEG-FUNC.

If any of those is left nil, hideshow will try to guess some values, see
`hs-grok-mode-type' for this.

Note that the regexps should not contain leading or trailing whitespace.")

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

;(defvar hs-menu-bar nil
;  "Menu bar for hideshow minor mode (Xemacs only).")

(defvar hs-c-start-regexp nil
  "Regexp for beginning of comments.  
Differs from mode-specific comment regexps in that 
surrounding whitespace is stripped.")

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

(defvar hs-adjust-block-beginning nil
  "Function used to tweak the block beginning.
It has effect only if `hs-show-hidden-short-form' is t. The block it
is hidden from the point returned by this function, as opposed to
hiding it from the point returned when searching
`hs-block-start-regexp'.  In c-like modes, if we wish to also hide the
curly braces (if you think they occupy too much space on the screen),
this function should return the starting point (at the end of line) of
the hidden region.  

It is called with a single argument ARG which is the the position in
buffer after the block beginning.

It should return the position from where we should start hiding.

It should not move the point.  

See `hs-c-like-adjust-block-beginning' for an example of using this.")

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

;; snarfed from outline.el;
(defun hs-flag-region (from to flag)
  "Hides or shows lines from FROM to TO, according to FLAG.  If FLAG
is nil then text is shown, while if FLAG is non-nil the text is
hidden. Actualy flag is realy either `comment' or `block' depending on
what kind of block it is suppose to hide."
    (save-excursion
      (goto-char from)
      (end-of-line)
      (hs-discard-overlays (point) to 'invisible 'hs)
      (if flag
	  (let ((overlay (make-overlay (point) to)))
	    ;; Make overlay hidden and intangible.
	    (overlay-put overlay 'invisible 'hs)
	    (overlay-put overlay 'hs t)
	    (when (or (eq hs-isearch-open t) (eq hs-isearch-open flag)) 
		(overlay-put overlay 'isearch-open-invisible 
			     'hs-isearch-open-invisible))
	    (overlay-put overlay 'intangible t)))))

;; This is set as an `isearch-open-invisible' property to hidden
;; overlays.
(defun hs-isearch-open-invisible (ov)
  (save-excursion
    (goto-char (overlay-start ov))
    (hs-show-block)))

;; Remove from the region BEG ... END all overlays
;; with a PROP property equal to VALUE.
;; Overlays with a PROP property different from VALUE are not touched.
(defun hs-discard-overlays (beg end prop value)
  (if (< end beg)
      (setq beg (prog1 end (setq end beg))))
  (save-excursion
    (goto-char beg)
    (let ((overlays (overlays-in beg end))
	  o)
      (while overlays
	(setq o (car overlays))
	(if (eq (overlay-get o prop) value)
	    (delete-overlay o))
	  (setq overlays (cdr overlays))))))

(defun hs-hide-block-at-point (&optional end comment-reg)
  "Hide block iff on block beginning, optional END means reposition at end.
COMMENT-REG is a list of the form (BEGIN . END) and specifies the limits 
of the comment, or nil if the block is not a comment." 
  (if comment-reg
      (progn 
	;; goto the end of line at the end of the comment
	(goto-char (nth 1 comment-reg))
	(unless hs-show-hidden-short-form (forward-line -1))
	(end-of-line)
	(hs-flag-region (car comment-reg)  (point) 'comment) 
	(goto-char (if end (nth 1 comment-reg) (car comment-reg))))
      (if (looking-at hs-block-start-regexp)
	  (let* ((p ;; p is the point at the end of the block beginning
		  (if (and  hs-show-hidden-short-form 
			    hs-adjust-block-beginning)
		      ;; we need to adjust the block beginning
		      (funcall hs-adjust-block-beginning (match-end 0)) 
		    (match-end 0)))
		 ;; q is the point at the end of the block
		 (q (progn (funcall hs-forward-sexp-func 1) (point))))
	    ;; position the point so we can call `hs-flag-region'
	    (unless hs-show-hidden-short-form (forward-line -1))
	    (end-of-line)
	    (if (and (< p (point)) (> (count-lines p q) 
				      (if hs-show-hidden-short-form 1 2)))
		(hs-flag-region p (point) 'block))
	    (goto-char (if end q p))))))

(defun hs-show-block-at-point (&optional end comment-reg)
  "Show block iff on block beginning.  Optional END means reposition at end.
COMMENT-REG is a list of the forme (BEGIN . END) and specifies the limits 
of the comment. It should be nil when hiding a block." 
  (if comment-reg
      (when (car comment-reg)
	(hs-flag-region (car comment-reg) (nth 1 comment-reg) nil)
	(goto-char (if end (nth 1 comment-reg) (car comment-reg))))
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
	  (goto-char (if end (1+ (point)) p))))))

(defun hs-safety-is-job-n ()
  "Warn `buffer-invisibility-spec' does not contain hs."
    (if (or buffer-invisibility-spec (assq 'hs buffer-invisibility-spec) )
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
	(skip-chars-forward " \t\n^L")
	(setq c-reg (hs-inside-comment-p))
	;; see if we have enough comment lines to hide
	(if (and c-reg (> (count-lines (car c-reg) (nth 1 c-reg)) 
			  (if hs-show-hidden-short-form 1 2)))
	    (hs-hide-block)
	  (goto-char p))))

(defun hs-inside-comment-p ()
  "Returns non-nil if point is inside a comment, otherwise nil.
Actually, returns a list containing the buffer position of the start 
and the end of the comment. A comment block can be hided only if on its 
starting line there are only white spaces preceding the actual comment 
beginning, if we are inside of a comment but this condition is not 
we return a list having a nil as its car and the end of comment position
as cdr."
  (save-excursion 
    ;; the idea is to look backwards for a comment start regexp, do a
    ;; forward comment, and see if we are inside, then extend extend
    ;; forward and backward as long as we have comments
    (let ((q (point)))
      (when (or (looking-at hs-c-start-regexp)
	      (re-search-backward hs-c-start-regexp (point-min) t))
	(forward-comment (- (buffer-size)))
	(skip-chars-forward " \t\n")
	(let ((p (point))
	      (not-hidable nil))
	  (beginning-of-line)
	  (unless (looking-at (concat "[ \t]*" hs-c-start-regexp))
	    ;; we are in this situation: (example)
	    ;; (defun bar ()
	    ;;      (foo)
	    ;;                ) ; comment
	    ;;                 ^
	    ;;   the point was here before doing (beginning-of-line)
	    ;; here we should advance till the next comment which 
	    ;; eventually has only white spaces preceding it on the same
	    ;; line 
	    (goto-char p)
	    (forward-comment 1)
	    (skip-chars-forward " \t\n")
	    (setq p (point))
	    (while (and (< (point) q) 
			(> (point) p) 
			(not (looking-at hs-c-start-regexp)))
	      (setq p (point))  ;; use this to avoid an infinit cycle.
	      (forward-comment 1)
	      (skip-chars-forward " \t\n"))
	    (if (or (not (looking-at hs-c-start-regexp)) 
		    (> (point) q))
		;; we cannot hide this comment block
		(setq not-hidable t)))
	  ;; goto the end of the comment
	  (forward-comment (buffer-size))
	  (skip-chars-backward " \t\n")
	  (end-of-line)
	  (if (>= (point) q)
	      (list (if not-hidable nil p) (point))))))))

(defun hs-grok-mode-type ()
  "Setup variables for new buffers where applicable."
  (when (and (boundp 'comment-start)
	     (boundp 'comment-end))
    (let ((lookup (assoc major-mode hs-special-modes-alist)))
      (setq hs-block-start-regexp (or (nth 1 lookup) "\\s\(")
	    hs-block-end-regexp (or (nth 2 lookup) "\\s\)")
	    hs-c-start-regexp (or (nth 3 lookup)
				  (let ((c-start-regexp 
					 (regexp-quote comment-start)))
				    (if (string-match " +$" c-start-regexp)
					 (substring c-start-regexp 0 (1- (match-end 0)))
				      c-start-regexp)))
	    hs-forward-sexp-func (or (nth 4 lookup) 'forward-sexp)
	    hs-adjust-block-beginning (nth 5 lookup)))))

(defun hs-find-block-beginning ()
  "Repositions point at block-start.
Return point, or nil if top-level."
  (let (done
	(try-again t)
	(here (point))
	(both-regexps (concat "\\(" hs-block-start-regexp "\\)\\|\\("
			      hs-block-end-regexp "\\)"))
	(buf-size (buffer-size)))
    (beginning-of-line)
    ;; A block beginning can span on multiple lines, if the point
    ;; is on one of those lines, trying a regexp search from
    ;; that point would fail to find the block beginning, so we look
    ;; backwards for the block beginning, or a block end.
    (while try-again
      (setq try-again nil)
      (if (and (re-search-backward both-regexps (point-min) t)
	       (match-beginning 1)) ; found a block beginning
	  (if (save-match-data (hs-inside-comment-p)) 
	      ;;but it was inside a comment, so we have to look for
	      ;;it again
	      (setq try-again t)
	    ;; that's what we were looking for
	    (setq done (match-beginning 0)))
	;; we found a block end, or we reached the beginning of the
	;; buffer look to see if we were on a block beginning when we
	;; started
	(if (and 
	     (re-search-forward hs-block-start-regexp (point-max) t)
	     (or 
	      (and (>= here (match-beginning 0)) (< here (match-end 0)))
	      (and hs-show-hidden-short-form hs-adjust-block-beginning
		   (save-match-data 
		     (= 1 (count-lines 
			   (funcall hs-adjust-block-beginning
				    (match-end 0)) here))))))
	      (setq done (match-beginning 0)))))
    (goto-char here)
    (while (and (not done)
		;; This had problems because the regexp can match something
		;; inside of a comment!
		;; Since inside a comment we can have incomplete sexps
		;; this would have signaled an error.
		(or (forward-comment (- buf-size)) t); `or' is a hack to
							 ; make it return t
		(re-search-backward both-regexps (point-min) t))
      (if (match-beginning 1)		; start of start-regexp
	  (setq done (match-beginning 0))
	(goto-char (match-end 0))	; end of end-regexp
	(funcall hs-forward-sexp-func -1)))
    (goto-char (or done here))
    done))

(defmacro hs-life-goes-on (&rest body)
  "Executes optional BODY iff variable `hs-minor-mode' is non-nil."
  (` (let ((inhibit-point-motion-hooks t))
       (when hs-minor-mode 
	   (,@ body)))))

(put 'hs-life-goes-on 'edebug-form-spec '(&rest form))

(defun hs-already-hidden-p ()
  "Return non-nil if point is in an already-hidden block, otherwise nil."
  (save-excursion
    (let ((c-reg (hs-inside-comment-p)))
      (if (and c-reg (nth 0 c-reg))
	  ;; point is inside a comment, and that comment is hidable
	  (goto-char (nth 0 c-reg))
	(if (and (not c-reg) (hs-find-block-beginning)
		 (looking-at hs-block-start-regexp))
	    ;; point is inside a block
	    (goto-char (match-end 0)))))
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

(defun hs-c-like-adjust-block-beginning (arg)
  "Function to be assigned to `hs-adjust-block-beginning' for C like modes.
Arg is a position in buffer just after {. This goes back to the end of
the function header. The purpose is to save some space on the screen
when displaying hidden blocks."
  (save-excursion
    (goto-char arg)
    (forward-char -1)
    (forward-comment (- (buffer-size)))
    (point)))

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
	 (let (c-reg
	       (count 0)
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
	       (if (and c-reg (car c-reg))
		   (if (> (count-lines (car c-reg) (nth 1 c-reg)) 
			  (if hs-show-hidden-short-form 1 2))
		       (progn
			 (hs-hide-block-at-point t c-reg)
			 (message "Hiding ... %d" (setq count (1+ count))))
		     (goto-char (nth 1 c-reg)))))))
       (let ((count 0)
	     (top-level-re (concat "^" hs-block-start-regexp))
	     (buf-size (buffer-size)))
	 (while 
	     (progn
	       (forward-comment buf-size)
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
With prefix arg, reposition at end. Block is defined as a sexp for
lispish modes, mode-specific otherwise. Comments are blocks, too.
Upon completion, point is at repositioned and the normal hook
`hs-hide-hook' is run.  See documentation for `run-hooks'."
  (interactive "P")
  (hs-life-goes-on
   (let ((c-reg (hs-inside-comment-p)))
     (cond
      ((and c-reg (or (null (nth 0 c-reg)) 
		      (<= (count-lines (car c-reg) (nth 1 c-reg)) 
			  (if hs-show-hidden-short-form 1 2))))
	   (message "Not enough comment lines to hide!"))
      ((or c-reg (looking-at hs-block-start-regexp)
	       (hs-find-block-beginning))
       (hs-hide-block-at-point end c-reg)
       (hs-safety-is-job-n)
       (run-hooks 'hs-hide-hook))))))

(defun hs-show-block (&optional end)
  "Selects a block and shows it.
With prefix arg, reposition at end. Upon completion, point is
repositioned and the normal hook `hs-show-hook' is run.  
See documentation for `hs-hide-block' and `run-hooks'."
  (interactive "P")
  (hs-life-goes-on
   (let ((c-reg (hs-inside-comment-p)))
     (if (or c-reg
	     (looking-at hs-block-start-regexp)
	     (hs-find-block-beginning))
	   (progn
	     (hs-show-block-at-point end c-reg)
	     (hs-safety-is-job-n)
	     (run-hooks 'hs-show-hook))))))

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
commands and the hideshow commands are enabled.  
The value '(hs . t) is added to `buffer-invisibility-spec'.
Last, the normal hook `hs-minor-mode-hook' is run; see the doc 
for `run-hooks'.

The main commands are: `hs-hide-all', `hs-show-all', `hs-hide-block'
and `hs-show-block'. 
Also see the documentation for the variable `hs-show-hidden-short-form'.

Turning hideshow minor mode off reverts the menu bar and the
variables to default values and disables the hideshow commands.

Key bindings:
\\{hs-minor-mode-map}"

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
    '("Hide Block" . hs-hide-block)))

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
(make-variable-buffer-local 'hs-block-start-regexp)
(make-variable-buffer-local 'hs-block-end-regexp)
(make-variable-buffer-local 'hs-forward-sexp-func)
(make-variable-buffer-local 'hs-adjust-block-beginning)
(put 'hs-minor-mode 'permanent-local t)
(put 'hs-c-start-regexp 'permanent-local t)
(put 'hs-block-start-regexp 'permanent-local t)
(put 'hs-block-end-regexp 'permanent-local t)
(put 'hs-forward-sexp-func 'permanent-local t)
(put 'hs-adjust-block-beginning 'permanent-local t)


;;;----------------------------------------------------------------------------
;;; that's it

(provide 'hideshow)

;;; hideshow.el ends here
