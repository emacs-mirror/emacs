;;; isearch.el --- incremental search minor mode.

;; Copyright (C) 1992, 1993, 1994 Free Software Foundation, Inc.

;; Author: Daniel LaLiberte <liberte@cs.uiuc.edu>

;; |$Date: 1995/03/18 18:10:21 $|$Revision: 1.85 $

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

;;;====================================================================
;; Instructions

;; For programmed use of isearch-mode, e.g. calling (isearch-forward),
;; isearch-mode behaves modally and does not return until the search
;; is completed.  It uses a recursive-edit to behave this way.  Note:
;; gnus does it wrong: (call-interactively 'isearch-forward).

;; The key bindings active within isearch-mode are defined below in
;; `isearch-mode-map' which is given bindings close to the default
;; characters of the original isearch.el.  With `isearch-mode',
;; however, you can bind multi-character keys and it should be easier
;; to add new commands.  One bug though: keys with meta-prefix cannot
;; be longer than two chars.  Also see minibuffer-local-isearch-map
;; for bindings active during `isearch-edit-string'.

;; Note to emacs version 19 users: isearch-mode should work even if
;; you switch windows with the mouse, in which case isearch-mode is
;; terminated automatically before the switch.  This is true of lemacs
;; too, with a few more cleanups I've neglected in this release. 
;; No one has supplied patches for epoch yet.

;; The search ring and completion commands automatically put you in
;; the minibuffer to edit the string.  This gives you a chance to
;; modify the search string before executing the search.  There are
;; three commands to terminate the editing: C-s and C-r exit the
;; minibuffer and search forward and reverse respectively, while C-m
;; exits and does a nonincremental search.

;; Exiting immediately from isearch uses isearch-edit-string instead
;; of nonincremental-search, if search-nonincremental-instead is non-nil.
;; The name of this option should probably be changed if we decide to
;; keep the behavior.  No point in forcing nonincremental search until
;; the last possible moment.

;; TODO
;; - Integrate the emacs 19 generalized command history.
;; - Think about incorporating query-replace.
;; - Hooks and options for failed search.

;;; Change Log:

;;; Changes before those recorded in ChangeLog:

;;; Revision 1.4  92/09/14  16:26:02  liberte
;;; Added prefix args to isearch-forward, etc. to switch between
;;;    string and regular expression searching.
;;; Added some support for lemacs.
;;; Added general isearch-highlight option - but only for lemacs so far.
;;; Added support for frame switching in emacs 19.
;;; Added word search option to isearch-edit-string.
;;; Renamed isearch-quit to isearch-abort.
;;; Numerous changes to comments and doc strings.
;;; 
;;; Revision 1.3  92/06/29  13:10:08  liberte
;;; Moved modal isearch-mode handling into isearch-mode.
;;; Got rid of buffer-local isearch variables.
;;; isearch-edit-string used by ring adjustments, completion, and
;;; nonincremental searching.  C-s and C-r are additional exit commands.
;;; Renamed all regex to regexp.
;;; Got rid of found-start and found-point globals.
;;; Generalized handling of upper-case chars.
 
;;; Revision 1.2  92/05/27  11:33:57  liberte
;;; Emacs version 19 has a search ring, which is supported here.
;;; Other fixes found in the version 19 isearch are included here.
;;;
;;; Also see variables search-caps-disable-folding,
;;; search-nonincremental-instead, search-whitespace-regexp, and
;;; commands isearch-toggle-regexp, isearch-edit-string.
;;;
;;; semi-modal isearching is supported.

;;; Changes for 1.1
;;; 3/18/92 Fixed invalid-regexp.
;;; 3/18/92 Fixed yanking in regexps.

;;; Code:


;;;========================================================================
;;; Some additional options and constants.

(defconst search-exit-option t
  "*Non-nil means random control characters terminate incremental search.")

(defvar search-slow-window-lines 1
  "*Number of lines in slow search display windows.
These are the short windows used during incremental search on slow terminals.
Negative means put the slow search window at the top (normally it's at bottom)
and the value is minus the number of lines.")

(defvar search-slow-speed 1200
  "*Highest terminal speed at which to use \"slow\" style incremental search.
This is the style where a one-line window is created to show the line
that the search has reached.")

(defvar search-upper-case 'not-yanks
  "*If non-nil, upper case chars disable case fold searching.
That is, upper and lower case chars must match exactly.
This applies no matter where the chars come from, but does not
apply to chars in regexps that are prefixed with `\\'.
If this value is `not-yanks', yanked text is always downcased.")

(defvar search-nonincremental-instead t
  "*If non-nil, do a nonincremental search instead if exiting immediately.
Actually, `isearch-edit-string' is called to let you enter the search
string, and RET terminates editing and does a nonincremental search.")

(defconst search-whitespace-regexp "\\s-+"
  "*If non-nil, regular expression to match a sequence of whitespace chars.
You might want to use something like \"[ \\t\\r\\n]+\" instead.")

;; I removed the * from the doc string because highlighting is not 
;; currently a clean thing to do.  Once highlighting is made clean, 
;; this feature can be re-enabled and advertised.
(defvar search-highlight nil
  "*Non-nil means incremental search highlights the current match.")

(defvar isearch-mode-hook nil
  "Function(s) to call after starting up an incremental search.")

(defvar isearch-mode-end-hook nil
  "Function(s) to call after terminating an incremental search.")

;;;==================================================================
;;; Search ring.

(defvar search-ring nil
  "List of search string sequences.")
(defvar regexp-search-ring nil
  "List of regular expression search string sequences.")

(defconst search-ring-max 16
  "*Maximum length of search ring before oldest elements are thrown away.")
(defconst regexp-search-ring-max 16
  "*Maximum length of regexp search ring before oldest elements are thrown away.")

(defvar search-ring-yank-pointer nil
  "Index in `search-ring' of last string reused.
nil if none yet.")
(defvar regexp-search-ring-yank-pointer nil
  "Index in `regexp-search-ring' of last string reused.
nil if none yet.")

(defvar search-ring-update nil
  "*Non-nil if advancing or retreating in the search ring should cause search.
Default value, nil, means edit the string instead.")

;;;====================================================
;;; Define isearch-mode keymap.

(defvar isearch-mode-map nil
  "Keymap for isearch-mode.")

(or isearch-mode-map
    (let* ((i 0)
	   (map (make-keymap)))
      (or (vectorp (nth 1 map))
	  (error "The initialization of isearch-mode-map must be updated"))
      ;; Give this map a vector 256 long, for dense binding
      ;; of a larger range of ordinary characters.
      (setcar (cdr map) (make-vector 256 nil))

      ;; Make function keys, etc, exit the search.
      (define-key map [t] 'isearch-other-control-char)
      ;; Control chars, by default, end isearch mode transparently.
      ;; We need these explicit definitions because, in a dense keymap, 
      ;; the binding for t does not affect characters.
      ;; We use a dense keymap to save space.
      (while (< i ?\ )
	(define-key map (make-string 1 i) 'isearch-other-control-char)
	(setq i (1+ i)))

      ;; Printing chars extend the search string by default.
      (setq i ?\ )
      (while (< i (length (nth 1 map)))
	(define-key map (vector i) 'isearch-printing-char)
	(setq i (1+ i)))

      ;; To handle local bindings with meta char prefix keys, define
      ;; another full keymap.  This must be done for any other prefix
      ;; keys as well, one full keymap per char of the prefix key.  It
      ;; would be simpler to disable the global keymap, and/or have a
      ;; default local key binding for any key not otherwise bound.
      (let ((meta-map (make-sparse-keymap)))
	(define-key map (char-to-string meta-prefix-char) meta-map)
	(define-key map [escape] meta-map))
      (define-key map (vector meta-prefix-char t) 'isearch-other-meta-char)

      ;; Several non-printing chars change the searching behavior.
      (define-key map "\C-s" 'isearch-repeat-forward)
      (define-key map "\C-r" 'isearch-repeat-backward)
      (define-key map "\177" 'isearch-delete-char)
      (define-key map "\C-g" 'isearch-abort)
      ;; This assumes \e is the meta-prefix-char.
      (or (= ?\e meta-prefix-char)
	  (error "Inconsistency in isearch.el"))
      (define-key map "\e\e\e" 'isearch-cancel)
      (define-key map  [escape escape escape] 'isearch-cancel)
    
      (define-key map "\C-q" 'isearch-quote-char)

      (define-key map "\r" 'isearch-exit)
      (define-key map "\C-j" 'isearch-printing-char)
      (define-key map "\t" 'isearch-printing-char)
      (define-key map " " 'isearch-whitespace-chars)
    
      (define-key map "\C-w" 'isearch-yank-word)
      (define-key map "\C-y" 'isearch-yank-line)

      ;; Bind the ASCII-equivalent "function keys" explicitly to nil
      ;; so that the default binding does not apply.
      ;; As a result, these keys translate thru function-key-map
      ;; as normal, and they have the effect of the equivalent ASCII char.
      ;; We bind [escape] below.
      (define-key map [tab] 'nil)
      (define-key map [kp-0] 'nil)
      (define-key map [kp-1] 'nil)
      (define-key map [kp-2] 'nil)
      (define-key map [kp-3] 'nil)
      (define-key map [kp-4] 'nil)
      (define-key map [kp-5] 'nil)
      (define-key map [kp-6] 'nil)
      (define-key map [kp-7] 'nil)
      (define-key map [kp-8] 'nil)
      (define-key map [kp-9] 'nil)
      (define-key map [kp-add] 'nil)
      (define-key map [kp-subtract] 'nil)
      (define-key map [kp-multiply] 'nil)
      (define-key map [kp-divide] 'nil)
      (define-key map [kp-decimal] 'nil)
      (define-key map [kp-separator] 'nil)
      (define-key map [kp-equal] 'nil)
      (define-key map [kp-tab] 'nil)
      (define-key map [kp-space] 'nil)
      (define-key map [kp-enter] 'nil)
      (define-key map [delete] 'nil)
      (define-key map [backspace] 'nil)
      (define-key map [return] 'nil)
      (define-key map [newline] 'nil)

      ;; Define keys for regexp chars * ? |.
      ;; Nothing special for + because it matches at least once.
      (define-key map "*" 'isearch-*-char)
      (define-key map "?" 'isearch-*-char)
      (define-key map "|" 'isearch-|-char)

;;; Turned off because I find I expect to get the global definition--rms.
;;;      ;; Instead bind C-h to special help command for isearch-mode.
;;;      (define-key map "\C-h" 'isearch-mode-help)

      (define-key map "\M-n" 'isearch-ring-advance)
      (define-key map "\M-p" 'isearch-ring-retreat)
      (define-key map "\M-y" 'isearch-yank-kill)

      (define-key map "\M-\t" 'isearch-complete)

      ;; Switching frames should terminate isearch-mode
      (define-key map [switch-frame] 'isearch-switch-frame-handler)
      
      (setq isearch-mode-map map)
      ))

;; Some bindings you may want to put in your isearch-mode-hook.
;; Suggest some alternates...
;; (define-key isearch-mode-map "\C-t" 'isearch-toggle-case-fold)
;; (define-key isearch-mode-map "\C-t" 'isearch-toggle-regexp)
;; (define-key isearch-mode-map "\C-^" 'isearch-edit-string)


(defvar minibuffer-local-isearch-map nil
  "Keymap for editing isearch strings in the minibuffer.")

(or minibuffer-local-isearch-map
    (let ((map (copy-keymap minibuffer-local-map)))
      (define-key map "\r" 'isearch-nonincremental-exit-minibuffer)
      (define-key map "\M-n" 'isearch-ring-advance-edit)
      (define-key map "\M-p" 'isearch-ring-retreat-edit)
      (define-key map "\M-\t" 'isearch-complete-edit)
      (define-key map "\C-s" 'isearch-forward-exit-minibuffer)
      (define-key map "\C-r" 'isearch-reverse-exit-minibuffer)
      (setq minibuffer-local-isearch-map map)
      ))

;;;========================================================
;; Internal variables declared globally for byte-compiler.
;; These are all set with setq while isearching
;; and bound locally while editing the search string.

(defvar isearch-forward nil)	; Searching in the forward direction.
(defvar isearch-regexp nil)	; Searching for a regexp.
(defvar isearch-word nil)	; Searching for words.

(defvar isearch-cmds nil)   ; Stack of search status sets.
(defvar isearch-string "")  ; The current search string.
(defvar isearch-message "") ; text-char-description version of isearch-string

(defvar isearch-success t)		; Searching is currently successful.
(defvar isearch-invalid-regexp nil)	; Regexp not well formed.
(defvar isearch-within-brackets nil)	; Regexp has unclosed [.
(defvar isearch-other-end nil)	; Start (end) of match if forward (backward).
(defvar isearch-wrapped nil)	; Searching restarted from the top (bottom).
(defvar isearch-barrier 0)

; case-fold-search while searching.
;   either nil, t, or 'yes.  'yes means the same as t except that mixed
;   case in the search string is ignored.
(defvar isearch-case-fold-search nil)

(defvar isearch-adjusted nil)
(defvar isearch-slow-terminal-mode nil)
;;; If t, using a small window.
(defvar isearch-small-window nil)
(defvar isearch-opoint 0)
;;; The window configuration active at the beginning of the search.
(defvar isearch-window-configuration nil)

;; Flag to indicate a yank occurred, so don't move the cursor.
(defvar isearch-yank-flag nil)

;;; A function to be called after each input character is processed.
;;; (It is not called after characters that exit the search.)
;;; It is only set from an optional argument to `isearch-mode'.
(defvar isearch-op-fun nil)

;;;  Is isearch-mode in a recursive edit for modal searching.
(defvar isearch-recursive-edit nil)

;;; Should isearch be terminated after doing one search?
(defvar isearch-nonincremental nil)

;; New value of isearch-forward after isearch-edit-string.
(defvar isearch-new-forward nil)


;;;==============================================================
;; Minor-mode-alist changes - kind of redundant with the
;; echo area, but if isearching in multiple windows, it can be useful.

(or (assq 'isearch-mode minor-mode-alist)
    (nconc minor-mode-alist
	   (list '(isearch-mode isearch-mode))))

(defvar isearch-mode nil) ;; Name of the minor mode, if non-nil.
(make-variable-buffer-local 'isearch-mode)

(define-key global-map "\C-s" 'isearch-forward)
(define-key esc-map "\C-s" 'isearch-forward-regexp)
(define-key global-map "\C-r" 'isearch-backward)
(define-key esc-map "\C-r" 'isearch-backward-regexp)

;;;===============================================================
;;; Entry points to isearch-mode.
;;; These four functions should replace those in loaddefs.el
;;; An alternative is to defalias isearch-forward etc to isearch-mode,
;;; and look at this-command to set the options accordingly.

(defun isearch-forward (&optional regexp-p no-recursive-edit)
  "\
Do incremental search forward.
With a prefix argument, do an incremental regular expression search instead.
\\<isearch-mode-map>
As you type characters, they add to the search string and are found.
The following non-printing keys are bound in `isearch-mode-map'.  

Type \\[isearch-delete-char] to cancel characters from end of search string.
Type \\[isearch-exit] to exit, leaving point at location found.
Type LFD (C-j) to match end of line.
Type \\[isearch-repeat-forward] to search again forward,\
 \\[isearch-repeat-backward] to search again backward.
Type \\[isearch-yank-word] to yank word from buffer onto end of search\
 string and search for it.
Type \\[isearch-yank-line] to yank rest of line onto end of search string\
 and search for it.
Type \\[isearch-quote-char] to quote control character to search for it.
\\[isearch-abort] while searching or when search has failed cancels input\
 back to what has
 been found successfully.
\\[isearch-abort] when search is successful aborts and moves point to\
 starting point.

Also supported is a search ring of the previous 16 search strings.
Type \\[isearch-ring-advance] to search for the next item in the search ring.
Type \\[isearch-ring-retreat] to search for the previous item in the search\
 ring.
Type \\[isearch-complete] to complete the search string using the search ring.

The above keys, bound in `isearch-mode-map', are often controlled by 
 options; do M-x apropos on search-.* to find them.
Other control and meta characters terminate the search
 and are then executed normally (depending on `search-exit-option').

If this function is called non-interactively, it does not return to
the calling function until the search is done."

  (interactive "P\np")
  (isearch-mode t (not (null regexp-p)) nil (not no-recursive-edit)))

(defun isearch-forward-regexp (&optional not-regexp no-recursive-edit)
  "\
Do incremental search forward for regular expression.
With a prefix argument, do a regular string search instead.
Like ordinary incremental search except that your input
is treated as a regexp.  See \\[isearch-forward] for more info."
  (interactive "P\np")
  (isearch-mode t (null not-regexp) nil (not no-recursive-edit)))

(defun isearch-backward (&optional regexp-p no-recursive-edit)
  "\
Do incremental search backward.
With a prefix argument, do a regular expression search instead.
See \\[isearch-forward] for more information."
  (interactive "P\np")
  (isearch-mode nil (not (null regexp-p)) nil (not no-recursive-edit)))

(defun isearch-backward-regexp (&optional not-regexp no-recursive-edit)
  "\
Do incremental search backward for regular expression.
With a prefix argument, do a regular string search instead.
Like ordinary incremental search except that your input
is treated as a regexp.  See \\[isearch-forward] for more info."
  (interactive "P\np")
  (isearch-mode nil (null not-regexp) nil (not no-recursive-edit)))


(defun isearch-mode-help ()
  (interactive)
  (describe-function 'isearch-forward)
  (isearch-update))


;;;==================================================================
;; isearch-mode only sets up incremental search for the minor mode.
;; All the work is done by the isearch-mode commands.

;; Not used yet:
;;(defconst isearch-commands '(isearch-forward isearch-backward
;;			     isearch-forward-regexp isearch-backward-regexp)
;;  "List of commands for which isearch-mode does not recursive-edit.")
			     

(defun isearch-mode (forward &optional regexp op-fun recursive-edit word-p)
  "Start isearch minor mode.  Called by isearch-forward, etc."

  ;; Initialize global vars.
  (setq isearch-forward forward
	isearch-regexp regexp
	isearch-word word-p
	isearch-op-fun op-fun
	isearch-case-fold-search case-fold-search
	isearch-string ""
	isearch-message ""
	isearch-cmds nil
	isearch-success t
	isearch-wrapped nil
	isearch-barrier (point)
	isearch-adjusted nil
	isearch-yank-flag nil
	isearch-invalid-regexp nil
	isearch-within-brackets nil
	isearch-slow-terminal-mode (and (<= baud-rate search-slow-speed)
					(> (window-height)
					   (* 4 search-slow-window-lines)))
	isearch-other-end nil
	isearch-small-window nil

	isearch-opoint (point)
	search-ring-yank-pointer nil
	regexp-search-ring-yank-pointer nil)
  (setq isearch-window-configuration
	(if isearch-slow-terminal-mode (current-window-configuration) nil))

  (setq	isearch-mode " Isearch")  ;; forward? regexp?
  (set-buffer-modified-p (buffer-modified-p)) ; update modeline

  (isearch-push-state)

  (make-local-variable 'overriding-local-map)
  (setq overriding-local-map isearch-mode-map)
  (isearch-update)
  (run-hooks 'isearch-mode-hook)

  (setq mouse-leave-buffer-hook '(isearch-done))

  ;; isearch-mode can be made modal (in the sense of not returning to 
  ;; the calling function until searching is completed) by entering 
  ;; a recursive-edit and exiting it when done isearching.
  (if recursive-edit
      (let ((isearch-recursive-edit t))
	(recursive-edit)))
  isearch-success)


;;;====================================================
;; Some high level utilities.  Others below.

(defun isearch-update ()
  ;; Called after each command to update the display.  
  (if (null unread-command-events)
      (progn
	(if (not (input-pending-p))
	    (isearch-message))
	(if (and isearch-slow-terminal-mode
		 (not (or isearch-small-window 
			  (pos-visible-in-window-p))))
	    (let ((found-point (point)))
	      (setq isearch-small-window t)
	      (move-to-window-line 0)
	      (let ((window-min-height 1))
		(split-window nil (if (< search-slow-window-lines 0)
				      (1+ (- search-slow-window-lines))
				    (- (window-height)
				       (1+ search-slow-window-lines)))))
	      (if (< search-slow-window-lines 0)
		  (progn (vertical-motion (- 1 search-slow-window-lines))
			 (set-window-start (next-window) (point))
			 (set-window-hscroll (next-window)
					     (window-hscroll))
			 (set-window-hscroll (selected-window) 0))
		(other-window 1))
	      (goto-char found-point)))
	(if isearch-other-end
	    (if (< isearch-other-end (point)) ; isearch-forward?
		(isearch-highlight isearch-other-end (point))
	      (isearch-highlight (point) isearch-other-end))
	  (isearch-dehighlight nil))
	))
  (setq ;; quit-flag nil  not for isearch-mode
   isearch-adjusted nil
   isearch-yank-flag nil)
  )

(defun isearch-done (&optional nopush edit)
  (setq mouse-leave-buffer-hook nil)
  ;; Called by all commands that terminate isearch-mode.
  ;; If NOPUSH is non-nil, we don't push the string on the search ring.
  (setq overriding-local-map nil)
  ;; (setq pre-command-hook isearch-old-pre-command-hook) ; for lemacs
  (isearch-dehighlight t)
  (let ((found-start (window-start (selected-window)))
	(found-point (point)))
    (if isearch-window-configuration
	(set-window-configuration isearch-window-configuration))

    (if isearch-small-window
	(goto-char found-point)
      ;; Exiting the save-window-excursion clobbers window-start; restore it.
      (set-window-start (selected-window) found-start t))

    ;; If there was movement, mark the starting position.
    ;; Maybe should test difference between and set mark iff > threshold.
    (if (/= (point) isearch-opoint)
	(or (and transient-mark-mode mark-active)
	    (progn
	      (push-mark isearch-opoint t)
	      (or executing-macro (> (minibuffer-depth) 0)
		  (message "Mark saved where search started"))))))

  (setq isearch-mode nil)
  (set-buffer-modified-p (buffer-modified-p))  ;; update modeline

  (if (and (> (length isearch-string) 0) (not nopush))
      ;; Update the ring data.
      (isearch-update-ring isearch-string isearch-regexp))

  (run-hooks 'isearch-mode-end-hook)
  (and (not edit) isearch-recursive-edit (exit-recursive-edit)))

(defun isearch-update-ring (string &optional regexp)
  "Add STRING to the beginning of the search ring.
REGEXP says which ring to use."
  (if regexp 
      (if (or (null regexp-search-ring)
	      (not (string= isearch-string (car regexp-search-ring))))
	  (progn
	    (setq regexp-search-ring
		  (cons isearch-string regexp-search-ring))
	    (if (> (length regexp-search-ring) regexp-search-ring-max)
		(setcdr (nthcdr (1- search-ring-max) regexp-search-ring)
			nil))))
    (if (or (null search-ring)
	    (not (string= isearch-string (car search-ring))))
	(progn
	  (setq search-ring (cons isearch-string search-ring))
	  (if (> (length search-ring) search-ring-max)
	      (setcdr (nthcdr (1- search-ring-max) search-ring) nil))))))

;;;=======================================================
;;; Switching buffers should first terminate isearch-mode.
;;; This is done quite differently for each variant of emacs.
;;; For lemacs, see Exiting in lemacs below

;; For Emacs 19, the frame switch event is handled.
(defun isearch-switch-frame-handler ()
  (interactive) ;; Is this necessary?
  ;; First terminate isearch-mode.
  (isearch-done)
  (handle-switch-frame (car (cdr (isearch-last-command-char)))))

;;;========================================================


;;;====================================================
;; Commands active while inside of the isearch minor mode.

(defun isearch-exit ()
  "Exit search normally.
However, if this is the first command after starting incremental
search and `search-nonincremental-instead' is non-nil, do a
nonincremental search instead via `isearch-edit-string'."
  (interactive)
  (if (and search-nonincremental-instead 
	   (= 0 (length isearch-string)))
      (let ((isearch-nonincremental t))
	(isearch-edit-string)))
  (isearch-done))


(defun isearch-edit-string ()
  "Edit the search string in the minibuffer.
The following additional command keys are active while editing.
\\<minibuffer-local-isearch-map>
\\[exit-minibuffer] to resume incremental searching with the edited string.
\\[isearch-nonincremental-exit-minibuffer] to do one nonincremental search.
\\[isearch-forward-exit-minibuffer] to resume isearching forward.
\\[isearch-reverse-exit-minibuffer] to resume isearching backward.
\\[isearch-ring-advance-edit] to replace the search string with the next item in the search ring.
\\[isearch-ring-retreat-edit] to replace the search string with the previous item in the search ring.
\\[isearch-complete-edit] to complete the search string using the search ring.
\\<isearch-mode-map>
If first char entered is \\[isearch-yank-word], then do word search instead."

  ;; This code is very hairy for several reasons, explained in the code.
  ;; Mainly, isearch-mode must be terminated while editing and then restarted.
  ;; If there were a way to catch any change of buffer from the minibuffer,
  ;; this could be simplified greatly.
  ;; Editing doesn't back up the search point.  Should it?
  (interactive)
  (condition-case err
      (let ((isearch-nonincremental isearch-nonincremental)

	    ;; Locally bind all isearch global variables to protect them
	    ;; from recursive isearching.
	    ;; isearch-string -message and -forward are not bound
	    ;; so they may be changed.  Instead, save the values.
	    (isearch-new-string isearch-string)
	    (isearch-new-message isearch-message)
	    (isearch-new-forward isearch-forward)
	    (isearch-new-word isearch-word)

	    (isearch-regexp isearch-regexp)
	    (isearch-op-fun isearch-op-fun)
	    (isearch-cmds isearch-cmds)
	    (isearch-success isearch-success)
	    (isearch-wrapped isearch-wrapped)
	    (isearch-barrier isearch-barrier)
	    (isearch-adjusted isearch-adjusted)
	    (isearch-yank-flag isearch-yank-flag)
	    (isearch-invalid-regexp isearch-invalid-regexp)
	    (isearch-within-brackets isearch-within-brackets)
	    (isearch-other-end isearch-other-end)
	    (isearch-opoint isearch-opoint)
	    (isearch-slow-terminal-mode isearch-slow-terminal-mode)
	    (isearch-small-window isearch-small-window)
	    (isearch-recursive-edit isearch-recursive-edit)
	    ;; Save current configuration so we can restore it here.
	    (isearch-window-configuration (current-window-configuration))
	    )

	;; Actually terminate isearching until editing is done.
	;; This is so that the user can do anything without failure, 
	;; like switch buffers and start another isearch, and return.
	(condition-case err
	    (isearch-done t t)
	  (exit nil))			; was recursive editing

	(isearch-message) ;; for read-char
	(unwind-protect
	    (let* (;; Why does following read-char echo?  
		   ;;(echo-keystrokes 0) ;; not needed with above message
		   (e (let ((cursor-in-echo-area t))
			(read-event)))
		   ;; Binding minibuffer-history-symbol to nil is a work-around
		   ;; for some incompatibility with gmhist.
		   (minibuffer-history-symbol)
		   (message-log-max nil))
	      ;; If the first character the user types when we prompt them
	      ;; for a string is the yank-word character, then go into
	      ;; word-search mode.  Otherwise unread that character and
	      ;; read a key the normal way.
	      ;; Word search does not apply (yet) to regexp searches,
	      ;; no check is made here.
	      (message (isearch-message-prefix nil nil t))
	      (if (eq 'isearch-yank-word
		      (lookup-key isearch-mode-map (vector e)))
		  (setq isearch-word t  ;; so message-prefix is right
			isearch-new-word t)
		(isearch-unread e))
	      (setq cursor-in-echo-area nil)
	      (setq isearch-new-string
		    (let (junk-ring)
		      (read-from-minibuffer
 		       (isearch-message-prefix nil nil isearch-nonincremental)
		       isearch-string
		       minibuffer-local-isearch-map nil
		       'junk-ring))
		    isearch-new-message
		    (mapconcat 'isearch-text-char-description
			       isearch-new-string "")))
	  ;; Always resume isearching by restarting it.
	  (isearch-mode isearch-forward 
			isearch-regexp 
			isearch-op-fun 
			nil
			isearch-word)

	  ;; Copy new local values to isearch globals
	  (setq isearch-string isearch-new-string
		isearch-message isearch-new-message
		isearch-forward isearch-new-forward
		isearch-word isearch-new-word))

	;; Empty isearch-string means use default.
	(if (= 0 (length isearch-string))
	    (setq isearch-string (car (if isearch-regexp regexp-search-ring
					search-ring)))
	  ;; This used to set the last search string,
	  ;; but I think it is not right to do that here.
	  ;; Only the string actually used should be saved.
	  )

	;; Reinvoke the pending search.
	(isearch-push-state)
	(isearch-search)
	(isearch-update)
	(if isearch-nonincremental 
	    (progn
	      ;; (sit-for 1) ;; needed if isearch-done does: (message "")
	      (isearch-done))))

    (quit  ; handle abort-recursive-edit
     (isearch-abort)  ;; outside of let to restore outside global values
     )))

(defun isearch-nonincremental-exit-minibuffer ()
  (interactive)
  (setq isearch-nonincremental t)
  (exit-minibuffer))

(defun isearch-forward-exit-minibuffer ()
  (interactive)
  (setq isearch-new-forward t)
  (exit-minibuffer))

(defun isearch-reverse-exit-minibuffer ()
  (interactive)
  (setq isearch-new-forward nil)
  (exit-minibuffer))

(defun isearch-cancel ()
  "Terminate the search and go back to the starting point."
  (interactive)
  (goto-char isearch-opoint)
  (isearch-done t)
  (signal 'quit nil))  ; and pass on quit signal

(defun isearch-abort ()
  "Abort incremental search mode if searching is successful, signalling quit.
Otherwise, revert to previous successful search and continue searching.
Use `isearch-exit' to quit without signalling."
  (interactive)
;;  (ding)  signal instead below, if quitting
  (discard-input)
  (if isearch-success
      ;; If search is successful, move back to starting point
      ;; and really do quit.
      (progn (goto-char isearch-opoint)
	     (setq isearch-success nil)
	     (isearch-done t)   ; exit isearch
	     (signal 'quit nil))  ; and pass on quit signal
    ;; If search is failing, or has an incomplete regexp,
    ;; rub out until it is once more successful.
    (while (or (not isearch-success) isearch-invalid-regexp)
      (isearch-pop-state))
    (isearch-update)))

(defun isearch-repeat (direction)
  ;; Utility for isearch-repeat-forward and -backward.
  (if (eq isearch-forward (eq direction 'forward))
      ;; C-s in forward or C-r in reverse.
      (if (equal isearch-string "")
	  ;; If search string is empty, use last one.
	  (setq isearch-string
		(or (if isearch-regexp
			(car regexp-search-ring)
		      (car search-ring))
		    "")
		isearch-message
		(mapconcat 'isearch-text-char-description
			   isearch-string ""))
	;; If already have what to search for, repeat it.
	(or isearch-success
	    (progn 

	      (goto-char (if isearch-forward (point-min) (point-max)))
	      (setq isearch-wrapped t))))
    ;; C-s in reverse or C-r in forward, change direction.
    (setq isearch-forward (not isearch-forward)))

  (setq isearch-barrier (point)) ; For subsequent \| if regexp.

  (if (equal isearch-string "")
      (setq isearch-success t)
    (if (and isearch-success (equal (match-end 0) (match-beginning 0)))
	;; If repeating a search that found
	;; an empty string, ensure we advance.
	(if (if isearch-forward (eobp) (bobp))
	    ;; If there's nowhere to advance to, fail (and wrap next time).
	    (progn
	      (setq isearch-success nil)
	      (ding))
	  (forward-char (if isearch-forward 1 -1))
	  (isearch-search))
      (isearch-search)))

  (isearch-push-state)
  (isearch-update))

(defun isearch-repeat-forward ()
  "Repeat incremental search forwards."
  (interactive)
  (isearch-repeat 'forward))

(defun isearch-repeat-backward ()
  "Repeat incremental search backwards."
  (interactive)
  (isearch-repeat 'backward))

(defun isearch-toggle-regexp ()
  "Toggle regexp searching on or off."
  ;; The status stack is left unchanged.
  (interactive)
  (setq isearch-regexp (not isearch-regexp))
  (if isearch-regexp (setq isearch-word nil))
  (isearch-update))

(defun isearch-toggle-case-fold ()
  "Toggle case folding in searching on or off."
  (interactive)
  (setq isearch-case-fold-search
	(if isearch-case-fold-search nil 'yes))
  (let ((message-log-max nil))
    (message "%s%s [case %ssensitive]"
	     (isearch-message-prefix nil nil isearch-nonincremental)
	     isearch-message
	     (if isearch-case-fold-search "in" "")))
  (setq isearch-adjusted t)
  (sit-for 1)
  (isearch-update))

(defun isearch-delete-char ()
  "Discard last input item and move point back.  
If no previous match was done, just beep."
  (interactive)
  (if (null (cdr isearch-cmds))
      (ding)
    (isearch-pop-state))
  (isearch-update))


(defun isearch-yank (chunk)
  ;; Helper for isearch-yank-word and isearch-yank-line
  ;; CHUNK should be word, line or kill.
  (let ((string (cond
                 ((eq chunk 'kill)
                  (current-kill 0))
                 (t
		  (save-excursion
		    (and (not isearch-forward) isearch-other-end
			 (goto-char isearch-other-end))
		    (buffer-substring
		     (point)
		     (save-excursion
		       (cond
			((eq chunk 'word)
			 (forward-word 1))
			((eq chunk 'line)
			 (end-of-line)))
		       (point))))))))
    ;; Downcase the string if not supposed to case-fold yanked strings.
    (if (and isearch-case-fold-search
	     (eq 'not-yanks search-upper-case))
	(setq string (downcase string)))
    (if isearch-regexp (setq string (regexp-quote string)))
    (setq isearch-string (concat isearch-string string)
	  isearch-message
	  (concat isearch-message
		  (mapconcat 'isearch-text-char-description
			     string ""))
	  ;; Don't move cursor in reverse search.
	  isearch-yank-flag t))
  (isearch-search-and-update))

(defun isearch-yank-kill ()
  "Pull string from kill ring into search string."
  (interactive)
  (isearch-yank 'kill))

(defun isearch-yank-word ()
  "Pull next word from buffer into search string."
  (interactive)
  (isearch-yank 'word))

(defun isearch-yank-line ()
  "Pull rest of line from buffer into search string."
  (interactive)
  (isearch-yank 'line))


(defun isearch-search-and-update ()
  ;; Do the search and update the display.
  (if (and (not isearch-success)
	   ;; unsuccessful regexp search may become
	   ;;  successful by addition of characters which
	   ;;  make isearch-string valid
	   (not isearch-regexp))
      nil
    ;; In reverse search, adding stuff at
    ;; the end may cause zero or many more chars to be
    ;; matched, in the string following point.
    ;; Allow all those possibilities without moving point as
    ;; long as the match does not extend past search origin.
    (if (and (not isearch-forward) (not isearch-adjusted)
	     (condition-case ()
		 (looking-at (if isearch-regexp isearch-string
			       (regexp-quote isearch-string)))
	       (error nil))
	       (or isearch-yank-flag
		   (<= (match-end 0) 
		       (min isearch-opoint isearch-barrier))))
	(setq isearch-success t 
	      isearch-invalid-regexp nil
	      isearch-within-brackets nil
	      isearch-other-end (match-end 0))
      ;; Not regexp, not reverse, or no match at point.
      (if (and isearch-other-end (not isearch-adjusted))
	  (goto-char (if isearch-forward isearch-other-end
		       (min isearch-opoint 
			    isearch-barrier 
			    (1+ isearch-other-end)))))
      (isearch-search)
      ))
  (isearch-push-state)
  (if isearch-op-fun (funcall isearch-op-fun))
  (isearch-update))


;; *, ?, and | chars can make a regexp more liberal.
;; They can make a regexp match sooner or make it succeed instead of failing.
;; So go back to place last successful search started
;; or to the last ^S/^R (barrier), whichever is nearer.
;; + needs no special handling because the string must match at least once.

(defun isearch-*-char ()
  "Handle * and ? specially in regexps."
  (interactive)
  (if isearch-regexp 

      (progn
	(setq isearch-adjusted t)
	(let ((cs (nth (if isearch-forward
			   5		; isearch-other-end
			 2)		; saved (point)
		       (car (cdr isearch-cmds)))))
	  ;; (car isearch-cmds) is after last search;
	  ;; (car (cdr isearch-cmds)) is from before it.
	  (setq cs (or cs isearch-barrier))
	  (goto-char
	   (if isearch-forward
	       (max cs isearch-barrier)
	     (min cs isearch-barrier))))))
  (isearch-process-search-char (isearch-last-command-char)))
  

(defun isearch-|-char ()
  "If in regexp search, jump to the barrier."
  (interactive)
  (if isearch-regexp
      (progn
	(setq isearch-adjusted t)
	(goto-char isearch-barrier)))
  (isearch-process-search-char (isearch-last-command-char)))


(defalias 'isearch-other-control-char 'isearch-other-meta-char)

(defun isearch-other-meta-char ()
  "Exit the search normally and reread this key sequence.
But only if `search-exit-option' is non-nil, the default.
If it is the symbol `edit', the search string is edited in the minibuffer
and the meta character is unread so that it applies to editing the string."
  (interactive)
  (cond ((eq search-exit-option 'edit)
	 (let ((key (this-command-keys)))
	   (apply 'isearch-unread (listify-key-sequence key)))
	 (isearch-edit-string))
	(search-exit-option
	 (let* ((key (this-command-keys))
		(main-event (aref key 0))
		window)
	   (apply 'isearch-unread (listify-key-sequence key))
	   ;; Properly handle scroll-bar and mode-line clicks
	   ;; for which a dummy prefix event was generated as (aref key 0).
	   (and (> (length key) 1)
		(symbolp (aref key 0))
		(listp (aref key 1))
		(not (numberp (posn-point (event-start (aref key 1)))))
		;; Convert the event back into its raw form,
		;; with the dummy prefix implicit in the mouse event,
		;; so it will get split up once again.
		(progn (setq unread-command-events
			     (cdr unread-command-events))
		       (setq main-event (car unread-command-events))
		       (setcar (cdr (event-start main-event))
			       (car (nth 1 (event-start main-event))))))
	   ;; If we got a mouse click, maybe it was read with the buffer
	   ;; it was clicked on.  If so, that buffer, not the current one,
	   ;; is in isearch mode.  So end the search in that buffer.
	   (if (and (listp main-event)
		    (setq window (posn-window (event-start main-event)))
		    (windowp window))
	       (save-excursion
		 (set-buffer (window-buffer window))
		 (isearch-done))
	     (isearch-done))))
	(t;; otherwise nil
	 (isearch-process-search-string (this-command-keys)
					(this-command-keys)))))

(defun isearch-quote-char ()
  "Quote special characters for incremental search."
  (interactive)
  (isearch-process-search-char (read-quoted-char (isearch-message t))))

(defun isearch-return-char ()
  "Convert return into newline for incremental search.
Obsolete."
  (interactive)
  (isearch-process-search-char ?\n))

(defun isearch-printing-char ()
  "Add this ordinary printing character to the search string and search."
  (interactive)
  (isearch-process-search-char (isearch-last-command-char)))

(defun isearch-whitespace-chars ()
  "Match all whitespace chars, if in regexp mode.
If you want to search for just a space, type C-q SPC."
  (interactive)
  (if isearch-regexp 
      (if (and search-whitespace-regexp (not isearch-within-brackets))
	  (isearch-process-search-string search-whitespace-regexp " ")
	(isearch-printing-char))
    (progn
      ;; This way of doing word search doesn't correctly extend current search.
      ;;      (setq isearch-word t)
      ;;      (setq isearch-adjusted t)
      ;;      (goto-char isearch-barrier)
      (isearch-printing-char))))

(defun isearch-process-search-char (char)
  ;; Append the char to the search string, update the message and re-search.
  (isearch-process-search-string 
   (isearch-char-to-string char) 
   (isearch-text-char-description char)))

(defun isearch-process-search-string (string message)
  (setq isearch-string (concat isearch-string string)
	isearch-message (concat isearch-message message))
  (isearch-search-and-update))


;;===========================================================
;; Search Ring

(defun isearch-ring-adjust1 (advance)
  ;; Helper for isearch-ring-adjust
  (let* ((ring (if isearch-regexp regexp-search-ring search-ring))
	 (length (length ring))
	 (yank-pointer-name (if isearch-regexp
				'regexp-search-ring-yank-pointer
			      'search-ring-yank-pointer))
	 (yank-pointer (eval yank-pointer-name)))
    (if (zerop length)
	()
      (set yank-pointer-name
	   (setq yank-pointer
		 (mod (+ (or yank-pointer 0)
			 (if advance -1 1))
		      length)))
      (setq isearch-string (nth yank-pointer ring)
	    isearch-message (mapconcat 'isearch-text-char-description
				       isearch-string "")))))

(defun isearch-ring-adjust (advance)
  ;; Helper for isearch-ring-advance and isearch-ring-retreat
  (if (cdr isearch-cmds)  ;; is there more than one thing on stack?
      (isearch-pop-state))
  (isearch-ring-adjust1 advance)
  (isearch-push-state)
  (if search-ring-update
      (progn
	(isearch-search)
	(isearch-update))
    (isearch-edit-string)
    ))

(defun isearch-ring-advance ()
  "Advance to the next search string in the ring."
  ;; This could be more general to handle a prefix arg, but who would use it.
  (interactive)
  (isearch-ring-adjust 'advance))

(defun isearch-ring-retreat ()
  "Retreat to the previous search string in the ring."
  (interactive)
  (isearch-ring-adjust nil))

(defun isearch-ring-advance-edit (n)
  "Insert the next element of the search history into the minibuffer."
  (interactive "p")
  (let* ((yank-pointer-name (if isearch-regexp
				'regexp-search-ring-yank-pointer
			      'search-ring-yank-pointer))
	 (yank-pointer (eval yank-pointer-name))
	 (ring (if isearch-regexp regexp-search-ring search-ring))
	 (length (length ring)))
    (if (zerop length)
	()
      (set yank-pointer-name
	   (setq yank-pointer
		 (mod (- (or yank-pointer 0) n)
		      length)))

      (erase-buffer)
      (insert (nth yank-pointer ring))
      (goto-char (point-max)))))

(defun isearch-ring-retreat-edit (n)
  "Inserts the previous element of the search history into the minibuffer."
  (interactive "p")
  (isearch-ring-advance-edit (- n)))

;;(defun isearch-ring-adjust-edit (advance)
;;  "Use the next or previous search string in the ring while in minibuffer."
;;  (isearch-ring-adjust1 advance)
;;  (erase-buffer)
;;  (insert isearch-string))

;;(defun isearch-ring-advance-edit ()
;;  (interactive)
;;  (isearch-ring-adjust-edit 'advance))

;;(defun isearch-ring-retreat-edit ()
;;  "Retreat to the previous search string in the ring while in the minibuffer."
;;  (interactive)
;;  (isearch-ring-adjust-edit nil))


(defun isearch-complete1 ()
  ;; Helper for isearch-complete and isearch-complete-edit
  ;; Return t if completion OK, nil if no completion exists.
  (let* ((ring (if isearch-regexp regexp-search-ring search-ring))
         (alist (mapcar (function (lambda (string) (list string))) ring))
         (completion-ignore-case case-fold-search)
         (completion (try-completion isearch-string alist)))
    (cond
     ((eq completion t)
      ;; isearch-string stays the same
      t)
     ((or completion ; not nil, must be a string
	  (= 0 (length isearch-string))) ; shouldnt have to say this
      (if (equal completion isearch-string)  ;; no extension?
	  (if completion-auto-help
	      (with-output-to-temp-buffer "*Isearch completions*"
		(display-completion-list 
		 (all-completions isearch-string alist))))
	(setq isearch-string completion))
      t)
     (t
      (message "No completion") ; waits a second if in minibuffer
      nil))))

(defun isearch-complete ()
  "Complete the search string from the strings on the search ring.
The completed string is then editable in the minibuffer.
If there is no completion possible, say so and continue searching."
  (interactive)
  (if (isearch-complete1)
      (isearch-edit-string)
    ;; else
    (sit-for 1)
    (isearch-update)))

(defun isearch-complete-edit ()
  "Same as `isearch-complete' except in the minibuffer."
  (interactive)
  (setq isearch-string (buffer-string))
  (if (isearch-complete1)
      (progn
	(erase-buffer)
	(insert isearch-string))))


;;;==============================================================
;; The search status stack (and isearch window-local variables, not used).
;; Need a structure for this.

(defun isearch-top-state ()
  (let ((cmd (car isearch-cmds)))
    (setq isearch-string (car cmd)
	  isearch-message (car (cdr cmd))
	  isearch-success (nth 3 cmd)
	  isearch-forward (nth 4 cmd)
	  isearch-other-end (nth 5 cmd)
	  isearch-word (nth 6 cmd)
	  isearch-invalid-regexp (nth 7 cmd)
	  isearch-wrapped (nth 8 cmd)
	  isearch-barrier (nth 9 cmd)
	  isearch-within-brackets (nth 10 cmd)
	  isearch-case-fold-search (nth 11 cmd))
    (goto-char (car (cdr (cdr cmd))))))

(defun isearch-pop-state ()
  (setq isearch-cmds (cdr isearch-cmds))
  (isearch-top-state)
  )

(defun isearch-push-state ()
  (setq isearch-cmds 
	(cons (list isearch-string isearch-message (point)
		    isearch-success isearch-forward isearch-other-end 
		    isearch-word
		    isearch-invalid-regexp isearch-wrapped isearch-barrier
		    isearch-within-brackets isearch-case-fold-search)
	      isearch-cmds)))


;;;==================================================================
;; Message string

(defun isearch-message (&optional c-q-hack ellipsis)
  ;; Generate and print the message string.
  (let ((cursor-in-echo-area ellipsis)
	(m (concat
	    (isearch-message-prefix c-q-hack ellipsis isearch-nonincremental)
	    isearch-message
	    (isearch-message-suffix c-q-hack ellipsis)
	    )))
    (if c-q-hack
	m
      (let ((message-log-max nil))
	(message "%s" m)))))

(defun isearch-message-prefix (&optional c-q-hack ellipsis nonincremental)
  ;; If about to search, and previous search regexp was invalid,
  ;; check that it still is.  If it is valid now,
  ;; let the message we display while searching say that it is valid.
  (and isearch-invalid-regexp ellipsis
       (condition-case ()
	   (progn (re-search-forward isearch-string (point) t)
		  (setq isearch-invalid-regexp nil
			isearch-within-brackets nil))
	 (error nil)))
  ;; If currently failing, display no ellipsis.
  (or isearch-success (setq ellipsis nil))
  (let ((m (concat (if isearch-success "" "failing ")
		   (if isearch-wrapped "wrapped ")
		   (if isearch-word "word " "")
		   (if isearch-regexp "regexp " "")
		   (if nonincremental "search" "I-search")
		   (if isearch-forward ": " " backward: ")
		   )))
    (aset m 0 (upcase (aref m 0)))
    m))


(defun isearch-message-suffix (&optional c-q-hack ellipsis)
  (concat (if c-q-hack "^Q" "")
	  (if isearch-invalid-regexp
	      (concat " [" isearch-invalid-regexp "]")
	    "")))


;;;========================================================
;;; Searching

(defun isearch-search ()
  ;; Do the search with the current search string.
  (isearch-message nil t)
  (if (and (eq isearch-case-fold-search t) search-upper-case)
      (setq isearch-case-fold-search
	    (isearch-no-upper-case-p isearch-string isearch-regexp)))
  (condition-case lossage
      (let ((inhibit-quit nil)
	    (case-fold-search isearch-case-fold-search))
	(if isearch-regexp (setq isearch-invalid-regexp nil))
	(setq isearch-within-brackets nil)
	(setq isearch-success
	      (funcall
	       (cond (isearch-word
		      (if isearch-forward
			  'word-search-forward 'word-search-backward))
		     (isearch-regexp
		      (if isearch-forward
			  're-search-forward 're-search-backward))
		     (t
		      (if isearch-forward 'search-forward 'search-backward)))
	       isearch-string nil t))
	(if isearch-success
	    (setq isearch-other-end
		  (if isearch-forward (match-beginning 0) (match-end 0)))))

    (quit (isearch-unread ?\C-g)
	  (setq isearch-success nil))

    (invalid-regexp 
     (setq isearch-invalid-regexp (car (cdr lossage)))
     (setq isearch-within-brackets (string-match "\\`Unmatched \\["
						 isearch-invalid-regexp))
     (if (string-match
	  "\\`Premature \\|\\`Unmatched \\|\\`Invalid "
	  isearch-invalid-regexp)
	 (setq isearch-invalid-regexp "incomplete input")))
    (error
     ;; stack overflow in regexp search.
     (setq isearch-invalid-regexp (car (cdr lossage)))))

  (if isearch-success
      nil
    ;; Ding if failed this time after succeeding last time.
    (and (nth 3 (car isearch-cmds))
	 (ding))
    (goto-char (nth 2 (car isearch-cmds)))))



;;;========================================================
;;; Highlighting

(defvar isearch-overlay nil)

(defun isearch-highlight (beg end)
  (if (or (null search-highlight) (null window-system))
      nil
    (or isearch-overlay (setq isearch-overlay (make-overlay beg end)))
    (move-overlay isearch-overlay beg end (current-buffer))
    (overlay-put isearch-overlay 'face
		 (if (internal-find-face 'isearch nil)
		     'isearch 'region))))

(defun isearch-dehighlight (totally)
  (if isearch-overlay
      (delete-overlay isearch-overlay)))

;;;===========================================================
;;; General utilities


(defun isearch-no-upper-case-p (string regexp-flag)
  "Return t if there are no upper case chars in STRING.
If REGEXP-FLAG is non-nil, disregard letters preceeded by `\\' (but not `\\\\')
since they have special meaning in a regexp."
  (let ((case-fold-search nil))
    (not (string-match (if regexp-flag "\\(^\\|\\\\\\\\\\|[^\\]\\)[A-Z]"
			 "[A-Z]")
		       string))))


;;;=================================================
;; Portability functions to support various Emacs versions.

;; To quiet the byte-compiler.
(defvar unread-command-event)
(defvar unread-command-events)
(defvar last-command-event)

(defun isearch-char-to-string (c)
  (make-string 1 c))

(defun isearch-text-char-description (c)
  (if (and (integerp c) (or (< c ?\ ) (= c ?\^?)))
      (text-char-description c)
    (isearch-char-to-string c)))

;; General function to unread characters or events.
(defun isearch-unread (&rest char-or-events)
  (setq unread-command-events
	(append char-or-events unread-command-events)))

(defun isearch-last-command-char ()
  ;; General function to return the last command character.
  last-command-char)

;;; isearch.el ends here
