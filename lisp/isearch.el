;; Incremental search minor mode.
;; Copyright (C) 1992 Free Software Foundation, Inc.

;; LCD Archive Entry:
;; isearch-mode|Daniel LaLiberte|liberte@cs.uiuc.edu
;; |A minor mode replacement for isearch.el.
;; |$Date: 1993/02/17 21:30:25 $|$Revision: 1.19 $|~/modes/isearch-mode.el

;; This file is not yet part of GNU Emacs, but it is based almost
;; entirely on isearch.el which is part of GNU Emacs.

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

;;;====================================================================
;; Instructions

;; Searching with isearch-mode.el should work just like isearch.el,
;; except it is done in a temporary minor mode that terminates when
;; you finish searching.

;; To use isearch-mode instead of the standard isearch.el, add the
;; following to your .emacs file.  The standard key bindings to
;; isearch-forward, etc, will then use isearch-mode instead of
;; isearch.

;; (fset 'isearch 'isearch-mode)
;; (autoload 'isearch-mode "isearch-mode")

;; For programmed use of isearch-mode, e.g. calling (isearch-forward),
;; isearch-mode behaves modally and does not return until the search
;; is completed.  It uses a recursive-edit to behave this way.  Note:
;; gnus does it wrong: (call-interactively 'isearch-forward).

;; If any package you use invokes isearching non-interactively to get
;; the modal behavior described above, you must use the redefinitions
;; of isearch-forward, etc. found in this file instead of those in
;; loaddefs.el.  The simplest way to ensure this is to just load
;; isearch-mode explicitly in your .emacs instead of using the above
;; fset and autoload.

;; (load "isearch-mode")

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
;; - Integrate the emacs 19 generalized commmand history.
;; - Think about incorporating query-replace.
;; - Hooks and options for failed search.

;;;====================================================================
;;; Change History

;;; $Header: /gd/gnu/emacs/19.0/lisp/RCS/isearch-mode.el,v 1.19 1993/02/17 21:30:25 rms Exp rms $
;;; $Log: isearch-mode.el,v $
; Revision 1.19  1993/02/17  21:30:25  rms
; Fix minor bugs in previous change.
;
; Revision 1.18  1993/02/17  20:34:20  rms
; (isearch-backward-regexp):
; New arg no-recursive-edit, always non-nil for interactive call.
; Rename first arg, and set it right in interactive call.
; (isearch-forward-regexp): Likewise.
; (isearch-forward, isearch-backward): Likewise no-recursive-edit.
;
; Revision 1.17  1993/01/26  01:48:27  jimb
; JimB's changes since January 18th
;
; Revision 1.16  1992/11/16  01:37:06  jimb
; 	* bytecomp.el: Declare unread-command-char an obsolete variable.
; 	* vip.el (vip-escape-to-emacs, vip-prefix-arg-value,
; 	vip-prefix-arg-com): Use unread-command-event instead of
; 	unread-command-char; respect its new semantics.
; 	* isearch-mode.el (isearch-update, isearch-unread): Same.
;
; Revision 1.15  1992/11/07  06:17:04  jimb
;         * isearch.el (isearch-frames-exist): This isn't what we want -
; 	replaced by...
; 	(isearch-gnu-emacs-events): non-nil if should expect events in the
; 	style generated by GNU Emacs 19.  Set if set-frame-height is
; 	fboundp; this is true on any GNU Emacs 19, whether or not it was
; 	compiled with multiple frame support.
; 	(isearch-mode-map): Test isearch-gnu-emacs-events instead of
; 	isearch-frames-exist to see if we should bind switch-frame events.
; 	(isearch-update): Test isearch-gnu-emacs-events instead of
; 	isearch-frames-exist to see if unread-command-char's quiescent
; 	value is nil or -1.
;
; Revision 1.14  1992/11/01  22:10:59  rms
; (isearch-search): Handle all sorts of errors from regexp search.
;
; Revision 1.13  1992/10/27  04:11:46  rms
; (isearch-edit-string):
; Bind cursor-in-echo-area only around read-char/allocate-event.
;
; Revision 1.12  1992/10/20  21:21:47  rms
; (isearch-mode-map): Make the top-level keymap dense.
; Explicitly bind control characters at that level.
;
; Revision 1.11  1992/10/11  05:25:11  rms
; (isearch-ring-advance-edit): Delete spurious `)'.
;
; Revision 1.10  1992/09/21  08:28:43  rms
; entered into RCS
;
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



;;;=========================================================================
;;; Emacs features

;; isearch-mode takes advantage of the features provided by several
;; different versions of emacs.  Rather than testing the version of
;; emacs, several constants are defined, one for each of the features.
;; Each of the tests below must work on any version of emacs.
;; (Perhaps provide and featurep could be used for this purpose.)

(defconst isearch-gnu-emacs-events (fboundp 'set-frame-height)) ;; emacs 19
(defconst isearch-pre-command-hook-exists (boundp 'pre-command-hook)) ;; lemacs
(defconst isearch-event-data-type nil)  ;; lemacs

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

;;;========================================================================
;;; Some additional options and constants.

(defvar search-upper-case t
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
  "Whether isearch and query-replace should highlight the text which 
currently matches the search-string.")


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

      ;; Make function keys, etc, exit the search.
      (define-key map [t] 'isearch-other-control-char)
      ;; Control chars, by default, end isearch mode transparently.
      ;; We need these explicit definitions because, in a dense keymap, 
      ;; the binding for t does not affect characters.
      ;; We use a dense keymap to save space.
      (while (< i ?\ )
	(define-key map (make-string 1 i) 'isearch-other-control-char)
	(setq i (1+ i)))

      ;; Printing chars extend the selection by default.
      (setq i ?\ )
      (while (< i 128)
	(define-key map (make-string 1 i) 'isearch-printing-char)
	(setq i (1+ i)))

      ;; Several non-printing chars change the searching behavior.
      (define-key map "\C-s" 'isearch-repeat-forward)
      (define-key map "\C-r" 'isearch-repeat-backward)
      (define-key map "\177" 'isearch-delete-char)
      (define-key map "\C-g" 'isearch-abort)
    
      (define-key map "\C-q" 'isearch-quote-char)

      ;;  (define-key map "\r" 'isearch-return-char)
      ;; For version 19, RET (C-m) terminates search and LFD (C-j) matches eol.
      ;; We could make this conditional.
      (define-key map "\r" 'isearch-exit)
      (define-key map "\C-j" 'isearch-printing-char)
      (define-key map "\t" 'isearch-printing-char)
      (define-key map " " 'isearch-whitespace-chars)
    
      (define-key map "\C-w" 'isearch-yank-word)
      (define-key map "\C-y" 'isearch-yank-line)

      ;; Define keys for regexp chars * ? |.
      ;; Nothing special for + because it matches at least once.
      (define-key map "*" 'isearch-*-char)
      (define-key map "?" 'isearch-*-char)
      (define-key map "|" 'isearch-|-char)

      ;; You can reenable global keys by binding them locally to nil.
      ;; For the help char this doesnt work quite as expected because
      ;; isearch-mode is not a major mode.  Also the echo area is not
      ;; restored after the help command while isearch-mode is
      ;; still active.  Furthermore, we should not assume that the
      ;; help-command is on C-h.  But here is how it would be done:
      ;; (define-key map "\C-h" nil)

      ;; Instead bind C-h to special help command for isearch-mode.
      (define-key map "\C-h" 'isearch-mode-help)

      ;; To handle local bindings with meta char prefix keys, define
      ;; another full keymap.  This must be done for any other prefix
      ;; keys as well, one full keymap per char of the prefix key.  It
      ;; would be simpler to disable the global keymap, and/or have a
      ;; default local key binding for any key not otherwise bound.
      (define-key map (char-to-string meta-prefix-char) (make-sparse-keymap))
      (define-key map (vector meta-prefix-char t) 'isearch-other-meta-char)
;;;      (setq i 0)
;;;      (while (< i 128)
;;;	(define-key map (char-to-string (+ 128 i));; Needs to be generalized.
;;;	  'isearch-other-meta-char)
;;;	(setq i (1+ i)))

      (define-key map "\M-n" 'isearch-ring-advance)
      (define-key map "\M-p" 'isearch-ring-retreat)

      (define-key map "\M-\t" 'isearch-complete)

      ;; For emacs 19, switching frames should terminate isearch-mode
      (if isearch-gnu-emacs-events
	  (define-key map [switch-frame] 'isearch-switch-frame-handler))
      
      (setq isearch-mode-map map)
      ))

;; Some bindings you may want to put in your isearch-mode-hook.
;; Suggest some alternates...
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
(defvar isearch-other-end nil)	; Start (end) of match if forward (backward).
(defvar isearch-wrapped nil)	; Searching restarted from the top (bottom).
(defvar isearch-barrier 0)

(defvar isearch-case-fold-search nil) ; case-fold-search while searching.

(defvar isearch-adjusted nil)
(defvar isearch-slow-terminal-mode nil)
;;; If t, using a small window.
(defvar isearch-small-window nil)
(defvar isearch-opoint 0)
;;; The window configuration active at the beginning of the search.
(defvar isearch-window-configuration nil)
(defvar isearch-old-local-map nil)

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
;;; An alternative is to fset isearch-forward etc to isearch-mode,
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
Type \\[isearch-whitespace-chars] to match all whitespace chars in regexp.
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
	;; Use (baud-rate) for now, for sake of other versions.
	isearch-slow-terminal-mode (and (<= (baud-rate) search-slow-speed)
					(> (window-height)
					   (* 4 search-slow-window-lines)))
	isearch-other-end nil
	isearch-small-window nil

	isearch-opoint (point)
	isearch-window-configuration (current-window-configuration)
	isearch-old-local-map (current-local-map)
	search-ring-yank-pointer nil
	regexp-search-ring-yank-pointer nil)
  (if isearch-pre-command-hook-exists
      (add-hook 'pre-command-hook 'isearch-pre-command-hook))
  (setq	isearch-mode " Isearch")  ;; forward? regexp?
  (set-buffer-modified-p (buffer-modified-p)) ; update modeline

  (isearch-push-state)

  (use-local-map isearch-mode-map)
  (isearch-update)
  (run-hooks 'isearch-mode-hook)

  ;; isearch-mode can be made modal (in the sense of not returning to 
  ;; the calling function until searching is completed) by entering 
  ;; a recursive-edit and exiting it when done isearching.
  (if recursive-edit (recursive-edit))
  )


;;;====================================================
;; Some high level utilities.  Others below.

(defun isearch-update ()
  ;; Called after each command to update the display.  
  (if (if isearch-event-data-type
	  (null unread-command-event)
	(if isearch-gnu-emacs-events
	    (null unread-command-events)
	  (< unread-command-char 0)))
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
	      (isearch-highlight (point) isearch-other-end)))
	))
  (setq ;; quit-flag nil  not for isearch-mode
   isearch-adjusted nil
   isearch-yank-flag nil)
  )


(defun isearch-done (&optional nopush)
  ;; Called by all commands that terminate isearch-mode.
  ;; If NOPUSH is non-nil, we don't push the string on the search ring.
  (use-local-map isearch-old-local-map)
  ;; (setq pre-command-hook isearch-old-pre-command-hook) ; for lemacs
  (isearch-dehighlight t)
  (let ((found-start (window-start (selected-window)))
	(found-point (point)))
    (set-window-configuration isearch-window-configuration)

    ;; If there was movement, mark the starting position.
    ;; Maybe should test difference between and set mark iff > threshold.
    (if (/= (point) isearch-opoint)
	(push-mark isearch-opoint)
      ;; (message "") why is this needed?
      )
    (if isearch-small-window
	(goto-char found-point)
      ;; Exiting the save-window-excursion clobbers window-start; restore it.
      (set-window-start (selected-window) found-start t)))

  (setq isearch-mode nil)
  (set-buffer-modified-p (buffer-modified-p))  ;; update modeline

  (if (and (> (length isearch-string) 0) (not nopush))
      ;; Update the ring data.
      (if isearch-regexp 
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

  (run-hooks 'isearch-mode-end-hook)
  (if isearch-recursive-edit (exit-recursive-edit)))

;;;=======================================================
;;; Switching buffers should first terminate isearch-mode.
;;; This is done quite differently for each varient of emacs.
;;; For lemacs, see Exiting in lemacs below

;; For Emacs 19, the frame switch event is handled.
(defun isearch-switch-frame-handler ()
  (interactive) ;; Is this necessary?
  ;; First terminate isearch-mode.
  (isearch-done)
  (select-frame (car (cdr (isearch-last-command-char)))))

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
\\[isearch-backward-exit-minibuffer] to resume isearching backward.
\\[isearch-ring-advance-edit] to replace the search string with the next item in the search ring.
\\[isearch-ring-retreat-edit] to replace the search string with the previou item in the search ring.
\\[isearch-complete-edit] to complete the search string using the search ring.

If first char entered is \\[isearch-yank-word], then do word search instead."

  ;; This code is very hairy for several reasons, explained in the code.
  ;; Mainly, isearch-mode must be terminated while editing and then restarted.
  ;; If there were a way to catch any change of buffer from the minibuffer,
  ;; this could be simplified greatly.
  ;; Editing doesnt back up the search point.  Should it?
  (interactive)
  (condition-case err
      (let (isearch-nonincremental	; should search nonincrementally?

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
	    (isearch-done t)
	  (exit nil))			; was recursive editing

	(isearch-message) ;; for read-char
	(unwind-protect
	    (let* (;; Why does following read-char echo?  
		   ;;(echo-keystrokes 0) ;; not needed with above message
		   (e (let ((cursor-in-echo-area t))
			(if isearch-event-data-type
			    (allocate-event) (read-char))))
		   ;; Binding minibuffer-history-symbol to nil is a work-around
		   ;; for some incompatibility with gmhist.
		   (minibuffer-history-symbol))
	      ;; If the first character the user types when we prompt them
	      ;; for a string is the yank-word character, then go into
	      ;; word-search mode.  Otherwise unread that character and
	      ;; read a key the normal way.
	      ;; Word search does not apply (yet) to regexp searches,
	      ;; no check is made here.
	      (message (isearch-message-prefix nil nil t))
	      (if (eq 'isearch-yank-word
		      (lookup-key
		       isearch-mode-map
		       (char-to-string
			(if isearch-event-data-type
			    (or (event-to-character (next-command-event e)) 0)
			  e))))
		  (setq isearch-word t  ;; so message-prefix is right
			isearch-new-word t)
		(isearch-unread e))
	      (setq cursor-in-echo-area nil)
	      (setq isearch-new-string
		    (let (junk-ring)
		      (read-from-minibuffer (isearch-message-prefix)
					    isearch-string
					    minibuffer-local-isearch-map nil
					    'junk-ring))
		    isearch-new-message (mapconcat 'text-char-description
						   isearch-new-string "")))
	  ;; Always resume isearching by restarting it.
	  (isearch-mode isearch-forward 
			isearch-regexp 
			isearch-op-fun 
			isearch-recursive-edit
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


(defun isearch-abort ()
  "Abort incremental search mode if searching is successful, signalling quit.
Otherwise, revert to previous successful search and continue searching.
Use `isearch-exit' to quit without signalling."
  (interactive)
;;  (ding)  signal instead below, if quiting
  (discard-input)
  (if isearch-success
      ;; If search is successful, move back to starting point
      ;; and really do quit.
      (progn (goto-char isearch-opoint)
	     (isearch-done t)   ; exit isearch
	     (signal 'quit nil))  ; and pass on quit signal
    ;; If search is failing, rub out until it is once more successful.
    (while (not isearch-success) (isearch-pop-state))
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
  (setq isearch-success t)
  (or (equal isearch-string "")
      (progn
	;; If repeating a search that found
	;; an empty string, ensure we advance.
	(if (equal (match-end 0) (match-beginning 0))
	    (forward-char (if isearch-forward 1 -1)))
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
  (let ((string (save-excursion
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
		     (point))))))
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


(fset 'isearch-other-control-char 'isearch-other-meta-char)

(defun isearch-other-meta-char ()
  "Exit the search normally and reread this key sequence.
But only if `search-exit-option' is non-nil, the default.
If it is the symbol `edit', the search string is edited in the minibuffer
and the meta character is unread so that it applies to editing the string."
  (interactive)
  (cond ((eq search-exit-option 'edit)
	 (let ((key (this-command-keys)))
	   (apply 'isearch-unread (append key nil)))
	 (isearch-edit-string))
	(search-exit-option
	 (let ((key (this-command-keys)))
	   (apply 'isearch-unread (append key nil)))
	 (isearch-done))
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
  "Any other printing character => add it to the search string and search."
  (interactive)
  (isearch-process-search-char (isearch-last-command-char)))

(defun isearch-whitespace-chars ()
  "Match all whitespace chars, if in regexp mode.
If not in regexp mode, activate word search."
  (interactive)
  (if isearch-regexp 
      (if search-whitespace-regexp
	  (isearch-process-search-string search-whitespace-regexp " ")
	(isearch-printing-char))
    (progn
      ;; This way of doing word search doesnt correctly extend current search.
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
		 (% (+ (or yank-pointer 0)
		       (if advance (1- length) 1))
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
		 (% (+ (or yank-pointer 0)
		       ;; Add LENGTH here to ensure a positive result.
		       length
		       (% (- n) length))
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
	  isearch-barrier (nth 9 cmd))
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
		    isearch-invalid-regexp isearch-wrapped isearch-barrier)
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
    (if c-q-hack m (message "%s" m))))

(defun isearch-message-prefix (&optional c-q-hack ellipsis nonincremental)
  ;; If about to search, and previous search regexp was invalid,
  ;; check that it still is.  If it is valid now,
  ;; let the message we display while searching say that it is valid.
  (and isearch-invalid-regexp ellipsis
       (condition-case ()
	   (progn (re-search-forward isearch-string (point) t)
		  (setq isearch-invalid-regexp nil))
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
  (if search-upper-case
      (setq isearch-case-fold-search (isearch-no-upper-case-p isearch-string)))
  (condition-case lossage
      (let ((inhibit-quit nil)
	    (case-fold-search isearch-case-fold-search))
	(if isearch-regexp (setq isearch-invalid-regexp nil))
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

(defun isearch-highlight (begin end))
(defun isearch-dehighlight (totally))

;; lemacs uses faces
'(progn
(defvar isearch-extent nil)

(or (find-face 'isearch)	;; this face is initialized by x-faces.el
    (make-face 'isearch))	;; since isearch is preloaded

(defun isearch-lemacs-highlight (begin end)
  (if (null isearch-highlight)
      nil
    (if (and (extentp isearch-extent)
	     (eq (extent-buffer isearch-extent) (current-buffer)))
	(set-extent-endpoints isearch-extent begin end)
      (if (and (extentp isearch-extent)
	       (bufferp (extent-buffer isearch-extent))
	       (buffer-name (extent-buffer isearch-extent)))
	  (delete-extent isearch-extent))
      (setq isearch-extent (make-extent begin end (current-buffer))))
    (set-extent-face isearch-extent 'isearch)))

(defun isearch-lemacs-dehighlight (totally)
  (if (and isearch-highlight isearch-extent)
      (if totally
	  (let ((inhibit-quit t))
	    (if (and (extentp isearch-extent)
		     (bufferp (extent-buffer isearch-extent))
		     (buffer-name (extent-buffer isearch-extent)))
		(delete-extent isearch-extent))
	    (setq isearch-extent nil))
	(if (and (extentp isearch-extent)
		 (bufferp (extent-buffer isearch-extent))
		 (buffer-name (extent-buffer isearch-extent)))
	    (set-extent-face isearch-extent 'default)
	  (isearch-dehighlight t)))))

(fset 'isearch-highlight (symbol-function 'isearch-lemacs-highlight))
(fset 'isearch-dehighlight (symbol-function 'isearch-lemacs-dehighlight))
)

;;;===========================================================
;;; General utilities

;; (fset 'isearch-member-equal (symbol-function 'member)) ; for emacs 19

(defun isearch-member-equal (item list)
  "Return non-nil if ITEM is `equal' to some item in LIST.
Actually return the list whose car is that item."
  (while (and list (not (equal item (car list))))
    (setq list (cdr list)))
  list)


(defun isearch-no-upper-case-p (string)
  "Return t if there are no upper case chars in string.
But upper case chars preceeded by \\ (but not \\\\) do not count since they
have special meaning in a regexp."
  (let ((case-fold-search nil))
    (not (string-match "\\(^\\|\\\\\\\\\\|[^\\]\\)[A-Z]" string))))


;;;=================================================
;;; Special functions for lemacs events.

;; To quiet the byte-compiler.
(defvar unread-command-event)
(defvar unread-command-events)
(defvar last-command-event)

(defun isearch-char-to-string (c)
  (if (integerp c)
      (make-string 1 c)
   (make-string 1 (event-to-character c))))

(defun isearch-text-char-description (c)
  (isearch-char-to-string c))

(defun isearch-unread (&rest char-or-events)
  (setq foo char-or-events)
  ;; General function to unread characters or events.
  (if isearch-gnu-emacs-events
      (setq unread-command-events (listify-key-sequence char-or-events))
    (let ((char (if (cdr char-or-events)
		    (+ 128 (car (last char-or-events)))
		  (car char-or-events))))
      (if isearch-event-data-type
	  (setq unread-command-event char)
	(setq unread-command-char char)))))

(defun isearch-last-command-char ()
  ;; General function to return the last command character.
  (if isearch-event-data-type
      last-command-event
    last-command-char))




;;;========================================================
;;; Exiting in lemacs

;; This is a large amount of code to support automatic termination of
;; isearch-mode when a command (however it is invoked) is not an
;; isearch command, or the buffer is switched out from under
;; isearch-mode.   Only later versions of lemacs have the pre-command-hook.

;;(if isearch-pre-command-hook-exists
;;(progn

;;;; This list must be modified whenever the available commands are modified.
;;(mapcar (function (lambda (command)
;;		    (put command 'isearch-command t)))
;;	'(isearch-printing-char
;;	  isearch-return-char
;;	  isearch-repeat-forward
;;	  isearch-repeat-backward
;;	  isearch-delete-char
;;	  isearch-abort	
;;	  isearch-quote-char
;;	  isearch-exit	
;;	  isearch-printing-char
;;	  isearch-printing-char
;;	  isearch-yank-word	
;;	  isearch-yank-line	
;;	  isearch-*-char	
;;	  isearch-*-char	
;;	  isearch-|-char	
;;	  isearch-toggle-regexp
;;	  isearch-edit-string
;;	  isearch-mode-help	
;;	  isearch-ring-advance
;;	  isearch-ring-retreat
;;	  isearch-ring-advance-edit
;;	  isearch-ring-retreat-edit
;;	  isearch-whitespace-chars
;;	  isearch-complete	
;;	  isearch-complete-edit
;;	  isearch-edit-string
;;	  isearch-toggle-regexp
;;	  ;; The following may not be needed since isearch-mode is off already.
;;	  isearch-forward-exit-minibuffer
;;	  isearch-reverse-exit-minibuffer
;;	  isearch-nonincremental-exit-minibuffer))

;;(defun isearch-pre-command-hook ()
;;  ;;
;;  ;; For use as the value of `pre-command-hook' when isearch-mode is active.
;;  ;; If the command about to be executed is not one of the isearch commands,
;;  ;; then isearch-mode is turned off before that command is executed.
;;  ;;
;;  ;; If the command about to be executed is self-insert-command, or is a
;;  ;; keyboard macro of a single key sequence which is bound to self-insert-
;;  ;; command, then we add those chars to the search ring instead of inserting
;;  ;; them in the buffer.  In this way, the set of self-searching characters
;;  ;; need not be exhaustively enumerated, but is derived from other maps.
;;  ;;
;;  (isearch-maybe-frob-keyboard-macros)
;;  (if (and (symbolp this-command)
;;	   (get this-command 'isearch-command))
;;      nil
;;    (isearch-done)))

;;(defun isearch-maybe-frob-keyboard-macros ()
;;  ;;
;;  ;; If the command about to be executed is `self-insert-command' then change
;;  ;; the command to `isearch-printing-char' instead, meaning add the last-
;;  ;; typed character to the search string.
;;  ;;
;;  ;; If `this-command' is a string or a vector (that is, a keyboard macro)
;;  ;; and it contains only one command, which is bound to self-insert-command,
;;  ;; then do the same thing as for self-inserting commands: arrange for that
;;  ;; character to be added to the search string.  If we didn't do this, then
;;  ;; typing a compose sequence (a la x-compose.el) would terminate the search
;;  ;; and insert the character, instead of searching for that character.
;;  ;;
;;  (cond ((eq this-command 'self-insert-command)
;;	 (setq this-command 'isearch-printing-char))
;;	((and (stringp this-command)
;;	      (eq (key-binding this-command) 'self-insert-command))
;;	 (setq last-command-char (aref this-command 0)
;;	       last-command-event (character-to-event last-command-char)
;;	       this-command 'isearch-printing-char))
;;	((and (vectorp this-command)
;;	      (eq (key-binding this-command) 'self-insert-command))
;;	 (let* ((desc (aref this-command 0))
;;		(code (cond ((integerp desc) desc)
;;			    ((symbolp desc) (get desc character-set-property))
;;			    ((consp desc)
;;			     (and (null (cdr desc))
;;				  (get (car desc) character-set-property)))
;;			    (t nil))))
;;	   (if code
;;	       (setq last-command-char code
;;		     last-command-event (character-to-event last-command-char)
;;		     this-command 'isearch-printing-char))))
;;	))

;;))

