;; Incremental search minor mode.
;; Copyright (C) 1992 Free Software Foundation, Inc.

;; LCD Archive Entry:
;; isearch-mode|Daniel LaLiberte|liberte@cs.uiuc.edu
;; |A minor mode replacement for isearch.el.
;; |$Date: 92/05/27 11:33:57 $|$Revision: 1.2 $|~/modes/isearch-mode.el

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

;;;====================================================================
;;; Change History

;;; $Header: /import/kaplan/kaplan/liberte/Isearch/RCS/isearch-mode.el,v 1.2 92/05/27 11:33:57 liberte Exp Locker: liberte $
;;; $Log:	isearch-mode.el,v $
;;; Revision 1.2  92/05/27  11:33:57  liberte
;;; Several new commands and features have been added.  Emacs version
;;; 19 has a search ring, which is supported here.  Other fixes found
;;; in the version 19 isearch are included here.  Also see variables
;;; search-caps-disable-folding, search-nonincremental-instead,
;;; search-whitespace-regexp, and commands isearch-toggle-regexp,
;;; isearch-edit-string,
;;; 
;;; Semi-modal searching is supported, using a recursive edit. If
;;; isearching is started non-interactively by calling one of the
;;; isearch commands (e.g. isearch-forward), it does not return
;;; until the search is completed.  You should still be able switch
;;; buffers, so be careful not to get things confused.
;;; 

;;; Changes for 1.1
;;; 3/18/92 Fixed invalid-regexp.
;;; 3/18/92 Fixed yanking in regexps.

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

;; The key bindings active within isearch-mode are defined below in
;; `isearch-mode-map' and `isearch-mode-meta-map' which are given
;; bindings close to the default characters of isearch.el for
;; version 19.  With `isearch-mode', however, you can bind
;; multi-character keys and it should be easier to add new commands.

;; Note to epoch and emacs version 19 users: isearch-mode should
;; work even if you switch windows with the mouse.  However, if
;; you isearch in a buffer that is also displayed in another window,
;; when you switch to that other window you will still be in
;; isearch mode but not necessarily in the right state for it to work.
;; So ... don't do it unless you are in an experimental mood.
;; You can also experiment with the window-local-variable routines
;; contained in this package but not yet used.
;; Also, I am not sure what happens when you return to an isearching
;; buffer; ideally, the echo area should redisplay the searching status.
;; A select-window-hook might be useful.

;;;=========================================================================
;;; The following, defined in loaddefs.el, are still used with isearch-mode.

;(defvar search-last-string ""
;  "Last string search for by a search command.
;This does not include direct calls to the primitive search functions,
;and does not include searches that are aborted.")

;(defvar search-last-regexp ""
;  "Last string searched for by a regexp search command.
;This does not include direct calls to the primitive search functions,
;and does not include searches that are aborted.")

;(defconst search-exit-option t
;  "Non-nil means random control characters terminate incremental search.")

;(defvar search-slow-window-lines 1
;  "*Number of lines in slow search display windows.")

;(defconst search-slow-speed 1200
;  "*Highest terminal speed at which to use \"slow\" style incremental search.
;This is the style where a one-line window is created to show the line
;that the search has reached.")

;;;========================================================================
;;; Some additional options and constants.

(defvar search-caps-disable-folding t
  "*If non-nil, upper case chars disable case fold searching.
This does not yet apply to yanked strings, however.")

(defvar search-nonincremental-instead t
  "*If non-nil, do a nonincremental search instead if exiting immediately.
The default value of t reflects the default behavior of old  
isearch.")

(defconst search-whitespace-regexp "\\s-+"
  "*If non-nil, regular expression to match a sequence of whitespace chars.
You might want to use something like \"[ \\t\\r\\n]+\" instead.")

;;;==================================================================
;;; Search ring.
;;; "regex" == "regexp".  One should become the standard term.

(defvar search-ring nil
  "List of search string sequences.")
(defvar regex-search-ring nil   ;; Is `regex' the new spelling?
  "List of regular expression search string sequences.")

(defconst search-ring-max 16
  "*Maximum length of search ring before oldest elements are thrown away.")
(defconst regex-search-ring-max 16
  "*Maximum length of regex search ring before oldest elements are thrown away.")

(defvar search-ring-yank-pointer nil
  "The tail of the search ring whose car is the last thing searched for.")
(defvar regex-search-ring-yank-pointer nil
  "The tail of the regular expression search ring whose car is the last
thing searched for.")

;;;====================================================
;;; Define isearch-mode keymap.

(defvar isearch-mode-map nil
  "Keymap for isearch-mode.")

(defvar isearch-mode-meta-map nil
  "Keymap for isearch-mode for keys with meta char prefix.")


;; To handle meta char prefix keys, define another full keymap.
;; The same must be done for any other prefix keys.
;; It would be simpler to disable to global keymap, and/or
;; have a default local key binding for any key not otherwise bound.
(if isearch-mode-meta-map
    nil
  (setq isearch-mode-meta-map 	
	(list 'keymap (make-vector 128 'isearch-other-meta-char)))
  (define-key isearch-mode-meta-map "n" 'isearch-ring-advance)
  (define-key isearch-mode-meta-map "p" 'isearch-ring-retreat)
  (define-key isearch-mode-meta-map " " 'isearch-whitespace-chars)  
;for regexps

;;  (define-key isearch-mode-meta-map "?" nil) ; my help key is M-?
  )

(if isearch-mode-map
    nil
  (let ((i 0)

	;; Printing chars extend the selection by default.
	(array (make-vector 128 'isearch-printing-char)))

    ;; Non-printing chars by default suspend isearch mode transparently
    (while (< i ?\ )
      (aset array i 'isearch-other-control-char)
      (setq i (1+ i)))

    (setq i ?A)
    (while (<= i ?Z)
      (aset array i 'isearch-upper-case-char)
      (setq i (1+ i)))

    (setq isearch-mode-map (list 'keymap array))

    ;; You can reenable global keys by unbinding them locally.  

    ;; For the help char this doesnt work quite as expected because
    ;; isearch-mode is not a major mode, and the echo area is not
    ;; restored after the help command.
    ;; Also, we should not assume that the help-command is on C-h.
    ;; If it is not, and the prefix is not the meta-char, we would
    ;; have to create another map for its prefix.
;    (define-key isearch-mode-map "\C-h" nil)

    ;; Several non-printing chars change the searching behavior.
    (define-key isearch-mode-map "\C-s" 'isearch-repeat-forward)
    (define-key isearch-mode-map "\C-r" 'isearch-repeat-backward)
    (define-key isearch-mode-map "\177" 'isearch-delete-char)
    (define-key isearch-mode-map "\C-g" 'isearch-quit)
    

    (define-key isearch-mode-map "\C-q" 'isearch-quote-char)

    ;;  (define-key isearch-mode-map "\r" 'isearch-return-char)
    ;; For version 19, CR (C-m) terminates search and LFD (C-j) matches eol.
    (define-key isearch-mode-map "\r" 'isearch-exit)
    (define-key isearch-mode-map "\C-j" 'isearch-printing-char)
    

    (define-key isearch-mode-map "\C-w" 'isearch-yank-word)
    (define-key isearch-mode-map "\C-y" 'isearch-yank-line)

    (define-key isearch-mode-map "\C-t" 'isearch-toggle-regexp)
    (define-key isearch-mode-map "\C-^" 'isearch-edit-string)

    ;; define keys for regexp chars * ? |
    (define-key isearch-mode-map "*" 'isearch-*-char)
    (define-key isearch-mode-map "?" 'isearch-*-char)
    (define-key isearch-mode-map "|" 'isearch-|-char)

    ;; Assumes meta-prefix-char is \e.
    ;; isearch-mode-meta-map must be a keymap before this.
    (define-key isearch-mode-map "\e" isearch-mode-meta-map)
    ))

;;;========================================================
;; Internal variables declared globally for byte-compiler.
;; These are all made buffer-local during searching.

(defvar isearch-cmds nil
  "Stack of search status sets.")
(defvar isearch-string ""
  "The current search string.")
(defvar isearch-message ""
  "The text-char-description version of isearch-string")
(defvar isearch-success t)
(defvar isearch-forward nil)
(defvar isearch-other-end nil	
  "Start of last match if forward, end if backward.")
(defvar isearch-invalid-regexp nil)
(defvar isearch-wrapped nil)
(defvar isearch-barrier 0)

(defvar isearch-regexp nil)
(defvar isearch-case-fold-search nil
  "Value of case-fold-search while actually searching.")

(defvar isearch-adjusted nil)
(defvar isearch-slow-terminal-mode nil)
(defvar isearch-small-window nil
  "If t, using a small window.")
(defvar isearch-found-point nil
  "to restore point from a small window.")

(defvar isearch-found-start nil
  "This is the window-start value found by the search.")
(defvar isearch-opoint 0)
(defvar isearch-window-configuration nil
  "The window configuration active at the beginning of the search.")
(defvar isearch-old-local-map [])

(defvar isearch-yank-flag nil
  "Flag to indicate a yank occurred, so don't move the cursor.")

(defvar isearch-op-fun nil
  "A function to be called after each input character is processed.
(It is not called after characters that exit the search.)
It is only set from an optional argument to `isearch-mode'.")

;; This is a global variable to avoid byte-compile warnings.
(defvar isearch-last-command-char -1
  "Last command char.")

(defvar isearch-mode-hook nil
  "List of functions to call after starting up an incremental search.
See `isearch-modal' for an example.
Set with `(setq isearch-mode-hook (cons 'myhook isearch-mode-hook))
where myhook can be a function name or lambda expression.")

(defvar isearch-mode-end-hook nil
  "List of functions to call after terminating an incremental search.
See `isearch-mode-hook' for more details.")

;;;==============================================================
;; Minor-mode-alist changes - kind of redundant with the
;; echo area, but if isearching in multiple windows, it can be useful.

(or (assq 'isearch-mode minor-mode-alist)
    (nconc minor-mode-alist
	   (list '(isearch-mode isearch-mode))))

(defvar isearch-mode nil)
(make-variable-buffer-local 'isearch-mode)

;;;===============================================================
;;; Entry points to isearch-mode.
;;; These four functions should be moved to loaddefs.el

(defun isearch-forward ()
  "\
Do incremental search forward.
As you type characters, they add to the search string and are found.

\\<isearch-mode-map>
Type \\[isearch-delete-char] to cancel characters from end of search  
string.
Type \\[isearch-exit] to exit, leaving point at location found.
Type \\[isearch-repeat-forward] to search again forward,  
\\[isearch-repeat-backward] to search again backward.
Type \\[isearch-toggle-regexp] to toggle regular expression with normal searching.
Type \\[isearch-yank-word] to yank word from buffer onto end of  
search string and search for it.
Type \\[isearch-yank-line] to yank rest of line onto end of search string, etc.
Type \\[isearch-quote-char] to quote control character to search for it.
Type C-j to match end of line.

Also supported is a search ring of the previous 16 search strings.
Type \\[isearch-ring-advance] to search for the next item in the search ring.
Type \\[isearch-ring-retreat] to search for the previous item in the search ring.

Other control and meta characters terminate the search
 and are then executed normally.

\\[isearch-quit] while searching or when search has failed
 cancels input back to what has been found successfully.
\\[isearch-quit] when search is successful aborts and moves point to starting point.

All of these keys are bound in `isearch-mode-map' and
`isearch-mode-meta-map'.  If `isearch-forward' is called
non-interactively, it does not return to the calling function until
the search is done."
  (interactive)
  (if (interactive-p)
      (isearch-mode t)
    (isearch-modal t)))

(defun isearch-forward-regexp ()
  "\
Do incremental search forward for regular expression.
Like ordinary incremental search except that your input
is treated as a regexp.  See \\[isearch-forward] for more info."
  (interactive)
  (if (interactive-p)
      (isearch-mode t t)
    (isearch-modal t t)))

(defun isearch-backward ()
  "\
Do incremental search backward.
See \\[isearch-forward] for more information."
  (interactive)
  (if (interactive-p)
      (isearch-mode nil)
    (isearch-modal nil)))

(defun isearch-backward-regexp ()
  "\
Do incremental search backward for regular expression.
Like ordinary incremental search except that your input
is treated as a regexp.  See \\[isearch-forward] for more info."
  (interactive)
  (if (interactive-p)
      (isearch-mode nil t)
    (isearch-modal nil t)))


(defun isearch-modal (forward &optional regexp op-fun)
  ;; As an example of using the hooks, isearch-mode can be made
  ;; modal (in the sense of not returning to the calling function
  ;; until searching is completed) by entering a recursive-edit.
  ;; This is used if the above functions are called non-interactively.
  (let ((isearch-mode-hook 
	 (cons (function (lambda () (recursive-edit)))
	       isearch-mode-hook))
	(isearch-mode-end-hook
	 (cons (function (lambda () (exit-recursive-edit)))
	       isearch-mode-end-hook)))
    (isearch-mode forward regexp op-fun)))


;;;==================================================================
;; isearch-mode only sets up incremental search for the minor mode.
;; All the work is done by the isearch-mode commands.

(defun isearch-mode (forward &optional regexp op-fun)
  "Start isearch minor mode.  Called by isearch-forward, etc."
  ;; Make buffer-local variables for isearching.
  ;; We really need window-local variables.
  (mapcar 
   'make-local-variable
   '(isearch-forward 
     isearch-regexp isearch-string isearch-message
     isearch-case-fold-search
     isearch-cmds isearch-success isearch-wrapped
     isearch-barrier isearch-adjusted isearch-invalid-regexp
     isearch-slow-terminal-mode isearch-other-end isearch-small-window
     isearch-found-point isearch-found-start isearch-opoint 
     isearch-window-configuration isearch-old-local-map))

  ;; Initialize global vars.
  (setq isearch-forward forward
	isearch-regexp regexp
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
	isearch-slow-terminal-mode (and (<= (baud-rate) search-slow-speed)
					(> (window-height)
					   (* 4 search-slow-window-lines)))
	isearch-other-end nil
	isearch-small-window nil
	isearch-found-point nil

	isearch-found-start nil
	isearch-opoint (point)
	isearch-window-configuration (current-window-configuration)
	isearch-old-local-map (current-local-map)

;;	inhibit-quit t
	)
  (setq	isearch-mode " Isearch")  ;; forward? regexp?
  (set-buffer-modified-p (buffer-modified-p)) ; update modeline

  (isearch-push-state)

  (use-local-map isearch-mode-map)
  (isearch-update)
  (run-hooks 'isearch-mode-hook)
  )


;;;====================================================
;; Some high level utilities.  Others below.

(defun isearch-update ()
  ;; Called after each command to update the display.  
  (or unread-command-char
      (progn
	(if (not (input-pending-p))
	    (isearch-message))
	(if (and isearch-slow-terminal-mode
		 (not (or isearch-small-window 
			  (pos-visible-in-window-p))))
	    (progn
	      (setq isearch-small-window t)
	      (setq isearch-found-point (point))
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
	      (goto-char isearch-found-point)))))
  (setq ;; quit-flag nil  not for isearch-mode
   isearch-adjusted nil
   isearch-yank-flag nil)
  )


(defun isearch-done ()
  ;; Called by all commands that terminate isearch-mode.
  (use-local-map isearch-old-local-map)
  (setq isearch-found-start (window-start (selected-window)))
  (setq isearch-found-point (point))
  (set-window-configuration isearch-window-configuration)

  (if (> (length isearch-string) 0)
      ;; Update the ring data.
      (if isearch-regexp 
	  (if (not (setq regex-search-ring-yank-pointer
			 (memq isearch-string regex-search-ring)))
	      (progn
		(setq regex-search-ring (cons isearch-string regex-search-ring)
		      regex-search-ring-yank-pointer regex-search-ring)
		(if (> (length regex-search-ring) regex-search-ring-max)
		    (setcdr (nthcdr (1- search-ring-max) regex-search-ring)
			    nil))))
	(if (not (setq search-ring-yank-pointer
		       (memq isearch-string search-ring)))
	    (progn
	      (setq search-ring (cons isearch-string search-ring)
		    search-ring-yank-pointer search-ring)
	      (if (> (length search-ring) search-ring-max)
		  (setcdr (nthcdr (1- search-ring-max) search-ring) nil))))))

  ;; If there was movement, mark the starting position.
  ;; Maybe should test difference between and set mark iff > threshold.
  (if (/= (point) isearch-opoint)
      (push-mark isearch-opoint)
    (message ""))
  (if isearch-small-window
      (goto-char isearch-found-point)
    ;; Exiting the save-window-excursion clobbers window-start; restore it.
    (set-window-start (selected-window) isearch-found-start t))

  ;; Kill buffer-local variables for isearching
  (mapcar 
   'kill-local-variable
   '(isearch-forward 
     isearch-regexp isearch-string isearch-message
     isearch-case-fold-search
     isearch-cmds isearch-success isearch-wrapped
     isearch-barrier isearch-adjusted isearch-invalid-regexp
     isearch-slow-terminal-mode isearch-other-end isearch-small-window
     isearch-found-point isearch-found-start isearch-opoint 
     isearch-window-configuration isearch-old-local-map))

  (setq isearch-mode nil)
  (set-buffer-modified-p (buffer-modified-p))
  (run-hooks 'isearch-mode-end-hook)
  )


;;;====================================================
;; Commands active while inside of the isearch minor mode.

(defun isearch-exit ()
  "Exit search normally.
However, if this is the first command after starting incremental
search and `search-nonincremental-instead' is non-nil, do a
nonincremental search instead."

  (interactive)
  (if (and search-nonincremental-instead 
	   (= 0 (length isearch-string)))
      (nonincremental-search isearch-forward isearch-regexp))
  (isearch-done))


(defun isearch-edit-string ()
  "Edit the search string in the minibuffer and return to incremental search."
  ;; This doesnt back up the search point.
  (interactive)
  (setq isearch-string (read-string (isearch-message-prefix) isearch-string)
	isearch-message (mapconcat 'text-char-description
				   isearch-string ""))
  (isearch-push-state)
  (isearch-search)
  (isearch-update))


(defun isearch-quit ()
  "Quit incremental search mode if searching is successful.
Otherwise, revert to previous successful search and continue searching."
  (interactive)
  (ding)
  (discard-input)
  (if isearch-success
      ;; If search is successful, move back to starting point
      ;; and really do quit.
      (progn (goto-char isearch-opoint)
	     (isearch-done))  ; exit and quit
    ;; If search is failing, rub out until it is once more
    ;;  successful.
    (while (not isearch-success) (isearch-pop-state))
    (isearch-update)))


(defun isearch-repeat (direction)
  ;; Utility for isearch-repeat-forward and -backward.
  (if (eq isearch-forward (eq direction 'forward))
      ;; C-s in forward or C-r in reverse.
      (if (equal isearch-string "")
	  ;; If search string is empty, use last one.
	  (setq isearch-string
;;		(if isearch-regexp
;;		    search-last-regexp search-last-string)
		(or (if isearch-regexp
			(if regex-search-ring-yank-pointer
			    (car regex-search-ring-yank-pointer)
			  (car regex-search-ring))
		      (if search-ring-yank-pointer
			  (car search-ring-yank-pointer)
			(car search-ring)))
		    "")
		isearch-message
		(mapconcat 'text-char-description
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
  (let ((word (save-excursion
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
    (if isearch-regexp (setq word (regexp-quote word)))
    (setq isearch-string (concat isearch-string word)
	  isearch-message
	  (concat isearch-message
		  (mapconcat 'text-char-description
			     word ""))
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
;; They can make a regexp match sooner
;; or make it succeed instead of failing.
;; So go back to place last successful search started
;; or to the last ^S/^R (barrier), whichever is nearer.

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
  (isearch-process-search-char last-command-char))
  


(defun isearch-|-char ()
  "If in regexp search, jump to the barrier."
  (interactive)
  (if isearch-regexp
      (progn
	(setq isearch-adjusted t)
	(goto-char isearch-barrier)))
  (isearch-process-search-char last-command-char))



(defun isearch-other-control-char ()
  "Any other control char => unread it and exit the search normally.
But only if `search-exit-option' is non-nil."
  (interactive)
  (if search-exit-option
      (progn
	(setq unread-command-char last-command-char)
	(isearch-done))
    ;; otherwise
    (isearch-search-and-update)))


(defun isearch-other-meta-char ()
  "Any other meta char => exit the search normally and reexecute the whole key.
But only if `search-exit-option' is non-nil."
  ;; This will probably work in place of isearch-other-control-char too,
  ;; but here we use unwind-protect and command-execute since it is
  ;; a multi-char key we would want to unread.
  (interactive)
  (if search-exit-option
      (unwind-protect
	  (isearch-done)  ;; this exits recursive edit
	;; Reexecute the key.
	(command-execute (this-command-keys)))
    ;; otherwise
    (isearch-search-and-update)))


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
  (isearch-process-search-char last-command-char))


(defun isearch-upper-case-char ()
  "Any upper case char => turn off case fold search for remainder of search."
  ;; This feature only applies to interactively entered chars,
  ;; but not yanked chars, repeat default searches, or search ring searches.
  ;; Support for these should be easy to add.
  (interactive)
  (if search-caps-disable-folding
      (setq isearch-case-fold-search nil))
  (isearch-printing-char))

(defun isearch-whitespace-chars ()
  "Match all whitespace chars, if in regexp mode."
  (interactive)
  (if (and isearch-regexp search-whitespace-regexp)
      (isearch-process-search-string search-whitespace-regexp " ")
    (isearch-other-meta-char)))

(defun isearch-process-search-char (char)
  ;; Append the char to the search string, update the message and re-search.
  (isearch-process-search-string (char-to-string char) 

				 (text-char-description char)))

(defun isearch-process-search-string (string message)
  (setq isearch-string (concat isearch-string string)
	isearch-message (concat isearch-message message))
  (isearch-search-and-update))


;;===========================================================
;; Search Ring

(defun isearch-ring-adjust (advance)
  ;; helper for isearch-ring-advance and isearch-ring-retreat
  (if (cdr isearch-cmds)
      (isearch-pop-state))
  (let* ((ring (if isearch-regexp regex-search-ring search-ring))
	 (length (length ring))
	 (yank-pointer-name (if isearch-regexp
				'regex-search-ring-yank-pointer
			      'search-ring-yank-pointer))
	 (yank-pointer (eval yank-pointer-name)))
    (if (zerop length)
	()
      (set yank-pointer-name
	   (setq yank-pointer
		 (nthcdr (% (+ (- length (length yank-pointer))
			       (if advance (1- length) 1))
			    length) ring)))
      (setq isearch-string (car yank-pointer)
	    isearch-message (mapconcat 'text-char-description
				       isearch-string ""))))
  (isearch-push-state)
  (isearch-search)
  (isearch-update))

(defun isearch-ring-advance ()
  "Advance to the next search string in the ring."
  (interactive)
  (isearch-ring-adjust 'advance))

(defun isearch-ring-retreat ()
  "Retreat to the previous search string in the ring."
  (interactive)
  (isearch-ring-adjust nil))


;;;=============================================================
;; Window-local variables
;; (not used yet - and maybe never)

(defvar window-local-variable-alist nil
  "An alist of windows associated with window local variables and values.
The cdr of each item is another alist of variables and values.")

(defvar last-local-window nil)
(defvar last-window-local-vars nil)

(defun kill-window-local-variables ()
  "Remove the old variable list, if any."
  (setq window-local-variable-alist
	(delq window-local-variable-alist
	      (assq (selected-window)
		    window-local-variable-alist))))

;; Assume that window-local variables are not buffer-local
;; so we can delay storing until absolutely necessary.

(defun store-window-local-variables (&rest vars-and-vals)
  "Store the window local variables for selected window."
  (setq last-local-window (selected-window))
  (setq last-window-local-vars vars-and-vals))


(defun fetch-window-local-variables ()
 "Fetch the window local variables for selected window.
Does nothing if the last store was for the same window."
  (if (not (eq (selected-window) last-local-window))
      (progn
	;; First store the previous values.
	(setq window-local-variable-alist
	      (cons (cons last-local-window
			  last-window-local-vars)
		    (delq window-local-variable-alist
			  (assq last-local-window
				window-local-variable-alist))))
	;; Now fetch the values for the selected-window.
	(setq last-local-window (selected-window))
	(setq last-window-local-vars 
		(cdr (assq last-local-window window-local-variable-alist)))
	(let ((vars-and-vals last-window-local-vars))
	  (while vars-and-vals
	    (set (car vars-and-vals) (car (cdr vars-and-vals)))
	    (setq vars-and-vals (cdr (cdr vars-and-vals))))))))
		    


;;;==============================================================
;; The search status stack (and isearch window-local variables, not used).

(defun isearch-top-state ()
;;  (fetch-window-local-variables)
  (let ((cmd (car isearch-cmds)))
    (setq isearch-string (car cmd)
	  isearch-message (car (cdr cmd))
	  isearch-success (nth 3 cmd)
	  isearch-forward (nth 4 cmd)
	  isearch-other-end (nth 5 cmd)
	  isearch-invalid-regexp (nth 6 cmd)
	  isearch-wrapped (nth 7 cmd)
	  isearch-barrier (nth 8 cmd))
    (goto-char (car (cdr (cdr cmd))))))

(defun isearch-pop-state ()
;;  (fetch-window-local-variables)
  (setq isearch-cmds (cdr isearch-cmds))
  (isearch-top-state)
  )

(defun isearch-push-state ()
  (setq isearch-cmds 
	(cons (list isearch-string isearch-message (point)
		    isearch-success isearch-forward isearch-other-end 
		    isearch-invalid-regexp isearch-wrapped isearch-barrier)
	      isearch-cmds)))

(defun isearch-store-variables ()
  (store-window-local-variables 
   'isearch-cmds isearch-cmds
   'isearch-regexp isearch-regexp
   'isearch-adjusted isearch-adjusted
   'isearch-slow-terminal-mode isearch-slow-terminal-mode
   'isearch-small-window isearch-small-window
   'isearch-found-point isearch-found-point
   'isearch-found-start isearch-found-start
   'isearch-opoint isearch-opoint
   'isearch-window-configuration isearch-window-configuration
   'isearch-old-local-map isearch-old-local-map
   ))


;;;==================================================================
;; Message string

(defun isearch-message (&optional c-q-hack ellipsis)
  ;; Generate and print the message string.
  (let ((cursor-in-echo-area ellipsis)
	(m (concat
	    (isearch-message-prefix c-q-hack ellipsis)
	    isearch-message
	    (isearch-message-suffix c-q-hack ellipsis)
	    )))
    (if c-q-hack m (message "%s" m))))

(defun isearch-message-prefix (&optional c-q-hack ellipsis)
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
		   (if isearch-regexp "regexp " "")
		   "I-search"
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
  (condition-case lossage
      (let ((inhibit-quit nil)
	    (case-fold-search isearch-case-fold-search))
	(if isearch-regexp (setq isearch-invalid-regexp nil))
	(setq isearch-success
	      (funcall
	       (if isearch-regexp
		   (if isearch-forward 're-search-forward 're-search-backward)
		 (if isearch-forward 'search-forward 'search-backward))
	       isearch-string nil t))
	(if isearch-success
	    (setq isearch-other-end
		  (if isearch-forward (match-beginning 0) (match-end 0)))))

    (quit (setq unread-command-char ?\C-g)
	  (setq isearch-success nil))

    (invalid-regexp 
     (setq isearch-invalid-regexp (car (cdr lossage)))
     (if (string-match
	  "\\`Premature \\|\\`Unmatched \\|\\`Invalid "
	  isearch-invalid-regexp)
	 (setq isearch-invalid-regexp "incomplete input"))))

  (if isearch-success
      nil
    ;; Ding if failed this time after succeeding last time.
    (and (nth 3 (car isearch-cmds))
	 (ding))
    (goto-char (nth 2 (car isearch-cmds)))))

;;;=================================================
;; This is called from incremental-search
;; if the first input character is the exit character.

;; We store the search string in `isearch-string'
;; which has been bound already by `isearch-search'
;; so that, when we exit, it is copied into `search-last-string'.

(defun nonincremental-search (forward regexp)
  ;; This may be broken.  Anyway, it could be replaced by the
  ;; isearch-edit-string command instead.
  (setq isearch-forward forward
	isearch-regexp regexp)
  (let (char function
	inhibit-quit
	(cursor-in-echo-area t))
    ;; Prompt assuming not word search,
    (setq isearch-message 

	  (if isearch-regexp 

	      (if isearch-forward "Regexp search: "
		"Regexp search backward: ")
	    (if isearch-forward "Search: " "Search backward: ")))
    (message "%s" isearch-message)
    ;; Read 1 char and switch to word search if it is ^W.
    (setq char (read-char))
    (if (eq char search-yank-word-char)
	(setq isearch-message (if isearch-forward "Word search: " 

				"Word search backward: "))
      ;; Otherwise let that 1 char be part of the search string.
      (setq unread-command-char char))
    (setq function
	  (if (eq char search-yank-word-char)
	      (if isearch-forward 'word-search-forward 'word-search-backward)
	    (if isearch-regexp
		(if isearch-forward 're-search-forward 're-search-backward)
	      (if isearch-forward 'search-forward 'search-backward))))
    ;; Read the search string with corrected prompt.
    (setq isearch-string (read-string isearch-message isearch-string))
    ;; Empty means use default.
    (if (= 0 (length isearch-string))
	(setq isearch-string search-last-string)
      ;; Set last search string now so it is set even if we fail.
      (setq search-last-string isearch-string))
    ;; Since we used the minibuffer, we should be available for redo.
    (setq command-history 

	  (cons (list function isearch-string) command-history))
    ;; Go ahead and search.
    (let ((case-fold-search isearch-case-fold-search))
      (funcall function isearch-string))))
