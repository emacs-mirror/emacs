; Terminal mode for Wyse 50
; should work well for Televideo Tvi 925 though it's an overkill
; Author Daniel Pfeiffer <pfeiffer@cix.cict.fr> january 1991

(require 'keypad)

; at least some of these should be transferred to keypad.el
(keypad-default "A" '(lambda () (interactive)
		       ; actually insert an empty line
		       (beginning-of-line)
		       (open-line 1)))
(keypad-default "E" 'kill-line)
; (keypad-default "h" 'execute-extended-command)
(define-key function-keymap "h" 'execute-extended-command)  ; bad, bad !!
(keypad-default "H" 'shell-command)
(keypad-default "I" '(lambda () (interactive)
		       (insert ? ))) ; works even in overwrite-mode
(keypad-default "L" '(lambda () (interactive)
		       ; delete the whole line
		       (beginning-of-line)
		       (kill-line 1)))
(keypad-default "M" 'overwrite-mode)
(keypad-default "\^e" 'shell)			; F5
(keypad-default "\^f" 'dired)			; F6
(keypad-default "\^g" 'rnews)			; F7
(keypad-default "\^h" 'rmail)			; F8

(keypad-default "\^i" 'delete-other-windows)	; F9
(keypad-default "\^j" 'other-window)		; F10
(keypad-default "\^k" 'split-window-vertically)	; F11

(keypad-default "\^m" 'help-for-help)		; F13
(keypad-default "\^n" 'toggle-screen-width)	; F14
(keypad-default "\^o" 'set-function-key)	; F15


; Keys that don't conflict with Emacs defaults
; I write \M-x and \C-x for what the user types, \ex and \^x for key sequences
(setup-terminal-keymap global-map
      '(("\M-?" . ?\?)	; Esc ?
	("\eI" . ?T)	; Shift Tab
	("\eJ" . ?P)	; Shift Prev PAGE
	("\eK" . ?N)	; PAGE Next
	("\eY" . ?C)	; Shift Scrn CLR
	("\eT" . ?E)	; CLR Line
	("\^^" . ?h)	; Home
	("\M-\^^" . ?H)	; Esc Home
	("\eQ" . ?I)	; INS Char
	("\eE" . ?A)	; Shift Line INS
	("\eW" . ?D)	; DEL Char
	("\eR" . ?L)))	; Shift Line DEL

; Print -- put in some extra security
(global-set-key "\eP" '(lambda () (interactive)
			  (if (y-or-n-p
			       (concat "Print buffer "
				       (buffer-name) "? "))
			      (print-buffer))))


; this is an ugly hack for a nasty problem:
; Wyse 50 takes one character cell to store video attributes (which seems to
; explain width 79 rather than 80, column 1 is not used!!!).
; On killing (C-x C-c) the end inverse code (on column 1 of line 24)
; of the mode line is overwritten AFTER all the y-or-n questions.
; This causes the attribute to remain in effect until the mode line has
; scrolled of the screen.  Suspending (C-z) does not cause this problem.
; On such terminals, Emacs should sacrifice the first and last character of
; each mode line, rather than a whole screen column!
(setq kill-emacs-hook '(lambda () (interactive)
			 (send-string-to-terminal
			  (concat "\ea23R" (1+ (screen-width)) "C\eG0"))))


; This function does more than its name which was copied from term/vt100.el
; Some more neutral name should be used thru-out term/*.el to simplify
; programming term-setup-hook
(defun enable-arrow-keys ()
  "To be called by term-setup-hook. Overrides 6 Emacs standard keys
whose functions are then typed as follows:
C-a	Funct Left-arrow
C-h	M-?
LFD	Funct Return, some modes override down-arrow via LFD
C-k	CLR Line
C-l	Scrn CLR
M-r	M-x move-to-window-line, Funct up-arrow or down-arrow are similar
All special keys except Send, Shift Ins, Shift Home and shifted functions keys
are assigned some hopefully useful meaning."
  (interactive)

  ; Function keys
  (define-prefix-command 'Funct-prefix)
  (define-key global-map "\^a" 'Funct-prefix)

  ; Arrow keys
  (setup-terminal-keymap global-map
      '(("\C-a\C-a" . beginning-of-line)	; for auld lang syne
	("\^a\^m\^m" . newline-and-indent)

	("\^k" . ?u)	; up-arrow
	("\^j" . ?d)	; down-arrow
	("\^l" . ?r)	; right-arrow
	("\^h" . ?l)	; left-arrow

	; Terminal needs both Ins and Repl but Emacs knows how to toggle
	; with just one key. No need to override Ins which is "\eq".
	("\er" . ?M)		; Repl

	("\^a\^i\^m" . ?t)	; Funct Tab

	; Function keys F1 thru F16  (we don't define shifted function keys,
	; they send the same code with the middle character in lowercase.
	; eg. "Shift F2" is the same as "Funct a" which is more mnemonic but
	; keypad.el doesn't provide enough codes to accomodate all these)
	("\^a@\^m" . 1)		("\^aH\^m" . 9)
	("\^aA\^m" . 2)		("\^aI\^m" . 10)
	("\^aB\^m" . 3)		("\^aJ\^m" . 11)
	("\^aC\^m" . 4)		("\^aK\^m" . 12)
	("\^aD\^m" . 5)		("\^aL\^m" . 13)
	("\^aE\^m" . 6)		("\^aM\^m" . 14)
	("\^aF\^m" . 7)		("\^aN\^m" . 15)
	("\^aG\^m" . 8)		("\^aO\^m" . 16)

	; Funct Arrow keys
	("\^a\^k\^m" . (lambda (n) (interactive "p")
			    (move-to-window-line (1- n))))
	("\^a\^j\^m" . (lambda (n) (interactive "p")
			    (move-to-window-line (- n))))
	("\^a\^h\^m" . beginning-of-line)
	("\^a\^l\^m" . end-of-line)))

  ; forget self to put memory to some serious use
  (fset 'enable-arrow-keys nil))


(defun toggle-screen-width ()
  "Alternate between 80 and 132 columns."
  (interactive)
  (if (<= (screen-width) 80)
      (progn
	(send-string-to-terminal "\e`;")
	(set-screen-width 131))
    (send-string-to-terminal "\e`:")
    (set-screen-width 79)))

;-----------------------------------------------------------------------------
; this function is completely independent of wyse, it should be auto-loadable
; (presumably from keypad.el) for use in ~/emacs.  It should be the only thing
; users need to know about all this unintelligible "forwarding" gibberish.
; This paves the way for a save-function-keys (some day or sleepless night)
; that will edit calls like (set-function-key ?x 'do-whatever) in ~/.emacs.
(defun set-function-key (key &optional def)
  "Prompt for a function or other special key and assign it a meaning.
The key must have been \"forwarded\" to a character in term/*.el.

As a function takes two args CHAR and DEF, with DEF as in define-key.
If your terminals term/*.el forwards a physical key to CHAR (before or after
calling this function), then that key will mean DEF, else it is ignored.
CHAR is one of the following:
For numbered function keys
	0, 1, ..., 24  (or ?\\^@, ?\\^a, ..., ?\\^x which is the same)
For keypad keys in application mode
	?0, ?1, ..., ?9  --  keypad key labelled with that digit,
		but only if that key is not an arrow key (see ?u, ?d, ?r, ?l).
	?-  --  keypad key labelled `-'.
	?.  --  keypad key labelled `.'.
	?,  --  keypad key labelled `,'.
	?e  --  key labelled enter.
For keys labelled with some words or a symbol
	?a  --  clear all tabs key.
	?A  --  insert line key.
	?C  --  clear screen key.
	?c  --  erase key.
	?D  --  delete character key.
	?d  --  down-arrow.
	?E  --  clear to end of line key.
	?e  --  key labelled enter.
	?f  --  find key or search key.
	?F  --  scroll forward key.
	?H  --  home-down.
	?h  --  home-position key.
	?I  --  insert character key
		If there is just an \"insert\" key, it should be this.
	?k  --  delete key or remove key.
	?L  --  delete line key.
	?l  --  left-arrow.
	?M  --  exit insert mode key.
	?N  --  next page key.
	?p  --  portrait mode.
	?P  --  previous page key.
	?q  --  landscape mode.
	?r  --  right-arrow.
	?R  --  scroll reverse key.
	?S  --  clear to end of screen key.
	?s  --  select key.
	?t  --  clear tab this column key.
	?T  --  set tab this column key.
	?u  --  up-arrow.
	?x  --  do key.
	?\\?  --  help."
  (interactive "kKey to redefine: ")
  (let ((map function-keymap))
    (if (integerp key)
	()
      ; reinvent lookup-key to get (map . char) instead of def of char in map
      (setq map (or (lookup-key global-map
				(substring key 0 (1- (length key))))
		    global-map)
	    key (string-to-char (substring key (1- (length key)))))
      (while (symbolp map)
	(setq map (symbol-function map)))
      (setq map (if (listp map)
		    (cdr (assq key (cdr map)))
		  (aref map key)))
      (if (and (consp map)
	       (integerp (cdr map)))
	  (setq key (cdr map)
		map (car map))
        (error "Key is not a \"forwarded\" definition.")))
    (if def
        ()
      (setq def (read-command "command (default last keyboard macro): "))
      (if (string-equal (symbol-name def) "")
          (setq def last-kbd-macro))
      (setq command-history	; nonsense really, since you don't see
	    (cons		; key as in a function call (?char)
	     (list 'set-function-key key
		   (if (stringp def) def (list 'quote def)))
	     command-history)))
    ; all we do when called as a function
    (define-key map (char-to-string key) def)))


