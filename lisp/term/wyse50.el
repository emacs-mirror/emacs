;;;; Terminal mode for Wyse 50
;;;; Should work well for Televideo TVI 925 although it's overkill
;;;; Author Daniel Pfieffer (pfieffer@cix.cict.fr) January 1991
;;;; Rewritten for Emacs 19 by Jim Blandy (jimb@occs.cs.oberlin.edu)
;;;;   January 1992


;;; Functions especially for this terminal.

(defun wyse-50-insert-line ()
  "Insert an empty line."
  (interactive)
  (beginning-of-line)
  (open-line 1))

(defun wyse-50-delete-line ()
  "Delete all of the current line."
  (interactive)
  (beginning-of-line)
  (kill-line 1))

(defun wyse-50-insert-char ()
  "Insert a space, even in overwrite mode."
  (interactive)
  (insert ? ))

(defun wyse-50-print-buffer ()
  "Like ``print-buffer'', but verifies before printing.
The `print' key is easy to hit on a Wyse 50."
  (interactive)
  (if (y-or-n-p
       (concat "Print buffer "
	       (buffer-name) "? "))
      (print-buffer)))

(defun wyse-50-top-of-window (n)
  "Move point to the top line of the current window.
With an argument N, move to the Nth line of the window."
  (interactive "p")
  (move-to-window-line (1- n)))

(defun wyse-50-bottom-of-window (n)
  "Move point to the last line of the current window.
With an argument N, move to the Nth line from the bottom of the window."
  (interactive "p")
  (move-to-window-line (- n)))

(defun wyse-50-toggle-screen-width ()
  "Alternate between 80 and 132 columns."
  (interactive)
  (if (<= (frame-width) 80)
      (progn
	(send-string-to-terminal "\e`;")
	(set-frame-width 131))
    (send-string-to-terminal "\e`:")
    (set-frame-width 79)))


;;; Define the escape sequences for the function keys.
(define-key function-key-map "\C-a" (make-keymap))
(mapcar (function (lambda (key-definition)
		    (define-key function-key-map
		      (car key-definition) (nth 1 key-definition))))
	'(("\eI" [S-tab])
	  ("\eJ" [S-prior])
	  ("\eK" [next])
	  ("\eY" [clear])
	  ("\eT" [clear-eol])
	  ("\^^" [home])
	  ("\e\^^" [home-down])
	  ("\eQ" [insert])
	  ("\eE" [insertline])
	  ("\eW" [?\C-?])
	  ("\eR" [deleteline])
	  ("\eP" [print])
	  ("\C-k" [up])
	  ("\C-j" [down])
	  ("\C-l" [right])
	  ("\C-h" [left])
	  ("\C-a\C-k\C-m" [funct-up])
	  ("\C-a\C-j\C-m" [funct-down])
	  ("\C-a\C-l\C-m" [funct-right])
	  ("\C-a\C-h\C-m" [funct-left])
	  ("\er" [replace])
	  ("\^a\^m\^m" [funct-return])
	  ("\^a\^i\^m" [funct-tab])
	  ("\^a@\^m" [f1])
	  ("\^a`\^m" [S-f1])
	  ("\^aA\^m" [f2])
	  ("\^aa\^m" [S-f2])
	  ("\^aB\^m" [f3])
	  ("\^ab\^m" [S-f3])
	  ("\^aC\^m" [f4])
	  ("\^ac\^m" [S-f4])
	  ("\^aD\^m" [f5])
	  ("\^ad\^m" [S-f5])
	  ("\^aE\^m" [f6])
	  ("\^ae\^m" [S-f6])
	  ("\^aF\^m" [f7])
	  ("\^af\^m" [S-f7])
	  ("\^aG\^m" [f8])
	  ("\^ag\^m" [S-f8])
	  ("\^aH\^m" [f9])
	  ("\^ah\^m" [S-f9])
	  ("\^aI\^m" [f10])
	  ("\^ai\^m" [S-f10])
	  ("\^aJ\^m" [f11])
	  ("\^aj\^m" [S-f11])
	  ("\^aK\^m" [f12])
	  ("\^ak\^m" [S-f12])
	  ("\^aL\^m" [f13])
	  ("\^al\^m" [S-f13])
	  ("\^aM\^m" [f14])
	  ("\^am\^m" [S-f14])
	  ("\^aN\^m" [f15])
	  ("\^an\^m" [S-f15])
	  ("\^aO\^m" [f16])
	  ("\^ao\^m" [S-f16])))


;;; Define some of the function keys.
(mapcar (function (lambda (key-definition)
		    (global-set-key (car key-definition)
		     (nth 1 key-definition))))
	'(([insertline]	 wyse-50-insert-line)
	  ([clear]	 recenter)
	  ([clear-eol]   kill-line)
	  ([home]        execute-extended-command)
	  ([home-down]	 shell-command)
	  ([insert]	 wyse-50-insert-char)
	  ([deleteline]	 wyse-50-delete-line)
	  ([replace]	 overwrite-mode)
	  ([print]	 wyse-50-print-buffer)
	  ([funct-up]	 wyse-50-top-of-window)
	  ([funct-down]  wyse-50-bottom-of-window)
	  ([funct-left]  beginning-of-line)
	  ([funct-right] end-of-line)
	  ([f5]		 shell)
	  ([f6]		 dired)
	  ([f7]		 rnews)
	  ([f8]		 rmail)
	  ([f9]		 delete-othe-windows)
	  ([f10]	 other-window)
	  ([f11]	 split-window-vertically)
	  ([f13]	 help-for-help)
	  ([f14]	 wyse-50-toggle-screen-width)
	  ([f15]	 global-set-key)
	  ("\M-?"	 help-for-help)))


;;; Miscellaneous hacks

;;; This is an ugly hack for a nasty problem:
;;; Wyse 50 takes one character cell to store video attributes (which seems to
;;; explain width 79 rather than 80, column 1 is not used!!!).
;;; On killing (C-x C-c) the end inverse code (on column 1 of line 24)
;;; of the mode line is overwritten AFTER all the y-or-n questions.
;;; This causes the attribute to remain in effect until the mode line has
;;; scrolled of the screen.  Suspending (C-z) does not cause this problem.
;;; On such terminals, Emacs should sacrifice the first and last character of
;;; each mode line, rather than a whole screen column!
(setq kill-emacs-hook
      (function (lambda () (interactive)
		  (send-string-to-terminal
		   (concat "\ea23R" (1+ (frame-width)) "C\eG0")))))

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
  (mapcar (function (lambda (key-definition)
		      (global-set-key (car key-definition)
				      (nth 1 key-definition))))
	  ;; By unsetting C-a and then binding it to a prefix, we
	  ;; allow the rest of the function keys which start with C-a
	  ;; to be recognized.
	  '(("\C-a"	nil)
	    ("\C-a\C-a"	beginning-of-line)
	    ("\C-k"	nil)
	    ("\C-j"	nil)
	    ("\C-l"	nil)
	    ("\C-h"	nil)
	    ("\er"	nil)))
  (fset 'enable-arrow-keys nil))
