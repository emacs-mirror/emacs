;;; wyse50.el --- terminal support code for Wyse 50

;; Author: Daniel Pfieffer <pfieffer@cix.cict.fr> January 1991
;;	Jim Blandy <jimb@occs.cs.oberlin.edu>
;; Keywords: terminals

;; Copyright (C) 1989 Free Software Foundation, Inc.

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

;;; Commentary:

;; Uses the Emacs 19 terminal initialization features --- won't work with 18.
;; Rewritten for Emacs 19 by jimb,  January 1992
;; Cleaned up for new terminal package cinventions by esr, March 1993
;; Should work well for Televideo TVI 925 although it's overkill.
;;
;; The Wyse50 is ergonomically wonderful, but its escape-sequence design sucks
;; rocks.  The left-arrow key emits a backspace (!) and the down-arrow a line
;; feed (!!).  Thus, you have to unbind some commonly-used Emacs keys to
;; enable the arrows.

;;; Code:

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
	'(
	  ;; These might be set up by termcap and terminfo
	  ("\C-k"	[up])
	  ("\C-j"	[down])
	  ("\C-l"	[right])
	  ("\C-h"	[left])
	  ("\^a@\^m"	[f1])
	  ("\^aA\^m"	[f2])
	  ("\^aB\^m"	[f3])
	  ("\^aC\^m"	[f4])
	  ("\^aD\^m"	[f5])
	  ("\^aE\^m"	[f6])
	  ("\^aF\^m"	[f7])
	  ("\^aG\^m"	[f8])
	  ("\^aH\^m"	[f9])

	  ;; These might be set up by terminfo
	  ("\eK"	[next])
	  ("\eT"	[clearline])
	  ("\^^"	[home])
	  ("\e\^^"	[end])
	  ("\eQ"	[insert])
	  ("\eE"	[insertline])
	  ("\eR"	[deleteline])
	  ("\eP"	[print])
	  ("\er"	[replace])
	  ("\^aI\^m"	[f10])
	  ("\^aJ\^m"	[f11])
	  ("\^aK\^m"	[f12])
	  ("\^aL\^m"	[f13])
	  ("\^aM\^m"	[f14])
	  ("\^aN\^m"	[f15])
	  ("\^aO\^m"	[f16])
	  ("\^a`\^m"	[f17])
	  ("\^aa\^m"	[f18])
	  ("\^ab\^m"	[f19])
	  ("\^ac\^m"	[f20])
	  ("\^ad\^m"	[f21])
	  ("\^ae\^m"	[f22])
	  ("\^af\^m"	[f23])
	  ("\^ag\^m"	[f24])
	  ("\^ah\^m"	[f25])
	  ("\^ai\^m"	[f26])
	  ("\^aj\^m"	[f27])
	  ("\^ak\^m"	[f28])
	  ("\^al\^m"	[f29])
	  ("\^am\^m"	[f30])
	  ("\^an\^m"	[f31])
	  ("\^ao\^m"	[f32])

	  ;; Terminfo may know about these, but X won't
	  ("\eI"	[key-stab])		;; Not an X keysym
	  ("\eJ"	[key-snext])		;; Not an X keysym
	  ("\eY"	[key-clear])		;; Not an X keysym

	  ;; These are totally strange :-)
	  ("\eW"	[?\C-?])	;; Not an X keysym
	  ("\^a\^k\^m"	[funct-up])	;; Not an X keysym
	  ("\^a\^j\^m"	[funct-down])	;; Not an X keysym
	  ("\^a\^l\^m"	[funct-right])	;; Not an X keysym
	  ("\^a\^h\^m"	[funct-left])	;; Not an X keysym
	  ("\^a\^m\^m"	[funct-return])	;; Not an X keysym
	  ("\^a\^i\^m"	[funct-tab])	;; Not an X keysym
))

(defun enable-arrow-keys ()
  "To be called by term-setup-hook. Overrides 6 Emacs standard keys
whose functions are then typed as follows:
C-a	Funct Left-arrow
C-h	M-?
LFD	Funct Return, some modes override down-arrow via LFD
C-k	CLR Line
C-l	Scrn CLR
M-r	M-x move-to-window-line, Funct up-arrow or down-arrow are similar
"
  (interactive)
  (mapcar (function (lambda (key-definition)
		      (global-set-key (car key-definition)
				      (nth 1 key-definition))))
	  ;; By unsetting C-a and then binding it to a prefix, we
	  ;; allow the rest of the function keys which start with C-a
	  ;; to be recognized.
	  '(("\C-a"	nil)
	    ("\C-k"	nil)
	    ("\C-j"	nil)
	    ("\C-l"	nil)
	    ("\C-h"	nil)
	    ("\er"	nil)))
  (fset 'enable-arrow-keys nil))


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

;;; wyse50.el ends here
