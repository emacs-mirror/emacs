;;; pc-select.el --- emulate mark, cut, copy and paste from Motif
;;;		     (or MAC GUI or MS-windoze (bah)) look-and-feel
;;;		     including key bindings.

;; Copyright (C) 1995, 1996, 1997, 2000 Free Software Foundation, Inc.

;; Author: Michael Staats <michael@thp.Uni-Duisburg.DE>
;; Keywords: convenience emulation
;; Created: 26 Sep 1995

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

;;; Commentary:

;; This package emulates the mark, copy, cut and paste look-and-feel of motif
;; programs (which is the same as the MAC gui and (sorry for that) MS-Windows).
;; It modifies the keybindings of the cursor keys and the next, prior,
;; home and end keys. They will modify mark-active.
;; You can still get the old behaviour of cursor moving with the
;; control sequences C-f, C-b, etc.
;; This package uses transient-mark-mode and
;; delete-selection-mode.
;;
;; In addition to that all key-bindings from the pc-mode are
;; done here too (as suggested by RMS).
;;
;; As I found out after I finished the first version, s-region.el tries
;; to do the same.... But my code is a little more complete and using
;; delete-selection-mode is very important for the look-and-feel.
;; Pete Forman <pete.forman@airgun.wg.waii.com> provided some motif
;; compliant keybindings which I added. I had to modify them a little
;; to add the -mark and -nomark functionality of cursor moving.
;;
;; Credits:
;; Many thanks to all who made comments.
;; Thanks to RMS and Ralf Muschall <prm@rz.uni-jena.de> for criticism.
;; Kevin Cutts <cutts@ukraine.corp.mot.com> added the beginning-of-buffer
;; and end-of-buffer functions which I modified a little.
;; David Biesack <sasdjb@unx.sas.com> suggested some more cleanup.
;; Thanks to Pete Forman <pete.forman@airgun.wg.waii.com>
;; for additional motif keybindings.
;; Thanks to jvromans@squirrel.nl (Johan Vromans) for a bug report
;; concerning setting of this-command.
;; Dan Nicolaescu <done@ece.arizona.ro> suggested suppressing the
;; scroll-up/scroll-down error.
;; Eli Barzilay (eli@cs.bgu.ac.il) suggested the sexps functions and
;; keybindings.
;;
;; Ok, some details about the idea of pc-selection-mode:
;;
;;  o The standard keys for moving around (right, left, up, down, home, end,
;;    prior, next, called "move-keys" from now on) will always de-activate
;;    the mark.
;;  o If you press "Shift" together with the "move-keys", the region
;;    you pass along is activated
;;  o You have the copy, cut and paste functions (as in many other programs)
;;    which will operate on the active region
;;    It was not possible to bind them to C-v, C-x and C-c for obvious
;;    emacs reasons.
;;    They will be bound according to the "old" behaviour to S-delete (cut),
;;    S-insert (paste) and C-insert (copy). These keys do the same in many
;;    other programs.
;;

;;; Code:

;; Customization:
(defgroup pc-select nil
  "Emulate pc bindings."
  :prefix "pc-select"
  :group 'editing-basics
  :group 'convenience)

(defcustom pc-select-override-scroll-error t
  "*Non-nil means don't generate error on scrolling past edge of buffer.
This variable applies in PC Selection mode only.
The scroll commands normally generate an error if you try to scroll
past the top or bottom of the buffer.  This is annoying when selecting
text with these commands.  If you set this variable to non-nil, these
errors are suppressed."
  :type 'boolean
  :group 'pc-select)

(defcustom pc-select-selection-keys-only nil
  "*Non-nil means only bind the basic selection keys when started.
Other keys that emulate pc-behavior will be untouched.
This gives mostly Emacs-like behaviour with only the selection keys enabled."
  :type 'boolean
  :group 'pc-select)

(defcustom pc-select-meta-moves-sexps nil
  "*Non-nil means move sexp-wise with Meta key, otherwise move word-wise."
  :type 'boolean
  :group 'pc-select)

;;;;
;; misc
;;;;

(provide 'pc-select)

(defun copy-region-as-kill-nomark (beg end)
  "Save the region as if killed; but don't kill it; deactivate mark.
If `interprogram-cut-function' is non-nil, also save the text for a window
system cut and paste.

Deactivating mark is to avoid confusion with `delete-selection-mode'
and `transient-mark-mode'."
 (interactive "r")
 (copy-region-as-kill beg end)
 (setq mark-active nil)
 (message "Region saved"))

(defun exchange-point-and-mark-nomark  ()
  "Like `exchange-point-and-mark' but without activating the mark."
  (interactive)
  (exchange-point-and-mark)
  (setq mark-active nil))

;;;;
;; non-interactive
;;;;
(defun ensure-mark()
  ;; make sure mark is active
  ;; test if it is active, if it isn't, set it and activate it
  (or mark-active (set-mark-command nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; forward and mark
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun forward-char-mark (&optional arg)
  "Ensure mark is active; move point right ARG characters (left if ARG negative).
On reaching end of buffer, stop and signal error."
  (interactive "p")
  (ensure-mark)
  (forward-char arg))

(defun forward-word-mark (&optional arg)
  "Ensure mark is active; move point right ARG words (backward if ARG is negative).
Normally returns t.
If an edge of the buffer is reached, point is left there
and nil is returned."
  (interactive "p")
  (ensure-mark)
  (forward-word arg))

(defun forward-line-mark (&optional arg)
  "Ensure mark is active; move cursor vertically down ARG lines."
  (interactive "p")
  (ensure-mark)
  (forward-line arg)
  (setq this-command 'forward-line)
)

(defun forward-sexp-mark (&optional arg)
  "Ensure mark is active; move forward across one balanced expression (sexp).
With argument, do it that many times.  Negative arg -N means
move backward across N balanced expressions."
  (interactive "p")
  (ensure-mark)
  (forward-sexp arg))

(defun forward-paragraph-mark (&optional arg)
  "Ensure mark is active; move forward to end of paragraph.
With arg N, do it N times; negative arg -N means move backward N paragraphs.

A line which `paragraph-start' matches either separates paragraphs
\(if `paragraph-separate' matches it also) or is the first line of a paragraph.
A paragraph end is the beginning of a line which is not part of the paragraph
to which the end of the previous line belongs, or the end of the buffer."
  (interactive "p")
  (ensure-mark)
  (forward-paragraph arg))

(defun next-line-mark (&optional arg)
  "Ensure mark is active; move cursor vertically down ARG lines.
If there is no character in the target line exactly under the current column,
the cursor is positioned after the character in that line which spans this
column, or at the end of the line if it is not long enough.
If there is no line in the buffer after this one, behavior depends on the
value of `next-line-add-newlines'.  If non-nil, it inserts a newline character
to create a line, and moves the cursor to that line.  Otherwise it moves the
cursor to the end of the buffer \(if already at the end of the buffer, an error
is signaled).

The command \\[set-goal-column] can be used to create
a semipermanent goal column to which this command always moves.
Then it does not try to move vertically.  This goal column is stored
in `goal-column', which is nil when there is none."
  (interactive "p")
  (ensure-mark)
  (next-line arg)
  (setq this-command 'next-line))

(defun end-of-line-mark (&optional arg)
  "Ensure mark is active; move point to end of current line.
With argument ARG not nil or 1, move forward ARG - 1 lines first.
If scan reaches end of buffer, stop there without error."
  (interactive "p")
  (ensure-mark)
  (end-of-line arg)
  (setq this-command 'end-of-line))

(defun backward-line-mark (&optional arg)
  "Ensure mark is active; move cursor vertically up ARG lines."
  (interactive "p")
  (ensure-mark)
  (if (null arg)
      (setq arg 1))
  (forward-line (- arg))
  (setq this-command 'forward-line)
)

(defun scroll-down-mark (&optional arg)
  "Ensure mark is active; scroll down ARG lines; or near full screen if no ARG.
A near full screen is `next-screen-context-lines' less than a full screen.
Negative ARG means scroll upward.
When calling from a program, supply a number as argument or nil."
  (interactive "P")
  (ensure-mark)
  (cond (pc-select-override-scroll-error
	 (condition-case nil (scroll-down arg)
	   (beginning-of-buffer (goto-char (point-min)))))
	(t (scroll-down arg))))

(defun end-of-buffer-mark (&optional arg)
  "Ensure mark is active; move point to the end of the buffer.
With arg N, put point N/10 of the way from the end.

If the buffer is narrowed, this command uses the beginning and size
of the accessible part of the buffer.

Don't use this command in Lisp programs!
\(goto-char \(point-max)) is faster and avoids clobbering the mark."
  (interactive "P")
  (ensure-mark)
  (let ((size (- (point-max) (point-min))))
    (goto-char (if arg
		   (- (point-max)
		      (if (> size 10000)
			  ;; Avoid overflow for large buffer sizes!
			  (* (prefix-numeric-value arg)
			     (/ size 10))
			(/ (* size (prefix-numeric-value arg)) 10)))
		 (point-max))))
  ;; If we went to a place in the middle of the buffer,
  ;; adjust it to the beginning of a line.
  (if arg (forward-line 1)
    ;; If the end of the buffer is not already on the screen,
    ;; then scroll specially to put it near, but not at, the bottom.
    (if (let ((old-point (point)))
	  (save-excursion
		    (goto-char (window-start))
		    (vertical-motion (window-height))
		    (< (point) old-point)))
	(progn
	  (overlay-recenter (point))
	  (recenter -3)))))

;;;;;;;;;
;;;;; no mark
;;;;;;;;;

(defun forward-char-nomark (&optional arg)
  "Deactivate mark; move point right ARG characters \(left if ARG negative).
On reaching end of buffer, stop and signal error."
  (interactive "p")
  (setq mark-active nil)
  (forward-char arg))

(defun forward-word-nomark (&optional arg)
  "Deactivate mark; move point right ARG words \(backward if ARG is negative).
Normally returns t.
If an edge of the buffer is reached, point is left there
and nil is returned."
  (interactive "p")
  (setq mark-active nil)
  (forward-word arg))

(defun forward-line-nomark (&optional arg)
  "Deactivate mark; move cursor vertically down ARG lines."
  (interactive "p")
  (setq mark-active nil)
  (forward-line arg)
  (setq this-command 'forward-line)
)

(defun forward-sexp-nomark (&optional arg)
  "Deactivate mark; move forward across one balanced expression (sexp).
With argument, do it that many times.  Negative arg -N means
move backward across N balanced expressions."
  (interactive "p")
  (setq mark-active nil)
  (forward-sexp arg))

(defun forward-paragraph-nomark (&optional arg)
  "Deactivate mark; move forward to end of paragraph.
With arg N, do it N times; negative arg -N means move backward N paragraphs.

A line which `paragraph-start' matches either separates paragraphs
\(if `paragraph-separate' matches it also) or is the first line of a paragraph.
A paragraph end is the beginning of a line which is not part of the paragraph
to which the end of the previous line belongs, or the end of the buffer."
  (interactive "p")
  (setq mark-active nil)
  (forward-paragraph arg))

(defun next-line-nomark (&optional arg)
  "Deactivate mark; move cursor vertically down ARG lines.
If there is no character in the target line exactly under the current column,
the cursor is positioned after the character in that line which spans this
column, or at the end of the line if it is not long enough.
If there is no line in the buffer after this one, behavior depends on the
value of `next-line-add-newlines'.  If non-nil, it inserts a newline character
to create a line, and moves the cursor to that line.  Otherwise it moves the
cursor to the end of the buffer (if already at the end of the buffer, an error
is signaled).

The command \\[set-goal-column] can be used to create
a semipermanent goal column to which this command always moves.
Then it does not try to move vertically.  This goal column is stored
in `goal-column', which is nil when there is none."
  (interactive "p")
  (setq mark-active nil)
  (next-line arg)
  (setq this-command 'next-line))

(defun end-of-line-nomark (&optional arg)
  "Deactivate mark; move point to end of current line.
With argument ARG not nil or 1, move forward ARG - 1 lines first.
If scan reaches end of buffer, stop there without error."
  (interactive "p")
  (setq mark-active nil)
  (end-of-line arg)
  (setq this-command 'end-of-line))

(defun backward-line-nomark (&optional arg)
  "Deactivate mark; move cursor vertically up ARG lines."
  (interactive "p")
  (setq mark-active nil)
  (if (null arg)
      (setq arg 1))
  (forward-line (- arg))
  (setq this-command 'forward-line)
)

(defun scroll-down-nomark (&optional arg)
  "Deactivate mark; scroll down ARG lines; or near full screen if no ARG.
A near full screen is `next-screen-context-lines' less than a full screen.
Negative ARG means scroll upward.
When calling from a program, supply a number as argument or nil."
  (interactive "P")
  (setq mark-active nil)
  (cond (pc-select-override-scroll-error
	 (condition-case nil (scroll-down arg)
	   (beginning-of-buffer (goto-char (point-min)))))
	(t (scroll-down arg))))

(defun end-of-buffer-nomark (&optional arg)
  "Deactivate mark; move point to the end of the buffer.
With arg N, put point N/10 of the way from the end.

If the buffer is narrowed, this command uses the beginning and size
of the accessible part of the buffer.

Don't use this command in Lisp programs!
\(goto-char (point-max)) is faster and avoids clobbering the mark."
  (interactive "P")
  (setq mark-active nil)
  (let ((size (- (point-max) (point-min))))
    (goto-char (if arg
		   (- (point-max)
		      (if (> size 10000)
			  ;; Avoid overflow for large buffer sizes!
			  (* (prefix-numeric-value arg)
			     (/ size 10))
			(/ (* size (prefix-numeric-value arg)) 10)))
		 (point-max))))
  ;; If we went to a place in the middle of the buffer,
  ;; adjust it to the beginning of a line.
  (if arg (forward-line 1)
    ;; If the end of the buffer is not already on the screen,
    ;; then scroll specially to put it near, but not at, the bottom.
    (if (let ((old-point (point)))
	  (save-excursion
		    (goto-char (window-start))
		    (vertical-motion (window-height))
		    (< (point) old-point)))
	(progn
	  (overlay-recenter (point))
	  (recenter -3)))))


;;;;;;;;;;;;;;;;;;;;
;;;;;; backwards and mark
;;;;;;;;;;;;;;;;;;;;

(defun backward-char-mark (&optional arg)
"Ensure mark is active; move point left ARG characters (right if ARG negative).
On attempt to pass beginning or end of buffer, stop and signal error."
  (interactive "p")
  (ensure-mark)
  (backward-char arg))

(defun backward-word-mark (&optional arg)
  "Ensure mark is active; move backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (ensure-mark)
  (backward-word arg))

(defun backward-sexp-mark (&optional arg)
  "Ensure mark is active; move backward across one balanced expression (sexp).
With argument, do it that many times.  Negative arg -N means
move forward across N balanced expressions."
  (interactive "p")
  (ensure-mark)
  (backward-sexp arg))

(defun backward-paragraph-mark (&optional arg)
  "Ensure mark is active; move backward to start of paragraph.
With arg N, do it N times; negative arg -N means move forward N paragraphs.

A paragraph start is the beginning of a line which is a
`first-line-of-paragraph' or which is ordinary text and follows a
paragraph-separating line; except: if the first real line of a
paragraph is preceded by a blank line, the paragraph starts at that
blank line.

See `forward-paragraph' for more information."
  (interactive "p")
  (ensure-mark)
  (backward-paragraph arg))

(defun previous-line-mark (&optional arg)
  "Ensure mark is active; move cursor vertically up ARG lines.
If there is no character in the target line exactly over the current column,
the cursor is positioned after the character in that line which spans this
column, or at the end of the line if it is not long enough.

The command \\[set-goal-column] can be used to create
a semipermanent goal column to which this command always moves.
Then it does not try to move vertically.

If you are thinking of using this in a Lisp program, consider using
`forward-line' with a negative argument instead.  It is usually easier
to use and more reliable (no dependence on goal column, etc.)."
  (interactive "p")
  (ensure-mark)
  (previous-line arg)
  (setq this-command 'previous-line))

(defun beginning-of-line-mark (&optional arg)
  "Ensure mark is active; move point to beginning of current line.
With argument ARG not nil or 1, move forward ARG - 1 lines first.
If scan reaches end of buffer, stop there without error."
  (interactive "p")
  (ensure-mark)
  (beginning-of-line arg))


(defun scroll-up-mark (&optional arg)
"Ensure mark is active; scroll upward ARG lines; or near full screen if no ARG.
A near full screen is `next-screen-context-lines' less than a full screen.
Negative ARG means scroll downward.
When calling from a program, supply a number as argument or nil."
  (interactive "P")
  (ensure-mark)
  (cond (pc-select-override-scroll-error
	 (condition-case nil (scroll-up arg)
	   (end-of-buffer (goto-char (point-max)))))
	(t (scroll-up arg))))

(defun beginning-of-buffer-mark (&optional arg)
  "Ensure mark is active; move point to the beginning of the buffer.
With arg N, put point N/10 of the way from the beginning.

If the buffer is narrowed, this command uses the beginning and size
of the accessible part of the buffer.

Don't use this command in Lisp programs!
\(goto-char (p\oint-min)) is faster and avoids clobbering the mark."
  (interactive "P")
  (ensure-mark)
  (let ((size (- (point-max) (point-min))))
    (goto-char (if arg
		   (+ (point-min)
		      (if (> size 10000)
			  ;; Avoid overflow for large buffer sizes!
			  (* (prefix-numeric-value arg)
			     (/ size 10))
			(/ (+ 10 (* size (prefix-numeric-value arg))) 10)))
		 (point-min))))
  (if arg (forward-line 1)))

;;;;;;;;
;;; no mark
;;;;;;;;

(defun backward-char-nomark (&optional arg)
  "Deactivate mark; move point left ARG characters (right if ARG negative).
On attempt to pass beginning or end of buffer, stop and signal error."
  (interactive "p")
  (setq mark-active nil)
  (backward-char arg))

(defun backward-word-nomark (&optional arg)
  "Deactivate mark; move backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (setq mark-active nil)
  (backward-word arg))

(defun backward-sexp-nomark (&optional arg)
  "Deactivate mark; move backward across one balanced expression (sexp).
With argument, do it that many times.  Negative arg -N means
move forward across N balanced expressions."
  (interactive "p")
  (setq mark-active nil)
  (backward-sexp arg))

(defun backward-paragraph-nomark (&optional arg)
  "Deactivate mark; move backward to start of paragraph.
With arg N, do it N times; negative arg -N means move forward N paragraphs.

A paragraph start is the beginning of a line which is a
`first-line-of-paragraph' or which is ordinary text and follows a
paragraph-separating line; except: if the first real line of a
paragraph is preceded by a blank line, the paragraph starts at that
blank line.

See `forward-paragraph' for more information."
  (interactive "p")
  (setq mark-active nil)
  (backward-paragraph arg))

(defun previous-line-nomark (&optional arg)
  "Deactivate mark; move cursor vertically up ARG lines.
If there is no character in the target line exactly over the current column,
the cursor is positioned after the character in that line which spans this
column, or at the end of the line if it is not long enough.

The command \\[set-goal-column] can be used to create
a semipermanent goal column to which this command always moves.
Then it does not try to move vertically."
  (interactive "p")
  (setq mark-active nil)
  (previous-line arg)
  (setq this-command 'previous-line))

(defun beginning-of-line-nomark (&optional arg)
  "Deactivate mark; move point to beginning of current line.
With argument ARG not nil or 1, move forward ARG - 1 lines first.
If scan reaches end of buffer, stop there without error."
  (interactive "p")
  (setq mark-active nil)
  (beginning-of-line arg))

(defun scroll-up-nomark (&optional arg)
  "Deactivate mark; scroll upward ARG lines; or near full screen if no ARG.
A near full screen is `next-screen-context-lines' less than a full screen.
Negative ARG means scroll downward.
When calling from a program, supply a number as argument or nil."
  (interactive "P")
  (setq mark-active nil)
  (cond (pc-select-override-scroll-error
	 (condition-case nil (scroll-up arg)
	   (end-of-buffer (goto-char (point-max)))))
	(t (scroll-up arg))))

(defun beginning-of-buffer-nomark (&optional arg)
  "Deactivate mark; move point to the beginning of the buffer.
With arg N, put point N/10 of the way from the beginning.

If the buffer is narrowed, this command uses the beginning and size
of the accessible part of the buffer.

Don't use this command in Lisp programs!
\(goto-char (point-min)) is faster and avoids clobbering the mark."
  (interactive "P")
  (setq mark-active nil)
  (let ((size (- (point-max) (point-min))))
    (goto-char (if arg
		   (+ (point-min)
		      (if (> size 10000)
			  ;; Avoid overflow for large buffer sizes!
			  (* (prefix-numeric-value arg)
			     (/ size 10))
			(/ (+ 10 (* size (prefix-numeric-value arg))) 10)))
		 (point-min))))
  (if arg (forward-line 1)))

;;;###autoload
(defun pc-selection-mode ()
  "Change mark behaviour to emulate Motif, MAC or MS-Windows cut and paste style.

This mode enables Delete Selection mode and Transient Mark mode.

The arrow keys (and others) are bound to new functions
which modify the status of the mark.

The ordinary arrow keys disable the mark.
The shift-arrow keys move, leaving the mark behind.

C-LEFT and C-RIGHT move back or forward one word, disabling the mark.
S-C-LEFT and S-C-RIGHT move back or forward one word, leaving the mark behind.

M-LEFT and M-RIGHT move back or forward one word or sexp, disabling the mark.
S-M-LEFT and S-M-RIGHT move back or forward one word or sexp, leaving the mark
behind.  To control whether these keys move word-wise or sexp-wise set the
variable `pc-select-meta-moves-sexps' after loading pc-select.el but before
turning `pc-selection-mode' on.

C-DOWN and C-UP move back or forward a paragraph, disabling the mark.
S-C-DOWN and S-C-UP move back or forward a paragraph, leaving the mark behind.

HOME moves to beginning of line, disabling the mark.
S-HOME moves to beginning of line, leaving the mark behind.
With Ctrl or Meta, these keys move to beginning of buffer instead.

END moves to end of line, disabling the mark.
S-END moves to end of line, leaving the mark behind.
With Ctrl or Meta, these keys move to end of buffer instead.

PRIOR or PAGE-UP scrolls and disables the mark.
S-PRIOR or S-PAGE-UP scrolls and leaves the mark behind.

S-DELETE kills the region (`kill-region').
S-INSERT yanks text from the kill ring (`yank').
C-INSERT copies the region into the kill ring (`copy-region-as-kill').

In addition, certain other PC bindings are imitated (to avoid this, set
the variable `pc-select-selection-keys-only' to t after loading pc-select.el
but before calling `pc-selection-mode'):

  F6           `other-window'
  DELETE       `delete-char'
  C-DELETE     `kill-line'
  M-DELETE     `kill-word'
  C-M-DELETE   `kill-sexp'
  C-BACKSPACE  `backward-kill-word'
  M-BACKSPACE  `undo'"
  ;; FIXME: make into a proper minor mode (i.e. undoable).
  ;; FIXME: bring pc-bindings-mode here ?
  (interactive)
  ;;
  ;; keybindings
  ;;

  ;; This is to avoid confusion with the delete-selection-mode
  ;; On simple displays you can't see that a region is active and
  ;; will be deleted on the next keypress.  IMHO especially for
  ;; copy-region-as-kill this is confusing.
  ;; The same goes for exchange-point-and-mark
  (define-key global-map "\M-w" 'copy-region-as-kill-nomark)
  (define-key global-map "\C-x\C-x" 'exchange-point-and-mark-nomark)
  ;; The following keybindings are for standard ISO keyboards
  ;; as they are used with IBM compatible PCs, IBM RS/6000,
  ;; MACs, many X-Stations and probably more
  (define-key global-map [S-right]   'forward-char-mark)
  (define-key global-map [right]     'forward-char-nomark)
  (define-key global-map [C-S-right] 'forward-word-mark)
  (define-key global-map [C-right]   'forward-word-nomark)
  (define-key global-map [S-left]    'backward-char-mark)
  (define-key global-map [left]      'backward-char-nomark)
  (define-key global-map [C-S-left]  'backward-word-mark)
  (define-key global-map [C-left]    'backward-word-nomark)
  (cond (pc-select-meta-moves-sexps
	 (define-key global-map [M-S-right] 'forward-sexp-mark)
	 (define-key global-map [M-right]   'forward-sexp-nomark)
	 (define-key global-map [M-S-left]  'backward-sexp-mark)
	 (define-key global-map [M-left]    'backward-sexp-nomark))
	(t
	 (define-key global-map [M-S-right] 'forward-word-mark)
	 (define-key global-map [M-right]   'forward-word-nomark)
	 (define-key global-map [M-S-left]  'backward-word-mark)
	 (define-key global-map [M-left]    'backward-word-nomark)))

  (define-key global-map [S-down]    'next-line-mark)
  (define-key global-map [down]      'next-line-nomark)

  (define-key global-map [S-end]     'end-of-line-mark)
  (define-key global-map [end]       'end-of-line-nomark)
  (global-set-key [S-C-end]          'end-of-buffer-mark)
  (global-set-key [C-end]            'end-of-buffer-nomark)
  (global-set-key [S-M-end]          'end-of-buffer-mark)
  (global-set-key [M-end]            'end-of-buffer-nomark)

  (define-key global-map [S-next]    'scroll-up-mark)
  (define-key global-map [next]      'scroll-up-nomark)

  (define-key global-map [S-up]      'previous-line-mark)
  (define-key global-map [up]        'previous-line-nomark)

  (define-key global-map [S-home]    'beginning-of-line-mark)
  (define-key global-map [home]      'beginning-of-line-nomark)
  (global-set-key [S-C-home]         'beginning-of-buffer-mark)
  (global-set-key [C-home]           'beginning-of-buffer-nomark)
  (global-set-key [S-M-home]         'beginning-of-buffer-mark)
  (global-set-key [M-home]           'beginning-of-buffer-nomark)

  (define-key global-map [M-S-down]  'forward-line-mark)
  (define-key global-map [M-down]    'forward-line-nomark)
  (define-key global-map [M-S-up]    'backward-line-mark)
  (define-key global-map [M-up]      'backward-line-nomark)

  (define-key global-map [S-prior]   'scroll-down-mark)
  (define-key global-map [prior]     'scroll-down-nomark)

  ;; Next four lines are from Pete Forman.
  (global-set-key [C-down] 'forward-paragraph-nomark)	; KNextPara     cDn
  (global-set-key [C-up] 'backward-paragraph-nomark)	; KPrevPara     cUp
  (global-set-key [S-C-down] 'forward-paragraph-mark)
  (global-set-key [S-C-up] 'backward-paragraph-mark)

  (unless pc-select-selection-keys-only
    ;; We are behaving like normal-erase-is-backspace-mode, so
    ;; say so explicitly.  But don't do that on a Unix tty, since
    ;; some of them have keyboards that by default already behave
    ;; as if normal-erase-is-backspace mode is on, and turning it
    ;; a second time screws them up.
    (if (or (eq window-system 'x)
	    (memq system-name '(ms-dos windows-nt macos)))
	(progn
	  (setq-default normal-erase-is-backspace t)
	  (normal-erase-is-backspace-mode 1))
      ;; This is for tty.  We don't turn on normal-erase-is-backspace,
      ;; but bind keys as pc-selection-mode did before
      ;; normal-erase-is-backspace was invented, to keep us back
      ;; compatible.
      (global-set-key [delete] 'delete-char) ; KDelete       Del
      (define-key function-key-map  [M-delete] [?\M-d])
      (global-set-key [C-backspace] 'backward-kill-word))
    (define-key global-map [S-insert]  'yank)
    (define-key global-map [C-insert]  'copy-region-as-kill)
    (define-key global-map [S-delete]  'kill-region)

    ;; The following bindings are useful on Sun Type 3 keyboards
    ;; They implement the Get-Delete-Put (copy-cut-paste)
    ;; functions from sunview on the L6, L8 and L10 keys
    ;; Sam Steingold <sds@gnu.org> says that f16 is copy and f18 is paste.
    (define-key global-map [f16]  'copy-region-as-kill)
    (define-key global-map [f18]  'yank)
    (define-key global-map [f20]  'kill-region)

    ;; The following bindings are from Pete Forman.
    (global-set-key [f6] 'other-window)	; KNextPane     F6
    (global-set-key [C-delete] 'kill-line) ; KEraseEndLine cDel
    (global-set-key "\M-\d" 'undo)	; KUndo         aBS

    ;; The following binding is taken from pc-mode.el
    ;; as suggested by RMS.
    ;; I only used the one that is not covered above.
    (global-set-key [C-M-delete]  'kill-sexp)
    ;; Next line proposed by Eli Barzilay
    (global-set-key [C-escape]    'electric-buffer-list))
  ;;
  ;; setup
  ;;
  ;; Next line proposed by Eli Barzilay
  (setq highlight-nonselected-windows nil)
  (transient-mark-mode 1)
  (setq mark-even-if-inactive t)
  (delete-selection-mode 1))

;;;###autoload
(defcustom pc-selection-mode nil
  "Toggle PC Selection mode.
Change mark behaviour to emulate Motif, MAC or MS-Windows cut and paste style,
and cursor movement commands.
This mode enables Delete Selection mode and Transient Mark mode.
You must modify via \\[customize] for this variable to have an effect."
  :set (lambda (symbol value)
	(if value (pc-selection-mode)))
  :type 'boolean
  :group 'pc-select
  :require 'pc-select)

;;; pc-select.el ends here
