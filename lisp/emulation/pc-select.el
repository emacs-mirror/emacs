;;; pc-select.el --- emulate mark, cut, copy and paste from motif
;;;		     (or MAC GUI) or MS-windoze (bah)) look-and-feel
;;;		     including key bindings

;; Copyright (C) 1995, 1996 Free Software Foundation, Inc.

;; Author: Michael Staats <michael@thp.Uni-Duisburg.DE>
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
;;
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

;;; Code:

;;;;
;; misc
;;;;

(provide 'pc-select)

(defun copy-region-as-kill-nomark (beg end)
  "Save the region as if killed; but don't kill it; deactivate mark.
If `interprogram-cut-function' is non-nil, also save the text for a window
system cut and paste.\n
Deactivating mark is to avoid confusion with delete-selection-mode
and transient-mark-mode."
 (interactive "r")
 (copy-region-as-kill beg end)
 (setq mark-active nil)
 (message "Region saved"))

;;;;
;; non-interactive
;;;;
(defun ensure-mark()
  ;; make sure mark is active
  ;; test if it is active, if it isn't, set it and activate it
  (and (not mark-active) (set-mark-command nil)))

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

(defun forward-paragraph-mark (&optional arg)
  "Ensure mark is active; move forward to end of paragraph.
With arg N, do it N times; negative arg -N means move backward N paragraphs.\n
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
is signaled).\n
The command C-x C-n can be used to create
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

(defun scroll-down-mark (&optional arg)
  "Ensure mark is active; scroll down ARG lines; or near full screen if no ARG.
A near full screen is `next-screen-context-lines' less than a full screen.
Negative ARG means scroll upward.
When calling from a program, supply a number as argument or nil."
  (interactive "P") 
  (ensure-mark)
  (scroll-down arg))

(defun end-of-buffer-mark (&optional arg)
  "Ensure mark is active; move point to the end of the buffer.
With arg N, put point N/10 of the way from the end.\n
If the buffer is narrowed, this command uses the beginning and size
of the accessible part of the buffer.\n
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

(defun forward-paragraph-nomark (&optional arg)
  "Deactivate mark; move forward to end of paragraph.
With arg N, do it N times; negative arg -N means move backward N paragraphs.\n
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
is signaled).\n
The command C-x C-n can be used to create
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

(defun scroll-down-nomark (&optional arg)
  "Deactivate mark; scroll down ARG lines; or near full screen if no ARG.
A near full screen is `next-screen-context-lines' less than a full screen.
Negative ARG means scroll upward.
When calling from a program, supply a number as argument or nil."
  (interactive "P")
  (setq mark-active nil)
  (scroll-down arg))

(defun end-of-buffer-nomark (&optional arg)
  "Deactivate mark; move point to the end of the buffer.
With arg N, put point N/10 of the way from the end.\n
If the buffer is narrowed, this command uses the beginning and size
of the accessible part of the buffer.\n
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

(defun backward-paragraph-mark (&optional arg)
  "Ensure mark is active; move backward to start of paragraph.
With arg N, do it N times; negative arg -N means move forward N paragraphs.\n
A paragraph start is the beginning of a line which is a
`first-line-of-paragraph' or which is ordinary text and follows a
paragraph-separating line; except: if the first real line of a
paragraph is preceded by a blank line, the paragraph starts at that
blank line.\n
See `forward-paragraph' for more information."
  (interactive "p")
  (ensure-mark)
  (backward-paragraph arg))

(defun previous-line-mark (&optional arg)
  "Ensure mark is active; move cursor vertically up ARG lines.
If there is no character in the target line exactly over the current column,
the cursor is positioned after the character in that line which spans this
column, or at the end of the line if it is not long enough.\n
The command C-x C-n can be used to create
a semipermanent goal column to which this command always moves.
Then it does not try to move vertically.\n
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
  (scroll-up arg))

(defun beginning-of-buffer-mark (&optional arg)
  "Ensure mark is active; move point to the beginning of the buffer.
With arg N, put point N/10 of the way from the beginning.\n
If the buffer is narrowed, this command uses the beginning and size
of the accessible part of the buffer.\n
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

(defun backward-paragraph-nomark (&optional arg)
  "Deactivate mark; move backward to start of paragraph.
With arg N, do it N times; negative arg -N means move forward N paragraphs.\n
A paragraph start is the beginning of a line which is a
`first-line-of-paragraph' or which is ordinary text and follows a
paragraph-separating line; except: if the first real line of a
paragraph is preceded by a blank line, the paragraph starts at that
blank line.\n
See `forward-paragraph' for more information."
  (interactive "p")
  (setq mark-active nil)
  (backward-paragraph arg))

(defun previous-line-nomark (&optional arg)
  "Deactivate mark; move cursor vertically up ARG lines.
If there is no character in the target line exactly over the current column,
the cursor is positioned after the character in that line which spans this
column, or at the end of the line if it is not long enough.\n
The command C-x C-n can be used to create
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
  (scroll-up arg))

(defun beginning-of-buffer-nomark (&optional arg)
  "Deactivate mark; move point to the beginning of the buffer.
With arg N, put point N/10 of the way from the beginning.\n
If the buffer is narrowed, this command uses the beginning and size
of the accessible part of the buffer.\n
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
  "Change mark behaviour to emulate motif, MAC or MS-Windows cut and paste style.\n
This mode will switch on delete-selection-mode and
transient-mark-mode.\n
The cursor keys (and others) are bound to new functions
which will modify the status of the mark. It will be
possible to select regions with shift-cursorkeys. All this
tries to emulate the look-and-feel of GUIs like motif,
the MAC GUI or MS-Windows (sorry for the last one)."
  (interactive)
  ;;
  ;; keybindings
  ;;

  ;; This is to avoid confusion with the delete-selection-mode
  ;; On simple displays you can't see that a region is active and
  ;; will be deleted on the next keypress. IMHO especially for
  ;; copy-region-as-kill this is confusing
  (define-key global-map "\M-w" 'copy-region-as-kill-nomark) 


  ;; The following keybindings are for standard ISO keyboards
  ;; as they are used with IBM compatible PCs, IBM RS/6000,
  ;; MACs, many X-Stations and probably more
  (define-key global-map [S-right]   'forward-char-mark)
  (define-key global-map [right]     'forward-char-nomark)
  (define-key global-map [C-S-right] 'forward-word-mark)
  (define-key global-map [C-right]   'forward-word-nomark)
  (define-key global-map [M-S-right] 'forward-word-mark)
  (define-key global-map [M-right]   'forward-word-nomark)

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

  (define-key global-map [S-left]    'backward-char-mark)
  (define-key global-map [left]      'backward-char-nomark)
  (define-key global-map [C-S-left]  'backward-word-mark)
  (define-key global-map [C-left]    'backward-word-nomark)
  (define-key global-map [M-S-left]  'backward-word-mark)
  (define-key global-map [M-left]    'backward-word-nomark)

  (define-key global-map [S-up]      'previous-line-mark)
  (define-key global-map [up]        'previous-line-nomark)

  (define-key global-map [S-home]    'beginning-of-line-mark)
  (define-key global-map [home]      'beginning-of-line-nomark)
  (global-set-key [S-C-home]         'beginning-of-buffer-mark)
  (global-set-key [C-home]           'beginning-of-buffer-nomark)
  (global-set-key [S-M-home]         'beginning-of-buffer-mark)
  (global-set-key [M-home]           'beginning-of-buffer-nomark)

  (define-key global-map [S-prior]   'scroll-down-mark)
  (define-key global-map [prior]     'scroll-down-nomark)

  (define-key global-map [S-insert]  'yank)
  (define-key global-map [C-insert]  'copy-region-as-kill)
  (define-key global-map [S-delete]  'kill-region)

  ;; The following bindings are useful on Sun Type 3 keyboards
  ;; They implement the Get-Delete-Put (copy-cut-paste)
  ;; functions from sunview on the L6, L8 and L10 keys
  (define-key global-map [f16]  'yank)
  (define-key global-map [f18]  'copy-region-as-kill)
  (define-key global-map [f20]  'kill-region)

  ;; The following bindings are from Pete Forman.
  ;; I modified them a little to work together with the
  ;; mark functionality I added.

  (global-set-key [f1] 'help)		; KHelp         F1
  (global-set-key [f6] 'other-window)	; KNextPane     F6
  (global-set-key [delete] 'delete-char) ; KDelete       Del
  (global-set-key [C-delete] 'kill-line) ; KEraseEndLine cDel
  (global-set-key [M-backspace] 'undo)	; KUndo         aBS
  (global-set-key [C-down] 'forward-paragraph-nomark) ; KNextPara     cDn
  (global-set-key [C-up] 'backward-paragraph-nomark) ; KPrevPara     cUp
  (global-set-key [S-C-down] 'forward-paragraph-mark)
  (global-set-key [S-C-up] 'backward-paragraph-mark) 

  ;; The following bindings are taken from pc-mode.el
  ;; as suggested by RMS.
  ;; I only used the ones that are not covered above.
  (define-key function-key-map  [M-delete] [?\M-d])
  (global-set-key [C-M-delete]  'kill-sexp)
  (global-set-key [C-backspace] 'backward-kill-word)
  (global-set-key [C-escape]    'list-buffers)

  ;;        
  ;; setup
  ;;
  (setq transient-mark-mode t)
  (setq mark-even-if-inactive t)
  (delete-selection-mode 1))

;;; pc-select.el ends here
