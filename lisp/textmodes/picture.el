;;; picture.el --- "Picture mode" -- editing using quarter-plane screen model

;; Copyright (C) 1985, 1994 Free Software Foundation, Inc.

;; Author: K. Shane Hartman
;; Maintainer: FSF
;; Keywords: convenience wp

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

;; This code provides the picture-mode commands documented in the Emacs
;; manual.  The screen is treated as a semi-infinite quarter-plane with
;; support for rectangle operations and `etch-a-sketch' character
;; insertion in any of eight directions.

;;; Code:

(defgroup picture nil
  "Picture mode --- editing using quarter-plane screen model."
  :prefix "picture-"
  :group 'editing)

(defcustom picture-rectangle-ctl ?+
  "*Character `picture-draw-rectangle' uses for top left corners."
  :type 'character
  :group 'picture)
(defcustom picture-rectangle-ctr ?+
  "*Character `picture-draw-rectangle' uses for top right corners."
  :type 'character
  :group 'picture)
(defcustom picture-rectangle-cbr ?+
  "*Character `picture-draw-rectangle' uses for bottom right corners."
  :type 'character
  :group 'picture)
(defcustom picture-rectangle-cbl ?+
  "*Character `picture-draw-rectangle' uses for bottom left corners."
  :type 'character
  :group 'picture)
(defcustom picture-rectangle-v   ?|
  "*Character `picture-draw-rectangle' uses for vertical lines."
  :type 'character
  :group 'picture)
(defcustom picture-rectangle-h   ?-
  "*Character `picture-draw-rectangle' uses for horizontal lines."
  :type 'character
  :group 'picture)


;; Picture Movement Commands

;; When a cursor is on a wide-column character (e.g. Chinese,
;; Japanese, Korean), this variable tells the desired current column
;; which may be different from (current-column).
(defvar picture-desired-column 0)

;; If the value of picture-desired-column is far from the current
;; column, or if the arg ADJUST-TO-CURRENT is non-nil, set it to the
;; current column.   Return the current column.
(defun picture-update-desired-column (adjust-to-current)
  (let ((current-column (current-column)))
    (if (or adjust-to-current
	    (< picture-desired-column (1- current-column))
	    (> picture-desired-column (1+ current-column)))
	(setq picture-desired-column current-column))
    current-column))

(defun picture-beginning-of-line (&optional arg)
  "Position point at the beginning of the line.
With ARG not nil, move forward ARG - 1 lines first.
If scan reaches end of buffer, stop there without error."
  (interactive "P")
  (if arg (forward-line (1- (prefix-numeric-value arg))))
  (beginning-of-line)
  (setq picture-desired-column 0))

(defun picture-end-of-line (&optional arg)
  "Position point after last non-blank character on current line.
With ARG not nil, move forward ARG - 1 lines first.
If scan reaches end of buffer, stop there without error."
  (interactive "P")
  (if arg (forward-line (1- (prefix-numeric-value arg))))
  (beginning-of-line)
  (skip-chars-backward " \t" (prog1 (point) (end-of-line)))
  (setq picture-desired-column (current-column)))

(defun picture-forward-column (arg &optional interactive)
  "Move cursor right, making whitespace if necessary.
With argument, move that many columns."
  (interactive "p\nd")
  (picture-update-desired-column interactive)
  (setq picture-desired-column (max 0 (+ picture-desired-column arg)))
  (let ((current-column (move-to-column picture-desired-column t)))
    (if (and (> current-column picture-desired-column)
	     (< arg 0))
	;; It seems that we have just tried to move to the right
	;; column of a multi-column character.
	(forward-char -1))))

(defun picture-backward-column (arg &optional interactive)
  "Move cursor left, making whitespace if necessary.
With argument, move that many columns."
  (interactive "p\nd")
  (picture-update-desired-column interactive)
  (picture-forward-column (- arg)))

(defun picture-move-down (arg)
  "Move vertically down, making whitespace if necessary.
With argument, move that many lines."
  (interactive "p")
  (picture-update-desired-column nil)
  (picture-newline arg)
  (let ((current-column (move-to-column picture-desired-column t)))
    (if (> current-column picture-desired-column)
	(forward-char -1))))

(defvar picture-vertical-step 0
  "Amount to move vertically after text character in Picture mode.")

(defvar picture-horizontal-step 1
  "Amount to move horizontally after text character in Picture mode.")

(defun picture-move-up (arg)
  "Move vertically up, making whitespace if necessary.
With argument, move that many lines."
  (interactive "p")
  (picture-update-desired-column nil)
  (picture-move-down (- arg)))

(defun picture-movement-right ()
  "Move right after self-inserting character in Picture mode."
  (interactive)
  (picture-set-motion 0 1))

(defun picture-movement-left ()
  "Move left after self-inserting character in Picture mode."
  (interactive)
  (picture-set-motion 0 -1))

(defun picture-movement-up ()
  "Move up after self-inserting character in Picture mode."
  (interactive)
  (picture-set-motion -1 0))

(defun picture-movement-down ()
  "Move down after self-inserting character in Picture mode."
  (interactive)
  (picture-set-motion 1 0))

(defun picture-movement-nw (&optional arg)
  "Move up and left after self-inserting character in Picture mode.
With prefix argument, move up and two-column left."
  (interactive "P")
  (picture-set-motion -1 (if arg -2 -1)))

(defun picture-movement-ne (&optional arg)
  "Move up and right after self-inserting character in Picture mode.
With prefix argument, move up and two-column right."
  (interactive "P")
  (picture-set-motion -1 (if arg 2 1)))

(defun picture-movement-sw (&optional arg)
  "Move down and left after self-inserting character in Picture mode.
With prefix argument, move down and two-column left."
  (interactive "P")
  (picture-set-motion 1 (if arg -2 -1)))

(defun picture-movement-se (&optional arg)
  "Move down and right after self-inserting character in Picture mode.
With prefix argument, move down and two-column right."
  (interactive "P")
  (picture-set-motion 1 (if arg 2 1)))

(defun picture-set-motion (vert horiz)
  "Set VERTICAL and HORIZONTAL increments for movement in Picture mode.
The mode line is updated to reflect the current direction."
  (setq picture-vertical-step vert
	picture-horizontal-step horiz)
  (setq mode-name
	(format "Picture:%s"
		(nth (+ 2 (% horiz 3) (* 5 (1+ (% vert 2))))
		     '(wnw nw up ne ene Left left none right Right
			   wsw sw down se ese))))
  (force-mode-line-update)
  (message ""))

(defun picture-move ()
  "Move in direction of `picture-vertical-step' and `picture-horizontal-step'."
  (if (/= picture-vertical-step 0)
      (picture-move-down picture-vertical-step))
  (if (/= picture-horizontal-step 0)
      (picture-forward-column picture-horizontal-step)))

(defun picture-motion (arg)
  "Move point in direction of current picture motion in Picture mode.
With ARG do it that many times.  Useful for delineating rectangles in
conjunction with diagonal picture motion.
Do \\[command-apropos]  picture-movement  to see commands which control motion."
  (interactive "p")
  (picture-move-down (* arg picture-vertical-step))
  (picture-forward-column (* arg picture-horizontal-step)))

(defun picture-motion-reverse (arg)
  "Move point in direction opposite of current picture motion in Picture mode.
With ARG do it that many times.  Useful for delineating rectangles in
conjunction with diagonal picture motion.
Do \\[command-apropos] `picture-movement' to see commands which control motion."
  (interactive "p")
  (picture-motion (- arg)))


;; Picture insertion and deletion.

(defun picture-insert (ch arg)
  (let* ((width (char-width ch))
	 ;; We must be sure that the succeeding insertion won't delete
	 ;; the just inserted character.
	 (picture-horizontal-step
	  (if (and (= picture-vertical-step 0)
		   (> width 1)
		   (< (abs picture-horizontal-step) 2))
	      (* picture-horizontal-step 2)
	    picture-horizontal-step)))
    (while (> arg 0)
      (setq arg (1- arg))
      (if (/= picture-desired-column (current-column))
	  (move-to-column picture-desired-column t))
      (let ((col (+ picture-desired-column width)))
	(or (eolp)
	    (let ((pos (point)))
	      (move-to-column col t)
	      (delete-region pos (point)))))
      (insert ch)
      (forward-char -1)
      (picture-move))))

(defun picture-self-insert (arg)
  "Insert this character in place of character previously at the cursor.
The cursor then moves in the direction you previously specified
with the commands `picture-movement-right', `picture-movement-up', etc.
Do \\[command-apropos] `picture-movement' to see those commands."
  (interactive "p")
  (picture-update-desired-column (not (eq this-command last-command)))
  (picture-insert last-command-event arg)) ; Always a character in this case.

(defun picture-clear-column (arg)
  "Clear out ARG columns after point without moving."
  (interactive "p")
  (let* ((original-col (current-column))
	 (target-col (max 0 (+ original-col arg)))
	 pos)
    (move-to-column target-col t)
    (setq pos (point))
    (move-to-column original-col)
    (delete-region pos (point))
    (save-excursion
     (indent-to (max target-col original-col))))
  (setq picture-desired-column (current-column)))

(defun picture-backward-clear-column (arg)
  "Clear out ARG columns before point, moving back over them."
  (interactive "p")
  (picture-clear-column (- arg)))

(defun picture-clear-line (arg)
  "Clear out rest of line; if at end of line, advance to next line.
Cleared-out line text goes into the kill ring, as do newlines that are
advanced over.  With argument, clear out (and save in kill ring) that
many lines."
  (interactive "P")
  (if arg
      (progn
       (setq arg (prefix-numeric-value arg))
       (kill-line arg)
       (newline (if (> arg 0) arg (- arg))))
    (if (looking-at "[ \t]*$")
	(kill-ring-save (point) (progn (forward-line 1) (point)))
      (kill-region (point) (progn (end-of-line) (point))))))

(defun picture-newline (arg)
  "Move to the beginning of the following line.
With argument, moves that many lines (up, if negative argument);
always moves to the beginning of a line."
  (interactive "p")
  (if (< arg 0)
      (forward-line arg)
    (while (> arg 0)
      (end-of-line)
      (if (eobp) (newline) (forward-char 1))
      (setq arg (1- arg)))))

(defun picture-open-line (arg)
  "Insert an empty line after the current line.
With positive argument insert that many lines."
  (interactive "p")
  (save-excursion
   (end-of-line)
   (open-line arg)))

(defun picture-duplicate-line ()
  "Insert a duplicate of the current line, below it."
  (interactive)
  (save-excursion
   (let ((contents
	  (buffer-substring
	   (progn (beginning-of-line) (point))
	   (progn (picture-newline 1) (point)))))
     (forward-line -1)
     (insert contents))))

;; Like replace-match, but overwrites.
(defun picture-replace-match (newtext fixedcase literal)
  (let (ocolumn change pos)
    (goto-char (setq pos (match-end 0)))
    (setq ocolumn (current-column))
    ;; Make the replacement and undo it, to see how it changes the length.
    (let ((buffer-undo-list nil)
	  list1)
      (replace-match newtext fixedcase literal)
      (setq change (- (current-column) ocolumn))
      (setq list1 buffer-undo-list)
      (while list1
	(setq list1 (primitive-undo 1 list1))))
    (goto-char pos)
    (if (> change 0)
	(delete-region (point)
		       (progn
			 (move-to-column (+ change (current-column)) t)
			 (point))))
    (replace-match newtext fixedcase literal)
    (if (< change 0)
	(insert-char ?\ (- change)))))

;; Picture Tabs

(defcustom picture-tab-chars "!-~"
  "*A character set which controls behavior of commands.
\\[picture-set-tab-stops] and \\[picture-tab-search].  It is NOT a
regular expression, any regexp special characters will be quoted.
It defines a set of \"interesting characters\" to look for when setting
\(or searching for) tab stops, initially \"!-~\" (all printing characters).
For example, suppose that you are editing a table which is formatted thus:
| foo		| bar + baz | 23  *
| bubbles	| and + etc | 97  *
and that `picture-tab-chars' is \"|+*\".  Then invoking
\\[picture-set-tab-stops] on either of the previous lines would result
in the following tab stops
		:     :     :     :
Another example - \"A-Za-z0-9\" would produce the tab stops
  :		  :	:     :

Note that if you want the character `-' to be in the set, it must be
included in a range or else appear in a context where it cannot be
taken for indicating a range (e.g. \"-A-Z\" declares the set to be the
letters `A' through `Z' and the character `-').  If you want the
character `\\' in the set it must be preceded by itself: \"\\\\\".

The command \\[picture-tab-search] is defined to move beneath (or to) a
character belonging to this set independent of the tab stops list."
  :type 'string
  :group 'picture)

(defun picture-set-tab-stops (&optional arg)
  "Set value of `tab-stop-list' according to context of this line.
This controls the behavior of \\[picture-tab].  A tab stop is set at
every column occupied by an \"interesting character\" that is preceded
by whitespace.  Interesting characters are defined by the variable
`picture-tab-chars', see its documentation for an example of usage.
With ARG, just (re)set `tab-stop-list' to its default value.  The tab
stops computed are displayed in the minibuffer with `:' at each stop."
  (interactive "P")
  (save-excursion
    (let (tabs)
      (if arg
	  (setq tabs (default-value 'tab-stop-list))
	(let ((regexp (concat "[ \t]+[" (regexp-quote picture-tab-chars) "]")))
	  (beginning-of-line)
	  (let ((bol (point)))
	    (end-of-line)
	    (while (re-search-backward regexp bol t)
	      (skip-chars-forward " \t")
	      (setq tabs (cons (current-column) tabs)))
	    (if (null tabs)
		(error "No characters in set %s on this line"
		       (regexp-quote picture-tab-chars))))))
      (setq tab-stop-list tabs)
      (let ((blurb (make-string (1+ (nth (1- (length tabs)) tabs)) ?\ )))
	(while tabs
	  (aset blurb (car tabs) ?:)
	  (setq tabs (cdr tabs)))
	(message blurb)))))

(defun picture-tab-search (&optional arg)
  "Move to column beneath next interesting char in previous line.
With ARG move to column occupied by next interesting character in this
line.  The character must be preceded by whitespace.
\"interesting characters\" are defined by variable `picture-tab-chars'.
If no such character is found, move to beginning of line."
  (interactive "P")
  (let ((target (current-column)))
    (save-excursion
      (if (and (not arg)
	       (progn
		 (beginning-of-line)
		 (skip-chars-backward
		  (concat "^" (regexp-quote picture-tab-chars))
		  (point-min))
		 (not (bobp))))
	  (move-to-column target))
      (if (re-search-forward
	   (concat "[ \t]+[" (regexp-quote picture-tab-chars) "]")
	   (save-excursion (end-of-line) (point))
	   'move)
	  (setq target (1- (current-column)))
	(setq target nil)))
    (if target
	(move-to-column target t)
      (beginning-of-line))))

(defun picture-tab (&optional arg)
  "Tab transparently (just move point) to next tab stop.
With prefix arg, overwrite the traversed text with spaces.  The tab stop
list can be changed by \\[picture-set-tab-stops] and \\[edit-tab-stops].
See also documentation for variable `picture-tab-chars'."
  (interactive "P")
  (let* ((opoint (point)))
    (move-to-tab-stop)
    (if arg
	(let (indent-tabs-mode
	      (column (current-column)))
	  (delete-region opoint (point))
	  (indent-to column)))))

;; Picture Rectangles

(defvar picture-killed-rectangle nil
  "Rectangle killed or copied by \\[picture-clear-rectangle] in Picture mode.
The contents can be retrieved by \\[picture-yank-rectangle]")

(defun picture-clear-rectangle (start end &optional killp)
  "Clear and save rectangle delineated by point and mark.
The rectangle is saved for yanking by \\[picture-yank-rectangle] and replaced
with whitespace.  The previously saved rectangle, if any, is lost.  With
prefix argument, the rectangle is actually killed, shifting remaining text."
  (interactive "r\nP")
  (setq picture-killed-rectangle (picture-snarf-rectangle start end killp)))

(defun picture-clear-rectangle-to-register (start end register &optional killp)
  "Clear rectangle delineated by point and mark into REGISTER.
The rectangle is saved in REGISTER and replaced with whitespace.  With
prefix argument, the rectangle is actually killed, shifting remaining text."
  (interactive "r\ncRectangle to register: \nP")
  (set-register register (picture-snarf-rectangle start end killp)))

(defun picture-snarf-rectangle (start end &optional killp)
  (let ((column (current-column))
	(indent-tabs-mode nil))
    (prog1 (save-excursion
             (if killp
                 (delete-extract-rectangle start end)
               (prog1 (extract-rectangle start end)
                      (clear-rectangle start end))))
	   (move-to-column column t))))

(defun picture-yank-rectangle (&optional insertp)
  "Overlay rectangle saved by \\[picture-clear-rectangle]
The rectangle is positioned with upper left corner at point, overwriting
existing text.  With prefix argument, the rectangle is inserted instead,
shifting existing text.  Leaves mark at one corner of rectangle and
point at the other (diagonally opposed) corner."
  (interactive "P")
  (if (not (consp picture-killed-rectangle))
      (error "No rectangle saved")
    (picture-insert-rectangle picture-killed-rectangle insertp)))

(defun picture-yank-at-click (click arg)
  "Insert the last killed rectangle at the position clicked on.
Also move point to one end of the text thus inserted (normally the end).
Prefix arguments are interpreted as with \\[yank].
If `mouse-yank-at-point' is non-nil, insert at point
regardless of where you click."
  (interactive "e\nP")
  (or mouse-yank-at-point (mouse-set-point click))
  (picture-yank-rectangle arg))

(defun picture-yank-rectangle-from-register (register &optional insertp)
  "Overlay rectangle saved in REGISTER.
The rectangle is positioned with upper left corner at point, overwriting
existing text.  With prefix argument, the rectangle is
inserted instead, shifting existing text.  Leaves mark at one corner
of rectangle and point at the other (diagonally opposed) corner."
  (interactive "cRectangle from register: \nP")
  (let ((rectangle (get-register register)))
    (if (not (consp rectangle))
	(error "Register %c does not contain a rectangle" register)
      (picture-insert-rectangle rectangle insertp))))

(defun picture-insert-rectangle (rectangle &optional insertp)
  "Overlay RECTANGLE with upper left corner at point.
Optional argument INSERTP, if non-nil causes RECTANGLE to be inserted.
Leaves the region surrounding the rectangle."
  (let ((indent-tabs-mode nil))
    (if (not insertp)
	(save-excursion
	  (delete-rectangle (point)
			    (progn
			      (picture-forward-column (length (car rectangle)))
			      (picture-move-down (1- (length rectangle)))
			      (point)))))
    (push-mark)
    (insert-rectangle rectangle)))

(defun picture-current-line ()
  "Return the vertical position of point.  Top line is 1."
  (+ (count-lines (point-min) (point))
     (if (= (current-column) 0) 1 0)))

(defun picture-draw-rectangle (start end)
  "Draw a rectangle around region."
  (interactive "*r")                    ; start will be less than end
  (let* ((sl     (picture-current-line))
         (sc     (current-column))
         (pvs    picture-vertical-step)
         (phs    picture-horizontal-step)
         (c1     (progn (goto-char start) (current-column)))
         (r1     (picture-current-line))
         (c2     (progn (goto-char end) (current-column)))
         (r2     (picture-current-line))
         (right  (max c1 c2))
         (left   (min c1 c2))
         (top    (min r1 r2))
         (bottom (max r1 r2)))
    (goto-line top)
    (move-to-column left t)
    (picture-update-desired-column t)

    (picture-movement-right)
    (picture-insert picture-rectangle-ctl 1)
    (picture-insert picture-rectangle-h (- right picture-desired-column))

    (picture-movement-down)
    (picture-insert picture-rectangle-ctr 1)
    (picture-insert picture-rectangle-v (- bottom (picture-current-line)))

    (picture-movement-left)
    (picture-insert picture-rectangle-cbr 1)
    (picture-insert picture-rectangle-h (- picture-desired-column left))

    (picture-movement-up)
    (picture-insert picture-rectangle-cbl 1)
    (picture-insert picture-rectangle-v (- (picture-current-line) top))

    (picture-set-motion pvs phs)
    (goto-line sl)
    (move-to-column sc t)))


;; Picture Keymap, entry and exit points.

(defvar picture-mode-map nil)

(defun picture-substitute (oldfun newfun)
  (substitute-key-definition oldfun newfun picture-mode-map global-map))

(if (not picture-mode-map)
    (progn
      (setq picture-mode-map (make-keymap))
      (picture-substitute 'self-insert-command 'picture-self-insert)
      (picture-substitute 'completion-separator-self-insert-command
			  'picture-self-insert)
      (picture-substitute 'completion-separator-self-insert-autofilling
			  'picture-self-insert)
      (picture-substitute 'forward-char 'picture-forward-column)
      (picture-substitute 'backward-char 'picture-backward-column)
      (picture-substitute 'delete-char 'picture-clear-column)
      ;; There are two possibilities for what is normally on DEL.
      (picture-substitute 'backward-delete-char-untabify 'picture-backward-clear-column)
      (picture-substitute 'delete-backward-char 'picture-backward-clear-column)
      (picture-substitute 'kill-line 'picture-clear-line)
      (picture-substitute 'open-line 'picture-open-line)
      (picture-substitute 'newline 'picture-newline)
      (picture-substitute 'newline-and-indent 'picture-duplicate-line)
      (picture-substitute 'next-line 'picture-move-down)
      (picture-substitute 'previous-line 'picture-move-up)
      (picture-substitute 'beginning-of-line 'picture-beginning-of-line)
      (picture-substitute 'end-of-line 'picture-end-of-line)

      (define-key picture-mode-map "\C-c\C-d" 'delete-char)
      (define-key picture-mode-map "\e\t" 'picture-toggle-tab-state)
      (define-key picture-mode-map "\t" 'picture-tab)
      (define-key picture-mode-map "\e\t" 'picture-tab-search)
      (define-key picture-mode-map "\C-c\t" 'picture-set-tab-stops)
      (define-key picture-mode-map "\C-c\C-k" 'picture-clear-rectangle)
      (define-key picture-mode-map "\C-c\C-w" 'picture-clear-rectangle-to-register)
      (define-key picture-mode-map "\C-c\C-y" 'picture-yank-rectangle)
      (define-key picture-mode-map "\C-c\C-x" 'picture-yank-rectangle-from-register)
      (define-key picture-mode-map "\C-c\C-r" 'picture-draw-rectangle)
      (define-key picture-mode-map "\C-c\C-c" 'picture-mode-exit)
      (define-key picture-mode-map "\C-c\C-f" 'picture-motion)
      (define-key picture-mode-map "\C-c\C-b" 'picture-motion-reverse)
      (define-key picture-mode-map "\C-c<" 'picture-movement-left)
      (define-key picture-mode-map "\C-c>" 'picture-movement-right)
      (define-key picture-mode-map "\C-c^" 'picture-movement-up)
      (define-key picture-mode-map "\C-c." 'picture-movement-down)
      (define-key picture-mode-map "\C-c`" 'picture-movement-nw)
      (define-key picture-mode-map "\C-c'" 'picture-movement-ne)
      (define-key picture-mode-map "\C-c/" 'picture-movement-sw)
      (define-key picture-mode-map "\C-c\\" 'picture-movement-se)))

(defcustom picture-mode-hook nil
  "If non-nil, its value is called on entry to Picture mode.
Picture mode is invoked by the command \\[picture-mode]."
  :type 'hook
  :group 'picture)

(defvar picture-mode-old-local-map)
(defvar picture-mode-old-mode-name)
(defvar picture-mode-old-major-mode)
(defvar picture-mode-old-truncate-lines)

;;;###autoload
(defun picture-mode ()
  "Switch to Picture mode, in which a quarter-plane screen model is used.
Printing characters replace instead of inserting themselves with motion
afterwards settable by these commands:
  C-c <	  Move left after insertion.
  C-c >	  Move right after insertion.
  C-c ^	  Move up after insertion.
  C-c .	  Move down after insertion.
  C-c `	  Move northwest (nw) after insertion.
  C-c '	  Move northeast (ne) after insertion.
  C-c /	  Move southwest (sw) after insertion.
  C-c \\   Move southeast (se) after insertion.
  C-u C-c `  Move westnorthwest (wnw) after insertion.
  C-u C-c '  Move eastnortheast (ene) after insertion.
  C-u C-c /  Move westsouthwest (wsw) after insertion.
  C-u C-c \\  Move eastsoutheast (ese) after insertion.
The current direction is displayed in the mode line.  The initial
direction is right.  Whitespace is inserted and tabs are changed to
spaces when required by movement.  You can move around in the buffer
with these commands:
  \\[picture-move-down]	  Move vertically to SAME column in previous line.
  \\[picture-move-up]	  Move vertically to SAME column in next line.
  \\[picture-end-of-line]	  Move to column following last non-whitespace character.
  \\[picture-forward-column]	  Move right inserting spaces if required.
  \\[picture-backward-column]	  Move left changing tabs to spaces if required.
  C-c C-f Move in direction of current picture motion.
  C-c C-b Move in opposite direction of current picture motion.
  Return  Move to beginning of next line.
You can edit tabular text with these commands:
  M-Tab	  Move to column beneath (or at) next interesting character.
	    `Indents' relative to a previous line.
  Tab	  Move to next stop in tab stop list.
  C-c Tab Set tab stops according to context of this line.
	    With ARG resets tab stops to default (global) value.
	    See also documentation of variable	picture-tab-chars
	    which defines \"interesting character\".  You can manually
	    change the tab stop list with command \\[edit-tab-stops].
You can manipulate text with these commands:
  C-d	  Clear (replace) ARG columns after point without moving.
  C-c C-d Delete char at point - the command normally assigned to C-d.
  \\[picture-backward-clear-column]  Clear (replace) ARG columns before point, moving back over them.
  \\[picture-clear-line]	  Clear ARG lines, advancing over them.	 The cleared
	    text is saved in the kill ring.
  \\[picture-open-line]	  Open blank line(s) beneath current line.
You can manipulate rectangles with these commands:
  C-c C-k Clear (or kill) a rectangle and save it.
  C-c C-w Like C-c C-k except rectangle is saved in named register.
  C-c C-y Overlay (or insert) currently saved rectangle at point.
  C-c C-x Like C-c C-y except rectangle is taken from named register.
  C-c C-r Draw a rectangular box around mark and point.
  \\[copy-rectangle-to-register]   Copies a rectangle to a register.
  \\[advertised-undo]   Can undo effects of rectangle overlay commands
	    commands if invoked soon enough.
You can return to the previous mode with:
  C-c C-c Which also strips trailing whitespace from every line.
	    Stripping is suppressed by supplying an argument.

Entry to this mode calls the value of `picture-mode-hook' if non-nil.

Note that Picture mode commands will work outside of Picture mode, but
they are not defaultly assigned to keys."
  (interactive)
  (if (eq major-mode 'picture-mode)
      (error "You are already editing a picture")
    (set (make-local-variable 'picture-mode-old-local-map) (current-local-map))
    (use-local-map picture-mode-map)
    (set (make-local-variable 'picture-mode-old-mode-name) mode-name)
    (set (make-local-variable 'picture-mode-old-major-mode) major-mode)
    (setq major-mode 'picture-mode)
    (set (make-local-variable 'picture-killed-rectangle) nil)
    (set (make-local-variable 'tab-stop-list) (default-value 'tab-stop-list))
    (set (make-local-variable 'picture-tab-chars)
	 (default-value 'picture-tab-chars))
    (make-local-variable 'picture-vertical-step)
    (make-local-variable 'picture-horizontal-step)
    (set (make-local-variable 'picture-mode-old-truncate-lines) truncate-lines)
    (setq truncate-lines t)
    (picture-set-motion 0 1)

    ;; edit-picture-hook is what we used to run, picture-mode-hook is in doc.
    (run-hooks 'edit-picture-hook 'picture-mode-hook)
    (message "Type %s in this buffer to return it to %s mode."
	     (substitute-command-keys "\\[picture-mode-exit]")
	     picture-mode-old-mode-name)))

;;;###autoload
(defalias 'edit-picture 'picture-mode)

(defun picture-mode-exit (&optional nostrip)
  "Undo `picture-mode' and return to previous major mode.
With no argument strips whitespace from end of every line in Picture buffer
  otherwise just return to previous mode."
  (interactive "P")
  (if (not (eq major-mode 'picture-mode))
      (error "You aren't editing a Picture")
    (if (not nostrip) (delete-trailing-whitespace))
    (setq mode-name picture-mode-old-mode-name)
    (use-local-map picture-mode-old-local-map)
    (setq major-mode picture-mode-old-major-mode)
    (kill-local-variable 'tab-stop-list)
    (setq truncate-lines picture-mode-old-truncate-lines)
    (force-mode-line-update)))

(provide 'picture)

;;; picture.el ends here
