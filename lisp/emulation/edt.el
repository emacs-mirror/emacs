;;; edt.el --- EDT emulation in Emacs

;; Copyright (C) 1986 Free Software Foundation, Inc.

;; Author: Mike Clarkson <mike@yetti.UUCP>
;; Maintainer: FSF
;; Created: 27 Aug 1986
;; Keywords: emulations

;;  This started from public domain code by Mike Clarkson
;;  but has been greatly altered.

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

;; Here's my EDT emulation for GNU Emacs that is based on the EDT emulation
;; for Gosling's Emacs sent out on the net a couple of years ago by Lynn Olson
;; at Tektronics.  This emulation was widely distributed as the file edt.ml
;; in the maclib directory of most Emacs distributions.
;;      
;; I will gladly take all criticisms and complaints to heart, and will fix what
;; bugs I can find.  As this is my first Emacs Lisp hack, you may have to root
;; out a few nasties hidden in the code.  Please let me know if you find any
;; (sorry, no rewards :-).  I would also be interested if there are better,
;; cleaner, faster ways of doing some of the things that I have done.
;;      
;; You must understand some design considerations that I had in mind.
;; The intention was not really to "emulate" EDT, but rather to take advantage
;; of the years of EDT experience that had accumulated in my right hand,
;; while at the same time taking advantage of EMACS.
;;      
;; Some major differences are:
;;      
;; HELP            is describe-key;
;; GOLD/HELP       is describe-function;
;; FIND            is isearch-forward/backward;
;; GOLD/HELP       is occur-menu, which finds all instances of a search string;
;; ENTER           is other-window;
;; SUBS            is subprocess-command.  Note that you have to change this
;;                 to `shell' if you are running Un*x;
;; PAGE            is next-paragraph, because that's more useful than page.
;; SPECINS         is copy-to-killring;
;; GOLD/GOLD       is mark-section-wisely, which is my command to mark the
;;                 section in a manner consistent with the major-mode.  It
;;                 uses mark-defun for emacs-lisp, lisp, mark-c-function for C,
;;                 and mark-paragraph for other modes.
;;      
;;      
;; Some subtle differences are:
;;      
;; APPEND          is append-to-buffer.  One doesn't append to the kill ring
;;                 much and SPECINS is now copy-to-killring;
;; REPLACE         is replace-regexp;
;; FILL            is fill-region-wisely, which uses indent-region for C, lisp
;;                 emacs-lisp, and fill-region for others. It asks if you
;;                 really want to fill-region in TeX-mode, because I find this
;;                 to be very dangerous.
;; CHNGCASE        is case-flip for the character under the cursor only.
;;                 I felt that case-flip region is unlikely, as usually you
;;                 upcase-region or downcase region.  Also, unlike EDT it
;;                 is independent of the direction you are going, as that
;;                 drives me nuts.
;;      
;; I use Emacs definition of what a word is.  This is considerably different
;; from what EDT thinks a word is.  This is not good for dyed-in-the-wool EDT
;; fans, but is probably preferable for experienced Emacs users.  My assumption
;; is that the former are a dying breed now that GNU Emacs has made it to VMS,
;; but let me know how you feel.  Also, when you undelete a word it leave the
;; point at the end of the undeleted text, rather than the beginning.  I might
;; change this as I'm not sure if I like this or not. I'm also not sure if I
;; want it to set the mark each time you delete a character or word.
;;      
;; Backspace does not invoke beginning-of-line, because ^H is the help prefix,
;; and I felt it should be left as such.  You can change this if you like.
;;      
;; The ADVANCE and BACKUP keys do not work as terminators for forward or
;; backward searches. In Emacs, all search strings are terminated by return.
;; The searches will however go forward or backward depending on your current
;; direction.  Also, when you change directions, the mode line will not be
;; updated immediately, but only when you next execute an emacs function.
;; Personally, I consider this to be a bug, not a feature.
;;      
;; This should also work with VT-2xx's, though I haven't tested it extensively
;; on those terminals.  It assumes that the CSI-map of vt_200.el has been
;; defined.
;;      
;; There are also a whole bunch of GOLD letter, and GOLD character bindings:
;; look at edtdoc.el for them, or better still, look at the edt.el lisp code,
;; because after all, in the true Lisp tradition, the source code is *assumed*
;; to be self-documenting :-)
;;      
;; Mike Clarkson,            ...!allegra \             BITNET:  mike@YUYETTI or
;; CRESS, York University,   ...!decvax   \                 SYMALG@YUSOL
;; 4700 Keele Street,        ...!ihnp4     > !utzoo!yetti!mike
;; North York, Ontario,      ...!linus    /
;; CANADA M3J 1P3.           ...!watmath /      Phone: +1 (416) 736-2100 x 7767
;;      
;; Note that I am not on ARPA, and must gateway any ARPA mail through BITNET or
;; UUCP.  If you have a UUCP or BITNET address please use it for communication
;; so that I can reach you directly.  If you have both, the BITNET address
;; is preferred.

;;; Code:

(defvar edt-last-deleted-lines ""
  "Last text deleted by an EDT emulation `line-delete' command.")
(defvar edt-last-deleted-words ""
  "Last text deleted by an EDT emulation `word-delete' command.")
(defvar edt-last-deleted-chars ""
  "Last text deleted by an EDT emulation `character-delete' command.")

(defun delete-current-line (num)
  "Delete one or specified number of lines after point.
This includes the newline character at the end of each line.
They are saved for the EDT `undelete-lines' command."
  (interactive "p")
  (let ((beg (point)))
    (forward-line num)
    (if (not (eq (preceding-char) ?\n))
	(insert "\n"))
    (setq edt-last-deleted-lines
	  (buffer-substring beg (point)))
    (delete-region beg (point))))

(defun delete-to-eol (num)
  "Delete text up to end of line.
With argument, delete up to to Nth line-end past point.
They are saved for the EDT `undelete-lines' command."
  (interactive "p")
  (let ((beg (point)))
    (forward-char 1)
    (end-of-line num)
    (setq edt-last-deleted-lines
	  (buffer-substring beg (point)))
    (delete-region beg (point))))

(defun delete-current-word (num)
  "Delete one or specified number of words after point.
They are saved for the EDT `undelete-words' command."
  (interactive "p")
  (let ((beg (point)))
    (forward-word num)
    (setq edt-last-deleted-words
	  (buffer-substring beg (point)))
    (delete-region beg (point))))

(defun edt-delete-previous-word (num)
  "Delete one or specified number of words before point.
They are saved for the EDT `undelete-words' command."
  (interactive "p")
  (let ((beg (point)))
    (forward-word (- num))
    (setq edt-last-deleted-words
	  (buffer-substring (point) beg))
    (delete-region beg (point))))

(defun delete-current-char (num)
  "Delete one or specified number of characters after point.
They are saved for the EDT `undelete-chars' command."
  (interactive "p")
  (setq edt-last-deleted-chars
	(buffer-substring (point) (min (point-max) (+ (point) num))))
  (delete-region (point) (min (point-max) (+ (point) num))))

(defun delete-previous-char (num)
  "Delete one or specified number of characters before point.
They are saved for the EDT `undelete-chars' command."
  (interactive "p")
  (setq edt-last-deleted-chars
	(buffer-substring (max (point-min) (- (point) num)) (point)))
  (delete-region (max (point-min) (- (point) num)) (point)))

(defun undelete-lines ()
  "Yank lines deleted by last EDT `line-delete' command."
  (interactive)
  (insert edt-last-deleted-lines))

(defun undelete-words ()
  "Yank words deleted by last EDT `word-delete' command."
  (interactive)
  (insert edt-last-deleted-words))

(defun undelete-chars ()
  "Yank characters deleted by last EDT `character-delete' command."
  (interactive)
  (insert edt-last-deleted-chars))

(defun next-end-of-line (num)
  "Move to end of line; if at end, move to end of next line.
Accepts a prefix argument for the number of lines to move."
  (interactive "p")
  (forward-char)
  (end-of-line num))

(defun previous-end-of-line (num)
  "Move EOL upward.
Accepts a prefix argument for the number of lines to move."
  (interactive "p")
  (end-of-line (- 1 num)))

(defun forward-to-word (num)
  "Move to next word-beginning, or to Nth following word-beginning."
  (interactive "p")
  (forward-word (1+ num))
  (forward-word -1))

(defun backward-to-word (num)
  "Move back to word-end, or to Nth word-end seen."
  (interactive "p")
  (forward-word (- (1+ num)))
  (forward-word 1))

(defun backward-line (num)
  "Move point to start of previous line.
Prefix argument serves as repeat-count."
  (interactive "p")
  (forward-line (- num)))

(defun scroll-window-down (num)
  "Scroll the display down a window-full.
Accepts a prefix argument for the number of window-fulls to scroll."
  (interactive "p")
  (scroll-down (- (* (window-height) num) 2)))

(defun scroll-window-up (num)
  "Scroll the display up a window-full.
Accepts a prefix argument for the number of window-fulls to scroll."
  (interactive "p")
  (scroll-up (- (* (window-height) num) 2)))

(defun next-paragraph (num)
  "Move to beginning of the next indented paragraph.
Accepts a prefix argument for the number of paragraphs."
  (interactive "p")
  (while (> num 0)
    (next-line 1)
    (forward-paragraph)
    (previous-line 1)
    (if (eolp) (next-line 1))
    (setq num (1- num))))

(defun previous-paragraph (num)
  "Move to beginning of previous indented paragraph.
Accepts a prefix argument for the number of paragraphs."
  (interactive "p")
  (while (> num 0)
    (backward-paragraph)
    (previous-line 1)
    (if (eolp) (next-line 1))
    (setq num (1- num))))

(defun move-to-beginning ()
  "Move cursor to the beginning of buffer, but don't set the mark."
  (interactive)
  (goto-char (point-min)))

(defun move-to-end ()
  "Move cursor to the end of buffer, but don't set the mark."
  (interactive)
  (goto-char (point-max)))

(defun goto-percent (perc)
  "Move point to ARG percentage of the buffer."
  (interactive "NGoto-percentage: ")
  (if (or (> perc 100) (< perc 0))
      (error "Percentage %d out of range 0 < percent < 100" perc)
    (goto-char (/ (* (point-max) perc) 100))))

(defun update-mode-line ()
  "Ensure mode-line reflects all changes."
  (set-buffer-modified-p (buffer-modified-p))
  (sit-for 0))

(defun advance-direction ()
  "Set EDT Advance mode so keypad commands move forward."
  (interactive)
  (setq edt-direction-string " ADVANCE")
  (global-set-key [kp-f3] 'isearch-forward)
  (global-set-key [kp-8] 'scroll-window-up)
  (global-set-key [kp-7] 'next-paragraph)
  (global-set-key [kp-1] 'forward-to-word)
  (global-set-key [kp-2] 'next-end-of-line)
  (global-set-key [kp-3] 'forward-char)
  (global-set-key [kp-0] 'forward-line)
  (update-mode-line))

(defun backup-direction ()
  "Set EDT Backup mode so keypad commands move backward."
  (interactive)
  (setq edt-direction-string " BACKUP")
  (global-set-key [kp-f3] 'isearch-backward)
  (global-set-key [kp-8] 'scroll-window-down)
  (global-set-key [kp-7] 'previous-paragraph)
  (global-set-key [kp-1] 'backward-to-word)
  (global-set-key [kp-2] 'previous-end-of-line)
  (global-set-key [kp-3] 'backward-char)
  (global-set-key [kp-9] 'backward-line)
  (update-mode-line))

(defun edt-beginning-of-window ()
  "Home cursor to top of window."
  (interactive)
  (move-to-window-line 0))

(defun edt-line-to-bottom-of-window ()
  "Move the current line to the top of the window."
  (interactive)
  (recenter -1))

(defun edt-line-to-top-of-window ()
  "Move the current line to the top of the window."
  (interactive)
  (recenter 0))

(defun case-flip-character (num)
  "Change the case of the character under the cursor.
Accepts a prefix argument of the number of characters to invert."
  (interactive "p")
  (while (> num 0)
    (funcall (if (<= ?a (following-char))
		 'upcase-region 'downcase-region)
	     (point) (1+ (point)))
    (forward-char 1)
    (setq num (1- num))))

(defun indent-or-fill-region ()
  "Fill region in text modes, indent region in programming language modes."
  (interactive)
  (if (string= paragraph-start "^$\\|^")
      (indent-region (point) (mark) nil)
    (fill-region (point) (mark))))

(defun mark-section-wisely ()
  "Mark the section in a manner consistent with the major-mode.
Uses mark-defun for emacs-lisp, lisp,
mark-c-function for C,
and mark-paragraph for other modes."
  (interactive)
  (cond  ((eq major-mode 'emacs-lisp-mode)
	  (mark-defun))
	 ((eq major-mode 'lisp-mode)
	  (mark-defun))
	 ((eq major-mode 'c-mode)
	  (mark-c-function))
	 (t (mark-paragraph))))

;;; Key Bindings
;;;###autoload
(defun edt-emulation-on ()
  "Emulate DEC's EDT editor.
Note that many keys are rebound; including nearly all keypad keys.
Use \\[edt-emulation-off] to undo all rebindings except the keypad keys."
  (interactive)
  (advance-direction)
  (edt-bind-gold-keypad)
  (setq edt-mode-old-c-\\ (lookup-key global-map "\C-\\"))
  (global-set-key "\C-\\" 'quoted-insert)
  (setq edt-mode-old-delete (lookup-key global-map "\177"))
  (global-set-key "\177" 'delete-previous-char)      ;"Delete"
  (setq edt-mode-old-lisp-delete (lookup-key emacs-lisp-mode-map "\177"))
  (define-key emacs-lisp-mode-map "\177" 'delete-previous-char) ;"Delete"
  (define-key lisp-mode-map "\177" 'delete-previous-char) ;"Delete"
  (setq edt-mode-old-linefeed (lookup-key global-map "\C-j"))
  (global-set-key "\C-j" 'edt-delete-previous-word)           ;"LineFeed"
  (define-key esc-map "?" 'apropos))                      ;"<ESC>?"

(defun edt-emulation-off ()
  "Return from EDT emulation to normal Emacs key bindings.
The keys redefined by \\[edt-emulation-on] are given their old definitions."
  (interactive)
  (setq edt-direction-string nil)
  (global-set-key "\C-\\" edt-mode-old-c-\\)
  (global-set-key "\177" edt-mode-old-delete)		;"Delete"
  (define-key emacs-lisp-mode-map "\177" edt-mode-old-lisp-delete) ;"Delete"
  (define-key lisp-mode-map "\177" edt-mode-old-lisp-delete) ;"Delete"
  (global-set-key "\C-j" edt-mode-old-linefeed))           ;"LineFeed"

(defvar GOLD-map (make-keymap)
   "`GOLD-map' maps the function keys on the VT100 keyboard preceded
by the PF1 key.  GOLD is the ASCII the 7-bit escape sequence <ESC>OP.")

(defalias 'GOLD-prefix GOLD-map)

(global-set-key [home] 'edt-beginning-of-window)
(global-set-key [kp-f2] 'describe-key)
(global-set-key [kp-f4] 'delete-current-line)
(global-set-key [kp-9] 'append-to-buffer)
(global-set-key [kp-subtract] 'delete-current-word)
(global-set-key [kp-4] 'advance-direction)
(global-set-key [kp-5] 'backup-direction)
(global-set-key [kp-6] 'kill-region)
(global-set-key [kp-separator] 'delete-current-char)
(global-set-key [kp-decimal] 'set-mark-command)
(global-set-key [kp-enter] 'other-window)
(global-set-key [kp-f1] 'GOLD-prefix)

;;Bind GOLD/Keyboard keys

(define-key GOLD-map "\C-g"  'keyboard-quit)            ; just for safety
(define-key GOLD-map "\177" 'delete-window)             ;"Delete"
(define-key GOLD-map "\C-h" 'delete-other-windows)      ;"BackSpace"
(define-key GOLD-map "\C-m" 'newline-and-indent)        ;"Return"
(define-key GOLD-map " " 'undo)				;"Spacebar"
(define-key GOLD-map "%" 'goto-percent)                 ; "%"
(define-key GOLD-map "=" 'goto-line)                    ; "="
(define-key GOLD-map "`" 'what-line)                    ; "`"
(define-key GOLD-map "\C-\\" 'split-window-vertically)  ; "Control-\"

; GOLD letter combinations:
(define-key GOLD-map "b" 'buffer-menu)                  ; "b"
(define-key GOLD-map "B" 'buffer-menu)                  ; "B"
(define-key GOLD-map "d" 'delete-window)                ; "d"
(define-key GOLD-map "D" 'delete-window)                ; "D"
(define-key GOLD-map "e" 'compile)                      ; "e"
(define-key GOLD-map "E" 'compile)                      ; "E"
(define-key GOLD-map "i" 'insert-file)                  ; "i"
(define-key GOLD-map "I" 'insert-file)                  ; "I"
(define-key GOLD-map "l" 'goto-line)                    ; "l"
(define-key GOLD-map "L" 'goto-line)                    ; "L"
(define-key GOLD-map "m" 'save-some-buffers)		; "m"
(define-key GOLD-map "M" 'save-some-buffers)		; "m"
(define-key GOLD-map "n" 'next-error)                           ; "n"
(define-key GOLD-map "N" 'next-error)                           ; "N"
(define-key GOLD-map "o" 'switch-to-buffer-other-window)        ; "o"
(define-key GOLD-map "O" 'switch-to-buffer-other-window)        ; "O"
(define-key GOLD-map "r" 'revert-file)                          ; "r"
(define-key GOLD-map "r" 'revert-file)                          ; "R"
(define-key GOLD-map "s" 'save-buffer)                          ; "s"
(define-key GOLD-map "S" 'save-buffer)                          ; "S"
(define-key GOLD-map "v" 'find-file-other-window)               ; "v"
(define-key GOLD-map "V" 'find-file-other-window)               ; "V"
(define-key GOLD-map "w" 'write-file)                           ; "w"
(define-key GOLD-map "w" 'write-file)                           ; "W"
;(define-key GOLD-map "z" 'shrink-window)                 ; "z"
;(define-key GOLD-map "Z" 'shrink-window)                 ; "z"

;Bind GOLD/Keypad keys
(defun edt-bind-gold-keypad ()
  (define-key GOLD-map [up] 'edt-line-to-top-of-window)
  (define-key GOLD-map [down] 'edt-line-to-bottom-of-window)
  (define-key GOLD-map [left] 'backward-sentence)
  (define-key GOLD-map [right] 'forward-sentence)
  (define-key GOLD-map [kp-f1] 'mark-section-wisely)
  (define-key GOLD-map [kp-f2] 'describe-function)
  (define-key GOLD-map [kp-f3] 'occur)
  (define-key GOLD-map [kp-f4] 'undelete-lines)
  (define-key GOLD-map [kp-0] 'open-line)
  (define-key GOLD-map [kp-1] 'case-flip-character)
  (define-key GOLD-map [kp-2] 'delete-to-eol)
  (define-key GOLD-map [kp-3] 'copy-region-as-kill)
  (define-key GOLD-map [kp-4] 'move-to-end)
  (define-key GOLD-map [kp-5] 'move-to-beginning)
  (define-key GOLD-map [kp-6] 'yank)
  (define-key GOLD-map [kp-7] 'execute-extended-command)
  (define-key GOLD-map [kp-8] 'indent-or-fill-region)
  (define-key GOLD-map [kp-9] 'replace-regexp)
  (define-key GOLD-map [kp-subtract] 'undelete-words)
  (define-key GOLD-map [kp-separator] 'undelete-chars)
  (define-key GOLD-map [kp-decimal] 'redraw-display)
  (define-key GOLD-map [kp-enter] 'shell-command))

;; Make direction of motion show in mode line
;; while EDT emulation is turned on.
;; Note that the keypad is always turned on when in Emacs.

(or (assq 'edt-direction-string minor-mode-alist)
    (setq minor-mode-alist (cons '(edt-direction-string edt-direction-string)
				 minor-mode-alist)))

;;; edt.el ends here
