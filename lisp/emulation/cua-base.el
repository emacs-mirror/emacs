;;; cua-base.el --- emulate CUA key bindings

;; Copyright (C) 1997-2002 Free Software Foundation, Inc.

;; Author: Kim F. Storm <storm@cua.dk>
;; Keywords: keyboard emulation convenience cua

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

;; This is the CUA package which provides a complete emulation of the
;; standard CUA key bindings (Motif/Windows/Mac GUI) for selecting and
;; manipulating the region where S-<movement> is used to highlight &
;; extend the region.

;; CUA style key bindings for cut and paste
;; ----------------------------------------

;; This package allows the C-z, C-x, C-c, and C-v keys to be
;; bound appropriately according to the Motif/Windows GUI, i.e.
;;	C-z	-> undo
;;	C-x	-> cut
;;	C-c	-> copy
;;	C-v	-> paste
;;
;; The tricky part is the handling of the C-x and C-c keys which
;; are normally used as prefix keys for most of emacs' built-in
;; commands.  With CUA they still do!!!
;;
;; Only when the region is currently active (and highlighted since
;; transient-mark-mode is used), the C-x and C-c keys will work as CUA
;; keys
;; 	C-x -> cut
;; 	C-c -> copy
;; When the region is not active, C-x and C-c works as prefix keys!
;;
;; This probably sounds strange and difficult to get used to - but
;; based on my own experience and the feedback from many users of
;; this package, it actually works very well and users adapt to it
;; instantly - or at least very quickly.  So give it a try!  
;; ... and in the few cases where you make a mistake and accidentally
;; delete the region - you just undo the mistake (with C-z).
;;
;; If you really need to perform a command which starts with one of
;; the prefix keys even when the region is active, you have three options:
;; - press the prefix key twice very quickly (within 0.2 seconds),
;; - press the prefix key and the following key within 0.2 seconds), or
;; - use the SHIFT key with the prefix key, i.e. C-X or C-C
;;
;; This behaviour can be customized via the
;; cua-prefix-override-inhibit-delay variable.

;; In addition to using the shifted movement keys, you can also use
;; [C-space] to start the region and use unshifted movement keys to extend
;; it. To cancel the region, use [C-space] or [C-g].

;; If you prefer to use the standard emacs cut, copy, paste, and undo
;; bindings, customize cua-enable-cua-keys to nil.


;; Typing text replaces the region
;; -------------------------------

;; When the region is active, i.e. highlighted, the text in region is
;; replaced by the text you type.

;; The replaced text is saved in register 0 which can be inserted using
;; the key sequence M-0 C-v (see the section on register support below).

;; If you have just replaced a highlighted region with typed text,
;; you can repeat the replace with M-v.  This will search forward
;; for a streach of text identical to the previous contents of the
;; region (i.e. the contents of register 0) and replace it with the
;; text you typed to replace the original region.  Repeating M-v will
;; replace the next matching region and so on.
;;
;; Example:  Suppose you have a line like this
;;   The redo operation will redo the last redoable command
;; which you want to change into
;;   The repeat operation will repeat the last repeatable command
;; This is done by highlighting the first occurrence of "redo"
;; and type "repeat" M-v M-v.

;; Note: Since CUA-mode duplicates the functionality of the
;; delete-selection-mode, that mode is automatically disabled when
;; CUA-mode is enabled.


;; CUA mode indications
;; --------------------
;; You can choose to let CUA use different cursor colors to indicate
;; overwrite mode and read-only buffers.  For example, the following
;; setting will use a RED cursor in normal (insertion) mode in
;; read-write buffers, a YELLOW cursor in overwrite mode in read-write
;; buffers, and a GREEN cursor read-only buffers:
;;
;;  (setq cua-normal-cursor-color "red")
;;  (setq cua-overwrite-cursor-color "yellow")
;;  (setq cua-read-only-cursor-color "green")
;;

;; CUA register support
;; --------------------
;; Emacs' standard register support is also based on a separate set of
;; "register commands".
;; 
;; CUA's register support is activated by providing a numeric
;; prefix argument to the C-x, C-c, and C-v commands. For example,
;; to copy the selected region to register 2, enter [M-2 C-c].
;; Or if you have activated the keypad prefix mode, enter [kp-2 C-c].
;; 
;; And CUA will copy and paste normal region as well as rectangles
;; into the registers, i.e. you use exactly the same command for both.
;; 
;; In addition, the last highlighted text that is deleted (not
;; copied), e.g. by [delete] or by typing text over a highlighted
;; region, is automatically saved in register 0, so you can insert it
;; using [M-0 C-v].

;; CUA rectangle support
;; ---------------------
;; Emacs' normal rectangle support is based on interpreting the region
;; between the mark and point as a "virtual rectangle", and using a
;; completely separate set of "rectangle commands" [C-x r ...] on the
;; region to copy, kill, fill a.s.o. the virtual rectangle.
;; 
;; cua-mode's superior rectangle support is based on using a true visual
;; representation of the selected rectangle. To start a rectangle, use
;; [S-return] and extend it using the normal movement keys (up, down,
;; left, right, home, end, C-home, C-end). Once the rectangle has the
;; desired size, you can cut or copy it using C-x and C-c (or C-w and M-w),
;; and you can subsequently insert it - as a rectangle - using C-v (or
;; C-y).  So the only new command you need to know to work with
;; cua-mode rectangles is S-return!
;;
;; Normally, when you paste a rectangle using C-v (C-y), each line of
;; the rectangle is inserted into the existing lines in the buffer.
;; If overwrite-mode is active when you paste a rectangle, it is
;; inserted as normal (multi-line) text.
;; 
;; Furthermore, cua-mode's rectangles are not limited to the actual
;; contents of the buffer, so if the cursor is currently at the end of a
;; short line, you can still extend the rectangle to include more columns
;; of longer lines in the same rectangle.  Sounds strange? Try it!
;; 
;; You can enable padding for just this rectangle by pressing [M-p];
;; this works like entering `picture-mode' where the tabs and spaces
;; are automatically converted/inserted to make the rectangle truly
;; rectangular. Or you can do it for all rectangles by setting the
;; `cua-auto-expand-rectangles' variable.

;; And there's more: If you want to extend or reduce the size of the
;; rectangle in one of the other corners of the rectangle, just use
;; [return] to move the cursor to the "next" corner.  Or you can use
;; the [M-up], [M-down], [M-left], and [M-right] keys to move the
;; entire rectangle overlay (but not the contents) in the given
;; direction.
;;
;; [S-return] cancels the rectangle
;; [C-space] activates the region bounded by the rectangle

;; If you type a normal (self-inserting) character when the rectangle is
;; active, the character is inserted on the "current side" of every line
;; of the rectangle.  The "current side" is the side on which the cursor
;; is currently located. If the rectangle is only 1 column wide,
;; insertion will be performed to the left when the cursor is at the
;; bottom of the rectangle.  So, for example, to comment out an entire
;; paragraph like this one, just place the cursor on the first character
;; of the first line, and enter the following:
;;     S-return M-} ; ; <space>  S-return
 
;; cua-mode's rectangle support also includes all the normal rectangle
;; functions with easy access:
;;
;; [M-a] aligns all words at the left edge of the rectangle
;; [M-b] fills the rectangle with blanks (tabs and spaces)
;; [M-c] closes the rectangle by removing all blanks at the left edge
;;       of the rectangle
;; [M-f] fills the rectangle with a single character (prompt)
;; [M-i] increases the first number found on each line of the rectangle
;;       by the amount given by the numeric prefix argument (default 1)
;;       It recognizes 0x... as hexadecimal numbers
;; [M-k] kills the rectangle as normal multi-line text (for paste)
;; [M-l] downcases the rectangle
;; [M-m] copies the rectangle as normal multi-line text (for paste)
;; [M-n] fills each line of the rectangle with increasing numbers using
;;       a supplied format string (prompt)
;; [M-o] opens the rectangle by moving the highlighted text to the 
;;       right of the rectangle and filling the rectangle with blanks.
;; [M-p] toggles rectangle padding, i.e. insert tabs and spaces to
;;       make rectangles truly rectangular
;; [M-q] performs text filling on the rectangle
;; [M-r] replaces REGEXP (prompt) by STRING (prompt) in rectangle
;; [M-R] reverse the lines in the rectangle
;; [M-s] fills each line of the rectangle with the same STRING (prompt)
;; [M-t] performs text fill of the rectangle with TEXT (prompt)
;; [M-u] upcases the rectangle
;; [M-|] runs shell command on rectangle
;; [M-'] restricts rectangle to lines with CHAR (prompt) at left column
;; [M-/] restricts rectangle to lines matching REGEXP (prompt)
;; [C-?] Shows a brief list of the above commands.

;; [M-C-up] and [M-C-down] scrolls the lines INSIDE the rectangle up
;; and down; lines scrolled outside the top or bottom of the rectangle
;; are lost, but can be recovered using [C-z].

;; CUA Global Mark
;; --------------- 
;; The final feature provided by CUA is the "global mark", which
;; makes it very easy to copy bits and pieces from the same and other
;; files into the current text.  To enable and cancel the global mark,
;; use [S-C-space].  The cursor will blink when the global mark
;; is active.  The following commands behave differently when the global
;; mark is set:
;; <ch>  All characters (including newlines) you type are inserted 
;;       at the global mark!
;; [C-x] If you cut a region or rectangle, it is automatically inserted
;;       at the global mark, and the global mark is advanced.
;; [C-c] If you copy a region or rectangle, it is immediately inserted
;;       at the global mark, and the global mark is advanced.
;; [C-v] Copies a single character to the global mark.
;; [C-d] Moves (i.e. deletes and inserts) a single character to the
;;       global mark.
;; [backspace] deletes the character before the global mark, while
;; [delete] deltes the character after the global mark.

;; [S-C-space] Jumps to and cancels the global mark.
;; [C-u S-C-space] Cancels the global mark (stays in current buffer).

;; [TAB] Indents the current line or rectangle to the column of the
;;       global mark.

;;; Code:

;;; Customization

(defgroup cua nil
  "Emulate CUA key bindings including C-x and C-c."
  :prefix "cua"
  :group 'editing-basics
  :group 'convenience
  :group 'emulations
  :link '(emacs-commentary-link :tag "Commentary" "cua-base.el")
  :link '(emacs-library-link :tag "Lisp File" "cua-base.el"))

;;;###autoload
(defcustom cua-mode nil
  "Non-nil means that CUA emulation mode is enabled.
In CUA mode, shifted movement keys highlight and extend the region.
When a region is highlighted, the binding of the C-x and C-c keys are
temporarily changed to work as Motif, MAC or MS-Windows cut and paste.
Also, insertion commands first delete the region and then insert.
This mode enables Transient Mark mode and it provides a superset of the
PC Selection Mode and Delete Selection Modes.

Setting this variable directly does not take effect;
use either \\[customize] or the function `cua-mode'."
  :set (lambda (symbol value)
	 (cua-mode (or value 0)))
  :initialize 'custom-initialize-default
  :set-after '(cua-enable-modeline-indications cua-use-hyper-key)
  :require 'cua-base
  :link '(emacs-commentary-link "cua-base.el")
  :version "21.4"
  :type 'boolean
  :group 'cua)


(defcustom cua-enable-cua-keys t
  "*Enable using C-z, C-x, C-c, and C-v for undo, cut, copy, and paste.
If the value is t, these mappings are always enabled.  If the value is
'shift, these keys are only enabled if the last region was marked with
a shifted movement key.  If the value is nil, these keys are never
enabled."
  :type '(choice (const :tag "Disabled" nil) 
		 (const :tag "Shift region only" shift)
		 (other :tag "Enabled" t))
  :group 'cua)

(defcustom cua-highlight-region-shift-only nil
  "*If non-nil, only highlight region if marked with S-<move>.
When this is non-nil, CUA toggles `transient-mark-mode' on when the region
is marked using shifted movement keys, and off when the mark is cleared.
But when the mark was set using \\[cua-set-mark], transient-mark-mode
is not turned on."
  :type 'boolean
  :group 'cua)

(defcustom cua-prefix-override-inhibit-delay 
  (if (featurep 'lisp-float-type) (/ (float 1) (float 5)) nil)
  "*If non-nil, time in seconds to delay before overriding prefix key.
If there is additional input within this time, the prefix key is
used as a normal prefix key.  So typing a key sequence quickly will
inhibit overriding the prefix key.
As a special case, if the prefix keys repeated within this time, the
first prefix key is discarded, so typing a prefix key twice in quick
succession will also inhibit overriding the prefix key.
If the value is nil, use a shifted prefix key to inhibit the override."
  :type '(choice (number :tag "Inhibit delay")
		 (const :tag "No delay" nil))
  :group 'cua)

(defcustom cua-keep-region-after-copy nil
  "If non-nil, don't deselect the region after copying."
  :type 'boolean
  :group 'cua)

(defcustom cua-enable-register-prefix 'not-ctrl-u
  "*If non-nil, registers are supported via numeric prefix arg.
If the value is t, any numeric prefix arg in the range 0 to 9 will be
interpreted as a register number. 
If the value is not-ctrl-u, using C-u to enter a numeric prefix is not
interpreted as a register number. 
If the value is ctrl-u-only, only numeric prefix entered with C-u is
interpreted as a register number."
  :type '(choice (const :tag "Disabled" nil) 
		 (const :tag "Enabled, but C-u arg is not a register" not-ctrl-u)
		 (const :tag "Enabled, but only for C-u arg" ctrl-u-only)
		 (other :tag "Enabled" t))
  :group 'cua)

(defcustom cua-delete-copy-to-register-0 t
  "*If non-nil, save last deleted region or rectangle to register 0."
  :type 'boolean
  :group 'cua)

(defcustom cua-use-hyper-key nil
  "*If non-nil, bind rectangle commands to H-? instead of M-?.
If set to 'also, toggle region command is also on S-return.
Must be set prior to enabling CUA."
  :type '(choice (const :tag "Meta key and S-return" nil)
		 (const :tag "Hyper key only" only)
		 (const :tag "Hyper key and S-return" also))
  :group 'cua)

(defcustom cua-enable-region-auto-help nil
  "*If non-nil, automatically show help for active region."
  :type 'boolean
  :group 'cua)

(defcustom cua-enable-modeline-indications nil
  "*If non-nil, use minor-mode hook to show status in mode line."
  :type 'boolean
  :group 'cua)

(defcustom cua-check-pending-input t
  "*If non-nil, don't override prefix key if input pending.
It is rumoured that input-pending-p is unreliable under some window
managers, so try setting this to nil, if prefix override doesn't work."
  :type 'boolean
  :group 'cua)


;;; Rectangle Customization

(defcustom cua-auto-expand-rectangles nil
  "*If non-nil, rectangles are padded with spaces to make straight edges.
This implies modifying buffer contents by expanding tabs and inserting spaces.
Consequently, this is inhibited in read-only buffers.
Can be toggled by [M-p] while the rectangle is active,"
  :type 'boolean
  :group 'cua)

(defcustom cua-enable-rectangle-auto-help t
  "*If non-nil, automatically show help for region, rectangle and global mark."
  :type 'boolean
  :group 'cua)

(defface cua-rectangle-face 'nil
  "*Font used by CUA for highlighting the rectangle."
  :group 'cua)

(defface cua-rectangle-noselect-face 'nil
  "*Font used by CUA for highlighting the non-selected rectangle lines."
  :group 'cua)

(defcustom cua-undo-max 64
  "*Max no of undoable CUA rectangle changes (including undo)."
  :type 'integer
  :group 'cua)


;;; Global Mark Customization

(defcustom cua-global-mark-keep-visible t
  "*If non-nil, always keep global mark visible in other window."
  :type 'boolean
  :group 'cua)

(defface cua-global-mark-face '((((class color)) 
                                  (:foreground "black")
				  (:background "yellow"))
                                 (t (:bold t)))
  "*Font used by CUA for highlighting the global mark."
  :group 'cua)

(defcustom cua-global-mark-blink-cursor-interval 0.20
  "*Blink cursor at this interval when global mark is active."
  :type '(choice (number :tag "Blink interval")
		 (const :tag "No blink" nil))
  :group 'cua)


;;; Cursor Indication Customization

(defcustom cua-enable-cursor-indications nil
  "*If non-nil, use different cursor colors for indications."
  :type 'boolean
  :group 'cua)

(defcustom cua-normal-cursor-color nil
  "Normal (non-overwrite) cursor color.
Also used to indicate that rectangle padding is not in effect.
Automatically loaded from frame parameters, if nil."
  :initialize (lambda (symbol value)
		(set symbol (or value
				(and (boundp 'initial-cursor-color) initial-cursor-color)
				(and (boundp 'initial-frame-alist)
				     (assoc 'cursor-color initial-frame-alist)
				     (cdr (assoc 'cursor-color initial-frame-alist)))
				(and (boundp 'default-frame-alist)
				     (assoc 'cursor-color default-frame-alist)
				     (cdr (assoc 'cursor-color default-frame-alist)))
				(frame-parameter nil 'cursor-color))))
  :type 'color
  :group 'cua)

(defcustom cua-read-only-cursor-color "darkgreen"
  "*Cursor color used in read-only buffers, if non-nil."
  :type 'color
  :group 'cua)

(defcustom cua-overwrite-cursor-color "yellow"
  "*Cursor color used when overwrite mode is set, if non-nil.
Also used to indicate that rectangle padding is in effect."
  :type 'color
  :group 'cua)

(defcustom cua-global-mark-cursor-color "cyan"
  "*Indication for active global mark.
Will change cursor color to specified color if string."
  :type 'color
  :group 'cua)


;;; Rectangle support is in cua-rect.el

(autoload 'cua-set-rectangle-mark "cua-rect" nil t nil)

;; Stub definitions until it is loaded

(when (not (featurep 'cua-rect))
  (defvar cua--rectangle)
  (setq cua--rectangle nil)
  (defvar cua--last-killed-rectangle)
  (setq cua--last-killed-rectangle nil))



;;; Global Mark support is in cua-gmrk.el

(autoload 'cua-toggle-global-mark "cua-gmrk.el" nil t nil)

;; Stub definitions until cua-gmrk.el is loaded

(when (not (featurep 'cua-gmrk))
  (defvar cua--global-mark-active)
  (setq cua--global-mark-active nil))


(provide 'cua-base)

(eval-when-compile
  (require 'cua-rect)
  (require 'cua-gmrk)
  )


;;; Low-level Interface

(defvar cua-inhibit-cua-keys nil
  "Buffer-local variable that may disable the cua keymappings.")
(make-variable-buffer-local 'cua-inhibit-cua-keys)

;;; Aux. variables

;; Current region was started using cua-set-mark.
(defvar cua--explicit-region-start nil)

;; Latest region was started using shifted movement command.
(defvar cua--last-region-shifted nil)

;; buffer + point prior to current command when rectangle is active
;; checked in post-command hook to see if point was moved
(defvar cua--buffer-and-point-before-command nil)

;; status string for mode line indications
(defvar cua--status-string nil)

(defvar cua--debug nil)


;;; Prefix key override mechanism

;; The prefix override (when mark-active) operates in three substates:
;; [1] Before using a prefix key
;; [2] Immediately after using a prefix key
;; [3] A fraction of a second later

;; In state [1], the cua--prefix-override-keymap is active.
;; This keymap binds the C-x and C-c prefix keys to the
;; cua--prefix-override-handler function.

;; When a prefix key is typed in state [1], cua--prefix-override-handler
;; will push back the keys already read to the event queue.  If input is
;; pending, it changes directly to state [3].  Otherwise, a short timer [T]
;; is started, and it changes to state [2].

;; In state [2], the cua--prefix-override-keymap is inactive.  Instead the
;; cua--prefix-repeat-keymap is active.  This keymap binds C-c C-c and C-x
;; C-x to the cua--prefix-repeat-handler function.

;; If the prefix key is repeated in state [2], cua--prefix-repeat-handler
;; will cancel [T], back the keys already read (except for the second prefix
;; keys) to the event queue, and changes to state [3].

;; The basic cua--cua-keys-keymap binds [C-x timeout] to kill-region and
;; [C-c timeout] to copy-region-as-kill, so if [T] times out in state [2],
;; the cua--prefix-override-timeout function will push a `timeout' event on
;; the event queue, and changes to state [3].

;; In state [3] both cua--prefix-override-keymap and cua--prefix-repeat-keymap
;; are inactive, so the timeout in cua-global-keymap binding is used, or the
;; normal prefix key binding from the global or local map will be used.

;; The pre-command hook (executed as a consequence of the timeout or normal
;; prefix key binding) will cancel [T] and change from state [3] back to
;; state [1].  So cua--prefix-override-handler and cua--prefix-repeat-handler
;; are always called with state reset to [1]!

;; State [1] is recognized by cua--prefix-override-timer is nil,
;; state [2] is recognized by cua--prefix-override-timer is a timer, and
;; state [3] is recognized by cua--prefix-override-timer is t.

(defvar cua--prefix-override-timer nil)
(defvar cua--prefix-override-length nil)

(defun cua--prefix-override-replay (arg repeat)
  (let* ((keys (this-command-keys))
	 (i (length keys))
	 (key (aref keys (1- i))))
    (setq cua--prefix-override-length (- i repeat))
    (setq cua--prefix-override-timer
	  (or
	   ;; In state [2], change to state [3]
	   (> repeat 0)
	   ;; In state [1], change directly to state [3]
	   (and cua-check-pending-input (input-pending-p))
	   ;; In state [1], [T] disabled, so change to state [3]
	   (not (numberp cua-prefix-override-inhibit-delay))
	   (<= cua-prefix-override-inhibit-delay 0)
	   ;; In state [1], start [T] and change to state [2]
	   (run-with-timer cua-prefix-override-inhibit-delay nil 
			   'cua--prefix-override-timeout)))
    ;; Don't record this command
    (setq this-command last-command)
    ;; Restore the prefix arg
    (setq prefix-arg arg)
    (reset-this-command-lengths)
    ;; Push the key back on the event queue
    (setq unread-command-events (cons key unread-command-events))))

(defun cua--prefix-override-handler (arg)
  "Start timer waiting for prefix key to be followed by another key.
Repeating prefix key when region is active works as a single prefix key."
  (interactive "P")
  (cua--prefix-override-replay arg 0))

(defun cua--prefix-repeat-handler (arg)
  "Repeating prefix key when region is active works as a single prefix key."
  (interactive "P")
  (cua--prefix-override-replay arg 1))

(defun cua--prefix-copy-handler (arg)
  "Copy region/rectangle, then replay last key."
  (interactive "P")
  (if cua--rectangle
      (cua-copy-rectangle arg)
    (cua-copy-region arg))
  (let ((keys (this-single-command-keys)))
    (setq unread-command-events 
	  (cons (aref keys (1- (length keys))) unread-command-events))))

(defun cua--prefix-cut-handler (arg)
  "Cut region/rectangle, then replay last key."
  (interactive "P")
  (if cua--rectangle
      (cua-cut-rectangle arg)
    (cua-cut-region arg))
  (let ((keys (this-single-command-keys)))
    (setq unread-command-events 
	  (cons (aref keys (1- (length keys))) unread-command-events))))

(defun cua--prefix-override-timeout ()
  (setq cua--prefix-override-timer t)
  (when (= (length (this-command-keys)) cua--prefix-override-length)
    (setq unread-command-events (cons 'timeout unread-command-events))
    (if prefix-arg
      (reset-this-command-lengths)
      (setq overriding-terminal-local-map nil))
    (cua--select-keymaps)))


;;; Aux. functions

(defun cua--fallback ()
  ;; Execute original command
  (setq this-command this-original-command)
  (call-interactively this-command))
  
(defun cua--keep-active ()
  (setq mark-active t
	deactivate-mark nil))

(defun cua--deactivate (&optional now)
  (setq cua--explicit-region-start nil)
  (if (not now)
      (setq deactivate-mark t)
    (setq mark-active nil)
    (run-hooks 'deactivate-mark-hook)))


;; The current register prefix
(defvar cua--register nil)

(defun cua--prefix-arg (arg)
  (setq cua--register  
	(and cua-enable-register-prefix
	     (integerp (this-command-keys))
	     (cond ((eq cua-enable-register-prefix 'not-ctrl-u)
		    (not (= (aref (this-command-keys) 0) ?\C-u)))
		   ((eq cua-enable-register-prefix 'ctrl-u-only)
		    (= (aref (this-command-keys) 0) ?\C-u))
		   (t t))
	     (integerp arg) (>= arg 0) (< arg 10)
	     (+ arg ?0)))
  (if cua--register nil arg))


;;; Enhanced undo - restore rectangle selections

(defun cua-undo (&optional arg)
  "Undo some previous changes.
Knows about CUA rectangle highlighting in addition to standard undo."
  (interactive "*P")
  (if (fboundp 'cua--rectangle-undo)
      (cua--rectangle-undo arg)
    (undo arg)))

;;; Region specific commands

(defun cua-delete-region ()
  "Delete the active region.
Save a copy in register 0 if `cua-delete-copy-to-register-0' is non-nil."
  (interactive)
  (let ((start (mark)) (end (point)))
    (or (<= start end)
	(setq start (prog1 end (setq end start))))
    (if cua-delete-copy-to-register-0
	(copy-to-register ?0 start end nil))
    (delete-region start end)
    (cua--deactivate)))

(defun cua-replace-region ()
  "Replace the active region with the character you type."
  (interactive)
  (cua-delete-region)
  (if (not (eq this-original-command this-command))
      (cua--fallback)))

(defun cua-copy-region (arg)
  "Copy the region to the kill ring.
With numeric prefix arg, copy to register 0-9 instead."
  (interactive "P")
  (setq arg (cua--prefix-arg arg))
  (setq cua--last-killed-rectangle nil)
  (let ((start (mark)) (end (point)))
    (or (<= start end)
	(setq start (prog1 end (setq end start))))
    (if cua--register
	(copy-to-register cua--register start end nil)
      (copy-region-as-kill start end))
    (if cua-keep-region-after-copy
	(cua--keep-active)
      (cua--deactivate))))

(defun cua-cut-region (arg)
  "Cut the region and copy to the kill ring.
With numeric prefix arg, copy to register 0-9 instead."
  (interactive "P")
  (setq cua--last-killed-rectangle nil)
  (if buffer-read-only
      (cua-copy-region arg)
    (setq arg (cua--prefix-arg arg))
    (let ((start (mark)) (end (point)))
      (or (<= start end)
	  (setq start (prog1 end (setq end start))))
      (if cua--register
	  (copy-to-register cua--register start end t)
	(kill-region start end)))
    (cua--deactivate)))

;;; Generic commands for regions, rectangles, and global marks

(defun cua-cancel ()
  "Cancel the active region, rectangle, or global mark."
  (interactive)
  (setq mark-active nil)
  (setq cua--explicit-region-start nil)
  (if (fboundp 'cua--cancel-rectangle)
      (cua--cancel-rectangle)))

(defun cua-paste (arg)
  "Paste last cut or copied region or rectangle.
An active region is deleted before executing the command.
With numeric prefix arg, paste from register 0-9 instead.
If global mark is active, copy from register or one character."
  (interactive "P")
  (setq arg (cua--prefix-arg arg))
  (let ((regtxt (and cua--register (get-register cua--register)))
	(count (prefix-numeric-value arg)))
    (cond
     ((and cua--register (not regtxt))
      (message "Nothing in register %c" cua--register))
     (cua--global-mark-active
      (if regtxt
	  (cua--insert-at-global-mark regtxt)
	(when (not (eobp))
	  (cua--insert-at-global-mark (buffer-substring (point) (+ (point) count)))
	  (forward-char count))))
     (buffer-read-only
      (message "Cannot paste into a read-only buffer"))
     (t
      ;; Must save register here, since delete may override reg 0.
      (if mark-active
	  ;; Before a yank command, make sure we don't yank
	  ;; the same region that we are going to delete.
	  ;; That would make yank a no-op.
	  (if cua--rectangle
	      (cua--delete-rectangle)
	    (if (string= (buffer-substring (point) (mark))
			 (car kill-ring))
		(current-kill 1))
	    (cua-delete-region)))
      (cond
       (regtxt
	(cond
	 ((consp regtxt) (cua--insert-rectangle regtxt))
	 ((stringp regtxt) (insert-for-yank regtxt))
	 (t (message "Unknown data in register %c" cua--register))))
       ((and cua--last-killed-rectangle
	     (eq (and kill-ring (car kill-ring)) (car cua--last-killed-rectangle)))
	(let ((pt (point)))
	  (when (not (eq buffer-undo-list t))
	    (setq this-command 'cua--paste-rectangle)
	    (undo-boundary)
	    (setq buffer-undo-list (cons pt buffer-undo-list)))
	  (cua--insert-rectangle (cdr cua--last-killed-rectangle))
	  (if arg (goto-char pt))))
       (t (yank arg)))))))

(defun cua-paste-pop (arg)
  "Replace a just-pasted text or rectangle with a different text.
See `yank-pop' for details."
  (interactive "P")
  (if (eq last-command 'cua--paste-rectangle)
      (progn
	(undo)
	(yank arg))
    (yank-pop (prefix-numeric-value arg))))

(defun cua-exchange-point-and-mark (arg)
  "Exchanges point and mark, but don't activate the mark.
Activates the mark if a prefix argument is given."
  (interactive "P")
  (if arg
      (setq mark-active t)
    (let (mark-active)
      (exchange-point-and-mark)
      (if cua--rectangle
	  (cua--rectangle-corner 0)))))

;; Typed text that replaced the highlighted region.
(defvar cua--repeat-replace-text nil)

(defun cua-repeat-replace-region (arg)
  "Repeat replacing text of highlighted region with typed text.
Searches for the next streach of text identical to the region last
replaced by typing text over it and replaces it with the same streach
of text.  
Note: Works only when used immediately after typing the last character.
After that, it can be repeated (fairly) reliable until a buffer is
modified in any other way than repeating this command."
  (interactive "P")
  (unless (or (eq this-command last-command)
	      (not cua--repeat-replace-text)
	      (not (eq last-command 'self-insert-command)))
    (setq cua--repeat-replace-text
	  (and (mark t)
	       (/= (point) (mark t))
	       (buffer-substring-no-properties (point) (mark t)))))
  (let ((old (get-register ?0)))
    (if (and old
	     cua--repeat-replace-text
	     (search-forward old nil t nil))
	(replace-match cua--repeat-replace-text arg t))))

(defun cua-help-for-region (&optional help)
  "Show region specific help in echo area."
  (interactive)
  (message 
   (concat (if help "C-?:help " "")
	   "C-z:undo C-x:cut C-c:copy C-v:paste S-ret:rect")))


;;; Shift activated / extended region

(defun cua-set-mark (&optional arg)
  "Set mark at where point is, clear mark, or jump to mark.
With no prefix argument, set mark, push old mark position on local mark
ring, and push mark on global mark ring, or if mark is already set, clear mark.
With argument, jump to mark, and pop a new position for mark off the ring;
then it jumps to the next mark off the ring if repeated with no argument, or
sets the mark at the new position if repeated with argument."
  (interactive "P")
  (cond
   ((eq last-command 'pop-to-mark-command)
    (if (and (consp arg) (> (prefix-numeric-value arg) 4))
	(push-mark-command nil)
      (setq this-command 'pop-to-mark-command)
      (pop-to-mark-command)))
   (arg
    (setq this-command 'pop-to-mark-command)
    (pop-to-mark-command))
   (mark-active
    (cua--deactivate)
    (message "Mark Cleared"))
   (t
    (push-mark-command nil nil)
    (setq cua--explicit-region-start t)
    (setq cua--last-region-shifted nil)
    (if cua-enable-region-auto-help
	(cua-help-for-region t)))))

(defvar cua--standard-movement-commands
  '(forward-char backward-char
    next-line previous-line
    forward-word backward-word
    end-of-line beginning-of-line
    end-of-buffer beginning-of-buffer
    scroll-up scroll-down    forward-paragraph backward-paragraph)
  "List of standard movement commands.
Extra commands should be added to `cua-user-movement-commands'")

(defvar cua-movement-commands nil
  "User may add additional movement commands to this list.")


;;; Cursor indications

(defun cua--update-indications ()
  (let ((cursor
	 (cond
	  ((and cua--global-mark-active
		(stringp cua-global-mark-cursor-color))
	   cua-global-mark-cursor-color)
	  ((and buffer-read-only
		(stringp cua-read-only-cursor-color))
	   cua-read-only-cursor-color)
	  ((and (stringp cua-overwrite-cursor-color)
		(or overwrite-mode
		    (and cua--rectangle (cua--rectangle-padding))))
	   cua-overwrite-cursor-color)
	  (t cua-normal-cursor-color))))
    (if (and cursor
	     (not (equal cursor (frame-parameter nil 'cursor-color))))
	(set-cursor-color cursor))
    cursor))


;;; Pre-command hook

(defun cua--pre-command-handler ()
  (condition-case nil
      (let ((movement (or (memq this-command cua--standard-movement-commands)
			  (memq this-command cua-movement-commands))))

	;; Cancel prefix key timeout if user enters another key.
	(when cua--prefix-override-timer
	  (if (timerp cua--prefix-override-timer)
	      (cancel-timer cua--prefix-override-timer))
	  (setq cua--prefix-override-timer nil))

	;; Handle shifted cursor keys and other movement commands.
	;; If region is not active, region is activated if key is shifted.
	;; If region is active, region is cancelled if key is unshifted (and region not started with C-SPC).
	;; If rectangle is active, expand rectangle in specified direction and ignore the movement.
	(if movement
	    (cond
	     ((memq 'shift (event-modifiers (aref (this-single-command-raw-keys) 0)))
	      (unless mark-active
		(push-mark-command nil t))
	      (setq cua--last-region-shifted t)
	      (setq cua--explicit-region-start nil))
	     ((or cua--explicit-region-start cua--rectangle)
	      (unless mark-active
		(push-mark-command nil nil)))
	     (t
	      ;; If we set mark-active to nil here, the region highlight will not be 
	      ;; removed by the direct_output_ commands.
	      (setq deactivate-mark t)))

	  ;; Handle delete-selection property on other commands
	  (if (and mark-active (not deactivate-mark))
	      (let* ((ds (or (get this-command 'delete-selection)
			     (get this-command 'pending-delete)))
		     (nc (cond
			  ((not ds) nil)
			  ((eq ds 'yank) 
			   'cua-paste)
			  ((eq ds 'kill)
			   (if cua--rectangle
			       'cua-copy-rectangle
			     'cua-copy-region))
			  ((eq ds 'supersede)
			   (if cua--rectangle
			       'cua-delete-rectangle ;; replace?
			     'cua-replace-region))
			  (t
			   (if cua--rectangle
			       'cua-delete-rectangle
			     'cua-delete-region)))))
		(if nc
		    (setq this-original-command this-command
			  this-command nc)))))
	  
	;; Detect extension of rectangles by mouse or other movement
	(setq cua--buffer-and-point-before-command 
	      (if cua--rectangle (cons (current-buffer) (point))))
	)
    (error nil)))

;;; Post-command hook

(defun cua--post-command-handler ()
  (condition-case nil
      (progn
	(when cua--global-mark-active
	  (cua--global-mark-post-command))
	(when (fboundp 'cua--rectangle-post-command)
	  (cua--rectangle-post-command))
	(setq cua--buffer-and-point-before-command nil)
	(if (or (not mark-active) deactivate-mark)
	    (setq cua--explicit-region-start nil))

	;; Debugging
	(if cua--debug
	    (cond 
	     (cua--rectangle (cua--rectangle-assert))
	     (mark-active (message "Mark=%d Point=%d Expl=%s"
				   (mark t) (point) cua--explicit-region-start))))

	;; Disable transient-mark-mode if rectangle active in current buffer.
	(if (not (window-minibuffer-p (selected-window)))
	    (setq transient-mark-mode (and (not cua--rectangle)
					   (if cua-highlight-region-shift-only
					       (not cua--explicit-region-start)
					     t))))
	(if cua-enable-cursor-indications
	    (cua--update-indications))

	(cua--select-keymaps)
	)

    (error nil)))


;;; Keymaps

(defun cua--M/H-key (map key fct)
  ;; bind H-KEY or M-KEY to FCT in MAP
  (if (eq key 'space) (setq key ? ))
  (unless (listp key) (setq key (list key)))
  (define-key map (vector (cons (if cua-use-hyper-key 'hyper 'meta) key)) fct))

(defun cua--self-insert-char-p (def)
  ;; Return DEF if current key sequence is self-inserting in
  ;; global-map.
  (if (memq (global-key-binding (this-single-command-keys))
	    '(self-insert-command self-insert-iso))
      def nil))

(defvar cua-global-keymap (make-sparse-keymap)
  "Global keymap for cua-mode; users may add to this keymap.")

(defvar cua--cua-keys-keymap (make-sparse-keymap))
(defvar cua--prefix-override-keymap (make-sparse-keymap))
(defvar cua--prefix-repeat-keymap (make-sparse-keymap))
(defvar cua--global-mark-keymap (make-sparse-keymap)) ; Initalized when cua-gmrk.el is loaded
(defvar cua--rectangle-keymap (make-sparse-keymap))   ; Initalized when cua-rect.el is loaded
(defvar cua--region-keymap (make-sparse-keymap))

(defvar cua--ena-cua-keys-keymap nil)
(defvar cua--ena-prefix-override-keymap nil)
(defvar cua--ena-prefix-repeat-keymap nil)
(defvar cua--ena-region-keymap nil)
(defvar cua--ena-global-mark-keymap nil)

(defvar cua--keymap-alist
  `((cua--ena-prefix-override-keymap . ,cua--prefix-override-keymap)
    (cua--ena-prefix-repeat-keymap . ,cua--prefix-repeat-keymap)
    (cua--ena-cua-keys-keymap . ,cua--cua-keys-keymap)
    (cua--ena-global-mark-keymap . ,cua--global-mark-keymap)
    (cua--rectangle . ,cua--rectangle-keymap)
    (cua--ena-region-keymap . ,cua--region-keymap)
    (cua-mode . ,cua-global-keymap)))

(defun cua--select-keymaps ()
  ;; Setup conditions for selecting the proper keymaps in cua--keymap-alist.
  (setq cua--ena-region-keymap
	(and mark-active (not deactivate-mark)))
  (setq cua--ena-prefix-override-keymap
	(and cua--ena-region-keymap
	     cua-enable-cua-keys
	     (not cua-inhibit-cua-keys)
	     (or (eq cua-enable-cua-keys t)
		 (not cua--explicit-region-start))
	     (not executing-kbd-macro)
	     (not cua--prefix-override-timer)))
  (setq cua--ena-prefix-repeat-keymap
	(and cua--ena-region-keymap
	     (timerp cua--prefix-override-timer)))
  (setq cua--ena-cua-keys-keymap
	(and cua-enable-cua-keys
	     (not cua-inhibit-cua-keys)
	     (or (eq cua-enable-cua-keys t)
		 cua--last-region-shifted)))
  (setq cua--ena-global-mark-keymap
	(and cua--global-mark-active
	     (not (window-minibuffer-p)))))

(defvar cua--keymaps-initalized nil)

(defun cua--init-keymaps ()
  (unless (eq cua-use-hyper-key 'only)
    (define-key cua-global-keymap [(shift return)]	'cua-set-rectangle-mark))
  (when cua-use-hyper-key
    (cua--M/H-key cua-global-keymap 'space	'cua-set-rectangle-mark)
    (define-key cua-global-keymap [(hyper mouse-1)] 'cua-mouse-set-rectangle-mark))

  (define-key cua-global-keymap [(shift control ? )]	'cua-toggle-global-mark)

  ;; replace region with rectangle or element on kill ring
  (define-key cua-global-keymap [remap yank]		'cua-paste)
  (define-key cua-global-keymap [remap clipboard-yank]	'cua-paste)
  ;; replace current yank with previous kill ring element
  (define-key cua-global-keymap [remap yank-pop]		'cua-paste-pop)
  ;; set mark
  (define-key cua-global-keymap [remap set-mark-command]	'cua-set-mark)
  ;; undo
  (define-key cua-global-keymap [remap undo]		'cua-undo)
  (define-key cua-global-keymap [remap advertised-undo]	'cua-undo)

  (define-key cua--cua-keys-keymap [(control x) timeout] 'kill-region)
  (define-key cua--cua-keys-keymap [(shift control x)] 'Control-X-prefix)
  (define-key cua--cua-keys-keymap [(control c) timeout] 'copy-region-as-kill)
  (define-key cua--cua-keys-keymap [(shift control c)] 'mode-specific-command-prefix)
  (define-key cua--cua-keys-keymap [(control z)] 'undo)
  (define-key cua--cua-keys-keymap [(control v)] 'yank)
  (define-key cua--cua-keys-keymap [(meta v)] 'cua-repeat-replace-region)
  (define-key cua--cua-keys-keymap [remap exchange-point-and-mark] 'cua-exchange-point-and-mark)

  (define-key cua--prefix-override-keymap [(control x)] 'cua--prefix-override-handler)
  (define-key cua--prefix-override-keymap [(control c)] 'cua--prefix-override-handler)
  
  (define-key cua--prefix-repeat-keymap [(control x) (control x)] 'cua--prefix-repeat-handler)
  (define-key cua--prefix-repeat-keymap [(control x) up]    'cua--prefix-cut-handler)
  (define-key cua--prefix-repeat-keymap [(control x) down]  'cua--prefix-cut-handler)
  (define-key cua--prefix-repeat-keymap [(control x) left]  'cua--prefix-cut-handler)
  (define-key cua--prefix-repeat-keymap [(control x) right] 'cua--prefix-cut-handler)
  (define-key cua--prefix-repeat-keymap [(control c) (control c)] 'cua--prefix-repeat-handler)
  (define-key cua--prefix-repeat-keymap [(control c) up]    'cua--prefix-copy-handler)
  (define-key cua--prefix-repeat-keymap [(control c) down]  'cua--prefix-copy-handler)
  (define-key cua--prefix-repeat-keymap [(control c) left]  'cua--prefix-copy-handler)
  (define-key cua--prefix-repeat-keymap [(control c) right] 'cua--prefix-copy-handler)

  ;; replace current region
  (define-key cua--region-keymap [remap self-insert-command]	'cua-replace-region)
  (define-key cua--region-keymap [remap self-insert-iso]	'cua-replace-region)
  (define-key cua--region-keymap [remap insert-register]	'cua-replace-region)
  (define-key cua--region-keymap [remap newline-and-indent]	'cua-replace-region)
  (define-key cua--region-keymap [remap newline]		'cua-replace-region)
  (define-key cua--region-keymap [remap open-line]		'cua-replace-region)
  ;; delete current region
  (define-key cua--region-keymap [remap delete-backward-char]	'cua-delete-region)
  (define-key cua--region-keymap [remap backward-delete-char]	'cua-delete-region)
  (define-key cua--region-keymap [remap backward-delete-char-untabify] 'cua-delete-region)
  (define-key cua--region-keymap [remap delete-char]		'cua-delete-region)
  ;; kill region
  (define-key cua--region-keymap [remap kill-region]		'cua-cut-region)
  ;; copy region
  (define-key cua--region-keymap [remap copy-region-as-kill]	'cua-copy-region)
  (define-key cua--region-keymap [remap kill-ring-save]		'cua-copy-region)
  ;; cancel current region/rectangle
  (define-key cua--region-keymap [remap keyboard-escape-quit]	'cua-cancel)
  (define-key cua--region-keymap [remap keyboard-quit]		'cua-cancel)
  )

;; State prior to enabling cua-mode
;; Value is a list with the following elements:
;;   transient-mark-mode
;;   delete-selection-mode
;;   pc-selection-mode

(defvar cua--saved-state nil)

;;;###autoload
(defun cua-mode (&optional arg)
  "Toggle CUA key-binding mode.
When enabled, using shifted movement keys will activate the region (and
highlight the region using `transient-mark-mode'), and typed text replaces
the active selection.  C-z, C-x, C-c, and C-v will undo, cut, copy, and
paste (in addition to the normal emacs bindings)."
  (interactive "P")
  (setq cua-mode
	(cond
	 ((null arg) (not cua-mode))
	 ((symbolp arg) t)
	 (t (> (prefix-numeric-value arg) 0))))

  (setq mark-even-if-inactive t)
  (setq highlight-nonselected-windows nil)
  (make-variable-buffer-local 'cua--explicit-region-start)
  (make-variable-buffer-local 'cua--status-string)

  (unless cua--keymaps-initalized
    (cua--init-keymaps)
    (setq cua--keymaps-initalized t))

  (if cua-mode
      (progn
	(add-hook 'pre-command-hook 'cua--pre-command-handler)
	(add-hook 'post-command-hook 'cua--post-command-handler)
	(if (and cua-enable-modeline-indications (not (assoc 'cua-mode minor-mode-alist)))
	    (setq minor-mode-alist (cons '(cua-mode cua--status-string) minor-mode-alist)))
	)
    (remove-hook 'pre-command-hook 'cua--pre-command-handler)
    (remove-hook 'post-command-hook 'cua--post-command-handler))

  (if (not cua-mode)
      (setq emulation-mode-map-alists (delq 'cua--keymap-alist emulation-mode-map-alists))
    (add-to-list 'emulation-mode-map-alists 'cua--keymap-alist)
    (cua--select-keymaps))

  (if (fboundp 'cua--rectangle-on-off)
      (cua--rectangle-on-off cua-mode))

  (cond
   (cua-mode
    (setq cua--saved-state
	  (list
	   transient-mark-mode
	   (and (boundp 'delete-selection-mode) delete-selection-mode)
	   (and (boundp 'pc-selection-mode) pc-selection-mode)))
    (if (and (boundp 'delete-selection-mode) delete-selection-mode)
	(delete-selection-mode))
    (if (and (boundp 'pc-selection-mode) pc-selection-mode)
	(pc-selection-mode))
    (setq transient-mark-mode (and cua-mode
				   (if cua-highlight-region-shift-only
				       (not cua--explicit-region-start)
				     t)))
    (if (interactive-p)
	(message "CUA mode enabled")))
   (cua--saved-state
    (setq transient-mark-mode (car cua--saved-state))
    (if (nth 1 cua--saved-state)
	(delete-selection-mode 1))
    (if (nth 2 cua--saved-state)
	(pc-selection-mode 1))
    (if (interactive-p)
	(message "CUA mode disabled.%s%s%s%s"
		 (if (nth 1 cua--saved-state) " Delete-Selection" "")
		 (if (and (nth 1 cua--saved-state) (nth 2 cua--saved-state)) " and" "")
		 (if (nth 2 cua--saved-state) " PC-Selection" "")
		 (if (or (nth 1 cua--saved-state) (nth 2 cua--saved-state)) " enabled" "")))
    (setq cua--saved-state nil))

   (t
    (if (interactive-p)
	(message "CUA mode disabled")))))

(defun cua-debug ()
  "Toggle cua debugging."
  (interactive)
  (setq cua--debug (not cua--debug)))

;;; cua-base.el ends here
