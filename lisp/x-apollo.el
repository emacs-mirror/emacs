;;; x-apollo.el --- Apollo support functions
;; Copyright (C) 1995 Free Software Foundation, Inc.

;;; This file is part of GNU Emacs.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Code:

(defun apollo-kill-entire-line ()
  "Kill the entire line containing point."
  (interactive)
  (beginning-of-line)
  (kill-line 1))

(defun apollo-scroll-window-right ()
  "Scroll window to right ten columns."
  (interactive)
  (scroll-left 10))

(defun apollo-scroll-window-left ()
  "Scroll window to left ten columns."
  (interactive)
  (scroll-right 10))

(defun apollo-scroll-window-forward-line ()
  "Move window forward one line leaving cursor at position in window."
  (interactive)
  (scroll-up 1))

(defun apollo-scroll-window-backward-line ()
  "Move window backward one line leaving cursor at position in window."
  (interactive)
  (scroll-down 1))

;;; Define and Enable the Function Key Bindings.

(global-set-key [S-tab] "\C-i")	;Shift TAB
(global-set-key [C-tab] "\C-i")	;Control TAB
(global-set-key [S-return] "\C-m") ;Shift RET
(global-set-key [C-return] "\C-m") ;Control RET
(global-set-key [linedel] 'apollo-kill-entire-line) ;LINE DEL
(global-set-key [chardel] 'delete-char) ;CHAR DEL
(global-set-key [leftbar] 'beginning-of-line) ;LEFT BAR ARROW
(global-set-key [rightbar] 'end-of-line) ;RIGHT BAR ARROW
(global-set-key [leftbox] 'apollo-scroll-window-left) ;LEFT BOX ARROW
(global-set-key [rightbox] 'apollo-scroll-window-right) ;RIGHT BOX ARROW
(global-set-key [S-up] 'apollo-scroll-window-backward-line) ;Shift UP ARROW
(global-set-key [S-down] 'apollo-scroll-window-forward-line) ;Shift DOWN ARROW
(global-set-key [select] 'set-mark-command) ;MARK
(global-set-key [S-insert] 'overwrite-mode) ;INS MODE
(global-set-key [S-linedel] 'yank) ;Shift LINE DEL
(global-set-key [S-chardel] 'delete-char)	;Shift CHAR DEL
(global-set-key [copy] 'copy-region-as-kill) ;COPY
(global-set-key [S-cut] 'kill-region) ;CUT
(global-set-key [paste] 'yank) ;PASTE
(global-set-key [S-undo] 'undo)	;UNDO
(global-set-key [S-left] 'backward-word) ;Shift LEFT ARROW
(global-set-key [S-right] 'forward-word) ;Shift RIGHT ARROW
(global-set-key [upbox] 'scroll-down) ;UP BOX ARROW
(global-set-key [S-upbox] 'beginning-of-buffer) ;Shift UP BOX ARROW
(global-set-key [downbox] 'scroll-up) ;DOWN BOX ARROW
(global-set-key [S-downbox] 'end-of-buffer) ;Shift DOWN BOX ARROW
(global-set-key [S-redo] 'toggle-read-only) ;Shift AGAIN
(global-set-key [exit] 'save-buffer) ;EXIT
(global-set-key [S-cancel] 'kill-buffer) ;ABORT
(global-set-key [S-save] 'save-buffer) ;SAVE
(global-set-key [S-leftbar] 'beginning-of-buffer) ;Shift LEFT BAR ARROW
(global-set-key [cmd] 'execute-extended-command) ;CMD
(global-set-key [S-rightbar] 'end-of-buffer) ;Shift RIGHT BAR ARROW
(global-set-key [next] 'other-window) ;NEXT WNDW
(global-set-key [S-next] 'delete-window) ;Shift NEXT WNDW
(global-set-key [read] 'find-file-read-only) ;READ
(global-set-key [edit] 'find-file) ;EDIT
(global-set-key [S-shell] 'shell) ;SHELL
(global-set-key [S-help] 'manual-entry) ;HELP
