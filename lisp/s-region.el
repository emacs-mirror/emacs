;;; s-region.el --- set region using shift key.
;;; Copyright (C) 1994 Free Software Foundation, Inc.

;; Author: Morten Welinder (terra@diku.dk)
;; Version: 1.00
;; Keywords: terminals
;; Favourite-brand-of-beer: None, I hate beer.

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

;; Having loaded this code you can set the region by holding down the
;; shift key and move the cursor to the other end of the region.  The
;; functionallity provided by this code is similar to that provided by
;; the editors of Borland International's compilers for ms-dos.

;; Currently, s-region-move may be bound only to events that are vectors
;; of length one and whose last element is a symbol.  Also, the functions
;; that are given this kind of overlay should be (interactive "p") 
;; functions.

;; If the following keys are not already bound then...
;; C-insert is bound to copy-region-as-kill
;; S-delete is bound to kill-region
;; S-insert is bound to yank

;;; Code:

(defvar s-region-overlay (make-overlay 1 1))
(overlay-put s-region-overlay 'face 'region)
(overlay-put s-region-overlay 'priority 1000000) ; for hilit19

(defun s-region-unshift (key)
  "Remove shift modifier from last keypress KEY and return that as a key."
  (if (vectorp key)
      (let ((last (aref key (1- (length key)))))
	(if (symbolp last)
	    (let* ((keyname (symbol-name last))
		   (pos (string-match "S-" keyname)))
	      (if pos
		  ;; We skip all initial parts of the event assuming that
		  ;; those are setting up the prefix argument to the command.
		  (vector (intern (concat (substring keyname 0 pos)
					  (substring keyname (+ 2 pos)))))
		(error "Non-shifted key: %S" key)))
	  (error "Key does not end in a symbol: %S" key)))
    (error "Non-vector key: %S" key)))

(defun s-region-move (&rest arg)
  "This is an overlay function to point-moving keys."
  (interactive "p")
  (if (if mark-active (not (equal last-command 's-region-move)) t)
      (set-mark-command nil)
    (message "")) ; delete the "Mark set" message
  (apply (key-binding (s-region-unshift (this-command-keys))) arg)
  (move-overlay s-region-overlay (mark) (point) (current-buffer))
  (sit-for 1)
  (delete-overlay s-region-overlay))

(defun s-region-bind (keylist &optional map)
  "Bind keys in KEYLIST to `s-region-move'.
Each key in KEYLIST is bound to `s-region-move'
provided it is already bound to some command or other.
Optional second argument MAP specifies keymap to
add binding to, defaulting to global keymap."
  (or map (setq map global-map))
  (while keylist
    (if (commandp (key-binding (car keylist)))
	(define-key
	  map
	  (vector (intern (concat "S-" (symbol-name (aref (car keylist) 0)))))
	  's-region-move))
    (setq keylist (cdr keylist))))

(s-region-bind
 (list [right] [left] [up] [down]
       [C-left] [C-right] [C-up] [C-down]
       [M-left] [M-right] [M-up] [M-down]
       [next] [previous] [home] [end]
       [C-next] [C-previous] [C-home] [C-end]
       [M-next] [M-previous] [M-home] [M-end]))

(or (global-key-binding [C-insert])
    (global-set-key [C-insert] 'copy-region-as-kill))
(or (global-key-binding [S-delete])
    (global-set-key [S-delete] 'kill-region))
(or (global-key-binding [S-insert])
    (global-set-key [S-insert] 'yank))

(provide 's-region)

;; s-region.el ends here.
