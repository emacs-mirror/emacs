;;; Help for lusers on cu(1) or terminals with wired-in ^S/^Q flow control
;;;
;;; Copyright (C) 1990 Free Software Foundation, Inc.
;;; Copyright (C) 1991 Kevin Gallagher
;;; Adapted for Emacs 19 by Eric S. Raymond <eric@snark.thyrsus.com>
;;;
;;; GNU Emacs is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY.  No author or distributor accepts
;;; RESPONSIBILITY TO anyone for the consequences of using it or for
;;; whether it serves any particular purpose or works at all, unless 
;;; he says so in writing.  Refer to the GNU Emacs General Public
;;; License for full details.
;;;
;;; Everyone is granted permission to copy, modify and redistribute
;;; GNU Emacs, but only under the conditions described in the GNU
;;; Emacs General Public License.  A copy of this license is supposed
;;; to have been given to you along with GNU Emacs so you can know
;;; your rights and responsibilities.  It should be in a file named
;;; COPYING.  Among other things, the Copyright notice and this notice
;;; must be preserved on all copies.
;;;

;;;; Terminals that use XON/XOFF flow control can cause problems with
;;;; GNU Emacs users.  This file contains elisp code that makes it
;;;; easy for a user to deal with this problem, when using such a
;;;; terminal. 
;;;;      
;;;; To invoke these adjustments, a user need only invoke the function
;;;; evade-flow-control-on with a list of terminal types in his/her own
;;;; .emacs file.  As arguments, give it the names of one or more terminal
;;;; types in use by that user which require flow control adjustments.
;;;; Here's an example: 
;;;; 
;;;;	(evade-flow-control-on "vt200" "vt300" "vt101" "vt131")

;;; Portability note: This uses (getenv "TERM"), and therefore probably
;;; won't work outside of UNIX-like environments.

(defun evade-flow-control ()
  "Replace C-s with C-\ and C-q with C-^ and tell emacs to pass C-s
and C-q characters to OS."
  (interactive)
  ;; Tell emacs to pass C-s and C-q to OS.
  (set-input-mode nil t)
  ;; Initialize translate table, saving previous mappings, if any.
  (let ((the-table (make-string 128 0)))
    (let ((i 0)
	  (j (length keyboard-translate-table)))
      (while (< i j)
	(aset the-table i (elt keyboard-translate-table i))
	(setq i (1+ i)))
      (while (< i 128)
	(aset the-table i i)
	(setq i (1+ i))))
    (setq keyboard-translate-table the-table))
  ;; Swap C-s and C-\
  (aset keyboard-translate-table ?\034 ?\^s)
  (aset keyboard-translate-table ?\^s ?\034)
  ;; Swap C-q and C-^
  (aset keyboard-translate-table ?\036 ?\^q)
  (aset keyboard-translate-table ?\^q ?\036)
  (message (concat 
             "XON/XOFF adjustment for " 
             (getenv "TERM") 
             ":  use C-\\ for C-s  and  use C-^ for C-q."))
  (sleep-for 2)) ; Give user a chance to see message.

(defun memstr= (e s)
  (cond ((null s) nil)
	((string= e (car s)) t)
	(t (memstr= e (cdr s)))))

;;;###autoload
(defun evade-flow-control-on (&rest losing-terminal-types)
  (let ((term (getenv "TERM"))
	hyphend)
    ;; Strip off hyphen and what follows
    (while (setq hyphend (string-match "[-_][^-_]+$" term))
      (setq term (substring term 0 hyphend)))
    (and (memstr= term losing-terminal-types) (evade-flow-control)))
  )

(provide 'flow-ctrl)

;;; flow-ctrl.el ends here

