;;; flow-ctrl.el --- help for lusers on cu(1) or ttys with wired-in ^S/^Q flow control

;;; Copyright (C) 1990, 1991 Free Software Foundation, Inc.

;; Author Kevin Gallagher
;; Maintainer: FSF
;; Adapted-By: ESR
;; Keywords: hardware

;;; This file is part of GNU Emacs.
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

;;; Commentary:

;;;; Terminals that use XON/XOFF flow control can cause problems with
;;;; GNU Emacs users.  This file contains Emacs Lisp code that makes it
;;;; easy for a user to deal with this problem, when using such a
;;;; terminal. 
;;;;      
;;;; To invoke these adjustments, a user need only invoke the function
;;;; enable-flow-control-on with a list of terminal types in his/her own
;;;; .emacs file.  As arguments, give it the names of one or more terminal
;;;; types in use by that user which require flow control adjustments.
;;;; Here's an example: 
;;;; 
;;;;	(enable-flow-control-on "vt200" "vt300" "vt101" "vt131")

;;; Portability note: This uses (getenv "TERM"), and therefore probably
;;; won't work outside of UNIX-like environments.

;;; Code:

(defvar flow-control-c-s-replacement ?\034
  "Character that replaces C-s, when flow control handling is enabled.")
(defvar flow-control-c-q-replacement ?\036
  "Character that replaces C-q, when flow control handling is enabled.")

;;;###autoload
(defun enable-flow-control (&optional argument)
  "Toggle flow control handling.
When handling is enabled, user can type C-s as C-\\, and C-q as C-^.
With arg, enable flow control mode if arg is positive, otherwise disable."
  (interactive "P")
  (if (if argument
	  ;; Argument means enable if arg is positive.
	  (<= (prefix-numeric-value argument) 0)
	;; No arg means toggle.
	(nth 1 (current-input-mode)))
      (progn
	;; Turn flow control off, and stop exchanging chars.
	(set-input-mode t nil (nth 2 (current-input-mode)))
	(aset keyboard-translate-table flow-control-c-s-replacement nil)
	(aset keyboard-translate-table ?\^s nil)
	(aset keyboard-translate-table flow-control-c-q-replacement nil)
	(aset keyboard-translate-table ?\^q nil))
    ;; Turn flow control on.
    ;; Tell emacs to pass C-s and C-q to OS.
    (set-input-mode nil t (nth 2 (current-input-mode)))
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
    (aset keyboard-translate-table flow-control-c-s-replacement ?\^s)
    (aset keyboard-translate-table ?\^s flow-control-c-s-replacement)
    ;; Swap C-q and C-^
    (aset keyboard-translate-table flow-control-c-q-replacement ?\^q)
    (aset keyboard-translate-table ?\^q flow-control-c-q-replacement)
    (message (concat 
	      "XON/XOFF adjustment for " 
	      (getenv "TERM") 
	      ":  use C-\\ for C-s  and  use C-^ for C-q."))
    (sleep-for 2)))			; Give user a chance to see message.

;;;###autoload
(defun enable-flow-control-on (&rest losing-terminal-types)
  "Enable flow control if using one of a specified set of terminal types.
Use `(enable-flow-control-on \"vt100\" \"h19\")' to enable flow control
on VT-100 and H19 terminals.  When flow control is enabled,
you must type C-\\ to get the effect of a C-s, and type C-^
to get the effect of a C-q."
  (let ((term (getenv "TERM"))
	hyphend)
    ;; Strip off hyphen and what follows
    (while (setq hyphend (string-match "[-_][^-_]+$" term))
      (setq term (substring term 0 hyphend)))
    (and (member term losing-terminal-types)
	 (enable-flow-control))))

(provide 'flow-ctrl)

;;; flow-ctrl.el ends here
