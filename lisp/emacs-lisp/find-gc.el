;;; find-gc.el --- detect functions that call the garbage collector

;; Copyright (C) 1992 Free Software Foundation, Inc.

;; Maintainer: FSF

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Produce in unsafe-list the set of all functions that may invoke GC.
;; This expects the Emacs sources to live in emacs-source-directory.
;; It creates a temporary working directory /tmp/esrc.

;;; Code:

(defun find-gc-unsafe ()
  (trace-call-tree nil)
  (trace-use-tree)
  (find-unsafe-funcs 'Fgarbage_collect)
  (setq unsafe-list (sort unsafe-list
			  (function (lambda (x y)
				      (string-lessp (car x) (car y))))))
)

(setq emacs-source-directory "/usr/gnu/src/dist/src")


;;; This does a depth-first search to find all functions that can
;;; ultimately call the function "target".  The result is an a-list
;;; in unsafe-list; the cars are the unsafe functions, and the cdrs
;;; are (one of) the unsafe functions that these functions directly
;;; call.

(defun find-unsafe-funcs (target)
  (setq unsafe-list (list (list target)))
  (trace-unsafe target)
)

(defun trace-unsafe (func)
  (let ((used (assq func subrs-used)))
    (or used
	(error "No subrs-used for %s" (car unsafe-list)))
    (while (setq used (cdr used))
      (or (assq (car used) unsafe-list)
	  (memq (car used) noreturn-list)
	  (progn
	    (setq unsafe-list (cons (cons (car used) func) unsafe-list))
	    (trace-unsafe (car used))))))
)


;;; Functions on this list are safe, even if they appear to be able
;;; to call the target.

(setq noreturn-list '( Fsignal Fthrow wrong_type_argument ))


;;; This produces an a-list of functions in subrs-called.  The cdr of
;;; each entry is a list of functions which the function in car calls.

(defun trace-call-tree (&optional already-setup)
  (message "Setting up directories...")
  (or already-setup
      (progn
	;; Gee, wouldn't a built-in "system" function be handy here.
	(call-process "csh" nil nil nil "-c" "rm -rf /tmp/esrc")
	(call-process "csh" nil nil nil "-c" "mkdir /tmp/esrc")
	(call-process "csh" nil nil nil "-c"
		      (format "ln -s %s/*.[ch] /tmp/esrc"
			      emacs-source-directory))))
  (save-excursion
    (set-buffer (get-buffer-create "*Trace Call Tree*"))
    (setq subrs-called nil)
    (let ((case-fold-search nil)
	  (files source-files)
	  name entry)
      (while files
	(message "Compiling %s..." (car files))
	(call-process "csh" nil nil nil "-c"
		      (format "gcc -dr -c /tmp/esrc/%s -o /dev/null"
			      (car files)))
	(erase-buffer)
	(insert-file-contents (concat "/tmp/esrc/" (car files) ".rtl"))
	(while (re-search-forward ";; Function \\|(call_insn " nil t)
	  (if (= (char-after (- (point) 3)) ?o)
	      (progn
		(looking-at "[a-zA-Z0-9_]+")
		(setq name (intern (buffer-substring (match-beginning 0)
						     (match-end 0))))
		(message "%s : %s" (car files) name)
		(setq entry (list name)
		      subrs-called (cons entry subrs-called)))
	    (if (looking-at ".*\n?.*\"\\([A-Za-z0-9_]+\\)\"")
		(progn
		  (setq name (intern (buffer-substring (match-beginning 1)
						       (match-end 1))))
		  (or (memq name (cdr entry))
		      (setcdr entry (cons name (cdr entry))))))))
	(delete-file (concat "/tmp/esrc/" (car files) ".rtl"))
	(setq files (cdr files)))))
)


;;; This was originally generated directory-files, but there were
;;; too many files there that were not actually compiled.  The
;;; list below was created for a HP-UX 7.0 system.

(setq source-files '("dispnew.c" "scroll.c" "xdisp.c" "window.c"
		     "term.c" "cm.c" "emacs.c" "keyboard.c" "macros.c"
		     "keymap.c" "sysdep.c" "buffer.c" "filelock.c"
		     "insdel.c" "marker.c" "minibuf.c" "fileio.c"
		     "dired.c" "filemode.c" "cmds.c" "casefiddle.c"
		     "indent.c" "search.c" "regex.c" "undo.c"
		     "alloc.c" "data.c" "doc.c" "editfns.c"
		     "callint.c" "eval.c" "fns.c" "print.c" "lread.c"
		     "abbrev.c" "syntax.c" "unexec.c"
		     "bytecode.c" "process.c" "callproc.c" "doprnt.c"
		     "x11term.c" "x11fns.c"))


;;; This produces an inverted a-list in subrs-used.  The cdr of each
;;; entry is a list of functions that call the function in car.

(defun trace-use-tree ()
  (setq subrs-used (mapcar 'list (mapcar 'car subrs-called)))
  (let ((ptr subrs-called)
	p2 found)
    (while ptr
      (setq p2 (car ptr))
      (while (setq p2 (cdr p2))
	(if (setq found (assq (car p2) subrs-used))
	    (setcdr found (cons (car (car ptr)) (cdr found)))))
      (setq ptr (cdr ptr))))
)

(provide 'find-gc)

;;; arch-tag: 4a26a538-a008-40d9-a1ef-23bb6dbecef4
;;; find-gc.el ends here
