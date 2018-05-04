;;; find-gc.el --- detect functions that call the garbage collector

;; Copyright (C) 1992, 2001-2018 Free Software Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Produce in find-gc-unsafe-list the set of all functions that may invoke GC.
;; This expects the Emacs sources to live in find-gc-source-directory.

;;; Code:

(defvar find-gc-unsafe-list nil
  "The list of unsafe functions is placed here by `find-gc-unsafe'.")

(defvar find-gc-source-directory
  (file-name-as-directory (expand-file-name "src" source-directory))
  "Directory containing Emacs C sources.")

(defvar find-gc-subrs-callers nil
  "Alist of users of subrs, from GC testing.
Each entry has the form (FUNCTION . FUNCTIONS-THAT-CALL-IT).")

(defvar find-gc-subrs-called nil
  "Alist of subrs called, in GC testing.
Each entry has the form (FUNCTION . FUNCTIONS-IT-CALLS).")


;;; Functions on this list are safe, even if they appear to be able
;;; to call the target.

(defvar find-gc-noreturn-list '(Fsignal Fthrow wrong_type_argument))

;;; This was originally generated directory-files, but there were
;;; too many files there that were not actually compiled.  The
;;; list below was created for a HP-UX 7.0 system.

(defvar find-gc-source-files
  '("dispnew.c" "scroll.c" "xdisp.c" "window.c"
    "term.c" "cm.c" "emacs.c" "keyboard.c" "macros.c"
    "keymap.c" "sysdep.c" "buffer.c" "filelock.c"
    "insdel.c" "marker.c" "minibuf.c" "fileio.c"
    "dired.c" "cmds.c" "casefiddle.c"
    "indent.c" "search.c" "regex.c" "undo.c"
    "alloc.c" "data.c" "doc.c" "editfns.c"
    "callint.c" "eval.c" "fns.c" "print.c" "lread.c"
    "syntax.c" "unexcoff.c"
    "bytecode.c" "process.c" "callproc.c" "doprnt.c"
    "xterm.c" "xfns.c"))


(defun find-gc-unsafe ()
  "Return a list of unsafe functions--that is, which can call GC.
Also store it in `find-gc-unsafe-list'."
  (trace-call-tree nil)
  (trace-use-tree)
  (find-unsafe-funcs 'Fgarbage_collect)
  (setq find-gc-unsafe-list
	(sort find-gc-unsafe-list
	      (function (lambda (x y)
			  (string-lessp (car x) (car y)))))))

;;; This does a depth-first search to find all functions that can
;;; ultimately call the function "target".  The result is an a-list
;;; in find-gc-unsafe-list; the cars are the unsafe functions, and the cdrs
;;; are (one of) the unsafe functions that these functions directly
;;; call.

(defun find-unsafe-funcs (target)
  (setq find-gc-unsafe-list (list (list target)))
  (trace-unsafe target))

(defun trace-unsafe (func)
  (let ((used (assq func find-gc-subrs-callers)))
    (or used
	(error "No find-gc-subrs-callers for %s" (car find-gc-unsafe-list)))
    (while (setq used (cdr used))
      (or (assq (car used) find-gc-unsafe-list)
	  (memq (car used) find-gc-noreturn-list)
	  (progn
	    (push (cons (car used) func) find-gc-unsafe-list)
	    (trace-unsafe (car used)))))))




(defun trace-call-tree (&optional ignored)
  (message "Setting up directories...")
  (setq find-gc-subrs-called nil)
  (let ((case-fold-search nil)
	(default-directory find-gc-source-directory)
	(files find-gc-source-files)
	name entry rtlfile)
    (dolist (file files)
      (message "Compiling %s..." file)
      (call-process "gcc" nil nil nil "-I" "." "-I" "../lib"
		    "-fdump-rtl-expand" "-o" null-device "-c" file)
      (setq rtlfile
	    (file-expand-wildcards (format "%s.*.expand" file) t))
      (if (/= 1 (length rtlfile))
	  (message "Error compiling `%s'?" file)
	(with-temp-buffer
	  (insert-file-contents (setq rtlfile (car rtlfile)))
	  (delete-file rtlfile)
	  (while (re-search-forward ";; Function \\|(call_insn " nil t)
	    (if (= (char-after (- (point) 3)) ?o)
		(progn
		  (looking-at "[a-zA-Z0-9_]+")
		  (setq name (intern (match-string 0)))
		  (message "%s : %s" (car files) name)
		  (setq entry (list name)
			find-gc-subrs-called
			(cons entry find-gc-subrs-called)))
	      (if (looking-at ".*\n?.*\"\\([A-Za-z0-9_]+\\)\"")
		  (progn
		    (setq name (intern (match-string 1)))
		    (or (memq name (cdr entry))
			(setcdr entry (cons name (cdr entry)))))))))))))

(defun trace-use-tree ()
  (setq find-gc-subrs-callers (mapcar 'list (mapcar 'car find-gc-subrs-called)))
  (let ((ptr find-gc-subrs-called)
	p2 found)
    (while ptr
      (setq p2 (car ptr))
      (while (setq p2 (cdr p2))
	(if (setq found (assq (car p2) find-gc-subrs-callers))
	    (setcdr found (cons (car (car ptr)) (cdr found)))))
      (setq ptr (cdr ptr)))))

(provide 'find-gc)

;;; find-gc.el ends here
