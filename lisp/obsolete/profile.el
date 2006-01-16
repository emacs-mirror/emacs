;;; profile.el --- Emacs profiler (OBSOLETE; use elp.el instead)

;; Copyright (C) 1992, 1994, 1998, 2002, 2003, 2004,
;;   2005 Free Software Foundation, Inc.

;; Author: Boaz Ben-Zvi <boaz@lcs.mit.edu>
;; Created: 07 Feb 1992
;; Version: 1.0
;; Adapted-By: ESR
;; Keywords: lisp, tools

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

;; DESCRIPTION:
;; ------------
;;   This program can be used to monitor running time performance of Emacs Lisp
;; functions.  It takes a list of functions and report the real time spent
;; inside these functions.  (Actually, for each function it reports the amount
;; of time spent while at least one instance of that function is on the call
;; stack.  So if profiled function FOO calls profiled function BAR, the time
;; spent inside BAR is credited to both functions.)

;; HOW TO USE:
;; -----------
;;   Set the variable  profile-functions-list  to the list of functions
;; (as symbols) You want to profile. Call  M-x  profile-functions to set
;; this list on and start using your program.  Note that profile-functions
;; MUST be called AFTER all the functions in profile-functions-list have
;; been loaded !!   (This call modifies the code of the profiled functions.
;; Hence if you reload these functions, you need to call  profile-functions
;; again! ).
;;   To display the results do  M-x  profile-results .  For example:
;;-------------------------------------------------------------------
;;  (setq profile-functions-list '(sokoban-set-mode-line sokoban-load-game
;;	                          sokoban-move-vertical sokoban-move))
;;  (load "sokoban")
;;  M-x profile-functions
;;     ...  I play the sokoban game ..........
;;  M-x profile-results
;;
;;      Function                     Time (Seconds.Useconds)
;;      ========                     =======================
;;      sokoban-move                     0.539088
;;      sokoban-move-vertical            0.410130
;;      sokoban-load-game                0.453235
;;      sokoban-set-mode-line            1.949203
;;-----------------------------------------------------
;; To clear all the settings to profile use profile-finish.
;; To set one function at a time (instead of or in addition to setting the
;; above list and  M-x profile-functions) use M-x profile-a-function.

;;; Code:

;;;
;;;  User modifiable VARIABLES
;;;

(defvar profile-functions-list nil "*List of functions to profile.")
(defvar profile-buffer "*profile*"
  "Name of profile buffer.")
(defvar profile-distinct nil
  "If non-nil, each time slice gets credited to at most one function.
\(Namely, the most recent one in the call stack.)  If nil, then the
time reported for a function includes the entire time from beginning
to end, even if it called some other function that was also profiled.")

;;;
;;; V A R I A B L E S
;;;

(defvar profile-time-list nil
  "List of cumulative calls and time for each profiled function.
Each element looks like (FUN NCALLS SEC . USEC).")
(defvar profile-init-list nil
  "List of entry time for each function.
Both how many times invoked and real time of start.
Each element looks like (FUN DEPTH HISEC LOSEC USEC), where DEPTH is
the current recursion depth, and HISEC, LOSEC, and USEC represent the
starting time of the call (or of the outermost recursion).")
(defvar profile-max-fun-name 0
  "Max length of name of any function profiled.")
(defvar profile-call-stack nil
  "A list of the profiled functions currently executing.
Used only when profile-distinct is non-nil.")
(defvar profile-last-time nil
  "The start time of the current time slice.
Used only when profile-distinct is non-nil.")

(defconst profile-million 1000000)

;;;
;;; F U N C T I O N S
;;;

(defun profile-functions (&optional flist)
  "Profile all the functions listed in `profile-functions-list'.
With argument FLIST, use the list FLIST instead."
  (interactive "P")
  (mapcar 'profile-a-function (or flist profile-functions-list)))

(defun profile-print (entry)
  "Print one ENTRY (from `profile-time-list')."
  (let* ((calls (car (cdr entry)))
	 (timec (cdr (cdr entry)))
	 (avgtime (and (not (zerop calls))
		       (/ (+ (car timec)
			     (/ (cdr timec) (float profile-million)))
			  calls))))
    (insert (format (concat "%-"
			    (int-to-string profile-max-fun-name)
			    "s %7d %10d.%06d")
		    (car entry) calls (car timec) (cdr timec))
	    (if (null avgtime)
		"\n"
	      (format " %18.6f\n" avgtime)))))

(defun profile-results ()
  "Display profiling results in the buffer `*profile*'.
\(The buffer name comes from `profile-buffer'.)"
  (interactive)
  (switch-to-buffer profile-buffer)
  (erase-buffer)
  (insert "Function" (make-string (- profile-max-fun-name 6) ? ))
  (insert " Calls  Total time (sec)  Avg time per call\n")
  (insert (make-string profile-max-fun-name ?=) "  ")
  (insert "======  ================  =================\n")
  (mapcar 'profile-print profile-time-list))

(defun profile-add-time (dest now prev)
  "Add to DEST the difference between timestamps NOW and PREV.
DEST is a pair (SEC . USEC) which is modified in place.
NOW and PREV are triples as returned by `current-time'."
  (let ((sec (+ (car dest)
		(* 65536 (- (car now) (car prev)))
		(- (cadr now) (cadr prev))))
	(usec (+ (cdr dest)
		 (- (car (cddr now)) (car (cddr prev))))))
    (if (< usec 0)
	(setq sec (1- sec)
	      usec (+ usec profile-million))
      (if (>= usec profile-million)
	  (setq sec (1+ sec)
		usec (- usec profile-million))))
    (setcar dest sec)
    (setcdr dest usec)))

(defun profile-function-prolog (fun)
  "Mark the beginning of a call to function FUN."
  (if profile-distinct
      (let ((profile-time (current-time)))
	(if profile-call-stack
	    (profile-add-time (cdr (cdr (assq (car profile-call-stack)
					      profile-time-list)))
			      profile-time profile-last-time))
	(setq profile-call-stack (cons fun profile-call-stack)
	      profile-last-time profile-time))
    (let ((profile-time (current-time))
	  (init-time (cdr (assq fun profile-init-list))))
      (if (null init-time) (error "Function %s missing from list" fun))
      (if (not (zerop (car init-time)));; is it a recursive call ?
	  (setcar init-time (1+ (car init-time)))
	(setcar init-time 1)		; mark first entry
	(setcdr init-time profile-time)))))

(defun profile-function-epilog (fun)
  "Mark the end of a call to function FUN."
  (if profile-distinct
      (let ((profile-time (current-time))
	    (accum (cdr (assq fun profile-time-list))))
	(setcar accum (1+ (car accum)))
	(profile-add-time (cdr accum) profile-time profile-last-time)
	(setq profile-call-stack (cdr profile-call-stack)
	      profile-last-time profile-time))
    (let ((profile-time (current-time))
	  (init-time (cdr (assq fun profile-init-list)))
	  (accum (cdr (assq fun profile-time-list))))
      (if (or (null init-time)
	      (null accum))
	  (error "Function %s missing from list" fun))
      (setcar init-time (1- (car init-time))) ; pop one level in recursion
      ;; Update only if we've finished the outermost recursive call
      (when (zerop (car init-time))
	(setcar accum (1+ (car accum)))
	(profile-add-time (cdr accum) profile-time (cdr init-time))))))

(defun profile-convert-byte-code (function)
  (let ((defn (symbol-function function)))
    (if (byte-code-function-p defn)
	;; It is a compiled code object.
	(let* ((contents (append defn nil))
	       (body
		(list (list 'byte-code (nth 1 contents)
			    (nth 2 contents) (nth 3 contents)))))
	  (if (nthcdr 5 contents)
	      (setq body (cons (list 'interactive (nth 5 contents)) body)))
	  (if (nth 4 contents)
	      ;; Use `documentation' here, to get the actual string,
	      ;; in case the compiled function has a reference
	      ;; to the .elc file.
	      (setq body (cons (documentation function) body)))
	  (fset function (cons 'lambda (cons (car contents) body)))))))

(defun profile-a-function (fun)
  "Profile the function FUN."
  (interactive "aFunction to profile: ")
  (let ((def (symbol-function fun)))
    (when (eq (car-safe def) 'autoload)
      (load (car (cdr def)))
      (setq def (symbol-function fun)))
    (fetch-bytecode def))
  (profile-convert-byte-code fun)
  (let ((def (symbol-function fun)) (funlen (length (symbol-name fun))))
    (or (eq (car def) 'lambda)
	(error "To profile: %s must be a user-defined function" fun))
    (setq profile-time-list		; add a new entry
	  (cons (cons fun (cons 0 (cons 0 0))) profile-time-list))
    (setq profile-init-list		; add a new entry
	  (cons (cons fun (cons 0 nil)) profile-init-list))
    (if (< profile-max-fun-name funlen) (setq profile-max-fun-name funlen))
    (fset fun (profile-fix-fun fun def))))

(defun profile-fix-fun (fun def)
  "Take function FUN and return it fixed for profiling.
DEF is (symbol-function FUN)."
  (if (< (length def) 3)
      def		; nothing to change
    (let ((prefix (list (car def) (car (cdr def))))
	  (suffix (cdr (cdr def))))
      ;; Skip the doc string, if there is a string
      ;; which serves only as a doc string,
      ;; and put it in PREFIX.
      (if (and (stringp (car suffix)) (cdr suffix))
	  (setq prefix (nconc prefix (list (car suffix)))
		suffix (cdr suffix)))
      ;; Check for an interactive spec.
      ;; If found, put it into PREFIX and skip it.
      (if (and (listp (car suffix))
	       (eq (car (car suffix)) 'interactive))
	  (setq prefix (nconc prefix (list (car suffix)))
		suffix (cdr suffix)))
      (if (eq (car-safe (car suffix)) 'profile-function-prolog)
	  def				; already profiled
	;; Prepare new function definition.
	;; If you change this structure, also change profile-restore-fun.
	(nconc prefix
	       (list (list 'profile-function-prolog
			   (list 'quote fun))
		     (list 'unwind-protect
			   (cons 'progn suffix)
			   (list 'profile-function-epilog
				 (list 'quote fun)))))))))

(defun profile-restore-fun (fun)
  "Restore profiled function FUN to its original state."
  (let ((def (symbol-function fun)) body index)
    ;; move index beyond header
    (setq index (cdr-safe def))
    (if (stringp (car (cdr index)))
	(setq index (cdr index)))
    (if (eq (car-safe (car (cdr index))) 'interactive)
	(setq index (cdr index)))
    (if (eq (car-safe (car (cdr index))) 'profile-function-prolog)
	(setcdr index (cdr (car (cdr (car (cdr (cdr index))))))))))

(defun profile-finish ()
  "Stop profiling functions.  Clear all the settings."
  (interactive)
  (while profile-time-list
    (profile-restore-fun (car (car profile-time-list)))
    (setq profile-time-list (cdr profile-time-list)))
  (setq profile-max-fun-name 0)
  (setq profile-init-list nil))

(provide 'profile)

;;; arch-tag: 816f97e8-efff-4da2-9a95-7bc392f58b19
;;; profile.el ends here
