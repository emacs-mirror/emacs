;;; profile.el --- generate run time measurements of Emacs Lisp functions

;; Copyright (C) 1992 Free Software Foundation, Inc.

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

; DESCRIPTION:
; ------------
;   This program can be used to monitor running time performance of Emacs Lisp
; functions. It takes a list of functions and report the real time spent 
; inside these functions. It runs a process with a separate timer program.
;   Caveat: the C code included with this package requires BSD-compatible
; time-of-day functions.  If you're running an AT&T version prior to SVr4,
; you may have difficulty getting it to work.  Your X library may supply
; the required routines if the standard C library does not.

; HOW TO USE:
; -----------
;   Set the variable  profile-functions-list  to the list of functions
; (as symbols) You want to profile. Call  M-x  profile-functions to set 
; this list on and start using your program.  Note that profile-functions 
; MUST be called AFTER all the functions in profile-functions-list have 
; been loaded !!   (This call modifies the code of the profiled functions.
; Hence if you reload these functions, you need to call  profile-functions  
; again! ).
;   To display the results do  M-x  profile-results .  For example:
;-------------------------------------------------------------------
;  (setq profile-functions-list '(sokoban-set-mode-line sokoban-load-game 
;	                          sokoban-move-vertical sokoban-move))
;  (load "sokoban")
;  M-x profile-functions
;     ...  I play the sokoban game ..........
;  M-x profile-results
;
;      Function                     Time (Seconds.Useconds)
;      ========                     =======================
;      sokoban-move                     0.539088
;      sokoban-move-vertical            0.410130
;      sokoban-load-game                0.453235
;      sokoban-set-mode-line            1.949203
;-----------------------------------------------------
; To clear all the settings to profile use   profile-finish. 
; To set one function at a time (instead of or in addition to setting the 
; above list and  M-x profile-functions ) use  M-x profile-a-function  .

; HOW TO INSTALL:
; ---------------
;   First you need to compile and install the following C program in your
; path under the name "emacs-timer" (or set the variable  
; profile-timer-program  to whatever name you picked).
;
;/**
; **  To be run as an emacs process. Input string that starts with:
; **    'z' -- resets the watch (to zero).
; **    'p' -- return time (on stdout) as string with format <sec>.<micro-sec>
; **    'q' -- exit.
; **
; **  abstraction : a stopwatch
; **  operations: reset_watch, get_time
; */
;#include <strings.h>
;#include <sys/time.h>
;#include <stdio.h>
;static struct timeval TV1,TV2;
;static struct timezone *tzp = (struct timezone *) NULL; /* no need timezone */
;static int watch_not_started = 1 ; /* flag */
;static char time_string[30];
;
;int reset_watch()    /* this call resets the stopwatch to zero */
;{
;    gettimeofday(&TV1, tzp) ;
;    watch_not_started = 0;
;}
;
;char *get_time()
;   /* this call returns the time since the last reset_watch() call. The time
;       is returned as a string with the format  <seconds>.<micro-seconds> 
;       If reset_watch() was not called yet, returns NULL */
;{
;    char *result = time_string ;
;    int i;
;    if (watch_not_started) return((char *) 0);  /* call reset_watch first ! */
;    gettimeofday(&TV2, tzp);
;    if ( TV1.tv_usec > TV2.tv_usec )
;    {
;	TV2.tv_usec += 1000000;
;	TV2.tv_sec--;
;    }
;    sprintf(result,"%lu.%6lu",
;	    TV2.tv_sec - TV1.tv_sec, TV2.tv_usec - TV1.tv_usec);
;    for (result = index(result,'.') + 1 ; *result == ' ' ; result++ ) 
;	*result = '0';
;    return(time_string);
;}
;
;void main()
;{
;    char inp[10];
;    while (1)
;    {
;	gets(inp);
;	switch (inp[0])
;	{
;	case 'z': reset_watch();
;	    break;
;	case 'p': puts(get_time());
;	    break;
;	case 'q': exit(0);
;	}
;   }
;}
; -------- end of clip ----------------

;;; Code:

;;;
;;;  User modifiable VARIABLES
;;;

(defvar profile-functions-list nil "*List of functions to profile")
(defvar profile-timer-program "emacs-timer" "*Name of the timer program")

;;;
;;; V A R I A B L E S
;;;

(defvar profile-timer-process nil "Process running the timer")
(defvar profile-time-list nil 
    "List of accumulative time for each profiled function")
(defvar profile-init-list nil
    "List of entry time for each function. \n\
Both how many times invoked and real time of start.")
(defvar profile-max-fun-name 0 "Max length of name of any function profiled")
(defvar profile-temp-result- nil "Should NOT be used anywhere else")
(defvar profile-time (cons 0 0) "Used to return result from a filter")
(defvar profile-buffer "*profile*" "Name of profile buffer")

;;;
;;; F U N C T I O N S
;;;

(defun profile-functions (&optional flist)
    "Profile all the functions listed in profile-functions-list.\n\
With argument FLIST, use the list FLIST instead."
    (interactive "*P")
    (if (null flist) (setq flist profile-functions-list))
    (mapcar 'profile-a-function flist))

(defun profile-filter (process input)
    "Filter for the timer process. Sets profile-time to the returned time."
    (if (zerop (string-match "\\." input)) 
	    (error "Bad output from %s" profile-timer-program)
	(setcar profile-time 
		(string-to-int (substring input 0 (match-beginning 0))))
	(setcdr profile-time 
		(string-to-int (substring input (match-end 0))))))


(defun profile-print (entry)
    "Print one ENTRY (from profile-time-list) ."
    (let ((time (cdr entry)) str (offset 5))
	(insert (format "%s" (car entry)) space)
	(move-to-column ref-column)
	(setq str (int-to-string (car time)))
	(insert str)
	(if (>= (length str) offset) nil
	    (move-to-column ref-column)
	    (insert (substring spaces 0 (- offset (length str))))
	    (forward-char (length str)))
	(setq str (int-to-string (cdr time)))
	(insert "." (substring "000000" 0 (- 6 (length str))) str "\n")
	))

(defconst spaces "                                                         ")

(defun profile-results ()
    "Display profiling results in  profile-buffer ."
    (interactive)
    (let* ((ref-column (+ 8 profile-max-fun-name))
	   (space (substring spaces 0 ref-column)))
	(switch-to-buffer profile-buffer)
	(erase-buffer)
	(insert "Function" space)
	(move-to-column ref-column)
	(insert "Time (Seconds.Useconds)\n" "========" space )
	(move-to-column ref-column)
	(insert	"=======================\n")
	(mapcar 'profile-print profile-time-list)))
    
(defun profile-reset-timer ()
    (process-send-string profile-timer-process "z\n"))

(defun profile-check-zero-init-times (entry)
    "If ENTRY has non zero time, give an error."
    (let ((time (cdr (cdr entry))))
	(if (and (zerop (car time)) (zerop (cdr time))) nil ; OK
	    (error "Process timer died while making performance profile."))))

(defun profile-get-time ()
    "Get time from timer process into profile-time ."
    ;; first time or if process dies
    (if (and (processp profile-timer-process)
	     (eq 'run (process-status profile-timer-process))) nil
	(setq profile-timer-process   ;; [re]start the timer process
	      (start-process "timer" 
			     (get-buffer-create profile-buffer) 
			     profile-timer-program))
	(set-process-filter profile-timer-process 'profile-filter)
	(process-kill-without-query profile-timer-process)
	(profile-reset-timer)
	;; check if timer died during time measurement
	(mapcar 'profile-check-zero-init-times profile-init-list)) 
    ;; make timer process return current time
    (process-send-string profile-timer-process "p\n")
    (accept-process-output))

(defun profile-find-function (fun flist)
    "Linear search for FUN in FLIST ."
    (if (null flist) nil
	(if (eq fun (car (car flist))) (cdr (car flist))
	    (profile-find-function fun (cdr flist)))))

(defun profile-start-function (fun)
    "On entry, keep current time for function FUN."
    ;; assumes that profile-time contains the current time
    (let ((init-time (profile-find-function fun profile-init-list)))
	(if (null init-time) (error "Function %s missing from list" fun))
	(if (not (zerop (car init-time))) ;; is it a recursive call ?
		(setcar init-time (1+ (car init-time)))
	    (setcar init-time 1) ; mark first entry
	    (setq init-time (cdr init-time))
	    (setcar init-time (car profile-time))
	    (setcdr init-time (cdr profile-time)))
	))
	
(defconst profile-million 1000000)

(defun profile-update-function (fun)
    "When the call to the function FUN is finished, add its run time."
    ;; assumes that profile-time contains the current time
    (let ((init-time (profile-find-function fun profile-init-list))
	  (accum (profile-find-function fun profile-time-list))
	  sec usec)
	(if (or (null init-time)
		(null accum)) (error "Function %s missing from list" fun))
	(setcar init-time (1- (car init-time))) ; pop one level in recursion
	(if (not (zerop (car init-time))) 
		nil ; in some recursion level, do not update accum. time
	    (setq init-time (cdr init-time))
	    (setq sec (- (car profile-time) (car init-time))
		  usec (- (cdr profile-time) (cdr init-time)))
	    (setcar init-time 0) ;  reset time to check for error
	    (setcdr init-time 0) ;  in case timer process dies
	    (if (>= usec 0) nil
		(setq usec (+ usec profile-million))
		(setq sec (1- sec)))
	    (setcar accum (+ sec (car accum)))
	    (setcdr accum (+ usec (cdr accum)))
	    (if (< (cdr accum) profile-million) nil
		(setcar accum (1+ (car accum)))
		(setcdr accum (- (cdr accum) profile-million)))
	    )))

(defun profile-a-function (fun)
    "Profile the function FUN"
    (interactive "aFunction to profile: ")
    (let ((def (symbol-function fun)) (funlen (length (symbol-name fun))))
	(if (eq (car def) 'lambda) nil
	    (error "To profile: %s must be a user-defined function" fun))
	(setq profile-time-list                       ; add a new entry
	      (cons (cons fun (cons 0 0)) profile-time-list))
	(setq profile-init-list                  ; add a new entry
	      (cons (cons fun (cons 0 (cons 0 0))) profile-init-list))
	(if (< profile-max-fun-name funlen) (setq profile-max-fun-name funlen))
	(fset fun (profile-fix-fun fun def))))

(defun profile-fix-fun (fun def)
    "Take function FUN and return it fixed for profiling.\n\
DEF is (symbol-function FUN) ."
    (let (prefix first second third (count 2) inter suffix)
	(if (< (length def) 3) nil ; nothing to see
	    (setq first (car def) second (car (cdr def))
		  third (car (nthcdr 2 def)))
	    (setq prefix (list first second))
	    (if (and (stringp third) (< (length def) 3)) nil ; nothing to see
		(if (not (stringp third))  (setq inter third) 
		    (setq count 3  ; suffix to start after doc string
			  prefix (nconc prefix (list third))
			  inter (car (nthcdr 3 def))) ; fourth sexp
		    )
		(if (not (and (listp inter) 
			      (eq (car inter) 'interactive))) nil
		    (setq prefix (nconc prefix (list inter)))
		    (setq count (1+ count))) ; skip this sexp for suffix
		(setq suffix (nthcdr count def))
		(if (equal (car suffix) '(profile-get-time)) nil ;; already set
		    ;; prepare new function
		    (nconc prefix
			   (list '(profile-get-time))  ; read time
			   (list (list 'profile-start-function 
				       (list 'quote fun)))
			   (list (list 'setq 'profile-temp-result- 
				       (nconc (list 'progn) suffix)))
			   (list '(profile-get-time))  ; read time
			   (list (list 'profile-update-function 
				       (list 'quote fun)))
			   (list 'profile-temp-result-)
			   ))))))

(defun profile-restore-fun (fun)
    "Restore profiled function FUN to its original state."
    (let ((def (symbol-function (car fun))) body index)
	;; move index beyond header
	(setq index (cdr def))
	(if (stringp (car (cdr index))) (setq index (cdr index)))
	(if (and (listp (car (cdr index)))
		 (eq (car (car (cdr index))) 'interactive))
		(setq index (cdr index)))
	(setq body (car (nthcdr 3 index)))
	(if (and (listp body)  ; the right element ?
		 (eq (car (cdr body)) 'profile-temp-result-))
		(setcdr index (cdr (car (cdr (cdr body))))))))

(defun profile-finish ()
    "Stop profiling functions. Clear all the settings."
    (interactive)
    (mapcar 'profile-restore-fun profile-time-list)
    (setq profile-max-fun-name 0)
    (setq profile-time-list nil)
    (setq profile-init-list nil))

(defun profile-quit ()
    "Kill the timer process."
    (interactive)
    (process-send-string profile-timer-process "q\n"))

;;; profile.el ends here
