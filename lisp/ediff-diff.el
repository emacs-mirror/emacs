;;; ediff-diff.el --- diff-related utilities
;;; Copyright (C) 1994 Free Software Foundation, Inc.

;; Author: Michael Kifer <kifer@cs.sunysb.edu>

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

(require 'ediff-init)


(defvar ediff-shell
  (cond ((eq system-type 'emx) "cmd") ; OS/2
	((memq system-type '(vax-vms axp-vms)) "*dcl*") ; VMS
	(t  "sh")) ; UNIX
  "*The shell used to run diff and patch.  If user's .profile or
.cshrc files are set up correctly, any shell will do.  However, some people
set $prompt or other things incorrectly, which leads to undesirable output
messages.  These may cause Ediff to fail.  In such a case, set ediff-shell
to a shell that you are not using or, better, fix your shell's startup file.")


(defvar ediff-diff-program "diff"
  "*Program to use for generating the differential of the two files.")
(defvar ediff-diff-options ""  
  "*Options to pass to `ediff-diff-program'. 
If diff\(1\) is used as `ediff-diff-program', then the most useful options are
`-w', to ignore space, and `-i', to ignore case of letters.
At present, the option `-c' is ignored, since Ediff doesn't understand this
type of output.")

(defvar ediff-custom-diff-program ediff-diff-program
  "*Program to use for generating custom diff output for saving it in a file.
This output is not used by Ediff internally.")
(defvar ediff-custom-diff-options "-c"
  "*Options to pass to `ediff-custom-diff-program'.")

;;; Support for diff3

(defvar ediff-match-diff3-line "^====\\(.?\\)$"
  "Pattern to match lines produced by diff3 that describe differences.")
(defvar ediff-diff3-program "diff3"
  "*Program to be used for three-way comparison.
Must produce output compatible with Unix's diff3 program.")
(defvar ediff-diff3-options ""  
  "*Options to pass to `ediff-diff3-program'.")
(defvar ediff-diff3-ok-lines-regexp
  "^\\([1-3]:\\|====\\|  \\|.*Warning *:\\|.*No newline\\|.*missing newline\\)"
  "*Regexp that matches normal output lines from `ediff-diff3-program'.
Lines that do not match are assumed to be error messages.")

;; keeps the status of the current diff in 3-way jobs.
;; the status can be =diff(A), =diff(B), or =diff(A+B)
(ediff-defvar-local ediff-diff-status "" "")

;; Support for patch 

(defvar ediff-patch-program "patch"
  "*Name of the program that applies patches.")
(defvar ediff-patch-options ""
  "*Options to pass to ediff-patch-program.")

;; The buffer of the patch file.
(defvar ediff-patch-buf nil)
;; The buffer where patch would display its diagnostics.
(defvar ediff-patch-diagnostics nil)

  
;;; Fine differences 

(ediff-defvar-local ediff-auto-refine (if window-system 'on 'nix)
  "If `on', Ediff auto-highlights fine diffs for the current diff region.
If `off', auto-highlighting is not used. If `nix', no fine diffs are shown
at all, unless the user force-refines the region by hitting `*'.

This variable can be set either in .emacs or toggled interactively.
Use `setq-default' if setting it in .emacs")

(ediff-defvar-local ediff-ignore-similar-regions nil
  "*If t, skip over difference regions that differ only in the white space and line breaks.
This variable can be set either in .emacs or toggled interactively.
Use `setq-default' if setting it in .emacs")

(ediff-defvar-local ediff-auto-refine-limit 700
  "Auto-refine only those regions that are smaller than this number of bytes.")
  
;;; General

(defvar ediff-diff-ok-lines-regexp  
  "^\\([0-9,]+[acd][0-9,]+$\\|[<>] \\|---\\|.*Warning *:\\|.*No newline\\|.*missing newline\\)"
  "Regexp that matches normal output lines from `ediff-diff-program'.
This is mostly lifted from Emerge, except that Ediff also considers
warnings and `Missing newline'-type messages to be normal output.
Lines that do not match are assumed to be error messages.")

(defvar ediff-match-diff-line (let ((x "\\([0-9]+\\)\\(\\|,\\([0-9]+\\)\\)"))
				(concat "^" x "\\([acd]\\)" x "$"))
  "Pattern to match lines produced by diff that describe differences.")

(ediff-defvar-local ediff-setup-diff-regions-function nil
  "value is a function symbol depending on the kind of job is to be done.
For 2-way jobs and for ediff-merge, it should be `ediff-setup-diff-regions'.
For jobs requiring diff3, it should be `ediff-setup-diff-regions3'.

The function should take three mandatory arguments, file-A, file-B, and
file-C. It may ignore file C for diff2 jobs. It should also take
one optional arguments, diff-number to refine.")

  
  
;;; Functions

;; Generate the difference vector and overlays for the two files
;; With optional arg REG-TO-REFINE, refine this region.
;; File-C argument is not used here. It is there just because
;; ediff-setup-diff-regions is called via a funcall to
;; ediff-setup-diff-regions-function, which can also have the value
;; ediff-setup-diff-regions3, which takes 4 arguments.
(defun ediff-setup-diff-regions (file-A file-B file-C)
  ;; force all minibuffers to display ediff's messages.
  ;; when xemacs implements minibufferless screens, this won't be necessary
  (if ediff-xemacs-p (setq synchronize-minibuffers t))
						  
  (or (ediff-buffer-live-p ediff-diff-buffer)
      (setq ediff-diff-buffer
	    (get-buffer-create (ediff-unique-buffer-name "*ediff-diff" "*"))))
  (or (ediff-buffer-live-p ediff-custom-diff-buffer)
      (setq ediff-custom-diff-buffer
	    (get-buffer-create
	     (ediff-unique-buffer-name "*ediff-custom-diff" "*"))))
  
  (ediff-eval-in-buffer ediff-diff-buffer
    (erase-buffer)
    ;; shell-command tends to display old shell command buffers even when it
    ;; puts output in another buffer---probably an Emacs bug.
    (ediff-kill-buffer-carefully "*Shell Command Output*")
    (let ((shell-file-name ediff-shell))
      (message "Computing differences ...") (sit-for 0)
      (shell-command
       (format "%s %s %s %s"
	       ediff-diff-program ediff-diff-options
	       (ediff-protect-metachars file-A)
	       (ediff-protect-metachars file-B))
       t)
      ))
  
  ;; compute custom diffs
  (let ((shell-file-name ediff-shell)
	(recomp-diff-key
	 (string-to-char
	  (substitute-command-keys "\\[ediff-update-diffs]"))))
    (if (or (= (buffer-size) 0)  ; was the first time
	    (eq last-command-char recomp-diff-key)) ; was a recompute cmd
	;; do asynchronously & insert output in ediff-custom-diff-buffer
	(ediff-shell-command
	 (format "%s %s %s %s"
		 ediff-custom-diff-program ediff-custom-diff-options
		 (ediff-protect-metachars file-A)
		 (ediff-protect-metachars file-B))
	 ediff-custom-diff-buffer)))
  
  (ediff-prepare-error-list ediff-diff-ok-lines-regexp ediff-diff-buffer)
  (message "Computing differences ... done")(sit-for 0)
  (ediff-convert-diffs-to-overlays
   (ediff-extract-diffs
    ediff-diff-buffer ediff-word-mode ediff-narrow-bounds))
  )
     
;; If file-A/B/C is nil, do 2-way comparison with the non-nil buffers
;; This function works for diff3 and diff2 jobs
(defun ediff-setup-fine-diff-regions (file-A file-B file-C reg-num)
  (or (ediff-buffer-live-p ediff-fine-diff-buffer)
      (setq ediff-fine-diff-buffer
	    (get-buffer-create
	     (ediff-unique-buffer-name "*ediff-fine-diff" "*"))))
  
  (let (diff3-job diff-program diff-options ok-regexp diff-list)
    (setq diff3-job ediff-3way-job
	  diff-program (if diff3-job ediff-diff3-program ediff-diff-program)
	  diff-options ""
	  ok-regexp (if diff3-job
			ediff-diff3-ok-lines-regexp
			ediff-diff-ok-lines-regexp))
    
    (ediff-eval-in-buffer ediff-fine-diff-buffer
      (erase-buffer)
      ;; shell-command tends to display old shell command buffers even when
      ;; it puts output in another buffer---probably an Emacs bug.
      (ediff-kill-buffer-carefully "*Shell Command Output*")
      (let ((shell-file-name ediff-shell))
	(message "Refining difference region %d ..." (1+ reg-num))
	(shell-command
	 (format "%s %s %s %s %s"
		 diff-program diff-options 
		 (if file-A 
		     (ediff-protect-metachars file-A) 
		   (ediff-protect-metachars file-B))
		 (if file-B
		     (ediff-protect-metachars file-B) 
		   (ediff-protect-metachars file-A))
		 (if diff3-job
		     (if file-C 
			 (ediff-protect-metachars file-C) 
		       (ediff-protect-metachars file-B))
		   "")
		 )
	 t)
	))
  
    (ediff-prepare-error-list ok-regexp ediff-fine-diff-buffer)
    (message "Refining difference region %d ... done" (1+ reg-num))
    
    (setq diff-list
	  (if diff3-job
	      (ediff-extract-diffs3
	       ediff-fine-diff-buffer '3way-comparison 'word-mode)
	    (ediff-extract-diffs ediff-fine-diff-buffer 'word-mode)))
    ;; fixup diff-list
    (if diff3-job
	(cond ((not file-A)
	       (mapcar (function (lambda (elt)
				   (aset elt 0 nil)
				   (aset elt 1 nil)))
		       (cdr diff-list)))
	      ((not file-B)
	       (mapcar (function (lambda (elt)
				   (aset elt 2 nil)
				   (aset elt 3 nil)))
		       (cdr diff-list)))
	      ((not file-C)
	       (mapcar (function (lambda (elt)
				   (aset elt 4 nil)
				   (aset elt 5 nil)))
		       (cdr diff-list)))
	  ))
    
    (ediff-convert-fine-diffs-to-overlays diff-list reg-num)
    ))
  
    
(defun ediff-prepare-error-list (ok-regexp diff-buff)
    (ediff-eval-in-buffer ediff-error-buffer
      (erase-buffer)
      (insert-buffer diff-buff)
      (delete-matching-lines ok-regexp)
      (if (memq system-type '(vax-vms axp-vms))
	  (delete-matching-lines "^$")))
    ;; If diff reports errors, show them then quit.
    (if (/= 0 (ediff-eval-in-buffer ediff-error-buffer (buffer-size)))
	(let ((ctl-buf ediff-control-buffer)
	      (error-buf ediff-error-buffer))
	  (ediff-skip-unsuitable-frames)
	  (switch-to-buffer error-buf)
	  (ediff-kill-buffer-carefully ctl-buf)
	  (error "Errors in diff output. Diff output is in %S" diff-buff))))

;; BOUNDS specifies visibility bounds to use.
;; WORD-MODE tells whether we are in the word-mode or not.
;; If WORD-MODE, also construct vector of diffs using word numbers.
;; Else, use point values.
;; This function handles diff-2 jobs including the case of
;; merging buffers and files without ancestor.
(defun ediff-extract-diffs (diff-buffer word-mode &optional bounds)
  (let ((A-buffer ediff-buffer-A)
	(B-buffer ediff-buffer-B)
	(C-buffer ediff-buffer-C)
	(a-prev 1) ; this is needed to set the first diff line correctly
	(b-prev 1)
	(c-prev 1)
	diff-list shift-A shift-B
	)
 
    ;; diff list contains word numbers, unless changed later
    (setq diff-list (cons (if word-mode 'words 'points)
			  diff-list))
    ;; we don't use visibility bounds for buffer C when merging
    (if bounds
	(setq shift-A
	      (ediff-overlay-start
	       (ediff-get-value-according-to-buffer-type 'A bounds))
	      shift-B 
	      (ediff-overlay-start
	       (ediff-get-value-according-to-buffer-type 'B bounds))))
    
    ;; reset point in buffers A/B/C
    (ediff-eval-in-buffer A-buffer
      (goto-char (if shift-A shift-A (point-min))))
    (ediff-eval-in-buffer B-buffer
      (goto-char (if shift-B shift-B (point-min))))
    (if (ediff-buffer-live-p C-buffer)
	(ediff-eval-in-buffer C-buffer
	  (goto-char (point-min))))
    
    (ediff-eval-in-buffer diff-buffer
      (goto-char (point-min))
      (while (re-search-forward ediff-match-diff-line nil t)
       (let* ((a-begin (string-to-int (buffer-substring (match-beginning 1)
							(match-end 1))))
	      (a-end  (let ((b (match-beginning 3))
			    (e (match-end 3)))
			(if b
			    (string-to-int (buffer-substring b e))
			  a-begin)))
	      (diff-type (buffer-substring (match-beginning 4) (match-end 4)))
	      (b-begin (string-to-int (buffer-substring (match-beginning 5)
							(match-end 5))))
	      (b-end (let ((b (match-beginning 7))
			   (e (match-end 7)))
		       (if b
			   (string-to-int (buffer-substring b e))
			 b-begin)))
	      a-begin-pt a-end-pt b-begin-pt b-end-pt
	      c-begin c-end c-begin-pt c-end-pt)
	 ;; fix the beginning and end numbers, because diff is somewhat
	 ;; strange about how it numbers lines
	 (if (string-equal diff-type "a")
	     (setq b-end (1+ b-end)
		   a-begin (1+ a-begin)
		   a-end a-begin)
	   (if (string-equal diff-type "d")
	       (setq a-end (1+ a-end)
		     b-begin (1+ b-begin)
		     b-end b-begin)
	     ;; (string-equal diff-type "c")
	     (setq a-end (1+ a-end)
		   b-end (1+ b-end))))
		   
	 (if (eq ediff-default-variant 'default-B)
	     (setq c-begin b-begin
		   c-end b-end)
	   (setq c-begin a-begin
		 c-end a-end))
	 
	 ;; compute main diff vector
	 (if word-mode
	     ;; make diff-list contain word numbers
	     (setq diff-list 
		   (nconc diff-list
			  (list
			   (if (ediff-buffer-live-p C-buffer)
			       (vector (- a-begin a-prev) (- a-end a-begin)
				       (- b-begin b-prev) (- b-end b-begin)
				       (- c-begin c-prev) (- c-end c-begin)
				       nil ; state of diff
				       nil ; state of merge
				       )
			     (vector (- a-begin a-prev) (- a-end a-begin)
				     (- b-begin b-prev) (- b-end b-begin)
				     nil nil ; dummy buf C
				     nil     ; state of diff
				     nil     ; state of merge
				     ))
			   ))
		   a-prev a-end
		   b-prev b-end
		   c-prev c-end)
	   ;; else convert lines to points
	   (ediff-eval-in-buffer A-buffer
	     (forward-line (- a-begin a-prev))
	     (setq a-begin-pt (point))
	     (forward-line (- a-end a-begin))
	     (setq a-end-pt (point)
		   a-prev a-end))
	   (ediff-eval-in-buffer B-buffer
	     (forward-line (- b-begin b-prev))
	     (setq b-begin-pt (point))
	     (forward-line (- b-end b-begin))
	     (setq b-end-pt (point)
		   b-prev b-end))
	   (if (ediff-buffer-live-p C-buffer)
	       (ediff-eval-in-buffer C-buffer
		 (forward-line (- c-begin c-prev))
		 (setq c-begin-pt (point))
		 (forward-line (- c-end c-begin))
		 (setq c-end-pt (point)
		       c-prev c-end)))
	   (setq diff-list 
		 (nconc
		  diff-list
		  (list
		   (if (ediff-buffer-live-p C-buffer)
		       (vector
			a-begin-pt a-end-pt b-begin-pt b-end-pt
			c-begin-pt c-end-pt
			;; state of diff
			;; shows which buff is different from the other two
			(if (eq ediff-default-variant 'default-B) 'A 'B)
			;; state of merge
			ediff-default-variant
			)
		     (vector a-begin-pt a-end-pt b-begin-pt b-end-pt
			     nil nil ; dummy buf C
			     nil nil ; dummy state of diff & merge
			     ))) 
		  )))
		  
	 ))) ; end ediff-eval-in-buffer
    diff-list
    ))
    

(defun ediff-set-diff-overlays-in-one-buffer (buf-type diff-list)
  (let* ((current-diff -1)
	 (buff (ediff-get-buffer buf-type))
	 ;; ediff-extract-diffs puts the type of diff-list as the first elt
	 ;; of this list. The type is either 'points or 'words
	 (diff-list-type (car diff-list))
	 (shift (ediff-overlay-start
		 (ediff-get-value-according-to-buffer-type
		  buf-type ediff-narrow-bounds)))
	 (limit (ediff-overlay-end
		 (ediff-get-value-according-to-buffer-type 
		  buf-type ediff-narrow-bounds)))
	 diff-overlay-list list-element total-diffs
	 begin end pt-saved overlay state-of-diff state-of-merge)

    (setq diff-list (cdr diff-list)) ; discard diff list type
    (setq total-diffs (length diff-list))
      
    ;; shift, if necessary
    (ediff-eval-in-buffer buff (setq pt-saved shift))
	   
    (while diff-list
      (setq current-diff (1+ current-diff)
	    list-element (car diff-list)
	    begin 	 (aref list-element (cond ((eq buf-type 'A) 0)
						  ((eq buf-type 'B) 2)
						  (t 4)))  ; buff C
	    end 	 (aref list-element (cond ((eq buf-type 'A) 1)
						  ((eq buf-type 'B) 3)
						  (t 5)))  ; buff C
	    state-of-diff (aref list-element 6)
	    state-of-merge (aref list-element 7))
		
      (cond ((and (not (eq buf-type state-of-diff))
		  (memq state-of-diff '(A B C)))
	     (setq state-of-diff
		   (car (delq buf-type (delq state-of-diff (list 'A 'B 'C)))))
	     (setq state-of-diff (format "=diff(%S)" state-of-diff))
	     (if (and ediff-merge-job state-of-merge (eq buf-type 'C))
		 (setq state-of-merge (format "%S" state-of-merge)))
	     )
	    (t (setq state-of-diff nil
		     state-of-merge nil)))
	    
      ;; Put overlays at appropriate places in buffer
      ;; convert word numbers to points, if necessary
      (if (eq diff-list-type 'words)
	  (progn
	    (ediff-eval-in-buffer buff (goto-char pt-saved))
	    (setq begin (ediff-goto-word (1+ begin) buff)
		  end (ediff-goto-word end buff 'end))
	    (if (> end limit) (setq end limit))
	    (if (> begin end) (setq begin end))
	    (setq pt-saved (ediff-eval-in-buffer buff (point)))))
      (setq overlay (ediff-make-bullet-proof-overlay begin end buff))
      
      ;; Priorities of overlays should be equal in all ediff control
      ;; panel buffers. Otherwise it won't work due to Emacs
      ;; bug, as insert-in-front-hooks will be called 
      ;; only on behalf of the buffer with higher priority.
      (ediff-overlay-put overlay 'priority ediff-shadow-overlay-priority)
      (ediff-overlay-put overlay 'ediff-diff-num current-diff)
      (ediff-overlay-put 
       overlay 'insert-in-front-hooks '(ediff-insert-in-front))
      (ediff-overlay-put
       overlay 'face (if (ediff-odd-p current-diff)
			 (intern 
			  (format "ediff-odd-diff-face-%S-var" buf-type))
		       (intern
			(format "ediff-even-diff-face-%S-var" buf-type))))

      (if (= 0 (mod current-diff 10))
	  (message "Buffer %S: Processing difference region %d of %d"
		   buf-type current-diff total-diffs))
      ;; record all overlays for this difference
      ;; the second elt, nill, is a place holder for the fine diff vector.
      ;; the last nil is a place holder for no-fine-diffs flag.
      (setq diff-overlay-list
	    (nconc
	     diff-overlay-list
	     (list (vector overlay nil nil state-of-diff state-of-merge)))
	    diff-list
	    (cdr diff-list))
      ) ; while
      
    (set (intern (format "ediff-difference-vector-%S" buf-type))
	 (apply 'vector diff-overlay-list))
    ))
	 
;; `n' is the diff region to work on.  Default is ediff-current-difference.
;; if `flag' is 'noforce then make fine-diffs only if this region's fine
;; diffs have not been computed before.
;; if `flag' is 'skip then don't compute fine diffs for this region.
;;
(defun ediff-make-fine-diffs (&optional n flag)       
  (or n  (setq n ediff-current-difference))
  
  (if (< ediff-number-of-differences 1)
      (error "No differences found"))
      
  (if ediff-word-mode
      (setq flag 'skip
	    ediff-auto-refine 'nix))
  
  (or (< n 0)
      (>= n ediff-number-of-differences)
      ;; n is within the range
      (let ((tmp-buffer (get-buffer-create ediff-tmp-buffer))
	    (file-A ediff-temp-file-A)
	    (file-B ediff-temp-file-B)
	    (file-C ediff-temp-file-C)
	    (empty-A (ediff-empty-diff-region-p n 'A))
	    (empty-B (ediff-empty-diff-region-p n 'B))
	    (empty-C (ediff-empty-diff-region-p n 'C))
	    (whitespace-A (ediff-whitespace-diff-region-p n 'A))
	    (whitespace-B (ediff-whitespace-diff-region-p n 'B))
	    (whitespace-C (ediff-whitespace-diff-region-p n 'C))
	    cumulative-fine-diff-length)
	
	(cond ((and (eq flag 'noforce) (ediff-get-fine-diff-vector n 'A))
	       ;; don't compute fine diffs if diff vector exists
	       (if (and (ediff-no-fine-diffs-p n) ediff-verbose-p)
		   (message "Only white-space differences in region %d"
			    (1+ n))))
	      ;; if one of the regions is empty (or 2 in 3way comparison
	      ;; then don't refine
	      ((> (length (delq nil (list empty-A empty-B empty-C))) 1)
	       (if (and (ediff-looks-like-combined-merge n)
			ediff-merge-job)
		   (ediff-set-fine-overlays-in-one-buffer 'C nil n))
	       (if ediff-verbose-p
		   (if ediff-3way-comparison-job
		       (message
			"Region %d is empty in all buffers but %S"
			(1+ n) 
			(cond ((not empty-A) 'A)
			      ((not empty-B) 'B)
			      ((not empty-C) 'C)))
		     (message
		      "Region %d in buffer %S is empty"
		      (1+ n) 
		      (cond (empty-A 'A)
			    (empty-B 'B)
			    (empty-C 'C)))
		     ))
	       ;; if all regions happen to be empty, say, as a result of
	       ;; deletions from non-empty regions, then mark as space only
	       (if (and empty-A empty-B empty-C)
		   (ediff-mark-diff-as-space-only n t)
		 (ediff-mark-diff-as-space-only n nil)))
	      ;; don't compute fine diffs for this region
	      ((eq flag 'skip)
	       (or (ediff-get-fine-diff-vector n 'A)
		   (memq ediff-auto-refine '(off nix))
		   (not ediff-verbose-p)
		   (message
		    "Region %d exceeds auto-refine limit. `%s' force-refines"
		    (1+ n)
		    (substitute-command-keys "\\[ediff-make-or-kill-fine-diffs]")
		    )))
	      (t
	       ;; delete old fine diffs
	       ;;(ediff-clear-fine-differences n)
	       ;; recompute fine diffs
	       (ediff-wordify
		(ediff-get-diff-posn 'A 'beg n)
		(ediff-get-diff-posn 'A 'end n)
		ediff-buffer-A
		tmp-buffer
		ediff-control-buffer)
	       (ediff-eval-in-buffer tmp-buffer
		 (setq file-A (ediff-make-temp-file "fineDiffA" file-A)))
	       
	       (ediff-wordify
		(ediff-get-diff-posn 'B 'beg n)
		(ediff-get-diff-posn 'B 'end n)
		ediff-buffer-B
		tmp-buffer
		ediff-control-buffer)
	       (ediff-eval-in-buffer tmp-buffer
		 (setq file-B (ediff-make-temp-file "fineDiffB" file-B)))
		 
	       (if ediff-3way-job
		   (progn
		     (ediff-wordify
		      (ediff-get-diff-posn 'C 'beg n)
		      (ediff-get-diff-posn 'C 'end n)
		      ediff-buffer-C
		      tmp-buffer
		      ediff-control-buffer)
		     (ediff-eval-in-buffer tmp-buffer
		       (setq file-C
			     (ediff-make-temp-file "fineDiffC" file-C)))))
	       
	       ;; save temp file names.
	       (setq ediff-temp-file-A file-A
		     ediff-temp-file-B file-B
		     ediff-temp-file-C file-C)
	       
	       ;; set the new vector of fine diffs, if none exists
	       (cond ((and ediff-3way-job whitespace-A)
		      (ediff-setup-fine-diff-regions nil file-B file-C n))
		     ((and ediff-3way-job whitespace-B)
		      (ediff-setup-fine-diff-regions file-A nil file-C n))
		     ((and ediff-3way-job
			   (or whitespace-C
			       (and ediff-merge-job
				    (ediff-looks-like-combined-merge n))))
		      (ediff-setup-fine-diff-regions file-A file-B nil n))
		     (t
		      (ediff-setup-fine-diff-regions file-A file-B file-C n)))
		      
	       (setq cumulative-fine-diff-length
		     (+ (length (ediff-get-fine-diff-vector n 'A))
				   (length (ediff-get-fine-diff-vector n 'B))
				   (if file-C
				       (length
					(ediff-get-fine-diff-vector n 'C))
				     0)))
		      
	       (cond ((or
		       ;; all regions are white space
		       (and whitespace-A whitespace-B whitespace-C)
		       ;; none is white space and no fine diffs detected
		       (and (not whitespace-A)
			    (not whitespace-B)
			    (not (and ediff-3way-job whitespace-C))
			    (eq cumulative-fine-diff-length 0)))
		      (ediff-mark-diff-as-space-only n t)
		      (message
		       "Only white-space differences in region %d" (1+ n)))
		     ((eq cumulative-fine-diff-length 0)
		      (ediff-mark-diff-as-space-only n nil)
		      (message
		       "Only white-space differences in region %d %s"
		       (1+ n)
		       (cond (whitespace-A "in buffers B & C")
			     (whitespace-B "in buffers A & C")
			     (whitespace-C "in buffers A & B"))))
		     (t 
		      (ediff-mark-diff-as-space-only n nil)))
	       )
	      ) ; end cond
	(ediff-set-fine-diff-properties n)
	)))
	
;; Interface to ediff-make-fine-diffs. Checks for auto-refine limit, etc.
(defun ediff-install-fine-diff-if-necessary (n)
  (cond ((eq ediff-auto-refine 'on)
	 (if (and
	      (> ediff-auto-refine-limit
		 (- (ediff-get-diff-posn 'A 'end n)
		    (ediff-get-diff-posn 'A 'beg n)))
	      (> ediff-auto-refine-limit
		 (- (ediff-get-diff-posn 'B 'end n)
		    (ediff-get-diff-posn 'B 'beg n))))
	     (ediff-make-fine-diffs n 'noforce)
	   (ediff-make-fine-diffs n 'skip)))
	
	;; highlight iff fine diffs already exist
	((eq ediff-auto-refine 'off)
	 (ediff-make-fine-diffs n 'skip))))
    
    
;; if fine diff vector is not set for diff N, then do nothing
(defun ediff-set-fine-diff-properties (n &optional default)
  (or (not window-system)
      (< n 0)
      (>= n ediff-number-of-differences)
      ;; in a window system, set faces and priorities of fine overlays
      (progn
	(ediff-set-fine-diff-properties-in-one-buffer 'A n default)
	(ediff-set-fine-diff-properties-in-one-buffer 'B n default)
	(if ediff-3way-job
	    (ediff-set-fine-diff-properties-in-one-buffer 'C n default)))))
	
(defun ediff-set-fine-diff-properties-in-one-buffer (buf-type
						     n &optional default)
  (let ((fine-diff-vector  (ediff-get-fine-diff-vector n buf-type))
	(face (if default 
		  'default
		(face-name
		 (intern (format "ediff-fine-diff-face-%S" buf-type)))))
;;		(if (and (eq buf-type 'C)
;;			 (ediff-looks-like-combined-merge n))
;;		    'ediff-combined-diff-face
;;		  (face-name
;;		   (intern (format "ediff-fine-diff-face-%S" buf-type))))))
	(priority (if default
		      0
		    (1+ (or (ediff-overlay-get
			     (symbol-value
			      (intern
			       (format
				"ediff-current-diff-overlay-%S" buf-type)))
			     'priority)
			    0)))))
    (mapcar
     (function (lambda (overl)
		 (ediff-overlay-put overl 'face face)
		 (ediff-overlay-put overl 'priority priority)))
     fine-diff-vector)))
     
;; This assumes buffer C and that the region looks like a combination of
;; regions in buffer A and C.
(defun ediff-set-fine-overlays-for-combined-merge (diff-list reg-num)
  (let (overlay1 overlay2 overlay3)
    (setq overlay1 (ediff-make-bullet-proof-overlay (nth 0 diff-list)
						    (nth 1 diff-list)
						    ediff-buffer-C)
	  overlay2 (ediff-make-bullet-proof-overlay (nth 2 diff-list)
						    (nth 3 diff-list)
						    ediff-buffer-C)
	  overlay3 (ediff-make-bullet-proof-overlay (nth 4 diff-list)
						    (nth 5 diff-list)
						    ediff-buffer-C))
    (ediff-set-fine-diff-vector reg-num 'C (vector overlay1 overlay2 overlay3))
    ))
	
    
;; Convert diff list to overlays for a given DIFF-REGION
;; in buffer of type BUF-TYPE
(defun ediff-set-fine-overlays-in-one-buffer (buf-type diff-list region-num)
  (let* ((current-diff -1)
	 (reg-start (ediff-get-diff-posn buf-type 'beg region-num))
	 (buff (ediff-get-buffer buf-type))
	 combined-merge-diff-list
	 diff-overlay-list list-element
	 begin end overlay)

    (ediff-clear-fine-differences-in-one-buffer region-num buf-type)
    (setq diff-list (cdr diff-list)) ; discard list type (words or points)
    (ediff-eval-in-buffer buff (goto-char reg-start))
    
    ;; if it is a combined merge then set overlays in buff C specially
    (if (and ediff-merge-job (eq buf-type 'C)
	     (setq combined-merge-diff-list
		   (ediff-looks-like-combined-merge region-num)))
	(ediff-set-fine-overlays-for-combined-merge
	 combined-merge-diff-list region-num)
      ;; regular fine diff
      (while diff-list
	(setq current-diff (1+ current-diff)
	      list-element (car diff-list)
	      begin 	 (aref list-element (cond ((eq buf-type 'A) 0)
						  ((eq buf-type 'B) 2)
						  (t 4)))  ; buf C
	      end 	 (aref list-element (cond ((eq buf-type 'A) 1)
						  ((eq buf-type 'B) 3)
						  (t 5)))) ; buf C
	(if (not (or begin end))
	    () ; skip this diff
	  ;; Put overlays at appropriate places in buffers
	  ;; convert lines to points, if necessary
	  (setq begin (ediff-goto-word (1+ begin) buff)
		end (ediff-goto-word end buff 'end))
	  (setq overlay (ediff-make-bullet-proof-overlay begin end buff))
	  ;; record all overlays for this difference region
	  (setq diff-overlay-list (nconc diff-overlay-list (list overlay))))
	
	(setq diff-list (cdr diff-list))
	) ; while
      ;; convert the list of difference information into a vector
      ;; for fast access
      (ediff-set-fine-diff-vector 
       region-num buf-type (apply 'vector diff-overlay-list))
      )))


;; Stolen from emerge.el
(defun ediff-get-diff3-group (file)
  ;; This save-excursion allows ediff-get-diff3-group to be called for the
  ;; various groups of lines (1, 2, 3) in any order, and for the lines to
  ;; appear in any order.  The reason this is necessary is that Gnu diff3
  ;; can produce the groups in the order 1, 2, 3 or 1, 3, 2.
  (save-excursion
    (re-search-forward
     (concat "^" file ":\\([0-9]+\\)\\(,\\([0-9]+\\)\\)?\\([ac]\\)$"))
    (beginning-of-line 2)
    ;; treatment depends on whether it is an "a" group or a "c" group
    (if (string-equal (buffer-substring (match-beginning 4) (match-end 4)) "c")
	;; it is a "c" group
	(if (match-beginning 2)
	    ;; it has two numbers
	    (list (string-to-int
		   (buffer-substring (match-beginning 1) (match-end 1)))
		  (1+ (string-to-int
		       (buffer-substring (match-beginning 3) (match-end 3)))))
	  ;; it has one number
	  (let ((x (string-to-int
		    (buffer-substring (match-beginning 1) (match-end 1)))))
	    (list x (1+ x))))
      ;; it is an "a" group
      (let ((x (1+ (string-to-int
		    (buffer-substring (match-beginning 1) (match-end 1))))))
	(list x x)))))


;; If WORD-MODE, construct vector of diffs using word numbers.
;; Else, use point values.
;; WORD-MODE also tells if we are in the word-mode or not.
;; If THREE-WAY-COMP, then it is a 3-way comparison. Else, it is merging
;; with ancestor, in which case buffer-C contents is identical to buffer-A
;; contents (unless buffer-A is narrowed.
;; BOUNDS specifies visibility bounds to use.
(defun ediff-extract-diffs3 (diff-buffer word-mode three-way-comp
					  &optional bounds)
  (let ((A-buffer ediff-buffer-A)
	(B-buffer ediff-buffer-B)
	(C-buffer ediff-buffer-C)
	(a-prev 1) ; needed to set the first diff line correctly
	(b-prev 1)
	(c-prev 1)
	diff-list shift-A shift-B shift-C
	)
 
    ;; diff list contains word numbers or points, depending on word-mode
    (setq diff-list (cons (if word-mode 'words 'points)
			  diff-list))
    (if bounds
	(setq shift-A
	      (ediff-overlay-start
	       (ediff-get-value-according-to-buffer-type 'A bounds))
	      shift-B 
	      (ediff-overlay-start
	       (ediff-get-value-according-to-buffer-type 'B bounds))
	      shift-C 
	      (if three-way-comp
		  (ediff-overlay-start
		   (ediff-get-value-according-to-buffer-type 'C bounds)))))
    
    ;; reset point in buffers A, B, C
    (ediff-eval-in-buffer A-buffer
      (goto-char (if shift-A shift-A (point-min))))
    (ediff-eval-in-buffer B-buffer
      (goto-char (if shift-B shift-B (point-min))))
    (ediff-eval-in-buffer C-buffer
      (goto-char (if shift-C shift-C (point-min))))
    
    (ediff-eval-in-buffer diff-buffer
      (goto-char (point-min))
      (while (re-search-forward ediff-match-diff3-line nil t)
	;; leave point after matched line
       (beginning-of-line 2)
       (let ((agreement (buffer-substring (match-beginning 1) (match-end 1))))
	 ;; if the A and B files are the same and not 3way-comparison,
	 ;; ignore the difference
	 (if (or three-way-comp (not (string-equal agreement "3")))
	     (let* ((a-begin (car (ediff-get-diff3-group "1")))
		    (a-end  (nth 1 (ediff-get-diff3-group "1")))
		    (b-begin (car (ediff-get-diff3-group "2")))
		    (b-end (nth 1 (ediff-get-diff3-group "2")))
		    (state-of-merge
		     (cond ((string-equal agreement "1") 'prefer-A)
			   ((string-equal agreement "2") 'prefer-B)
			   (t ediff-default-variant)))
		    (state-of-diff-merge
		     (if (memq state-of-merge '(default-A prefer-A)) 'B 'A))
		    (state-of-diff-comparison
		     (cond ((string-equal agreement "1") 'A)
			   ((string-equal agreement "2") 'B)
			   ((string-equal agreement "3") 'C)))
		    c-begin c-end
		    a-begin-pt a-end-pt
		    b-begin-pt b-end-pt c-begin-pt c-end-pt)
		    
	       (if three-way-comp
		   (setq c-begin (car (ediff-get-diff3-group "3"))
			 c-end (nth 1 (ediff-get-diff3-group "3")))
		 (if (eq ediff-default-variant 'default-B)
		     (setq c-begin b-begin
			   c-end b-end)
		   (setq c-begin a-begin
			 c-end a-end)))
	 
	       ;; compute main diff vector
	       (if word-mode
		   ;; make diff-list contain word numbers
		   (setq diff-list 
			 (nconc diff-list
				(list (vector
				       (- a-begin a-prev) (- a-end a-begin)
				       (- b-begin b-prev) (- b-end b-begin)
				       (- c-begin c-prev) (- c-end c-begin)
				       nil nil))) ; state of diff & merge
			 a-prev a-end
			 b-prev b-end
			 c-prev c-end)
		 ;; else convert lines to points
		 (ediff-eval-in-buffer A-buffer
		   (forward-line (- a-begin a-prev))
		   (setq a-begin-pt (point))
		   (forward-line (- a-end a-begin))
		   (setq a-end-pt (point)
			 a-prev a-end))
		 (ediff-eval-in-buffer B-buffer
		   (forward-line (- b-begin b-prev))
		   (setq b-begin-pt (point))
		   (forward-line (- b-end b-begin))
		   (setq b-end-pt (point)
			 b-prev b-end))
		 (ediff-eval-in-buffer C-buffer
		   (forward-line (- c-begin c-prev))
		   (setq c-begin-pt (point))
		   (forward-line (- c-end c-begin))
		   (setq c-end-pt (point)
			 c-prev c-end))
		 (setq diff-list 
		       (nconc
			diff-list
			;; if comparing with ancestor, then there also is a
			;; state-of-difference marker
			(if three-way-comp
			    (list (vector
				   a-begin-pt a-end-pt
				   b-begin-pt b-end-pt
				   c-begin-pt c-end-pt
				   state-of-diff-comparison
				   nil           ; state of merge
				   ))
			  (list (vector a-begin-pt a-end-pt
					b-begin-pt b-end-pt
					c-begin-pt c-end-pt
					state-of-diff-merge
					state-of-merge
					)))
			)))
	       ))
	       
	 ))) ; end ediff-eval-in-buffer
    diff-list
    ))
    
;; Generate the difference vector and overlays for three files
;; File-C is either the third file to compare (in case of 3-way comparison)
;; or it is the ancestor file.
(defun ediff-setup-diff-regions3 (file-A file-B file-C)
  
  ;; force all minibuffers to display ediff's messages.
  ;; when xemacs implements minibufferless screens, this won't be necessary
  (if ediff-xemacs-p (setq synchronize-minibuffers t))
						  
  (or (ediff-buffer-live-p ediff-diff-buffer)
      (setq ediff-diff-buffer
	    (get-buffer-create (ediff-unique-buffer-name "*ediff-diff" "*"))))
  
  (ediff-eval-in-buffer ediff-diff-buffer
    (erase-buffer)
    ;; shell-command tends to display old shell command buffers even when it
    ;; puts output in another buffer---probably an Emacs bug.
    (ediff-kill-buffer-carefully "*Shell Command Output*")
    (let ((shell-file-name ediff-shell))
      (message "Computing differences ...")(sit-for 0)
      (shell-command
       (format "%s %s %s %s %s"
	       ediff-diff3-program ediff-diff3-options
	       (ediff-protect-metachars file-A)
	       (ediff-protect-metachars file-B)
	       (ediff-protect-metachars file-C))
       t)
      ))
  
  (ediff-prepare-error-list ediff-diff3-ok-lines-regexp ediff-diff-buffer)
  (message "Computing differences ... done")(sit-for 0)
  (ediff-convert-diffs-to-overlays
   (ediff-extract-diffs3
    ediff-diff-buffer
    ediff-word-mode ediff-3way-comparison-job ediff-narrow-bounds)
   ))
   

;; Execute shell COMMAND asynchronously and insert output in BUFFER.
;; BUFFER must be a buffer object, and must be alive.
;; There should be NO ampersand at the end of the command.
;; We introduce this command because Emacs shell-command doesn't let us
;; insert output in current buffer asynchronously.
;; Most of this command was stolen from Emacs shell-command."
(defun ediff-shell-command (command buffer)
  (let ((data (match-data)))
    (unwind-protect
	(let ((directory default-directory)
	      proc)
	  ;; If will kill a process, query first.
	  (setq proc (get-buffer-process buffer))
	  (if proc (kill-process proc))
	  (save-excursion
	    (set-buffer buffer)
	    (erase-buffer)
	    (setq default-directory directory)
	    (setq proc (start-process "Shell" buffer
				      shell-file-name "-c" command))
	    (setq mode-line-process '(":%s"))
	    (set-process-sentinel proc 'ediff-shell-command-sentinel)
	    (set-process-filter proc 'ediff-shell-command-filter)
	    ))
      (store-match-data data))))
      
;; This is shell-command-filter from simple.el in FSF Emacs.
;; Copied here because XEmacs doesn't have it.
(defun ediff-shell-command-filter (proc string)
  ;; Do save-excursion by hand so that we can leave point numerically unchanged
  ;; despite an insertion immediately after it.
  (let* ((obuf (current-buffer))
         (buffer (process-buffer proc))
         opoint
         (window (get-buffer-window buffer))
         (pos (window-start window)))
    (unwind-protect
        (progn
          (set-buffer buffer)
          (or (= (point) (point-max))
              (setq opoint (point)))
          (goto-char (point-max))
          (insert-before-markers string))
      ;; insert-before-markers moved this marker: set it back.
      (set-window-start window pos)
      ;; Finish our save-excursion.
      (if opoint
          (goto-char opoint))
      (set-buffer obuf))))
      
;; like shell-command-sentinel but doesn't print an exit status message
;; we do this because diff always exits with status 1, if diffs are found
;; so shell-command-sentinel displays a confusing message to the user
(defun ediff-shell-command-sentinel (process signal)
  (if (and (memq (process-status process) '(exit signal))
           (buffer-name (process-buffer process)))
      (progn
        (save-excursion
          (set-buffer (process-buffer process))
          (setq mode-line-process nil))
        (delete-process process))))
	

;;; Word functions used to refine the current diff	    

(defvar ediff-forward-word-function 'ediff-forward-word
  "*Function to call to move to the next word.
Used for splitting difference regions into individual words.")

(defvar ediff-whitespace " \n\t\f"
  "*Characters constituting white space.
These characters are ignored when differing regions are split into words.")

;;(defvar ediff-word-1 "a-zA-Z---_`'.?!:"
(defvar ediff-word-1 "a-zA-Z---_"
  "*Characters that constitute words of type 1.
More precisely, [ediff-word-1] is a regexp that matches type 1 words.
See `ediff-forward-word' for more details.")  

(defvar ediff-word-2 "0-9.,"
  "*Characters that constitute words of type 2.
More precisely, [ediff-word-2] is a regexp that matches type 2 words.
See `ediff-forward-word' for more details.")

(defvar ediff-word-3 "`'?!:;\"{}[]()"
  "*Characters that constitute words of type 3.
More precisely, [ediff-word-3] is a regexp that matches type 3 words.
See `ediff-forward-word' for more details.")

(defvar ediff-word-4
  (concat "^" ediff-word-1 ediff-word-2 ediff-word-3 ediff-whitespace)
  "*Characters that constitute words of type 4.
More precisely, [ediff-word-4] is a regexp that matches type 4 words.
See `ediff-forward-word' for more details.")  

;; Split region along word boundaries. Each word will be on its own line.
;; Output to buffer out-buffer.
(defun ediff-forward-word ()
  "Move point one word forward.
There are four types of words, each of which consists entirely of
characters in `ediff-word-1', `ediff-word-2', `ediff-word-3', or
`ediff-word-4'.  Words are recognized by passing these in turn as the
argument to `skip-chars-forward'."
  (or (> (skip-chars-forward ediff-word-1) 0)
      (> (skip-chars-forward ediff-word-2) 0)
      (> (skip-chars-forward ediff-word-3) 0)
      (> (skip-chars-forward ediff-word-4) 0)
      ))

(defun ediff-wordify (beg end in-buffer out-buffer &optional control-buf)
  (let (sv-point string)
    (save-excursion
     (set-buffer in-buffer)
     (setq string (buffer-substring beg end))

     (set-buffer out-buffer)
     (erase-buffer)
     (insert string)
     (goto-char (point-min))
     (skip-chars-forward ediff-whitespace)
     (delete-region (point-min) (point))
     
     (while (not (eobp))
       ;; eval incontrol buf to let user create local versions for
       ;; different invocations
       (if control-buf
	   (funcall 
	    (ediff-eval-in-buffer control-buf ediff-forward-word-function))
	 (funcall ediff-forward-word-function))
       (setq sv-point (point))
       (skip-chars-forward ediff-whitespace)
       (delete-region sv-point (point))
       (insert "\n")))))
       
;; copy string from BEG END from IN-BUF to OUT-BUF
(defun ediff-copy-to-buffer (beg end in-buffer out-buffer)
  (let (string)
    (save-excursion
      (set-buffer in-buffer)
     (setq string (buffer-substring beg end))

     (set-buffer out-buffer)
     (erase-buffer)
     (insert string)
     (goto-char (point-min)))))


;; goto word #n starting at current position in buffer `buf'
;; For ediff, a word is either a string of a-z,A-Z, incl `-' and `_';
;; or a string of other non-blanks. A blank is a \n\t\f
;; If `flag' is non-nil, goto the end of the n-th word.
(defun ediff-goto-word (n buf &optional flag)
  ;; remember val ediff-forward-word-function has in ctl buf
  (let ((fwd-word-fun ediff-forward-word-function))
    (ediff-eval-in-buffer buf
      (skip-chars-forward ediff-whitespace)
      (while (> n 1)
	(funcall fwd-word-fun)
	(skip-chars-forward ediff-whitespace)
	(setq n (1- n)))
      (if (and flag (> n 0))
	  (funcall fwd-word-fun))
      (point))))


(provide 'ediff-diff)


;; ediff-diff.el ends here
