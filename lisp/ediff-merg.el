;;; ediff-merg.el --- merging utilities
;;; Copyright (C) 1994, 1995 Free Software Foundation, Inc.

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

(defvar ediff-default-variant 'default-A
  "*The variant to be used as a default for buffer C in merging.
Valid values are the symbols `default-A', `default-B', and `combined'.")

(defvar ediff-combination-pattern 
  '("#ifdef NEW /* variant A */" "#else /* variant B */" "#endif /* NEW */")
  "*Pattern to be used for combining difference regions in buffers A and B.
The value is (STRING1 STRING2 STRING3). The combined text will look like this:

STRING1
diff region from variant A
STRING2
diff region from variant B
STRING3
")

(ediff-defvar-local ediff-show-clashes-only  nil
  "*If t, show only those diff regions where both buffers disagree with the ancestor.
This means that regions that have status prefer-A or prefer-B will be
skiped over. Nil means show all regions.")

	
(defsubst ediff-get-combined-region (n)
  (concat (nth 0 ediff-combination-pattern) "\n"
	  (ediff-get-region-contents n 'A ediff-control-buffer)
	  (nth 1 ediff-combination-pattern) "\n"
	  (ediff-get-region-contents n 'B ediff-control-buffer)
	  (nth 2 ediff-combination-pattern) "\n"))
    
(defsubst ediff-set-state-of-all-diffs-in-all-buffers (ctl-buf)
  (let ((n 0))
    (while (< n ediff-number-of-differences)
      (ediff-set-state-of-diff-in-all-buffers n ctl-buf)
      (setq n (1+ n)))))
     
(defun ediff-set-state-of-diff-in-all-buffers (n ctl-buf)
  (let ((regA (ediff-get-region-contents n 'A ctl-buf))
	(regB (ediff-get-region-contents n 'B ctl-buf))
	(regC (ediff-get-region-contents n 'C ctl-buf)))
    (cond ((and (string= regA regB) (string= regA  regC))
	   (ediff-set-state-of-diff n 'A "=diff(B)")
	   (ediff-set-state-of-diff n 'B "=diff(C)")
	   (ediff-set-state-of-diff n 'C "=diff(A)"))
	  ((string= regA regB)
	   (ediff-set-state-of-diff n 'A "=diff(B)")
	   (ediff-set-state-of-diff n 'B "=diff(A)")
	   (ediff-set-state-of-diff n 'C nil))
	  ((string= regA regC)
	   (ediff-set-state-of-diff n 'A "=diff(C)")
	   (ediff-set-state-of-diff n 'C "=diff(A)")
	   (ediff-set-state-of-diff n 'B nil))
	  ((string= regB regC)
	   (ediff-set-state-of-diff n 'C "=diff(B)")
	   (ediff-set-state-of-diff n 'B "=diff(C)")
	   (ediff-set-state-of-diff n 'A nil))
	  ((string= regC (ediff-get-combined-region n))
	   (ediff-set-state-of-diff n 'A nil)
	     (ediff-set-state-of-diff n 'B nil)
	     (ediff-set-state-of-diff n 'C "=diff(A+B)"))
	  (t (ediff-set-state-of-diff n 'A nil)
	     (ediff-set-state-of-diff n 'B nil)
	     (ediff-set-state-of-diff n 'C nil)))
    ))
    
(defun ediff-set-merge-mode ()
  ;; by Stig@hackvan.com
  (normal-mode t)
  (remove-hook 'local-write-file-hooks 'ediff-set-merge-mode))
	
;; Go over all diffs starting with DIFF-NUM and copy regions into buffer C
;; according to the state of the difference.
;; Since ediff-copy-diff refuses to copy identical diff regions, there is
;; no need to optimize ediff-do-merge any further.
;;
;; If re-merging, change state of merge in all diffs starting with
;; DIFF-NUM, except those where the state is prefer-* or where it is
;; `default-*' or `combined' but the buf C region appears to be modified
;; since last set by default.
(defun ediff-do-merge (diff-num &optional remerging)
  (if (< diff-num 0) (setq diff-num 0))
  (let ((n diff-num)
	(default-state-of-merge (format "%S" ediff-default-variant))
	do-not-copy state-of-merge)
    (while (< n ediff-number-of-differences)
      (if (= (mod n 10) 0)
	  (message "%s buffers A & B into C ... region %d of %d"
		   (if remerging "Re-merging" "Merging")
		   n
		   ediff-number-of-differences))
	     
      (setq state-of-merge (ediff-get-state-of-merge n))
      (setq do-not-copy (string= state-of-merge default-state-of-merge))

      (if remerging
	  (let ((reg-A (ediff-get-region-contents n 'A ediff-control-buffer))
		(reg-B (ediff-get-region-contents n 'B ediff-control-buffer))
		(reg-C (ediff-get-region-contents n 'C ediff-control-buffer)))
		
		    ;;; was edited since first set by default
	    (if (or (and (string= state-of-merge "default-A")
		      (not (string= reg-A reg-C)))
		    ;; was edited since first set by default
		    (and (string= state-of-merge "default-B")
			 (not (string= reg-B reg-C)))
		    ;; was edited since first set by default
		    (and (string= state-of-merge "combined")
			 (not (string=
			       (ediff-make-combined-diff reg-A reg-B) reg-C)))
		    ;; was prefered--ignore
		    (string-match "prefer" state-of-merge))
		(setq do-not-copy t))
		
	    ;; change state of merge for this diff, if necessary
	    (if (and (string-match "\\(default\\|combined\\)" state-of-merge)
		     (not do-not-copy))
		(ediff-set-state-of-merge
		 n (format "%S" ediff-default-variant)))
	    ))
	  
      ;; state-of-merge may have changed via ediff-set-state-of-merge, so
      ;; check it once again
      (setq state-of-merge (ediff-get-state-of-merge n))
      
      (or do-not-copy
	  (if (string= state-of-merge "combined")
	      ;; use n+1 because ediff-combine-diffs works via user numbering
	      ;; of diffs, which is 1+ to what ediff uses internally
	      (ediff-combine-diffs (1+ n) 'batch)
	    (ediff-copy-diff 
	     n (if (string-match "-A" state-of-merge) 'A 'B) 'C 'batch)))
      (setq n (1+ n)))
    (message "Merging buffers A & B into C ... Done")
    ))
    

(defun ediff-re-merge ()
  "Remerge unmodified diff regions using a new default. Start with the current region."
  (interactive)
  (let* ((default-variant-alist
	   (list '("default-A") '("default-B") '("combined")))
	 (actual-alist
	  (delete (list (symbol-name ediff-default-variant))
		  default-variant-alist)))
    (setq ediff-default-variant
	  (intern
	   (completing-read 
	    (format "Current merge default is `%S'. New default: "
		    ediff-default-variant)
	    actual-alist nil 'must-match)))
    (ediff-do-merge ediff-current-difference 'remerge)
    (ediff-recenter)
  ))
    
(defun ediff-shrink-window-C (arg)
  "Shrink window C to just one line.
With a prefix argument, returns window C to its normal size.
Used only for merging jobs."
  (interactive "P")
  (if (not ediff-merge-job)
      (error "ediff-shrink-window-C can be used only for merging jobs"))
  (cond ((eq arg '-) (setq arg -1))
	((not (numberp arg)) (setq arg nil)))
  (cond ((null arg)
	 (let ((ediff-merge-window-share
		(if (< (window-height ediff-window-C) 3)
		    ediff-merge-window-share 0)))
	   (setq ediff-window-config-saved "") ; force redisplay
	   (ediff-recenter 'no-rehighlight)))
	((and (< arg 0) (> (window-height ediff-window-C) 2))
	 (setq ediff-merge-window-share (* ediff-merge-window-share 0.9))
	 (setq ediff-window-config-saved "") ; force redisplay
	 (ediff-recenter 'no-rehighlight))
	((and (> arg 0) (> (window-height ediff-window-A) 2))
	 (setq ediff-merge-window-share (* ediff-merge-window-share 1.1))
	 (setq ediff-window-config-saved "") ; force redisplay
	 (ediff-recenter 'no-rehighlight))))


;; N here is the user's region number. It is 1+ what Ediff uses internally.
(defun ediff-combine-diffs (n &optional batch-invocation)
  "Combine Nth diff regions of buffers A and B and place the combination in C.
Combining is done using the list in variable `ediff-combination-pattern'."
  (interactive "P")
  (setq n (if n (1- n) ediff-current-difference))
  
  (let (regA regB reg-combined)
    (setq regA (ediff-get-region-contents n 'A ediff-control-buffer)
	  regB (ediff-get-region-contents n 'B ediff-control-buffer))
    
    (setq reg-combined (ediff-make-combined-diff regA regB))
    
    (ediff-copy-diff n nil 'C batch-invocation reg-combined))
    (or batch-invocation (ediff-recenter)))
    
(defsubst ediff-make-combined-diff (regA regB)
  (concat (nth 0 ediff-combination-pattern) "\n"
	  regA
	  (nth 1 ediff-combination-pattern) "\n"
	  regB
	  (nth 2 ediff-combination-pattern) "\n"))


;; Checks if the region in buff C looks like a combination of the regions
;; in buffers A and B. Returns a list (reg-a-beg reg-a-end reg-b-beg reg-b-end)
;; These refer to where the copies of region A and B start and end in buffer C
(defun ediff-looks-like-combined-merge (region-num)
  (if ediff-merge-job
      (let ((combined (string-match (regexp-quote "(A+B)")
				    (or (ediff-get-state-of-diff region-num 'C)
					"")))
	    (reg-beg (ediff-get-diff-posn 'C 'beg region-num))
	    (reg-end (ediff-get-diff-posn 'C 'end region-num))
	    (pat1 (nth 0 ediff-combination-pattern))
	    (pat2 (nth 1 ediff-combination-pattern))
	    (pat3 (nth 2 ediff-combination-pattern))
	    reg-a-beg reg-a-end reg-b-beg reg-b-end reg-c-beg reg-c-end)
	
	(if combined
	    (ediff-eval-in-buffer ediff-buffer-C
	      (goto-char reg-beg)
	      (search-forward pat1 reg-end 'noerror)
	      (setq reg-a-beg (match-beginning 0))
	      (setq reg-a-end (match-end 0))
	      (search-forward pat2 reg-end 'noerror)
	      (setq reg-b-beg (match-beginning 0))
	      (setq reg-b-end (match-end 0))
	      (search-forward pat3 reg-end 'noerror)
	      (setq reg-c-beg (match-beginning 0))
	      (setq reg-c-end (match-end 0))))
	
	(if (and reg-a-beg reg-a-end reg-b-beg reg-b-end)
	    (list reg-a-beg reg-a-end reg-b-beg reg-b-end reg-c-beg reg-c-end))
	)))
  

(provide 'ediff-merg)

;; ediff-merg.el ends here
