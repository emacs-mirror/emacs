;;; ediff-util.el --- the core commands and utilities of ediff
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


;;; Code:

(require 'ediff-init)


;;; Functions

(defun ediff-mode ()
  "Ediff mode is used by the Ediff file-difference package.
It is entered only through one of the following commands:
	`ediff'
	`ediff-files'
	`ediff-buffers'
	`ediff3'
	`ediff-files3'
	`ediff-buffers3'
	`ediff-merge'
	`ediff-merge-files'
	`ediff-merge-files-with-ancestor'
	`ediff-merge-buffers'
	`ediff-merge-buffers-with-ancestor'
	`ediff-merge-revisions'
	`ediff-merge-revisions-with-ancestor'
	`ediff-windows-wordwise'
	`ediff-windows-linewise'
	`ediff-regions-wordwise'
	`ediff-regions-linewise'
	`epatch'
	`ediff-patch-file'
	`ediff-patch-buffer'
	`epatch-buffer'
	`ediff-revision'

Commands:
\\{ediff-mode-map}"
  (kill-all-local-variables)
  (setq major-mode 'ediff-mode)
  (setq mode-name "Ediff")
  (run-hooks 'ediff-mode-hooks))

(defun ediff-version ()
  "Return string describing the version of Ediff.
When called interactively, displays the version."
  (interactive)
  (if (interactive-p)
      (message (ediff-version))
    (format "Ediff %s of %s" ediff-version ediff-date)))
    
    
(require 'ediff-diff)
(require 'ediff-merg)
  

;;; Build keymaps

(ediff-defvar-local ediff-mode-map nil
  "Local keymap used in Ediff mode.
This is local to each Ediff Control Panel, so they may vary from invocation
to invocation.")

;; Set up the keymap in the control buffer
(defun ediff-set-keys ()
  "Set up Ediff keymap, if necessary."
  (if (null ediff-mode-map)
      (ediff-setup-keymap))
  (use-local-map ediff-mode-map))
  
;; Reload Ediff keymap.  For debugging only.
(defun ediff-reload-keymap ()
  (interactive)
  (setq ediff-mode-map nil)
  (ediff-set-keys))
  

(defun ediff-setup-keymap ()
  "Set up the keymap used in the control buffer of Ediff."
  (setq ediff-mode-map (make-sparse-keymap))
  (suppress-keymap ediff-mode-map)
  
  (define-key ediff-mode-map "p" 'ediff-previous-difference)
  (define-key ediff-mode-map "\C-?" 'ediff-previous-difference)
  (define-key ediff-mode-map [backspace] 'ediff-previous-difference)
  (define-key ediff-mode-map [delete] 'ediff-previous-difference)
  (define-key ediff-mode-map "\C-h" (if ediff-no-emacs-help-in-control-buffer
					'ediff-previous-difference nil))
  (define-key ediff-mode-map "n" 'ediff-next-difference)
  (define-key ediff-mode-map " " 'ediff-next-difference)
  (define-key ediff-mode-map "j" 'ediff-jump-to-difference)
  (define-key ediff-mode-map "g"  nil)
  (define-key ediff-mode-map "ga" 'ediff-jump-to-difference-at-point)
  (define-key ediff-mode-map "gb" 'ediff-jump-to-difference-at-point)
  (define-key ediff-mode-map "q" 'ediff-quit)
  (define-key ediff-mode-map "z" 'ediff-suspend)
  (define-key ediff-mode-map "\C-l" 'ediff-recenter)
  (define-key ediff-mode-map "|" 'ediff-toggle-split)
  (define-key ediff-mode-map "h" 'ediff-toggle-hilit)
  (or ediff-word-mode
      (define-key ediff-mode-map "@" 'ediff-toggle-autorefine))
  (if ediff-narrow-job
      (define-key ediff-mode-map "%" 'ediff-toggle-narrow-region))
  (define-key ediff-mode-map "~" 'ediff-swap-buffers)
  (define-key ediff-mode-map "v" 'ediff-scroll-vertically)
  (define-key ediff-mode-map "\C-v" 'ediff-scroll-vertically)
  (define-key ediff-mode-map "^" 'ediff-scroll-vertically)
  (define-key ediff-mode-map "\M-v" 'ediff-scroll-vertically)
  (define-key ediff-mode-map "V" 'ediff-scroll-vertically)
  (define-key ediff-mode-map "<" 'ediff-scroll-horizontally)
  (define-key ediff-mode-map ">" 'ediff-scroll-horizontally)
  (define-key ediff-mode-map "i" 'ediff-status-info)
  (define-key ediff-mode-map "D" 'ediff-documentation)
  (define-key ediff-mode-map "?" 'ediff-toggle-help)
  (define-key ediff-mode-map "!" 'ediff-update-diffs)
  (or ediff-word-mode
      (define-key ediff-mode-map "*" 'ediff-make-or-kill-fine-diffs))
  (define-key ediff-mode-map "a"  nil)
  (define-key ediff-mode-map "b"  nil)
  (define-key ediff-mode-map "r"  nil)
  (cond (ediff-merge-job
	 ;; In merging, we allow only A->C and B->C copying.
	 (define-key ediff-mode-map "a" (function
					 (lambda (arg)
					   (interactive "P")
					   (ediff-diff-to-diff arg "ac"))))
	 (define-key ediff-mode-map "b" (function
					 (lambda (arg)
					   (interactive "P")
					   (ediff-diff-to-diff arg "bc"))))
	 (define-key ediff-mode-map "r" (function
					 (lambda (arg)
					   (interactive "P")
					   (ediff-restore-diff arg ?c))))
	 (define-key ediff-mode-map "s" 'ediff-shrink-window-C)
	 (define-key ediff-mode-map "+" 'ediff-combine-diffs)
	 (define-key ediff-mode-map "$" 'ediff-toggle-show-clashes-only)
	 (define-key ediff-mode-map "&" 'ediff-re-merge))
	(ediff-3way-comparison-job
	 (define-key ediff-mode-map "ab" 'ediff-diff-to-diff)
	 (define-key ediff-mode-map "ba" 'ediff-diff-to-diff)
	 (define-key ediff-mode-map "ac" 'ediff-diff-to-diff)
	 (define-key ediff-mode-map "bc" 'ediff-diff-to-diff)
	 (define-key ediff-mode-map "c" nil)
	 (define-key ediff-mode-map "ca" 'ediff-diff-to-diff)
	 (define-key ediff-mode-map "cb" 'ediff-diff-to-diff)
	 (define-key ediff-mode-map "ra" 'ediff-restore-diff)
	 (define-key ediff-mode-map "rb" 'ediff-restore-diff)
	 (define-key ediff-mode-map "rc" 'ediff-restore-diff)
	 (define-key ediff-mode-map "C"  'ediff-toggle-read-only))
	(t ; 2-way comparison
	 (define-key ediff-mode-map "a" (function
					 (lambda (arg)
					   (interactive "P")
					   (ediff-diff-to-diff arg "ab"))))
	 (define-key ediff-mode-map "b" (function
					 (lambda (arg)
					   (interactive "P")
					   (ediff-diff-to-diff arg "ba"))))
	 (define-key ediff-mode-map "ra" 'ediff-restore-diff)
	 (define-key ediff-mode-map "rb" 'ediff-restore-diff))
	) ; cond
  (define-key ediff-mode-map "G" 'ediff-submit-report)
  (define-key ediff-mode-map "#"  nil)
  (define-key ediff-mode-map "#h"  'ediff-toggle-regexp-match)
  (define-key ediff-mode-map "#f"  'ediff-toggle-regexp-match)
  (or ediff-word-mode
      (define-key ediff-mode-map "##"  'ediff-toggle-skip-similar))
  (define-key ediff-mode-map "o"   nil)
  (define-key ediff-mode-map "A"  'ediff-toggle-read-only)
  (define-key ediff-mode-map "B"  'ediff-toggle-read-only)
  (define-key ediff-mode-map "w"   nil)
  (define-key ediff-mode-map "wa"  'ediff-save-buffer)
  (define-key ediff-mode-map "wb"  'ediff-save-buffer)
  (define-key ediff-mode-map "wd"  'ediff-save-buffer)
  (if ediff-3way-job
      (progn
	(define-key ediff-mode-map "wc" 'ediff-save-buffer)
	(define-key ediff-mode-map "gc" 'ediff-jump-to-difference-at-point)
	))

  (define-key ediff-mode-map "m" 'ediff-toggle-wide-display)
	
  (define-key ediff-mode-map "k"   nil)
  (define-key ediff-mode-map "kkk" 'ediff-reload-keymap) ; for debugging
  
  ;; Allow ediff-mode-map to be referenced indirectly
  (fset 'ediff-mode-map ediff-mode-map)
  (run-hooks 'ediff-keymap-setup-hooks))


;;; Setup functions

(require 'ediff-wind)

;; Common startup entry for all Ediff functions
;; It now returns control buffer so other functions can do post-processing
(defun ediff-setup (buffer-A file-A buffer-B file-B buffer-C file-C
			     startup-hooks setup-parameters)
  (setq file-A (expand-file-name file-A))
  (setq file-B (expand-file-name file-B))
  (if (stringp file-C)
      (setq file-C (expand-file-name file-C)))
  (let* ((control-buffer-name 
	  (ediff-unique-buffer-name "*Ediff Control Panel" "*"))
	 (control-buffer (ediff-eval-in-buffer buffer-A
			   (get-buffer-create control-buffer-name))))
    (ediff-eval-in-buffer control-buffer
      (ediff-mode)                 
      
      ;; unwrap set up parameters passed as argument
      (while setup-parameters
	(set (car (car setup-parameters)) (cdr (car setup-parameters)))
	(setq setup-parameters (cdr setup-parameters)))
	
      ;; set variables classifying the current ediff job
      (setq ediff-3way-comparison-job (ediff-3way-comparison-job)
	    ediff-merge-job (ediff-merge-job)
	    ediff-merge-with-ancestor-job (ediff-merge-with-ancestor-job)
	    ediff-3way-job (ediff-3way-job)
	    ediff-diff3-job (ediff-diff3-job)
	    ediff-narrow-job (ediff-narrow-job)
	    ediff-windows-job (ediff-windows-job)
	    ediff-word-mode-job (ediff-word-mode-job))
	
      (make-local-variable 'ediff-prefer-long-help-message)
      (make-local-variable 'ediff-prefer-iconified-control-frame)
      (make-local-variable 'ediff-split-window-function)
      (make-local-variable 'ediff-default-variant)
      (make-local-variable 'ediff-merge-window-share)
      (make-local-variable 'ediff-window-setup-function)
      
      ;; adjust for merge jobs
      (if ediff-merge-job
	  (let ((buf
		 ;; Use buf A even if `combined'. The right stuff is
		 ;; inserted by ediff-do-merge
		 (if (eq ediff-default-variant 'default-B) buffer-B buffer-A)))
		 
	    (setq ediff-split-window-function
		  ediff-merge-split-window-function) 
	    
	    ;; remember the ancestor buffer, if any
	    (setq ediff-ancestor-buffer buffer-C)
	    
	    (setq buffer-C
		  (get-buffer-create
		   (ediff-unique-buffer-name "*ediff-merge" "*")))
	    (save-excursion
	      (set-buffer buffer-C)
	      (insert-buffer buf)
	      (funcall (ediff-eval-in-buffer buf major-mode))
	      ;; after Stig@hackvan.com
	      (add-hook 'local-write-file-hooks 'ediff-set-merge-mode)
	      )))
      (setq buffer-read-only nil    
	    ediff-buffer-A buffer-A
	    ediff-buffer-B buffer-B
	    ediff-buffer-C buffer-C
	    ediff-control-buffer control-buffer)
	   
      (setq ediff-control-buffer-suffix
	    (if (string-match "<[0-9]*>" control-buffer-name)
		(substring control-buffer-name
			   (match-beginning 0) (match-end 0))
	      "")
	    ediff-control-buffer-number
	    (max
	     0
	     (1-
	      (string-to-number
	       (substring
		ediff-control-buffer-suffix
		(or
		 (string-match "[0-9]+" ediff-control-buffer-suffix)
		 0))))))
	   
      (setq ediff-error-buffer (get-buffer-create (ediff-unique-buffer-name
						   "*ediff-errors" "*")))
      
      (ediff-eval-in-buffer buffer-A (ediff-strip-mode-line-format))
      (ediff-eval-in-buffer buffer-B (ediff-strip-mode-line-format))
      (if ediff-3way-job
	  (ediff-eval-in-buffer buffer-C (ediff-strip-mode-line-format)))
      
      (ediff-save-protected-variables) ; save variables to be restored on exit
      
      ;; ediff-setup-diff-regions-function must be set after setup
      ;; parameters are processed.
      (setq ediff-setup-diff-regions-function
	    (if ediff-diff3-job
		'ediff-setup-diff-regions3
	      'ediff-setup-diff-regions))
	
      (setq ediff-wide-bounds
	    (list (ediff-make-bullet-proof-overlay
		   '(point-min) '(point-max) ediff-buffer-A)
		  (ediff-make-bullet-proof-overlay
		   '(point-min) '(point-max) ediff-buffer-B)
		  (ediff-make-bullet-proof-overlay
		   '(point-min) '(point-max) ediff-buffer-C)))
      
      ;; This has effect only on ediff-windows/regions
      ;; In all other cases, ediff-visible-region sets visibility bounds to
      ;; ediff-wide-bounds, and ediff-narrow-bounds are ignored.
      (if ediff-start-narrowed
	  (setq ediff-visible-bounds ediff-narrow-bounds)
	(setq ediff-visible-bounds ediff-wide-bounds))
      
      (ediff-set-keys) ; comes after parameter setup
      
      ;; set up ediff-narrow-bounds, if not set
      (or ediff-narrow-bounds
	  (setq ediff-narrow-bounds ediff-wide-bounds))
      
      ;; All these must be inside ediff-eval-in-buffer control-buffer,
      ;; since these vars are local to control-buffer
      ;; These won't run if there are errors in diff
      (ediff-eval-in-buffer ediff-buffer-A
	(ediff-nuke-selective-display)
	(run-hooks 'ediff-prepare-buffer-hooks)
	(if (ediff-eval-in-buffer control-buffer ediff-merge-job)
	    (setq buffer-read-only t))
	;; add control-buffer to the list of sessions--no longer used, but may
	;; be used again in the future
	(or (memq control-buffer ediff-this-buffer-control-sessions)
	    (setq ediff-this-buffer-control-sessions
		  (cons control-buffer ediff-this-buffer-control-sessions)))
	)
      (ediff-eval-in-buffer ediff-buffer-B
	(ediff-nuke-selective-display)
	(run-hooks 'ediff-prepare-buffer-hooks)
	(if (ediff-eval-in-buffer control-buffer ediff-merge-job)
	    (setq buffer-read-only t))
	;; add control-buffer to the list of sessions
	(or (memq control-buffer ediff-this-buffer-control-sessions)
	    (setq ediff-this-buffer-control-sessions
		  (cons control-buffer ediff-this-buffer-control-sessions)))
	)
      (if ediff-3way-job
	  (ediff-eval-in-buffer ediff-buffer-C
	    (ediff-nuke-selective-display)
	    (run-hooks 'ediff-prepare-buffer-hooks)
	    ;; add control-buffer to the list of sessions
	    (or (memq control-buffer ediff-this-buffer-control-sessions)
		(setq ediff-this-buffer-control-sessions
		      (cons control-buffer
			    ediff-this-buffer-control-sessions))) 
	    ))
      
      ;; must come after setting up  ediff-narrow-bounds AND after
      ;; nuking selective display
      (funcall ediff-setup-diff-regions-function file-A file-B file-C)
      (setq ediff-number-of-differences (length ediff-difference-vector-A))
      (setq ediff-current-difference -1)
      
      (ediff-make-current-diff-overlay 'A)
      (ediff-make-current-diff-overlay 'B)
      (if ediff-3way-job
	  (ediff-make-current-diff-overlay 'C))
	  
      (if (ediff-window-display-p)
	  (ediff-init-var-faces))
	  
      (ediff-setup-windows buffer-A buffer-B buffer-C control-buffer)
      
      (let ((shift-A (ediff-overlay-start
		      (ediff-get-value-according-to-buffer-type
		       'A ediff-narrow-bounds)))
	    (shift-B (ediff-overlay-start
		      (ediff-get-value-according-to-buffer-type
		       'B ediff-narrow-bounds)))
	    (shift-C (ediff-overlay-start
		      (ediff-get-value-according-to-buffer-type
		       'C ediff-narrow-bounds))))
	;; position point in buf A
	(save-excursion
	  (select-window ediff-window-A)
	  (goto-char shift-A))
	;; position point in buf B
	(save-excursion
	  (select-window ediff-window-B)
	  (goto-char shift-B))
	(if ediff-3way-job
	    (save-excursion
	      (select-window ediff-window-C)
	      (goto-char shift-C)))
	)
      
      (select-window ediff-control-window)
      (ediff-visible-region)
      
      ;; The following is a hack to placate OS/2
      ;; The problem is that OS/2 doesn't let us delete files used by other
      ;; processes. Thus, in ediff-buffers and other functions, we can't
      ;; delete temp files because they might be used by the asynchronous
      ;; process that computes custom diffs. So, under OS/1 we have to wait
      ;; for some time until custom diffs are done.
      (if (eq system-type 'emx)
	  (ediff-eval-in-buffer ediff-custom-diff-buffer
	    (let ((proc (get-buffer-process (current-buffer))))
	      (while (and (processp proc)
			  (eq (process-status proc) 'run))
		(message "Waiting for the diff program to quit")
		(sleep-for 1))
	      (message "")
	      )))
      
      (run-hooks 'startup-hooks 'ediff-startup-hooks)
      (ediff-refresh-mode-lines)
      (setq buffer-read-only t))
    control-buffer))
      
      
;; This function assumes that we are in the window where control buffer is
;; to reside. 
(defun ediff-setup-control-buffer (ctl-buf)
  "Set up window for control buffer."
  (if (window-dedicated-p (selected-window))
      (set-buffer ctl-buf) ; we are in control frame but just in case
    (switch-to-buffer ctl-buf))
  (let ((window-min-height 2))
    (erase-buffer)
    (ediff-set-help-message)
    (insert ediff-help-message)
    (shrink-window-if-larger-than-buffer)
    (or (ediff-multiframe-setup-p)
	(ediff-indent-help-message))
    (set-buffer-modified-p nil)
    (ediff-refresh-mode-lines)
    (setq ediff-control-window (selected-window))
    (setq ediff-window-config-saved
	  (format "%S%S%S%S%S%S%S"
		  ediff-control-window
		  ediff-window-A
		  ediff-window-B
		  ediff-window-C
		  ediff-split-window-function
		  (ediff-multiframe-setup-p)
		  ediff-wide-display-p))
    (goto-char (point-min))
    (skip-chars-forward ediff-whitespace)))
    
;; assuming we are in control window, calculate length of the first line in
;; help message
(defun ediff-help-message-line-length ()
  (save-excursion
    (goto-char (point-min))
    (if ediff-prefer-long-help-message
	(next-line 1))
    (end-of-line)
    (current-column)))
    
    
(defun ediff-indent-help-message ()
  (let* ((shift (/ (max 0 (- (window-width (selected-window))
			     (ediff-help-message-line-length)))
		   2))
	 (str (make-string shift ?\ )))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
	(insert str)
	(beginning-of-line)
	(forward-line 1)))))
      

(defun ediff-set-help-message ()
  (setq ediff-long-help-message
	(cond ((and ediff-long-help-message-custom
		    (or (symbolp ediff-long-help-message-custom)
			(consp ediff-long-help-message-custom)))
	       (funcall ediff-long-help-message-custom))
	      (ediff-word-mode 
	       (concat ediff-long-help-message-head
		       ediff-long-help-message-word-mode
		       ediff-long-help-message-tail))
	      (ediff-narrow-job
	       (concat ediff-long-help-message-head
		       ediff-long-help-message-narrow2
		       ediff-long-help-message-tail))
	      (ediff-merge-job 
	       (concat ediff-long-help-message-head
		       ediff-long-help-message-merge
		       ediff-long-help-message-tail))
	      (ediff-diff3-job
	       (concat ediff-long-help-message-head
		       ediff-long-help-message-compare3
		       ediff-long-help-message-tail))
	      (t 
	       (concat ediff-long-help-message-head
		       ediff-long-help-message-compare2
		       ediff-long-help-message-tail))))
  (setq ediff-brief-help-message 
	(cond ((and ediff-brief-help-message-custom
		    (or (symbolp ediff-brief-help-message-custom)
			(consp ediff-brief-help-message-custom)))
	       (funcall ediff-brief-help-message-custom))
	      ((stringp ediff-brief-help-message-custom)
	       ediff-brief-help-message-custom)
	      ((ediff-multiframe-setup-p) ediff-brief-message-string)
	      (t ; long brief msg, not multiframe --- put in the middle
	       ediff-brief-message-string)
	      ))
  (setq ediff-help-message (if ediff-prefer-long-help-message
			       ediff-long-help-message
			     ediff-brief-help-message))
  (run-hooks 'ediff-display-help-hooks)
  )




;;; Commands for working with Ediff
     
(defun ediff-update-diffs ()
  "Recompute difference regions in buffers A, B, and C.
Buffers are not synchronized with their respective files, so changes done
to these buffers are not saved at this point---the user can do this later,
if necessary."
  (interactive)
  (let ((point-A (ediff-eval-in-buffer ediff-buffer-A (point)))
	;;(point-B (ediff-eval-in-buffer ediff-buffer-B (point)))
	(tmp-buffer (get-buffer-create ediff-tmp-buffer))
	(buf-A-file-name
	 (file-name-nondirectory (or (buffer-file-name ediff-buffer-A)
				     (buffer-name ediff-buffer-A)
				     )))
	(buf-B-file-name
	 (file-name-nondirectory (or (buffer-file-name ediff-buffer-B)
				     (buffer-name ediff-buffer-B)
				     )))
	(buf-C-file-name
	 (file-name-nondirectory (or (buffer-file-name ediff-buffer-C)
				     ;; if (null ediff-buffer-C), there is
				     ;; no danger, since we later check if
				     ;; ediff-buffer-C is alive
				     (buffer-name ediff-buffer-C)
				     )))
	(overl-A (ediff-get-value-according-to-buffer-type
		  'A ediff-narrow-bounds))
	(overl-B (ediff-get-value-according-to-buffer-type
		  'B ediff-narrow-bounds))
	(overl-C (ediff-get-value-according-to-buffer-type
		  'C ediff-narrow-bounds))
	beg-A end-A beg-B end-B beg-C end-C
	file-A file-B file-C)
    (ediff-unselect-and-select-difference -1)
    
    (setq beg-A (ediff-overlay-start overl-A)
	  beg-B (ediff-overlay-start overl-B)
	  beg-C (ediff-overlay-start overl-C)
	  end-A (ediff-overlay-end overl-A)
	  end-B (ediff-overlay-end overl-B)
	  end-C (ediff-overlay-end overl-C))
	  
    (if ediff-word-mode
	(progn
	  (ediff-wordify beg-A end-A ediff-buffer-A tmp-buffer)
	  (ediff-eval-in-buffer tmp-buffer
	    (setq file-A (ediff-make-temp-file "regA")))
	  (ediff-wordify beg-B end-B ediff-buffer-B tmp-buffer)
	  (ediff-eval-in-buffer tmp-buffer
	    (setq file-B (ediff-make-temp-file "regB")))
	  (if ediff-3way-job
	      (progn
		(ediff-wordify beg-C end-C ediff-buffer-C tmp-buffer)
		(ediff-eval-in-buffer tmp-buffer
		  (setq file-C (ediff-make-temp-file "regC")))))
	  )
      ;; not word-mode
      (ediff-eval-in-buffer ediff-buffer-A
	(setq file-A (ediff-make-temp-file buf-A-file-name)))
      (ediff-eval-in-buffer ediff-buffer-B
	(setq file-B (ediff-make-temp-file buf-B-file-name)))
      (if ediff-3way-job
	  (ediff-eval-in-buffer ediff-buffer-C
	    (setq file-C (ediff-make-temp-file buf-C-file-name))))
      )
	
    (ediff-clear-diff-vector 'ediff-difference-vector-A 'fine-diffs-also)
    (ediff-clear-diff-vector 'ediff-difference-vector-B 'fine-diffs-also)
    (ediff-clear-diff-vector 'ediff-difference-vector-C 'fine-diffs-also)
    (setq ediff-killed-diffs-alist nil) ; invalidate saved killed diff regions
    
    ;; In case of merge job, full it into thinking that it is just doing
    ;; comparison
    (let ((ediff-setup-diff-regions-function ediff-setup-diff-regions-function)
	  (ediff-job-name ediff-job-name))
      (if ediff-merge-job
	  (setq ediff-setup-diff-regions-function 'ediff-setup-diff-regions3
		ediff-job-name 'ediff-files3))
      (funcall ediff-setup-diff-regions-function file-A file-B file-C))
	    
    (setq ediff-number-of-differences (length ediff-difference-vector-A))
    (delete-file file-A)
    (delete-file file-B)
    (if file-C
	(delete-file file-C))
	
    (if ediff-3way-job
	(ediff-set-state-of-all-diffs-in-all-buffers ediff-control-buffer))
	
    (ediff-jump-to-difference (ediff-diff-at-point 'A point-A))
    (message "")
    ))
    
;; Not bound to any key---to dangerous. A user can do it if necessary.
(defun ediff-revert-buffers-then-recompute-diffs (noconfirm)
  "Revert buffers A, B and C. Then rerun Ediff on file A and file B."
  (interactive "P")
  (let ((bufA ediff-buffer-A)
	(bufB ediff-buffer-B)
	(bufC ediff-buffer-C)
	(ctl-buf ediff-control-buffer)
	(ancestor-buf ediff-ancestor-buffer)
	(ancestor-job ediff-merge-with-ancestor-job)
	(merge ediff-merge-job)
	(comparison ediff-3way-comparison-job))
    (ediff-eval-in-buffer bufA
      (revert-buffer t noconfirm))
    (ediff-eval-in-buffer bufB
      (revert-buffer t noconfirm))
    ;; this should only be executed in a 3way comparison, not in merge
    (if comparison
	(ediff-eval-in-buffer bufC
	  (revert-buffer t noconfirm)))
    (if merge
	(progn
	  (set-buffer ctl-buf)
	  (ediff-really-quit)
	  (kill-buffer bufC)
	  (if ancestor-job
	      (ediff-merge-buffers-with-ancestor bufA bufB ancestor-buf)
	    (ediff-merge-buffers bufA bufB)))
      (ediff-update-diffs))))


;; optional NO-REHIGHLIGHT says to not rehighlight buffers 
(defun ediff-recenter (&optional no-rehighlight)
  "Bring the highlighted region of all buffers being compared into view.
Reestablish the default three-window display."
  (interactive)
  
  ;; force all minibuffer to display ediff's messages.
  ;; when xemacs implements minibufferless frames, this won't be necessary
  (if ediff-xemacs-p (setq synchronize-minibuffers t))
  
  (setq ediff-disturbed-overlays nil) ; clear after use
  (let (buffer-read-only)
    (if (and (ediff-buffer-live-p ediff-buffer-A)
	     (ediff-buffer-live-p ediff-buffer-B)
	     (or (not ediff-3way-job)
		 (ediff-buffer-live-p ediff-buffer-C)))
	(ediff-setup-windows
	 ediff-buffer-A ediff-buffer-B ediff-buffer-C ediff-control-buffer)
      (or (eq this-command 'ediff-quit)
	  (message
	   "You've killed an essential Ediff buffer---Please quit Ediff"
	   (beep 1)))
      ))
      
  ;; set visibility range appropriate to this invocation of Ediff.
  (ediff-visible-region)
  
  ;; raise
  (if (and (ediff-window-display-p)
	   (symbolp this-command)
	   (symbolp last-command)
	   ;; Either one of the display-changing commands
	   (or (memq this-command
		     '(ediff-recenter
		       ediff-toggle-wide-display ediff-toggle-multiframe))
	       ;; Or one of the movement cmds and prev cmd was an Ediff cmd
	       ;; This avoids rasing frames unnecessarily.
	       (and (memq this-command
			  '(ediff-next-difference
			    ediff-previous-difference
			    ediff-jump-to-difference
			    ediff-jump-to-difference-at-point))
		    (not (string-match "^ediff-" (symbol-name last-command)))
		    )))
      (progn
	(if (window-live-p ediff-window-A)
	    (raise-frame (window-frame ediff-window-A)))
	(if (window-live-p ediff-window-B)
	    (raise-frame (window-frame ediff-window-B)))
	(if (window-live-p ediff-window-C)
	    (raise-frame (window-frame ediff-window-C)))))
  (if (and (ediff-window-display-p)
	   (frame-live-p ediff-control-frame)
	   (not (ediff-frame-iconified-p ediff-control-frame)))
      (raise-frame ediff-control-frame))
  
  ;; Redisplay whatever buffers are showing, if there is a selected difference
  (let* ((control-frame ediff-control-frame)
	 (control-buf ediff-control-buffer))
    (if (and (ediff-buffer-live-p ediff-buffer-A)
	     (ediff-buffer-live-p ediff-buffer-B)
	     (or (not ediff-3way-job)
		 (ediff-buffer-live-p ediff-buffer-C))
	     (ediff-valid-difference-p))
	(progn
	  (or no-rehighlight
	      (ediff-select-difference ediff-current-difference))
	    
	  (ediff-recenter-one-window 'A)
	  (ediff-recenter-one-window 'B)
	  (if ediff-3way-job
	      (ediff-recenter-one-window 'C))
	  
	  (if (and (ediff-multiframe-setup-p)
		   (not (ediff-frame-iconified-p ediff-control-frame)))
	      (progn
		(ediff-reset-mouse control-frame)
		(or ediff-xemacs-p
		    (cond ((eq (ediff-device-type) 'ns)
			   (unfocus-frame (selected-frame)))
			  (t (unfocus-frame))))
		))
	  ))
    (ediff-eval-in-buffer control-buf
      (ediff-refresh-mode-lines))
    ))
  
;; this function returns to the window it was called from
;; (which was the control window)
(defun ediff-recenter-one-window (buf-type)
  (let* (;; context must be saved before switching to windows A/B/C
	 (ctl-wind (selected-window))
	 (shift (ediff-overlay-start
		 (ediff-get-value-according-to-buffer-type 
		  buf-type ediff-narrow-bounds)))
	 (job-name ediff-job-name)
	 (control-buf ediff-control-buffer)
	 (window-name (intern (format "ediff-window-%S" buf-type)))
	 (window (if (window-live-p (symbol-value window-name))
		     (symbol-value window-name))))
	 
    (if (and window ediff-windows-job)
	(set-window-start window shift))
    (if window
	(progn
	  (select-window window)
	  (ediff-deactivate-mark)
	  (ediff-position-region
	   (ediff-get-diff-posn buf-type 'beg nil control-buf)
	   (ediff-get-diff-posn buf-type 'end nil control-buf)
	   (ediff-get-diff-posn buf-type 'beg nil control-buf)
	   job-name
	   )))
    (select-window ctl-wind)
    ))

	
;; This will have to be refined for 3way jobs
(defun ediff-toggle-split ()
  "Toggle vertical/horizontal window split. 
Does nothing if file-A and file-B are in different frames."
  (interactive)
  (let* ((wind-A (if (window-live-p ediff-window-A) ediff-window-A))
	 (wind-B (if (window-live-p ediff-window-B) ediff-window-B))
	 (wind-C (if (window-live-p ediff-window-C) ediff-window-C))
	 (frame-A (if wind-A (window-frame wind-A)))
	 (frame-B (if wind-B (window-frame wind-B)))
	 (frame-C (if wind-C (window-frame wind-C))))
    (if (or (eq frame-A frame-B)
	    (not (frame-live-p frame-A))
	    (not (frame-live-p frame-B))
	    (if ediff-3way-comparison-job
		(or (not (frame-live-p frame-C))
		    (eq frame-A frame-C) (eq frame-B frame-C))))
	(setq ediff-split-window-function
	      (if (eq ediff-split-window-function 'split-window-vertically)
		  'split-window-horizontally
		'split-window-vertically))
      (message "Buffers being compared are in different frames"))
    (ediff-recenter 'no-rehighlight)))
  
(defun ediff-toggle-hilit ()
  "Switch between highlighting using ASCII flags and highlighting using faces.
On a dumb terminal, switches between ASCII highlighting and no highlighting." 
  (interactive)
  (if (not (ediff-window-display-p))
      (if (eq ediff-highlighting-style 'ascii)
	  (progn
	    (message "ASCII highlighting flags removed")
	    (ediff-unselect-and-select-difference ediff-current-difference
						  'unselect-only)
	    (setq ediff-highlighting-style 'off))
	(ediff-unselect-and-select-difference ediff-current-difference
					      'select-only))
    (ediff-unselect-and-select-difference ediff-current-difference
					  'unselect-only)
    ;; cycle through highlighting
    (cond ((and ediff-use-faces ediff-highlight-all-diffs)
	   (message "Unhighlighting unselected difference regions")
	   (setq ediff-highlight-all-diffs nil))
	  (ediff-use-faces
	   (message "Highlighting with ASCII flags")
	   (setq ediff-use-faces nil))
	  (t
	   (message "Re-highlighting all difference regions")
	   (setq ediff-use-faces t
		 ediff-highlight-all-diffs t)))
		 
    (if (and ediff-use-faces ediff-highlight-all-diffs)
	(if (not (face-differs-from-default-p 'ediff-odd-diff-face-A-var))
	    (progn
	      (copy-face ediff-odd-diff-face-A 'ediff-odd-diff-face-A-var)
	      (copy-face ediff-odd-diff-face-B 'ediff-odd-diff-face-B-var)
	      (copy-face ediff-odd-diff-face-C 'ediff-odd-diff-face-C-var)
	      (copy-face ediff-even-diff-face-A 'ediff-even-diff-face-A-var)
	      (copy-face ediff-even-diff-face-B 'ediff-even-diff-face-B-var)
	      (copy-face ediff-even-diff-face-C 'ediff-even-diff-face-C-var)
	      ))
      (copy-face 'default 'ediff-odd-diff-face-A-var)
      (copy-face 'default 'ediff-odd-diff-face-B-var)
      (copy-face 'default 'ediff-odd-diff-face-C-var)
      (copy-face 'default 'ediff-even-diff-face-A-var)
      (copy-face 'default 'ediff-even-diff-face-B-var)
      (copy-face 'default 'ediff-even-diff-face-C-var))
    
    (ediff-unselect-and-select-difference
     ediff-current-difference 'select-only))
  )
  
(defun ediff-toggle-autorefine ()
  "Toggle auto-refine mode."
  (interactive)
  (if ediff-word-mode
      (error "No fine differences in this mode"))
  (cond ((eq ediff-auto-refine 'nix)
	 (setq ediff-auto-refine 'on)
	 (ediff-make-fine-diffs ediff-current-difference 'noforce)
	 (message "Auto-refining is ON"))
	((eq ediff-auto-refine 'on)
	 (message "Auto-refining is OFF")
	 (setq ediff-auto-refine 'off))
	(t ;; nix 'em
	 (ediff-set-fine-diff-properties ediff-current-difference 'default)
	 (message "Refinements are HIDDEN")
	 (setq ediff-auto-refine 'nix))
	))
	
(defun ediff-make-or-kill-fine-diffs (arg)
  "Compute fine diffs. With negative prefix arg, kill fine diffs.
In both cases, operates on the currrent difference region." 
  (interactive "P")
  (cond ((eq arg '-)
	 (ediff-clear-fine-differences ediff-current-difference))
	((and (numberp arg) (< arg 0))
	 (ediff-clear-fine-differences ediff-current-difference))
	(t (ediff-make-fine-diffs))))
	 
  
(defun ediff-toggle-help ()
  "Toggle short/long help message."
  (interactive)
  (let (buffer-read-only)
    (erase-buffer)
    (setq ediff-prefer-long-help-message (not ediff-prefer-long-help-message))
    (ediff-set-help-message))
  ;; remember the icon status of the control frame when the user requested
  ;; full control message
  (if (and ediff-prefer-long-help-message (ediff-multiframe-setup-p))
      (setq ediff-prefer-iconified-control-frame
	    (ediff-frame-iconified-p ediff-control-frame)))
	    
  (setq ediff-window-config-saved "") ; force redisplay
  (ediff-recenter 'no-rehighlight))
  
  
;; If BUF, this is the buffer to toggle, not current buffer.
(defun ediff-toggle-read-only (&optional buf)
  "Toggle read-only in current buffer.
If buffer is under version control and locked, check it out first.
If optional argument BUF is specified, toggle read-only in that buffer instead
of the current buffer."
  (interactive)
  (let ((ctl-buf (if (null buf) (current-buffer))))
    (or buf (ediff-recenter))
    (or buf
	(setq buf
	      (ediff-get-buffer (ediff-char-to-buftype last-command-char))))
	      
    (ediff-eval-in-buffer buf     ; eval in buf A/B/C
      (let* ((file (buffer-file-name buf))
	     (file-writable (and file
				 (file-exists-p file)
				 (file-writable-p file)))
	     (toggle-ro-cmd (cond (ediff-toggle-read-only-function)
				  ((ediff-file-checked-out-p file)
				   'toggle-read-only)
				  (file-writable 'toggle-read-only)
				  (t (key-binding "\C-x\C-q")))))
	;; If the file is checked in, make sure we don't make buffer modifiable
	;; without warning the user. The user can fool our checks by making the
	;; buffer non-RO without checking the file out. We regard this as a
	;; user problem.
	(if (and (ediff-file-checked-in-p file)
		 ;; If ctl-buf is null, this means we called this
		 ;; non-interactively, in which case don't ask questions
		 ctl-buf)
	    (cond ((not buffer-read-only)
		   (setq toggle-ro-cmd 'toggle-read-only))
		  ((and (or (beep 1) t) ; always beep
			(y-or-n-p
			 (format
			  "File %s is under version control. Check it out? "
			  (abbreviate-file-name file))))
		   ;; if we checked the file out, we should also change the
		   ;; original state of buffer-read-only to nil.  If we don't
		   ;; do this, the mode line will show %%, since the file was
		   ;; RO before ediff started, so the user will think the file
		   ;; is checked in.
		   (ediff-eval-in-buffer ctl-buf
		     (ediff-change-saved-variable
		      'buffer-read-only nil
		      (ediff-char-to-buftype last-command-char))))
		  (t
		   (setq toggle-ro-cmd 'toggle-read-only)
		   (beep 1) (beep 1)
		   (message
		    "Boy, this is risky! Better don't change this file...")
		   (sit-for 3))))
	(if (and toggle-ro-cmd 
		 (string-match "toggle-read-only" (symbol-name toggle-ro-cmd)))
	    (save-excursion
	      (save-window-excursion
		(command-execute toggle-ro-cmd)))
	  (error "Don't know how to toggle read-only in buffer %S" buf))
	
	;; Check if we made the current buffer updatable, but its file is RO.
	;; Signal a warning in this case.
	(if (and file (not buffer-read-only)
		 (eq this-command 'ediff-toggle-read-only)
		 (file-exists-p file)
		 (not (file-writable-p file)))
	    (message "Warning: file %s is read-only"
		     (abbreviate-file-name file) (beep 1)))
	))))
  

;; This is a simple-minded check for whether a file is under version control
;; and is checked out.
;; If file,v exists but file doesn't, this file is considered to be not checked
;; in and not checked out for the purpose of patching (since patch won't be
;; able to read such a file anyway).
;; FILE is a string representing file name
(defsubst ediff-file-checked-out-p (file)
  (and (stringp file)
       (file-exists-p file)
       (file-writable-p file)
       (file-exists-p (concat file ",v"))))
(defsubst ediff-file-checked-in-p (file)
  (and (stringp file)
       (file-exists-p file)
       (not (file-writable-p file))
       (file-exists-p (concat file ",v"))))
      
(defun ediff-swap-buffers ()
  "Rotate the display of buffers A, B, and C."
  (interactive)
  (if (and (window-live-p ediff-window-A) (window-live-p ediff-window-B))
      (let ((buf ediff-buffer-A)
	    (values ediff-buffer-values-orig-A)
	    (diff-vec ediff-difference-vector-A)
	    (hide-regexp ediff-regexp-hide-A)
	    (focus-regexp ediff-regexp-focus-A)
	    (wide-visibility-p (eq ediff-visible-bounds ediff-wide-bounds))
	    (overlay (if (ediff-window-display-p)
			 ediff-current-diff-overlay-A)))
	(if ediff-3way-comparison-job
	    (progn
	      (set-window-buffer ediff-window-A ediff-buffer-C)
	      (set-window-buffer ediff-window-B ediff-buffer-A)
	      (set-window-buffer ediff-window-C ediff-buffer-B)
	      )
	  (set-window-buffer ediff-window-A ediff-buffer-B)
	  (set-window-buffer ediff-window-B ediff-buffer-A))
	;; swap diff buffers
	(if ediff-3way-comparison-job
	    (setq ediff-buffer-A ediff-buffer-C
		  ediff-buffer-C ediff-buffer-B
		  ediff-buffer-B buf)
	  (setq ediff-buffer-A ediff-buffer-B
		ediff-buffer-B buf))
		
	;; swap saved buffer characteristics
	(if ediff-3way-comparison-job
	    (setq ediff-buffer-values-orig-A ediff-buffer-values-orig-C
		  ediff-buffer-values-orig-C ediff-buffer-values-orig-B
		  ediff-buffer-values-orig-B values)
	  (setq ediff-buffer-values-orig-A ediff-buffer-values-orig-B
		ediff-buffer-values-orig-B values))
	
	;; swap diff vectors
	(if ediff-3way-comparison-job
	    (setq ediff-difference-vector-A ediff-difference-vector-C
		  ediff-difference-vector-C ediff-difference-vector-B
		  ediff-difference-vector-B diff-vec)
	  (setq ediff-difference-vector-A ediff-difference-vector-B
		ediff-difference-vector-B diff-vec))
		
	;; swap hide/focus regexp
	(if ediff-3way-comparison-job
	    (setq ediff-regexp-hide-A ediff-regexp-hide-C
		  ediff-regexp-hide-C ediff-regexp-hide-B
		  ediff-regexp-hide-B hide-regexp
		  ediff-regexp-focus-A ediff-regexp-focus-C
		  ediff-regexp-focus-C ediff-regexp-focus-B
		  ediff-regexp-focus-B focus-regexp)
	  (setq ediff-regexp-hide-A ediff-regexp-hide-B
		ediff-regexp-hide-B hide-regexp
		ediff-regexp-focus-A ediff-regexp-focus-B
		ediff-regexp-focus-B focus-regexp))
	
	;; The following is needed for XEmacs, since there one can't move
	;; overlay to another buffer. In Emacs, this swap is redundant.
	(if (ediff-window-display-p)
	    (if ediff-3way-comparison-job
		(setq ediff-current-diff-overlay-A ediff-current-diff-overlay-C
		      ediff-current-diff-overlay-C ediff-current-diff-overlay-B
		      ediff-current-diff-overlay-B overlay)
	      (setq ediff-current-diff-overlay-A ediff-current-diff-overlay-B
		    ediff-current-diff-overlay-B overlay)))
		    
	;; swap wide bounds
	(setq ediff-wide-bounds
	      (cond (ediff-3way-comparison-job
		     (list (nth 2 ediff-wide-bounds)
			   (nth 0 ediff-wide-bounds)
			   (nth 1 ediff-wide-bounds)))
		    (ediff-3way-job
		     (list (nth 1 ediff-wide-bounds)
			   (nth 0 ediff-wide-bounds)
			   (nth 2 ediff-wide-bounds)))
		    (t 
		     (list (nth 1 ediff-wide-bounds)
			   (nth 0 ediff-wide-bounds)))))
	;; swap narrow bounds
	(setq ediff-narrow-bounds
	      (cond (ediff-3way-comparison-job
		     (list (nth 2 ediff-narrow-bounds)
			   (nth 0 ediff-narrow-bounds)
			   (nth 1 ediff-narrow-bounds)))
		    (ediff-3way-job
		     (list (nth 1 ediff-narrow-bounds)
			   (nth 0 ediff-narrow-bounds)
			   (nth 2 ediff-narrow-bounds)))
		    (t 
		     (list (nth 1 ediff-narrow-bounds)
			   (nth 0 ediff-narrow-bounds)))))
	(if wide-visibility-p
	    (setq ediff-visible-bounds ediff-wide-bounds)
	  (setq ediff-visible-bounds ediff-narrow-bounds))
	))
  (if ediff-3way-job
      (ediff-set-state-of-all-diffs-in-all-buffers ediff-control-buffer))
  (ediff-recenter 'no-rehighlight)
  )
  

(defun ediff-toggle-wide-display ()
  "Toggle wide/regular display.
This is especially useful when comparing buffers side-by-side."
  (interactive)
  (or (ediff-window-display-p)
      (error "%sEmacs is not running as a window application"
	     (if ediff-emacs-p "" "X")))
  (ediff-recenter 'no-rehighlight) ; make sure buffs are displayed in windows
  (let ((ctl-buf ediff-control-buffer))
    (setq ediff-wide-display-p (not ediff-wide-display-p))
    (if (not ediff-wide-display-p)
	(ediff-eval-in-buffer ctl-buf
	  (modify-frame-parameters
	   ediff-wide-display-frame ediff-wide-display-orig-parameters)
	  (sit-for (if ediff-xemacs-p 0.4 0))
	  ;; restore control buf, since ctl window may have been deleted
	  ;; during resizing
	  (set-buffer ctl-buf)
	  (setq ediff-wide-display-orig-parameters nil
		ediff-window-B nil) ; force update of window config
	  (ediff-recenter 'no-rehighlight))
      (funcall ediff-make-wide-display-function)
      (sit-for (if ediff-xemacs-p 0.4 0))
      (ediff-eval-in-buffer ctl-buf
	(setq ediff-window-B nil) ; force update of window config
	(ediff-recenter 'no-rehighlight)))))
	
(defun ediff-toggle-multiframe ()
  "Switch from the multiframe display to single-frame display and back.
This is primarily for debugging, but one can use it for fun, too."
  (interactive)
  (or (ediff-window-display-p)
      (error "%sEmacs is not running as a window application"
	     (if ediff-emacs-p "" "X")))
  (cond ((eq ediff-window-setup-function 'ediff-setup-windows-multiframe)
	 (setq ediff-window-setup-function 'ediff-setup-windows-plain))
	((eq ediff-window-setup-function 'ediff-setup-windows-plain)
	 (setq ediff-window-setup-function 'ediff-setup-windows-multiframe)))
  (setq ediff-window-B nil)
  (ediff-recenter 'no-rehighlight))
	       
;; Merging

(defun ediff-toggle-show-clashes-only ()
  "Toggle the mode where only the regions where both buffers differ with the ancestor are shown."
  (interactive)
  (if (not ediff-merge-with-ancestor-job)
      (error "This command makes sense only when merging with an ancestor"))
  (setq ediff-show-clashes-only (not ediff-show-clashes-only))
  (if ediff-show-clashes-only
      (message "Focus on regions where both buffers differ from the ancestor")
    (message "Canceling focus on regions where changes clash")))
	       
;; Widening/narrowing

(defun ediff-toggle-narrow-region ()
  "Toggle narrowing in buffers A, B, and C.
Used in ediff-windows/regions only."
  (interactive)
  (if (eq ediff-buffer-A ediff-buffer-B)
      (error
       "Buffers A and B are the same. Can't narrow to two different regions"))
  (if (eq ediff-visible-bounds ediff-wide-bounds)
      (setq ediff-visible-bounds ediff-narrow-bounds)
    (setq ediff-visible-bounds ediff-wide-bounds))
  (ediff-recenter 'no-rehighlight))
  
;; Narrow bufs A/B/C to ediff-visible-bounds. If this is currently set to
;; ediff-wide-bounds, then this actually widens.
;; This function does nothing if job-name is not
;; ediff-regions-wordwise/linewise or ediff-windows-wordwise/linewise. 
;; Does nothing if buffer-A  = buffer-B since we can't narrow
;; to two different regions in one buffer.
(defun ediff-visible-region ()
  (if (or (eq ediff-buffer-A ediff-buffer-B)
	  (eq ediff-buffer-A ediff-buffer-C)
	  (eq ediff-buffer-C ediff-buffer-B))
      ()
    ;; If ediff-*-regions/windows, ediff-visible-bounds is already set
    ;; Otherwise, always use full range.
    (if (not ediff-narrow-job)
	(setq ediff-visible-bounds ediff-wide-bounds))
    (let ((overl-A (ediff-get-value-according-to-buffer-type
		    'A  ediff-visible-bounds))
	  (overl-B (ediff-get-value-according-to-buffer-type
		    'B  ediff-visible-bounds))
	  (overl-C (ediff-get-value-according-to-buffer-type
		    'C  ediff-visible-bounds))
	  )
      (ediff-eval-in-buffer ediff-buffer-A
	(narrow-to-region
	 (ediff-overlay-start overl-A) (ediff-overlay-end overl-A)))
      (ediff-eval-in-buffer ediff-buffer-B
	(narrow-to-region
	 (ediff-overlay-start overl-B) (ediff-overlay-end overl-B)))
      
      (if ediff-3way-job
	  (ediff-eval-in-buffer ediff-buffer-C
	    (narrow-to-region
	     (ediff-overlay-start overl-C) (ediff-overlay-end overl-C))))
      )))
  

;; Window scrolling operations

;; Performs some operation on the two file windows (if they are showing).
;; Traps all errors on the operation in windows A/B/C.
;; Usually, errors come from scrolling off the
;; beginning or end of the buffer, and this gives error messages.
(defun ediff-operate-on-windows (operation arg)
	 
  ;; make sure windows aren't dead
  (if (not (and (window-live-p ediff-window-A) (window-live-p ediff-window-B)))
      (ediff-recenter 'no-rehighlight))
  (if (not (and (ediff-buffer-live-p ediff-buffer-A)
		(ediff-buffer-live-p ediff-buffer-B)
		(or (not ediff-3way-job) ediff-buffer-C)
		))
      (error "You've killed an essential Ediff buffer---Please quit Ediff"))
	
  (let* ((wind (selected-window))
	 (wind-A ediff-window-A)
	 (wind-B ediff-window-B)
	 (wind-C ediff-window-C)
	 (three-way ediff-3way-job))
	     
    (select-window wind-A)
    (condition-case nil
	(funcall operation arg)
      (error))
    (select-window wind-B)
    (condition-case nil
	(funcall operation arg)
      (error))
    (if three-way
	(progn
	  (select-window wind-C)
	  (condition-case nil
	      (funcall operation arg)
	    (error))))
    
    (select-window wind)))

(defun ediff-scroll-vertically (&optional arg)
  "Vertically scroll buffers A, B \(and C if appropriate\).
With optional argument ARG, scroll ARG lines; otherwise scroll by nearly
the height of window-A."
  (interactive "P")
  
  ;; make sure windows aren't dead
  (if (not (and (window-live-p ediff-window-A) (window-live-p ediff-window-B)))
      (ediff-recenter 'no-rehighlight))
  (if (not (and (ediff-buffer-live-p ediff-buffer-A)
		(ediff-buffer-live-p ediff-buffer-B)
		(or (not ediff-3way-job)
		    (ediff-buffer-live-p ediff-buffer-C))
		))
      (error "You've killed an essential Ediff buffer---Please quit Ediff"))
      
  (ediff-operate-on-windows
   (if (memq last-command-char '(?v ?\C-v))
       'scroll-up 
     'scroll-down)
   ;; calculate argument to scroll-up/down
   ;; if there is an explicit argument
   (if (and arg (not (equal arg '-)))
       ;; use it
       (prefix-numeric-value arg)
     ;; if not, see if we can determine a default amount (the window height)
     (let* (default-amount)
       (setq default-amount 
	     (- (min (window-height ediff-window-A)
		     (window-height ediff-window-B)
		     (if ediff-3way-job
			 (window-height ediff-window-C)
		       123) ; some large number
		     )
		1 next-screen-context-lines))
       ;; window found
       (if arg
	   ;; C-u as argument means half of default amount
	   (/ default-amount 2)
	 ;; no argument means default amount
	 default-amount)))))


(defun ediff-scroll-horizontally (&optional arg)
  "Horizontally scroll buffers A, B \(and C if appropriate\).
If an argument is given, that is how many columns are scrolled, else nearly
the width of the A/B/C windows."
  (interactive "P")
  
  ;; make sure windows aren't dead
  (if (not (and (window-live-p ediff-window-A) (window-live-p ediff-window-B)))
      (ediff-recenter 'no-rehighlight))
  (if (not (and (ediff-buffer-live-p ediff-buffer-A)
		(ediff-buffer-live-p ediff-buffer-B)
		(or (not ediff-3way-job)
		    (ediff-buffer-live-p ediff-buffer-C))
		))
      (error "You've killed an essential Ediff buffer---Please quit Ediff"))
    
  (ediff-operate-on-windows
   (if (= last-command-char ?<)
       'scroll-left
     'scroll-right)
   ;; calculate argument to scroll-left/right
   ;; if there is an explicit argument
   (if (and arg (not (equal arg '-)))
       ;; use it
       (prefix-numeric-value arg)
     ;; if not, see if we can determine a default amount
     ;; (half the window width)
     (if (null ediff-control-window)
	 ;; no control window, use nil
	 nil
       (let ((default-amount
	       (- (/ (min (window-width ediff-window-A)
			  (window-width ediff-window-B)
			  (if ediff-3way-comparison-job
			      (window-width ediff-window-C)
			    500) ; some large number
			  )
		     2)
		  3)))
	 ;; window found
	 (if arg
	     ;; C-u as argument means half of default amount
	     (/ default-amount 2)
	   ;; no argument means default amount
	   default-amount))))))


;;BEG, END show the region to be positioned.
;;JOB-NAME holds ediff-job-name. Ediff-windows job positions regions
;;differently. 
(defun ediff-position-region (beg end pos job-name)
  (if (> end (point-max))
      (setq end (point-max)))
  (if ediff-windows-job
      (if (pos-visible-in-window-p end)
	  () ; do nothing, wind is already positioned
	;; at this point, windows are positioned at the beginning of the
	;; file regions (not diff-regions)  being compared.
	(save-excursion
	  (move-to-window-line (- (window-height) 2))
	  (let ((amount (+ 2 (count-lines (point) end))))
	    (scroll-up amount))))
    (set-window-start (selected-window) beg)
    (if (pos-visible-in-window-p end)
	;; Determine the number of lines that the region occupies
	(let ((lines 0)
	      (prev-point 0))
	  (while ( and (> end (progn
				(move-to-window-line lines)
				(point)))
		       ;; `end' may be beyond the window bottom, so check
		       ;; that we are making progres
		       (< prev-point (point)))
	    (setq prev-point (point))
	    (setq lines (1+ lines)))
	  ;; And position the beginning on the right line
	  (goto-char beg)
	  (recenter (/ (1+ (max (- (1- (window-height (selected-window)))
				   lines)
				1)
			   )
		       2))))
    (goto-char pos)
    ))


(defun ediff-next-difference (&optional arg)
  "Advance to the next difference. 
With a prefix argument, go back that many differences."
  (interactive "P")
  (if (< ediff-current-difference ediff-number-of-differences)
      (let ((n (min ediff-number-of-differences
		    (+ ediff-current-difference (if arg arg 1))))
	    regexp-skip)
	    
	(or (>= n ediff-number-of-differences)
	    (setq regexp-skip (funcall ediff-skip-diff-region-function n))
	    (ediff-install-fine-diff-if-necessary n))
	(while (and (< n ediff-number-of-differences)
		    (or
		     ;; regexp skip
		     regexp-skip
		     ;; skip clashes, if necessary
		     (and ediff-show-clashes-only
			  (string-match "prefer"
					(or (ediff-get-state-of-merge n) "")))
		     ;; skip difference regions that differ in white space
		     (and ediff-ignore-similar-regions
			  (ediff-no-fine-diffs-p n))))
	  (setq n (1+ n))
	  (if (= 0 (mod n 20))
	      (message "Skipped over region %d and counting ..."  n))
	  (or (>= n ediff-number-of-differences)
	      (setq regexp-skip (funcall ediff-skip-diff-region-function n))
	      (ediff-install-fine-diff-if-necessary n))
	  )
	(message "")
	(ediff-unselect-and-select-difference n)
	) ; let
    (ediff-visible-region)
    (error "At end of the difference list")))

(defun ediff-previous-difference (&optional arg)
  "Go to the previous difference. 
With a prefix argument, go back that many differences."
  (interactive "P")
  (if (> ediff-current-difference -1)
      (let ((n (max -1 (- ediff-current-difference (if arg arg 1))))
	    regexp-skip)
	    
	(or (< n 0)
	    (setq regexp-skip (funcall ediff-skip-diff-region-function n))
	    (ediff-install-fine-diff-if-necessary n))
	(while (and (> n -1)
		    (or
		     ;; regexp skip
		     regexp-skip
		     ;; skip clashes, if necessary
		     (and ediff-show-clashes-only
			  (string-match "prefer"
					(or (ediff-get-state-of-merge n) "")))
		     ;; skip difference regions that differ in white space
		     (and ediff-ignore-similar-regions
			  (ediff-no-fine-diffs-p n))))
	  (if (= 0 (mod (1+ n) 20))
	      (message "Skipped over region %d and counting ..."  (1+ n)))
	  (setq n (1- n))
	  (or (< n 0)
	      (setq regexp-skip (funcall ediff-skip-diff-region-function n))
	      (ediff-install-fine-diff-if-necessary n))
	  )
	(message "")
	(ediff-unselect-and-select-difference n)
	) ; let
    (ediff-visible-region)
    (error "At beginning of the difference list")))

(defun ediff-jump-to-difference (difference-number)
  "Go to the difference specified as a prefix argument."
  (interactive "p")
  (setq difference-number (1- difference-number))
  (if (and (>= difference-number -1)
	   (< difference-number (1+ ediff-number-of-differences)))
      (ediff-unselect-and-select-difference difference-number)
    (error "Bad difference number, %d" difference-number)))
      
(defun ediff-jump-to-difference-at-point ()
  "Go to difference closest to the point in buffer A, B, or C.
The type of buffer depends on last command character \(a, b, or c\) that
invoked this command."
  (interactive)
  (let ((buf-type (ediff-char-to-buftype last-command-char)))
    (ediff-jump-to-difference (ediff-diff-at-point buf-type))))
	
      
;; find region most related to the current point position (or POS, if given)
(defun ediff-diff-at-point (buf-type &optional pos)
  (let ((buffer (ediff-get-buffer buf-type))
	(ctl-buffer ediff-control-buffer)
	(max-dif-num (1- ediff-number-of-differences))
	(diff-no -1)
	(prev-beg 0)
	(prev-end 0)
	(beg 0)
	(end 0))
	
    (ediff-eval-in-buffer buffer
      (setq pos (or pos (point)))
      (while (and (or (< pos prev-beg) (> pos beg))
		  (< diff-no max-dif-num))
	(setq diff-no (1+ diff-no))
	(setq prev-beg beg
	      prev-end end)
	(setq beg (ediff-get-diff-posn buf-type 'beg diff-no ctl-buffer)
	      end (ediff-get-diff-posn buf-type 'end diff-no ctl-buffer))
	)
      
     (if (< (abs (- pos prev-end))
	    (abs (- pos beg)))
	 diff-no
       (1+ diff-no)) ; jump-to-diff works with diff nums higher by 1
     )))


;;; Copying diffs.

(defun ediff-diff-to-diff (arg &optional keys)
  "Copy buffer-X'th diff to buffer Y \(X,Y are A, B, or C\).
If numerical prefix argument, copy this diff specified in the arg.
Otherwise, copy the difference given by `ediff-current-difference'.
This command assumes it is bound to a 2-character key sequence, `ab', `ba',
`ac', etc., which is used to determine the types of buffers to be used for
copying difference regions. The first character in the sequence specifies
the source buffer and the second specifies the target.

If the second optional argument, a 2-character string, is given, use it to
determine the source and the target buffers instead of the command keys."
  (interactive "P")
  (or keys (setq keys (this-command-keys)))
  (if arg
      (ediff-jump-to-difference arg))
  (let* ((key1 (aref keys 0))
	 (key2 (aref keys 1))
	 (char1 (if (and ediff-xemacs-p (eventp key1)) (event-key key1) key1))
	 (char2 (if (and ediff-xemacs-p (eventp key1)) (event-key key2) key2)))
    (ediff-copy-diff ediff-current-difference
		     (ediff-char-to-buftype char1)
		     (ediff-char-to-buftype char2))
    (ediff-recenter 'no-rehighlight)))


;; Copy diff N from FROM-BUF-TYPE \(given as A, B or C\) to TO-BUF-TYPE.
;; If optional DO-NOT-SAVE is non-nil, do not save the old value of the
;; target diff. This is used in merging, when constructing the merged
;; version.
(defun ediff-copy-diff (n from-buf-type to-buf-type
			  &optional batch-invocation reg-to-copy)
  (let* ((to-buf (ediff-get-buffer to-buf-type))
	 ;;(from-buf (if (not reg-to-copy) (ediff-get-buffer from-buf-type)))
	 (ctrl-buf ediff-control-buffer)
	 (saved-p t)
	 (three-way ediff-3way-job)
	 messg
	 ediff-verbose-p
	 reg-to-delete reg-to-delete-beg reg-to-delete-end)
	
    (setq reg-to-delete-beg
	  (ediff-get-diff-posn to-buf-type 'beg n ctrl-buf))
    (setq reg-to-delete-end
	  (ediff-get-diff-posn to-buf-type 'end n ctrl-buf))
	  
    (if reg-to-copy
	(setq from-buf-type nil)
      (setq reg-to-copy (ediff-get-region-contents n from-buf-type ctrl-buf)))
    
    (setq reg-to-delete (ediff-get-region-contents
			 n to-buf-type ctrl-buf
			 reg-to-delete-beg reg-to-delete-end))
    
    (setq ediff-disturbed-overlays nil) ; clear before use
    
    (if (string= reg-to-delete reg-to-copy)
	(setq saved-p nil)     ; refuse to copy identical buffers
      ;; seems ok to copy
      (if (or batch-invocation (ediff-test-save-region n to-buf-type))
	  (condition-case conds
	      (progn
		(ediff-eval-in-buffer to-buf
		  ;; to prevent flags from interfering if buffer is writable
		  (let ((inhibit-read-only (null buffer-read-only)))
		    ;; these two insert a dummy char to overcome a bug in
		    ;; XEmacs, which sometimes prevents 0-length extents
		    ;; from sensing insertion at its end-points.
		    (if ediff-xemacs-p
			(progn
			  (goto-char reg-to-delete-end)
			  (insert-before-markers "@")))
		    
		    (goto-char reg-to-delete-end)
		    (insert-before-markers reg-to-copy)
		    
		    ;; delete the dummy char "@"
		    (if ediff-xemacs-p
			(delete-char 1))
		    
		    (if ediff-xemacs-p
			(if (> reg-to-delete-end reg-to-delete-beg)
			    (kill-region reg-to-delete-beg
					 reg-to-delete-end))
		      (if (> reg-to-delete-end reg-to-delete-beg)
			  (kill-region reg-to-delete-beg reg-to-delete-end)
			(ediff-move-disturbed-overlays reg-to-delete-beg)
			))
		    ))
		(or batch-invocation
		    (setq 
		     messg
		     (ediff-save-diff-region n to-buf-type reg-to-delete))))
	    (error (message "ediff-copy-diff: %s %s"
			    (car conds)
			    (mapconcat 'prin1-to-string (cdr conds) " "))
		   (beep 1)
		   (sit-for 2)
		   (setq saved-p nil)
		   )))
      )
    
    ;; adjust state of difference in case 3-way and diff was copied ok
    (if (and saved-p three-way)
	(ediff-set-state-of-diff-in-all-buffers n ctrl-buf))

    (if batch-invocation
	(ediff-clear-fine-differences n)
      ;; If diff3 job, we should recompute fine diffs so we clear them
      ;; before reinserting flags (and thus before ediff-recenter).
      (if (and saved-p three-way)
	  (ediff-clear-fine-differences n))
      
      (ediff-refresh-mode-lines)
      
      ;; For diff2 jobs, don't recompute fine diffs, since we know there
      ;; aren't any. So we clear diffs after ediff-recenter.
      (if (and saved-p (not three-way))
	  (ediff-clear-fine-differences n))
      ;; Make sure that the message about saving and how to restore is seen
      ;; by the user
      (message messg))
    ))
     
;; Save Nth diff of buffer BUF-TYPE \(A, B, or C\).
;; That is to say, the Nth diff on the `ediff-killed-diffs-alist'.  REG
;; is the region to save.  It is redundant here, but is passed anyway, for
;; convenience.
(defun ediff-save-diff-region (n buf-type reg)
  (let* ((n-th-diff-saved (assoc n ediff-killed-diffs-alist))
	 (buf (ediff-get-buffer buf-type))
	 (this-buf-n-th-diff-saved (assoc buf (cdr n-th-diff-saved))))
	 
    (if this-buf-n-th-diff-saved
	;; either nothing saved for n-th diff and buffer or we OK'ed
	;; overriding
	(setcdr this-buf-n-th-diff-saved reg)
      (if n-th-diff-saved ;; n-th diff saved, but for another buffer
	  (nconc n-th-diff-saved  (list (cons buf reg)))
	(setq ediff-killed-diffs-alist  ;; create record for n-th diff
	      (cons (list n (cons buf reg))
		    ediff-killed-diffs-alist))))
    (message "Saving old diff region #%d of buffer %S. To recover, type `r%s'"
	     (1+ n) buf-type
	     (if ediff-merge-job
		 "" (downcase (symbol-name buf-type))))
    ))
    
;; Test if saving Nth difference region of buffer BUF-TYPE is possible.
(defun ediff-test-save-region (n buf-type)
  (let* ((n-th-diff-saved (assoc n ediff-killed-diffs-alist))
	 (buf (ediff-get-buffer buf-type))
	 (this-buf-n-th-diff-saved (assoc buf (cdr n-th-diff-saved))))
	 
    (if this-buf-n-th-diff-saved
	(if (yes-or-no-p
	     (format 
	      "You've previously copied diff region %d to buffer %S. Confirm "
	      (1+ n) buf-type))
	    t
	  (error "Quit"))
      t)))
	  
(defun ediff-pop-diff (n buf-type)
  "Pop last killed Nth diff region from buffer BUF-TYPE."
  (let* ((n-th-record (assoc n ediff-killed-diffs-alist))
	 (buf (ediff-get-buffer buf-type))
	 (saved-rec (assoc buf (cdr n-th-record)))
	 (three-way ediff-3way-job)
	 (ctl-buf ediff-control-buffer)
	 ediff-verbose-p
	 saved-diff reg-beg reg-end recovered)
	
    (if (cdr saved-rec)
	(setq saved-diff (cdr saved-rec))
      (if (> ediff-number-of-differences 0)
	  (error "Nothing saved for diff %d in buffer %S" (1+ n) buf-type)
	(error "No differences found")))
    
    (setq reg-beg (ediff-get-diff-posn buf-type 'beg n ediff-control-buffer))
    (setq reg-end (ediff-get-diff-posn buf-type 'end n ediff-control-buffer))
    (setq ediff-disturbed-overlays nil) ; clear before use
    
    (condition-case conds
	(ediff-eval-in-buffer buf
	  (let ((inhibit-read-only (null buffer-read-only)))
	    ;; these two insert a dummy char to overcome a bug in XEmacs,
	    ;; which sometimes prevents 0-length extents from sensing
	    ;; insertion at its end-points.
	    (if ediff-xemacs-p
		(progn
		  (goto-char reg-end)
		  (insert-before-markers "@")))
	    
	    (goto-char reg-end)
	    (insert-before-markers saved-diff)
	    
	    ;; delete dummy char
	    (if ediff-xemacs-p
		(delete-char 1))
	    
	    (if ediff-xemacs-p
		(if (> reg-end reg-beg)
		    (kill-region reg-beg reg-end))
	      (if (> reg-end reg-beg)
		  (kill-region reg-beg reg-end)
		(ediff-move-disturbed-overlays reg-beg)
		))
	    
	    (setq recovered t)
	    ))
      (error (message "ediff-pop-diff: %s %s"
		      (car conds)
		      (mapconcat 'prin1-to-string (cdr conds) " "))
	     (beep 1)))
    
    ;; Clearing fine diffs is necessary for
    ;; ediff-unselect-and-select-difference to properly recompute them. We
    ;; can't rely on ediff-copy-diff to clear this vector, as the user might
    ;; have modified diff regions after copying and, thus, may have recomputed
    ;; fine diffs.
    (if recovered
	(ediff-clear-fine-differences n))
	  
    ;; adjust state of difference
    (if (and three-way recovered)
	(ediff-set-state-of-diff-in-all-buffers n ctl-buf))
	
    (ediff-refresh-mode-lines)
    
    (if recovered
	(progn
	  (setq n-th-record (delq saved-rec n-th-record))
	  (message "Diff region %d in buffer %S restored" (1+ n) buf-type)
	  ))
    ))
      
(defun ediff-restore-diff  (arg &optional key)
  "Restore ARGth diff from `ediff-killed-diffs-alist'.
ARG is a prefix argument.  If ARG is nil, restore current-difference.

If the second optional argument, a character, is given, use it to
determine the target buffer instead of last-command-char"
  (interactive "P")
  (if arg
      (ediff-jump-to-difference arg))
  (ediff-pop-diff ediff-current-difference 
		  (ediff-char-to-buftype (or key last-command-char)))
  (ediff-recenter 'no-rehighlight))
  
(defun ediff-toggle-regexp-match ()
  "Toggle between focusing and hiding of difference regions that match
a regular expression typed in by the user."
  (interactive)
  (let ((regexp-A "")
	(regexp-B "")
	(regexp-C "")
	msg-connective alt-msg-connective alt-connective)
    (cond
     ((or (and (eq ediff-skip-diff-region-function
		   'ediff-focus-on-regexp-matches)
	       (eq last-command-char ?f))
	  (and (eq ediff-skip-diff-region-function 'ediff-hide-regexp-matches)
	       (eq last-command-char ?h)))
      (message "Selective browsing by regexp turned off")
      (setq ediff-skip-diff-region-function 'ediff-show-all-diffs))
     ((eq last-command-char ?h)
      (setq ediff-skip-diff-region-function 'ediff-hide-regexp-matches
	    regexp-A 
	    (read-string
	     (format 
	      "Ignore A-regions matching this regexp (default \"%s\"): "
	      ediff-regexp-hide-A))
	    regexp-B
	    (read-string
	     (format 
	      "Ignore B-regions matching this regexp (default \"%s\"): "
	      ediff-regexp-hide-B)))
      (if ediff-3way-comparison-job
	  (setq regexp-C
		(read-string
		 (format 
		  "Ignore C-regions matching this regexp (default \"%s\"): "
		  ediff-regexp-hide-C))))
      (if (eq ediff-hide-regexp-connective 'and)
	  (setq msg-connective "BOTH"
		alt-msg-connective "ONE OF"
		alt-connective 'or)
	(setq msg-connective "ONE OF"
	      alt-msg-connective "BOTH"
	      alt-connective 'and))
      (if (y-or-n-p
	   (format
	    "Hiding diff regions that match %s regexps. Use %s instead? "
	    msg-connective alt-msg-connective))
	  (progn
	    (setq ediff-hide-regexp-connective alt-connective)
	    (message "Hiding diff regions that match %s regexps"
		     alt-msg-connective))
	(message "Hiding diff regions that match %s regexps" msg-connective))
      (or (string= regexp-A "") (setq ediff-regexp-hide-A regexp-A))
      (or (string= regexp-B "") (setq ediff-regexp-hide-B regexp-B))
      (or (string= regexp-C "") (setq ediff-regexp-hide-C regexp-C)))
     ((eq last-command-char ?f)
      (setq ediff-skip-diff-region-function 'ediff-focus-on-regexp-matches
	    regexp-A 
	    (read-string
	     (format 
	      "Focus on A-regions matching this regexp (default \"%s\"): "
	      ediff-regexp-focus-A))
	    regexp-B
	    (read-string
	     (format 
	      "Focus on B-regions matching this regexp (default \"%s\"): "
	      ediff-regexp-focus-B)))
      (if ediff-3way-comparison-job
	  (setq regexp-C
		(read-string
		 (format 
		  "Focus on C-regions matching this regexp (default \"%s\"): "
		  ediff-regexp-focus-C))))
      (if (eq ediff-focus-regexp-connective 'and)
	  (setq msg-connective "BOTH"
		alt-msg-connective "ONE OF"
		alt-connective 'or)
	(setq msg-connective "ONE OF"
	      alt-msg-connective "BOTH"
	      alt-connective 'and))
      (if (y-or-n-p
	   (format
	    "Focusing on diff regions that match %s regexps. Use %s instead? "
	    msg-connective alt-msg-connective))
	  (progn
	    (setq ediff-focus-regexp-connective alt-connective)
	    (message "Focusing on diff regions that match %s regexps" 
		     alt-msg-connective))
	(message "Focusing on diff regions that match %s regexps"
		 msg-connective))
      (or (string= regexp-A "") (setq ediff-regexp-focus-A regexp-A))
      (or (string= regexp-B "") (setq ediff-regexp-focus-B regexp-B))
      (or (string= regexp-C "") (setq ediff-regexp-focus-C regexp-C))))))
      
(defun ediff-toggle-skip-similar ()
  (interactive)
  (if (not (eq ediff-auto-refine 'on))
      (error
       "Can't skip over whitespace regions: first turn auto-refining on"))
  (setq ediff-ignore-similar-regions (not ediff-ignore-similar-regions))
  (if ediff-ignore-similar-regions
      (message
       "Skipping regions that differ only in white space & line breaks")
    (message "Skipping over white-space differences turned off")))
  
(defun ediff-focus-on-regexp-matches (n)
  "Focus on diffs that match regexp `ediff-regexp-focus-A/B'.
Regions to be ignored according to this function are those where   
buf A region doesn't match `ediff-regexp-focus-A' and buf B region
doesn't match `ediff-regexp-focus-B'.
This function returns nil if the region number N (specified as
an argument) is not to be ignored and t if region N is to be ignored.

N is a region number used by Ediff internally. It is 1 less
the number seen by the user."
  (if (ediff-valid-difference-p n)
      (let* ((ctl-buf ediff-control-buffer)
	     (regex-A ediff-regexp-focus-A)
	     (regex-B ediff-regexp-focus-B)
	     (regex-C ediff-regexp-focus-C)
	     (reg-A-match (ediff-eval-in-buffer ediff-buffer-A
			    (goto-char (ediff-get-diff-posn 'A 'beg n ctl-buf))
			    (re-search-forward
			     regex-A
			     (ediff-get-diff-posn 'A 'end n ctl-buf)
			     t)))
	     (reg-B-match (ediff-eval-in-buffer ediff-buffer-B
			    (goto-char (ediff-get-diff-posn 'B 'beg n ctl-buf))
			    (re-search-forward
			     regex-B
			     (ediff-get-diff-posn 'B 'end n ctl-buf)
			     t)))
	     (reg-C-match (if ediff-3way-comparison-job
			      (ediff-eval-in-buffer ediff-buffer-C
				(goto-char
				 (ediff-get-diff-posn 'C 'beg n ctl-buf))
				(re-search-forward
				 regex-C
				 (ediff-get-diff-posn 'C 'end n ctl-buf)
				 t)))))
	(not (eval (if ediff-3way-comparison-job
		       (list ediff-focus-regexp-connective
			     reg-A-match reg-B-match reg-C-match)
		     (list ediff-focus-regexp-connective
			   reg-A-match reg-B-match))))
	)))
  
(defun ediff-hide-regexp-matches (n)  
  "Hide diffs that match regexp `ediff-regexp-hide-A/B/C'.
Regions to be ignored are those where buf A region matches
`ediff-regexp-hide-A' and buf B region matches `ediff-regexp-hide-B'.
This function returns nil if the region number N (specified as
an argument) is not to be ignored and t if region N is to be ignored.

N is a region number used by Ediff internally. It is 1 less
the number seen by the user."
  (if (ediff-valid-difference-p n)
      (let* ((ctl-buf ediff-control-buffer)
	     (regex-A ediff-regexp-hide-A)
	     (regex-B ediff-regexp-hide-B)
	     (regex-C ediff-regexp-hide-C)
	     (reg-A-match (ediff-eval-in-buffer ediff-buffer-A
			    (goto-char (ediff-get-diff-posn 'A 'beg n ctl-buf))
			    (re-search-forward
			     regex-A
			     (ediff-get-diff-posn 'A 'end n ctl-buf)
			     t)))
	     (reg-B-match (ediff-eval-in-buffer ediff-buffer-B
			    (goto-char (ediff-get-diff-posn 'B 'beg n ctl-buf))
			    (re-search-forward
			     regex-B
			     (ediff-get-diff-posn 'B 'end n ctl-buf)
			     t)))
	     (reg-C-match (if ediff-3way-comparison-job
			      (ediff-eval-in-buffer ediff-buffer-C
				(goto-char
				 (ediff-get-diff-posn 'C 'beg n ctl-buf))
				(re-search-forward
				 regex-C
				 (ediff-get-diff-posn 'C 'end n ctl-buf)
				 t)))))
	(eval (if ediff-3way-comparison-job
		  (list ediff-hide-regexp-connective
			reg-A-match reg-B-match reg-C-match)
		(list ediff-hide-regexp-connective reg-A-match reg-B-match)))
	)))
    


;;; Quitting, suspending, etc.

(defun ediff-quit ()
  "Finish an Ediff session and exit Ediff.
Unselects the selected difference, if any, restores the read-only and modified
flags of the compared file buffers, kills Ediff buffers for this session
\(but not buffers A, B, C\)."
  (interactive)
  (if (prog1
	  (y-or-n-p "Do you really want to exit Ediff? ")
	(message ""))
      (ediff-really-quit)))


;; Perform the quit operations.
(defun ediff-really-quit ()
  (ediff-unhighlight-diffs-totally)
  (ediff-clear-diff-vector 'ediff-difference-vector-A 'fine-diffs-also)
  (ediff-clear-diff-vector 'ediff-difference-vector-B 'fine-diffs-also)
  (ediff-clear-diff-vector 'ediff-difference-vector-C 'fine-diffs-also)
  
  (ediff-delete-temp-files)
				  
  ;; Restore visibility range. This affects only ediff-*-regions/windows.
  ;; Since for other job names ediff-visible-region sets
  ;; ediff-visible-bounds to ediff-wide-bounds, the settings below are
  ;; ignored for such jobs.
  (if ediff-quit-widened
      (setq ediff-visible-bounds ediff-wide-bounds)
    (setq ediff-visible-bounds ediff-narrow-bounds))
  
  ;; Apply selective display to narrow or widen
  (ediff-visible-region)
  (mapcar (function (lambda (overl)
		      (if (ediff-overlayp overl)
			  (ediff-delete-overlay overl))))
	  ediff-wide-bounds)
  (mapcar (function (lambda (overl)
		      (if (ediff-overlayp overl)
			  (ediff-delete-overlay overl))))
	  ediff-narrow-bounds)
  
  ;; restore buffer mode line id's in buffer-A/B/C
  (let ((control-buffer ediff-control-buffer))
    (condition-case nil
	(ediff-eval-in-buffer ediff-buffer-A
	  (setq ediff-this-buffer-control-sessions 
		(delq control-buffer ediff-this-buffer-control-sessions))
	  (kill-local-variable 'mode-line-buffer-identification)
	  (kill-local-variable 'mode-line-format)
	  )
      (error))
      
    (condition-case nil
	(ediff-eval-in-buffer ediff-buffer-B
	  (setq ediff-this-buffer-control-sessions 
		(delq control-buffer ediff-this-buffer-control-sessions))
	  (kill-local-variable 'mode-line-buffer-identification)
	  (kill-local-variable 'mode-line-format)
	  )
      (error))
    
    (condition-case nil
	(ediff-eval-in-buffer ediff-buffer-C
	  (kill-local-variable 'mode-line-buffer-identification)
	  (kill-local-variable 'mode-line-format)
	  )
      (error))
    )
  ;; restore state of buffers to what it was before ediff
  (ediff-restore-protected-variables)
  ;; good place to kill buffers A/B/C
  (run-hooks 'ediff-cleanup-hooks)
  (run-hooks 'ediff-quit-hooks))
  
  
(defun ediff-delete-temp-files ()
  (if (stringp ediff-temp-file-A)
      (delete-file ediff-temp-file-A))
  (if (stringp ediff-temp-file-B)
      (delete-file ediff-temp-file-B))
  (if (stringp ediff-temp-file-C)
      (delete-file ediff-temp-file-C)))
  

;; Kill control buffer, other auxiliary Ediff buffers.
;; Leave one of the frames split between buffers A/B/C
(defun ediff-cleanup-mess ()
  (let ((buff-A ediff-buffer-A)
	(buff-B ediff-buffer-B)
	(buff-C ediff-buffer-C)
	(ctl-buf  ediff-control-buffer)
	(ctl-frame ediff-control-frame)
	(three-way-job ediff-3way-job))
	
    (ediff-kill-buffer-carefully ediff-diff-buffer)
    (ediff-kill-buffer-carefully ediff-custom-diff-buffer)
    (ediff-kill-buffer-carefully ediff-fine-diff-buffer)
    (ediff-kill-buffer-carefully ediff-tmp-buffer)
    (ediff-kill-buffer-carefully ediff-error-buffer)
    (ediff-kill-buffer-carefully ediff-patch-diagnostics)
    (ediff-kill-buffer-carefully ediff-msg-buffer)
    (ediff-kill-buffer-carefully ediff-debug-buffer)

    ;;(redraw-display)
    (if (and (ediff-window-display-p) (frame-live-p ctl-frame))
	(delete-frame ctl-frame))
    (ediff-kill-buffer-carefully ctl-buf)
      
    (delete-other-windows)
    
    ;; display only if not visible
    (condition-case nil
	(or (ediff-get-visible-buffer-window buff-B)
	    (switch-to-buffer buff-B))
      (error))
    (condition-case nil
	(or (ediff-get-visible-buffer-window buff-A)
	    (progn
	      (if (ediff-get-visible-buffer-window buff-B)
		  (split-window-vertically))
	      (switch-to-buffer buff-A)))
      (error))
    (if three-way-job
	(condition-case nil
	    (or (ediff-get-visible-buffer-window buff-C)
		(progn
		  (if (or (ediff-get-visible-buffer-window buff-A)
			  (ediff-get-visible-buffer-window buff-B))
		      (split-window-vertically))
		  (switch-to-buffer buff-C)
		  (balance-windows)))
	  (error)))
    (message "")
    ))

(defun ediff-janitor ()
  "Kill buffers A, B, and, possibly, C, if these buffers aren't modified.
In merge jobs, buffer C is never deleted.
However, the side effect of cleaning up may be that you cannot compare the same
buffer in two separate Ediff sessions: quitting one of them will delete this
buffer in another session as well."
  (or (not (ediff-buffer-live-p ediff-buffer-A))
      (buffer-modified-p ediff-buffer-A)
      (ediff-kill-buffer-carefully ediff-buffer-A))
  (or (not (ediff-buffer-live-p ediff-buffer-B))
      (buffer-modified-p ediff-buffer-B)
      (ediff-kill-buffer-carefully ediff-buffer-B))
  (if ediff-merge-job  ; don't del buf C if merging--del ancestor buf instead
      (or (not (ediff-buffer-live-p ediff-ancestor-buffer))
	  (buffer-modified-p ediff-ancestor-buffer)
	  (ediff-kill-buffer-carefully ediff-ancestor-buffer))
    (or (not (ediff-buffer-live-p ediff-buffer-C))
	(buffer-modified-p ediff-buffer-C)
	(ediff-kill-buffer-carefully ediff-buffer-C))))

;; The default way of suspending Ediff.
;; Buries Ediff buffers, kills all windows.
(defun ediff-default-suspend-hook ()
  (let* ((buf-A ediff-buffer-A)
	 (buf-B ediff-buffer-B)
	 (buf-C ediff-buffer-C)
	 (buf-A-wind (ediff-get-visible-buffer-window buf-A))
	 (buf-B-wind (ediff-get-visible-buffer-window buf-B))
	 (buf-C-wind (ediff-get-visible-buffer-window buf-C))
	 (buf-patch ediff-patch-buf)
	 (buf-patch-diag ediff-patch-diagnostics)
	 (buf-err  ediff-error-buffer)
	 (buf-diff ediff-diff-buffer)
	 (buf-custom-diff ediff-custom-diff-buffer)
	 (buf-fine-diff ediff-fine-diff-buffer))
    
    ;; hide the control panel
    (if (and (ediff-window-display-p) (frame-live-p ediff-control-frame))
	(iconify-frame ediff-control-frame)
      (bury-buffer)) 
    (if buf-err (bury-buffer buf-err))
    (if buf-diff (bury-buffer buf-diff))
    (if buf-custom-diff (bury-buffer buf-custom-diff))
    (if buf-fine-diff (bury-buffer buf-fine-diff))
    (if buf-patch (bury-buffer buf-patch))
    (if buf-patch-diag (bury-buffer buf-patch-diag))
    (if (window-live-p buf-A-wind)
	(progn
	  (select-window buf-A-wind)
	  (delete-other-windows)
	  (bury-buffer))
      (if (ediff-buffer-live-p buf-A) (bury-buffer buf-A)))
    (if (window-live-p buf-B-wind)
	(progn
	  (select-window buf-B-wind)
	  (delete-other-windows)
	  (bury-buffer))
      (if (ediff-buffer-live-p buf-B) (bury-buffer buf-B)))
    (if (window-live-p buf-C-wind)
	(progn
	  (select-window buf-C-wind)
	  (delete-other-windows)
	  (bury-buffer))
      (if (ediff-buffer-live-p buf-C) (bury-buffer buf-C)))

    ))

     
(defun ediff-suspend ()
  "Suspend Ediff.
To resume, switch to the appropriate `Ediff Control Panel'
buffer and then type \\[ediff-recenter].  Ediff will automatically set
up an appropriate window config."
  (interactive)
  (let ((key (substitute-command-keys "\\[ediff-recenter]")))
  (run-hooks 'ediff-suspend-hooks)
  (message
   "To resume, switch to Ediff Control Panel and hit `%s'" key)))


(defun ediff-status-info ()
  "Show the names of the buffers or files being operated on by Ediff.
Hit \\[ediff-recenter] to reset the windows afterward."
  (interactive)
  (with-output-to-temp-buffer " *ediff-info*"
    (princ (ediff-version))
    (princ "\n\n")
    (ediff-eval-in-buffer ediff-buffer-A
      (if buffer-file-name
	  (princ
	   (format "File A = %S\n" buffer-file-name))
	(princ 
	 (format "Buffer A = %S\n" (buffer-name)))))
    (ediff-eval-in-buffer ediff-buffer-B
      (if buffer-file-name
	  (princ
	   (format "File B = %S\n" buffer-file-name))
	(princ 
	 (format "Buffer B = %S\n" (buffer-name)))))
    (if ediff-3way-job
	(ediff-eval-in-buffer ediff-buffer-C
	  (if buffer-file-name
	      (princ
	       (format "File C = %S\n" buffer-file-name))
	    (princ 
	     (format "Buffer C = %S\n" (buffer-name))))))
			      
    (let* ((A-line (ediff-eval-in-buffer ediff-buffer-A
		     (1+ (count-lines (point-min) (point)))))
	   (B-line (ediff-eval-in-buffer ediff-buffer-B
		     (1+ (count-lines (point-min) (point)))))
	   C-line)
      (princ (format "\Buffer A's point is on line %d\n" A-line))
      (princ (format "Buffer B's point is on line %d\n" B-line))
      (if ediff-3way-job
	  (progn
	    (setq C-line (ediff-eval-in-buffer ediff-buffer-C
			   (1+ (count-lines (point-min) (point)))))
	    (princ (format "Buffer C's point is on line %d\n" C-line)))))
      
    (princ (format "\nCurrent difference number = %S\n"
		   (cond ((< ediff-current-difference 0) 'start)
			 ((>= ediff-current-difference
			      ediff-number-of-differences) 'end)
			 (t (1+ ediff-current-difference)))))

    (cond (ediff-ignore-similar-regions
	   (princ "\nSkipping regions that differ only in white space & line breaks"))
	  (t 
	   (princ "\nNot skipping regions that differ in white space & line breaks")))
    
    (cond ((eq ediff-skip-diff-region-function 'ediff-show-all-diffs)
	   (princ "\nSelective browsing by regexp is off.\n"))
	  ((eq ediff-skip-diff-region-function 'ediff-hide-regexp-matches)
	   (princ
	    "\nIgnoring regions that match")
	   (princ
	    (format 
	     "\n\t regexp `%s' in buffer A  %S\n\t regexp `%s' in buffer B\n"
	     ediff-regexp-hide-A ediff-hide-regexp-connective
	     ediff-regexp-hide-B)))
	  ((eq ediff-skip-diff-region-function 'ediff-focus-on-regexp-matches)
	   (princ
	    "\nFocusing on regions that match")
	   (princ
	    (format
	     "\n\t regexp `%s' in buffer A  %S\n\t regexp `%s' in buffer B\n"
	     ediff-regexp-focus-A ediff-focus-regexp-connective
	     ediff-regexp-focus-B)))
	  (t (princ "\nSelective browsing via a user-defined method.\n")))
    
    (princ
     (format "\nBugs/suggestions: type `%s' while in Ediff Control Panel."
	     (substitute-command-keys "\\[ediff-submit-report]")))
    ))
    
(defun ediff-documentation ()
  "Jump to Ediff's Info file."
  (interactive)
  (let ((ctl-window ediff-control-window)
	(ctl-buf ediff-control-buffer))

    (ediff-skip-unsuitable-frames)
    (condition-case nil
	(progn
	  (pop-to-buffer (get-buffer-create "*info*"))
	  (info "ediff")
	  (message "Type `i' to search for a specific topic"))
      (error (beep 1)
	     (with-output-to-temp-buffer " *ediff-info*"
	       (princ (format "
The Info file for Ediff does not seem to be installed.

This file is part of the distribution of %sEmacs.
Please contact your system administrator. "
			      (if ediff-xemacs-p "X" ""))))
	     (if (window-live-p ctl-window)
		 (progn
		   (select-window ctl-window)
		   (switch-to-buffer ctl-buf)))))))
    



;;; Support routines

;; Select a difference by placing the ASCII flags around the appropriate
;; group of lines in the A, B buffers
;; This may have to be modified for buffer C, when it will be supported.
(defun ediff-select-difference (n)
  (if (and (ediff-buffer-live-p ediff-buffer-A)
	   (ediff-buffer-live-p ediff-buffer-B)
	   (ediff-valid-difference-p n))
      (progn
	(if (and (ediff-window-display-p) ediff-use-faces)
	    (progn
	      (ediff-highlight-diff n)
	      (setq ediff-highlighting-style 'face))
	  (setq ediff-highlighting-style 'ascii)
	  (ediff-place-flags-in-buffer
	   'A ediff-buffer-A ediff-control-buffer n)
	  (ediff-place-flags-in-buffer
	   'B ediff-buffer-B ediff-control-buffer n)
	  (if ediff-3way-job
	      (ediff-place-flags-in-buffer
	       'C ediff-buffer-C ediff-control-buffer n))
	  ) 
				       
	(ediff-install-fine-diff-if-necessary n)
	(run-hooks 'ediff-select-hooks))))
	

;; Unselect a difference by removing the ASCII flags in the buffers.
;; This may have to be modified for buffer C, when it will be supported.
(defun ediff-unselect-difference (n)
  (if (ediff-valid-difference-p n)
      (progn 
	(cond ((and (ediff-window-display-p) ediff-use-faces)
	       (ediff-unhighlight-diff))
	      ((eq ediff-highlighting-style 'ascii)
	       (ediff-remove-flags-from-buffer
		ediff-buffer-A
		(ediff-get-diff-overlay n 'A))
	       (ediff-remove-flags-from-buffer
		ediff-buffer-B
		(ediff-get-diff-overlay n 'B))
	       (if ediff-3way-job
		   (ediff-remove-flags-from-buffer
		    ediff-buffer-C
		    (ediff-get-diff-overlay n 'C)))
	       ))
	(setq ediff-highlighting-style nil)
	
	;; unhighlight fine diffs
	(ediff-set-fine-diff-properties ediff-current-difference 'default)
	(run-hooks 'ediff-unselect-hooks))))
  

;; Unselects prev diff and selects a new one, if FLAG has value other than
;; 'select-only or 'unselect-only.  If FLAG is 'select-only, the
;; next difference is selected, but the current selection is not
;; unselected.  If FLAG is 'unselect-only then the current selection is
;; unselected, but the next one is not selected.  If NO-RECENTER is non-nil,
;; don't recenter buffers after selecting/unselecting.
;; 
(defun ediff-unselect-and-select-difference (n &optional flag no-recenter)
  (let ((ediff-current-difference n))
    (or no-recenter
	(ediff-recenter 'no-rehighlight)))
	  
  (let ((control-buf ediff-control-buffer))
    (unwind-protect    
	(progn
	  (or (eq flag 'select-only)
	      (ediff-unselect-difference ediff-current-difference))
	  
	  (or (eq flag 'unselect-only)
	      (ediff-select-difference n))
	  (setq ediff-current-difference n)
	  ) ; end protected section
      
      (ediff-eval-in-buffer control-buf
	(ediff-refresh-mode-lines))
      )))


(defun ediff-read-file-name (prompt default-dir default-file)
; This is a modified version of a similar function in `emerge.el'.
; PROMPT should not have trailing ': ', so that it can be modified
; according to context.
; If default-file is set, it should be used as the default value.
; If default-dir is non-nil, use it as the default directory.
; Otherwise, use the value of Emacs' variable `default-directory.'

  ;; hack default-dir if it is not set
  (setq default-dir
	(file-name-as-directory
	 (abbreviate-file-name
	  (expand-file-name (or default-dir
				(and default-file
				     (file-name-directory default-file))
				default-directory)))))

  ;; strip the directory from default-file
  (if default-file
      (setq default-file (file-name-nondirectory default-file)))
  (if (string= default-file "")
      (setq default-file nil))

  (let (f)
    (setq f (expand-file-name
	     (read-file-name
	      (format "%s%s: "
		      prompt
		      (if default-file
			  (concat " (default " default-file ")")
			""))
	      default-dir
	      default-file
	      t  ; must match, no-confirm
	      (if default-file (file-name-directory default-file))
	      )
	     default-dir
	     ))
    ;; If user enters a directory name, expand the default file in that
    ;; directory.  This allows the user to enter a directory name for the
    ;; B-file and diff against the default-file in that directory instead
    ;; of a DIRED listing!
    (if (and (file-directory-p f) default-file)
	(setq f (expand-file-name
		 (file-name-nondirectory default-file) f)))
    f)) 
  
;; If `prefix' is given, then it is used as a prefix for the temp file
;; name. Otherwise, `_buffer-name' is used. If `file' is given, use this
;; file and don't create a new one.
;; Also, save buffer from START to END in the file.
;; START defaults to (point-min), END to (point-max)
(defun ediff-make-temp-file (&optional prefix given-file start end)
  (let ((f (or given-file
	       (make-temp-name (concat
				ediff-temp-file-prefix
				(if prefix
				    (concat prefix "_")
				    "ediff_"))))))
    ;; create the file
    (write-region (if start start (point-min))
		  (if end end (point-max))
		  f
		  nil          ; don't append---erase
		  'no-message) 
    (set-file-modes f ediff-temp-file-mode)
    f))

;; Quote metacharacters (using \) when executing diff in Unix, but not in
;; EMX OS/2
(defun ediff-protect-metachars (str)
  (or (memq system-type '(emx vax-vms axp-vms))
      (let ((limit 0))
	(while (string-match ediff-metachars str limit)
	  (setq str (concat (substring str 0 (match-beginning 0))
			    "\\"
			    (substring str (match-beginning 0))))
	  (setq limit (1+ (match-end 0))))))
  str)

;; Make sure the current buffer (for a file) has the same contents as the
;; file on disk, and attempt to remedy the situation if not.
;; Signal an error if we can't make them the same, or the user doesn't want
;; to do what is necessary to make them the same.
;; If file has file handlers (indicated by the optional arg), then we
;; offer to instead of saving. This is one difference with Emerge. 
;; Another is that we always offer to revert obsolete files, whether they
;; are modified or not.
(defun ediff-verify-file-buffer (&optional file-magic)
  ;; First check if the file has been modified since the buffer visited it.
  (if (verify-visited-file-modtime (current-buffer))
      (if (buffer-modified-p)
	  ;; If buffer is not obsolete and is modified, offer to save
	  (if (yes-or-no-p 
	       (format "Buffer out of sync with visited file. %s file %s? "
		       (if file-magic "Revert" "Save")
		       buffer-file-name))
	      (if (not file-magic)
		  (save-buffer)
		;; for some reason, file-name-handlers append instead of
		;; replacing, so we have to erase first.
		(erase-buffer)
		(revert-buffer t t))
	    (error "Buffer out of sync for file %s" buffer-file-name))
	;; If buffer is not obsolete and is not modified, do nothing
	nil)
    ;; If buffer is obsolete, offer to revert
    (if (yes-or-no-p
	 (format "Buffer out of sync with visited file. Revert file %s? "
		 buffer-file-name))
	(progn
	  (if file-magic
	      (erase-buffer))
	  (revert-buffer t t))
      (error "Buffer out of sync for file %s" buffer-file-name))))



(defun ediff-check-for-ascii-flags ()
  (eval
   (cons 'or
	 (mapcar (function (lambda (buf)
			     (if (ediff-buffer-live-p buf)
				 (ediff-eval-in-buffer buf
				   (eq ediff-highlighting-style 'ascii)))))
		 ediff-this-buffer-control-sessions))))

;; It would be nice to use these two functions as hooks instead of
;; ediff-insert-in-front and ediff-move-disturbed-overlays.
;; However, Emacs has a bug that causes BEG and END, below, to be
;; the same, i.e., the end of inserted text is not passed correctly.
;; Since the overlay doesn't move when these hooks run, 
;; there is no way to correctly determine the new (desired) position of
;; the overlay end.
;; Either this bug is fixed, or (better) use sticky overlays when they will
;; be implemented in Emacs, like they are in XEmacs.
;;(defun ediff-capture-inserts-in-front (overl beg end)
;;  (if (ediff-overlay-get overl 'ediff-diff-num)
;;      (ediff-move-overlay
;;       overl beg (+ (- end beg) (ediff-overlay-end overl)))
;;  ))
;;(defun ediff-capture-inserts-behind (overl beg end)
;;  (if (ediff-overlay-get overl 'ediff-diff-num)
;;      (ediff-move-overlay overl (ediff-overlay-start overl) end))
;;  ))
  
;; to be deleted in due time
;; Capture overlays that had insertions in the front.
;; Called when overlay OVERL gets insertion in front.
(defun ediff-insert-in-front (overl &optional flag beg end length)
  (if (ediff-overlay-get overl 'ediff-diff-num)
      (setq ediff-disturbed-overlays
	    (cons overl ediff-disturbed-overlays))))
  
  
;; to be deleted in due time
;; We can't move overlays directly in insert-in-front-hooks
;; because when diff is highlighted  with ascii flags, they will  disturb
;; overlays and so they will be included in them, which we don't want.
(defun  ediff-move-disturbed-overlays (posn)  
  (mapcar (function (lambda (overl)
		       (ediff-move-overlay overl
					   posn
					   (ediff-overlay-end overl))
		       ))
	  ediff-disturbed-overlays)
  (setq ediff-disturbed-overlays nil))
  
  
(defun ediff-save-buffer (arg)
  "Safe way of saving buffers A, B, C, and the diff output.
`wa' saves buffer A, `wb' saves buffer B, `wc' saves buffer C,
and `wd' saves the diff output."
  (interactive "P")
  (ediff-eval-in-buffer
      (cond ((memq last-command-char '(?a ?b ?c))
	     (ediff-get-buffer
	      (ediff-char-to-buftype last-command-char)))
	    ((eq last-command-char ?d)
	     (message "Saving diff output ...")(sit-for 1)
	     (if arg ediff-diff-buffer ediff-custom-diff-buffer)
	     ))
    (save-buffer)))
    

;; will simplify it in due time, when emacs acquires before/after strings
(defun ediff-remove-flags-from-buffer (buffer overlay)
  (ediff-eval-in-buffer buffer
    (let ((inhibit-read-only t))
      (if ediff-xemacs-p
	  (ediff-overlay-put overlay 'begin-glyph nil)
	;; before-string is not yet implemented in emacs.
	;; when it will be, I will be able to delete much of the rest of
	;; this function
	(ediff-overlay-put overlay 'before-string nil))
      
      (if ediff-xemacs-p
	  (ediff-overlay-put overlay 'end-glyph nil)
	;; after-string is not yet implemented in emacs.
	(ediff-overlay-put overlay 'after-string nil))
      )))



;; will simplify it in due time, when emacs acquires before/after strings
(defun ediff-place-flags-in-buffer (buf-type buffer ctl-buffer diff)
  (ediff-eval-in-buffer buffer
    (ediff-place-flags-in-buffer1 buf-type ctl-buffer diff)))


(defun ediff-place-flags-in-buffer1 (buf-type ctl-buffer diff-no)
  (let* ((curr-overl (ediff-eval-in-buffer ctl-buffer
		       (ediff-get-diff-overlay diff-no buf-type)))
	 (before (ediff-get-diff-posn buf-type 'beg diff-no ctl-buffer))
	 after beg-of-line flag)
	
    ;; insert flag before the difference
    (goto-char before)
    (setq beg-of-line (bolp))
    
    (setq flag (ediff-eval-in-buffer ctl-buffer
		 (if (eq ediff-highlighting-style 'ascii)
		     (if beg-of-line
			 ediff-before-flag-bol ediff-before-flag-mol))))
    
    ;; insert the flag itself
    (if ediff-xemacs-p
	(ediff-overlay-put curr-overl 'begin-glyph flag)
      (ediff-overlay-put curr-overl 'before-string flag))
    
    ;; insert the flag after the difference
    ;; `after' must be set here, after the before-flag was inserted
    (setq after (ediff-get-diff-posn buf-type 'end diff-no ctl-buffer))
    (goto-char after)
    (setq beg-of-line (bolp))
    
    (setq flag (ediff-eval-in-buffer ctl-buffer
		 (if (eq ediff-highlighting-style 'ascii)
		     (if beg-of-line
			 ediff-after-flag-eol ediff-after-flag-mol))))
    
    ;; insert the flag itself
    (if ediff-xemacs-p
	(ediff-overlay-put curr-overl 'end-glyph flag)
      (ediff-overlay-put curr-overl 'after-string flag))
    ))

  
(defun ediff-get-diff-posn (buf-type pos &optional n control-buf)
  "Returns positions of difference sectors in the BUF-TYPE buffer.
BUF-TYPE should be a symbol--either `A' or `B'. 
POS is either `beg' or `end'--it specifies whether you want the position at the
beginning of a difference or at the end.

The optional argument N says which difference \(default:
`ediff-current-difference'\).  The optional argument CONTROL-BUF says
which control buffer is in effect in case it is not the current
buffer."
  (let (diff-overlay)
    (or control-buf
	(setq control-buf (current-buffer)))

    (ediff-eval-in-buffer control-buf
      (or n  (setq n ediff-current-difference))
      (if (or (< n 0) (>= n ediff-number-of-differences))
	  (if (> ediff-number-of-differences 0)
	      (error "There is no diff %d. Valid diffs are 1 to %d"
		     (1+ n) ediff-number-of-differences)
	    (error "No differences found")))
      (setq diff-overlay (ediff-get-diff-overlay n buf-type)))
    
    (if (eq pos 'beg)
	(ediff-overlay-start diff-overlay)
      (ediff-overlay-end diff-overlay))
    ))



(defun ediff-highlight-diff-in-one-buffer (n buf-type)
  (if (ediff-buffer-live-p (ediff-get-buffer buf-type))
      (let* ((buff (ediff-get-buffer buf-type))
	     (last (ediff-eval-in-buffer buff (point-max)))
	     (begin (ediff-get-diff-posn buf-type 'beg n))
	     (end (ediff-get-diff-posn buf-type 'end n))
	     (xtra (if (equal begin end) 1 0))
	     (end-hilit (min last (+ end xtra)))
	     (current-diff-overlay 
	      (symbol-value
	       (intern (format "ediff-current-diff-overlay-%S" buf-type))))
	     (odd-diff-face
	      (symbol-value
	       (intern (format "ediff-odd-diff-face-%S" buf-type))))
	     (even-diff-face
	      (symbol-value
	       (intern (format "ediff-even-diff-face-%S" buf-type))))
	     (odd-diff-face-var
	      (intern (format "ediff-odd-diff-face-%S-var" buf-type)))
	     (even-diff-face-var
	      (intern (format "ediff-even-diff-face-%S-var" buf-type)))
	     )
	
	(if ediff-xemacs-p
	    (ediff-move-overlay current-diff-overlay begin end-hilit)
	  (ediff-move-overlay current-diff-overlay begin end-hilit buff))
	;; giving priority of 0 and then changing it may look funny, but
	;; this overcomes an obscure Emacs bug.
	(ediff-overlay-put current-diff-overlay 'priority  0)
	(ediff-overlay-put current-diff-overlay 'priority  
			   (ediff-highest-priority begin end-hilit buff))
	
	(or (face-differs-from-default-p odd-diff-face-var)
	    (not ediff-highlight-all-diffs)
	    (progn
	      (copy-face odd-diff-face odd-diff-face-var)
	      (copy-face even-diff-face even-diff-face-var)))
	
	;; unhighlight the background overlay for diff n so they won't
	;; interfere with the current diff overlay
	(ediff-overlay-put (ediff-get-diff-overlay n buf-type) 'face nil)
	)))


(defun ediff-unhighlight-diff-in-one-buffer (buf-type)
  (if (ediff-buffer-live-p (ediff-get-buffer buf-type))
      (let ((current-diff-overlay 
	     (symbol-value
	      (intern (format "ediff-current-diff-overlay-%S" buf-type))))
	    (odd-diff-face-var
	     (intern (format "ediff-odd-diff-face-%S-var" buf-type)))
	    (even-diff-face-var
	     (intern (format "ediff-even-diff-face-%S-var" buf-type)))
	    (overlay
	     (ediff-get-diff-overlay ediff-current-difference buf-type))
	    )
    
	(ediff-move-overlay current-diff-overlay 1 1)
	
	;; rehighlight the overlay in the background of the
	;; current difference region
	(ediff-overlay-put overlay
			   'face (if (ediff-odd-p ediff-current-difference)
				     odd-diff-face-var
				   even-diff-face-var))
	)))

(defun ediff-unhighlight-diffs-totally-in-one-buffer (buf-type)
  (ediff-unselect-and-select-difference -1)
  (if (and (ediff-window-display-p) ediff-use-faces)
      (let* ((inhibit-quit t)
	     (current-diff-overlay-var
	      (intern (format "ediff-current-diff-overlay-%S" buf-type)))
	     (current-diff-overlay (symbol-value current-diff-overlay-var))
	     (odd-diff-face-var
	      (intern (format "ediff-odd-diff-face-%S-var" buf-type)))
	     (even-diff-face-var
	      (intern (format "ediff-even-diff-face-%S-var" buf-type))))
	(if (face-differs-from-default-p odd-diff-face-var)
	    (progn
	      (copy-face 'default odd-diff-face-var)
	      (copy-face 'default even-diff-face-var)))
	(if (ediff-overlayp current-diff-overlay)
	    (ediff-delete-overlay current-diff-overlay))
	(set current-diff-overlay-var nil)
	)))

    
;; null out difference overlays so they won't slow down future
;; editing operations
;; VEC is either a difference vector or a fine-diff vector
(defun ediff-clear-diff-vector (vec-var &optional fine-diffs-also)
  (if (vectorp (symbol-value vec-var))
      (mapcar (function
	       (lambda (elt)
		 (ediff-delete-overlay 
		  (ediff-get-diff-overlay-from-diff-record elt))
		 (if fine-diffs-also
		     (ediff-clear-fine-diff-vector elt))
		 ))
	      (symbol-value vec-var)))
  ;; allow them to be garbage collected
  (set vec-var nil))
    
       

;;; Misc

;; These two functions are here to neutralize XEmacs unwillingless to
;; handle overlays whose buffers were deleted.
(defun ediff-move-overlay (overlay beg end &optional buffer)
  "Calls `move-overlay' in Emacs and `set-extent-endpoints' in Lemacs.
Checks if overlay's buffer exists before actually doing the move."
  (let ((buf (and overlay (ediff-overlay-buffer overlay))))
    (if (ediff-buffer-live-p buf)
	(if ediff-xemacs-p
	    (set-extent-endpoints overlay beg end)
	  (move-overlay overlay beg end buffer))
      ;; buffer's dead
      (if overlay
	  (ediff-delete-overlay overlay)))))
	  
(defun ediff-overlay-put (overlay prop value)
  "Calls `overlay-put' or `set-extent-property' depending on Emacs version.
Checks if overlay's buffer exists."
  (if (ediff-buffer-live-p (ediff-overlay-buffer overlay))
      (if ediff-xemacs-p
	  (set-extent-property overlay prop value)
	(overlay-put overlay prop value))
    (ediff-delete-overlay overlay)))
    

;; In Emacs, this just makes overlay. In the future, when Emacs will start
;; supporting sticky overlays, this function will make a sticky overlay.
;; BEG and END are expressions telling where overlay starts.
;; If they are numbers or buffers, then all is well. Otherwise, they must
;; be expressions to be evaluated in buffer BUF in order to get the overlay
;; bounds.
;; If BUFF is not a live buffer, then return nil; otherwise, return the
;; newly created overlay.
(defun ediff-make-bullet-proof-overlay (beg end buff)
  (if (ediff-buffer-live-p buff)
      (let (overl)
	(ediff-eval-in-buffer buff
	  (or (number-or-marker-p beg)
	      (setq beg (eval beg)))
	  (or (number-or-marker-p end)
	      (setq end (eval end)))
	  (setq overl (ediff-make-overlay beg end buff))
	  
	  (if ediff-xemacs-p
	      (progn
		;; take precautions against detached extents
		(ediff-overlay-put overl 'detachable nil)
		;; chars inserted at both ends will be inside extent
		(ediff-overlay-put overl 'start-open nil)
		(ediff-overlay-put overl 'end-open nil))
	    (ediff-overlay-put overl 'ediff-diff-num 0)
	    ;;(ediff-overlay-put overl 'insert-in-front-hooks
	    ;;		         (list 'ediff-capture-inserts-in-front))
	    ;;(ediff-overlay-put overl 'insert-behind-hooks
	    ;;  	         (list 'ediff-capture-inserts-behind))
	    ;; These two are not yet implemented in Emacs
	    ;;(ediff-overlay-put overl 'rear-sticky t)
	    ;;(ediff-overlay-put overl 'front-sticky t)
	    )
	  overl))))
	  
(defsubst ediff-overlay-start (overl)
  (if (ediff-overlayp overl)
      (if ediff-emacs-p
	  (overlay-start overl)
	(extent-start-position overl))))
	
(defsubst ediff-overlay-end  (overl)
  (if (ediff-overlayp overl)
      (if ediff-emacs-p
	  (overlay-end overl)
	(extent-end-position overl))))

  
;; Like other-buffer, but prefers visible buffers and ignores temporary or
;; other insignificant buffers (those beginning with "^[ *]").
;; Gets one arg--buffer name or a list of buffer names (it won't return
;; these buffers).
(defun ediff-other-buffer (buff)
  (if (not (listp buff)) (setq buff (list buff)))
  (let* ((frame-buffers (buffer-list))
	 (significant-buffers
	  (mapcar
	   (function (lambda (x)
		       (cond ((member (buffer-name x) buff)
			      nil)
			     ((not (ediff-get-visible-buffer-window x))
			      nil)
			     ((string-match "^ " (buffer-name x))
			      nil)
			     (t x))))
	   frame-buffers))
	 (buffers (delq nil significant-buffers))
	 less-significant-buffers)
	 
    (cond (buffers (car buffers))
	  ;; try also buffers that are not displayed in windows
	  ((setq less-significant-buffers
		 (delq nil
		       (mapcar
			(function
			 (lambda (x)
			   (cond ((member (buffer-name x) buff) nil)
				 ((string-match "^[ *]" (buffer-name x)) nil)
				 (t x))))
			frame-buffers)))
	   (car less-significant-buffers))
	  (t (other-buffer (current-buffer))))
    ))
      
      
;; Construct a unique buffer name.
;; The first one tried is prefixsuffix, then prefix<2>suffix, 
;; prefix<3>suffix, etc.
(defun ediff-unique-buffer-name (prefix suffix)
  (if (null (get-buffer (concat prefix suffix)))
      (concat prefix suffix)
    (let ((n 2))
      (while (get-buffer (format "%s<%d>%s" prefix n suffix))
	(setq n (1+ n)))
      (format "%s<%d>%s" prefix n suffix))))
  

(defun ediff-submit-report ()
  "Submit bug report on Ediff."
  (interactive)
  (let ((reporter-prompt-for-summary-p t)
	(ctl-buf ediff-control-buffer)
	(ediff-device-type (ediff-device-type))
	varlist salutation buffer-name)
    (setq varlist '(ediff-diff-program ediff-diff-options
		    ediff-patch-program ediff-patch-options
		    ediff-shell
		    ediff-use-faces 
		    ediff-auto-refine ediff-highlighting-style
		    ediff-buffer-A ediff-buffer-B ediff-control-buffer
		    ediff-forward-word-function
		    ediff-control-frame
		    ediff-control-frame-parameters
		    ediff-control-frame-position-function
		    ediff-prefer-iconified-control-frame
		    ediff-window-setup-function
		    ediff-split-window-function
		    ediff-job-name
		    ediff-word-mode
		    buffer-name
		    ediff-device-type
		    ))
    (setq salutation "
Congratulations! You may have unearthed a bug in Ediff!

Please make a concise and accurate summary of what happened
and mail it to the address above.
-----------------------------------------------------------
")
    
    (ediff-skip-unsuitable-frames)
    (ediff-reset-mouse)
    
    (switch-to-buffer ediff-msg-buffer)
    (erase-buffer)
    (delete-other-windows)
    (insert "
Please read this first:
----------------------

Some ``bugs'' may actually be no bugs at all. For instance, if you are
reporting that certain difference regions are not matched as you think they
should, this is most likely due to the way Unix diff program decides what
constitutes a difference region. Ediff is an Emacs interface to diff, and
it has nothing to do with those decisions---it only takes the output from
diff and presents it in a way that is better suited for human browsing and
manipulation.

If Emacs happens to dump core, this is NOT an Ediff problem---it is
an Emacs bug. Report this to Emacs maintainers.

Another popular topic for reports is compilation messages.  Because Ediff
interfaces to several other packages and runs under Emacs and XEmacs,
byte-compilation may produce output like this:

       While compiling toplevel forms in file ediff.el:
	 ** reference to free variable pm-color-alist
	   ........................
       While compiling the end of the data:
	 ** The following functions are not known to be defined: 
	   ediff-valid-color-p, ediff-set-face,
	   ........................

These are NOT errors, but inevitable warnings, which ought to be ignored.

Please do not report those and similar things.  However, comments and
suggestions are always welcome.

Mail anyway? (y or n) ")
      
    (if (y-or-n-p "Mail anyway? ")
	(progn
	  (if (ediff-buffer-live-p ctl-buf)
	      (set-buffer ctl-buf))
	  (setq buffer-name (buffer-name))
	  (require 'reporter)
	  (reporter-submit-bug-report "kifer@cs.sunysb.edu"
				      (ediff-version)
				      varlist
				      nil 
				      'delete-other-windows
				      salutation))
      (bury-buffer)
      (beep 1)(message "Bug report aborted")
      (if (ediff-buffer-live-p ctl-buf)
	  (ediff-eval-in-buffer ctl-buf
	    (ediff-recenter 'no-rehighlight))))
    ))
			     
       
(defun ediff-union (list1 list2)
  "Combine LIST1 and LIST2 using a set-union operation.
The result list contains all items that appear in either LIST1 or LIST2.
This is a non-destructive function; it makes a copy of the data if necessary
to avoid corrupting the original LIST1 and LIST2.
This is a slightly simplified version from `cl-seq.el'.  Added here to
avoid loading cl-*."
  (cond ((null list1) list2) ((null list2) list1)
	((equal list1 list2) list1)
	(t
	 (or (>= (length list1) (length list2))
	     (setq list1 (prog1 list2 (setq list2 list1))))
	 (while list2
	   (or (memq (car list2) list1)
	       (setq list1 (cons (car list2) list1)))
	   (setq list2 (cdr list2)))
	 list1)))
	 
(defun ediff-deactivate-mark ()
  (if ediff-xemacs-p
	  (zmacs-deactivate-region)
	(deactivate-mark)))

(cond ((fboundp 'nuke-selective-display)
       ;; XEmacs 19.12 has nuke-selective-display
       (fset 'ediff-nuke-selective-display 'nuke-selective-display))
      (t
       (defun ediff-nuke-selective-display ()
	 (save-excursion
	   (save-restriction
	     (widen)
	     (goto-char (point-min))
	     (let ((mod-p (buffer-modified-p))
		   buffer-read-only end)
	       (and (eq t selective-display)
		    (while (search-forward "\^M" nil t)
		      (end-of-line)
		      (setq end (point))
		      (beginning-of-line)
		      (while (search-forward "\^M" end t)
			(delete-char -1)
			(insert "\^J"))))
	       (set-buffer-modified-p mod-p)
	       (setq selective-display nil)))))
       ))

	 
;; The next two are modified versions from emerge.el.
;; VARS must be a list of symbols
;; ediff-save-variables returns an association list: ((var . val) ...)
(defsubst ediff-save-variables (vars)
  (mapcar (function (lambda (v) (cons v (symbol-value v))))
	  vars))
;; VARS is a list of variable symbols.
(defun ediff-restore-variables (vars assoc-list)
  (while vars
    (set (car vars) (cdr (assoc (car vars) assoc-list)))
    (setq vars (cdr vars))))
(defun ediff-change-saved-variable (var value buf-type)
  (let* ((assoc-list
	  (symbol-value (intern
			 (concat "ediff-buffer-values-orig-"
				 (symbol-name buf-type)))))
	 (assoc-elt (assoc var assoc-list)))
  (if assoc-elt
      (setcdr assoc-elt value))))
  
  
;; must execute in control buf
(defun ediff-save-protected-variables ()
  (setq ediff-buffer-values-orig-A
	(ediff-eval-in-buffer ediff-buffer-A
	  (ediff-save-variables ediff-protected-variables)))
  (setq ediff-buffer-values-orig-B
	(ediff-eval-in-buffer ediff-buffer-B
	  (ediff-save-variables ediff-protected-variables)))
  (if ediff-3way-comparison-job
      (setq ediff-buffer-values-orig-C
	    (ediff-eval-in-buffer ediff-buffer-C
	      (ediff-save-variables ediff-protected-variables)))))

;; must execute in control buf
(defun ediff-restore-protected-variables ()
  (let ((values-A ediff-buffer-values-orig-A)
	(values-B ediff-buffer-values-orig-B)
	(values-C ediff-buffer-values-orig-C))
  (ediff-eval-in-buffer ediff-buffer-A
    (ediff-restore-variables ediff-protected-variables values-A))
  (ediff-eval-in-buffer ediff-buffer-B
    (ediff-restore-variables ediff-protected-variables values-B))
  (if ediff-3way-comparison-job
      (ediff-eval-in-buffer ediff-buffer-C
	(ediff-restore-variables ediff-protected-variables values-C)))))


;;; Debug

(ediff-defvar-local ediff-command-begin-time '(0 0 0) "")

;; calculate time used by command
(defun ediff-calc-command-time ()
  (let ((end (current-time))
	micro sec)
    (setq micro
	  (if (>= (nth 2 end) (nth 2 ediff-command-begin-time))
	      (- (nth 2 end) (nth 2 ediff-command-begin-time))
	    (+ (nth 2 end) (- 1000000 (nth 2 ediff-command-begin-time)))))
    (setq sec (- (nth 1 end) (nth 1 ediff-command-begin-time)))
    (or (equal ediff-command-begin-time '(0 0 0))
	(message "Elapsed time: %d second(s) + %d microsecond(s)" sec micro))))

(defsubst ediff-save-time ()
  (setq ediff-command-begin-time (current-time)))
    
(defun ediff-profile ()
  "Toggle profiling Ediff commands."
  (interactive)
  (or (ediff-buffer-live-p ediff-control-buffer)
      (error "This command runs only out of Ediff Control Buffer"))
  (make-local-variable 'pre-command-hook)
  (make-local-variable 'post-command-hook)
  (if (memq 'ediff-save-time pre-command-hook)
      (progn (remove-hook 'pre-command-hook 'ediff-save-time)
	     (remove-hook 'post-command-hook 'ediff-calc-command-time)
	     (setq ediff-command-begin-time '(0 0 0))
	     (message "Ediff profiling disabled"))
    (add-hook 'pre-command-hook 'ediff-save-time t)
    (add-hook 'post-command-hook 'ediff-calc-command-time)
    (message "Ediff profiling enabled")))
    
(defun ediff-print-diff-vector (diff-vector-var)
  (princ (format "\n*** %S ***\n" diff-vector-var))
  (mapcar (function
	   (lambda (overl-vec)
	     (princ
	      (format
	       "Diff %d: \tOverlay:    %S
\t\tFine diffs: %s
\t\tNo-fine-diff-flag: %S
\t\tState-of-diff:\t   %S
\t\tState-of-merge:\t   %S
" 
	       (1+ (ediff-overlay-get (aref overl-vec 0) 'ediff-diff-num))
	       (aref overl-vec 0)
	       ;; fine-diff-vector
	       (if (= (length (aref overl-vec 1)) 0)
		   "none\n"
		 (mapconcat 'prin1-to-string
			    (aref overl-vec 1) "\n\t\t\t    "))
	       (aref overl-vec 2) ; no fine diff flag
	       (aref overl-vec 3) ; state-of-diff
	       (aref overl-vec 4) ; state-of-merge
	       ))))
	  (eval diff-vector-var)))

  

(defun ediff-debug-info ()
  (interactive)
  (or (ediff-buffer-live-p ediff-control-buffer)
      (error "This command runs only out of Ediff Control Buffer"))
  (with-output-to-temp-buffer ediff-debug-buffer
    (princ (format "\nCtl buffer: %S\n" ediff-control-buffer))
    (ediff-print-diff-vector (intern (concat "ediff-difference-vector-" "A")))
    (ediff-print-diff-vector (intern (concat "ediff-difference-vector-" "B")))
    (ediff-print-diff-vector (intern (concat "ediff-difference-vector-" "C")))
    ))

;; don't report error if version control package wasn't found
;;(ediff-load-version-control 'silent)

(run-hooks 'ediff-load-hooks)
    

;;; Local Variables:
;;; eval: (put 'ediff-defvar-local 'lisp-indent-hook 'defun)
;;; eval: (put 'ediff-eval-in-buffer 'lisp-indent-hook 1)
;;; End:

(provide 'ediff-util)

;;; ediff-util.el ends here
