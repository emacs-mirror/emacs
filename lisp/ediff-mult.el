;;; ediff-meta.el --- support for multi-file/multi-buffer processing in Ediff
;;; Copyright (C) 1995 Free Software Foundation, Inc.

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

;;; Notes:
;;
;; Users are strongly encourage to add functionality to this file.
;; In particular, epatch needs to be enhanced to work with multi-file
;; patches. The present file contains all the infrastructure needed for that.
;;
;; Generally, to to implement a new multisession capability within Ediff,
;; you need to tell it 
;;
;;	1. How to display the session group buffer.
;;	   This function must indicate which Ediff sessions are active (+) and
;;	   which are finished (-).
;;	   See ediff-redraw-directory-group-buffer for an example.
;;	   In all likelihood, ediff-redraw-directory-group-buffer can be used
;;	   directly or after a small modification.
;;	2. What action to take when the user clicks button 2 or types v,e, or
;;	   RET.  See ediff-dir-action.
;;	3. Provide a list of pairs or triples of file names (or buffers,
;;	   depending on the particular Ediff operation you want to invoke)
;;	   in the following format:
;;	  	((obj1 obj2 [optional obj3]) (...) ...)
;;	   Actually, the format of this list is pretty much up to the
;;	   developer. The only thing is that it must be a list of lists.
;;	   Also, keep in mind that the function ediff-prepare-meta-buffer
;;	   (which see) prepends nil in fron of each list (i.e., the above list
;;	   will become ((nil obj1 obj2 ...) (nil ...) ...).
;;	   Ediff expects that your function (in 2 above) will arrange to
;;	   replace this prepended nil (via setcar) with the actual ediff
;;	   control buffer associated with an appropriate Ediff session.
;;	   This is arranged through internal startup hooks that can be passed
;;	   to any of Ediff major entries (such as ediff-files, epatch, etc.).
;;	   See how this is done in ediff-dir-action.
;;	4. Write a function that makes a call to ediff-prepare-meta-buffer
;;	   passing all this info. 
;;	   You may be able to use ediff-directories-internal as a template.
;;	5. If you intend to add several related pieces of functionality,
;;	   you may want to keep the function in 4 as an internal version
;;	   and then write several top-level interactive functions that call it
;;	   with different parameters.
;;	   See how ediff-directories, ediff-merge-directories, and
;;	   ediff-merge-directories-with-ancestor all use
;;	   ediff-directories-internal. 
;;
;; In case of multifile patching, the easiest thing is to first apply the patch
;; and then find out which files were patched (using the algorithm utilized by
;; Unix patch and by parsing the patch file). The procedure ediff-patch-file
;; works for single-file patches only. However, it can deal with remote and
;; compressed files. Check out ediff-patch-file for details.
;;
;; Another useful addition here could be session groups selected by patterns
;; (which are different in each directory). For instance, one may want to
;; compare files of the form abc{something}.c to files old{something}.d
;; which may be in the same or different directories. Or, one may want to
;; compare all files of the form {something} to files of the form {something}~.
;;
;; Implementing this require writing an collating function, which would pair up
;; appropriate files. It will also require a generalization of the functions
;; that do the layout of the meta- and differences buffers and of
;; ediff-dir-action.

(require 'ediff-init)

;; meta-buffer
(ediff-defvar-local ediff-meta-buffer nil "")
(ediff-defvar-local ediff-parent-meta-buffer nil "")
;; the registry buffer
(defvar ediff-registry-buffer nil)

(defconst ediff-meta-buffer-message "This is an Ediff Session Group Panel: %s

Useful commands:
     button2, `v', RET over a session line:   start that Ediff session
     `M' in any session invoked from here:    bring back this buffer
     `R':\tdisplay the registry of active Ediff sessions
     `h':\tmark session for hiding; with prefix arg--unmark
     `x':\thide marked sessions; with prefix arg--unhide hidden sessions
     `m':\tmark session for non-hiding operation; with prefix arg--unmark
     SPC:\tnext session
     DEL:\tprevious session
     `E':\tbrowse Ediff on-line manual
     `q':\tquit this session group
")

(ediff-defvar-local ediff-meta-buffer-map nil
  "The keymap for the meta buffer.")
(defvar ediff-dir-diffs-buffer-map (make-sparse-keymap)
  "The keymap to be installed in the buffer showing differences between
directories.")

;; Variable specifying the action to take when the use invokes ediff in the
;; meta buffer. This is usually ediff-registry-action or ediff-dir-action
(ediff-defvar-local ediff-meta-action-function nil "")
;; Tells ediff-update-meta-buffer how to redraw it
(ediff-defvar-local ediff-meta-redraw-function nil "")
;; Tells ediff-dir-action and similar procedures how to invoke Ediff for the
;; sessions in a given session group
(ediff-defvar-local ediff-session-action-function nil "")

(ediff-defvar-local ediff-metajob-name nil "")

;; buffer used to collect custom diffs from individual sessions in the group
(ediff-defvar-local ediff-meta-diff-buffer nil "")

;; history var to use for filtering groups
(defvar ediff-filtering-regexp-history nil "")

;; This has the form ((ctl-buf file1 file2) (stl-buf file1 file2) ...)
;; If ctl-buf is nil, the file-pare wasn't processed yet. If it is
;; killed-buffer object, the file pair has been processed. If it is a live
;; buffer, this means ediff is still working on the pair
(ediff-defvar-local ediff-meta-list nil "")


;; the difference list between directories in a directory session group
(ediff-defvar-local ediff-dir-difference-list nil "")
(ediff-defvar-local ediff-dir-diffs-buffer nil "")

;; The registry of Ediff sessions. A list of control buffers.
(defvar ediff-session-registry nil)

(defvar ediff-registry-setup-hook nil
  "*Hooks run just after the registry control panel is set up.")
(defvar ediff-session-group-setup-hook nil
  "*Hooks run just after a meta-buffer controlling a session group, such as
ediff-directories, is run.")
(defvar ediff-show-registry-hook nil
  "*Hooks run just after the registry buffer is shown.")
(defvar ediff-show-session-group-hook nil
  "*Hooks run just after a session group buffer is shown.")

;;; API

(defun ediff-get-group-buffer (meta-list)
  (nth 0 (car meta-list)))
(defun ediff-get-group-regexp (meta-list)
  (nth 1 (car meta-list)))
(defun ediff-get-group-objA (meta-list)
  (nth 2 (car meta-list)))
(defun ediff-get-group-objB (meta-list)
  (nth 3 (car meta-list)))
(defun ediff-get-group-objC (meta-list)
  (nth 4 (car meta-list)))
(defun ediff-get-session-buffer (elt)
  (nth 0 elt))
(defun ediff-get-session-status (elt)
  (nth 1 elt))
(defun ediff-get-session-objA (elt)
  (nth 2 elt))
(defun ediff-get-session-objB (elt)
  (nth 3 elt))
(defun ediff-get-session-objC (elt)
  (nth 4 elt))
(defun ediff-set-session-status (session-info new-status)
  (setcar (cdr session-info) new-status))

;; set up the keymap in the meta buffer
(defun ediff-setup-meta-map()
  (setq ediff-meta-buffer-map (make-sparse-keymap))
  (suppress-keymap ediff-meta-buffer-map)
  (define-key ediff-meta-buffer-map "q" 'ediff-quit-meta-buffer)
  (define-key ediff-meta-buffer-map "R" 'ediff-show-registry)
  (define-key ediff-meta-buffer-map "E" 'ediff-documentation)
  (define-key ediff-meta-buffer-map "v" ediff-meta-action-function)
  (define-key ediff-meta-buffer-map "\C-m" ediff-meta-action-function)
  (define-key ediff-meta-buffer-map  " "  'ediff-next-meta-item)
  (define-key ediff-meta-buffer-map  "\C-?"  'ediff-previous-meta-item)
  (define-key ediff-meta-buffer-map  [delete]  'ediff-previous-meta-item)
  (define-key ediff-meta-buffer-map  [backspace]  'ediff-previous-meta-item)
  (if ediff-no-emacs-help-in-control-buffer
      (define-key ediff-meta-buffer-map  "\C-h"  'ediff-previous-meta-item))
  (if ediff-emacs-p
      (define-key ediff-meta-buffer-map [mouse-2] ediff-meta-action-function)
    (define-key ediff-meta-buffer-map [button2] ediff-meta-action-function))

  (use-local-map ediff-meta-buffer-map))

(defun ediff-meta-mode ()
  "This mode controls all operations on Ediff session groups.
It is entered through one of the following commands:
	`ediff-directories'
	`edirs'
	`ediff-directories3'
	`edirs3'
	`ediff-merge-directories'
	`edirs-merge'
	`ediff-merge-directories-with-ancestor'
	`edirs-merge-with-ancestor'
	`ediff-directory-revisions'
	`edir-revisions'
	`ediff-merge-directory-revisions'
	`edir-merge-revisions'
	`ediff-merge-directory-revisions-with-ancestor'
	`edir-merge-revisions-with-ancestor'

Commands:
\\{ediff-meta-buffer-map}"
  (kill-all-local-variables)
  (setq major-mode 'ediff-meta-mode)
  (setq mode-name "MetaEdiff"))


;; the keymap for the buffer showing directory differences
(suppress-keymap ediff-dir-diffs-buffer-map)
(define-key ediff-dir-diffs-buffer-map "q" 'ediff-bury-dir-diffs-buffer)
(define-key ediff-dir-diffs-buffer-map " " 'next-line)
(define-key ediff-dir-diffs-buffer-map "\C-?" 'previous-line)
(define-key ediff-dir-diffs-buffer-map [delete] 'previous-line)
(define-key ediff-dir-diffs-buffer-map [backspace] 'previous-line)

(defun ediff-next-meta-item (count)
  "Move to the next item in Ediff registry or session group buffer.
Moves in circular fashion. With numeric prefix arg, skip this many items."
  (interactive "p")
  (or count (setq count 1))
  (while (< 0 count)
    (setq count (1- count))
    (ediff-next-meta-item1)))

;; Move to the next meta item
(defun ediff-next-meta-item1 ()
  (let (pos)
    (setq pos (ediff-next-meta-overlay-start (point)))
;;;	;; skip deleted
;;;    (while (memq (ediff-get-session-status
;;;	          (ediff-get-meta-info (current-buffer) pos 'noerror))
;;;		 '(?H ?I))
;;;      (setq pos (ediff-next-meta-overlay-start pos)))
    
    (if pos (goto-char pos))
    (if (eq ediff-metajob-name 'ediff-registry)
	(if (search-forward "*Ediff" nil t)
	    (skip-chars-backward "a-zA-Z*"))
      (if (> (skip-chars-forward "-+?H* \t0-9") 0)
	  (backward-char 1)))))


(defun ediff-previous-meta-item (count)
  "Move to the previous item in Ediff registry or session group buffer.
Moves in circular fashion. With numeric prefix arg, skip this many items."
  (interactive "p")
  (or count (setq count 1))
  (while (< 0 count)
    (setq count (1- count))
    (ediff-previous-meta-item1)))

(defun ediff-previous-meta-item1 ()
  (let (pos)
    (setq pos (ediff-previous-meta-overlay-start (point)))
;;;	;; skip deleted
;;;    (while (ediff-get-session-status
;;;	    (ediff-get-meta-info (current-buffer) pos 'noerror))
;;;      (setq pos (ediff-previous-meta-overlay-start pos)))
    
    (if pos (goto-char pos))
    (if (eq ediff-metajob-name 'ediff-registry)
	(if (search-forward "*Ediff" nil t)
	    (skip-chars-backward "a-zA-Z*"))
      (if (> (skip-chars-forward "-+?H* \t0-9") 0)
	  (backward-char 1)))))



;; DIR1, DIR2, DIR3 are directories.
;; REGEXP is a regexp used to filter
;; files in the directories.
;; If a file is a directory in dir1 but not dir2 (or vice versa), it is not
;; included in the intersection. However, a regular file that is a dir in dir3
;; is included, since dir3 files are supposed to be ancestors for merging.
;; Returns a list of the form:
;;	((dir1 dir2 dir3) (f1 f2 f3) (f1 f2 f3) ...)
;; dir3, f3 can be nil if intersecting only 2 directories.
;; If COMPARISON-FUNC is given, use it. Otherwise, use string=
;; DIFF-VAR is contains the name of the variable in which to return the
;; difference list. The diff list is of the form:
;;	((dir1 dir2 dir3) (file . num) (file . num)...)
;; where num encodes the set of dirs where the file is found:
;; 2 - only dir1; 3 - only dir2; 5 - only dir3; 6 - dir1&2; 10 - dir1&3; etc.
(defun ediff-intersect-directories (jobname diff-var regexp dir1 dir2
					    &optional dir3 comparison-func)
  (require 'cl)
  (setq comparison-func (or comparison-func 'string=))
  (let (lis1 lis2 lis3 common auxdir1 auxdir2 auxdir3 difflist)

    (setq auxdir1	(file-name-as-directory dir1)
	  lis1		(directory-files auxdir1 nil regexp)
	  auxdir2	(file-name-as-directory dir2)
	  lis2		(directory-files auxdir2 nil regexp))

    (if (stringp dir3)
	(setq auxdir3	(file-name-as-directory dir3)
	      lis3	(directory-files auxdir3 nil regexp)))

    (setq lis1 (delete "."  lis1)
	  lis1 (delete ".." lis1))

    (setq common (intersection lis1 lis2 :test comparison-func))
    ;; get rid of files that are directories in dir1 but not dir2
    (mapcar (function (lambda (elt)
			(if (Xor (file-directory-p (concat auxdir1 elt))
				 (file-directory-p (concat auxdir2 elt)))
			    (setq common (delq elt common)))))
	    common)
    ;; intersect with the third dir
    (if lis3 (setq common (intersection common lis3 :test comparison-func)))
    (if (ediff-comparison-metajob3 jobname)
	(mapcar (function (lambda (elt)
			    (if (Xor (file-directory-p (concat auxdir1 elt))
				     (file-directory-p (concat auxdir3 elt)))
				(setq common (delq elt common)))))
		common))

    ;; trying to avoid side effects of sorting
    (setq common (sort (copy-list common) 'string-lessp))

    ;; compute difference list
    (setq difflist (set-difference
		    (union (union lis1 lis2 :test comparison-func)
			   lis3
			   :test comparison-func)
		    common
		    :test comparison-func)
	  difflist (delete "."  difflist)
	  ;; copy-list needed because sort sorts it by side effects
	  difflist (sort (copy-list (delete ".." difflist)) 'string-lessp))

    (setq difflist (mapcar (function (lambda (elt) (cons elt 1))) difflist))

    ;; check for files belonging to lis1/2/3
    (mapcar (function (lambda (elt) 
			(if (member (car elt) lis1)
			    (setcdr elt (* (cdr elt) 2)))
			(if (member (car elt) lis2)
			    (setcdr elt (* (cdr elt) 3)))
			(if (member (car elt) lis3)
			    (setcdr elt (* (cdr elt) 5)))
			))
	    difflist)
    (setq difflist (cons (list regexp auxdir1 auxdir2 auxdir3) difflist))
    
    (set diff-var difflist)

    ;; return result
    (cons (list regexp auxdir1 auxdir2 auxdir3)
	  (mapcar (function (lambda (elt) 
			      (list (concat auxdir1 elt)
				    (concat auxdir2 elt)
				    (if lis3
					(concat auxdir3 elt)))))
		  common))
    ))

;; find directory files that are under revision.
;; display subdirectories, too, since we may visit them recursively.
(defun ediff-get-directory-files-under-revision (jobname regexp dir1)
  (require 'cl)
  (let (lis1 elt common auxdir1)
    (setq auxdir1 (file-name-as-directory dir1)
	  lis1	  (directory-files auxdir1 nil regexp))

    (while lis1
      (setq elt  (car lis1)
	    lis1 (cdr lis1))
      ;; take files under revision control
      (cond ((file-directory-p (concat auxdir1 elt))
	     (setq common (cons elt common)))
	    ((file-exists-p (concat auxdir1 elt ",v"))
	     (setq common (cons elt common))))
      ) ; while

    (setq common (delete "."  common)
	  common (delete ".." common))

    ;; trying to avoid side effects of sorting
    (setq common (sort (copy-list common) 'string-lessp))

    ;; return result
    (cons (list regexp auxdir1 nil nil)
	  (mapcar (function (lambda (elt) 
			      (list (concat auxdir1 elt)
				    nil nil)))
		  common))
    ))
      

;; If file groups selected by patterns will ever be implemented, this
;; comparison function might become useful.
;;;; uses external variables PAT1 PAT2 to compare str1/2
;;;; patterns must be of the form ???*???? where ??? are strings of chars
;;;; containing no *.
;;(defun ediff-pattern= (str1 str2)
;;  (let (pos11 pos12 pos21 pos22 len1 len2)
;;    (setq pos11 0
;;	  len  (length epat1)
;;	  pos12 len)
;;    (while (and (< pos11 len) (not (= (aref epat1 pos11) ?*)))
;;      (setq pos11 (1+ pos11)))
;;    (while (and (> pos12 0) (not (= (aref epat1 (1- pos12)) ?*)))
;;      (setq pos12 (1- pos12)))
;;
;;    (setq pos21 0
;;	  len  (length epat2)
;;	  pos22 len)
;;    (while (and (< pos21 len) (not (= (aref epat2 pos21) ?*)))
;;      (setq pos21 (1+ pos21)))
;;    (while (and (> pos22 0) (not (= (aref epat2 (1- pos22)) ?*)))
;;      (setq pos22 (1- pos22)))
;;
;;    (if (and (> (length str1) pos12) (>= pos12 pos11) (> pos11 -1)
;;	     (> (length str2) pos22) (>= pos22 pos21) (> pos21 -1))
;;	(string= (substring str1 pos11 pos12)
;;		 (substring str2 pos21 pos22)))
;;    ))


;; Prepare meta-buffer in accordance with the argument-function and
;; redraw-function. Must return the created  meta-buffer.
(defun ediff-prepare-meta-buffer (action-func meta-list
				  meta-buffer-name redraw-function
				  jobname &optional startup-hooks)
  (let* ((meta-buffer-name 
	  (ediff-unique-buffer-name meta-buffer-name "*"))
	 (meta-buffer (get-buffer-create meta-buffer-name)))
    (ediff-eval-in-buffer meta-buffer

      ;; comes first
      (ediff-meta-mode)

      (setq ediff-meta-action-function action-func
	    ediff-meta-redraw-function redraw-function
	    ediff-metajob-name jobname
	    ediff-meta-buffer meta-buffer)

      ;; comes after ediff-meta-action-function is set
      (ediff-setup-meta-map)
      
      (if (eq ediff-metajob-name 'ediff-registry)
	  (progn
	    (setq ediff-registry-buffer meta-buffer
		  ediff-meta-list meta-list)
	    ;; this func is used only from registry buffer, not from other
	    ;; meta-buffs.
	    (define-key
	      ediff-meta-buffer-map "M" 'ediff-show-meta-buff-from-registry))
	;; initialize the meta list -- don't do this for registry we prepend
	;; '(nil nil) nil to all elts of meta-list, except the first.  The
	;; first nil will later be replaced by the session buffer. The second
	;; is reserved for session status.
	;; (car ediff-meta-list) gets cons'ed with the session group buffer.
	(setq ediff-meta-list
	      (cons (cons meta-buffer (car meta-list))
		    (mapcar (function
			     (lambda (elt)
			       (cons nil (cons nil elt))))
			    (cdr meta-list)))))
	
      (or (eq meta-buffer ediff-registry-buffer)
	  (setq ediff-session-registry
		(cons meta-buffer ediff-session-registry)))
	
      ;; redraw-function uses ediff-meta-list
      (funcall redraw-function ediff-meta-list)
      
      ;; set read-only/non-modified
      (setq buffer-read-only t)
      (set-buffer-modified-p nil)

      (run-hooks 'startup-hooks)
      ;; arrange for showing directory contents differences
      ;; must be after run startup-hooks, since ediff-dir-difference-list is
      ;; set inside these hooks
      (if (eq action-func 'ediff-dir-action)
	  (progn
	    ;; put meta buffer in (car ediff-dir-difference-list)
	    (setq ediff-dir-difference-list
		  (cons (cons meta-buffer (car ediff-dir-difference-list))
			(cdr ediff-dir-difference-list)))

	    (or (ediff-dir1-metajob jobname)
		(ediff-draw-dir-diffs ediff-dir-difference-list))
	    (define-key ediff-meta-buffer-map "h" 'ediff-mark-for-hiding)
	    (define-key
	      ediff-meta-buffer-map "x" 'ediff-hide-marked-sessions)
	    (define-key ediff-meta-buffer-map "m" 'ediff-mark-for-operation)
	    (if (ediff-collect-diffs-metajob jobname)
		(define-key
		  ediff-meta-buffer-map "P" 'ediff-collect-custom-diffs))
	    (define-key ediff-meta-buffer-map "u" 'ediff-up-meta-hierarchy)
	    (define-key ediff-meta-buffer-map "D" 'ediff-show-dir-diffs)))

      (if (eq ediff-metajob-name 'ediff-registry)
	  (run-hooks 'ediff-registry-setup-hook)
	(run-hooks 'ediff-session-group-setup-hook))
      ) ; eval in meta-buffer
    meta-buffer))


;; this is a setup function for ediff-directories
;; must return meta-buffer
(defun ediff-redraw-directory-group-buffer (meta-list)
  ;; extract directories
  (let ((meta-buf (ediff-get-group-buffer meta-list))
	(empty t)
	(sessionNum 0)
	regexp elt session-buf f1 f2 f3 pt 
	point tmp-list buffer-read-only)
    (ediff-eval-in-buffer meta-buf
      (setq point (point))
      (erase-buffer)
      (insert (format ediff-meta-buffer-message
		      (ediff-abbrev-jobname ediff-metajob-name)))

      (setq regexp (ediff-get-group-regexp meta-list))
      
      (if (ediff-collect-diffs-metajob)
	  (insert
	   "     `P':\tcollect custom diffs of all marked sessions\n"))
      (insert
       "     `u':\tshow parent session group
     `D':\tdisplay differences among the contents of directories\n\n")

      (if (and (stringp regexp) (> (length regexp) 0))
	  (insert (format "Filter-through regular expression: %s\n" regexp)))
			  
      (insert "\n
        Size  Name
    -----------------------------------------------------------------------

")
      
      ;; discard info on directories and regexp
      (setq meta-list (cdr meta-list)
	    tmp-list meta-list)
      (while (and tmp-list empty)
	(if (and (car tmp-list)
		 (not (eq (ediff-get-session-status (car tmp-list)) ?I)))
	    (setq empty nil))
	(setq tmp-list (cdr tmp-list)))

      (if empty
	  (insert
	   "     ******   ******   This session group has no members\n"))
      
      ;; now organize file names like this:
      ;; preferred format:
      ;;     use-mark sizeA dateA  sizeB dateB  filename
      ;; I don't have time to mess up with calculating last modtimes
      ;; (XEmacs has no decode-time function), so
      ;; the actual format is:
      ;;     use-mark Size    filename
      ;; make sure directories are displayed with a trailing slash.
      ;; If one is a directory and another isn't, indicate this with a `?'
      (while meta-list
	(setq elt (car meta-list)
	      meta-list (cdr meta-list)
	      sessionNum (1+ sessionNum))
	(if (eq (ediff-get-session-status elt) ?I)
	    ()
	  (setq session-buf (ediff-get-session-buffer elt)
		f1 (ediff-get-session-objA elt)
		f2 (ediff-get-session-objB elt)
		f3 (ediff-get-session-objC elt))
	  (setq pt (point))
	  ;; insert markers
	  (insert (cond ((null session-buf) " ") ; virgin session
			((ediff-buffer-live-p session-buf) "+") ;active session
			(t "-"))) ; finished session
	  (insert (cond ((ediff-get-session-status elt)) ; session has status,
						       ;;; e.g., ?H, ?I
			(t " "))) ; normal session
	  (insert "  Session " (int-to-string sessionNum) ":\n")
	  (ediff-meta-insert-file-info f1)
	  (ediff-meta-insert-file-info f2)
	  (ediff-meta-insert-file-info f3)
	  (ediff-set-meta-overlay pt (point) elt)))
      (set-buffer-modified-p nil)
      (goto-char point)
      meta-buf)))

;; Check if this is a problematic session.
;; Return nil if not. Otherwise, return symbol representing the problem
;; At present, problematic sessions occur only in -with-ancestor comparisons
;; when the ancestor is a directory rather than a file.
(defun ediff-problematic-session-p (session)
  (let ((f1 (ediff-get-session-objA session))
	(f2 (ediff-get-session-objB session))
	(f3 (ediff-get-session-objC session)))
    (cond ((and (stringp f1) (not (file-directory-p f1))
		(stringp f2) (not (file-directory-p f2))
		(stringp f3) (file-directory-p f3)
		(ediff-ancestor-metajob))
	   ;; more may be added later
	   'ancestor-is-dir)
	  (t nil))))

(defun ediff-meta-insert-file-info (file)
  (if (stringp file)
      (insert
       (format
	"  %10d  %s\n"
	(nth 7 (file-attributes file))
	;; dir names in meta lists have no trailing `/' so insert it
	(cond ((file-directory-p file)
	       (file-name-as-directory (ediff-abbreviate-file-name file)))
	      (t (ediff-abbreviate-file-name file)))))
    ))
      


(defun ediff-draw-dir-diffs (diff-list)
  (if (null diff-list) (error "Lost difference info on these directories"))
  (let* ((buf-name (ediff-unique-buffer-name
		    "*Ediff File Group Differences" "*"))
	 (regexp (ediff-get-group-regexp diff-list))
	 (dir1 (ediff-abbreviate-file-name (ediff-get-group-objA diff-list)))
	 (dir2 (ediff-abbreviate-file-name (ediff-get-group-objB diff-list)))
	 (dir3 (ediff-get-group-objC diff-list))
	 (dir3 (if (stringp dir3) (ediff-abbreviate-file-name dir3)))
	 (meta-buf (ediff-get-group-buffer diff-list))
	 (underline (make-string 26 ?-))
	 file code 
	 buffer-read-only)
    ;; skip the directory part
    (setq diff-list (cdr diff-list))
    (setq ediff-dir-diffs-buffer (get-buffer-create buf-name))
    (ediff-eval-in-buffer ediff-dir-diffs-buffer
      (use-local-map ediff-dir-diffs-buffer-map)
      (erase-buffer)
      (setq ediff-meta-buffer meta-buf)
      (insert "\t\t*** Directory Differences ***\n")
      (insert "
Useful commands:
     `q': hide this buffer
     SPC: next line
     DEL: previous line\n\n")

      (if (and (stringp regexp) (> (length regexp) 0))
	  (insert (format "Filter-through regular expression: %s\n" regexp)))
      (insert "\n")
      (insert (format "\n%-27s%-26s"
		      (ediff-truncate-string-left
		       (ediff-abbreviate-file-name
			(file-name-as-directory dir1))
		       25)
		      (ediff-truncate-string-left
		       (ediff-abbreviate-file-name
			(file-name-as-directory dir2))
		       25)))
      (if dir3
	  (insert (format " %-25s\n"
			  (ediff-truncate-string-left
			   (ediff-abbreviate-file-name
			    (file-name-as-directory dir3))
			   25)))
	(insert "\n"))
      (insert (format "%s%s" underline underline))
      (if (stringp dir3)
	  (insert (format "%s\n\n" underline))
	(insert "\n\n"))

      (if (null diff-list)
	  (insert "\n\t***  No differences  ***\n"))

      (while diff-list
	(setq file (car (car diff-list))
	      code (cdr (car diff-list))
	      diff-list (cdr diff-list))
	(if (= (mod code 2) 0) ; dir1
	    (insert (format "%-27s"
			    (ediff-truncate-string-left
			     (ediff-abbreviate-file-name
			      (if (file-directory-p (concat dir1 file))
				  (file-name-as-directory file)
				file))
			     24)))
	  (insert (format "%-27s" "---")))
	(if (= (mod code 3) 0) ; dir2
	    (insert (format "%-26s"
			    (ediff-truncate-string-left
			     (ediff-abbreviate-file-name
			      (if (file-directory-p (concat dir2 file))
				  (file-name-as-directory file)
				file))
			     24)))
	  (insert (format "%-26s" "---")))
	(if (stringp dir3)
	    (if (= (mod code 5) 0) ; dir3
		(insert (format " %-25s" 
				(ediff-truncate-string-left
				 (ediff-abbreviate-file-name
				  (if (file-directory-p (concat dir3 file))
				      (file-name-as-directory file)
				    file))
				 24)))
	      (insert (format " %-25s" "---"))))
	(insert "\n"))
      (setq buffer-read-only t)
      (set-buffer-modified-p nil)) ; eval in diff buffer
  ))

(defun ediff-bury-dir-diffs-buffer ()
  "Bury the directory difference buffer. Display the meta buffer instead."
  (interactive)
  (let ((buf ediff-meta-buffer)
	wind)
    (bury-buffer)
    (if (setq wind (ediff-get-visible-buffer-window buf))
	(select-window wind)
      (set-window-buffer (selected-window) buf))))

;; executes in dir session group buffer
;; show buffer differences
(defun ediff-show-dir-diffs ()
  "Display differences among the directories involved in session group."
  (interactive)
  (if (ediff-dir1-metajob)
      (error "This command is inapplicable in the present context"))
  (or (ediff-buffer-live-p ediff-dir-diffs-buffer)
      (ediff-draw-dir-diffs ediff-dir-difference-list))
  (let ((buf ediff-dir-diffs-buffer))
    (other-window 1)
    (set-window-buffer (selected-window) buf)
    (goto-char (point-min))))

(defun ediff-up-meta-hierarchy ()
  "Go to the parent session group buffer."
  (interactive)
  (if (ediff-buffer-live-p ediff-parent-meta-buffer)
      (ediff-show-meta-buffer ediff-parent-meta-buffer)
    (error "This session group has no parent")))
  

;; argument is ignored
(defun ediff-redraw-registry-buffer (&optional ignore)
  (ediff-eval-in-buffer ediff-registry-buffer
    (let ((point (point))
	  elt bufAname bufBname bufCname cur-diff total-diffs pt
	  job-name meta-list registry-list buffer-read-only)
      (erase-buffer)
      (insert "This is a registry of all active Ediff sessions.

Useful commands:
     button2, `v', RET over a session record:  switch to that session
     `M' over a session record:  display the associated session group
     `R' in any Ediff session:   display session registry
     SPC:\tnext session
     DEL:\tprevious session
     `E':\tbrowse Ediff on-line manual
     `q':\tbury registry


\t\tActive Ediff Sessions:
\t\t----------------------

")
      ;; purge registry list from dead buffers
      (mapcar (function (lambda (elt)
			  (if (not (ediff-buffer-live-p elt))
			      (setq ediff-session-registry
				    (delq elt ediff-session-registry)))))
	      ediff-session-registry)

      (if (null ediff-session-registry)
	  (insert "       ******* No active Ediff sessions *******\n"))

      (setq registry-list ediff-session-registry)
      (while registry-list
	(setq elt (car registry-list)
	      registry-list (cdr registry-list))
	
	(if (ediff-buffer-live-p elt)
	    (if (ediff-eval-in-buffer elt
		  (setq job-name ediff-metajob-name
			meta-list ediff-meta-list)
		  (and ediff-metajob-name
		       (not (eq ediff-metajob-name 'ediff-registry))))
		(progn
		  (setq pt (point))
		  (insert (format "  *group*\t%s: %s\n"
				  (buffer-name elt)
				  (ediff-abbrev-jobname job-name)))
		  (insert (format "\t\t   %s   %s   %s\n"
				  (ediff-abbreviate-file-name
				   (ediff-get-group-objA meta-list))
				  (ediff-abbreviate-file-name
				   (or (ediff-get-group-objB meta-list) ""))
				  (ediff-abbreviate-file-name
				   (or (ediff-get-group-objC meta-list) ""))))
		  (ediff-set-meta-overlay pt (point) elt))
	      (progn
		(ediff-eval-in-buffer elt
		  (setq bufAname (if (ediff-buffer-live-p ediff-buffer-A)
				     (buffer-name ediff-buffer-A)
				   "!!!killed buffer!!!")
			bufBname (if (ediff-buffer-live-p ediff-buffer-B)
				     (buffer-name ediff-buffer-B)
				   "!!!killed buffer!!!")
			bufCname (cond ((not (ediff-3way-job))
					"")
				       ((ediff-buffer-live-p ediff-buffer-C)
					(buffer-name ediff-buffer-C))
				       (t "!!!killed buffer!!!")))
		  (setq total-diffs (format "%-4d" ediff-number-of-differences)
			cur-diff
			(cond ((= ediff-current-difference -1) "   _")
			      ((= ediff-current-difference
				  ediff-number-of-differences)
			       "   $")
			      (t (format
				  "%4d" (1+ ediff-current-difference))))
			job-name ediff-job-name))
		;; back in the meta buf
		(setq pt (point))
		(insert cur-diff "/" total-diffs "\t"
			(buffer-name elt)
			(format ": %s" 	(ediff-abbrev-jobname job-name)))
		(insert
		 "\n\t\t   " bufAname "   " bufBname "   " bufCname "\n")
		(ediff-set-meta-overlay pt (point) elt))))
	) ; while
      (set-buffer-modified-p nil)
      (goto-char point)
      )))

;; sets overlay around a meta record with 'ediff-meta-info property PROP
(defun ediff-set-meta-overlay (b e prop)
  (let (overl)
    (setq overl (ediff-make-overlay b e))
    (if ediff-emacs-p
	(ediff-overlay-put overl 'mouse-face 'highlight)
      (ediff-overlay-put overl 'highlight t))
    (ediff-overlay-put overl 'ediff-meta-info prop)))

(defun ediff-mark-for-hiding (unmark)
  "Mark session for hiding. With prefix arg, unmark."
  (interactive "P")
  (let* ((pos (ediff-event-point last-command-event))
	 (meta-buf (ediff-event-buffer last-command-event))
	 ;; ediff-get-meta-info gives error if meta-buf or pos are invalid
	 (info (ediff-get-meta-info meta-buf pos))
	 (session-buf (ediff-get-session-buffer info)))

    (if unmark
	(ediff-set-session-status info nil)
      (if (ediff-buffer-live-p session-buf)
	  (error "Can't hide active session, %s" (buffer-name session-buf)))
      (ediff-set-session-status info ?H))
    (ediff-update-meta-buffer meta-buf)
    ))

(defun ediff-mark-for-operation (unmark)
  "Mark session for a group operation. With prefix arg, unmark."
  (interactive "P")
  (let* ((pos (ediff-event-point last-command-event))
	 (meta-buf (ediff-event-buffer last-command-event))
	 ;; ediff-get-meta-info gives error if meta-buf or pos are invalid
	 (info (ediff-get-meta-info meta-buf pos)))

    (if unmark
	(ediff-set-session-status info nil)
      (ediff-set-session-status info ?*))
    (ediff-update-meta-buffer meta-buf)
    ))

(defun ediff-hide-marked-sessions (unhide)
  "Hide marked sessions. With prefix arg, unhide."
  (interactive "P")
  (let ((grp-buf (ediff-get-group-buffer ediff-meta-list))
	(meta-list (cdr ediff-meta-list))
	(from (if unhide ?I ?H))
	(to (if unhide ?H ?I))
	(numMarked 0)
	elt)
    (while meta-list
      (setq elt (car meta-list)
	    meta-list (cdr meta-list))
      (if (eq (ediff-get-session-status elt) from)
	  (progn
	    (setq numMarked (1+ numMarked))
	    (ediff-set-session-status elt to))))
    (if (> numMarked 0)
	(ediff-update-meta-buffer grp-buf)
      (beep)
      (if unhide
	  (message "Nothing to reveal...")
	(message "Nothing to hide...")))
    ))

;; Apply OPERATION to marked sessions. Operation expects one argument of type
;; meta-list member (not the first one), i.e., a regular session description.
;; Returns number of marked sessions on which operation was performed
(defun ediff-operate-on-marked-sessions (operation)
  (let ((grp-buf (ediff-get-group-buffer ediff-meta-list))
	(meta-list (cdr ediff-meta-list))
	(marksym ?*)
	(numMarked 0)
	(sessionNum 0)
	elt)
    (while meta-list
      (setq elt (car meta-list)
	    meta-list (cdr meta-list)
	    sessionNum (1+ sessionNum))
      (if (eq (ediff-get-session-status elt) marksym)
	  (save-excursion
	    (setq numMarked (1+ numMarked))
	    (funcall operation elt sessionNum))))
    (ediff-update-meta-buffer grp-buf) ; just in case
    numMarked
    ))

(defun ediff-append-custom-diff (session sessionNum)
  (or (ediff-collect-diffs-metajob)
      (error "Sorry, I don't do this for everyone..."))
  (let ((session-buf (ediff-get-session-buffer session))
	(meta-diff-buff ediff-meta-diff-buffer)
	(metajob ediff-metajob-name)
	tmp-buf custom-diff-buf)
    (if (ediff-buffer-live-p session-buf)
	(ediff-eval-in-buffer session-buf
	  (if (eq ediff-control-buffer session-buf) ; individual session
	      (setq custom-diff-buf ediff-custom-diff-buffer))))

    (or (ediff-buffer-live-p meta-diff-buff)
	(error "Ediff: something wrong--no multiple diffs buffer"))

    (cond ((ediff-buffer-live-p custom-diff-buf)
	   (save-excursion
	     (set-buffer meta-diff-buff)
	     (goto-char (point-max))
	     (insert-buffer custom-diff-buf)
	     (insert "\n")))
	  ((eq metajob 'ediff-directories)
	   ;; get diffs by calling shell command on ediff-custom-diff-program
	   (save-excursion
	     (set-buffer (setq tmp-buf (get-buffer-create ediff-tmp-buffer)))
	     (erase-buffer)
	     (shell-command
	      (format "%s %s %s %s"
		      ediff-custom-diff-program ediff-custom-diff-options
		      (ediff-get-session-objA session)
		      (ediff-get-session-objB session))
	      t))
	   (save-excursion
	     (set-buffer meta-diff-buff)
	     (goto-char (point-max))
	     (insert-buffer tmp-buf)
	     (insert "\n")))
	  (t
	   (error
	    "Session %d is marked but inactive--can't make its diff"
	    sessionNum)))
	  ))

(defun ediff-collect-custom-diffs ()
  "Collect custom diffs of marked sessions in buffer `*Ediff Multifile Diffs*'.
This operation is defined only for `ediff-directories' and
`ediff-directory-revisions', since its intent is to produce
multifile patches. For `ediff-directory-revisions', we insist that
all marked sessions must be active."
  (interactive)
  (or (ediff-buffer-live-p ediff-meta-diff-buffer)
      (setq ediff-meta-diff-buffer
	    (get-buffer-create
	     (ediff-unique-buffer-name "*Ediff Multifile Diffs" "*"))))
  (ediff-eval-in-buffer ediff-meta-diff-buffer
    (erase-buffer))
  (if (> (ediff-operate-on-marked-sessions 'ediff-append-custom-diff) 0)
      ;; did something
      (display-buffer ediff-meta-diff-buffer 'not-this-window)
    (beep)
    (message "No marked sessions found")))

	      
;; This function executes in meta buffer. It knows where event happened.
(defun ediff-dir-action ()
  "Execute appropriate action for the selected session."
  (interactive)
  (let* ((pos (ediff-event-point last-command-event))
	 (meta-buf (ediff-event-buffer last-command-event))
	 ;; ediff-get-meta-info gives error if meta-buf or pos are invalid
	 (info (ediff-get-meta-info meta-buf pos))
	 session-buf file1 file2 file3 regexp)

    (setq session-buf (ediff-get-session-buffer info)
	  file1 (ediff-get-session-objA info)
	  file2 (ediff-get-session-objB info)
	  file3 (ediff-get-session-objC info))

    ;; make sure we don't start on hidden sessions
    ;; ?H means marked for hiding. ?I means invalid (hidden).
    (if (memq (ediff-get-session-status info) '(?H ?I))
	(progn
	  (beep)
	  (if (y-or-n-p "This session is marked as hidden, unmark? ")
	      (progn
		(ediff-set-session-status info nil)
		(ediff-update-meta-buffer meta-buf))
	    (error "Aborted"))))

    (ediff-eval-in-buffer meta-buf
      (goto-char pos) ; if the user clicked on session--move point there
      ;; First handle sessions involving directories (which are themselves
      ;; session groups)
      ;; After that handle individual sessions
      (cond ((and (file-directory-p file1) 
		  (stringp file2) (file-directory-p file2)
		  (if (stringp file3) (file-directory-p file1) t))
	     ;; do ediff/ediff-merge on subdirectories
	     (if (ediff-buffer-live-p session-buf)
		 (ediff-show-meta-buffer session-buf)
	       (setq regexp (read-string "Filter through regular expression: " 
					 nil 'ediff-filtering-regexp-history))
	       (ediff-directories-internal
		file1 file2 file3 regexp
		ediff-session-action-function
		ediff-metajob-name 
		;; make it update car info after startup
		(` (list (lambda () 
			   ;; child session group should know its parent
			   (setq ediff-parent-meta-buffer
				 (quote (, ediff-meta-buffer)))
			   ;; and parent will know its child
			   (setcar (quote (, info)) ediff-meta-buffer)))))))

	    ;; Do ediff-revision on a subdirectory
	    ((and (ediff-dir1-metajob) (file-directory-p file1))
	     (if (ediff-buffer-live-p session-buf)
		 (ediff-show-meta-buffer session-buf)
	       (setq regexp (read-string "Filter through regular expression: " 
					 nil 'ediff-filtering-regexp-history))
	       (ediff-directory-revisions-internal
		file1 regexp
		ediff-session-action-function ediff-metajob-name
		;; make it update car info after startup
		(` (list (lambda () 
			   ;; child session group should know its parent
			   (setq ediff-parent-meta-buffer
				 (quote (, ediff-meta-buffer)))
			   ;; and parent will know its child
			   (setcar (quote (, info)) ediff-meta-buffer)))))))

	    ;; From here on---only individual session handlers

	    ;; handle an individual session with live control buffer
	    ((ediff-buffer-live-p session-buf)
	     (ediff-eval-in-buffer session-buf
	       (setq ediff-mouse-pixel-position (mouse-pixel-position))
	       (ediff-recenter 'no-rehighlight)))

	    ((ediff-problematic-session-p info)
	     (beep)
	     (if (y-or-n-p
		  "This session's ancestor is a directory, merge without the ancestor? ")
		 (ediff-merge-files
		  file1 file2
		  ;; arrange startup hooks 
		  (` (list (lambda () 
			     (setq ediff-meta-buffer (, (current-buffer)))
			     ;; see below for the explanation of what this does
			     (setcar
			      (quote (, info)) ediff-control-buffer)))))
	       (error "Aborted")))
	    ((ediff-dir1-metajob) 	; needs 1 file arg
	     (funcall ediff-session-action-function
		      file1
		      ;; arrange startup hooks 
		      (` (list (lambda () 
				 (setq ediff-meta-buffer (, (current-buffer)))
				 ;; see below for explanation of what this does
				 (setcar
				  (quote (, info)) ediff-control-buffer))))))
	    ((not (ediff-metajob3))      ; need 2 file args
	     (funcall ediff-session-action-function
		      file1 file2
		      ;; arrange startup hooks 
		      (` (list (lambda () 
				 (setq ediff-meta-buffer (, (current-buffer)))
				 ;; this makes ediff-startup pass the value of
				 ;; ediff-control-buffer back to the meta
				 ;; level, to the record in the meta list
				 ;; containing the information about the
				 ;; session associated with that
				 ;; ediff-control-buffer
				 (setcar
				  (quote (, info)) ediff-control-buffer))))))
	    ((ediff-metajob3)      ; need 3 file args
	     (funcall ediff-session-action-function
		      file1 file2 file3
		      ;; arrange startup hooks 
		      (` (list (lambda () 
				 (setq ediff-meta-buffer (, (current-buffer)))
				 (setcar
				  (quote (, info)) ediff-control-buffer))))))
	    ) ; cond
      ) ; eval in meta-buf
    ))

(defun ediff-registry-action ()
  "Switch to a selected session."
  (interactive)
  (let* ((pos (ediff-event-point last-command-event))
	 (buf (ediff-event-buffer last-command-event))
	 (ctl-buf (ediff-get-meta-info buf pos)))

    (if (ediff-buffer-live-p ctl-buf)
	;; check if this is ediff-control-buffer or ediff-meta-buffer
	(if (ediff-eval-in-buffer ctl-buf
	      (eq (key-binding "q") 'ediff-quit-meta-buffer))
	    ;; it's a meta-buffer -- last action should just display it
	    (ediff-show-meta-buffer ctl-buf)
	  ;; it's a session buffer -- invoke go back to session
	  (ediff-eval-in-buffer ctl-buf
	    (setq ediff-mouse-pixel-position (mouse-pixel-position))
	    (ediff-recenter 'no-rehighlight)))
      (beep)
      (message "You've selected a stale session --- try again")
      (ediff-update-registry))
    (ediff-eval-in-buffer buf
      (goto-char pos))
    ))


(defun ediff-show-meta-buffer (&optional meta-buf)
  "Show the session group buffer."
  (interactive)
  (let (wind frame silent)
    (if meta-buf (setq silent t))

    (setq meta-buf (or meta-buf ediff-meta-buffer))
    (cond ((not (bufferp meta-buf))
	   (error "This Ediff session is not part of a session group"))
	  ((not (ediff-buffer-live-p meta-buf))
	   (error
	    "Can't find this session's group panel -- session itself is ok")))

    (ediff-cleanup-meta-buffer meta-buf)
    (ediff-eval-in-buffer meta-buf
      (save-excursion
	(cond ((setq wind (ediff-get-visible-buffer-window meta-buf))
	       (or silent
		   (message
		    "Already showing the group panel for this session"))
	       (set-window-buffer wind meta-buf)
	       (select-window wind))
	      ((window-live-p (setq wind ediff-window-C)) ;in merge--merge buf
	       (set-window-buffer ediff-window-C meta-buf)
	       (select-window wind))
	      ((window-live-p (setq wind ediff-window-A))
	       (set-window-buffer ediff-window-A meta-buf)
	       (select-window wind))
	      ((window-live-p (setq wind ediff-window-B))
	       (set-window-buffer ediff-window-B meta-buf)
	       (select-window wind))
	      ((and
		(setq wind
		      (ediff-get-visible-buffer-window ediff-registry-buffer))
		(ediff-window-display-p))
	       (select-window wind)
	       (other-window 1)
	       (set-window-buffer (selected-window) meta-buf))
	      (t (ediff-skip-unsuitable-frames 'ok-unsplittable)
		 (set-window-buffer (selected-window) meta-buf)))
	))
    (if (ediff-window-display-p)
	(progn
	  (setq frame
		(window-frame (ediff-get-visible-buffer-window meta-buf)))
	  (raise-frame frame)
	  (ediff-reset-mouse frame)))
    (run-hooks 'ediff-show-session-group-hook)
    ))

(defun ediff-show-meta-buff-from-registry ()
  "Display the session group buffer for a selected session group."
  (interactive)
  (let* ((pos (ediff-event-point last-command-event))
	 (meta-buf (ediff-event-buffer last-command-event))
	 (info (ediff-get-meta-info meta-buf pos))
	 (meta-or-session-buf info))
    (ediff-eval-in-buffer meta-or-session-buf
      (ediff-show-meta-buffer))))

;;;###autoload
(defun ediff-show-registry ()
  "Display Ediff's registry."
  (interactive)
  (ediff-update-registry)
  (if (not (ediff-buffer-live-p ediff-registry-buffer))
      (error "No active Ediff sessions or corrupted session registry"))
  (let (wind frame)
    ;; for some reason, point moves in ediff-registry-buffer, so we preserve it
    ;; explicity
    (ediff-eval-in-buffer ediff-registry-buffer
      (save-excursion
	(cond  ((setq wind
		      (ediff-get-visible-buffer-window ediff-registry-buffer))
		(message "Already showing the registry")
		(set-window-buffer wind ediff-registry-buffer)
		(select-window wind))
	       ((window-live-p ediff-window-C)
		(set-window-buffer ediff-window-C ediff-registry-buffer)
		(select-window ediff-window-C))
	       ((window-live-p ediff-window-A)
		(set-window-buffer ediff-window-A ediff-registry-buffer)
		(select-window ediff-window-A))
	       ((window-live-p ediff-window-B)
		(set-window-buffer ediff-window-B ediff-registry-buffer)
		(select-window ediff-window-B))
	       ((and (setq wind
			   (ediff-get-visible-buffer-window ediff-meta-buffer))
		     (ediff-window-display-p))
		(select-window wind)
		(other-window 1)
		(set-window-buffer (selected-window) ediff-registry-buffer))
	       (t (ediff-skip-unsuitable-frames 'ok-unsplittable)
		  (set-window-buffer (selected-window) ediff-registry-buffer)))
	))
    (if (ediff-window-display-p)
	(progn
	  (setq frame 
		(window-frame
		 (ediff-get-visible-buffer-window ediff-registry-buffer)))
	  (raise-frame frame)
	  (ediff-reset-mouse frame)))
    (run-hooks 'ediff-show-registry-hook)
    ))

;;;###autoload
(defalias 'eregistry 'ediff-show-registry)

;; If meta-buf doesn't exist, it is created. In that case, id doesn't have a
;; parent meta-buf
;; Check if META-BUF exists before calling this function
(defun ediff-update-meta-buffer (meta-buf)
  (ediff-eval-in-buffer (current-buffer)
    (if (ediff-buffer-live-p meta-buf)
	(ediff-eval-in-buffer meta-buf
	  (funcall ediff-meta-redraw-function ediff-meta-list))
      )))

(defun ediff-update-registry ()
  (ediff-eval-in-buffer (current-buffer)
    (if (ediff-buffer-live-p ediff-registry-buffer)
	(ediff-redraw-registry-buffer)
      (ediff-prepare-meta-buffer 
       'ediff-registry-action
       ediff-session-registry
       "*Ediff Registry"
       'ediff-redraw-registry-buffer
       'ediff-registry))
    ))

;; if meta-buf exists, it is redrawn along with parent. Otherwise, nothing
;; happens
(defun ediff-cleanup-meta-buffer (meta-buffer)
  (if (ediff-buffer-live-p meta-buffer)
      (ediff-eval-in-buffer meta-buffer
	(ediff-update-meta-buffer meta-buffer)
	(if (ediff-buffer-live-p ediff-parent-meta-buffer)
	    (ediff-update-meta-buffer ediff-parent-meta-buffer)))))

;; t if no session in progress
(defun ediff-safe-to-quit (meta-buffer)
  (if (ediff-buffer-live-p meta-buffer)
      (let ((lis ediff-meta-list)
	    (cont t)
	    buffer-read-only)
	(ediff-update-meta-buffer meta-buffer)
	(ediff-eval-in-buffer meta-buffer
	  (setq lis (cdr lis)) ; discard the description part of meta-list
	  (while (and cont lis)
	    (if (ediff-buffer-live-p
		 (ediff-get-group-buffer lis)) ; in progress
		(setq cont nil))
	    (setq lis (cdr lis)))
	  cont))))

(defun ediff-quit-meta-buffer ()
  "If no unprocessed sessions in the group, delete the meta buffer.
If no session is in progress, ask to confirm before deleting meta buffer.
Otherwise, bury the meta buffer.
If this is a session registry buffer then just bury it."
  (interactive)
  (let* ((buf (current-buffer))
	 (dir-diffs-buffer ediff-dir-diffs-buffer)
	 (meta-diff-buffer ediff-meta-diff-buffer)
	 (parent-buf ediff-parent-meta-buffer)
	 (dont-show-registry (eq buf ediff-registry-buffer)))
    (if dont-show-registry
	(bury-buffer)
      (ediff-cleanup-meta-buffer buf)
      (cond ((and (ediff-safe-to-quit buf)
		  (y-or-n-p "Quit this session group? "))
	     (ediff-dispose-of-meta-buffer buf))
	    ((ediff-safe-to-quit buf)
	     (bury-buffer))
	    (t
	     (bury-buffer)
	     (beep)
	     (message
	      "Group has active sessions, panel not deleted")))
      (ediff-cleanup-meta-buffer parent-buf)
      (ediff-kill-buffer-carefully dir-diffs-buffer)
      (ediff-kill-buffer-carefully meta-diff-buffer)
      (if (ediff-buffer-live-p parent-buf)
	  (progn
	    (setq dont-show-registry t)
	    (ediff-show-meta-buffer parent-buf)))
      )
    (or dont-show-registry
	(ediff-show-registry))))

(defun ediff-dispose-of-meta-buffer (buf)
  (setq ediff-session-registry (delq buf ediff-session-registry))
  (ediff-eval-in-buffer buf
    (if (ediff-buffer-live-p ediff-dir-diffs-buffer)
	(kill-buffer ediff-dir-diffs-buffer)))
  (kill-buffer buf))
    

;; obtain information on a meta record where the user clicked or typed
;; BUF is the buffer where this happened and POINT is the position
;; If optional NOERROR arg is given, don't report error and return nil if no
;; meta info is found on line.
(defun ediff-get-meta-info (buf point &optional noerror)
  (let (result olist tmp)
    (if (and point (ediff-buffer-live-p buf))
	(ediff-eval-in-buffer buf
	  (if ediff-xemacs-p
	      (setq result
		    (if (setq tmp (extent-at point buf 'ediff-meta-info))
			(ediff-overlay-get tmp 'ediff-meta-info)))
	    (setq olist (overlays-at point))
	    (setq olist
		  (mapcar (function (lambda (elt)
				      (overlay-get elt 'ediff-meta-info)))
			  olist))
	    (while (and olist (null (car olist))
			(overlay-get (car olist) 'invisible))
	      (setq olist (cdr olist)))
	    (setq result (car olist)))))
    (if result
	result
      (if noerror
	  nil
	(ediff-update-registry)
	(error "No session info in this line")))))

;; return location of the next meta overlay after point
(defun ediff-next-meta-overlay-start (point)
  (let (overl)
    (if ediff-xemacs-p
	(progn
	  (setq overl (extent-at point (current-buffer) 'ediff-meta-info))
	  (if overl
	      (setq overl (next-extent overl))
	    (setq overl (next-extent (current-buffer))))
	  (if overl
	      (extent-start-position overl)
	    (point-max)))
      (if (= point (point-max)) (setq point (point-min)))
      (setq overl (car (overlays-at point)))
      (if (and overl (overlay-get overl 'ediff-meta-info))
	  (overlay-end overl)
	(next-overlay-change point)))))

(defun ediff-previous-meta-overlay-start (point)
  (let (overl)
    (if ediff-xemacs-p
	(progn
	  (setq overl (extent-at point (current-buffer) 'ediff-meta-info))
	  (if overl
	      (setq overl (previous-extent overl))
	    (setq overl (previous-extent (current-buffer))))
	  (if overl
	      (extent-start-position overl)
	    (point-max)))
      ;;(if (bobp) (setq point (point-max)))
      (setq overl (car (overlays-at point)))
      (setq point (if (and overl (overlay-get overl 'ediff-meta-info))
		      (previous-overlay-change (overlay-start overl))
		    (previous-overlay-change point)))
      (if (= point (point-min)) (point-max) point)
      )))


;;; Local Variables:
;;; eval: (put 'ediff-defvar-local 'lisp-indent-hook 'defun)
;;; eval: (put 'ediff-eval-in-buffer 'lisp-indent-hook 1)
;;; End:

(provide 'ediff-meta)
(require 'ediff-util)

;;; ediff-meta.el ends here
