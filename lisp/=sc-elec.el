;; -*- Mode: Emacs-Lisp -*-
;; sc-elec.el  --  Version 2.3

;; ========== Introduction ==========
;; This file contains sc-electric mode for viewing reference headers.
;; It is loaded automatically by supercite.el when needed.

;; ========== Disclaimer ==========
;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor accepts
;; responsibility to anyone for the consequences of using it or for
;; whether it serves any particular purpose or works at all, unless he
;; says so in writing.

;; Some of this software was written as part of the supercite author's
;; official duty as an employee of the United States Government and is
;; thus in the public domain.  You are free to use that particular
;; software as you wish, but WITHOUT ANY WARRANTY WHATSOEVER.  It
;; would be nice, though if when you use any of this code, you give
;; due credit to the author.

;; Other parts of this code were written by other people.  Wherever
;; possible, credit to that author, and the copy* notice supplied by
;; the author are included with that code. In all cases, the spirit,
;; if not the letter of the GNU General Public Licence applies.

;; ========== Author (unless otherwise stated) ==========
;; NAME: Barry A. Warsaw        USMAIL: Century Computing, Inc.
;; TELE: (301) 593-3330                 1014 West Street
;; UUCP: uunet!cen.com!bwarsaw          Laurel, MD 20707
;; INET: bwarsaw@cen.com                    

;; Want to be on the Supercite mailing list?
;;
;; Send articles to:
;;         INET: supercite@anthem.nlm.nih.gov
;;         UUCP: uunet!anthem.nlm.nih.gov!supercite
;; 
;; Send administrivia (additions/deletions to list, etc) to:
;;         INET: supercite-request@anthem.nlm.nih.gov
;;         UUCP: uunet!anthem.nlm.nih.gov!supercite-request
;;
(provide 'sc-elec)


;; ======================================================================
;; set up vars for major mode

(defconst sc-electric-bufname "*sc-erefs*"
  "*Supercite's electric buffer name.")


(defvar sc-electric-mode-hook nil
  "*Hook for sc-electric-mode.")



;; ======================================================================
;; sc-electric-mode

(defun sc-electric-mode (&optional arg)
  "Quasi major mode for viewing supercite reference headers.
Commands are: \\{sc-electric-mode-map}
Sc-electric-mode is not intended to be run interactively, but rather
accessed through supercite's electric reference feature.  See
sc-insert-reference for more details. Optional ARG is the initial
header style to use, unless not supplied or invalid, in which case
sc-preferred-header-style is used."
  (let ((gal sc-gal-information)
	(sc-eref-style (if arg  ;; assume passed arg is okay
			   arg
			 (if (and (natnump sc-preferred-header-style)
				  (sc-valid-index-p sc-preferred-header-style))
			     sc-preferred-header-style
			   0))))
    (get-buffer-create sc-electric-bufname)
    ;; set up buffer and enter command loop
    (save-excursion
      (save-window-excursion
	(pop-to-buffer sc-electric-bufname)
	(kill-all-local-variables)
	(setq sc-gal-information gal
	      buffer-read-only t
	      mode-name "Supercite-Electric-References"
	      major-mode 'sc-electric-mode)
	(use-local-map sc-electric-mode-map)
	(sc-eref-show sc-eref-style)
	(run-hooks 'sc-electric-mode-hook)
	(recursive-edit)
	))
    (if sc-eref-style
	(condition-case nil
	    (eval (nth sc-eref-style sc-rewrite-header-list))
	  (error nil)
	  ))	  
    ;; now restore state
    (kill-buffer sc-electric-bufname)
    ))



;; ======================================================================
;; functions for electric mode

(defun sc-eref-index (index)
  "Check INDEX to be sure it is a valid index into sc-rewrite-header-list.
If sc-electric-circular-p is non-nil, then list is considered circular
so that movement across the ends of the list wraparound."
  (let ((last (1- (length sc-rewrite-header-list))))
    (cond ((sc-valid-index-p index) index)
	  ((< index 0)
	   (if sc-electric-circular-p last
	     (progn (error "No preceding reference headers in list.") 0)))
	  ((> index last)
	   (if sc-electric-circular-p 0
	     (progn (error "No following reference headers in list.") last)))
	  )
    ))


(defun sc-eref-show (index)
  "Show reference INDEX in sc-rewrite-header-list."
  (setq sc-eref-style (sc-eref-index index))
  (save-excursion
    (set-buffer sc-electric-bufname)
    (let ((ref (nth sc-eref-style sc-rewrite-header-list))
	  (buffer-read-only nil))
      (erase-buffer)
      (goto-char (point-min))
      (condition-case err
	  (progn
	    (set-mark (point-min))
	    (eval ref)
	    (message "Showing reference header %d." sc-eref-style)
	    (goto-char (point-max))
	    )
	(void-function
	 (progn (message
		 "Symbol's function definition is void: %s (Header %d)"
		 (symbol-name (car (cdr err)))
		 sc-eref-style)
		(beep)
		))
	))))



;; ======================================================================
;; interactive commands

(defun sc-eref-next ()
  "Display next reference in other buffer."
  (interactive)
  (sc-eref-show (1+ sc-eref-style)))


(defun sc-eref-prev ()
  "Display previous reference in other buffer."
  (interactive)
  (sc-eref-show (1- sc-eref-style)))


(defun sc-eref-setn ()
  "Set reference header selected as preferred."
  (interactive)
  (setq sc-preferred-header-style sc-eref-style)
  (message "Preferred reference style set to header %d." sc-eref-style))


(defun sc-eref-goto (refnum)
  "Show reference style indexed by REFNUM.
If REFNUM is an invalid index, don't go to that reference and return
nil."
  (interactive "NGoto Reference: ")
  (if (sc-valid-index-p refnum)
      (sc-eref-show refnum)
    (error "Invalid reference: %d. (Range: [%d .. %d])"
	   refnum 0 (1- (length sc-rewrite-header-list)))
    ))


(defun sc-eref-jump ()
  "Set reference header to preferred header."
  (interactive)
  (sc-eref-show sc-preferred-header-style))


(defun sc-eref-abort ()
  "Exit from electric reference mode without inserting reference."
  (interactive)
  (setq sc-eref-style nil)
  (exit-recursive-edit))


(defun sc-eref-exit ()
  "Exit from electric reference mode and insert selected reference."
  (interactive)
  (exit-recursive-edit))
