;;; cc-defs.el --- compile time definitions for CC Mode

;; Copyright (C) 1985,87,92,93,94,95,96,97,98 Free Software Foundation, Inc.

;; Authors:    1998 Barry A. Warsaw and Martin Stjernholm
;;             1992-1997 Barry A. Warsaw
;;             1987 Dave Detlefs and Stewart Clamen
;;             1985 Richard M. Stallman
;; Maintainer: bug-cc-mode@gnu.org
;; Created:    22-Apr-1997 (split from cc-mode.el)
;; Version:    See cc-mode.el
;; Keywords:   c languages oop

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;; Get all the necessary compile time definitions.
(require 'custom)
(require 'cc-menus)
(require 'derived)			;only necessary in Emacs 20

;; cc-mode-19.el contains compatibility macros that should be compiled
;; in if needed.
(if (or (not (fboundp 'functionp))
	(not (condition-case nil
		 (progn (char-before) t)
	       (error nil)))
	(not (condition-case nil
		 (progn (char-after) t)
	       (error nil)))
	(not (fboundp 'when))
	(not (fboundp 'unless)))
    (require 'cc-mode-19))


(defsubst c-point (position)
  ;; Returns the value of point at certain commonly referenced POSITIONs.
  ;; POSITION can be one of the following symbols:
  ;; 
  ;; bol  -- beginning of line
  ;; eol  -- end of line
  ;; bod  -- beginning of defun
  ;; eod  -- end of defun
  ;; boi  -- back to indentation
  ;; ionl -- indentation of next line
  ;; iopl -- indentation of previous line
  ;; bonl -- beginning of next line
  ;; bopl -- beginning of previous line
  ;; 
  ;; This function does not modify point or mark.
  (let ((here (point)))
    (cond
     ((eq position 'bol)  (beginning-of-line))
     ((eq position 'eol)  (end-of-line))
     ((eq position 'boi)  (back-to-indentation))
     ((eq position 'bonl) (forward-line 1))
     ((eq position 'bopl) (forward-line -1))
     ((eq position 'iopl)
      (forward-line -1)
      (back-to-indentation))
     ((eq position 'ionl)
      (forward-line 1)
      (back-to-indentation))
     ((eq position 'eod)  (c-end-of-defun))
     ((eq position 'bod)
      (if (and (fboundp 'buffer-syntactic-context-depth)
	       c-enable-xemacs-performance-kludge-p)
	  ;; XEmacs only.  This can improve the performance of
	  ;; c-parse-state to between 3 and 60 times faster when
	  ;; braces are hung.  It can also degrade performance by
	  ;; about as much when braces are not hung.
	  (let (pos)
	    (while (not pos)
	      (save-restriction
		(widen)
		(setq pos (scan-lists (point) -1
				      (buffer-syntactic-context-depth)
				      nil t)))
	      (cond
	       ((bobp) (setq pos (point-min)))
	       ((not pos)
		(let ((distance (skip-chars-backward "^{")))
		  ;; unbalanced parenthesis, while illegal C code,
		  ;; shouldn't cause an infloop!  See unbal.c
		  (when (zerop distance)
		    ;; Punt!
		    (beginning-of-defun)
		    (setq pos (point)))))
	       ((= pos 0))
	       ((not (eq (char-after pos) ?{))
		(goto-char pos)
		(setq pos nil))
	       ))
	    (goto-char pos))
	;; Emacs, which doesn't have buffer-syntactic-context-depth
	;;
	;; NOTE: This should be the only explicit use of
	;; beginning-of-defun in CC Mode.  Eventually something better
	;; than b-o-d will be available and this should be the only
	;; place the code needs to change.  Everything else should use
	;; (goto-char (c-point 'bod))
	(beginning-of-defun)
	;; if defun-prompt-regexp is non-nil, b-o-d won't leave us at
	;; the open brace.
	(and defun-prompt-regexp
	     (looking-at defun-prompt-regexp)
	     (goto-char (match-end 0)))
	))
     (t (error "unknown buffer position requested: %s" position))
     )
    (prog1
	(point)
      (goto-char here))))


(defmacro c-safe (&rest body)
  ;; safely execute BODY, return nil if an error occurred
  (` (condition-case nil
	 (progn (,@ body))
       (error nil))))

(defmacro c-forward-sexp (&optional arg)
  ;; like forward-sexp except
  ;;   1. this is much stripped down from the XEmacs version
  ;;   2. this cannot be used as a command, so we're insulated from
  ;;      XEmacs' losing efforts to make forward-sexp more user
  ;;      friendly
  ;;   3. Preserves the semantics most of CC Mode is based on
  (or arg (setq arg 1))
  `(goto-char (or (scan-sexps (point) ,arg)
		  ,(if (numberp arg)
		       (if (> arg 0) `(point-max) `(point-min))
		     `(if (> ,arg 0) (point-max) (point-min))))))

(defmacro c-backward-sexp (&optional arg)
  ;; See c-forward-sexp and reverse directions
  (or arg (setq arg 1))
  `(c-forward-sexp ,(if (numberp arg) (- arg) `(- ,arg))))

(defmacro c-add-syntax (symbol &optional relpos)
  ;; a simple macro to append the syntax in symbol to the syntax list.
  ;; try to increase performance by using this macro
  (` (setq syntax (cons (cons (, symbol) (, relpos)) syntax))))

(defsubst c-auto-newline ()
  ;; if auto-newline feature is turned on, insert a newline character
  ;; and return t, otherwise return nil.
  (and c-auto-newline
       (not (c-in-literal))
       (not (newline))))

(defsubst c-intersect-lists (list alist)
  ;; return the element of ALIST that matches the first element found
  ;; in LIST.  Uses assq.
  (let (match)
    (while (and list
		(not (setq match (assq (car list) alist))))
      (setq list (cdr list)))
    match))

(defsubst c-lookup-lists (list alist1 alist2)
  ;; first, find the first entry from LIST that is present in ALIST1,
  ;; then find the entry in ALIST2 for that entry.
  (assq (car (c-intersect-lists list alist1)) alist2))

(defsubst c-langelem-col (langelem &optional preserve-point)
  ;; convenience routine to return the column of langelem's relpos.
  ;; Leaves point at the relpos unless preserve-point is non-nil.
  (let ((here (point)))
    (goto-char (cdr langelem))
    (prog1 (current-column)
      (if preserve-point
	  (goto-char here))
      )))

(defsubst c-update-modeline ()
  ;; set the c-auto-hungry-string for the correct designation on the modeline
  (setq c-auto-hungry-string
	(if c-auto-newline
	    (if c-hungry-delete-key "/ah" "/a")
	  (if c-hungry-delete-key "/h" nil)))
  (force-mode-line-update))

(defsubst c-keep-region-active ()
  ;; Do whatever is necessary to keep the region active in XEmacs.
  ;; Ignore byte-compiler warnings you might see.  This is not needed
  ;; for Emacs.
  (and (boundp 'zmacs-region-stays)
       (setq zmacs-region-stays t)))

(defsubst c-region-is-active-p ()
  ;; Return t when the region is active.  The determination of region
  ;; activeness is different in both Emacs and XEmacs.
  (cond
   ;; XEmacs
   ((and (fboundp 'region-active-p)
	 zmacs-regions)
    (region-active-p))
   ;; Emacs
   ((boundp 'mark-active) mark-active)
   ;; fallback; shouldn't get here
   (t (mark t))))

(defsubst c-major-mode-is (mode)
  (eq (derived-mode-class major-mode) mode))


(provide 'cc-defs)
;;; cc-defs.el ends here
