;;; cc-defs.el --- definitions for CC Mode

;; Copyright (C) 1985,87,92,93,94,95,96,97 Free Software Foundation, Inc.

;; Authors:    1992-1997 Barry A. Warsaw
;;             1987 Dave Detlefs and Stewart Clamen
;;             1985 Richard M. Stallman
;; Maintainer: cc-mode-help@python.org
;; Created:    22-Apr-1997 (split from cc-mode.el)
;; Version:    5.17
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


;; Figure out what features this Emacs has
(defconst c-emacs-features
  (let ((infodock-p (boundp 'infodock-version))
	(comments
	 ;; XEmacs 19 and beyond use 8-bit modify-syntax-entry flags.
	 ;; Emacs 19 uses a 1-bit flag.  We will have to set up our
	 ;; syntax tables differently to handle this.
	 (let ((table (copy-syntax-table))
	       entry)
	   (modify-syntax-entry ?a ". 12345678" table)
	   (cond
	    ;; XEmacs 19, and beyond Emacs 19.34
	    ((arrayp table)
	     (setq entry (aref table ?a))
	     ;; In Emacs, table entries are cons cells
	     (if (consp entry) (setq entry (car entry))))
	    ;; XEmacs 20
	    ((fboundp 'get-char-table) (setq entry (get-char-table ?a table)))
	    ;; before and including Emacs 19.34
	    ((and (fboundp 'char-table-p)
		  (char-table-p table))
	     (setq entry (car (char-table-range table [?a]))))
	    ;; incompatible
	    (t (error "CC Mode is incompatible with this version of Emacs")))
	   (if (= (logand (lsh entry -16) 255) 255)
	       '8-bit
	     '1-bit))))
    (if infodock-p
	(list comments 'infodock)
      (list comments)))
  "A list of features extant in the Emacs you are using.
There are many flavors of Emacs out there, each with different
features supporting those needed by CC Mode.  Here's the current
supported list, along with the values for this variable:

 XEmacs 19:                  (8-bit)
 XEmacs 20:                  (8-bit)
 Emacs 19:                   (1-bit)

Infodock (based on XEmacs) has an additional symbol on this list:
'infodock.")



(defsubst c-point (position)
  ;; Returns the value of point at certain commonly referenced POSITIONs.
  ;; POSITION can be one of the following symbols:
  ;; 
  ;; bol  -- beginning of line
  ;; eol  -- end of line
  ;; bod  -- beginning of defun
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
     ((eq position 'bod)
      (beginning-of-defun)
      ;; if defun-prompt-regexp is non-nil, b-o-d won't leave us at
      ;; the open brace.
      (and defun-prompt-regexp
	   (looking-at defun-prompt-regexp)
	   (goto-char (match-end 0)))
      )
     ((eq position 'boi)  (back-to-indentation))
     ((eq position 'bonl) (forward-line 1))
     ((eq position 'bopl) (forward-line -1))
     ((eq position 'iopl)
      (forward-line -1)
      (back-to-indentation))
     ((eq position 'ionl)
      (forward-line 1)
      (back-to-indentation))
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


(provide 'cc-defs)
;;; cc-defs.el ends here
