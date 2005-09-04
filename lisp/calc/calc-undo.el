;;; calc-undo.el --- undo functions for Calc

;; Copyright (C) 1990, 1991, 1992, 1993, 2001, 2002, 2003, 2004,
;;   2005 Free Software Foundation, Inc.

;; Author: David Gillespie <daveg@synaptics.com>
;; Maintainer: Jay Belanger <belanger@truman.edu>

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;;; Commentary:

;;; Code:

;; This file is autoloaded from calc-ext.el.

(require 'calc-ext)
(require 'calc-macs)

;;; Undo.

(defun calc-undo (n)
  (interactive "p")
  (when calc-executing-macro
    (error "Use C-x e, not X, to run a keyboard macro that uses Undo"))
  (if (<= n 0)
      (if (< n 0)
	  (calc-redo (- n))
	(calc-last-args 1))
    (calc-wrapper
     (when (null (nthcdr (1- n) calc-undo-list))
       (error "No further undo information available"))
     (setq calc-undo-list
	   (prog1
	       (nthcdr n calc-undo-list)
	     (let ((saved-stack-top calc-stack-top))
	       (let ((calc-stack-top 0))
		 (calc-handle-undos calc-undo-list n))
	       (setq calc-stack-top saved-stack-top))))
     (message "Undo!"))))

(defun calc-handle-undos (cl n)
  (when (> n 0)
    (let ((old-redo calc-redo-list))
      (setq calc-undo-list nil)
      (calc-handle-undo (car cl))
      (setq calc-redo-list (append calc-undo-list old-redo)))
    (calc-handle-undos (cdr cl) (1- n))))

(defun calc-handle-undo (list)
  (when list
       (let ((action (car list)))
	 (cond
	  ((eq (car action) 'push)
	   (calc-pop-stack 1 (nth 1 action) t))
	  ((eq (car action) 'pop)
	   (calc-push-list (nth 2 action) (nth 1 action)))
	  ((eq (car action) 'set)
	   (calc-record-undo (list 'set (nth 1 action)
				   (symbol-value (nth 1 action))))
	   (set (nth 1 action) (nth 2 action)))
	  ((eq (car action) 'store)
	   (let ((v (intern (nth 1 action))))
	     (calc-record-undo (list 'store (nth 1 action)
				     (and (boundp v) (symbol-value v))))
	     (if (y-or-n-p (format "Un-store variable %s? " 
                                   (calc-var-name (nth 1 action))))
		 (progn
		   (if (nth 2 action)
		       (set v (nth 2 action))
		     (makunbound v))
		   (calc-refresh-evaltos v)))))
	  ((eq (car action) 'eval)
	   (calc-record-undo (append (list 'eval (nth 2 action) (nth 1 action))
				     (cdr (cdr (cdr action)))))
	   (apply (nth 1 action) (cdr (cdr (cdr action))))))
	 (calc-handle-undo (cdr list)))))

(defun calc-redo (n)
  (interactive "p")
  (when calc-executing-macro
    (error "Use C-x e, not X, to run a keyboard macro that uses Redo"))
  (if (<= n 0)
      (calc-undo (- n))
    (calc-wrapper
     (when (null (nthcdr (1- n) calc-redo-list))
       (error "Unable to redo"))
     (setq calc-redo-list
	   (prog1
	       (nthcdr n calc-redo-list)
	     (let ((saved-stack-top calc-stack-top))
	       (let ((calc-stack-top 0))
		 (calc-handle-redos calc-redo-list n))
	       (setq calc-stack-top saved-stack-top))))
     (message "Redo!"))))

(defun calc-handle-redos (cl n)
  (when (> n 0)
    (let ((old-undo calc-undo-list))
      (setq calc-undo-list nil)
      (calc-handle-undo (car cl))
      (setq calc-undo-list (append calc-undo-list old-undo)))
    (calc-handle-redos (cdr cl) (1- n))))

(defun calc-last-args (n)
  (interactive "p")
  (when calc-executing-macro
    (error "Use C-x e, not X, to run a keyboard macro that uses last-args"))
  (calc-wrapper
   (let ((urec (calc-find-last-x calc-undo-list n)))
     (if urec
	 (calc-handle-last-x urec)
       (error "Not enough undo information available")))))

(defun calc-handle-last-x (list)
  (when list
    (let ((action (car list)))
      (if (eq (car action) 'pop)
	  (calc-pop-push-record-list 0 "larg"
				     (delq 'top-of-stack (nth 2 action))))
      (calc-handle-last-x (cdr list)))))

(defun calc-find-last-x (ul n)
  (when ul
    (if (calc-undo-does-pushes (car ul))
	(if (<= n 1)
	    (car ul)
	  (calc-find-last-x (cdr ul) (1- n)))
      (calc-find-last-x (cdr ul) n))))

(defun calc-undo-does-pushes (list)
  (and list
       (or (eq (car (car list)) 'pop)
	   (calc-undo-does-pushes (cdr list)))))

(provide 'calc-undo)

;;; arch-tag: eeb485d2-fb3d-454a-9d79-450af1f50d6c
;;; calc-undo.el ends here
