;;; loadhist.el --- lisp functions for working with feature groups
;;; Copyright (C) 1995 Free Software Foundation, Inc.

;; Author: Eric S. Raymond <esr@snark.thyrsus.com>
;; Version: 1.0
;; Keywords: internal

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

;; These functions exploit the load-history system variable.
;; Entry points include `unload-feature', `symbol-file', and `feature-file'.

;;; Code:

(defun symbol-file (sym)
  "Return the input source from which SYM was loaded.
This is a file name, or nil if the source was a buffer with no associated file."
  (catch 'foundit
    (mapcar
     (function (lambda (x) (if (memq sym (cdr x)) (throw 'foundit (car x)))))
     load-history)
    nil))

(defun feature-symbols (feature)
  "Return the file and list of symbols associated with a given FEATURE."
   (catch 'foundit
     (mapcar
      (function (lambda (x) 
		  (if (member (cons 'provide feature) (cdr x))
		      (throw 'foundit x))))
      load-history)
     nil))

(defun feature-file (feature)
  "Return the file name from which a given FEATURE was loaded.
Actually, return the load argument, if any; this is sometimes the name of a
Lisp file without an extension.  If the feature came from an eval-buffer on
a buffer with no associated file, or an eval-region, return nil."
  (if (not (featurep feature))
      (error "%s is not a currently loaded feature." (symbol-name feature))
    (car (feature-symbols feature))))

(defun file-provides (file)
  "Return the list of features provided by FILE."
  (let ((symbols (cdr (assoc file load-history))) (provides nil))
    (mapcar
     (function (lambda (x)
		 (if (and (consp x) (eq (car x) 'provide))
		     (setq provides (cons (cdr x) provides)))))
     symbols)
    provides
    ))

(defun file-requires (file)
  "Return the list of features required by FILE."
  (let ((symbols (cdr (assoc file load-history))) (requires nil))
    (mapcar
     (function (lambda (x)
		 (if (and (consp x) (eq (car x) 'require))
		     (setq requires (cons (cdr x) requires)))))
     symbols)
    requires
    ))

(defun set-intersect (p q)
  ;; Return the set intersection of two lists
  (let ((ret nil))
    (mapcar
     (function (lambda (x) (if (memq x q) (setq ret (cons x ret)))))
     p)
    ret
    ))

(defun file-dependents (file)
  "Return the list of loaded libraries that depend on FILE.
This can include FILE itself."
  (let ((provides (file-provides file)) (dependents nil))
    (mapcar
     (function (lambda (x) 
		 (if (set-intersect provides (file-requires (car x)))
		     (setq dependents (cons (car x) dependents)))))
     load-history)
    dependents
    ))

;;;###autoload
(defun unload-feature (feature &optional force)
  "Unload the library that provided FEATURE, restoring all its autoloads.
If the feature is required by any other loaded code, and optional FORCE
is nil, raise an error."
  (interactive "SFeature: ")
  (if (not (featurep feature))
      (error "%s is not a currently loaded feature." (symbol-name feature)))
  (if (not force)
      (let* ((file (feature-file feature))
	     (dependents (delete file (copy-sequence (file-dependents file)))))
	(if dependents
	    (error "Loaded libraries %s depend on %s."
		   (prin1-to-string dependents) file)
	    )))
  (let* ((flist (feature-symbols feature)) (file (car flist)))
    (mapcar
     (function (lambda (x) 
		 (cond ((stringp x) nil)
		       ((consp x)
			;; Remove any feature names that this file provided.
			(if (eq (car x) 'provide)
			    (setq features (delq (cdr x) features))))
		       ((boundp x) (makunbound x))
		       ((fboundp x)
			(fmakunbound x)
			(let ((aload (get x 'autoload)))
			  (if aload (fset x (cons 'autoload aload)))))))
	       )
     (cdr flist))))

(provide 'loadhist)

;;; loadhist.el ends here
