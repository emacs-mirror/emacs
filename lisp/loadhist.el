;;; loadhist.el --- lisp functions for working with feature groups

;; Copyright (C) 1995, 1998, 2000, 2005 Free Software Foundation, Inc.

;; Author: Eric S. Raymond <esr@snark.thyrsus.com>
;; Maintainer: FSF
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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; These functions exploit the load-history system variable.
;; Entry points include `unload-feature', `symbol-file', and
;; `feature-file', documented in the Emacs Lisp manual.

;;; Code:

(defun feature-symbols (feature)
  "Return the file and list of definitions associated with FEATURE.
The value is actually the element of `load-history'
for the file that did (provide FEATURE)."
   (catch 'foundit
     (mapc (lambda (x)
	     (if (member (cons 'provide feature) (cdr x))
		 (throw 'foundit x)))
	   load-history)
     nil))

(defun feature-file (feature)
  "Return the file name from which a given FEATURE was loaded.
Actually, return the load argument, if any; this is sometimes the name of a
Lisp file without an extension.  If the feature came from an `eval-buffer' on
a buffer with no associated file, or an `eval-region', return nil."
  (if (not (featurep feature))
      (error "%S is not a currently loaded feature" feature)
    (car (feature-symbols feature))))

(defun file-loadhist-lookup (file)
  "Return the `load-history' element for FILE."
  ;; First look for FILE as given.
  (let ((symbols (assoc file load-history)))
    ;; Try converting a library name to an absolute file name.
    (and (null symbols)
	 (let ((absname (find-library-name file)))
	   (if (not (equal absname file))
	       (setq symbols (cdr (assoc absname load-history))))))
    ;; Try converting an absolute file name to a library name.
    (and (null symbols) (string-match "[.]el\\'" file)
	 (let ((libname (file-name-nondirectory file)))
	   (string-match "[.]el\\'" libname)
	   (setq libname (substring libname 0 (match-beginning 0)))
	   (setq symbols (cdr (assoc libname load-history)))))
    symbols))

(defun file-provides (file)
  "Return the list of features provided by FILE."
  (let ((symbols (file-loadhist-lookup file))
	provides)
    (mapc (lambda (x)
	    (if (and (consp x) (eq (car x) 'provide))
		(setq provides (cons (cdr x) provides))))
	  symbols)
    provides))

(defun file-requires (file)
  "Return the list of features required by FILE."
  (let ((symbols (file-loadhist-lookup file))
	requires)
    (mapc (lambda (x)
	    (if (and (consp x) (eq (car x) 'require))
		(setq requires (cons (cdr x) requires))))
	  symbols)
    requires))

(defsubst file-set-intersect (p q)
  "Return the set intersection of two lists."
  (let ((ret nil))
    (dolist (x p ret)
      (if (memq x q) (setq ret (cons x ret))))
    ret))

(defun file-dependents (file)
  "Return the list of loaded libraries that depend on FILE.
This can include FILE itself."
  (let ((provides (file-provides file))
	(dependents nil))
    (dolist (x load-history dependents)
      (if (file-set-intersect provides (file-requires (car x)))
	  (setq dependents (cons (car x) dependents))))
    dependents))

(defun read-feature (prompt)
  "Read a feature name \(string\) from the minibuffer.
Prompt with PROMPT and completing from `features', and
return the feature \(symbol\)."
  (intern (completing-read prompt
			   (mapcar (lambda (feature)
				     (list (symbol-name feature)))
				   features)
			   nil t)))

(defvaralias 'loadhist-hook-functions 'unload-feature-special-hooks)
(defvar unload-feature-special-hooks
  '(after-change-functions
    after-insert-file-functions auto-fill-function
    before-change-functions blink-paren-function
    buffer-access-fontify-functions command-line-functions
    comment-indent-function kill-buffer-query-functions
    kill-emacs-query-functions lisp-indent-function
    mouse-position-function
    redisplay-end-trigger-functions temp-buffer-show-function
    window-scroll-functions window-size-change-functions
    write-region-annotate-functions)
  "A list of special hooks from Info node `(elisp)Standard Hooks'.

These are symbols with hook-type values whose names don't end in
`-hook' or `-hooks', from which `unload-feature' tries to remove
pertinent symbols.")

(defvar unload-hook-features-list nil
  "List of features of the package being unloaded.

This is meant to be used by FEATURE-unload-hook hooks, see the
documentation of `unload-feature' for details.")

;;;###autoload
(defun unload-feature (feature &optional force)
  "Unload the library that provided FEATURE, restoring all its autoloads.
If the feature is required by any other loaded code, and prefix arg FORCE
is nil, raise an error.

This function tries to undo modifications made by the package to
hooks.  Packages may define a hook FEATURE-unload-hook that is called
instead of the normal heuristics for doing this.  Such a hook should
undo all the relevant global state changes that may have been made by
loading the package or executing functions in it.  It has access to
the package's feature list (before anything is unbound) in the
variable `unload-hook-features-list' and could remove features from it
in the event that the package has done something normally-ill-advised,
such as redefining an Emacs function."
  (interactive (list (read-feature "Feature: ") current-prefix-arg))
  (unless (featurep feature)
    (error "%s is not a currently loaded feature" (symbol-name feature)))
  (unless force
    (let* ((file (feature-file feature))
	   (dependents (delete file (copy-sequence (file-dependents file)))))
      (when dependents
	(error "Loaded libraries %s depend on %s"
	       (prin1-to-string dependents) file))))
  (let* ((unload-hook-features-list (feature-symbols feature))
         (file (pop unload-hook-features-list))
         (unload-hook (intern-soft (concat (symbol-name feature)
                                           "-unload-hook"))))
    ;; Try to avoid losing badly when hooks installed in critical
    ;; places go away.  (Some packages install things on
    ;; `kill-buffer-hook', `activate-menubar-hook' and the like.)
    ;; First off, provide a clean way for package FOO to arrange
    ;; this by adding hooks on the variable `FOO-unload-hook'.
    (if unload-hook
        (run-hooks unload-hook)
      ;; Otherwise, do our best.  Look through the obarray for symbols
      ;; which seem to be hook variables or special hook functions and
      ;; remove anything from them which matches the feature-symbols
      ;; about to get zapped.  Obviously this won't get anonymous
      ;; functions which the package might just have installed, and
      ;; there might be other important state, but this tactic
      ;; normally works.
      (mapatoms
       (lambda (x)
         (when (and (boundp x)
		    (or (and (consp (symbol-value x)) ; Random hooks.
			     (string-match "-hooks?\\'" (symbol-name x)))
			(memq x unload-feature-special-hooks)))	; Known abnormal hooks etc.
	   (dolist (y unload-hook-features-list)
	     (when (and (eq (car-safe y) 'defun)
			(not (get (cdr y) 'autoload)))
	       (remove-hook x (cdr y)))))))
      ;; Remove any feature-symbols from auto-mode-alist as well.
      (dolist (y unload-hook-features-list)
	(when (and (eq (car-safe y) 'defun)
		   (not (get (cdr y) 'autoload)))
	  (setq auto-mode-alist
		(rassq-delete-all (cdr y) auto-mode-alist)))))
    (when (fboundp 'elp-restore-function) ; remove ELP stuff first
      (dolist (elt unload-hook-features-list)
	(when (symbolp elt)
	  (elp-restore-function elt))))
    (dolist (x unload-hook-features-list)
      (if (consp x)
	  (cond
	   ;; Remove any feature names that this file provided.
	   ((eq (car x) 'provide)
	    (setq features (delq (cdr x) features)))
	   ((eq (car x) 'defun)
	    (let ((fun (cdr x)))
	      (when (fboundp fun)
		(when (fboundp 'ad-unadvise)
		  (ad-unadvise fun))
		(fmakunbound fun)
		(let ((aload (get fun 'autoload)))
		  (when aload
		    (fset fun (cons 'autoload aload))))))))
	;; Kill local values as much as possible.
	(dolist (buf (buffer-list))
	  (with-current-buffer buf
	    (kill-local-variable x)))
	;; Get rid of the default binding if we can.
	(unless (local-variable-if-set-p x)
	  (makunbound x))))
    ;; Delete the load-history element for this file.
    (setq load-history (delq (assoc file load-history) load-history))))

(provide 'loadhist)

;;; arch-tag: 70bb846a-c413-4f01-bf88-78dba4ac0798
;;; loadhist.el ends here
