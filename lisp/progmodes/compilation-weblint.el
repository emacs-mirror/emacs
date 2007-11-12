;;; compilation-weblint.el --- error regexps for weblint

;; Copyright (C) 2007 Free Software Foundation, Inc.

;; Author: Kevin Ryde <user42@zip.com.au>
;; Version: 1
;; Keywords: processes
;; URL: http://www.geocities.com/user42_kevin/compilation/index.html
;; EmacsWiki: CompilationMode

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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

;; This is a spot of code adding a `compilation-error-regexp-alist'
;; pattern for messages from the weblint program (the one based on the
;; perl HTML::Lint modules).

;;; Install:

;; Put compilation-weblint.el somewhere in your `load-path', and in
;; .emacs put
;;
;;     (eval-after-load "compile" '(require 'compilation-weblint))
;;
;; There's an autoload cookie below for this, if you use
;; `update-file-autoloads' and friends.

;;; Emacsen:

;; Works in Emacs 22, Emacs 21, and XEmacs 21.

;;; History:

;; Version 1 - the first version.


;;; Code:

;;;###autoload (eval-after-load "compile" '(require 'compilation-weblint))

(eval-after-load "compile"
  '(progn
     ;; The style comes from HTML::Lint::Error::as_string(), eg.
     ;; index.html (13:1) Unknown element <fdjsk>
     ;;
     ;; The pattern only matches filenames without spaces, since that
     ;; should be usual and should help reduce the chance of a false
     ;; match of a message from some unrelated program.
     ;;
     ;; This message style is quite close to the "ibm" entry of
     ;; emacs22 `compilation-error-regexp-alist-alist' which is for
     ;; IBM C, though that ibm bit doesn't put a space after the
     ;; filename.
     ;;
     (let ((elem '(compilation-weblint
		   "^\\([^ \t\r\n(]+\\) (\\([0-9]+\\):\\([0-9]+\\)) "
		   1 2 3)))

       (cond
	((boundp 'compilation-error-regexp-systems-list)
	 ;; xemacs21
	 (add-to-list 'compilation-error-regexp-alist-alist
		      (list (car elem) (cdr elem)))
	 (compilation-build-compilation-error-regexp-alist))

	((boundp 'compilation-error-regexp-alist-alist)
	 ;; emacs22
	 (add-to-list 'compilation-error-regexp-alist-alist elem)
	 (add-to-list 'compilation-error-regexp-alist (car elem)))

	(t
	 ;; emacs21
	 (add-to-list 'compilation-error-regexp-alist (cdr elem))

	 ;; Remove the "4.3BSD lint pass 3" element because it wrongly
	 ;; matches weblint messages.  It's apparently supposed to
	 ;; match something like
	 ;;
	 ;;     bloofle defined( /users/wolfgang/foo.c(4) ) ...
	 ;;
	 ;; but it's rather loose and ends up matching the "(13:1)"
	 ;; part from weblint as if "13" is the filename and "1" is
	 ;; the line number.  Forcibly removing this is a bit nasty,
	 ;; but emacs22 has dropped it, so consider it an upgrade!
	 ;;
	 ;; xemacs21 has the same pattern, but somehow the problem
	 ;; doesn't arise, so leave it alone there, for now.
	 ;;
	 (setq compilation-error-regexp-alist
		    (remove '(".*([ \t]*\\([a-zA-Z]?:?[^:( \t\n]+\\)[:(][ \t]*\\([0-9]+\\))" 1 2)
			    compilation-error-regexp-alist)))))))

(provide 'compilation-weblint)

;;; compilation-weblint.el ends here
