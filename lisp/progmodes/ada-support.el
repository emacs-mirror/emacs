;; @(#) ada-support.el --- Override some standard Emacs functions

;; Copyright (C) 1994-1999 Free Software Foundation, Inc.

;; Author: Emmanuel Briot <briot@gnat.com>
;; Maintainer: Emmanuel Briot <briot@gnat.com>
;; Ada Core Technologies's version:   $Revision: 1.3 $
;; Keywords: languages ada xref

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;; This file overrides some functions that are defined in Emacs/XEmacs,
;;; since some of them have known bugs in old versions.
;;; This is intended as a support package for older Emacs versions, and
;;; should not be needed for the latest version of Emacs (currently 20.4)
;;; where these bugs have been fixed



;;; Some functions have been renamed from one version to the other
;;; `easy-menu-create-keymaps' has been renamed `easy-menu-create-menu'
;;;  from Emacs >= 20.3
;;; Do nothing for XEmacs

(unless (or (ada-check-emacs-version 20 3)
	    (not (ada-check-emacs-version 1 1 t)))
  
  (if (and (not (fboundp 'easy-menu-create-menu))
	   (fboundp 'easy-menu-create-keymaps))
        (defun easy-menu-create-menu (menu-name menu-items)
	  "Alias redefined in ada-support.el"
          (easy-menu-create-keymaps menu-name menu-items))))



;;; A fix for Emacs <= 20.3
;;; Imenu does not support name overriding in submenus (the first such name
;;; is always selected, whichever the user actually chose).
;;; This has been fixed in Emacs 20.4
;;; Fix was: use assq instead of assoc in the submenus

(unless (ada-check-emacs-version 20 4)

  (defun imenu--mouse-menu (index-alist event &optional title)
    "Overrides the default imenu--mouse-menu from imenu.el, that has a bug.
The default one does not know anything about overriding in submenus, since
it is using assoc instead of assq"
    (set 'index-alist (imenu--split-submenus index-alist))
    (let* ((menu  (imenu--split-menu index-alist
				     (or title (buffer-name))))
	   position)
      (set 'menu (imenu--create-keymap-1 (car menu)
					 (if (< 1 (length (cdr menu)))
					     (cdr menu)
					   (cdr (car (cdr menu))))))
      (set 'position (x-popup-menu event menu))
      (cond ((eq position nil)
	     position)
	    ;; If one call to x-popup-menu handled the nested menus,
	    ;; find the result by looking down the menus here.
	    ((and (listp position)
		  (numberp (car position))
		  (stringp (nth (1- (length position)) position)))
	     (let ((final menu))
	       (while position
		 (set 'final (assq (car position) final))
		 (set 'position (cdr position)))
	       (or (string= (car final) (car imenu--rescan-item))
		   (nthcdr 3 final))))
	    ;; If x-popup-menu went just one level and found a leaf item,
	    ;; return the INDEX-ALIST element for that.
	    ((and (consp position)
		  (stringp (car position))
		  (null (cdr position)))
	     (or (string= (car position) (car imenu--rescan-item))
		 (assq (car position) index-alist)))
	    ;; If x-popup-menu went just one level
	    ;; and found a non-leaf item (a submenu),
	    ;; recurse to handle the rest.
	    ((listp position)
	     (imenu--mouse-menu position event
				(if title
				    (concat title imenu-level-separator
					    (car (rassq position index-alist)))
				  (car (rassq position index-alist))))))))
  )

(provide 'ada-support)