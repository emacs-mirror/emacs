;;; gnus-mule.el --- Provide backward compatibility function to GNUS

;; Copyright (C) 1995,1997 Free Software Foundation, Inc.
;; Copyright (C) 1995, 2000 Electrotechnical Laboratory, JAPAN.

;; Keywords: gnus, mule

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

;;; Commentary:

;; This file provides the function `gnus-mule-add-group' for backward
;; compatibility with old version of Gnus included in Emacs 20.

(require 'gnus-sum)

;;;###autoload
(defun gnus-mule-add-group (name coding-system)
  "Specify that articles of news group NAME are encoded in CODING-SYSTEM.
All news groups deeper than NAME are also the target.
If CODING-SYSTEM is a cons, the car part is used and the cdr
part is ignored.

This function exists for backward comaptibility with Emacs 20.  It is
recommended to customize the variable `gnus-group-charset-alist'
rather than using this function."
  (if (consp coding-system)
      ;; Ignore the cdr part because now Gnus can't use different
      ;; coding systems for encoding and decoding.
      (setq coding-system (car coding-system)))
  (let ((tail gnus-group-charset-alist)
	(prev nil)
	(pattern (concat "^" (regexp-quote name))))
    ;; Check entries of `gnus-group-charset-alist' if they match NAME.
    (while (not (string-match (car (car tail)) name))
      (setq prev tail tail (cdr tail)))
    (if tail
	;; A matching entry was found.
	(if (string= pattern (car (car tail)))
	    ;; We can modify this entry.
	    (setcar (cdr (car tail)) coding-system)
	  ;; We must add a new entry before this.
	  (if prev
	      (setcdr prev (cons (list pattern coding-system)
				 (cdr prev)))
	    (setq gnus-group-charset-alist
		  (cons (list pattern coding-system)
			gnus-group-charset-alist))))
      ;; We must prepend a new entry.
      (setq gnus-group-charset-alist
	    (cons (list pattern coding-system)
		  gnus-group-charset-alist)))))

(provide 'gnus-mule)

;; gnus-mule.el ends here
