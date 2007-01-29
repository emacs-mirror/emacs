;;; pcmpl-linux.el --- functions for dealing with GNU/Linux completions

;; Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007 Free Software Foundation, Inc.

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

;; These functions are for use with GNU/Linux.  Since they depend on a
;; certain knowledge of the layout of such systems, they probably
;; won't work very well on other operating systems.

;;; Code:

(provide 'pcmpl-linux)

(require 'pcomplete)

(defgroup pcmpl-linux nil
  "Functions for dealing with GNU/Linux completions."
  :group 'pcomplete)

;; Functions:

;;;###autoload
(defun pcomplete/kill ()
  "Completion for GNU/Linux `kill', using /proc filesystem."
  (if (pcomplete-match "^-\\(.*\\)" 0)
      (pcomplete-here
       (pcomplete-uniqify-list
	(split-string
	 (pcomplete-process-result "kill" "-l")))
       (pcomplete-match-string 1 0)))
  (while (pcomplete-here
	  (if (file-directory-p "/proc")
	      (let ((default-directory "/proc/"))
		(mapcar 'directory-file-name
			(pcomplete-entries "[0-9]+/$"))))
	  nil 'identity)))

;;;###autoload
(defun pcomplete/umount ()
  "Completion for GNU/Linux `umount'."
  (pcomplete-opt "hVafrnvt(pcmpl-linux-fs-types)")
  (while (pcomplete-here (pcmpl-linux-mounted-directories)
			 nil 'identity)))

;;;###autoload
(defun pcomplete/mount ()
  "Completion for GNU/Linux `mount'."
  (pcomplete-opt "hVanfFrsvwt(pcmpl-linux-fs-types)o?L?U?")
  (while (pcomplete-here (pcomplete-entries) nil 'identity)))

(defun pcmpl-linux-fs-types ()
  "Return a list of available fs modules on GNU/Linux systems."
  (let ((kernel-ver (pcomplete-process-result "uname" "-r")))
    (mapcar
     (function
      (lambda (fsobj)
	(substring fsobj 0 (- (length fsobj) 2))))
     (let ((default-directory
	     (concat "/lib/modules/" kernel-ver "/fs/")))
       (pcomplete-entries "\\.o$")))))

(defun pcmpl-linux-mounted-directories ()
  "Return a list of mounted directory names."
  (let (points)
    (when (file-readable-p "/etc/mtab")
      (with-temp-buffer
	(insert-file-contents-literally "/etc/mtab")
	(while (not (eobp))
	  (let* ((line (buffer-substring (point) (line-end-position)))
		 (args (split-string line " ")))
	    (setq points (cons (nth 1 args) points)))
	  (forward-line)))
      (pcomplete-uniqify-list points))))

(defun pcmpl-linux-mountable-directories ()
  "Return a list of mountable directory names."
  (let (points)
    (when (file-readable-p "/etc/fstab")
      (with-temp-buffer
	(insert-file-contents-literally "/etc/fstab")
	(while (not (eobp))
	  (let* ((line (buffer-substring (point) (line-end-position)))
		 (args (split-string line "\\s-+")))
	    (setq points (cons (nth 1 args) points)))
	  (forward-line)))
      (pcomplete-pare-list
       (pcomplete-uniqify-list points)
       (cons "swap" (pcmpl-linux-mounted-directories))))))

;;; arch-tag: bb0961a6-a623-463d-92c6-497c317293b1
;;; pcmpl-linux.el ends here
