;;; version.el --- record version number of Emacs

;;; Copyright (C) 1985, 1992, 1994, 1995, 1999, 2000, 2001
;;;   Free Software Foundation, Inc.

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(defconst emacs-version "22.0.0" "\
Version numbers of this version of Emacs.")

(defconst emacs-major-version
  (progn (string-match "^[0-9]+" emacs-version)
	 (string-to-int (match-string 0 emacs-version)))
  "Major version number of this version of Emacs.
This variable first existed in version 19.23.")

(defconst emacs-minor-version
  (progn (string-match "^[0-9]+\\.\\([0-9]+\\)" emacs-version)
	 (string-to-int (match-string 1 emacs-version)))
  "Minor version number of this version of Emacs.
This variable first existed in version 19.23.")

(defconst emacs-build-time (current-time) "\
Time at which Emacs was dumped out.")

(defconst emacs-build-system (system-name))

(defun emacs-version  (&optional here) "\
Return string describing the version of Emacs that is running.
If optional argument HERE is non-nil, insert string at point.
Don't use this function in programs to choose actions according
to the system configuration; look at `system-configuration' instead."
  (interactive "P")
  (let ((version-string 
         (format (if (not (interactive-p))
		     "GNU Emacs %s (%s%s%s)\n of %s on %s"
		   "GNU Emacs %s (%s%s%s) of %s on %s")
                 emacs-version
		 system-configuration
		 (cond ((featurep 'motif) 
			(concat ", " (substring motif-version-string 4)))
		       ((featurep 'x-toolkit) ", X toolkit")
		       (t ""))
		 (if (and (boundp 'x-toolkit-scroll-bars)
			  (memq x-toolkit-scroll-bars '(xaw xaw3d)))
		     (format ", %s scroll bars"
			     (capitalize (symbol-name x-toolkit-scroll-bars)))
		   "")
		 (format-time-string "%Y-%m-%d" emacs-build-time)
                 emacs-build-system)))
    (if here 
        (insert version-string)
      (if (interactive-p)
          (message "%s" version-string)
        version-string))))

;; We hope that this alias is easier for people to find.
(defalias 'version 'emacs-version)

;; We put version info into the executable in the form that `ident' uses.
(or (memq system-type '(vax-vms windows-nt))
    (purecopy (concat "\n$Id: " (subst-char-in-string ?\n ? (emacs-version))
		      " $\n")))

;;Local variables:
;;version-control: never
;;End:

;;; version.el ends here
