;;; url-privacy.el --- Global history tracking for URL package  -*- lexical-binding: t; -*-

;; Copyright (C) 1996-1999, 2004-2024 Free Software Foundation, Inc.

;; Keywords: comm, data, processes, hypermedia

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'url-vars)

(defun url-device-type (&optional _device)
  (declare (obsolete nil "27.1"))
  (or window-system 'tty))

;;;###autoload
(defun url-setup-privacy-info ()
  "Setup variables that expose info about you and your system."
  (interactive)
  (setq url-system-type
	(cond
	 ((or (eq url-privacy-level 'paranoid)
	      (and (listp url-privacy-level)
		   (memq 'os url-privacy-level)))
	  nil)
	 ;; First, we handle the inseparable OS/Windowing system
	 ;; combinations
	 ((memq system-type '(windows-nt cygwin))
          (concat "MS-Windows; "
                  (if (string-match-p "\\`x86_64" system-configuration)
                      "64bit"
                    "32bit")
                  "; "
                  (cond ((eq window-system 'w32) "w32")
                        ((eq window-system 'x) "X11")
                        (t "TTY"))))
	 ((eq system-type 'ms-dos) "MS-DOS; 32bit")
	 (t
	  (pcase (or window-system 'tty)
	    ('x "X11")
	    ('ns "OpenStep")
            ('pgtk "PureGTK")
	    ('tty "TTY")
	    (_ nil)))))

  (setq url-personal-mail-address (or url-personal-mail-address
				      user-mail-address
				      (format "%s@%s"  (user-real-login-name)
					      (system-name))))

  (if (or (memq url-privacy-level '(paranoid high))
	  (and (listp url-privacy-level)
	       (memq 'email url-privacy-level)))
      (setq url-personal-mail-address nil))

  (setq url-os-type
	(cond
	 ((or (eq url-privacy-level 'paranoid)
	      (and (listp url-privacy-level)
		   (memq 'os url-privacy-level)))
	  nil)
	 ((boundp 'system-configuration) system-configuration)
	 ((boundp 'system-type) (symbol-name system-type))
	 (t nil))))

(provide 'url-privacy)

;;; url-privacy.el ends here
