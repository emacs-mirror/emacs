;;; url-privacy.el --- Global history tracking for URL package
;; Author: $Author: fx $
;; Created: $Date: 2001/10/05 17:10:26 $
;; Version: $Revision: 1.4 $
;; Keywords: comm, data, processes, hypermedia

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1993 - 1996 by William M. Perry <wmperry@cs.indiana.edu>
;;; Copyright (c) 1996 - 1999 Free Software Foundation, Inc.
;;;
;;; This file is part of GNU Emacs.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA 02111-1307, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'cl))
(require 'url-vars)

(if (fboundp 'device-type)
    (defalias 'url-device-type 'device-type)
  (defun url-device-type (&optional device) (or window-system 'tty)))

;;;###autoload
(defun url-setup-privacy-info ()
  (interactive)
  (setq url-system-type
	(cond
	 ((or (eq url-privacy-level 'paranoid)
	      (and (listp url-privacy-level)
		   (memq 'os url-privacy-level)))
	  nil)
	 ;; First, we handle the inseparable OS/Windowing system
	 ;; combinations
	 ((eq system-type 'Apple-Macintosh) "Macintosh")
	 ((eq system-type 'next-mach) "NeXT")
	 ((eq system-type 'windows-nt) "Windows-NT; 32bit")
	 ((eq system-type 'ms-windows) "Windows; 16bit")
	 ((eq system-type 'ms-dos) "MS-DOS; 32bit")
	 ((memq (url-device-type) '(win32 w32)) "Windows; 32bit")
	 ((eq (url-device-type) 'pm) "OS/2; 32bit")
	 (t
	  (case (url-device-type)
	    (x "X11")
	    (ns "OpenStep")
	    (tty "TTY")
	    (otherwise nil)))))

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
	 ((boundp 'system-configuration)
	  system-configuration)
	 ((boundp 'system-type)
	  (symbol-name system-type))
	 (t nil))))

(provide 'url-privacy)
