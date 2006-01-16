;;; tramp-ftp.el --- Tramp convenience functions for Ange-FTP -*- coding: iso-8859-1; -*-

;; Copyright (C) 2002, 2003, 2004, 2005 Free Software Foundation, Inc.

;; Author: Michael Albinus <michael.albinus@gmx.de>
;; Keywords: comm, processes

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

;; Convenience functions for calling Ange-FTP from Tramp.
;; Most of them are displaced from tramp.el.

;;; Code:

(require 'tramp)

(eval-when-compile
  (require 'cl)
  (require 'custom)
  ;; Emacs 19.34 compatibility hack -- is this needed?
  (or (>= emacs-major-version 20)
      (load "cl-seq")))

;; Disable Ange-FTP from file-name-handler-alist.
;; To handle EFS, the following functions need to be dealt with:
;;
;; * dired-before-readin-hook contains efs-dired-before-readin
;; * file-name-handler-alist contains efs-file-handler-function
;;   and efs-root-handler-function and efs-sifn-handler-function
;; * find-file-hooks contains efs-set-buffer-mode
;;
;; But it won't happen for EFS since the XEmacs maintainers
;; don't want to use a unified filename syntax.
(defun tramp-disable-ange-ftp ()
  "Turn Ange-FTP off.
This is useful for unified remoting.  See
`tramp-file-name-structure-unified' and
`tramp-file-name-structure-separate' for details.  Requests suitable
for Ange-FTP will be forwarded to Ange-FTP.  Also see the variables
`tramp-ftp-method', `tramp-default-method', and
`tramp-default-method-alist'.

This function is not needed in Emacsen which include Tramp, but is
present for backward compatibility."
  (let ((a1 (rassq 'ange-ftp-hook-function file-name-handler-alist))
	(a2 (rassq 'ange-ftp-completion-hook-function file-name-handler-alist)))
    (setq file-name-handler-alist
	  (delete a1 (delete a2 file-name-handler-alist)))))
(tramp-disable-ange-ftp)
(eval-after-load "ange-ftp" '(tramp-disable-ange-ftp))

;; Define FTP method ...
(defcustom tramp-ftp-method "ftp"
  "*When this method name is used, forward all calls to Ange-FTP."
  :group 'tramp
  :type 'string)

;; ... and add it to the method list.
(add-to-list 'tramp-methods (cons tramp-ftp-method nil))

;; Add some defaults for `tramp-default-method-alist'
(add-to-list 'tramp-default-method-alist
	     (list "\\`ftp\\." "" tramp-ftp-method))
(add-to-list 'tramp-default-method-alist
	     (list "" "\\`\\(anonymous\\|ftp\\)\\'" tramp-ftp-method))

;; Add completion function for FTP method.
(unless (memq system-type '(windows-nt))
  (tramp-set-completion-function
   tramp-ftp-method
   '((tramp-parse-netrc "~/.netrc"))))

(defun tramp-ftp-file-name-handler (operation &rest args)
  "Invoke the Ange-FTP handler for OPERATION.
First arg specifies the OPERATION, second arg is a list of arguments to
pass to the OPERATION."
  (save-match-data
    (or (boundp 'ange-ftp-name-format)
	(require 'ange-ftp))
    (let ((ange-ftp-name-format
	   (list (nth 0 tramp-file-name-structure)
		 (nth 3 tramp-file-name-structure)
		 (nth 2 tramp-file-name-structure)
		 (nth 4 tramp-file-name-structure)))
	  ;; ange-ftp uses `ange-ftp-ftp-name-arg' and `ange-ftp-ftp-name-res'
	  ;; for optimization in `ange-ftp-ftp-name'. If Tramp wasn't active,
	  ;; there could be incorrect values from previous calls in case the
	  ;; "ftp" method is used in the Tramp file name. So we unset
	  ;; those values.
	  (ange-ftp-ftp-name-arg "")
	  (ange-ftp-ftp-name-res nil))
      (cond
       ;; If argument is a symlink, `file-directory-p' and `file-exists-p'
       ;; call the traversed file recursively. So we cannot disable the
       ;; file-name-handler this case.
       ((memq operation '(file-directory-p file-exists-p))
	(apply 'ange-ftp-hook-function operation args))
	;; Normally, the handlers must be discarded
	(t (let* ((inhibit-file-name-handlers
		   (list 'tramp-file-name-handler
			 'tramp-completion-file-name-handler
			 (and (eq inhibit-file-name-operation operation)
			      inhibit-file-name-handlers)))
		  (inhibit-file-name-operation operation))
	     (apply 'ange-ftp-hook-function operation args)))))))

(defun tramp-ftp-file-name-p (filename)
  "Check if it's a filename that should be forwarded to Ange-FTP."
  (let ((v (tramp-dissect-file-name filename)))
    (string=
     (tramp-find-method
      (tramp-file-name-multi-method v)
      (tramp-file-name-method v)
      (tramp-file-name-user v)
      (tramp-file-name-host v))
     tramp-ftp-method)))

(add-to-list 'tramp-foreign-file-name-handler-alist
	     (cons 'tramp-ftp-file-name-p 'tramp-ftp-file-name-handler))

(provide 'tramp-ftp)

;;; TODO:

;; * In case of "/ftp:host:file" this works only for functions which
;;   are defined in `tramp-file-name-handler-alist'.  Call has to be
;;   pretended in `tramp-file-name-handler' otherwise.
;;   Furthermore, there are no backup files on FTP hosts.
;;   Worth further investigations.
;; * Map /multi:ssh:out@gate:ftp:kai@real.host:/path/to.file
;;   on Ange-FTP gateways.

;;; arch-tag: 759fb338-5c63-4b99-bd36-b4d59db91cff
;;; tramp-ftp.el ends here
