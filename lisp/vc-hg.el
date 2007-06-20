;;; vc-hg.el --- VC backend for the mercurial version control system

;; Copyright (C) 2006, 2007 Free Software Foundation, Inc.

;; Author: Ivan Kanis
;; Keywords: tools
;; Version: 1889

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

;; This is a mercurial version control backend

;;; Thanks:

;;; Bugs:

;;; Installation:

;;; Todo:

;; Implement the rest of the vc interface

;; Implement Stefan Monnier's advice: 
;; vc-hg-registered and vc-hg-state
;; Both of those functions should be super extra careful to fail gracefully in
;; unexpected circumstances.  The most important such case is when the `hg'
;; executable is not available.  The reason this is important is that any error
;; there will prevent the user from even looking at the file :-(
;; Ideally, just like in vc-arch and vc-cvs, checking that the file is under
;; mercurial's control and extracting the current revision should be done
;; without even using `hg' (this way even if you don't have `hg' installed,
;; Emacs is able to tell you this file is under mercurial's control).

;;; History:
;; 

;;; Code:

(eval-when-compile
  (require 'vc))

;; XXX This should be moved to vc-hooks when the full vc interface is
;; implemented.
(add-to-list 'vc-handled-backends 'HG)

;;; Customization options

(defcustom vc-hg-global-switches nil
  "*Global switches to pass to any Hg command."
  :type '(choice (const :tag "None" nil)
         (string :tag "Argument String")
         (repeat :tag "Argument List"
             :value ("")
             string))
;;  :version "22.2"
  :group 'vc)

;;; State querying functions

;;; Modelled after the similar function in vc-bzr.el
(defun vc-hg-registered (file)
  "Return non-nil if FILE is registered with hg."
  (if (vc-find-root file ".hg")       ; short cut
      (vc-hg-state file)))            ; expensive

(defun vc-hg-state (file)
  "Hg-specific version of `vc-state'."
  (let ((out (vc-hg-internal-status file)))
    (if (eq 0 (length out)) 'up-to-date
      (let ((state (aref out 0)))
        (cond
         ((eq state ?M) 'edited)
         ((eq state ?A) 'edited)
         ((eq state ?P) 'needs-patch)
	 ((eq state ??) nil)
         (t 'up-to-date))))))

(defun vc-hg-workfile-version (file)
  "Hg-specific version of `vc-workfile-version'."
  (let ((out (vc-hg-internal-log file)))
    (if (string-match "changeset: *\\([0-9]*\\)" out)
        (match-string 1 out)
      "0")))

;;; History functions

(defun vc-hg-print-log(file &optional buffer)
  "Get change log associated with FILE."
  (vc-hg-command
   buffer
   (if (and (vc-stay-local-p file) (fboundp 'start-process)) 'async 0)
   file "log"))

(defun vc-hg-diff (file &optional oldvers newvers buffers)
  "Get a difference report using hg between two versions of FILE."
  (when buffers (message buffers))
  (unless buffers (setq buffers "*vc-diff*"))
  (when oldvers (message oldvers))
  (when newvers (message newvers))
  (call-process "hg" nil buffers nil
                "--cwd" (file-name-directory file)
                "diff" (file-name-nondirectory file)))

(defun vc-hg-register (file &optional rev comment)
  "Register FILE under hg.
REV is ignored.
COMMENT is ignored."
  (vc-hg-command nil nil file "add"))

;;; Modelled after the similar function in vc-bzr.el
(defun vc-hg-checkout (file &optional editable rev workfile)
  "Retrieve a revision of FILE into a WORKFILE.
EDITABLE is ignored.
REV is the revision to check out into WORKFILE."
  (unless workfile
    (setq workfile (vc-version-backup-file-name file rev)))
  (let ((coding-system-for-read 'binary)
        (coding-system-for-write 'binary))
  (with-temp-file workfile
    (if rev
        (vc-hg-command t nil file "cat" "-r" rev)
      (vc-hg-command t nil file "cat")))))

(defun vc-hg-checkout-model (file)
  'implicit)

;;; Internal functions

(defun vc-hg-command (buffer okstatus file &rest flags)
  "A wrapper around `vc-do-command' for use in vc-hg.el.
The difference to vc-do-command is that this function always invokes `hg',
and that it passes `vc-hg-global-switches' to it before FLAGS."
  (apply 'vc-do-command buffer okstatus "hg" file
         (if (stringp vc-hg-global-switches)
             (cons vc-hg-global-switches flags)
           (append vc-hg-global-switches
                   flags))))

(defun vc-hg-internal-log (file)
  "Return log of FILE."
  (with-output-to-string
    (with-current-buffer
        standard-output
      (call-process
       "hg" nil t nil "--cwd" (file-name-directory file)
       "log" "-l1" (file-name-nondirectory file)))))

(defun vc-hg-internal-status(file)
  "Return status of FILE."
  (with-output-to-string
    (with-current-buffer
        standard-output
      (call-process
       "hg" nil t nil "--cwd" (file-name-directory file)
       "status" (file-name-nondirectory file)))))

(provide 'vc-hg)

;;; vc-hg.el ends here
