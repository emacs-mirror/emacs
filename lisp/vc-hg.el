;;; vc-hg.el --- VC backend for the mercurial version control system

;; Copyright (C) 2006  Ivan Kanis
;; Author: Ivan Kanis
;; $Id: vc-hg.el 1889 2007-06-17 12:39:26Z ivan $
;;
;; This program is free software ; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation ; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY ; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program ; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

;;; Commentary:

;; This is a mercurial version control backend

;;; THANKS:

;;; BUGS:

;;; INSTALLATION:

;;; Code:

(eval-when-compile
  (require 'vc))

;; (setq vc-handled-backends '(CVS SVN hg))

;;; Customization options

(defcustom vc-hg-global-switches nil
  "*Global switches to pass to any Hg command."
  :type '(choice (const :tag "None" nil)
         (string :tag "Argument String")
         (repeat :tag "Argument List"
             :value ("")
             string))
  :version "22.1"
  :group 'vc)

;;; State querying functions

(defun vc-hg-registered (file)
  "Return t if FILE is registered in Hg"
  (if (eq 0 (call-process "hg" nil nil nil
                "--cwd" (file-name-directory file)
                "status" (file-name-nondirectory file)))
      (vc-file-setprop file 'vc-name file) nil))

(defun vc-hg-state (file)
  "Return state of files in Hg"
  (let ((out (vc-hg-internal-status file)))
    (if (eq 0 (length out)) 'up-to-date
      (let ((state (aref out 0)))
        (cond
         ((eq state ?M) 'edited)
         ((eq state ?P) 'needs-patch)
         (t 'up-to-date))))))

(defun vc-hg-workfile-version (file)
  "Return version number of file"
  (let ((out (vc-hg-internal-log file)))
    (if (string-match "changeset: *\\([0-9]*\\)" out)
        (match-string 1 out)
      "0")))

(defun vc-hg-internal-log(file)
  "Return log of FILE"
  (with-output-to-string
    (with-current-buffer
        standard-output
      (call-process
       "hg" nil t nil "--cwd" (file-name-directory file)
       "log" "-l1" (file-name-nondirectory file)))))

;;; History functions

(defun vc-hg-print-log(file &optional buffer)
  "Get change log associated with FILE."
  (vc-hg-command
   buffer
   (if (and (vc-stay-local-p file) (fboundp 'start-process)) 'async 0)
   file "log"))

(defun vc-hg-internal-status(file)
  "Return status of FILE"
  (with-output-to-string
    (with-current-buffer
        standard-output
      (call-process
       "hg" nil t nil "--cwd" (file-name-directory file)
       "status" (file-name-nondirectory file)))))

(defun vc-hg-diff (file &optional oldvers newvers buffers)
  "Get a difference report using hg between two versions of FILE."
  (when buffers (message buffers))
  (unless buffers (setq buffers "*vc-diff*"))
  (when oldvers (message oldvers))
  (when newvers (message newvers))
  (call-process "hg" nil buffers nil
                "--cwd" (file-name-directory file)
                "diff" (file-name-nondirectory file)))

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

(provide 'vc-hg)

