;;; mspools.el --- Show mail spools waiting to be read

;; Copyright (C) 1997 Free Software Foundation, Inc.

;; Author: Stephen Eglen <stephen@cns.ed.ac.uk>
;; Maintainer: Stephen Eglen <stephen@cns.ed.ac.uk>
;; Created: 22 Jan 1997
;; Keywords: mail

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

;; If you use a mail filter (e.g. procmail, filter) to put mail messages in
;; folders, this file will let you see which folders have mail waiting
;; to be read in them.  It assumes that new mail for the file `folder'
;; is written by the filter to a file called `folder.spool'.  (If the
;; file writes directly to `folder' you may lose mail if new mail
;; arrives whilst you are reading the folder in emacs, hence the use
;; of a spool file.)  For example, the following procmail recipe puts
;; any mail with `emacs' in the subject line into the spool file
;; `emacs.spool', ready to go into the folder `emacs'.
;:0:
;* ^Subject.*emacs
;emacs.spool

;; It also assumes that all of your spool files and mail folders live
;; in the directory pointed to by `mspools-folder-directory', so you must
;; set this (see Installation).

;; When you run `mspools-show', it creates a *spools* buffer containing
;; all of the spools in the folder directory that are waiting to be
;; read.  On each line is the spool name and its size in bytes.  Move
;; to the line of the folder that you would like to read, and then
;; press return or space.  The mailer (VM or RMAIL) should then read
;; that folder and get the new mail for you.  When you return to the
;; *spools* buffer, you will either see "*" to indicate that the spool
;; has been read, or the remaining unread spools, depending on the
;; value of `mspools-update'.

;; This file should work with both VM and RMAIL.  See the variable
;; `mspools-using-vm' for details.


;;; Installation

;; Basic
;(autoload 'mspools-show "mspools" "Show outstanding mail spools." t)
; Point to directory where spool files and folders are:
; (setq mspools-folder-directory "~/MAIL/")
;
; If you use VM, mspools-folder-directory will default to vm-folder-directory
; unless you have already given it a value.

;; Extras
; possibly bind it to a key:
;(global-set-key  '[S-f1] 'mspools-show)
;(setq mspools-update t)

;; Interface with the mail filter
; We assume that the mail filter drops new mail into the spool
; `folder.spool'.  If your spool files are something like folder.xyz
; for inbox `folder', then do
; (setq spool-suffix "xyz")
; If you use other conventions for your spool files, this code will
; need rewriting.

;;; Warning for VM users
;; Dont use if you are not sure what you are doing!  The value of
;; vm-spool-files is altered, so you may not be able to read incoming
;; mail with VM if this is incorrectly set.

;; Useful settings for VM
;vm-auto-get-new-mail should be t (default t)

;;; Acknowledgements
;; The code for setting up vm-spool-files came from 
;;http://www-users.informatik.rwth-aachen.de/~berg/archive/procmail/0047.html
;;  Thanks to jond@mitre.org (Jonathan Doughty)

;;; TODO

;; What if users have mail spools in more than one directory?  Extend
;; mspools-folder-directory to be a list of files?

;; I was going to add mouse support so that you could click on a line
;; to visit the buffer.  Tell me if you want it, and I can put the
;; code in (I dont use the mouse much, so I havent bothered with it so
;; far).


;; Rather than showing size in bytes, could we see the number of msgs
;; waiting?  (Could be more time demanding / system dependent).
;; Perl script counts the number of /^From / occurences.
;; ?
;; Include date
;; (substring  (current-time-string (nth 4 (file-attributes "~/INBOX")))  4 19)
;; Maybe just call a perl script to do all the hard work, and
;; visualise the results in the buffer.

;; Shrink wrap the buffer to remove excess white-space?


;;; User Variables


(defvar mspools-update nil
  "*Non-nil means update *spools* buffer after visiting any folder.")

(defvar mspools-suffix "spool"
  "*Extension used for spool files (not including full stop).")

;;; Internal Variables

(defvar mspools-vm-system-mail (getenv "MAIL")
  "Main mailbox used.  Only used by VM.")

(defvar mspools-vm-system-mail-crash 
  (concat mspools-vm-system-mail ".crash")
  "Crash box for main mailbox.  See also `mspools-vm-system-mail'.  
Only used by VM." )


(defvar mspools-files nil
  "List of entries (SPOOL . SIZE) giving spool name and file size.")

(defvar mspools-files-len nil
  "Length of `mspools-files' list.")

(defvar mspools-buffer "*spools*"
  "Name of buffer for displaying spool info.")

(defvar mspools-mode-map nil
  "Keymap for the *spools* buffer.")

(defvar mspools-folder-directory
  (if (boundp 'vm-folder-directory)
      vm-folder-directory
    nil)
  "Directory where mail folders are kept.  Defaults to
`vm-folder-directory' if bound else nil.  Make sure it has a trailing /
at the end. ")


(defvar mspools-using-vm 
  (fboundp 'vm)
  "*Non-nil if VM is used as mail reader, otherwise RMAIL is used.")


;;; Code

;;; VM Specific code
(if mspools-using-vm
    ;; set up vm if not already loaded.
    (progn
      (require 'vm-vars)
      (if (not vm-init-file-loaded)
	  (load-file vm-init-file))
      (if (not mspools-folder-directory)
	  (setq mspools-folder-directory vm-folder-directory))
      ))

(defun mspools-set-vm-spool-files ()
  "Set value of `vm-spool-files'.  Only needed for VM."
  (setq		
   vm-spool-files 
   (append
    (list
     ;; Main mailbox
     (list vm-primary-inbox
	   mspools-vm-system-mail; your mailbox
	   mspools-vm-system-mail-crash ; crash for mailbox
	   ))
    
    ;; Mailing list inboxes
    ;; must have VM already loaded to get vm-folder-directory.
    (mapcar '(lambda (s)
	       "make the appropriate entry for vm-spool-files"
	       (list
		(concat vm-folder-directory s)
		(concat vm-folder-directory s "." mspools-suffix)
		(concat vm-folder-directory s ".crash")))
	    ;; So I create a vm-spool-files entry for each of those mail drops
	    (mapcar 'file-name-sans-extension 
		    (directory-files vm-folder-directory nil 
				     (format "^[^.]+\\.%s" mspools-suffix)))
	    ))
   ))



;;; MSPOOLS-SHOW -- the main function
(defun mspools-show ( &optional noshow) 
  "Show the list of non-empty spool files in the *spools* buffer.
Buffer is not displayed if SHOW is non-nil."
  (interactive)
  (if (get-buffer mspools-buffer)
      ;; buffer exists
      (progn
	(set-buffer mspools-buffer)	
	(setq buffer-read-only nil)      
	(delete-region (point-min) (point-max)))
    ;; else buff. doesnt exist so create it
    (get-buffer-create mspools-buffer))
  
  ;; generate the list of spool files
  (if mspools-using-vm
      (mspools-set-vm-spool-files))
  
  (mspools-get-spool-files)
  (if (not noshow) (pop-to-buffer mspools-buffer))
  
  (setq buffer-read-only t)
  (mspools-mode)
  )




(defun mspools-visit-spool ()
  "Visit the folder on the current line of the *spools* buffer."
  (interactive)
  (let ( spool-name folder-name)
    (setq spool-name (mspools-get-spool-name))
    (setq folder-name (mspools-get-folder-from-spool spool-name))

    ;; put in a little "*" to indicate spool file has been read.
    (if (not mspools-update)
	(save-excursion
	  (setq buffer-read-only nil)
	  (beginning-of-line)
	  (insert "*")
	  (delete-char 1)
	  (setq buffer-read-only t)
	  ))
    

    (message "folder %s spool %s" folder-name spool-name)
    (if (eq (count-lines (point-min) 
			 (save-excursion
			   (end-of-line)
			   (point)))
	    mspools-files-len)
	(next-line (- 1 mspools-files-len)) ;back to top of list
      ;; else just on to next line
      (next-line 1))

    ;; Choose whether to use VM or RMAIL for reading folder.
    (if mspools-using-vm 
	(vm-visit-folder (concat mspools-folder-directory folder-name))
      ;; else using RMAIL 
      (rmail (concat mspools-folder-directory folder-name))
      (setq rmail-inbox-list 
	    (list (concat mspools-folder-directory spool-name)))
      (rmail-get-new-mail))
    
    
    (if mspools-update
	;; generate new list of spools.
	(save-excursion 
	  (mspools-show-again 'noshow)))
    ))




(defun mspools-get-folder-from-spool (name)
  "Return folder name corresponding to the spool file NAME."
  ;; Simply strip of the extension.
  (file-name-sans-extension name))

;; Alternative version if you have more complicated mapping of spool name
;; to file name.
;(defun get-folder-from-spool-safe (name)
;  "Return the folder name corresponding to the spool file NAME."
;  (if (string-match "^\\(.*\\)\.spool$" name)
;      (substring name (match-beginning 1) (match-end 1))
;    (error "Could not extract folder name from spool name %s" name)))

; test
;(mspools-get-folder-from-spool "happy.spool")
;(mspools-get-folder-from-spool "happy.sp")



(defun mspools-get-spool-name ()
  "Return the name of the spool on the current line."
  (let ((line-num (1- (count-lines (point-min)
				   (save-excursion
				     (end-of-line)
				     (point))
				   ))))
    (car (nth line-num mspools-files))))

;;; Keymap

(if mspools-mode-map
    ()
  (setq mspools-mode-map (make-sparse-keymap))
  
  (define-key mspools-mode-map "\C-c\C-c" 'mspools-visit-spool)
  (define-key mspools-mode-map "\C-m" 'mspools-visit-spool)
  (define-key mspools-mode-map " " 'mspools-visit-spool)
  (define-key mspools-mode-map "?" 'mspools-help)
  (define-key mspools-mode-map "q" 'mspools-quit)
  (define-key mspools-mode-map "g" 'revert-buffer))


;;; Spools mode functions  

(defun mspools-revert-buffer (ignore noconfirm)
  "Re-run mspools-show to revert the *spools* buffer."
  (mspools-show 'noshow))

(defun mspools-show-again (&optional noshow)
  "Update the *spools* buffer.  This is useful if mspools-update is
nil."
  (interactive)
  (mspools-show noshow))
  
(defun mspools-help ()
  "Show help for `mspools-mode'."
  (interactive)
  (describe-function 'mspools-mode))

(defun mspools-quit ()
  "Quit the *spools* buffer."
  (interactive)
  (kill-buffer mspools-buffer))
  

(defun mspools-mode ()
  "Major mode for output from mspools-show.
\\<mspools-mode-map>Move point to one of the items in this buffer, then use
\\[mspools-visit-spool] to go to the spool that the current line refers to.
\\[revert-buffer] to regenerate the list of spools.
\\{mspools-mode-map}"
  (kill-all-local-variables)
  (make-local-variable 'revert-buffer-function)
  (setq revert-buffer-function 'mspools-revert-buffer)
  (use-local-map mspools-mode-map)
  (setq major-mode 'mspools-mode)
  (setq mode-name "MSpools")
  )


(defun mspools-get-spool-files ()
  "Find the list of spool files and display them in *spools* buffer."
  (let (folders head spool len beg end any)
    (setq folders (directory-files mspools-folder-directory nil 
				   (format "^[^.]+\\.%s" mspools-suffix)))
    
    
    (setq folders (mapcar 'mspools-size-folder folders))
    (setq folders (delq nil folders))
    (setq mspools-files folders)
    (setq mspools-files-len (length mspools-files))
    (set-buffer mspools-buffer)
    (while folders
      (setq any t)
      (setq head (car folders))
      (setq spool (car head))
      (setq len (cdr head))
      (setq folders (cdr folders))
      (setq beg (point))
      (insert (format " %10d %s" len spool))
      (setq end (point))
      (insert "\n")
      ;;(put-text-property beg end 'mouse-face 'highlight)
      )
    (if any
	(delete-char -1))			;delete last RET
    (goto-char (point-min))
    ))



(defun mspools-size-folder (spool)
  "Return (SPOOL . SIZE ) iff SIZE of spool file is non-zero."
  ;; 7th file attribute is the size of the file in bytes.
  (let ((file (concat mspools-folder-directory spool))
	size)
    (setq file (or (file-symlink-p file) file))
    (setq size (nth 7 (file-attributes file)))
    (if (> size 0)
	(cons spool  size)
      ;; else SPOOL is empty
      nil)))

(provide 'mspools)
;;; mspools.el ends here

