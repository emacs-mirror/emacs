;;; vc-hooks.el --- resident support for version-control

;; Copyright (C) 1992 Free Software Foundation, Inc.

;; Author: Eric S. Raymond <esr@snark.thyrsus.com>
;; Version: 5.3

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; See the commentary of vc.el.

;;; Code:

(defvar vc-master-templates
  '(("%sRCS/%s,v" . RCS) ("%s%s,v" . RCS) ("%sRCS/%s" . RCS)
    ("%sSCCS/s.%s" . SCCS) ("%ss.%s". SCCS))
  "*Where to look for version-control master files.
The first pair corresponding to a given back end is used as a template
when creating new masters.")

(defvar vc-make-backup-files nil
  "*If non-nil, backups of registered files are made according to
the make-backup-files variable.  Otherwise, prevents backups being made.")

(defvar vc-rcs-status t
  "*If non-nil, revision and locks on RCS working file displayed in modeline.
Otherwise, not displayed.")

;; Tell Emacs about this new kind of minor mode
(if (not (assoc 'vc-mode minor-mode-alist))
    (setq minor-mode-alist (cons '(vc-mode vc-mode)
				 minor-mode-alist)))

(make-variable-buffer-local 'vc-mode)
(put 'vc-mode 'permanent-local t)

;; We need a notion of per-file properties because the version
;; control state of a file is expensive to derive --- we don't
;; want to recompute it even on every find.

(defmacro vc-error-occurred (&rest body)
  (list 'condition-case nil (cons 'progn (append body '(nil))) '(error t)))

(defvar vc-file-prop-obarray [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
  "Obarray for per-file properties.")

(defun vc-file-setprop (file property value)
  ;; set per-file property
  (put (intern file vc-file-prop-obarray) property value))

(defun vc-file-getprop (file property)
  ;; get per-file property
  (get (intern file vc-file-prop-obarray) property))

;;; actual version-control code starts here

(defun vc-registered (file)
  (let (handler handlers)
    (if (boundp 'file-name-handler-alist)
	(save-match-data
	  (setq handlers file-name-handler-alist)
	  (while (and (consp handlers) (null handler))
	    (if (and (consp (car handlers))
		     (stringp (car (car handlers)))
		     (string-match (car (car handlers)) file))
		(setq handler (cdr (car handlers))))
	    (setq handlers (cdr handlers)))))
    (if handler
	(funcall handler 'vc-registered file)
      ;; Search for a master corresponding to the given file
      (let ((dirname (or (file-name-directory file) ""))
	    (basename (file-name-nondirectory file)))
	(catch 'found
	  (mapcar
	   (function (lambda (s)
	      (let ((trial (format (car s) dirname basename)))
		(if (and (file-exists-p trial)
			 ;; Make sure the file we found with name
			 ;; TRIAL is not the source file itself.
			 ;; That can happen with RCS-style names
			 ;; if the file name is truncated
			 ;; (e.g. to 14 chars).  See if either
			 ;; directory or attributes differ.
			 (or (not (string= dirname
					   (file-name-directory trial)))
			     (not (equal
				   (file-attributes file)
				   (file-attributes trial)))))
		    (throw 'found (cons trial (cdr s)))))))
	   vc-master-templates)
	  nil)))))

(defun vc-backend-deduce (file)
  "Return the version-control type of a file, nil if it is not registered"
  (and file
       (or (vc-file-getprop file 'vc-backend)
	   (vc-file-setprop file 'vc-backend (cdr (vc-registered file))))))

(defun vc-toggle-read-only ()
  "Change read-only status of current buffer, perhaps via version control.
If the buffer is visiting a file registered with version control,
then check the file in or out.  Otherwise, just change the read-only flag
of the buffer."
  (interactive)
  (if (vc-backend-deduce (buffer-file-name))
      (vc-next-action nil)
    (toggle-read-only)))
(define-key global-map "\C-x\C-q" 'vc-toggle-read-only)

(defun vc-mode-line (file &optional label)
  "Set `vc-mode' to display type of version control for FILE.
The value is set in the current buffer, which should be the buffer
visiting FILE."
  (interactive (list buffer-file-name nil))
  (let ((vc-type (vc-backend-deduce file)))
    (if vc-type
        (setq vc-mode
              (concat (if (and vc-rcs-status (eq vc-type 'RCS))
                          (vc-rcs-status file))
                      " " (or label (symbol-name vc-type)))))
    ;; force update of mode line
    (set-buffer-modified-p (buffer-modified-p))
    vc-type))

(defun vc-rcs-status (file)
  ;; Return string " [LOCKERS:]REV" if FILE under RCS control, otherwise nil,
  ;; for placement in modeline by `vc-mode-line'.

  ;; If FILE is not locked then return just " REV", where
  ;; REV is the number of last revision checked in.  If the FILE is locked
  ;; then return *all* the locks currently set, in a single string of the
  ;; form " LOCKER1:REV1 LOCKER2:REV2 ..."

  ;; Algorithm: 

  ;; 1. Check for master file corresponding to FILE being visited in
  ;; subdirectory RCS of current directory and then, if not found there, in
  ;; the current directory.  some of the vc-hooks machinery could be used
  ;; here.
  ;; 
  ;; 2. Insert the header, first 200 characters, of master file into a work
  ;; buffer.
  ;;  
  ;; 3. Search work buffer for line starting with "date" indicating enough
  ;; of header was included; if not found, then successive increments of 100
  ;; characters are inserted until "date" is located or 1000 characters is
  ;; reached.
  ;; 
  ;; 4. Search work buffer for line starting with "locks" and *not* followed
  ;; immediately by a semi-colon; this indicates that locks exist; it extracts
  ;; all the locks currently enabled and removes controls characters
  ;; separating them, like newlines; the string " user1:revision1
  ;; user2:revision2 ..." is returned.
  ;; 
  ;; 5. If "locks;" is found instead, indicating no locks, then search work
  ;; buffer for lines starting with string "head" and "branch" and parses
  ;; their contents; if contents of branch is non-nil then it is returned
  ;; otherwise the contents of head is returned either as string " revision".

  ;; Limitations:

  ;; The output doesn't show which version you are actually looking at.
  ;; The modeline can get quite cluttered when there are multiple locks.

  ;; Make sure name is expanded -- not needed?
  (setq file (expand-file-name file))

  (let (master found locks head branch status (eof 200))

    ;; Find the name of the master file -- perhaps use `vc-name'?
    (setq master (concat (file-name-directory file) "RCS/"
                         (file-name-nondirectory file) ",v"))

    ;; If master file exists, then parse its contents, otherwise we return the 
    ;; nil value of this if form.
    (if (or (file-readable-p master)
            (file-readable-p (setq master (concat file ",v")))) ; current dir?

        (save-excursion

          ;; Create work buffer.
          (set-buffer (get-buffer-create "*vc-rcs-status*"))
          (setq buffer-read-only nil
                default-directory (file-name-directory master))
          (erase-buffer)

          ;; Limit search to header.
          (insert-file-contents master nil 0 eof)
          (goto-char (point-min))

          ;; Check if we have enough of the header.  If not, then keep
          ;; including more until enough or until 1000 chars is reached.
          (setq found (re-search-forward "^date" nil t))

          (while (and (not found) (<= eof 1000))
            (goto-char (point-max))
            (insert-file-contents master nil (+ eof 1) (setq eof (+ eof 100)))
            (goto-char (point-min))
            (setq found (re-search-forward "^date" nil t)))

          ;; If we located "^date" we can extract the status information, 
          ;; otherwise we return `status' which was initialized to nil.
          (if found
              (progn
                (goto-char (point-min))

                ;; First see if any revisions have any locks on them.
                (if (re-search-forward "^locks[ \t\n\r\f]+\\([^;]*\\)" nil t)

                    ;; At least one lock - clean controls characters from text.
                    (save-restriction
                      (narrow-to-region (match-beginning 1) (match-end 1))
                      (goto-char (point-min))
                      (while (re-search-forward "[ \t\n\r\f]+" nil t)
                        (replace-match " " t t))
                      (setq locks (buffer-string)))

                  ;; Not locked - find head and branch.
                  ;; ...more information could be extracted here.
                  (setq locks ""
                        head (vc-rcs-glean-field "head")
                        branch (vc-rcs-glean-field "branch")))

                ;; In case of RCS unlocked files: if non-nil branch is
                ;; displayed, else if non-nil head is displayed.  if both nil,
                ;; nothing is displayed.  In case of RCS locked files: locks
                ;; is displayed.

                (setq status (concat " " (or branch head locks)))))

          ;; Clean work buffer.
          (erase-buffer)
          (set-buffer-modified-p nil)

          ;; Return status, which is nil if "^date" was not located.
          status))))

(defun vc-rcs-glean-field (field)
  ;; Parse ,v file in current buffer and return contents of FIELD,
  ;; which should be a field like "head" or "branch", with a
  ;; revision number as value.
  ;; Returns nil if FIELD is not found.
  (goto-char (point-min))
  (if (re-search-forward
       (concat "^" (regexp-quote field) "[ \t\n\r\f]+\\([0-9.]+\\)")
       nil t)
      (buffer-substring (match-beginning 1)
                        (match-end 1))))

;;; install a call to the above as a find-file hook
(defun vc-find-file-hook ()
  ;; Recompute whether file is version controlled,
  ;; if user has killed the buffer and revisited.
  (if buffer-file-name
      (vc-file-setprop buffer-file-name 'vc-backend nil))
  (if (and (vc-mode-line buffer-file-name) (not vc-make-backup-files))
      (progn
	(make-local-variable 'make-backup-files)
	(setq make-backup-files nil))))

(or (memq 'vc-find-file-hook find-file-hooks)
    (setq find-file-hooks
	  (cons 'vc-find-file-hook find-file-hooks)))

;;; more hooks, this time for file-not-found
(defun vc-file-not-found-hook ()
  "When file is not found, try to check it out from RCS or SCCS.
Returns t if checkout was successful, nil otherwise."
  (if (vc-backend-deduce buffer-file-name)
      (progn
	(require 'vc)
	(not (vc-error-occurred (vc-checkout buffer-file-name))))))

(or (memq 'vc-file-not-found-hook find-file-not-found-hooks)
    (setq find-file-not-found-hooks
	  (cons 'vc-file-not-found-hook find-file-not-found-hooks)))

;;; Now arrange for bindings and autoloading of the main package.
;;; Bindings for this have to go in the global map, as we'll often
;;; want to call them from random buffers.

(setq vc-prefix-map (lookup-key global-map "\C-xv"))
(if (not (keymapp vc-prefix-map))
    (progn
      (setq vc-prefix-map (make-sparse-keymap))
      (define-key global-map "\C-xv" vc-prefix-map)
      (define-key vc-prefix-map "a" 'vc-update-change-log)
      (define-key vc-prefix-map "c" 'vc-cancel-version)
      (define-key vc-prefix-map "d" 'vc-directory)
      (define-key vc-prefix-map "h" 'vc-insert-headers)
      (define-key vc-prefix-map "i" 'vc-register)
      (define-key vc-prefix-map "l" 'vc-print-log)
      (define-key vc-prefix-map "r" 'vc-retrieve-snapshot)
      (define-key vc-prefix-map "s" 'vc-create-snapshot)
      (define-key vc-prefix-map "u" 'vc-revert-buffer)
      (define-key vc-prefix-map "v" 'vc-next-action)
      (define-key vc-prefix-map "=" 'vc-diff)
      ))

(provide 'vc-hooks)

;;; vc-hooks.el ends here
