;;;; makeinfo.el -- run makeinfo conveniently.
;;; Copyright (C) 1991, 1993 Free Software Foundation, Inc.

;;; Author: Robert J. Chassell      
;;; Maintainer: FSF

;;; This file is part of GNU Emacs.

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

;;; The Texinfo mode `makeinfo' related commands are:

;; makeinfo-region      to run makeinfo on the current region.
;; makeinfo-buffer      to run makeinfo on the current buffer, or
;;                        with optional prefix arg, on current region
;; kill-compilation     to kill currently running makeinfo job
;; makeinfo-recenter-makeinfo-buffer  to redisplay *compilation* buffer

;;; Keybindings (defined in `texinfo.el')

;; makeinfo bindings
; (define-key texinfo-mode-map "\C-c\C-m\C-r" 'makeinfo-region)
; (define-key texinfo-mode-map "\C-c\C-m\C-b" 'makeinfo-buffer)
; (define-key texinfo-mode-map "\C-c\C-m\C-k" 'kill-compilation)
; (define-key texinfo-mode-map "\C-c\C-m\C-l"
;   'makeinfo-recenter-compilation-buffer)

;;; Code:

;;; Variables used by `makeinfo'

(require 'compile)

(defvar makeinfo-run-command "makeinfo"
  "*Command used to run `makeinfo' subjob.
The name of the file is appended to this string, separated by a space.")

(defvar makeinfo-options "+fill-column=70"
  "*String containing options for running `makeinfo'.  
Do not include `--footnote-style' or `--paragraph-indent';
the proper way to specify those is with the Texinfo commands
`@footnotestyle` and `@paragraphindent'.")

(require 'texinfo)
(require 'texinfmt)

(defvar makeinfo-compilation-process nil
  "Process that runs `makeinfo'.  Should start out nil.")

(defvar makeinfo-temp-file nil
  "Temporary file name used for text being sent as input to `makeinfo'.")

(defvar makeinfo-output-file-name nil
  "Info file name used for text output by `makeinfo'.")


;;; The `makeinfo' function definitions

(defun makeinfo-region (region-beginning region-end)
  "Make Info file from region of current Texinfo file, and switch to it.

This command does not offer the `next-error' feature since it would
apply to a temporary file, not the original; use the `makeinfo-buffer'
command to gain use of `next-error'."
  
  (interactive "r")
  (let (filename-or-header
        filename-or-header-beginning
        filename-or-header-end)
    ;; Cannot use `let' for makeinfo-temp-file or
    ;; makeinfo-output-file-name since `makeinfo-compilation-sentinel'
    ;; needs them.

    (setq makeinfo-temp-file
          (concat
           (make-temp-name
            (substring (buffer-file-name)
                       0 
                       (or (string-match "\\.tex" (buffer-file-name)) 
                           (length (buffer-file-name)))))
           ".texinfo"))
    
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let ((search-end (save-excursion (forward-line 100) (point))))
          ;; Find and record the Info filename,
          ;; or else explain that a filename is needed.
          (if (re-search-forward 
               "^@setfilename[ \t]+\\([^ \t\n]+\\)[ \t]*"
               search-end t)
              (setq makeinfo-output-file-name 
                    (buffer-substring (match-beginning 1) (match-end 1)))
            (error
             "The texinfo file needs a line saying: @setfilename <name>"))

          ;; Find header and specify its beginning and end.
          (goto-char (point-min))
          (if (and 
               (prog1 
                   (search-forward texinfo-start-of-header search-end t)
                 (beginning-of-line)
                 ;; Mark beginning of header.
                 (setq filename-or-header-beginning (point)))
               (prog1 
                   (search-forward texinfo-end-of-header nil t)
                 (beginning-of-line)
                 ;; Mark end of header
                 (setq filename-or-header-end (point))))
              
              ;; Insert the header into the temporary file.
              (write-region
               (min filename-or-header-beginning region-beginning)
               filename-or-header-end
               makeinfo-temp-file nil nil)
            
            ;; Else no header; insert @filename line into temporary file.
            (goto-char (point-min))
            (search-forward "@setfilename" search-end t)
            (beginning-of-line)
            (setq filename-or-header-beginning (point))
            (forward-line 1)
            (setq filename-or-header-end (point))
            (write-region
             (min filename-or-header-beginning region-beginning)
             filename-or-header-end
             makeinfo-temp-file nil nil))
          
          ;; Insert the region into the file.
          (write-region
           (max region-beginning filename-or-header-end)
           region-end
           makeinfo-temp-file t nil)

          ;; Run the `makeinfo-compile' command in the *compilation* buffer
          (save-excursion
            (makeinfo-compile
             (concat makeinfo-run-command
                     " "
                     makeinfo-options
                     " " 
                     makeinfo-temp-file)
             "Use `makeinfo-buffer' to gain use of the `next-error' command.")))))))

;; Based on `compile1' in compile.el; changed so to make it possible
;; to delete temporary file.
(defun makeinfo-compile (command error-message &optional name-of-mode)
  ;(save-some-buffers)                   ; Don't need to save other buffers.
  (if makeinfo-compilation-process
      (if (or (not (eq (process-status makeinfo-compilation-process) 'run))
	      (yes-or-no-p
               "A `makeinfo' compilation process is running; kill it? "))
	  (condition-case ()
	      (let ((comp-proc makeinfo-compilation-process))
		(interrupt-process comp-proc)
		(sit-for 1)
		(delete-process comp-proc))
	    (error nil))
	(error "Cannot have two makeinfo processes")))
  (setq makeinfo-compilation-process nil)
  (compilation-forget-errors)
  (setq compilation-error-list t)
  (setq compilation-error-message error-message)
  (setq makeinfo-compilation-process
	(start-process "makeinfo" "*compilation*"         
		       shell-file-name
		       "-c" (concat "exec " command)))
  (with-output-to-temp-buffer "*compilation*"             
    (princ "cd ")
    (princ default-directory)
    (terpri)
    (princ command)
    (terpri))
  (let ((regexp compilation-error-regexp))
    (save-excursion
      (set-buffer "*compilation*")                        
      (make-local-variable 'compilation-error-regexp)
      (setq compilation-error-regexp regexp)))
  (set-process-sentinel
   makeinfo-compilation-process 'makeinfo-compilation-sentinel)
  (let* ((thisdir default-directory)
	 (outbuf (process-buffer makeinfo-compilation-process))
	 (outwin (get-buffer-window outbuf)))
    (if (eq outbuf (current-buffer))
	(goto-char (point-max)))
    (save-excursion
      (set-buffer outbuf)
      (buffer-flush-undo outbuf)
      (let ((start (save-excursion (set-buffer outbuf) (point-min))))
	(set-window-start outwin start)
	(or (eq outwin (selected-window))
	    (set-window-point outwin start)))
      (setq default-directory thisdir)
      (fundamental-mode)
      (setq mode-name (or name-of-mode "compilation"))    
      ;; Make log buffer's mode line show process state
      (setq mode-line-process '(": %s")))))

;; Delete makeinfo-temp-file after proccessing is finished,
;; and visit Info file.
;; This function is called when the compilation process changes state.
;; Based on `compilation-sentinel' in compile.el
(defun makeinfo-compilation-sentinel (proc msg)
  (cond ((null (buffer-name (process-buffer proc)))
	 ;; buffer killed
	 (set-process-buffer proc nil))
	((memq (process-status proc) '(signal exit))
	 (let* ((obuf (current-buffer))
		omax opoint)
	   ;; save-excursion isn't the right thing if
	   ;;  process-buffer is current-buffer
	   (unwind-protect
	       (progn
		 ;; Write something in *compilation* and hack
		 ;; its mode line,
		 (set-buffer (process-buffer proc))
		 (setq omax (point-max) opoint (point))
		 (goto-char (point-max))
		 (insert ?\n mode-name " " msg)
		 (forward-char -1)
		 (insert " at "
			 (substring (current-time-string) 0 -5))
		 (forward-char 1)
		 (setq mode-line-process
		       (concat ": "
			       (symbol-name (process-status proc))))
		 ;; If buffer and mode line will show that the process
		 ;; is dead, we can delete it now.  Otherwise it
		 ;; will stay around until M-x list-processes.
		 (delete-process proc))
	     (setq makeinfo-compilation-process nil)
	     ;; Force mode line redisplay soon
	     (set-buffer-modified-p (buffer-modified-p)))
	   (if (and opoint (< opoint omax))
	       (goto-char opoint))
	   (set-buffer obuf))
         (if (and makeinfo-temp-file (file-exists-p makeinfo-temp-file))
             (delete-file makeinfo-temp-file))
         ;; Always use the version on disk.
         (if (get-file-buffer makeinfo-output-file-name)
             (progn (set-buffer makeinfo-output-file-name)
                    (revert-buffer t t)))
         (find-file makeinfo-output-file-name)
         (goto-char (point-min)))))

(defun makeinfo-buffer (buffer)
  "Make Info file from current buffer.

The \\[next-error] command can be used to move to the next error 
\(if any are found\)."
  
  (interactive "bRun `makeinfo' on: ")
  (cond ((null buffer-file-name)
         (error "Buffer not visiting any file!"))
        ((buffer-modified-p)
         (if (y-or-n-p "Buffer modified; do you want to save it? ")
             (save-buffer))))
  
  ;; Find and record the Info filename,
  ;; or else explain that a filename is needed.
  (save-excursion
    (goto-char (point-min))
    (let ((search-end (save-excursion (forward-line 100) (point))))
      (if (re-search-forward 
           "^@setfilename[ \t]+\\([^ \t\n]+\\)[ \t]*"
           search-end t)
          (setq makeinfo-output-file-name 
                (buffer-substring (match-beginning 1) (match-end 1)))
        (error
         "The texinfo file needs a line saying: @setfilename <name>"))))
  
  (save-excursion
    (makeinfo-compile
     (concat makeinfo-run-command
             " " 
             makeinfo-options
             " " 
             "+footnote-style="
             texinfo-footnote-style
             " "
             (buffer-file-name
              (get-buffer buffer)))
     "No more errors.")))

(defun makeinfo-recenter-compilation-buffer (linenum)
  "Redisplay `*compilation*' buffer so most recent output can be seen.
The last line of the buffer is displayed on
line LINE of the window, or centered if LINE is nil."
  (interactive "P")
  (let ((makeinfo-buffer (get-buffer "*compilation*"))
	(old-buffer (current-buffer)))
    (if (null makeinfo-buffer)
	(message "No *compilation* buffer")
      (pop-to-buffer makeinfo-buffer)
      (bury-buffer makeinfo-buffer)
      (goto-char (point-max))
      (recenter (if linenum
		    (prefix-numeric-value linenum)
		  (/ (window-height) 2)))
      (pop-to-buffer old-buffer)
      )))


;;; Place `provide' at end of file.
(provide 'makeinfo)

;;; makeinfo.el ends here

