;;; mh-pick --- make a search pattern and search for a message in mh-e
;; Time-stamp: <94/12/22 20:34:51 gildea>

;; Copyright (C) 1993, 1995 Free Software Foundation, Inc.

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

;; Internal support for mh-e package.

;;; Change Log:

;; $Id: mh-pick.el,v 1.2 1995/04/09 22:30:17 kwzh Exp kwzh $

;;; Code:

(provide 'mh-pick)
(require 'mh-e)

(defvar mh-pick-mode-hook nil
  "Invoked in `mh-pick-mode' on a new pattern.")

;;; Internal variables:

(defvar mh-pick-mode-map (make-sparse-keymap)
  "Keymap for searching folder.")

(defvar mh-searching-folder nil)	;Folder this pick is searching.

(defun mh-search-folder (folder)
  "Search FOLDER for messages matching a pattern.
Add the messages found to the sequence named `search'."
  (interactive (list (mh-prompt-for-folder "Search"
					   mh-current-folder
					   t)))
  (switch-to-buffer-other-window "pick-pattern")
  (if (or (zerop (buffer-size))
	  (not (y-or-n-p "Reuse pattern? ")))
      (mh-make-pick-template)
    (message ""))
  (setq mh-searching-folder folder))

(defun mh-make-pick-template ()
  ;; Initialize the current buffer with a template for a pick pattern.
  (erase-buffer)
  (insert "From: \n"
	  "To: \n"
	  "Cc: \n"
	  "Date: \n"
	  "Subject: \n"
	  "---------\n")
  (mh-pick-mode)
  (goto-char (point-min))
  (end-of-line))

(put 'mh-pick-mode 'mode-class 'special)

(defun mh-pick-mode ()
  "Mode for creating search templates in mh-e.\\<mh-pick-mode-map>
After each field name, enter the pattern to search for.  If a field's
value does not matter for the search, leave it empty.  To search the
entire message, supply the pattern in the \"body\" of the template.
Each non-empty field must be matched for a message to be selected.
To effect a logical \"or\", use \\[mh-search-folder] multiple times.
When you have finished, type  \\[mh-do-pick-search]  to do the search.
\\{mh-pick-mode-map}
Turning on mh-pick-mode calls the value of the variable mh-pick-mode-hook
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'mh-searching-folder)
  (use-local-map mh-pick-mode-map)
  (setq major-mode 'mh-pick-mode)
  (mh-set-mode-name "MH-Pick")
  (run-hooks 'mh-pick-mode-hook))


(defun mh-do-pick-search ()
  "Find messages that match the qualifications in the current pattern buffer.
Messages are searched for in the folder named in mh-searching-folder.
Add the messages found to the sequence named `search'."
  (interactive)
  (let ((pattern-buffer (buffer-name))
	(searching-buffer mh-searching-folder)
	range
	msgs
	(finding-messages t)
	(pattern nil)
	(new-buffer nil))
    (save-excursion
      (cond ((get-buffer searching-buffer)
	     (set-buffer searching-buffer)
	     (setq range (list (format "%d-%d"
				       mh-first-msg-num mh-last-msg-num))))
	    (t
	     (mh-make-folder searching-buffer)
	     (setq range '("all"))
	     (setq new-buffer t))))
    (message "Searching...")
    (goto-char (point-min))
    (while (and range
		(setq pattern (mh-next-pick-field pattern-buffer)))
      (setq msgs (mh-seq-from-command searching-buffer
				      'search
				      (mh-list-to-string
				       (list "pick" pattern searching-buffer
					     "-list"
					     (mh-coalesce-msg-list range)))))
      (setq range msgs))		;restrict the pick range for next pass
    (message "Searching...done")
    (if new-buffer
	(mh-scan-folder searching-buffer msgs)
	(switch-to-buffer searching-buffer))
    (mh-add-msgs-to-seq msgs 'search)
    (delete-other-windows)))


(defun mh-seq-from-command (folder seq seq-command)
  ;; In FOLDER, make a sequence named SEQ by executing COMMAND.
  ;; COMMAND is a list.  The first element is a program name
  ;; and the subsequent elements are its arguments, all strings.
  (let ((msg)
	(msgs ())
	(case-fold-search t))
    (save-excursion
      (save-window-excursion
	(if (eq 0 (apply 'mh-exec-cmd-quiet nil seq-command))
	    ;; "pick" outputs one number per line
	    (while (setq msg (car (mh-read-msg-list)))
	      (setq msgs (cons msg msgs))
	      (forward-line 1))))
      (set-buffer folder)
      (setq msgs (nreverse msgs))	;put in ascending order
      msgs)))


(defun mh-next-pick-field (buffer)
  ;; Return the next piece of a pick argument that can be extracted from the
  ;; BUFFER.
  ;; Return a list like ("--fieldname" "pattern") or ("-search" "bodypat")
  ;; or NIL if no pieces remain.
  (set-buffer buffer)
  (let ((case-fold-search t))
    (cond ((eobp)
	   nil)
	  ((re-search-forward "^\\([a-z][^: \t\n]*\\):[ \t]*\\([a-z0-9].*\\)$" nil t)
	   (let* ((component
		   (format "--%s"
			   (downcase (buffer-substring (match-beginning 1)
						       (match-end 1)))))
		  (pat (buffer-substring (match-beginning 2) (match-end 2))))
	       (forward-line 1)
	       (list component pat)))
	  ((re-search-forward "^-*$" nil t)
	   (forward-char 1)
	   (let ((body (buffer-substring (point) (point-max))))
	     (if (and (> (length body) 0) (not (equal body "\n")))
		 (list "-search" body)
		 nil)))
	  (t
	   nil))))

;;; Build the pick-mode keymap:

(define-key mh-pick-mode-map "\C-c\C-c" 'mh-do-pick-search)
(define-key mh-pick-mode-map "\C-c\C-f\C-b" 'mh-to-field)
(define-key mh-pick-mode-map "\C-c\C-f\C-c" 'mh-to-field)
(define-key mh-pick-mode-map "\C-c\C-f\C-d" 'mh-to-field)
(define-key mh-pick-mode-map "\C-c\C-f\C-f" 'mh-to-field)
(define-key mh-pick-mode-map "\C-c\C-f\C-r" 'mh-to-field)
(define-key mh-pick-mode-map "\C-c\C-f\C-s" 'mh-to-field)
(define-key mh-pick-mode-map "\C-c\C-f\C-t" 'mh-to-field)
(define-key mh-pick-mode-map "\C-c\C-fb" 'mh-to-field)
(define-key mh-pick-mode-map "\C-c\C-fc" 'mh-to-field)
(define-key mh-pick-mode-map "\C-c\C-fd" 'mh-to-field)
(define-key mh-pick-mode-map "\C-c\C-ff" 'mh-to-field)
(define-key mh-pick-mode-map "\C-c\C-fr" 'mh-to-field)
(define-key mh-pick-mode-map "\C-c\C-fs" 'mh-to-field)
(define-key mh-pick-mode-map "\C-c\C-ft" 'mh-to-field)
