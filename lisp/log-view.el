;;; log-view.el --- Major mode for browsing CVS log output

;; Copyright (C) 1999-2000  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@cs.yale.edu>
;; Keywords: pcl-cvs cvs log
;; Version: $Name:  $
;; Revision: $Id: log-view.el,v 1.2 2000/03/03 20:58:09 monnier Exp $

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

;; Todo:

;; - extract version info in log-view-current-tag
;; - add support for SCCS' output format
;; - add compatibility with cvs-log.el
;; - add ability to modify a log-entry (via cvs-mode-admin ;-)

;;; Code:

(eval-when-compile (require 'cl))
;;(require 'pcvs-defs)
(require 'pcvs-util)


(defgroup log-view nil
  "Major mode for browsing log output for PCL-CVS."
  :group 'pcl-cvs
  :prefix "log-view-")

(easy-mmode-defmap log-view-mode-map
  '(("n" . log-view-next-message)
    ("N" . log-view-next-file)
    ("M-n" . log-view-next-file)
    ("p" . log-view-prev-message)
    ("P" . log-view-prev-file)
    ("M-p" . log-view-prev-file))
  "Log-View's keymap."
  :group 'log-view
  :inherit 'cvs-mode-map)

(defvar log-view-mode-hook nil
  "Hook run at the end of `log-view-mode'.")

(defface log-view-file-face
  '((((class color) (background light))
     (:background "grey70" :bold t))
    (t (:bold t)))
  "Face for the file header line in `log-view-mode'."
  :group 'log-view)
(defvar log-view-file-face 'log-view-file-face)

(defface log-view-message-face
  '((((class color) (background light))
     (:background "grey85"))
    (t (:bold t)))
  "Face for the message header line in `log-view-mode'."
  :group 'log-view)
(defvar log-view-message-face 'log-view-message-face)

(defconst log-view-file-re
  (concat "^\\("
	  "Working file: \\(.+\\)"
	  "\\|SCCS/s\\.\\(.+\\):"
	  "\\)\n"))
(defconst log-view-message-re "^----------------------------$")

(defconst log-view-font-lock-keywords
  `((,log-view-file-re
     (2 'cvs-filename-face nil t)
     (3 'cvs-filename-face nil t)
     (0 'log-view-file-face append))
    (,log-view-message-re . log-view-message-face)))
(defconst log-view-font-lock-defaults
  '(log-view-font-lock-keywords t nil nil nil))

;;;; 
;;;; Actual code
;;;; 

;;;###autoload
(autoload 'log-view-mode "log-view" "Major mode for browsing CVS log output." t)
(eval-when-compile (autoload 'easy-mmode-define-derived-mode "easy-mmode"))
(easy-mmode-define-derived-mode log-view-mode fundamental-mode "Log-View"
  "Major mode for browsing CVS log output."
  (set (make-local-variable 'font-lock-defaults) log-view-font-lock-defaults)
  (set (make-local-variable 'cvs-minor-wrap-function) 'log-view-minor-wrap))

;;;;
;;;; Navigation
;;;;

(defun log-view-next-message (&optional count)
  "Move to next (COUNT'th) log message."
  (interactive "p")
  (unless count (setq count 1))
  (if (< count 0) (log-view-prev-message (- count))
    (when (looking-at log-view-message-re) (incf count))
    (re-search-forward log-view-message-re nil nil count)
    (goto-char (match-beginning 0))))

(defun log-view-next-file (&optional count)
  "Move to next (COUNT'th) file."
  (interactive "p")
  (unless count (setq count 1))
  (if (< count 0) (log-view-prev-file (- count))
    (when (looking-at log-view-file-re) (incf count))
    (re-search-forward log-view-file-re nil nil count)
    (goto-char (match-beginning 0))))

(defun log-view-prev-message (&optional count)
  "Move to previous (COUNT'th) log message."
  (interactive "p")
  (unless count (setq count 1))
  (if (< count 0) (log-view-next-message (- count))
    (re-search-backward log-view-message-re nil nil count)))

(defun log-view-prev-file (&optional count)
  "Move to previous (COUNT'th) file."
  (interactive "p")
  (unless count (setq count 1))
  (if (< count 0) (log-view-next-file (- count))
    (re-search-backward log-view-file-re nil nil count)))

;;;;
;;;; Linkage to PCL-CVS (mostly copied from cvs-status.el)
;;;;

(defconst log-view-dir-re "^cvs[.ex]* [a-z]+: Logging \\(.+\\)$")

(defun log-view-current-file ()
  (save-excursion
    (forward-line 1)
    (or (re-search-backward log-view-file-re nil t)
	(re-search-forward log-view-file-re))
    (let* ((file (or (match-string 2) (match-string 3)))
	   (cvsdir (and (re-search-backward log-view-dir-re nil t)
			(match-string 1)))
	   (pcldir (and (re-search-backward cvs-pcl-cvs-dirchange-re nil t)
			(match-string 1)))
	   (dir ""))
      (let ((default-directory ""))
	(when pcldir (setq dir (expand-file-name pcldir dir)))
	(when cvsdir (setq dir (expand-file-name cvsdir dir)))
	(expand-file-name file dir)))))

(defun log-view-current-tag ()
  nil);; FIXME

(defun log-view-minor-wrap (buf f)
  (let ((data (with-current-buffer buf
		(cons
		 (cons (log-view-current-file)
		       (log-view-current-tag))
		 (when (ignore-errors (mark))
		   ;; `mark-active' is not provided by XEmacs :-(
		   (save-excursion
		     (goto-char (mark))
		     (cons (log-view-current-file)
			   (log-view-current-tag))))))))
    (let ((cvs-branch-prefix (cdar data))
	  (cvs-secondary-branch-prefix (and (cdar data) (cddr data)))
	  (cvs-minor-current-files
	   (cons (caar data)
		 (when (and (cadr data) (not (equal (caar data) (cadr data))))
		   (list (cadr data)))))
	  ;; FIXME:  I need to force because the fileinfos are UNKNOWN
	  (cvs-force-command "/F"))
      (funcall f))))

(provide 'log-view)
;;; log-view.el ends here
