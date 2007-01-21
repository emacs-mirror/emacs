;;; log-view.el --- Major mode for browsing RCS/CVS/SCCS log output

;; Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2005,
;;   2006, 2007 Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords: rcs sccs cvs log version-control

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

;; Major mode to browse revision log histories.
;; Currently supports the format output by:
;;  RCS, SCCS, CVS, Subversion, and DaRCS.

;; Examples of log output:

;;;; RCS/CVS:

;; ----------------------------
;; revision 1.35	locked by: turlutut
;; date: 2005-03-22 18:48:38 +0000;  author: monnier;  state: Exp;  lines: +6 -8
;; (gnus-display-time-event-handler):
;; Check display-time-timer at runtime rather than only at load time
;; in case display-time-mode is turned off in the mean time.
;; ----------------------------
;; revision 1.34
;; date: 2005-02-09 15:50:38 +0000;  author: kfstorm;  state: Exp;  lines: +7 -7
;; branches:  1.34.2;
;; Change release version from 21.4 to 22.1 throughout.
;; Change development version from 21.3.50 to 22.0.50.

;;;; SCCS:

;;;; Subversion:

;;;; Darcs:

;; Changes to darcsum.el:
;; 
;; Mon Nov 28 15:19:38 GMT 2005  Dave Love <fx@gnu.org>
;;   * Abstract process startup into darcsum-start-process.  Use TERM=dumb.
;;   TERM=dumb avoids escape characters, at least, for any old darcs that 
;;   doesn't understand DARCS_DONT_COLOR & al.
;; 
;; Thu Nov 24 15:20:45 GMT 2005  Dave Love <fx@gnu.org>
;;   * darcsum-mode-related changes.
;;   Don't call font-lock-mode (unnecessary) or use-local-map (redundant).
;;   Use mode-class 'special.  Add :group.
;;   Add trailing-whitespace option to mode hook and fix
;;   darcsum-display-changeset not to use trailing whitespace.

;;; Todo:

;; - add ability to modify a log-entry (via cvs-mode-admin ;-)
;; - remove references to cvs-*
;; - make it easier to add support for new backends without changing the code.

;;; Code:

(eval-when-compile (require 'cl))
(require 'pcvs-util)
(autoload 'vc-find-version "vc")
(autoload 'vc-version-diff "vc")

(defvar cvs-minor-wrap-function)

(defgroup log-view nil
  "Major mode for browsing log output of RCS/CVS/SCCS."
  :group 'pcl-cvs
  :prefix "log-view-")

(easy-mmode-defmap log-view-mode-map
  '(("q" . quit-window)
    ("z" . kill-this-buffer)
    ("m" . set-mark-command)
    ;; ("e" . cvs-mode-edit-log)
    ("d" . log-view-diff)
    ("f" . log-view-find-version)
    ("n" . log-view-msg-next)
    ("p" . log-view-msg-prev)
    ("N" . log-view-file-next)
    ("P" . log-view-file-prev)
    ("\M-n" . log-view-file-next)
    ("\M-p" . log-view-file-prev))
  "Log-View's keymap."
  :group 'log-view
  ;; Here I really need either buffer-local keymap-inheritance
  ;; or a minor-mode-map with lower precedence than the local map.
  :inherit (if (boundp 'cvs-mode-map) cvs-mode-map))

(defvar log-view-mode-hook nil
  "Hook run at the end of `log-view-mode'.")

(defface log-view-file
  '((((class color) (background light))
     (:background "grey70" :weight bold))
    (t (:weight bold)))
  "Face for the file header line in `log-view-mode'."
  :group 'log-view)
;; backward-compatibility alias
(put 'log-view-file-face 'face-alias 'log-view-file)
(defvar log-view-file-face 'log-view-file)

(defface log-view-message
  '((((class color) (background light))
     (:background "grey85"))
    (t (:weight bold)))
  "Face for the message header line in `log-view-mode'."
  :group 'log-view)
;; backward-compatibility alias
(put 'log-view-message-face 'face-alias 'log-view-message)
(defvar log-view-message-face 'log-view-message)

(defconst log-view-file-re
  (concat "^\\(?:Working file: \\(.+\\)"                ;RCS and CVS.
          "\\|\\(?:SCCS/s\\.\\|Changes to \\)\\(.+\\):" ;SCCS and Darcs.
	  "\\)\n"))                   ;Include the \n for font-lock reasons.

(defconst log-view-message-re
  (concat "^\\(?:revision \\([.0-9]+\\)\\(?:\t.*\\)?" ; RCS and CVS.
          "\\|r\\([0-9]+\\) | .* | .*"                ; Subversion.
          "\\|D \\([.0-9]+\\) .*"                     ; SCCS.
          ;; Darcs doesn't have revision names.  VC-darcs uses patch names
          ;; instead.  Darcs patch names are hashcodes, which do not appear
          ;; in the log output :-(, but darcs accepts any prefix of the log
          ;; message as a patch name, so we match the first line of the log
          ;; message.
          ;; First loosely match the date format.
          (concat "\\|[^ \n].*[^0-9\n][0-9][0-9]:[0-9][0-9][^0-9\n].*[^ \n]"
                  ;;Email of user and finally Msg, used as revision name.
                  "  .*@.*\n\\(?:  \\* \\(.*\\)\\)?")
          "\\)$"))

(defconst log-view-font-lock-keywords
  `((,log-view-file-re
     (1 (if (boundp 'cvs-filename-face) cvs-filename-face) nil t)
     (2 (if (boundp 'cvs-filename-face) cvs-filename-face) nil t)
     (0 log-view-file-face append))
    (,log-view-message-re . log-view-message-face)))
(defconst log-view-font-lock-defaults
  '(log-view-font-lock-keywords t nil nil nil))

;;;;
;;;; Actual code
;;;;

;;;###autoload
(define-derived-mode log-view-mode fundamental-mode "Log-View"
  "Major mode for browsing CVS log output."
  (setq buffer-read-only t)
  (set (make-local-variable 'font-lock-defaults) log-view-font-lock-defaults)
  (set (make-local-variable 'cvs-minor-wrap-function) 'log-view-minor-wrap))

;;;;
;;;; Navigation
;;;;

;; define log-view-{msg,file}-{next,prev}
(easy-mmode-define-navigation log-view-msg log-view-message-re "log message")
(easy-mmode-define-navigation log-view-file log-view-file-re "file")

(defun log-view-goto-rev (rev)
  (goto-char (point-min))
  (ignore-errors
    (while (not (equal rev (log-view-current-tag)))
      (log-view-msg-next))
    t))

;;;;
;;;; Linkage to PCL-CVS (mostly copied from cvs-status.el)
;;;;

(defconst log-view-dir-re "^cvs[.ex]* [a-z]+: Logging \\(.+\\)$")

(defun log-view-current-file ()
  (save-excursion
    (forward-line 1)
    (or (re-search-backward log-view-file-re nil t)
	(re-search-forward log-view-file-re))
    (let* ((file (or (match-string 1) (match-string 2)))
	   (cvsdir (and (re-search-backward log-view-dir-re nil t)
			(match-string 1)))
	   (pcldir (and (boundp 'cvs-pcl-cvs-dirchange-re)
			(re-search-backward cvs-pcl-cvs-dirchange-re nil t)
			(match-string 1)))
	   (dir ""))
      (let ((default-directory ""))
	(when pcldir (setq dir (expand-file-name pcldir dir)))
	(when cvsdir (setq dir (expand-file-name cvsdir dir))))
      (expand-file-name file dir))))

(defun log-view-current-tag (&optional where)
  (save-excursion
    (when where (goto-char where))
    (forward-line 1)
    (let ((pt (point)))
      (when (re-search-backward log-view-message-re nil t)
        (let (rev)
          ;; Find the subgroup that matched.
          (dotimes (i (/ (length (match-data 'integers)) 2))
            (setq rev (or rev (match-string (1+ i)))))
	  (unless (re-search-forward log-view-file-re pt t)
	    rev))))))

(defvar cvs-minor-current-files)
(defvar cvs-branch-prefix)
(defvar cvs-secondary-branch-prefix)

(defun log-view-minor-wrap (buf f)
  (let ((data (with-current-buffer buf
		(let* ((beg (point))
		       (end (if mark-active (mark) (point)))
		       (fr (log-view-current-tag beg))
		       (to (log-view-current-tag end)))
		  (when (string-equal fr to)
		    (save-excursion
		      (goto-char end)
		      (log-view-msg-next)
		      (setq to (log-view-current-tag))))
		  (cons
                   ;; The first revision has to be the one at point, for
                   ;; operations that only take one revision
                   ;; (e.g. cvs-mode-edit).
		   (cons (log-view-current-file) fr)
		   (cons (log-view-current-file) to))))))
    (let ((cvs-branch-prefix (cdar data))
	  (cvs-secondary-branch-prefix (and (cdar data) (cddr data)))
	  (cvs-minor-current-files
	   (cons (caar data)
		 (when (and (cadr data) (not (equal (caar data) (cadr data))))
		   (list (cadr data)))))
	  ;; FIXME:  I need to force because the fileinfos are UNKNOWN
	  (cvs-force-command "/F"))
      (funcall f))))

(defun log-view-find-version (pos)
  "Visit the version at point."
  (interactive "d")
  (save-excursion
    (goto-char pos)
    (switch-to-buffer (vc-find-version (log-view-current-file)
                                       (log-view-current-tag)))))

;;
;; diff
;;

(defun log-view-diff (beg end)
  "Get the diff between two revisions.
If the mark is not active or the mark is on the revision at point,
get the diff between the revision at point and its previous revision.
Otherwise, get the diff between the revisions where the region starts
and ends."
  (interactive
   (list (if mark-active (region-beginning) (point))
         (if mark-active (region-end) (point))))
  (let ((fr (log-view-current-tag beg))
        (to (log-view-current-tag end)))
    (when (string-equal fr to)
      (save-excursion
        (goto-char end)
        (log-view-msg-next)
        (setq to (log-view-current-tag))))
    (vc-version-diff (log-view-current-file) to fr)))

(provide 'log-view)

;; arch-tag: 0d64220b-ce7e-4f62-9c2a-6b04c2f81f4f
;;; log-view.el ends here
