;;; smerge-mode.el --- Minor mode to resolve diff3 conflicts

;; Copyright (C) 1999, 2000, 2001  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@cs.yale.edu>
;; Keywords: merge diff3 cvs conflict
;; Revision: $Id: smerge-mode.el,v 1.14 2001/07/31 08:28:43 gerd Exp $

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

;; Provides a lightweight alternative to emerge/ediff.
;; To use it, simply add to your .emacs the following lines:
;;
;;   (autoload 'smerge-mode "smerge-mode" nil t)
;;
;; you can even have it turned on automatically with the following
;; piece of code in your .emacs:
;;
;;   (defun sm-try-smerge ()
;;     (save-excursion
;;   	 (goto-char (point-min))
;;   	 (when (re-search-forward "^<<<<<<< " nil t)
;;   	   (smerge-mode 1))))
;;   (add-hook 'find-file-hooks 'sm-try-smerge t)

;;; Todo:

;; - if requested, ask the user whether he wants to call ediff right away

;;; Code:

(eval-when-compile (require 'cl))


(defgroup smerge ()
  "Minor mode to resolve diff3 conflicts."
  :group 'tools
  :prefix "smerge-")

(defcustom smerge-diff-buffer-name "*smerge-diff*"
  "Buffer name to use for displaying diffs."
  :group 'smerge
  :type '(choice
	  (const "*vc-diff*")
	  (const "*cvs-diff*")
	  (const "*smerge-diff*")
	  string))

(defcustom smerge-diff-switches
  (append '("-d" "-b")
	  (if (listp diff-switches) diff-switches (list diff-switches)))
  "*A list of strings specifying switches to be be passed to diff.
Used in `smerge-diff-base-mine' and related functions."
  :group 'smerge
  :type '(repeat string))

(defcustom smerge-auto-leave t
  "*Non-nil means to leave `smerge-mode' when the last conflict is resolved."
  :group 'smerge
  :type 'boolean)

(defface smerge-mine-face
  '((((background light))
     (:foreground "blue"))
    (((background dark))
     (:foreground "cyan")))
  "Face for your code."
  :group 'smerge)
(defvar smerge-mine-face 'smerge-mine-face)

(defface smerge-other-face
  '((((background light))
     (:foreground "darkgreen"))
    (((background dark))
     (:foreground "lightgreen")))
  "Face for the other code."
  :group 'smerge)
(defvar smerge-other-face 'smerge-other-face)

(defface smerge-base-face
  '((((background light))
     (:foreground "red"))
    (((background dark))
     (:foreground "orange")))
  "Face for the base code."
  :group 'smerge)
(defvar smerge-base-face 'smerge-base-face)

(defface smerge-markers-face
  '((((background light))
     (:background "grey85"))
    (((background dark))
     (:background "grey30")))
  "Face for the conflict markers."
  :group 'smerge)
(defvar smerge-markers-face 'smerge-markers-face)

(easy-mmode-defmap smerge-basic-map
  `(("n" . smerge-next)
    ("p" . smerge-prev)
    ("a" . smerge-keep-all)
    ("b" . smerge-keep-base)
    ("o" . smerge-keep-other)
    ("m" . smerge-keep-mine)
    ("E" . smerge-ediff)
    ("\C-m" . smerge-keep-current)
    ("=" . ,(make-sparse-keymap "Diff"))
    ("=<" "base-mine" . smerge-diff-base-mine)
    ("=>" "base-other" . smerge-diff-base-other)
    ("==" "mine-other" . smerge-diff-mine-other))
  "The base keymap for `smerge-mode'.")

(defcustom smerge-command-prefix "\C-c^"
  "Prefix for `smerge-mode' commands."
  :group 'smerge
  :type '(choice (string "\e") (string "\C-c^") (string "") string))

(easy-mmode-defmap smerge-mode-map
  `((,smerge-command-prefix . ,smerge-basic-map))
  "Keymap for `smerge-mode'.")

(easy-menu-define smerge-mode-menu smerge-mode-map
  "Menu for `smerge-mode'."
  '("SMerge"
    ["Next" smerge-next :help "Go to next conflict"]
    ["Previous" smerge-prev :help "Go to previous conflict"]
    ["Keep All" smerge-keep-all :help "Keep all three versions"]
    ["Revert to Base" smerge-keep-base :help "Revert to base version"]
    ["Keep Other" smerge-keep-other :help "Keep `other' version"]
    ["Keep Yours" smerge-keep-mine :help "Keep your version"]
    ["Keep Current" smerge-keep-current :help "Use current (at point) version"]
    "--"
    ["Diff Base/Mine" smerge-diff-base-mine
     :help "Diff `base' and `mine' for current conflict"]
    ["Diff Base/Other" smerge-diff-base-other
     :help "Diff `base' and `other' for current conflict"]
    ["Diff Mine/Other" smerge-diff-mine-other
     :help "Diff `mine' and `other' for current conflict"]
    "--"
    ["Invoke Ediff" smerge-ediff
     :help "Use Ediff to resolve the conflicts"]
    ))

(defconst smerge-font-lock-keywords
  '((smerge-find-conflict
     (1 smerge-mine-face prepend t)
     (2 smerge-base-face prepend t)
     (3 smerge-other-face prepend t)
     ;; FIXME: `keep' doesn't work right with syntactic fontification.
     (0 smerge-markers-face keep)
     (4 nil t t)
     (5 nil t t)))
  "Font lock patterns for `smerge-mode'.")

(defconst smerge-begin-re "^<<<<<<< \\(.*\\)\n")
(defconst smerge-end-re "^>>>>>>> .*\n")
(defconst smerge-base-re "^||||||| .*\n")
(defconst smerge-other-re "^=======\n")

(defvar smerge-conflict-style nil
  "Keep track of which style of conflict is in use.
Can be nil if the style is undecided, or else:
- `diff3-E'
- `diff3-A'")

;; Compiler pacifiers
(defvar font-lock-mode)
(defvar font-lock-keywords)
(eval-when-compile
  (unless (fboundp 'font-lock-fontify-region)
    (autoload 'font-lock-fontify-region "font-lock")))

;;;;
;;;; Actual code
;;;;

;; Define smerge-next and smerge-prev
(easy-mmode-define-navigation smerge smerge-begin-re "conflict")

(defconst smerge-match-names ["conflict" "mine" "base" "other"])

(defun smerge-ensure-match (n)
  (unless (match-end n)
    (error (format "No `%s'" (aref smerge-match-names n)))))

(defun smerge-auto-leave ()
  (when (and smerge-auto-leave
	     (save-excursion (goto-char (point-min))
			     (not (re-search-forward smerge-begin-re nil t))))
    (smerge-mode -1)))
    

(defun smerge-keep-all ()
  "Keep all three versions.
Convenient for the kind of conflicts that can arise in ChangeLog files."
  (interactive)
  (smerge-match-conflict)
  (replace-match (concat (or (match-string 1) "")
			 (or (match-string 2) "")
			 (or (match-string 3) ""))
		 t t)
  (smerge-auto-leave))

(defun smerge-keep-base ()
  "Revert to the base version."
  (interactive)
  (smerge-match-conflict)
  (smerge-ensure-match 2)
  (replace-match (match-string 2) t t)
  (smerge-auto-leave))

(defun smerge-keep-other ()
  "Use \"other\" version."
  (interactive)
  (smerge-match-conflict)
  ;;(smerge-ensure-match 3)
  (replace-match (match-string 3) t t)
  (smerge-auto-leave))

(defun smerge-keep-mine ()
  "Keep your version."
  (interactive)
  (smerge-match-conflict)
  ;;(smerge-ensure-match 1)
  (replace-match (match-string 1) t t)
  (smerge-auto-leave))

(defun smerge-keep-current ()
  "Use the current (under the cursor) version."
  (interactive)
  (smerge-match-conflict)
  (let ((i 3))
    (while (or (not (match-end i))
	       (< (point) (match-beginning i))
	       (>= (point) (match-end i)))
      (decf i))
    (if (<= i 0) (error "Not inside a version")
      (replace-match (match-string i) t t)
      (smerge-auto-leave))))

(defun smerge-diff-base-mine ()
  "Diff 'base' and 'mine' version in current conflict region."
  (interactive)
  (smerge-diff 2 1))

(defun smerge-diff-base-other ()
  "Diff 'base' and 'other' version in current conflict region."
  (interactive)
  (smerge-diff 2 3))

(defun smerge-diff-mine-other ()
  "Diff 'mine' and 'other' version in current conflict region."
  (interactive)
  (smerge-diff 1 3))

(defun smerge-match-conflict ()
  "Get info about the conflict.  Puts the info in the `match-data'.
The submatches contain:
 0:  the whole conflict.
 1:  your code.
 2:  the base code.
 3:  other code.
An error is raised if not inside a conflict."
  (save-excursion
    (condition-case nil
	(let* ((orig-point (point))

	       (_ (forward-line 1))
	       (_ (re-search-backward smerge-begin-re))

	       (start (match-beginning 0))
	       (mine-start (match-end 0))
	       (filename (match-string 1))

	       (_ (re-search-forward smerge-end-re))
	       (_ (assert (< orig-point (match-end 0))))
	       
	       (other-end (match-beginning 0))
	       (end (match-end 0))

	       (_ (re-search-backward smerge-other-re start))

	       (mine-end (match-beginning 0))
	       (other-start (match-end 0))

	       base-start base-end)

	  ;; handle the various conflict styles
	  (cond
	   ((re-search-backward smerge-base-re start t)
	    ;; a 3-parts conflict
	    (set (make-local-variable 'smerge-conflict-style) 'diff3-A)
	    (setq base-end mine-end)
	    (setq mine-end (match-beginning 0))
	    (setq base-start (match-end 0)))

	  ((string= filename (file-name-nondirectory
			      (or buffer-file-name "")))
	   ;; a 2-parts conflict
	   (set (make-local-variable 'smerge-conflict-style) 'diff3-E))

	  ((and (not base-start)
		(or (eq smerge-conflict-style 'diff3-A)
		    (string-match "^[.0-9]+\\'" filename)))
	   ;; a same-diff conflict
	   (setq base-start mine-start)
	   (setq base-end   mine-end)
	   (setq mine-start other-start)
	   (setq mine-end   other-end)))
	       
	  (store-match-data (list start end
				  mine-start mine-end
				  base-start base-end
				  other-start other-end
				  (when base-start (1- base-start)) base-start
				  (1- other-start) other-start))
	  t)
      (search-failed (error "Point not in conflict region")))))

(defun smerge-find-conflict (&optional limit)
  "Find and match a conflict region.  Intended as a font-lock MATCHER.
The submatches are the same as in `smerge-match-conflict'.
Returns non-nil if a match is found between the point and LIMIT.
The point is moved to the end of the conflict."
  (when (re-search-forward smerge-begin-re limit t)
    (ignore-errors
      (smerge-match-conflict)
      (goto-char (match-end 0)))))

(defun smerge-diff (n1 n2)
  (smerge-match-conflict)
  (smerge-ensure-match n1)
  (smerge-ensure-match n2)
  (let ((name1 (aref smerge-match-names n1))
	(name2 (aref smerge-match-names n2))
	;; Read them before the match-data gets clobbered.
	(beg1 (match-beginning n1))
	(end1 (match-end n1))
	(beg2 (match-beginning n2))
	(end2 (match-end n2))
	(file1 (make-temp-file "smerge1"))
	(file2 (make-temp-file "smerge2"))
	(dir default-directory)
	(file (file-relative-name buffer-file-name))
	(coding-system-for-read buffer-file-coding-system))
    (write-region beg1 end1 file1)
    (write-region beg2 end2 file2)
    (unwind-protect
	(with-current-buffer (get-buffer-create smerge-diff-buffer-name)
	  (setq default-directory dir)
	  (let ((inhibit-read-only t))
	    (erase-buffer)
	    (apply 'call-process diff-command nil t nil
		   (append smerge-diff-switches
			   (list "-L" (concat name1 "/" file)
				 "-L" (concat name2 "/" file)
				 file1 file2))))
	  (goto-char (point-min))
	  (diff-mode)
	  (display-buffer (current-buffer) t))
      (delete-file file1)
      (delete-file file2))))

(eval-when-compile
  ;; compiler pacifiers
  (defvar smerge-ediff-windows)
  (defvar smerge-ediff-buf)
  (defvar ediff-buffer-A)
  (defvar ediff-buffer-B)
  (defvar ediff-buffer-C)
  (unless (fboundp 'ediff-cleanup-mess)
    (autoload 'ediff-cleanup-mess "ediff-util")))

(defun smerge-ediff ()
  "Invoke ediff to resolve the conflicts."
  (interactive)
  (let* ((buf (current-buffer))
	 (mode major-mode)
	 ;;(ediff-default-variant 'default-B)
	 (config (current-window-configuration))
	 (filename (file-name-nondirectory buffer-file-name))
	 (mine (generate-new-buffer (concat "*" filename " MINE*")))
	 (other (generate-new-buffer (concat "*" filename " OTHER*")))
	 base)
    (with-current-buffer mine
      (buffer-disable-undo)
      (insert-buffer-substring buf)
      (goto-char (point-min))
      (while (smerge-find-conflict)
	(when (match-beginning 2) (setq base t))
	(replace-match (match-string 1) t t))
      (buffer-enable-undo)
      (set-buffer-modified-p nil)
      (funcall mode))

    (with-current-buffer other
      (buffer-disable-undo)
      (insert-buffer-substring buf)
      (goto-char (point-min))
      (while (smerge-find-conflict)
	(replace-match (match-string 3) t t))
      (buffer-enable-undo)
      (set-buffer-modified-p nil)
      (funcall mode))
    
    (when base
      (setq base (generate-new-buffer (concat "*" filename " BASE*")))
      (with-current-buffer base
	(buffer-disable-undo)
	(insert-buffer-substring buf)
	(goto-char (point-min))
	(while (smerge-find-conflict)
	  (replace-match (or (match-string 2) "") t t))
	(buffer-enable-undo)
	(set-buffer-modified-p nil)
	(funcall mode)))
    
    ;; the rest of the code is inspired from vc.el
    ;; Fire up ediff.
    (set-buffer
     (if base
	 (ediff-merge-buffers-with-ancestor mine other base)
	  ;; nil 'ediff-merge-revisions-with-ancestor buffer-file-name)
       (ediff-merge-buffers mine other)))
        ;; nil 'ediff-merge-revisions buffer-file-name)))
    
    ;; Ediff is now set up, and we are in the control buffer.
    ;; Do a few further adjustments and take precautions for exit.
    (set (make-local-variable 'smerge-ediff-windows) config)
    (set (make-local-variable 'smerge-ediff-buf) buf)
    (set (make-local-variable 'ediff-quit-hook)
	 (lambda ()
	   (let ((buffer-A ediff-buffer-A)
		 (buffer-B ediff-buffer-B)
		 (buffer-C ediff-buffer-C)
		 (buffer-Ancestor ediff-ancestor-buffer)
		 (buf smerge-ediff-buf)
		 (windows smerge-ediff-windows))
	     (ediff-cleanup-mess)
	     (with-current-buffer buf
	       (erase-buffer)
	       (insert-buffer buffer-C)
	       (kill-buffer buffer-A)
	       (kill-buffer buffer-B)
	       (kill-buffer buffer-C)
	       (when (bufferp buffer-Ancestor) (kill-buffer buffer-Ancestor))
	       (set-window-configuration windows)
	       (message "Conflict resolution finished; you may save the buffer")))))
    (message "Please resolve conflicts now; exit ediff when done")))


;;;###autoload
(define-minor-mode smerge-mode
  "Minor mode to simplify editing output from the diff3 program.
\\{smerge-mode-map}"
  nil " SMerge" nil
  (when (and (boundp 'font-lock-mode) font-lock-mode)
    (set (make-local-variable 'font-lock-multiline) t)
    (save-excursion
      (if smerge-mode
	  (font-lock-add-keywords nil smerge-font-lock-keywords 'append)
	(font-lock-remove-keywords nil smerge-font-lock-keywords))
      (goto-char (point-min))
      (while (smerge-find-conflict)
	(save-excursion
	  (font-lock-fontify-region (match-beginning 0) (match-end 0) nil))))))


(provide 'smerge-mode)
;;; smerge-mode.el ends here
