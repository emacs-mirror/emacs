;;; diff-mode.el --- A mode for viewing/editing context diffs

;; Copyright (C) 1998, 1999, 2000  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@cs.yale.edu>
;; Keywords: patch diff
;; Revision: $Id: diff-mode.el,v 1.18 2000/09/20 06:40:30 miles Exp $

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

;; Provides support for font-lock patterns, outline-regexps, navigation
;; commands, editing and various conversions as well as jumping
;; to the corresponding source file.

;; inspired by Pavel Machek's patch-mode.el (<pavel@atrey.karlin.mff.cuni.cz>)
;; some efforts were spent to have it somewhat compatible with XEmacs'
;; diff-mode as well as with compilation-minor-mode

;; to use it, simply add to your .emacs the following lines:
;; 
;; (autoload 'diff-mode "diff-mode" "Diff major mode" t)
;; (add-to-list 'auto-mode-alist '("\\.\\(diffs?\\|patch\\|rej\\)\\'" . diff-mode))

;; Bugs:

;; - Reverse doesn't work with normal diffs.
;; - (nitpick) The mark is not always quite right in diff-goto-source.

;; Todo:

;; - Add change-log support.
;; - Spice up the minor-mode with font-lock support.
;; - Improve narrowed-view support.
;; - Improve the `compile' support (?).
;; - Recognize pcl-cvs' special string for `cvs-execute-single'.
;; - Support for # comments in context->unified.
;; - Do a fuzzy search in diff-goto-source.
;; - Allow diff.el to use diff-mode.
;;   This mostly means ability to jump from half-hunk to half-hunk
;;   in context (and normal) diffs and to jump to the corresponding
;;   (i.e. new or old) file.
;; - Handle `diff -b' output in context->unified.

;;; Code:

(eval-when-compile (require 'cl))


(defgroup diff-mode ()
  "Major-mode for viewing/editing diffs"
  :version "21.1"
  :group 'tools
  :group 'diff)

(defcustom diff-jump-to-old-file-flag nil
  "*Non-nil means `diff-goto-source' jumps to the old file.
Else, it jumps to the new file."
  :group 'diff-mode
  :type '(boolean))

(defcustom diff-update-on-the-fly-flag t
  "*Non-nil means hunk headers are kept up-to-date on-the-fly.
When editing a diff file, the line numbers in the hunk headers
need to be kept consistent with the actual diff.  This can
either be done on the fly (but this sometimes interacts poorly with the
undo mechanism) or whenever the file is written (can be slow
when editing big diffs)."
  :group 'diff-mode
  :type '(boolean))

(defvar diff-mode-hook nil
  "Run after setting up the `diff-mode' major mode.")

(defvar diff-outline-regexp
  "\\([*+][*+][*+] [^0-9]\\|@@ ...\\|\\*\\*\\* [0-9].\\|--- [0-9]..\\)")

;;;; 
;;;; keymap, menu, ...
;;;; 

(easy-mmode-defmap diff-mode-shared-map
  '(;; From Pavel Machek's patch-mode.
    ("n" . diff-hunk-next)
    ("N" . diff-file-next)
    ("p" . diff-hunk-prev)
    ("P" . diff-file-prev)
    ("k" . diff-hunk-kill)
    ("K" . diff-file-kill)
    ;; From compilation-minor-mode.
    ("}" . diff-file-next)
    ("{" . diff-file-prev)
    ("\C-m" . diff-goto-source)
    ([mouse-2] . diff-mouse-goto-source)
    ;; From XEmacs' diff-mode.
    ("W" . widen)
    ;;("." . diff-goto-source)		;display-buffer
    ;;("f" . diff-goto-source)		;find-file
    ("o" . diff-goto-source)		;other-window
    ;;("w" . diff-goto-source)		;other-frame
    ;;("N" . diff-narrow)
    ;;("h" . diff-show-header)
    ;;("j" . diff-show-difference)	;jump to Nth diff
    ;;("q" . diff-quit)
    (" " . scroll-up)
    ("\177" . scroll-down)
    ;; Our very own bindings.
    ("A" . diff-ediff-patch)
    ("r" . diff-restrict-view)
    ("R" . diff-reverse-direction)
    ("U" . diff-context->unified)
    ("C" . diff-unified->context))
  "Basic keymap for `diff-mode', bound to various prefix keys.")

(easy-mmode-defmap diff-mode-map
  `(("\e" . ,diff-mode-shared-map)
    ;; From compilation-minor-mode.
    ("\C-c\C-c" . diff-goto-source)
    ;; Misc operations.
    ("\C-cda" . diff-apply-hunk)
    ("\C-cdt" . diff-test-hunk))
  "Keymap for `diff-mode'.  See also `diff-mode-shared-map'.")

(easy-menu-define diff-mode-menu diff-mode-map
  "Menu for `diff-mode'."
  '("Diff"
    ["Jump to Source"		diff-goto-source	t]
    ["Apply with Ediff"		diff-ediff-patch	t]
    ["-----" nil nil]
    ["Reverse direction"	diff-reverse-direction	t]
    ["Context -> Unified"	diff-context->unified	t]
    ["Unified -> Context"	diff-unified->context	t]
    ;;["Fixup Headers"		diff-fixup-modifs	(not buffer-read-only)]
    ))

(defcustom diff-minor-mode-prefix "\C-cd"
  "Prefix key for `diff-minor-mode' commands."
  :group 'diff-mode
  :type '(choice (string "\e") (string "C-cd") string))

(easy-mmode-defmap diff-minor-mode-map
  `((,diff-minor-mode-prefix . ,diff-mode-shared-map))
  "Keymap for `diff-minor-mode'.  See also `diff-mode-shared-map'.")


;;;; 
;;;; font-lock support
;;;; 

(defface diff-header-face
  '((t (:inherit highlight)))
  "`diff-mode' face inherited by hunk, file and index header faces."
  :group 'diff-mode)
(defvar diff-header-face 'diff-header-face)

(defface diff-file-header-face
  '((t (:bold t))) ;; :height 1.3
  "`diff-mode' face used to highlight file header lines."
  :group 'diff-mode)
(defvar diff-file-header-face 'diff-file-header-face)

(defface diff-index-face
  '((t (:inherit diff-file-header-face)))
  "`diff-mode' face used to highlight index header lines."
  :group 'diff-mode)
(defvar diff-index-face 'diff-index-face)

(defface diff-hunk-header-face
  '((t (:inherit diff-header-face)))
  "`diff-mode' face used to highlight hunk header lines."
  :group 'diff-mode)
(defvar diff-hunk-header-face 'diff-hunk-header-face)

(defface diff-removed-face
  '((t (:inherit diff-changed-face)))
  "`diff-mode' face used to highlight removed lines."
  :group 'diff-mode)
(defvar diff-removed-face 'diff-removed-face)

(defface diff-added-face
  '((t (:inherit diff-changed-face)))
  "`diff-mode' face used to highlight added lines."
  :group 'diff-mode)
(defvar diff-added-face 'diff-added-face)

(defface diff-changed-face
  '((t ()))
  "`diff-mode' face used to highlight changed lines."
  :group 'diff-mode)
(defvar diff-changed-face 'diff-changed-face)

(defface diff-comment-face
  '((t (:inherit font-lock-comment-face)))
  "`diff-mode' face used to highlight context and other side-information."
  :group 'diff-mode)
(defvar diff-comment-face 'diff-comment-face)

(defvar diff-font-lock-keywords
  '(("^\\(@@ -[0-9,]+ \\+[0-9,]+ @@\\)\\(.*\\)$" ;unified
     (1 diff-hunk-header-face)
     (2 diff-comment-face))
    ("^--- .+ ----$"		;context
     . diff-hunk-header-face)
    ("\\(\\*\\{15\\}\\)\\(.*\\)$"	;context
     (1 diff-hunk-header-face)
     (2 diff-comment-face))
    ("^\\*\\*\\* .+ \\*\\*\\*\\*"	;context
     . diff-hunk-header-face)
    ("^\\(---\\|\\+\\+\\+\\|\\*\\*\\*\\) \\(\\S-+\\).*[^*-]\n"
     (0 diff-header-face) (2 diff-file-header-face prepend))
    ("^[0-9,]+[acd][0-9,]+$" . diff-hunk-header-face)
    ("^!.*\n" . diff-changed-face)	;context
    ("^[+>].*\n" . diff-added-face)
    ("^[-<].*\n" . diff-removed-face)
    ("^Index: \\(.+\\).*\n" (0 diff-header-face) (1 diff-index-face prepend))
    ("^#.*" . font-lock-string-face)
    ("^[^-=+*!<>].*\n" . diff-comment-face)))

(defconst diff-font-lock-defaults
  '(diff-font-lock-keywords t nil nil nil (font-lock-multiline . nil)))

(defvar diff-imenu-generic-expression
  ;; Prefer second name as first is most likely to be a backup or
  ;; version-control name.
  '((nil "\\+\\+\\+\\ \\([^\t\n]+\\)\t" 1) ; unidiffs
    (nil "^--- \\([^\t\n]+\\)\t.*\n\\*" 1))) ; context diffs

;;;;
;;;; Compile support
;;;;

(defvar diff-file-regexp-alist
  '(("Index: \\(.+\\)" 1)))

(defvar diff-error-regexp-alist
  '(("@@ -\\([0-9]+\\),[0-9]+ \\+\\([0-9]+\\),[0-9]+ @@" nil 2)
    ("--- \\([0-9]+\\),[0-9]+ ----" nil 1)
    ("\\([0-9]+\\)\\(,[0-9]+\\)?[adc]\\([0-9]+\\)" nil 3)))

;;;; 
;;;; Movement
;;;; 

(defconst diff-hunk-header-re "^\\(@@ -[0-9,]+ \\+[0-9,]+ @@.*\\|\\*\\{15\\}.*\n\\*\\*\\* .+ \\*\\*\\*\\*\\|[0-9]+\\(,[0-9]+\\)?[acd][0-9]+\\(,[0-9]+\\)?\\)$")
(defconst diff-file-header-re (concat "^\\(--- .+\n\\+\\+\\+\\|\\*\\*\\* .+\n---\\|[^-+!<>0-9@* ]\\).+\n" (substring diff-hunk-header-re 1)))
(defvar diff-narrowed-to nil)

(defun diff-end-of-hunk (&optional style)
  (if (looking-at diff-hunk-header-re) (goto-char (match-end 0)))
  (let ((end (and (re-search-forward (case style
				       (unified "^[^-+# \\]")
				       (context "^[^-+#! \\]")
				       (normal "^[^<>#\\]")
				       (t "^[^-+#!<> \\]"))
				     nil t)
		  (match-beginning 0))))
    ;; The return value is used by easy-mmode-define-navigation.
    (goto-char (or end (point-max)))))

(defun diff-beginning-of-hunk ()
  (beginning-of-line)
  (unless (looking-at diff-hunk-header-re)
    (forward-line 1)
    (condition-case ()
	(re-search-backward diff-hunk-header-re)
      (error (error "Can't find the beginning of the hunk")))))

(defun diff-beginning-of-file ()
  (beginning-of-line)
  (unless (looking-at diff-file-header-re)
    (forward-line 2)
    (condition-case ()
	(re-search-backward diff-file-header-re)
      (error (error "Can't find the beginning of the file")))))

(defun diff-end-of-file ()
  (re-search-forward "^[-+#!<>0-9@* \\]" nil t)
  (re-search-forward "^[^-+#!<>0-9@* \\]" nil 'move)
  (beginning-of-line))

;; Define diff-{hunk,file}-{prev,next}
(easy-mmode-define-navigation
 diff-hunk diff-hunk-header-re "hunk" diff-end-of-hunk)
(easy-mmode-define-navigation
 diff-file diff-file-header-re "file" diff-end-of-hunk)

(defun diff-restrict-view (&optional arg)
  "Restrict the view to the current hunk.
If the prefix ARG is given, restrict the view to the current file instead."
  (interactive "P")
  (save-excursion
    (if arg (diff-beginning-of-file) (diff-beginning-of-hunk))
    (narrow-to-region (point)
		      (progn (if arg (diff-end-of-file) (diff-end-of-hunk))
			     (point)))
    (set (make-local-variable 'diff-narrowed-to) (if arg 'file 'hunk))))


(defun diff-hunk-kill ()
  "Kill current hunk."
  (interactive)
  (diff-beginning-of-hunk)
  (let ((start (point))
	(firsthunk (save-excursion
		     (ignore-errors
		       (diff-beginning-of-file) (diff-hunk-next) (point))))
	(nexthunk  (save-excursion
		     (ignore-errors
		       (diff-hunk-next) (point))))
	(nextfile (save-excursion
		    (ignore-errors
		      (diff-file-next) (point)))))
    (if (and firsthunk (= firsthunk start)
	     (or (null nexthunk)
		 (and nextfile (> nexthunk nextfile))))
	;; we're the only hunk for this file, so kill the file
	(diff-file-kill)
      (diff-end-of-hunk)
      (kill-region start (point)))))

(defun diff-file-kill ()
  "Kill current file's hunks."
  (interactive)
  (diff-beginning-of-file)
  (let* ((start (point))
	 (prevhunk (save-excursion
		     (ignore-errors
		       (diff-hunk-prev) (point))))
	 (index (save-excursion
		  (re-search-backward "^Index: " prevhunk t))))
    (when index (setq start index))
    (diff-end-of-file)
    (kill-region start (point))))

(defun diff-kill-junk ()
  "Kill spurious empty diffs."
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (while (re-search-forward (concat "^\\(Index: .*\n\\)"
					"\\([^-+!* <>].*\n\\)*?"
					"\\(\\(Index:\\) \\|"
					diff-file-header-re "\\)")
				nil t)
	(delete-region (if (match-end 4) (match-beginning 0) (match-end 1))
		       (match-beginning 3))
	(beginning-of-line)))))

;;;;
;;;; jump to other buffers
;;;;

(defvar diff-remembered-files-alist nil)

(defun diff-filename-drop-dir (file)
  (when (string-match "/" file) (substring file (match-end 0))))

(defun diff-merge-strings (ancestor from to)
  "Merge the diff between ANCESTOR and FROM into TO.
Returns the merged string if successful or nil otherwise.
The strings are assumed not to contain any \"\\n\" (i.e. end of line).
If ANCESTOR = FROM, returns TO.
If ANCESTOR = TO, returns FROM.
The heuristic is simplistic and only really works for cases
like \(diff-merge-strings \"b/foo\" \"b/bar\" \"/a/c/foo\")."
  ;; Ideally, we want:
  ;;   AMB ANB CMD -> CND
  ;; but that's ambiguous if `foo' or `bar' is empty:
  ;; a/foo a/foo1 b/foo.c -> b/foo1.c but not 1b/foo.c or b/foo.c1
  (let ((str (concat ancestor "\n" from "\n" to)))
    (when (and (string-match (concat
			      "\\`\\(.*?\\)\\(.*\\)\\(.*\\)\n"
			      "\\1\\(.*\\)\\3\n"
			      "\\(.*\\(\\2\\).*\\)\\'") str)
	       (equal to (match-string 5 str)))
      (concat (substring str (match-beginning 5) (match-beginning 6))
	      (match-string 4 str)
	      (substring str (match-end 6) (match-end 5))))))

(defun diff-find-file-name (&optional old)
  "Return the file corresponding to the current patch.
Non-nil OLD means that we want the old file."
  (save-excursion
    (unless (looking-at diff-file-header-re)
      (or (ignore-errors (diff-beginning-of-file))
	  (re-search-forward diff-file-header-re nil t)))
    (let* ((limit (save-excursion
		   (condition-case ()
		       (progn (diff-hunk-prev) (point))
		     (error (point-min)))))
	   (header-files
	    (if (looking-at "[-*][-*][-*] \\(\\S-+\\)\\s-.*\n[-+][-+][-+] \\(\\S-+\\)\\s-.*$")
		(list (if old (match-string 1) (match-string 2))
		      (if old (match-string 2) (match-string 1)))
	      (forward-line 1) nil))
	   (fs (append
		(when (save-excursion
			(re-search-backward "^Index: \\(.+\\)" limit t))
		  (list (match-string 1)))
		header-files
		(when (re-search-backward "^diff \\(-\\S-+ +\\)*\\(\\S-+\\)\\( +\\(\\S-+\\)\\)?" nil t)
		  (list (if old (match-string 2) (match-string 4))
			(if old (match-string 4) (match-string 2))))))
	   (fs (delq nil fs)))
      (or
       ;; use any previously used preference
       (cdr (assoc fs diff-remembered-files-alist))
       ;; try to be clever and use previous choices as an inspiration
       (dolist (rf diff-remembered-files-alist)
	 (let ((newfile (diff-merge-strings (caar rf) (car fs) (cdr rf))))
	   (if (and newfile (file-exists-p newfile)) (return newfile))))
       ;; look for each file in turn.  If none found, try again but
       ;; ignoring the first level of directory, ...
       (do* ((files fs (delq nil (mapcar 'diff-filename-drop-dir files)))
	     (file nil nil))
	   ((or (null files)
		(setq file (do* ((files files (cdr files))
				 (file (car files) (car files)))
			       ((or (null file) (file-exists-p file))
				file))))
	    file))
       ;; <foo>.rej patches implicitly apply to <foo>
       (and (string-match "\\.rej\\'" (or buffer-file-name ""))
	    (let ((file (substring buffer-file-name 0 (match-beginning 0))))
	      (when (file-exists-p file) file)))
       ;; if all else fails, ask the user
       (let ((file (read-file-name (format "Use file %s: " (or (first fs) ""))
				   nil (first fs) t (first fs))))
	 (set (make-local-variable 'diff-remembered-files-alist)
	      (cons (cons fs file) diff-remembered-files-alist))
	 file)))))

(defun diff-mouse-goto-source (event)
  "Run `diff-goto-source' for the diff at a mouse click."
  (interactive "e")
  (save-excursion
    (mouse-set-point event)
    (diff-goto-source)))

(defun diff-ediff-patch ()
  "Call `ediff-patch-file' on the current buffer."
  (interactive)
  (condition-case err
      (ediff-patch-file nil (current-buffer))
    (wrong-number-of-arguments (ediff-patch-file))))

;;;; 
;;;; Conversion functions
;;;; 

;;(defvar diff-inhibit-after-change nil
;;  "Non-nil means inhibit `diff-mode's after-change functions.")

(defun diff-unified->context (start end)
  "Convert unified diffs to context diffs.
START and END are either taken from the region (if a prefix arg is given) or
else cover the whole bufer."
  (interactive (if current-prefix-arg
		   (list (mark) (point))
		 (list (point-min) (point-max))))
  (unless (markerp end) (setq end (copy-marker end)))
  (let (;;(diff-inhibit-after-change t)
	(inhibit-read-only t))
    (save-excursion
      (goto-char start)
      (while (and (re-search-forward "^\\(\\(---\\) .+\n\\(\\+\\+\\+\\) .+\\|@@ -\\([0-9]+\\),\\([0-9]+\\) \\+\\([0-9]+\\),\\([0-9]+\\) @@.*\\)$" nil t)
		  (< (point) end))
	(combine-after-change-calls
	  (if (match-beginning 2)
	      ;; we matched a file header
	      (progn
		;; use reverse order to make sure the indices are kept valid
		(replace-match "---" t t nil 3)
		(replace-match "***" t t nil 2))
	    ;; we matched a hunk header
	    (let ((line1 (match-string 4))
		  (lines1 (match-string 5))
		  (line2 (match-string 6))
		  (lines2 (match-string 7)))
	      (replace-match
	       (concat "***************\n*** " line1 ","
		       (number-to-string (+ (string-to-number line1)
					    (string-to-number lines1)
					    -1)) " ****"))
	      (forward-line 1)
	      (save-restriction
		(narrow-to-region (point)
				  (progn (diff-end-of-hunk 'unified) (point)))
		(let ((hunk (buffer-string)))
		  (goto-char (point-min))
		  (if (not (save-excursion (re-search-forward "^-" nil t)))
		      (delete-region (point) (point-max))
		    (goto-char (point-max))
		    (let ((modif nil) last-pt)
		      (while (progn (setq last-pt (point))
				    (= (forward-line -1) 0))
			(case (char-after)
			  (?  (insert " ") (setq modif nil) (backward-char 1))
			  (?+ (delete-region (point) last-pt) (setq modif t))
			  (?- (if (not modif)
				  (progn (forward-char 1)
					 (insert " "))
				(delete-char 1)
				(insert "! "))
			      (backward-char 2))
			  (?\\ (when (save-excursion (forward-line -1)
						     (= (char-after) ?+))
				 (delete-region (point) last-pt) (setq modif t)))
			  (t (setq modif nil))))))
		  (goto-char (point-max))
		  (save-excursion
		    (insert "--- " line2 ","
			    (number-to-string (+ (string-to-number line2)
						 (string-to-number lines2)
						 -1)) " ----\n" hunk))
		  ;;(goto-char (point-min))
		  (forward-line 1)
		  (if (not (save-excursion (re-search-forward "^+" nil t)))
		      (delete-region (point) (point-max))
		    (let ((modif nil) (delete nil))
		      (while (not (eobp))
			(case (char-after)
			  (?  (insert " ") (setq modif nil) (backward-char 1))
			  (?- (setq delete t) (setq modif t))
			  (?+ (if (not modif)
				  (progn (forward-char 1)
					 (insert " "))
				(delete-char 1)
				(insert "! "))
			      (backward-char 2))
			  (?\\ (when (save-excursion (forward-line 1)
						     (not (eobp)))
				 (setq delete t) (setq modif t)))
			  (t (setq modif nil)))
			(let ((last-pt (point)))
			  (forward-line 1)
			  (when delete
			    (delete-region last-pt (point))
			    (setq delete nil)))))))))))))))

(defun diff-context->unified (start end)
  "Convert context diffs to unified diffs.
START and END are either taken from the region (if a prefix arg is given) or
else cover the whole bufer."
  (interactive (if current-prefix-arg
		   (list (mark) (point))
		 (list (point-min) (point-max))))
  (unless (markerp end) (setq end (copy-marker end)))
  (let (;;(diff-inhibit-after-change t)
	(inhibit-read-only t))
    (save-excursion
      (goto-char start)
      (while (and (re-search-forward "^\\(\\(\\*\\*\\*\\) .+\n\\(---\\) .+\\|\\*\\{15\\}.*\n\\*\\*\\* \\([0-9]+\\),\\(-?[0-9]+\\) \\*\\*\\*\\*\\)$" nil t)
		  (< (point) end))
	(combine-after-change-calls
	  (if (match-beginning 2)
	      ;; we matched a file header
	      (progn
		;; use reverse order to make sure the indices are kept valid
		(replace-match "+++" t t nil 3)
		(replace-match "---" t t nil 2))
	    ;; we matched a hunk header
	    (let ((line1s (match-string 4))
		  (line1e (match-string 5))
		  (pt1 (match-beginning 0)))
	      (replace-match "")
	      (unless (re-search-forward
		       "^--- \\([0-9]+\\),\\(-?[0-9]+\\) ----$" nil t)
		(error "Can't find matching `--- n1,n2 ----' line"))
	      (let ((line2s (match-string 1))
		    (line2e (match-string 2))
		    (pt2 (progn
			   (delete-region (progn (beginning-of-line) (point))
					  (progn (forward-line 1) (point)))
			   (point-marker))))
		(goto-char pt1)
		(forward-line 1)
		(while (< (point) pt2)
		  (case (char-after)
		    ((?! ?-) (delete-char 2) (insert "-") (forward-line 1))
		    (?\ 		;merge with the other half of the chunk
		     (let* ((endline2
			     (save-excursion
			       (goto-char pt2) (forward-line 1) (point)))
			    (c (char-after pt2)))
		       (case c
			 ((?! ?+)
			  (insert "+"
				  (prog1 (buffer-substring (+ pt2 2) endline2)
				    (delete-region pt2 endline2))))
			 (?\ 		;FIXME: check consistency
			  (delete-region pt2 endline2)
			  (delete-char 1)
			  (forward-line 1))
			 (?\\ (forward-line 1))
			 (t (delete-char 1) (forward-line 1)))))
		    (t (forward-line 1))))
		(while (looking-at "[+! ] ")
		  (if (/= (char-after) ?!) (forward-char 1)
		    (delete-char 1) (insert "+"))
		  (delete-char 1) (forward-line 1))
		(save-excursion
		  (goto-char pt1)
		  (insert "@@ -" line1s ","
			  (number-to-string (- (string-to-number line1e)
					       (string-to-number line1s)
					       -1))
			  " +" line2s ","
			  (number-to-string (- (string-to-number line2e)
					       (string-to-number line2s)
					       -1)) " @@"))))))))))

(defun diff-reverse-direction (start end)
  "Reverse the direction of the diffs.
START and END are either taken from the region (if a prefix arg is given) or
else cover the whole bufer."
  (interactive (if current-prefix-arg
		   (list (mark) (point))
		 (list (point-min) (point-max))))
  (unless (markerp end) (setq end (copy-marker end)))
  (let (;;(diff-inhibit-after-change t)
	(inhibit-read-only t))
    (save-excursion
      (goto-char start)
      (while (and (re-search-forward "^\\(\\([-*][-*][-*] \\)\\(.+\\)\n\\([-+][-+][-+] \\)\\(.+\\)\\|\\*\\{15\\}.*\n\\*\\*\\* \\(.+\\) \\*\\*\\*\\*\\|@@ -\\([0-9,]+\\) \\+\\([0-9,]+\\) @@.*\\)$" nil t)
		  (< (point) end))
	(combine-after-change-calls
	  (cond
	   ;; a file header
	   ((match-beginning 2) (replace-match "\\2\\5\n\\4\\3" nil))
	   ;; a context-diff hunk header
	   ((match-beginning 6)
	    (let ((pt-lines1 (match-beginning 6))
		  (lines1 (match-string 6)))
	      (replace-match "" nil nil nil 6)
	      (forward-line 1)
	      (let ((half1s (point)))
		(while (looking-at "[-! \\][ \t]\\|#")
		  (when (= (char-after) ?-) (delete-char 1) (insert "+"))
		  (forward-line 1))
		(let ((half1 (delete-and-extract-region half1s (point))))
		  (unless (looking-at "^--- \\([0-9]+,-?[0-9]+\\) ----$")
		    (insert half1)
		    (error "Can't find matching `--- n1,n2 ----' line"))
		  (let ((str1 (match-string 1)))
		    (replace-match lines1 nil nil nil 1)
		    (forward-line 1)
		    (let ((half2s (point)))
		      (while (looking-at "[!+ \\][ \t]\\|#")
			(when (= (char-after) ?+) (delete-char 1) (insert "-"))
			(forward-line 1))
		      (let ((half2 (delete-and-extract-region half2s (point))))
			(insert (or half1 ""))
			(goto-char half1s)
			(insert (or half2 ""))))
		    (goto-char pt-lines1)
		    (insert str1))))))
	   ;; a unified-diff hunk header
	   ((match-beginning 7)
	    (replace-match "@@ -\\8 +\\7 @@" nil)
	    (forward-line 1)
	    (let ((c (char-after)) first last)
	      (while (case (setq c (char-after))
		       (?- (setq first (or first (point)))
			   (delete-char 1) (insert "+") t)
		       (?+ (setq last (or last (point)))
			   (delete-char 1) (insert "-") t)
		       ((?\\ ?#) t)
		       (t (when (and first last (< first last))
			    (let ((str
				   (save-excursion
				     (delete-and-extract-region first last))))
			      (insert str)))
			  (setq first nil last nil)
			  (equal ?\  c)))
		(forward-line 1))))))))))

(defun diff-fixup-modifs (start end)
  "Fixup the hunk headers (in case the buffer was modified).
START and END are either taken from the region (if a prefix arg is given) or
else cover the whole bufer."
  (interactive (if current-prefix-arg
		   (list (mark) (point))
		 (list (point-min) (point-max))))
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char end) (diff-end-of-hunk)
      (let ((plus 0) (minus 0) (space 0) (bang 0))
	(while (and (= (forward-line -1) 0) (<= start (point)))
	  (if (not (looking-at "\\(@@ -[0-9,]+ \\+[0-9,]+ @@.*\\|[-*][-*][-*] .+ [-*][-*][-*][-*]\\)$"))
	      (case (char-after)
		(?\  (incf space))
		(?+ (incf plus))
		(?- (incf minus))
		(?! (incf bang))
		((?\\ ?#) nil)
		(t  (setq space 0 plus 0 minus 0 bang 0)))
	    (cond
	     ((looking-at "@@ -[0-9]+,\\([0-9]*\\) \\+[0-9]+,\\([0-9]*\\) @@.*$")
	      (let* ((old1 (match-string 1))
		     (old2 (match-string 2))
		     (new1 (number-to-string (+ space minus)))
		     (new2 (number-to-string (+ space plus))))
		(unless (string= new2 old2) (replace-match new2 t t nil 2))
		(unless (string= new1 old1) (replace-match new1 t t nil 1))))
	     ((looking-at "--- \\([0-9]+\\),\\([0-9]*\\) ----$")
	      (when (> (+ space bang plus) 0)
		(let* ((old1 (match-string 1))
		       (old2 (match-string 2))
		       (new (number-to-string
			     (+ space bang plus -1 (string-to-number old1)))))
		  (unless (string= new old2) (replace-match new t t nil 2)))))
	     ((looking-at "\\*\\*\\* \\([0-9]+\\),\\(-?[0-9]*\\) \\*\\*\\*\\*$")
	      (when (> (+ space bang minus) 0)
		(let* ((old (match-string 1))
		       (new (format
			     (concat "%0" (number-to-string (length old)) "d")
			     (+ space bang minus -1 (string-to-number old)))))
		  (unless (string= new old) (replace-match new t t nil 2))))))
	    (setq space 0 plus 0 minus 0 bang 0)))))))

;;;; 
;;;; Hooks
;;;; 

(defun diff-write-contents-hooks ()
  "Fixup hunk headers if necessary."
  (if (buffer-modified-p) (diff-fixup-modifs (point-min) (point-max)))
  nil)

;; It turns out that making changes in the buffer from within an
;; *-change-function is asking for trouble, whereas making them
;; from a post-command-hook doesn't pose much problems
(defvar diff-unhandled-changes nil)
(defun diff-after-change-function (beg end len)
  "Remember to fixup the hunk header.
See `after-change-functions' for the meaning of BEG, END and LEN."
  ;; Ignoring changes when inhibit-read-only is set is strictly speaking
  ;; incorrect, but it turns out that inhibit-read-only is normally not set
  ;; inside editing commands, while it tends to be set when the buffer gets
  ;; updated by an async process or by a conversion function, both of which
  ;; would rather not be uselessly slowed down by this hook.
  (when (and (not undo-in-progress) (not inhibit-read-only))
    (if diff-unhandled-changes
	(setq diff-unhandled-changes
	      (cons (min beg (car diff-unhandled-changes))
		    (max beg (cdr diff-unhandled-changes))))
      (setq diff-unhandled-changes (cons beg end)))))

(defun diff-post-command-hook ()
  "Fixup hunk headers if necessary."
  (when (consp diff-unhandled-changes)
    (ignore-errors
      (save-excursion
	(goto-char (car diff-unhandled-changes))
	(unless (ignore-errors
		  (diff-beginning-of-hunk)
		  (save-excursion
		    (diff-end-of-hunk)
		    (> (point) (car diff-unhandled-changes))))
	  (goto-char (car diff-unhandled-changes))
	  (re-search-forward diff-hunk-header-re (cdr diff-unhandled-changes))
	  (diff-beginning-of-hunk))
	(diff-fixup-modifs (point) (cdr diff-unhandled-changes))))
    (setq diff-unhandled-changes nil)))

;;;; 
;;;; The main function
;;;; 

;;;###autoload
(define-derived-mode diff-mode fundamental-mode "Diff"
  "Major mode for viewing/editing context diffs.
Supports unified and context diffs as well as (to a lesser extent) normal diffs.
When the buffer is read-only, the ESC prefix is not necessary.
This mode runs `diff-mode-hook'.
\\{diff-mode-map}"
  (set (make-local-variable 'font-lock-defaults) diff-font-lock-defaults)
  (set (make-local-variable 'outline-regexp) diff-outline-regexp)
  (set (make-local-variable 'imenu-generic-expression)
       diff-imenu-generic-expression)
  ;; These are not perfect.  They would be better done separately for
  ;; context diffs and unidiffs.
  ;; (set (make-local-variable 'paragraph-start)
  ;;        (concat "@@ "			; unidiff hunk
  ;; 	       "\\|\\*\\*\\* "		; context diff hunk or file start
  ;; 	       "\\|--- [^\t]+\t"))	; context or unidiff file
  ;; 					; start (first or second line)
  ;;   (set (make-local-variable 'paragraph-separate) paragraph-start)
  ;;   (set (make-local-variable 'page-delimiter) "--- [^\t]+\t")
  ;; compile support
  (set (make-local-variable 'compilation-file-regexp-alist)
       diff-file-regexp-alist)
  (set (make-local-variable 'compilation-error-regexp-alist)
       diff-error-regexp-alist)
  (when (string-match "\\.rej\\'" (or buffer-file-name ""))
    (set (make-local-variable 'compilation-current-file)
	 (substring buffer-file-name 0 (match-beginning 0))))
  (compilation-shell-minor-mode 1)
  ;; setup change hooks
  (toggle-read-only t)
  (if (not diff-update-on-the-fly-flag)
      (add-hook 'write-contents-hooks 'diff-write-contents-hooks)
    (make-local-variable 'diff-unhandled-changes)
    (add-hook (make-local-hook 'after-change-functions)
	      'diff-after-change-function nil t)
    (add-hook (make-local-hook 'post-command-hook)
	      'diff-post-command-hook nil t))
  ;; Neat trick from Dave Love to add more bindings in read-only mode:
  (add-to-list (make-local-variable 'minor-mode-overriding-map-alist)
  	       (cons 'buffer-read-only diff-mode-shared-map)))

;;;###autoload
(define-minor-mode diff-minor-mode
  "Minor mode for viewing/editing context diffs.
\\{diff-minor-mode-map}"
  nil " Diff" nil
  ;; FIXME: setup font-lock
  ;; setup change hooks
  (if (not diff-update-on-the-fly-flag)
      (add-hook 'write-contents-hooks 'diff-write-contents-hooks)
    (make-local-variable 'diff-unhandled-changes)
    (add-hook (make-local-hook 'after-change-functions)
	      'diff-after-change-function nil t)
    (add-hook (make-local-hook 'post-command-hook)
	      'diff-post-command-hook nil t)))


;;;
;;; Misc operations that have proved useful at some point.
;;;

(defun diff-next-complex-hunk ()
  "Jump to the next \"complex\" hunk.
\"Complex\" is approximated by \"the hunk changes the number of lines\".
Only works for unified diffs."
  (interactive)
  (while
      (and (re-search-forward "^@@ [-0-9]+,\\([0-9]+\\) [+0-9]+,\\([0-9]+\\) @@"
			      nil t)
	   (equal (match-string 1) (match-string 2)))))

(defun diff-hunk-text (hunk destp &optional line-offset)
  "Returns the literal source text from HUNK.
if DESTP is nil return the source, otherwise the destination text.
If LINE-OFFSET is non-nil, it should be a line-offset in
HUNK, and instead of a string, a cons cell is returned whose car is the
appropriate text, and whose cdr is the corresponding line-offset in that text."
  (with-temp-buffer
     (insert hunk)
     (goto-char (point-min))
     (let ((src-pos nil)
	   (dst-pos nil)
	   (divider-pos nil)
	   (num-pfx-chars 2))
       ;; Set the following variables:
       ;;  SRC-POS     buffer pos of the source part of the hunk or nil if none
       ;;  DST-POS     buffer pos of the destination part of the hunk or nil
       ;;  DIVIDER-POS buffer pos of any divider line separating the src & dst
       ;;  NUM-PFX-CHARS  number of line-prefix characters used by this format"
       (cond ((looking-at "^@@")
	      ;; unified diff
	      (setq num-pfx-chars 1)
	      (forward-line 1)
	      (setq src-pos (point) dst-pos (point)))
	     ((looking-at "^\\*\\*")
	      ;; context diff
	      (forward-line 2)
	      (setq src-pos (point))
	      (re-search-forward "^--- " nil t)
	      (forward-line 0)
	      (setq divider-pos (point))
	      (forward-line 1)
	      (setq dst-pos (point)))
	     ((looking-at "^[0-9]+a[0-9,]+$")
	      ;; normal diff, insert
	      (forward-line 1)
	      (setq dst-pos (point)))
	     ((looking-at "^[0-9,]+d[0-9]+$")
	      ;; normal diff, delete
	      (forward-line 1)
	      (setq src-pos (point)))
	     ((looking-at "^[0-9,]+c[0-9,]+$")
	      ;; normal diff, change
	      (forward-line 1)
	      (setq src-pos (point))
	      (re-search-forward "^---$" nil t)
	      (forward-line 0)
	      (setq divider-pos (point))
	      (forward-line 1)
	      (setq dst-pos (point)))
	     (t
	      (error "Unknown diff hunk type")))
    (if (if destp (null dst-pos) (null src-pos))
	;; Implied empty text
	(if line-offset '("" . 0) "")

      (when line-offset
	(goto-char (point-min))
	(forward-line line-offset))

      ;; Get rid of anything except the desired text.
      (save-excursion
	;; Delete unused text region
	(let ((keep (if destp dst-pos src-pos))
	      (kill (or divider-pos (if destp src-pos dst-pos))))
	  (when (and kill (> kill keep))
	    (delete-region kill (point-max)))
	  (delete-region (point-min) keep))
	;; Remove line-prefix characters, and unneeded lines (unified diffs).
	(let ((kill-char (if destp ?- ?+)))
	  (goto-char (point-min))
	  (while (not (eobp))
	    (if (eq (char-after) kill-char)
		(delete-region (point) (progn (forward-line 1) (point)))
	      (delete-char num-pfx-chars)
	      (forward-line 1)))))

      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
	(if line-offset
	    (cons text (count-lines (point-min) (point)))
	  text))))))

(defun diff-find-text (text)
  "Return the buffer position of the nearest occurance of TEXT.
If TEXT isn't found, nil is returned."
  (let* ((orig (point))
	 (forw (and (search-forward text nil t)
			  (match-beginning 0)))
	 (back (and (goto-char (+ orig (length text)))
		    (search-backward text nil t)
			  (match-beginning 0))))
	  ;; Choose the closest match.
    (if (and forw back)
	(if (> (- forw orig) (- orig back)) back forw)
      (or back forw))))

(defun diff-find-source-location (&optional other-file reverse)
  "Find out (BUF LINE POS SRC DST SWITCHED)."
  (save-excursion
    (let* ((old (if (not other-file) diff-jump-to-old-file-flag
		  (not diff-jump-to-old-file-flag)))
	   (orig-point (point))
	   (hunk-line-offset
	    (progn (diff-beginning-of-hunk) (count-lines (point) orig-point)))
	   ;; Find the location specification.
	   (line (if (not (looking-at "\\(?:\\*\\{15\\}.*\n\\)?[-@* ]*\\([0-9,]+\\)\\([ acd+]+\\([0-9,]+\\)\\)?"))
		    (error "Can't find the hunk header")
		  (if old (match-string 1)
		    (if (match-end 3) (match-string 3)
		      (unless (re-search-forward "^--- \\([0-9,]+\\)" nil t)
			(error "Can't find the hunk separator"))
		      (match-string 1)))))
	   (file (or (diff-find-file-name old) (error "Can't find the file")))
	   (buf (find-file-noselect file))
	   (hunk
	    (buffer-substring (point) (progn (diff-end-of-hunk) (point))))
	   (old (diff-hunk-text hunk reverse hunk-line-offset))
	   (new (diff-hunk-text hunk (not reverse) hunk-line-offset)))
      ;; Update the user preference if he so wished.
      (when (> (prefix-numeric-value other-file) 8)
	(setq diff-jump-to-old-file-flag old))
      (with-current-buffer buf
	(goto-line (string-to-number line))
	(let* ((orig-pos (point))
	       (pos (diff-find-text (car old)))
	       (switched nil))
	  (when (null pos)
	    (setq pos (diff-find-text (car new)) switched t))
	  (list* buf (string-to-number line) pos
		 (if switched (list new old t) (list old new))))))))

(defun diff-apply-hunk (&optional reverse other-file dry-run popup noerror)
  "Apply the current hunk to the source file.
By default, the new source file is patched, but if the variable
`diff-jump-to-old-file-flag' is non-nil, then the old source file is
patched instead (some commands, such as `diff-goto-source' can change
the value of this variable when given an appropriate prefix argument).

With a prefix argument, REVERSE the hunk.
If OTHER-FILE is non-nil, patch the old file by default, and reverse the
  sense of `diff-jump-to-old-file-flag'.
If DRY-RUN is non-nil, don't actually modify anything, just see whether
  it's possible to do so.
If POPUP is non-nil, pop up the patched file in another window; if POPUP
  is `select' then select the new window too.
If NOERROR is non-nil, then no error is signaled in the case where the hunk
  cannot be found in the source file (other errors may still be signaled).

Return values are t if the hunk was sucessfully applied (or could be
applied, in the case where DRY-RUN was non-nil), `reversed' if the hunk
was applied backwards, or nil if the hunk couldn't be found and NOERROR
was non-nil."
  (interactive (list current-prefix-arg nil nil t))

  (when other-file
    ;; OTHER-FILE inverts the sense of the hunk
    (setq reverse (not reverse)))
  (when diff-jump-to-old-file-flag
    ;; The global variable `diff-jump-to-old-file-flag' inverts the
    ;; sense of OTHER-FILE (in `diff-find-source-location')
    (setq reverse (not reverse)))

  (destructuring-bind (buf patch-line pos old new &optional switched)
      (diff-find-source-location other-file reverse)

    (when (and pos switched popup)
      ;; A reversed patch was detected, perhaps apply it in reverse
      ;; (this is only done in `interactive' mode, when POPUP is non-nil).
      (if (or dry-run
	      (save-window-excursion
		(pop-to-buffer buf)
		(goto-char pos)
		(forward-line (cdr old))
		(if reverse
		    (y-or-n-p
		     "Hunk hasn't been applied yet, so can't reverse it; apply it now? ")
		  (y-or-n-p "Hunk has already been applied; undo it? "))))

	  nil
	;; The user has chosen not to apply the reversed hunk, but we
	;; don't want to given an error message, so set things up so
	;; nothing else gets done down below
	(setq pos nil)
	(message "(Nothing done)")
	(setq noerror t)))

    (if (null pos)
	;; POS is nil, so we couldn't find the source text.
	(unless noerror
	  (error "Can't find the text to patch"))

      (let ((reversed (if switched (not reverse) reverse)))
	(unless dry-run
	  ;; Apply the hunk
	  (with-current-buffer buf
	    (goto-char pos)
	    (delete-char (length (car old)))
	    (insert (car new))))

	(when popup
	  (with-current-buffer buf
	    ;; Show a message describing what was done
	    (let ((real-line (1+ (count-lines (point-min) pos)))
		  (msg
		   (if dry-run
		       (if reversed "already applied" "not yet applied")
		     (if reversed "undone" "applied"))))
	      (cond ((= real-line patch-line)
		     (message "Hunk %s" msg))
		    ((= real-line (1+ patch-line))
		     (message "Hunk %s at offset 1 line" msg))
		    (t
		     (message "Hunk %s at offset %d lines"
			      msg
			      (- real-line patch-line)))))

	    ;; fixup POS to reflect the hunk line offset
	    (goto-char pos)
	    (forward-line (cdr (if dry-run old new)))
	    (setq pos (point)))

	  ;; Display BUF in a window, and maybe select it
	  (let ((win (display-buffer buf)))
	    (set-window-point win pos)
	    (when (eq popup 'select)
	      (select-window win))))

	;; Return an appropriate indicator of success
	(if reversed 'reversed t)))))
      
      
(defun diff-test-hunk (&optional reverse)
  "See whether it's possible to apply the current hunk.
With a prefix argument, REVERSE the hunk."
  (interactive "P")
  (diff-apply-hunk reverse nil t t))

(defun diff-goto-source (&optional other-file)
  "Jump to the corresponding source line.
`diff-jump-to-old-file-flag' (or its opposite if the OTHER-FILE prefix arg
is give) determines whether to jump to the old or the new file.
If the prefix arg is bigger than 8 (for example with \\[universal-argument] \\[universal-argument])
  then `diff-jump-to-old-file-flag' is also set, for the next invocations."
  (interactive "P")
  (destructuring-bind (buf patch-line pos src &rest ignore)
      (diff-find-source-location other-file)
    (pop-to-buffer buf)
    (if (null pos)
	(progn
	  (goto-line patch-line)
	  (message "Hunk text not found"))
      (goto-char pos)
      (forward-line (cdr src)))))



;; provide the package
(provide 'diff-mode)

;;; Old Change Log from when diff-mode wasn't part of Emacs:
;; Revision 1.11  1999/10/09 23:38:29  monnier
;; (diff-mode-load-hook): dropped.
;; (auto-mode-alist): also catch *.diffs.
;; (diff-find-file-name, diff-mode):  add smarts to find the right file
;;     for *.rej files (that lack any file name indication).
;;
;; Revision 1.10  1999/09/30 15:32:11  monnier
;; added support for "\ No newline at end of file".
;;
;; Revision 1.9  1999/09/15 00:01:13  monnier
;; - added basic `compile' support.
;; - have diff-kill-hunk call diff-kill-file if it's the only hunk.
;; - diff-kill-file now tries to kill the leading garbage as well.
;;
;; Revision 1.8  1999/09/13 21:10:09  monnier
;; - don't use CL in the autoloaded code
;; - accept diffs using -T
;;
;; Revision 1.7  1999/09/05 20:53:03  monnier
;; interface to ediff-patch
;;
;; Revision 1.6  1999/09/01 20:55:13  monnier
;; (ediff=patch-file):  add bindings to call ediff-patch.
;; (diff-find-file-name):  taken out of diff-goto-source.
;; (diff-unified->context, diff-context->unified, diff-reverse-direction,
;;  diff-fixup-modifs):  only use the region if a prefix arg is given.
;;
;; Revision 1.5  1999/08/31 19:18:52  monnier
;; (diff-beginning-of-file, diff-prev-file):  fixed wrong parenthesis.
;;
;; Revision 1.4  1999/08/31 13:01:44  monnier
;; use `combine-after-change-calls' to minimize the slowdown of font-lock.
;;

;;; diff-mode.el ends here
