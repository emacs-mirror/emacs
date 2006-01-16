;;; diff-mode.el --- a mode for viewing/editing context diffs

;; Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005 Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@cs.yale.edu>
;; Keywords: convenience patch diff

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

;; Provides support for font-lock, outline, navigation
;; commands, editing and various conversions as well as jumping
;; to the corresponding source file.

;; Inspired by Pavel Machek's patch-mode.el (<pavel@@atrey.karlin.mff.cuni.cz>)
;; Some efforts were spent to have it somewhat compatible with XEmacs'
;; diff-mode as well as with compilation-minor-mode

;; Bugs:

;; - Reverse doesn't work with normal diffs.

;; Todo:

;; - Add a `delete-after-apply' so C-c C-a automatically deletes hunks.
;;   Also allow C-c C-a to delete already-applied hunks.
;;
;; - Try `diff <file> <hunk>' to try and fuzzily discover the source location
;;   of a hunk.  Show then the changes between <file> and <hunk> and make it
;;   possible to apply them to <file>, <hunk-src>, or <hunk-dst>.
;;   Or maybe just make it into a ".rej to diff3-markers converter".
;;
;; - Refine hunk on a word-by-word basis.
;;
;; - Handle `diff -b' output in context->unified.

;;; Code:

(eval-when-compile (require 'cl))

(defvar add-log-buffer-file-name-function)


(defgroup diff-mode ()
  "Major mode for viewing/editing diffs."
  :version "21.1"
  :group 'tools
  :group 'diff)

(defcustom diff-default-read-only nil
  "If non-nil, `diff-mode' buffers default to being read-only."
  :type 'boolean
  :group 'diff-mode)

(defcustom diff-jump-to-old-file nil
  "*Non-nil means `diff-goto-source' jumps to the old file.
Else, it jumps to the new file."
  :type 'boolean
  :group 'diff-mode)

(defcustom diff-update-on-the-fly t
  "*Non-nil means hunk headers are kept up-to-date on-the-fly.
When editing a diff file, the line numbers in the hunk headers
need to be kept consistent with the actual diff.  This can
either be done on the fly (but this sometimes interacts poorly with the
undo mechanism) or whenever the file is written (can be slow
when editing big diffs)."
  :type 'boolean
  :group 'diff-mode)

(defcustom diff-advance-after-apply-hunk t
  "*Non-nil means `diff-apply-hunk' will move to the next hunk after applying."
  :type 'boolean
  :group 'diff-mode)


(defcustom diff-mode-hook nil
  "Run after setting up the `diff-mode' major mode."
  :type 'hook
  :options '(diff-delete-empty-files diff-make-unified)
  :group 'diff-mode)

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
    ([mouse-2] . diff-goto-source)
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
    ("C" . diff-unified->context)
    ("q" . quit-window))
  "Basic keymap for `diff-mode', bound to various prefix keys.")

(easy-mmode-defmap diff-mode-map
  `(("\e" . ,diff-mode-shared-map)
    ;; From compilation-minor-mode.
    ("\C-c\C-c" . diff-goto-source)
    ;; Misc operations.
    ("\C-c\C-r" . diff-refine-hunk)
    ("\C-c\C-s" . diff-split-hunk)
    ("\C-c\C-a" . diff-apply-hunk)
    ("\C-c\C-t" . diff-test-hunk)
    ("\C-c\C-f" . next-error-follow-minor-mode))
  "Keymap for `diff-mode'.  See also `diff-mode-shared-map'.")

(easy-menu-define diff-mode-menu diff-mode-map
  "Menu for `diff-mode'."
  '("Diff"
    ["Jump to Source"		diff-goto-source	t]
    ["Apply hunk"		diff-apply-hunk		t]
    ["Apply diff with Ediff"	diff-ediff-patch	t]
    ["-----" nil nil]
    ["Reverse direction"	diff-reverse-direction	t]
    ["Context -> Unified"	diff-context->unified	t]
    ["Unified -> Context"	diff-unified->context	t]
    ;;["Fixup Headers"		diff-fixup-modifs	(not buffer-read-only)]
    ))

(defcustom diff-minor-mode-prefix "\C-c="
  "Prefix key for `diff-minor-mode' commands."
  :type '(choice (string "\e") (string "C-c=") string)
  :group 'diff-mode)

(easy-mmode-defmap diff-minor-mode-map
  `((,diff-minor-mode-prefix . ,diff-mode-shared-map))
  "Keymap for `diff-minor-mode'.  See also `diff-mode-shared-map'.")


;;;;
;;;; font-lock support
;;;;

(defface diff-header
  '((((class color) (min-colors 88) (background light))
     :background "grey85")
    (((class color) (min-colors 88) (background dark))
     :background "grey45")
    (((class color) (background light))
     :foreground "blue1" :weight bold)
    (((class color) (background dark))
     :foreground "green" :weight bold)
    (t :weight bold))
  "`diff-mode' face inherited by hunk and index header faces."
  :group 'diff-mode)
;; backward-compatibility alias
(put 'diff-header-face 'face-alias 'diff-header)
(defvar diff-header-face 'diff-header)

(defface diff-file-header
  '((((class color) (min-colors 88) (background light))
     :background "grey70" :weight bold)
    (((class color) (min-colors 88) (background dark))
     :background "grey60" :weight bold)
    (((class color) (background light))
     :foreground "green" :weight bold)
    (((class color) (background dark))
     :foreground "cyan" :weight bold)
    (t :weight bold))			; :height 1.3
  "`diff-mode' face used to highlight file header lines."
  :group 'diff-mode)
;; backward-compatibility alias
(put 'diff-file-header-face 'face-alias 'diff-file-header)
(defvar diff-file-header-face 'diff-file-header)

(defface diff-index
  '((t :inherit diff-file-header))
  "`diff-mode' face used to highlight index header lines."
  :group 'diff-mode)
;; backward-compatibility alias
(put 'diff-index-face 'face-alias 'diff-index)
(defvar diff-index-face 'diff-index)

(defface diff-hunk-header
  '((t :inherit diff-header))
  "`diff-mode' face used to highlight hunk header lines."
  :group 'diff-mode)
;; backward-compatibility alias
(put 'diff-hunk-header-face 'face-alias 'diff-hunk-header)
(defvar diff-hunk-header-face 'diff-hunk-header)

(defface diff-removed
  '((t :inherit diff-changed))
  "`diff-mode' face used to highlight removed lines."
  :group 'diff-mode)
;; backward-compatibility alias
(put 'diff-removed-face 'face-alias 'diff-removed)
(defvar diff-removed-face 'diff-removed)

(defface diff-added
  '((t :inherit diff-changed))
  "`diff-mode' face used to highlight added lines."
  :group 'diff-mode)
;; backward-compatibility alias
(put 'diff-added-face 'face-alias 'diff-added)
(defvar diff-added-face 'diff-added)

(defface diff-changed
  '((((type tty pc) (class color) (background light))
     :foreground "magenta" :weight bold :slant italic)
    (((type tty pc) (class color) (background dark))
     :foreground "yellow" :weight bold :slant italic))
  "`diff-mode' face used to highlight changed lines."
  :group 'diff-mode)
;; backward-compatibility alias
(put 'diff-changed-face 'face-alias 'diff-changed)
(defvar diff-changed-face 'diff-changed)

(defface diff-indicator-removed
  '((t :inherit diff-removed))
  "`diff-mode' face used to highlight indicator of removed lines (-, <)."
  :group 'diff-mode
  :version "22.1")
(defvar diff-indicator-removed-face 'diff-indicator-removed)

(defface diff-indicator-added
  '((t :inherit diff-added))
  "`diff-mode' face used to highlight indicator of added lines (+, >)."
  :group 'diff-mode
  :version "22.1")
(defvar diff-indicator-added-face 'diff-indicator-added)

(defface diff-indicator-changed
  '((t :inherit diff-changed))
  "`diff-mode' face used to highlight indicator of changed lines."
  :group 'diff-mode
  :version "22.1")
(defvar diff-indicator-changed-face 'diff-indicator-changed)

(defface diff-function
  '((t :inherit diff-context))
  "`diff-mode' face used to highlight function names produced by \"diff -p\"."
  :group 'diff-mode)
;; backward-compatibility alias
(put 'diff-function-face 'face-alias 'diff-function)
(defvar diff-function-face 'diff-function)

(defface diff-context
  '((((class color grayscale) (min-colors 88)) :inherit shadow))
  "`diff-mode' face used to highlight context and other side-information."
  :group 'diff-mode)
;; backward-compatibility alias
(put 'diff-context-face 'face-alias 'diff-context)
(defvar diff-context-face 'diff-context)

(defface diff-nonexistent
  '((t :inherit diff-file-header))
  "`diff-mode' face used to highlight nonexistent files in recursive diffs."
  :group 'diff-mode)
;; backward-compatibility alias
(put 'diff-nonexistent-face 'face-alias 'diff-nonexistent)
(defvar diff-nonexistent-face 'diff-nonexistent)

(defconst diff-yank-handler '(diff-yank-function))
(defun diff-yank-function (text)
  ;; FIXME: the yank-handler is now called separately on each piece of text
  ;; with a yank-handler property, so the next-single-property-change call
  ;; below will always return nil :-(   --stef
  (let ((mixed (next-single-property-change 0 'yank-handler text))
	(start (point)))
    ;; First insert the text.
    (insert text)
    ;; If the text does not include any diff markers and if we're not
    ;; yanking back into a diff-mode buffer, get rid of the prefixes.
    (unless (or mixed (derived-mode-p 'diff-mode))
      (undo-boundary)		; Just in case the user wanted the prefixes.
      (let ((re (save-excursion
		  (if (re-search-backward "^[><!][ \t]" start t)
		      (if (eq (char-after) ?!)
			  "^[!+- ][ \t]" "^[<>][ \t]")
		    "^[ <>!+-]"))))
	(save-excursion
	  (while (re-search-backward re start t)
	    (replace-match "" t t)))))))


(defvar diff-font-lock-keywords
  `(("^\\(@@ -[0-9,]+ \\+[0-9,]+ @@\\)\\(.*\\)$"          ;unified
     (1 diff-hunk-header-face) (2 diff-function-face))
    ("^\\(\\*\\{15\\}\\)\\(.*\\)$"                        ;context
     (1 diff-hunk-header-face) (2 diff-function-face))
    ("^\\*\\*\\* .+ \\*\\*\\*\\*". diff-hunk-header-face) ;context
    ("^--- .+ ----$"             . diff-hunk-header-face) ;context
    ("^[0-9,]+[acd][0-9,]+$"     . diff-hunk-header-face) ;normal
    ("^---$"                     . diff-hunk-header-face) ;normal
    ("^\\(---\\|\\+\\+\\+\\|\\*\\*\\*\\) \\(\\S-+\\)\\(.*[^*-]\\)?\n"
     (0 diff-header-face) (2 diff-file-header-face prepend))
    ("^\\([-<]\\)\\(.*\n\\)"
     (1 diff-indicator-removed-face) (2 diff-removed-face))
    ("^\\([+>]\\)\\(.*\n\\)"
     (1 diff-indicator-added-face) (2 diff-added-face))
    ("^\\(!\\)\\(.*\n\\)"
     (1 diff-indicator-changed-face) (2 diff-changed-face))
    ("^Index: \\(.+\\).*\n"
     (0 diff-header-face) (1 diff-index-face prepend))
    ("^Only in .*\n" . diff-nonexistent-face)
    ("^\\(#\\)\\(.*\\)"
     (1 font-lock-comment-delimiter-face)
     (2 font-lock-comment-face))
    ("^[^-=+*!<>#].*\n" (0 diff-context-face))))

(defconst diff-font-lock-defaults
  '(diff-font-lock-keywords t nil nil nil (font-lock-multiline . nil)))

(defvar diff-imenu-generic-expression
  ;; Prefer second name as first is most likely to be a backup or
  ;; version-control name.  The [\t\n] at the end of the unidiff pattern
  ;; catches Debian source diff files (which lack the trailing date).
  '((nil "\\+\\+\\+\\ \\([^\t\n]+\\)[\t\n]" 1) ; unidiffs
    (nil "^--- \\([^\t\n]+\\)\t.*\n\\*" 1))) ; context diffs

;;;;
;;;; Movement
;;;;

(defconst diff-hunk-header-re "^\\(@@ -[0-9,]+ \\+[0-9,]+ @@.*\\|\\*\\{15\\}.*\n\\*\\*\\* .+ \\*\\*\\*\\*\\|[0-9]+\\(,[0-9]+\\)?[acd][0-9]+\\(,[0-9]+\\)?\\)$")
(defconst diff-file-header-re (concat "^\\(--- .+\n\\+\\+\\+ \\|\\*\\*\\* .+\n--- \\|[^-+!<>0-9@* ]\\).+\n" (substring diff-hunk-header-re 1)))
(defvar diff-narrowed-to nil)

(defun diff-end-of-hunk (&optional style)
  (when (looking-at diff-hunk-header-re)
    (unless style
      ;; Especially important for unified (because headers are ambiguous).
      (setq style (cdr (assq (char-after) '((?@ . unified) (?* . context))))))
    (goto-char (match-end 0)))
  (let ((end (and (re-search-forward (case style
				       ;; A `unified' header is ambiguous.
				       (unified (concat "^[^-+# \\]\\|"
							diff-file-header-re))
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
  (re-search-forward (concat "^[^-+#!<>0-9@* \\]\\|" diff-file-header-re)
		     nil 'move)
  (if (match-beginning 1)
      (goto-char (match-beginning 1))
    (beginning-of-line)))

;; Define diff-{hunk,file}-{prev,next}
(easy-mmode-define-navigation
 diff-hunk diff-hunk-header-re "hunk" diff-end-of-hunk diff-restrict-view)
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
  (let* ((start (point))
	 (nexthunk (when (re-search-forward diff-hunk-header-re nil t)
		     (match-beginning 0)))
	 (firsthunk (ignore-errors
		      (goto-char start)
		      (diff-beginning-of-file) (diff-hunk-next) (point)))
	 (nextfile (ignore-errors (diff-file-next) (point))))
    (goto-char start)
    (if (and firsthunk (= firsthunk start)
	     (or (null nexthunk)
		 (and nextfile (> nexthunk nextfile))))
	;; It's the only hunk for this file, so kill the file.
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
    (if (looking-at "^\n") (forward-char 1)) ;`tla' generates such diffs.
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

(defun diff-count-matches (re start end)
  (save-excursion
    (let ((n 0))
      (goto-char start)
      (while (re-search-forward re end t) (incf n))
      n)))

(defun diff-split-hunk ()
  "Split the current (unified diff) hunk at point into two hunks."
  (interactive)
  (beginning-of-line)
  (let ((pos (point))
	(start (progn (diff-beginning-of-hunk) (point))))
    (unless (looking-at "@@ -\\([0-9]+\\),[0-9]+ \\+\\([0-9]+\\),[0-9]+ @@")
      (error "diff-split-hunk only works on unified context diffs"))
    (forward-line 1)
    (let* ((start1 (string-to-number (match-string 1)))
	   (start2 (string-to-number (match-string 2)))
	   (newstart1 (+ start1 (diff-count-matches "^[- \t]" (point) pos)))
	   (newstart2 (+ start2 (diff-count-matches "^[+ \t]" (point) pos))))
      (goto-char pos)
      ;; Hopefully the after-change-function will not screw us over.
      (insert "@@ -" (number-to-string newstart1) ",1 +"
	      (number-to-string newstart2) ",1 @@\n")
      ;; Fix the original hunk-header.
      (diff-fixup-modifs start pos))))


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

(defun diff-tell-file-name (old name)
  "Tell Emacs where the find the source file of the current hunk.
If the OLD prefix arg is passed, tell the file NAME of the old file."
  (interactive
   (let* ((old current-prefix-arg)
	  (fs (diff-hunk-file-names current-prefix-arg)))
     (unless fs (error "No file name to look for"))
     (list old (read-file-name (format "File for %s: " (car fs))
			       nil (diff-find-file-name old) t))))
  (let ((fs (diff-hunk-file-names old)))
    (unless fs (error "No file name to look for"))
    (push (cons fs name) diff-remembered-files-alist)))

(defun diff-hunk-file-names (&optional old)
  "Give the list of file names textually mentioned for the current hunk."
  (save-excursion
    (unless (looking-at diff-file-header-re)
      (or (ignore-errors (diff-beginning-of-file))
	  (re-search-forward diff-file-header-re nil t)))
    (let ((limit (save-excursion
		   (condition-case ()
		       (progn (diff-hunk-prev) (point))
		     (error (point-min)))))
	  (header-files
	   (if (looking-at "[-*][-*][-*] \\(\\S-+\\)\\(\\s-.*\\)?\n[-+][-+][-+] \\(\\S-+\\)")
	       (list (if old (match-string 1) (match-string 3))
		     (if old (match-string 3) (match-string 1)))
	     (forward-line 1) nil)))
      (delq nil
	    (append
	     (when (and (not old)
			(save-excursion
			  (re-search-backward "^Index: \\(.+\\)" limit t)))
	       (list (match-string 1)))
	     header-files
	     (when (re-search-backward
		    "^diff \\(-\\S-+ +\\)*\\(\\S-+\\)\\( +\\(\\S-+\\)\\)?"
		    nil t)
	       (list (if old (match-string 2) (match-string 4))
		     (if old (match-string 4) (match-string 2)))))))))

(defun diff-find-file-name (&optional old)
  "Return the file corresponding to the current patch.
Non-nil OLD means that we want the old file."
  (save-excursion
    (unless (looking-at diff-file-header-re)
      (or (ignore-errors (diff-beginning-of-file))
	  (re-search-forward diff-file-header-re nil t)))
    (let ((fs (diff-hunk-file-names old)))
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
			  (?\s (insert " ") (setq modif nil) (backward-char 1))
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
			  (?\s (insert " ") (setq modif nil) (backward-char 1))
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
		    (?\s		;merge with the other half of the chunk
		     (let* ((endline2
			     (save-excursion
			       (goto-char pt2) (forward-line 1) (point)))
			    (c (char-after pt2)))
		       (case c
			 ((?! ?+)
			  (insert "+"
				  (prog1 (buffer-substring (+ pt2 2) endline2)
				    (delete-region pt2 endline2))))
			 (?\s		;FIXME: check consistency
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
			    (insert (delete-and-extract-region first last)))
			  (setq first nil last nil)
			  (equal ?\s c)))
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
	  (if (not (looking-at
		    (concat "@@ -[0-9,]+ \\+[0-9,]+ @@"
			    "\\|[-*][-*][-*] [0-9,]+ [-*][-*][-*][-*]$"
			    "\\|--- .+\n\\+\\+\\+ ")))
	      (case (char-after)
		(?\s (incf space))
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
		    (max end (cdr diff-unhandled-changes))))
      (setq diff-unhandled-changes (cons beg end)))))

(defun diff-post-command-hook ()
  "Fixup hunk headers if necessary."
  (when (consp diff-unhandled-changes)
    (ignore-errors
      (save-excursion
	(goto-char (car diff-unhandled-changes))
	;; Maybe we've cut the end of the hunk before point.
	(if (and (bolp) (not (bobp))) (backward-char 1))
	;; We used to fixup modifs on all the changes, but it turns out
	;; that it's safer not to do it on big changes, for example
	;; when yanking a big diff, since we might then screw up perfectly
	;; correct values.  -stef
	;; (unless (ignore-errors
	;; 	  (diff-beginning-of-hunk)
	;; 	  (save-excursion
	;; 	    (diff-end-of-hunk)
	;; 	    (> (point) (car diff-unhandled-changes))))
	;;   (goto-char (car diff-unhandled-changes))
	;; (re-search-forward diff-hunk-header-re (cdr diff-unhandled-changes))
	;;   (diff-beginning-of-hunk))
	;; (diff-fixup-modifs (point) (cdr diff-unhandled-changes))
	(diff-beginning-of-hunk)
	(when (save-excursion
		(diff-end-of-hunk)
		(>= (point) (cdr diff-unhandled-changes)))
	  (diff-fixup-modifs (point) (cdr diff-unhandled-changes)))))
    (setq diff-unhandled-changes nil)))

(defun diff-next-error (arg reset)
  ;; Select a window that displays the current buffer so that point
  ;; movements are reflected in that window.  Otherwise, the user might
  ;; never see the hunk corresponding to the source she's jumping to.
  (pop-to-buffer (current-buffer))
  (if reset (goto-char (point-min)))
  (diff-hunk-next arg)
  (diff-goto-source))

;;;###autoload
(define-derived-mode diff-mode fundamental-mode "Diff"
  "Major mode for viewing/editing context diffs.
Supports unified and context diffs as well as (to a lesser extent)
normal diffs.
When the buffer is read-only, the ESC prefix is not necessary.
If you edit the buffer manually, diff-mode will try to update the hunk
headers for you on-the-fly.

You can also switch between context diff and unified diff with \\[diff-context->unified],
or vice versa with \\[diff-unified->context] and you can also reverse the direction of
a diff with \\[diff-reverse-direction]."
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
  (set (make-local-variable 'next-error-function) 'diff-next-error)

  (when (and (> (point-max) (point-min)) diff-default-read-only)
    (toggle-read-only t))
  ;; setup change hooks
  (if (not diff-update-on-the-fly)
      (add-hook 'write-contents-functions 'diff-write-contents-hooks nil t)
    (make-local-variable 'diff-unhandled-changes)
    (add-hook 'after-change-functions 'diff-after-change-function nil t)
    (add-hook 'post-command-hook 'diff-post-command-hook nil t))
  ;; Neat trick from Dave Love to add more bindings in read-only mode:
  (let ((ro-bind (cons 'buffer-read-only diff-mode-shared-map)))
    (add-to-list 'minor-mode-overriding-map-alist ro-bind)
    ;; Turn off this little trick in case the buffer is put in view-mode.
    (add-hook 'view-mode-hook
	      `(lambda ()
		 (setq minor-mode-overriding-map-alist
		       (delq ',ro-bind minor-mode-overriding-map-alist)))
	      nil t))
  ;; add-log support
  (set (make-local-variable 'add-log-current-defun-function)
       'diff-current-defun)
  (set (make-local-variable 'add-log-buffer-file-name-function)
       'diff-find-file-name))

;;;###autoload
(define-minor-mode diff-minor-mode
  "Minor mode for viewing/editing context diffs.
\\{diff-minor-mode-map}"
  :group 'diff-mode :lighter " Diff"
  ;; FIXME: setup font-lock
  ;; setup change hooks
  (if (not diff-update-on-the-fly)
      (add-hook 'write-contents-functions 'diff-write-contents-hooks nil t)
    (make-local-variable 'diff-unhandled-changes)
    (add-hook 'after-change-functions 'diff-after-change-function nil t)
    (add-hook 'post-command-hook 'diff-post-command-hook nil t)))

;;; Handy hook functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun diff-delete-if-empty ()
  ;; An empty diff file means there's no more diffs to integrate, so we
  ;; can just remove the file altogether.  Very handy for .rej files if we
  ;; remove hunks as we apply them.
  (when (and buffer-file-name
	     (eq 0 (nth 7 (file-attributes buffer-file-name))))
    (delete-file buffer-file-name)))

(defun diff-delete-empty-files ()
  "Arrange for empty diff files to be removed."
  (add-hook 'after-save-hook 'diff-delete-if-empty nil t))

(defun diff-make-unified ()
  "Turn context diffs into unified diffs if applicable."
  (if (save-excursion
	(goto-char (point-min))
	(and (looking-at diff-hunk-header-re) (eq (char-after) ?*)))
      (let ((mod (buffer-modified-p)))
	(unwind-protect
	    (diff-context->unified (point-min) (point-max))
	  (restore-buffer-modified-p mod)))))

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

(defun diff-hunk-text (hunk destp char-offset)
  "Return the literal source text from HUNK as (TEXT . OFFSET).
if DESTP is nil TEXT is the source, otherwise the destination text.
CHAR-OFFSET is a char-offset in HUNK, and OFFSET is the corresponding
char-offset in TEXT."
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
	  (if char-offset '("" . 0) "")

	;; For context diffs, either side can be empty, (if there's only
	;; added or only removed text).  We should then use the other side.
	(cond ((equal src-pos divider-pos) (setq src-pos dst-pos))
	      ((equal dst-pos (point-max)) (setq dst-pos src-pos)))

	(when char-offset (goto-char (+ (point-min) char-offset)))

	;; Get rid of anything except the desired text.
	(save-excursion
	  ;; Delete unused text region
	  (let ((keep (if destp dst-pos src-pos)))
	    (when (and divider-pos (> divider-pos keep))
	      (delete-region divider-pos (point-max)))
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
	  (if char-offset (cons text (- (point) (point-min))) text))))))


(defun diff-find-text (text)
  "Return the buffer position (BEG . END) of the nearest occurrence of TEXT.
If TEXT isn't found, nil is returned."
  (let* ((orig (point))
	 (forw (and (search-forward text nil t)
		    (cons (match-beginning 0) (match-end 0))))
	 (back (and (goto-char (+ orig (length text)))
		    (search-backward text nil t)
		    (cons (match-beginning 0) (match-end 0)))))
    ;; Choose the closest match.
    (if (and forw back)
	(if (> (- (car forw) orig) (- orig (car back))) back forw)
      (or back forw))))

(defun diff-find-approx-text (text)
  "Return the buffer position (BEG . END) of the nearest occurrence of TEXT.
Whitespace differences are ignored."
  (let* ((orig (point))
	 (re (concat "^[ \t\n]*"
		     (mapconcat 'regexp-quote (split-string text) "[ \t\n]+")
		     "[ \t\n]*\n"))
	 (forw (and (re-search-forward re nil t)
		    (cons (match-beginning 0) (match-end 0))))
	 (back (and (goto-char (+ orig (length text)))
		    (re-search-backward re nil t)
		    (cons (match-beginning 0) (match-end 0)))))
    ;; Choose the closest match.
    (if (and forw back)
	(if (> (- (car forw) orig) (- orig (car back))) back forw)
      (or back forw))))

(defsubst diff-xor (a b) (if a (not b) b))

(defun diff-find-source-location (&optional other-file reverse)
  "Find out (BUF LINE-OFFSET POS SRC DST SWITCHED).
BUF is the buffer corresponding to the source file.
LINE-OFFSET is the offset between the expected and actual positions
  of the text of the hunk or nil if the text was not found.
POS is a pair (BEG . END) indicating the position of the text in the buffer.
SRC and DST are the two variants of text as returned by `diff-hunk-text'.
  SRC is the variant that was found in the buffer.
SWITCHED is non-nil if the patch is already applied."
  (save-excursion
    (let* ((other (diff-xor other-file diff-jump-to-old-file))
	   (char-offset (- (point) (progn (diff-beginning-of-hunk) (point))))
	   (hunk (buffer-substring (point)
				   (save-excursion (diff-end-of-hunk) (point))))
	   (old (diff-hunk-text hunk reverse char-offset))
	   (new (diff-hunk-text hunk (not reverse) char-offset))
	   ;; Find the location specification.
	   (line (if (not (looking-at "\\(?:\\*\\{15\\}.*\n\\)?[-@* ]*\\([0-9,]+\\)\\([ acd+]+\\([0-9,]+\\)\\)?"))
		     (error "Can't find the hunk header")
		   (if other (match-string 1)
		     (if (match-end 3) (match-string 3)
		       (unless (re-search-forward "^--- \\([0-9,]+\\)" nil t)
			 (error "Can't find the hunk separator"))
		       (match-string 1)))))
	   (file (or (diff-find-file-name other) (error "Can't find the file")))
	   (buf (find-file-noselect file)))
      ;; Update the user preference if he so wished.
      (when (> (prefix-numeric-value other-file) 8)
	(setq diff-jump-to-old-file other))
      (with-current-buffer buf
	(goto-line (string-to-number line))
	(let* ((orig-pos (point))
	       (switched nil)
	       ;; FIXME: Check for case where both OLD and NEW are found.
	       (pos (or (diff-find-text (car old))
			(progn (setq switched t) (diff-find-text (car new)))
			(progn (setq switched nil)
			       (condition-case nil
				   (diff-find-approx-text (car old))
				 (invalid-regexp nil)))	;Regex too big.
			(progn (setq switched t)
			       (condition-case nil
				   (diff-find-approx-text (car new))
				 (invalid-regexp nil)))	;Regex too big.
			(progn (setq switched nil) nil))))
	  (nconc
	   (list buf)
	   (if pos
	       (list (count-lines orig-pos (car pos)) pos)
	     (list nil (cons orig-pos (+ orig-pos (length (car old))))))
	   (if switched (list new old t) (list old new))))))))


(defun diff-hunk-status-msg (line-offset reversed dry-run)
  (let ((msg (if dry-run
		 (if reversed "already applied" "not yet applied")
	       (if reversed "undone" "applied"))))
    (message (cond ((null line-offset) "Hunk text not found")
		   ((= line-offset 0) "Hunk %s")
		   ((= line-offset 1) "Hunk %s at offset %d line")
		   (t "Hunk %s at offset %d lines"))
	     msg line-offset)))


(defun diff-apply-hunk (&optional reverse)
  "Apply the current hunk to the source file and go to the next.
By default, the new source file is patched, but if the variable
`diff-jump-to-old-file' is non-nil, then the old source file is
patched instead (some commands, such as `diff-goto-source' can change
the value of this variable when given an appropriate prefix argument).

With a prefix argument, REVERSE the hunk."
  (interactive "P")
  (destructuring-bind (buf line-offset pos old new &optional switched)
      ;; If REVERSE go to the new file, otherwise go to the old.
      (diff-find-source-location (not reverse) reverse)
    (cond
     ((null line-offset)
      (error "Can't find the text to patch"))
     ((and switched
	   ;; A reversed patch was detected, perhaps apply it in reverse.
	   (not (save-window-excursion
		  (pop-to-buffer buf)
		  (goto-char (+ (car pos) (cdr old)))
		  (y-or-n-p
		   (if reverse
		       "Hunk hasn't been applied yet; apply it now? "
		     "Hunk has already been applied; undo it? ")))))
      (message "(Nothing done)"))
     (t
      ;; Apply the hunk
      (with-current-buffer buf
	(goto-char (car pos))
	(delete-region (car pos) (cdr pos))
	(insert (car new)))
      ;; Display BUF in a window
      (set-window-point (display-buffer buf) (+ (car pos) (cdr new)))
      (diff-hunk-status-msg line-offset (diff-xor switched reverse) nil)
      (when diff-advance-after-apply-hunk
	(diff-hunk-next))))))


(defun diff-test-hunk (&optional reverse)
  "See whether it's possible to apply the current hunk.
With a prefix argument, try to REVERSE the hunk."
  (interactive "P")
  (destructuring-bind (buf line-offset pos src dst &optional switched)
      ;; If REVERSE go to the new file, otherwise go to the old.
      (diff-find-source-location (not reverse) reverse)
    (set-window-point (display-buffer buf) (+ (car pos) (cdr src)))
    (diff-hunk-status-msg line-offset (diff-xor reverse switched) t)))


(defalias 'diff-mouse-goto-source 'diff-goto-source)

(defun diff-goto-source (&optional other-file event)
  "Jump to the corresponding source line.
`diff-jump-to-old-file' (or its opposite if the OTHER-FILE prefix arg
is given) determines whether to jump to the old or the new file.
If the prefix arg is bigger than 8 (for example with \\[universal-argument] \\[universal-argument])
  then `diff-jump-to-old-file' is also set, for the next invocations."
  (interactive (list current-prefix-arg last-input-event))
  ;; When pointing at a removal line, we probably want to jump to
  ;; the old location, and else to the new (i.e. as if reverting).
  ;; This is a convenient detail when using smerge-diff.
  (if event (posn-set-point (event-end event)))
  (let ((rev (not (save-excursion (beginning-of-line) (looking-at "[-<]")))))
    (destructuring-bind (buf line-offset pos src dst &optional switched)
	(diff-find-source-location other-file rev)
      (pop-to-buffer buf)
      (goto-char (+ (car pos) (cdr src)))
      (diff-hunk-status-msg line-offset (diff-xor rev switched) t))))


(defun diff-current-defun ()
  "Find the name of function at point.
For use in `add-log-current-defun-function'."
  (save-excursion
    (when (looking-at diff-hunk-header-re)
      (forward-line 1)
      (re-search-forward "^[^ ]" nil t))
    (destructuring-bind (buf line-offset pos src dst &optional switched)
	(diff-find-source-location)
      (beginning-of-line)
      (or (when (memq (char-after) '(?< ?-))
	    ;; Cursor is pointing at removed text.  This could be a removed
	    ;; function, in which case, going to the source buffer will
	    ;; not help since the function is now removed.  Instead,
	    ;; try to figure out the function name just from the code-fragment.
	    (let ((old (if switched dst src)))
	      (with-temp-buffer
		(insert (car old))
		(funcall (with-current-buffer buf major-mode))
		(goto-char (+ (point-min) (cdr old)))
		(add-log-current-defun))))
	  (with-current-buffer buf
	    (goto-char (+ (car pos) (cdr src)))
	    (add-log-current-defun))))))

(defun diff-refine-hunk ()
  "Refine the current hunk by ignoring space differences."
  (interactive)
  (let* ((char-offset (- (point) (progn (diff-beginning-of-hunk) (point))))
	 (opts (case (char-after) (?@ "-bu") (?* "-bc") (t "-b")))
	 (line-nb (and (or (looking-at "[^0-9]+\\([0-9]+\\)")
			   (error "Can't find line number"))
		       (string-to-number (match-string 1))))
	 (hunk (delete-and-extract-region
		(point) (save-excursion (diff-end-of-hunk) (point))))
	 (lead (make-string (1- line-nb) ?\n)) ;Line nums start at 1.
	 (file1 (make-temp-file "diff1"))
	 (file2 (make-temp-file "diff2"))
	 (coding-system-for-read buffer-file-coding-system)
	 old new)
    (unwind-protect
	(save-excursion
	  (setq old (diff-hunk-text hunk nil char-offset))
	  (setq new (diff-hunk-text hunk t char-offset))
	  (write-region (concat lead (car old)) nil file1 nil 'nomessage)
	  (write-region (concat lead (car new)) nil file2 nil 'nomessage)
	  (with-temp-buffer
	    (let ((status
		   (call-process diff-command nil t nil
				 opts file1 file2)))
	      (case status
		(0 nil)			;Nothing to reformat.
		(1 (goto-char (point-min))
		   ;; Remove the file-header.
		   (when (re-search-forward diff-hunk-header-re nil t)
		     (delete-region (point-min) (match-beginning 0))))
		(t (goto-char (point-max))
		   (unless (bolp) (insert "\n"))
		   (insert hunk)))
	      (setq hunk (buffer-string))
	      (unless (memq status '(0 1))
		(error "Diff returned: %s" status)))))
      ;; Whatever happens, put back some equivalent text: either the new
      ;; one or the original one in case some error happened.
      (insert hunk)
      (delete-file file1)
      (delete-file file2))))

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

;; arch-tag: 2571d7ff-bc28-4cf9-8585-42e21890be66
;;; diff-mode.el ends here
