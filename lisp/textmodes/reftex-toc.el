;;; reftex-toc.el --- RefTeX's table of contents mode
;; Copyright (c) 1997, 1998, 1999, 2000 Free Software Foundation, Inc.

;; Author: Carsten Dominik <dominik@strw.LeidenUniv.nl>
;; Version: 4.16

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

;;; Code:

(eval-when-compile (require 'cl))
(provide 'reftex-toc)
(require 'reftex)
;;;

(defvar reftex-toc-map (make-sparse-keymap)
  "Keymap used for *toc* buffer.")

(defvar reftex-toc-menu)

(defun reftex-toc-mode ()
  "Major mode for managing Table of Contents for LaTeX files.
This buffer was created with RefTeX.
Press `?' for a summary of important key bindings.

Here are all local bindings.

\\{reftex-toc-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'reftex-toc-mode
	mode-name "TOC")
  (use-local-map reftex-toc-map)
  (set (make-local-variable 'revert-buffer-function) 'reftex-toc-revert)
  (set (make-local-variable 'reftex-toc-include-labels-indicator) "")
  (set (make-local-variable 'reftex-toc-max-level-indicator)
       (if (= reftex-toc-max-level 100)
	   "ALL"
	 (int-to-string reftex-toc-max-level)))
  (setq mode-line-format
	(list "----  " 'mode-line-buffer-identification
	      "  " 'global-mode-string "   (" mode-name ")"
	      "  L<" 'reftex-toc-include-labels-indicator ">"
	      "  I<" 'reftex-toc-include-index-indicator ">"
	      "  T<" 'reftex-toc-max-level-indicator ">"
	      " -%-"))
  (setq truncate-lines t)
  (make-local-variable 'reftex-last-follow-point)
  (add-hook 'post-command-hook 'reftex-toc-post-command-hook nil t)
  (add-hook 'pre-command-hook  'reftex-toc-pre-command-hook nil t)
  (easy-menu-add reftex-toc-menu reftex-toc-map)
  (run-hooks 'reftex-toc-mode-hook))

(defvar reftex-last-toc-file nil
  "Stores the file name from which `reftex-toc' was called.  For redo command.")

(defvar reftex-last-window-height nil)
(defvar reftex-toc-include-labels-indicator nil)
(defvar reftex-toc-include-index-indicator nil)
(defvar reftex-toc-max-level-indicator nil)

(defvar reftex-toc-return-marker (make-marker)
  "Marker which makes it possible to return from toc to old position.")

(defconst reftex-toc-help
"                      AVAILABLE KEYS IN TOC BUFFER
                      ============================
n / p      next-line / previous-line
SPC        Show the corresponding location of the LaTeX document.
TAB        Goto the location and keep the *toc* window.
RET        Goto the location and hide the *toc* window (also on mouse-2).
C-c >      Display Index. With prefix arg, restrict index to current section.
q / k      Hide/Kill *toc* buffer, return to position of reftex-toc command.
l i c F    Toggle display of  [l]abels,  [i]ndex,  [c]ontext,  [F]ile borders.
t          Change maximum toc depth (e.g. `3 t' hides levels greater than 3).
f / g      Toggle follow mode on and off  / Refresh *toc* buffer.
r / C-u r  Reparse the LaTeX document     / Reparse entire LaTeX document.
.          In other window, show position from where `reftex-toc' was called.
x          Switch to TOC of external document (with LaTeX package `xr').
z          Jump to a specific section (e.g. '3 z' goes to section 3")

(defun reftex-toc (&optional rebuild)
  "Show the table of contents for the current document.
When called with a raw C-u prefix, rescan the document first."

  (interactive)

  (if (or (not (string= reftex-last-toc-master (reftex-TeX-master-file)))
          current-prefix-arg)
      (reftex-erase-buffer "*toc*"))

  (setq reftex-last-toc-file   (buffer-file-name))
  (setq reftex-last-toc-master (reftex-TeX-master-file))

  (set-marker reftex-toc-return-marker (point))

  ;; If follow mode is active, arrange to delay it one command
  (if reftex-toc-follow-mode
      (setq reftex-toc-follow-mode 1))

  (and reftex-toc-include-index-entries
       (reftex-ensure-index-support))
  (or reftex-support-index
      (setq reftex-toc-include-index-entries nil))

  ;; Ensure access to scanning info and rescan buffer if prefix are is '(4)
  (reftex-access-scan-info current-prefix-arg)

  (let* ((this-buf (current-buffer))
	 (docstruct-symbol reftex-docstruct-symbol)
	 (xr-data (assq 'xr (symbol-value reftex-docstruct-symbol)))
	 (xr-alist (cons (cons "" (buffer-file-name)) (nth 1 xr-data)))
	 (here-I-am (if rebuild 
			(get 'reftex-toc :reftex-data)
		      (car (reftex-where-am-I))))
	 offset)

    (if (get-buffer-window "*toc*")
        (select-window (get-buffer-window "*toc*"))
      (when (or (not reftex-toc-keep-other-windows)
		(< (window-height) (* 2 window-min-height)))
	(delete-other-windows))
      (setq reftex-last-window-height (window-height))  ; remember
      (split-window)
      (let ((default-major-mode 'reftex-toc-mode))
	(switch-to-buffer "*toc*")))

    (or (eq major-mode 'reftex-toc-mode) (reftex-toc-mode))
    (set (make-local-variable 'reftex-docstruct-symbol) docstruct-symbol)
    (setq reftex-toc-include-labels-indicator
	  (if (eq reftex-toc-include-labels t)
	      "ALL"
	    reftex-toc-include-labels))
    (setq reftex-toc-include-index-indicator
	  (if (eq reftex-toc-include-index-entries t)
	      "ALL"
	    reftex-toc-include-index-entries))

    (cond
     ((= (buffer-size) 0)
      ;; buffer is empty - fill it with the table of contents
      (message "Building *toc* buffer...")

      (setq buffer-read-only nil)
      (insert (format
"TABLE-OF-CONTENTS on %s
SPC=view TAB=goto RET=goto+hide [q]uit [r]escan [l]abels [f]ollow [x]r [?]Help
------------------------------------------------------------------------------
" (abbreviate-file-name reftex-last-toc-master)))

      (if (reftex-use-fonts)
          (put-text-property 1 (point) 'face reftex-toc-header-face))
      (put-text-property 1 (point) 'intangible t)
      (put-text-property 1 2 'xr-alist xr-alist)

      (setq offset
	    (reftex-insert-docstruct
	     this-buf
	     t ; include toc
	     reftex-toc-include-labels
	     reftex-toc-include-index-entries
	     reftex-toc-include-file-boundaries
	     reftex-toc-include-context
	     nil ; counter
	     nil ; commented
	     here-I-am 
	     ""     ; xr-prefix
	     t      ; a toc buffer
	     ))
       
      (run-hooks 'reftex-display-copied-context-hook)
      (message "Building *toc* buffer...done.")
      (setq buffer-read-only t))
     (t
      ;; Only compute the offset
      (setq offset
	    (or (reftex-get-offset this-buf here-I-am
				   (if reftex-toc-include-labels " " nil)
				   t
				   reftex-toc-include-index-entries
				   reftex-toc-include-file-boundaries)
		(reftex-last-assoc-before-elt 
		 'toc here-I-am
		 (symbol-value reftex-docstruct-symbol))))
      (put 'reftex-toc :reftex-line 3)
      (goto-line 3)
      (beginning-of-line)))

    ;; Find the correct starting point
    (reftex-find-start-point (point) offset (get 'reftex-toc :reftex-line))
    (setq reftex-last-follow-point (point))))

(defun reftex-toc-pre-command-hook ()
  ;; used as pre command hook in *toc* buffer
  (reftex-unhighlight 0)
  (reftex-unhighlight 1))

(defun reftex-toc-post-command-hook ()
  ;; used in the post-command-hook for the *toc* buffer
  (when (get-text-property (point) :data)
    (put 'reftex-toc :reftex-data (get-text-property (point) :data))
    (and (> (point) 1)
	 (not (get-text-property (point) 'intangible))
	 (memq reftex-highlight-selection '(cursor both))
	 (reftex-highlight 1
	   (or (previous-single-property-change (1+ (point)) :data)
	       (point-min))
	   (or (next-single-property-change (point) :data)
	       (point-max)))))
  (if (integerp reftex-toc-follow-mode)
      ;; remove delayed action
      (setq reftex-toc-follow-mode t)
    (and reftex-toc-follow-mode
	 (not (equal reftex-last-follow-point (point)))
	 ;; show context in other window
	 (setq reftex-last-follow-point (point))
	 (condition-case nil
	     (reftex-toc-visit-location nil (not reftex-revisit-to-follow))
	   (error t)))))

(defun reftex-re-enlarge ()
  ;; Enlarge windiw to a remembered size
  (enlarge-window
   (max 0 (- (or reftex-last-window-height (window-height))
	     (window-height)))))

(defun reftex-toc-show-help ()
  "Show a summary of special key bindings."
  (interactive)
  (with-output-to-temp-buffer "*RefTeX Help*"
    (princ reftex-toc-help))
  (reftex-enlarge-to-fit "*RefTeX Help*" t)
  ;; If follow mode is active, arrange to delay it one command
  (if reftex-toc-follow-mode
      (setq reftex-toc-follow-mode 1)))

(defun reftex-toc-next (&optional arg)
  "Move to next selectable item."
  (interactive "p")
  (setq reftex-callback-fwd t)
  (or (eobp) (forward-char 1))
  (goto-char (or (next-single-property-change (point) :data) 
		 (point))))
(defun reftex-toc-previous (&optional arg)
  "Move to previous selectable item."
  (interactive "p")
  (setq reftex-callback-fwd nil)
  (goto-char (or (previous-single-property-change (point) :data)
		 (point))))
(defun reftex-toc-next-heading (&optional arg)
  "Move to next table of contentes line."
  (interactive "p")
  (end-of-line)
  (re-search-forward "^ " nil t arg)
  (beginning-of-line))
(defun reftex-toc-previous-heading (&optional arg)
  "Move to previous table of contentes line."
  (interactive "p")
  (re-search-backward "^ " nil t arg))
(defun reftex-toc-toggle-follow ()
  "Toggle follow (other window follows with context)."
  (interactive)
  (setq reftex-last-follow-point -1)
  (setq reftex-toc-follow-mode (not reftex-toc-follow-mode)))
(defun reftex-toc-toggle-file-boundary ()
  "Toggle inclusion of file boundaries in *toc* buffer."
  (interactive)
  (setq reftex-toc-include-file-boundaries
	(not reftex-toc-include-file-boundaries))
  (reftex-toc-revert))
(defun reftex-toc-toggle-labels (arg)
  "Toggle inclusion of labels in *toc* buffer.
With prefix ARG, prompt for a label type and include only labels of
that specific type."
  (interactive "P")
  (setq reftex-toc-include-labels 
	(if arg (reftex-query-label-type)
	  (not reftex-toc-include-labels)))
  (reftex-toc-revert))
(defun reftex-toc-toggle-index (arg)
  "Toggle inclusion of index in *toc* buffer.
With prefix arg, prompt for an index tag and include only entries of that
specific index."
  (interactive "P")
  (setq reftex-toc-include-index-entries
	(if arg (reftex-index-select-tag)
	  (not reftex-toc-include-index-entries)))
  (reftex-toc-revert))
(defun reftex-toc-toggle-context ()
  "Toggle inclusion of label context in *toc* buffer.
Label context is only displayed when the labels are there as well."
  (interactive)
  (setq reftex-toc-include-context (not reftex-toc-include-context))
  (reftex-toc-revert))
(defun reftex-toc-max-level (arg)
  "Set the maximum level of toc lines in this buffer to value of prefix ARG.
When no prefix is given, set the max level to a large number, so that all
levels are shown.  For eaxample, to set the level to 3, type `3 m'."
  (interactive "P")
  (setq reftex-toc-max-level (if arg
				 (prefix-numeric-value arg)
			       100))
  (setq reftex-toc-max-level-indicator
	(if arg (int-to-string reftex-toc-max-level) "ALL"))
  (reftex-toc-revert))
(defun reftex-toc-view-line ()
  "View document location in other window."
  (interactive)
  (reftex-toc-visit-location))
(defun reftex-toc-goto-line-and-hide ()
  "Go to document location in other window.  Hide the *toc* window."
  (interactive)
  (reftex-toc-visit-location 'hide))
(defun reftex-toc-goto-line ()
  "Go to document location in other window. *toc* window stays."
  (interactive)
  (reftex-toc-visit-location t))
(defun reftex-toc-mouse-goto-line-and-hide (ev)
  "Go to document location in other window.  Hide the *toc* window."
  (interactive "e")
  (mouse-set-point ev)
  (reftex-toc-visit-location 'hide))
(defun reftex-toc-show-calling-point ()
  "Show point where reftex-toc was called from."
  (interactive)
  (let ((this-window (selected-window)))
    (unwind-protect
	(progn
	  (switch-to-buffer-other-window
	   (marker-buffer reftex-toc-return-marker))
	  (goto-char (marker-position reftex-toc-return-marker))
	  (recenter '(4)))
      (select-window this-window))))
(defun reftex-toc-quit ()
  "Hide the *toc* window and do not move point."
  (interactive)
  (or (one-window-p) (delete-window))
  (switch-to-buffer (marker-buffer reftex-toc-return-marker))
  (reftex-re-enlarge)
  (goto-char (or (marker-position reftex-toc-return-marker) (point))))
(defun reftex-toc-quit-and-kill ()
  "Kill the *toc* buffer."
  (interactive)
  (kill-buffer "*toc*")
  (or (one-window-p) (delete-window))
  (switch-to-buffer (marker-buffer reftex-toc-return-marker))
  (reftex-re-enlarge)
  (goto-char (marker-position reftex-toc-return-marker)))
(defun reftex-toc-display-index (&optional arg)
  "Display the index buffer for the current document.
This works just like `reftex-display-index' from a LaTeX buffer.
With prefix arg 1, restrict index to the section at point."
  (interactive "P")
  (let ((data (get-text-property (point) :data))
	(docstruct (symbol-value reftex-docstruct-symbol))
	bor eor restr)
    (when (equal arg 2)
      (setq bor (reftex-last-assoc-before-elt 'toc data docstruct)
	    eor (assoc 'toc (cdr (memq bor docstruct)))
	    restr (list (nth 6 bor) bor eor)))
    (reftex-toc-goto-line)
    (reftex-display-index (if restr nil arg) restr)))
(defun reftex-toc-rescan (&rest ignore)
  "Regenerate the *toc* buffer by reparsing file of section at point."
  (interactive)
  (if (and reftex-enable-partial-scans 
	   (null current-prefix-arg))
      (let* ((data (get-text-property (point) :data))
	     (what (car data))
	     (file (cond ((eq what 'toc) (nth 3 data))
			 ((memq what '(eof bof file-error)) (nth 1 data))
			 ((stringp what) (nth 3 data))
			 ((eq what 'index) (nth 3 data))))
	     (line (+ (count-lines (point-min) (point)) (if (bolp) 1 0))))
        (if (not file)
            (error "Don't know which file to rescan.  Try `C-u r'")
	  (put 'reftex-toc :reftex-line line)
          (switch-to-buffer-other-window
           (reftex-get-file-buffer-force file))
	  (setq current-prefix-arg '(4))
          (reftex-toc t)))
    (reftex-toc-Rescan))
  (reftex-kill-temporary-buffers))
(defun reftex-toc-Rescan (&rest ignore)
  "Regenerate the *toc* buffer by reparsing the entire document."
  (interactive)
  (switch-to-buffer-other-window
   (reftex-get-file-buffer-force reftex-last-toc-file))
  (setq current-prefix-arg '(16))
  (reftex-toc t))
(defun reftex-toc-revert (&rest ignore)
  "Regenerate the *toc* from the internal lists."
  (interactive)
  (switch-to-buffer-other-window
   (reftex-get-file-buffer-force reftex-last-toc-file))
  (reftex-erase-buffer "*toc*")
  (setq current-prefix-arg nil)
  (reftex-toc t))
(defun reftex-toc-external (&rest ignore)
  "Switch to table of contents of an external document."
  (interactive)
  (let* ((old-buf (current-buffer))
	 (xr-alist (get-text-property 1 'xr-alist))
	 (xr-index (reftex-select-external-document
		   xr-alist 0)))
    (switch-to-buffer-other-window (or (reftex-get-file-buffer-force
					(cdr (nth xr-index xr-alist)))
				       (error "Cannot switch document")))
    (reftex-toc)
    (if (equal old-buf (current-buffer))
	(message "")
      (message "Switched document"))))

(defun reftex-toc-jump (arg)
  "Jump to a specific section.  E.g. '3 z' jumps to section 3.
Useful for large TOC's."
  (interactive "P")
  (goto-char (point-min))
  (re-search-forward
   (concat "^ *" (number-to-string (if (numberp arg) arg 1)) " ")
   nil t)
  (beginning-of-line))

(defun reftex-toc-visit-location (&optional final no-revisit)
  ;; Visit the tex file corresponding to the toc entry on the current line.
  ;; If FINAL is t, stay there
  ;; If FINAL is 'hide, hide the *toc* window.
  ;; Otherwise, move cursor back into *toc* window.
  ;; NO-REVISIT means don't visit files, just use live biffers.
  ;; This function is pretty clever about finding back a section heading,
  ;; even if the buffer is not live, or things like outline, x-symbol etc.
  ;; have been active.

  (let* ((toc (get-text-property (point) :data))
         (toc-window (selected-window))
         show-window show-buffer match)

    (unless toc (error "Don't know which toc line to visit"))
    
    (cond
  
     ((eq (car toc) 'toc)
      ;; a toc entry
      (setq match (reftex-toc-find-section toc no-revisit)))

     ((eq (car toc) 'index)
      ;; an index entry
      (setq match (reftex-index-show-entry toc no-revisit)))

     ((memq (car toc) '(bof eof))
      ;; A file entry
      (setq match
	    (let ((where (car toc))
		  (file (nth 1 toc)))
	      (if (or (not no-revisit) (reftex-get-buffer-visiting file))
		  (progn
		    (switch-to-buffer-other-window 
		     (reftex-get-file-buffer-force file nil))
		    (goto-char (if (eq where 'bof) (point-min) (point-max))))
		(message reftex-no-follow-message) nil))))

     ((stringp (car toc))
      ;; a label
      (setq match (reftex-show-label-location toc reftex-callback-fwd
						no-revisit t))))

    (setq show-window (selected-window)
          show-buffer (current-buffer))

    (unless match
      (select-window toc-window)
      (error "Cannot find location"))

    (select-window toc-window)

    ;; use the `final' parameter to decide what to do next
    (cond
     ((eq final t)
      (reftex-unhighlight 0)
      (select-window show-window))
     ((eq final 'hide)
      (reftex-unhighlight 0)
      (or (one-window-p) (delete-window))
      (switch-to-buffer show-buffer)
      (reftex-re-enlarge))
     (t nil))))

(defun reftex-toc-find-section (toc &optional no-revisit)
  (let* ((file (nth 3 toc))
	 (marker (nth 4 toc))
	 (level (nth 5 toc))
	 (literal (nth 7 toc))
	 (emergency-point (nth 8 toc))
	 (match
	  (cond
	   ((and (markerp marker) (marker-buffer marker))
	    ;; Buffer is still live and we have the marker.  Should be easy.
	    (switch-to-buffer-other-window (marker-buffer marker))
	    (goto-char (marker-position marker))
	    (or (looking-at (regexp-quote literal))
		(looking-at (reftex-make-regexp-allow-for-ctrl-m literal))
		(looking-at (reftex-make-desperate-section-regexp literal))
		(looking-at (concat "\\\\"
				    (regexp-quote
				     (car 
				      (rassq level 
					     reftex-section-levels-all)))
				    "[[{]?"))))
	   ((or (not no-revisit)
		(reftex-get-buffer-visiting file))
	    ;; Marker is lost.  Use the backup method.
	    (switch-to-buffer-other-window
	     (reftex-get-file-buffer-force file nil))
	    (goto-char (or emergency-point (point-min)))
	    (or (looking-at (regexp-quote literal))
		(let ((len (length literal)))
		  (or (reftex-nearest-match (regexp-quote literal) len)
		      (reftex-nearest-match
		       (reftex-make-regexp-allow-for-ctrl-m literal) len)
		      (reftex-nearest-match
		       (reftex-make-desperate-section-regexp literal) len)))))
	   (t (message reftex-no-follow-message) nil))))
    (when match
      (goto-char (match-beginning 0))
      (if (not (= (point) (point-max))) (recenter 1))
      (reftex-highlight 0 (match-beginning 0) (match-end 0) (current-buffer)))
    match))

(defun reftex-make-desperate-section-regexp (old)
  ;; Return a regexp which will still match a section statement even if
  ;; x-symbol or isotex or the like have been at work in the mean time.
  (let* ((n (1+ (string-match "[[{]" old)))
         (new (regexp-quote (substring old 0 (1+ (string-match "[[{]" old)))))
         (old (substring old n)))
    (while (string-match
            "\\([\r\n]\\)\\|\\(\\`\\|[ \t\n\r]\\)\\([a-zA-Z0-9]+\\)\\([ \t\n\r]\\|}\\'\\)"
            old)
      (if (match-beginning 1)
          (setq new (concat new "[^\n\r]*[\n\r]"))
        (setq new (concat new "[^\n\r]*" (match-string 3 old))))
      (setq old (substring old (match-end 0))))
    new))

;; Table of Contents map
(define-key reftex-toc-map (if (featurep 'xemacs) [(button2)] [(mouse-2)])
  'reftex-toc-mouse-goto-line-and-hide)

(substitute-key-definition
 'next-line 'reftex-toc-next reftex-toc-map global-map)
(substitute-key-definition
 'previous-line 'reftex-toc-previous reftex-toc-map global-map)

(loop for x in
      '(("n"    . reftex-toc-next)
	("p"    . reftex-toc-previous)
	("?"    . reftex-toc-show-help)
	(" "    . reftex-toc-view-line)
	("\C-m" . reftex-toc-goto-line-and-hide)
	("\C-i" . reftex-toc-goto-line)
	("\C-c>". reftex-toc-display-index)
	("r"    . reftex-toc-rescan)
	("R"    . reftex-toc-Rescan)
	("g"    . revert-buffer)
	("q"    . reftex-toc-quit)
	("k"    . reftex-toc-quit-and-kill)
	("f"    . reftex-toc-toggle-follow)
	("F"    . reftex-toc-toggle-file-boundary)
	("i"    . reftex-toc-toggle-index)
	("l"    . reftex-toc-toggle-labels)
	("t"    . reftex-toc-max-level)
	("c"    . reftex-toc-toggle-context)
	("%"    . reftex-toc-toggle-commented)
	("x"    . reftex-toc-external)
	("z"    . reftex-toc-jump)
	("."    . reftex-toc-show-calling-point)
	("\C-c\C-n" . reftex-toc-next-heading)
	("\C-c\C-p" . reftex-toc-previous-heading))
      do (define-key reftex-toc-map (car x) (cdr x)))

(loop for key across "0123456789" do
      (define-key reftex-toc-map (vector (list key)) 'digit-argument))
(define-key reftex-toc-map "-" 'negative-argument)

(easy-menu-define 
 reftex-toc-menu reftex-toc-map
 "Menu for Table of Contents buffer"
 '("TOC"
   ["Show Location" reftex-toc-view-line t]
   ["Go To Location" reftex-toc-goto-line t]
   ["Exit & Go To Location" reftex-toc-goto-line-and-hide t]
   ["Index" reftex-toc-display-index t]
   ["Quit" reftex-toc-quit t]
   "--"
   ["External Document TOC  " reftex-toc-external t]
   "--"
   ("Update"
    ["Rebuilt *toc* Buffer" revert-buffer t]
    ["Rescan One File" reftex-toc-rescan reftex-enable-partial-scans]
    ["Rescan Entire Document" reftex-toc-Rescan t])
   ("Options"
    "TOC Items"
    ["File Boundaries" reftex-toc-toggle-file-boundary :style toggle
     :selected reftex-toc-include-file-boundaries]
    ["Labels" reftex-toc-toggle-labels :style toggle
     :selected reftex-toc-include-labels]
    ["Index Entries" reftex-toc-toggle-index :style toggle
     :selected reftex-toc-include-index-entries]
    ["Context" reftex-toc-toggle-context :style toggle
     :selected reftex-toc-include-context]
    "--"
    ["Follow Mode" reftex-toc-toggle-follow :style toggle 
     :selected reftex-toc-follow-mode])
   "--"
   ["Help" reftex-toc-show-help t]))


;;; reftex-toc.el ends here
