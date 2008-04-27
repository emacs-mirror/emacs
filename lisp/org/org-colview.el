;;; org-colview.el --- Column View in Org-mode

;; Copyright (C) 2004, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;; Version: 6.02b
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file contains the face definitons for Org.

;;; Code:

(eval-when-compile (require 'cl))
(require 'org)

;;; Column View

(defvar org-columns-overlays nil
  "Holds the list of current column overlays.")

(defvar org-columns-current-fmt nil
  "Local variable, holds the currently active column format.")
(make-variable-buffer-local 'org-columns-current-fmt)
(defvar org-columns-current-fmt-compiled nil
  "Local variable, holds the currently active column format.
This is the compiled version of the format.")
(make-variable-buffer-local 'org-columns-current-fmt-compiled)
(defvar org-columns-current-widths nil
  "Loval variable, holds the currently widths of fields.")
(make-variable-buffer-local 'org-columns-current-widths)
(defvar org-columns-current-maxwidths nil
  "Loval variable, holds the currently active maximum column widths.")
(make-variable-buffer-local 'org-columns-current-maxwidths)
(defvar org-columns-begin-marker (make-marker)
  "Points to the position where last a column creation command was called.")
(defvar org-columns-top-level-marker (make-marker)
  "Points to the position where current columns region starts.")

(defvar org-columns-map (make-sparse-keymap)
  "The keymap valid in column display.")

(defun org-columns-content ()
  "Switch to contents view while in columns view."
  (interactive)
  (org-overview)
  (org-content))

(org-defkey org-columns-map "c" 'org-columns-content)
(org-defkey org-columns-map "o" 'org-overview)
(org-defkey org-columns-map "e" 'org-columns-edit-value)
(org-defkey org-columns-map "\C-c\C-t" 'org-columns-todo)
(org-defkey org-columns-map "\C-c\C-c" 'org-columns-set-tags-or-toggle)
(org-defkey org-columns-map "\C-c\C-o" 'org-columns-open-link)
(org-defkey org-columns-map "v" 'org-columns-show-value)
(org-defkey org-columns-map "q" 'org-columns-quit)
(org-defkey org-columns-map "r" 'org-columns-redo)
(org-defkey org-columns-map "g" 'org-columns-redo)
(org-defkey org-columns-map [left] 'backward-char)
(org-defkey org-columns-map "\M-b" 'backward-char)
(org-defkey org-columns-map "a" 'org-columns-edit-allowed)
(org-defkey org-columns-map "s" 'org-columns-edit-attributes)
(org-defkey org-columns-map "\M-f" (lambda () (interactive) (goto-char (1+ (point)))))
(org-defkey org-columns-map [right] (lambda () (interactive) (goto-char (1+ (point)))))
(org-defkey org-columns-map [(shift right)] 'org-columns-next-allowed-value)
(org-defkey org-columns-map "n" 'org-columns-next-allowed-value)
(org-defkey org-columns-map [(shift left)] 'org-columns-previous-allowed-value)
(org-defkey org-columns-map "p" 'org-columns-previous-allowed-value)
(org-defkey org-columns-map "<" 'org-columns-narrow)
(org-defkey org-columns-map ">" 'org-columns-widen)
(org-defkey org-columns-map [(meta right)] 'org-columns-move-right)
(org-defkey org-columns-map [(meta left)] 'org-columns-move-left)
(org-defkey org-columns-map [(shift meta right)] 'org-columns-new)
(org-defkey org-columns-map [(shift meta left)] 'org-columns-delete)

(easy-menu-define org-columns-menu org-columns-map "Org Column Menu"
  '("Column"
    ["Edit property" org-columns-edit-value t]
    ["Next allowed value" org-columns-next-allowed-value t]
    ["Previous allowed value" org-columns-previous-allowed-value t]
    ["Show full value" org-columns-show-value t]
    ["Edit allowed values" org-columns-edit-allowed t]
    "--"
    ["Edit column attributes" org-columns-edit-attributes t]
    ["Increase column width" org-columns-widen t]
    ["Decrease column width" org-columns-narrow t]
    "--"
    ["Move column right" org-columns-move-right t]
    ["Move column left" org-columns-move-left t]
    ["Add column" org-columns-new t]
    ["Delete column" org-columns-delete t]
    "--"
    ["CONTENTS" org-columns-content t]
    ["OVERVIEW" org-overview t]
    ["Refresh columns display" org-columns-redo t]
    "--"
    ["Open link" org-columns-open-link t]
    "--"
    ["Quit" org-columns-quit t]))

(defun org-columns-new-overlay (beg end &optional string face)
  "Create a new column overlay and add it to the list."
  (let ((ov (org-make-overlay beg end)))
    (org-overlay-put ov 'face (or face 'secondary-selection))
    (org-overlay-display ov string face)
    (push ov org-columns-overlays)
    ov))

(defun org-columns-display-here (&optional props)
  "Overlay the current line with column display."
  (interactive)
  (let* ((fmt org-columns-current-fmt-compiled)
	 (beg (point-at-bol))
	 (level-face (save-excursion
		       (beginning-of-line 1)
		       (and (looking-at "\\(\\**\\)\\(\\* \\)")
			    (org-get-level-face 2))))
	 (ref-face (or level-face
		       (and (eq major-mode 'org-agenda-mode)
			    (get-text-property (point-at-bol) 'face))
		       'default))
	 (color (list :foreground
		      (face-attribute ref-face :foreground)
		      :weight 'normal :strike-through nil
		      :underline nil))
	 (face (list color 'org-column level-face))
	 pom property ass width f string ov column val modval)
    ;; Check if the entry is in another buffer.
    (unless props
      (if (eq major-mode 'org-agenda-mode)
	  (setq pom (or (get-text-property (point) 'org-hd-marker)
			(get-text-property (point) 'org-marker))
		props (if pom (org-entry-properties pom) nil))
	(setq props (org-entry-properties nil))))
    ;; Walk the format
    (while (setq column (pop fmt))
      (setq property (car column)
	    ass (if (equal property "ITEM")
		    (cons "ITEM"
			  (save-match-data
			    (org-no-properties
			     (org-remove-tabs
			      (buffer-substring-no-properties
			       (point-at-bol) (point-at-eol))))))
		  (assoc property props))
	    width (or (cdr (assoc property org-columns-current-maxwidths))
		      (nth 2 column)
		      (length property))
	    f (format "%%-%d.%ds | " width width)
	    val (or (cdr ass) "")
	    modval (if (equal property "ITEM")
		       (org-columns-cleanup-item val org-columns-current-fmt-compiled))
	    string (format f (or modval val)))
      ;; Create the overlay
      (org-unmodified
       (setq ov (org-columns-new-overlay
		 beg (setq beg (1+ beg)) string face))
       (org-overlay-put ov 'keymap org-columns-map)
       (org-overlay-put ov 'org-columns-key property)
       (org-overlay-put ov 'org-columns-value (cdr ass))
       (org-overlay-put ov 'org-columns-value-modified modval)
       (org-overlay-put ov 'org-columns-pom pom)
       (org-overlay-put ov 'org-columns-format f))
      (if (or (not (char-after beg))
	      (equal (char-after beg) ?\n))
	  (let ((inhibit-read-only t))
	    (save-excursion
	      (goto-char beg)
	      (org-unmodified (insert " ")))))) ;; FIXME: add props and remove later?
    ;; Make the rest of the line disappear.
    (org-unmodified
     (setq ov (org-columns-new-overlay beg (point-at-eol)))
     (org-overlay-put ov 'invisible t)
     (org-overlay-put ov 'keymap org-columns-map)
     (org-overlay-put ov 'intangible t)
     (push ov org-columns-overlays)
     (setq ov (org-make-overlay (1- (point-at-eol)) (1+ (point-at-eol))))
     (org-overlay-put ov 'keymap org-columns-map)
     (push ov org-columns-overlays)
     (let ((inhibit-read-only t))
       (put-text-property (max (point-min) (1- (point-at-bol)))
			  (min (point-max) (1+ (point-at-eol)))
			  'read-only "Type `e' to edit property")))))

(defvar org-columns-full-header-line-format nil
  "Fthe full header line format, will be shifted by horizontal scrolling." )
(defvar org-previous-header-line-format nil
  "The header line format before column view was turned on.")
(defvar org-columns-inhibit-recalculation nil
  "Inhibit recomputing of columns on column view startup.")
(defvar org-columns-flyspell-was-active nil
  "Remember the state of `flyspell-mode' before column view.
Flyspell-mode can cause problems in columns view, so it is turned off
for the duration of the command.")

(defvar header-line-format)
(defvar org-columns-previous-hscroll 0)
(defun org-columns-display-here-title ()
  "Overlay the newline before the current line with the table title."
  (interactive)
  (let ((fmt org-columns-current-fmt-compiled)
	string (title "")
	property width f column str widths)
    (while (setq column (pop fmt))
      (setq property (car column)
	    str (or (nth 1 column) property)
	    width (or (cdr (assoc property org-columns-current-maxwidths))
		      (nth 2 column)
		      (length str))
	    widths (push width widths)
	    f (format "%%-%d.%ds | " width width)
	    string (format f str)
	    title (concat title string)))
    (setq title (concat
		 (org-add-props " " nil 'display '(space :align-to 0))
		 ;;(org-add-props title nil 'face '(:weight bold :underline t :inherit default))))
		 (org-add-props title nil 'face 'org-column-title)))
    (org-set-local 'org-previous-header-line-format header-line-format)
    (org-set-local 'org-columns-current-widths (nreverse widths))
    (setq org-columns-full-header-line-format title)
    (setq org-columns-previous-hscroll -1)
;    (org-columns-hscoll-title)
    (org-add-hook 'post-command-hook 'org-columns-hscoll-title nil 'local)))

(defun org-columns-hscoll-title ()
  "Set the header-line-format so that it scrolls along with the table."
  (sit-for .0001) ; need to force a redisplay to update window-hscroll
  (when (not (= (window-hscroll) org-columns-previous-hscroll))
    (setq header-line-format
	  (concat (substring org-columns-full-header-line-format 0 1)
		  (substring org-columns-full-header-line-format
			     (1+ (window-hscroll))))
	  org-columns-previous-hscroll (window-hscroll))
    (force-mode-line-update)))

(defun org-columns-remove-overlays ()
  "Remove all currently active column overlays."
  (interactive)
  (when (marker-buffer org-columns-begin-marker)
    (with-current-buffer (marker-buffer org-columns-begin-marker)
      (when (local-variable-p 'org-previous-header-line-format)
	(setq header-line-format org-previous-header-line-format)
	(kill-local-variable 'org-previous-header-line-format)
	(remove-hook 'post-command-hook 'org-columns-hscoll-title 'local))
      (move-marker org-columns-begin-marker nil)
      (move-marker org-columns-top-level-marker nil)
      (org-unmodified
       (mapc 'org-delete-overlay org-columns-overlays)
       (setq org-columns-overlays nil)
       (let ((inhibit-read-only t))
	 (remove-text-properties (point-min) (point-max) '(read-only t))))
      (when org-columns-flyspell-was-active
	(flyspell-mode 1)))))

(defun org-columns-cleanup-item (item fmt)
  "Remove from ITEM what is a column in the format FMT."
  (if (not org-complex-heading-regexp)
      item
    (when (string-match org-complex-heading-regexp item)
      (concat
       (org-add-props (concat (match-string 1 item) " ") nil
	 'org-whitespace (* 2 (1- (org-reduced-level (- (match-end 1) (match-beginning 1))))))
       (and (match-end 2) (not (assoc "TODO" fmt)) (concat " " (match-string 2 item)))
       (and (match-end 3) (not (assoc "PRIORITY" fmt)) (concat " " (match-string 3 item)))
       " " (match-string 4 item)
       (and (match-end 5) (not (assoc "TAGS" fmt)) (concat " " (match-string 5 item)))))))

(defun org-columns-show-value ()
  "Show the full value of the property."
  (interactive)
  (let ((value (get-char-property (point) 'org-columns-value)))
    (message "Value is: %s" (or value ""))))

(defvar org-agenda-columns-active) ;; defined in org-agenda.el
(defun org-columns-quit ()
  "Remove the column overlays and in this way exit column editing."
  (interactive)
  (org-unmodified
   (org-columns-remove-overlays)
   (let ((inhibit-read-only t))
     (remove-text-properties (point-min) (point-max) '(read-only t))))
  (when (eq major-mode 'org-agenda-mode)
    (setq org-agenda-columns-active nil)
    (message
     "Modification not yet reflected in Agenda buffer, use `r' to refresh")))

(defun org-columns-check-computed ()
  "Check if this column value is computed.
If yes, throw an error indicating that changing it does not make sense."
  (let ((val (get-char-property (point) 'org-columns-value)))
    (when (and (stringp val)
	       (get-char-property 0 'org-computed val))
      (error "This value is computed from the entry's children"))))

(defun org-columns-todo (&optional arg)
  "Change the TODO state during column view."
  (interactive "P")
  (org-columns-edit-value "TODO"))

(defun org-columns-set-tags-or-toggle (&optional arg)
  "Toggle checkbox at point, or set tags for current headline."
  (interactive "P")
  (if (string-match "\\`\\[[ xX-]\\]\\'"
		    (get-char-property (point) 'org-columns-value))
      (org-columns-next-allowed-value)
    (org-columns-edit-value "TAGS")))

(defun org-columns-edit-value (&optional key)
  "Edit the value of the property at point in column view.
Where possible, use the standard interface for changing this line."
  (interactive)
  (org-columns-check-computed)
  (let* ((external-key key)
	 (col (current-column))
	 (key (or key (get-char-property (point) 'org-columns-key)))
	 (value (get-char-property (point) 'org-columns-value))
	 (bol (point-at-bol)) (eol (point-at-eol))
	 (pom (or (get-text-property bol 'org-hd-marker)
		  (point))) ; keep despite of compiler waring
	 (line-overlays
	  (delq nil (mapcar (lambda (x)
			      (and (eq (overlay-buffer x) (current-buffer))
				   (>= (overlay-start x) bol)
				   (<= (overlay-start x) eol)
				   x))
			    org-columns-overlays)))
	 nval eval allowed)
    (cond
     ((equal key "CLOCKSUM")
      (error "This special column cannot be edited"))
     ((equal key "ITEM")
      (setq eval '(org-with-point-at pom
		    (org-edit-headline))))
     ((equal key "TODO")
      (setq eval '(org-with-point-at pom
		    (let ((current-prefix-arg
			   (if external-key current-prefix-arg '(4))))
		      (call-interactively 'org-todo)))))
     ((equal key "PRIORITY")
      (setq eval '(org-with-point-at pom
		    (call-interactively 'org-priority))))
     ((equal key "TAGS")
      (setq eval '(org-with-point-at pom
		    (let ((org-fast-tag-selection-single-key
			   (if (eq org-fast-tag-selection-single-key 'expert)
			       t org-fast-tag-selection-single-key)))
		      (call-interactively 'org-set-tags)))))
     ((equal key "DEADLINE")
      (setq eval '(org-with-point-at pom
		    (call-interactively 'org-deadline))))
     ((equal key "SCHEDULED")
      (setq eval '(org-with-point-at pom
		    (call-interactively 'org-schedule))))
     (t
      (setq allowed (org-property-get-allowed-values pom key 'table))
      (if allowed
	  (setq nval (completing-read "Value: " allowed nil t))
	(setq nval (read-string "Edit: " value)))
      (setq nval (org-trim nval))
      (when (not (equal nval value))
	(setq eval '(org-entry-put pom key nval)))))
    (when eval

      (cond
       ((equal major-mode 'org-agenda-mode)
	(org-columns-eval '(org-entry-put pom key nval))
	;; The following let preserves the current format, and makes sure
	;; that in only a single file things need to be upated.
	(let* ((org-agenda-overriding-columns-format org-columns-current-fmt)
	       (buffer (marker-buffer pom))
	       (org-agenda-contributing-files
		(list (with-current-buffer buffer
			(buffer-file-name (buffer-base-buffer))))))
	  (org-agenda-columns)))
       (t
	(let ((inhibit-read-only t))
	  (org-unmodified
	   (remove-text-properties
	    (max (point-min) (1- bol)) eol '(read-only t)))
	  (unwind-protect
	      (progn
		(setq org-columns-overlays
		      (org-delete-all line-overlays org-columns-overlays))
		(mapc 'org-delete-overlay line-overlays)
		(org-columns-eval eval))
	    (org-columns-display-here)))
	(org-move-to-column col)
	(if (and (org-mode-p)
		 (nth 3 (assoc key org-columns-current-fmt-compiled)))
	    (org-columns-update key)))))))

(defun org-edit-headline () ; FIXME: this is not columns specific.  Make interactive?????  Use from agenda????
  "Edit the current headline, the part without TODO keyword, TAGS."
  (org-back-to-heading)
  (when (looking-at org-todo-line-regexp)
    (let ((pre (buffer-substring (match-beginning 0) (match-beginning 3)))
	  (txt (match-string 3))
	  (post "")
	  txt2)
      (if (string-match (org-re "[ \t]+:[[:alnum:]:_@]+:[ \t]*$") txt)
	  (setq post (match-string 0 txt)
		txt (substring txt 0 (match-beginning 0))))
      (setq txt2 (read-string "Edit: " txt))
      (when (not (equal txt txt2))
	(beginning-of-line 1)
	(insert pre txt2 post)
	(delete-region (point) (point-at-eol))
	(org-set-tags nil t)))))

(defun org-columns-edit-allowed ()
  "Edit the list of allowed values for the current property."
  (interactive)
  (let* ((pom (or (get-text-property (point-at-bol) 'org-marker)
		  (get-text-property (point-at-bol) 'org-hd-marker)
		  (point)))
	 (key (get-char-property (point) 'org-columns-key))
	 (key1 (concat key "_ALL"))
	 (allowed (org-entry-get pom key1 t))
	 nval)
    ;; FIXME: Cover editing TODO, TAGS etc in-buffer settings.????
    ;; FIXME: Write back to #+PROPERTY setting if that is needed.
    (setq nval (read-string "Allowed: " allowed))
    (org-entry-put
     (cond ((marker-position org-entry-property-inherited-from)
	    org-entry-property-inherited-from)
	   ((marker-position org-columns-top-level-marker)
	    org-columns-top-level-marker)
	   (t pom))
     key1 nval)))

(defun org-columns-eval (form)
  (let (hidep)
    (save-excursion
      (beginning-of-line 1)
      ;; `next-line' is needed here, because it skips invisible line.
      (condition-case nil (org-no-warnings (next-line 1)) (error nil))
      (setq hidep (org-on-heading-p 1)))
    (eval form)
    (and hidep (hide-entry))))

(defun org-columns-previous-allowed-value ()
  "Switch to the previous allowed value for this column."
  (interactive)
  (org-columns-next-allowed-value t))

(defun org-columns-next-allowed-value (&optional previous)
  "Switch to the next allowed value for this column."
  (interactive)
  (org-columns-check-computed)
  (let* ((col (current-column))
	 (key (get-char-property (point) 'org-columns-key))
	 (value (get-char-property (point) 'org-columns-value))
	 (bol (point-at-bol)) (eol (point-at-eol))
	 (pom (or (get-text-property bol 'org-hd-marker)
		  (point))) ; keep despite of compiler waring
	 (line-overlays
	  (delq nil (mapcar (lambda (x)
			      (and (eq (overlay-buffer x) (current-buffer))
				   (>= (overlay-start x) bol)
				   (<= (overlay-start x) eol)
				   x))
			    org-columns-overlays)))
	 (allowed (or (org-property-get-allowed-values pom key)
		      (and (memq
			    (nth 4 (assoc key org-columns-current-fmt-compiled))
			    '(checkbox checkbox-n-of-m checkbox-percent))
			   '("[ ]" "[X]"))))
	 nval)
    (when (equal key "ITEM")
      (error "Cannot edit item headline from here"))
    (unless (or allowed (member key '("SCHEDULED" "DEADLINE")))
      (error "Allowed values for this property have not been defined"))
    (if (member key '("SCHEDULED" "DEADLINE"))
	(setq nval (if previous 'earlier 'later))
      (if previous (setq allowed (reverse allowed)))
      (if (member value allowed)
	  (setq nval (car (cdr (member value allowed)))))
      (setq nval (or nval (car allowed)))
      (if (equal nval value)
	  (error "Only one allowed value for this property")))
    (cond
     ((equal major-mode 'org-agenda-mode)
      (org-columns-eval '(org-entry-put pom key nval))
      ;; The following let preserves the current format, and makes sure
      ;; that in only a single file things need to be upated.
      (let* ((org-agenda-overriding-columns-format org-columns-current-fmt)
	     (buffer (marker-buffer pom))
	     (org-agenda-contributing-files
	      (list (with-current-buffer buffer
		      (buffer-file-name (buffer-base-buffer))))))
	(org-agenda-columns)))
     (t
      (let ((inhibit-read-only t))
	(remove-text-properties (1- bol) eol '(read-only t))
	(unwind-protect
	    (progn
	      (setq org-columns-overlays
		    (org-delete-all line-overlays org-columns-overlays))
	      (mapc 'org-delete-overlay line-overlays)
	      (org-columns-eval '(org-entry-put pom key nval)))
	  (org-columns-display-here)))
      (org-move-to-column col)
      (and (nth 3 (assoc key org-columns-current-fmt-compiled))
	   (org-columns-update key))))))

(defun org-verify-version (task)
  (cond
   ((eq task 'columns)
    (if (or (featurep 'xemacs)
	    (< emacs-major-version 22))
	(error "Emacs 22 is required for the columns feature")))))

(defun org-columns-open-link (&optional arg)
  (interactive "P")
  (let ((value (get-char-property (point) 'org-columns-value)))
    (org-open-link-from-string value arg)))

(defun org-columns-get-format-and-top-level ()
  (let (fmt)
    (when (condition-case nil (org-back-to-heading) (error nil))
      (move-marker org-entry-property-inherited-from nil)
      (setq fmt (org-entry-get nil "COLUMNS" t)))
    (setq fmt (or fmt org-columns-default-format))
    (org-set-local 'org-columns-current-fmt fmt)
    (org-columns-compile-format fmt)
    (if (marker-position org-entry-property-inherited-from)
	(move-marker org-columns-top-level-marker
		     org-entry-property-inherited-from)
      (move-marker org-columns-top-level-marker (point)))
    fmt))

(defun org-columns ()
  "Turn on column view on an org-mode file."
  (interactive)
  (org-verify-version 'columns)
  (org-columns-remove-overlays)
  (move-marker org-columns-begin-marker (point))
  (let (beg end fmt cache maxwidths)
    (setq fmt (org-columns-get-format-and-top-level))
    (save-excursion
      (goto-char org-columns-top-level-marker)
      (setq beg (point))
      (unless org-columns-inhibit-recalculation
	(org-columns-compute-all))
      (setq end (or (condition-case nil (org-end-of-subtree t t) (error nil))
		    (point-max)))
      ;; Get and cache the properties
      (goto-char beg)
      (when (assoc "CLOCKSUM" org-columns-current-fmt-compiled)
	(save-excursion
	  (save-restriction
	    (narrow-to-region beg end)
	    (org-clock-sum))))
      (while (re-search-forward (concat "^" outline-regexp) end t)
	(push (cons (org-current-line) (org-entry-properties)) cache))
      (when cache
	(setq maxwidths (org-columns-get-autowidth-alist fmt cache))
	(org-set-local 'org-columns-current-maxwidths maxwidths)
	(org-columns-display-here-title)
	(when (org-set-local 'org-columns-flyspell-was-active
			     (org-bound-and-true-p flyspell-mode))
	  (flyspell-mode 0))
	(mapc (lambda (x)
		(goto-line (car x))
		(org-columns-display-here (cdr x)))
	      cache)))))

(defun org-columns-new (&optional prop title width op fmt &rest rest)
  "Insert a new column, to the left of the current column."
  (interactive)
  (let ((editp (and prop (assoc prop org-columns-current-fmt-compiled)))
	cell)
    (setq prop (completing-read
		"Property: " (mapcar 'list (org-buffer-property-keys t nil t))
		nil nil prop))
    (setq title (read-string (concat "Column title [" prop "]: ") (or title prop)))
    (setq width (read-string "Column width: " (if width (number-to-string width))))
    (if (string-match "\\S-" width)
	(setq width (string-to-number width))
      (setq width nil))
    (setq fmt (completing-read "Summary [none]: "
			       '(("none") ("add_numbers") ("currency") ("add_times") ("checkbox") ("checkbox-n-of-m") ("checkbox-percent"))
			       nil t))
    (if (string-match "\\S-" fmt)
	(setq fmt (intern fmt))
      (setq fmt nil))
    (if (eq fmt 'none) (setq fmt nil))
    (if editp
	(progn
	  (setcar editp prop)
	  (setcdr editp (list title width nil fmt)))
      (setq cell (nthcdr (1- (current-column))
			 org-columns-current-fmt-compiled))
      (setcdr cell (cons (list prop title width nil fmt)
			 (cdr cell))))
    (org-columns-store-format)
    (org-columns-redo)))

(defun org-columns-delete ()
  "Delete the column at point from columns view."
  (interactive)
  (let* ((n (current-column))
	 (title (nth 1 (nth n org-columns-current-fmt-compiled))))
    (when (y-or-n-p
	   (format "Are you sure you want to remove column \"%s\"? " title))
      (setq org-columns-current-fmt-compiled
	    (delq (nth n org-columns-current-fmt-compiled)
		  org-columns-current-fmt-compiled))
      (org-columns-store-format)
      (org-columns-redo)
      (if (>= (current-column) (length org-columns-current-fmt-compiled))
	  (backward-char 1)))))

(defun org-columns-edit-attributes ()
  "Edit the attributes of the current column."
  (interactive)
  (let* ((n (current-column))
	 (info (nth n org-columns-current-fmt-compiled)))
    (apply 'org-columns-new info)))

(defun org-columns-widen (arg)
  "Make the column wider by ARG characters."
  (interactive "p")
  (let* ((n (current-column))
	 (entry (nth n org-columns-current-fmt-compiled))
	 (width (or (nth 2 entry)
		    (cdr (assoc (car entry) org-columns-current-maxwidths)))))
    (setq width (max 1 (+ width arg)))
    (setcar (nthcdr 2 entry) width)
    (org-columns-store-format)
    (org-columns-redo)))

(defun org-columns-narrow (arg)
  "Make the column nrrower by ARG characters."
  (interactive "p")
  (org-columns-widen (- arg)))

(defun org-columns-move-right ()
  "Swap this column with the one to the right."
  (interactive)
  (let* ((n (current-column))
	 (cell (nthcdr n org-columns-current-fmt-compiled))
	 e)
    (when (>= n (1- (length org-columns-current-fmt-compiled)))
      (error "Cannot shift this column further to the right"))
    (setq e (car cell))
    (setcar cell (car (cdr cell)))
    (setcdr cell (cons e (cdr (cdr cell))))
    (org-columns-store-format)
    (org-columns-redo)
    (forward-char 1)))

(defun org-columns-move-left ()
  "Swap this column with the one to the left."
  (interactive)
  (let* ((n (current-column)))
    (when (= n 0)
      (error "Cannot shift this column further to the left"))
    (backward-char 1)
    (org-columns-move-right)
    (backward-char 1)))

(defun org-columns-store-format ()
  "Store the text version of the current columns format in appropriate place.
This is either in the COLUMNS property of the node starting the current column
display, or in the #+COLUMNS line of the current buffer."
  (let (fmt (cnt 0))
    (setq fmt (org-columns-uncompile-format org-columns-current-fmt-compiled))
    (org-set-local 'org-columns-current-fmt fmt)
    (if (marker-position org-columns-top-level-marker)
	(save-excursion
	  (goto-char org-columns-top-level-marker)
	  (if (and (org-at-heading-p)
		   (org-entry-get nil "COLUMNS"))
	      (org-entry-put nil "COLUMNS" fmt)
	    (goto-char (point-min))
	    ;; Overwrite all #+COLUMNS lines....
	    (while (re-search-forward "^#\\+COLUMNS:.*" nil t)
	      (setq cnt (1+ cnt))
	      (replace-match (concat "#+COLUMNS: " fmt) t t))
	    (unless (> cnt 0)
	      (goto-char (point-min))
	      (or (org-on-heading-p t) (outline-next-heading))
	      (let ((inhibit-read-only t))
		(insert-before-markers "#+COLUMNS: " fmt "\n")))
	    (org-set-local 'org-columns-default-format fmt))))))

(defvar org-agenda-overriding-columns-format nil
  "When set, overrides any other format definition for the agenda.
Don't set this, this is meant for dynamic scoping.")

(defun org-columns-get-autowidth-alist (s cache)
  "Derive the maximum column widths from the format and the cache."
  (let ((start 0) rtn)
    (while (string-match (org-re "%\\([[:alpha:]][[:alnum:]_-]*\\)") s start)
      (push (cons (match-string 1 s) 1) rtn)
      (setq start (match-end 0)))
    (mapc (lambda (x)
	    (setcdr x (apply 'max
			     (mapcar
			      (lambda (y)
				(length (or (cdr (assoc (car x) (cdr y))) " ")))
			      cache))))
	  rtn)
    rtn))

(defun org-columns-compute-all ()
  "Compute all columns that have operators defined."
  (org-unmodified
   (remove-text-properties (point-min) (point-max) '(org-summaries t)))
  (let ((columns org-columns-current-fmt-compiled) col)
    (while (setq col (pop columns))
      (when (nth 3 col)
	(save-excursion
	  (org-columns-compute (car col)))))))

(defun org-columns-update (property)
  "Recompute PROPERTY, and update the columns display for it."
  (org-columns-compute property)
  (let (fmt val pos)
    (save-excursion
      (mapc (lambda (ov)
	      (when (equal (org-overlay-get ov 'org-columns-key) property)
		(setq pos (org-overlay-start ov))
		(goto-char pos)
		(when (setq val (cdr (assoc property
					    (get-text-property
					     (point-at-bol) 'org-summaries))))
		  (setq fmt (org-overlay-get ov 'org-columns-format))
		  (org-overlay-put ov 'org-columns-value val)
		  (org-overlay-put ov 'display (format fmt val)))))
	    org-columns-overlays))))

(defun org-columns-compute (property)
  "Sum the values of property PROPERTY hierarchically, for the entire buffer."
  (interactive)
  (let* ((re (concat "^" outline-regexp))
	 (lmax 30) ; Does anyone use deeper levels???
	 (lsum (make-vector lmax 0))
	 (lflag (make-vector lmax nil))
	 (level 0)
	 (ass (assoc property org-columns-current-fmt-compiled))
	 (format (nth 4 ass))
	 (printf (nth 5 ass))
	 (beg org-columns-top-level-marker)
	 last-level val valflag flag end sumpos sum-alist sum str str1 useval)
    (save-excursion
      ;; Find the region to compute
      (goto-char beg)
      (setq end (condition-case nil (org-end-of-subtree t) (error (point-max))))
      (goto-char end)
      ;; Walk the tree from the back and do the computations
      (while (re-search-backward re beg t)
	(setq sumpos (match-beginning 0)
	      last-level level
	      level (org-outline-level)
	      val (org-entry-get nil property)
	      valflag (and val (string-match "\\S-" val)))
	(cond
	 ((< level last-level)
	  ;; put the sum of lower levels here as a property
	  (setq sum (aref lsum last-level)   ; current sum
		flag (aref lflag last-level) ; any valid entries from children?
		str (org-columns-number-to-string sum format printf)
		str1 (org-add-props (copy-sequence str) nil 'org-computed t 'face 'bold)
		useval (if flag str1 (if valflag val ""))
		sum-alist (get-text-property sumpos 'org-summaries))
	  (if (assoc property sum-alist)
	      (setcdr (assoc property sum-alist) useval)
	    (push (cons property useval) sum-alist)
	    (org-unmodified
	     (add-text-properties sumpos (1+ sumpos)
				  (list 'org-summaries sum-alist))))
	  (when (and val (not (equal val (if flag str val))))
	    (org-entry-put nil property (if flag str val)))
	  ;; add current to current  level accumulator
	  (when (or flag valflag)
	    (aset lsum level (+ (aref lsum level)
				(if flag sum (org-column-string-to-number
					      (if flag str val) format))))
	    (aset lflag level t))
	  ;; clear accumulators for deeper levels
	  (loop for l from (1+ level) to (1- lmax) do
		(aset lsum l 0)
		(aset lflag l nil)))
	 ((>= level last-level)
	  ;; add what we have here to the accumulator for this level
	  (aset lsum level (+ (aref lsum level)
			      (org-column-string-to-number (or val "0") format)))
	  (and valflag (aset lflag level t)))
	 (t (error "This should not happen")))))))

(defun org-columns-redo ()
  "Construct the column display again."
  (interactive)
  (message "Recomputing columns...")
  (save-excursion
    (if (marker-position org-columns-begin-marker)
	(goto-char org-columns-begin-marker))
    (org-columns-remove-overlays)
    (if (org-mode-p)
	(call-interactively 'org-columns)
      (call-interactively 'org-agenda-columns)))
  (message "Recomputing columns...done"))

(defun org-columns-not-in-agenda ()
  (if (eq major-mode 'org-agenda-mode)
      (error "This command is only allowed in Org-mode buffers")))


(defun org-string-to-number (s)
  "Convert string to number, and interpret hh:mm:ss."
  (if (not (string-match ":" s))
      (string-to-number s)
    (let ((l (nreverse (org-split-string s ":"))) (sum 0.0))
      (while l
	(setq sum (+ (string-to-number (pop l)) (/ sum 60))))
      sum)))

(defun org-columns-number-to-string (n fmt &optional printf)
  "Convert a computed column number to a string value, according to FMT."
  (cond
   ((eq fmt 'add_times)
    (let* ((h (floor n)) (m (floor (+ 0.5 (* 60 (- n h))))))
      (format "%d:%02d" h m)))
   ((eq fmt 'checkbox)
    (cond ((= n (floor n)) "[X]")
	  ((> n 1.) "[-]")
	  (t "[ ]")))
   ((memq fmt '(checkbox-n-of-m checkbox-percent))
    (let* ((n1 (floor n)) (n2 (floor (+ .5 (* 1000000 (- n n1))))))
      (org-nofm-to-completion n1 (+ n2 n1) (eq fmt 'checkbox-percent))))
   (printf (format printf n))
   ((eq fmt 'currency)
    (format "%.2f" n))
   (t (number-to-string n))))

(defun org-nofm-to-completion (n m &optional percent)
  (if (not percent)
      (format "[%d/%d]" n m)
    (format "[%d%%]"(floor (+ 0.5 (* 100. (/ (* 1.0 n) m)))))))

(defun org-column-string-to-number (s fmt)
  "Convert a column value to a number that can be used for column computing."
  (cond
   ((string-match ":" s)
    (let ((l (nreverse (org-split-string s ":"))) (sum 0.0))
      (while l
	(setq sum (+ (string-to-number (pop l)) (/ sum 60))))
      sum))
   ((memq fmt '(checkbox checkbox-n-of-m checkbox-percent))
    (if (equal s "[X]") 1. 0.000001))
   (t (string-to-number s))))

(defun org-columns-uncompile-format (cfmt)
  "Turn the compiled columns format back into a string representation."
  (let ((rtn "") e s prop title op width fmt printf)
    (while (setq e (pop cfmt))
      (setq prop (car e)
	    title (nth 1 e)
	    width (nth 2 e)
	    op (nth 3 e)
	    fmt (nth 4 e)
	    printf (nth 5 e))
      (cond
       ((eq fmt 'add_times) (setq op ":"))
       ((eq fmt 'checkbox) (setq op "X"))
       ((eq fmt 'checkbox-n-of-m) (setq op "X/"))
       ((eq fmt 'checkbox-percent) (setq op "X%"))
       ((eq fmt 'add_numbers) (setq op "+"))
       ((eq fmt 'currency) (setq op "$")))
      (if (and op printf) (setq op (concat op ";" printf)))
      (if (equal title prop) (setq title nil))
      (setq s (concat "%" (if width (number-to-string width))
		      prop
		      (if title (concat "(" title ")"))
		      (if op (concat "{" op "}"))))
      (setq rtn (concat rtn " " s)))
    (org-trim rtn)))

(defun org-columns-compile-format (fmt)
  "Turn a column format string into an alist of specifications.
The alist has one entry for each column in the format.  The elements of
that list are:
property     the property
title        the title field for the columns
width        the column width in characters, can be nil for automatic
operator     the operator if any
format       the output format for computed results, derived from operator
printf       a printf format for computed values"
  (let ((start 0) width prop title op f printf)
    (setq org-columns-current-fmt-compiled nil)
    (while (string-match
	    (org-re "%\\([0-9]+\\)?\\([[:alnum:]_-]+\\)\\(?:(\\([^)]+\\))\\)?\\(?:{\\([^}]+\\)}\\)?\\s-*")
	    fmt start)
      (setq start (match-end 0)
	    width (match-string 1 fmt)
	    prop (match-string 2 fmt)
	    title (or (match-string 3 fmt) prop)
	    op (match-string 4 fmt)
	    f nil
	    printf nil)
      (if width (setq width (string-to-number width)))
      (when (and op (string-match ";" op))
	(setq printf (substring op (match-end 0))
	      op (substring op 0 (match-beginning 0))))
      (cond
       ((equal op "+")  (setq f 'add_numbers))
       ((equal op "$")  (setq f 'currency))
       ((equal op ":")  (setq f 'add_times))
       ((equal op "X")  (setq f 'checkbox))
       ((equal op "X/") (setq f 'checkbox-n-of-m))
       ((equal op "X%") (setq f 'checkbox-percent))
       )
      (push (list prop title width op f printf) org-columns-current-fmt-compiled))
    (setq org-columns-current-fmt-compiled
	  (nreverse org-columns-current-fmt-compiled))))


;;; Dynamic block for Column view

(defun org-columns-capture-view (&optional maxlevel skip-empty-rows)
  "Get the column view of the current buffer or subtree.
The first optional argument MAXLEVEL sets the level limit.  A
second optional argument SKIP-EMPTY-ROWS tells whether to skip
empty rows, an empty row being one where all the column view
specifiers except ITEM are empty.  This function returns a list
containing the title row and all other rows.  Each row is a list
of fields."
  (save-excursion
    (let* ((title (mapcar 'cadr org-columns-current-fmt-compiled))
	   (n (length title)) row tbl)
      (goto-char (point-min))
      (while (and (re-search-forward "^\\(\\*+\\) " nil t)
		  (or (null maxlevel)
		      (>= maxlevel
			  (if org-odd-levels-only
			      (/ (1+ (length (match-string 1))) 2)
			    (length (match-string 1))))))
	(when (get-char-property (match-beginning 0) 'org-columns-key)
	  (setq row nil)
	  (loop for i from 0 to (1- n) do
		(push (or (get-char-property (+ (match-beginning 0) i) 'org-columns-value-modified)
			  (get-char-property (+ (match-beginning 0) i) 'org-columns-value)
			  "")
		      row))
	  (setq row (nreverse row))
	  (unless (and skip-empty-rows
		       (eq 1 (length (delete "" (delete-dups row)))))
	    (push row tbl))))
      (append (list title 'hline) (nreverse tbl)))))

(defun org-dblock-write:columnview (params)
  "Write the column view table.
PARAMS is a property list of parameters:

:width    enforce same column widths with <N> specifiers.
:id       the :ID: property of the entry where the columns view
          should be built, as a string.  When `local', call locally.
          When `global' call column view with the cursor at the beginning
          of the buffer (usually this means that the whole buffer switches
          to column view).
:hlines   When t, insert a hline before each item.  When a number, insert
          a hline before each level <= that number.
:vlines   When t, make each column a colgroup to enforce vertical lines.
:maxlevel When set to a number, don't capture headlines below this level.
:skip-empty-rows
          When t, skip rows where all specifiers other than ITEM are empty."
  (let ((pos (move-marker (make-marker) (point)))
	(hlines (plist-get params :hlines))
	(vlines (plist-get params :vlines))
	(maxlevel (plist-get params :maxlevel))
	(skip-empty-rows (plist-get params :skip-empty-rows))
	tbl id idpos nfields tmp)
    (save-excursion
      (save-restriction
	(when (setq id (plist-get params :id))
	  (cond ((not id) nil)
		((eq id 'global) (goto-char (point-min)))
		((eq id 'local)  nil)
		((setq idpos (org-find-entry-with-id id))
		 (goto-char idpos))
		(t (error "Cannot find entry with :ID: %s" id))))
	(org-columns)
	(setq tbl (org-columns-capture-view maxlevel skip-empty-rows))
	(setq nfields (length (car tbl)))
	(org-columns-quit)))
    (goto-char pos)
    (move-marker pos nil)
    (when tbl
      (when (plist-get params :hlines)
	(setq tmp nil)
	(while tbl
	  (if (eq (car tbl) 'hline)
	      (push (pop tbl) tmp)
	    (if (string-match "\\` *\\(\\*+\\)" (caar tbl))
		(if (and (not (eq (car tmp) 'hline))
			 (or (eq hlines t)
			     (and (numberp hlines) (<= (- (match-end 1) (match-beginning 1)) hlines))))
		    (push 'hline tmp)))
	    (push (pop tbl) tmp)))
	(setq tbl (nreverse tmp)))
      (when vlines
	(setq tbl (mapcar (lambda (x)
			    (if (eq 'hline x) x (cons "" x)))
			  tbl))
	(setq tbl (append tbl (list (cons "/" (make-list nfields "<>"))))))
      (setq pos (point))
      (insert (org-listtable-to-string tbl))
      (when (plist-get params :width)
	(insert "\n|" (mapconcat (lambda (x) (format "<%d>" (max 3 x)))
				 org-columns-current-widths "|")))
      (goto-char pos)
      (org-table-align))))

(defun org-listtable-to-string (tbl)
  "Convert a listtable TBL to a string that contains the Org-mode table.
The table still need to be alligned.  The resulting string has no leading
and tailing newline characters."
  (mapconcat
   (lambda (x)
     (cond
      ((listp x)
       (concat "|" (mapconcat 'identity x "|") "|"))
      ((eq x 'hline) "|-|")
      (t (error "Garbage in listtable: %s" x))))
   tbl "\n"))

(defun org-insert-columns-dblock ()
  "Create a dynamic block capturing a column view table."
  (interactive)
  (let ((defaults '(:name "columnview" :hlines 1))
	(id (completing-read
	     "Capture columns (local, global, entry with :ID: property) [local]: "
	     (append '(("global") ("local"))
		     (mapcar 'list (org-property-values "ID"))))))
    (if (equal id "") (setq id 'local))
    (if (equal id "global") (setq id 'global))
    (setq defaults (append defaults (list :id id)))
    (org-create-dblock defaults)
    (org-update-dblock)))

;;; Column view in the agenda

(defvar org-agenda-view-columns-initially nil
  "When set, switch to columns view immediately after creating the agenda.")

(defvar org-agenda-columns-show-summaries) ; defined in org-agenda.el
(defvar org-agenda-columns-compute-summary-properties); defined in org-agenda.el
(defvar org-agenda-columns-add-appointments-to-effort-sum); as well

(defun org-agenda-columns ()
  "Turn on or update column view in the agenda."
  (interactive)
  (org-verify-version 'columns)
  (org-columns-remove-overlays)
  (move-marker org-columns-begin-marker (point))
  (let (fmt cache maxwidths m p a d)
    (cond
     ((and (boundp 'org-agenda-overriding-columns-format)
	   org-agenda-overriding-columns-format)
      (setq fmt org-agenda-overriding-columns-format)
      (org-set-local 'org-agenda-overriding-columns-format fmt))
     ((setq m (get-text-property (point-at-bol) 'org-hd-marker))
      (setq fmt (or (org-entry-get m "COLUMNS" t)
		    (with-current-buffer (marker-buffer m)
		      org-columns-default-format))))
     ((and (boundp 'org-columns-current-fmt)
	   (local-variable-p 'org-columns-current-fmt)
	   org-columns-current-fmt)
      (setq fmt org-columns-current-fmt))
     ((setq m (next-single-property-change (point-min) 'org-hd-marker))
      (setq m (get-text-property m 'org-hd-marker))
      (setq fmt (or (org-entry-get m "COLUMNS" t)
		    (with-current-buffer (marker-buffer m)
		      org-columns-default-format)))))
    (setq fmt (or fmt org-columns-default-format))
    (org-set-local 'org-columns-current-fmt fmt)
    (org-columns-compile-format fmt)
    (when org-agenda-columns-compute-summary-properties
      (org-agenda-colview-compute org-columns-current-fmt-compiled))
    (save-excursion
      ;; Get and cache the properties
      (goto-char (point-min))
      (while (not (eobp))
	(when (setq m (or (get-text-property (point) 'org-hd-marker)
			  (get-text-property (point) 'org-marker)))
	  (setq p (org-entry-properties m))

	  (when (or (not (setq a (assoc org-effort-property p)))
			 (not (string-match "\\S-" (or (cdr a) ""))))
	    ;; OK, the property is not defined.  Use appointment duration?
	    (when (and org-agenda-columns-add-appointments-to-effort-sum
		       (setq d (get-text-property (point) 'duration)))
	      (setq d (org-minutes-to-hh:mm-string d))
	      (put-text-property 0 (length d) 'face 'org-warning d)
	      (push (cons org-effort-property d) p)))
	  (push (cons (org-current-line) p) cache))
	(beginning-of-line 2))
      (when cache
	(setq maxwidths (org-columns-get-autowidth-alist fmt cache))
	(org-set-local 'org-columns-current-maxwidths maxwidths)
	(org-columns-display-here-title)
	(when (org-set-local 'org-columns-flyspell-was-active
			     (org-bound-and-true-p flyspell-mode))
	  (flyspell-mode 0))
	(mapc (lambda (x)
		(goto-line (car x))
		(org-columns-display-here (cdr x)))
	      cache)
	(when org-agenda-columns-show-summaries
	  (org-agenda-colview-summarize cache))))))

(defun org-agenda-colview-summarize (cache)
  "Summarize the summarizable columns in column view in the agenda.
This will add overlays to the date lines, to show the summary for each day."
  (let* ((fmt (mapcar (lambda (x)
			(list (car x) (if (equal (car x) "CLOCKSUM")
					  'add_times (nth 4 x))))
		      org-columns-current-fmt-compiled))
	 line c c1 stype props lsum entries prop v)
    (catch 'exit
      (when (delq nil (mapcar 'cadr fmt))
	;; OK, at least one summation column, it makes sense to try this
	(goto-char (point-max))
	(while t
	  (when (or (get-text-property (point) 'org-date-line)
		    (eq (get-text-property (point) 'face)
			'org-agenda-structure))
	    ;; OK, this is a date line that should be used
	    (setq line (org-current-line))
	    (setq entries nil c cache cache nil)
	    (while (setq c1 (pop c))
	      (if (> (car c1) line)
		  (push c1 entries)
		(push c1 cache)))
	    ;; now ENTRIES are the ones we want to use, CACHE is the rest
	    ;; Compute the summaries for the properties we want,
	    ;; set nil properties for the rest.
	    (when (setq entries (mapcar 'cdr entries))
	      (setq props
		    (mapcar
		     (lambda (f)
		       (setq prop (car f) stype (nth 1 f))
		       (cond
			((equal prop "ITEM")
			 (cons prop (buffer-substring (point-at-bol)
						      (point-at-eol))))
			((not stype) (cons prop ""))
			(t
			 ;; do the summary
			 (setq lsum 0)
			 (mapc (lambda (x)
				 (setq v (cdr (assoc prop x)))
				 (if v (setq lsum (+ lsum
						     (org-column-string-to-number
						      v stype)))))
			       entries)
			 (setq lsum (org-columns-number-to-string lsum stype))
			 (put-text-property
			  0 (length lsum) 'face 'bold lsum)
			 (cons prop lsum))))
		     fmt))
	      (org-columns-display-here props)
	      (org-set-local 'org-agenda-columns-active t)))
	  (if (bobp) (throw 'exit t))
	  (beginning-of-line 0))))))

(defun org-agenda-colview-compute (fmt)
  "Compute the relevant columns in the contributing source buffers."
  (let ((files org-agenda-contributing-files)
	(org-columns-begin-marker (make-marker))
	(org-columns-top-level-marker (make-marker))
	f fm a b)
    (while (setq f (pop files))
      (setq b (find-buffer-visiting f))
      (with-current-buffer (or (buffer-base-buffer b) b)
	(save-excursion
	  (save-restriction
	    (widen)
	    (org-unmodified
	     (remove-text-properties (point-min) (point-max)
				     '(org-summaries t)))
	    (goto-char (point-min))
	    (org-columns-get-format-and-top-level)
	    (while (setq fm (pop fmt))
	      (if (equal (car fm) "CLOCKSUM")
		  (org-clock-sum)
		(when (and (nth 4 fm)
			   (setq a (assoc (car fm)
					  org-columns-current-fmt-compiled))
			   (equal (nth 4 a) (nth 4 fm)))
		  (org-columns-compute (car fm)))))))))))

(provide 'org-colview)

;;; org-colview.el ends here

