;;; libc.el -- lookup C symbols in the GNU C Library Reference Manual.

;; Copyright (C) 1995, 1996, 1997 Free Software Foundation, Inc.

;;; Author: Ralph Schleicher <rs@purple.UL.BaWue.DE>
;;; Keywords: local c info

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

;; This code has a long history.  It started as a minor
;; mode for C mode.  This era ended with the release of version 2
;; of the GNU C Library in 1997.  The code was therefore rewritten
;; more or less from scratch so that all lookups are performed via
;; indices.  Not finding an existing symbol in an index means that
;; there is an error in the manual.  Long missed features like a
;; separate input history, symbol name completion in the mini-buffer,
;; highlighting of looked up symbol names in the Info buffer, and
;; implicitly prepending `struct', `union' or `enum' to data types
;; were added in this phase too.

;;; Code:

(require 'info)


(defvar libc-info-file-name "libc"
  "Basename of the Info file of the GNU C Library Reference Manual.")

(defvar libc-highlight-face 'highlight
  "*Face for highlighting looked up symbol names in the Info buffer.
`nil' disables highlighting.")

(defvar libc-highlight-overlay nil
  "Overlay object used for highlighting.")

(defconst libc-symbol-completions nil
  "Alist of documented C symbols.")

(defconst libc-file-completions nil
  "Alist of documented programs or files.")

(defvar libc-history nil
  "History of previous input lines.")

;;;###autoload
(defun libc-describe-symbol (symbol-name)
  "Display the documentation of a C symbol in another window.
SYMBOL-NAME must be documented in the GNU C Library Reference Manual.

If called interactively, SYMBOL-NAME will be read from the mini-buffer.
Optional prefix argument means insert the default symbol (if any) into
the mini-buffer so that it can be edited.  The default symbol is the
one found at point.

If SYMBOL-NAME is a public function, variable, or data type of the GNU
C Library but `libc-describe-symbol' fails to display it's documentation,
then you have found a bug in the manual.  Please report that to the mail
address `bug-glibc-manual@prep.ai.mit.edu' so that it can be fixed."
  (interactive
   (let* ((completion-ignore-case nil)
	  (enable-recursive-minibuffers t)
	  (symbol (libc-symbol-at-point))
	  (value (completing-read
		  (if symbol
		      (format "Describe symbol (default %s): " symbol)
		    (format "Describe symbol: "))
		  libc-symbol-completions nil nil
		  (and current-prefix-arg symbol) 'libc-history)))
     (list (if (equal value "") symbol value))))
  (or (assoc symbol-name libc-symbol-completions)
      (error "Not documented as a C symbol: %s" (or symbol-name "")))
  (or (libc-lookup-function symbol-name)
      (libc-lookup-variable symbol-name)
      (libc-lookup-type symbol-name)))

;;;###autoload
(defun libc-describe-file (file-name)
  "Display the documentation of a program or file in another window.
FILE-NAME must be documented in the GNU C Library Reference Manual."
  (interactive
   (let* ((completion-ignore-case nil)
	  (enable-recursive-minibuffers t))
     (list (completing-read
	    "Describe program or file: "
	    libc-file-completions nil nil nil 'libc-history))))
  (or (assoc file-name libc-file-completions)
      (error "Not documented as a program or file: %s" (or file-name "")))
  (libc-lookup-file file-name))

;;;###autoload
(defun libc-search (regexp &optional arg)
  "Search in the GNU C Library Reference Manual for REGEXP.
Prefix argument means search should ignore case."
  (interactive "sSearch `libc.info' for regexp: \nP")
  (or (get-buffer "*info*")
      (save-window-excursion
	(info)))
  (switch-to-buffer-other-window "*info*")
  (Info-goto-node (concat "(" libc-info-file-name ")"))
  (let ((case-fold-search arg))
    (Info-search regexp)))


(defun libc-make-completion-alist (info-nodes &optional regexp)
  "Create a unique alist from all menu items in the Info nodes INFO-NODES
of the GNU C Reference Manual.

Optional second argument REGEXP means include only menu items matching the
regular expression REGEXP."
  (condition-case nil
      (let (completions item)
	(save-window-excursion
	  (info libc-info-file-name)
	  (while info-nodes
	    (Info-goto-node (car info-nodes))
	    (goto-char (point-min))
	    (and (search-forward "\n* Menu:" nil t)
		 (while (re-search-forward "\n\\* \\([^:\t\n]*\\):" nil t)
		   (setq item (buffer-substring
			       (match-beginning 1) (match-end 1)))
		   (and (not (assoc item completions))
			(if regexp (string-match regexp item) t)
			(setq completions (cons (cons item nil)
						completions)))))
	    (setq info-nodes (cdr info-nodes)))
	  (Info-directory))
	completions)
    (error nil)))

(defun libc-after-manual-update ()
  "This function must only be called after a new version of the
GNU C Library Reference Manual was installed on your system."
  (setq libc-symbol-completions (libc-make-completion-alist
				 '("Function Index"
				   "Variable Index"
				   "Type Index"))
	libc-file-completions (libc-make-completion-alist
			       '("File Index") "^[^ \t]+$")))

(or (and libc-symbol-completions
	 libc-file-completions)
    (libc-after-manual-update))

(defun libc-symbol-at-point ()
  "Get the C symbol at point."
  (condition-case nil
      (save-excursion
	(backward-sexp)
	(let ((start (point))
	      prefix name)
	  ;; Test for a leading `struct', `union', or `enum' keyword
	  ;; but ignore names like `foo_struct'.
	  (setq prefix (and (< (skip-chars-backward " \t\n") 0)
			    (< (skip-chars-backward "_a-zA-Z0-9") 0)
			    (looking-at "\\(struct\\|union\\|enum\\)\\s ")
			    (concat (buffer-substring
				     (match-beginning 1) (match-end 1))
				    " ")))
	  (goto-char start)
	  (and (looking-at "[_a-zA-Z][_a-zA-Z0-9]*")
	       (setq name (buffer-substring
			   (match-beginning 0) (match-end 0))))
	  ;; Caveat!  Look forward if point is at `struct' etc.
	  (and (not prefix)
	       (or (string-equal name "struct")
		   (string-equal name "union")
		   (string-equal name "enum"))
	       (looking-at "[a-z]+\\s +\\([_a-zA-Z][_a-zA-Z0-9]*\\)")
	       (setq prefix (concat name " ")
		     name (buffer-substring
			   (match-beginning 1) (match-end 1))))
	  (and (or prefix name)
	       (concat prefix name))))
    (error nil)))

(defun libc-lookup-function (function)
  (libc-search-index "Function Index" function
		     "^[ \t]+- \\(Function\\|Macro\\): .*\\<" "\\>"))

(defun libc-lookup-variable (variable)
  (libc-search-index "Variable Index" variable
		     "^[ \t]+- \\(Variable\\|Macro\\): .*\\<" "\\>"))

(defun libc-lookup-type (data-type)
  (libc-search-index "Type Index" data-type
		     "^[ \t]+- Data Type: \\<" "\\>"))

(defun libc-lookup-file (file-name)
  (libc-search-index "File Index" file-name))

(defun libc-search-index (index item &optional prefix suffix)
  "Search ITEM in the Info index INDEX and go to that Info node.

Value is ITEM or `nil' if an error occurs.

If PREFIX and/or SUFFIX are non-`nil', then search the Info node for
the first occurrence of the regular expression `PREFIX ITEM SUFFIX' and
leave point at the beginning of the first line of the match.  ITEM will
be highlighted with `libc-highlight-face' iff `libc-highlight-face' is
not `nil'."
  (condition-case nil
      (save-selected-window
	(or (get-buffer "*info*")
	    (save-window-excursion
	      (info)))
	(switch-to-buffer-other-window "*info*")
	(Info-goto-node (concat "(" libc-info-file-name ")" index))
	(Info-menu item)
	(if (or prefix suffix)
	    (let ((case-fold-search nil)
		  (buffer-read-only nil))
	      (goto-char (point-min))
	      (re-search-forward
	       (concat prefix (regexp-quote item) suffix))
	      (goto-char (match-beginning 0))
	      (and window-system libc-highlight-face
		   ;; Search again for ITEM so that the first
		   ;; occurence of ITEM will be highlighted.
		   (save-excursion
		     (re-search-forward (regexp-quote item))
		     (let ((start (match-beginning 0))
			   (end (match-end 0)))
		       (if (overlayp libc-highlight-overlay)
			   (move-overlay libc-highlight-overlay
					 start end (current-buffer))
			 (setq libc-highlight-overlay
			       (make-overlay start end))))
		     (overlay-put libc-highlight-overlay
				  'face libc-highlight-face)))
	      (beginning-of-line)))
	item)
    (error nil)))


(provide 'libc)

;;; libc.el ends here
