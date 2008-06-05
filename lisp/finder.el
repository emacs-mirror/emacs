;;; finder.el --- topic & keyword-based code finder

;; Copyright (C) 1992, 1997, 1998, 1999, 2001, 2002, 2003, 2004, 2005,
;;   2006, 2007, 2008  Free Software Foundation, Inc.

;; Author: Eric S. Raymond <esr@snark.thyrsus.com>
;; Created: 16 Jun 1992
;; Version: 1.0
;; Keywords: help

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This mode uses the Keywords library header to provide code-finding
;; services by keyword.
;;
;; Things to do:
;;    1. Support multiple keywords per search.  This could be extremely hairy;
;; there doesn't seem to be any way to get completing-read to exit on
;; an EOL with no substring pending, which is what we'd want to end the loop.
;;    2. Search by string in synopsis line?
;;    3. Function to check finder-package-info for unknown keywords.

;;; Code:

(require 'lisp-mnt)
(require 'find-func)			;for find-library(-suffixes)
;; Use `load' rather than `require' so that it doesn't get loaded
;; during byte-compilation (at which point it might be missing).
(load "finder-inf" t t)

;; These are supposed to correspond to top-level customization groups,
;; says rms.
(defvar finder-known-keywords
  '(
    (abbrev	. "abbreviation handling, typing shortcuts, macros")
    ;; Too specific:
    (bib	. "code related to the `bib' bibliography processor")
    (c		. "support for the C language and related languages")
    (calendar	. "calendar and time management support")
    (comm	. "communications, networking, remote access to files")
    (convenience . "convenience features for faster editing")
    (data	. "support for editing files of data")
    (docs	. "support for Emacs documentation")
    (emulations	. "emulations of other editors")
    (extensions	. "Emacs Lisp language extensions")
    (faces	. "support for multiple fonts")
    (files      . "support for editing and manipulating files")
    (frames     . "support for Emacs frames and window systems")
    (games	. "games, jokes and amusements")
    (hardware	. "support for interfacing with exotic hardware")
    (help	. "support for on-line help systems")
    (hypermedia . "support for links between text or other media types")
    (i18n	. "internationalization and alternate character-set support")
    (internal	. "code for Emacs internals, build process, defaults")
    (languages	. "specialized modes for editing programming languages")
    (lisp	. "Lisp support, including Emacs Lisp")
    (local	. "code local to your site")
    (maint	. "maintenance aids for the Emacs development group")
    (mail	. "modes for electronic-mail handling")
    (matching	. "various sorts of searching and matching")
    (mouse	. "mouse support")
    (multimedia . "images and sound support")
    (news	. "support for netnews reading and posting")
    (oop        . "support for object-oriented programming")
    (outlines   . "support for hierarchical outlining")
    (processes	. "process, subshell, compilation, and job control support")
    (terminals	. "support for terminal types")
    (tex	. "supporting code for the TeX formatter")
    (tools	. "programming tools")
    (unix	. "front-ends/assistants for, or emulators of, UNIX-like features")
;; Not a custom group and not currently useful.
;;    (vms	. "support code for vms")
    (wp		. "word processing")
    ))

(defvar finder-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map " "	'finder-select)
    (define-key map "f"	'finder-select)
    (define-key map [follow-link] 'mouse-face)
    (define-key map [mouse-2]	'finder-mouse-select)
    (define-key map "\C-m"	'finder-select)
    (define-key map "?"	'finder-summary)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "q"	'finder-exit)
    (define-key map "d"	'finder-list-keywords)
    map))

(defvar finder-mode-syntax-table
  (let ((st (make-syntax-table emacs-lisp-mode-syntax-table)))
    (modify-syntax-entry ?\; ".   " st)
    st)
  "Syntax table used while in `finder-mode'.")

(defvar finder-font-lock-keywords
  '(("`\\([^']+\\)'" 1 font-lock-constant-face prepend))
  "Font-lock keywords for Finder mode.")

(defvar finder-headmark nil
  "Internal finder-mode variable, local in finder buffer.")

;;; Code for regenerating the keyword list.

(defvar finder-package-info nil
  "Assoc list mapping file names to description & keyword lists.")

(defvar generated-finder-keywords-file "finder-inf.el"
  "The function `finder-compile-keywords' writes keywords into this file.")

;; Skip autogenerated files, because they will never contain anything
;; useful, and because in parallel builds of Emacs they may get
;; modified while we are trying to read them.
;; http://lists.gnu.org/archive/html/emacs-pretest-bug/2007-01/msg00469.html
(defvar finder-no-scan-regexp "\\(^\\.#\\|\\(loaddefs\\|cus-load\\|\
finder-inf\\|esh-groups\\|subdirs\\)\\.el$\\)"
  "Regexp matching file names not to scan for keywords.")

(autoload 'autoload-rubric "autoload")

(defun finder-compile-keywords (&rest dirs)
  "Regenerate the keywords association list into `generated-finder-keywords-file'.
Optional arguments DIRS are a list of Emacs Lisp directories to compile from;
no arguments compiles from `load-path'."
  (save-excursion
    (let (processed summary keystart keywords)
      (find-file generated-finder-keywords-file)
      (setq buffer-undo-list t)
      (erase-buffer)
      (insert (autoload-rubric generated-finder-keywords-file
                               "keyword-to-package mapping"))
      (search-backward "")
      (insert "\n(setq finder-package-info '(\n")
      (mapc
       (lambda (d)
	 (when (file-exists-p (directory-file-name d))
	   (message "Directory %s" d)
	   (mapc
	    (lambda (f)
              ;; FIXME should this not be using (expand-file-name f d)?
	      (unless (or (member f processed)
                          (string-match finder-no-scan-regexp f))
                (setq processed (cons f processed))
                (with-temp-buffer
                  (insert-file-contents (expand-file-name f d))
                  (setq summary (lm-synopsis)
                        keywords (lm-keywords)))
                (insert
                 (format "    (\"%s\"\n        "
                         (if (string-match "\\.\\(gz\\|Z\\)$" f)
                             (file-name-sans-extension f)
                           f)))
                (prin1 summary (current-buffer))
                (insert "\n        ")
                (setq keystart (point))
                (insert (if keywords (format "(%s)" keywords) "nil")
                        ")\n")
                (subst-char-in-region keystart (point) ?, ? )))
	    (directory-files d nil
                             ;; Allow compressed files also.  FIXME:
                             ;; generalize this, especially for
                             ;; MS-DOG-type filenames.
                             "^[^=].*\\.el\\(\\.\\(gz\\|Z\\)\\)?$"
                             ))))
       (or dirs load-path))
      (insert "))\n")
      (eval-buffer)       ; so we get the new keyword list immediately
      (basic-save-buffer))))

(defun finder-compile-keywords-make-dist ()
  "Regenerate `finder-inf.el' for the Emacs distribution."
  (apply 'finder-compile-keywords command-line-args-left)
  (kill-emacs))

;;; Now the retrieval code

(defun finder-insert-at-column (column &rest strings)
  "Insert, at column COLUMN, other args STRINGS."
  (if (>= (current-column) column) (insert "\n"))
  (move-to-column column t)
  (apply 'insert strings))

(defvar finder-help-echo nil)

(defun finder-mouse-face-on-line ()
  "Put `mouse-face' and `help-echo' properties on the previous line."
  (save-excursion
    (forward-line -1)
    (unless finder-help-echo
      (setq finder-help-echo
	    (let* ((keys1 (where-is-internal 'finder-select
					     finder-mode-map))
		   (keys (nconc (where-is-internal
				 'finder-mouse-select finder-mode-map)
				keys1)))
	      (concat (mapconcat 'key-description keys ", ")
		      ": select item"))))
    (add-text-properties
     (line-beginning-position) (line-end-position)
     '(mouse-face highlight
		  help-echo finder-help-echo))))

;;;###autoload
(defun finder-list-keywords ()
  "Display descriptions of the keywords in the Finder buffer."
  (interactive)
  (if (get-buffer "*Finder*")
      (pop-to-buffer "*Finder*")
    (pop-to-buffer (get-buffer-create "*Finder*"))
    (finder-mode)
    (setq buffer-read-only nil
          buffer-undo-list t)
    (erase-buffer)
    (mapc
     (lambda (assoc)
       (let ((keyword (car assoc)))
	 (insert (symbol-name keyword))
	 (finder-insert-at-column 14 (concat (cdr assoc) "\n"))
	 (finder-mouse-face-on-line)))
     finder-known-keywords)
    (goto-char (point-min))
    (setq finder-headmark (point)
          buffer-read-only t)
    (set-buffer-modified-p nil)
    (balance-windows)
    (finder-summary)))

(defun finder-list-matches (key)
  (pop-to-buffer (set-buffer (get-buffer-create "*Finder Category*")))
  (finder-mode)
  (setq buffer-read-only nil
         buffer-undo-list t)
  (erase-buffer)
  (let ((id (intern key)))
    (insert
     "The following packages match the keyword `" key "':\n\n")
    (setq finder-headmark (point))
    (mapc
     (lambda (x)
       (if (memq id (car (cdr (cdr x))))
	   (progn
	     (insert (car x))
	     (finder-insert-at-column 16 (concat (nth 1 x) "\n"))
	     (finder-mouse-face-on-line))))
     finder-package-info)
    (goto-char (point-min))
    (forward-line)
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)
    (shrink-window-if-larger-than-buffer)
    (finder-summary)))

;;;###autoload
(defun finder-commentary (file)
  "Display FILE's commentary section.
FILE should be in a form suitable for passing to `locate-library'."
  (interactive
   (list
    (completing-read "Library name: "
		     (apply-partially 'locate-file-completion-table
                                      (or find-function-source-path load-path)
                                      (find-library-suffixes)))))
  (let ((str (lm-commentary (find-library-name file))))
    (or str (error "Can't find any Commentary section"))
    ;; This used to use *Finder* but that would clobber the
    ;; directory of categories.
    (delete-other-windows)
    (pop-to-buffer "*Finder-package*")
    (setq buffer-read-only nil
          buffer-undo-list t)
    (erase-buffer)
    (insert str)
    (goto-char (point-min))
    (delete-blank-lines)
    (goto-char (point-max))
    (delete-blank-lines)
    (goto-char (point-min))
    (while (re-search-forward "^;+ ?" nil t)
      (replace-match "" nil nil))
    (goto-char (point-min))
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)
    (shrink-window-if-larger-than-buffer)
    (finder-mode)
    (finder-summary)))

(defun finder-current-item ()
  (let ((key (save-excursion
	       (beginning-of-line)
	       (current-word))))
    (if (or (and finder-headmark (< (point) finder-headmark))
	    (zerop (length key)))
	(error "No keyword or filename on this line")
      key)))

(defun finder-select ()
  "Select item on current line in a finder buffer."
  (interactive)
  (let ((key (finder-current-item)))
      (if (string-match "\\.el$" key)
	  (finder-commentary key)
	(finder-list-matches key))))

(defun finder-mouse-select (event)
  "Select item in a finder buffer with the mouse."
  (interactive "e")
  (save-excursion
    (set-buffer (window-buffer (posn-window (event-start event))))
    (goto-char (posn-point (event-start event)))
    (finder-select)))

;;;###autoload
(defun finder-by-keyword ()
  "Find packages matching a given keyword."
  (interactive)
  (finder-list-keywords))

(define-derived-mode finder-mode nil "Finder"
  "Major mode for browsing package documentation.
\\<finder-mode-map>
\\[finder-select]	more help for the item on the current line
\\[finder-exit]	exit Finder mode and kill the Finder buffer."
  :syntax-table finder-mode-syntax-table
  (setq font-lock-defaults '(finder-font-lock-keywords nil nil
                             (("+-*/.<>=!?$%_&~^:@" . "w")) nil))
  (set (make-local-variable 'finder-headmark) nil))

(defun finder-summary ()
  "Summarize basic Finder commands."
  (interactive)
  (message "%s"
   (substitute-command-keys
    "\\<finder-mode-map>\\[finder-select] = select, \
\\[finder-mouse-select] = select, \\[finder-list-keywords] = to \
finder directory, \\[finder-exit] = quit, \\[finder-summary] = help")))

(defun finder-exit ()
  "Exit Finder mode.
Delete the window and kill all Finder-related buffers."
  (interactive)
  (ignore-errors (delete-window))
  (dolist (buff '("*Finder*" "*Finder-package*" "*Finder Category*"))
    (and (get-buffer buff) (kill-buffer buff))))


(provide 'finder)

;; arch-tag: ec85ff49-8cb8-41f5-a63f-9131d53ce2c5
;;; finder.el ends here
