;;; octave-hlp.el --- getting help on Octave symbols using info

;; Copyright (C) 1997 Free Software Foundation, Inc.

;; Author: Kurt Hornik <Kurt.Hornik@ci.tuwien.ac.at>
;; Author: John Eaton <jwe@bevo.che.wisc.edu>
;; Maintainer: Kurt Hornik <Kurt.Hornik@ci.tuwien.ac.at>
;; Keywords: languages

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

;; Provides the command `octave-help' which allows index lookup of a
;; symbol in the Octave-related info files, as specified by the list
;; `octave-help-files'.

;; Other features may be added in future versions.

;;; Code:

(require 'octave-mod)
(require 'info)

(defvar octave-help-files '("octave")
  "List of info files with documentation for Octave.
Default is (\"octave\").")

(defvar octave-help-lookup-alist nil
  "Alist of Octave index entries for lookup.")

(defvar octave-help-completion-alist nil
  "Alist of Octave index entries for completion.
The entries are of the form (VAR . VAR), where VAR runs through all
different keys in `octave-help-lookup-alist'.")

;;;###autoload
(defun octave-help (key)
  "Get help on Octave symbols from the Octave info files.
Look up KEY in the function, operator and variable indices of the files
specified by `octave-help-files'.
If KEY is not a string, prompt for it with completion."
  (interactive
   (list
    (completing-read (format "Describe Octave symbol: ")
		     (octave-help-get-completion-alist)
		     nil t)))
  (if (get-buffer "*info*")
      (set-buffer "*info*"))
  (if (zerop (length key))
      (Info-find-node (car octave-help-files) "Top")
    (let ((alist (copy-alist (octave-help-get-lookup-alist)))
	  entry matches)
      (while (setq entry (car alist))
	(if (string-match key (car entry))
	    (add-to-list 'matches entry))
	(setq alist (cdr alist)))
      (if matches
	  (progn
	    (setq Info-index-alternatives matches)
	    (Info-index-next 0))))))

(defun octave-help-get-lookup-alist ()
  "Build the index lookup alist from all Octave info files.
The files specified by `octave-help-files' are searched."
  (if octave-help-lookup-alist
      ()
    (message "Building help lookup alist...")
    (let ((files octave-help-files) file key node)
      (save-window-excursion
	(while files
	  (setq file (car files))
 	  (Info-goto-node (concat "(" file ")"))
	  (condition-case nil
	      (progn
		(Info-index "")
		(while
		    (progn
		      (while (re-search-forward
			      "^\\* \\([^(:]+\\)[^:]*: *\\(.+\\)\\.$"
			      nil t)
			(setq key (match-string 1)
			      node (concat "(" file ")" (match-string 2)))
			(and (string-match "\\(.*\\>\\) *$" key)
			     (setq key (replace-match "\\1" t nil key)))
			(add-to-list 'octave-help-lookup-alist
				     (list key
					   node
					   (concat (concat "(" file ")")
						   Info-current-node)
					   0)))
		      (and (setq node (Info-extract-pointer "next" t))
			   (string-match
			    (concat "\\(Function\\|Operator\\|Variable\\) "
				    "\\<Index\\>")
			    node)))
		  (Info-goto-node node)))
	    (error nil))
	  (setq files (cdr files)))))
    (message "Building help lookup alist...done"))
  octave-help-lookup-alist)

(defun octave-help-get-completion-alist ()
  "Build the index completion alist from all Octave info files.
The files specified by `octave-help-files' are searched."
  (if octave-help-completion-alist
      ()
    (message "Building help completion alist...")
    (let ((alist (octave-help-get-lookup-alist)) entry)
      (while alist
	(setq entry (car alist))
	(add-to-list 'octave-help-completion-alist
		     (cons (car entry) (car entry)))
	(setq alist (cdr alist))))
    (message "Building help completion alist...done"))
  octave-help-completion-alist)

;;; provide ourself

(provide 'octave-hlp)

;;; arch-tag: df5ef8fa-76c9-44e5-9835-cb5a502c6282
;;; octave-hlp.el ends here
