;;; sgml-mode.el --- SGML-editing mode

;; Copyright (C) 1992 Free Software Foundation, Inc.

;; Author: James Clark <jjc@clark.com>
;; Adapted-By: ESR
;; Keywords: wp

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Major mode for editing the SGML document-markup language.

;;; Code:

(provide 'sgml-mode)
(require 'compile)

;;; sgmls is a free SGML parser available from
;;; ftp.uu.net:pub/text-processing/sgml
;;; Its error messages can be parsed by next-error.
;;; The -s option suppresses output.

(defconst sgml-validate-command
  "sgmls -s"
  "*The command to validate an SGML document.
The file name of current buffer file name will be appended to this,
separated by a space.")

(defvar sgml-saved-validate-command nil
  "The command last used to validate in this buffer.")

(defvar sgml-mode-map nil "Keymap for SGML mode")

(if sgml-mode-map
    ()
  (setq sgml-mode-map (make-sparse-keymap))
  (define-key sgml-mode-map ">" 'sgml-close-angle)
  (define-key sgml-mode-map "/" 'sgml-slash)
  (define-key sgml-mode-map "\C-c\C-v" 'sgml-validate))

;;;###autoload
(defun sgml-mode ()
  "Major mode for editing SGML.
Makes > display the matching <.  Makes / display matching /.
Use \\[sgml-validate] to validate your document with an SGML parser."
  (interactive)
  (kill-all-local-variables)
  (setq local-abbrev-table text-mode-abbrev-table)
  (use-local-map sgml-mode-map)
  (setq mode-name "SGML")
  (setq major-mode 'sgml-mode)
  (make-local-variable 'paragraph-start)
  ;; A start or end tag by itself on a line separates a paragraph.
  ;; This is desirable because SGML discards a newline that appears
  ;; immediately after a start tag or immediately before an end tag.
  (setq paragraph-start
	"^[ \t\n]\\|\
\\(</?\\([A-Za-z]\\([-.A-Za-z0-9= \t\n]\\|\"[^\"]*\"\\|'[^']*'\\)*\\)?>$\\)")
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate
	"^[ \t\n]*$\\|\
^</?\\([A-Za-z]\\([-.A-Za-z0-9= \t\n]\\|\"[^\"]*\"\\|'[^']*'\\)*\\)?>$")
  (make-local-variable 'sgml-saved-validate-command)
  (set-syntax-table text-mode-syntax-table)
  (make-local-variable 'comment-start)
  (setq comment-start "<!-- ")
  (make-local-variable 'comment-end)
  (setq comment-end " -->")
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'sgml-comment-indent)
  (make-local-variable 'comment-start-skip)
  ;; This will allow existing comments within declarations to be
  ;; recognized.
  (setq comment-start-skip "--[ \t]*")
  (run-hooks 'text-mode-hook 'sgml-mode-hook))

(defun sgml-comment-indent ()
  (if (and (looking-at "--")
	   (not (and (eq (char-after (1- (point))) ?!)
		     (eq (char-after (- (point) 2)) ?<))))
      (progn
	(skip-chars-backward " \t")
	(max comment-column (1+ (current-column))))
    0))

(defconst sgml-start-tag-regex
  "<[A-Za-z]\\([-.A-Za-z0-9= \n\t]\\|\"[^\"]*\"\\|'[^']*'\\)*"
  "Regular expression that matches a non-empty start tag.
Any terminating > or / is not matched.")

(defvar sgml-mode-markup-syntax-table nil
  "Syntax table used for scanning SGML markup.")

(if sgml-mode-markup-syntax-table
    ()
  (setq sgml-mode-markup-syntax-table (make-syntax-table))
  (modify-syntax-entry ?< "(>" sgml-mode-markup-syntax-table)
  (modify-syntax-entry ?> ")<" sgml-mode-markup-syntax-table)
  (modify-syntax-entry ?- "_ 1234" sgml-mode-markup-syntax-table)
  (modify-syntax-entry ?\' "\"" sgml-mode-markup-syntax-table))

(defconst sgml-angle-distance 4000
  "*If non-nil, is the maximum distance to search for matching <.")

(defun sgml-close-angle (arg)
  "Insert > and display matching <."
  (interactive "p")
  (insert-char ?> arg)
  (if (> arg 0)
      (let ((oldpos (point))
	    (blinkpos))
	(save-excursion
	  (save-restriction
	    (if sgml-angle-distance
		(narrow-to-region (max (point-min)
				       (- (point) sgml-angle-distance))
				  oldpos))
	    ;; See if it's the end of a marked section.
	    (and (> (- (point) (point-min)) 3)
		 (eq (char-after (- (point) 2)) ?\])
		 (eq (char-after (- (point) 3)) ?\])
		 (re-search-backward "<!\\[\\(-?[A-Za-z0-9. \t\n&;]\\|\
--\\([^-]\\|-[^-]\\)*--\\)*\\["
				     (point-min)
				     t)
		 (let ((msspos (point)))
		   (if (and (search-forward "]]>" oldpos t)
			    (eq (point) oldpos))
		       (setq blinkpos msspos))))
	    ;; This handles cases where the > ends one of the following:
	    ;; markup declaration starting with <! (possibly including a
	    ;; declaration subset); start tag; end tag; SGML declaration.
	    (if blinkpos
		()
	      (goto-char oldpos)
	      (condition-case ()
		  (let ((oldtable (syntax-table))
			(parse-sexp-ignore-comments t))
		    (unwind-protect
			(progn
			  (set-syntax-table sgml-mode-markup-syntax-table)
			  (setq blinkpos (scan-sexps oldpos -1)))
		      (set-syntax-table oldtable)))
		(error nil))
	      (and blinkpos
		   (goto-char blinkpos)
		   (or
		    ;; Check that it's a valid delimiter in context.
		    (not (looking-at
			  "<\\(\\?\\|/?[A-Za-z>]\\|!\\([[A-Za-z]\\|--\\)\\)"))
		    ;; Check that it's not a net-enabling start tag
		    ;; nor an unclosed start-tag.
		    (looking-at (concat sgml-start-tag-regex "[/<]"))
		    ;; Nor an unclosed end-tag.
		    (looking-at "</[A-Za-z][-.A-Za-z0-9]*[ \t]*<"))
		   (setq blinkpos nil)))
	    (if blinkpos
		()
	      ;; See if it's the end of a processing instruction.
	      (goto-char oldpos)
	      (if (search-backward "<?" (point-min) t)
		  (let ((pipos (point)))
		    (if (and (search-forward ">" oldpos t)
			     (eq (point) oldpos))
			(setq blinkpos pipos))))))
	  (if blinkpos
	      (progn
		(goto-char blinkpos)
		(if (pos-visible-in-window-p)
		    (sit-for 1)
		  (message "Matches %s"
			   (buffer-substring blinkpos
					     (progn (end-of-line)
						    (point)))))))))))

;;; I doubt that null end tags are used much for large elements,
;;; so use a small distance here.
(defconst sgml-slash-distance 1000
  "*If non-nil, is the maximum distance to search for matching /.")

(defun sgml-slash (arg)
  "Insert / and display any previous matching /.
Two /s are treated as matching if the first / ends a net-enabling
start tag, and the second / is the corresponding null end tag."
  (interactive "p")
  (insert-char ?/ arg)
  (if (> arg 0)
      (let ((oldpos (point))
	    (blinkpos)
	    (level 0))
	(save-excursion
	  (save-restriction
	    (if sgml-slash-distance
		(narrow-to-region (max (point-min)
				       (- (point) sgml-slash-distance))
				  oldpos))
	    (if (and (re-search-backward sgml-start-tag-regex (point-min) t)
		     (eq (match-end 0) (1- oldpos)))
		()
	      (goto-char (1- oldpos))
	      (while (and (not blinkpos)
			  (search-backward "/" (point-min) t))
		(let ((tagend (save-excursion
				(if (re-search-backward sgml-start-tag-regex
							(point-min) t)
				    (match-end 0)
				  nil))))
		  (if (eq tagend (point))
		      (if (eq level 0)
			  (setq blinkpos (point))
			(setq level (1- level)))
		    (setq level (1+ level)))))))
	  (if blinkpos
	      (progn
		(goto-char blinkpos)
		(if (pos-visible-in-window-p)
		    (sit-for 1)
		  (message "Matches %s"
			   (buffer-substring (progn
					       (beginning-of-line)
					       (point))
					     (1+ blinkpos))))))))))

(defun sgml-validate (command)
  "Validate an SGML document.
Runs COMMAND, a shell command, in a separate process asynchronously
with output going to the buffer *compilation*.
You can then use the command \\[next-error] to find the next error message
and move to the line in the SGML document that caused it."
  (interactive
   (list (read-string "Validate command: "
		      (or sgml-saved-validate-command
			  (concat sgml-validate-command
				  " "
				  (let ((name (buffer-file-name)))
				    (and name
					 (file-name-nondirectory name))))))))
  (setq sgml-saved-validate-command command)
  (compile-internal command "No more errors"))

;;; sgml-mode.el ends here
