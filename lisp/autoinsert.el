;;; autoinsert.el --- automatic mode-dependent insertion of text into new files
;; Copyright (C) 1985, 1986, 1987, 1994, 1995 Free Software Foundation, Inc.

;; Author: Charlie Martin <crm@cs.duke.edu>
;; Adapted-By: Daniel.Pfeiffer@Informatik.START.dbp.de, fax (+49 69) 7588-2389

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

;;  The following defines an association list for text to be
;;  automatically inserted when a new file is created, and a function
;;  which automatically inserts these files; the idea is to insert
;;  default text much as the mode is automatically set using
;;  auto-mode-alist.
;;
;;  To use: 
;;     (add-hook 'find-file-hooks 'auto-insert)
;;     setq auto-insert-directory to an appropriate slash-terminated value
;;
;;  Author:  Charlie Martin
;;           Department of Computer Science and
;;           National Biomedical Simulation Resource
;;           Box 3709
;;           Duke University Medical Center
;;           Durham, NC 27710
;;	      (crm@cs.duke.edu,mcnc!duke!crm) 

;;; Code:

(defvar auto-insert 'not-modified
  "*Controls automatic insertion into newly found empty files:
	nil	do nothing
	t	insert if possible
	other	insert if possible, but mark as unmodified.
Insertion is possible when something appropriate is found in
`auto-insert-alist'.  When the insertion is marked as unmodified, you can
save it with  \\[write-file] RET.
This variable is used when `auto-insert' is called as a function, e.g.
when you do (add-hook 'find-file-hooks 'auto-insert).
With \\[auto-insert], this is always treated as if it were `t'.")


(defvar auto-insert-query 'function
  "*If non-`nil', ask user before auto-inserting.
When this is `function', only ask when called non-interactively.")


(defvar auto-insert-prompt "Perform %s auto-insertion? "
  "*Prompt to use when querying whether to auto-insert.
If this contains a %s, that will be replaced by the matching rule.")


(defvar auto-insert-alist
  '((("\\.\\([Hh]\\|hh\\|hpp\\)\\'" . "C / C++ header")
     (upcase (concat (file-name-nondirectory
		      (substring buffer-file-name 0 (match-beginning 0)))
		     "_"
		     (substring buffer-file-name (1+ (match-beginning 0)))))
     "#ifndef " str \n
     "#define " str "\n\n"
     _ "\n\n#endif")

    (("\\.\\([Cc]\\|cc\\|cpp\\)\\'" . "C / C++ program")
     nil
     "#include \""
     ;; nop without latest cc-mode
     (and (fboundp 'c-companion-file)
	  ;(file-readable-p (c-companion-file 'name))
	  (file-name-nondirectory (c-companion-file 'name))) & ?\"
     | -10)

    ("[Mm]akefile\\'" . "makefile.inc")

    ("\\.html\\'"
     nil
     "<html>\n"
     "<head>\n"
     "<title>" _ "</title>\n"
     "</head>\n"
     "<body>\n\n"
     "</body>\n"
     "</html>")
    
    (plain-tex-mode . "tex-insert.tex")
    (bibtex-mode . "tex-insert.tex")
    (latex-mode
     ;; should try to offer completing read for these
     "options, RET: "
     "\\documentstyle[" str & ?\] | -1
     ?{ (read-string "class: ") "}\n"
     ("package, %s: "
      "\\usepackage[" (read-string "options, RET: ") & ?\] | -1 ?{ str "}\n")
     _ "\n\\begin{document}\n" _
     "\n\\end{document}")

    (("/bin/.*[^/]\\'" . "Shell-Script mode magic number")
     lambda ()
       (if (eq major-mode default-major-mode)
	 (sh-mode)))
    
    (ada-mode . ada-header)

    (("\\.el\\'" . "Emacs Lisp header")
     "Short description: "
     ";;; " (file-name-nondirectory (buffer-file-name)) " --- " str "

;; Copyright (C) " (substring (current-time-string) -4) " by "
 (getenv "ORGANIZATION") | "Free Software Foundation, Inc." "

;; Author: " (user-full-name)
'(if (search-backward "&" (save-excursion (beginning-of-line 1) (point)) t)
     (replace-match (capitalize (user-login-name)) t t))
'(end-of-line 1) " <" (user-login-name) ?@ (system-name) ">
;; Keywords: "
 '(require 'finder)
 ;;'(setq v1 (apply 'vector (mapcar 'car finder-known-keywords)))
 '(setq v1 (mapcar (lambda (x) (list (symbol-name (car x))))
		   finder-known-keywords)
	v2 (mapconcat (lambda (x) (format "%10.0s:  %s" (car x) (cdr x)))
	   finder-known-keywords
	   "\n"))
 ((let ((minibuffer-help-form v2))
    (completing-read "Keyword, C-h: " v1 nil t))
    str ", ") & -2 "

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

;; " _ "

;;; Code:



;;; " (file-name-nondirectory (buffer-file-name)) " ends here"))
  "A list specifying text to insert by default into a new file.
Elements look like (CONDITION . ACTION) or ((CONDITION . DESCRIPTION) . ACTION).
CONDITION maybe a regexp that must match the new file's name, or it may be
a symbol that must match the major mode for this element to apply.
Only the first matching element is effective.
Optional DESCRIPTION is a string for filling `auto-insert-prompt'.
ACTION may be a skeleton to insert (see `skeleton-insert'), an absolute
file-name or one relative to `auto-insert-directory' or a function to call.
ACTION may also be a vector containing several successive single actions as
described above, e.g. [\"header.insert\" date-and-author-update].")


;; Establish a default value for auto-insert-directory
(defvar auto-insert-directory "~/insert/"
  "*Directory from which auto-inserted files are taken.")


;;;###autoload
(defun auto-insert ()
  "Insert default contents into a new file if `auto-insert' is non-nil.
Matches the visited file name against the elements of `auto-insert-alist'."
  (interactive)
  (and (not buffer-read-only)
       (or (eq this-command 'auto-insert)
	   (and auto-insert
		(bobp) (eobp)))
       (let ((alist auto-insert-alist)
	     case-fold-search cond desc action)
	 (goto-char 1)
	 ;; find first matching alist entry
	 (while alist
	   (if (atom (setq cond (car (car alist))))
	       (setq desc cond)
	     (setq desc (cdr cond)
		   cond (car cond)))
	   (if (if (symbolp cond)
		   (eq cond major-mode)
		 (string-match cond buffer-file-name))
	       (setq action (cdr (car alist))
		     alist nil)
	     (setq alist (cdr alist))))

	 ;; Now, if we found something, do it
	 (and action
	      (if (stringp action)
		  (file-readable-p (concat auto-insert-directory action))
		t)
	      (if auto-insert-query
		  (or (if (eq auto-insert-query 'function)
			  (eq this-command 'auto-insert))
		      (y-or-n-p (format auto-insert-prompt desc)))
		t)
	      (mapcar
	       (lambda (action)
		 (if (stringp action)
		     (if (file-readable-p
			  (setq action (concat auto-insert-directory action)))
			 (insert-file-contents action))
		   (save-window-excursion
		     ;; make buffer visible before skeleton or function
		     ;; which might ask the user for something
		     (switch-to-buffer (current-buffer))
		     (if (and (consp action)
			      (not (eq (car action) 'lambda)))
			 (skeleton-insert action)
		       (funcall action)))))
	       (if (vectorp action)
		   action
		 (vector action))))
	 (and (buffer-modified-p)
	      (not (eq this-command 'auto-insert))
	      (set-buffer-modified-p (eq auto-insert t))))))


;;;###autoload
(defun define-auto-insert (key action &optional after)
  "Associate CONDITION with (additional) ACTION in `auto-insert-alist'.
Optional AFTER means to insert action after all existing actions for CONDITION,
or if CONDITION had no actions, after all other CONDITIONs."
  (let ((elt (assoc key auto-insert-alist)))
    (if elt
	(setcdr elt
		(if (vectorp (cdr elt))
		    (vconcat (if after (cdr elt))
			     (if (vectorp action) action (vector action))
			     (if after () (cdr elt)))
		  (if after
		      (vector (cdr elt) action)
		    (vector action (cdr elt)))))
      (if after
	  (nconc auto-insert-alist (list (cons key action)))
	(setq auto-insert-alist (cons (cons key action)
				      auto-insert-alist))))))

;;; autoinsert.el ends here
