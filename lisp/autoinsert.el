;;; autoinsert.el --- automatic mode-dependent insertion of text into new files

;; Copyright (C) 1985, 86, 87, 94, 95, 98 Free Software Foundation, Inc.

;; Author: Charlie Martin <crm@cs.duke.edu>
;; Adapted-By: Daniel Pfeiffer <occitan@esperanto.org>
;; Keywords: convenience
;; Maintainer: FSF

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
;;  You can also customize the variable `auto-insert-mode' to load the
;;  package.  Alternatively, add the following to your .emacs file:
;;  (auto-insert-mode 1)
;;
;;  Author:  Charlie Martin
;;           Department of Computer Science and
;;           National Biomedical Simulation Resource
;;           Box 3709
;;           Duke University Medical Center
;;           Durham, NC 27710
;;	      (crm@cs.duke.edu,mcnc!duke!crm)

;;; Code:

(defgroup auto-insert nil
  "Automatic mode-dependent insertion of text into new files."
  :prefix "auto-insert-"
  :group 'files
  :group 'convenience)


(defcustom auto-insert-mode nil
  "Toggle Auto-insert mode.
Setting this variable directly does not take effect;
use either \\[customize] or the function `auto-insert-mode'."
  :set (lambda (symbol value)
	 (auto-insert-mode (or value 0)))
  :initialize 'custom-initialize-default
  :type 'boolean
  :group 'auto-insert
  :require 'autoinsert)

(defcustom auto-insert 'not-modified
  "*Controls automatic insertion into newly found empty files.
Possible values:
	nil	do nothing
	t	insert if possible
	other	insert if possible, but mark as unmodified.
Insertion is possible when something appropriate is found in
`auto-insert-alist'.  When the insertion is marked as unmodified, you can
save it with  \\[write-file] RET.
This variable is used when `auto-insert' is called as a function, e.g.
when you do (add-hook 'find-file-hooks 'auto-insert).
With \\[auto-insert], this is always treated as if it were t."
  :type '(choice (const :tag "Insert if possible" t)
                 (const :tag "Do nothing" nil)
                 (other :tag "insert if possible, mark as unmodified."
                        not-modified))
  :group 'auto-insert)

(defcustom auto-insert-query 'function
  "*Non-nil means ask user before auto-inserting.
When this is `function', only ask when called non-interactively."
  :type '(choice (const :tag "Don't ask" nil)
                 (const :tag "Ask if called non-interactively" function)
                 (other :tag "Ask" t))
  :group 'auto-insert)

(defcustom auto-insert-prompt "Perform %s auto-insertion? "
  "*Prompt to use when querying whether to auto-insert.
If this contains a %s, that will be replaced by the matching rule."
  :type 'string
  :group 'auto-insert)


(defcustom auto-insert-alist
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

    (html-mode . (lambda () (sgml-tag "html")))
    
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
'(end-of-line 1) " <" (progn user-mail-address) ">
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

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

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
described above, e.g. [\"header.insert\" date-and-author-update]."
  :type 'sexp
  :group 'auto-insert)


;; Establish a default value for auto-insert-directory
(defcustom auto-insert-directory "~/insert/"
  "*Directory from which auto-inserted files are taken."
  :type 'directory
  :group 'auto-insert)


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
	      (set-buffer-modified-p (eq auto-insert t)))))
  ;; Return nil so that it could be used in
  ;; `find-file-not-found-hooks', though that's probably inadvisable.
  nil)


;;;###autoload
(defun define-auto-insert (condition action &optional after)
  "Associate CONDITION with (additional) ACTION in `auto-insert-alist'.
Optional AFTER means to insert action after all existing actions for CONDITION,
or if CONDITION had no actions, after all other CONDITIONs."
  (let ((elt (assoc condition auto-insert-alist)))
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
	  (nconc auto-insert-alist (list (cons condition action)))
	(setq auto-insert-alist (cons (cons condition action)
				      auto-insert-alist))))))

;;;###autoload
(defun auto-insert-mode (&optional arg)
  "Toggle Auto-insert mode.
With prefix ARG, turn Auto-insert mode on if and only if ARG is positive.
Returns the new status of Auto-insert mode (non-nil means on).

When Auto-insert mode is enabled, when new files are created you can
insert a template for the file depending on the mode of the buffer."
  (interactive "P")
  (let ((on-p (if arg
		  (> (prefix-numeric-value arg) 0)
		(not auto-insert-mode))))
    (if on-p
	(add-hook 'find-file-hooks 'auto-insert)
      (remove-hook 'find-file-hooks 'auto-insert))
    (if (interactive-p)
	(message "Auto-insert now %s." (if on-p "on" "off")))
    (setq auto-insert-mode on-p)))

(if auto-insert-mode
    (auto-insert-mode 1))

(provide 'autoinsert)

;;; autoinsert.el ends here
