;;; generic-mode.el --- A meta-mode which makes it easy to create small
;;   modes with basic comment and font-lock support
;;
;; Copyright (C) 1997 Free Software Foundation, Inc.
;;
;; Author:  Peter Breton
;; Created: Fri Sep 27 1996
;; Keywords: generic, comment, font-lock

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

;; Purpose:

;; Meta-mode to create simple major modes
;; with basic comment and font-lock support

;;; Commentary:

;; INTRODUCTION:

;; Generic-mode is a meta-mode which can be used to define small modes
;; which provide basic comment and font-lock support. These modes are
;; intended for the many configuration files and such which are too small
;; for a "real" mode, but still have a regular syntax, comment characters
;; and the like.
;;
;; Each generic mode can define the following:
;;
;; * List of comment-characters. The entries in this list should be
;;   either a character, a one or two character string or a cons pair.
;;   If the entry is a character or a one-character string
;;   LIMITATIONS:  Emacs does not support comment strings of more than
;;   two characters in length.
;;
;; * List of keywords to font-lock. Each keyword should be a string.
;;   If you have additional keywords which should be highlighted in a face
;;   different from 'font-lock-keyword-face', you can use the convenience
;;   function 'generic-make-keywords-list' (which see), and add the
;;   result to the following list:
;; 
;; * Additional expressions to font-lock. This should be a list of
;;   expressions, each of which should be of the same form
;;   as those in 'font-lock-defaults-alist'.
;;   
;; * List of regular expressions to be placed in auto-mode-alist.
;;
;; * List of functions to call to do some additional setup
;;
;; This should pretty much cover basic functionality; if you need much
;; more than this, or you find yourself writing extensive customizations,
;; perhaps you should be writing a major mode instead!
;;
;; LOCAL VARIABLES:
;;
;; To put a file into generic mode using local variables, use a line
;; like this in a Local Variables block:
;;
;;   mode: default-generic
;;
;; Do NOT use "mode: generic"!
;; See also "AUTOMATICALLY ENTERING GENERIC MODE" below.
;;   
;; DEFINING NEW GENERIC MODES:
;;
;; Use the 'define-generic-mode' function to define new modes.
;; For example:
;;
;;   (require 'generic-mode)
;;   (define-generic-mode 'foo-generic-mode
;;                        (list ?% )
;;                        (list "keyword")
;;                        nil
;;			  (list "\.FOO")
;;			  (list 'foo-setup-function))
;;
;; defines a new generic-mode 'foo-generic-mode', which has '%' as a
;; comment character, and "keyword" as a keyword. When files which end in
;; '.FOO' are loaded, Emacs will go into foo-generic-mode and call
;; foo-setup-function.  You can also use the function 'foo-generic-mode'
;; (which is interactive) to put a buffer into foo-generic-mode.
;;
;; AUTOMATICALLY ENTERING GENERIC MODE:
;;
;; Generic-mode provides a hook which automatically puts a
;; file into default-generic-mode if the first few lines of a file in
;; fundamental mode start with a hash comment character. To disable
;; this functionality, set the variable 'generic-use-find-file-hook'
;; to nil BEFORE loading generic-mode. See the variables
;; 'generic-lines-to-scan' and 'generic-find-file-regexp' for customization
;; options.
;; 
;; GOTCHAS:
;;
;; Be careful that your font-lock definitions are correct. Getting them
;; wrong can cause emacs to continually attempt to fontify! This problem
;; is not specific to generic-mode.
;; 

;; Credit for suggestions, brainstorming, patches and bug-fixes:
;;   ACorreir@pervasive-sw.com (Alfred Correira)

;;; Change log:
;; $Log: generic-mode.el,v $
;; Revision 1.6  1996/11/01 17:27:47  peter
;; Changed the function generic-function-name to return a string instead
;; of a symbol. Generic-mode now uses this for the mode's name
;;
;; Revision 1.5  1996/11/01 16:45:20  peter
;; Added GPL and LCD information.
;; Updated documentation
;; Added generic-find-file-regexp variable
;; Added generic-make-keywords-list function
;;
;; Revision 1.4  1996/10/19 12:16:59  peter
;; Small bug fixes: fontlock -> font-lock
;; New entries are added to the end of auto-mode-alist
;; Generic-font-lock-defaults are set to nil, not (list nil)
;; Comment-regexp in generic-mode-find-file-hook changed to allow optional
;; blank lines
;;
;; Revision 1.3  1996/10/17 08:24:25  peter
;; Added generic-mode-find-file-hook and associated variables
;;
;; Revision 1.2  1996/10/17 01:00:45  peter
;; Moved from a data-centered approach (generic-mode-alist) to
;; a function-based one (define-generic-mode)
;;
;; Revision 1.1  1996/10/10 11:37:36  peter
;; Initial revision
;;

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(make-variable-buffer-local
(defvar generic-font-lock-defaults nil
  "Global defaults for font-lock in a generic mode."))

(make-variable-buffer-local
(defvar generic-mode-name 'default-generic-mode
  "The name of the generic mode. 
This is the car of one of the items in `generic-mode-alist'. 
This variable is buffer-local."))

(make-variable-buffer-local
(defvar generic-comment-list nil
  "List of comment characters for a generic mode."))

(make-variable-buffer-local 
(defvar generic-keywords-list nil
  "List of keywords for a generic mode."))

(make-variable-buffer-local
(defvar generic-font-lock-expressions nil
  "List of font-lock expressions for a generic mode."))

(make-variable-buffer-local
(defvar generic-mode-function-list nil
  "List of customization functions to call for a generic mode."))

(make-variable-buffer-local
(defvar generic-mode-syntax-table nil
  "Syntax table for use in a generic mode."))

(defvar generic-mode-alist nil
  "An association list for generic-mode. 
Each entry in the list looks like this: 

 NAME COMMENT-LIST KEYWORD-LIST FONT-LOCK-LIST AUTO-MODE-LIST FUNCTION-LIST.

Do not add entries to this list directly; use `define-generic-mode' 
instead (which see).")

(defvar generic-use-find-file-hook t
  "*If non-nil, add a hook to enter default-generic-mode automatically
if the first few lines of a file in fundamental mode start with a hash 
comment character.")

(defvar generic-lines-to-scan 3
  "*Number of lines that `generic-mode-find-file-hook' looks at 
when deciding whether to enter generic-mode automatically. 
This variable should be set to a small positive number.")

(defvar generic-find-file-regexp "#.*\n\\(.*\n\\)?"
  "*Regular expression used by `generic-mode-find-file-hook'
to determine if files in fundamental mode should be put into
`default-generic-mode' instead.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inline functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsubst generic-read-type ()
  (completing-read
   "Generic Type: "
   (mapcar
    '(lambda (elt) (list (symbol-name (car elt))))
    generic-mode-alist) nil t))

;; Basic sanity checks. It does *not* check whether the elements of the lists
;; are of the correct type.
(defsubst generic-mode-sanity-check (name comment-list   keyword-list   
					  font-lock-list auto-mode-list  
					  function-list  &optional description)
  (if (not (symbolp name))
      (error "%s is not a symbol" (princ name)))

  (mapcar '(lambda (elt) 
	     (if (not (listp elt))
		 (error "%s is not a list" (princ elt))))
	  (list comment-list   keyword-list font-lock-list 
		auto-mode-list function-list))

  (if (not (or (null description) (stringp description)))
      (error "Description must be a string or nil"))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;### autoload
(defun define-generic-mode (name comment-list    keyword-list   font-lock-list 
				 auto-mode-list  function-list  
				 &optional description)
  "Create a new generic mode with NAME.
NAME should be a symbol; its string representation is used as the function 
name. If DESCRIPTION is provided, it is used as the docstring for the new 
function.

COMMENT-LIST is a list, whose entries are either a single character, 
a one or two character string or a cons pair. If the entry is a character 
or a one-character string, it is added to the mode's syntax table with
comment-start syntax. If the entry is a cons pair, the elements of the
pair are considered to be comment-start and comment-end respectively. 
Note that Emacs has limitations regarding comment characters.

KEYWORD-LIST is a list of keywords to highlight with `font-lock-keyword-face'.
Each keyword should be a string.

FONT-LOCK-LIST is a list of additional expressions to highlight. Each entry
in the list should have the same form as an entry in `font-lock-defaults-alist'

AUTO-MODE-LIST is a list of regular expressions to add to auto-mode-alist.
These regexps are added to auto-mode-alist as soon as `define-generic-mode' 
is called; any old regexps with the same name are removed. To modify the 
auto-mode-alist expressions, use `alter-generic-mode-auto-mode' (which see).

FUNCTION-LIST is a list of functions to call to do some additional setup.

See the file generic-extras.el for some examples of `define-generic-mode'."

  ;; Basic sanity check
  (generic-mode-sanity-check name 
			     comment-list    keyword-list   font-lock-list 
			     auto-mode-list  function-list  description)

  ;; Remove any old entry
  (setq generic-mode-alist
	(delq (assq name generic-mode-alist) 
	      generic-mode-alist))
  
  ;; Add a new entry
  (setq generic-mode-alist
	(append
	 (list
	  (list
	   name comment-list keyword-list font-lock-list 
	   auto-mode-list    function-list
	   ))
	 generic-mode-alist))

  ;; Add it to auto-mode-alist
  (generic-add-to-auto-mode name auto-mode-list t)
  
  ;; Define a function for it
  (generic-create-generic-function name description)
  )

(defun generic-add-to-auto-mode (mode auto-mode-list 
				      &optional remove-old prepend)
  "Add the entries for mode to `auto-mode-alist'. 
If remove-old is non-nil, removes old entries first. If prepend is
non-nil, prepends entries to auto-mode-alist; otherwise, appends them."

  (if (not (listp auto-mode-list))
      (error "%s is not a list" (princ auto-mode-list)))

  (let ((new-mode (intern (symbol-name mode))))
    (if remove-old
	(let ((auto-mode-entry))
	  (while (setq auto-mode-entry (rassq new-mode auto-mode-alist))
	    (setq auto-mode-alist
		  (delq auto-mode-entry
			auto-mode-alist)))))

    (mapcar '(lambda (entry) 
	       (generic-add-auto-mode-entry new-mode entry prepend))
	    auto-mode-list)))

(defun generic-add-auto-mode-entry (name entry &optional prepend)
  "Add a new entry to the end of auto-mode-alist.
If prepend is non-nil, add the entry to the front of the list."
  (let ((new-entry (list (cons entry name))))
    (setq auto-mode-alist
	  (if prepend
	      (append new-entry auto-mode-alist)
	    (append auto-mode-alist new-entry)))))
   
(defun generic-create-generic-function (name &optional description)
  "Create a generic mode function with NAME.
If DESCRIPTION is provided, it is used as the docstring."
  (let ((symname (symbol-name name)))
    (fset (intern symname)
	  (list 'lambda nil
		(or description 
		    (concat "Generic mode for type " symname))
		(list 'interactive)
		(list 'generic-mode-with-type (list 'quote name))))))

(defun generic-mode-with-type (&optional mode)
  "Go into the generic-mode MODE."
  (let* ((type (or mode generic-mode-name))
	 (generic-mode-list  (assoc type generic-mode-alist))
	 )

    (if (not generic-mode-list)
	(error "Can't find generic-mode information for type %s"
	       (princ generic-mode-name)))

    ;; Put this after the point where we read generic-mode-name!
    (kill-all-local-variables)

    (setq 
     generic-mode-name             type
     generic-comment-list          (nth 1 generic-mode-list)
     generic-keywords-list	   (nth 2 generic-mode-list)
     generic-font-lock-expressions (nth 3 generic-mode-list)
     generic-mode-function-list	   (nth 5 generic-mode-list)
     major-mode			   'generic-mode
     mode-name			   (symbol-name type)
     )

    (generic-mode-set-comments     generic-comment-list)

    ;; Font-lock functionality
    ;; Font-lock-defaults are always set even if there are no keywords
    ;; or font-lock expressions, so comments can be highlighted.
    (setq generic-font-lock-defaults nil)
    (generic-mode-set-font-lock      generic-keywords-list
				     generic-font-lock-expressions)
    (make-local-variable	    'font-lock-defaults)
    (setq font-lock-defaults (list 'generic-font-lock-defaults nil))

    ;; Call a list of functions
    (if generic-mode-function-list
	(mapcar 'funcall generic-mode-function-list))
    )
  )

;;;###autoload
(defun generic-mode (type)
  "A mode to do basic comment and font-lock functionality 
for files which are too small to warrant their own mode, but have
comment characters, keywords, and the like.

To define a generic-mode, use the function `define-generic-mode'.
To alter an existing generic-mode, use the `alter-generic-mode-'
convenience functions. 
Some generic modes are defined in generic-extras.el" 
  (interactive
   (list (generic-read-type)))
  (generic-mode-with-type (intern type)))

;;; Comment Functionality
(defun generic-mode-set-comments (comment-list)
  "Set up comment functionality for generic mode."
  (if (null comment-list)
      nil
    (let ((generic-mode-syntax-table (make-syntax-table)))
      (make-local-variable	     'comment-start)
      (make-local-variable	     'comment-start-skip)
      (make-local-variable	     'comment-end)
      (mapcar 'generic-mode-set-a-comment comment-list)
      (set-syntax-table    generic-mode-syntax-table))))

(defun generic-mode-set-a-comment (comment)
  (and (char-or-string-p comment)
       (if (stringp comment)
	   (cond 
	    ((eq (length comment) 1)
	     (generic-mode-set-comment-char 
	      (string-to-char comment)))
	    ((eq (length comment) 2)
	     (generic-mode-set-comment-string comment))
	    (t
	     (error "Character string %s must be one or two characters long"
		    comment))
	    )
	 (generic-mode-set-comment-char comment)))
  (if (consp comment)
      (generic-mode-set-comment-pair comment)))

(defun generic-mode-set-comment-char (comment-char)
  "Set the given character as a comment character for generic mode."
  (if (not comment-char)
      nil
    (setq 
     comment-end         ""
     comment-start       (char-to-string comment-char)
     comment-start-skip  (concat comment-start "+ *")
     )
      
    (modify-syntax-entry comment-char "<"
			 generic-mode-syntax-table)
    (modify-syntax-entry ?\n ">"
			 generic-mode-syntax-table)))

(defun generic-mode-set-comment-string (comment-string)
  "Set the given string as a comment string for generic mode."
  (if (not comment-string)
      nil
    (setq 
     comment-end         ""
     comment-start       comment-string
     comment-start-skip  (concat comment-start " *")
     )
      
    (let ((first  (elt comment-string 0))
	  (second (elt comment-string 1)))
      ;; C++ style comments
      (if (char-equal first second)
	  (progn
	    (modify-syntax-entry first "<12b"
				 generic-mode-syntax-table)
	    (modify-syntax-entry ?\n ">b"
				 generic-mode-syntax-table)))
      ;; Some other two character string
      (modify-syntax-entry first  "<1"
			   generic-mode-syntax-table)
      (modify-syntax-entry second "<2"
			   generic-mode-syntax-table)
      (modify-syntax-entry ?\n ">"
			   generic-mode-syntax-table))))

(defun generic-mode-set-comment-pair (comment-pair)
  "Set the given comment pair as a comment start and end for generic mode."
  (let ((generic-comment-start (car comment-pair))
	(generic-comment-end   (cdr comment-pair))
	)
    (setq 
     comment-end         generic-comment-end
     comment-start       generic-comment-start
     comment-start-skip  (concat generic-comment-start " *")
     )

    ;; Sanity checks
    (if (not (and (stringp generic-comment-start)
		  (stringp generic-comment-end)))
	(error "Elements of cons pair must be strings"))
    (if (not (and (equal (length generic-comment-start) 2)
		  (equal (length generic-comment-end) 2)))
	(error "Start and end must be exactly two characters long"))

    (let ((first   (elt generic-comment-start 0))
	  (second  (elt generic-comment-start 1))
	  (third   (elt generic-comment-end   0))
	  (fourth  (elt generic-comment-end   1))
	  )

      (modify-syntax-entry first   ". 1" generic-mode-syntax-table)
      (modify-syntax-entry second  ". 2" generic-mode-syntax-table)

      (modify-syntax-entry 
       third  
       (concat 
	"."
	(cond 
	 ((char-equal first   third) " 13")
	 ((char-equal second  third) " 23")
	 (t			     " 3"))
	)
       generic-mode-syntax-table)

      (modify-syntax-entry 
       fourth  
       (concat 
	"."
	(cond 
	 ((char-equal first   fourth) " 14")
	 ((char-equal second  fourth) " 24")
	 (t			      " 4"))
	)
       generic-mode-syntax-table)
      ))) 

(defun generic-mode-set-font-lock (keywords font-lock-expressions)
  "Set up font-lock functionality for generic mode."
  (let ((generic-font-lock-expressions))
    ;; Keywords
    (if keywords
	(setq
	 generic-font-lock-expressions
	 (append
	  (list
	   (list
	    (concat 
	     "\\(\\<"
	     (mapconcat 'identity keywords "\\>\\|\\<")
	     "\\>\\)") 
	    1 'font-lock-keyword-face))
	  generic-font-lock-expressions)))
    ;; Other font-lock expressions
    (if font-lock-expressions
	(setq generic-font-lock-expressions
	      (append
	       font-lock-expressions
	       generic-font-lock-expressions)))
    (if (not (or font-lock-expressions keywords))
	nil
      (setq generic-font-lock-defaults generic-font-lock-expressions))
    ))

;; Support for [KEYWORD] constructs found in INF, INI and Samba files
(defun generic-bracket-support ()
  (setq imenu-generic-expression 
	'((nil "^\\[\\(.*\\)\\]" 1))))

;; This generic mode is always defined
(define-generic-mode 'default-generic-mode (list ?#)  nil nil nil nil)

;; A more general solution would allow us to enter generic-mode for
;; *any* comment character, but would require us to synthesize a new
;; generic-mode on the fly. I think this gives us most of what we
;; want.
(defun generic-mode-find-file-hook ()
  "Hook to enter default-generic-mode automatically 
if the first few lines of a file in fundamental-mode start with a hash 
comment character. This hook will be installed if the variable 
`generic-use-find-file-hook' is non-nil. The variable `generic-lines-to-scan'
determines the number of lines to look at."
  (if (not (eq major-mode 'fundamental-mode))
      nil
    (if (or (> 1  generic-lines-to-scan)
	    (< 50 generic-lines-to-scan))
	(error "Variable `generic-lines-to-scan' should be set to a small"
	       " positive number"))
    (let ((comment-regexp "")
	  (count 0)
	  )
      (while (< count generic-lines-to-scan)
	(setq comment-regexp (concat comment-regexp 
				     generic-find-file-regexp))
	(setq count (1+ count)))
      (save-excursion
	(goto-char (point-min))
	(if (looking-at comment-regexp)
	    (generic-mode-with-type 'default-generic-mode))))))

(defun generic-mode-ini-file-find-file-hook ()
  "Hook to enter default-generic-mode automatically 
if the first few lines of a file in fundamental-mode look like an INI file.
This hook is NOT installed by default." 
  (if (not (eq major-mode 'fundamental-mode))
      nil
    (save-excursion
      (goto-char (point-min))
      (if (looking-at "^\\s-*\\[.*\\]")
	  (generic-mode-with-type 'ini-generic-mode)))))

(and generic-use-find-file-hook
    (add-hook 'find-file-hooks 'generic-mode-find-file-hook))

(defun generic-make-keywords-list (keywords-list face &optional prefix suffix)
  "Return a regular expression matching the specified keywords.
The regexp is highlighted with FACE."
  ;; Sanity checks
  ;; Don't check here; face may not be defined yet
  ;;   (if (not (facep face))
  ;;       (error "Face %s is not defined" (princ face)))
  (if (not (listp keywords-list))
      (error "Keywords argument must be a list of strings"))
  (list
   (concat 
    (or prefix "")
    "\\(\\<"
    (mapconcat 'identity keywords-list "\\>\\|\\<")
    "\\>\\)"
    (or suffix "")
    ) 1 face))

(provide 'generic-mode)

;;; generic-mode.el ends here
