;;; generic.el --- defining simple major modes with comment and font-lock
;;
;; Copyright (C) 1997, 1999 Free Software Foundation, Inc.
;;
;; Author:  Peter Breton <pbreton@cs.umb.edu>
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
;; which provide basic comment and font-lock support.  These modes are
;; intended for the many configuration files and such which are too small
;; for a "real" mode, but still have a regular syntax, comment characters
;; and the like.
;;
;; Each generic mode can define the following:
;;
;; * List of comment-characters.  The entries in this list should be
;;   either a character, a one or two character string or a cons pair.
;;   If the entry is a character or a one-character string
;;   LIMITATIONS:  Emacs does not support comment strings of more than
;;   two characters in length.
;;
;; * List of keywords to font-lock.  Each keyword should be a string.
;;   If you have additional keywords which should be highlighted in a face
;;   different from `font-lock-keyword-face', you can use the convenience
;;   function `generic-make-keywords-list' (which see), and add the
;;   result to the following list:
;;
;; * Additional expressions to font-lock.  This should be a list of
;;   expressions, each of which should be of the same form
;;   as those in `font-lock-keywords'.
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
;; Use the `define-generic-mode' function to define new modes.
;; For example:
;;
;;   (require 'generic)
;;   (define-generic-mode 'foo-generic-mode
;;                        (list ?% )
;;                        (list "keyword")
;;                        nil
;;			  (list "\\.FOO\\'")
;;			  (list 'foo-setup-function))
;;
;; defines a new generic-mode `foo-generic-mode', which has '%' as a
;; comment character, and "keyword" as a keyword. When files which end in
;; '.FOO' are loaded, Emacs will go into foo-generic-mode and call
;; foo-setup-function.  You can also use the function `foo-generic-mode'
;; (which is interactive) to put a buffer into foo-generic-mode.
;;
;; AUTOMATICALLY ENTERING GENERIC MODE:
;;
;; Generic-mode provides a hook which automatically puts a
;; file into default-generic-mode if the first few lines of a file in
;; fundamental mode start with a hash comment character. To disable
;; this functionality, set the variable `generic-use-find-file-hook'
;; to nil BEFORE loading generic-mode. See the variables
;; `generic-lines-to-scan' and `generic-find-file-regexp' for customization
;; options.
;;
;; GOTCHAS:
;;
;; Be careful that your font-lock definitions are correct.  Getting them
;; wrong can cause emacs to continually attempt to fontify! This problem
;; is not specific to generic-mode.
;;

;; Credit for suggestions, brainstorming, help with debugging:
;;   ACorreir@pervasive-sw.com (Alfred Correira)
;; Extensive cleanup by:
;;   Stefan Monnier (monnier+gnu/emacs@flint.cs.yale.edu)
;;
;;; Code:

(eval-when-compile
  (require 'cl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar generic-font-lock-defaults nil
  "Global defaults for font-lock in a generic mode.")
(make-variable-buffer-local 'generic-font-lock-defaults)

(defvar generic-mode-list nil
  "A list of mode names for `generic-mode'.
Do not add entries to this list directly; use `define-generic-mode'
instead (which see).")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup generic nil
  "Define simple major modes with comment and font-lock support."
  :prefix "generic-"
  :group 'extensions)

(defcustom generic-use-find-file-hook t
  "*If non-nil, add a hook to enter default-generic-mode automatically.
This is done if the first few lines of a file in fundamental mode start
with a hash comment character."
  :group 'generic
  :type  'boolean
  )

(defcustom generic-lines-to-scan 3
  "*Number of lines that `generic-mode-find-file-hook' looks at.
Relevant when deciding whether to enter `generic-mode' automatically.
This variable should be set to a small positive number."
  :group 'generic
  :type  'integer
  )

(defcustom generic-find-file-regexp "^#"
  "*Regular expression used by `generic-mode-find-file-hook'.
Files in fundamental mode whose first few lines contain a match for
this regexp, should be put into `default-generic-mode' instead.
The number of lines tested for the matches is specified by the value
of the variable `generic-lines-to-scan', which see."
  :group 'generic
  :type  'regexp
  )

(defcustom generic-ignore-files-regexp "[Tt][Aa][Gg][Ss]\\'"
  "*Regular expression used by `generic-mode-find-file-hook'.
Files whose names match this regular expression should not be put
into `default-generic-mode', even if they have lines which match the
regexp in `generic-find-file-regexp'.  If the value is nil,
`generic-mode-find-file-hook' does not check the file names."
  :group 'generic
  :type  '(choice (const :tag "Don't check file names" nil) regexp)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun define-generic-mode (name comment-list    keyword-list   font-lock-list
				 auto-mode-list  function-list
				 &optional description)
  "Create a new generic mode with NAME.

Args: (NAME COMMENT-LIST KEYWORD-LIST FONT-LOCK-LIST AUTO-MODE-LIST
            FUNCTION-LIST &optional DESCRIPTION)

NAME should be a symbol; its string representation is used as the function
name. If DESCRIPTION is provided, it is used as the docstring for the new
function.

COMMENT-LIST is a list, whose entries are either a single character,
a one or two character string or a cons pair. If the entry is a character
or a one-character string, it is added to the mode's syntax table with
`comment-start' syntax.  If the entry is a cons pair, the elements of the
pair are considered to be `comment-start' and `comment-end' respectively.
Note that Emacs has limitations regarding comment characters.

KEYWORD-LIST is a list of keywords to highlight with `font-lock-keyword-face'.
Each keyword should be a string.

FONT-LOCK-LIST is a list of additional expressions to highlight. Each entry
in the list should have the same form as an entry in `font-lock-keywords'.

AUTO-MODE-LIST is a list of regular expressions to add to `auto-mode-alist'.
These regexps are added to `auto-mode-alist' as soon as `define-generic-mode'
is called; any old regexps with the same name are removed.

FUNCTION-LIST is a list of functions to call to do some additional setup.

See the file generic-x.el for some examples of `define-generic-mode'."

  ;; Add a new entry
  (unless (assq name generic-mode-list)
    (push (list (symbol-name name)) generic-mode-list))

  ;; Add it to auto-mode-alist
  (dolist (re auto-mode-list)
    (add-to-list 'auto-mode-alist (cons re name)))

  ;; Define a function for it using `defalias' (not `fset') to make
  ;; the mode appear on load-history.
  (defalias name
    `(lambda nil
       ,(or description (concat "Generic mode for type " (symbol-name name)))
       (interactive)
       (generic-mode-internal ',name ',comment-list ',keyword-list
			      ',font-lock-list ',function-list)))
  )

(defun generic-mode-internal (mode comments keywords font-lock-list funs)
  "Go into the generic-mode MODE."
  (let* ((generic-mode-hooks (intern (concat (symbol-name mode) "-hook")))
	 (modename (symbol-name mode))
	 (name (if (string-match "-mode\\'" modename)
		   (substring modename 0 (match-beginning 0))
		 modename))
	 )

    ;; Put this after the point where we read generic-mode-name!
    (kill-all-local-variables)

    (setq
     major-mode			   mode
     mode-name			   (capitalize name)
     )

    (generic-mode-set-comments     comments)

    ;; Font-lock functionality
    ;; Font-lock-defaults are always set even if there are no keywords
    ;; or font-lock expressions, so comments can be highlighted.
    (setq generic-font-lock-defaults nil)
    (generic-mode-set-font-lock      keywords font-lock-list)
    (make-local-variable	    'font-lock-defaults)
    (setq font-lock-defaults (list 'generic-font-lock-defaults nil))

    ;; Call a list of functions
    (mapcar 'funcall funs)

    (run-hooks generic-mode-hooks)
    )
  )

;;;###autoload
(defun generic-mode (type)
  "Basic comment and font-lock functionality for `generic' files.
\(Files which are too small to warrant their own mode, but have
comment characters, keywords, and the like.)

To define a generic-mode, use the function `define-generic-mode'.
Some generic modes are defined in `generic-x.el'."
  (interactive
   (list (completing-read "Generic Type: " generic-mode-list nil t)))
  (funcall (intern type)))

;;; Comment Functionality
(defun generic-mode-set-comments (comment-list)
  "Set up comment functionality for generic mode."
  (let ((st (make-syntax-table))
	(chars nil)
	(comstyles))
    (make-local-variable	     'comment-start)
    (make-local-variable	     'comment-start-skip)
    (make-local-variable	     'comment-end)

    ;; Go through all the comments
    (dolist (start comment-list)
      (let ((end ?\n) (comstyle ""))
	;; Normalize
	(when (consp start)
	  (setq end (or (cdr start) end))
	  (setq start (car start)))
	(when (char-valid-p start) (setq start (char-to-string start)))
	(when (char-valid-p end)   (setq end (char-to-string end)))

	;; Setup the vars for `comment-region'
	(if comment-start
	    ;; We have already setup a comment-style, so use style b
	    (progn
	      (setq comstyle "b")
	      (setq comment-start-skip
		    (concat comment-start-skip "\\|" (regexp-quote start) "+\\s-*")))
	  ;; First comment-style
	  (setq comment-start start)
	  (setq comment-end (if (string-equal end "\n") "" end))
	  (setq comment-start-skip (concat (regexp-quote start) "+\\s-*")))

       ;; Reuse comstyles if necessary
       (setq comstyle
             (or (cdr (assoc start comstyles))
                 (cdr (assoc end comstyles))
                 comstyle))
       (push (cons start comstyle) comstyles)
       (push (cons end comstyle) comstyles)

	;; Setup the syntax table
	(if (= (length start) 1)
	    (modify-syntax-entry (string-to-char start)
				 (concat "< " comstyle) st)
	  (let ((c0 (elt start 0)) (c1 (elt start 1)))
	    ;; Store the relevant info but don't update yet
	    (push (cons c0 (concat (cdr (assoc c0 chars)) "1")) chars)
	    (push (cons c1 (concat (cdr (assoc c1 chars))
				   (concat "2" comstyle))) chars)))
	(if (= (length end) 1)
	    (modify-syntax-entry (string-to-char end)
				 (concat ">" comstyle) st)
	  (let ((c0 (elt end 0)) (c1 (elt end 1)))
	    ;; Store the relevant info but don't update yet
	    (push (cons c0 (concat (cdr (assoc c0 chars))
				   (concat "3" comstyle))) chars)
	    (push (cons c1 (concat (cdr (assoc c1 chars)) "4")) chars)))))

    ;; Process the chars that were part of a 2-char comment marker
    (dolist (cs (nreverse chars))
      (modify-syntax-entry (car cs)
			   (concat (char-to-string (char-syntax (car cs)))
				   " " (cdr cs))
			   st))
    (set-syntax-table st)))

(defun generic-mode-set-font-lock (keywords font-lock-expressions)
  "Set up font-lock functionality for generic mode."
  (setq generic-font-lock-defaults
	(append
	 (when keywords
	   (list (generic-make-keywords-list keywords font-lock-keyword-face)))
	 font-lock-expressions)))

;; Support for [KEYWORD] constructs found in INF, INI and Samba files
(defun generic-bracket-support ()
  (setq imenu-generic-expression
	'((nil "^\\[\\(.*\\)\\]" 1))
        imenu-case-fold-search t))

;; This generic mode is always defined
(define-generic-mode 'default-generic-mode (list ?#)  nil nil nil nil)

;; A more general solution would allow us to enter generic-mode for
;; *any* comment character, but would require us to synthesize a new
;; generic-mode on the fly. I think this gives us most of what we
;; want.
(defun generic-mode-find-file-hook ()
  "Hook function to enter `default-generic-mode' automatically.
Done if the first few lines of a file in `fundamental-mode' start with
a match for the regexp in `generic-find-file-regexp', unless the
file's name matches the regexp which is the value of the variable
`generic-ignore-files-regexp'.
This hook will be installed if the variable
`generic-use-find-file-hook' is non-nil.  The variable
`generic-lines-to-scan' determines the number of lines to look at."
  (when (and (eq major-mode 'fundamental-mode)
	     (or (null generic-ignore-files-regexp)
		 (not (string-match
		       generic-ignore-files-regexp
		       (file-name-sans-versions buffer-file-name)))))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward generic-find-file-regexp
			       (save-excursion
				 (forward-line generic-lines-to-scan)
				 (point)) t)
	(goto-char (point-min))
	(default-generic-mode)))))

(defun generic-mode-ini-file-find-file-hook ()
  "Hook function to enter default-generic-mode automatically for INI files.
Done if the first few lines of a file in `fundamental-mode' look like an
INI file.  This hook is NOT installed by default."
  (and (eq major-mode 'fundamental-mode)
       (save-excursion
	 (goto-char (point-min))
	 (and (looking-at "^\\s-*\\[.*\\]")
	      (ini-generic-mode)))))

(and generic-use-find-file-hook
    (add-hook 'find-file-hooks 'generic-mode-find-file-hook))

(defun generic-make-keywords-list (keywords-list face &optional prefix suffix)
  "Return a regular expression matching the specified KEYWORDS-LIST.
The regexp is highlighted with FACE."
  (unless (listp keywords-list)
    (error "Keywords argument must be a list of strings"))
  (list (concat prefix "\\<"
		;; Use an optimized regexp.
		(regexp-opt keywords-list t)
		"\\>" suffix)
	1
	face))

(provide 'generic)

;;; arch-tag: 239c1fc4-1303-48d9-9ac0-657d655669ea
;;; generic.el ends here
