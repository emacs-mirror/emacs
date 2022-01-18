;;; em-cmpl.el --- completion using the TAB key  -*- lexical-binding:t -*-

;; Copyright (C) 1999-2022 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Eshell, by using the pcomplete package, provides a full
;; programmable completion facility that is comparable to shells like
;; tcsh or zsh.
;;
;; Completions are context-sensitive, which means that pressing <TAB>
;; after the command 'rmdir' will result in a list of directories,
;; while doing so after 'rm' will result in a list of all file
;; entries.
;;
;; Many builtin completion rules are provided, for commands such as
;; `cvs', or RedHat's `rpm' utility.  Adding new completion rules is
;; no more difficult than writing a plain Lisp functions, and they can
;; be debugged, profiled, and compiled using exactly the same
;; facilities (since in fact, they *are* just Lisp functions).  See
;; the definition of the function `pcomplete/make' for an example of
;; how to write a completion function.
;;
;; The completion facility is very easy to use.  Just press TAB.  If
;; there are a large number of possible completions, a buffer will
;; appear showing a list of them.  Completions may be selected from
;; that buffer using the mouse.  If no completion is selected, and the
;; user starts doing something else, the display buffer will
;; automatically disappear.
;;
;; If the list of possible completions is very small, Eshell will
;; "cycle" through them, selecting a different entry each time <TAB>
;; is pressed.  <S-TAB> may be used to cycle in the opposite
;; direction.
;;
;; Glob patterns can also be cycled.  For example, entering 'echo
;; x*<tab>' will cycle through all the filenames beginning with 'x'.
;; This is done because the glob list is treated as though it were a
;; list of possible completions.  Pressing <C-c SPC> will insert all
;; of the matching glob patterns at point.
;;
;; If a Lisp form is being entered, <TAB> will complete the Lisp
;; symbol name, in exactly the same way that <M-TAB> does in Emacs
;; Lisp mode.
;;
;; The list of possible completions can be viewed at any point by
;; pressing <M-?>.
;;
;; Finally, context-related help can be accessed by pressing <C-c M-h>.
;; This only works well if the completion function has provided Eshell
;; with sufficient pointers to locate the relevant help text.

;;; Code:
(require 'pcomplete)

(require 'esh-mode)
(require 'esh-util)
(require 'em-dirs)

(eval-when-compile
  (require 'cl-lib)
  (require 'eshell))

;;;###autoload
(progn
(defgroup eshell-cmpl nil
  "This module provides a programmable completion function bound to
the TAB key, which allows for completing command names, file names,
variable names, arguments, etc."
  :tag "Argument completion"
  :group 'eshell-module))

;;; User Variables:

(defcustom eshell-cmpl-load-hook nil
  "A list of functions to run when `eshell-cmpl' is loaded."
  :version "24.1"			; removed eshell-cmpl-initialize
  :type 'hook)

(defcustom eshell-show-lisp-completions nil
  "If non-nil, include Lisp functions in the command completion list.
If this variable is nil, Lisp completion can still be done in command
position by using M-TAB instead of TAB."
  :type 'boolean)

(defcustom eshell-show-lisp-alternatives t
  "If non-nil, and no other completions found, show Lisp functions.
Setting this variable means nothing if `eshell-show-lisp-completions'
is non-nil."
  :type 'boolean)

(defcustom eshell-no-completion-during-jobs t
  "If non-nil, don't allow completion while a process is running."
  :type 'boolean)

(defcustom eshell-command-completions-alist
  '(("acroread" . "\\.pdf\\'")
    ("xpdf"     . "\\.pdf\\'")
    ("gunzip"   . "\\.t?gz\\'")
    ("bunzip2"  . "\\.t?bz2\\'")
    ("unxz"     . "\\.t?xz\\'")
    ("ar"       . "\\.[ao]\\'")
    ("gcc"      . "\\.[Cc]\\([Cc]\\|[Pp][Pp]\\)?\\'")
    ("g++"      . "\\.[Cc]\\([Cc]\\|[Pp][Pp]\\)?\\'")
    ("cc"       . "\\.[Cc]\\([Cc]\\|[Pp][Pp]\\)?\\'")
    ("CC"       . "\\.[Cc]\\([Cc]\\|[Pp][Pp]\\)?\\'")
    ("acc"      . "\\.[Cc]\\([Cc]\\|[Pp][Pp]\\)?\\'")
    ("bcc"      . "\\.[Cc]\\([Cc]\\|[Pp][Pp]\\)?\\'")
    ("readelf"  . "\\(\\`[^.]*\\|\\.\\([ao]\\|so\\)\\)\\'")
    ("objdump"  . "\\(\\`[^.]*\\|\\.\\([ao]\\|so\\)\\)\\'")
    ("nm"       . "\\(\\`[^.]*\\|\\.\\([ao]\\|so\\)\\)\\'")
    ("gdb"      . "\\`\\([^.]*\\|a\\.out\\)\\'")
    ("dbx"      . "\\`\\([^.]*\\|a\\.out\\)\\'")
    ("sdb"      . "\\`\\([^.]*\\|a\\.out\\)\\'")
    ("adb"      . "\\`\\([^.]*\\|a\\.out\\)\\'"))
  "An alist that defines simple argument type correlations.
This is provided for common commands, as a simplistic alternative
to writing a completion function."
  :type '(repeat (cons string regexp)))

(defun eshell-cmpl--custom-variable-docstring (pcomplete-var)
  "Generate the docstring of a variable derived from a pcomplete-* variable."
  (format "%s\n\nIts value is assigned to `%s' locally after eshell starts."
          (documentation-property pcomplete-var
                                  'variable-documentation t)
          (symbol-name pcomplete-var)))

(defcustom eshell-cmpl-file-ignore "~\\'"
  (eshell-cmpl--custom-variable-docstring 'pcomplete-file-ignore)
  :type (get 'pcomplete-file-ignore 'custom-type))

(defcustom eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\)/\\'"
  (eshell-cmpl--custom-variable-docstring 'pcomplete-dir-ignore)
  :type (get 'pcomplete-dir-ignore 'custom-type))

(defcustom eshell-cmpl-ignore-case (eshell-under-windows-p)
  (eshell-cmpl--custom-variable-docstring 'completion-ignore-case)
  :type (get 'completion-ignore-case 'custom-type))

(defcustom eshell-cmpl-autolist nil
  (eshell-cmpl--custom-variable-docstring 'pcomplete-autolist)
  :type (get 'pcomplete-autolist 'custom-type))

(defcustom eshell-cmpl-suffix-list (list ?/ ?:)
  (eshell-cmpl--custom-variable-docstring 'pcomplete-suffix-list)
  :type (get 'pcomplete-suffix-list 'custom-type)
  :group 'pcomplete)
;; Only labeled obsolete in 26.1, but all it does it set
;; pcomplete-suffix-list, which is itself obsolete since 24.1.
(make-obsolete-variable 'eshell-cmpl-suffix-list nil "24.1")

(defcustom eshell-cmpl-recexact nil
  (eshell-cmpl--custom-variable-docstring 'pcomplete-recexact)
  :type (get 'pcomplete-recexact 'custom-type))

(defcustom eshell-cmpl-man-function #'man
  (eshell-cmpl--custom-variable-docstring 'pcomplete-man-function)
  :type (get 'pcomplete-man-function 'custom-type))

(defcustom eshell-cmpl-compare-entry-function #'file-newer-than-file-p
  (eshell-cmpl--custom-variable-docstring 'pcomplete-compare-entry-function)
  :type (get 'pcomplete-compare-entry-function 'custom-type))

(defcustom eshell-cmpl-expand-before-complete nil
  (eshell-cmpl--custom-variable-docstring 'pcomplete-expand-before-complete)
  :type (get 'pcomplete-expand-before-complete 'custom-type))

(defcustom eshell-cmpl-cycle-completions t
  (eshell-cmpl--custom-variable-docstring 'pcomplete-cycle-completions)
  :type (get 'pcomplete-cycle-completions 'custom-type))

(defcustom eshell-cmpl-cycle-cutoff-length 5
  (eshell-cmpl--custom-variable-docstring 'pcomplete-cycle-cutoff-length)
  :type (get 'pcomplete-cycle-cutoff-length 'custom-type))

(defcustom eshell-cmpl-restore-window-delay 1
  (eshell-cmpl--custom-variable-docstring 'pcomplete-restore-window-delay)
  :type (get 'pcomplete-restore-window-delay 'custom-type))

(defcustom eshell-command-completion-function
  (lambda ()
    (pcomplete-here (eshell--complete-commands-list)))
  (eshell-cmpl--custom-variable-docstring 'pcomplete-command-completion-function)
  :type (get 'pcomplete-command-completion-function 'custom-type))

(defcustom eshell-cmpl-command-name-function
  #'eshell-completion-command-name
  (eshell-cmpl--custom-variable-docstring 'pcomplete-command-name-function)
  :type (get 'pcomplete-command-name-function 'custom-type))

(defcustom eshell-default-completion-function
  (lambda ()
    (while (pcomplete-here
            (pcomplete-dirs-or-entries
             (cdr (assoc (funcall eshell-cmpl-command-name-function)
                         eshell-command-completions-alist))))))
  (eshell-cmpl--custom-variable-docstring 'pcomplete-default-completion-function)
  :type (get 'pcomplete-default-completion-function 'custom-type))

(defcustom eshell-cmpl-use-paring t
  (eshell-cmpl--custom-variable-docstring 'pcomplete-use-paring)
  :type (get 'pcomplete-use-paring 'custom-type))

;;; Functions:

(defun eshell-complete-lisp-symbol ()
  "Try to complete the text around point as a Lisp symbol."
  (interactive)
  (let ((completion-at-point-functions '(elisp-completion-at-point)))
    (completion-at-point)))

(defvar eshell-cmpl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?i)] #'completion-at-point)
    ;; jww (1999-10-19): Will this work on anything but X?
    (define-key map [backtab] #'pcomplete-reverse)
    (define-key map [(meta ??)] #'completion-help-at-point)
    (define-key map [(meta control ?i)] #'eshell-complete-lisp-symbol)
    ;; C-c prefix:
    (define-key map (kbd "C-c M-h") #'eshell-completion-help)
    (define-key map (kbd "C-c TAB") #'pcomplete-expand-and-complete)
    (define-key map (kbd "C-c C-i") #'pcomplete-expand-and-complete)
    (define-key map (kbd "C-c SPC") #'pcomplete-expand)
    map))

(define-minor-mode eshell-cmpl-mode
  "Minor mode that provides a keymap when `eshell-cmpl' active.

\\{eshell-cmpl-mode-map}"
  :keymap eshell-cmpl-mode-map)

(defun eshell-cmpl-initialize ()    ;Called from `eshell-mode' via intern-soft!
  "Initialize the completions module."
  (setq-local pcomplete-command-completion-function
              eshell-command-completion-function)
  (setq-local pcomplete-command-name-function
              eshell-cmpl-command-name-function)
  (setq-local pcomplete-default-completion-function
              eshell-default-completion-function)
  (setq-local pcomplete-parse-arguments-function
              #'eshell-complete-parse-arguments)
  (setq-local pcomplete-file-ignore
              eshell-cmpl-file-ignore)
  (setq-local pcomplete-dir-ignore
              eshell-cmpl-dir-ignore)
  (setq-local completion-ignore-case
              eshell-cmpl-ignore-case)
  (setq-local pcomplete-autolist
              eshell-cmpl-autolist)
  (if (boundp 'pcomplete-suffix-list)
      (setq-local pcomplete-suffix-list
                  eshell-cmpl-suffix-list))
  (setq-local pcomplete-recexact
              eshell-cmpl-recexact)
  (setq-local pcomplete-man-function
              eshell-cmpl-man-function)
  (setq-local pcomplete-compare-entry-function
              eshell-cmpl-compare-entry-function)
  (setq-local pcomplete-expand-before-complete
              eshell-cmpl-expand-before-complete)
  (setq-local pcomplete-cycle-completions
              eshell-cmpl-cycle-completions)
  (setq-local pcomplete-cycle-cutoff-length
              eshell-cmpl-cycle-cutoff-length)
  (setq-local pcomplete-restore-window-delay
              eshell-cmpl-restore-window-delay)
  (setq-local pcomplete-use-paring
              eshell-cmpl-use-paring)
  ;; `comint-file-name-quote-list' should only be set after all the
  ;; load-hooks for any other extension modules have been run, which
  ;; is true at the time `eshell-mode-hook' is run
  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq-local comint-file-name-quote-list
                          eshell-special-chars-outside-quoting))
            nil t)
  (add-hook 'pcomplete-quote-arg-hook #'eshell-quote-backslash nil t)
  (add-hook 'completion-at-point-functions
            #'pcomplete-completions-at-point nil t)
  (eshell-cmpl-mode))

(defun eshell-completion-command-name ()
  "Return the command name, possibly sans globbing."
  (let ((cmd (file-name-nondirectory (pcomplete-arg 'first))))
    (setq cmd (if (and (> (length cmd) 0)
		       (eq (aref cmd 0) eshell-explicit-command-char))
		  (substring cmd 1)
		cmd))
    (if (eshell-under-windows-p)
	(file-name-sans-extension cmd)
      cmd)))

(defun eshell-completion-help ()
  (interactive)
  (if (= (point) eshell-last-output-end)
      (describe-prefix-bindings)
    (call-interactively 'pcomplete-help)))

(defun eshell-complete-parse-arguments ()
  "Parse the command line arguments for `pcomplete-argument'."
  (when (and eshell-no-completion-during-jobs
	     (eshell-interactive-process))
    (insert-and-inherit "\t")
    (throw 'pcompleted t))
  (let ((end (point-marker))
	(begin (save-excursion (eshell-bol) (point)))
	(posns (list t))
	args delim)
    (when (memq this-command '(pcomplete-expand
			       pcomplete-expand-and-complete))
      (run-hook-with-args 'eshell-expand-input-functions begin end)
      (if (= begin end)
	  (end-of-line))
      (setq end (point-marker)))
    (if (setq delim
	      (catch 'eshell-incomplete
		(ignore
		 (setq args (eshell-parse-arguments begin end)))))
	(cond ((memq (car delim) '(?\{ ?\<))
	       (setq begin (1+ (cadr delim))
		     args (eshell-parse-arguments begin end)))
	      ((eq (car delim) ?\()
	       (eshell-complete-lisp-symbol)
	       (throw 'pcompleted t))
	      (t
	       (insert-and-inherit "\t")
	       (throw 'pcompleted t))))
    (when (get-text-property (1- end) 'comment)
      (insert-and-inherit "\t")
      (throw 'pcompleted t))
    (let ((pos begin))
      (while (< pos end)
	(if (get-text-property pos 'arg-begin)
	    (nconc posns (list pos)))
	(setq pos (1+ pos))))
    (setq posns (cdr posns))
    (cl-assert (= (length args) (length posns)))
    (let ((a args)
	  (i 0)
	  l)
      (while a
	(if (and (consp (car a))
		 (eq (caar a) 'eshell-operator))
	    (setq l i))
	(setq a (cdr a) i (1+ i)))
      (and l
	   (setq args (nthcdr (1+ l) args)
		 posns (nthcdr (1+ l) posns))))
    (cl-assert (= (length args) (length posns)))
    (when (and args (eq (char-syntax (char-before end)) ? )
	       (not (eq (char-before (1- end)) ?\\)))
      (nconc args (list ""))
      (nconc posns (list (point))))
    (cons (mapcar
           (lambda (arg)
             (let ((val
                    (if (listp arg)
                        (let ((result
                               (eshell-do-eval
                                (list 'eshell-commands arg) t)))
                          (cl-assert (eq (car result) 'quote))
                          (cadr result))
                      arg)))
               (cond ((numberp val)
                      (setq val (number-to-string val)))
                     ;; expand .../ etc that only eshell understands to
                     ;; standard ../../
                     ((and (stringp val)) (string-match "\\.\\.\\.+/" val)
                      (setq val (eshell-expand-multiple-dots val))))
               (or val "")))
	   args)
	  posns)))

(defun eshell--complete-commands-list ()
  "Generate list of applicable, visible commands."
  ;; Building the commands list can take quite a while, especially over Tramp
  ;; (bug#41423), so do it lazily.
  (let ((glob-name
	 ;; When a command is specified using `eshell-explicit-command-char',
         ;; that char is not part of the command and hence not part of what
         ;; we complete.  Adjust `pcomplete-stub' accordingly!
	 (if (and (> (length pcomplete-stub) 0)
	          (eq (aref pcomplete-stub 0) eshell-explicit-command-char))
	     (setq pcomplete-stub (substring pcomplete-stub 1)))))
    (completion-table-dynamic
     (lambda (filename)
       (if (file-name-directory filename)
           (if eshell-force-execution
               (pcomplete-dirs-or-entries nil #'file-readable-p)
             (pcomplete-executables))
	 (let* ((paths (eshell-get-path))
		(cwd (file-name-as-directory
		      (expand-file-name default-directory)))
		(filepath "") (completions ()))
	   ;; Go thru each path in the search path, finding completions.
	   (dolist (path paths)
	     (setq path (file-name-as-directory
		         (expand-file-name (or path "."))))
	     ;; Go thru each completion found, to see whether it should
	     ;; be used.
	     (dolist (file (and (file-accessible-directory-p path)
		                (file-name-all-completions filename path)))
	       (setq filepath (concat path file))
	       (if (and (not (member file completions)) ;
			(or (string-equal path cwd)
			    (not (file-directory-p filepath)))
			;; FIXME: Those repeated file tests end up
			;; very costly over Tramp, we should cache the result.
			(if eshell-force-execution
                            (file-readable-p filepath)
                          (file-executable-p filepath)))
		   (push file completions))))
	   ;; Add aliases which are currently visible, and Lisp functions.
	   (pcomplete-uniquify-list
	    (if glob-name
	        completions
	      (setq completions
		    (append (if (fboundp 'eshell-alias-completions)
			        (eshell-alias-completions filename))
			    (eshell-winnow-list
			     (mapcar
                              (lambda (name)
                                (substring name 7))
			      (all-completions (concat "eshell/" filename)
					       obarray #'functionp))
			     nil '(eshell-find-alias-function))
			    completions))
	      (append (and (or eshell-show-lisp-completions
			       (and eshell-show-lisp-alternatives
				    (null completions)))
			   (all-completions filename obarray #'functionp))
		      completions)))))))))

(define-obsolete-function-alias 'eshell-pcomplete #'completion-at-point "27.1")

(provide 'em-cmpl)

;; Local Variables:
;; generated-autoload-file: "esh-groups.el"
;; End:

;;; em-cmpl.el ends here
