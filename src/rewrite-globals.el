;; Rewrite DEFVAR_LISP variables.
;; Each variable is renamed to start with impl_.
;; Compatibility defines are added to globals.h.
;; Invoke as:  emacs --script rewrite-globals.el

(defvar defvar-list '())

(defun extract-defvars ()
  (let ((case-fold-search nil))
    (while (re-search-forward "^[^#*]*\\(DEFVAR_[A-Z_]*\\)" nil 'move)
      (let ((kind (match-string 1)))
	(unless (member kind '("DEFVAR_KBOARD" "DEFVAR_PER_BUFFER"))
	  ;; Skip the paren and the first argument.
	  (skip-chars-forward " (")
	  (forward-sexp)
	  (skip-chars-forward ", \t\n&")
	  (if (looking-at "\\_<\\(\\sw\\|\\s_\\)+\\_>")
	      (let ((var-name (match-string 0)))
		  (if (equal kind "DEFVAR_LISP")
		      (push var-name defvar-list)))))))))

(defun munge-V ()
  (interactive)
  (while (re-search-forward "^\\(extern \\|static \\)?Lisp_Object " nil 'move)
    ;; skip function decls.
    (if (not (looking-at ".*("))
	(while (looking-at "[a-z0-9A-Z_]+")
	  (if (member (match-string 0) defvar-list)
	      (progn
		;; Rename them all to impl_
		(goto-char (match-beginning 0))
		(insert "impl_")))
	  (forward-sexp)
	  (skip-chars-forward ", \t\n")))))

(defconst V-dir ".")

(defun munge-V-directory ()
  ;; First extract all defvars.
  (dolist (file (directory-files V-dir t "[ch]$"))
    (save-excursion
      (message "Scanning %s" file)
      (find-file file)
      (extract-defvars)))

  (setq defvar-list (delete-dups (sort defvar-list #'string<)))

  (dolist (file (directory-files V-dir t "[ch]$"))
    (save-excursion
      (message "Processing %s" file)
      (find-file file)
      (goto-char (point-min))
      (munge-V)
      (save-buffer)))

  (find-file "globals.h")
  (erase-buffer)
  (dolist (v defvar-list)
    (insert "#define " v " *find_variable_location (&impl_" v ")\n"))

  ;; A few special cases for globals.h.
  (insert "\n")
  (dolist (v '("do_mouse_tracking" "Vmark_even_if_inactive" "Vprint_level"))
    (insert "extern Lisp_Object impl_" v ";\n"))
  (save-buffer))

(munge-V-directory)
