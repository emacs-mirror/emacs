;; Rewrite all references to buffer-objfwd fields in struct buffer
;; to use accessor macros.
;; This works in a tricky way: it renames all such fields, then
;; recompiles Emacs.  Then it visits each error location and
;; rewrites the expressions.
;; This has a few requirements in order to work.
;; First, Emacs must compile before the script is run.
;; It does not handle errors arising for other reasons.
;; Second, you need a GCC which has been hacked to emit proper
;; column location even when the -> expression in question has
;; been wrapped in a macro call.  (This is a one-liner in libcpp.)
;; After running this script, a few changes need to be made by hand.
;; These occur mostly in macros in headers, but also in
;; reset_buffer and reset_buffer_local_variables.

(defvar gcc-prefix "/home/tromey/gnu/Trunk/install/")

(defvar emacs-src "/home/tromey/gnu/Emacs/Gitorious/emacs-mt/src/")
(defvar emacs-build "/home/tromey/gnu/Emacs/Gitorious/build/src/")

(defun file-error (text)
  (error "%s:%d:%d: error: expected %s"
	 buffer-file-name (line-number-at-pos (point))
	 (current-column)
	 text))

(defun assert-looking-at (exp)
  (unless (looking-at exp)
    (file-error exp)))

(defvar field-names nil)

(defvar field-regexp nil)

(defun modify-buffer.h ()
  (message "Modifying fields in struct buffer")
  (find-file (expand-file-name "buffer.h" emacs-src))
  (goto-char (point-min))
  (re-search-forward "^struct buffer$")
  (forward-line)
  (assert-looking-at "^{")
  (let ((starting-point (point))
	(closing-brace (save-excursion
			 (forward-sexp)
			 (point))))
    ;; Find each field.
    (while (re-search-forward "^\\s *Lisp_Object\\s +"
			      closing-brace 'move)
      (goto-char (match-end 0))
      (while (not (looking-at ";"))
	(assert-looking-at "\\([A-Za-z0-9_]+\\)\\(;\\|,\\s *\\)")
	;; Remember the name so we can generate accessors.
	(push (match-string 1) field-names)
	;; Rename it.
	(goto-char (match-beginning 2))
	(insert "_")
	;; On to the next one, if any.
	(if (looking-at ",\\s *")
	    (goto-char (match-end 0)))))
    ;; Generate accessors.
    (goto-char starting-point)
    (forward-sexp)
    (forward-line)
    (insert "\n")
    (dolist (name field-names)
      (insert "#define BUF_" (upcase name) "(BUF) "
	      "*find_variable_location (&((BUF)->"
	      name "_))\n"))
    (insert "\n"))
  (setq field-regexp (concat "\\(->\\|\\.\\)"
			     (regexp-opt field-names t)
			     "\\_>"))
  (save-buffer))

(defun get-field-name ()
  (save-excursion
    (assert-looking-at "\\(\\.\\|->\\)\\([A-Za-z0-9_]+\\)\\_>")
    (prog1
	(match-string 2)
      (delete-region (match-beginning 0) (match-end 0)))))

(defun skip-backward-lhs ()
  (skip-chars-backward " \t\n")
  (cond
   ((eq (char-before) ?\])
    (file-error "array ref!")
    ;; fixme
    )
   ((eq (char-before) ?\))
    ;; A paren expression is preceding.
    ;; See if this is just a paren expression or whether it is a
    ;; function call.
    ;; For now assume that there are no function-calls-via-expr.
    (backward-sexp)
    (skip-chars-backward " \t\n")
    (if (save-excursion
	  (backward-char)
	  (looking-at "[A-Za-z0-9_]"))
	(backward-sexp)))
   ((save-excursion
      (backward-char)
      (looking-at "[A-Za-z0-9_]"))
    (backward-sexp))
   (t
    (file-error "unhandled case!"))))

(defun do-fix-instance ()
  (cond
   ((looking-at "->")
    (let ((field-name (get-field-name)))
      (insert ")")
      (backward-char)
      (skip-backward-lhs)
      (insert "BUF_" (upcase field-name) " (")))
   ((eq (char-after) ?.)
    (let ((field-name (get-field-name)))
      (insert ")")
      (backward-char)
      (backward-sexp)
      (assert-looking-at "\\(buffer_defaults\\|buffer_local_flags\\)")
      (insert "BUF_" (upcase field-name) " (&")))
   (t
    (message "%s:%d:%d: warning: did not see -> or ., probably macro"
	     buffer-file-name (line-number-at-pos (point))
	     (current-column)))))

(defun update-header-files ()
  (dolist (file (directory-files emacs-src t "h$"))
    (message "Applying header changes to %s" file)
    (find-file file)
    (while (re-search-forward
	    "\\(current_buffer->\\|buffer_defaults\\.\\)"
	    nil 'move)
      (goto-char (match-end 0))
      (skip-chars-backward "->.")
      (when (looking-at field-regexp)
	(do-fix-instance)))
    (goto-char (point-min))
    (while (search-forward "XBUFFER (" nil 'move)
      (goto-char (- (match-end 0) 1))
      (forward-sexp)
      ;; This works even for the new #define BUF_ macros
      ;; because the field-regexp ends with \_>.
      (when (looking-at field-regexp)
	(do-fix-instance)))
    (save-buffer)))

(defun fix-one-instance (filename line column)
  (message "%s:%d:%d: info: fixing instance" filename line column)
  (find-file filename)
  (goto-char (point-min))
  (forward-line (- line 1))
  ;; (move-to-column (- column 1))
  (forward-char (- column 1))
  (do-fix-instance))

(defvar make-accumulation "")

(defvar last-error-line nil)
(defvar error-list nil)

(defun make-filter (process string)
  (setq make-accumulation (concat make-accumulation string))
  (while (string-match "^[^\n]*\n" make-accumulation)
    (let ((line (substring (match-string 0 make-accumulation) 0 -1)))
      (setq make-accumulation (substring make-accumulation
					 (match-end 0)))
      (message "%s" line)
      (if (string-match "^\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)+: error:"
			line)
	  (save-excursion
	    (let ((file-name (match-string 1 line))
		  (line-no (string-to-number (match-string 2 line)))
		  (col-no (string-to-number (match-string 3 line))))
	      ;; Process all errors on a given line in reverse order.
	      (unless (eq line-no last-error-line)
		(dolist (one-item error-list)
		  (apply #'fix-one-instance one-item))
		(setq error-list nil)
		(setq last-error-line line-no))
	      (push (list file-name line-no col-no) error-list)))))))

(defvar make-done nil)

(defun make-sentinel (process string)
  (dolist (one-item error-list)
    (apply #'fix-one-instance one-item))
  (setq make-done t))

(defun recompile-emacs ()
  (let* ((default-directory emacs-build)
	 (output-buffer (get-buffer-create "*recompile*"))
	 (make (start-process "make" output-buffer "make" "-k")))
    (set-process-filter make #'make-filter)
    (set-process-sentinel make #'make-sentinel)
    (while (not make-done)
      (accept-process-output))))

(modify-buffer.h)
(update-header-files)
(recompile-emacs)
(dolist (buf (buffer-list))
  (with-current-buffer buf
    (when buffer-file-name
      (message "Saving %s" buffer-file-name)
      (save-buffer))))
