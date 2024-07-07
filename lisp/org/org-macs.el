;;; org-macs.el --- Top-level Definitions for Org -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2024 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten.dominik@gmail.com>
;; Keywords: outlines, hypermedia, calendar, text
;; URL: https://orgmode.org
;;
;; This file is part of GNU Emacs.
;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file contains macro definitions, defsubst definitions, other
;; stuff needed for compilation and top-level forms in Org mode, as
;; well lots of small functions that are not Org mode specific but
;; simply generally useful stuff.

;;; Code:

(require 'cl-lib)
(require 'format-spec)

;;; Org version verification.

(defvar org--inhibit-version-check nil
  "When non-nil, skip the detection of mixed-versions situations.
For internal use only.  See Emacs bug #62762.
This variable is only supposed to be changed by Emacs build scripts.
When nil, Org tries to detect when Org source files were compiled with
a different version of Org (which tends to lead to incorrect `.elc' files),
or when the current Emacs session has loaded a mix of files from different
Org versions (typically the one bundled with Emacs and another one installed
from GNU ELPA), which can happen if some parts of Org were loaded before
`load-path' was changed (e.g. before the GNU-ELPA-installed Org is activated
by `package-activate-all').")
(defmacro org-assert-version ()
  "Assert compile time and runtime version match."
  ;; We intentionally use a more permissive `org-release' instead of
  ;; `org-git-version' to work around deficiencies in Elisp
  ;; compilation after pulling latest changes.  Unchanged files will
  ;; not be re-compiled and thus their macro-expanded
  ;; `org-assert-version' calls would fail using strict
  ;; `org-git-version' check because the generated Org version strings
  ;; will not match.
  `(unless (or ,org--inhibit-version-check (equal (org-release) ,(org-release)))
     (warn "Org version mismatch.
This warning usually appears when a built-in Org version is loaded
prior to the more recent Org version.

Version mismatch is commonly encountered in the following situations:

1. Emacs is loaded using literate Org config and more recent Org
   version is loaded inside the file loaded by `org-babel-load-file'.
   `org-babel-load-file' triggers the built-in Org version clashing
   the newer Org version attempt to be loaded later.

   It is recommended to move the Org loading code before the
   `org-babel-load-file' call.

2. New Org version is loaded manually by setting `load-path', but some
   other package depending on Org is loaded before the `load-path' is
   configured.
   This \"other package\" is triggering built-in Org version, again
   causing the version mismatch.

   It is recommended to set `load-path' as early in the config as
   possible.

3. New Org version is loaded using straight.el package manager and
   other package depending on Org is loaded before straight triggers
   loading of the newer Org version.

   It is recommended to put

    %s

   early in the config.  Ideally, right after the straight.el
   bootstrap.  Moving `use-package' :straight declaration may not be
   sufficient if the corresponding `use-package' statement is
   deferring the loading.

4. A new Org version is synchronized with Emacs git repository and
   stale .elc files are still left from the previous build.

   It is recommended to remove .elc files from lisp/org directory and
   re-compile."
           ;; Avoid `warn' replacing "'" with "’" (see `format-message').
           "(straight-use-package 'org)")))

;; We rely on org-macs when generating Org version.  Checking Org
;; version here will interfere with Org build process.
;; (org-assert-version)

(declare-function org-mode "org" ())
(declare-function org-agenda-files "org" (&optional unrestricted archives))
(declare-function org-time-string-to-seconds "org" (s))
(declare-function org-fold-show-context "org-fold" (&optional key))
(declare-function org-fold-save-outline-visibility "org-fold" (use-markers &rest body))
(declare-function org-fold-next-visibility-change "org-fold" (&optional pos limit ignore-hidden-p previous-p))
(declare-function org-fold-core-with-forced-fontification "org-fold" (&rest body))
(declare-function org-fold-folded-p "org-fold" (&optional pos limit ignore-hidden-p previous-p))
(declare-function org-time-convert-to-list "org-compat" (time))
(declare-function org-buffer-text-pixel-width "org-compat" ())

(defvar org-ts-regexp0)
(defvar ffap-url-regexp)


;;; Macros

(defmacro org-require-package (symbol &optional name noerror)
  "Try to load library SYMBOL and display error otherwise.
With optional parameter NAME, use NAME as package name instead of
SYMBOL.  Show warning instead of error when NOERROR is non-nil."
  `(unless (require ,symbol nil t)
     (,(if noerror 'warn 'user-error)
      "`%s' failed to load required package \"%s\""
      this-command ,(or name symbol))))

(defmacro org-with-gensyms (symbols &rest body)
  (declare (debug (sexp body)) (indent 1))
  `(let ,(mapcar (lambda (s)
		   `(,s (make-symbol (concat "--" (symbol-name ',s)))))
                 symbols)
     ,@body))

;; Use `with-silent-modifications' to ignore cosmetic changes and
;; `org-unmodified' to ignore real text modifications.
(defmacro org-unmodified (&rest body)
  "Run BODY while preserving the buffer's `buffer-modified-p' state."
  (declare (debug (body)))
  (org-with-gensyms (was-modified)
    `(let ((,was-modified (buffer-modified-p)))
       (unwind-protect
           (let ((buffer-undo-list t)
		 (inhibit-modification-hooks t))
	     ,@body)
	 (set-buffer-modified-p ,was-modified)))))

(defmacro org-with-base-buffer (buffer &rest body)
  "Run BODY in base buffer for BUFFER.
If BUFFER is nil, use base buffer for `current-buffer'."
  (declare (debug (body)) (indent 1))
  `(with-current-buffer (or (buffer-base-buffer ,buffer)
                            (or ,buffer (current-buffer)))
     ,@body))

(defmacro org-with-point-at (epom &rest body)
  "Move to buffer and point of EPOM for the duration of BODY.
EPOM is an element, point, or marker."
  (declare (debug (form body)) (indent 1))
  (require 'org-element-ast)
  (org-with-gensyms (mepom)
    `(let ((,mepom ,epom))
       (save-excursion
         (cond
          ((markerp ,mepom)
           (set-buffer (marker-buffer ,mepom)))
          ((numberp ,mepom))
          (t
           (when (org-element-property :buffer ,mepom)
             (set-buffer (org-element-property :buffer ,mepom)))
           (setq ,mepom (org-element-property :begin ,mepom))))
	 (org-with-wide-buffer
	  (goto-char (or ,mepom (point)))
	  ,@body)))))

(defmacro org-with-remote-undo (buffer &rest body)
  "Execute BODY while recording undo information in current buffer and BUFFER.
This function is only useful when called from Agenda buffer."
  (declare (debug (form body)) (indent 1))
  (org-with-gensyms (cline cmd buf1 buf2 undo1 undo2 c1 c2)
    `(let ((,cline (org-current-line))
	   (,cmd this-command)
	   (,buf1 (current-buffer))
	   (,buf2 ,buffer)
	   (,undo1 buffer-undo-list)
	   (,undo2 (with-current-buffer ,buffer buffer-undo-list))
	   ,c1 ,c2)
       ,@body
       (when org-agenda-allow-remote-undo
	 (setq ,c1 (org-verify-change-for-undo
		    ,undo1 (with-current-buffer ,buf1 buffer-undo-list))
	       ,c2 (org-verify-change-for-undo
		    ,undo2 (with-current-buffer ,buf2 buffer-undo-list)))
	 (when (or ,c1 ,c2)
	   ;; make sure there are undo boundaries
	   (and ,c1 (with-current-buffer ,buf1 (undo-boundary)))
	   (and ,c2 (with-current-buffer ,buf2 (undo-boundary)))
	   ;; remember which buffer to undo
	   (push (list ,cmd ,cline ,buf1 ,c1 ,buf2 ,c2)
		 org-agenda-undo-list))))))

(defmacro org-no-read-only (&rest body)
  "Inhibit read-only for BODY."
  (declare (debug (body)))
  `(let ((inhibit-read-only t)) ,@body))

(defalias 'org-save-outline-visibility #'org-fold-save-outline-visibility)

(defmacro org-with-wide-buffer (&rest body)
  "Execute BODY while temporarily widening the buffer."
  (declare (debug (body)))
  `(save-excursion
     (save-restriction
       (widen)
       ,@body)))

(defmacro org-with-limited-levels (&rest body)
  "Execute BODY with limited number of outline levels."
  (declare (debug (body)))
  `(progn
     (defvar org-called-with-limited-levels)
     (defvar org-outline-regexp)
     (defvar outline-regexp)
     (defvar org-outline-regexp-bol)
     (let* ((org-called-with-limited-levels t)
            (org-outline-regexp (org-get-limited-outline-regexp))
            (outline-regexp org-outline-regexp)
            (org-outline-regexp-bol (org-get-limited-outline-regexp t)))
       ,@body)))

(defmacro org-eval-in-environment (environment form)
  (declare (debug (form form)) (indent 1) (obsolete cl-progv "2021"))
  `(eval (list 'let ,environment ',form)))

;;;###autoload
(defmacro org-load-noerror-mustsuffix (file)
  "Load FILE with optional arguments NOERROR and MUSTSUFFIX."
  `(load ,file 'noerror nil nil 'mustsuffix))

(defmacro org-preserve-local-variables (&rest body)
  "Execute BODY while preserving local variables."
  (declare (debug (body)))
  `(let ((local-variables
	  (org-with-wide-buffer
	   (goto-char (point-max))
	   (let ((case-fold-search t))
	     (and (re-search-backward "^[ \t]*# +Local Variables:"
				      (max (- (point) 3000) 1)
				      t)
               (let ((buffer-undo-list t))
	         (delete-and-extract-region (point) (point-max)))))))
         (tick-counter-before (buffer-modified-tick)))
     (unwind-protect (progn ,@body)
       (when local-variables
	 (org-with-wide-buffer
	  (goto-char (point-max))
	  (unless (bolp) (insert "\n"))
          (let ((modified (< tick-counter-before (buffer-modified-tick)))
                (buffer-undo-list t))
	    (insert local-variables)
            (unless modified
              (restore-buffer-modified-p nil))))))))

;;;###autoload
(defmacro org-element-with-disabled-cache (&rest body)
  "Run BODY without active org-element-cache."
  (declare (debug (form body)) (indent 0))
  `(cl-letf (((symbol-function #'org-element--cache-active-p) (lambda (&rest _) nil)))
     ,@body))


;;; Buffer and windows

(defun org-base-buffer (buffer)
  "Return the base buffer of BUFFER, if it has one.  Else return the buffer."
  (when buffer
    (or (buffer-base-buffer buffer)
	buffer)))

(defun org-find-base-buffer-visiting (file)
  "Like `find-buffer-visiting' but always return the base buffer.
FILE is the file name passed to `find-buffer-visiting'."
  (let ((buf (or (get-file-buffer file)
		 (find-buffer-visiting file))))
    (org-base-buffer buf)))

(defvar-local org-file-buffer-created nil
  "Non-nil when current buffer is created from `org-with-file-buffer'.
The value is FILE argument passed to `org-with-file-buffer'.")
(defmacro org-with-file-buffer (file &rest body)
  "Evaluate BODY with current buffer visiting FILE.
When no live buffer is visiting FILE, create one and kill after
evaluating BODY.
During evaluation, when the buffer was created, `org-file-buffer-created'
variable is set to FILE."
  (declare (debug (form body)) (indent 1))
  (org-with-gensyms (mark-function filename buffer)
    `(let ((,mark-function (lambda () (setq-local org-file-buffer-created ,file)))
           (,filename ,file)
           ,buffer)
       (add-hook 'find-file-hook ,mark-function)
       (unwind-protect
           (progn
             (setq ,buffer (find-file-noselect ,filename t))
             (with-current-buffer ,buffer
               (prog1 (progn ,@body)
                 (with-current-buffer ,buffer
                   (when (equal ,filename org-file-buffer-created)
                     (kill-buffer))))))
         (remove-hook 'find-file-hook ,mark-function)))))

(defun org-fit-window-to-buffer (&optional window max-height min-height
                                           shrink-only)
  "Fit WINDOW to the buffer, but only if it is not a side-by-side window.
WINDOW defaults to the selected window.  MAX-HEIGHT and MIN-HEIGHT are
passed through to `fit-window-to-buffer'.  If SHRINK-ONLY is set, call
`shrink-window-if-larger-than-buffer' instead, the height limit is
ignored in this case."
  (cond ((not (window-full-width-p window))
         ;; Do nothing if another window would suffer.
         )
        ((not shrink-only)
         (fit-window-to-buffer window max-height min-height))
        (t (shrink-window-if-larger-than-buffer window)))
  (or window (selected-window)))

(defun org-buffer-list (&optional predicate exclude-tmp)
  "Return a list of Org buffers.
PREDICATE can be `export', `files' or `agenda'.

export   restrict the list to Export buffers.
files    restrict the list to buffers visiting Org files.
agenda   restrict the list to buffers visiting agenda files.

If EXCLUDE-TMP is non-nil, ignore temporary buffers."
  (let* ((bfn nil)
	 (agenda-files (and (eq predicate 'agenda)
			    (mapcar 'file-truename (org-agenda-files t))))
	 (filter
	  (cond
	   ((eq predicate 'files)
	    (lambda (b) (with-current-buffer b (derived-mode-p 'org-mode))))
	   ((eq predicate 'export)
	    (lambda (b) (string-match "\\*Org .*Export" (buffer-name b))))
	   ((eq predicate 'agenda)
	    (lambda (b)
	      (with-current-buffer b
		(and (derived-mode-p 'org-mode)
		     (setq bfn (buffer-file-name b))
		     (member (file-truename bfn) agenda-files)))))
	   (t (lambda (b) (with-current-buffer b
			    (or (derived-mode-p 'org-mode)
				(string-match "\\*Org .*Export"
					      (buffer-name b)))))))))
    (delq nil
	  (mapcar
	   (lambda(b)
	     (if (and (funcall filter b)
		      (or (not exclude-tmp)
			  (not (string-match "tmp" (buffer-name b)))))
		 b
	       nil))
	   (buffer-list)))))



;;; File

(defun org-file-newer-than-p (file time)
  "Non-nil if FILE modification time is greater than TIME.
TIME should be obtained earlier for the same FILE name using

  \(file-attribute-modification-time (file-attributes file))

If TIME is nil (file did not exist) then any existing FILE
is considered as a newer one.  Some file systems have coarse
timestamp resolution, for example 1 second on HFS+ or 2 seconds on FAT,
so nil may be returned when file is updated twice within a short period
of time.  File timestamp and system clock `current-time' may have
different resolution, so attempts to compare them may give unexpected
results.

Consider `file-newer-than-file-p' to check up to date state
in target-prerequisite files relation."
  (let ((mtime (file-attribute-modification-time (file-attributes file))))
    (and mtime (or (not time) (time-less-p time mtime)))))


;;; Indentation

(defmacro org-current-text-indentation ()
  "Like `current-indentation', but ignore display/invisible properties."
  `(let ((buffer-invisibility-spec nil))
     (current-indentation)))

(defun org-do-remove-indentation (&optional n skip-fl)
  "Remove the maximum common indentation from the buffer.
Do not consider invisible text when calculating indentation.

When optional argument N is a positive integer, remove exactly
that much characters from indentation, if possible.  When
optional argument SKIP-FL is non-nil, skip the first
line.  Return nil if it fails."
  (catch :exit
    (goto-char (point-min))
    ;; Find maximum common indentation, if not specified.
    (let ((n (or n
		 (let ((min-ind (point-max)))
		   (save-excursion
                     (when skip-fl (forward-line))
		     (while (re-search-forward "^[ \t]*\\S-" nil t)
		       (let ((ind (org-current-text-indentation)))
			 (if (zerop ind) (throw :exit nil)
			   (setq min-ind (min min-ind ind))))))
		   min-ind))))
      (if (zerop n) (throw :exit nil)
	;; Remove exactly N indentation, but give up if not possible.
        (when skip-fl (forward-line))
	(while (not (eobp))
	  (let* ((buffer-invisibility-spec nil) ; do not treat invisible text specially
                 (ind (progn (skip-chars-forward " \t") (current-column))))
	    (cond ((< ind n)
                   (if (eolp) (delete-region (line-beginning-position) (point))
                     (throw :exit nil)))
		  (t (delete-region (line-beginning-position)
                                    (progn (move-to-column n t)
                                           (point)))))
	    (forward-line)))
	;; Signal success.
	t))))



;;; Input

(defun org-read-function (prompt &optional allow-empty?)
  "Prompt for a function.
If ALLOW-EMPTY? is non-nil, return nil rather than raising an
error when the user input is empty."
  (let ((func (completing-read prompt obarray #'fboundp t)))
    (cond ((not (string= func ""))
	   (intern func))
	  (allow-empty? nil)
	  (t (user-error "Empty input is not valid")))))

(declare-function org-timestamp-inactive "org" (&optional arg))

(defun org-completing-read (&rest args)
  "Completing-read with SPACE being a normal character."
  (let ((enable-recursive-minibuffers t)
	(minibuffer-local-completion-map
	 (copy-keymap minibuffer-local-completion-map)))
    (define-key minibuffer-local-completion-map " " #'self-insert-command)
    (define-key minibuffer-local-completion-map "?" #'self-insert-command)
    (define-key minibuffer-local-completion-map (kbd "C-c !")
                #'org-timestamp-inactive)
    (apply #'completing-read args)))

(defun org--mks-read-key (allowed-keys prompt navigation-keys)
  "Read a key and ensure it is a member of ALLOWED-KEYS.
Enable keys to scroll the window if NAVIGATION-KEYS is set.
TAB, SPC and RET are treated equivalently."
  (setq header-line-format (when navigation-keys "Use C-n, C-p, C-v, M-v to navigate."))
  (let ((char-key (read-char-exclusive prompt)))
    (if (and navigation-keys (memq char-key '(14 16 22 134217846)))
	(progn
	  (org-scroll char-key)
	  (org--mks-read-key allowed-keys prompt navigation-keys))
      (let ((key (char-to-string
		  (pcase char-key
		    ((or ?\s ?\t ?\r) ?\t)
		    (char char)))))
	(if (member key allowed-keys)
	    key
	  (message "Invalid key: `%s'" key)
	  (sit-for 1)
	  (org--mks-read-key allowed-keys prompt navigation-keys))))))

(defun org-mks (table title &optional prompt specials)
  "Select a member of an alist with multiple keys.

TABLE is the alist which should contain entries where the car is a string.
There should be two types of entries.

1. prefix descriptions like (\"a\" \"Description\")
   This indicates that `a' is a prefix key for multi-letter selection, and
   that there are entries following with keys like \"ab\", \"ax\"...

2. Select-able members must have more than two elements, with the first
   being the string of keys that lead to selecting it, and the second a
   short description string of the item.

The command will then make a temporary buffer listing all entries
that can be selected with a single key, and all the single key
prefixes.  When you press the key for a single-letter entry, it is selected.
When you press a prefix key, the commands (and maybe further prefixes)
under this key will be shown and offered for selection.

TITLE will be placed over the selection in the temporary buffer,
PROMPT will be used when prompting for a key.  SPECIALS is an
alist with (\"key\" \"description\") entries.  When one of these
is selected, only the bare key is returned."
  (save-window-excursion
    (let ((inhibit-quit t)
	  (buffer (switch-to-buffer-other-window "*Org Select*"))
	  (prompt (or prompt "Select: "))
	  case-fold-search
	  current)
      (unwind-protect
	  (catch 'exit
	    (while t
	      (erase-buffer)
	      (insert title "\n\n")
	      (let ((des-keys nil)
		    (allowed-keys '("\C-g"))
		    (tab-alternatives '("\s" "\t" "\r"))
		    (cursor-type nil))
		;; Populate allowed keys and descriptions keys
		;; available with CURRENT selector.
		(let ((re (format "\\`%s\\(.\\)\\'"
				  (if current (regexp-quote current) "")))
		      (prefix (if current (concat current " ") "")))
		  (dolist (entry table)
		    (pcase entry
		      ;; Description.
		      (`(,(and key (pred (string-match re))) ,desc)
		       (let ((k (match-string 1 key)))
			 (push k des-keys)
			 ;; Keys ending in tab, space or RET are equivalent.
			 (if (member k tab-alternatives)
			     (push "\t" allowed-keys)
			   (push k allowed-keys))
			 (insert prefix "[" k "]" "..." "  " desc "..." "\n")))
		      ;; Usable entry.
		      (`(,(and key (pred (string-match re))) ,desc . ,_)
		       (let ((k (match-string 1 key)))
			 (insert prefix "[" k "]" "     " desc "\n")
			 (push k allowed-keys)))
		      (_ nil))))
		;; Insert special entries, if any.
		(when specials
		  (insert "----------------------------------------------------\
---------------------------\n")
		  (pcase-dolist (`(,key ,description) specials)
		    (insert (format "[%s]     %s\n" key description))
		    (push key allowed-keys)))
		;; Display UI and let user select an entry or
		;; a sub-level prefix.
		(goto-char (point-min))
		(org-fit-window-to-buffer)
		(message "") ; With this line the prompt appears in
                                        ; the minibuffer. Else keystrokes may
                                        ; appear, which is spurious.
		(let ((pressed (org--mks-read-key
				allowed-keys prompt
				(not (pos-visible-in-window-p (1- (point-max)))))))
		  (setq current (concat current pressed))
		  (cond
		   ((equal pressed "\C-g") (user-error "Abort"))
		   ;; Selection is a prefix: open a new menu.
		   ((member pressed des-keys))
		   ;; Selection matches an association: return it.
		   ((let ((entry (assoc current table)))
		      (and entry (throw 'exit entry))))
		   ;; Selection matches a special entry: return the
		   ;; selection prefix.
		   ((assoc current specials) (throw 'exit current))
		   (t (error "No entry available")))))))
        (when buffer
          (when-let ((window (get-buffer-window buffer t)))
            (quit-window 'kill window))
          (kill-buffer buffer))))))


;;; List manipulation

(defsubst org-get-alist-option (option key)
  (cond ((eq key t) t)
	((eq option t) t)
	((assoc key option) (cdr (assoc key option)))
	(t (let ((r (cdr (assq 'default option))))
	     (if (listp r) (delq nil r) r)))))

(defsubst org-last (list)
  "Return the last element of LIST."
  (car (last list)))

(defsubst org-uniquify (list)
  "Non-destructively remove duplicate elements from LIST."
  (let ((res (copy-sequence list))) (delete-dups res)))

(defun org-uniquify-alist (alist)
  "Merge elements of ALIST with the same key.

For example, in this alist:

\(org-uniquify-alist \\='((a 1) (b 2) (a 3)))
  => ((a 1 3) (b 2))

merge (a 1) and (a 3) into (a 1 3).

The function returns the new ALIST."
  (let (rtn)
    (dolist (e alist rtn)
      (let (n)
	(if (not (assoc (car e) rtn))
	    (push e rtn)
	  (setq n (cons (car e) (append (cdr (assoc (car e) rtn)) (cdr e))))
	  (setq rtn (assq-delete-all (car e) rtn))
	  (push n rtn))))))

(defun org-delete-all (elts list)
  "Remove all elements in ELTS from LIST.
Comparison is done with `equal'.  It is a destructive operation
that may remove elements by altering the list structure."
  (while elts
    (setq list (delete (pop elts) list)))
  list)

(defun org-plist-delete-all (plist props)
  "Delete all elements in PROPS from PLIST."
  (dolist (e props plist)
    (setq plist (org-plist-delete plist e))))

(defun org-plist-delete (plist property)
  "Delete PROPERTY from PLIST.
This is in contrast to merely setting it to 0."
  (let (p)
    (while plist
      (if (not (eq property (car plist)))
	  (setq p (plist-put p (car plist) (nth 1 plist))))
      (setq plist (cddr plist)))
    p))

(defun org-combine-plists (&rest plists)
  "Create a single property list from all plists in PLISTS.
The process starts by copying the first list, and then setting properties
from the other lists.  Settings in the last list are the most significant
ones and overrule settings in the other lists."
  (let ((rtn (copy-sequence (pop plists)))
	p v ls)
    (while plists
      (setq ls (pop plists))
      (while ls
	(setq p (pop ls) v (pop ls))
	(setq rtn (plist-put rtn p v))))
    rtn))



;;; Local variables

(defconst org-unique-local-variables
  '(org-element--cache
    org-element--headline-cache
    org-element--cache-change-tic
    org-element--cache-last-buffer-size
    org-element--cache-change-warning
    org-element--cache-gapless
    org-element--cache-hash-left
    org-element--cache-hash-right
    org-element--cache-size
    org-element--headline-cache-size
    org-element--cache-sync-keys-value
    org-element--cache-diagnostics-ring
    org-element--cache-diagnostics-ring-size
    org-element--cache-sync-keys
    org-element--cache-sync-requests
    org-element--cache-sync-timer)
  "List of local variables that cannot be transferred to another buffer.")

(defun org-get-local-variables ()
  "Return a list of all local variables in an Org mode buffer."
  (delq nil
	(mapcar
	 (lambda (x)
	   (let* ((binding (if (symbolp x) (list x) (list (car x) (cdr x))))
		  (name (car binding)))
	     (and (not (get name 'org-state))
		  (not (memq name org-unique-local-variables))
		  (string-match-p
		   "\\`\\(org-\\|orgtbl-\\|outline-\\|comment-\\|paragraph-\\|\
auto-fill\\|normal-auto-fill\\|fill-paragraph\\|indent-\\)"
		   (symbol-name name))
		  binding)))
	 (with-temp-buffer
	   (org-mode)
	   (buffer-local-variables)))))

(defun org-clone-local-variables (from-buffer &optional regexp)
  "Clone local variables from FROM-BUFFER.
Optional argument REGEXP selects variables to clone."
  (dolist (pair (buffer-local-variables from-buffer))
    (pcase pair
      (`(,name . ,value)		;ignore unbound variables
       (when (and (not (memq name org-unique-local-variables))
		  (or (null regexp) (string-match-p regexp (symbol-name name))))
	 (ignore-errors (set (make-local-variable name) value)))))))


;;; Miscellaneous

(defsubst org-call-with-arg (command arg)
  "Call COMMAND interactively, but pretend prefix arg was ARG."
  (let ((current-prefix-arg arg)) (call-interactively command)))

(defsubst org-check-external-command (cmd &optional use no-error)
  "Check if external program CMD for USE exists, error if not.
When the program does exist, return its path.
When it does not exist and NO-ERROR is set, return nil.
Otherwise, throw an error.  The optional argument USE can describe what this
program is needed for, so that the error message can be more informative."
  (or (executable-find cmd)
      (if no-error
	  nil
	(error "Can't find `%s'%s" cmd
	       (if use (format " (%s)" use) "")))))

(defun org-display-warning (message)
  "Display the given MESSAGE as a warning."
  (display-warning 'org message :warning))

(defun org-unlogged-message (&rest args)
  "Display a message, but avoid logging it in the *Messages* buffer."
  (let ((message-log-max nil))
    (apply #'message args)))

(defmacro org-dlet (binders &rest body)
  "Like `let*' but using dynamic scoping."
  (declare (indent 1) (debug let))
  (let ((vars (mapcar (lambda (binder)
                        (if (consp binder) (car binder) binder))
                      binders)))
    `(progn
       (with-no-warnings
         ,@(mapcar (lambda (var) `(defvar ,var)) vars))
       (let* ,binders ,@body))))

(defmacro org-pushnew-to-end (val var)
  "Like `cl-pushnew' but pushes to the end of the list.
Uses `equal' for comparisons.

Beware: this performs O(N) memory allocations, so if you use it in a loop, you
get an unnecessary O(N²) space complexity, so you're usually better off using
`cl-pushnew' (with a final `reverse' if you care about the order of elements)."
  (declare (debug (form gv-place)))
  (let ((v (make-symbol "v")))
    `(let ((,v ,val))
       (unless (member ,v ,var)
         (setf ,var (append ,var (list ,v)))))))

(defun org-eval (form)
  "Eval FORM and return result."
  (condition-case-unless-debug error
      (eval form t)
    (error (format "%%![Error: %s]" error))))

(defvar org--headline-re-cache-no-bol nil
  "Plist holding association between headline level regexp.")
(defvar org--headline-re-cache-bol nil
  "Plist holding association between headline level regexp.")
(defsubst org-headline-re (true-level &optional no-bol)
  "Generate headline regexp for TRUE-LEVEL.
When NO-BOL is non-nil, regexp will not demand the regexp to start at
beginning of line."
  (or (plist-get
       (if no-bol
           org--headline-re-cache-no-bol
         org--headline-re-cache-bol)
       true-level)
      (let ((re (rx-to-string
                 (if no-bol
                     `(seq (** 1 ,true-level "*") " ")
                   `(seq line-start (** 1 ,true-level "*") " ")))))
        (if no-bol
            (setq org--headline-re-cache-no-bol
                  (plist-put
                   org--headline-re-cache-no-bol
                   true-level re))
          (setq org--headline-re-cache-bol
                (plist-put
                 org--headline-re-cache-bol
                 true-level re)))
        re)))

(defvar org-outline-regexp) ; defined in org.el
(defvar org-outline-regexp-bol) ; defined in org.el
(defvar org-odd-levels-only) ; defined in org.el
(defvar org-inlinetask-min-level) ; defined in org-inlinetask.el
(defun org-get-limited-outline-regexp (&optional with-bol)
  "Return outline-regexp with limited number of levels.
The number of levels is controlled by `org-inlinetask-min-level'.
Match at beginning of line when WITH-BOL is non-nil."
  (cond ((not (derived-mode-p 'org-mode))
         (if (string-prefix-p "^" outline-regexp)
             (if with-bol outline-regexp (substring outline-regexp 1))
           (if with-bol (concat "^" outline-regexp) outline-regexp)))
	((not (featurep 'org-inlinetask))
	 (if with-bol org-outline-regexp-bol org-outline-regexp))
	(t
	 (let* ((limit-level (1- org-inlinetask-min-level))
		(nstars (if org-odd-levels-only
			    (1- (* limit-level 2))
			  limit-level)))
           (org-headline-re nstars (not with-bol))))))

(defun org--line-empty-p (n)
  "Is the Nth next line empty?"
  (and (not (bobp))
       (save-excursion
	 (forward-line n)
         (skip-chars-forward " \t")
         (eolp))))

(defun org-previous-line-empty-p ()
  "Is the previous line a blank line?
When NEXT is non-nil, check the next line instead."
  (org--line-empty-p -1))

(defun org-next-line-empty-p ()
  "Is the previous line a blank line?
When NEXT is non-nil, check the next line instead."
  (org--line-empty-p 1))

(defun org-id-uuid ()
  "Return string with random (version 4) UUID."
  (let ((rnd (md5 (format "%s%s%s%s%s%s%s"
			  (random)
			  (org-time-convert-to-list nil)
			  (user-uid)
			  (emacs-pid)
			  (user-full-name)
			  user-mail-address
			  (recent-keys)))))
    (format "%s-%s-4%s-%s%s-%s"
	    (substring rnd 0 8)
	    (substring rnd 8 12)
	    (substring rnd 13 16)
	    (format "%x"
		    (logior
		     #b10000000
		     (logand
		      #b10111111
		      (string-to-number
		       (substring rnd 16 18) 16))))
	    (substring rnd 18 20)
	    (substring rnd 20 32))))


;;; Motion

(defsubst org-goto-line (N)
  (save-restriction
    (widen)
    (goto-char (point-min))
    (forward-line (1- N))))

(defsubst org-current-line (&optional pos)
  (save-excursion
    (and pos (goto-char pos))
    ;; works also in narrowed buffer, because we start at 1, not point-min
    (+ (if (bolp) 1 0) (count-lines 1 (point)))))



;;; Overlays and text properties

(defun org-overlay-display (ovl text &optional face evap)
  "Make overlay OVL display TEXT with face FACE."
  (overlay-put ovl 'display text)
  (when face (overlay-put ovl 'face face))
  (when evap (overlay-put ovl 'evaporate t)))

(defun org-overlay-before-string (ovl text &optional face evap)
  "Make overlay OVL display TEXT with face FACE."
  (when face (org-add-props text nil 'face face))
  (overlay-put ovl 'before-string text)
  (when evap (overlay-put ovl 'evaporate t)))

(defun org-find-overlays (prop &optional pos delete)
  "Find all overlays specifying PROP at POS or point.
If DELETE is non-nil, delete all those overlays."
  (let (found)
    (dolist (ov (overlays-at (or pos (point))) found)
      (cond ((not (overlay-get ov prop)))
	    (delete (delete-overlay ov))
	    (t (push ov found))))))

(defun org-find-text-property-region (pos prop)
  "Find a region around POS containing same non-nil value of PROP text property.
Return nil when PROP is not set at POS."
  (let* ((beg (and (get-text-property pos prop) pos))
	 (end beg))
    (when beg
      (unless (or (equal beg (point-min))
		  (not (eq (get-text-property beg prop)
			 (get-text-property (1- beg) prop))))
	(setq beg (previous-single-property-change pos prop nil (point-min))))
      (unless (or (equal end (point-max))
		  ;; (not (eq (get-text-property end prop)
		  ;; 	 (get-text-property (1+ end) prop)))
		  )
	(setq end (next-single-property-change pos prop nil (point-max))))
      (cons beg end))))


;;; Regexp matching

(defsubst org-pos-in-match-range (pos n)
  (and (match-beginning n)
       (<= (match-beginning n) pos)
       (>= (match-end n) pos)))

(defsubst org-skip-whitespace ()
  "Skip over space, tabs and newline characters."
  (skip-chars-forward " \t\n\r"))

(defun org-match-line (regexp)
  "Match REGEXP at the beginning of the current line."
  (save-excursion
    (forward-line 0)
    (looking-at regexp)))

(defun org-match-any-p (re list)
  "Non-nil if regexp RE matches an element in LIST."
  (cl-some (lambda (x) (string-match-p re x)) list))

(defun org-in-regexp (regexp &optional nlines visually)
  "Check if point is inside a match of REGEXP.

Normally only the current line is checked, but you can include
NLINES extra lines around point into the search.  If VISUALLY is
set, require that the cursor is not after the match but really
on, so that the block visually is on the match.

Return nil or a cons cell (BEG . END) where BEG and END are,
respectively, the positions at the beginning and the end of the
match."
  (catch :exit
    (let ((pos (point))
          (eol (line-end-position (if nlines (1+ nlines) 1))))
      (save-excursion
	(forward-line (- (or nlines 0)))
	(while (and (re-search-forward regexp eol t)
		    (<= (match-beginning 0) pos))
	  (let ((end (match-end 0)))
	    (when (or (> end pos) (and (= end pos) (not visually)))
	      (throw :exit (cons (match-beginning 0) (match-end 0))))))))))

(defun org-point-in-group (point group &optional context)
  "Check if POINT is in match-group GROUP.
If CONTEXT is non-nil, return a list with CONTEXT and the boundaries of the
match.  If the match group does not exist or point is not inside it,
return nil."
  (and (match-beginning group)
       (>= point (match-beginning group))
       (<= point (match-end group))
       (if context
	   (list context (match-beginning group) (match-end group))
	 t)))

(defun org-url-p (s)
  "Non-nil if string S is a URL."
  (require 'ffap)
  (and ffap-url-regexp (string-match-p ffap-url-regexp s)))

(defconst org-uuid-regexp
  "\\`[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}\\'"
  "Regular expression matching a universal unique identifier (UUID).")

(defun org-uuidgen-p (s)
  "Is S an ID created by UUIDGEN?"
  (string-match org-uuid-regexp (downcase s)))



;;; String manipulation

(defcustom org-sort-function #'string-collate-lessp
  "Function used to compare strings when sorting.
This function affects how Org mode sorts headlines, agenda items,
table lines, etc.

The function must accept either 2 or 4 arguments: strings to compare
and, optionally, LOCALE and IGNORE-CASE - locale name and flag to make
comparison case-insensitive.

The default value uses sorting rules according to OS language.  Users
who want to make sorting language-independent, may customize the value
to `org-sort-function-fallback'.

Note that some string sorting rules are known to be not accurate on
MacOS.  See https://debbugs.gnu.org/cgi/bugreport.cgi?bug=59275.
MacOS users may customize the value to
`org-sort-function-fallback'."
  :group 'org
  :package-version '(Org . "9.7")
  :type '(choice
          (const :tag "According to OS language" string-collate-lessp)
          (const :tag "Using string comparison" org-sort-function-fallback)
          (function :tag "Custom function")))

(defun org-sort-function-fallback (a b &optional _ ignore-case)
  "Return non-nil when downcased string A < string B.
Use `compare-strings' for comparison.  Honor IGNORE-CASE."
  (let ((ans (compare-strings a nil nil b nil nil ignore-case)))
    (cond
     ((and (numberp ans) (< ans 0)) t)
     (t nil))))

(defun org-string< (a b &optional locale ignore-case)
  "Return non-nil when string A < string B.
LOCALE is the locale name.  IGNORE-CASE, when non-nil, makes comparison
ignore case."
  (if (= 4 (cdr (func-arity org-sort-function)))
      (funcall org-sort-function a b locale ignore-case)
    (funcall org-sort-function a b)))

(defun org-string<= (a b &optional locale ignore-case)
  "Return non-nil when string A <= string B.
LOCALE is the locale name.  IGNORE-CASE, when non-nil, makes comparison
ignore case."
  (or (string= a b) (org-string< a b locale ignore-case)))

(defun org-string>= (a b &optional locale ignore-case)
  "Return non-nil when string A >= string B.
LOCALE is the locale name.  IGNORE-CASE, when non-nil, makes comparison
ignore case."
  (not (org-string< a b locale ignore-case)))

(defun org-string> (a b &optional locale ignore-case)
  "Return non-nil when string A > string B.
LOCALE is the locale name.  IGNORE-CASE, when non-nil, makes comparison
ignore case."
  (and (not (string= a b))
       (not (org-string< a b locale ignore-case))))

(defun org-string<> (a b)
  "Return non-nil when string A and string B are not equal."
  (not (string= a b)))

(defsubst org-trim (s &optional keep-lead)
  "Remove whitespace at the beginning and the end of string S.
When optional argument KEEP-LEAD is non-nil, removing blank lines
at the beginning of the string does not affect leading indentation."
  (replace-regexp-in-string
   (if keep-lead "\\`\\([ \t]*\n\\)+" "\\`[ \t\n\r]+") ""
   (replace-regexp-in-string "[ \t\n\r]+\\'" "" s)))

(defun org-string-nw-p (s)
  "Return S if S is a string containing a non-blank character.
Otherwise, return nil."
  (and (stringp s)
       (string-match-p "[^ \r\t\n]" s)
       s))

(defun org-reverse-string (string)
  "Return the reverse of STRING."
  (apply #'string (nreverse (string-to-list string))))

(defun org-split-string (string &optional separators)
  "Splits STRING into substrings at SEPARATORS.

SEPARATORS is a regular expression.  When nil, it defaults to
\"[ \\f\\t\\n\\r\\v]+\".

Unlike `split-string', matching SEPARATORS at the beginning and
end of string are ignored."
  (let ((separators (or separators "[ \f\t\n\r\v]+")))
    (if (not (string-match separators string)) (list string)
      (let ((i (match-end 0))
	    (results
	     (and (/= 0 (match-beginning 0)) ;skip leading separator
		  (list (substring string 0 (match-beginning 0))))))
	(while (string-match separators string i)
	  (push (substring string i (match-beginning 0))
		results)
	  (setq i (match-end 0)))
	(nreverse (if (= i (length string))
		      results		;skip trailing separator
		    (cons (substring string i) results)))))))

(defun org--string-from-props (s property beg end)
  "Return the visible part of string S.
Visible part is determined according to text PROPERTY, which is
either `invisible' or `display'.  BEG and END are 0-indices
delimiting S."
  (let ((width 0)
	(cursor beg))
    (while (setq beg (text-property-not-all beg end property nil s))
      (let* ((next (next-single-property-change beg property s end))
	     (spec (get-text-property beg property s))
	     (value
	      (pcase property
		(`invisible
		 ;; If `invisible' property means text is to be
		 ;; invisible, return 0.  Otherwise return nil so as
		 ;; to resume search.
		 (and (or (eq t buffer-invisibility-spec)
			  (assoc-string spec buffer-invisibility-spec))
		      0))
		(`display
		 (pcase spec
		   (`nil nil)
		   (`(space . ,props)
		    (let ((width (plist-get props :width)))
		      (and (wholenump width) width)))
		   (`(image . ,_)
                    (and (fboundp 'image-size)
                         (ceiling (car (image-size spec)))))
		   ((pred stringp)
		    ;; Displayed string could contain invisible parts,
		    ;; but no nested display.
		    (org--string-from-props spec 'invisible 0 (length spec)))
		   (_
		    ;; Un-handled `display' value.  Ignore it.
		    ;; Consider the original string instead.
		    nil)))
		(_ (error "Unknown property: %S" property)))))
	(when value
	  (cl-incf width
		   ;; When looking for `display' parts, we still need
		   ;; to look for `invisible' property elsewhere.
		   (+ (cond ((eq property 'display)
			     (org--string-from-props s 'invisible cursor beg))
			    ((= cursor beg) 0)
			    (t (string-width (substring s cursor beg))))
		      value))
	  (setq cursor next))
	(setq beg next)))
    (+ width
       ;; Look for `invisible' property in the last part of the
       ;; string.  See above.
       (cond ((eq property 'display)
	      (org--string-from-props s 'invisible cursor end))
	     ((= cursor end) 0)
	     (t (string-width (substring s cursor end)))))))

(defun org--string-width-1 (string)
  "Return width of STRING when displayed in the current buffer.
Unlike `string-width', this function takes into consideration
`invisible' and `display' text properties.  It supports the
latter in a limited way, mostly for combinations used in Org.
Results may be off sometimes if it cannot handle a given
`display' value."
  (org--string-from-props string 'display 0 (length string)))

(defun org-string-width (string &optional pixels default-face)
  "Return width of STRING when displayed in the current buffer.
Return width in pixels when PIXELS is non-nil.
When PIXELS is nil, DEFAULT-FACE is the face used to calculate relative
STRING width.  When REFERENCE-FACE is nil, `default' face is used."
  (if (and (version< emacs-version "28") (not pixels))
      ;; FIXME: Fallback to old limited version, because
      ;; `window-pixel-width' is buggy in older Emacs.
      (org--string-width-1 string)
    ;; Wrap/line prefix will make `window-text-pixel-size' return too
    ;; large value including the prefix.
    (remove-text-properties 0 (length string)
                            '(wrap-prefix t line-prefix t)
                            string)
    ;; Face should be removed to make sure that all the string symbols
    ;; are using default face with constant width.  Constant char width
    ;; is critical to get right string width from pixel width (not needed
    ;; when PIXELS are requested though).
    (unless pixels
      (put-text-property 0 (length string) 'face (or default-face 'default) string))
    (let (;; We need to remove the folds to make sure that folded table
          ;; alignment is not messed up.
          (current-invisibility-spec
           (or (and (not (listp buffer-invisibility-spec))
                    buffer-invisibility-spec)
               (let (result)
                 (dolist (el buffer-invisibility-spec)
                   (unless (or (memq el
                                     '(org-fold-drawer
                                       org-fold-block
                                       org-fold-outline))
                               (and (listp el)
                                    (memq (car el)
                                          '(org-fold-drawer
                                            org-fold-block
                                            org-fold-outline))))
                     (push el result)))
                 result)))
          (current-char-property-alias-alist char-property-alias-alist))
      (with-current-buffer (get-buffer-create " *Org string width*")
        (setq-local display-line-numbers nil)
        (setq-local line-prefix nil)
        (setq-local wrap-prefix nil)
        (setq-local buffer-invisibility-spec
                    (if (listp current-invisibility-spec)
                        (mapcar (lambda (el)
                                  ;; Consider ellipsis to have 0 width.
                                  ;; It is what Emacs 28+ does, but we have
                                  ;; to force it in earlier Emacs versions.
                                  (if (and (consp el) (cdr el))
                                      (list (car el))
                                    el))
                                current-invisibility-spec)
                      current-invisibility-spec))
        (setq-local char-property-alias-alist
                    current-char-property-alias-alist)
        (let (pixel-width symbol-width)
          (with-silent-modifications
            (erase-buffer)
            (insert string)
            (setq pixel-width (org-buffer-text-pixel-width))
            (unless pixels
              (erase-buffer)
              (insert (propertize "a" 'face (or default-face 'default)))
              (setq symbol-width (org-buffer-text-pixel-width))))
          (if pixels
              pixel-width
            (ceiling pixel-width symbol-width)))))))

(defmacro org-current-text-column ()
  "Like `current-column' but ignore display properties.
Throw an error when `tab-width' is not 8.

This function forces `tab-width' value because it is used as a part of
the parser, to ensure parser consistency when calculating list
indentation."
  `(progn
     (unless (= 8 tab-width) (error "Tab width in Org files must be 8, not %d.  Please adjust your `tab-width' settings for Org mode" tab-width))
     (string-width (buffer-substring-no-properties
                    (line-beginning-position) (point)))))

(defun org-not-nil (v)
  "If V not nil, and also not the string \"nil\", then return V.
Otherwise return nil."
  (and v (not (equal v "nil")) v))

(defun org-unbracket-string (pre post string)
  "Remove PRE/POST from the beginning/end of STRING.
Both PRE and POST must be pre-/suffixes of STRING, or neither is
removed.  Return the new string.  If STRING is nil, return nil."
  (declare (indent 2))
  (and string
       (if (and (string-prefix-p pre string)
		(string-suffix-p post string))
	   (substring string (length pre)
                      (and (not (string-equal "" post)) (- (length post))))
	 string)))

(defun org-strip-quotes (string)
  "Strip double quotes from around STRING, if applicable.
If STRING is nil, return nil."
  (org-unbracket-string "\"" "\"" string))

(defsubst org-current-line-string (&optional to-here)
  "Return current line, as a string.
If optional argument TO-HERE is non-nil, return string from
beginning of line up to point."
  (buffer-substring (line-beginning-position)
		    (if to-here (point) (line-end-position))))

(defun org-shorten-string (s maxlength)
  "Shorten string S so that it is no longer than MAXLENGTH characters.
If the string is shorter or has length MAXLENGTH, just return the
original string.  If it is longer, the functions finds a space in the
string, breaks this string off at that locations and adds three dots
as ellipsis.  Including the ellipsis, the string will not be longer
than MAXLENGTH.  If finding a good breaking point in the string does
not work, the string is just chopped off in the middle of a word
if necessary."
  (if (<= (length s) maxlength)
      s
    (let* ((n (max (- maxlength 4) 1))
	   (re (concat "\\`\\(.\\{1," (number-to-string n)
		       "\\}[^ ]\\)\\([ ]\\|\\'\\)")))
      (if (string-match re s)
	  (concat (match-string 1 s) "...")
	(concat (substring s 0 (max (- maxlength 3) 0)) "...")))))

(defun org-remove-tabs (s &optional width)
  "Replace tabulators in S with spaces.
Assumes that s is a single line, starting in column 0."
  (setq width (or width tab-width))
  (while (string-match "\t" s)
    (setq s (replace-match
	     (make-string
	      (- (* width (/ (+ (match-beginning 0) width) width))
		 (match-beginning 0)) ?\ )
	     t t s)))
  s)

(defun org-remove-blank-lines (s)
  "Remove blank lines in S."
  (replace-regexp-in-string (rx "\n" (1+ (0+ space) "\n")) "\n" s))

(defun org-wrap (string &optional width lines)
  "Wrap string to either a number of lines, or a width in characters.
If WIDTH is non-nil, the string is wrapped to that width, however many lines
that costs.  If there is a word longer than WIDTH, the text is actually
wrapped to the length of that word.
IF WIDTH is nil and LINES is non-nil, the string is forced into at most that
many lines, whatever width that takes.
The return value is a list of lines, without newlines at the end."
  (let* ((words (split-string string))
	 (maxword (apply #'max (mapcar #'org-string-width words)))
	 w ll)
    (cond (width
	   (org--do-wrap words (max maxword width)))
	  (lines
	   (setq w maxword)
	   (setq ll (org--do-wrap words maxword))
	   (if (<= (length ll) lines)
	       ll
	     (setq ll words)
	     (while (> (length ll) lines)
	       (setq w (1+ w))
	       (setq ll (org--do-wrap words w)))
	     ll))
	  (t (error "Cannot wrap this")))))

(defun org--do-wrap (words width)
  "Create lines of maximum width WIDTH (in characters) from word list WORDS."
  (let (lines line)
    (while words
      (setq line (pop words))
      (while (and words (< (+ (length line) (length (car words))) width))
	(setq line (concat line " " (pop words))))
      (setq lines (push line lines)))
    (nreverse lines)))

(defun org-remove-indentation (code &optional n)
  "Remove maximum common indentation in string CODE and return it.
N may optionally be the number of columns to remove.  Return CODE
as-is if removal failed."
  (with-temp-buffer
    (insert code)
    (if (org-do-remove-indentation n) (buffer-string) code)))

(defun org-fill-template (template alist)
  "Find each %key of ALIST in TEMPLATE and replace it."
  (let ((case-fold-search nil))
    (dolist (entry (sort (copy-sequence alist)
                         ; Sort from longest key to shortest, so that
                         ; "noweb-ref" and "tangle-mode" get processed
                         ; before "noweb" and "tangle", respectively.
                         (lambda (a b) (< (length (car b)) (length (car a))))))
      (setq template
	    (replace-regexp-in-string
	     (concat "%" (regexp-quote (car entry)))
	     (or (cdr entry) "") template t t)))
    template))

(defun org-replace-escapes (string table)
  "Replace %-escapes in STRING with values in TABLE.
TABLE is an association list with keys like \"%a\" and string values.
The sequences in STRING may contain normal field width and padding information,
for example \"%-5s\".  Replacements happen in the sequence given by TABLE,
so values can contain further %-escapes if they are define later in TABLE."
  (let ((tbl (copy-alist table))
	(case-fold-search nil)
        (pchg 0)
        re rpl)
    (dolist (e tbl)
      (setq re (concat "%-?[0-9.]*" (substring (car e) 1)))
      (when (and (cdr e) (string-match re (cdr e)))
        (let ((sref (substring (cdr e) (match-beginning 0) (match-end 0)))
              (safe (copy-sequence "SREF")))
          (add-text-properties 0 3 (list 'sref sref) safe)
          (setcdr e (replace-match safe t t (cdr e)))))
      (while (string-match re string)
        (setq rpl (format (concat (substring (match-string 0 string) 0 -1) "s")
                          (cdr e)))
        (setq string (replace-match rpl t t string))))
    (while (setq pchg (next-property-change pchg string))
      (let ((sref (get-text-property pchg 'sref string)))
	(when (and sref (string-match "SREF" string pchg))
	  (setq string (replace-match sref t t string)))))
    string))


;;; Text properties

(defconst org-rm-props '(invisible t face t keymap t intangible t mouse-face t
				   rear-nonsticky t mouse-map t fontified t
				   org-emphasis t)
  "Properties to remove when a string without properties is wanted.")

(defun org-buffer-substring-fontified (beg end)
  "Return fontified region between BEG and END."
  (when (bound-and-true-p jit-lock-mode)
    (when (text-property-not-all beg end 'fontified t)
      (save-excursion (save-match-data (font-lock-fontify-region beg end)))))
  (buffer-substring beg end))

(defun org-looking-at-fontified (re)
  "Call `looking-at' RE and make sure that the match is fontified."
  (prog1 (looking-at re)
    (when (bound-and-true-p jit-lock-mode)
      (when (text-property-not-all
             (match-beginning 0) (match-end 0)
             'fontified t)
        (save-excursion
          (save-match-data
            (font-lock-fontify-region (match-beginning 0)
                              (match-end 0))))))))

(defsubst org-no-properties (s &optional restricted)
  "Remove all text properties from string S.
When RESTRICTED is non-nil, only remove the properties listed
in `org-rm-props'."
  (if restricted (remove-text-properties 0 (length s) org-rm-props s)
    (set-text-properties 0 (length s) nil s))
  s)
(defun org-add-props (string plist &rest props)
  "Add text properties to entire string, from beginning to end.
PLIST may be a list of properties, PROPS are individual properties and values
that will be added to PLIST.  Returns the string that was modified."
  (declare (indent 2))
  (add-text-properties
   0 (length string) (if props (append plist props) plist) string)
  string)

(defun org-make-parameter-alist (plist)
  "Return alist based on PLIST.
PLIST is a property list with alternating symbol names and values.
The returned alist is a list of lists with the symbol name in `car'
and the value in `cadr'."
  (when plist
    (cons (list (car plist) (cadr plist))
	  (org-make-parameter-alist (cddr plist)))))

(defsubst org-get-at-bol (property)
  "Get text property PROPERTY at the beginning of line."
  (get-text-property (line-beginning-position) property))

(defun org-get-at-eol (property n)
  "Get text property PROPERTY at the end of line less N characters."
  (get-text-property (- (line-end-position) n) property))

(defun org-find-text-property-in-string (prop s)
  "Return the first non-nil value of property PROP in string S."
  (or (get-text-property 0 prop s)
      (get-text-property (or (next-single-property-change 0 prop s) 0)
			 prop s)))

;; FIXME: move to org-fold?
(defun org-invisible-p (&optional pos folding-only)
  "Non-nil if the character after POS is invisible.
If POS is nil, use `point' instead.  When optional argument
FOLDING-ONLY is non-nil, only consider invisible parts due to
folding of a headline, a block or a drawer, i.e., not because of
fontification."
  (let ((value (invisible-p (or pos (point)))))
    (cond ((not value) nil)
	  (folding-only (org-fold-folded-p (or pos (point))))
	  (t value))))

(defun org-truly-invisible-p ()
  "Check if point is at a character currently not visible.
This version does not only check the character property, but also
`visible-mode'."
  (unless (bound-and-true-p visible-mode)
    (org-invisible-p)))

(defun org-invisible-p2 ()
  "Check if point is at a character currently not visible.
If the point is at EOL (and not at the beginning of a buffer too),
move it back by one char before doing this check."
  (save-excursion
    (when (and (eolp) (not (bobp)))
      (backward-char 1))
    (org-invisible-p)))

(defun org-region-invisible-p (beg end)
  "Check if region if completely hidden."
  (org-with-wide-buffer
   (and (org-invisible-p beg)
        (org-invisible-p (org-fold-next-visibility-change beg end)))))

(defun org-find-visible ()
  "Return closest visible buffer position, or `point-max'."
  (if (org-invisible-p)
      (org-fold-next-visibility-change (point))
    (point)))

(defun org-find-invisible ()
  "Return closest invisible buffer position, or `point-max'."
  (if (org-invisible-p)
      (point)
    (org-fold-next-visibility-change (point))))


;;; Time

(defun org-2ft (s)
  "Convert S to a floating point time.
If S is already a number, just return it.  If it is a string,
parse it as a time string and apply `float-time' to it.  If S is
nil, just return 0."
  (cond
   ((numberp s) s)
   ((stringp s)
    (condition-case nil
	(org-time-string-to-seconds s)
      (error 0)))
   (t 0)))

(defun org-time= (a b)
  (let ((a (org-2ft a))
	(b (org-2ft b)))
    (and (> a 0) (> b 0) (= a b))))

(defun org-time< (a b)
  (let ((a (org-2ft a))
	(b (org-2ft b)))
    (and (> a 0) (> b 0) (< a b))))

(defun org-time<= (a b)
  (let ((a (org-2ft a))
	(b (org-2ft b)))
    (and (> a 0) (> b 0) (<= a b))))

(defun org-time> (a b)
  (let ((a (org-2ft a))
	(b (org-2ft b)))
    (and (> a 0) (> b 0) (> a b))))

(defun org-time>= (a b)
  (let ((a (org-2ft a))
	(b (org-2ft b)))
    (and (> a 0) (> b 0) (>= a b))))

(defun org-time<> (a b)
  (let ((a (org-2ft a))
	(b (org-2ft b)))
    (and (> a 0) (> b 0) (\= a b))))

(defmacro org-encode-time (&rest time)
  "Compatibility and convenience helper for `encode-time'.
TIME may be a 9 components list (SECONDS ... YEAR IGNORED DST ZONE)
as the recommended way since Emacs-27 or 6 or 9 separate arguments
similar to the only possible variant for Emacs-26 and earlier.
6 elements list as the only argument causes wrong type argument till
Emacs-29.

Warning: use -1 for DST to guess the actual value, nil means no
daylight saving time and may be wrong at particular time.

DST value is ignored prior to Emacs-27.  Since Emacs-27 DST value matters
even when multiple arguments is passed to this macro and such
behavior is different from `encode-time'.  See
Info node `(elisp)Time Conversion' for details and caveats,
preferably the latest version."
  (if (version< emacs-version "27.1")
      (if (cdr time)
          `(encode-time ,@time)
        `(apply #'encode-time ,@time))
    (if (ignore-errors (with-no-warnings (encode-time '(0 0 0 1 1 1971))))
        (pcase (length time) ; Emacs-29 since d75e2c12eb
          (1 `(encode-time ,@time))
          ((or 6 9) `(encode-time (list ,@time)))
          (_ (error "`org-encode-time' may be called with 1, 6, or 9 arguments but %d given"
                    (length time))))
      (pcase (length time)
        (1 `(encode-time ,@time))
        (6 `(encode-time (list ,@time nil -1 nil)))
        (9 `(encode-time (list ,@time)))
        (_ (error "`org-encode-time' may be called with 1, 6, or 9 arguments but %d given"
                  (length time)))))))

(defun org-parse-time-string (s &optional nodefault)
  "Parse Org time string S.

If time is not given, defaults to 0:00.  However, with optional
NODEFAULT, hour and minute fields are nil if not given.

Throw an error if S does not contain a valid Org time string.
Note that the first match for YYYY-MM-DD will be used (e.g.,
\"-52000-02-03\" will be taken as \"2000-02-03\").

This should be a lot faster than the `parse-time-string'."
  (unless (string-match org-ts-regexp0 s)
    (error "Not an Org time string: %s" s))
  (list 0
	(cond ((match-beginning 8) (string-to-number (match-string 8 s)))
	      (nodefault nil)
	      (t 0))
	(cond ((match-beginning 7) (string-to-number (match-string 7 s)))
	      (nodefault nil)
	      (t 0))
	(string-to-number (match-string 4 s))
	(string-to-number (match-string 3 s))
	(string-to-number (match-string 2 s))
	nil -1 nil))

(defun org-matcher-time (s)
  "Interpret a time comparison value S as a floating point time.

S can be an Org time stamp, a modifier, e.g., \"<+2d>\", or the
following special strings: \"<now>\", \"<today>\",
\"<tomorrow>\", and \"<yesterday>\".

Return 0. if S is not recognized as a valid value."
  (let ((today (float-time (org-encode-time
                            (append '(0 0 0) (nthcdr 3 (decode-time)))))))
    (save-match-data
      (cond
       ((string= s "<now>") (float-time))
       ((string= s "<today>") today)
       ((string= s "<tomorrow>") (+ 86400.0 today))
       ((string= s "<yesterday>") (- today 86400.0))
       ((string-match "\\`<\\([-+][0-9]+\\)\\([hdwmy]\\)>\\'" s)
	(+ (if (string= (match-string 2 s) "h") (float-time) today)
	   (* (string-to-number (match-string 1 s))
	      (cdr (assoc (match-string 2 s)
			  '(("h" . 3600.0)
			    ("d" . 86400.0)   ("w" . 604800.0)
			    ("m" . 2678400.0) ("y" . 31557600.0)))))))
       ((string-match org-ts-regexp0 s) (org-2ft s))
       (t 0.)))))


;;; Misc

(defun org-scroll (key &optional additional-keys)
  "Receive KEY and scroll the current window accordingly.
When ADDITIONAL-KEYS is not nil, also include SPC and DEL in the
allowed keys for scrolling, as expected in the export dispatch
window."
  (let ((scrlup (if additional-keys '(?\s ?\C-v) ?\C-v))
	(scrldn (if additional-keys `(?\d ?\M-v) ?\M-v)))
    (pcase key
      (?\C-n (if (not (pos-visible-in-window-p (point-max)))
	         (ignore-errors (scroll-up 1))
	       (message "End of buffer")
	       (sit-for 1)))
      (?\C-p (if (not (pos-visible-in-window-p (point-min)))
	         (ignore-errors (scroll-down 1))
	       (message "Beginning of buffer")
	       (sit-for 1)))
      ;; SPC or
      ((guard (memq key scrlup))
       (if (not (pos-visible-in-window-p (point-max)))
	   (scroll-up nil)
	 (message "End of buffer")
	 (sit-for 1)))
      ;; DEL
      ((guard (memq key scrldn))
       (if (not (pos-visible-in-window-p (point-min)))
	   (scroll-down nil)
	 (message "Beginning of buffer")
	 (sit-for 1))))))

(cl-defun org-knuth-hash (number &optional (base 32))
  "Calculate Knuth's multiplicative hash for NUMBER.
BASE is the maximum bitcount.
Credit: https://stackoverflow.com/questions/11871245/knuth-multiplicative-hash#41537995"
  (cl-assert (and (<= 0 base 32)))
  (ash (* number 2654435769) (- base 32)))

(defvar org-sxhash-hashes (make-hash-table :weakness 'key :test 'equal))
(defvar org-sxhash-objects (make-hash-table :weakness 'value))
(defun org-sxhash-safe (obj &optional counter)
  "Like `sxhash' for OBJ, but collision-free for in-memory objects.
When COUNTER is non-nil, return safe hash for (COUNTER . OBJ)."
  ;; Note: third-party code may modify OBJ by side effect.
  ;; Should not affect anything as long as `org-sxhash-safe'
  ;; is used to calculate hash.
  (or (and (not counter) (gethash obj org-sxhash-hashes))
      (let* ((hash (sxhash (if counter (cons counter obj) obj)))
	     (obj-old (gethash hash org-sxhash-objects)))
	(if obj-old ; collision
	    (org-sxhash-safe obj (if counter (1+ counter) 1))
	  ;; No collision.  Remember and return normal hash.
	  (puthash hash obj org-sxhash-objects)
	  (puthash obj hash org-sxhash-hashes)))))

(defun org-compile-file (source process ext &optional err-msg log-buf spec)
  "Compile a SOURCE file using PROCESS.

See `org-compile-file-commands' for information on PROCESS, EXT, and SPEC.
If PROCESS fails, an error will be raised.  The error message can
then be refined by providing string ERR-MSG, which is appended to
the standard message.

PROCESS must create a file with the same base name and directory
as SOURCE, but ending with EXT.  The function then returns its
filename.  Otherwise, it raises an error.

When PROCESS is a list of commands, optional argument LOG-BUF can
be set to a buffer or a buffer name.  `shell-command' then uses
it for output."
  (let* ((commands (org-compile-file-commands source process ext spec err-msg))
         (output (concat (file-name-sans-extension source) "." ext))
         ;; Resolve symlinks in default-directory to correctly handle
         ;; absolute source paths or relative paths with ..
         (relname (if (file-name-absolute-p source)
                      (let ((pwd (file-truename default-directory)))
                        (file-relative-name source pwd))
                    source))
         (log-buf (and log-buf (get-buffer-create log-buf)))
         (time (file-attribute-modification-time (file-attributes output))))
    (save-window-excursion
      (dolist (command commands)
        (cond
         ((functionp command)
          (funcall command (shell-quote-argument relname)))
         ((stringp command)
          (let ((shell-command-dont-erase-buffer t))
            (shell-command command log-buf))))))
    ;; Check for process failure.  Output file is expected to be
    ;; located in the same directory as SOURCE.
    (unless (org-file-newer-than-p output time)
      (ignore (defvar org-batch-test))
      ;; Display logs when running tests.
      (when (bound-and-true-p org-batch-test)
        (message "org-compile-file log ::\n-----\n%s\n-----\n"
                 (with-current-buffer log-buf (buffer-string))))
      (error
       (format
        "File %S wasn't produced%s"
        output
        (if (org-string-nw-p err-msg)
            (concat "  " (org-trim err-msg))
          err-msg))))
    output))

(defun org-compile-file-commands (source process ext &optional spec err-msg)
  "Return list of commands used to compile SOURCE file.

The commands are formed from PROCESS, which is either a function or
a list of shell commands, as strings.  EXT is a file extension, without
the leading dot, as a string.  After PROCESS has been executed,
a file with the same basename and directory as SOURCE but with the
file extension EXT is expected to be produced.
Failure to produce this file will be interpreted as PROCESS failing.

If PROCESS is a function, it is called with a single argument:
the SOURCE file.

If PROCESS is a list of commands, each of them is called using
`shell-command'.  By default, in each command, %b, %f, %F, %o and
%O are replaced with, respectively, SOURCE base name, relative
file name, absolute file name, relative directory and absolute
output file name.  It is possible, however, to use more
place-holders by specifying them in optional argument SPEC, as an
alist following the pattern

  (CHARACTER . REPLACEMENT-STRING).

Throw an error if PROCESS does not satisfy the described patterns.
The error string will be appended with ERR-MSG, when it is a string."
  (let* ((basename (file-name-base source))
         ;; Resolve symlinks in default-directory to correctly handle
         ;; absolute source paths or relative paths with ..
         (pwd (file-truename default-directory))
         (absname (expand-file-name source pwd))
         (relname (if (file-name-absolute-p source)
                        (file-relative-name source pwd)
                      source))
	 (relpath (or (file-name-directory relname) "./"))
	 (output (concat (file-name-sans-extension absname) "." ext))
	 (err-msg (if (stringp err-msg) (concat ".  " err-msg) "")))
    (pcase process
      ((pred functionp) (list process))
      ((pred consp)
       (let ((spec (append spec
			   `((?b . ,(shell-quote-argument basename))
			     (?f . ,(shell-quote-argument relname))
			     (?F . ,(shell-quote-argument absname))
			     (?o . ,(shell-quote-argument relpath))
			     (?O . ,(shell-quote-argument output))))))
         (mapcar (lambda (command) (format-spec command spec)) process)))
      (_ (error "No valid command to process %S%s" source err-msg)))))

(defun org-display-buffer-split (buffer alist)
  "Display BUFFER in the current frame split in two parts.
The frame will display two buffers - current buffer and BUFFER.
ALIST is an association list of action symbols and values.  See
Info node `(elisp) Buffer Display Action Alists' for details of
such alists.

Use `display-buffer-in-direction' internally.

This is an action function for buffer display, see Info
node `(elisp) Buffer Display Action Functions'.  It should be
called only by `display-buffer' or a function directly or
indirectly called by the latter."
  (let ((window-configuration (current-window-configuration)))
    (ignore-errors (delete-other-windows))
    (or (display-buffer-in-direction buffer alist)
        (display-buffer-pop-up-window buffer alist)
        (prog1 nil
          (set-window-configuration window-configuration)))))

(defun org-display-buffer-in-window (buffer alist)
  "Display BUFFER in specific window.
The window is defined according to the `window' slot in the ALIST.
Then `same-frame' slot in the ALIST is set, only display buffer when
window is present in the current frame.

This is an action function for buffer display, see Info
node `(elisp) Buffer Display Action Functions'.  It should be
called only by `display-buffer' or a function directly or
indirectly called by the latter."
  (let ((window (alist-get 'window alist)))
    (when (and window
               (window-live-p window)
               (or (not (alist-get 'same-frame alist))
                   (eq (window-frame) (window-frame window))))
      (window--display-buffer buffer window 'reuse alist))))

(provide 'org-macs)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; org-macs.el ends here
