;;; ibuf-ext.el --- extensions for ibuffer  -*- lexical-binding:t -*-

;; Copyright (C) 2000-2022 Free Software Foundation, Inc.

;; Author: Colin Walters <walters@verbum.org>
;; Maintainer: John Paul Wallington <jpw@gnu.org>
;; Created: 2 Dec 2001
;; Keywords: buffer, convenience
;; Package: ibuffer

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

;; These functions should be automatically loaded when called, but you
;; can explicitly (require 'ibuf-ext) in your ~/.emacs to have them
;; preloaded.
;;
;; For details on the structure of ibuffer filters and filter groups,
;; see the documentation for variables `ibuffer-filtering-qualifiers',
;; `ibuffer-filter-groups', and `ibuffer-saved-filters' in that order.
;; The variable `ibuffer-filtering-alist' contains names and
;; descriptions of the currently defined filters; also see the macro
;; `define-ibuffer-filter'.

;;; Code:

(require 'ibuffer)

(eval-when-compile
  (require 'ibuf-macs)
  (require 'cl-lib)
  (require 'subr-x))

;;; Utility functions
(defun ibuffer-remove-alist (key alist)
  "Remove all entries in ALIST that have a key equal to KEY."
  (while (ibuffer-awhen (assoc key alist)
           (setq alist (remove it alist)) it))
  alist)

;; borrowed from Gnus
(defun ibuffer-remove-duplicates (list)
  "Return a copy of LIST with duplicate elements removed."
  (let ((new nil)
	(tail list))
    (while tail
      (or (member (car tail) new)
	  (setq new (cons (car tail) new)))
      (setq tail (cdr tail)))
    (nreverse new)))

(defun ibuffer-split-list (ibuffer-split-list-fn ibuffer-split-list-elts)
  (let ((hip-crowd nil)
	(lamers nil))
    (dolist (ibuffer-split-list-elt ibuffer-split-list-elts)
      (if (funcall ibuffer-split-list-fn ibuffer-split-list-elt)
	  (push ibuffer-split-list-elt hip-crowd)
	(push ibuffer-split-list-elt lamers)))
    ;; Too bad Emacs Lisp doesn't have multiple values.
    (list (nreverse hip-crowd) (nreverse lamers))))

(defcustom ibuffer-never-show-predicates nil
  "A list of predicates (a regexp or function) for buffers not to display.
If a regexp, then it will be matched against the buffer's name.
If a function, it will be called with the buffer as an argument, and
should return non-nil if this buffer should not be shown."
  :type '(repeat (choice regexp function))
  :require 'ibuf-ext
  :group 'ibuffer)

(defcustom ibuffer-always-show-predicates nil
  "A list of predicates (a regexp or function) for buffers to always display.
If a regexp, then it will be matched against the buffer's name.
If a function, it will be called with the buffer as an argument, and
should return non-nil if this buffer should be shown.
Note that buffers matching one of these predicates will be shown
regardless of any active filters in this buffer."
  :type '(repeat (choice regexp function))
  :group 'ibuffer)

(defcustom ibuffer-never-search-content-name
  (let* ((names    '("Completions" "Help" "Messages" "Pp Eval Output"
                     "CompileLog" "Info" "Buffer List" "Ibuffer" "Apropos"))
         (partial  '("Customize Option: " "Async Shell Command\\*"
                     "Shell Command Output\\*" "ediff "))
         (beg      "\\`\\*")
         (end      "\\*\\'")
         (excluded (mapcar (lambda (x)
                             (format "%s%s" beg x)) partial)))
    (dolist (str names (nreverse excluded))
      (push (format "%s%s%s" beg str end) excluded)))
  "A list of regexps for buffers ignored by `ibuffer-mark-by-content-regexp'.
Buffers whose name matches a regexp in this list, are not searched."
  :version "26.1"
  :type '(repeat regexp)
  :require 'ibuf-ext
  :group 'ibuffer)

(defcustom ibuffer-never-search-content-mode '(dired-mode)
  "A list of major modes ignored by `ibuffer-mark-by-content-regexp'.
Buffers whose major mode is in this list, are not searched."
  :version "26.1"
  :type '(repeat (symbol :tag "Major mode"))
  :require 'ibuf-ext
  :group 'ibuffer)

(defvar ibuffer-tmp-hide-regexps nil
  "A list of regexps which should match buffer names to not show.")

(defvar ibuffer-tmp-show-regexps nil
  "A list of regexps which should match buffer names to always show.")

(defvar ibuffer-auto-buffers-changed nil)

(defun ibuffer-update-saved-filters-format (filters)
  "Transform alist from old to new `ibuffer-saved-filters' format.

Specifically, converts old-format alist with values of the
form (STRING (FILTER-SPECS...)) to alist with values of the
form (STRING FILTER-SPECS...), where each filter spec should be a
cons cell with a symbol in the car.  Any elements in the latter
form are kept as is.

Returns (OLD-FORMAT-DETECTED . UPDATED-SAVED-FILTERS-LIST)."
  (when filters
    (let* ((old-format-detected nil)
           (fix-filter (lambda (filter-spec)
                         (if (symbolp (car (cadr filter-spec)))
                             filter-spec
                           (setq old-format-detected t) ; side-effect
                           (cons (car filter-spec) (cadr filter-spec)))))
           (fixed (mapcar fix-filter filters)))
      (cons old-format-detected fixed))))

(defcustom ibuffer-saved-filters '(("programming"
                                    (or (derived-mode . prog-mode)
                                        (mode         . ess-mode)
                                        (mode         . compilation-mode)))
                                   ("text document"
                                    (and (derived-mode . text-mode)
                                         (not (starred-name))))
                                   ("TeX"
                                    (or (derived-mode . tex-mode)
                                        (mode         . latex-mode)
                                        (mode         . context-mode)
                                        (mode         . ams-tex-mode)
                                        (mode         . bibtex-mode)))
                                   ("web"
                                    (or (derived-mode . sgml-mode)
                                        (derived-mode . css-mode)
                                        (mode         . javascript-mode)
                                        (mode         . js2-mode)
                                        (mode         . scss-mode)
                                        (derived-mode . haml-mode)
                                        (mode         . sass-mode)))
                                   ("gnus"
                                    (or (mode . message-mode)
                                        (mode . mail-mode)
                                        (mode . gnus-group-mode)
                                        (mode . gnus-summary-mode)
                                        (mode . gnus-article-mode))))

  "An alist mapping saved filter names to filter specifications.

Each element should look like (\"NAME\" . FILTER-LIST), where
FILTER-LIST has the same structure as the variable
`ibuffer-filtering-qualifiers', which see.  The filters defined
here are joined with an implicit logical `and' and associated
with NAME.  The combined specification can be used by name in
other filter specifications via the `saved' qualifier (again, see
`ibuffer-filtering-qualifiers'). They can also be switched to by
name (see the functions `ibuffer-switch-to-saved-filters' and
`ibuffer-save-filters').  The variable `ibuffer-save-with-custom'
affects how this information is saved for future sessions.  This
variable can be set directly from lisp code."
  :version "26.1"
  :type '(alist :key-type (string :tag "Filter name")
                :value-type (repeat :tag "Filter specification" sexp))
  :set (lambda (symbol value)
         ;; Just set-default but update legacy old-style format
         (set-default symbol (cdr (ibuffer-update-saved-filters-format value))))
  :group 'ibuffer)

(defvar ibuffer-old-saved-filters-warning
  (concat "Deprecated format detected for variable `ibuffer-saved-filters'.

The format has been repaired and the variable modified accordingly.
You can save the current value through the customize system by
either clicking or hitting return "
          (make-text-button
           "here" nil
           'face '(:weight bold :inherit button)
           'mouse-face '(:weight normal :background "gray50" :inherit button)
           'follow-link t
           'help-echo "Click or RET: save new value in customize"
           'action (lambda (_)
                     (customize-save-variable 'ibuffer-saved-filters
                                              ibuffer-saved-filters)
                     (message "Saved updated ibuffer-saved-filters.")))
          ".  See below for
an explanation and alternative ways to save the repaired value.

Explanation: For the list variable `ibuffer-saved-filters',
elements of the form (STRING (FILTER-SPECS...)) are deprecated
and should instead have the form (STRING FILTER-SPECS...), where
each filter spec is a cons cell with a symbol in the car.  See
`ibuffer-saved-filters' for details.  The repaired value fixes
this format without changing the meaning of the saved filters.

Alternative ways to save the repaired value:

  1. Do M-x customize-variable and entering `ibuffer-saved-filters'
     when prompted.

  2. Set the updated value manually by copying the
     following emacs-lisp form to your emacs init file.

%s
"))

(defvar ibuffer-filtering-qualifiers nil
  "A list specifying the filters currently acting on the buffer list.

If this list is nil, then no filters are currently in
effect.  Otherwise, each element of this list specifies a single
filter, and all of the specified filters in the list are applied
successively to the buffer list.

Each filter specification can be of two types: simple or compound.

A simple filter specification has the form (SYMBOL . QUALIFIER),
where SYMBOL is a key in the alist `ibuffer-filtering-alist' that
determines the filter function to use and QUALIFIER is the data
passed to that function (along with the buffer being considered).

A compound filter specification can have one of four forms:

-- (not FILTER-SPEC)

   Represents the logical complement of FILTER-SPEC, which
   is any single filter specification, simple or compound.
   The form (not . FILTER-SPEC) is also accepted here.

-- (and FILTER-SPECS...)

   Represents the logical-and of the filters defined by one or
   more filter specifications FILTER-SPECS..., where each
   specification can be simple or compound.  Note that and is
   implicitly applied to the filters in the top-level list.

-- (or FILTER-SPECS...)

   Represents the logical-or of the filters defined by one or
   more filter specifications FILTER-SPECS..., where each
   specification can be simple or compound.

-- (saved . \"NAME\")

   Represents the filter saved under the string NAME
   in the alist `ibuffer-saved-filters'.  It is an
   error to name a filter that has not been saved.

This variable is local to each ibuffer buffer.")

;; This is now frobbed by `define-ibuffer-filter'.
(defvar ibuffer-filtering-alist nil
  "An alist of (SYMBOL DESCRIPTION FUNCTION) which describes a filter.

You most likely do not want to modify this variable directly; see
`define-ibuffer-filter'.

SYMBOL is the symbolic name of the filter.  DESCRIPTION is used when
displaying information to the user.  FUNCTION is given a buffer and
the value of the qualifier, and returns non-nil if and only if the
buffer should be displayed.")

(defcustom ibuffer-filter-format-alist nil
  "An alist which has special formats used when a filter is active.
The contents of this variable should look like:
 ((FILTER (FORMAT FORMAT ...)) (FILTER (FORMAT FORMAT ...)) ...)

For example, suppose that when you add a filter for buffers whose
major mode is `emacs-lisp-mode', you only want to see the mark and the
name of the buffer.  You could accomplish that by adding:
 (mode ((mark \" \" name)))
to this variable."
  :type '(repeat (list :tag "Association" (symbol :tag "Filter")
                       (list :tag "Formats" (repeat (sexp :tag "Format")))))
  :group 'ibuffer)

(defvar ibuffer-cached-filter-formats nil)
(defvar ibuffer-compiled-filter-formats nil)

(defvar ibuffer-filter-groups nil
  "An alist giving this buffer's active filter groups, or nil if none.

This alist maps filter group labels to filter specification
lists.  Each element has the form (\"LABEL\" FILTER-SPECS...),
where FILTER-SPECS... represents one or more filter
specifications of the same form as allowed as elements of
`ibuffer-filtering-qualifiers'.

Each filter group is displayed as a separate section in the
ibuffer list, headed by LABEL and displaying only the buffers
that pass through all the filters associated with NAME in this
list.")

(defcustom ibuffer-show-empty-filter-groups t
  "If non-nil, then show the names of filter groups which are empty."
  :type 'boolean
  :group 'ibuffer)

(defcustom ibuffer-saved-filter-groups nil
  "An alist of filtering groups to switch between.

Each element is of the form (\"NAME\" . FILTER-GROUP-LIST),
where NAME is a unique but arbitrary name and FILTER-GROUP-LIST
is a list of filter groups with the same structure as
allowed for `ibuffer-filter-groups'.

For instance:

  (setq ibuffer-saved-filter-groups
        \\='((\"Home\"
           (\"Modified\" (predicate buffer-modified-p (current-buffer)))
           (\"Helm\" (name . \"\\\\*helm.+\"))
           (\"Dev\" (or (filename . \".+\\\\.css\\\\'\")
                      (filename . \".+\\\\.html?\\\\'\")
                      (mode . android-mode)
                      (mode . clojure-mode))))))

See also the functions `ibuffer-save-filter-groups' and
`ibuffer-switch-to-saved-filter-groups' for saving and switching
between sets of filter groups, and the variable
`ibuffer-save-with-custom' that affects how this information is
saved."
  :type '(repeat sexp)
  :group 'ibuffer)

(defvar ibuffer-hidden-filter-groups nil
  "The list of filter groups that are currently hidden.")

(defvar ibuffer-filter-group-kill-ring nil)

(defcustom ibuffer-old-time 72
  "The number of hours before a buffer is considered \"old\"."
  :type '(choice (const :tag "72 hours (3 days)" 72)
 		 (const :tag "48 hours (2 days)" 48)
 		 (const :tag "24 hours (1 day)" 24)
		 (integer :tag "hours"))
  :group 'ibuffer)

(defcustom ibuffer-save-with-custom t
  "If non-nil, then use Custom to save interactively changed variables.
Currently, this only applies to `ibuffer-saved-filters' and
`ibuffer-saved-filter-groups'."
  :type 'boolean
  :group 'ibuffer)

(defun ibuffer-repair-saved-filters ()
  "Update `ibuffer-saved-filters' to its new-style format, if needed.

If this list has any elements of the old-style format, a
deprecation warning is raised, with a button allowing persistent
update.  Any updated filters retain their meaning in the new
format.  See `ibuffer-update-saved-filters-format' and
`ibuffer-saved-filters' for details of the old and new formats."
  (interactive)
  (when (and (boundp 'ibuffer-saved-filters) ibuffer-saved-filters)
    (let ((fixed (ibuffer-update-saved-filters-format ibuffer-saved-filters)))
      (prog1
          (setq ibuffer-saved-filters (cdr fixed))
        (when-let* ((old-format-detected (car fixed)))
          (let ((warning-series t)
                (updated-form
                 (with-output-to-string
                   (pp `(setq ibuffer-saved-filters ',ibuffer-saved-filters)))))
            (display-warning
             'ibuffer
             (format ibuffer-old-saved-filters-warning updated-form))))))))

(defun ibuffer-ext-visible-p (buf all &optional ibuffer-buf)
  (or
   (ibuffer-buf-matches-predicates buf ibuffer-tmp-show-regexps)
   (and (not
	 (or
	  (ibuffer-buf-matches-predicates buf ibuffer-tmp-hide-regexps)
	  (ibuffer-buf-matches-predicates buf ibuffer-never-show-predicates)))
	(or all
	    (not
	     (ibuffer-buf-matches-predicates buf ibuffer-maybe-show-predicates)))
	(or ibuffer-view-ibuffer
	    (and ibuffer-buf
		 (not (eq ibuffer-buf buf))))
	(or
	 (ibuffer-included-in-filters-p buf ibuffer-filtering-qualifiers)
	 (ibuffer-buf-matches-predicates buf ibuffer-always-show-predicates)))))

;;;###autoload
(define-minor-mode ibuffer-auto-mode
  "Toggle use of Ibuffer's auto-update facility (Ibuffer Auto mode)."
  :lighter nil
  (unless (derived-mode-p 'ibuffer-mode)
    (error "This buffer is not in Ibuffer mode"))
  (cond (ibuffer-auto-mode
         (frame-or-buffer-changed-p 'ibuffer-auto-buffers-changed) ; Initialize state vector
         (add-hook 'post-command-hook 'ibuffer-auto-update-changed))
        (t
         (remove-hook 'post-command-hook 'ibuffer-auto-update-changed))))

(defun ibuffer-auto-update-changed ()
  (when (frame-or-buffer-changed-p 'ibuffer-auto-buffers-changed)
    (dolist (buf (buffer-list))
      (ignore-errors
	(with-current-buffer buf
	  (when (and ibuffer-auto-mode
		     (derived-mode-p 'ibuffer-mode))
	    (ibuffer-update nil t)))))))

;;;###autoload
(defun ibuffer-mouse-filter-by-mode (event)
  "Enable or disable filtering by the major mode chosen via mouse."
  (interactive "e")
  (ibuffer-interactive-filter-by-mode event))

;;;###autoload
(defun ibuffer-interactive-filter-by-mode (event-or-point)
  "Enable or disable filtering by the major mode at point."
  (interactive "d")
  (if (eventp event-or-point)
      (posn-set-point (event-end event-or-point))
    (goto-char event-or-point))
  (let ((buf (ibuffer-current-buffer)))
    (if (assq 'mode ibuffer-filtering-qualifiers)
	(setq ibuffer-filtering-qualifiers
	      (ibuffer-remove-alist 'mode ibuffer-filtering-qualifiers))
      (ibuffer-push-filter (cons 'mode (buffer-local-value 'major-mode buf)))))
  (ibuffer-update nil t))

;;;###autoload
(defun ibuffer-mouse-toggle-filter-group (event)
  "Toggle the display status of the filter group chosen with the mouse."
  (interactive "e")
  (ibuffer-toggle-filter-group-1 (save-excursion
				   (mouse-set-point event)
				   (point))))

;;;###autoload
(defun ibuffer-toggle-filter-group ()
  "Toggle the display status of the filter group on this line."
  (interactive)
  (ibuffer-toggle-filter-group-1 (point)))

(defun ibuffer-toggle-filter-group-1 (posn)
  (let ((name (get-text-property posn 'ibuffer-filter-group-name)))
    (unless (stringp name)
      (error "No filtering group name present"))
    (if (member name ibuffer-hidden-filter-groups)
	(setq ibuffer-hidden-filter-groups
	      (delete name ibuffer-hidden-filter-groups))
      (push name ibuffer-hidden-filter-groups))
    (ibuffer-update nil t)))

;;;###autoload
(defun ibuffer-forward-filter-group (&optional count)
  "Move point forwards by COUNT filtering groups."
  (interactive "P")
  (unless count
    (setq count 1))
  (when (> count 0)
    (when (get-text-property (point) 'ibuffer-filter-group-name)
      (goto-char (next-single-property-change
		  (point) 'ibuffer-filter-group-name
		  nil (point-max))))
    (goto-char (next-single-property-change
		(point) 'ibuffer-filter-group-name
		nil (point-max)))
    (ibuffer-forward-filter-group (1- count)))
  (ibuffer-forward-line 0))

;;;###autoload
(defun ibuffer-backward-filter-group (&optional count)
  "Move point backwards by COUNT filtering groups."
  (interactive "P")
  (unless count
    (setq count 1))
  (when (> count 0)
    (when (get-text-property (point) 'ibuffer-filter-group-name)
      (goto-char (previous-single-property-change
		  (point) 'ibuffer-filter-group-name
		  nil (point-min))))
    (goto-char (previous-single-property-change
		(point) 'ibuffer-filter-group-name
		nil (point-min)))
    (ibuffer-backward-filter-group (1- count)))
  (when (= (point) (point-min))
    (goto-char (point-max))
    (ibuffer-backward-filter-group 1))
  (ibuffer-forward-line 0))

(defun ibuffer--maybe-erase-shell-cmd-output ()
  (let ((buf (get-buffer shell-command-buffer-name)))
    (when (and (buffer-live-p buf)
               (not shell-command-dont-erase-buffer)
               (not (zerop (buffer-size buf))))
      (with-current-buffer buf (erase-buffer)))))

;;;###autoload (autoload 'ibuffer-do-shell-command-pipe "ibuf-ext")
(define-ibuffer-op shell-command-pipe (command)
  "Pipe the contents of each marked buffer to shell command COMMAND."
  (:interactive "sPipe to shell command: "
   :opstring "Shell command executed on"
   :before (ibuffer--maybe-erase-shell-cmd-output)
   :modifier-p nil)
  (let ((out-buf (get-buffer-create shell-command-buffer-name)))
    (with-current-buffer out-buf (goto-char (point-max)))
    (call-shell-region (point-min) (point-max)
                       command nil out-buf)))

;;;###autoload (autoload 'ibuffer-do-shell-command-pipe-replace "ibuf-ext")
(define-ibuffer-op shell-command-pipe-replace (command)
  "Replace the contents of marked buffers with output of pipe to COMMAND."
  (:interactive "sPipe to shell command (replace): "
   :opstring "Buffer contents replaced in"
   :active-opstring "replace buffer contents in"
   :dangerous t
   :modifier-p t)
  (call-shell-region (point-min) (point-max)
                     command 'delete buf))

;;;###autoload (autoload 'ibuffer-do-shell-command-file "ibuf-ext")
(define-ibuffer-op shell-command-file (command)
  "Run shell command COMMAND separately on files of marked buffers."
  (:interactive "sShell command on buffer's file: "
   :opstring "Shell command executed on"
   :before (ibuffer--maybe-erase-shell-cmd-output)
   :modifier-p nil)
  (let ((file (and (not (buffer-modified-p))
                   buffer-file-name))
        (out-buf (get-buffer-create shell-command-buffer-name)))
    (unless (and file (file-exists-p file))
      (setq file
            (make-temp-file
             (substring
              (buffer-name) 0
              (min 10 (length (buffer-name))))))
      (write-region nil nil file nil 0))
    (with-current-buffer out-buf (goto-char (point-max)))
    (call-process-shell-command
     (format "%s %s"
             command
             (shell-quote-argument file))
     nil out-buf nil)))

;;;###autoload (autoload 'ibuffer-do-eval "ibuf-ext")
(define-ibuffer-op eval (form)
  "Evaluate FORM in each of the buffers.
Does not display the buffer during evaluation.
See `ibuffer-do-view-and-eval' for that."
  (:interactive
   (list (read--expression "Eval in buffers (form): "))
   :opstring "evaluated in"
   :modifier-p :maybe)
  (eval form))

;;;###autoload (autoload 'ibuffer-do-view-and-eval "ibuf-ext")
(define-ibuffer-op view-and-eval (form)
  "Evaluate FORM while displaying each of the marked buffers.
To evaluate a form without viewing the buffer, see `ibuffer-do-eval'."
  (:interactive
   (list
    (read-from-minibuffer
     "Eval viewing in buffers (form): "
     nil read-expression-map t 'read-expression-history))
   :opstring "evaluated in"
   :complex t
   :modifier-p :maybe)
  (let ((ibuffer-buf (current-buffer)))
    (unwind-protect
	(progn
	  (switch-to-buffer buf)
	  (eval form))
      (switch-to-buffer ibuffer-buf))))

;;;###autoload (autoload 'ibuffer-do-rename-uniquely "ibuf-ext")
(define-ibuffer-op rename-uniquely ()
  "Rename marked buffers as with `rename-uniquely'."
  (:opstring "renamed"
   :modifier-p t)
  (rename-uniquely))

;;;###autoload (autoload 'ibuffer-do-revert "ibuf-ext")
(define-ibuffer-op revert ()
  "Revert marked buffers as with `revert-buffer'."
  (:dangerous t
   :opstring "reverted"
   :active-opstring "revert"
   :modifier-p :maybe)
  (revert-buffer t t))

;;;###autoload (autoload 'ibuffer-do-isearch "ibuf-ext")
(define-ibuffer-op ibuffer-do-isearch ()
  "Perform a `isearch-forward' in marked buffers."
  (:interactive ()
   :opstring "searched in"
   :complex t
   :modifier-p :maybe)
  (multi-isearch-buffers (ibuffer-get-marked-buffers)))

;;;###autoload (autoload 'ibuffer-do-isearch-regexp "ibuf-ext")
(define-ibuffer-op ibuffer-do-isearch-regexp ()
  "Perform a `isearch-forward-regexp' in marked buffers."
  (:interactive ()
   :opstring "searched regexp in"
   :complex t
   :modifier-p :maybe)
  (multi-isearch-buffers-regexp (ibuffer-get-marked-buffers)))

;;;###autoload (autoload 'ibuffer-do-replace-regexp "ibuf-ext")
(define-ibuffer-op replace-regexp (from-str to-str)
  "Perform a `replace-regexp' in marked buffers."
  (:interactive
   (let* ((from-str (read-from-minibuffer "Replace regexp: "))
	  (to-str (read-from-minibuffer (concat "Replace " from-str
						" with: "))))
     (list from-str to-str))
   :opstring "replaced in"
   :complex t
   :modifier-p :maybe)
  (save-window-excursion
    (switch-to-buffer buf)
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search ibuffer-case-fold-search))
	(while (re-search-forward from-str nil t)
	  (replace-match to-str))))
    t))

;;;###autoload (autoload 'ibuffer-do-query-replace "ibuf-ext")
(define-ibuffer-op query-replace (&rest args)
  "Perform a `query-replace' in marked buffers."
  (:interactive
   (query-replace-read-args "Query replace" t t)
   :opstring "replaced in"
   :complex t
   :modifier-p :maybe)
  (save-window-excursion
    (switch-to-buffer buf)
    (save-excursion
      (let ((case-fold-search ibuffer-case-fold-search))
	(goto-char (point-min))
	(apply #'query-replace args)))
    t))

;;;###autoload (autoload 'ibuffer-do-query-replace-regexp "ibuf-ext")
(define-ibuffer-op query-replace-regexp (&rest args)
  "Perform a `query-replace-regexp' in marked buffers."
  (:interactive
   (query-replace-read-args "Query replace regexp" t t)
   :opstring "replaced in"
   :complex t
   :modifier-p :maybe)
  (save-window-excursion
    (switch-to-buffer buf)
    (save-excursion
      (let ((case-fold-search ibuffer-case-fold-search))
	(goto-char (point-min))
	(apply #'query-replace-regexp args)))
    t))

;;;###autoload (autoload 'ibuffer-do-print "ibuf-ext")
(define-ibuffer-op print ()
  "Print marked buffers as with `print-buffer'."
  (:opstring "printed"
   :modifier-p nil)
  (print-buffer))

;;;###autoload
(defun ibuffer-included-in-filters-p (buf filters)
  "Return non-nil if BUF passes all FILTERS.

BUF is a Lisp buffer object, and FILTERS is a list of filter
specifications with the same structure as
`ibuffer-filtering-qualifiers'."
  (not
   (memq nil ;; a filter will return nil if it failed
         (mapcar (lambda (filter)
                   (ibuffer-included-in-filter-p buf filter))
                 filters))))

(defun ibuffer-unary-operand (filter)
  "Extract operand from a unary compound FILTER specification.

FILTER should be a cons cell of either form (f . d) or (f d),
where operand d is itself a cons cell, or nil.  Returns d."
  (let* ((tail (cdr filter))
         (maybe-q (car-safe tail)))
    (if (consp maybe-q) maybe-q tail)))

(defun ibuffer-included-in-filter-p (buf filter)
  "Return non-nil if BUF pass FILTER.

BUF is a Lisp buffer object, and FILTER is a filter
specification, with the same structure as an element of the list
`ibuffer-filtering-qualifiers'."
  (if (eq (car filter) 'not)
      (let ((inner (ibuffer-unary-operand filter)))
        ;; Allows (not (not ...)) etc, which may be overkill
        (if (eq (car inner) 'not)
            (ibuffer-included-in-filter-p buf (ibuffer-unary-operand inner))
          (not (ibuffer-included-in-filter-p-1 buf inner))))
    (ibuffer-included-in-filter-p-1 buf filter)))

(defun ibuffer-included-in-filter-p-1 (buf filter)
  (not
   (not
    (pcase (car filter)
      ('or
       ;;; ATTN: Short-circuiting alternative with parallel structure w/`and
       ;;(catch 'has-match
       ;;  (dolist (filter-spec (cdr filter) nil)
       ;;    (when (ibuffer-included-in-filter-p buf filter-spec)
       ;;      (throw 'has-match t))))
       (memq t (mapcar (lambda (x)
                         (ibuffer-included-in-filter-p buf x))
                       (cdr filter))))
      ('and
       (catch 'no-match
         (dolist (filter-spec (cdr filter) t)
           (unless (ibuffer-included-in-filter-p buf filter-spec)
             (throw 'no-match nil)))))
      ('saved
       (let ((data (assoc (cdr filter) ibuffer-saved-filters)))
	 (unless data
	   (ibuffer-filter-disable t)
	   (error "Unknown saved filter %s" (cdr filter)))
	 (ibuffer-included-in-filters-p buf (cdr data))))
      (_
       (pcase-let ((`(,_type ,_desc ,func)
                    (assq (car filter) ibuffer-filtering-alist)))
         (unless func
           (ibuffer-filter-disable t)
           (error "Undefined filter %s" (car filter)))
         (funcall func buf (cdr filter))))))))

(defun ibuffer-generate-filter-groups (bmarklist &optional noempty nodefault)
  (let ((filter-group-alist (if nodefault
				ibuffer-filter-groups
			      (append ibuffer-filter-groups
				      (list (cons "Default" nil))))))
    ;; (dolist (hidden ibuffer-hidden-filter-groups)
    ;;   (setq filter-group-alist (ibuffer-remove-alist
    ;;     			   hidden filter-group-alist)))
    (let ((vec (make-vector (length filter-group-alist) nil))
	  (i 0))
      (dolist (filtergroup filter-group-alist)
	(let ((filterset (cdr filtergroup)))
	  (cl-destructuring-bind (hip-crowd lamers)
	      (ibuffer-split-list (lambda (bufmark)
				    (ibuffer-included-in-filters-p (car bufmark)
								   filterset))
				  bmarklist)
	    (aset vec i hip-crowd)
	    (cl-incf i)
	    (setq bmarklist lamers))))
      (let (ret)
	(dotimes (j i)
	  (let ((bufs (aref vec j)))
	    (unless (and noempty (null bufs))
	      (push (cons (car (nth j filter-group-alist))
			  bufs)
		    ret))))
        ret))))

;;;###autoload
(defun ibuffer-filters-to-filter-group (name)
  "Make the current filters into a filtering group."
  (interactive "sName for filtering group: ")
  (when (null ibuffer-filtering-qualifiers)
    (error "No filters in effect"))
  (push (cons name ibuffer-filtering-qualifiers) ibuffer-filter-groups)
  (ibuffer-filter-disable))

;;;###autoload
(defun ibuffer-set-filter-groups-by-mode ()
  "Set the current filter groups to filter by mode."
  (interactive)
  (setq ibuffer-filter-groups
        (mapcar (lambda (mode)
                  (cons (format "%s" mode) `((mode . ,mode))))
                (let ((modes
                       (ibuffer-remove-duplicates
                        (mapcar (lambda (buf)
				  (buffer-local-value 'major-mode buf))
                                (buffer-list)))))
                  (if ibuffer-view-ibuffer
		      modes
		    (delq 'ibuffer-mode modes)))))
  (ibuffer-update nil t))

;;;###autoload
(defun ibuffer-pop-filter-group ()
  "Remove the first filter group."
  (interactive)
  (when (null ibuffer-filter-groups)
    (error "No filter groups active"))
  (setq ibuffer-hidden-filter-groups
	(delete (pop ibuffer-filter-groups)
		ibuffer-hidden-filter-groups))
  (ibuffer-update nil t))

(defun ibuffer-read-filter-group-name (msg &optional nodefault noerror)
  (when (and (not noerror) (null ibuffer-filter-groups))
    (error "No filter groups active"))
  ;; `ibuffer-generate-filter-groups' returns all non-hidden filter
  ;; groups, possibly excluding empty groups or Default.
  ;; We add `ibuffer-hidden-filter-groups' to the list, excluding
  ;; Default if necessary.
  (completing-read msg (nconc
			(ibuffer-generate-filter-groups
			 (ibuffer-current-state-list)
			 (not ibuffer-show-empty-filter-groups)
			 nodefault)
			(if nodefault
			    (remove "Default" ibuffer-hidden-filter-groups)
			  ibuffer-hidden-filter-groups))
		   nil t))

;;;###autoload
(defun ibuffer-decompose-filter-group (group)
  "Decompose the filter group GROUP into active filters."
  (interactive
   (list (ibuffer-read-filter-group-name "Decompose filter group: " t)))
  (let ((data (cdr (assoc group ibuffer-filter-groups))))
    (setq ibuffer-filter-groups (ibuffer-remove-alist
				 group ibuffer-filter-groups)
	  ibuffer-filtering-qualifiers data))
  (ibuffer-update nil t))

;;;###autoload
(defun ibuffer-clear-filter-groups ()
  "Remove all filter groups."
  (interactive)
  (setq ibuffer-filter-groups nil
	ibuffer-hidden-filter-groups nil)
  (ibuffer-update nil t))

(defun ibuffer-current-filter-groups-with-position ()
  (save-excursion
    (goto-char (point-min))
    (let ((pos nil)
	  (result nil))
      (while (and (not (eobp))
		  (setq pos (next-single-property-change
			     (point) 'ibuffer-filter-group-name)))
	(goto-char pos)
	(push (cons (get-text-property (point) 'ibuffer-filter-group-name)
		    pos)
	      result)
	(goto-char (next-single-property-change
		    pos 'ibuffer-filter-group-name)))
      (nreverse result))))

;;;###autoload
(defun ibuffer-jump-to-filter-group (name)
  "Move point to the filter group whose name is NAME."
  (interactive
   (list (ibuffer-read-filter-group-name "Jump to filter group: ")))
  (ibuffer-aif (assoc name (ibuffer-current-filter-groups-with-position))
      (goto-char (cdr it))
    (error "No filter group with name %s" name)))

;;;###autoload
(defun ibuffer-kill-filter-group (name)
  "Kill the filter group named NAME.
The group will be added to `ibuffer-filter-group-kill-ring'."
  (interactive (list (ibuffer-read-filter-group-name "Kill filter group: " t)))
  (when (equal name "Default")
    (error "Can't kill default filter group"))
  (ibuffer-aif (assoc name ibuffer-filter-groups)
      (progn
	(push (copy-tree it) ibuffer-filter-group-kill-ring)
	(setq ibuffer-filter-groups (ibuffer-remove-alist
				     name ibuffer-filter-groups))
	(setq ibuffer-hidden-filter-groups
	      (delete name ibuffer-hidden-filter-groups)))
    (error "No filter group with name \"%s\"" name))
  (ibuffer-update nil t))

;;;###autoload
(defun ibuffer-kill-line (&optional arg interactive-p)
  "Kill the filter group at point.
See also `ibuffer-kill-filter-group'."
  (interactive "P\np")
  (ibuffer-aif (save-excursion
		 (ibuffer-forward-line 0)
		 (get-text-property (point) 'ibuffer-filter-group-name))
      (progn
	(ibuffer-kill-filter-group it))
      (funcall (if interactive-p #'call-interactively #'funcall)
	       #'kill-line arg)))

(defun ibuffer-insert-filter-group-before (newgroup group)
  (let* ((found nil)
	 (pos (let ((groups (mapcar #'car ibuffer-filter-groups))
		    (res 0))
		(while groups
		  (if (equal (car groups) group)
		      (setq found t
			    groups nil)
		    (cl-incf res)
		    (setq groups (cdr groups))))
		res)))
    (cond ((not found)
	   (setq ibuffer-filter-groups
		 (nconc ibuffer-filter-groups (list newgroup))))
	  ((zerop pos)
	   (push newgroup ibuffer-filter-groups))
	  (t
	   (let ((cell (nthcdr pos ibuffer-filter-groups)))
	     (setf (cdr cell) (cons (car cell) (cdr cell)))
	     (setf (car cell) newgroup))))))

;;;###autoload
(defun ibuffer-yank ()
  "Yank the last killed filter group before group at point."
  (interactive)
  (ibuffer-yank-filter-group
   (or (get-text-property (point) 'ibuffer-filter-group-name)
       (get-text-property (point) 'ibuffer-filter-group)
       (error "No filter group at point"))))

;;;###autoload
(defun ibuffer-yank-filter-group (name)
  "Yank the last killed filter group before group named NAME."
  (interactive (list (ibuffer-read-filter-group-name
			"Yank filter group before group: ")))
  (unless ibuffer-filter-group-kill-ring
    (error "The Ibuffer filter group kill-ring is empty"))
  (save-excursion
    (ibuffer-forward-line 0)
    (ibuffer-insert-filter-group-before (pop ibuffer-filter-group-kill-ring)
					name))
  (ibuffer-update nil t))

;;;###autoload
(defun ibuffer-save-filter-groups (name groups)
  "Save all active filter groups GROUPS as NAME.
They are added to `ibuffer-saved-filter-groups'.  Interactively,
prompt for NAME, and use the current filters."
  (interactive
   (if (null ibuffer-filter-groups)
       (error "No filter groups active")
     (list
      (read-from-minibuffer "Save current filter groups as: ")
      ibuffer-filter-groups)))
  (ibuffer-aif (assoc name ibuffer-saved-filter-groups)
      (setcdr it groups)
    (push (cons name groups) ibuffer-saved-filter-groups))
  (ibuffer-maybe-save-stuff))

;;;###autoload
(defun ibuffer-delete-saved-filter-groups (name)
  "Delete saved filter groups with NAME.
They are removed from `ibuffer-saved-filter-groups'."
  (interactive
   (list
    (if (null ibuffer-saved-filter-groups)
	(error "No saved filter groups")
      (completing-read "Delete saved filter group: "
		       ibuffer-saved-filter-groups nil t))))
  (setq ibuffer-saved-filter-groups
	(ibuffer-remove-alist name ibuffer-saved-filter-groups))
  (ibuffer-maybe-save-stuff)
  (ibuffer-update nil t))

;;;###autoload
(defun ibuffer-switch-to-saved-filter-groups (name)
  "Set this buffer's filter groups to saved version with NAME.
The value from `ibuffer-saved-filter-groups' is used."
  (interactive
   (list
    (cond ((null ibuffer-saved-filter-groups)
           (error "No saved filters"))
          ;; `ibuffer-saved-filter-groups' is a user variable that defaults
          ;; to nil.  We assume that with one element in this list the user
          ;; knows what she wants.  See bug#12331.
          ((null (cdr ibuffer-saved-filter-groups))
           (caar ibuffer-saved-filter-groups))
          (t
           (completing-read "Switch to saved filter group: "
                            ibuffer-saved-filter-groups nil t)))))
  (setq ibuffer-filter-groups (cdr (assoc name ibuffer-saved-filter-groups))
	ibuffer-hidden-filter-groups nil)
  (ibuffer-update nil t))

;;;###autoload
(defun ibuffer-filter-disable (&optional delete-filter-groups)
  "Disable all filters currently in effect in this buffer.
With optional arg DELETE-FILTER-GROUPS non-nil, delete all filter
group definitions by setting `ibuffer-filter-groups' to nil."
  (interactive)
  (setq ibuffer-filtering-qualifiers nil)
  (if delete-filter-groups
      (setq ibuffer-filter-groups nil))
  (let ((buf (ibuffer-current-buffer)))
    (ibuffer-update nil t)
    (when buf
      (ibuffer-jump-to-buffer (buffer-name buf)))))

;;;###autoload
(defun ibuffer-pop-filter ()
  "Remove the top filter in this buffer."
  (interactive)
  (when (null ibuffer-filtering-qualifiers)
    (error "No filters in effect"))
  (pop ibuffer-filtering-qualifiers)
  (let ((buf (ibuffer-current-buffer)))
    (ibuffer-update nil t)
    (when buf
      (ibuffer-jump-to-buffer (buffer-name buf)))))

(defun ibuffer-push-filter (filter-specification)
  "Add FILTER-SPECIFICATION to `ibuffer-filtering-qualifiers'.
If FILTER-SPECIFICATION is already in the list then return nil.  Otherwise,
return the updated list."
  (unless (member filter-specification ibuffer-filtering-qualifiers)
    (push filter-specification ibuffer-filtering-qualifiers)))

;;;###autoload
(defun ibuffer-decompose-filter ()
  "Separate this buffer's top compound filter (AND, OR, NOT, or SAVED).

This means that the topmost filter on the filtering stack, which must
be a complex filter like (OR [name: foo] [mode: bar-mode]), will be
turned into separate filters, like [name: foo] and [mode: bar-mode]."
  (interactive)
  (unless ibuffer-filtering-qualifiers
    (error "No filters in effect"))
  (let* ((filters ibuffer-filtering-qualifiers)
         (head (cdar filters))
         (tail (cdr filters))
         (value
          (pcase (caar filters)
            ((or 'or 'and) (nconc head tail))
            ('saved
             (let ((data (assoc head ibuffer-saved-filters)))
               (unless data
                 (ibuffer-filter-disable)
                 (error "Unknown saved filter %s" head))
               (append (cdr data) tail)))
            ('not (cons (ibuffer-unary-operand (car filters)) tail))
            (_
             (error "Filter type %s is not compound" (caar filters))))))
    (setq ibuffer-filtering-qualifiers value))
  (ibuffer-update nil t))

;;;###autoload
(defun ibuffer-exchange-filters ()
  "Exchange the top two filters on the stack in this buffer."
  (interactive)
  (let ((filters ibuffer-filtering-qualifiers))
    (when (< (length filters) 2)
      (error "Need two filters to exchange"))
    (cl-rotatef (car filters) (cadr filters))
    (ibuffer-update nil t)))

;;;###autoload
(defun ibuffer-negate-filter ()
  "Negate the sense of the top filter in the current buffer."
  (interactive)
  (when (null ibuffer-filtering-qualifiers)
    (error "No filters in effect"))
  (let ((lim (pop ibuffer-filtering-qualifiers)))
    (push (if (eq (car lim) 'not)
	      (cdr lim)
	    (cons 'not lim))
	  ibuffer-filtering-qualifiers))
  (ibuffer-update nil t))

(defun ibuffer--or-and-filter (op decompose)
  (if decompose
      (if (eq op (caar ibuffer-filtering-qualifiers))
          (ibuffer-decompose-filter)
        (error "Top filter is not an %s" (upcase (symbol-name op))))
    (when (< (length ibuffer-filtering-qualifiers) 2)
      (error "Need two filters to %s" (upcase (symbol-name op))))
    ;; If either filter is an op, eliminate unnecessary nesting.
    (let ((first (pop ibuffer-filtering-qualifiers))
          (second (pop ibuffer-filtering-qualifiers)))
      (push (nconc (if (eq op (car first)) first (list op first))
                   (if (eq op (car second)) (cdr second) (list second)))
            ibuffer-filtering-qualifiers)))
  (ibuffer-update nil t))

;;;###autoload
(defun ibuffer-or-filter (&optional decompose)
  "Replace the top two filters in this buffer with their logical OR.
If optional argument DECOMPOSE is non-nil, instead break the top OR
filter into parts."
  (interactive "P")
  (ibuffer--or-and-filter 'or decompose))

;;;###autoload
(defun ibuffer-and-filter (&optional decompose)
  "Replace the top two filters in this buffer with their logical AND.
If optional argument DECOMPOSE is non-nil, instead break the top AND
filter into parts."
  (interactive "P")
  (ibuffer--or-and-filter 'and decompose))

(defun ibuffer-maybe-save-stuff ()
  (when ibuffer-save-with-custom
    (customize-save-variable 'ibuffer-saved-filters
                             ibuffer-saved-filters)
    (customize-save-variable 'ibuffer-saved-filter-groups
                             ibuffer-saved-filter-groups)))

;;;###autoload
(defun ibuffer-save-filters (name filters)
  "Save FILTERS in this buffer with name NAME in `ibuffer-saved-filters'.
Interactively, prompt for NAME, and use the current filters."
  (interactive
   (if (null ibuffer-filtering-qualifiers)
       (error "No filters currently in effect")
     (list
      (read-from-minibuffer "Save current filters as: ")
      ibuffer-filtering-qualifiers)))
  (ibuffer-aif (assoc name ibuffer-saved-filters)
      (setcdr it filters)
    (push (cons name filters) ibuffer-saved-filters))
  (ibuffer-maybe-save-stuff))

;;;###autoload
(defun ibuffer-delete-saved-filters (name)
  "Delete saved filters with NAME from `ibuffer-saved-filters'."
  (interactive
   (list
    (if (null ibuffer-saved-filters)
	(error "No saved filters")
      (completing-read "Delete saved filters: "
		       ibuffer-saved-filters nil t))))
  (setq ibuffer-saved-filters
	(ibuffer-remove-alist name ibuffer-saved-filters))
  (ibuffer-maybe-save-stuff)
  (ibuffer-update nil t))

;;;###autoload
(defun ibuffer-add-saved-filters (name)
  "Add saved filters from `ibuffer-saved-filters' to this buffer's filters."
  (interactive
   (list
    (if (null ibuffer-saved-filters)
	(error "No saved filters")
      (completing-read "Add saved filters: "
		       ibuffer-saved-filters nil t))))
  (push (cons 'saved name) ibuffer-filtering-qualifiers)
  (ibuffer-update nil t))

;;;###autoload
(defun ibuffer-switch-to-saved-filters (name)
  "Set this buffer's filters to filters with NAME from `ibuffer-saved-filters'."
  (interactive
   (list
    (if (null ibuffer-saved-filters)
	(error "No saved filters")
      (completing-read "Switch to saved filters: "
		       ibuffer-saved-filters nil t))))
  (setq ibuffer-filtering-qualifiers (list (cons 'saved name)))
  (ibuffer-update nil t))

(defun ibuffer-format-filter-group-data (filter)
  (if (equal filter "Default")
      ""
    (concat "Filter:" (mapconcat #'ibuffer-format-qualifier
				 (cdr (assq filter ibuffer-filter-groups))
				 " "))))

(defun ibuffer-format-qualifier (qualifier)
  (if (eq (car-safe qualifier) 'not)
      (concat " [NOT"
              (ibuffer-format-qualifier-1 (ibuffer-unary-operand qualifier))
              "]")
    (ibuffer-format-qualifier-1 qualifier)))

(defun ibuffer-format-qualifier-1 (qualifier)
  (pcase (car qualifier)
    ('saved
     (concat " [filter: " (cdr qualifier) "]"))
    ('or
     (concat " [OR" (mapconcat #'ibuffer-format-qualifier
                               (cdr qualifier) "") "]"))
    ('and
     (concat " [AND" (mapconcat #'ibuffer-format-qualifier
                                (cdr qualifier) "") "]"))
    (_
     (let ((type (assq (car qualifier) ibuffer-filtering-alist)))
       (unless qualifier
         (error "Ibuffer: Bad qualifier %s" qualifier))
       (concat " [" (cadr type) ": " (format "%s]" (cdr qualifier)))))))

(defun ibuffer-list-buffer-modes (&optional include-parents)
  "Create a completion table of buffer modes currently in use.
If INCLUDE-PARENTS is non-nil then include parent modes."
  (let ((modes))
    (dolist (buf (buffer-list))
      (let ((this-mode (buffer-local-value 'major-mode buf)))
        (while (and this-mode (not (memq this-mode modes)))
          (push this-mode modes)
          (setq this-mode (and include-parents
                               (get this-mode 'derived-mode-parent))))))
    (mapcar #'symbol-name modes)))


;;; Extra operation definitions

;;;###autoload (autoload 'ibuffer-filter-by-mode "ibuf-ext")
(define-ibuffer-filter mode
    "Limit current view to buffers with major mode(s) specified by QUALIFIER.
QUALIFIER is the mode name as a symbol or a list of symbols.
Called interactively, accept a comma separated list of mode names."
  (:description "major mode"
   :reader
   (let* ((buf (ibuffer-current-buffer))
          (default (if (and buf (buffer-live-p buf))
                       (symbol-name (buffer-local-value
                                     'major-mode buf)))))
     (mapcar #'intern
             (completing-read-multiple
              (format-prompt "Filter by major mode" default)
              obarray
              (lambda (e)
                (string-match "-mode\\'" (if (symbolp e) (symbol-name e) e)))
              t nil nil default)))
   :accept-list t)
  (eq qualifier (buffer-local-value 'major-mode buf)))

;;;###autoload (autoload 'ibuffer-filter-by-used-mode "ibuf-ext")
(define-ibuffer-filter used-mode
    "Limit current view to buffers with major mode(s) specified by QUALIFIER.
QUALIFIER is the mode name as a symbol or a list of symbols.

Called interactively, accept a comma separated list of mode names
currently used by buffers."
  (:description "major mode in use"
   :reader
   (let* ((buf (ibuffer-current-buffer))
          (default (if (and buf (buffer-live-p buf))
                       (symbol-name (buffer-local-value
                                     'major-mode buf)))))
     (mapcar #'intern
             (completing-read-multiple
              (format-prompt "Filter by major mode" default)
              (ibuffer-list-buffer-modes) nil t nil nil default)))
   :accept-list t)
  (eq qualifier (buffer-local-value 'major-mode buf)))

;;;###autoload (autoload 'ibuffer-filter-by-derived-mode "ibuf-ext")
(define-ibuffer-filter derived-mode
    "Limit current view to buffers with major mode(s) specified by QUALIFIER.
QUALIFIER is the mode name as a symbol or a list of symbols.
 Restrict the view to buffers whose major mode derivates
 from modes specified by QUALIFIER.
Called interactively, accept a comma separated list of mode names."
  (:description "derived mode"
        :reader
        (mapcar #'intern
         (completing-read-multiple "Filter by derived mode: "
                       (ibuffer-list-buffer-modes t)
                       nil t))
        :accept-list t)
  (with-current-buffer buf (derived-mode-p qualifier)))

;;;###autoload (autoload 'ibuffer-filter-by-name "ibuf-ext")
(define-ibuffer-filter name
  "Limit current view to buffers with name matching QUALIFIER."
  (:description "buffer name"
   :reader (read-from-minibuffer "Filter by name (regexp): "))
  (string-match qualifier (buffer-name buf)))

;;;###autoload (autoload 'ibuffer-filter-by-process "ibuf-ext")
(define-ibuffer-filter process
    "Limit current view to buffers running a process."
  (:description "process")
  (get-buffer-process buf))

;;;###autoload (autoload 'ibuffer-filter-by-starred-name "ibuf-ext")
(define-ibuffer-filter starred-name
    "Limit current view to buffers with name beginning and ending
with *, along with an optional suffix of the form digits or
<digits>."
  (:description "starred buffer name"
   :reader nil)
  (string-match "\\`\\*[^*]+\\*\\(?:<[[:digit:]]+>\\)?\\'" (buffer-name buf)))

;;;###autoload (autoload 'ibuffer-filter-by-filename "ibuf-ext")
(define-ibuffer-filter filename
    "Limit current view to buffers with full file name matching QUALIFIER.

For example, for a buffer associated with file '/a/b/c.d', this
matches against '/a/b/c.d'."
  (:description "full file name"
   :reader (read-from-minibuffer "Filter by full file name (regexp): "))
  (ibuffer-awhen (with-current-buffer buf (ibuffer-buffer-file-name))
    (string-match qualifier it)))

;;;###autoload (autoload 'ibuffer-filter-by-basename "ibuf-ext")
(define-ibuffer-filter basename
    "Limit current view to buffers with file basename matching QUALIFIER.

For example, for a buffer associated with file `/a/b/c.d', this
matches against `c.d'."
  (:description "file basename"
   :reader (read-from-minibuffer
            "Filter by file name, without directory part (regex): "))
  (ibuffer-awhen (with-current-buffer buf (ibuffer-buffer-file-name))
    (string-match qualifier (file-name-nondirectory it))))

;;;###autoload (autoload 'ibuffer-filter-by-file-extension "ibuf-ext")
(define-ibuffer-filter file-extension
    "Limit current view to buffers with filename extension matching QUALIFIER.

The separator character (typically `.') is not part of the
pattern.  For example, for a buffer associated with file
`/a/b/c.d', this matches against `d'."
  (:description "filename extension"
   :reader (read-from-minibuffer
            "Filter by filename extension without separator (regex): "))
  (ibuffer-awhen (with-current-buffer buf (ibuffer-buffer-file-name))
    (string-match qualifier (or (file-name-extension it) ""))))

;;;###autoload (autoload 'ibuffer-filter-by-directory "ibuf-ext")
(define-ibuffer-filter directory
    "Limit current view to buffers with directory matching QUALIFIER.

For a buffer associated with file '/a/b/c.d', this matches
against '/a/b'.  For a buffer not associated with a file, this
matches against the value of `default-directory' in that buffer."
  (:description "directory name"
   :reader (read-from-minibuffer "Filter by directory name (regex): "))
  (ibuffer-aif (with-current-buffer buf (ibuffer-buffer-file-name))
      (let ((dirname (file-name-directory it)))
        (when dirname (string-match qualifier dirname)))
    (when default-directory (string-match qualifier default-directory))))

;;;###autoload (autoload 'ibuffer-filter-by-size-gt  "ibuf-ext")
(define-ibuffer-filter size-gt
  "Limit current view to buffers with size greater than QUALIFIER."
  (:description "size greater than"
   :reader
   (string-to-number (read-from-minibuffer "Filter by size greater than: ")))
  (> (with-current-buffer buf (buffer-size))
     qualifier))

;;;###autoload (autoload 'ibuffer-filter-by-size-lt  "ibuf-ext")
(define-ibuffer-filter size-lt
    "Limit current view to buffers with size less than QUALIFIER."
  (:description "size less than"
   :reader
   (string-to-number (read-from-minibuffer "Filter by size less than: ")))
  (< (with-current-buffer buf (buffer-size))
     qualifier))

;;;###autoload (autoload 'ibuffer-filter-by-modified "ibuf-ext")
(define-ibuffer-filter modified
    "Limit current view to buffers that are marked as modified."
  (:description "modified"
   :reader nil)
  (buffer-modified-p buf))

;;;###autoload (autoload 'ibuffer-filter-by-visiting-file "ibuf-ext")
(define-ibuffer-filter visiting-file
    "Limit current view to buffers that are visiting a file."
  (:description "visiting a file"
   :reader nil)
  (with-current-buffer buf (buffer-file-name)))

;;;###autoload (autoload 'ibuffer-filter-by-content "ibuf-ext")
(define-ibuffer-filter content
   "Limit current view to buffers whose contents match QUALIFIER."
  (:description "content"
   :reader (read-from-minibuffer "Filter by content (regexp): "))
  (with-current-buffer buf
    (save-excursion
      (goto-char (point-min))
      (re-search-forward qualifier nil t))))

;;;###autoload (autoload 'ibuffer-filter-by-predicate "ibuf-ext")
(define-ibuffer-filter predicate
   "Limit current view to buffers for which QUALIFIER returns non-nil."
  (:description "predicate"
   :reader (read-minibuffer "Filter by predicate (form): "))
  (with-current-buffer buf
    (eval qualifier)))

;;;###autoload (autoload 'ibuffer-filter-chosen-by-completion "ibuf-ext")
(defun ibuffer-filter-chosen-by-completion ()
  "Select and apply filter chosen by completion against available filters.
Indicates corresponding key sequences in echo area after filtering.

The completion matches against the filter description text of
each filter in `ibuffer-filtering-alist'."
  (interactive)
  (let* ((filters (mapcar (lambda (x) (cons (cadr x) (car x)))
                          ibuffer-filtering-alist))
         (match (completing-read "Filter by: " filters nil t))
         (filter (cdr (assoc match filters)))
         (command (intern (concat "ibuffer-filter-by-" (symbol-name filter)))))
    (call-interactively command)
    (message "%s can be run with key sequences: %s"
             command
             (mapconcat #'key-description
                        (where-is-internal command ibuffer-mode-map nil t)
                        "or "))))


;;; Sorting

;;;###autoload
(defun ibuffer-toggle-sorting-mode ()
  "Toggle the current sorting mode.
Default sorting modes are:
 Recency - the last time the buffer was viewed
 Name - the name of the buffer
 Major Mode - the name of the major mode of the buffer
 Size - the size of the buffer"
  (interactive)
  (let ((modes (mapcar #'car ibuffer-sorting-functions-alist)))
    (cl-pushnew 'recency modes)
    (setq modes (sort modes #'string-lessp))
    (let ((next (or (car-safe (cdr-safe (memq ibuffer-sorting-mode modes)))
                    (car modes))))
      (setq ibuffer-sorting-mode next)
      (message "Sorting by %s" next)))
  (ibuffer-redisplay t))

;;;###autoload
(defun ibuffer-invert-sorting ()
  "Toggle whether or not sorting is in reverse order."
  (interactive)
  (setq ibuffer-sorting-reversep (not ibuffer-sorting-reversep))
  (message "Sorting order %s"
	   (if ibuffer-sorting-reversep
	       "reversed"
	     "normal"))
  (ibuffer-redisplay t))

;;;###autoload (autoload 'ibuffer-do-sort-by-major-mode "ibuf-ext")
(define-ibuffer-sorter major-mode
  "Sort the buffers by major modes.
Ordering is lexicographic."
  (:description "major mode")
  (string-lessp (downcase
		 (symbol-name (buffer-local-value 'major-mode (car a))))
		(downcase
		 (symbol-name (buffer-local-value 'major-mode (car b))))))

;;;###autoload (autoload 'ibuffer-do-sort-by-mode-name "ibuf-ext")
(define-ibuffer-sorter mode-name
  "Sort the buffers by their mode name.
Ordering is lexicographic."
  (:description "major mode name")
  (string-lessp (downcase
		 (with-current-buffer
		     (car a)
		   (format-mode-line mode-name)))
		(downcase
		 (with-current-buffer
		     (car b)
		   (format-mode-line mode-name)))))

;;;###autoload (autoload 'ibuffer-do-sort-by-alphabetic "ibuf-ext")
(define-ibuffer-sorter alphabetic
  "Sort the buffers by their names.
Ordering is lexicographic."
  (:description "buffer name")
  (string-lessp
   (buffer-name (car a))
   (buffer-name (car b))))

;;;###autoload (autoload 'ibuffer-do-sort-by-size "ibuf-ext")
(define-ibuffer-sorter size
 "Sort the buffers by their size."
  (:description "size")
  (< (with-current-buffer (car a)
       (buffer-size))
     (with-current-buffer (car b)
       (buffer-size))))

;;;###autoload (autoload 'ibuffer-do-sort-by-filename/process "ibuf-ext")
(define-ibuffer-sorter filename/process
 "Sort the buffers by their file name/process name."
  (:description "file name")
  (string-lessp
   ;; FIXME: For now just compare the file name and the process name
   ;; (if it exists).  Is there a better way to do this?
   (or (with-current-buffer (car a) (ibuffer-buffer-file-name))
       (let ((pr-a (get-buffer-process (car a))))
	 (and (processp pr-a) (process-name pr-a))))
   (or (with-current-buffer (car b) (ibuffer-buffer-file-name))
       (let ((pr-b (get-buffer-process (car b))))
	 (and (processp pr-b) (process-name pr-b))))))

;;; Functions to emulate bs.el

;;;###autoload
(defun ibuffer-bs-show ()
  "Emulate `bs-show' from the bs.el package."
  (interactive)
  (ibuffer t "*Ibuffer-bs*" '((filename . ".*")) nil t)
  (define-key (current-local-map) "a" 'ibuffer-bs-toggle-all))

(defun ibuffer-bs-toggle-all ()
  "Emulate `bs-toggle-show-all' from the bs.el package."
  (interactive)
  (if ibuffer-filtering-qualifiers
      (ibuffer-pop-filter)
    (progn (ibuffer-push-filter '(filename . ".*"))
	   (ibuffer-update nil t))))

;;; Handy functions

;;;###autoload
(defun ibuffer-add-to-tmp-hide (regexp)
  "Add REGEXP to `ibuffer-tmp-hide-regexps'.
This means that buffers whose name matches REGEXP will not be shown
for this Ibuffer session."
  (interactive
   (list
    (read-from-minibuffer "Never show buffers matching: "
			  (regexp-quote (buffer-name (ibuffer-current-buffer t))))))
  (push regexp ibuffer-tmp-hide-regexps))

;;;###autoload
(defun ibuffer-add-to-tmp-show (regexp)
  "Add REGEXP to `ibuffer-tmp-show-regexps'.
This means that buffers whose name matches REGEXP will always be shown
for this Ibuffer session."
  (interactive
   (list
    (read-from-minibuffer "Always show buffers matching: "
			  (regexp-quote (buffer-name (ibuffer-current-buffer t))))))
  (push regexp ibuffer-tmp-show-regexps))

;;;###autoload
(defun ibuffer-forward-next-marked (&optional count mark direction)
  "Move forward by COUNT marked buffers (default 1).

If MARK is non-nil, it should be a character denoting the type of mark
to move by.  The default is `ibuffer-marked-char'.

If DIRECTION is non-nil, it should be an integer; negative integers
mean move backwards, non-negative integers mean move forwards."
  (interactive "P")
  (unless count
    (setq count 1))
  (unless mark
    (setq mark ibuffer-marked-char))
  (unless direction
    (setq direction 1))
  ;; Skip the title
  (ibuffer-forward-line 0)
  (let ((opos (point)))
    (ibuffer-forward-line direction)
    (while (not (or (= (point) opos)
		    (eq (ibuffer-current-mark) mark)))
      (ibuffer-forward-line direction))
    (when (and (= (point) opos)
	       (not (eq (ibuffer-current-mark) mark)))
      (error "No buffers with mark %c" mark))))

;;;###autoload
(defun ibuffer-backwards-next-marked (&optional count mark)
   "Move backwards by COUNT marked buffers (default 1).

If MARK is non-nil, it should be a character denoting the type of mark
to move by.  The default is `ibuffer-marked-char'."
   (interactive "P")
   (ibuffer-forward-next-marked count mark -1))

;;;###autoload
(defun ibuffer-do-kill-lines ()
  "Hide all of the currently marked lines."
  (interactive)
  (if (= (ibuffer-count-marked-lines) 0)
      (message "No buffers marked; use `m' to mark a buffer")
    (let ((count
	   (ibuffer-map-marked-lines
            (lambda (_buf _mark)
              'kill))))
      (message "Killed %s lines" count))))

;;;###autoload
(defun ibuffer-jump-to-buffer (name)
  "Move point to the buffer whose name is NAME.

If called interactively, prompt for a buffer name and go to the
corresponding line in the Ibuffer buffer.  If said buffer is in a
hidden group filter, open it.

If `ibuffer-jump-offer-only-visible-buffers' is non-nil, only offer
visible buffers in the completion list.  Calling the command with
a prefix argument reverses the meaning of that variable."
  (interactive (list
		(let ((only-visible ibuffer-jump-offer-only-visible-buffers))
		  (when current-prefix-arg
		    (setq only-visible (not only-visible)))
		  (if only-visible
                      (let ((table (mapcar (lambda (x)
                                             (buffer-name (car x)))
					   (ibuffer-current-state-list))))
			(when (null table)
			  (error "No buffers!"))
			(completing-read "Jump to buffer: "
					 table nil t))
		    (read-buffer "Jump to buffer: " nil t)))))
  (when (not (string= "" name))
    (let (buf-point)
      ;; Blindly search for our buffer: it is very likely that it is
      ;; not in a hidden filter group.
      (ibuffer-map-lines (lambda (buf _marks)
                           (when (string= (buffer-name buf) name)
                             (setq buf-point (point))
                             nil))
			 t nil)
      (when (and
	     (null buf-point)
	     (not (null ibuffer-hidden-filter-groups)))
	;; We did not find our buffer.  It must be in a hidden filter
	;; group, so go through all hidden filter groups to find it.
	(catch 'found
	  (dolist (group ibuffer-hidden-filter-groups)
	    (ibuffer-jump-to-filter-group group)
	    (ibuffer-toggle-filter-group)
            (ibuffer-map-lines (lambda (buf _marks)
                                 (when (string= (buffer-name buf) name)
                                   (setq buf-point (point))
                                   nil))
			       t group)
	    (if buf-point
		(throw 'found nil)
	      (ibuffer-toggle-filter-group)))))
      (if (null buf-point)
	  ;; Still not found even though we expanded all hidden filter
	  ;; groups: that must be because it's hidden by predicate:
	  ;; we won't bother trying to display it.
	  (error "No buffer with name %s" name)
	(goto-char buf-point)))))

(declare-function diff-sentinel "diff"
		  (code &optional old-temp-file new-temp-file))

(defun ibuffer-diff-buffer-with-file-1 (buffer)
  (let ((bufferfile (buffer-local-value 'buffer-file-name buffer))
	(tempfile (make-temp-file "buffer-content-")))
    (when bufferfile
      (unwind-protect
	  (progn
	    (with-current-buffer buffer
	      (write-region nil nil tempfile nil 'nomessage))
	    (let* ((old (expand-file-name bufferfile))
		   (new (expand-file-name tempfile))
		   (oldtmp (file-local-copy old))
		   (newtmp (file-local-copy new))
		   (switches diff-switches)
		   (command
		    (mapconcat
		     'identity
		     `(,diff-command
		       ;; Use explicitly specified switches
		       ,@(if (listp switches) switches (list switches))
		       ,@(if (or old new)
			     (list "-L" (shell-quote-argument old)
				   "-L" (shell-quote-argument
					 (format "Buffer %s" (buffer-name buffer)))))
		       ,(shell-quote-argument (or oldtmp old))
		       ,(shell-quote-argument (or newtmp new)))
		     " ")))
	      (let ((inhibit-read-only t))
		(insert command "\n")
		(diff-sentinel
		 (call-process shell-file-name nil
			       (current-buffer) nil
			       shell-command-switch command))
		(insert "\n")))))
      (sit-for 0)
      (when (file-exists-p tempfile)
	(delete-file tempfile)))))

;;;###autoload
(defun ibuffer-diff-with-file ()
  "View the differences between marked buffers and their associated files.
If no buffers are marked, use buffer at point.
This requires the external program \"diff\" to be in your `exec-path'."
  (interactive)
  (require 'diff)
  (let ((marked-bufs (ibuffer-get-marked-buffers)))
    (when (null marked-bufs)
      (setq marked-bufs (list (ibuffer-current-buffer t))))
    (with-current-buffer (get-buffer-create "*Ibuffer Diff*")
      (setq buffer-read-only nil)
      (buffer-disable-undo (current-buffer))
      (erase-buffer)
      (buffer-enable-undo (current-buffer))
      (diff-mode)
      (dolist (buf marked-bufs)
	(unless (buffer-live-p buf)
	  (error "Buffer %s has been killed" buf))
	(ibuffer-diff-buffer-with-file-1 buf))
      (setq buffer-read-only t)))
  (switch-to-buffer "*Ibuffer Diff*"))

;;;###autoload
(defun ibuffer-copy-filename-as-kill (&optional arg)
  "Copy filenames of marked (or next ARG) buffers into the kill ring.

The names are separated by a space.
If a buffer has no filename, it is ignored.

With no prefix arg, use the filename sans its directory of each marked file.
With a zero prefix arg, use the complete filename of each marked file.
With \\[universal-argument], use the filename of each marked file relative
to `ibuffer-default-directory' if non-nil, otherwise `default-directory'.

You can then feed the file name(s) to other commands with \\[yank]."
  (interactive "P")
  (let* ((buffers (cond ((and (integerp arg) (not (zerop arg)))
                         (ibuffer--near-buffers arg))
                        (t
                         (or (ibuffer-get-marked-buffers)
                             (list (ibuffer-current-buffer))))))
         (file-names
          (mapcar
           (lambda (buf)
             (let ((name (with-current-buffer buf
                           (ibuffer-buffer-file-name))))
               (if (null name)
                   ""
                 (cond ((and (integerp arg) (zerop arg)) name)
                       ((consp arg)
                        (file-relative-name
                         name (or ibuffer-default-directory
                                  default-directory)))
                       (t (file-name-nondirectory name))))))
           buffers))
         (string
          (mapconcat 'identity (delete "" file-names) " ")))
    (unless (string= string "")
      (if (eq last-command 'kill-region)
          (kill-append string nil)
        (kill-new string))
      (message "%s" string))))

;;;###autoload
(defun ibuffer-copy-buffername-as-kill (&optional arg)
  "Copy buffer names of marked (or next ARG) buffers into the kill ring.
The names are separated by a space.
You can then feed the file name(s) to other commands with \\[yank]."
  (interactive "P")
  (let* ((buffers (cond ((and (integerp arg) (not (zerop arg)))
                         (ibuffer--near-buffers arg))
                        (t
                         (or (ibuffer-get-marked-buffers)
                             (list (ibuffer-current-buffer))))))
         (string (mapconcat #'buffer-name buffers " ")))
    (unless (string= string "")
      (if (eq last-command 'kill-region)
          (kill-append string nil)
        (kill-new string))
      (message "%s" string))))

(defun ibuffer-mark-on-buffer (func &optional ibuffer-mark-on-buffer-mark group)
  (let ((count
	 (ibuffer-map-lines
          (lambda (buf _mark)
            (when (funcall func buf)
              (ibuffer-set-mark-1 (or ibuffer-mark-on-buffer-mark
                                      ibuffer-marked-char))
              t))
	  nil
	  group)))
    (ibuffer-redisplay t)
    (unless (eq ibuffer-mark-on-buffer-mark ?\s)
      (message "Marked %s buffers" count))))

;;;###autoload
(defun ibuffer-mark-by-name-regexp (regexp)
  "Mark all buffers whose name matches REGEXP."
  (interactive "sMark by name (regexp): ")
  (ibuffer-mark-on-buffer
   (lambda (buf)
     (string-match regexp (buffer-name buf)))))

(defun ibuffer-locked-buffer-p (&optional buf)
  "Return non-nil if BUF is locked.
When BUF nil, default to the buffer at current line."
  (let ((cbuffer (or buf (ibuffer-current-buffer))))
    (when cbuffer
      (with-current-buffer cbuffer
        (and (boundp 'emacs-lock-mode) emacs-lock-mode)))))

;;;###autoload
(defun ibuffer-mark-by-locked ()
  "Mark all locked buffers."
  (interactive)
  (when (featurep 'emacs-lock)
    (ibuffer-mark-on-buffer
     (lambda (buf)
       (ibuffer-locked-buffer-p buf)))))

;;;###autoload
(defun ibuffer-mark-by-mode-regexp (regexp)
  "Mark all buffers whose major mode matches REGEXP."
  (interactive "sMark by major mode (regexp): ")
  (ibuffer-mark-on-buffer
   (lambda (buf)
     (with-current-buffer buf
       (string-match regexp (format-mode-line mode-name nil nil buf))))))

;;;###autoload
(defun ibuffer-mark-by-file-name-regexp (regexp)
  "Mark all buffers whose file name matches REGEXP."
  (interactive (list (read-regexp "Mark by file name (regexp)")))
  (ibuffer-mark-on-buffer
   (lambda (buf)
     (when-let ((name (with-current-buffer buf (ibuffer-buffer-file-name))))
       ;; Match on the displayed file name (which is abbreviated).
       (string-match-p regexp (ibuffer--abbreviate-file-name name))))))

;;;###autoload
(defun ibuffer-mark-by-content-regexp (regexp &optional all-buffers)
  "Mark all buffers whose content matches REGEXP.
Optional arg ALL-BUFFERS, if non-nil, then search in all buffers.
Otherwise buffers whose name matches an element of
`ibuffer-never-search-content-name' or whose major mode is on
`ibuffer-never-search-content-mode' are excluded."
  (interactive (let ((reg (read-string "Mark by content (regexp): ")))
                 (list reg current-prefix-arg)))
  (ibuffer-mark-on-buffer
   (lambda (buf)
     (let ((mode (with-current-buffer buf major-mode))
           res)
       (cond ((and (not all-buffers)
                   (or
                    (memq mode ibuffer-never-search-content-mode)
                    (cl-dolist (x ibuffer-never-search-content-name nil)
                      (when-let ((found (string-match x (buffer-name buf))))
                        (cl-return found)))))
              (setq res nil))
             (t
              (with-current-buffer buf
                (save-mark-and-excursion
                  (goto-char (point-min))
                  (setq res (re-search-forward regexp nil t)))))) res))))

;;;###autoload
(defun ibuffer-mark-by-mode (mode)
  "Mark all buffers whose major mode equals MODE."
  (interactive
   (let* ((buf (ibuffer-current-buffer))
          (default (if (and buf (buffer-live-p buf))
                       (symbol-name (buffer-local-value
                                     'major-mode buf)))))
     (list (intern
            (completing-read
             (format-prompt "Mark by major mode" default)
             (ibuffer-list-buffer-modes) nil t nil nil default)))))
  (ibuffer-mark-on-buffer
   (lambda (buf)
     (eq (buffer-local-value 'major-mode buf) mode))))

;;;###autoload
(defun ibuffer-mark-modified-buffers ()
  "Mark all modified buffers."
  (interactive)
  (ibuffer-mark-on-buffer
   (lambda (buf) (buffer-modified-p buf))))

;;;###autoload
(defun ibuffer-mark-unsaved-buffers ()
  "Mark all modified buffers that have an associated file."
  (interactive)
  (ibuffer-mark-on-buffer
   (lambda (buf) (and (buffer-local-value 'buffer-file-name buf)
                 (buffer-modified-p buf)))))

;;;###autoload
(defun ibuffer-mark-dissociated-buffers ()
  "Mark all buffers whose associated file does not exist."
  (interactive)
  (ibuffer-mark-on-buffer
   (lambda (buf)
     (with-current-buffer buf
       (or
        (and buffer-file-name
             (not (file-exists-p buffer-file-name)))
        (and (eq major-mode 'dired-mode)
             (boundp 'dired-directory)
             (stringp dired-directory)
             (not (file-exists-p (file-name-directory dired-directory)))))))))

;;;###autoload
(defun ibuffer-mark-help-buffers ()
  "Mark buffers whose major mode is in variable `ibuffer-help-buffer-modes'."
  (interactive)
  (ibuffer-mark-on-buffer
   (lambda (buf)
     (with-current-buffer buf
       (memq major-mode ibuffer-help-buffer-modes)))))

;;;###autoload
(defun ibuffer-mark-compressed-file-buffers ()
  "Mark buffers whose associated file is compressed."
  (interactive)
  (ibuffer-mark-on-buffer
   (lambda (buf)
     (with-current-buffer buf
       (and buffer-file-name
            (string-match ibuffer-compressed-file-name-regexp
                          buffer-file-name))))))

;;;###autoload
(defun ibuffer-mark-old-buffers ()
  "Mark buffers which have not been viewed in `ibuffer-old-time' hours."
  (interactive)
  (ibuffer-mark-on-buffer
   (lambda (buf)
     (with-current-buffer buf
       (when buffer-display-time
         (time-less-p
          (* 60 60 ibuffer-old-time)
          (time-since buffer-display-time)))))))

;;;###autoload
(defun ibuffer-mark-special-buffers ()
  "Mark all buffers whose name begins and ends with `*'."
  (interactive)
  (ibuffer-mark-on-buffer
   (lambda (buf) (string-match "^\\*.+\\*$"
                          (buffer-name buf)))))

;;;###autoload
(defun ibuffer-mark-read-only-buffers ()
  "Mark all read-only buffers."
  (interactive)
  (ibuffer-mark-on-buffer
   (lambda (buf) (buffer-local-value 'buffer-read-only buf))))

;;;###autoload
(defun ibuffer-mark-dired-buffers ()
  "Mark all `dired' buffers."
  (interactive)
  (ibuffer-mark-on-buffer
   (lambda (buf) (eq (buffer-local-value 'major-mode buf) 'dired-mode))))

;;;###autoload
(defun ibuffer-do-occur (regexp &optional nlines)
  "View lines which match REGEXP in all marked buffers.
Optional argument NLINES says how many lines of context to display: it
defaults to one."
  (interactive (occur-read-primary-args))
  (if (or (not (integerp nlines))
	  (< nlines 0))
      (setq nlines 0))
  (when (zerop (ibuffer-count-marked-lines))
    (ibuffer-set-mark ibuffer-marked-char))
  (let ((ibuffer-do-occur-bufs nil))
    ;; Accumulate a list of marked buffers
    (ibuffer-map-marked-lines
     (lambda (buf _mark)
       (push buf ibuffer-do-occur-bufs)))
    (occur-1 regexp nlines ibuffer-do-occur-bufs)))

(provide 'ibuf-ext)

;; Local Variables:
;; generated-autoload-file: "ibuffer-loaddefs.el"
;; End:

;;; ibuf-ext.el ends here
