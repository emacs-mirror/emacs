;;; easy-mmode.el --- easy definition for major and minor modes

;; Copyright (C) 1997, 2000-2018 Free Software Foundation, Inc.

;; Author: Georges Brun-Cottan <Georges.Brun-Cottan@inria.fr>
;; Maintainer: Stefan Monnier <monnier@gnu.org>
;; Package: emacs

;; Keywords: extensions lisp

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

;; Minor modes are useful and common.  This package makes defining a
;; minor mode easy, by focusing on the writing of the minor mode
;; functionalities themselves.  Moreover, this package enforces a
;; conventional naming of user interface primitives, making things
;; natural for the minor-mode end-users.

;; For each mode, easy-mmode defines the following:
;; <mode>      : The minor mode predicate. A buffer-local variable.
;; <mode>-map  : The keymap possibly associated to <mode>.
;;       see `define-minor-mode' documentation
;;
;; eval
;;  (pp (macroexpand '(define-minor-mode <your-mode> <doc>)))
;; to check the result before using it.

;; The order in which minor modes are installed is important.  Keymap
;; lookup proceeds down minor-mode-map-alist, and the order there
;; tends to be the reverse of the order in which the modes were
;; installed.  Perhaps there should be a feature to let you specify
;; orderings.

;; Additionally to `define-minor-mode', the package provides convenient
;; ways to define keymaps, and other helper functions for major and minor modes.

;;; Code:

(defun easy-mmode-pretty-mode-name (mode &optional lighter)
  "Turn the symbol MODE into a string intended for the user.
If provided, LIGHTER will be used to help choose capitalization by,
replacing its case-insensitive matches with the literal string in LIGHTER."
  (let* ((case-fold-search t)
	 ;; Produce "Foo-Bar minor mode" from foo-bar-minor-mode.
	 (name (concat (replace-regexp-in-string
			;; If the original mode name included "-minor" (some
			;; of them don't, e.g. auto-revert-mode), then
			;; replace it with " minor".
			"-Minor" " minor"
			;; "foo-bar-minor" -> "Foo-Bar-Minor"
			(capitalize (replace-regexp-in-string
				     ;; "foo-bar-minor-mode" -> "foo-bar-minor"
				     "toggle-\\|-mode\\'" ""
                                     (symbol-name mode))))
		       " mode")))
    (setq name (replace-regexp-in-string "\\`Global-" "Global " name))
    (if (not (stringp lighter)) name
      ;; Strip leading and trailing whitespace from LIGHTER.
      (setq lighter (replace-regexp-in-string "\\`\\s-+\\|\\s-+\\'" ""
					      lighter))
      ;; Replace any (case-insensitive) matches for LIGHTER in NAME
      ;; with a literal LIGHTER.  E.g., if NAME is "Iimage mode" and
      ;; LIGHTER is " iImag", then this will produce "iImage mode".
      ;; (LIGHTER normally comes from the mode-line string passed to
      ;; define-minor-mode, and normally includes at least one leading
      ;; space.)
      (replace-regexp-in-string (regexp-quote lighter) lighter name t t))))

;;;###autoload
(defalias 'easy-mmode-define-minor-mode 'define-minor-mode)
;;;###autoload
(defmacro define-minor-mode (mode doc &optional init-value lighter keymap &rest body)
  "Define a new minor mode MODE.
This defines the toggle command MODE and (by default) a control variable
MODE (you can override this with the :variable keyword, see below).
DOC is the documentation for the mode toggle command.

The defined mode command takes one optional (prefix) argument.
Interactively with no prefix argument, it toggles the mode.
A prefix argument enables the mode if the argument is positive,
and disables it otherwise.

When called from Lisp, the mode command toggles the mode if the
argument is `toggle', disables the mode if the argument is a
non-positive integer, and enables the mode otherwise (including
if the argument is omitted or nil or a positive integer).

If DOC is nil, give the mode command a basic doc-string
documenting what its argument does.

Optional INIT-VALUE is the initial value of the mode's variable.
Optional LIGHTER is displayed in the mode line when the mode is on.
Optional KEYMAP is the default keymap bound to the mode keymap.
  If non-nil, it should be a variable name (whose value is a keymap),
  or an expression that returns either a keymap or a list of
  (KEY . BINDING) pairs where KEY and BINDING are suitable for
  `define-key'.  If you supply a KEYMAP argument that is not a
  symbol, this macro defines the variable MODE-map and gives it
  the value that KEYMAP specifies.

BODY contains code to execute each time the mode is enabled or disabled.
  It is executed after toggling the mode, and before running MODE-hook.
  Before the actual body code, you can write keyword arguments, i.e.
  alternating keywords and values.  If you provide BODY, then you must
  provide (even if just nil) INIT-VALUE, LIGHTER, and KEYMAP, or provide
  at least one keyword argument, or both; otherwise, BODY would be
  misinterpreted as the first omitted argument.  The following special
  keywords are supported (other keywords are passed to `defcustom' if
  the minor mode is global):

:group GROUP	Custom group name to use in all generated `defcustom' forms.
		Defaults to MODE without the possible trailing \"-mode\".
		Don't use this default group name unless you have written a
		`defgroup' to define that group properly.
:global GLOBAL	If non-nil specifies that the minor mode is not meant to be
		buffer-local, so don't make the variable MODE buffer-local.
		By default, the mode is buffer-local.
:init-value VAL	Same as the INIT-VALUE argument.
		Not used if you also specify :variable.
:lighter SPEC	Same as the LIGHTER argument.
:keymap MAP	Same as the KEYMAP argument.
:require SYM	Same as in `defcustom'.
:variable PLACE	The location to use instead of the variable MODE to store
		the state of the mode.	This can be simply a different
		named variable, or a generalized variable.
		PLACE can also be of the form (GET . SET), where GET is
		an expression that returns the current state, and SET is
		a function that takes one argument, the new state, and
		sets it.  If you specify a :variable, this function does
		not define a MODE variable (nor any of the terms used
		in :variable).

:after-hook     A single lisp form which is evaluated after the mode hooks
                have been run.  It should not be quoted.

For example, you could write
  (define-minor-mode foo-mode \"If enabled, foo on you!\"
    :lighter \" Foo\" :require \\='foo :global t :group \\='hassle :version \"27.5\"
    ...BODY CODE...)"
  (declare (doc-string 2)
           (debug (&define name string-or-null-p
			   [&optional [&not keywordp] sexp
			    &optional [&not keywordp] sexp
			    &optional [&not keywordp] sexp]
			   [&rest [keywordp sexp]]
			   def-body)))

  ;; Allow skipping the first three args.
  (cond
   ((keywordp init-value)
    (setq body (if keymap `(,init-value ,lighter ,keymap ,@body)
		 `(,init-value ,lighter))
	  init-value nil lighter nil keymap nil))
   ((keywordp lighter)
    (setq body `(,lighter ,keymap ,@body) lighter nil keymap nil))
   ((keywordp keymap) (push keymap body) (setq keymap nil)))

  (let* ((last-message (make-symbol "last-message"))
         (mode-name (symbol-name mode))
	 (pretty-name (easy-mmode-pretty-mode-name mode lighter))
	 (globalp nil)
	 (set nil)
	 (initialize nil)
	 (group nil)
	 (type nil)
	 (extra-args nil)
	 (extra-keywords nil)
         (variable nil)          ;The PLACE where the state is stored.
         (setter `(setq ,mode))  ;The beginning of the exp to set the mode var.
         (getter mode)           ;The exp to get the mode value.
         (modefun mode)          ;The minor mode function name we're defining.
	 (require t)
	 (after-hook nil)
	 (hook (intern (concat mode-name "-hook")))
	 (hook-on (intern (concat mode-name "-on-hook")))
	 (hook-off (intern (concat mode-name "-off-hook")))
	 keyw keymap-sym tmp)

    ;; Check keys.
    (while (keywordp (setq keyw (car body)))
      (setq body (cdr body))
      (pcase keyw
	(`:init-value (setq init-value (pop body)))
	(`:lighter (setq lighter (purecopy (pop body))))
	(`:global (setq globalp (pop body))
         (when (and globalp (symbolp mode))
           (setq setter `(setq-default ,mode))
           (setq getter `(default-value ',mode))))
	(`:extra-args (setq extra-args (pop body)))
	(`:set (setq set (list :set (pop body))))
	(`:initialize (setq initialize (list :initialize (pop body))))
	(`:group (setq group (nconc group (list :group (pop body)))))
	(`:type (setq type (list :type (pop body))))
	(`:require (setq require (pop body)))
	(`:keymap (setq keymap (pop body)))
        (`:variable (setq variable (pop body))
         (if (not (and (setq tmp (cdr-safe variable))
                       (or (symbolp tmp)
                           (functionp tmp))))
             ;; PLACE is not of the form (GET . SET).
             (progn
               (setq setter `(setf ,variable))
               (setq getter variable))
           (setq getter (car variable))
           (setq setter `(funcall #',(cdr variable)))))
	(`:after-hook (setq after-hook (pop body)))
	(_ (push keyw extra-keywords) (push (pop body) extra-keywords))))

    (setq keymap-sym (if (and keymap (symbolp keymap)) keymap
		       (intern (concat mode-name "-map"))))

    (unless set (setq set '(:set #'custom-set-minor-mode)))

    (unless initialize
      (setq initialize '(:initialize 'custom-initialize-default)))

    (unless group
      ;; We might as well provide a best-guess default group.
      (setq group
	    `(:group ',(intern (replace-regexp-in-string
				"-mode\\'" "" mode-name)))))

    ;; TODO? Mark booleans as safe if booleanp?  Eg abbrev-mode.
    (unless type (setq type '(:type 'boolean)))

    `(progn
       ;; Define the variable to enable or disable the mode.
       ,(cond
         ;; If :variable is specified, then the var will be
         ;; declared elsewhere.
         (variable nil)
         ((not globalp)
          `(progn
             :autoload-end
             (defvar ,mode ,init-value ,(format "Non-nil if %s is enabled.
Use the command `%s' to change this variable." pretty-name mode))
             (make-variable-buffer-local ',mode)))
         (t
	  (let ((base-doc-string
                 (concat "Non-nil if %s is enabled.
See the `%s' command
for a description of this minor mode."
                         (if body "
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `%s'."))))
	    `(defcustom ,mode ,init-value
	       ,(format base-doc-string pretty-name mode mode)
	       ,@set
	       ,@initialize
	       ,@group
	       ,@type
	       ,@(unless (eq require t) `(:require ,require))
               ,@(nreverse extra-keywords)))))

       ;; The actual function.
       (defun ,modefun (&optional arg ,@extra-args)
	 ,(or doc
	      (format (concat "Toggle %s on or off.
With a prefix argument ARG, enable %s if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.
\\{%s}") pretty-name pretty-name keymap-sym))
	 ;; Use `toggle' rather than (if ,mode 0 1) so that using
	 ;; repeat-command still does the toggling correctly.
	 (interactive (list (or current-prefix-arg 'toggle)))
	 (let ((,last-message (current-message)))
           (,@setter
            (if (eq arg 'toggle)
                (not ,getter)
              ;; A nil argument also means ON now.
              (> (prefix-numeric-value arg) 0)))
           ,@body
           ;; The on/off hooks are here for backward compatibility only.
           (run-hooks ',hook (if ,getter ',hook-on ',hook-off))
           (if (called-interactively-p 'any)
               (progn
                 ,(if (and globalp (not variable))
                      `(customize-mark-as-set ',mode))
                 ;; Avoid overwriting a message shown by the body,
                 ;; but do overwrite previous messages.
                 (unless (and (current-message)
                              (not (equal ,last-message
                                          (current-message))))
                   (let ((local ,(if globalp "" " in current buffer")))
		     (message ,(format "%s %%sabled%%s" pretty-name)
			      (if ,getter "en" "dis") local)))))
	   ,@(when after-hook `(,after-hook)))
	 (force-mode-line-update)
	 ;; Return the new setting.
	 ,getter)

       ;; Autoloading a define-minor-mode autoloads everything
       ;; up-to-here.
       :autoload-end

       (defvar ,hook nil)
       (unless (get ',hook 'variable-documentation)
         (put ',hook 'variable-documentation
              ,(format "Hook run after entering or leaving `%s'.
No problems result if this variable is not bound.
`add-hook' automatically binds it.  (This is true for all hook variables.)"
                       modefun)))

       ;; Define the minor-mode keymap.
       ,(unless (symbolp keymap)	;nil is also a symbol.
	  `(defvar ,keymap-sym
	     (let ((m ,keymap))
	       (cond ((keymapp m) m)
		     ((listp m) (easy-mmode-define-keymap m))
		     (t (error "Invalid keymap %S" m))))
	     ,(format "Keymap for `%s'." mode-name)))

       ,(let ((modevar (pcase getter (`(default-value ',v) v) (_ getter))))
          (if (not (symbolp modevar))
              (if (or lighter keymap)
                  (error ":lighter and :keymap unsupported with mode expression %S" getter))
            `(with-no-warnings
               (add-minor-mode ',modevar ',lighter
                               ,(if keymap keymap-sym
                                  `(if (boundp ',keymap-sym) ,keymap-sym))
                               nil
                               ,(unless (eq mode modefun) `',modefun))))))))

;;;
;;; make global minor mode
;;;

;;;###autoload
(defalias 'easy-mmode-define-global-mode 'define-globalized-minor-mode)
;;;###autoload
(defalias 'define-global-minor-mode 'define-globalized-minor-mode)
;;;###autoload
(defmacro define-globalized-minor-mode (global-mode mode turn-on &rest keys)
  "Make a global mode GLOBAL-MODE corresponding to buffer-local minor MODE.
TURN-ON is a function that will be called with no args in every buffer
  and that should try to turn MODE on if applicable for that buffer.
KEYS is a list of CL-style keyword arguments.  As the minor mode
  defined by this function is always global, any :global keyword is
  ignored.  Other keywords have the same meaning as in `define-minor-mode',
  which see.  In particular, :group specifies the custom group.
  The most useful keywords are those that are passed on to the
  `defcustom'.  It normally makes no sense to pass the :lighter
  or :keymap keywords to `define-globalized-minor-mode', since these
  are usually passed to the buffer-local version of the minor mode.

If MODE's set-up depends on the major mode in effect when it was
enabled, then disabling and reenabling MODE should make MODE work
correctly with the current major mode.  This is important to
prevent problems with derived modes, that is, major modes that
call another major mode in their body.

When a major mode is initialized, MODE is actually turned on just
after running the major mode's hook.  However, MODE is not turned
on if the hook has explicitly disabled it."
  (declare (doc-string 2))
  (let* ((global-mode-name (symbol-name global-mode))
	 (mode-name (symbol-name mode))
	 (pretty-name (easy-mmode-pretty-mode-name mode))
	 (pretty-global-name (easy-mmode-pretty-mode-name global-mode))
	 (group nil)
	 (extra-keywords nil)
	 (MODE-buffers (intern (concat global-mode-name "-buffers")))
	 (MODE-enable-in-buffers
	  (intern (concat global-mode-name "-enable-in-buffers")))
	 (MODE-check-buffers
	  (intern (concat global-mode-name "-check-buffers")))
	 (MODE-cmhh (intern (concat global-mode-name "-cmhh")))
	 (minor-MODE-hook (intern (concat mode-name "-hook")))
	 (MODE-set-explicitly (intern (concat mode-name "-set-explicitly")))
	 (MODE-major-mode (intern (concat (symbol-name mode) "-major-mode")))
	 keyw)

    ;; Check keys.
    (while (keywordp (setq keyw (car keys)))
      (setq keys (cdr keys))
      (pcase keyw
	(`:group (setq group (nconc group (list :group (pop keys)))))
	(`:global (setq keys (cdr keys)))
	(_ (push keyw extra-keywords) (push (pop keys) extra-keywords))))

    (unless group
      ;; We might as well provide a best-guess default group.
      (setq group
	    `(:group ',(intern (replace-regexp-in-string
				"-mode\\'" "" (symbol-name mode))))))

    `(progn
       (progn
         :autoload-end
         (defvar ,MODE-major-mode nil)
         (make-variable-buffer-local ',MODE-major-mode))
       ;; The actual global minor-mode
       (define-minor-mode ,global-mode
	 ;; Very short lines to avoid too long lines in the generated
	 ;; doc string.
	 ,(format "Toggle %s in all buffers.
With prefix ARG, enable %s if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

%s is enabled in all buffers where
`%s' would do it.
See `%s' for more information on %s."
		  pretty-name pretty-global-name
		  pretty-name turn-on mode pretty-name)
	 :global t ,@group ,@(nreverse extra-keywords)

	 ;; Setup hook to handle future mode changes and new buffers.
	 (if ,global-mode
	     (progn
	       (add-hook 'after-change-major-mode-hook
			 ',MODE-enable-in-buffers)
	       (add-hook 'find-file-hook ',MODE-check-buffers)
	       (add-hook 'change-major-mode-hook ',MODE-cmhh))
	   (remove-hook 'after-change-major-mode-hook ',MODE-enable-in-buffers)
	   (remove-hook 'find-file-hook ',MODE-check-buffers)
	   (remove-hook 'change-major-mode-hook ',MODE-cmhh))

	 ;; Go through existing buffers.
	 (dolist (buf (buffer-list))
	   (with-current-buffer buf
	     (if ,global-mode (funcall #',turn-on) (when ,mode (,mode -1))))))

       ;; Autoloading define-globalized-minor-mode autoloads everything
       ;; up-to-here.
       :autoload-end

       ;; MODE-set-explicitly is set in MODE-set-explicitly and cleared by
       ;; kill-all-local-variables.
       (defvar-local ,MODE-set-explicitly nil)
       (defun ,MODE-set-explicitly ()
         (setq ,MODE-set-explicitly t))
       (put ',MODE-set-explicitly 'definition-name ',global-mode)

       ;; A function which checks whether MODE has been disabled in the major
       ;; mode hook which has just been run.
       (add-hook ',minor-MODE-hook ',MODE-set-explicitly)

       ;; List of buffers left to process.
       (defvar ,MODE-buffers nil)

       ;; The function that calls TURN-ON in each buffer.
       (defun ,MODE-enable-in-buffers ()
	 (dolist (buf ,MODE-buffers)
	   (when (buffer-live-p buf)
	     (with-current-buffer buf
               (unless ,MODE-set-explicitly
		 (unless (eq ,MODE-major-mode major-mode)
		   (if ,mode
		       (progn
			 (,mode -1)
			 (funcall #',turn-on))
		     (funcall #',turn-on))))
	       (setq ,MODE-major-mode major-mode)))))
       (put ',MODE-enable-in-buffers 'definition-name ',global-mode)

       (defun ,MODE-check-buffers ()
	 (,MODE-enable-in-buffers)
	 (setq ,MODE-buffers nil)
	 (remove-hook 'post-command-hook ',MODE-check-buffers))
       (put ',MODE-check-buffers 'definition-name ',global-mode)

       ;; The function that catches kill-all-local-variables.
       (defun ,MODE-cmhh ()
	 (add-to-list ',MODE-buffers (current-buffer))
	 (add-hook 'post-command-hook ',MODE-check-buffers))
       (put ',MODE-cmhh 'definition-name ',global-mode))))

;;;
;;; easy-mmode-defmap
;;;

(defun easy-mmode-set-keymap-parents (m parents)
  (set-keymap-parent
   m (if (cdr parents) (make-composed-keymap parents) (car parents))))

;;;###autoload
(defun easy-mmode-define-keymap (bs &optional name m args)
  "Return a keymap built from bindings BS.
BS must be a list of (KEY . BINDING) where
KEY and BINDINGS are suitable for `define-key'.
Optional NAME is passed to `make-sparse-keymap'.
Optional map M can be used to modify an existing map.
ARGS is a list of additional keyword arguments.

Valid keywords and arguments are:

  :name      Name of the keymap; overrides NAME argument.
  :dense     Non-nil for a dense keymap.
  :inherit   Parent keymap.
  :group     Ignored.
  :suppress  Non-nil to call `suppress-keymap' on keymap,
             `nodigits' to suppress digits as prefix arguments."
  (let (inherit dense suppress)
    (while args
      (let ((key (pop args))
	    (val (pop args)))
	(pcase key
	 (`:name (setq name val))
	 (`:dense (setq dense val))
	 (`:inherit (setq inherit val))
	 (`:suppress (setq suppress val))
	 (`:group)
	 (_ (message "Unknown argument %s in defmap" key)))))
    (unless (keymapp m)
      (setq bs (append m bs))
      (setq m (if dense (make-keymap name) (make-sparse-keymap name))))
    (when suppress
      (suppress-keymap m (eq suppress 'nodigits)))
    (dolist (b bs)
      (let ((keys (car b))
	    (binding (cdr b)))
	(dolist (key (if (consp keys) keys (list keys)))
	  (cond
	   ((symbolp key)
	    (substitute-key-definition key binding m global-map))
	   ((null binding)
	    (unless (keymapp (lookup-key m key)) (define-key m key binding)))
	   ((let ((o (lookup-key m key)))
	      (or (null o) (numberp o) (eq o 'undefined)))
	    (define-key m key binding))))))
    (cond
     ((keymapp inherit) (set-keymap-parent m inherit))
     ((consp inherit) (easy-mmode-set-keymap-parents m inherit)))
    m))

;;;###autoload
(defmacro easy-mmode-defmap (m bs doc &rest args)
  "Define a constant M whose value is the result of `easy-mmode-define-keymap'.
The M, BS, and ARGS arguments are as per that function.  DOC is
the constant's documentation."
  (declare (indent 1))
  `(defconst ,m
     (easy-mmode-define-keymap ,bs nil (if (boundp ',m) ,m) ,(cons 'list args))
     ,doc))


;;;
;;; easy-mmode-defsyntax
;;;

(defun easy-mmode-define-syntax (css args)
  (let ((st (make-syntax-table (plist-get args :copy)))
	(parent (plist-get args :inherit)))
    (dolist (cs css)
      (let ((char (car cs))
	    (syntax (cdr cs)))
	(if (sequencep char)
	    (mapc (lambda (c) (modify-syntax-entry c syntax st)) char)
	  (modify-syntax-entry char syntax st))))
    (if parent (set-char-table-parent
		st (if (symbolp parent) (symbol-value parent) parent)))
    st))

;;;###autoload
(defmacro easy-mmode-defsyntax (st css doc &rest args)
  "Define variable ST as a syntax-table.
CSS contains a list of syntax specifications of the form (CHAR . SYNTAX)."
  (declare (indent 1))
  `(progn
     (autoload 'easy-mmode-define-syntax "easy-mmode")
     (defconst ,st (easy-mmode-define-syntax ,css ,(cons 'list args)) ,doc)))



;;;
;;; easy-mmode-define-navigation
;;;

(defmacro easy-mmode-define-navigation (base re &optional name endfun narrowfun
                                             &rest body)
  "Define BASE-next and BASE-prev to navigate in the buffer.
RE determines the places the commands should move point to.
NAME should describe the entities matched by RE.  It is used to build
  the docstrings of the two functions.
BASE-next also tries to make sure that the whole entry is visible by
  searching for its end (by calling ENDFUN if provided or by looking for
  the next entry) and recentering if necessary.
ENDFUN should return the end position (with or without moving point).
NARROWFUN non-nil means to check for narrowing before moving, and if
found, do `widen' first and then call NARROWFUN with no args after moving.
BODY is executed after moving to the destination location."
  (declare (indent 5) (debug (exp exp exp def-form def-form &rest def-body)))
  (let* ((base-name (symbol-name base))
	 (prev-sym (intern (concat base-name "-prev")))
	 (next-sym (intern (concat base-name "-next")))
         (when-narrowed
          (lambda (body)
            (if (null narrowfun) body
              `(let ((was-narrowed
                      (prog1 (or (< (- (point-max) (point-min)) (buffer-size)))
                        (widen))))
                 ,body
                 (when was-narrowed (funcall #',narrowfun)))))))
    (unless name (setq name base-name))
    `(progn
       (defun ,next-sym (&optional count)
	 ,(format "Go to the next COUNT'th %s." name)
	 (interactive "p")
	 (unless count (setq count 1))
	 (if (< count 0) (,prev-sym (- count))
	   (if (looking-at ,re) (setq count (1+ count)))
           ,(funcall when-narrowed
             `(if (not (re-search-forward ,re nil t count))
                  (if (looking-at ,re)
                      (goto-char (or ,(if endfun `(funcall #',endfun)) (point-max)))
                    (user-error "No next %s" ,name))
                (goto-char (match-beginning 0))
                (when (and (eq (current-buffer) (window-buffer))
                           (called-interactively-p 'interactive))
                  (let ((endpt (or (save-excursion
                                     ,(if endfun `(funcall #',endfun)
                                        `(re-search-forward ,re nil t 2)))
                                   (point-max))))
                    (unless (pos-visible-in-window-p endpt nil t)
                      (recenter '(0)))))))
           ,@body))
       (put ',next-sym 'definition-name ',base)
       (defun ,prev-sym (&optional count)
	 ,(format "Go to the previous COUNT'th %s" (or name base-name))
	 (interactive "p")
	 (unless count (setq count 1))
	 (if (< count 0) (,next-sym (- count))
           ,(funcall when-narrowed
             `(unless (re-search-backward ,re nil t count)
                (user-error "No previous %s" ,name)))
           ,@body))
       (put ',prev-sym 'definition-name ',base))))


(provide 'easy-mmode)

;;; easy-mmode.el ends here
