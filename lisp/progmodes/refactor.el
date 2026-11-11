;;; refactor.el --- Language-agnostic refactoring interface  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.
;; Author: João Távora <joaotavora@gmail.com>

;; Version: 0.1.1
;; Keywords: convenience, languages
;; Package-Requires: ((emacs "26.3") (eldoc "1.14.0"))

;; This is a GNU ELPA :core package.  Avoid adding functionality that
;; is not available in the version of Emacs recorded above.

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

;; `refactor.el' is a library for adding refactoring support for any
;; programming language.  A refactoring, or refactor, is any rewrite to
;; a program that maintains its intended meaning, while (hopefully)
;; improving it.  It could be extracting a function, inlining a
;; variable, renaming an identifier, organizing imports, or
;; automatically fixing a diagnostic.  In `refactor.el' each such
;; potential rewrite is called an _action_ and embodied by the
;; `refactor-action' object.
;;
;; Note that this library is completely agnostic about the programming
;; language and major mode.  By itself, it has no knowledge of how these
;; actions are discovered and eventually carried out: that's for other
;; Elisp programs to define and set up, in the form of `refactor.el'
;; _backends_.  A very significant part of the code was extracted from
;; Emacs' LSP client Eglot, which now contains a simpler backend that
;; makes use of refactor.el.
;;
;;;; User interface
;;
;; In buffers where a working backend is set up, the library provides
;; some useful UI: a number of interactive commands for selecting
;; actions -- `refactor', `refactor-rename' and a few more shortcut
;; commands -- as well as the `refactor-suggestion-mode' minor mode,
;; which leverages the ElDoc system and endeavors to constantly keep the
;; user informed of which actions are available.  This minor mode is the
;; familiar "lightning bolt/lightbulb on the side" that some IDEs
;; display.  The variable `refactor-indications' is its main point of
;; customization.
;;
;; Each action has a "kind", which is used by the aforementioned
;; shortcut commands.  For example, `refactor-quickfix' restricts
;; actions at point to the specific kind `quickfix' and ignores are
;; actions of other kids (like, say `extract' or `inline').  Depending
;; on the number of actions avaiable and the specific command, actions
;; are either automatically selected or presented to the user for
;; selection.
;;
;; Once an action is selected, it is executed, but its changes aren't
;; necessarily applied right away.  The variable `refactor-confirmation'
;; controls whether and how the user confirms them first: as a summary
;; in the minibuffer, as a diff of the proposed changes, or not at all.
;; The choice can depend on the command, on the kinds of file
;; operations involved, and on whether the affected files are visited
;; in buffers.
;;
;;;; Elisp API
;;
;; The Elisp-facing parts of this library are useful for backend
;; authors: there are two distinct Elisp-facing interfaces, independent
;; of each other.  Either is useful alone, though they can and normally
;; work together.
;;
;; - Action discovery and selection
;;
;;   The interactive commands are entry points that communicate with
;;   "refactor backends", i.e. any object returned by
;;   `refactor-backend-functions'.  The base `refactor' command asks
;;   backends via `refactor-backend-actions' what "actions"
;;   (i.e. potential refactorings) can be carried out for the buffer
;;   near point.  This is much like `xref-backend-functions' except that
;;   unlike Xref, every applicable backend contributes.  The backend who
;;   provided the chosen action (regardless of how it was chosen) is
;;   asked again to carry it out.  This invokes its implementation of
;;   the `refactor-backend-execute' method.  Executing an action could
;;   entail changes to the current buffer or to other buffers, even
;;   changes to unvisited files, creation or deletion of files: the
;;   library puts no constraints on the scope of an action.
;;
;; - Changeset application:
;;
;;   To simplify and give consistency to the execution of actions,
;;   we use the concept of _changeset_, an ordered list of
;;   `refactor-operation' objects.  Though not required, a normal
;;   implementation of `refactor-backend-execute' would compute such a
;;   changeset, then offer to apply it by calling
;;   `refactor-apply-changeset'.  `refactor-apply-changeset' carries out
;;   each `refactor-operation' -- edits to files, creation of files,
;;   renaming and deletion of files -- according to certain rules.  For
;;   example, it may offer a preview to the user as a summarized
;;   description and ask for confirmation in the minibuffer, or it may
;;   present the changeset as a diff, or it may skip confirmation
;;   altogether.  This is according to the user preferences in
;;   `refactor-confirmation'.  Note that an Elisp program may call this
;;   directly for applying changes in a consistent and uniform manner
;;   outside the context of `refactor-backend-execute'.
;;
;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'flymake)
(eval-when-compile (require 'subr-x))

(defgroup refactor nil
  "Refactoring support." ; hmmm, a bit short no?
  :prefix "refactor-"
  :group 'tools)

;;;; Backends
;; JT@2026-08-21: While a hook may be overkill (a plain buffer-local
;; variable would likely do), it matches the Xref precedent.

(defvar refactor-backend-functions nil
  "Special hook to find the refactor backends for the current context.
Each function in this hook is called in turn with no arguments, and
should return nil if it is not applicable, or a refactor backend, that
is, a value on which the `refactor-backend-*' generic functions
dispatch.  Unlike `xref-backend-functions' (which inspired this) every
backend contributes: for example the actions each supplies are merged
into a single list for the user to choose from.")

(defun refactor-find-backends ()
  "Return the refactor backends applicable in the current context.
Run every function on `refactor-backend-functions' in turn and
collect the non-nil backends they return."
  (let (retval)
    (run-hook-wrapped 'refactor-backend-functions
                      (lambda (a)
                        (when-let* ((x (funcall a))) (push x retval))
                        nil))
    (reverse retval)))

(cl-defgeneric refactor-backend-name (backend)
  "Return a short human-readable name for BACKEND."
  (:method (backend) (symbol-name backend)))

(cl-defgeneric refactor-backend-actions
    (backend beg end &key rkind callback trigger-kind)
  "Compute refactoring actions BACKEND offers between BEG and END.

RKIND, if non-nil, restricts the result to that kind and its
sub-kinds.

If CALLBACK is nil, return the list of `refactor-action' objects
directly; blocking to do so is acceptable.

If CALLBACK is non-nil, either return the list of actions directly
anyway (when that is cheap) or return `:async' after arranging for
CALLBACK to be called with the list of actions eventually (but at most
once).  CALLBACK may also be invoked synchronously, before this
function returns.")

(cl-defgeneric refactor-backend-execute (backend action)
  "Have BACKEND carry ACTION out, by whatever means it likes."
  (:method (_backend _action)
   (error "Refactor backend doesn't know how to execute actions")))

(cl-defgeneric refactor-backend-rename-bounds (backend)
  "Return bounds of thing BACKEND could rename near point.
If the return value is a cons of buffer positions (BEG . END), these are
the bounds of the nearby thing that BACKEND claims as renameable.
A nil return means BACKEND does not claim this as renameable."
  (:method (_backend) nil))

(cl-defgeneric refactor-backend-rename (backend newname)
  "Compute a changeset renaming the identifier at point to NEWNAME."
  (:method (_backend _newname) nil))


;;;; Utils

(defmacro refactor--defclass (name superclasses docstring &rest specs)
  "Util for cutting down on defclass boilerplate.
NAME, SUPERCLASSES and DOCSTRING are as in `defclass'.  Each of SPECS
is (SLOT INITFORM SLOT-DOCSTRING)."
  (declare (indent 2) (debug (&define name sexp stringp &rest sexp)))
  `(defclass ,name ,superclasses
     ,(mapcar (lambda (spec)
                (cl-destructuring-bind (slot initform slotdoc) spec
                  `(,slot :initarg ,(intern (concat ":" (symbol-name slot)))
                          :initform ,initform
                          :documentation ,slotdoc)))
              specs)
     :documentation ,docstring))


;;;; Actions

(refactor--defclass refactor-action ()
  "A refactoring a backend offers to perform."
  (title nil "One-line description, shown to the user.")
  (kind nil "A symbol from `refactor-kinds', or nil.")
  (preferred nil "Non-nil if this is the obvious choice here.")
  (backend nil "The backend that offered this action.")
  (data nil "Opaque payload, meaningful to the backend."))


;;;; Kinds

;; Every action is tagged with a "kind" symbol.  Could be e.g.
;; `quickfix' or `extract'.  Kinds are organized in a hierarchy which is
;; what RKIND in `refactor' matches against.  Specialized commands scope
;; down to just some kinds: so `M-x refactor-quickfix' offers only
;; actions tagged `quickfix'.  In contrast, asking for the `refactor'
;; kind (with a prefix argument to `M-x refactor') also offers
;; `extract', `inline', `rewrite' and `move' ones.  Plain `M-x refactor'
;; doesn't filter, and offers everything.  Backends tag their actions
;; with any of the kinds defined below or may use the macros to define
;; their own.

(defvar refactor-kinds nil
  "List of all refactoring kinds defined so far, in definition order.")

(defmacro refactor-defkind (name parent docstring)
  "Define NAME as a kind of refactoring.
PARENT is the kind NAME specializes, or nil if NAME is itself a
top-level kind.  DOCSTRING describes NAME to the user."
  (declare (doc-string 3) (indent 2))
  `(progn
     (put ',name 'refactor-kind-parent ',parent)
     (put ',name 'refactor-kind-documentation ,docstring)
     (unless (memq ',name refactor-kinds)
       (setq refactor-kinds (append refactor-kinds (list ',name))))
     ',name))

(defun refactor-kind-matches-p (kind filter)
  "Return non-nil if KIND is FILTER or one of its sub-kinds.
A nil FILTER matches everything."
  (or (null filter)
      (cl-loop for k = kind then (get k 'refactor-kind-parent)
               while k thereis (eq k filter))))

(refactor-defkind quickfix nil "Fix a problem reported at point.")
(refactor-defkind refactor nil "Change code without changing what it does.")
(refactor-defkind extract refactor "Extract code into a new named entity.")
(refactor-defkind inline refactor "Inline a named entity into its call sites.")
(refactor-defkind rewrite refactor "Restate code in a different form.")
(refactor-defkind move refactor "Move an entity somewhere else.")
(refactor-defkind source nil "Act on a file rather than on a selection.")
(refactor-defkind organize-imports source "Tidy up import declarations.")
(refactor-defkind fix-all source "Apply every fix available in the file.")


;;;; Changesets

;; A changeset is an ordered list of operations, each an instance of a
;; class below.  Backends make them with `make-instance', and may
;; subclass the classes for operations we haven't thought of;
;; `refactor-apply-changeset' shows them to the user and carries them
;; out.  FIXME: move this to the manual later.

(refactor--defclass refactor-operation ()
  "Superclass of the operations making up a changeset.")

(refactor--defclass refactor-file-edit (refactor-operation)
  "Operation changing the text of a single file."
  (file nil "Absolute name of the file to change.")
  (edits nil "\
Either a list of (BEG END NEWTEXT), where BEG and END are integer
positions valid in the widened buffer visiting the file, or a
function of no arguments returning such a list, called with that
buffer current.  Edits must not overlap."))

(refactor--defclass refactor-file-creation (refactor-operation)
  "Operation creating a file."
  (file nil "Absolute name of the file to create.")
  (contents nil "Initial contents, or nil for an empty file.")
  (if-exists 'error "What to do if file exists: `error', `skip' or `overwrite'."))

(refactor--defclass refactor-file-renaming (refactor-operation)
  "Operation renaming a file."
  (from nil "Absolute name of the file to rename.")
  (to nil "Absolute name to rename it to.")
  (if-exists 'error "What to do if new name is taken: `error', `skip' or `overwrite'."))

(refactor--defclass refactor-file-deletion (refactor-operation)
  "Operation deleting a file."
  (file nil "Absolute name of the file to delete.")
  (recursive nil "Non-nil to delete a directory's contents too.")
  (if-missing 'error "What to do if file does not exist: `error' or `skip'."))

(cl-defgeneric refactor-operation-kind (operation)
  "Return a symbol classifying OPERATION.
One of `edit', `create', `rename' or `delete' (the symbols
`refactor-confirmation' matches against)."
  (:method ((_ refactor-file-edit)) 'edit)
  (:method ((_ refactor-file-creation)) 'create)
  (:method ((_ refactor-file-renaming)) 'rename)
  (:method ((_ refactor-file-deletion)) 'delete))

(cl-defgeneric refactor-describe (operation)
  "Return a one-line description of OPERATION."
  (:method ((op refactor-file-edit))
   (with-slots (file edits) op
     (if (functionp edits)
         (format "Change `%s'" file)
       (format "Change `%s' (%d change%s)" file
               (length edits) (if (cdr edits) "s" "")))))
  (:method ((op refactor-file-creation))
   (format "Create `%s'" (oref op file)))
  (:method ((op refactor-file-renaming))
   (with-slots (from to) op
     (format "Rename `%s' to `%s'" from to)))
  (:method ((op refactor-file-deletion))
   (format "Delete `%s'" (oref op file))))


;;;; Collecting actions

(cl-defun refactor--collect
    (beg end &key rkind callback trigger-kind)
  "Gather actions for BEG END from every applicable backend.

This is the workhorse for `refactor' and `refactor-suggestion', the sync
and async modes of collecting actions, respectively.

If CALLBACK is nil, return the merged list of `refactor-action's.

If CALLBACK is non-nil, return `:async' and call CALLBACK once with the
merged list when every backend has delivered (which may happen before
this function returns, depending on the backends).  A backend that
signals an error counts as having delivered no actions."
  (let ((slots
         ;; Each slot is (BACKEND DELIVERED-P . ACTIONS), DELIVERED-P
         ;; being non-nil once BACKEND has delivered ACTIONS, via its
         ;; callback or by returning them.
         (mapcar (lambda (backend) (list backend nil))
                 (refactor-find-backends))))
    (cl-labels
        ((merged ()
           ;; Keep the first of each title, in hook order rather than
           ;; delivery order, so that earlier backends win clashes even
           ;; if they are slower.
           (cl-remove-duplicates (apply #'append (mapcar #'cddr slots))
                                 :key (lambda (a) (oref a title))
                                 :test #'equal :from-end t))
         (deliver (result slot)
           (setcar (cdr slot) t)
           (setcdr (cdr slot) result)
           (when (and callback (cl-every #'cadr slots))
             (funcall callback (merged))))
         (filter (actions)
           (cl-remove-if-not
            (lambda (a)
              (refactor-kind-matches-p (oref a kind) rkind))
            actions)))
      (dolist (slot slots)
        (condition-case-unless-debug oops
            (let ((result
                   (refactor-backend-actions
                    (car slot) beg end
                    :callback
                    (and callback
                         (lambda (result)
                           (deliver (filter result) slot)))
                    :rkind rkind
                    :trigger-kind trigger-kind)))
              ;; A backend may even call its callback before returning
              ;; actual actions.  When it does, trust the callback's
              ;; result over the return value.
              (unless (or (cadr slot)
                          (eq result :async))
                (deliver (filter result) slot)))
          (error
           (message "refactor: backend %S failed: %S"
                    (car slot) (cdr oops))
           ;; Deliver no actions, unless already delivered.
           (unless (cadr slot) (deliver nil slot)))))
      (if callback :async (merged)))))


;;;; Commands

(defvar refactor--suggestion-overlay)

(defun refactor--bounds ()
  "Return (BEG END) for the current refactoring context.
This is the active region, if any, else it's the span covering any
Flymake diagnostics at point, else the bounds of the expression at
point, else point."
  (let (diags boftap)
    (cond ((use-region-p) `(,(region-beginning) ,(region-end)))
          ((setq diags (flymake-diagnostics (point) (point)))
           (cl-loop for d in diags
                    minimizing (flymake-diagnostic-beg d) into beg
                    maximizing (flymake-diagnostic-end d) into end
                    finally (cl-return (list beg end))))
          ((setq boftap (bounds-of-thing-at-point 'sexp))
           (list (car boftap) (cdr boftap)))
          (t
           (list (point) (point))))))

(defun refactor--read-execute-action (actions)
  "Choose and execute one of ACTIONS, a list of `refactor-action's.
If there is only one, execute it without asking."
  (let* ((menu-items (cl-loop for a in actions
                              collect (cons (oref a title) a)))
         (preferred-action
          (cl-find-if (lambda (menu-item)
                        (oref (cdr menu-item) preferred))
                      menu-items))
         (default-action (car (or preferred-action (car menu-items))))
         (chosen (if (null (cdr menu-items))
                     (cdr (car menu-items))
                   (if (listp last-nonmenu-event)
                       (x-popup-menu last-nonmenu-event
                                     `("Refactor:"
                                       ("dummy" ,@menu-items)))
                     (cdr (assoc (completing-read
                                  (format "Pick a refactoring (default %s): "
                                          default-action)
                                  menu-items nil t nil nil default-action)
                                 menu-items))))))
    (when chosen (refactor-backend-execute (oref chosen backend) chosen))))

;;;###autoload
(defun refactor (beg &optional end rkind interactive)
  "Find refactoring actions between BEG and END, and offer to run them.

If RKIND, a symbol, is non-nil, restrict search to actions of that kind
and its sub-kinds; interactively with a prefix arg, choose from
`refactor-kinds'.

Interactively, BEG and END default to automatically calculated bounds
and a prefix argument prompts for KIND.  If invoked by a mouse event,
select its window and calculate bounds where it happened.

When INTERACTIVE is nil, return the list of `refactor-action' objects."
  (interactive
   (let ((posn (and (mouse-event-p last-nonmenu-event)
                    (event-start last-nonmenu-event))))
     (when posn (select-window (posn-window posn)))
     `(,@(save-excursion
           ;; Non-text-area clicks (`posn-area' non-nil) already assume
           ;; correct point, but text area clicks are likely on Flymake
           ;; some diagnostic and need moving before `refactor--bounds'.
           (when (and posn (null (posn-area posn)))
             (goto-char (posn-point posn)))
           (refactor--bounds))
       ,(and current-prefix-arg
             (intern
              (completing-read
               "Kind of refactoring: "
               (mapcar (lambda (kind)
                         (cons (symbol-name kind) kind))
                       refactor-kinds)
               nil t)))
       t)))
  (let* ((shortcut
          ;; Check if we can do an optimization/shortcut.
          ;; `refactor-suggestion' may just recently have computed
          ;; exactly what we wanted, stashing it in
          ;; `refactor--suggestion-overlay'.  Not when asked for a
          ;; specific RKIND, though: those suggestions were requested
          ;; with an "automatic" trigger kind, and backends may hold
          ;; back some actions, like `fix-all' ones, in that case.
          (and interactive
               (null rkind)
               (not (listp last-nonmenu-event)) ; not run by mouse
               (overlay-buffer refactor--suggestion-overlay)
               (= beg (overlay-start refactor--suggestion-overlay))
               (= end (overlay-end refactor--suggestion-overlay))))
         (actions
          (if shortcut
              ;; ...in which case we can skip re-contacting the backend.
              (overlay-get refactor--suggestion-overlay 'refactor--actions)
            (refactor--collect beg end :rkind rkind))))
    (cond ((not interactive) actions)
          ((null actions)
           (user-error (if rkind "No \"%s\" refactorings here"
                         "No refactorings here")
                       rkind))
          (t (refactor--read-execute-action actions)))))

(defmacro refactor--define-kind-command (name kind)
  "Define NAME to execute KIND refactorings between BEG and END."
  `(defun ,name (beg &optional end)
     ,(format "Execute `%s' refactorings between BEG and END." kind)
     (interactive (refactor--bounds))
     (refactor beg end ',kind t)))

(refactor--define-kind-command refactor-organize-imports organize-imports)
(refactor--define-kind-command refactor-extract extract)
(refactor--define-kind-command refactor-inline inline)
(refactor--define-kind-command refactor-rewrite rewrite)
(refactor--define-kind-command refactor-quickfix quickfix)
(refactor--define-kind-command refactor-fix-all fix-all)

;;;###autoload
(defun refactor-rename (newname backend)
  "Rename the symbol at point to NEWNAME via BACKEND.
Interactively, BACKEND is chosen to be the first backend in
`refactor-backend-functions' that claims the symbol at point."
  (interactive
   (pcase-let ((`(,backend . ,bounds)
                (cl-loop for b in (refactor-find-backends)
                         when (refactor-backend-rename-bounds b)
                         return (cons b it))))
     (unless backend
       (user-error "No backend can rename the symbol at point"))
     (let ((sym-name (buffer-substring-no-properties (car bounds) (cdr bounds))))
       (list
        (string-trim (read-from-minibuffer (format "Rename `%s' to: " sym-name)
                                           nil nil nil nil sym-name))
        backend))))
  (refactor-apply-changeset (refactor-backend-rename backend newname)
                            :origin this-command))


;;;; Suggestions

;; Like in eglot, the state is a single overlay marking the bounds of
;; the actions available at point; `refactor-suggestion' is an ElDoc
;; member computing them, possibly asynchronously.
;; `refactor-suggestion-mode' adds it to `eldoc-documentation-functions'
;; and cleans up the overlay when disabled.  The mode's lighter is
;; `refactor-mode-line-indicator', which shows `refactor-indicator'
;; while the overlay exists.  Eglot, for one, does it when entering
;; `eglot--managed-mode'.

(defcustom refactor-indications '(eldoc-hint left-fringe margin)
  "How refactor backends indicate there are actions available at point.
Value is a list of symbols, more than one can be specified:

- `eldoc-hint': ElDoc is used to hint about at-point actions;
- `left-fringe': A special indicator appears on the left fringe;
- `margin': A special indicator appears in the margin;
- `nearby': A special indicator appears near point.

If the list is empty, no hinting happens.

Note additionally:

- Some values are incompatible; if one or more of `nearby',
  `left-fringe' and `margin' are specified, earlier values take
  precedence.
- The indicators for many of these are customizable via
  `refactor-indicator' (which see), except for `left-fringe'."
  :type '(set
          :tag "Tick the ones you're interested in"
          (const :tag "ElDoc textual hint" eldoc-hint)
          (const :tag "Right besides point" nearby)
          (const :tag "In left fringe" left-fringe)
          (const :tag "In margin" margin)))

(defface refactor-indicator-face
  '((t (:inherit warning :weight bold)))
  "Face used for action suggestions.")

(defcustom refactor-indicator
  (cl-loop for c in '(?↯ ?⭍ ?✓ ?α ??)
           when (char-displayable-p c)
           return (make-string 1 c))
  "Indicator string for action suggestions."
  :type (let ((basic-choices
               (cl-loop for c in '(?↯ ?⭍ ?✓ ?α ??)
                        when (char-displayable-p c)
                        collect `(const :tag ,(format "Use `%c'" c)
                                        ,(make-string 1 c)))))
          `(choice ,@basic-choices
                   (string :tag "Specify your own"))))

(defvar-local refactor--suggestion-overlay (make-overlay 0 0)
  "Overlay for `refactor-suggestion'.")

;; Emacs < 27 lacks `define-fringe-bitmap' without a window system.
(when (fboundp 'define-fringe-bitmap)
  (define-fringe-bitmap 'refactor--fringe-action
    [#b00000111
     #b00001110
     #b00011100
     #b00111000
     #b01111111
     #b00001110
     #b01011100
     #b01111000
     #b01110000
     #b01111000]
    nil nil 'center))

(defmacro refactor--when-buffer-window (buf &rest body)
  "Check BUF showing somewhere, then do BODY in it."
  (declare (indent 1) (debug t))
  (let ((b (gensym)))
    `(let ((,b ,buf))
       (when (get-buffer-window ,b)
         (with-current-buffer ,b ,@body)))))

(defvar refactor-mouse-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] #'refactor)
    (define-key map [left-margin mouse-1] #'refactor)
    map)
  "Keymap active on action indicators and clickable diagnostics.")

(defun refactor-suggestion (cb &rest _)
  "A member of `eldoc-documentation-functions', for suggesting actions."
  (when (and refactor-indications (refactor-find-backends))
    (let ((buf (current-buffer))
          (win (selected-window))
          (bounds (refactor--bounds))
          (use-text-p (memq 'eldoc-hint refactor-indications))
          indicator blurb)
      (refactor--collect
       (car bounds) (cadr bounds)
       :trigger-kind 2
       :callback
       (lambda (actions)
         (refactor--when-buffer-window
          buf
          (when (overlay-buffer refactor--suggestion-overlay)
            (delete-overlay refactor--suggestion-overlay))
          (when (cl-plusp (length actions))
            (setq blurb
                  (substitute-command-keys
                   (format "\\[refactor]: %s"
                           (oref (car actions) title))))
            (when (cdr actions)
              (setq blurb (concat blurb (format " (and %s more actions)"
                                                (length (cdr actions))))))
            (setq indicator
                  (propertize refactor-indicator
                              'face 'refactor-indicator-face
                              'help-echo "mouse-2: execute actions at point"
                              'mouse-face 'highlight
                              'keymap refactor-mouse-map))
            (save-excursion
              (goto-char (car bounds))
              (let ((ov (make-overlay (car bounds) (cadr bounds))))
                (overlay-put ov 'refactor--actions actions)
                ;; Elsewhere, the indicator could be far from point.
                (overlay-put ov 'window win)
                (overlay-put
                 ov 'before-string
                 (cond
                  ((memq 'nearby refactor-indications)
                   indicator)
                  ((and (memq 'left-fringe refactor-indications)
                        (< 0 (nth 0 (window-fringes))))
                   (propertize
                    "⚡" 'display `(left-fringe
                                    refactor--fringe-action
                                    refactor-indicator-face)))
                  ((memq 'margin refactor-indications)
                   (propertize
                    "⚡" 'display `((margin left-margin)
                                    ,(propertize
                                      indicator 'help-echo
                                      "mouse-1: execute actions at point"))))))
                (setq refactor--suggestion-overlay ov)))))
         ;; Answer the ElDoc strategy even if BUF isn't showing, else it
         ;; could wait for us forever (like in non-interactive tests).
         (when use-text-p (funcall cb blurb))))
      (and use-text-p t))))

;;;###autoload
(define-minor-mode refactor-suggestion-mode
  "Minor mode for showing refactoring suggestions at point.
Add `refactor-suggestion' to `eldoc-documentation-functions' and
show `refactor-mode-line-indicator' while actions are available."
  :init-value nil
  :lighter refactor-mode-line-indicator
  (if refactor-suggestion-mode
      (add-hook 'eldoc-documentation-functions #'refactor-suggestion nil t)
    (remove-hook 'eldoc-documentation-functions #'refactor-suggestion t)
    (when (overlay-buffer refactor--suggestion-overlay)
      (delete-overlay refactor--suggestion-overlay))))

(defvar refactor--mode-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] #'refactor)
    map)
  "Keymap active on `refactor-mode-line-indicator'.")

(defconst refactor-mode-line-indicator
  '(:eval
    (when (and (overlay-buffer refactor--suggestion-overlay)
               (eq (overlay-get refactor--suggestion-overlay 'window)
                   (selected-window)))
      `(" " (:propertize
             ,refactor-indicator
             face refactor-indicator-face
             keymap ,refactor--mode-line-map
             help-echo "mouse-1: execute actions at point"
             mouse-face mode-line-highlight))))
  "Mode line construct for at-point refactoring actions.")
(put 'refactor-mode-line-indicator 'risky-local-variable t)


;;;; Flymake integration

;; Flymake backends may use these categories if they want their
;; diagnostics to be clickable.  `refactor-mouse-map' contains
;; a mouse binding for `refactor'.
(cl-loop
 for i from 1
 for type in '(refactor-note refactor-warning refactor-error)
 for ftype in '(flymake-note flymake-warning flymake-error)
 do
 (put type 'flymake-category ftype)
 (put type 'flymake-overlay-control
      `((mouse-face . highlight)
        (priority . ,(+ 50 i))
        (keymap . ,refactor-mouse-map))))


;;;; Applying changes

(cl-defun refactor-apply-text-edits (edits &key silent)
  "Apply EDITS to current buffer as single undoable step.

EDITS is a list of (BEG END NEWTEXT), or a function of no
arguments returning such a list, which is called with this buffer
current.  BEG and END are positions valid in the widened buffer.
Edits must not overlap, but need not be sorted.

Unless SILENT, report progress in the echo area."
  (when (functionp edits) (setq edits (funcall edits)))
  (unless edits (cl-return-from refactor-apply-text-edits))
  (atomic-change-group
    (let* ((change-group (prepare-change-group))
           (howmany (length edits))
           (reporter (unless silent
                       (make-progress-reporter
                        (format "Applying %s edits to `%s'..."
                                howmany (current-buffer))
                        0 howmany)))
           (done 0))
      ;; Convert every edit to markers up front, so that applying one
      ;; cannot invalidate the positions of the others.
      (mapc (pcase-lambda (`(,beg ,end ,newtext))
              (if (> emacs-major-version 30)
                  (replace-region-contents beg end newtext)
                ;; Before Emacs 31, `replace-region-contents' wanted a
                ;; function, so go through a temporary buffer.
                (let ((source (current-buffer)))
                  (with-temp-buffer
                    (insert newtext)
                    (let ((temp (current-buffer)))
                      (with-current-buffer source
                        (save-excursion
                          (save-restriction
                            (narrow-to-region beg end)
                            (with-no-warnings
                              (replace-buffer-contents temp)))))))))
              (when reporter
                (progress-reporter-update reporter (cl-incf done))))
            (mapcar (pcase-lambda (`(,beg ,end ,newtext))
                      (list (copy-marker beg) (copy-marker end) newtext))
                    (reverse edits)))
      (undo-amalgamate-change-group change-group)
      (when reporter (progress-reporter-done reporter)))))

(defcustom refactor-confirmation '((t . maybe-summary))
  "Control if changes proposed by a backend should be confirmed with user.

If this variable's value is the symbol `diff', a diff buffer pops
up, allowing the user to apply each change individually.  If the
symbol `summary' or any other non-nil value, the user is prompted
in the minibuffer with a short summary of changes.  The symbols
`maybe-diff' and `maybe-summary' mean that the confirmation is
offered to the user only if the changes target files not visited
in buffers.  Finally, a nil value means all changes are applied
directly without any confirmation.

This variable's value can also be an alist ((KEY . ACTION) ...)
where KEY is either a symbol designating the invoked Emacs command
\(such as `refactor-rename', `refactor', `refactor-quickfix',
etc.), or a list of file operation kinds (`create', `rename',
`delete') contained in the changeset.  ACTION is one of the
symbols described above.  The value t for KEY is accepted and its
ACTION is the default value for commands or file operation kinds
not in the alist."
  :type (let ((basic-choices
               '((const :tag "Use diff" diff)
                 (const :tag "Summarize and prompt" summary)
                 (const :tag "Maybe use diff" maybe-diff)
                 (const :tag "Maybe summarize and prompt" maybe-summary)
                 (const :tag "Don't confirm" nil))))
          `(choice ,@basic-choices
                   (alist :tag "Per-command or per-kind alist"
                          :key-type (choice (function :tag "Command")
                                            (repeat :tag "File operation kinds" symbol)
                                            (const :tag "Default" t))
                          :value-type (choice . ,basic-choices))))
  :version "32.1")

(defconst refactor--changes-buffer-name "*refactor changes*"
  "Name of the buffer used to preview proposed changes as a diff.")

(defun refactor--confirmation (origin operations)
  "Return the confirmation ACTION to use for OPERATIONS.
ORIGIN is a symbol designating the command that asked for them.  Reads
`refactor-confirmation' and returns a symbol like `diff' or `summary',
or nil to apply without confirming."
  (let ((v (lambda (candidate) (cdr (assq candidate refactor-confirmation))))
        kinds)
    (or (and (symbolp refactor-confirmation) refactor-confirmation)
        ;; Check for command-based entry
        (funcall v origin)
        ;; Check for operation-kind-based entry
        (and (setq kinds (mapcar #'refactor-operation-kind operations))
             (cl-some (lambda (entry)
                        (and (listp (car entry))
                             (cl-some (lambda (kind)
                                        (memq kind (car entry)))
                                      kinds)
                             (cdr entry)))
                      refactor-confirmation))
        ;; Default entry
        (funcall v t))))

(cl-defgeneric refactor--apply-operation (operation)
  "Carry OPERATION out."
  (:method ((op refactor-file-edit))
   (with-slots (file edits) op
     (with-current-buffer (find-file-noselect file)
       (refactor-apply-text-edits edits))))
  (:method ((op refactor-file-creation))
   (with-slots (file contents if-exists) op
     (let ((exists (file-exists-p file)))
       (when (and exists (eq if-exists 'error))
         (error "File %s already exists" file))
       (when (or (not exists) (eq if-exists 'overwrite))
         (let ((dir (file-name-directory file)))
           (unless (file-directory-p dir) (make-directory dir t)))
         (write-region (or contents "") nil file nil 'nomessage)))))
  (:method ((op refactor-file-renaming))
   (with-slots (from to if-exists) op
     (let ((new-exists (file-exists-p to)))
       (when (and new-exists (eq if-exists 'error))
         (error "File %s already exists" to))
       (unless (and new-exists (eq if-exists 'skip))
         (let ((dir (file-name-directory to)))
           (unless (file-directory-p dir) (make-directory dir t)))
         ;; If the old file is visited, rename the buffer too
         (when-let* ((buf (find-buffer-visiting from)))
           (with-current-buffer buf (set-visited-file-name to t t)))
         (rename-file from to (eq if-exists 'overwrite))))))
  (:method ((op refactor-file-deletion))
   (with-slots (file recursive if-missing) op
     (let ((exists (file-exists-p file)))
       (when (and (not exists) (eq if-missing 'error))
         (error "File %s does not exist" file))
       (when exists
         ;; Kill the buffer if the file is visited
         (when-let* ((buf (find-buffer-visiting file))) (kill-buffer buf))
         (delete-file file recursive))))))

(defun refactor--apply-and-report (operation)
  "Carry OPERATION out and say so in the echo area."
  (refactor--apply-operation operation)
  (unless (refactor-file-edit-p operation)
    (message "%s" (replace-regexp-in-string
                   "^\\([^ ]+\\) " "\\1d "
                   (refactor-describe operation)))))

(defun refactor--file-text (file)
  "Insert the full text of FILE into the current buffer.
If a buffer is visiting FILE, take the text from there instead,
disregarding any narrowing, so that positions in the result agree
with positions in the widened buffer."
  (if-let* ((buf (find-buffer-visiting file)))
      (insert (with-current-buffer buf
                (save-restriction
                  (widen)
                  (buffer-substring-no-properties (point-min) (point-max)))))
    (insert-file-contents file)))

(defun refactor--propose-changes-as-diff (operations)
  "Return a `diff-mode' buffer with the changes OPERATIONS would make.
OPERATIONS are all `refactor-file-edit's.  The buffer is ready to
apply with \\<diff-mode-map>\\[diff-apply-buffer]."
  (with-current-buffer (get-buffer-create refactor--changes-buffer-name)
    (buffer-disable-undo (current-buffer))
    (let ((inhibit-read-only t)
          (target (current-buffer)))
      (diff-mode)
      (erase-buffer)
      (dolist (op operations)
        (with-temp-buffer
          (let* ((diff (current-buffer))
                 (path (oref op file))
                 (existing-buf (find-buffer-visiting path))
                 (existing-buf-label (prin1-to-string existing-buf)))
            (with-temp-buffer
              (refactor--file-text path)
              (refactor-apply-text-edits (oref op edits)
                                         :silent t)
              (diff-no-select (or existing-buf path) (current-buffer) nil t diff)
              (when existing-buf
                ;; Here we have to pretend the label of the unsaved
                ;; buffer is the actual file, so that `diff-apply-buffer'
                ;; works.  If there's a better way, it probably involves
                ;; changes to `diff.el'.
                (with-current-buffer diff
                  (goto-char (point-min))
                  (while (search-forward existing-buf-label nil t)
                    (replace-match (buffer-file-name existing-buf))))))
            (with-current-buffer target
              (insert-buffer-substring diff))))))
    (setq-local buffer-read-only t)
    (buffer-enable-undo (current-buffer))
    (goto-char (point-min))
    (font-lock-ensure)
    (current-buffer)))

(cl-defun refactor-apply-changeset (operations &key origin)
  "Apply OPERATIONS, a changeset, after confirming with the user.

OPERATIONS is a list of `refactor-operation' objects, applied in
order.  ORIGIN is a symbol designating the command that produced
them; `refactor-confirmation' may key on it.

Return a list (APPLIED REASON) saying whether the changes were
applied and, if not, why not; REASON is nil when they were."
  (cl-labels
      ((user-accepts-p ()
         (y-or-n-p
          (format "These changes will be made:\n%s\nProceed? "
                  (mapconcat (lambda (op)
                               (concat "  " (refactor-describe op)))
                             operations "\n"))))
       (apply-all ()
         (mapc #'refactor--apply-and-report operations)
         (eldoc)
         (message "Refactoring applied")
         `(t nil)))
    (let* ((decision (refactor--confirmation origin operations))
           (all-edits (cl-every #'refactor-file-edit-p operations))
           (peaceful (and all-edits
                          (cl-every (lambda (op)
                                      (find-buffer-visiting (oref op file)))
                                    operations))))
      (cond
       ((and (memq decision '(maybe-diff maybe-summary)) peaceful)
        (apply-all))
       ((memq decision '(diff maybe-diff))
        (cond (all-edits
               (pop-to-buffer (refactor--propose-changes-as-diff operations))
               `(nil "decision to apply manually"))
              (t
               ;; `map-y-or-n-p' heroics.  Iterate over operations with
               ;; individual prompts, showing diffs for edits.
               (let* ((wconf (current-window-configuration))
                      (remaining operations)
                      (applied 0)
                      (total (length operations)))
                 (unwind-protect
                     (progn
                       (map-y-or-n-p
                        (lambda (op)
                          (when (refactor-file-edit-p op)
                            (display-buffer
                             (refactor--propose-changes-as-diff (list op))))
                          (format "%s? " (refactor-describe op)))
                        (lambda (op)
                          (set-window-configuration wconf)
                          (refactor--apply-and-report op)
                          (cl-incf applied))
                        (lambda ()
                          ;; Skip edits to files that don't exist (e.g.
                          ;; the user skipped the create operation).
                          (cl-loop for op = (pop remaining) while op
                                   when (or (not (refactor-file-edit-p op))
                                            (file-exists-p (oref op file)))
                                   return op))
                        '("change" "changes" "apply"))
                       (if (= applied total)
                           (progn
                             (eldoc)
                             (message "Refactoring applied")
                             `(t nil))
                         `(nil "decision to abort")))
                   (set-window-configuration wconf))))))
       ((memq decision '(t summary maybe-summary))
        (if (user-accepts-p) (apply-all) `(nil "decision to decline")))
       ((apply-all))))))

(provide 'refactor)
;;; refactor.el ends here
