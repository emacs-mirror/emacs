;;; xref.el --- Cross-referencing commands              -*-lexical-binding:t-*-

;; Copyright (C) 2014-2022 Free Software Foundation, Inc.
;; Version: 1.3.0
;; Package-Requires: ((emacs "26.1"))

;; This is a GNU ELPA :core package.  Avoid functionality that is not
;; compatible with the version of Emacs recorded above.

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

;; This file provides a somewhat generic infrastructure for cross
;; referencing commands, in particular "find-definition".
;;
;; Some part of the functionality must be implemented in a language
;; dependent way and that's done by defining an xref backend.
;;
;; That consists of a constructor function, which should return a
;; backend value, and a set of implementations for the generic
;; functions:
;;
;; `xref-backend-identifier-at-point',
;; `xref-backend-identifier-completion-table',
;; `xref-backend-definitions', `xref-backend-references',
;; `xref-backend-apropos', which see.
;;
;; A major mode would normally use `add-hook' to add the backend
;; constructor to `xref-backend-functions'.
;;
;; The last three methods operate with "xref" and "location" values.
;;
;; One would usually call `xref-make' and `xref-make-file-location',
;; `xref-make-buffer-location' or `xref-make-bogus-location' to create
;; them.  More generally, a location must be an instance of a type for
;; which methods `xref-location-group' and `xref-location-marker' are
;; implemented.
;;
;; There's a special kind of xrefs we call "match xrefs", which
;; correspond to search results.  For these values,
;; `xref-match-length' must be defined, and `xref-location-marker'
;; must return the beginning of the match.
;;
;; Each identifier must be represented as a string.  Implementers can
;; use string properties to store additional information about the
;; identifier, but they should keep in mind that values returned from
;; `xref-backend-identifier-completion-table' should still be
;; distinct, because the user can't see the properties when making the
;; choice.
;;
;; Older versions of Xref used EIEIO for implementation of the
;; built-in types, and included a class called `xref-location' which
;; was supposed to be inherited from.  Neither is true anymore.
;;
;; See the etags and elisp-mode implementations for full examples.

;;; Code:

(require 'cl-lib)
(require 'ring)
(require 'project)

(eval-and-compile
  (when (version< emacs-version "28")
    ;; etags.el in Emacs 26 and 27 uses EIEIO, and its location type
    ;; inherits from `xref-location'.
    (require 'eieio)

    ;; Suppressing byte-compilation warnings (in Emacs 28+) about
    ;; `defclass' not being defined, which happens because the
    ;; `require' statement above is not evaluated either.
    ;; FIXME: Use `with-suppressed-warnings' when we stop supporting Emacs 26.
    (with-no-warnings
      (defclass xref-location () ()
        :documentation "(Obsolete) location represents a position in a file or buffer."))))

(defgroup xref nil "Cross-referencing commands."
  :version "25.1"
  :group 'tools)


;;; Locations

(cl-defgeneric xref-location-marker (location)
  "Return the marker for LOCATION.")

(cl-defgeneric xref-location-group (location)
  "Return a string used to group a set of locations.
This is typically a file name, but can also be a package name, or
some other label.

When it is a file name, it should be the \"expanded\" version.")

(cl-defgeneric xref-location-line (_location)
  "Return the line number corresponding to the location."
  nil)

(cl-defgeneric xref-match-length (_item)
  "Return the length of the match."
  nil)

;;;; Commonly needed location types are defined here:

(defcustom xref-file-name-display 'project-relative
  "Style of file name display in *xref* buffers.

If the value is the symbol `abs', show the file names in their
full absolute form.

If `nondirectory', show only the nondirectory (a.k.a. \"base name\")
part of the file name.

If `project-relative', the default, show only the file name
relative to the current project root.  If there is no current
project, or if the file resides outside of its root, show that
particular file name in its full absolute form."
  :type '(choice (const :tag "absolute file name" abs)
                 (const :tag "nondirectory file name" nondirectory)
                 (const :tag "relative to project root" project-relative))
  :version "27.1")

;; FIXME: might be useful to have an optional "hint" i.e. a string to
;; search for in case the line number is slightly out of date.
(cl-defstruct (xref-file-location
               (:constructor xref-make-file-location (file line column)))
  "A file location is a file/line/column triple.
Line numbers start from 1 and columns from 0."
  file line column)

(cl-defmethod xref-location-group ((l xref-file-location))
  (xref-file-location-file l))

(cl-defmethod xref-location-line ((l xref-file-location))
  (xref-file-location-line l))

(cl-defmethod xref-location-marker ((l xref-file-location))
  (pcase-let (((cl-struct xref-file-location file line column) l))
    (with-current-buffer
        (or (get-file-buffer file)
            (let ((find-file-suppress-same-file-warnings t))
              (find-file-noselect file)))
      (save-restriction
        (widen)
        (save-excursion
          (goto-char (point-min))
          (ignore-errors
            ;; xref location may be out of date; it may be past the
            ;; end of the current file, or the file may have been
            ;; deleted. Return a reasonable location; the user will
            ;; figure it out.
            (beginning-of-line line)
            (forward-char column))
          (point-marker))))))

(cl-defstruct (xref-buffer-location
               (:constructor xref-make-buffer-location (buffer position)))
  buffer position)

(cl-defmethod xref-location-marker ((l xref-buffer-location))
  (pcase-let (((cl-struct xref-buffer-location buffer position) l))
    (let ((m (make-marker)))
      (move-marker m position buffer))))

(cl-defmethod xref-location-group ((l xref-buffer-location))
  (pcase-let (((cl-struct xref-buffer-location buffer) l))
    (or (buffer-file-name buffer)
        (format "(buffer %s)" (buffer-name buffer)))))

(cl-defstruct (xref-bogus-location
               (:constructor xref-make-bogus-location (message)))
  "Bogus locations are sometimes useful to indicate errors,
e.g. when we know that a function exists but the actual location
is not known."
  message)

(cl-defmethod xref-location-marker ((l xref-bogus-location))
  (user-error "%s" (xref-bogus-location-message l)))

(cl-defmethod xref-location-group ((_ xref-bogus-location)) "(No location)")


;;; Cross-reference

(cl-defstruct (xref-item
               (:constructor xref-make (summary location))
               (:noinline t))
  "An xref item describes a reference to a location somewhere."
  (summary nil :documentation "String which describes the location.

When `xref-location-line' returns non-nil (a number), the summary
is implied to be the contents of a file or buffer line containing
the location.  When multiple locations in a row report the same
line, in the same group (corresponding to the case of multiple
locations on one line), the summaries are concatenated in the
Xref output buffer.  Consequently, any code that creates xref
values should take care to slice the summary values when several
locations point to the same line.

This behavior is new in Emacs 28.")
  location)

(cl-defstruct (xref-match-item
               (:include xref-item)
               (:constructor xref-make-match (summary location length))
               (:noinline t))
  "A match xref item describes a search result."
  length)

(cl-defgeneric xref-match-length ((item xref-match-item))
  "Return the length of the match."
  (xref-match-item-length item))


;;; API

(defvar xref-backend-functions nil
  "Special hook to find the xref backend for the current context.
Each function on this hook is called in turn with no arguments,
and should return either nil to mean that it is not applicable,
or an xref backend, which is a value to be used to dispatch the
generic functions.")

;; We make the etags backend the default for now, until something
;; better comes along.  Use APPEND so that any `add-hook' calls made
;; before this package is loaded put new items before this one.
(add-hook 'xref-backend-functions #'etags--xref-backend t)

;;;###autoload
(defun xref-find-backend ()
  (run-hook-with-args-until-success 'xref-backend-functions))

(cl-defgeneric xref-backend-definitions (backend identifier)
  "Find definitions of IDENTIFIER.

The result must be a list of xref objects.  If IDENTIFIER
contains sufficient information to determine a unique definition,
return only that definition.  If there are multiple possible
definitions, return all of them.  If no definitions can be found,
return nil.

IDENTIFIER can be any string returned by
`xref-backend-identifier-at-point', or from the table returned by
`xref-backend-identifier-completion-table'.

To create an xref object, call `xref-make'.")

(cl-defgeneric xref-backend-references (_backend identifier)
  "Find references of IDENTIFIER.
The result must be a list of xref objects.  If no references can
be found, return nil.

The default implementation uses `semantic-symref-tool-alist' to
find a search tool; by default, this uses \"find | grep\" in the
current project's main and external roots."
  (mapcan
   (lambda (dir)
     (message "Searching %s..." dir)
     (redisplay)
     (prog1
         (xref-references-in-directory identifier dir)
       (message "Searching %s... done" dir)))
   (let ((pr (project-current t)))
     (cons
      (xref--project-root pr)
      (project-external-roots pr)))))

(cl-defgeneric xref-backend-apropos (backend pattern)
  "Find all symbols that match PATTERN string.
The second argument has the same meaning as in `apropos'.

If BACKEND is implemented in Lisp, it can use
`xref-apropos-regexp' to convert the pattern to regexp.")

(cl-defgeneric xref-backend-identifier-at-point (_backend)
  "Return the relevant identifier at point.

The return value must be a string, or nil meaning no identifier
at point found.

If it's hard to determine the identifier precisely (e.g., because
it's a method call on unknown type), the implementation can
return a simple string (such as symbol at point) marked with a
special text property which e.g. `xref-backend-definitions' would
recognize and then delegate the work to an external process."
  (let ((thing (thing-at-point 'symbol)))
    (and thing (substring-no-properties thing))))

(cl-defgeneric xref-backend-identifier-completion-table (backend)
  "Return the completion table for identifiers.")

(cl-defgeneric xref-backend-identifier-completion-ignore-case (_backend)
  "Return t if case is not significant in identifier completion."
  completion-ignore-case)


;;; misc utilities
(defun xref--alistify (list key)
  "Partition the elements of LIST into an alist.
KEY extracts the key from an element."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (e list)
      (let* ((k (funcall key e))
             (probe (gethash k table)))
        (if probe
            (puthash k (cons e probe) table)
          (puthash k (list e) table))))
    ;; Put them back in order.
    (cl-loop for key being hash-keys of table using (hash-values value)
             collect (cons key (nreverse value)))))

(defun xref--insert-propertized (props &rest strings)
  "Insert STRINGS with text properties PROPS."
  (let ((start (point)))
    (apply #'insert strings)
    (add-text-properties start (point) props)))

(defun xref--search-property (property &optional backward)
    "Search the next text range where text property PROPERTY is non-nil.
Return the value of PROPERTY.  If BACKWARD is non-nil, search
backward."
  (let ((next (if backward
                  #'previous-single-char-property-change
                #'next-single-char-property-change))
        (start (point))
        (value nil))
    (while (progn
             (goto-char (funcall next (point) property))
             (not (or (setq value (get-text-property (point) property))
                      (eobp)
                      (bobp)))))
    (cond (value)
          (t (goto-char start) nil))))


;;; Marker stack  (M-. pushes, M-, pops)

(defcustom xref-marker-ring-length 16
  "Length of the xref marker ring.
If this variable is not set through Customize, you must call
`xref-set-marker-ring-length' for changes to take effect."
  :type 'integer
  :initialize #'custom-initialize-default
  :set #'xref-set-marker-ring-length)

(defcustom xref-prompt-for-identifier '(not xref-find-definitions
                                            xref-find-definitions-other-window
                                            xref-find-definitions-other-frame)
  "If non-nil, prompt for the identifier to find.

When t, always prompt for the identifier name.

When nil, prompt only when there's no value at point we can use,
or when the command has been called with the prefix argument.

Otherwise, it's a list of xref commands which will always prompt,
with the identifier at point, if any, used as the default.
If the list starts with `not', the meaning of the rest of the
elements is negated: these commands will NOT prompt."
  :type '(choice (const :tag "Always prompt for identifier" t)
                 (const :tag "Prompt if no identifier at point" nil)
                 (set :menu-tag "Prompt according to command"
                      :tag "Prompt according to command"
		      :value (not)
		      (const :tag "Except for commands listed below" not)
		      (repeat :inline t (symbol :tag "command")))))

(defcustom xref-after-jump-hook '(recenter
                                  xref-pulse-momentarily)
  "Functions called after jumping to an xref."
  :type 'hook)

(defcustom xref-after-return-hook '(xref-pulse-momentarily)
  "Functions called after returning to a pre-jump location."
  :type 'hook)

(defcustom xref-after-update-hook nil
  "Functions called after the xref buffer is updated."
  :type 'hook
  :version "28.1"
  :package-version '(xref . "1.0.4"))

(defcustom xref-auto-jump-to-first-definition nil
  "If t, `xref-find-definitions' always jumps to the first result.
`show' means to show the first result's location, but keep the
focus on the Xref buffer's window.
`move' means to only move point to the first result.
This variable also affects the variants of `xref-find-definitions',
such as `xref-find-definitions-other-window'."
  :type '(choice (const :tag "Jump" t)
                 (const :tag "Show" show)
                 (const :tag "Move point only" move)
                 (const :tag "No auto-jump" nil))
  :version "28.1"
  :package-version '(xref . "1.2.0"))

(defcustom xref-auto-jump-to-first-xref nil
  "If t, `xref-find-references' always jumps to the first result.
`show' means to show the first result's location, but keep the
focus on the Xref buffer's window.
`move' means to only move point to the first result.
This variable also affects commands similar to `xref-find-references',
such as `xref-find-references-at-mouse', `xref-find-apropos',
and `project-find-regexp'.

Please be careful when changing the value if you are using Emacs 27
or earlier: it can break `dired-do-find-regexp-and-replace'."
  :type '(choice (const :tag "Jump" t)
                 (const :tag "Show" show)
                 (const :tag "Move point only" move)
                 (const :tag "No auto-jump" nil))
  :version "28.1"
  :package-version '(xref . "1.2.0"))

(defvar xref--marker-ring (make-ring xref-marker-ring-length)
  "Ring of markers to implement the marker stack.")

(defun xref-set-marker-ring-length (var val)
  "Set `xref-marker-ring-length'.
VAR is the symbol `xref-marker-ring-length' and VAL is the new
value."
  (set-default var val)
  (if (ring-p xref--marker-ring)
      (ring-resize xref--marker-ring val)))

(defun xref-push-marker-stack (&optional m)
  "Add point M (defaults to `point-marker') to the marker stack."
  (ring-insert xref--marker-ring (or m (point-marker))))

;;;###autoload
(defun xref-pop-marker-stack ()
  "Pop back to where \\[xref-find-definitions] was last invoked."
  (interactive)
  (let ((ring xref--marker-ring))
    (when (ring-empty-p ring)
      (user-error "Marker stack is empty"))
    (let ((marker (ring-remove ring 0)))
      (switch-to-buffer (or (marker-buffer marker)
                            (user-error "The marked buffer has been deleted")))
      (goto-char (marker-position marker))
      (set-marker marker nil nil)
      (run-hooks 'xref-after-return-hook))))

(defvar xref--current-item nil)

(defun xref-pulse-momentarily ()
  (pcase-let ((`(,beg . ,end)
               (save-excursion
                 (or
                  (let ((length (xref-match-length xref--current-item)))
                    (and length (cons (point) (+ (point) length))))
                  (back-to-indentation)
                  (if (eolp)
                      (cons (line-beginning-position) (1+ (point)))
                    (cons (point) (line-end-position)))))))
    (pulse-momentary-highlight-region beg end 'next-error)))

;; etags.el needs this
(defun xref-clear-marker-stack ()
  "Discard all markers from the marker stack."
  (let ((ring xref--marker-ring))
    (while (not (ring-empty-p ring))
      (let ((marker (ring-remove ring)))
        (set-marker marker nil nil)))))

;;;###autoload
(defun xref-marker-stack-empty-p ()
  "Return t if the marker stack is empty; nil otherwise."
  (ring-empty-p xref--marker-ring))



(defun xref--goto-char (pos)
  (cond
   ((and (<= (point-min) pos) (<= pos (point-max))))
   (widen-automatically (widen))
   (t (user-error "Position is outside accessible part of buffer")))
  (goto-char pos))

(defun xref--goto-location (location)
  "Set buffer and point according to `xref-location' LOCATION."
  (let ((marker (xref-location-marker location)))
    (set-buffer (marker-buffer marker))
    (xref--goto-char marker)))

(defun xref-pop-to-location (item &optional action)
  "Go to the location of ITEM and display the buffer.
ACTION controls how the buffer is displayed:
  nil      -- `switch-to-buffer'
  `window' -- `pop-to-buffer' (other window)
  `frame'  -- `pop-to-buffer' (other frame)
If SELECT is non-nil, select the target window."
  (let* ((marker (save-excursion
                   (xref-location-marker (xref-item-location item))))
         (buf (marker-buffer marker)))
    (cl-ecase action
      ((nil)  (switch-to-buffer buf))
      (window (pop-to-buffer buf t))
      (frame  (let ((pop-up-frames t)) (pop-to-buffer buf t))))
    (xref--goto-char marker))
  (let ((xref--current-item item))
    (run-hooks 'xref-after-jump-hook)))


;;; XREF buffer (part of the UI)

;; The xref buffer is used to display a set of xrefs.
(defconst xref-buffer-name "*xref*"
  "The name of the buffer to show xrefs.")

(defface xref-file-header '((t :inherit compilation-info))
  "Face used to highlight file header in the xref buffer."
  :version "27.1")

(defface xref-line-number '((t :inherit compilation-line-number))
  "Face for displaying line numbers in the xref buffer."
  :version "27.1")

(defface xref-match '((t :inherit match))
  "Face used to highlight matches in the xref buffer."
  :version "27.1")

(defmacro xref--with-dedicated-window (&rest body)
  `(let* ((xref-w (get-buffer-window xref-buffer-name))
          (xref-w-dedicated (window-dedicated-p xref-w)))
     (unwind-protect
         (progn
           (when xref-w
             (set-window-dedicated-p xref-w 'soft))
           ,@body)
       (when xref-w
         (set-window-dedicated-p xref-w xref-w-dedicated)))))

(defvar-local xref--original-window-intent nil
  "Original window-switching intent before xref buffer creation.")

(defvar-local xref--original-window nil
  "The original window this xref buffer was created from.")

(defvar-local xref--fetcher nil
  "The original function to call to fetch the list of xrefs.")

(defun xref--show-pos-in-buf (pos buf)
  "Goto and display position POS of buffer BUF in a window.
Honor `xref--original-window-intent', run `xref-after-jump-hook'
and finally return the window."
  (let* ((pop-up-frames
          (or (eq xref--original-window-intent 'frame)
              pop-up-frames))
         (action
          (cond ((eq xref--original-window-intent 'frame)
                 t)
                ((eq xref--original-window-intent 'window)
                 `((xref--display-buffer-in-other-window)
                   (window . ,xref--original-window)))
                ((and
                  (window-live-p xref--original-window)
                  (or (not (window-dedicated-p xref--original-window))
                      (eq (window-buffer xref--original-window) buf)))
                 `((xref--display-buffer-in-window)
                   (window . ,xref--original-window))))))
    (with-selected-window (display-buffer buf action)
      (xref--goto-char pos)
      (run-hooks 'xref-after-jump-hook)
      (selected-window))))

(defun xref--display-buffer-in-other-window (buffer alist)
  (let ((window (assoc-default 'window alist)))
    (cl-assert window)
    (xref--with-dedicated-window
     (with-selected-window window
       (display-buffer buffer t)))))

(defun xref--display-buffer-in-window (buffer alist)
  (let ((window (assoc-default 'window alist)))
    (cl-assert window)
    (with-selected-window window
      (display-buffer buffer '(display-buffer-same-window)))))

(defun xref--show-location (location &optional select)
  "Help `xref-show-xref' and `xref-goto-xref' do their job.
Go to LOCATION and if SELECT is non-nil select its window.  If
SELECT is `quit', also quit the *xref* window."
  (condition-case err
      (let* ((marker (xref-location-marker location))
             (buf (marker-buffer marker))
             (xref-buffer (current-buffer)))
        (cond (select
               (if (eq select 'quit) (quit-window nil nil))
               (select-window
                (with-current-buffer xref-buffer
                  (xref--show-pos-in-buf marker buf))))
              (t
               (save-selected-window
                 (xref--with-dedicated-window
                  (xref--show-pos-in-buf marker buf))))))
    (user-error (message (error-message-string err)))))

(defun xref--set-arrow ()
  "Set the overlay arrow at the line at point."
  (setq overlay-arrow-position
        (set-marker (or overlay-arrow-position (make-marker))
                    (line-beginning-position))))

(defun xref-show-location-at-point ()
  "Display the source of xref at point in the appropriate window, if any."
  (interactive)
  (let* ((xref (xref--item-at-point))
         (xref--current-item xref))
    (when xref
      (xref--set-arrow)
      (xref--show-location (xref-item-location xref)))))

(defun xref-next-line-no-show ()
  "Move to the next xref but don't display its source."
  (interactive)
  (xref--search-property 'xref-item))

(defun xref-next-line ()
  "Move to the next xref and display its source in the appropriate window."
  (interactive)
  (xref-next-line-no-show)
  (xref-show-location-at-point))

(defun xref-prev-line-no-show ()
  "Move to the previous xref but don't display its source."
  (interactive)
  (xref--search-property 'xref-item t))

(defun xref-prev-line ()
  "Move to the previous xref and display its source in the appropriate window."
  (interactive)
  (xref-prev-line-no-show)
  (xref-show-location-at-point))

(defun xref-next-group ()
  "Move to the first item of the next xref group and display its source."
  (interactive)
  (xref--search-property 'xref-group)
  (xref--search-property 'xref-item)
  (xref-show-location-at-point))

(defun xref-prev-group ()
  "Move to the first item of the previous xref group and display its source."
  (interactive)
  ;; Search for the xref group of the current item, provided that the
  ;; point is not already in an xref group.
  (unless (plist-member (text-properties-at (point)) 'xref-group)
    (xref--search-property 'xref-group t))
  ;; Search for the previous xref group.
  (xref--search-property 'xref-group t)
  (xref--search-property 'xref-item)
  (xref-show-location-at-point))

(defun xref--item-at-point ()
  (get-text-property
   (if (eolp) (1- (point)) (point))
   'xref-item))

(defun xref-goto-xref (&optional quit)
  "Jump to the xref on the current line and select its window.
If QUIT is non-nil (interactively, with prefix argument), also
quit the *xref* buffer."
  (interactive "P")
  (let* ((buffer (current-buffer))
         (xref (or (xref--item-at-point)
                   (user-error "Choose a reference to visit")))
         (xref--current-item xref))
    (xref--set-arrow)
    (xref--show-location (xref-item-location xref) (if quit 'quit t))
    (if (fboundp 'next-error-found)
        (next-error-found buffer (current-buffer))
      ;; Emacs < 27
      (setq next-error-last-buffer buffer))))

(defun xref-quit-and-goto-xref ()
  "Quit *xref* buffer, then jump to xref on current line."
  (interactive)
  (xref-goto-xref t))

(defun xref-quit-and-pop-marker-stack ()
  "Quit *xref* buffer, then pop the xref marker stack."
  (interactive)
  (quit-window)
  (xref-pop-marker-stack))

(defun xref-query-replace-in-results (from to)
  "Perform interactive replacement of FROM with TO in all displayed xrefs.

This command interactively replaces FROM with TO in the names of the
references displayed in the current *xref* buffer."
  (interactive
   (let ((fr (read-regexp "Xref query-replace (regexp)" ".*")))
     (list fr
           (read-regexp (format "Xref query-replace (regexp) %s with: " fr)))))
  (let* (item xrefs iter)
    (save-excursion
      (while (setq item (xref--search-property 'xref-item))
        (when (xref-match-length item)
          (push item xrefs))))
    (unwind-protect
        (progn
          (goto-char (point-min))
          (setq iter (xref--buf-pairs-iterator (nreverse xrefs)))
          (xref--query-replace-1 from to iter))
      (funcall iter :cleanup))))

(defun xref--buf-pairs-iterator (xrefs)
  (let (chunk-done item next-pair file-buf pairs all-pairs)
    (lambda (action)
      (pcase action
        (:next
         (when (or xrefs next-pair)
           (setq chunk-done nil)
           (when next-pair
             (setq file-buf (marker-buffer (car next-pair))
                   pairs (list next-pair)
                   next-pair nil))
           (while (and (not chunk-done)
                       (setq item (pop xrefs)))
             (save-excursion
               (let* ((loc (xref-item-location item))
                      (beg (xref-location-marker loc))
                      (end (move-marker (make-marker)
                                        (+ beg (xref-match-length item))
                                        (marker-buffer beg))))
                 (let ((pair (cons beg end)))
                   (push pair all-pairs)
                   ;; Perform sanity check first.
                   (xref--goto-location loc)
                   (if (xref--outdated-p item)
                       (message "Search result out of date, skipping")
                     (cond
                      ((null file-buf)
                       (setq file-buf (marker-buffer beg))
                       (push pair pairs))
                      ((equal file-buf (marker-buffer beg))
                       (push pair pairs))
                      (t
                       (setq chunk-done t
                             next-pair pair))))))))
           (cons file-buf (nreverse pairs))))
        (:cleanup
         (dolist (pair all-pairs)
           (move-marker (car pair) nil)
           (move-marker (cdr pair) nil)))))))

(defun xref--outdated-p (item)
  "Check that the match location at current position is up-to-date.
ITEMS is an xref item which " ; FIXME: Expand documentation.
  ;; FIXME: The check should most likely be a generic function instead
  ;; of the assumption that all matches' summaries relate to the
  ;; buffer text in a particular way.
  (let* ((summary (xref-item-summary item))
         ;; Sometimes buffer contents include ^M, and sometimes Grep
         ;; output includes it, and they don't always match.
         (strip (lambda (s) (if (string-match "\r\\'" s)
                           (substring-no-properties s 0 -1)
                         s)))
         (stripped-summary (funcall strip summary))
         (lendpos (line-end-position))
         (check (lambda ()
                  (let ((comparison-end
                         (+ (point) (length stripped-summary))))
                    (and (>= lendpos comparison-end)
                         (equal stripped-summary
                                (buffer-substring-no-properties
                                 (point) comparison-end)))))))
    (not
     (or
      ;; Either summary contains match text and after
      ;; (2nd+ match on the line)...
      (funcall check)
      ;; ...or it starts at bol, includes the match and after.
      (and (< (point) (+ (line-beginning-position)
                         (length stripped-summary)))
           (save-excursion
             (forward-line 0)
             (funcall check)))))))

;; FIXME: Write a nicer UI.
(defun xref--query-replace-1 (from to iter)
  (let* ((query-replace-lazy-highlight nil)
         (continue t)
         did-it-once buf-pairs pairs
         current-beg current-end
         ;; Counteract the "do the next match now" hack in
         ;; `perform-replace'.  And still, it'll report that those
         ;; matches were "filtered out" at the end.
         (isearch-filter-predicate
          (lambda (beg end)
            (and current-beg
                 (>= beg current-beg)
                 (<= end current-end))))
         (replace-re-search-function
          (lambda (from &optional _bound noerror)
            (let (found pair)
              (while (and (not found) pairs)
                (setq pair (pop pairs)
                      current-beg (car pair)
                      current-end (cdr pair))
                (goto-char current-beg)
                (when (re-search-forward from current-end noerror)
                  (setq found t)))
              found))))
    (while (and continue (setq buf-pairs (funcall iter :next)))
      (if did-it-once
          ;; Reuse the same window for subsequent buffers.
          (switch-to-buffer (car buf-pairs))
        (xref--with-dedicated-window
         (pop-to-buffer (car buf-pairs)))
        (setq did-it-once t))
      (setq pairs (cdr buf-pairs))
      (setq continue
            (perform-replace from to t t nil nil multi-query-replace-map)))
    (unless did-it-once (user-error "No suitable matches here"))
    (when (and continue (not buf-pairs))
      (message "All results processed"))))

(defvar xref--xref-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'xref-next-line)
    (define-key map (kbd "p") #'xref-prev-line)
    (define-key map (kbd "N") #'xref-next-group)
    (define-key map (kbd "P") #'xref-prev-group)
    (define-key map (kbd "r") #'xref-query-replace-in-results)
    (define-key map (kbd "RET") #'xref-goto-xref)
    (define-key map (kbd "TAB")  #'xref-quit-and-goto-xref)
    (define-key map (kbd "C-o") #'xref-show-location-at-point)
    ;; suggested by Johan Claesson "to further reduce finger movement":
    (define-key map (kbd ".") #'xref-next-line)
    (define-key map (kbd ",") #'xref-prev-line)
    (define-key map (kbd "g") #'xref-revert-buffer)
    (define-key map (kbd "M-,") #'xref-quit-and-pop-marker-stack)
    map))

(define-derived-mode xref--xref-buffer-mode special-mode "XREF"
  "Mode for displaying cross-references."
  (setq buffer-read-only t)
  (setq next-error-function #'xref--next-error-function)
  (setq next-error-last-buffer (current-buffer))
  (setq imenu-prev-index-position-function
        #'xref--imenu-prev-index-position)
  (setq imenu-extract-index-name-function
        #'xref--imenu-extract-index-name))

(defvar xref--transient-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'xref-quit-and-goto-xref)
    (set-keymap-parent map xref--xref-buffer-mode-map)
    map))

(define-derived-mode xref--transient-buffer-mode
  xref--xref-buffer-mode
  "XREF Transient")

(defun xref--imenu-prev-index-position ()
  "Move point to previous line in `xref' buffer.
This function is used as a value for
`imenu-prev-index-position-function'."
  (if (bobp)
      nil
    (xref--search-property 'xref-group t)))

(defun xref--imenu-extract-index-name ()
  "Return imenu name for line at point.
This function is used as a value for
`imenu-extract-index-name-function'.  Point should be at the
beginning of the line."
  (buffer-substring-no-properties (line-beginning-position)
                                  (line-end-position)))

(defun xref--next-error-function (n reset?)
  (when reset?
    (goto-char (point-min)))
  (let ((backward (< n 0))
        (n (abs n))
        (xref nil))
    (if (= n 0)
        (setq xref (get-text-property (point) 'xref-item))
      (dotimes (_ n)
        (setq xref (xref--search-property 'xref-item backward))))
    (cond (xref
           ;; Save the current position (when the buffer is visible,
           ;; it gets reset to that window's point from time to time).
           (let ((win (get-buffer-window (current-buffer))))
             (and win (set-window-point win (point))))
           (xref--set-arrow)
           (let ((xref--current-item xref))
             (xref--show-location (xref-item-location xref) t)))
          (t
           (error "No %s xref" (if backward "previous" "next"))))))

(defvar xref--button-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'xref-goto-xref)
    (define-key map [mouse-2] #'xref-select-and-show-xref)
    map))

(defun xref-select-and-show-xref (event)
  "Move point to the button and show the xref definition.
The window showing the xref buffer will be selected."
  (interactive "e")
  (mouse-set-point event)
  (forward-line 0)
  (or (get-text-property (point) 'xref-item)
      (xref--search-property 'xref-item))
  (xref-show-location-at-point))

(define-obsolete-function-alias
  'xref--mouse-2 #'xref-select-and-show-xref "28.1")

(defcustom xref-truncation-width 400
  "The column to visually \"truncate\" each Xref buffer line to."
  :type '(choice
          (integer :tag "Number of columns")
          (const :tag "Disable truncation" nil)))

(defun xref--apply-truncation ()
  (let ((bol (line-beginning-position))
        (eol (line-end-position))
        (inhibit-read-only t)
        pos adjusted-bol)
    (when (and xref-truncation-width
               (> (- eol bol) xref-truncation-width)
               ;; Either truncation not applied yet, or it hides the current
               ;; position: need to refresh.
               (or (and (null (get-text-property (1- eol) 'invisible))
                        (null (get-text-property bol 'invisible)))
                   (get-text-property (point) 'invisible)))
      (setq adjusted-bol
            (cond
             ((eq (get-text-property bol 'face) 'xref-line-number)
              (next-single-char-property-change bol 'face))
             (t bol)))
      (cond
       ((< (- (point) bol) xref-truncation-width)
        (setq pos (+ bol xref-truncation-width))
        (remove-text-properties bol pos '(invisible))
        (put-text-property pos eol 'invisible 'ellipsis))
       ((< (- eol (point)) xref-truncation-width)
        (setq pos (- eol xref-truncation-width))
        (remove-text-properties pos eol '(invisible))
        (put-text-property adjusted-bol pos 'invisible 'ellipsis))
       (t
        (setq pos (- (point) (/ xref-truncation-width 2)))
        (put-text-property adjusted-bol pos 'invisible 'ellipsis)
        (remove-text-properties pos (+ pos xref-truncation-width) '(invisible))
        (put-text-property (+ pos xref-truncation-width) eol 'invisible 'ellipsis))))))

(defun xref--insert-xrefs (xref-alist)
  "Insert XREF-ALIST in the current buffer.
XREF-ALIST is of the form ((GROUP . (XREF ...)) ...), where
GROUP is a string for decoration purposes and XREF is an
`xref-item' object."
  (require 'compile) ; For the compilation faces.
  (cl-loop for (group . xrefs) in xref-alist
           for max-line = (cl-loop for xref in xrefs
                                   maximize (xref-location-line
                                             (xref-item-location xref)))
           for line-format = (and max-line
                                  (format "%%%dd: " (1+ (floor (log max-line 10)))))
           with item-text-props = (list 'mouse-face 'highlight
                                        'keymap xref--button-map
                                        'help-echo
                                        (concat "mouse-2: display in another window, "
                                                "RET or mouse-1: follow reference"))
           with prev-group = nil
           with prev-line = nil
           do
           (xref--insert-propertized '(face xref-file-header xref-group t)
                                     group "\n")
           (dolist (xref xrefs)
             (pcase-let (((cl-struct xref-item summary location) xref))
               (let* ((line (xref-location-line location))
                      (prefix
                       (cond
                        ((not line) "  ")
                        ((and (equal line prev-line)
                              (equal prev-group group))
                         "")
                        (t (propertize (format line-format line)
                                       'face 'xref-line-number)))))
                 ;; Render multiple matches on the same line, together.
                 (when (and (equal prev-group group)
                            (or (null line)
                                (not (equal prev-line line))))
                   (insert "\n"))
                 (xref--insert-propertized (nconc (list 'xref-item xref)
                                                  item-text-props)
                                           prefix summary)
                 (setq prev-line line
                       prev-group group))))
           (insert "\n"))
  (add-to-invisibility-spec '(ellipsis . t))
  (save-excursion
    (goto-char (point-min))
    (while (= 0 (forward-line 1))
      (xref--apply-truncation)))
  (run-hooks 'xref-after-update-hook))

(defun xref--group-name-for-display (group project-root)
  "Return GROUP formatted in the preferred style.

The style is determined by the value of `xref-file-name-display'.
If GROUP looks like a file name, its value is formatted according
to that style.  Otherwise it is returned unchanged."
  ;; XXX: The way we verify that it's indeed a file name and not some
  ;; other kind of string, e.g. Java package name or TITLE from
  ;; `tags-apropos-additional-actions', is pretty lax.  But we don't
  ;; want to use `file-exists-p' for performance reasons.  If this
  ;; ever turns out to be a problem, some other alternatives are to
  ;; either have every location type which uses file names format the
  ;; values themselves (e.g. by piping through some public function),
  ;; or adding a new accessor to locations, like GROUP-TYPE.
  (cl-ecase xref-file-name-display
    (abs group)
    (nondirectory
     (if (string-match-p "\\`~?/" group)
         (file-name-nondirectory group)
       group))
    (project-relative
     (if (and project-root
              (string-prefix-p project-root group))
         (substring group (length project-root))
       group))))

(defun xref--analyze (xrefs)
  "Find common groups in XREFS and format group names.
Return an alist of the form ((GROUP . (XREF ...)) ...)."
  (let* ((alist
          (xref--alistify xrefs
                          (lambda (x)
                            (xref-location-group (xref-item-location x)))))
         (project (and
                   (eq xref-file-name-display 'project-relative)
                   (project-current)))
         (project-root (and project
                            (expand-file-name (xref--project-root project)))))
    (mapcar
     (lambda (pair)
       (cons (xref--group-name-for-display (car pair) project-root)
             (cdr pair)))
     alist)))

(defun xref--show-xref-buffer (fetcher alist)
  (cl-assert (functionp fetcher))
  (let* ((xrefs
          (or
           (assoc-default 'fetched-xrefs alist)
           (funcall fetcher)))
         (xref-alist (xref--analyze xrefs))
         (dd default-directory)
         buf)
    (with-current-buffer (get-buffer-create xref-buffer-name)
      (setq default-directory dd)
      (xref--xref-buffer-mode)
      (xref--show-common-initialize xref-alist fetcher alist)
      (pop-to-buffer (current-buffer))
      (setq buf (current-buffer)))
    (xref--auto-jump-first buf (assoc-default 'auto-jump alist))
    buf))

(defun xref--project-root (project)
  (if (fboundp 'project-root)
      (project-root project)
    (with-no-warnings
      (car (project-roots project)))))

(defun xref--show-common-initialize (xref-alist fetcher alist)
  (setq buffer-undo-list nil)
  (let ((inhibit-read-only t)
        (buffer-undo-list t)
        (inhibit-modification-hooks t))
    (erase-buffer)
    (setq overlay-arrow-position nil)
    (xref--insert-xrefs xref-alist)
    (add-hook 'post-command-hook 'xref--apply-truncation nil t)
    (goto-char (point-min))
    (setq xref--original-window (assoc-default 'window alist)
          xref--original-window-intent (assoc-default 'display-action alist))
    (setq xref--fetcher fetcher)))

(defun xref-revert-buffer ()
  "Refresh the search results in the current buffer."
  (interactive)
  (let ((inhibit-read-only t)
        (buffer-undo-list t)
        (inhibit-modification-hooks t))
    (save-excursion
      (condition-case err
          (let ((alist (xref--analyze (funcall xref--fetcher))))
            (erase-buffer)
            (xref--insert-xrefs alist))
        (user-error
         (erase-buffer)
         (insert
          (propertize
           (error-message-string err)
           'face 'error)))))))

(defun xref--auto-jump-first (buf value)
  (when value
    (select-window (get-buffer-window buf))
    (goto-char (point-min)))
  (cond
   ((eq value t)
    (xref-next-line-no-show)
    (xref-goto-xref))
   ((eq value 'show)
    (xref-next-line))
   ((eq value 'move)
    (forward-line 1))))

(defun xref-show-definitions-buffer (fetcher alist)
  "Show the definitions list in a regular window.

When only one definition found, jump to it right away instead."
  (let ((xrefs (funcall fetcher))
        buf)
    (cond
     ((not (cdr xrefs))
      (xref-pop-to-location (car xrefs)
                            (assoc-default 'display-action alist)))
     (t
      (setq buf
            (xref--show-xref-buffer fetcher
                                    (cons (cons 'fetched-xrefs xrefs)
                                          alist)))
      (xref--auto-jump-first buf (assoc-default 'auto-jump alist))
      buf))))

(define-obsolete-function-alias
  'xref--show-defs-buffer #'xref-show-definitions-buffer "28.1")

(defun xref-show-definitions-buffer-at-bottom (fetcher alist)
  "Show the definitions list in a window at the bottom.

When there is more than one definition, split the selected window
and show the list in a small window at the bottom.  And use a
local keymap that binds `RET' to `xref-quit-and-goto-xref'."
  (let* ((xrefs (funcall fetcher))
         (dd default-directory)
         ;; XXX: Make percentage customizable maybe?
         (max-height (/ (window-height) 2))
         (size-fun (lambda (window)
                     (fit-window-to-buffer window max-height)))
         buf)
    (cond
     ((not (cdr xrefs))
      (xref-pop-to-location (car xrefs)
                            (assoc-default 'display-action alist)))
     (t
      (with-current-buffer (get-buffer-create xref-buffer-name)
        (setq default-directory dd)
        (xref--transient-buffer-mode)
        (xref--show-common-initialize (xref--analyze xrefs) fetcher alist)
        (pop-to-buffer (current-buffer)
                       `(display-buffer-in-direction . ((direction . below)
                                                        (window-height . ,size-fun))))
        (setq buf (current-buffer)))
      (xref--auto-jump-first buf (assoc-default 'auto-jump alist))
      buf))))

(define-obsolete-function-alias 'xref--show-defs-buffer-at-bottom
  #'xref-show-definitions-buffer-at-bottom "28.1")

(defun xref--completing-read-group (cand transform)
  "Return group title of candidate CAND or TRANSFORM the candidate."
  (if transform
      (substring cand (1+ (next-single-property-change 0 'xref--group cand)))
    (get-text-property 0 'xref--group cand)))

(defun xref-show-definitions-completing-read (fetcher alist)
  "Let the user choose the target definition with completion.

When there is more than one definition, let the user choose
between them by typing in the minibuffer with completion."
  (let* ((xrefs (funcall fetcher))
         (xref-alist (xref--analyze xrefs))
         xref-alist-with-line-info
         xref
         (group-prefix-length
          ;; FIXME: Groups are not always file names, but they often
          ;; are.  At least this shouldn't make the other kinds of
          ;; groups look worse.
          (let ((common-prefix (try-completion "" xref-alist)))
            (if (> (length common-prefix) 0)
                (length (file-name-directory common-prefix))
              0))))

    (cl-loop for ((group . xrefs) . more1) on xref-alist
             do
             (cl-loop for (xref . more2) on xrefs do
                      (let* ((summary (xref-item-summary xref))
                             (location (xref-item-location xref))
                             (line (xref-location-line location))
                             (line-fmt
                              (if line
                                  (format #("%d:" 0 2 (face xref-line-number))
                                          line)
                                ""))
                             (group-prefix
                              (substring group group-prefix-length))
                             (group-fmt
                              (propertize group-prefix
                                          'face 'xref-file-header
                                          'xref--group group-prefix))
                             (candidate
                              (format "%s:%s%s" group-fmt line-fmt summary)))
                        (push (cons candidate xref) xref-alist-with-line-info))))

    (setq xref (if (not (cdr xrefs))
                   (car xrefs)
                 (let* ((collection (reverse xref-alist-with-line-info))
                        (ctable
                         (lambda (string pred action)
                           (cond
                            ((eq action 'metadata)
                             `(metadata
                               . ((category . xref-location)
                                  (group-function . ,#'xref--completing-read-group))))
                            (t
                             (complete-with-action action collection string pred)))))
                        (def (caar collection)))
                   (cdr (assoc (completing-read "Choose definition: "
                                                ctable nil t
                                                nil nil
                                                def)
                               collection)))))

    (xref-pop-to-location xref (assoc-default 'display-action alist))))

;; TODO: Can delete this alias before Emacs 28's release.
(define-obsolete-function-alias
  'xref--show-defs-minibuffer #'xref-show-definitions-completing-read "28.1")


(defcustom xref-show-xrefs-function 'xref--show-xref-buffer
  "Function to display a list of search results.

It should accept two arguments: FETCHER and ALIST.

FETCHER is a function of no arguments that returns a list of xref
values.  It must not depend on the current buffer or selected
window.

ALIST can include, but limited to, the following keys:

WINDOW for the window that was selected before the current
command was called.

DISPLAY-ACTION indicates where the target location should be
displayed.  The possible values are nil, `window' meaning the
other window, or `frame' meaning the other frame."
  :type 'function)

(defcustom xref-show-definitions-function 'xref-show-definitions-buffer
  "Function to handle the definition search results.

Accepts the same arguments as `xref-show-xrefs-function'.

Generally, it is expected to jump to the definition if there's
only one, and otherwise provide some way to choose among the
definitions."
  :type '(choice
          (const :tag "Show a regular list of locations"
                 xref-show-definitions-buffer)
          (const :tag "Show a \"transient\" list at the bottom of the window"
                 xref-show-definitions-buffer-at-bottom)
          (const :tag "Choose the definition with completion"
                 xref-show-definitions-completing-read)
          (function :tag "Custom function")))

(defvar xref--read-identifier-history nil)

(defvar xref--read-pattern-history nil)

(defun xref--show-xrefs (fetcher display-action &optional _always-show-list)
  (xref--push-markers)
  (unless (functionp fetcher)
    ;; Old convention.
    (let ((xrefs fetcher))
      (setq fetcher
            (lambda ()
              (if (eq xrefs 'called-already)
                  (user-error "Refresh is not supported")
                (prog1
                    xrefs
                  (setq xrefs 'called-already)))))))
  (funcall xref-show-xrefs-function fetcher
           `((window . ,(selected-window))
             (display-action . ,display-action)
             (auto-jump . ,xref-auto-jump-to-first-xref))))

(defun xref--show-defs (xrefs display-action)
  (xref--push-markers)
  (funcall xref-show-definitions-function xrefs
           `((window . ,(selected-window))
             (display-action . ,display-action)
             (auto-jump . ,xref-auto-jump-to-first-definition))))

(defun xref--push-markers ()
  (unless (region-active-p) (push-mark nil t))
  (xref-push-marker-stack))

(defun xref--prompt-p (command)
  (or (eq xref-prompt-for-identifier t)
      (if (eq (car xref-prompt-for-identifier) 'not)
          (not (memq command (cdr xref-prompt-for-identifier)))
        (memq command xref-prompt-for-identifier))))

(defun xref--read-identifier (prompt)
  "Return the identifier at point or read it from the minibuffer."
  (let* ((backend (xref-find-backend))
         (def (xref-backend-identifier-at-point backend))
         (completion-ignore-case
          (xref-backend-identifier-completion-ignore-case backend)))
    (cond ((or current-prefix-arg
               (not def)
               (xref--prompt-p this-command))
           (let ((id
                  (completing-read
                   (if def
                       (format "%s (default %s): "
                               (substring prompt 0 (string-match
                                                    "[ :]+\\'" prompt))
                               def)
                     prompt)
                   (xref-backend-identifier-completion-table backend)
                   nil nil nil
                   'xref--read-identifier-history def)))
             (if (equal id "")
                 (or def (user-error "There is no default identifier"))
               id)))
          (t def))))


;;; Commands

(defun xref--find-xrefs (input kind arg display-action)
  (xref--show-xrefs
   (xref--create-fetcher input kind arg)
   display-action))

(defun xref--find-definitions (id display-action)
  (xref--show-defs
   (xref--create-fetcher id 'definitions id)
   display-action))

(defun xref--create-fetcher (input kind arg)
  "Return an xref list fetcher function.

It revisits the saved position and delegates the finding logic to
the xref backend method indicated by KIND and passes ARG to it."
  (let* ((orig-buffer (current-buffer))
         (orig-position (point))
         (backend (xref-find-backend))
         (method (intern (format "xref-backend-%s" kind))))
    (lambda ()
      (save-excursion
        ;; Xref methods are generally allowed to depend on the text
        ;; around point, not just on their explicit arguments.
        ;;
        ;; There is only so much we can do, however, to recreate that
        ;; context, given that the user is free to change the buffer
        ;; contents freely in the meantime.
        (when (buffer-live-p orig-buffer)
          (set-buffer orig-buffer)
          (ignore-errors (goto-char orig-position)))
        (let ((xrefs (funcall method backend arg)))
          (unless xrefs
            (xref--not-found-error kind input))
          xrefs)))))

(defun xref--not-found-error (kind input)
  (user-error "No %s found for: %s" (symbol-name kind) input))

;;;###autoload
(defun xref-find-definitions (identifier)
  "Find the definition of the identifier at point.
With prefix argument or when there's no identifier at point,
prompt for it.

If sufficient information is available to determine a unique
definition for IDENTIFIER, display it in the selected window.
Otherwise, display the list of the possible definitions in a
buffer where the user can select from the list.

Use \\[xref-pop-marker-stack] to return back to where you invoked this command."
  (interactive (list (xref--read-identifier "Find definitions of: ")))
  (xref--find-definitions identifier nil))

;;;###autoload
(defun xref-find-definitions-other-window (identifier)
  "Like `xref-find-definitions' but switch to the other window."
  (interactive (list (xref--read-identifier "Find definitions of: ")))
  (xref--find-definitions identifier 'window))

;;;###autoload
(defun xref-find-definitions-other-frame (identifier)
  "Like `xref-find-definitions' but switch to the other frame."
  (interactive (list (xref--read-identifier "Find definitions of: ")))
  (xref--find-definitions identifier 'frame))

;;;###autoload
(defun xref-find-references (identifier)
  "Find references to the identifier at point.
This command might prompt for the identifier as needed, perhaps
offering the symbol at point as the default.
With prefix argument, or if `xref-prompt-for-identifier' is t,
always prompt for the identifier.  If `xref-prompt-for-identifier'
is nil, prompt only if there's no usable symbol at point."
  (interactive (list (xref--read-identifier "Find references of: ")))
  (xref--find-xrefs identifier 'references identifier nil))

;;;###autoload
(defun xref-find-definitions-at-mouse (event)
  "Find the definition of identifier at or around mouse click.
This command is intended to be bound to a mouse event."
  (interactive "e")
  (let ((identifier
         (save-excursion
           (mouse-set-point event)
           (xref-backend-identifier-at-point (xref-find-backend)))))
    (if identifier
        (xref-find-definitions identifier)
      (user-error "No identifier here"))))

;;;###autoload
(defun xref-find-references-at-mouse (event)
  "Find references to the identifier at or around mouse click.
This command is intended to be bound to a mouse event."
  (interactive "e")
  (let ((identifier
         (save-excursion
           (mouse-set-point event)
           (xref-backend-identifier-at-point (xref-find-backend)))))
    (if identifier
        (let ((xref-prompt-for-identifier nil))
          (xref-find-references identifier))
      (user-error "No identifier here"))))

(declare-function apropos-parse-pattern "apropos" (pattern))

;;;###autoload
(defun xref-find-apropos (pattern)
  "Find all meaningful symbols that match PATTERN.
The argument has the same meaning as in `apropos'.
See `tags-apropos-additional-actions' for how to augment the
output of this command when the backend is etags."
  (interactive (list (read-string
                      "Search for pattern (word list or regexp): "
                      nil 'xref--read-pattern-history
                      (xref-backend-identifier-at-point
                       (xref-find-backend)))))
  (require 'apropos)
  (let* ((newpat
          (if (and (version< emacs-version "28.0.50")
                   (memq (xref-find-backend) '(elisp etags)))
              ;; Handle backends in older Emacs.
              (xref-apropos-regexp pattern)
            ;; Delegate pattern handling to the backend fully.
            ;; The old way didn't work for "external" backends.
            pattern)))
    (xref--find-xrefs pattern 'apropos newpat nil)))

(defun xref-apropos-regexp (pattern)
  "Return an Emacs regexp from PATTERN similar to `apropos'."
  (apropos-parse-pattern
   (if (string-equal (regexp-quote pattern) pattern)
       ;; Split into words
       (or (split-string pattern "[ \t]+" t)
           (user-error "No word list given"))
     pattern)))


;;; Key bindings

;;;###autoload (define-key esc-map "." #'xref-find-definitions)
;;;###autoload (define-key esc-map "," #'xref-pop-marker-stack)
;;;###autoload (define-key esc-map "?" #'xref-find-references)
;;;###autoload (define-key esc-map [?\C-.] #'xref-find-apropos)
;;;###autoload (define-key ctl-x-4-map "." #'xref-find-definitions-other-window)
;;;###autoload (define-key ctl-x-5-map "." #'xref-find-definitions-other-frame)


;;; Helper functions

(defvar xref-etags-mode--saved nil)

(define-minor-mode xref-etags-mode
  "Minor mode to make xref use etags again.

Certain major modes install their own mechanisms for listing
identifiers and navigation.  Turn this on to undo those settings
and just use etags."
  :lighter ""
  (if xref-etags-mode
      (progn
        (setq xref-etags-mode--saved xref-backend-functions)
        (kill-local-variable 'xref-backend-functions))
    (setq-local xref-backend-functions xref-etags-mode--saved)))

(declare-function semantic-symref-instantiate "semantic/symref")
(declare-function semantic-symref-perform-search "semantic/symref")
(declare-function grep-expand-template "grep")
(defvar ede-minor-mode) ;; ede.el

;;;###autoload
(defun xref-references-in-directory (symbol dir)
  "Find all references to SYMBOL in directory DIR.
Return a list of xref values.

This function uses the Semantic Symbol Reference API, see
`semantic-symref-tool-alist' for details on which tools are used,
and when."
  (cl-assert (directory-name-p dir))
  (require 'semantic/symref)
  (defvar semantic-symref-tool)

  ;; Some symref backends use `ede-project-root-directory' as the root
  ;; directory for the search, rather than `default-directory'. Since
  ;; the caller has specified `dir', we bind `ede-minor-mode' to nil
  ;; to force the backend to use `default-directory'.
  (let* ((ede-minor-mode nil)
         (default-directory dir)
         ;; FIXME: Remove CScope and Global from the recognized tools?
         ;; The current implementations interpret the symbol search as
         ;; "find all calls to the given function", but not function
         ;; definition. And they return nothing when passed a variable
         ;; name, even a global one.
         (semantic-symref-tool 'detect)
         (case-fold-search nil)
         (inst (semantic-symref-instantiate :searchfor symbol
                                            :searchtype 'symbol
                                            :searchscope 'subdirs
                                            :resulttype 'line-and-text)))
    (xref--convert-hits (semantic-symref-perform-search inst)
                        (format "\\_<%s\\_>" (regexp-quote symbol)))))

(define-obsolete-function-alias
  'xref-collect-references
  #'xref-references-in-directory
  "27.1")

;;;###autoload
(defun xref-matches-in-directory (regexp files dir ignores)
  "Find all matches for REGEXP in directory DIR.
Return a list of xref values.
Only files matching some of FILES and none of IGNORES are searched.
FILES is a string with glob patterns separated by spaces.
IGNORES is a list of glob patterns for files to ignore."
  ;; DIR can also be a regular file for now; let's not advertise that.
  (grep-compute-defaults)
  (defvar grep-find-template)
  (defvar grep-highlight-matches)
  (pcase-let*
      ((grep-find-template (replace-regexp-in-string "<C>" "<C> -E"
                                                     grep-find-template t t))
       (grep-highlight-matches nil)
       ;; TODO: Sanitize the regexp to remove Emacs-specific terms,
       ;; so that Grep can search for the "relaxed" version.  Can we
       ;; do that reliably enough, without creating false negatives?
       (command (xref--rgrep-command (xref--regexp-to-extended regexp)
                                     files
                                     "."
                                     ignores))
       (local-dir (directory-file-name
                   (file-name-unquote
                    (file-local-name (expand-file-name dir)))))
       (buf (get-buffer-create " *xref-grep*"))
       (`(,grep-re ,file-group ,line-group . ,_) (car grep-regexp-alist))
       (status nil)
       (hits nil))
    (with-current-buffer buf
      (erase-buffer)
      (setq default-directory dir)
      (setq status
            (process-file-shell-command command nil t))
      (goto-char (point-min))
      ;; Can't use the exit status: Grep exits with 1 to mean "no
      ;; matches found".  Find exits with 1 if any of the invocations
      ;; exit with non-zero. "No matches" and "Grep program not found"
      ;; are all the same to it.
      (when (and (/= (point-min) (point-max))
                 (not (looking-at grep-re)))
        (user-error "Search failed with status %d: %s" status (buffer-string)))
      (while (re-search-forward grep-re nil t)
        (push (list (string-to-number (match-string line-group))
                    (concat local-dir (substring (match-string file-group) 1))
                    (buffer-substring-no-properties (point) (line-end-position)))
              hits)))
    (xref--convert-hits (nreverse hits) regexp)))

(define-obsolete-function-alias
  'xref-collect-matches
  #'xref-matches-in-directory
  "27.1")

(declare-function tramp-tramp-file-p "tramp")
(declare-function tramp-file-local-name "tramp")

;; TODO: Experiment with 'xargs -P4' (or any other number).
;; This speeds up either command, even more than rg's '-j4' does.
;; Ripgrep gets jumbled output, though, even with --line-buffered.
;; But Grep seems to be stable. Even without --line-buffered.
(defcustom xref-search-program-alist
  '((grep
     .
     ;; '-s' because 'git ls-files' can output broken symlinks.
     "xargs -0 grep <C> --null -snHE -e <R>")
    (ripgrep
     .
     ;; '!*/' is there to filter out dirs (e.g. submodules).
     "xargs -0 rg <C> --null -nH --no-heading --no-messages -g '!*/' -e <R>"
     ))
  "Associative list mapping program identifiers to command templates.

Program identifier should be a symbol, named after the search program.

The command template must be a shell command (or usually a
pipeline) that will search the files based on the list of file
names that is piped from stdin, separated by null characters.
The template should have the following fields:

  <C> for extra arguments such as -i and --color
  <R> for the regexp itself (in Extended format)"
  :type '(repeat
          (cons (symbol :tag "Program identifier")
                (string :tag "Command template")))
  :version "28.1"
  :package-version '(xref . "1.0.4"))

(defcustom xref-search-program 'grep
  "The program to use for regexp search inside files.

This must reference a corresponding entry in `xref-search-program-alist'.

This variable is used in `xref-matches-in-files', which is the
utility function used by commands like `dired-do-find-regexp' and
`project-find-regexp'."
  :type '(choice
          (const :tag "Use Grep" grep)
          (const :tag "Use ripgrep" ripgrep)
          (symbol :tag "User defined"))
  :version "28.1"
  :package-version '(xref . "1.0.4"))

;;;###autoload
(defun xref-matches-in-files (regexp files)
  "Find all matches for REGEXP in FILES.
Return a list of xref values.
FILES must be a list of absolute file names.

See `xref-search-program' and `xref-search-program-alist' for how
to control which program to use when looking for matches."
  (cl-assert (consp files))
  (require 'grep)
  (defvar grep-highlight-matches)
  (pcase-let*
      ((output (get-buffer-create " *project grep output*"))
       (`(,grep-re ,file-group ,line-group . ,_) (car grep-regexp-alist))
       (status nil)
       (hits nil)
       ;; Support for remote files.  The assumption is that, if the
       ;; first file is remote, they all are, and on the same host.
       (dir (file-name-directory (car files)))
       (remote-id (file-remote-p dir))
       ;; The 'auto' default would be fine too, but ripgrep can't handle
       ;; the options we pass in that case.
       (grep-highlight-matches nil)
       (command (grep-expand-template (cdr
                                       (or
                                        (assoc
                                         xref-search-program
                                         xref-search-program-alist)
                                        (user-error "Unknown search program `%s'"
                                                    xref-search-program)))
                                      (xref--regexp-to-extended regexp))))
    (when remote-id
      (require 'tramp)
      (setq files (mapcar
                   (if (tramp-tramp-file-p dir)
                       #'tramp-file-local-name
                       #'file-local-name)
                   files)))
    (when (file-name-quoted-p (car files))
      (setq files (mapcar #'file-name-unquote files)))
    (with-current-buffer output
      (erase-buffer)
      (with-temp-buffer
        (insert (mapconcat #'identity files "\0"))
        (setq default-directory dir)
        (setq status
              (xref--process-file-region (point-min)
                                         (point-max)
                                         shell-file-name
                                         output
                                         nil
                                         shell-command-switch
                                         command)))
      (goto-char (point-min))
      (when (and (/= (point-min) (point-max))
                 (not (looking-at grep-re))
                 ;; TODO: Show these matches as well somehow?
                 (not (looking-at "Binary file .* matches")))
        (user-error "Search failed with status %d: %s" status
                    (buffer-substring (point-min) (line-end-position))))
      (while (re-search-forward grep-re nil t)
        (push (list (string-to-number (match-string line-group))
                    (match-string file-group)
                    (buffer-substring-no-properties (point) (line-end-position)))
              hits)))
    ;; By default, ripgrep's output order is non-deterministic
    ;; (https://github.com/BurntSushi/ripgrep/issues/152)
    ;; because it does the search in parallel.
    ;; Grep's output also comes out in seemingly arbitrary order,
    ;; though stable one. Let's sort both for better UI.
    (setq hits
          (sort (nreverse hits)
                (lambda (h1 h2)
                  (string< (cadr h1) (cadr h2)))))
    (xref--convert-hits hits regexp)))

(defun xref--process-file-region ( start end program
                                   &optional buffer display
                                   &rest args)
  ;; FIXME: This branching shouldn't be necessary, but
  ;; call-process-region *is* measurably faster, even for a program
  ;; doing some actual work (for a period of time). Even though
  ;; call-process-region also creates a temp file internally
  ;; (https://lists.gnu.org/archive/html/emacs-devel/2019-01/msg00211.html).
  (if (not (file-remote-p default-directory))
      (apply #'call-process-region
             start end program nil buffer display args)
    (let ((infile (make-temp-file "ppfr")))
      (unwind-protect
          (progn
            (write-region start end infile nil 'silent)
            (apply #'process-file program infile buffer display args))
        (delete-file infile)))))

(defun xref--rgrep-command (regexp files dir ignores)
  (require 'find-dired)      ; for `find-name-arg'
  (defvar grep-find-template)
  (defvar find-name-arg)
  ;; `shell-quote-argument' quotes the tilde as well.
  (cl-assert (not (string-match-p "\\`~" dir)))
  (grep-expand-template
   grep-find-template
   regexp
   (concat (shell-quote-argument "(")
           " " find-name-arg " "
           (mapconcat
            #'shell-quote-argument
            (split-string files)
            (concat " -o " find-name-arg " "))
           " "
           (shell-quote-argument ")"))
   (shell-quote-argument dir)
   (xref--find-ignores-arguments ignores dir)))

(defun xref--find-ignores-arguments (ignores dir)
  "Convert IGNORES and DIR to a list of arguments for 'find'.
IGNORES is a list of glob patterns.  DIR is an absolute
directory, used as the root of the ignore globs."
  (cl-assert (not (string-match-p "\\`~" dir)))
  (if (not ignores)
      ""
    ;; TODO: All in-tree callers are passing in just "." or "./".
    ;; We can simplify.
    ;; And, if we ever end up deleting xref-matches-in-directory, move
    ;; this function to the project package.
    (setq dir (file-name-as-directory dir))
    (concat
     (shell-quote-argument "(")
     " -path "
     (mapconcat
      (lambda (ignore)
        (when (string-match-p "/\\'" ignore)
          (setq ignore (concat ignore "*")))
        (shell-quote-argument (if (string-match "\\`\\./" ignore)
                                  (replace-match dir t t ignore)
                                (if (string-prefix-p "*" ignore)
                                    ignore
                                  (concat "*/" ignore)))))
      ignores
      " -o -path ")
     " "
     (shell-quote-argument ")")
     " -prune -o ")))

(defun xref--regexp-to-extended (str)
  (replace-regexp-in-string
   ;; FIXME: Add tests.  Move to subr.el, make a public function.
   ;; Maybe error on Emacs-only constructs.
   "\\(?:\\\\\\\\\\)*\\(?:\\\\[][]\\)?\\(?:\\[.+?\\]\\|\\(\\\\?[(){}|]\\)\\)"
   (lambda (str)
     (cond
      ((not (match-beginning 1))
       str)
      ((eq (length (match-string 1 str)) 2)
       (concat (substring str 0 (match-beginning 1))
               (substring (match-string 1 str) 1 2)))
      (t
       (concat (substring str 0 (match-beginning 1))
               "\\"
               (match-string 1 str)))))
   str t t))

(defun xref--regexp-syntax-dependent-p (str)
  "Return non-nil when STR depends on the buffer's syntax.
Such as the current syntax table and the applied syntax properties."
  (let ((case-fold-search nil))
    (string-match-p (rx
                     (or string-start (not (in ?\\)))
                     (0+ (= 2 ?\\))
                     ?\\
                     (in ?b ?B ?< ?> ?w ?W ?_ ?s ?S))
                    str)))

(defvar xref--last-file-buffer nil)
(defvar xref--temp-buffer-file-name nil)

(defun xref--convert-hits (hits regexp)
  (let (xref--last-file-buffer
        (tmp-buffer (generate-new-buffer " *xref-temp*"))
        (remote-id (file-remote-p default-directory))
        (syntax-needed (xref--regexp-syntax-dependent-p regexp)))
    (unwind-protect
        (mapcan (lambda (hit)
                  (xref--collect-matches hit regexp tmp-buffer remote-id syntax-needed))
                hits)
      (kill-buffer tmp-buffer))))

(defun xref--collect-matches (hit regexp tmp-buffer remote-id syntax-needed)
  (pcase-let* ((`(,line ,file ,text) hit)
               (file (and file (concat remote-id file)))
               (buf (xref--find-file-buffer file))
               (inhibit-modification-hooks t))
    (if buf
        (with-current-buffer buf
          (save-excursion
            (save-restriction
              (widen)
              (goto-char (point-min))
              (forward-line (1- line))
              (xref--collect-matches-1 regexp file line
                                       (line-beginning-position)
                                       (line-end-position)
                                       syntax-needed))))
      ;; Using the temporary buffer is both a performance and a buffer
      ;; management optimization.
      (with-current-buffer tmp-buffer
        (erase-buffer)
        (when (and syntax-needed
                   (not (equal file xref--temp-buffer-file-name)))
          (insert-file-contents file nil 0 200)
          ;; Can't (setq-local delay-mode-hooks t) because of
          ;; bug#23272, but the performance penalty seems minimal.
          (let ((buffer-file-name file)
                (inhibit-message t)
                message-log-max)
            (ignore-errors
              (set-auto-mode t)))
          (setq-local xref--temp-buffer-file-name file)
          (setq-local inhibit-read-only t)
          (erase-buffer))
        (insert text)
        (goto-char (point-min))
        (xref--collect-matches-1 regexp file line
                                 (point)
                                 (point-max)
                                 syntax-needed)))))

(defun xref--collect-matches-1 (regexp file line line-beg line-end syntax-needed)
  (let (matches
        stop beg end
        last-beg last-end
        summary-end)
    (when syntax-needed
      (syntax-propertize line-end))
    (while (not stop)
      (if (and
           ;; REGEXP might match an empty string.  Or line.
           (not (and last-beg (eql end line-beg)))
           (re-search-forward regexp line-end t))
          (setq beg (match-beginning 0)
                end (match-end 0)
                summary-end beg)
        (setq stop t
              summary-end line-end))
      (when last-beg
        (let* ((beg-column (- last-beg line-beg))
               (end-column (- last-end line-beg))
               (summary-start (if matches last-beg line-beg))
               (summary (buffer-substring summary-start
                                          summary-end))
               (loc (xref-make-file-location file line beg-column)))
          (add-face-text-property (- last-beg summary-start)
                                  (- last-end summary-start)
                                  'xref-match t summary)
          (push (xref-make-match summary loc (- end-column beg-column))
                matches)))
      (setq last-beg beg
            last-end end))
    (nreverse matches)))

(defun xref--find-file-buffer (file)
  (unless (equal (car xref--last-file-buffer) file)
    (setq xref--last-file-buffer
          ;; `find-buffer-visiting' is considerably slower,
          ;; especially on remote files.
          (cons file (get-file-buffer file))))
  (cdr xref--last-file-buffer))

(provide 'xref)

;;; xref.el ends here
