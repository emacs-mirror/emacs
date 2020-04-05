;; xref.el --- Cross-referencing commands              -*-lexical-binding:t-*-

;; Copyright (C) 2014-2020 Free Software Foundation, Inc.

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

;; NOTE: The xref API is still experimental and can change in major,
;; backward-incompatible ways.  Everyone is encouraged to try it, and
;; report to us any problems or use cases we hadn't anticipated, by
;; sending an email to emacs-devel, or `M-x report-emacs-bug'.
;;
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
;; One would usually call `make-xref' and `xref-make-file-location',
;; `xref-make-buffer-location' or `xref-make-bogus-location' to create
;; them.  More generally, a location must be an instance of an EIEIO
;; class inheriting from `xref-location' and implementing
;; `xref-location-group' and `xref-location-marker'.
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
;; See the etags and elisp-mode implementations for full examples.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'ring)
(require 'project)

(defgroup xref nil "Cross-referencing commands"
  :version "25.1"
  :group 'tools)


;;; Locations

(defclass xref-location () ()
  :documentation "A location represents a position in a file or buffer.")

(cl-defgeneric xref-location-marker (location)
  "Return the marker for LOCATION.")

(cl-defgeneric xref-location-group (location)
  "Return a string used to group a set of locations.
This is typically the filename.")

(cl-defgeneric xref-location-line (_location)
  "Return the line number corresponding to the location."
  nil)

(cl-defgeneric xref-match-length (_item)
  "Return the length of the match."
  nil)

;;;; Commonly needed location classes are defined here:

(defcustom xref-file-name-display 'abs
  "Style of file name display in *xref* buffers.
If the value is the symbol `abs', the default, show the file names
in their full absolute form.
If `nondirectory', show only the nondirectory (a.k.a. \"base name\")
part of the file name."
  :type '(choice (const :tag "absolute file name" abs)
                 (const :tag "nondirectory file name" nondirectory))
  :version "27.1")

;; FIXME: might be useful to have an optional "hint" i.e. a string to
;; search for in case the line number is slightly out of date.
(defclass xref-file-location (xref-location)
  ((file :type string :initarg :file)
   (line :type fixnum :initarg :line :reader xref-location-line)
   (column :type fixnum :initarg :column :reader xref-file-location-column))
  :documentation "A file location is a file/line/column triple.
Line numbers start from 1 and columns from 0.")

(defun xref-make-file-location (file line column)
  "Create and return a new `xref-file-location'."
  (make-instance 'xref-file-location :file file :line line :column column))

(cl-defmethod xref-location-marker ((l xref-file-location))
  (with-slots (file line column) l
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

(cl-defmethod xref-location-group ((l xref-file-location))
  (cl-ecase xref-file-name-display
    (abs (oref l file))
    (nondirectory (file-name-nondirectory (oref l file)))))

(defclass xref-buffer-location (xref-location)
  ((buffer :type buffer :initarg :buffer)
   (position :type fixnum :initarg :position)))

(defun xref-make-buffer-location (buffer position)
  "Create and return a new `xref-buffer-location'."
  (make-instance 'xref-buffer-location :buffer buffer :position position))

(cl-defmethod xref-location-marker ((l xref-buffer-location))
  (with-slots (buffer position) l
    (let ((m (make-marker)))
      (move-marker m position buffer))))

(cl-defmethod xref-location-group ((l xref-buffer-location))
  (with-slots (buffer) l
    (or (buffer-file-name buffer)
        (format "(buffer %s)" (buffer-name buffer)))))

(defclass xref-bogus-location (xref-location)
  ((message :type string :initarg :message
            :reader xref-bogus-location-message))
  :documentation "Bogus locations are sometimes useful to
indicate errors, e.g. when we know that a function exists but the
actual location is not known.")

(defun xref-make-bogus-location (message)
  "Create and return a new `xref-bogus-location'."
  (make-instance 'xref-bogus-location :message message))

(cl-defmethod xref-location-marker ((l xref-bogus-location))
  (user-error "%s" (oref l message)))

(cl-defmethod xref-location-group ((_ xref-bogus-location)) "(No location)")


;;; Cross-reference

(defclass xref-item ()
  ((summary :type string :initarg :summary
            :reader xref-item-summary
            :documentation "One line which will be displayed for
this item in the output buffer.")
   (location :initarg :location
             :reader xref-item-location
             :documentation "An object describing how to navigate
to the reference's target."))
  :comment "An xref item describes a reference to a location
somewhere.")

(defun xref-make (summary location)
  "Create and return a new `xref-item'.
SUMMARY is a short string to describe the xref.
LOCATION is an `xref-location'."
  (make-instance 'xref-item :summary summary :location location))

(defclass xref-match-item ()
  ((summary :type string :initarg :summary
            :reader xref-item-summary)
   (location :initarg :location
             :type xref-file-location
             :reader xref-item-location)
   (length :initarg :length :reader xref-match-length))
  :comment "A match xref item describes a search result.")

(defun xref-make-match (summary location length)
  "Create and return a new `xref-match-item'.
SUMMARY is a short string to describe the xref.
LOCATION is an `xref-location'.
LENGTH is the match length, in characters."
  (make-instance 'xref-match-item :summary summary
                 :location location :length length))


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
return only that definition. If there are multiple possible
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
`project-current' roots."
  (cl-mapcan
   (lambda (dir)
     (xref-references-in-directory identifier dir))
   (let ((pr (project-current t)))
     (append
      (project-roots pr)
      (project-external-roots pr)))))

(cl-defgeneric xref-backend-apropos (backend pattern)
  "Find all symbols that match regexp PATTERN.")

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
(defun xref--alistify (list key test)
  "Partition the elements of LIST into an alist.
KEY extracts the key from an element and TEST is used to compare
keys."
  (let ((alist '()))
    (dolist (e list)
      (let* ((k (funcall key e))
             (probe (cl-assoc k alist :test test)))
        (if probe
            (setcdr probe (cons e (cdr probe)))
          (push (cons k (list e)) alist))))
    ;; Put them back in order.
    (cl-loop for (key . value) in (reverse alist)
             collect (cons key (reverse value)))))

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
  "Set buffer and point according to xref-location LOCATION."
  (let ((marker (xref-location-marker location)))
    (set-buffer (marker-buffer marker))
    (xref--goto-char marker)))

(defun xref-pop-to-location (item &optional action)
  "Go to the location of ITEM and display the buffer.
ACTION controls how the buffer is displayed:
  nil      -- switch-to-buffer
  `window' -- pop-to-buffer (other window)
  `frame'  -- pop-to-buffer (other frame)
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

(defface xref-match '((t :inherit highlight))
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
  (let* ((xref-buf (current-buffer))
         (pop-up-frames
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
      (let ((buf (current-buffer)))
        (with-current-buffer xref-buf
          (setq-local other-window-scroll-buffer buf)))
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

(defun xref-show-location-at-point ()
  "Display the source of xref at point in the appropriate window, if any."
  (interactive)
  (let* ((xref (xref--item-at-point))
         (xref--current-item xref))
    (when xref
      (xref--show-location (xref-item-location xref)))))

(defun xref-next-line ()
  "Move to the next xref and display its source in the appropriate window."
  (interactive)
  (xref--search-property 'xref-item)
  (xref-show-location-at-point))

(defun xref-prev-line ()
  "Move to the previous xref and display its source in the appropriate window."
  (interactive)
  (xref--search-property 'xref-item t)
  (xref-show-location-at-point))

(defun xref--item-at-point ()
  (save-excursion
    (back-to-indentation)
    (get-text-property (point) 'xref-item)))

(defun xref-goto-xref (&optional quit)
  "Jump to the xref on the current line and select its window.
Non-interactively, non-nil QUIT means to first quit the *xref*
buffer."
  (interactive)
  (let* ((buffer (current-buffer))
         (xref (or (xref--item-at-point)
                   (user-error "No reference at point")))
         (xref--current-item xref))
    (xref--show-location (xref-item-location xref) (if quit 'quit t))
    (next-error-found buffer (current-buffer))))

(defun xref-quit-and-goto-xref ()
  "Quit *xref* buffer, then jump to xref on current line."
  (interactive)
  (xref-goto-xref t))

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
                   (if (xref--outdated-p item
                                         (buffer-substring-no-properties
                                          (line-beginning-position)
                                          (line-end-position)))
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

(defun xref--outdated-p (item line-text)
  ;; FIXME: The check should probably be a generic function instead of
  ;; the assumption that all matches contain the full line as summary.
  (let ((summary (xref-item-summary item))
        (strip (lambda (s) (if (string-match "\r\\'" s)
                          (substring-no-properties s 0 -1)
                        s))))
    (not
     ;; Sometimes buffer contents include ^M, and sometimes Grep
     ;; output includes it, and they don't always match.
     (equal (funcall strip line-text)
            (funcall strip summary)))))

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
    (define-key map (kbd "r") #'xref-query-replace-in-results)
    (define-key map (kbd "RET") #'xref-goto-xref)
    (define-key map (kbd "TAB")  #'xref-quit-and-goto-xref)
    (define-key map (kbd "C-o") #'xref-show-location-at-point)
    ;; suggested by Johan Claesson "to further reduce finger movement":
    (define-key map (kbd ".") #'xref-next-line)
    (define-key map (kbd ",") #'xref-prev-line)
    (define-key map (kbd "g") #'xref-revert-buffer)
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
           (xref--show-location (xref-item-location xref) t))
          (t
           (error "No %s xref" (if backward "previous" "next"))))))

(defvar xref--button-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'xref-goto-xref)
    (define-key map [mouse-2] #'xref--mouse-2)
    map))

(defun xref--mouse-2 (event)
  "Move point to the button and show the xref definition."
  (interactive "e")
  (mouse-set-point event)
  (forward-line 0)
  (or (get-text-property (point) 'xref-item)
      (xref--search-property 'xref-item))
  (xref-show-location-at-point))

(defun xref--insert-xrefs (xref-alist)
  "Insert XREF-ALIST in the current-buffer.
XREF-ALIST is of the form ((GROUP . (XREF ...)) ...), where
GROUP is a string for decoration purposes and XREF is an
`xref-item' object."
  (require 'compile) ; For the compilation faces.
  (cl-loop for ((group . xrefs) . more1) on xref-alist
           for max-line-width =
           (cl-loop for xref in xrefs
                    maximize (let ((line (xref-location-line
                                          (oref xref location))))
                               (length (and line (format "%d" line)))))
           for line-format = (and max-line-width
                                  (format "%%%dd: " max-line-width))
           do
           (xref--insert-propertized '(face xref-file-header xref-group t)
                                     group "\n")
           (cl-loop for (xref . more2) on xrefs do
                    (with-slots (summary location) xref
                      (let* ((line (xref-location-line location))
                             (prefix
                              (if line
                                  (propertize (format line-format line)
                                              'face 'xref-line-number)
                                "  ")))
                        (xref--insert-propertized
                         (list 'xref-item xref
                               'mouse-face 'highlight
                               'keymap xref--button-map
                               'help-echo
                               (concat "mouse-2: display in another window, "
                                       "RET or mouse-1: follow reference"))
                         prefix summary)))
                    (insert "\n"))))

(defun xref--analyze (xrefs)
  "Find common filenames in XREFS.
Return an alist of the form ((FILENAME . (XREF ...)) ...)."
  (xref--alistify xrefs
                  (lambda (x)
                    (xref-location-group (xref-item-location x)))
                  #'equal))

(defun xref--show-xref-buffer (fetcher alist)
  (cl-assert (functionp fetcher))
  (let* ((xrefs
          (or
           (assoc-default 'fetched-xrefs alist)
           (funcall fetcher)))
         (xref-alist (xref--analyze xrefs)))
    (with-current-buffer (get-buffer-create xref-buffer-name)
      (xref--xref-buffer-mode)
      (xref--show-common-initialize xref-alist fetcher alist)
      (pop-to-buffer (current-buffer))
      (current-buffer))))

(defun xref--show-common-initialize (xref-alist fetcher alist)
  (setq buffer-undo-list nil)
  (let ((inhibit-read-only t)
        (buffer-undo-list t))
    (erase-buffer)
    (xref--insert-xrefs xref-alist)
    (goto-char (point-min))
    (setq xref--original-window (assoc-default 'window alist)
          xref--original-window-intent (assoc-default 'display-action alist))
    (setq xref--fetcher fetcher)))

(defun xref-revert-buffer ()
  "Refresh the search results in the current buffer."
  (interactive)
  (let ((inhibit-read-only t)
        (buffer-undo-list t))
    (save-excursion
      (erase-buffer)
      (condition-case err
          (xref--insert-xrefs
           (xref--analyze (funcall xref--fetcher)))
        (user-error
         (insert
          (propertize
           (error-message-string err)
           'face 'error))))
      (goto-char (point-min)))))

(defun xref--show-defs-buffer (fetcher alist)
  (let ((xrefs (funcall fetcher)))
    (cond
     ((not (cdr xrefs))
      (xref-pop-to-location (car xrefs)
                            (assoc-default 'display-action alist)))
     (t
      (xref--show-xref-buffer fetcher
                              (cons (cons 'fetched-xrefs xrefs)
                                    alist))))))

(defun xref--show-defs-buffer-at-bottom (fetcher alist)
  "Show definitions list in a window at the bottom.
When there is more than one definition, split the selected window
and show the list in a small window at the bottom.  And use a
local keymap that binds `RET' to `xref-quit-and-goto-xref'."
  (let ((xrefs (funcall fetcher)))
    (cond
     ((not (cdr xrefs))
      (xref-pop-to-location (car xrefs)
                            (assoc-default 'display-action alist)))
     (t
      (with-current-buffer (get-buffer-create xref-buffer-name)
        (xref--transient-buffer-mode)
        (xref--show-common-initialize (xref--analyze xrefs) fetcher alist)
        (pop-to-buffer (current-buffer)
                       '(display-buffer-in-direction . ((direction . below))))
        (current-buffer))))))


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

(defcustom xref-show-definitions-function 'xref--show-defs-buffer
  "Function to display a list of definitions.

Accepts the same arguments as `xref-show-xrefs-function'."
  :type 'function)

(defvar xref--read-identifier-history nil)

(defvar xref--read-pattern-history nil)

(defun xref--show-xrefs (fetcher display-action)
  (xref--push-markers)
  (funcall xref-show-xrefs-function fetcher
           `((window . ,(selected-window))
             (display-action . ,display-action))))

(defun xref--show-defs (xrefs display-action)
  (xref--push-markers)
  (funcall xref-show-definitions-function xrefs
           `((window . ,(selected-window))
             (display-action . ,display-action))))

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
buffer where the user can select from the list."
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

(declare-function apropos-parse-pattern "apropos" (pattern))

;;;###autoload
(defun xref-find-apropos (pattern)
  "Find all meaningful symbols that match PATTERN.
The argument has the same meaning as in `apropos'."
  (interactive (list (read-string
                      "Search for pattern (word list or regexp): "
                      nil 'xref--read-pattern-history)))
  (require 'apropos)
  (xref--find-xrefs pattern 'apropos
                    (apropos-parse-pattern
                     (if (string-equal (regexp-quote pattern) pattern)
                         ;; Split into words
                         (or (split-string pattern "[ \t]+" t)
                             (user-error "No word list given"))
                       pattern))
                    nil))


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
                                     (file-local-name (expand-file-name dir))
                                     ignores))
       (def default-directory)
       (buf (get-buffer-create " *xref-grep*"))
       (`(,grep-re ,file-group ,line-group . ,_) (car grep-regexp-alist))
       (status nil)
       (hits nil))
    (with-current-buffer buf
      (erase-buffer)
      (setq default-directory def)
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
                    (match-string file-group)
                    (buffer-substring-no-properties (point) (line-end-position)))
              hits)))
    (xref--convert-hits (nreverse hits) regexp)))

(define-obsolete-function-alias
  'xref-collect-matches
  #'xref-matches-in-directory
  "27.1")

(declare-function tramp-tramp-file-p "tramp")
(declare-function tramp-file-local-name "tramp")

;;;###autoload
(defun xref-matches-in-files (regexp files)
  "Find all matches for REGEXP in FILES.
Return a list of xref values.
FILES must be a list of absolute file names."
  (cl-assert (consp files))
  (pcase-let*
      ((output (get-buffer-create " *project grep output*"))
       (`(,grep-re ,file-group ,line-group . ,_) (car grep-regexp-alist))
       (status nil)
       (hits nil)
       ;; Support for remote files.  The assumption is that, if the
       ;; first file is remote, they all are, and on the same host.
       (dir (file-name-directory (car files)))
       (remote-id (file-remote-p dir))
       ;; 'git ls-files' can output broken symlinks.
       (command (format "xargs -0 grep %s -snHE -e %s"
                        (if (and case-fold-search
                                 (isearch-no-upper-case-p regexp t))
                            "-i"
                          "")
                        (shell-quote-argument (xref--regexp-to-extended regexp)))))
    (when remote-id
      (require 'tramp)
      (setq files (mapcar
                   (if (tramp-tramp-file-p dir)
                       #'tramp-file-local-name
                       #'file-local-name)
                   files)))
    (with-current-buffer output
      (erase-buffer)
      (with-temp-buffer
        (insert (mapconcat #'identity files "\0"))
        (setq default-directory dir)
        (setq status
              (project--process-file-region (point-min)
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
    (xref--convert-hits (nreverse hits) regexp)))

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
    (concat
     (shell-quote-argument "(")
     " -path "
     (mapconcat
      (lambda (ignore)
        (when (string-match-p "/\\'" ignore)
          (setq ignore (concat ignore "*")))
        (if (string-match "\\`\\./" ignore)
            (setq ignore (replace-match dir t t ignore))
          (unless (string-prefix-p "*" ignore)
            (setq ignore (concat "*/" ignore))))
        (shell-quote-argument ignore))
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
        (tmp-buffer (generate-new-buffer " *xref-temp*")))
    (unwind-protect
        (cl-mapcan (lambda (hit) (xref--collect-matches hit regexp tmp-buffer))
                   hits)
      (kill-buffer tmp-buffer))))

(defun xref--collect-matches (hit regexp tmp-buffer)
  (pcase-let* ((`(,line ,file ,text) hit)
               (remote-id (file-remote-p default-directory))
               (file (and file (concat remote-id file)))
               (buf (xref--find-file-buffer file))
               (syntax-needed (xref--regexp-syntax-dependent-p regexp)))
    (if buf
        (with-current-buffer buf
          (save-excursion
            (goto-char (point-min))
            (forward-line (1- line))
            (xref--collect-matches-1 regexp file line
                                     (line-beginning-position)
                                     (line-end-position)
                                     syntax-needed)))
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
  (let (matches)
    (when syntax-needed
      (syntax-propertize line-end))
    ;; FIXME: This results in several lines with the same
    ;; summary. Solve with composite pattern?
    (while (and
            ;; REGEXP might match an empty string.  Or line.
            (or (null matches)
                (> (point) line-beg))
            (re-search-forward regexp line-end t))
      (let* ((beg-column (- (match-beginning 0) line-beg))
             (end-column (- (match-end 0) line-beg))
             (loc (xref-make-file-location file line beg-column))
             (summary (buffer-substring line-beg line-end)))
        (add-face-text-property beg-column end-column 'xref-match
                                t summary)
        (push (xref-make-match summary loc (- end-column beg-column))
              matches)))
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
