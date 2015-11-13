;; xref.el --- Cross-referencing commands              -*-lexical-binding:t-*-

;; Copyright (C) 2014-2015 Free Software Foundation, Inc.

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides a somewhat generic infrastructure for cross
;; referencing commands, in particular "find-definition".
;;
;; Some part of the functionality must be implemented in a language
;; dependent way and that's done by defining `xref-find-function',
;; `xref-identifier-at-point-function' and
;; `xref-identifier-completion-table-function', which see.
;;
;; A major mode should make these variables buffer-local first.
;;
;; `xref-find-function' can be called in several ways, see its
;; description.  It has to operate with "xref" and "location" values.
;;
;; One would usually call `make-xref' and `xref-make-file-location',
;; `xref-make-buffer-location' or `xref-make-bogus-location' to create
;; them.  More generally, a location must be an instance of an EIEIO
;; class inheriting from `xref-location' and implementing
;; `xref-location-group' and `xref-location-marker'.
;;
;; Each identifier must be represented as a string.  Implementers can
;; use string properties to store additional information about the
;; identifier, but they should keep in mind that values returned from
;; `xref-identifier-completion-table-function' should still be
;; distinct, because the user can't see the properties when making the
;; choice.
;;
;; See the functions `etags-xref-find' and `elisp-xref-find' for full
;; examples.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'ring)
(require 'pcase)
(require 'project)

(eval-when-compile
  (require 'semantic/symref)) ;; for hit-lines slot

(defgroup xref nil "Cross-referencing commands"
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

(cl-defgeneric xref-match-bounds (_item)
  "Return a cons with columns of the beginning and end of the match."
  nil)

;;;; Commonly needed location classes are defined here:

;; FIXME: might be useful to have an optional "hint" i.e. a string to
;; search for in case the line number is sightly out of date.
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
          (beginning-of-line line)
          (move-to-column column)
          (point-marker))))))

(cl-defmethod xref-location-group ((l xref-file-location))
  (oref l file))

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
   (end-column :initarg :end-column))
  :comment "An xref item describes a reference to a location
somewhere.")

(cl-defmethod xref-match-bounds ((i xref-match-item))
  (with-slots (end-column location) i
    (cons (xref-file-location-column location)
          end-column)))

(defun xref-make-match (summary end-column location)
  "Create and return a new `xref-match-item'.
SUMMARY is a short string to describe the xref.
END-COLUMN is the match end column number inside SUMMARY.
LOCATION is an `xref-location'."
  (make-instance 'xref-match-item :summary summary :location location
                 :end-column end-column))


;;; API

(declare-function etags-xref-find "etags" (action id))
(declare-function tags-lazy-completion-table "etags" ())

;; For now, make the etags backend the default.
(defvar xref-find-function #'etags-xref-find
  "Function to look for cross-references.
It can be called in several ways:

 (definitions IDENTIFIER): Find definitions of IDENTIFIER.  The
result must be a list of xref objects.  If IDENTIFIER contains
sufficient information to determine a unique definition, returns
only that definition. If there are multiple possible definitions,
return all of them.  If no definitions can be found, return nil.

 (references IDENTIFIER): Find references of IDENTIFIER.  The
result must be a list of xref objects.  If no references can be
found, return nil.

 (apropos PATTERN): Find all symbols that match PATTERN.  PATTERN
is a regexp.

IDENTIFIER can be any string returned by
`xref-identifier-at-point-function', or from the table returned
by `xref-identifier-completion-table-function'.

To create an xref object, call `xref-make'.")

(defvar xref-identifier-at-point-function #'xref-default-identifier-at-point
  "Function to get the relevant identifier at point.

The return value must be a string or nil.  nil means no
identifier at point found.

If it's hard to determine the identifier precisely (e.g., because
it's a method call on unknown type), the implementation can
return a simple string (such as symbol at point) marked with a
special text property which `xref-find-function' would recognize
and then delegate the work to an external process.")

(defvar xref-identifier-completion-table-function #'tags-lazy-completion-table
  "Function that returns the completion table for identifiers.")

(defun xref-default-identifier-at-point ()
  (let ((thing (thing-at-point 'symbol)))
    (and thing (substring-no-properties thing))))


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
  "Length of the xref marker ring."
  :type 'integer)

(defcustom xref-prompt-for-identifier '(not xref-find-definitions
                                            xref-find-definitions-other-window
                                            xref-find-definitions-other-frame)
  "When t, always prompt for the identifier name.

When nil, prompt only when there's no value at point we can use,
or when the command has been called with the prefix argument.

Otherwise, it's a list of xref commands which will prompt
anyway (the value at point, if any, will be used as the default).

If the list starts with `not', the meaning of the rest of the
elements is negated."
  :type '(choice (const :tag "always" t)
                 (const :tag "auto" nil)
                 (set :menu-tag "command specific" :tag "commands"
		      :value (not)
		      (const :tag "Except" not)
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

(defun xref-push-marker-stack (&optional m)
  "Add point M (defaults to `point-marker') to the marker stack."
  (ring-insert xref--marker-ring (or m (point-marker))))

;;;###autoload
(defun xref-pop-marker-stack ()
  "Pop back to where \\[xref-find-definitions] was last invoked."
  (interactive)
  (let ((ring xref--marker-ring))
    (when (ring-empty-p ring)
      (error "Marker stack is empty"))
    (let ((marker (ring-remove ring 0)))
      (switch-to-buffer (or (marker-buffer marker)
                            (error "The marked buffer has been deleted")))
      (goto-char (marker-position marker))
      (set-marker marker nil nil)
      (run-hooks 'xref-after-return-hook))))

(defvar xref--current-item nil)

(defun xref-pulse-momentarily ()
  (pcase-let ((`(,beg . ,end)
               (save-excursion
                 (or
                  (xref--match-buffer-bounds xref--current-item)
                  (back-to-indentation)
                  (if (eolp)
                      (cons (line-beginning-position) (1+ (point)))
                    (cons (point) (line-end-position)))))))
    (pulse-momentary-highlight-region beg end 'next-error)))

(defun xref--match-buffer-bounds (item)
  (save-excursion
    (let ((bounds (xref-match-bounds item)))
      (when bounds
        (cons (progn (move-to-column (car bounds))
                     (point))
              (progn (move-to-column (cdr bounds))
                     (point)))))))

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

(defun xref--pop-to-location (item &optional window)
  "Go to the location of ITEM and display the buffer.
WINDOW controls how the buffer is displayed:
  nil      -- switch-to-buffer
  `window' -- pop-to-buffer (other window)
  `frame'  -- pop-to-buffer (other frame)"
  (let* ((marker (save-excursion
                   (xref-location-marker (xref-item-location item))))
         (buf (marker-buffer marker)))
    (cl-ecase window
      ((nil)  (switch-to-buffer buf))
      (window (pop-to-buffer buf t))
      (frame  (let ((pop-up-frames t)) (pop-to-buffer buf t))))
    (xref--goto-char marker))
  (let ((xref--current-item item))
    (run-hooks 'xref-after-jump-hook)))


;;; XREF buffer (part of the UI)

;; The xref buffer is used to display a set of xrefs.

(defvar-local xref--display-history nil
  "List of pairs (BUFFER . WINDOW), for temporarily displayed buffers.")

(defun xref--save-to-history (buf win)
  (let ((restore (window-parameter win 'quit-restore)))
    ;; Save the new entry if the window displayed another buffer
    ;; previously.
    (when (and restore (not (eq (car restore) 'same)))
      (push (cons buf win) xref--display-history))))

(defun xref--display-position (pos other-window buf)
  ;; Show the location, but don't hijack focus.
  (let ((xref-buf (current-buffer)))
    (with-selected-window (display-buffer buf other-window)
      (xref--goto-char pos)
      (run-hooks 'xref-after-jump-hook)
      (let ((buf (current-buffer))
            (win (selected-window)))
        (with-current-buffer xref-buf
          (setq-local other-window-scroll-buffer buf)
          (xref--save-to-history buf win))))))

(defun xref--show-location (location)
  (condition-case err
      (let* ((marker (xref-location-marker location))
             (buf (marker-buffer marker)))
        (xref--display-position marker t buf))
    (user-error (message (error-message-string err)))))

(defun xref-show-location-at-point ()
  "Display the source of xref at point in the other window, if any."
  (interactive)
  (let* ((xref (xref--item-at-point))
         (xref--current-item xref))
    (when xref
      (xref--show-location (xref-item-location xref)))))

(defun xref-next-line ()
  "Move to the next xref and display its source in the other window."
  (interactive)
  (xref--search-property 'xref-item)
  (xref-show-location-at-point))

(defun xref-prev-line ()
  "Move to the previous xref and display its source in the other window."
  (interactive)
  (xref--search-property 'xref-item t)
  (xref-show-location-at-point))

(defun xref--item-at-point ()
  (save-excursion
    (back-to-indentation)
    (get-text-property (point) 'xref-item)))

(defvar-local xref--window nil
  "ACTION argument to call `display-buffer' with.")

(defun xref-goto-xref ()
  "Jump to the xref on the current line and bury the xref buffer."
  (interactive)
  (let ((xref (or (xref--item-at-point)
                 (user-error "No reference at point")))
        (window xref--window))
    (xref-quit)
    (xref--pop-to-location xref window)))

(defun xref-query-replace (from to)
  "Perform interactive replacement in all current matches."
  (interactive
   (list (read-regexp "Query replace regexp in matches" ".*")
         (read-regexp "Replace with: ")))
  (let (pairs item)
    (unwind-protect
        (progn
          (save-excursion
            (goto-char (point-min))
            ;; TODO: Check that none of the matches are out of date;
            ;; offer to re-scan otherwise.  Note that saving the last
            ;; modification tick won't work, as long as not all of the
            ;; buffers are kept open.
            (while (setq item (xref--search-property 'xref-item))
              (when (xref-match-bounds item)
                (save-excursion
                  ;; FIXME: Get rid of xref--goto-location, by making
                  ;; xref-match-bounds return markers already.
                  (xref--goto-location (xref-item-location item))
                  (let ((bounds (xref--match-buffer-bounds item))
                        (beg (make-marker))
                        (end (make-marker)))
                    (move-marker beg (car bounds))
                    (move-marker end (cdr bounds))
                    (push (cons beg end) pairs)))))
            (setq pairs (nreverse pairs)))
          (unless pairs (user-error "No suitable matches here"))
          (xref--query-replace-1 from to pairs))
      (dolist (pair pairs)
        (move-marker (car pair) nil)
        (move-marker (cdr pair) nil)))))

(defun xref--query-replace-1 (from to pairs)
  (let* ((query-replace-lazy-highlight nil)
         current-pair current-buf
         ;; Counteract the "do the next match now" hack in
         ;; `perform-replace'.  And still, it'll report that those
         ;; matches were "filtered out" at the end.
         (isearch-filter-predicate
          (lambda (beg end)
            (and current-pair
                 (eq (current-buffer) current-buf)
                 (>= beg (car current-pair))
                 (<= end (cdr current-pair)))))
         (replace-re-search-function
          (lambda (from &optional _bound noerror)
            (let (found)
              (while (and (not found) pairs)
                (setq current-pair (pop pairs)
                      current-buf  (marker-buffer (car current-pair)))
                (pop-to-buffer current-buf)
                (goto-char (car current-pair))
                (when (re-search-forward from (cdr current-pair) noerror)
                  (setq found t)))
              found))))
    ;; FIXME: Despite this being a multi-buffer replacement, `N'
    ;; doesn't work, because we're not using
    ;; `multi-query-replace-map', and it would expect the below
    ;; function to be called once per buffer.
    (perform-replace from to t t nil)))

(defvar xref--xref-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap quit-window] #'xref-quit)
    (define-key map (kbd "n") #'xref-next-line)
    (define-key map (kbd "p") #'xref-prev-line)
    (define-key map (kbd "r") #'xref-query-replace)
    (define-key map (kbd "RET") #'xref-goto-xref)
    (define-key map (kbd "C-o") #'xref-show-location-at-point)
    ;; suggested by Johan Claesson "to further reduce finger movement":
    (define-key map (kbd ".") #'xref-next-line)
    (define-key map (kbd ",") #'xref-prev-line)
    map))

(define-derived-mode xref--xref-buffer-mode special-mode "XREF"
  "Mode for displaying cross-references."
  (setq buffer-read-only t)
  (setq next-error-function #'xref--next-error-function)
  (setq next-error-last-buffer (current-buffer)))

(defun xref--next-error-function (n reset?)
  (when reset?
    (goto-char (point-min)))
  (let ((backward (< n 0))
        (n (abs n))
        (xref nil))
    (dotimes (_ n)
      (setq xref (xref--search-property 'xref-item backward)))
    (cond (xref
           (xref--pop-to-location xref))
          (t
           (error "No %s xref" (if backward "previous" "next"))))))

(defun xref-quit (&optional kill)
  "Bury temporarily displayed buffers, then quit the current window.

If KILL is non-nil, also kill the current buffer.

The buffers that the user has otherwise interacted with in the
meantime are preserved."
  (interactive "P")
  (let ((window (selected-window))
        (history xref--display-history))
    (setq xref--display-history nil)
    (pcase-dolist (`(,buf . ,win) history)
      (when (and (window-live-p win)
                 (eq buf (window-buffer win)))
        (quit-window nil win)))
    (quit-window kill window)))

(defconst xref-buffer-name "*xref*"
  "The name of the buffer to show xrefs.")

(defvar xref--button-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?m)] #'xref-goto-xref)
    (define-key map [mouse-1] #'xref-goto-xref)
    (define-key map [mouse-2] #'xref--mouse-2)
    map))

(defun xref--mouse-2 (event)
  "Move point to the button and show the xref definition."
  (interactive "e")
  (mouse-set-point event)
  (forward-line 0)
  (xref--search-property 'xref-item)
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
           (xref--insert-propertized '(face compilation-info) group "\n")
           (cl-loop for (xref . more2) on xrefs do
                    (with-slots (summary location) xref
                      (let* ((line (xref-location-line location))
                             (prefix
                              (if line
                                  (propertize (format line-format line)
                                              'face 'compilation-line-number)
                                "  ")))
                        (xref--insert-propertized
                         (list 'xref-item xref
                               ;; 'face 'font-lock-keyword-face
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

(defun xref--show-xref-buffer (xrefs alist)
  (let ((xref-alist (xref--analyze xrefs)))
    (with-current-buffer (get-buffer-create xref-buffer-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (xref--insert-xrefs xref-alist)
        (xref--xref-buffer-mode)
        (pop-to-buffer (current-buffer))
        (goto-char (point-min))
        (setq xref--window (assoc-default 'window alist))
        (current-buffer)))))


;; This part of the UI seems fairly uncontroversial: it reads the
;; identifier and deals with the single definition case.
;; (FIXME: do we really want this case to be handled like that in
;; "find references" and "find regexp searches"?)
;;
;; The controversial multiple definitions case is handed off to
;; xref-show-xrefs-function.

(defvar xref-show-xrefs-function 'xref--show-xref-buffer
  "Function to display a list of xrefs.")

(defvar xref--read-identifier-history nil)

(defvar xref--read-pattern-history nil)

(defun xref--show-xrefs (xrefs window)
  (cond
   ((not (cdr xrefs))
    (xref-push-marker-stack)
    (xref--pop-to-location (car xrefs) window))
   (t
    (xref-push-marker-stack)
    (funcall xref-show-xrefs-function xrefs
             `((window . ,window))))))

(defun xref--prompt-p (command)
  (or (eq xref-prompt-for-identifier t)
      (if (eq (car xref-prompt-for-identifier) 'not)
          (not (memq command (cdr xref-prompt-for-identifier)))
        (memq command xref-prompt-for-identifier))))

(defun xref--read-identifier (prompt)
  "Return the identifier at point or read it from the minibuffer."
  (let ((id (funcall xref-identifier-at-point-function)))
    (cond ((or current-prefix-arg
               (not id)
               (xref--prompt-p this-command))
           (completing-read (if id
                                (format "%s (default %s): "
                                        (substring prompt 0 (string-match
                                                             "[ :]+\\'" prompt))
                                        id)
                              prompt)
                            (funcall xref-identifier-completion-table-function)
                            nil nil nil
                            'xref--read-identifier-history id))
          (t id))))


;;; Commands

(defun xref--find-xrefs (input kind arg window)
  (let ((xrefs (funcall xref-find-function kind arg)))
    (unless xrefs
      (user-error "No %s found for: %s" (symbol-name kind) input))
    (xref--show-xrefs xrefs window)))

(defun xref--find-definitions (id window)
  (xref--find-xrefs id 'definitions id window))

;;;###autoload
(defun xref-find-definitions (identifier)
  "Find the definition of the identifier at point.
With prefix argument or when there's no identifier at point,
prompt for it.

If the backend has sufficient information to determine a unique
definition for IDENTIFIER, it returns only that definition. If
there are multiple possible definitions, it returns all of them.

If the backend returns one definition, jump to it; otherwise,
display the list in a buffer."
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
With prefix argument, prompt for the identifier."
  (interactive (list (xref--read-identifier "Find references of: ")))
  (xref--find-xrefs identifier 'references identifier nil))

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
        (setq xref-etags-mode--saved
              (cons xref-find-function
                    xref-identifier-completion-table-function))
        (kill-local-variable 'xref-find-function)
        (kill-local-variable 'xref-identifier-completion-table-function))
    (setq-local xref-find-function (car xref-etags-mode--saved))
    (setq-local xref-identifier-completion-table-function
                (cdr xref-etags-mode--saved))))

(declare-function semantic-symref-find-references-by-name "semantic/symref")
(declare-function semantic-find-file-noselect "semantic/fw")
(declare-function grep-expand-template "grep")

(defun xref-collect-references (symbol dir)
  "Collect references to SYMBOL inside DIR.
This function uses the Semantic Symbol Reference API, see
`semantic-symref-find-references-by-name' for details on which
tools are used, and when."
  (cl-assert (directory-name-p dir))
  (require 'semantic/symref)
  (defvar semantic-symref-tool)
  (let* ((default-directory dir)
         (semantic-symref-tool 'detect)
         (res (semantic-symref-find-references-by-name symbol 'subdirs))
         (hits (and res (oref res hit-lines)))
         (orig-buffers (buffer-list)))
    (unwind-protect
        (delq nil
              (mapcar (lambda (hit) (xref--collect-match
                                hit (format "\\_<%s\\_>" (regexp-quote symbol))))
                      hits))
      (mapc #'kill-buffer
            (cl-set-difference (buffer-list) orig-buffers)))))

(defun xref-collect-matches (regexp files dir ignores)
  "Collect matches for REGEXP inside FILES in DIR.
FILES is a string with glob patterns separated by spaces.
IGNORES is a list of glob patterns."
  (cl-assert (directory-name-p dir))
  (require 'semantic/fw)
  (grep-compute-defaults)
  (defvar grep-find-template)
  (defvar grep-highlight-matches)
  (let* ((grep-find-template (replace-regexp-in-string "-e " "-E "
                                                       grep-find-template t t))
         (grep-highlight-matches nil)
         (command (xref--rgrep-command (xref--regexp-to-extended regexp)
                                       files dir ignores))
         (orig-buffers (buffer-list))
         (buf (get-buffer-create " *xref-grep*"))
         (grep-re (caar grep-regexp-alist))
         hits)
    (with-current-buffer buf
      (erase-buffer)
      (call-process-shell-command command nil t)
      (goto-char (point-min))
      (while (re-search-forward grep-re nil t)
        (push (cons (string-to-number (match-string 2))
                    (match-string 1))
              hits)))
    (unwind-protect
        (delq nil
              (mapcar (lambda (hit) (xref--collect-match hit regexp))
                      (nreverse hits)))
      (mapc #'kill-buffer
            (cl-set-difference (buffer-list) orig-buffers)))))

(defun xref--rgrep-command (regexp files dir ignores)
  (require 'find-dired)      ; for `find-name-arg'
  (defvar grep-find-template)
  (defvar find-name-arg)
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
   dir
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

(defun xref--collect-match (hit regexp)
  (pcase-let* ((`(,line . ,file) hit)
               (buf (or (find-buffer-visiting file)
                        (semantic-find-file-noselect file))))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (forward-line (1- line))
        (syntax-propertize (line-end-position))
        ;; TODO: Handle multiple matches per line.
        (when (re-search-forward regexp (line-end-position) t)
          (goto-char (match-beginning 0))
          (let ((loc (xref-make-file-location file line
                                              (current-column))))
            (goto-char (match-end 0))
            (xref-make-match (buffer-substring
                              (line-beginning-position)
                              (line-end-position))
                             (current-column)
                             loc)))))))

(provide 'xref)

;;; xref.el ends here
