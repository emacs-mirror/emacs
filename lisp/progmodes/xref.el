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

(defgroup xref nil "Cross-referencing commands"
  :group 'tools)


;;; Locations

(defclass xref-location () ()
  :documentation "A location represents a position in a file or buffer.")

;; If a backend decides to subclass xref-location it can provide
;; methods for some of the following functions:
(cl-defgeneric xref-location-marker (location)
  "Return the marker for LOCATION.")

(cl-defgeneric xref-location-group (location)
  "Return a string used to group a set of locations.
This is typically the filename.")

;;;; Commonly needed location classes are defined here:

;; FIXME: might be useful to have an optional "hint" i.e. a string to
;; search for in case the line number is sightly out of date.
(defclass xref-file-location (xref-location)
  ((file :type string :initarg :file)
   (line :type fixnum :initarg :line)
   (column :type fixnum :initarg :column))
  :documentation "A file location is a file/line/column triple.
Line numbers start from 1 and columns from 0.")

(defun xref-make-file-location (file line column)
  "Create and return a new xref-file-location."
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
  (oref l :file))

(defclass xref-buffer-location (xref-location)
  ((buffer :type buffer :initarg :buffer)
   (position :type fixnum :initarg :position)))

(defun xref-make-buffer-location (buffer position)
  "Create and return a new xref-buffer-location."
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
  "Create and return a new xref-bogus-location."
  (make-instance 'xref-bogus-location :message message))

(cl-defmethod xref-location-marker ((l xref-bogus-location))
  (user-error "%s" (oref l :message)))

(cl-defmethod xref-location-group ((_ xref-bogus-location)) "(No location)")

;; This should be in elisp-mode.el, but it's preloaded, and we can't
;; preload defclass and defmethod (at least, not yet).
(defclass xref-elisp-location (xref-location)
  ((symbol :type symbol :initarg :symbol)
   (type   :type symbol :initarg :type)
   (file   :type string :initarg :file
           :reader xref-location-group))
  :documentation "Location of an Emacs Lisp symbol definition.")

(defun xref-make-elisp-location (symbol type file)
  (make-instance 'xref-elisp-location :symbol symbol :type type :file file))

(cl-defmethod xref-location-marker ((l xref-elisp-location))
  (with-slots (symbol type file) l
    (let ((buffer-point
           (pcase type
             (`defun (find-function-search-for-symbol symbol nil file))
             ((or `defvar `defface)
              (find-function-search-for-symbol symbol type file))
             (`feature
              (cons (find-file-noselect file) 1)))))
      (with-current-buffer (car buffer-point)
        (goto-char (or (cdr buffer-point) (point-min)))
        (point-marker)))))


;;; Cross-reference

(defclass xref--xref ()
  ((description :type string :initarg :description
                :reader xref--xref-description)
   (location :type xref-location :initarg :location
             :reader xref--xref-location))
  :comment "An xref is used to display and locate constructs like
variables or functions.")

(defun xref-make (description location)
  "Create and return a new xref.
DESCRIPTION is a short string to describe the xref.
LOCATION is an `xref-location'."
  (make-instance 'xref--xref :description description :location location))


;;; API

(declare-function etags-xref-find "etags" (action id))
(declare-function tags-lazy-completion-table "etags" ())

;; For now, make the etags backend the default.
(defvar xref-find-function #'etags-xref-find
  "Function to look for cross-references.
It can be called in several ways:

 (definitions IDENTIFIER): Find definitions of IDENTIFIER.  The
result must be a list of xref objects.  If no definitions can be
found, return nil.

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
  :type 'integer
  :version "25.1")

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
      (set-marker marker nil nil))))

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


(defun xref--goto-location (location)
  "Set buffer and point according to xref-location LOCATION."
  (let ((marker (xref-location-marker location)))
    (set-buffer (marker-buffer marker))
    (cond ((and (<= (point-min) marker) (<= marker (point-max))))
          (widen-automatically (widen))
          (t (error "Location is outside accessible part of buffer")))
    (goto-char marker)))

(defun xref--pop-to-location (location &optional window)
  "Goto xref-location LOCATION and display the buffer.
WINDOW controls how the buffer is displayed:
  nil      -- switch-to-buffer
  'window  -- pop-to-buffer (other window)
  'frame   -- pop-to-buffer (other frame)"
  (xref--goto-location location)
  (cl-ecase window
    ((nil)  (switch-to-buffer (current-buffer)))
    (window (pop-to-buffer (current-buffer) t))
    (frame  (let ((pop-up-frames t)) (pop-to-buffer (current-buffer) t)))))


;;; XREF buffer (part of the UI)

;; The xref buffer is used to display a set of xrefs.

(defvar-local xref--display-history nil
  "List of pairs (BUFFER . WINDOW), for temporarily displayed buffers.")

(defvar-local xref--temporary-buffers nil
  "List of buffers created by xref code.")

(defvar-local xref--current nil
  "Non-nil if this buffer was once current, except while displaying xrefs.
Used for temporary buffers.")

(defvar xref--inhibit-mark-current nil)

(defun xref--mark-selected ()
  (unless xref--inhibit-mark-current
    (setq xref--current t))
  (remove-hook 'buffer-list-update-hook #'xref--mark-selected t))

(defun xref--save-to-history (buf win)
  (let ((restore (window-parameter win 'quit-restore)))
    ;; Save the new entry if the window displayed another buffer
    ;; previously.
    (when (and restore (not (eq (car restore) 'same)))
      (push (cons buf win) xref--display-history))))

(defun xref--display-position (pos other-window recenter-arg xref-buf)
  ;; Show the location, but don't hijack focus.
  (with-selected-window (display-buffer (current-buffer) other-window)
    (goto-char pos)
    (recenter recenter-arg)
    (let ((buf (current-buffer))
          (win (selected-window)))
      (with-current-buffer xref-buf
        (setq-local other-window-scroll-buffer buf)
        (xref--save-to-history buf win)))))

(defun xref--show-location (location)
  (condition-case err
      (let ((xref-buf (current-buffer))
            (bl (buffer-list))
            (xref--inhibit-mark-current t))
        (xref--goto-location location)
        (let ((buf (current-buffer)))
          (unless (memq buf bl)
            ;; Newly created.
            (add-hook 'buffer-list-update-hook #'xref--mark-selected nil t)
            (with-current-buffer xref-buf
              (push buf xref--temporary-buffers))))
        (xref--display-position (point) t 1 xref-buf))
    (user-error (message (error-message-string err)))))

(defun xref-show-location-at-point ()
  "Display the source of xref at point in the other window, if any."
  (interactive)
  (let ((loc (xref--location-at-point)))
    (when loc
      (xref--show-location loc))))

(defun xref-next-line ()
  "Move to the next xref and display its source in the other window."
  (interactive)
  (xref--search-property 'xref-location)
  (xref-show-location-at-point))

(defun xref-prev-line ()
  "Move to the previous xref and display its source in the other window."
  (interactive)
  (xref--search-property 'xref-location t)
  (xref-show-location-at-point))

(defun xref--location-at-point ()
  (get-text-property (point) 'xref-location))

(defvar-local xref--window nil
  "ACTION argument to call `display-buffer' with.")

(defun xref-goto-xref ()
  "Jump to the xref on the current line and bury the xref buffer."
  (interactive)
  (back-to-indentation)
  (let ((loc (or (xref--location-at-point)
                 (user-error "No reference at point")))
        (window xref--window))
    (xref-quit)
    (xref--pop-to-location loc window)))

(defvar xref--xref-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap quit-window] #'xref-quit)
    (define-key map (kbd "n") #'xref-next-line)
    (define-key map (kbd "p") #'xref-prev-line)
    (define-key map (kbd "RET") #'xref-goto-xref)
    (define-key map (kbd "C-o") #'xref-show-location-at-point)
    ;; suggested by Johan Claesson "to further reduce finger movement":
    (define-key map (kbd ".") #'xref-next-line)
    (define-key map (kbd ",") #'xref-prev-line)
    map))

(define-derived-mode xref--xref-buffer-mode special-mode "XREF"
  "Mode for displaying cross-references."
  (setq buffer-read-only t))

(defun xref-quit (&optional kill)
  "Bury temporarily displayed buffers, then quit the current window.

If KILL is non-nil, kill all buffers that were created in the
process of showing xrefs, and also kill the current buffer.

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
    (when kill
      (let ((xref--inhibit-mark-current t)
            kill-buffer-query-functions)
        (dolist (buf xref--temporary-buffers)
          (unless (buffer-local-value 'xref--current buf)
            (kill-buffer buf)))
        (setq xref--temporary-buffers nil)))
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
  (xref--search-property 'xref-location)
  (xref-show-location-at-point))

(defun xref--insert-xrefs (xref-alist)
  "Insert XREF-ALIST in the current-buffer.
XREF-ALIST is of the form ((GROUP . (XREF ...)) ...).  Where
GROUP is a string for decoration purposes and XREF is an
`xref--xref' object."
  (cl-loop for ((group . xrefs) . more1) on xref-alist do
           (xref--insert-propertized '(face bold) group "\n")
           (cl-loop for (xref . more2) on xrefs do
                    (insert "  ")
                    (with-slots (description location) xref
                      (xref--insert-propertized
                       (list 'xref-location location
                             'face 'font-lock-keyword-face
                             'mouse-face 'highlight
                             'keymap xref--button-map
                             'help-echo
                             (concat "mouse-2: display in another window, "
                                     "RET or mouse-1: follow reference"))
                       description))
                    (when (or more1 more2)
                      (insert "\n")))))

(defun xref--analyze (xrefs)
  "Find common filenames in XREFS.
Return an alist of the form ((FILENAME . (XREF ...)) ...)."
  (xref--alistify xrefs
                  (lambda (x)
                    (xref-location-group (xref--xref-location x)))
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
        (setq xref--temporary-buffers (assoc-default 'temporary-buffers alist))
        (dolist (buf xref--temporary-buffers)
          (with-current-buffer buf
            (add-hook 'buffer-list-update-hook #'xref--mark-selected nil t)))
        (current-buffer)))))


;; This part of the UI seems fairly uncontroversial: it reads the
;; identifier and deals with the single definition case.
;;
;; The controversial multiple definitions case is handed off to
;; xref-show-xrefs-function.

(defvar xref-show-xrefs-function 'xref--show-xref-buffer
  "Function to display a list of xrefs.")

(defvar xref--read-identifier-history nil)

(defvar xref--read-pattern-history nil)

(defun xref--show-xrefs (input kind arg window)
  (let* ((bl (buffer-list))
         (xrefs (funcall xref-find-function kind arg))
         (tb (cl-set-difference (buffer-list) bl)))
    (cond
     ((null xrefs)
      (user-error "No known %s for: %s" (symbol-name kind) input))
     ((not (cdr xrefs))
      (xref-push-marker-stack)
      (xref--pop-to-location (xref--xref-location (car xrefs)) window))
     (t
      (xref-push-marker-stack)
      (funcall xref-show-xrefs-function xrefs
               `((window . ,window)
                 (temporary-buffers . ,tb)))))))

(defun xref--read-identifier (prompt)
  "Return the identifier at point or read it from the minibuffer."
  (let ((id (funcall xref-identifier-at-point-function)))
    (cond ((or current-prefix-arg (not id))
           (completing-read prompt
                            (funcall xref-identifier-completion-table-function)
                            nil t id
                            'xref--read-identifier-history))
          (t id))))


;;; Commands

(defun xref--find-definitions (id window)
  (xref--show-xrefs id 'definitions id window))

;;;###autoload
(defun xref-find-definitions (identifier)
  "Find the definition of the identifier at point.
With prefix argument or when there's no identifier at point,
prompt for it."
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
  (xref--show-xrefs identifier 'references identifier nil))

(declare-function apropos-parse-pattern "apropos" (pattern))

;;;###autoload
(defun xref-find-apropos (pattern)
  "Find all meaningful symbols that match PATTERN.
The argument has the same meaning as in `apropos'."
  (interactive (list (read-from-minibuffer
                      "Search for pattern (word list or regexp): "
                      nil nil nil 'xref--read-pattern-history)))
  (require 'apropos)
  (xref--show-xrefs pattern 'apropos
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


(provide 'xref)

;;; xref.el ends here
