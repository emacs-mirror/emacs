;;; window.el --- GNU Emacs window commands aside from those written in C  -*- lexical-binding:t -*-

;; Copyright (C) 1985, 1989, 1992-1994, 2000-2022 Free Software
;; Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org
;; Keywords: internal
;; Package: emacs

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

;; Window tree functions.

;;; Code:

(defun internal--before-save-selected-window ()
  (cons (selected-window)
        ;; We save and restore all frames' selected windows, because
        ;; `select-window' can change the frame-selected-window of
        ;; whatever frame that window is in.  Each text terminal's
        ;; top-frame is preserved by putting it last in the list.
        (apply #'append
               (mapcar (lambda (terminal)
                         (let ((frames (frames-on-display-list terminal))
                               (top-frame (tty-top-frame terminal))
                               alist)
                           (if top-frame
                               (setq frames
                                     (cons top-frame
                                           (delq top-frame frames))))
                           (dolist (f frames)
                             (push (cons f (frame-selected-window f))
                                   alist))
                           alist))
                       (terminal-list)))))

(defun internal--after-save-selected-window (state)
  (dolist (elt (cdr state))
    (and (frame-live-p (car elt))
         (window-live-p (cdr elt))
         (set-frame-selected-window (car elt) (cdr elt) 'norecord)))
  (when (window-live-p (car state))
    (select-window (car state) 'norecord)))

(defmacro save-selected-window (&rest body)
  "Execute BODY, then select the previously selected window.
The value returned is the value of the last form in BODY.

This macro saves and restores the selected window, as well as the
selected window in each frame.  If the previously selected window
is no longer live, then whatever window is selected at the end of
BODY remains selected.  If the previously selected window of some
frame is no longer live at the end of BODY, that frame's selected
window is left alone.

This macro saves and restores the current buffer, since otherwise
its normal operation could make a different buffer current.  The
order of recently selected windows and the buffer list ordering
are not altered by this macro (unless they are altered in BODY)."
  (declare (indent 0) (debug t))
  `(let ((save-selected-window--state (internal--before-save-selected-window)))
     (save-current-buffer
       (unwind-protect
	   (progn ,@body)
         (internal--after-save-selected-window save-selected-window--state)))))

(defvar temp-buffer-window-setup-hook nil
  "Normal hook run by `with-temp-buffer-window' before buffer display.
This hook is run by `with-temp-buffer-window' with the buffer to be
displayed current.")

(defvar temp-buffer-window-show-hook nil
  "Normal hook run by `with-temp-buffer-window' after buffer display.
This hook is run by `with-temp-buffer-window' with the buffer
displayed and current and its window selected.")

(defun temp-buffer-window-setup (buffer-or-name)
  "Set up temporary buffer specified by BUFFER-OR-NAME.
Return the buffer."
  (let ((old-dir default-directory)
	(buffer (get-buffer-create buffer-or-name)))
    (with-current-buffer buffer
      (kill-all-local-variables)
      (setq default-directory old-dir)
      (delete-all-overlays)
      (setq buffer-read-only nil)
      (setq buffer-file-name nil)
      (setq buffer-undo-list t)
      (let ((inhibit-read-only t)
	    (inhibit-modification-hooks t))
	(erase-buffer)
	(run-hooks 'temp-buffer-window-setup-hook))
      ;; Return the buffer.
      buffer)))

(defun temp-buffer-window-show (buffer &optional action)
  "Show temporary buffer BUFFER in a window.
Return the window showing BUFFER.  Pass ACTION as action argument
to `display-buffer'."
  (let (window frame)
    (with-current-buffer buffer
      (set-buffer-modified-p nil)
      (setq buffer-read-only t)
      (goto-char (point-min))
      (when (let ((window-combination-limit
		   ;; When `window-combination-limit' equals
		   ;; `temp-buffer' or `temp-buffer-resize' and
		   ;; `temp-buffer-resize-mode' is enabled in this
		   ;; buffer bind it to t so resizing steals space
		   ;; preferably from the window that was split.
		   (if (or (eq window-combination-limit 'temp-buffer)
			   (and (eq window-combination-limit
				    'temp-buffer-resize)
				temp-buffer-resize-mode))
		       t
		     window-combination-limit)))
	      (setq window (display-buffer buffer action)))
	(setq frame (window-frame window))
	(unless (eq frame (selected-frame))
	  (raise-frame frame))
	(setq minibuffer-scroll-window window)
	(set-window-hscroll window 0)
	(with-selected-window window
	  (run-hooks 'temp-buffer-window-show-hook)
	  (when temp-buffer-resize-mode
	    (resize-temp-buffer-window window)))
	;; Return the window.
	window))))

(defmacro with-temp-buffer-window (buffer-or-name action quit-function &rest body)
  "Bind `standard-output' to BUFFER-OR-NAME, eval BODY, show the buffer.
BUFFER-OR-NAME must specify either a live buffer, or the name of
a buffer (if it does not exist, this macro creates it).

Make the buffer specified by BUFFER-OR-NAME empty before running
BODY and bind `standard-output' to that buffer, so that output
generated with `prin1' and similar functions in BODY goes into
that buffer.  Do not make that buffer current for running the
forms in BODY.  Use `with-current-buffer-window' instead if you
need to run BODY with that buffer current.

At the end of BODY, mark the specified buffer unmodified and
read-only, and display it in a window (but do not select it).
The display happens by calling `display-buffer' passing it the
ACTION argument.  If `temp-buffer-resize-mode' is enabled, the
corresponding window may be resized automatically.

Return the value returned by BODY, unless QUIT-FUNCTION specifies
a function.  In that case, run that function with two arguments -
the window showing the specified buffer and the value returned by
BODY - and return the value returned by that function.

If the buffer is displayed on a new frame, the window manager may
decide to select that frame.  In that case, it's usually a good
strategy if QUIT-FUNCTION selects the window showing the buffer
before reading any value from the minibuffer; for example, when
asking a `yes-or-no-p' question.

This runs the hook `temp-buffer-window-setup-hook' before BODY,
with the specified buffer temporarily current.  It runs the hook
`temp-buffer-window-show-hook' after displaying the buffer, with
that buffer temporarily current, and the window that was used to
display it temporarily selected.

This construct is similar to `with-output-to-temp-buffer' but,
neither runs `temp-buffer-setup-hook' which usually puts the
buffer in Help mode, nor `temp-buffer-show-function' (the ACTION
argument replaces this)."
  (declare (debug t) (indent 3))
  (let ((buffer (make-symbol "buffer"))
	(window (make-symbol "window"))
	(value (make-symbol "value")))
    (macroexp-let2* nil ((vbuffer-or-name buffer-or-name)
			 (vaction action)
			 (vquit-function quit-function))
      `(let* ((,buffer (temp-buffer-window-setup ,vbuffer-or-name))
	      (standard-output ,buffer)
	      ,window ,value)
	 (setq ,value (progn ,@body))
	 (with-current-buffer ,buffer
	   (setq ,window (temp-buffer-window-show ,buffer ,vaction)))

	 (if (functionp ,vquit-function)
	     (funcall ,vquit-function ,window ,value)
	   ,value)))))

(defmacro with-current-buffer-window (buffer-or-name action quit-function &rest body)
  "Evaluate BODY with a buffer BUFFER-OR-NAME current and show that buffer.
This construct is like `with-temp-buffer-window' but unlike that,
makes the buffer specified by BUFFER-OR-NAME current for running
BODY."
  (declare (debug t) (indent 3))
  (let ((buffer (make-symbol "buffer"))
	(window (make-symbol "window"))
	(value (make-symbol "value")))
    (macroexp-let2* nil ((vbuffer-or-name buffer-or-name)
			 (vaction action)
			 (vquit-function quit-function))
      `(let* ((,buffer (temp-buffer-window-setup ,vbuffer-or-name))
	      (standard-output ,buffer)
	      ,window ,value)
	 (with-current-buffer ,buffer
	   (setq ,value (progn ,@body))
	   (setq ,window (temp-buffer-window-show ,buffer ,vaction)))

	 (if (functionp ,vquit-function)
	     (funcall ,vquit-function ,window ,value)
	   ,value)))))

(defmacro with-displayed-buffer-window (buffer-or-name action quit-function &rest body)
  "Show a buffer BUFFER-OR-NAME and evaluate BODY in that buffer.
This construct is like `with-current-buffer-window' but unlike that,
displays the buffer specified by BUFFER-OR-NAME before running BODY."
  (declare (debug t) (indent 3)
           (obsolete "use `with-current-buffer-window' with action alist entry `body-function'."
                     "28.1"))
  (let ((buffer (make-symbol "buffer"))
	(window (make-symbol "window"))
	(value (make-symbol "value")))
    (macroexp-let2* nil ((vbuffer-or-name buffer-or-name)
			 (vaction action)
			 (vquit-function quit-function))
      `(let* ((,buffer (temp-buffer-window-setup ,vbuffer-or-name))
	      (standard-output ,buffer)
              ;; If a 'window-height' entry specifies a function,
              ;; remember it here in order to call it below but replace
              ;; the entry so `window--try-to-split-window' will bind
              ;; `window-combination-limit' to t and the function does
              ;; not resize any other window but the one we split this
              ;; one off (Bug#25055, Bug#25179).
              (vheight-function
               (let ((window-height (assq 'window-height (cdr ,vaction))))
                 (when (functionp (cdr window-height))
                   (cdr window-height))))
              (vaction-copied
               (when vheight-function
                 (cons (car , vaction)
                       (cons
                        '(window-height . t)
                        (assq-delete-all
                         'window-height (cdr (copy-sequence ,vaction)))))))
	      ,window ,value)
	 (with-current-buffer ,buffer
	   (setq ,window (temp-buffer-window-show
                          ,buffer (or vaction-copied ,vaction))))

	 (let ((inhibit-read-only t)
	       (inhibit-modification-hooks t))
	   (setq ,value (progn ,@body)))

	 (set-window-point ,window (point-min))

	 (when vheight-function
	   (ignore-errors
	     (set-window-parameter ,window 'preserve-size nil)
             (funcall vheight-function ,window)))

	 (when (consp (cdr (assq 'preserve-size (cdr ,vaction))))
	   (window-preserve-size
	    ,window t (cadr (assq 'preserve-size (cdr ,vaction))))
	  (window-preserve-size
	    ,window nil (cddr (assq 'preserve-size (cdr ,vaction)))))

	 (if (functionp ,vquit-function)
	     (funcall ,vquit-function ,window ,value)
	   ,value)))))

(defmacro with-window-non-dedicated (window &rest body)
  "Evaluate BODY with WINDOW temporarily made non-dedicated.
If WINDOW is nil, use the selected window.  Return the value of
the last form in BODY."
  (declare (indent 1) (debug t))
  (let ((window-dedicated-sym (gensym))
        (window-sym (gensym)))
    `(let* ((,window-sym (window-normalize-window ,window t))
            (,window-dedicated-sym (window-dedicated-p ,window-sym)))
       (set-window-dedicated-p ,window-sym nil)
       (unwind-protect
           (progn ,@body)
         ;; `window-dedicated-p' returns the value set by
         ;; `set-window-dedicated-p', which differentiates non-nil and
         ;; t, so we cannot simply use t here. That's why we use
         ;; `window-dedicated-sym'.
         (set-window-dedicated-p ,window-sym ,window-dedicated-sym)))))

;; The following two functions are like `window-next-sibling' and
;; `window-prev-sibling' but the WINDOW argument is _not_ optional (so
;; they don't substitute the selected window for nil), and they return
;; nil when WINDOW doesn't have a parent (like a frame's root window or
;; a minibuffer window).
(defun window-right (window)
  "Return WINDOW's right sibling.
Return nil if WINDOW is the root window of its frame.  WINDOW can
be any window."
  (and window (window-parent window) (window-next-sibling window)))

(defun window-left (window)
  "Return WINDOW's left sibling.
Return nil if WINDOW is the root window of its frame.  WINDOW can
be any window."
  (and window (window-parent window) (window-prev-sibling window)))

(defun window-child (window)
  "Return WINDOW's first child window.
WINDOW can be any window."
  (or (window-top-child window) (window-left-child window)))

(defun window-child-count (window)
  "Return number of WINDOW's child windows.
WINDOW can be any window."
  (let ((count 0))
    (when (and (windowp window) (setq window (window-child window)))
      (while window
	(setq count (1+ count))
	(setq window (window-next-sibling window))))
    count))

(defun window-last-child (window)
  "Return last child window of WINDOW.
WINDOW can be any window."
  (when (and (windowp window) (setq window (window-child window)))
    (while (window-next-sibling window)
      (setq window (window-next-sibling window))))
  window)

(defun window-normalize-buffer (buffer-or-name)
  "Return buffer specified by BUFFER-OR-NAME.
BUFFER-OR-NAME must be a live buffer, a string naming a live
buffer or nil which means to return the current buffer.

This function is commonly used to process the (usually optional)
\"BUFFER-OR-NAME\" argument of window related functions where nil
stands for the current buffer."
  (let ((buffer
         (cond
          ((not buffer-or-name)
           (current-buffer))
          ((bufferp buffer-or-name)
           buffer-or-name)
          ((stringp buffer-or-name)
           (get-buffer buffer-or-name))
          (t
           (error "No such buffer %s" buffer-or-name)))))
    (if (buffer-live-p buffer)
	buffer
      (error "No such live buffer %s" buffer-or-name))))

(defun window-normalize-frame (frame)
  "Return frame specified by FRAME.
FRAME must be a live frame or nil which means to return the
selected frame.

This function is commonly used to process the (usually optional)
\"FRAME\" argument of window and frame related functions where
nil stands for the selected frame."
  (if frame
      (if (frame-live-p frame)
	  frame
	(error "%s is not a live frame" frame))
    (selected-frame)))

(defun window-normalize-window (window &optional live-only)
  "Return window specified by WINDOW.
If WINDOW is nil, return the selected window.  Otherwise, if
WINDOW is a live or an internal window, return WINDOW; if
LIVE-ONLY is non-nil, return WINDOW for a live window only.
Otherwise, signal an error.

This function is commonly used to process the (usually optional)
\"WINDOW\" argument of window related functions where nil stands
for the selected window."
  (cond
   ((null window)
    (selected-window))
   (live-only
    (if (window-live-p window)
	window
      (error "%s is not a live window" window)))
   ((window-valid-p window)
    window)
   (t
    (error "%s is not a valid window" window))))

;; Maybe this should go to frame.el.
(defun frame-char-size (&optional window-or-frame horizontal)
  "Return the value of `frame-char-height' for WINDOW-OR-FRAME.
If WINDOW-OR-FRAME is a live frame, return the value of
`frame-char-height' for that frame.  If WINDOW-OR-FRAME is a
valid window, return the value of `frame-char-height' for that
window's frame.  In any other case, return the value of
`frame-char-height' for the selected frame.

Optional argument HORIZONTAL non-nil means to return the value of
`frame-char-width' for WINDOW-OR-FRAME."
  (let ((frame
	 (cond
	  ((window-valid-p window-or-frame)
	   (window-frame window-or-frame))
	  ((frame-live-p window-or-frame)
	   window-or-frame)
	  (t (selected-frame)))))
    (if horizontal
	(frame-char-width frame)
      (frame-char-height frame))))

(defvar ignore-window-parameters nil
  "If non-nil, standard functions ignore window parameters.
The functions currently affected by this are `split-window',
`delete-window', `delete-other-windows' and `other-window'.

An application may bind this to a non-nil value around calls to
these functions to inhibit processing of window parameters.")

;; This must go to C, finally (or get removed).
(defconst window-safe-min-height 1
  "The absolute minimum number of lines of any window.
Anything less might crash Emacs.")

(defun window-safe-min-pixel-height (&optional window)
  "Return the absolute minimum pixel height of WINDOW."
  (* window-safe-min-height
     (frame-char-size (window-normalize-window window))))

(defcustom window-min-height 4
  "The minimum total height, in lines, of any window.
The value has to accommodate one text line, a mode and header
line, a horizontal scroll bar and a bottom divider, if present.
A value less than `window-safe-min-height' is ignored.  The value
of this variable is honored when windows are resized or split.

Applications should never rebind this variable.  To resize a
window to a height less than the one specified here, an
application should instead call `window-resize' with a non-nil
IGNORE argument.  In order to have `split-window' make a window
shorter, explicitly specify the SIZE argument of that function."
  :type 'integer
  :version "24.1"
  :group 'windows)

(defun window-min-pixel-height (&optional window)
  "Return the minimum pixel height of window WINDOW."
  (* (max (if (window-minibuffer-p window) 1 window-min-height)
          window-safe-min-height)
     (frame-char-size window)))

;; This must go to C, finally (or get removed).
(defconst window-safe-min-width 2
  "The absolute minimum number of columns of a window.
Anything less might crash Emacs.")

(defun window-safe-min-pixel-width (&optional window)
  "Return the absolute minimum pixel width of WINDOW."
  (* window-safe-min-width
     (frame-char-size (window-normalize-window window) t)))

(defcustom window-min-width 10
  "The minimum total width, in columns, of any window.
The value has to accommodate two text columns as well as margins,
fringes, a scroll bar and a right divider, if present.  A value
less than `window-safe-min-width' is ignored.  The value of this
variable is honored when windows are resized or split.

Applications should never rebind this variable.  To resize a
window to a width less than the one specified here, an
application should instead call `window-resize' with a non-nil
IGNORE argument.  In order to have `split-window' make a window
narrower, explicitly specify the SIZE argument of that function."
  :type 'integer
  :version "24.1"
  :group 'windows)

(defun window-min-pixel-width (&optional window)
  "Return the minimum pixel width of window WINDOW."
  (* (max window-min-width window-safe-min-width)
     (frame-char-size window t)))

(defun window-safe-min-pixel-size (&optional window horizontal)
  "Return the absolute minimum pixel height of WINDOW.
Optional argument HORIZONTAL non-nil means return the absolute
minimum pixel width of WINDOW."
  (if horizontal
      (window-safe-min-pixel-width window)
    (window-safe-min-pixel-height window)))

(defun window-min-pixel-size (&optional window horizontal)
  "Return the minimum pixel height of WINDOW.
Optional argument HORIZONTAL non-nil means return the minimum
pixel width of WINDOW."
  (if horizontal
      (window-min-pixel-width window)
    (window-min-pixel-height window)))

(defun window-combined-p (&optional window horizontal)
  "Return non-nil if WINDOW has siblings in a given direction.
WINDOW must be a valid window and defaults to the selected one.

HORIZONTAL determines a direction for the window combination.  If
HORIZONTAL is omitted or nil, return non-nil if WINDOW is part of
a vertical window combination.  If HORIZONTAL is non-nil, return
non-nil if WINDOW is part of a horizontal window combination."
  (setq window (window-normalize-window window))
  (let ((parent (window-parent window)))
    (and parent
	 (if horizontal
	     (window-left-child parent)
	   (window-top-child parent)))))

(defun window-combination-p (&optional window horizontal)
  "Return WINDOW's first child if WINDOW is a vertical combination.
WINDOW can be any window and defaults to the selected one.
Optional argument HORIZONTAL non-nil means return WINDOW's first
child if WINDOW is a horizontal combination."
  (setq window (window-normalize-window window))
  (if horizontal
      (window-left-child window)
    (window-top-child window)))

(defun window-combinations (window &optional horizontal ignore-fixed)
  "Return largest number of windows vertically arranged within WINDOW.
WINDOW must be a valid window and defaults to the selected one.
If HORIZONTAL is non-nil, return the largest number of
windows horizontally arranged within WINDOW.

Optional argument IGNORE-FIXED, if non-nil, means to ignore
fixed-size windows in the calculation."
  (setq window (window-normalize-window window))
  (cond
   ((window-live-p window)
    ;; If WINDOW is live, return 1.
    1)
   ((if horizontal
	(window-left-child window)
      (window-top-child window))
    ;; If WINDOW is iso-combined, return the sum of the values for all
    ;; child windows of WINDOW.
    (let ((child (window-child window))
	  (count 0))
      (while child
	(unless (and ignore-fixed (window-size-fixed-p child horizontal))
	  (setq count
		(+ (window-combinations child horizontal ignore-fixed)
		   count)))
	(setq child (window-right child)))
      count))
   (t
    ;; If WINDOW is not iso-combined, return the maximum value of any
    ;; child window of WINDOW.
    (let ((child (window-child window))
	  (count 1))
      (while child
	(unless (and ignore-fixed (window-size-fixed-p child horizontal))
	  (setq count
		(max (window-combinations child horizontal ignore-fixed)
		     count)))
	(setq child (window-right child)))
      count))))

(defun walk-window-tree-1 (fun walk-window-tree-window any &optional sub-only)
  "Helper function for `walk-window-tree' and `walk-window-subtree'."
  (let (walk-window-tree-buffer)
    (while walk-window-tree-window
      (setq walk-window-tree-buffer
	    (window-buffer walk-window-tree-window))
      (when (or walk-window-tree-buffer any)
	(funcall fun walk-window-tree-window))
      (unless walk-window-tree-buffer
	(walk-window-tree-1
	 fun (window-left-child walk-window-tree-window) any)
	(walk-window-tree-1
	 fun (window-top-child walk-window-tree-window) any))
      (if sub-only
	  (setq walk-window-tree-window nil)
	(setq walk-window-tree-window
	      (window-right walk-window-tree-window))))))

(defun walk-window-tree (fun &optional frame any minibuf)
  "Run function FUN on each live window of FRAME.
FUN must be a function with one argument - a window.  FRAME must
be a live frame and defaults to the selected one.  ANY, if
non-nil, means to run FUN on all live and internal windows of
FRAME.

Optional argument MINIBUF t means run FUN on FRAME's minibuffer
window even if it isn't active.  MINIBUF nil or omitted means run
FUN on FRAME's minibuffer window only if it's active.  In either
case the minibuffer window must be part of FRAME.  MINIBUF
neither nil nor t means never run FUN on the minibuffer window.

This function performs a pre-order, depth-first traversal of the
window tree.  If FUN changes the window tree, the result is
unpredictable."
  (let ((root (frame-root-window frame))
        (mini (minibuffer-window frame)))
    (setq frame (window-normalize-frame frame))
    (unless (eq root mini)
      (walk-window-tree-1 fun root any))
    ;; Run FUN on FRAME's minibuffer window if requested.
    (when (and (window-live-p mini)
	       (eq (window-frame mini) frame)
	       (or (eq minibuf t)
		   (and (not minibuf)
                        (minibuffer-window-active-p mini))))
	(funcall fun mini))))

(defun walk-window-subtree (fun &optional window any)
  "Run function FUN on the subtree of windows rooted at WINDOW.
WINDOW defaults to the selected window.  FUN must be a function
with one argument - a window.  By default, run FUN only on live
windows of the subtree.  If the optional argument ANY is non-nil,
run FUN on all live and internal windows of the subtree.  If
WINDOW is live, run FUN on WINDOW only.

This function performs a pre-order, depth-first traversal of the
subtree rooted at WINDOW.  If FUN changes that tree, the result
is unpredictable."
  (setq window (window-normalize-window window))
  (walk-window-tree-1 fun window any t))

(defun window-with-parameter (parameter &optional value frame any minibuf)
  "Return first window on FRAME with PARAMETER non-nil.
FRAME defaults to the selected frame.  Optional argument VALUE
non-nil means only return a window whose `window-parameter' value
for PARAMETER equals VALUE (comparison is done with `equal').
Optional argument ANY non-nil means consider internal windows
too.

Optional argument MINIBUF t means consider FRAME's minibuffer
window even if it isn't active.  MINIBUF nil or omitted means
consider FRAME's minibuffer window only if it's active.  In both
cases the minibuffer window must be part of FRAME.  MINIBUF
neither nil nor t means never consider the minibuffer window."
  (let (this-value)
    (catch 'found
      (walk-window-tree
       (lambda (window)
	 (when (and (setq this-value (window-parameter window parameter))
		    (or (not value) (equal value this-value)))
	   (throw 'found window)))
       frame any minibuf))))

;;; Atomic windows.
(defun window-atom-root (&optional window)
  "Return root of atomic window WINDOW is a part of.
WINDOW must be a valid window and defaults to the selected one.
Return nil if WINDOW is not part of an atomic window."
  (setq window (window-normalize-window window))
  (let (root)
    (while (and window (window-parameter window 'window-atom))
      (setq root window)
      (setq window (window-parent window)))
    root))

(defun window-make-atom (window)
  "Make WINDOW an atomic window.
WINDOW must be an internal window.  Return WINDOW."
  (if (not (window-child window))
      (error "Window %s is not an internal window" window)
    (walk-window-subtree
     (lambda (window)
       (unless (window-parameter window 'window-atom)
	 (set-window-parameter window 'window-atom t)))
     window t)
    window))

(defun display-buffer-in-atom-window (buffer alist)
  "Display BUFFER in an atomic window.
This function displays BUFFER in a new window that will be
combined with an existing window to form an atomic window.  If
the existing window is already part of an atomic window, add the
new window to that atomic window.  Operations like `split-window'
or `delete-window', when applied to a constituent of an atomic
window, are applied atomically to the root of that atomic window.

ALIST is an association list of action symbols and values.  See
Info node `(elisp) Buffer Display Action Alists' for details of
such alists.  The following two symbols have a special meaning:

`window' specifies the existing window the new window shall be
  combined with.  Use `window-atom-root' to make the new window a
  sibling of an atomic window's root.  If an internal window is
  specified here, all children of that window become part of the
  atomic window too.  If no window is specified, the new window
  becomes a sibling of the selected window.  By default, the
  `window-atom' parameter of the existing window is set to `main'
  provided the window is live and the parameter is not set yet.

`side' denotes the side of the existing window where the new
  window shall be located.  Valid values are `below', `right',
  `above' and `left'.  The default is `below'.  By default, the
  `window-atom' parameter of the new window is set to this value.

The return value is the new window, nil when creating that window
failed.

This is an action function for buffer display, see Info
node `(elisp) Buffer Display Action Functions'.  It should be
called only by `display-buffer' or a function directly or
indirectly called by the latter."
  (let* ((ignore-window-parameters t)
	 (window-combination-limit t)
	 (window-combination-resize 'atom)
	 (window (cdr (assq 'window alist)))
	 (side (or (cdr (assq 'side alist)) 'below))
	 (atom (when window (window-parameter window 'window-atom)))
	 root new)
    (setq window (window-normalize-window window))
    (setq root (window-atom-root window))
    ;; Split off new window.
    (when (setq new (split-window-no-error window nil side))
      (window-make-atom
       (if (and root (not (eq root window)))
	   ;; When WINDOW was part of an atomic window and we did not
	   ;; split its root, root atomic window at old root.
	   root
	 ;; Otherwise, root atomic window at WINDOW's new parent.
	 (window-parent window)))
      ;; Assign `window-atom' parameters, if needed.
      (when (and (not atom) (window-live-p window))
	(set-window-parameter window 'window-atom 'main))
      (set-window-parameter new 'window-atom side)
      ;; Display BUFFER in NEW and return NEW.
      (window--display-buffer buffer new 'window alist))))

(defun window--atom-check-1 (window)
  "Subroutine of `window--atom-check'."
  (when window
    (if (window-parameter window 'window-atom)
	(let ((count 0))
	  (when (or (catch 'reset
		      (walk-window-subtree
		       (lambda (window)
			 (if (window-parameter window 'window-atom)
			     (setq count (1+ count))
			   (throw 'reset t)))
		       window t))
		    ;; count >= 1 must hold here.  If there's no other
		    ;; window around dissolve this atomic window.
		    (= count 1))
	    ;; Dissolve atomic window.
	    (walk-window-subtree
	     (lambda (window)
	       (set-window-parameter window 'window-atom nil))
	     window t)))
      ;; Check children.
      (unless (window-buffer window)
	(window--atom-check-1 (window-left-child window))
	(window--atom-check-1 (window-top-child window))))
    ;; Check right sibling
    (window--atom-check-1 (window-right window))))

(defun window--atom-check (&optional frame)
  "Check atomicity of all windows on FRAME.
FRAME defaults to the selected frame.  If an atomic window is
wrongly configured, reset the atomicity of all its windows on
FRAME to nil.  An atomic window is wrongly configured if it has
no child windows or one of its child windows is not atomic."
  (window--atom-check-1 (frame-root-window frame)))

;; Side windows.
(defcustom window-sides-vertical nil
  "If non-nil, left and right side windows occupy full frame height.
If nil, top and bottom side windows occupy full frame width."
  :type 'boolean
  :initialize 'custom-initialize-default
  :set 'window--sides-verticalize
  :group 'windows
  :version "26.1")

(defcustom window-sides-reversed nil
  "Whether top/bottom side windows appear in reverse order.
When this is nil, side windows on the top and bottom of a frame
are always drawn from left to right with increasing slot values.
When this is t, side windows on the top and bottom of a frame are
always drawn from right to left with increasing slot values.

When this is `bidi', the drawing order is like that for the value
t if the value of `bidi-paragraph-direction' is `right-to-left'
in the buffer most recently shown in the window selected within
the main window area of this frame.

The layout of side windows on the left or right of a frame is not
affected by the value of this variable."
  :type
  '(choice (const :tag "Never" nil)
	   (const :tag "Bidi" bidi)
	   (const :tag "Always" t))
  :initialize 'custom-initialize-default
  :set 'window--sides-reverse
  :group 'windows
  :version "26.1")

(defcustom window-sides-slots '(nil nil nil nil)
  "Number of available side window slots on each side of a frame.
The value is a list of four elements specifying the maximum
number of side windows that may be created on the left, top,
right and bottom side of any frame.

If an element is a number, `display-buffer-in-side-window' will
refrain from making a new side window if the number of windows on
that side is equal to or exceeds that number.  Rather, it will
reuse the window whose `window-slot' value is nearest to the slot
specified via its ALIST argument.  If an element is nil, this
means there's no bound on the number of windows on that side."
  :version "24.1"
  :risky t
  :type
  '(list
    :value (nil nil nil nil)
    (choice
     :tag "Left"
     :help-echo "Maximum number of left side windows."
     :value nil
     :format "%[Left%] %v\n"
     (const :tag "Unlimited" :format "%t" nil)
     (integer :tag "Number" :value 2 :size 5))
    (choice
     :tag "Top"
     :help-echo "Maximum number of top side windows."
     :value nil
     :format "%[Top%] %v\n"
     (const :tag "Unlimited" :format "%t" nil)
     (integer :tag "Number" :value 3 :size 5))
    (choice
     :tag "Right"
     :help-echo "Maximum number of right side windows."
     :value nil
     :format "%[Right%] %v\n"
     (const :tag "Unlimited" :format "%t" nil)
     (integer :tag "Number" :value 2 :size 5))
    (choice
     :tag "Bottom"
     :help-echo "Maximum number of bottom side windows."
     :value nil
     :format "%[Bottom%] %v\n"
     (const :tag "Unlimited" :format "%t" nil)
     (integer :tag "Number" :value 3 :size 5)))
  :group 'windows)

(defvar-local window--sides-shown nil
  "Non-nil if this buffer was shown in a side window once.
If this variable is non-nil in a buffer, `switch-to-prev-buffer'
and `switch-to-next-buffer' will refrain from showing this buffer
within the main window area.  `display-buffer-in-side-window'
sets this variable automatically.

Killing buffer local variables after showing the buffer in a side
window annihilates any effect provided by this variable.")

(defvar window--sides-inhibit-check nil
  "Non-nil means inhibit any checks on side windows.")

(defun window--sides-reverse-on-frame-p (frame)
  "Return non-nil when side windows should appear reversed on FRAME.
This uses some heuristics to guess the user's intentions when the
selected window of FRAME is a side window."
  (cond
   ;; Reverse when `window-sides-reversed' is t.  Do not reverse when
   ;; `window-sides-reversed' is nil.
   ((memq window-sides-reversed '(nil t))
    window-sides-reversed)
   ;; Reverse when FRAME's selected window shows a right-to-left buffer.
   ((let ((window (frame-selected-window frame)))
      (when (and (not (window-parameter window 'window-side))
                 (or (not (window-minibuffer-p window))
                     (setq window (minibuffer-selected-window))))
        (with-current-buffer (window-buffer window)
          (eq bidi-paragraph-direction 'right-to-left)))))
   ;; Reverse when FRAME's `window-sides-main-selected-window' parameter
   ;; specifies a live window showing a right-to-left buffer.
   ((let ((window (frame-parameter
                   frame 'window-sides-main-selected-window)))
      (when (window-live-p window)
        (with-current-buffer (window-buffer window)
          (eq bidi-paragraph-direction 'right-to-left)))))
   ;; Reverse when all windows in FRAME's main window show right-to-left
   ;; buffers.
   (t
    (catch 'found
      (walk-window-subtree
       (lambda (window)
         (with-current-buffer (window-buffer window)
           (when (eq bidi-paragraph-direction 'left-to-right)
             (throw 'found nil))))
       (window-main-window frame))
      t))))

(defun window-main-window (&optional frame)
  "Return the main window of specified FRAME.
The optional argument FRAME must be a live frame and defaults to
the selected one.

If FRAME has no side windows, return FRAME's root window.
Otherwise, return either an internal non-side window such that
all other non-side windows on FRAME descend from it, or the
single live non-side window of FRAME."
  (let ((frame (window-normalize-frame frame))
	main sibling)
    ;; Set main to the _last_ window found by `walk-window-tree' that
    ;; is not a side window but has a side window as its sibling.
    (walk-window-tree
     (lambda (window)
       (and (not (window-parameter window 'window-side))
	    (or (and (setq sibling (window-prev-sibling window))
		     (window-parameter sibling 'window-side))
		(and (setq sibling (window-next-sibling window))
		     (window-parameter sibling 'window-side)))
	    (setq main window)))
     frame t 'nomini)
    (or main (frame-root-window frame))))

(defun window--make-major-side-window-next-to (side)
  "Return window to split for making a major side window.
SIDE must be one of the symbols `left', `top', `right' or
`bottom'.

This is an auxiliary function of `window--make-major-side-window'
and must not be called when a window on SIDE exists already."
  (let ((root (frame-root-window))
        (window--sides-inhibit-check t)
        window)
    ;; (1) If a window on the opposite side exists, return that window's
    ;;     sibling.
    ;; (2) If the new window shall span the entire side, return the
    ;;     frame's root window.
    ;; (3) If a window on an orthogonal side exists, return that
    ;;     window's sibling.
    ;; (4) Otherwise return the frame's root window.
    (cond
     ((or (and (eq side 'left)
	       (setq window (window-with-parameter 'window-side 'right nil t)))
	  (and (eq side 'top)
	       (setq window (window-with-parameter 'window-side 'bottom nil t))))
      (window-prev-sibling window))
     ((or (and (eq side 'right)
	       (setq window (window-with-parameter 'window-side 'left nil t)))
	  (and (eq side 'bottom)
	       (setq window (window-with-parameter 'window-side 'top nil t))))
      (window-next-sibling window))
     ((memq side '(left right))
      (cond
       (window-sides-vertical
	root)
       ((setq window (window-with-parameter 'window-side 'top nil t))
	(window-next-sibling window))
       ((setq window (window-with-parameter 'window-side 'bottom nil t))
	(window-prev-sibling window))
       (t root)))
     ((memq side '(top bottom))
      (cond
       ((not window-sides-vertical)
	root)
       ((setq window (window-with-parameter 'window-side 'left nil t))
	(window-next-sibling window))
       ((setq window (window-with-parameter 'window-side 'right nil t))
	(window-prev-sibling window))
       (t root))))))

(defun window--make-major-side-window (buffer side slot &optional alist)
  "Display BUFFER in a new major side window on the selected frame.
SIDE must be one of `left', `top', `right' or `bottom'.  SLOT
specifies the slot to use.  ALIST is an association list of
symbols and values as passed to `display-buffer-in-side-window'.
Return the new window, nil if its creation failed.

This is an auxiliary function of `display-buffer-in-side-window'
and may be called only if no window on SIDE exists yet."
  (let* ((left-or-right (memq side '(left right)))
	 (next-to (window--make-major-side-window-next-to side))
	 (on-side (cond
		   ((eq side 'top) 'above)
		   ((eq side 'bottom) 'below)
		   (t side)))
         (window--sides-inhibit-check t)
	 ;; The following two bindings will tell `split-window' to take
	 ;; the space for the new window from the selected frame's main
	 ;; window and not make a new parent window unless needed.
	 (window-combination-resize 'side)
	 (window-combination-limit nil)
	 (window (split-window-no-error next-to nil on-side))
         (alist (if (assq 'dedicated alist)
                    alist
                  (cons `(dedicated . ,(or display-buffer-mark-dedicated 'side))
                        alist))))
    (when window
      ;; Initialize `window-side' parameter of new window to SIDE and
      ;; make that parameter persistent.
      (set-window-parameter window 'window-side side)
      (add-to-list 'window-persistent-parameters '(window-side . writable))
      ;; Install `window-slot' parameter of new window and make that
      ;; parameter persistent.
      (set-window-parameter window 'window-slot slot)
      (add-to-list 'window-persistent-parameters '(window-slot . writable))
      ;; Auto-adjust height/width of new window unless a size has been
      ;; explicitly requested.
      (unless (if left-or-right
		  (cdr (assq 'window-width alist))
		(cdr (assq 'window-height alist)))
	(setq alist
	      (cons
	       (cons
		(if left-or-right 'window-width 'window-height)
		(/ (window-total-size (frame-root-window) left-or-right)
		   ;; By default use a fourth of the size of the frame's
		   ;; root window.
		   4))
	       alist)))
      (with-current-buffer buffer
        (setq window--sides-shown t))
      ;; Install BUFFER in new window and return WINDOW.
      (window--display-buffer buffer window 'window alist))))

(defun display-buffer-in-side-window (buffer alist)
  "Display BUFFER in a side window of the selected frame.
ALIST is an association list of action symbols and values.  See
Info node `(elisp) Buffer Display Action Alists' for details of
such alists.  The following two symbols, when used in ALIST, have
a special meaning:

 `side' denotes the side of the frame where the new window shall
   be located.  Valid values are `bottom', `right', `top' and
   `left'.  The default is `bottom'.

 `slot' if non-nil, specifies the window slot where to display
   BUFFER.  A value of zero or nil means use the middle slot on
   the specified side.  A negative value means use a slot
   preceding (that is, above or on the left of) the middle slot.
   A positive value means use a slot following (that is, below or
   on the right of) the middle slot.  The default is zero.

If the current frame size or the settings of `window-sides-slots'
do not permit making a new window, a suitable existing window may
be reused and have its `window-slot' parameter value accordingly
modified.

Unless `display-buffer-mark-dedicated' is non-nil, dedicate the
side window used to BUFFER so that it does not get reused by
other `display-buffer' action functions.  Return the window used
for displaying BUFFER, nil if no suitable window can be found.

This function installs the `window-side' and `window-slot'
parameters and makes them persistent.  It neither modifies ALIST
nor installs any other window parameters unless they have been
explicitly provided via a `window-parameters' entry in ALIST.

This is an action function for buffer display, see Info
node `(elisp) Buffer Display Action Functions'.  It should be
called only by `display-buffer' or a function directly or
indirectly called by the latter."
  (let* ((side (or (cdr (assq 'side alist)) 'bottom))
         (slot (or (cdr (assq 'slot alist)) 0))
         (left-or-right (memq side '(left right))))
    (cond
     ((not (memq side '(top bottom left right)))
      (error "Invalid side %s specified" side))
     ((not (numberp slot))
      (error "Invalid slot %s specified" slot)))

    (let* ((major (window-with-parameter 'window-side side nil t))
	   ;; `major' is the major window on SIDE, `windows' the list of
	   ;; life windows on SIDE.
           (reversed (window--sides-reverse-on-frame-p (selected-frame)))
           (windows
	    (cond
             ((window-live-p major)
              (list major))
             ((window-valid-p major)
              (let* ((first (window-child major))
                     (next (window-next-sibling first))
                     (windows (list next first)))
                (setq reversed (> (window-parameter first 'window-slot)
                                  (window-parameter next 'window-slot)))
		(while (setq next (window-next-sibling next))
                  (setq windows (cons next windows)))
		(if reversed windows (nreverse windows))))))
	   (slots (when major (max 1 (window-child-count major))))
	   (max-slots
	    (nth (cond
		  ((eq side 'left) 0)
		  ((eq side 'top) 1)
		  ((eq side 'right) 2)
		  ((eq side 'bottom) 3))
		 window-sides-slots))
           (window--sides-inhibit-check t)
           (alist (if (assq 'dedicated alist)
                      alist
                    (cons `(dedicated . ,(or display-buffer-mark-dedicated 'side))
                          alist)))
           window this-window this-slot prev-window next-window
	   best-window best-slot abs-slot)

      (cond
       ((and (numberp max-slots) (<= max-slots 0))
	;; No side-slots available on this side.  Don't raise an error,
	;; just return nil.
	nil)
       ((not windows)
	;; No major side window exists on this side, make one.
	(window--make-major-side-window buffer side slot alist))
       (t
	;; Scan windows on SIDE.
	(catch 'found
	  (dolist (window windows)
	    (setq this-slot (window-parameter window 'window-slot))
	    (cond
	     ;; The following should not happen and probably be checked
	     ;; by window--sides-check.
	     ((not (numberp this-slot)))
	     ((= this-slot slot)
	      ;; A window with a matching slot has been found.
	      (setq this-window window)
	      (throw 'found t))
	     (t
	      ;; Check if this window has a better slot value wrt the
	      ;; slot of the window we want.
	      (setq abs-slot
		    (if (or (and (> this-slot 0) (> slot 0))
			    (and (< this-slot 0) (< slot 0)))
			(abs (- slot this-slot))
		      (+ (abs slot) (abs this-slot))))
	      (unless (and best-slot (<= best-slot abs-slot))
		(setq best-window window)
		(setq best-slot abs-slot))
	      (if reversed
                  (cond
                   ((<= this-slot slot)
                    (setq next-window window))
                   ((not prev-window)
                    (setq prev-window window)))
                (cond
                 ((<= this-slot slot)
                  (setq prev-window window))
                 ((not next-window)
                  (setq next-window window))))))))

        ;; `this-window' is the first window with the same SLOT.
	;; `prev-window' is the window with the largest slot < SLOT.  A new
	;; window will be created after it.
	;; `next-window' is the window with the smallest slot > SLOT.  A new
	;; window will be created before it.
	;; `best-window' is the window with the smallest absolute difference
	;; of its slot and SLOT.
	(or (and this-window
		 ;; Reuse `this-window'.
                 (with-current-buffer buffer
                   (setq window--sides-shown t))
		 (window--display-buffer buffer this-window 'reuse alist))
	    (and (or (not max-slots) (< slots max-slots))
		 (or (and next-window
			  ;; Make new window before `next-window'.
			  (let ((next-side (if left-or-right 'above 'left))
				(window-combination-resize 'side))
			    (setq window (split-window-no-error
                                          next-window nil next-side))))
		     (and prev-window
			  ;; Make new window after `prev-window'.
			  (let ((prev-side (if left-or-right 'below 'right))
				(window-combination-resize 'side))
			    (setq window (split-window-no-error
                                          prev-window nil prev-side)))))
		   (set-window-parameter window 'window-slot slot)
                   (with-current-buffer buffer
                     (setq window--sides-shown t))
		   (window--display-buffer buffer window 'window alist))
	    (and best-window
		 ;; Reuse `best-window'.
		 (progn
		   ;; Give best-window the new slot value.
		   (set-window-parameter best-window 'window-slot slot)
                   (with-current-buffer buffer
                     (setq window--sides-shown t))
                   (window--display-buffer
                    buffer best-window 'reuse alist)))))))))

(defun window-toggle-side-windows (&optional frame)
  "Toggle display of side windows on specified FRAME.
FRAME must be a live frame and defaults to the selected one.

If FRAME has at least one side window, delete all side
windows on FRAME after saving FRAME's state in the
FRAME's `window-state' frame parameter.  Otherwise,
restore any side windows recorded in FRAME's `window-state'
parameter, leaving FRAME's main window alone.  Signal an
error if FRAME has no side windows and no saved state for
it is found."
  (interactive)
  (let* ((frame (window-normalize-frame frame))
         (window--sides-inhibit-check t)
         state)
    (cond
     ((window-with-parameter 'window-side nil frame)
      ;; At least one side window exists.  Remove all side windows after
      ;; saving FRAME's state in its `window-state' parameter.
      (set-frame-parameter
       frame 'window-state (window-state-get (frame-root-window frame)))
      (let ((ignore-window-parameters t))
        (delete-other-windows (window-main-window frame))))
     ((setq state (frame-parameter frame 'window-state))
      ;; A window state was saved for FRAME.  Restore it and put the
      ;; current root window into its main window.
      (let ((window-combination-resize t)
            (main-state (window-state-get (frame-root-window frame))))
        (window-state-put state (frame-root-window frame) t)
        (window-state-put main-state (window-main-window frame)))
      (window--sides-reverse-frame frame))
     (t
      (error "No side windows state found")))))

(defun window--sides-reverse-all ()
  "Maybe reverse side windows on all frames."
  (unless window--sides-inhibit-check
    (dolist (frame (frame-list))
      (window--sides-reverse-frame frame))))

(defun window--sides-reverse-frame (frame)
  "Maybe reverse side windows on FRAME."
  (when (eq window-sides-reversed 'bidi)
    (let ((window (frame-selected-window frame)))
      (unless (or (window-parameter window 'window-side)
                  (window-minibuffer-p window))
        (set-frame-parameter
         frame 'window-sides-main-selected-window window))))
  (window--sides-reverse-side frame 'top)
  (window--sides-reverse-side frame 'bottom))

(defun window--sides-reverse-side (frame side)
  "Maybe reverse windows on SIDE of FRAME."
  (let ((major (window-with-parameter 'window-side side frame t))
        (window--sides-inhibit-check t))
    (when (and major (not (window-live-p major)))
      (let* ((first (window-child major))
             (reversed (> (window-parameter first 'window-slot)
                          (window-parameter
                           (window-next-sibling first) 'window-slot)))
             (reverse (window--sides-reverse-on-frame-p frame)))
        (unless (eq reversed reverse)
          ;; We have to reverse.
          (let ((last (window-last-child major)))
            (while (and (not (eq first last))
                        (not (eq first (window-next-sibling last))))
              (window-swap-states first last t)
              (setq first (window-next-sibling first))
              (setq last (window-prev-sibling last)))))))))

(defun window--sides-reverse (symbol value)
  "Helper function for customizing `window-sides-reversed'."
  (set-default symbol value)
  (remove-hook 'buffer-list-update-hook 'window--sides-reverse-all)
  (remove-hook 'window-configuration-change-hook 'window--sides-reverse-all)
  (dolist (frame (frame-list))
    (set-frame-parameter frame 'window-sides-main-selected-window nil))
  (when (eq value 'bidi)
    (add-hook 'buffer-list-update-hook 'window--sides-reverse-all)
    (add-hook 'window-configuration-change-hook 'window--sides-reverse-all))
  (window--sides-reverse-all))

(defun window--sides-verticalize-frame (&optional frame)
  "Maybe change side windows layout on specified FRAME."
  (setq frame (window-normalize-frame frame))
  (let ((window--sides-inhibit-check t)
        (root (frame-root-window frame))
	(main (window-main-window frame)))
    (when (and (not (eq main root))
	       (not (eq (window-parent main) root))
	       (window-combined-p main window-sides-vertical))
      (let* ((window--sides-inhibit-check t)
	     (ignore-window-parameters t)
	     (first (window-child root))
	     (first-state
	      (and first (window-parameter first 'window-side)
		   (window-state-get first)))
	     (last (window-last-child root))
	     (last-state
	      (and last (window-parameter last 'window-side)
		   (window-state-get last)))
	     (dummy (get-buffer-create " *dummy*"))
	     major)
	(unwind-protect
	    (progn
	      (when first-state (delete-window first))
	      (when last-state (delete-window last))
	      (when first-state
		(setq major (window--make-major-side-window
			     dummy (if window-sides-vertical 'top 'left) 0))
		(window-state-put first-state major t))
	      (when last-state
		(setq major (window--make-major-side-window
			     dummy (if window-sides-vertical 'bottom 'right) 0))
		(window-state-put last-state major t)))
	  (kill-buffer " *dummy*"))))))

(defun window--sides-verticalize (symbol value)
  "Helper function for customizing `window-sides-vertical'."
  (set-default symbol value)
  (dolist (frame (frame-list))
    (window--sides-verticalize-frame frame)))

(defun window--sides-check-failed (frame)
  "Helper function for `window--sides-check'."
  (catch 'failed
    ;; FRAME must have a main window.
    (unless (window-main-window frame)
      (error "Frame %s has no main window" frame)
      (throw 'failed t))
    ;; Now check the side windows.
    (dolist (side '(left top right bottom))
      (let ((window (window-with-parameter 'window-side side frame t)))
        (when window
          ;; If WINDOW is live there must be no other window on this frame
          ;; with the same `window-side' parameter.
          (if (window-live-p window)
              (walk-window-tree
               (lambda (this)
                 (when (and (eq (window-parameter this 'window-side) side)
                            (not (eq this window)))
                   (error "Window %s has same side %s as window %s but no common parent"
                          this side window)
                   (throw 'failed t)))
               frame t 'nomini)
            (walk-window-tree
             (lambda (this)
               (if (eq (window-parent this) window)
                   (unless (eq (window-parameter this 'window-side) side)
                     (error "Window %s has not same side %s as its parent %s"
                            this side window)
                     (throw 'failed t))
                 (when (and (eq (window-parameter this 'window-side) side)
                            (not (eq this window)))
                   (error "Window %s has same side %s as major side window %s but its parent is %s"
                          this side window (window-parent this))
                   (throw 'failed t))))
             frame t 'nomini)))))))

(defun window--sides-check (frame)
  "Check side windows configuration of FRAME.
In a valid side windows configuration there can be at most one
internal side window on each side and all its children must be
live and have the same `window-side' parameter and no other
window with the same `window-side' parameter exists on FRAME.  If
there is no such internal window, there may be at most one window
with this side's `window-side' parameter on FRAME.

If the configuration is invalid, reset the `window-side'
parameters of all windows on FRAME."
  (when (and (not window--sides-inhibit-check)
             (window-with-parameter 'window-side nil frame t)
             (window--sides-check-failed frame))
    ;; Reset all `window-side' parameters.
    (walk-window-tree
     (lambda (window)
       (set-window-parameter window 'window-side nil))
     frame t 'nomini)
    (message "Side windows configuration reset for frame %s" frame)))

(defun window--check (&optional frame)
  "Check atomic and side windows on FRAME.
FRAME defaults to the selected frame."
  (window--sides-check frame)
  (window--atom-check frame))

;; Dumping frame/window contents.
(defun window--dump-window (&optional window erase)
  "Dump WINDOW to buffer *window-frame-dump*.
WINDOW must be a valid window and defaults to the selected one.
Optional argument ERASE non-nil means erase *window-frame-dump*
before writing to it."
  (setq window (window-normalize-window window))
  (with-current-buffer (get-buffer-create "*window-frame-dump*")
    (when erase (erase-buffer))
    (insert
     (format "%s   parent: %s\n" window (window-parent window))
     (format "pixel left: %s   top: %s   size: %s x %s   new: %s\n"
	     (window-pixel-left window) (window-pixel-top window)
	     (window-size window t t) (window-size window nil t)
	     (window-new-pixel window))
     (format "char left: %s   top: %s   size: %s x %s   new: %s\n"
	     (window-left-column window) (window-top-line window)
	     (window-total-size window t) (window-total-size window)
	     (window-new-total window))
     (format "normal: %s x %s   new: %s\n"
	     (window-normal-size window t) (window-normal-size window)
	     (window-new-normal window)))
    (when (window-live-p window)
      (let ((fringes (window-fringes window))
	    (margins (window-margins window)))
	(insert
	 (format "body pixel: %s x %s   char: %s x %s\n"
		 (window-body-width window t) (window-body-height window t)
		 (window-body-width window) (window-body-height window))
	 (format "width left fringe: %s  left margin: %s  right margin: %s\n"
		 (car fringes) (or (car margins) 0) (or (cdr margins) 0))
	 (format "width right fringe: %s  scroll-bar: %s  divider: %s\n"
		 (cadr fringes)
		 (window-scroll-bar-width window)
		 (window-right-divider-width window))
	 (format "height tab-line: %s header-line: %s  mode-line: %s\n"
		 (window-tab-line-height window)
		 (window-header-line-height window)
		 (window-mode-line-height window))
	 (format "height scroll-bar: %s divider: %s"
		 (window-scroll-bar-height window)
		 (window-bottom-divider-width window)))))
    (insert "\n")))

(defun window--dump-frame (&optional window-or-frame)
  "Dump WINDOW-OR-FRAME to buffer *window-frame-dump*.
WINDOW-OR-FRAME can be a frame or a window and defaults to the
selected frame.  When WINDOW-OR-FRAME is a window, dump that
window's frame.  The buffer *window-frame-dump* is erased before
dumping to it."
  (let* ((window
	  (cond
	   ((or (not window-or-frame)
		(frame-live-p window-or-frame))
	    (frame-root-window window-or-frame))
	   ((or (window-live-p window-or-frame)
		(window-child window-or-frame))
	    window-or-frame)
	   (t
	    (frame-root-window))))
	 (frame (window-frame window)))
    (with-current-buffer (get-buffer-create "*window-frame-dump*")
      (erase-buffer)
      (insert
       (format "frame pixel: %s x %s   cols/lines: %s x %s   units: %s x %s\n"
	       (frame-pixel-width frame) (frame-pixel-height frame)
	       (frame-total-cols frame) (frame-total-lines frame)
	       (frame-char-width frame) (frame-char-height frame))
       (format "frame text pixel: %s x %s   cols/lines: %s x %s\n"
	       (frame-text-width frame) (frame-text-height frame)
	       (frame-text-cols frame) (frame-text-lines frame))
       (format "tab: %s  tool: %s  scroll: %s/%s  fringe: %s  border: %s  right: %s  bottom: %s\n\n"
	       (if (fboundp 'tab-bar-height)
		   (tab-bar-height frame t)
		 "0")
	       (if (fboundp 'tool-bar-height)
		   (tool-bar-height frame t)
		 "0")
	       (frame-scroll-bar-width frame)
	       (frame-scroll-bar-height frame)
	       (frame-fringe-width frame)
	       (frame-border-width frame)
	       (frame-right-divider-width frame)
	       (frame-bottom-divider-width frame)))
      (walk-window-tree 'window--dump-window frame t t))))

;;; Window sizes.
(defun window-total-size (&optional window horizontal round)
  "Return the total height or width of WINDOW.
WINDOW must be a valid window and defaults to the selected one.

If HORIZONTAL is omitted or nil, return the total height of
WINDOW, in lines.  If WINDOW is live, its total height includes,
in addition to the height of WINDOW's text, the heights of
WINDOW's mode and header line and a bottom divider, if any.

If HORIZONTAL is non-nil, return the total width of WINDOW, in
columns.  If WINDOW is live, its total width includes, in
addition to the width of WINDOW's text, the widths of WINDOW's
fringes, margins, scroll bars and its right divider, if any.

If WINDOW is internal, return the respective size of the screen
areas spanned by its children.

Optional argument ROUND is handled as for `window-total-height'
and `window-total-width'."
  (if horizontal
      (window-total-width window round)
    (window-total-height window round)))

(defun window-size (&optional window horizontal pixelwise round)
  "Return the height or width of WINDOW.
WINDOW must be a valid window and defaults to the selected one.

If HORIZONTAL is omitted or nil, return the total height of
WINDOW, in lines, like `window-total-height'.  Otherwise return
the total width, in columns, like `window-total-width'.

Optional argument PIXELWISE means return the pixel size of WINDOW
like `window-pixel-height' and `window-pixel-width'.

Optional argument ROUND is ignored if PIXELWISE is non-nil and
handled as for `window-total-height' and `window-total-width'
otherwise."
  (if horizontal
      (if pixelwise
	  (window-pixel-width window)
	(window-total-width window round))
    (if pixelwise
	(window-pixel-height window)
      (window-total-height window round))))

(defvar-local window-size-fixed nil
  "Non-nil in a buffer means windows displaying the buffer are fixed-size.
If the value is `height', then only the window's height is fixed.
If the value is `width', then only the window's width is fixed.
Any other non-nil value fixes both the width and the height.

Emacs won't change the size of any window displaying that buffer,
unless it has no other choice (like when deleting a neighboring
window).")

(defun window--preservable-size (window &optional horizontal)
  "Return height of WINDOW as `window-preserve-size' would preserve it.
Optional argument HORIZONTAL non-nil means to return the width of
WINDOW as `window-preserve-size' would preserve it."
  (if horizontal
      (window-body-width window t)
    (+ (window-body-height window t)
       (window-header-line-height window)
       (window-mode-line-height window))))

(defun window-preserve-size (&optional window horizontal preserve)
  "Preserve height of window WINDOW.
WINDOW must be a live window and defaults to the selected one.
Optional argument HORIZONTAL non-nil means preserve the width of
WINDOW.

PRESERVE t means to preserve the current height/width of WINDOW's
body in frame and window resizing operations whenever possible.
The height/width of WINDOW will change only if Emacs has no other
choice.  Resizing a window whose height/width is preserved never
throws an error.

PRESERVE nil means to stop preserving the height/width of WINDOW,
lifting the respective restraint induced by a previous call of
`window-preserve-size' for WINDOW.  Calling `enlarge-window',
`shrink-window', `split-window' or `fit-window-to-buffer' with
WINDOW as argument also removes the respective restraint.

Other values of PRESERVE are reserved for future use."
  (setq window (window-normalize-window window t))
  (let* ((parameter (window-parameter window 'window-preserved-size))
	 (width (nth 1 parameter))
	 (height (nth 2 parameter)))
    (if horizontal
	(set-window-parameter
	 window 'window-preserved-size
	 (list
	  (window-buffer window)
	  (and preserve (window--preservable-size window t))
	  height))
      (set-window-parameter
       window 'window-preserved-size
       (list
	(window-buffer window)
	width
	(and preserve (window--preservable-size window)))))))

(defun window-preserved-size (&optional window horizontal)
  "Return preserved height of window WINDOW.
WINDOW must be a live window and defaults to the selected one.
Optional argument HORIZONTAL non-nil means to return preserved
width of WINDOW."
  (setq window (window-normalize-window window t))
  (let* ((parameter (window-parameter window 'window-preserved-size))
	 (buffer (nth 0 parameter))
	 (width (nth 1 parameter))
	 (height (nth 2 parameter)))
    (when (eq buffer (window-buffer window))
      (if horizontal width height))))

(defun window--preserve-size (window horizontal)
  "Return non-nil when the height of WINDOW shall be preserved.
Optional argument HORIZONTAL non-nil means to return non-nil when
the width of WINDOW shall be preserved."
  (let ((size (window-preserved-size window horizontal)))
    (and (numberp size)
	 (= size (window--preservable-size window horizontal)))))

(defun window-safe-min-size (&optional window horizontal pixelwise)
  "Return safe minimum size of WINDOW.
WINDOW must be a valid window and defaults to the selected one.
Optional argument HORIZONTAL non-nil means return the minimum
number of columns of WINDOW; otherwise return the minimum number
of WINDOW's lines.

Optional argument PIXELWISE non-nil means return the minimum pixel-size
of WINDOW."
  (setq window (window-normalize-window window))
  (if pixelwise
      (if horizontal
	  (* window-safe-min-width
	     (frame-char-width (window-frame window)))
	(* window-safe-min-height
	   (frame-char-height (window-frame window))))
    (if horizontal window-safe-min-width window-safe-min-height)))

(defun window-min-size (&optional window horizontal ignore pixelwise)
  "Return the minimum size of WINDOW.
WINDOW must be a valid window and defaults to the selected one.
Optional argument HORIZONTAL non-nil means return the minimum
number of columns of WINDOW; otherwise return the minimum number
of WINDOW's lines.

The optional argument IGNORE has the same meaning as for
`window-resizable'.  Optional argument PIXELWISE non-nil means
return the minimum pixel-size of WINDOW."
  (window--min-size-1
   (window-normalize-window window) horizontal ignore pixelwise))

(defun window--min-size-ignore-p (window ignore)
  "Return non-nil if IGNORE says to ignore size restrictions for WINDOW."
  (if (window-valid-p ignore)
      (eq window ignore)
    (not (memq ignore '(nil preserved)))))

(defun window--min-size-1 (window horizontal ignore pixelwise)
  "Internal function of `window-min-size'."
  (let ((sub (window-child window)))
    (if sub
	(let ((value 0))
	  ;; WINDOW is an internal window.
	  (if (window-combined-p sub horizontal)
	      ;; The minimum size of an iso-combination is the sum of
	      ;; the minimum sizes of its child windows.
	      (while sub
		(setq value (+ value
			       (window--min-size-1
				sub horizontal ignore pixelwise)))
		(setq sub (window-right sub)))
	    ;; The minimum size of an ortho-combination is the maximum
	    ;; of the minimum sizes of its child windows.
	    (while sub
	      (setq value (max value
			       (window--min-size-1
				sub horizontal ignore pixelwise)))
	      (setq sub (window-right sub))))
	  value)
      (with-current-buffer (window-buffer window)
	(cond
	 ((window-size-fixed-p window horizontal ignore)
	  ;; The minimum size of a fixed size window is its size.
	  (window-size window horizontal pixelwise))
	 ((eq ignore 'safe)
	  ;; If IGNORE equals `safe' return the safe value.
	  (window-safe-min-size window horizontal pixelwise))
	 (horizontal
	  ;; For the minimum width of a window take fringes and
	  ;; scroll-bars into account.  This is questionable and should
	  ;; be removed as soon as we are able to split (and resize)
	  ;; windows such that the new (or resized) windows can get a
	  ;; size less than the user-specified `window-min-height' and
	  ;; `window-min-width'.
	  (let* ((char-size (frame-char-size window t))
		 (fringes (window-fringes window))
		 (margins (window-margins window))
                 ;; Let the 'min-margins' parameter override the actual
                 ;; widths of the margins.  We allow any number to
                 ;; replace the values specified by `window-margins'.
                 ;; See bug#24193 for the rationale of this parameter.
                 (min-margins (window-parameter window 'min-margins))
                 (left-min-margin (and min-margins
                                       (numberp (car min-margins))
                                       (car min-margins)))
                 (right-min-margin (and min-margins
                                        (numberp (cdr min-margins))
                                        (cdr min-margins)))
		 (pixel-width
		  (+ (window-safe-min-size window t t)
		     (* (or left-min-margin (car margins) 0) char-size)
		     (* (or right-min-margin(cdr margins) 0) char-size)
		     (car fringes) (cadr fringes)
		     (window-scroll-bar-width window)
		     (window-right-divider-width window))))
	    (if pixelwise
		(max
		 (if window-resize-pixelwise
		     pixel-width
		   ;; Round up to next integral of columns.
		   (* (ceiling pixel-width char-size) char-size))
		 (if (window--min-size-ignore-p window ignore)
		     0
		   (window-min-pixel-width window)))
	      (max
	       (ceiling pixel-width char-size)
	       (if (window--min-size-ignore-p window ignore)
		   0
		 window-min-width)))))
	 ((let ((char-size (frame-char-size window))
		(pixel-height
		 (+ (window-safe-min-size window nil t)
		    (window-tab-line-height window)
		    (window-header-line-height window)
		    (window-scroll-bar-height window)
		    (window-mode-line-height window)
		    (window-bottom-divider-width window))))
	    (if pixelwise
		(max
		 (if window-resize-pixelwise
		     pixel-height
		   ;; Round up to next integral of lines.
		   (* (ceiling pixel-height char-size) char-size))
		 (if (window--min-size-ignore-p window ignore)
		     0
		   (window-min-pixel-height window)))
	      (max (ceiling pixel-height char-size)
		   (if (window--min-size-ignore-p window ignore)
		       0
		     window-min-height))))))))))

(defun window-sizable (window delta &optional horizontal ignore pixelwise)
  "Return DELTA if DELTA lines can be added to WINDOW.
WINDOW must be a valid window and defaults to the selected one.
Optional argument HORIZONTAL non-nil means return DELTA if DELTA
columns can be added to WINDOW.  A return value of zero means
that no lines (or columns) can be added to WINDOW.

This function looks only at WINDOW and, recursively, its child
windows.  The function `window-resizable' looks at other windows
as well.

DELTA positive means WINDOW shall be enlarged by DELTA lines or
columns.  If WINDOW cannot be enlarged by DELTA lines or columns
return the maximum value in the range 0..DELTA by which WINDOW
can be enlarged.

DELTA negative means WINDOW shall be shrunk by -DELTA lines or
columns.  If WINDOW cannot be shrunk by -DELTA lines or columns,
return the minimum value in the range DELTA..0 by which WINDOW
can be shrunk.

The optional argument IGNORE has the same meaning as for
`window-resizable'.  Optional argument PIXELWISE non-nil means
interpret DELTA as pixels."
  (setq window (window-normalize-window window))
  (cond
   ((< delta 0)
    (let ((min-size (window-min-size window horizontal ignore pixelwise))
          (size (window-size window horizontal pixelwise)))
      (if (<= size min-size)
          0
        (max (- min-size size) delta))))
   ((> delta 0)
    (if (window-size-fixed-p window horizontal ignore)
	0
      delta))
   (t 0)))

(defun window-sizable-p (window delta &optional horizontal ignore pixelwise)
  "Return t if WINDOW can be resized by DELTA lines.
WINDOW must be a valid window and defaults to the selected one.
For the meaning of the arguments of this function see the
doc-string of `window-sizable'."
  (setq window (window-normalize-window window))
  (if (> delta 0)
      (>= (window-sizable window delta horizontal ignore pixelwise)
	  delta)
    (<= (window-sizable window delta horizontal ignore pixelwise)
	delta)))

(defun window--size-fixed-1 (window horizontal ignore)
  "Internal function for `window-size-fixed-p'."
  (let ((sub (window-child window)))
    (catch 'fixed
      (if sub
	  ;; WINDOW is an internal window.
	  (if (window-combined-p sub horizontal)
	      ;; An iso-combination is fixed size if all its child
	      ;; windows are fixed-size.
	      (progn
		(while sub
		  (unless (window--size-fixed-1 sub horizontal ignore)
		    ;; We found a non-fixed-size child window, so
		    ;; WINDOW's size is not fixed.
		    (throw 'fixed nil))
		  (setq sub (window-right sub)))
		;; All child windows are fixed-size, so WINDOW's size is
		;; fixed.
		(throw 'fixed t))
	    ;; An ortho-combination is fixed-size if at least one of its
	    ;; child windows is fixed-size.
	    (while sub
	      (when (window--size-fixed-1 sub horizontal ignore)
		;; We found a fixed-size child window, so WINDOW's size
		;; is fixed.
		(throw 'fixed t))
	      (setq sub (window-right sub))))
	;; WINDOW is a live window.
	(and (or (not (windowp ignore)) (not (eq window ignore)))
	     (or (and (not (eq ignore 'preserved))
		      (window--preserve-size window horizontal))
		 (with-current-buffer (window-buffer window)
		   (if horizontal
		       (memq window-size-fixed '(width t))
		     (memq window-size-fixed '(height t))))))))))

(defun window-size-fixed-p (&optional window horizontal ignore)
  "Return non-nil if WINDOW's height is fixed.
WINDOW must be a valid window and defaults to the selected one.
Optional argument HORIZONTAL non-nil means return non-nil if
WINDOW's width is fixed.  The optional argument IGNORE has the
same meaning as for `window-resizable'.

If this function returns nil, this does not necessarily mean that
WINDOW can be resized in the desired direction.  The function
`window-resizable' can tell that."
  (when (or (windowp ignore) (memq ignore '(nil preserved)))
    (window--size-fixed-1
     (window-normalize-window window) horizontal ignore)))

(defun window--min-delta-1 (window delta &optional horizontal ignore trail noup pixelwise)
  "Internal function for `window-min-delta'."
  (if (not (window-parent window))
      ;; If we can't go up, return zero.
      0
    ;; Else try to find a non-fixed-size sibling of WINDOW.
    (let* ((parent (window-parent window))
	   (sub (window-child parent)))
      (catch 'done
	(if (window-combined-p sub horizontal)
	    ;; In an iso-combination throw DELTA if we find at least one
	    ;; child window and that window is either not fixed-size or
	    ;; we can ignore fixed-sizeness.
	    (let ((skip (eq trail 'after)))
	      (while sub
		(cond
		 ((eq sub window)
		  (setq skip (eq trail 'before)))
		 (skip)
		 ((window-size-fixed-p sub horizontal ignore))
		 (t
		  ;; We found a non-fixed-size child window.
		  (throw 'done delta)))
		(setq sub (window-right sub))))
	  ;; In an ortho-combination set DELTA to the minimum value by
	  ;; which other child windows can shrink.
	  (while sub
	    (unless (eq sub window)
	      (setq delta
		    (min delta
			 (max (- (window-size sub horizontal pixelwise 'ceiling)
				 (window-min-size
				  sub horizontal ignore pixelwise))
			      0))))
	    (setq sub (window-right sub))))
	(if noup
	    delta
	  (window--min-delta-1
	   parent delta horizontal ignore trail nil pixelwise))))))

(defun window-min-delta (&optional window horizontal ignore trail noup nodown pixelwise)
  "Return number of lines by which WINDOW can be shrunk.
WINDOW must be a valid window and defaults to the selected one.
Return zero if WINDOW cannot be shrunk.

Optional argument HORIZONTAL non-nil means return number of
columns by which WINDOW can be shrunk.

The optional argument IGNORE has the same meaning as for
`window-resizable'.  Optional argument TRAIL restricts the
windows that can be enlarged.  If its value is `before', only
windows to the left of or above WINDOW can be enlarged.  If it is
`after', only windows to the right of or below WINDOW can be
enlarged.

Optional argument NOUP non-nil means don't go up in the window
tree, but try to enlarge windows within WINDOW's combination
only.  Optional argument NODOWN non-nil means don't check whether
WINDOW itself (and its child windows) can be shrunk; check only
whether at least one other window can be enlarged appropriately.

Optional argument PIXELWISE non-nil means return number of pixels
by which WINDOW can be shrunk."
  (setq window (window-normalize-window window))
  (let ((size (window-size window horizontal pixelwise 'floor))
	(minimum (window-min-size window horizontal ignore pixelwise)))
    (cond
     (nodown
      ;; If NODOWN is t, try to recover the entire size of WINDOW.
      (window--min-delta-1
       window size horizontal ignore trail noup pixelwise))
     ((<= size minimum)
      ;; If NODOWN is nil and WINDOW's size is already at its minimum,
      ;; there's nothing to recover.
      0)
     (t
      ;; Otherwise, try to recover whatever WINDOW is larger than its
      ;; minimum size.
      (window--min-delta-1
       window (- size minimum) horizontal ignore trail noup pixelwise)))))

(defun frame-windows-min-size (&optional frame horizontal ignore pixelwise)
  "Return minimum number of lines of FRAME's windows.
HORIZONTAL non-nil means return number of columns of FRAME's
windows.  The optional argument IGNORE has the same meaning as
for `window-resizable'.  PIXELWISE non-nil means return sizes in
pixels."
  (setq frame (window-normalize-frame frame))
  (let* ((root (frame-root-window frame))
	 (mini (window-next-sibling root)))
    (+ (window-min-size root horizontal ignore pixelwise)
       (if (and mini (not horizontal))
	   (window-min-size mini horizontal nil pixelwise)
	 0))))

(defun window--max-delta-1 (window delta &optional horizontal ignore trail noup pixelwise)
  "Internal function of `window-max-delta'."
  (if (not (window-parent window))
      ;; Can't go up.  Return DELTA.
      delta
    (let* ((parent (window-parent window))
	   (sub (window-child parent)))
      (catch 'fixed
	(if (window-combined-p sub horizontal)
	    ;; For an iso-combination calculate how much we can get from
	    ;; other child windows.
	    (let ((skip (eq trail 'after)))
	      (while sub
		(cond
		 ((eq sub window)
		  (setq skip (eq trail 'before)))
		 (skip)
		 (t
		  (setq delta
			(+ delta
			   (max
			    (- (window-size sub horizontal pixelwise 'floor)
			       (window-min-size
				sub horizontal ignore pixelwise))
			    0)))))
		(setq sub (window-right sub))))
	  ;; For an ortho-combination throw DELTA when at least one
	  ;; child window is fixed-size.
	  (while sub
	    (when (and (not (eq sub window))
		       (window-size-fixed-p sub horizontal ignore))
	      (throw 'fixed delta))
	    (setq sub (window-right sub))))
	(if noup
	    ;; When NOUP is nil, DELTA is all we can get.
	    delta
	  ;; Else try with parent of WINDOW, passing the DELTA we
	  ;; recovered so far.
	  (window--max-delta-1
	   parent delta horizontal ignore trail nil pixelwise))))))

(defun window-max-delta (&optional window horizontal ignore trail noup nodown pixelwise)
  "Return maximum number of lines by which WINDOW can be enlarged.
WINDOW must be a valid window and defaults to the selected one.
The return value is zero if WINDOW cannot be enlarged.

Optional argument HORIZONTAL non-nil means return maximum number
of columns by which WINDOW can be enlarged.

The optional argument IGNORE has the same meaning as for
`window-resizable'.  Optional argument TRAIL restricts the
windows that can be enlarged.  If its value is `before', only
windows to the left of or above WINDOW can be enlarged.  If it is
`after', only windows to the right of or below WINDOW can be
enlarged.

Optional argument NOUP non-nil means don't go up in the window
tree but try to obtain the entire space from windows within
WINDOW's combination.  Optional argument NODOWN non-nil means do
not check whether WINDOW itself (and its child windows) can be
enlarged; check only whether other windows can be shrunk
appropriately.

Optional argument PIXELWISE non-nil means return number of
pixels by which WINDOW can be enlarged."
  (setq window (window-normalize-window window))
  (if (and (not nodown) (window-size-fixed-p window horizontal ignore))
      ;; With IGNORE and NOWDON nil return zero if WINDOW has fixed
      ;; size.
      0
    ;; WINDOW has no fixed size.
    (window--max-delta-1 window 0 horizontal ignore trail noup pixelwise)))

;; Make NOUP also inhibit the min-size check.
(defun window--resizable (window delta &optional horizontal ignore trail noup nodown pixelwise)
  "Return DELTA if WINDOW can be resized vertically by DELTA lines.
WINDOW must be a valid window and defaults to the selected one.
Optional argument HORIZONTAL non-nil means return DELTA if WINDOW
can be resized horizontally by DELTA columns.  A return value of
zero means that WINDOW is not resizable.

DELTA positive means WINDOW shall be enlarged by DELTA lines or
columns.  If WINDOW cannot be enlarged by DELTA lines or columns,
return the maximum value in the range 0..DELTA by which WINDOW
can be enlarged.

DELTA negative means WINDOW shall be shrunk by -DELTA lines or
columns.  If WINDOW cannot be shrunk by -DELTA lines or columns,
return the minimum value in the range DELTA..0 that can be used
for shrinking WINDOW.

The optional argument IGNORE has the same meaning as for
`window-resizable'.  Optional argument TRAIL `before' means only
windows to the left of or below WINDOW can be shrunk.  Optional
argument TRAIL `after' means only windows to the right of or
above WINDOW can be shrunk.

Optional argument NOUP non-nil means don't go up in the window
tree but check only whether space can be obtained from (or given
to) WINDOW's siblings.  Optional argument NODOWN non-nil means
don't go down in the window tree.  This means do not check
whether resizing would violate size restrictions of WINDOW or its
child windows.

Optional argument PIXELWISE non-nil means interpret DELTA as
number of pixels."
  (setq window (window-normalize-window window))
  (cond
   ((< delta 0)
    (max (- (window-min-delta
	     window horizontal ignore trail noup nodown pixelwise))
	 delta))
   ((> delta 0)
    (min (window-max-delta
	  window horizontal ignore trail noup nodown pixelwise)
	 delta))
   (t 0)))

(defun window--resizable-p (window delta &optional horizontal ignore trail noup nodown pixelwise)
  "Return t if WINDOW can be resized vertically by DELTA lines.
WINDOW must be a valid window and defaults to the selected one.
For the meaning of the arguments of this function see the
doc-string of `window--resizable'.

Optional argument PIXELWISE non-nil means interpret DELTA as
pixels."
  (setq window (window-normalize-window window))
  (if (> delta 0)
      (>= (window--resizable
	   window delta horizontal ignore trail noup nodown pixelwise)
	  delta)
    (<= (window--resizable
	 window delta horizontal ignore trail noup nodown pixelwise)
	delta)))

(defun window-resizable (window delta &optional horizontal ignore pixelwise)
  "Return DELTA if WINDOW can be resized vertically by DELTA lines.
WINDOW must be a valid window and defaults to the selected one.
Optional argument HORIZONTAL non-nil means return DELTA if WINDOW
can be resized horizontally by DELTA columns.  A return value of
zero means that WINDOW is not resizable.

DELTA positive means WINDOW shall be enlarged by DELTA lines or
columns.  If WINDOW cannot be enlarged by DELTA lines or columns
return the maximum value in the range 0..DELTA by which WINDOW
can be enlarged.

DELTA negative means WINDOW shall be shrunk by -DELTA lines or
columns.  If WINDOW cannot be shrunk by -DELTA lines or columns,
return the minimum value in the range DELTA..0 that can be used
for shrinking WINDOW.

Optional argument IGNORE, if non-nil, means to ignore restraints
induced by fixed size windows or the values of the variables
`window-min-height' and `window-min-width'.  The following values
have special meanings: `safe' means that in addition live windows
are allowed to get as small as `window-safe-min-height' lines and
`window-safe-min-width' columns.  `preserved' means to ignore
only restrictions induced by `window-preserve-size'.  If IGNORE
is a window, then ignore restrictions for that window only.

Optional argument PIXELWISE non-nil means interpret DELTA as
pixels."
  (setq window (window-normalize-window window))
  (window--resizable window delta horizontal ignore nil nil nil pixelwise))

(defun window-resizable-p (window delta &optional horizontal ignore pixelwise)
  "Return t if WINDOW can be resized vertically by DELTA lines.
WINDOW must be a valid window and defaults to the selected one.
For the meaning of the arguments of this function see the
doc-string of `window-resizable'."
  (setq window (window-normalize-window window))
  (if (> delta 0)
      (>= (window--resizable
	   window delta horizontal ignore nil nil nil pixelwise)
	  delta)
    (<= (window--resizable
	 window delta horizontal ignore nil nil nil pixelwise)
	delta)))

;; Aliases of functions defined in window.c.
(defalias 'window-height 'window-total-height)
(defalias 'window-width 'window-body-width)
(defalias 'window-pixel-width-before-size-change 'window-old-pixel-width)
(defalias 'window-pixel-height-before-size-change 'window-old-pixel-height)

(defun window-full-height-p (&optional window)
  "Return t if WINDOW is as high as its containing frame.
More precisely, return t if and only if the total height of
WINDOW equals the total height of the root window of WINDOW's
frame.  WINDOW must be a valid window and defaults to the
selected one."
  (setq window (window-normalize-window window))
  (if (window-minibuffer-p window)
      (eq window (frame-root-window (window-frame window)))
    (= (window-pixel-height window)
       (window-pixel-height (frame-root-window window)))))

(defun window-full-width-p (&optional window)
  "Return t if WINDOW is as wide as its containing frame.
More precisely, return t if and only if the total width of WINDOW
equals the total width of the root window of WINDOW's frame.
WINDOW must be a valid window and defaults to the selected one."
  (setq window (window-normalize-window window))
  (= (window-pixel-width window)
     (window-pixel-width (frame-root-window window))))

(defun window-body-size (&optional window horizontal pixelwise)
  "Return the height or width of WINDOW's text area.
WINDOW must be a live window and defaults to the selected one.

If HORIZONTAL is omitted or nil, return the height of the text
area, like `window-body-height'.  Otherwise, return the width of
the text area, like `window-body-width'.  In either case, the
optional argument PIXELWISE is passed to the functions."
  (if horizontal
      (window-body-width window pixelwise)
    (window-body-height window pixelwise)))

(declare-function font-info "font.c" (name &optional frame))

(defun window-font-width (&optional window face)
   "Return average character width for the font of FACE used in WINDOW.
WINDOW must be a live window and defaults to the selected one.

If FACE is nil or omitted, the default face is used.  If FACE is
remapped (see `face-remapping-alist'), the function returns the
information for the remapped face."
   (with-selected-window (window-normalize-window window t)
     (if (display-multi-font-p)
	 (let* ((face (if face face 'default))
		(info (font-info (face-font face)))
		(width (aref info 11)))
	   (if (> width 0)
	      width
	     (aref info 10)))
       (frame-char-width))))

(defun window-font-height (&optional window face)
   "Return character height for the font of FACE used in WINDOW.
WINDOW must be a live window and defaults to the selected one.

If FACE is nil or omitted, the default face is used.  If FACE is
remapped (see `face-remapping-alist'), the function returns the
information for the remapped face."
   (with-selected-window (window-normalize-window window t)
     (if (display-multi-font-p)
	 (let* ((face (if face face 'default))
		(info (font-info (face-font face))))
	   (aref info 3))
       (frame-char-height))))

(defvar overflow-newline-into-fringe)

(defun window-max-chars-per-line (&optional window face)
  "Return the number of characters that can be displayed on one line in WINDOW.
WINDOW must be a live window and defaults to the selected one.

The character width of FACE is used for the calculation.  If FACE
is nil or omitted, the default face is used.  If FACE is
remapped (see `face-remapping-alist'), the function uses the
remapped face.

This function is different from `window-body-width' in two
ways.  First, it accounts for the portions of the line reserved
for the continuation glyph.  Second, it accounts for the size of
the font."
  (with-selected-window (window-normalize-window window t)
    (let* ((window-width (window-body-width window t))
	   (font-width (window-font-width window face))
	   (ncols (- (/ window-width font-width)
                     (ceiling (line-number-display-width 'columns)))))
      (if (and (display-graphic-p)
	       overflow-newline-into-fringe
               (not
                (or (eq left-fringe-width 0)
                    (and (null left-fringe-width)
                         (= (frame-parameter nil 'left-fringe) 0))))
               (not
                (or (eq right-fringe-width 0)
                    (and (null right-fringe-width)
                         (= (frame-parameter nil 'right-fringe) 0)))))
	  ncols
        ;; FIXME: This should remove 1 more column when there are no
        ;; fringes, lines are truncated, and the window is hscrolled,
        ;; but EOL is not in the view, because then there are 2
        ;; truncation glyphs, not one.
	(1- ncols)))))

(defun window-current-scroll-bars (&optional window)
  "Return the current scroll bar types for WINDOW.
WINDOW must be a live window and defaults to the selected one.

The return value is a cons cell (VERTICAL . HORIZONTAL) where
VERTICAL specifies the current location of the vertical scroll
bar (`left', `right' or nil), and HORIZONTAL specifies the
current location of the horizontal scroll bar (`bottom' or nil).

Unlike `window-scroll-bars', this function reports the scroll bar
type actually used, once frame defaults and `scroll-bar-mode' are
taken into account."
  (setq window (window-normalize-window window t))
  (let ((vertical (nth 2 (window-scroll-bars window)))
	(horizontal (nth 5 (window-scroll-bars window)))
	(inherited (frame-current-scroll-bars (window-frame window))))
    (when (eq vertical t)
      (setq vertical (car inherited)))
    (when (eq horizontal t)
      (setq horizontal (cdr inherited)))
    (cons vertical (and horizontal 'bottom))))

(defun walk-windows (fun &optional minibuf all-frames)
  "Cycle through all live windows, calling FUN for each one.
FUN must specify a function with a window as its sole argument.
The optional arguments MINIBUF and ALL-FRAMES specify the set of
windows to include in the walk.

MINIBUF t means include the minibuffer window even if the
minibuffer is not active.  MINIBUF nil or omitted means include
the minibuffer window only if the minibuffer is active.  Any
other value means do not include the minibuffer window even if
the minibuffer is active.

ALL-FRAMES nil or omitted means consider all windows on the
selected frame, plus the minibuffer window if specified by the
MINIBUF argument.  If the minibuffer counts, consider all windows
on all frames that share that minibuffer too.  The following
non-nil values of ALL-FRAMES have special meanings:

- t means consider all windows on all existing frames.

- `visible' means consider all windows on all visible frames on
  the current terminal.

- 0 (the number zero) means consider all windows on all visible
  and iconified frames on the current terminal.

- A frame means consider all windows on that frame only.

If ALL-FRAMES specifies a frame, the first window walked is the
first window on that frame (the one returned by `frame-first-window'),
not necessarily the selected window.

Anything else means consider all windows on the selected frame
and no others.

This function changes neither the order of recently selected
windows nor the buffer list."
  ;; If we start from the minibuffer window, don't fail to come
  ;; back to it.
  (when (window-minibuffer-p)
    (setq minibuf t))
  ;; Use `save-selected-window' to prevent FUN from messing up
  ;; the order of windows when it changes the selected window.
  (save-selected-window
    (dolist (walk-windows-window
             (window-list-1 (and (framep all-frames)
                                 (frame-first-window all-frames))
                            minibuf all-frames))
      (funcall fun walk-windows-window))))

(defun window-at-side-p (&optional window side)
  "Return t if WINDOW is at SIDE of its containing frame.
WINDOW must be a valid window and defaults to the selected one.
SIDE can be any of the symbols `left', `top', `right' or
`bottom'.  The default value nil is handled like `bottom'."
  (setq window (window-normalize-window window))
  (let ((edge
	 (cond
	  ((eq side 'left) 0)
	  ((eq side 'top) 1)
	  ((eq side 'right) 2)
	  ((memq side '(bottom nil)) 3))))
    (= (nth edge (window-pixel-edges window))
       (nth edge (window-pixel-edges (frame-root-window window))))))

(defun window-at-side-list (&optional frame side)
  "Return list of all windows on SIDE of FRAME.
FRAME must be a live frame and defaults to the selected frame.
SIDE can be any of the symbols `left', `top', `right' or
`bottom'.  The default value nil is handled like `bottom'."
  (setq frame (window-normalize-frame frame))
  (let (windows)
    (walk-window-tree
     (lambda (window)
       (when (window-at-side-p window side)
	 (setq windows (cons window windows))))
     frame nil 'nomini)
    (nreverse windows)))

(defun window--in-direction-2 (window posn &optional horizontal)
  "Support function for `window-in-direction'."
  (if horizontal
      (let ((top (window-pixel-top window)))
	(if (> top posn)
	    (- top posn)
	  (- posn top (window-pixel-height window))))
    (let ((left (window-pixel-left window)))
      (if (> left posn)
	  (- left posn)
	(- posn left (window-pixel-width window))))))

;; Predecessors to the below have been devised by Julian Assange in
;; change-windows-intuitively.el and Hovav Shacham in windmove.el.
;; Neither of these allow one to selectively ignore specific windows
;; (windows whose `no-other-window' parameter is non-nil) as targets of
;; the movement.
(defun window-in-direction (direction &optional window ignore sign wrap minibuf)
  "Return window in DIRECTION as seen from WINDOW.
More precisely, return the nearest window in direction DIRECTION
as seen from the position of `window-point' in window WINDOW.
DIRECTION should be one of `above', `below', `left' or `right'.
WINDOW must be a live window and defaults to the selected one.

Do not return a window whose `no-other-window' parameter is
non-nil.  If the nearest window's `no-other-window' parameter is
non-nil, try to find another window in the indicated direction.
If, however, the optional argument IGNORE is non-nil, return that
window even if its `no-other-window' parameter is non-nil.

Optional argument SIGN a negative number means to use the right
or bottom edge of WINDOW as reference position instead of
`window-point'.  SIGN a positive number means to use the left or
top edge of WINDOW as reference position.

Optional argument WRAP non-nil means to wrap DIRECTION around
frame borders.  This means to return for WINDOW at the top of the
frame and DIRECTION `above' the minibuffer window if the frame
has one, and a window at the bottom of the frame otherwise.

Optional argument MINIBUF t means to return the minibuffer
window even if it isn't active.  MINIBUF nil or omitted means
to return the minibuffer window if and only if it is currently active.
MINIBUF neither nil nor t means never return the minibuffer window.
However, if WRAP is non-nil, always act as if MINIBUF were nil.

Return nil if no suitable window can be found."
  (setq window (window-normalize-window window t))
  (cond
   ((eq direction 'up)
    (setq direction 'above))
   ((eq direction 'down)
    (setq direction 'below))
   ((not (memq direction '(above below left right)))
    (error "Wrong direction %s" direction)))
  (let* ((frame (window-frame window))
	 (hor (memq direction '(left right)))
	 (first (if hor
		    (window-pixel-left window)
		  (window-pixel-top window)))
	 (last (+ first (window-size window hor t)))
	 ;; The column / row value of `posn-at-point' can be nil for the
	 ;; mini-window, guard against that.
	 (posn
	  (cond
	   ((and (numberp sign) (< sign 0))
	    (if hor
		(1- (+ (window-pixel-top window) (window-pixel-height window)))
	      (1- (+ (window-pixel-left window) (window-pixel-width window)))))
	   ((and (numberp sign) (> sign 0))
	    (if hor
		(window-pixel-top window)
	      (window-pixel-left window)))
	   ((let ((posn-cons (nth 2 (posn-at-point (window-point window) window))))
	      (if hor
		  (+ (or (cdr posn-cons) 1) (window-pixel-top window))
		(+ (or (car posn-cons) 1) (window-pixel-left window)))))))
	 (best-edge
	  (cond
	   ((eq direction 'below) (frame-pixel-height frame))
	   ((eq direction 'right) (frame-pixel-width frame))
	   (t -1)))
	 (best-edge-2 best-edge)
	 (best-diff-2 (if hor (frame-pixel-height frame) (frame-pixel-width frame)))
	 best best-2 best-diff-2-new)
    (walk-window-tree
     (lambda (w)
       (let* ((w-top (window-pixel-top w))
	      (w-left (window-pixel-left w)))
	 (cond
	  ((or (eq window w)
	       ;; Ignore ourselves.
	       (and (window-parameter w 'no-other-window)
		    ;; Ignore W unless IGNORE is non-nil.
		    (not ignore))))
	  (hor
	   (cond
	    ((and (<= w-top posn)
		  (< posn (+ w-top (window-pixel-height w))))
	     ;; W is to the left or right of WINDOW and covers POSN.
	     (when (or (and (eq direction 'left)
			    (or (and (<= w-left first) (> w-left best-edge))
				(and wrap
				     (window-at-side-p window 'left)
				     (window-at-side-p w 'right))))
		       (and (eq direction 'right)
			    (or (and (>= w-left last) (< w-left best-edge))
				(and wrap
				     (window-at-side-p window 'right)
				     (window-at-side-p w 'left)))))
	       (setq best-edge w-left)
	       (setq best w)))
	    ((and (or (and (eq direction 'left)
			   (<= (+ w-left (window-pixel-width w)) first))
		      (and (eq direction 'right) (<= last w-left)))
		  ;; W is to the left or right of WINDOW but does not
		  ;; cover POSN.
		  (setq best-diff-2-new
			(window--in-direction-2 w posn hor))
		  (or (< best-diff-2-new best-diff-2)
		      (and (= best-diff-2-new best-diff-2)
			   (if (eq direction 'left)
			       (> w-left best-edge-2)
			     (< w-left best-edge-2)))))
	     (setq best-edge-2 w-left)
	     (setq best-diff-2 best-diff-2-new)
	     (setq best-2 w))))
	  ((and (<= w-left posn)
		(< posn (+ w-left (window-pixel-width w))))
	   ;; W is above or below WINDOW and covers POSN.
	   (when (or (and (eq direction 'above)
			  (or (and (<= w-top first) (> w-top best-edge))
			      (and wrap
				   (window-at-side-p window 'top)
				   (if (active-minibuffer-window)
				       (minibuffer-window-active-p w)
				     (window-at-side-p w 'bottom)))))
		     (and (eq direction 'below)
			  (or (and (>= w-top first) (< w-top best-edge))
			      (and wrap
				   (if (active-minibuffer-window)
				       (minibuffer-window-active-p window)
				     (window-at-side-p window 'bottom))
				   (window-at-side-p w 'top)))))
	     (setq best-edge w-top)
	     (setq best w)))
	  ((and (or (and (eq direction 'above)
			 (<= (+ w-top (window-pixel-height w)) first))
		    (and (eq direction 'below) (<= last w-top)))
		;; W is above or below WINDOW but does not cover POSN.
		(setq best-diff-2-new
		      (window--in-direction-2 w posn hor))
		(or (< best-diff-2-new best-diff-2)
		    (and (= best-diff-2-new best-diff-2)
			 (if (eq direction 'above)
			     (> w-top best-edge-2)
			   (< w-top best-edge-2)))))
	   (setq best-edge-2 w-top)
	   (setq best-diff-2 best-diff-2-new)
	   (setq best-2 w)))))
     frame nil minibuf)
    (or best best-2)))

(defun get-window-with-predicate (predicate &optional minibuf all-frames default)
  "Return a live window satisfying PREDICATE.
More precisely, cycle through all windows calling the function
PREDICATE on each one of them with the window as its sole
argument.  Return the first window for which PREDICATE returns
non-nil.  Windows are scanned starting with the window following
the selected window.  If no window satisfies PREDICATE, return
DEFAULT.

MINIBUF t means include the minibuffer window even if the
minibuffer is not active.  MINIBUF nil or omitted means include
the minibuffer window only if the minibuffer is active.  Any
other value means do not include the minibuffer window even if
the minibuffer is active.

ALL-FRAMES nil or omitted means consider all windows on the selected
frame, plus the minibuffer window if specified by the MINIBUF
argument.  If the minibuffer counts, consider all windows on all
frames that share that minibuffer too.  The following non-nil
values of ALL-FRAMES have special meanings:

- t means consider all windows on all existing frames.

- `visible' means consider all windows on all visible frames on
  the current terminal.

- 0 (the number zero) means consider all windows on all visible
  and iconified frames on the current terminal.

- A frame means consider all windows on that frame only.

Anything else means consider all windows on the selected frame
and no others."
  (catch 'found
    (dolist (window (window-list-1
		     (next-window nil minibuf all-frames)
		     minibuf all-frames))
      (when (funcall predicate window)
	(throw 'found window)))
    default))

(defalias 'some-window 'get-window-with-predicate)

(defun get-lru-window (&optional all-frames dedicated not-selected no-other)
   "Return the least recently used window on frames specified by ALL-FRAMES.
Return a full-width window if possible.  A minibuffer window is
never a candidate.  A dedicated window is never a candidate
unless DEDICATED is non-nil, so if all windows are dedicated, the
value is nil.  Avoid returning the selected window if possible.
Optional argument NOT-SELECTED non-nil means never return the
selected window.  Optional argument NO-OTHER non-nil means to
never return a window whose 'no-other-window' parameter is
non-nil.

The following non-nil values of the optional argument ALL-FRAMES
have special meanings:

- t means consider all windows on all existing frames.

- `visible' means consider all windows on all visible frames on
  the current terminal.

- 0 (the number zero) means consider all windows on all visible
  and iconified frames on the current terminal.

- A frame means consider all windows on that frame only.

Any other value of ALL-FRAMES means consider all windows on the
selected frame and no others."
   (let (best-window best-time second-best-window second-best-time time)
    (dolist (window (window-list-1 nil 'nomini all-frames))
      (when (and (or dedicated (not (window-dedicated-p window)))
		 (or (not not-selected) (not (eq window (selected-window))))
                 (or (not no-other)
                     (not (window-parameter window 'no-other-window))))
	(setq time (window-use-time window))
	(if (or (eq window (selected-window))
		(not (window-full-width-p window)))
	    (when (or (not second-best-time) (< time second-best-time))
	      (setq second-best-time time)
	      (setq second-best-window window))
	  (when (or (not best-time) (< time best-time))
	    (setq best-time time)
	    (setq best-window window)))))
    (or best-window second-best-window)))

(defun get-mru-window (&optional all-frames dedicated not-selected no-other)
   "Return the most recently used window on frames specified by ALL-FRAMES.
A minibuffer window is never a candidate.  A dedicated window is
never a candidate unless DEDICATED is non-nil, so if all windows
are dedicated, the value is nil.  Optional argument NOT-SELECTED
non-nil means never return the selected window.  Optional
argument NO-OTHER non-nil means to never return a window whose
'no-other-window' parameter is non-nil.

The following non-nil values of the optional argument ALL-FRAMES
have special meanings:

- t means consider all windows on all existing frames.

- `visible' means consider all windows on all visible frames on
  the current terminal.

- 0 (the number zero) means consider all windows on all visible
  and iconified frames on the current terminal.

- A frame means consider all windows on that frame only.

Any other value of ALL-FRAMES means consider all windows on the
selected frame and no others."
   (let (best-window best-time time)
    (dolist (window (window-list-1 nil 'nomini all-frames))
      (setq time (window-use-time window))
      (when (and (or dedicated (not (window-dedicated-p window)))
		 (or (not not-selected) (not (eq window (selected-window))))
                 (or (not no-other)
                     (not (window-parameter window 'no-other-window)))
                 (or (not best-time) (> time best-time)))
	(setq best-time time)
	(setq best-window window)))
    best-window))

(defun get-largest-window (&optional all-frames dedicated not-selected no-other)
  "Return the largest window on frames specified by ALL-FRAMES.
A minibuffer window is never a candidate.  A dedicated window is
never a candidate unless DEDICATED is non-nil, so if all windows
are dedicated, the value is nil.  Optional argument NOT-SELECTED
non-nil means never return the selected window.  Optional
argument NO-OTHER non-nil means to never return a window whose
'no-other-window' parameter is non-nil.

The following non-nil values of the optional argument ALL-FRAMES
have special meanings:

- t means consider all windows on all existing frames.

- `visible' means consider all windows on all visible frames on
  the current terminal.

- 0 (the number zero) means consider all windows on all visible
  and iconified frames on the current terminal.

- A frame means consider all windows on that frame only.

Any other value of ALL-FRAMES means consider all windows on the
selected frame and no others."
  (let ((best-size 0)
	best-window size)
    (dolist (window (window-list-1 nil 'nomini all-frames))
      (when (and (or dedicated (not (window-dedicated-p window)))
		 (or (not not-selected) (not (eq window (selected-window))))
                 (or (not no-other)
                     (not (window-parameter window 'no-other-window))))
	(setq size (* (window-pixel-height window)
		      (window-pixel-width window)))
	(when (> size best-size)
	  (setq best-size size)
	  (setq best-window window))))
    best-window))

(defun get-buffer-window-list (&optional buffer-or-name minibuf all-frames)
  "Return list of all windows displaying BUFFER-OR-NAME, or nil if none.
BUFFER-OR-NAME may be a buffer or the name of an existing buffer
and defaults to the current buffer.  If the selected window displays
BUFFER-OR-NAME, it will be the first in the resulting list.

MINIBUF t means include the minibuffer window even if the
minibuffer is not active.  MINIBUF nil or omitted means include
the minibuffer window only if the minibuffer is active.  Any
other value means do not include the minibuffer window even if
the minibuffer is active.

ALL-FRAMES nil or omitted means consider all windows on the
selected frame, plus the minibuffer window if specified by the
MINIBUF argument.  If the minibuffer counts, consider all windows
on all frames that share that minibuffer too.  The following
non-nil values of ALL-FRAMES have special meanings:

- t means consider all windows on all existing frames.

- `visible' means consider all windows on all visible frames on
  the current terminal.

- 0 (the number zero) means consider all windows on all visible
  and iconified frames on the current terminal.

- A frame means consider all windows on that frame only.

Anything else means consider all windows on the selected frame
and no others."
  (let ((buffer (window-normalize-buffer buffer-or-name))
	windows)
    (dolist (window (window-list-1 (selected-window) minibuf all-frames))
      (when (eq (window-buffer window) buffer)
	(setq windows (cons window windows))))
    (nreverse windows)))

(defun minibuffer-window-active-p (window)
  "Return t if WINDOW is the currently active minibuffer window."
  (and (window-live-p window) (eq window (active-minibuffer-window))))

(defun count-windows (&optional minibuf all-frames)
   "Return the number of live windows on the selected frame.

The optional argument MINIBUF specifies whether the minibuffer
window is included in the count.

If ALL-FRAMES is non-nil, count the windows in all frames instead
just the selected frame.

See `walk-windows' for the precise meaning of this argument."
   (length (window-list-1 nil minibuf all-frames)))

;;; Resizing windows.
(defun window--size-to-pixel (window size &optional horizontal pixelwise round-maybe)
  "For WINDOW convert SIZE lines to pixels.
SIZE is supposed to specify a height of WINDOW in terms of text
lines.  The return value is the number of pixels specifying that
height.

WINDOW must be a valid window.  Optional argument HORIZONTAL
non-nil means convert SIZE columns to pixels.

Optional argument PIXELWISE non-nil means SIZE already specifies
pixels but may have to be adjusted to a multiple of the character
size of WINDOW's frame.  Optional argument ROUND-MAYBE non-nil
means round to the nearest multiple of the character size of
WINDOW's frame if the option `window-resize-pixelwise' is nil."
  (setq window (window-normalize-window window))
  (let ((char-size (frame-char-size window horizontal)))
    (if pixelwise
	(if (and round-maybe (not window-resize-pixelwise))
	    (* (round size char-size) char-size)
	  size)
      (* size char-size))))

(defun window--pixel-to-total-1 (window horizontal char-size)
  "Subroutine of `window--pixel-to-total'."
  (let ((child (window-child window)))
    (if (window-combination-p window horizontal)
	;; In an iso-combination distribute sizes proportionally.
	(let ((remainder (window-new-total window))
	      size best-child rem best-rem)
	  ;; Initialize total sizes to each child's floor.
	  (while child
	    (setq size (max (/ (window-size child horizontal t) char-size) 1))
	    (set-window-new-total child size)
	    (setq remainder (- remainder size))
	    (setq child (window-next-sibling child)))
	  ;; Distribute remainder.
	  (while (> remainder 0)
	    (setq child (window-last-child window))
	    (setq best-child nil)
	    (setq best-rem 0)
	    (while child
	      (when (and (<= (window-new-total child)
			     (/ (window-size child horizontal t) char-size))
			 (> (setq rem (% (window-size child horizontal t)
					 char-size))
			    best-rem))
		   (setq best-child child)
		   (setq best-rem rem))
	      (setq child (window-prev-sibling child)))
	    ;; We MUST have a best-child here.
	    (set-window-new-total best-child 1 t)
	    (setq remainder (1- remainder)))
	  ;; Recurse.
	  (setq child (window-child window))
	  (while child
	    (window--pixel-to-total-1 child horizontal char-size)
	    (setq child (window-next-sibling child))))
      ;; In an ortho-combination assign new sizes directly.
      (let ((size (window-new-total window)))
	(while child
	  (set-window-new-total child size)
	  (window--pixel-to-total-1 child horizontal char-size)
	  (setq child (window-next-sibling child)))))))

(defun window--pixel-to-total (&optional frame horizontal)
  "On FRAME assign new total window heights from pixel heights.
FRAME must be a live frame and defaults to the selected frame.

Optional argument HORIZONTAL non-nil means assign new total
window widths from pixel widths."
  (setq frame (window-normalize-frame frame))
  (let* ((char-size (frame-char-size frame horizontal))
	 (root (frame-root-window frame))
	 (root-size (window-size root horizontal t))
	 ;; We have to care about the minibuffer window only if it
	 ;; appears together with the root window on this frame.
	 (mini (let ((mini (minibuffer-window frame)))
		 (and (eq (window-frame mini) frame)
		      (not (eq mini root)) mini)))
	 (mini-size (and mini (window-size mini horizontal t))))
    ;; We round the line/column sizes of windows here to the nearest
    ;; integer.  In some cases this can make windows appear _larger_
    ;; than the containing frame (line/column-wise) because the latter's
    ;; sizes are not (yet) rounded.  We might eventually fix that.
    (if (and mini (not horizontal))
	(let (lines)
	  (set-window-new-total root (max (/ root-size char-size) 1))
	  (set-window-new-total mini (max (/ mini-size char-size) 1))
	  (setq lines (- (round (+ root-size mini-size) char-size)
			 (+ (window-new-total root) (window-new-total mini))))
	  (while (> lines 0)
	    (if (>= (% root-size (window-new-total root))
		    (% mini-size (window-new-total mini)))
		(set-window-new-total root 1 t)
	      (set-window-new-total mini 1 t))
	    (setq lines (1- lines))))
      (set-window-new-total root (round root-size char-size))
      (when mini
	;; This is taken in the horizontal case only.
	(set-window-new-total mini (round mini-size char-size))))
    (unless (window-buffer root)
      (window--pixel-to-total-1 root horizontal char-size))
    ;; Apply the new sizes.
    (window-resize-apply-total frame horizontal)))

(defun window--resize-reset (&optional frame horizontal)
  "Reset resize values for all windows on FRAME.
FRAME defaults to the selected frame.

This function stores the current value of `window-size' applied
with argument HORIZONTAL in the new total size of all windows on
FRAME.  It also resets the new normal size of each of these
windows."
  (window--resize-reset-1
   (frame-root-window (window-normalize-frame frame)) horizontal))

(defun window--resize-reset-1 (window horizontal)
  "Internal function of `window--resize-reset'."
  ;; Register old size in the new total size.
  (set-window-new-pixel window (window-size window horizontal t))
  (set-window-new-total window (window-size window horizontal))
  ;; Reset new normal size.
  (set-window-new-normal window)
  (when (window-child window)
    (window--resize-reset-1 (window-child window) horizontal))
  (when (window-right window)
    (window--resize-reset-1 (window-right window) horizontal)))

;; The following is the internal function used when resizing mini
;; windows "manually", for example, when dragging a divider between
;; root and mini window.  The routines for automatic minibuffer window
;; resizing call `window--resize-root-window-vertically' instead.
(defun window--resize-mini-window (window delta)
  "Change height of mini window WINDOW by DELTA pixels.
If WINDOW cannot be resized by DELTA pixels make it as large (or
as small) as possible, but don't signal an error."
  (when (window-minibuffer-p window)
    (let* ((frame (window-frame window))
	   (root (frame-root-window frame))
	   (height (window-pixel-height window))
           (min-height (+ (frame-char-height frame)
                          (- (window-pixel-height window)
                             (window-body-height window t))))
           (max-delta (- (window-pixel-height root)
	                 (window-min-size root nil nil t))))
      ;; Don't make mini window too small.
      (when (< (+ height delta) min-height)
	(setq delta (- min-height height)))
      ;; Don't make root window too small.
      (when (> delta max-delta)
	(setq delta max-delta))

      (unless (zerop delta)
	(window--resize-reset frame)
	(window--resize-this-window root (- delta) nil nil t)
	(set-window-new-pixel window (+ height delta))
	;; The following routine catches the case where we want to resize
	;; a minibuffer-only frame.
	(when (resize-mini-window-internal window)
	  (window--pixel-to-total frame))))))

(defun window--resize-apply-p (frame &optional horizontal)
  "Return t when a window on FRAME shall be resized vertically.
Optional argument HORIZONTAL non-nil means return t when a window
shall be resized horizontally."
  (catch 'apply
    (walk-window-tree
     (lambda (window)
       (unless (= (window-new-pixel window)
		  (window-size window horizontal t))
	 (throw 'apply t)))
     frame t t)
    nil))

(defun window-resize (window delta &optional horizontal ignore pixelwise)
  "Resize WINDOW vertically by DELTA lines.
WINDOW can be an arbitrary window and defaults to the selected
one.  An attempt to resize the root window of a frame will raise
an error though.

DELTA a positive number means WINDOW shall be enlarged by DELTA
lines.  DELTA negative means WINDOW shall be shrunk by -DELTA
lines.

Optional argument HORIZONTAL non-nil means resize WINDOW
horizontally by DELTA columns.  In this case a positive DELTA
means enlarge WINDOW by DELTA columns.  DELTA negative means
WINDOW shall be shrunk by -DELTA columns.

Optional argument IGNORE, if non-nil, means to ignore restraints
induced by fixed size windows or the values of the variables
`window-min-height' and `window-min-width'.  The following values
have special meanings: `safe' means that in addition live windows
are allowed to get as small as `window-safe-min-height' lines and
`window-safe-min-width' columns.  `preserved' means to ignore
only restrictions induced by `window-preserve-size'.  If IGNORE
is a window, then ignore restrictions for that window only.

Optional argument PIXELWISE non-nil means resize WINDOW by DELTA
pixels.

This function resizes other windows proportionally and never
deletes any windows.  If you want to move only the low (right)
edge of WINDOW consider using `adjust-window-trailing-edge'
instead."
  (setq window (window-normalize-window window))
  (let* ((frame (window-frame window))
	 (minibuffer-window (minibuffer-window frame))
	 sibling)
    (setq delta (window--size-to-pixel
		 window delta horizontal pixelwise t))
    (cond
     ((eq window (frame-root-window frame))
      (error "Cannot resize the root window of a frame"))
     ((window-minibuffer-p window)
      (if horizontal
	  (error "Cannot resize minibuffer window horizontally")
	(window--resize-mini-window window delta)))
     ((and (not horizontal)
	   (window-full-height-p window)
	   (eq (window-frame minibuffer-window) frame)
	   (or (not resize-mini-windows)
	       (eq minibuffer-window (active-minibuffer-window))))
      ;; If WINDOW is full height and either `resize-mini-windows' is
      ;; nil or the minibuffer window is active, resize the minibuffer
      ;; window.
      (window--resize-mini-window minibuffer-window (- delta)))
     ((or (window--resizable-p
	   window delta horizontal ignore nil nil nil t)
	  (and (not ignore)
	       (setq ignore 'preserved)
	       (window--resizable-p
		window delta horizontal ignore nil nil nil t)))
      (window--resize-reset frame horizontal)
      (window--resize-this-window window delta horizontal ignore t)
      (if (and (not (eq window-combination-resize t))
	       (window-combined-p window horizontal)
	       (setq sibling (or (window-right window) (window-left window)))
	       (window-sizable-p
		sibling (- delta) horizontal ignore t))
	  ;; If window-combination-resize is nil, WINDOW is part of an
	  ;; iso-combination, and WINDOW's neighboring right or left
	  ;; sibling can be resized as requested, resize that sibling.
	  (let ((normal-delta
		 (/ (float delta)
		    (window-size (window-parent window) horizontal t))))
	    (window--resize-this-window sibling (- delta) horizontal nil t)
	    (set-window-new-normal
	     window (+ (window-normal-size window horizontal)
		       normal-delta))
	    (set-window-new-normal
	     sibling (- (window-normal-size sibling horizontal)
			normal-delta)))
	;; Otherwise, resize all other windows in the same combination.
	(window--resize-siblings window delta horizontal ignore))
      (when (window--resize-apply-p frame horizontal)
	(if (window-resize-apply frame horizontal)
	    (window--pixel-to-total frame horizontal)
	  (error "Failed to apply resizing %s" window))))
     (t
      (error "Cannot resize window %s" window)))))

(defun window-resize-no-error (window delta &optional horizontal ignore pixelwise)
  "Resize WINDOW vertically if it is resizable by DELTA lines.
This function is like `window-resize' but does not signal an
error when WINDOW cannot be resized.  For the meaning of the
optional arguments see the documentation of `window-resize'."
  (when (window--resizable-p
	 window delta horizontal ignore nil nil nil pixelwise)
    (window-resize window delta horizontal ignore pixelwise)))

(defun window--resize-child-windows-skip-p (window)
  "Return non-nil if WINDOW shall be skipped by resizing routines."
  (memq (window-new-normal window) '(ignore stuck skip)))

(defun window--resize-child-windows-normal (parent horizontal window this-delta &optional trail other-delta)
  "Recursively set new normal height of child windows of window PARENT.
HORIZONTAL non-nil means set the new normal width of these
windows.  WINDOW specifies a child window of PARENT that has been
resized by THIS-DELTA lines (columns).

Optional argument TRAIL either `before' or `after' means set values
only for windows before or after WINDOW.  Optional argument
OTHER-DELTA, a number, specifies that this many lines (columns)
have been obtained from (or returned to) an ancestor window of
PARENT in order to resize WINDOW."
  (let* ((delta-normal
	  (if (and (= (- this-delta)
		      (window-size window horizontal t))
		   (zerop other-delta))
	      ;; When WINDOW gets deleted and we can return its entire
	      ;; space to its siblings, use WINDOW's normal size as the
	      ;; normal delta.
	      (- (window-normal-size window horizontal))
	    ;; In any other case calculate the normal delta from the
	    ;; relation of THIS-DELTA to the total size of PARENT.
	    (/ (float this-delta)
	       (window-size parent horizontal t))))
	 (sub (window-child parent))
	 (parent-normal 0.0)
	 (skip (eq trail 'after)))

    ;; Set parent-normal to the sum of the normal sizes of all child
    ;; windows of PARENT that shall be resized, excluding only WINDOW
    ;; and any windows specified by the optional TRAIL argument.
    (while sub
      (cond
       ((eq sub window)
	(setq skip (eq trail 'before)))
       (skip)
       (t
	(setq parent-normal
	      (+ parent-normal (window-normal-size sub horizontal)))))
      (setq sub (window-right sub)))

    ;; Set the new normal size of all child windows of PARENT from what
    ;; they should have contributed for recovering THIS-DELTA lines
    ;; (columns).
    (setq sub (window-child parent))
    (setq skip (eq trail 'after))
    (while sub
      (cond
       ((eq sub window)
	(setq skip (eq trail 'before)))
       (skip)
       (t
	(let ((old-normal (window-normal-size sub horizontal)))
	  (set-window-new-normal
	   sub (min 1.0 ; Don't get larger than 1.
		    (max (- old-normal
			    (* (/ old-normal parent-normal)
			       delta-normal))
			 ;; Don't drop below 0.
			 0.0))))))
      (setq sub (window-right sub)))

    (when (numberp other-delta)
      ;; Set the new normal size of windows from what they should have
      ;; contributed for recovering OTHER-DELTA lines (columns).
      (setq delta-normal (/ (float (window-size parent horizontal t))
			    (+ (window-size parent horizontal t)
			       other-delta)))
      (setq sub (window-child parent))
      (setq skip (eq trail 'after))
      (while sub
	(cond
	 ((eq sub window)
	  (setq skip (eq trail 'before)))
	 (skip)
	 (t
	  (set-window-new-normal
	   sub (min 1.0 ; Don't get larger than 1.
		    (max (* (window-new-normal sub) delta-normal)
			 ;; Don't drop below 0.
			 0.0)))))
	(setq sub (window-right sub))))

    ;; Set the new normal size of WINDOW to what is left by the sum of
    ;; the normal sizes of its siblings.
    (set-window-new-normal
     window
     (let ((sum 0))
       (setq sub (window-child parent))
       (while sub
	 (cond
	  ((eq sub window))
	  ((not (numberp (window-new-normal sub)))
	   (setq sum (+ sum (window-normal-size sub horizontal))))
	  (t
	   (setq sum (+ sum (window-new-normal sub)))))
	 (setq sub (window-right sub)))
       ;; Don't get larger than 1 or smaller than 0.
       (min 1.0 (max (- 1.0 sum) 0.0))))))

(defun window--resize-child-windows (parent delta &optional horizontal window ignore trail edge char-size)
  "Resize child windows of window PARENT vertically by DELTA pixels.
PARENT must be a vertically combined internal window.

Optional argument HORIZONTAL non-nil means resize child windows
of PARENT horizontally by DELTA pixels.  In this case PARENT must
be a horizontally combined internal window.

WINDOW, if specified, must denote a child window of PARENT that
is resized by DELTA pixels.

The optional argument IGNORE has the same meaning as for
`window-resizable'.

Optional arguments TRAIL and EDGE, when non-nil, restrict the set
of windows that shall be resized.  If TRAIL equals `before',
resize only windows on the left or above EDGE.  If TRAIL equals
`after', resize only windows on the right or below EDGE.  Also,
preferably only resize windows adjacent to EDGE.

If the optional argument CHAR-SIZE is a positive integer, it specifies
the number of pixels by which windows are incrementally resized.
If CHAR-SIZE is nil, this means to use the value of
`frame-char-height' or `frame-char-width' of WINDOW's frame.

Return the symbol `normalized' if new normal sizes have been
already set by this routine."
  (let* ((first (window-child parent))
	 (last (window-last-child parent))
	 (parent-total (+ (window-size parent horizontal t)
			  delta))
	 (char-size (or char-size
			(and window-resize-pixelwise 1)
			(frame-char-size window horizontal)))
	 sub best-window best-value best-delta)

    (if (and edge (memq trail '(before after))
	     (progn
	       (setq sub first)
	       (while (and (window-right sub)
			   (or (and (eq trail 'before)
				    (not (window--resize-child-windows-skip-p
					  (window-right sub))))
			       (and (eq trail 'after)
				    (window--resize-child-windows-skip-p sub))))
		 (setq sub (window-right sub)))
	       sub)
	     (if horizontal
		 (if (eq trail 'before)
		     (= (+ (window-pixel-left sub) (window-pixel-width sub))
			edge)
		   (= (window-pixel-left sub) edge))
	       (if (eq trail 'before)
		   (= (+ (window-pixel-top sub) (window-pixel-height sub))
		      edge)
		 (= (window-pixel-top sub) edge)))
	     (window-sizable-p sub delta horizontal ignore t))
	;; Resize only windows adjacent to EDGE.
	(progn
	  (window--resize-this-window
	   sub delta horizontal ignore t trail edge)
	  (if (and window (eq (window-parent sub) parent))
	      (progn
		;; Assign new normal sizes.
		(set-window-new-normal
		 sub (/ (float (window-new-pixel sub)) parent-total))
		(set-window-new-normal
		 window (- (window-normal-size window horizontal)
			   (- (window-new-normal sub)
			      (window-normal-size sub horizontal)))))
	    (window--resize-child-windows-normal
	     parent horizontal sub 0 trail delta))
	  ;; Return 'normalized to notify `window--resize-siblings' that
	  ;; normal sizes have been already set.
	  'normalized)
      ;; Resize all windows proportionally.
      (setq sub last)
      (while sub
	(cond
	 ((or (window--resize-child-windows-skip-p sub)
	      ;; Ignore windows to skip and fixed-size child windows -
	      ;; in the latter case make it a window to skip.
	      (and (not ignore)
		   (window-size-fixed-p sub horizontal ignore)
		   (set-window-new-normal sub 'ignore))))
	 ((< delta 0)
	  ;; When shrinking store the number of lines/cols we can get
	  ;; from this window here together with the total/normal size
	  ;; factor.
	  (set-window-new-normal
	   sub
	   (cons
	    ;; We used to call this with NODOWN t, "fixed" 2011-05-11.
	    (window-min-delta sub horizontal ignore trail t nil t)
	    (- (/ (float (window-size sub horizontal t))
		  parent-total)
	       (window-normal-size sub horizontal)))))
	 ((> delta 0)
	  ;; When enlarging store the total/normal size factor only
	  (set-window-new-normal
	   sub
	   (- (/ (float (window-size sub horizontal t))
		 parent-total)
	      (window-normal-size sub horizontal)))))

	(setq sub (window-left sub)))

      (cond
       ((< delta 0)
	;; Shrink windows by delta.
	(setq best-window t)
	(while (and best-window (not (zerop delta)))
	  (setq sub last)
	  (setq best-window nil)
	  (setq best-value nil)
	  (while sub
	    (when (and (consp (window-new-normal sub))
		       (not (<= (car (window-new-normal sub)) 0))
		       (or (not best-value)
			   (> (cdr (window-new-normal sub)) best-value)))
	      (setq best-window sub)
	      (setq best-value (cdr (window-new-normal sub))))

	    (setq sub (window-left sub)))

	  (when best-window
	    (setq best-delta (min (car (window-new-normal best-window))
				  char-size (- delta)))
	    (setq delta (+ delta best-delta))
	    (set-window-new-pixel best-window (- best-delta) t)
	    (set-window-new-normal
	     best-window
	     (if (= (car (window-new-normal best-window)) best-delta)
		 'skip	    ; We can't shrink best-window any further.
	       (cons (- (car (window-new-normal best-window)) best-delta)
		     (- (/ (float (window-new-pixel best-window))
			   parent-total)
			(window-normal-size best-window horizontal))))))))
       ((> delta 0)
	;; Enlarge windows by delta.
	(setq best-window t)
	(while (and best-window (not (zerop delta)))
	  (setq sub last)
	  (setq best-window nil)
	  (setq best-value nil)
	  (while sub
	    (when (and (numberp (window-new-normal sub))
		       (or (not best-value)
		           (< (window-new-normal sub) best-value)))
	      (setq best-window sub)
	      (setq best-value (window-new-normal sub)))

	    (setq sub (window-left sub)))

	  (when best-window
	    (setq best-delta (min delta char-size))
	    (setq delta (- delta best-delta))
	    (set-window-new-pixel best-window best-delta t)
	    (set-window-new-normal
	     best-window
	     (- (/ (float (window-new-pixel best-window))
		   parent-total)
		(window-normal-size best-window horizontal)))))))

      (when best-window
	(setq sub last)
	(while sub
	  (when (or (consp (window-new-normal sub))
		    (numberp (window-new-normal sub)))
	    ;; Reset new normal size fields so `window-resize-apply'
	    ;; won't use them to apply new sizes.
	    (set-window-new-normal sub))

	  (unless (eq (window-new-normal sub) 'ignore)
	    ;; Resize this window's child windows (back-engineering
	    ;; delta from sub's old and new total sizes).
	    (let ((delta (- (window-new-pixel sub)
			    (window-size sub horizontal t))))
	      (unless (and (zerop delta) (not trail))
		;; For the TRAIL non-nil case we have to resize SUB
		;; recursively even if it's size does not change.
		(window--resize-this-window
		 sub delta horizontal ignore nil trail edge))))
	  (setq sub (window-left sub)))))))

(defun window--resize-siblings (window delta &optional horizontal ignore trail edge char-size)
  "Resize other windows when WINDOW is resized vertically by DELTA pixels.
Optional argument HORIZONTAL non-nil means resize other windows
when WINDOW is resized horizontally by DELTA pixels.  WINDOW
itself is not resized by this function.

The optional argument IGNORE has the same meaning as for
`window-resizable'.

Optional arguments TRAIL and EDGE, when non-nil, refine the set
of windows that shall be resized.  If TRAIL equals `before',
resize only windows on the left or above EDGE.  If TRAIL equals
`after', resize only windows on the right or below EDGE.  Also,
preferably only resize windows adjacent to EDGE."
  (when (window-parent window)
    (let* ((parent (window-parent window))
	   (sub (window-child parent)))
      (if (window-combined-p sub horizontal)
	  ;; In an iso-combination try to extract DELTA from WINDOW's
	  ;; siblings.
	  (let ((skip (eq trail 'after))
		this-delta other-delta)
	    ;; Decide which windows shall be left alone.
	    (while sub
	      (cond
	       ((eq sub window)
		;; Make sure WINDOW is left alone when
		;; resizing its siblings.
		(set-window-new-normal sub 'ignore)
		(setq skip (eq trail 'before)))
	       (skip
		;; Make sure this sibling is left alone when
		;; resizing its siblings.
		(set-window-new-normal sub 'ignore))
	       ((not (window-size-fixed-p sub horizontal ignore))
		;; Set this-delta to t to signal that we found a sibling
		;; of WINDOW whose size is not fixed.
		(setq this-delta t)))

	      (setq sub (window-right sub)))

	    ;; Set this-delta to what we can get from WINDOW's siblings.
	    (if (= (- delta) (window-size window horizontal t))
		;; A deletion, presumably.  We must handle this case
		;; specially since `window--resizable' can't be used.
		(if this-delta
		    ;; There's at least one resizable sibling we can
		    ;; give WINDOW's size to.
		    (setq this-delta delta)
		  ;; No resizable sibling exists.
		  (setq this-delta 0))
	      ;; Any other form of resizing.
	      (setq this-delta
		    (window--resizable
		     window delta horizontal ignore trail t nil t)))

	    ;; Set other-delta to what we still have to get from
	    ;; ancestor windows of parent.
	    (setq other-delta (- delta this-delta))
	    (unless (zerop other-delta)
	      ;; Unless we got everything from WINDOW's siblings, PARENT
	      ;; must be resized by other-delta lines or columns.
	      (set-window-new-pixel parent other-delta 'add))

	    (if (zerop this-delta)
		;; We haven't got anything from WINDOW's siblings but we
		;; must update the normal sizes to respect other-delta.
		(window--resize-child-windows-normal
		 parent horizontal window this-delta trail other-delta)
	      ;; We did get something from WINDOW's siblings which means
	      ;; we have to resize their child windows.
	      (unless (eq (window--resize-child-windows
			   parent (- this-delta) horizontal
			   window ignore trail edge char-size)
			  ;; If `window--resize-child-windows' returns
			  ;; 'normalized, this means it has set the
			  ;; normal sizes already.
			  'normalized)
		;; Set the normal sizes.
		(window--resize-child-windows-normal
		 parent horizontal window this-delta trail other-delta))
	      ;; Set DELTA to what we still have to get from ancestor
	      ;; windows.
	      (setq delta other-delta)))

	;; In an ortho-combination all siblings of WINDOW must be
	;; resized by DELTA.
	(set-window-new-pixel parent delta 'add)
	(while sub
	  (unless (eq sub window)
	    (window--resize-this-window
	     sub delta horizontal ignore t))
	  (setq sub (window-right sub))))

      (unless (zerop delta)
	;; "Go up."
	(window--resize-siblings
	 parent delta horizontal ignore trail edge char-size)))))

(defun window--resize-this-window (window delta &optional horizontal ignore add trail edge char-size)
  "Resize WINDOW vertically by DELTA pixels.
Optional argument HORIZONTAL non-nil means resize WINDOW
horizontally by DELTA pixels.

The optional argument IGNORE has the same meaning as for
`window-resizable'.  Optional argument ADD non-nil means add
DELTA to the new total size of WINDOW.

Optional arguments TRAIL and EDGE, when non-nil, refine the set
of windows that shall be resized.  If TRAIL equals `before',
resize only windows on the left or above EDGE.  If TRAIL equals
`after', resize only windows on the right or below EDGE.  Also,
preferably only resize windows adjacent to EDGE.

If the optional argument CHAR-SIZE is a positive integer, it specifies
the number of pixels by which windows are incrementally resized.
If CHAR-SIZE is nil, this means to use the value of
`frame-char-height' or `frame-char-width' of WINDOW's frame.

This function recursively resizes WINDOW's child windows to fit the
new size.  Make sure that WINDOW is `window--resizable' before
calling this function.  Note that this function does not resize
siblings of WINDOW or WINDOW's parent window.  You have to
eventually call `window-resize-apply' in order to make resizing
actually take effect."
  (when add
    ;; Add DELTA to the new total size of WINDOW.
    (set-window-new-pixel window delta t))

  (let ((sub (window-child window)))
    (cond
     ((not sub))
     ((window-combined-p sub horizontal)
      ;; In an iso-combination resize child windows according to their
      ;; normal sizes.
      (window--resize-child-windows
       window delta horizontal nil ignore trail edge char-size))
     ;; In an ortho-combination resize each child window by DELTA.
     (t
      (while sub
	(window--resize-this-window
	 sub delta horizontal ignore t trail edge char-size)
	(setq sub (window-right sub)))))))

(defun window--resize-root-window (window delta horizontal ignore pixelwise)
  "Resize root window WINDOW vertically by DELTA lines.
HORIZONTAL non-nil means resize root window WINDOW horizontally
by DELTA columns.

IGNORE non-nil means ignore any restrictions imposed by fixed
size windows, `window-min-height' or `window-min-width' settings.

This function is only called by the frame resizing routines.  It
resizes windows proportionally and never deletes any windows."
  (when (and (windowp window) (numberp delta))
    (let ((pixel-delta
	   (if pixelwise
	       delta
	     (window--size-to-pixel window delta horizontal))))
      (when (window-sizable-p window pixel-delta horizontal ignore t)
	(window--resize-reset (window-frame window) horizontal)
	(window--resize-this-window
	 window pixel-delta horizontal ignore t)))))

(defun window--resize-root-window-vertically (window delta pixelwise)
  "Resize root window WINDOW vertically by DELTA lines.
If DELTA is less than zero and we can't shrink WINDOW by DELTA
lines, shrink it as much as possible.  If DELTA is greater than
zero, this function can resize fixed-size windows in order to
recover the necessary lines.  Return the number of lines that
were recovered.

Third argument PIXELWISE non-nil means to interpret DELTA as
pixels and return the number of pixels that were recovered.

This function is called by the minibuffer window resizing
routines."
  (let* ((frame (window-frame window))
	 (pixel-delta
	  (cond
	   (pixelwise
	    delta)
	   ((numberp delta)
	    (* (frame-char-height frame) delta))
	   (t 0)))
	 ignore)
    (cond
     ((zerop pixel-delta))
     ((< pixel-delta 0)
      (setq pixel-delta (window-sizable window pixel-delta nil nil pixelwise))
      (window--resize-reset frame)
      ;; When shrinking the root window, emulate an edge drag in order
      ;; to not resize other windows if we can avoid it (Bug#12419).
      (window--resize-this-window
       window pixel-delta nil ignore t 'before
       (+ (window-pixel-top window) (window-pixel-height window)))
      ;; Don't record new normal sizes to make sure that shrinking back
      ;; proportionally works as intended.
      (walk-window-tree
       (lambda (window) (set-window-new-normal window 'ignore)) frame t))
     ((> pixel-delta 0)
      (window--resize-reset frame)
      (unless (window-sizable window pixel-delta nil nil pixelwise)
	(setq ignore t))
      ;; When growing the root window, resize proportionally.  This
      ;; should give windows back their original sizes (hopefully).
      (window--resize-this-window
       window pixel-delta nil ignore t)))
     ;; Return the possibly adjusted DELTA.
     (if pixelwise
	 pixel-delta
       (/ pixel-delta (frame-char-height frame)))))

(defun window--resize-mini-frame (frame)
  "Resize minibuffer-only frame FRAME."
  (if (functionp resize-mini-frames)
      (funcall resize-mini-frames frame)
    (fit-mini-frame-to-buffer frame)))

(defun window--sanitize-window-sizes (horizontal)
  "Assert that all windows on selected frame are large enough.
If necessary and possible, make sure that every window on frame
FRAME has its minimum height.  Optional argument HORIZONTAL
non-nil means to make sure that every window on frame FRAME has
its minimum width.  The minimum height/width of a window is the
respective value returned by `window-min-size' for that window.

Return t if all windows were resized appropriately.  Return nil
if at least one window could not be resized as requested, which
may happen when the FRAME is not large enough to accommodate it."
  (let ((value t))
    (walk-window-tree
     (lambda (window)
       (let  ((delta (- (window-min-size window horizontal nil t)
			(window-size window horizontal t))))
	 (when (> delta 0)
	   (if (window-resizable-p window delta horizontal nil t)
	       (window-resize window delta horizontal nil t)
	     (setq value nil)))))
     nil nil 'nomini)
    value))

(defun adjust-window-trailing-edge (window delta &optional horizontal pixelwise)
  "Move WINDOW's bottom edge by DELTA lines.
Optional argument HORIZONTAL non-nil means move WINDOW's right
edge by DELTA columns.  WINDOW must be a valid window and
defaults to the selected one.

Optional argument PIXELWISE non-nil means interpret DELTA as
number of pixels.

If DELTA is greater than zero, move the edge downwards or to the
right.  If DELTA is less than zero, move the edge upwards or to
the left.  If the edge can't be moved by DELTA lines or columns,
move it as far as possible in the desired direction."
  (setq window (window-normalize-window window))
  (let* ((frame (window-frame window))
	 (minibuffer-window (minibuffer-window frame))
	 (right window)
	 left first-left first-right this-delta min-delta max-delta ignore)

    (unless pixelwise
      (setq pixelwise t)
      (setq delta (* delta (frame-char-size window horizontal))))

    ;; Find the edge we want to move.
    (while (and (or (not (window-combined-p right horizontal))
		    (not (window-right right)))
		(setq right (window-parent right))))
    (cond
     ((and (not right) (not horizontal)
	   ;; Resize the minibuffer window if it's on the same frame as
	   ;; and immediately below WINDOW and it's either active or
	   ;; `resize-mini-windows' is nil.
	   (eq (window-frame minibuffer-window) frame)
	   (= (nth 1 (window-pixel-edges minibuffer-window))
	      (nth 3 (window-pixel-edges window)))
	   (or (not resize-mini-windows)
	       (eq minibuffer-window (active-minibuffer-window))))
      (window--resize-mini-window minibuffer-window (- delta)))
     ((or (not (setq left right)) (not (setq right (window-right right))))
      (if horizontal
	  (user-error "No window on the right of this one")
	(user-error "No window below this one")))
     (t
      ;; Set LEFT to the first resizable window on the left.  This step is
      ;; needed to handle fixed-size windows.
      (setq first-left left)
      (while (and left
		  (or (window-size-fixed-p left horizontal)
		      (and (< delta 0)
			   (<= (window-size left horizontal t)
			       (window-min-size left horizontal nil t)))))
	(setq left
	      (or (window-left left)
		  (progn
		    (while (and (setq left (window-parent left))
				(not (window-combined-p left horizontal))))
		    (window-left left)))))
      (unless left
	;; We have to resize a size-preserved window.  Start again with
	;; the window initially on the left.
	(setq ignore 'preserved)
	(setq left first-left)
	(while (and left
		    (or (window-size-fixed-p left horizontal 'preserved)
			(and (< delta 0)
                             (<= (window-size left horizontal t)
                                 (window-min-size
                                  left horizontal 'preserved t)))))
	  (setq left
		(or (window-left left)
		    (progn
		      (while (and (setq left (window-parent left))
				  (not (window-combined-p left horizontal))))
		      (window-left left)))))

	(unless left
	  (if horizontal
	      (user-error "No resizable window on the left of this one")
	    (user-error "No resizable window above this one"))))

      ;; Set RIGHT to the first resizable window on the right.  This step
      ;; is needed to handle fixed-size windows.
      (setq first-right right)
      (while (and right
		  (or (window-size-fixed-p right horizontal)
		      (and (> delta 0)
			   (<= (window-size right horizontal t)
			       (window-min-size
                                right horizontal 'preserved t)))))
	(setq right
	      (or (window-right right)
		  (progn
		    (while (and (setq right (window-parent right))
				(not (window-combined-p right horizontal))))
		    (window-right right)))))
      (unless right
	;; We have to resize a size-preserved window.  Start again with
	;; the window initially on the right.
	(setq ignore 'preserved)
	(setq right first-right)
	(while (and right
		    (or (window-size-fixed-p right horizontal 'preserved)
                        (and (> delta 0)
                             (<= (window-size right horizontal t)
                                 (window-min-size
                                  right horizontal 'preserved t)))))
	  (setq right
		(or (window-right right)
		    (progn
		      (while (and (setq right (window-parent right))
				  (not (window-combined-p right horizontal))))
		      (window-right right)))))
	(unless right
	  (if horizontal
	      (user-error "No resizable window on the right of this one")
	    (user-error "No resizable window below this one"))))

      ;; LEFT and RIGHT (which might be both internal windows) are now the
      ;; two windows we want to resize.
      (cond
       ((> delta 0)
	(setq max-delta
	      (window--max-delta-1
	       left 0 horizontal ignore 'after nil pixelwise))
	(setq min-delta
	      (window--min-delta-1
	       right (- delta) horizontal ignore 'before nil pixelwise))
	(when (or (< max-delta delta) (> min-delta (- delta)))
	  ;; We can't get the whole DELTA - move as far as possible.
	  (setq delta (min max-delta (- min-delta))))
	(unless (zerop delta)
	  ;; Start resizing.
	  (window--resize-reset frame horizontal)
	  ;; Try to enlarge LEFT first.
	  (setq this-delta
                (window--resizable
                 left delta horizontal ignore 'after nil nil pixelwise))
	  (unless (zerop this-delta)
	    (window--resize-this-window
	     left this-delta horizontal ignore t 'before
	     (if horizontal
		 (+ (window-pixel-left left) (window-pixel-width left))
	       (+ (window-pixel-top left) (window-pixel-height left)))))
	  ;; Shrink windows on right of LEFT.
	  (window--resize-siblings
	   left delta horizontal ignore 'after
	   (if horizontal
	       (window-pixel-left right)
	     (window-pixel-top right)))))
       ((< delta 0)
	(setq max-delta
	      (window--max-delta-1
	       right 0 horizontal ignore 'before nil pixelwise))
	(setq min-delta
	      (window--min-delta-1
	       left delta horizontal ignore 'after nil pixelwise))
	(when (or (< max-delta (- delta)) (> min-delta delta))
	  ;; We can't get the whole DELTA - move as far as possible.
	  (setq delta (max (- max-delta) min-delta)))
	(unless (zerop delta)
	  ;; Start resizing.
	  (window--resize-reset frame horizontal)
	  ;; Try to enlarge RIGHT.
	  (setq this-delta
		(window--resizable
		 right (- delta) horizontal ignore 'before nil nil pixelwise))
	  (unless (zerop this-delta)
	    (window--resize-this-window
	     right this-delta horizontal ignore t 'after
	     (if horizontal
		 (window-pixel-left right)
	       (window-pixel-top right))))
	  ;; Shrink windows on left of RIGHT.
	  (window--resize-siblings
	   right (- delta) horizontal ignore 'before
	   (if horizontal
	       (+ (window-pixel-left left) (window-pixel-width left))
	     (+ (window-pixel-top left) (window-pixel-height left)))))))
      (unless (zerop delta)
	;; Don't report an error in the standard case.
	(when (window--resize-apply-p frame horizontal)
	  (if (window-resize-apply frame horizontal)
	      (window--pixel-to-total frame horizontal)
	    ;; But do report an error if applying the changes fails.
	    (error "Failed adjusting window %s" window))))))))

(defun enlarge-window (delta &optional horizontal)
  "Make the selected window DELTA lines taller.
Interactively, if no argument is given, make the selected window
one line taller.  If optional argument HORIZONTAL is non-nil,
make selected window wider by DELTA columns.  If DELTA is
negative, shrink selected window by -DELTA lines or columns."
  (interactive "p")
  (let ((minibuffer-window (minibuffer-window)))
    (when (window-preserved-size nil horizontal)
      (window-preserve-size nil horizontal))
    (cond
     ((zerop delta))
     ((window-size-fixed-p nil horizontal)
      (user-error "Selected window has fixed size"))
     ((window-minibuffer-p)
      (if horizontal
	  (user-error "Cannot resize minibuffer window horizontally")
	(window--resize-mini-window
         (selected-window) (* delta (frame-char-height)))))
     ((and (not horizontal)
	   (window-full-height-p)
	   (eq (window-frame minibuffer-window) (selected-frame))
	   (not resize-mini-windows))
      ;; If the selected window is full height and `resize-mini-windows'
      ;; is nil, resize the minibuffer window.
      (window--resize-mini-window
       minibuffer-window (* (- delta) (frame-char-height))))
     ((window--resizable-p nil delta horizontal)
      (window-resize nil delta horizontal))
     ((window--resizable-p nil delta horizontal 'preserved)
      (window-resize nil delta horizontal 'preserved))
     ((eq this-command
	  (if horizontal 'enlarge-window-horizontally 'enlarge-window))
      ;; For backward compatibility don't signal an error unless this
      ;; command is `enlarge-window(-horizontally)'.
      (user-error "Cannot enlarge selected window"))
     (t
      (window-resize
       nil (if (> delta 0)
	       (window-max-delta nil horizontal)
	     (- (window-min-delta nil horizontal)))
       horizontal)))))

(defun shrink-window (delta &optional horizontal)
  "Make the selected window DELTA lines smaller.
Interactively, if no argument is given, make the selected window
one line smaller.  If optional argument HORIZONTAL is non-nil,
make selected window narrower by DELTA columns.  If DELTA is
negative, enlarge selected window by -DELTA lines or columns."
  (interactive "p")
  (let ((minibuffer-window (minibuffer-window)))
    (when (window-preserved-size nil horizontal)
      (window-preserve-size nil horizontal))
    (cond
     ((zerop delta))
     ((window-size-fixed-p nil horizontal)
      (user-error "Selected window has fixed size"))
     ((window-minibuffer-p)
      (if horizontal
	  (user-error "Cannot resize minibuffer window horizontally")
	(window--resize-mini-window
         (selected-window) (* (- delta) (frame-char-height)))))
     ((and (not horizontal)
	   (window-full-height-p)
	   (eq (window-frame minibuffer-window) (selected-frame))
	   (not resize-mini-windows))
      ;; If the selected window is full height and `resize-mini-windows'
      ;; is nil, resize the minibuffer window.
      (window--resize-mini-window
       minibuffer-window (* delta (frame-char-height))))
     ((window--resizable-p nil (- delta) horizontal)
      (window-resize nil (- delta) horizontal))
     ((window--resizable-p nil (- delta) horizontal 'preserved)
      (window-resize nil (- delta) horizontal 'preserved))
     ((eq this-command
	  (if horizontal 'shrink-window-horizontally 'shrink-window))
      ;; For backward compatibility don't signal an error unless this
      ;; command is `shrink-window(-horizontally)'.
      (user-error "Cannot shrink selected window"))
     (t
      (window-resize
       nil (if (> delta 0)
	       (- (window-min-delta nil horizontal))
	     (window-max-delta nil horizontal))
       horizontal)))))

(defun maximize-window (&optional window)
  "Maximize WINDOW.
Make WINDOW as large as possible without deleting any windows.
WINDOW must be a valid window and defaults to the selected one.

If the option `window-resize-pixelwise' is non-nil maximize
WINDOW pixelwise."
  (interactive)
  (setq window (window-normalize-window window))
  (window-resize
   window (window-max-delta window nil nil nil nil nil window-resize-pixelwise)
   nil nil window-resize-pixelwise)
  (window-resize
   window (window-max-delta window t nil nil nil nil window-resize-pixelwise)
   t nil window-resize-pixelwise))

(defun minimize-window (&optional window)
  "Minimize WINDOW.
Make WINDOW as small as possible without deleting any windows.
WINDOW must be a valid window and defaults to the selected one.

If the option `window-resize-pixelwise' is non-nil minimize
WINDOW pixelwise."
  (interactive)
  (setq window (window-normalize-window window))
  (window-resize
   window
   (- (window-min-delta window nil nil nil nil nil window-resize-pixelwise))
   nil nil window-resize-pixelwise)
  (window-resize
   window
   (- (window-min-delta window t nil nil nil nil window-resize-pixelwise))
   t nil window-resize-pixelwise))

;;; Window edges
(defun window-edges (&optional window body absolute pixelwise)
  "Return a list of the edge distances of WINDOW.
WINDOW must be a valid window and defaults to the selected one.
The list returned has the form (LEFT TOP RIGHT BOTTOM).

If the optional argument BODY is nil, this means to return the
edges corresponding to the total size of WINDOW.  BODY non-nil
means to return the edges of WINDOW's body (aka text area).  If
BODY is non-nil, WINDOW must specify a live window.

Optional argument ABSOLUTE nil means to return edges relative to
the position of WINDOW's native frame.  ABSOLUTE non-nil means to
return coordinates relative to the origin - the position (0, 0) -
of FRAME's display.  On non-graphical systems this argument has
no effect.

Optional argument PIXELWISE nil means to return the coordinates
in terms of the canonical character width and height of WINDOW's
frame, rounded if necessary.  PIXELWISE non-nil means to return
the coordinates in pixels where the values for RIGHT and BOTTOM
are one more than the actual value of these edges.  Note that if
ABSOLUTE is non-nil, PIXELWISE is implicitly non-nil too."
  (let* ((window (window-normalize-window window body))
	 (frame (window-frame window))
	 (border-width (frame-internal-border-width frame))
	 (char-width (frame-char-width frame))
	 (char-height (frame-char-height frame))
	 (left (if pixelwise
		   (+ (window-pixel-left window) border-width)
		 (+ (window-left-column window)
		    (/ border-width char-width))))
	 (left-body
	  (when body
	    (+ (window-pixel-left window) border-width
	       (if (eq (car (window-current-scroll-bars window)) 'left)
		   (window-scroll-bar-width window)
		 0)
	       (nth 0 (window-fringes window))
	       (* (or (nth 0 (window-margins window)) 0) char-width))))
	 (top (if pixelwise
		  (+ (window-pixel-top window) border-width)
		(+ (window-top-line window)
		   (/ border-width char-height))))
	 (top-body
	  (when body
	    (+ (window-pixel-top window) border-width
	       (window-header-line-height window))))
	 (right (+ left (if pixelwise
			    (window-pixel-width window)
			  (window-total-width window))))
	 (right-body (and body (+ left-body (window-body-width window t))))
	 (bottom (+ top (if pixelwise
			    (window-pixel-height window)
			  (window-total-height window))))
	 (bottom-body (and body (+ top-body (window-body-height window t)))))
    (if absolute
	(let* ((native-edges (frame-edges frame 'native-edges))
	       (left-off (nth 0 native-edges))
	       (top-off (nth 1 native-edges)))
	      (if body
		  (list (+ left-body left-off) (+ top-body top-off)
			(+ right-body left-off) (+ bottom-body top-off))
		(list (+ left left-off) (+ top top-off)
		      (+ right left-off) (+ bottom top-off))))
      (if body
	  (if pixelwise
	      (list left-body top-body right-body bottom-body)
	    (list (/ left-body char-width) (/ top-body char-height)
		  ;; Round up.
		  (/ (+ right-body char-width -1) char-width)
		  (/ (+ bottom-body char-height -1) char-height)))
	(list left top right bottom)))))

(defun window-body-edges (&optional window)
  "Return a list of the edge coordinates of WINDOW's body.
The return value is that of `window-edges' called with argument
BODY non-nil."
  (window-edges window t))
(defalias 'window-inside-edges 'window-body-edges)

(defun window-pixel-edges (&optional window)
  "Return a list of the edge pixel coordinates of WINDOW.
The return value is that of `window-edges' called with argument
PIXELWISE non-nil."
  (window-edges window nil nil t))

(defun window-body-pixel-edges (&optional window)
  "Return a list of the edge pixel coordinates of WINDOW's body.
The return value is that of `window-edges' called with arguments
BODY and PIXELWISE non-nil."
  (window-edges window t nil t))
(defalias 'window-inside-pixel-edges 'window-body-pixel-edges)

(defun window-absolute-pixel-edges (&optional window)
  "Return a list of the edge pixel coordinates of WINDOW.
The return value is that of `window-edges' called with argument
ABSOLUTE non-nil."
  (window-edges window nil t t))

(defun window-absolute-body-pixel-edges (&optional window)
  "Return a list of the edge pixel coordinates of WINDOW's text area.
The return value is that of `window-edges' called with arguments
BODY and ABSOLUTE non-nil."
  (window-edges window t t t))
(defalias 'window-inside-absolute-pixel-edges 'window-absolute-body-pixel-edges)

(defun window-absolute-pixel-position (&optional position window)
  "Return display coordinates of POSITION in WINDOW.
If the buffer position POSITION is visible in window WINDOW,
return the display coordinates of the upper/left corner of the
glyph at POSITION.  The return value is a cons of the X- and
Y-coordinates of that corner, relative to an origin at (0, 0) of
WINDOW's display.  Return nil if POSITION is not visible in
WINDOW.

WINDOW must be a live window and defaults to the selected window.
POSITION defaults to the value of `window-point' of WINDOW."
  (let* ((window (window-normalize-window window t))
	 (pos-in-window
	  (pos-visible-in-window-p
	   (or position (window-point window)) window t)))
    (when pos-in-window
      (let ((edges (window-absolute-body-pixel-edges window)))
	(cons (+ (nth 0 edges) (nth 0 pos-in-window))
	      (+ (nth 1 edges) (nth 1 pos-in-window)))))))

(defun frame-root-window-p (window)
  "Return t if WINDOW is the root window of its frame."
  (eq window (frame-root-window window)))

(defun window--subtree (window &optional next)
  "Return window subtree rooted at WINDOW.
Optional argument NEXT non-nil means include WINDOW's right
siblings in the return value.

See the documentation of `window-tree' for a description of the
return value."
  (let (list)
    (while window
      (setq list
	    (cons
	     (cond
	      ((window-top-child window)
	       (cons t (cons (window-edges window)
			     (window--subtree (window-top-child window) t))))
	      ((window-left-child window)
	       (cons nil (cons (window-edges window)
			       (window--subtree (window-left-child window) t))))
	      (t window))
	     list))
      (setq window (when next (window-next-sibling window))))
    (nreverse list)))

(defun window-tree (&optional frame)
  "Return the window tree of frame FRAME.
FRAME must be a live frame and defaults to the selected frame.
The return value is a list of the form (ROOT MINI), where ROOT
represents the window tree of the frame's root window, and MINI
is the frame's minibuffer window.

If the root window is not split, ROOT is the root window itself.
Otherwise, ROOT is a list (DIR EDGES W1 W2 ...) where DIR is nil
for a horizontal split, and t for a vertical split.  EDGES gives
the combined size and position of the child windows in the split,
and the rest of the elements are the child windows in the split.
Each of the child windows may again be a window or a list
representing a window split, and so on.  EDGES is a list (LEFT
TOP RIGHT BOTTOM) as returned by `window-edges'."
  (setq frame (window-normalize-frame frame))
  (window--subtree (frame-root-window frame) t))

(defun other-window (count &optional all-frames interactive)
  "Select another window in cyclic ordering of windows.
COUNT specifies the number of windows to skip, starting with the
selected window, before making the selection.  If COUNT is
positive, skip COUNT windows forwards.  If COUNT is negative,
skip -COUNT windows backwards.  COUNT zero means do not skip any
window, so select the selected window.  In an interactive call,
COUNT is the numeric prefix argument.  Return nil.

If the `other-window' parameter of the selected window is a
function and `ignore-window-parameters' is nil, call that
function with the arguments COUNT and ALL-FRAMES.

This function does not select a window whose `no-other-window'
window parameter is non-nil.

This function uses `next-window' for finding the window to
select.  The argument ALL-FRAMES has the same meaning as in
`next-window', but the MINIBUF argument of `next-window' is
always effectively nil."
  (interactive "p\ni\np")
  (let* ((window (selected-window))
         (original-window window)
	 (function (and (not ignore-window-parameters)
			(window-parameter window 'other-window)))
	 old-window old-count)
    (if (functionp function)
	(funcall function count all-frames)
      ;; `next-window' and `previous-window' may return a window we are
      ;; not allowed to select.  Hence we need an exit strategy in case
      ;; all windows are non-selectable.
      (catch 'exit
	(while (> count 0)
	  (setq window (next-window window nil all-frames))
	  (cond
	   ((eq window old-window)
	    (when (= count old-count)
	      ;; Keep out of infinite loops.  When COUNT has not changed
	      ;; since we last looked at `window' we're probably in one.
	      (throw 'exit nil)))
	   ((window-parameter window 'no-other-window)
	    (unless old-window
	      ;; The first non-selectable window `next-window' got us:
	      ;; Remember it and the current value of COUNT.
	      (setq old-window window)
	      (setq old-count count)))
	   (t
	    (setq count (1- count)))))
	(while (< count 0)
	  (setq window (previous-window window nil all-frames))
	  (cond
	   ((eq window old-window)
	    (when (= count old-count)
	      ;; Keep out of infinite loops.  When COUNT has not changed
	      ;; since we last looked at `window' we're probably in one.
	      (throw 'exit nil)))
	   ((window-parameter window 'no-other-window)
	    (unless old-window
	      ;; The first non-selectable window `previous-window' got
	      ;; us: Remember it and the current value of COUNT.
	      (setq old-window window)
	      (setq old-count count)))
	   (t
	    (setq count (1+ count)))))

        (when (and (eq window original-window)
                   interactive
                   (not (or executing-kbd-macro noninteractive)))
          (message "No other window to select"))

	(select-window window)
	;; Always return nil.
	nil))))

(defun other-window-prefix ()
  "Display the buffer of the next command in a new window.
The next buffer is the buffer displayed by the next command invoked
immediately after this command (ignoring reading from the minibuffer).
Creates a new window before displaying the buffer.
When `switch-to-buffer-obey-display-actions' is non-nil,
`switch-to-buffer' commands are also supported."
  (interactive)
  (display-buffer-override-next-command
   (lambda (buffer alist)
     (let ((alist (append '((inhibit-same-window . t)) alist))
           window type)
       (if (setq window (display-buffer-pop-up-window buffer alist))
           (setq type 'window)
         (setq window (display-buffer-use-some-window buffer alist)
               type 'reuse))
       (cons window type)))
   nil "[other-window]")
  (message "Display next command buffer in a new window..."))

(defun same-window-prefix ()
  "Display the buffer of the next command in the same window.
The next buffer is the buffer displayed by the next command invoked
immediately after this command (ignoring reading from the minibuffer).
Even when the default rule should display the buffer in a new window,
force its display in the already selected window.
When `switch-to-buffer-obey-display-actions' is non-nil,
`switch-to-buffer' commands are also supported."
  (interactive)
  (display-buffer-override-next-command
   (lambda (buffer alist)
     (setq alist (append '((inhibit-same-window . nil)) alist))
     (cons (or
            (display-buffer-same-window buffer alist)
            (display-buffer-use-some-window buffer alist))
           'reuse))
   nil "[same-window]")
  (message "Display next command buffer in the same window..."))

;; This should probably return non-nil when the selected window is part
;; of an atomic window whose root is the frame's root window.
(defun one-window-p (&optional nomini all-frames)
  "Return non-nil if the selected window is the only window.
Optional arg NOMINI non-nil means don't count the minibuffer
even if it is active.  Otherwise, the minibuffer is counted
when it is active.

Optional argument ALL-FRAMES specifies the set of frames to
consider, see also `next-window'.  ALL-FRAMES nil or omitted
means consider windows on the selected frame only, plus the
minibuffer window if specified by the NOMINI argument.  If the
minibuffer counts, consider all windows on all frames that share
that minibuffer too.  The remaining non-nil values of ALL-FRAMES
with a special meaning are:

- t means consider all windows on all existing frames.

- `visible' means consider all windows on all visible frames on
  the current terminal.

- 0 (the number zero) means consider all windows on all visible
  and iconified frames on the current terminal.

- A frame means consider all windows on that frame only.

Anything else means consider all windows on the selected frame
and no others."
  (let ((base-window (selected-window)))
    (if (and nomini (eq base-window (minibuffer-window)))
	(setq base-window (next-window base-window)))
    (eq base-window
	(next-window base-window (if nomini 'arg) all-frames))))

;;; Deleting windows.
(defun window-deletable-p (&optional window)
  "Return t if WINDOW can be safely deleted from its frame.
WINDOW must be a valid window and defaults to the selected one.

Return `frame' if WINDOW is the root window of its frame and that
frame can be safely deleted."
  (setq window (window-normalize-window window))

  (unless (or ignore-window-parameters
	      (eq (window-parameter window 'delete-window) t))
    ;; Handle atomicity.
    (when (window-parameter window 'window-atom)
      (setq window (window-atom-root window))))

  (let ((frame (window-frame window)))
    (cond
     ((frame-root-window-p window)
      ;; WINDOW's frame can be deleted only if there are other frames
      ;; on the same terminal, and it does not contain the active
      ;; minibuffer.
      (unless (or (eq frame (next-frame frame 0))
		  ;; We can delete our frame only if no other frame
		  ;; currently uses our minibuffer window.
		  (catch 'other
		    (dolist (other (frame-list))
		      (when (and (not (eq other frame))
				 (eq (window-frame (minibuffer-window other))
				     frame))
			(throw 'other t))))
		  (let ((minibuf (active-minibuffer-window)))
		    (and minibuf (eq frame (window-frame minibuf))
                         (not (eq (default-toplevel-value
                                    'minibuffer-follows-selected-frame)
                                  t)))))
	'frame))
     ((window-minibuffer-p window)
      ;; If WINDOW is the minibuffer window of a non-minibuffer-only
      ;; frame, it cannot be deleted separately.
      nil)
     ((or ignore-window-parameters
	  (not (eq window (window-main-window frame))))
      ;; Otherwise, WINDOW can be deleted unless it is the main window
      ;; of its frame.
      t))))

(defun window-at-x-y (x y &optional frame no-other)
  "Return live window at coordinates X, Y on specified FRAME.
X and Y are FRAME-relative pixel coordinates.  A coordinate on an
edge shared by two windows is attributed to the window on the
right (or below).  Return nil if no such window can be found.

Optional argument FRAME must specify a live frame and defaults to
the selected one.  Optional argument NO-OTHER non-nil means to
return nil if the window located at the specified coordinates has
a non-nil `no-other-window' parameter."
  (setq frame (window-normalize-frame frame))
  (let* ((root-edges (window-edges (frame-root-window frame) nil nil t))
         (root-left (nth 2 root-edges))
         (root-bottom (nth 3 root-edges)))
    (catch 'window
      (walk-window-tree
       (lambda (window)
         (let ((edges (window-edges window nil nil t)))
	   (when (and (>= x (nth 0 edges))
                      (or (< x (nth 2 edges)) (= x root-left))
		      (>= y (nth 1 edges))
                      (or (< y (nth 3 edges)) (= y root-bottom)))
             (if (and no-other (window-parameter window 'no-other-window))
                 (throw 'window nil)
	       (throw 'window window)))))
       frame))))

(defcustom delete-window-choose-selected 'mru
  "How to choose a frame's selected window after window deletion.
When a frame's selected window gets deleted, Emacs has to choose
another live window on that frame to serve as its selected
window.  This option allows to control which window gets selected
instead.

The possible choices are 'mru' (the default) to select the most
recently used window on that frame, and 'pos' to choose the
window at the frame coordinates of point of the previously
selected window.  If this is nil, choose the frame's first window
instead.  A window with a non-nil `no-other-window' parameter is
chosen only if all windows on that frame have that parameter set
to a non-nil value."
  :type '(choice (const :tag "Most recently used" mru)
                 (const :tag "At position of deleted" pos)
                 (const :tag "Frame's first " nil))
  :group 'windows
  :group 'frames
  :version "28.1")

(defun delete-window (&optional window)
  "Delete specified WINDOW.
WINDOW must be a valid window and defaults to the selected one.
Return nil.

If the variable `ignore-window-parameters' is non-nil or the
`delete-window' parameter of WINDOW equals t, do not process any
parameters of WINDOW.  Otherwise, if the `delete-window'
parameter of WINDOW specifies a function, call that function with
WINDOW as its sole argument and return the value returned by that
function.

Otherwise, if WINDOW is part of an atomic window, call
`delete-window' with the root of the atomic window as its
argument.  Signal an error if WINDOW is either the only window on
its frame, the last non-side window, or part of an atomic window
that is its frame's root window.

If WINDOW is the selected window on its frame, choose some other
window as that frame's selected window according to the value of
the option `delete-window-choose-selected'."
  (interactive)
  (setq window (window-normalize-window window))
  (let* ((frame (window-frame window))
	 (function (window-parameter window 'delete-window))
	 (parent (window-parent window))
	 atom-root)
    (window--check frame)
    (catch 'done
      ;; Handle window parameters.
      (cond
       ;; Ignore window parameters if `ignore-window-parameters' tells
       ;; us so or `delete-window' equals t.
       ((or ignore-window-parameters (eq function t)))
       ((functionp function)
	;; The `delete-window' parameter specifies the function to call.
	;; If that function is `ignore' nothing is done.  It's up to the
	;; function called here to avoid infinite recursion.
	(throw 'done (funcall function window)))
       ((and (window-parameter window 'window-atom)
	     (setq atom-root (window-atom-root window))
	     (not (eq atom-root window)))
	(if (eq atom-root (frame-root-window frame))
	    (error "Root of atomic window is root window of its frame")
	  (throw 'done (delete-window atom-root))))
       ((not parent)
	(error "Attempt to delete minibuffer or sole ordinary window"))
       ((eq window (window-main-window frame))
	(error "Attempt to delete main window of frame %s" frame)))

      (let* ((horizontal (window-left-child parent))
	     (size (window-size window horizontal t))
             (window-combination-resize
              (or window-combination-resize
                  (window-parameter parent 'window-side)))
             (frame-selected-window (frame-selected-window frame))
	     ;; Emacs 23 preferably gives WINDOW's space to its left
	     ;; sibling.
	     (sibling (or (window-left window) (window-right window)))
             frame-selected-window-edges frame-selected-window-pos)
	(window--resize-reset frame horizontal)
	(cond
	 ((and (not (eq window-combination-resize t))
	       sibling (window-sizable-p sibling size horizontal nil t))
	  ;; Resize WINDOW's sibling.
	  (window--resize-this-window sibling size horizontal nil t)
	  (set-window-new-normal
	   sibling (+ (window-normal-size sibling horizontal)
		      (window-normal-size window horizontal))))
	 ((window--resizable-p window (- size) horizontal nil nil nil t t)
	  ;; Can do without resizing fixed-size windows.
	  (window--resize-siblings window (- size) horizontal))
	 (t
	  ;; Can't do without resizing fixed-size windows.
	  (window--resize-siblings window (- size) horizontal t)))

        (when (eq delete-window-choose-selected 'pos)
          ;; Remember edges and position of point of the selected window
          ;; of WINDOW'S frame.
          (setq frame-selected-window-edges
                (window-edges frame-selected-window nil nil t))
          (setq frame-selected-window-pos
                (nth 2 (posn-at-point nil frame-selected-window))))

	;; Actually delete WINDOW.
	(delete-window-internal window)
	(window--pixel-to-total frame horizontal)

        ;; If we deleted the selected window of WINDOW's frame, choose
        ;; another one based on `delete-window-choose-selected'.  Note
        ;; that both `window-at-x-y' and `get-mru-window' may fail to
        ;; produce a suitable window in which case we will fall back on
        ;; its frame's first window, chosen by `delete-window-internal'.
        (cond
         ((window-live-p frame-selected-window))
         ((and frame-selected-window-pos
               ;; We have a recorded position of point of the previously
               ;; selected window.  Try to find the window that is now
               ;; at that position.
               (let ((new-frame-selected-window
		      (window-at-x-y
                       (+ (nth 0 frame-selected-window-edges)
                          (car frame-selected-window-pos))
                       (+ (nth 1 frame-selected-window-edges)
                          (cdr frame-selected-window-pos))
                       frame t)))
                 (and new-frame-selected-window
                      ;; Select window at WINDOW's position at point.
	              (set-frame-selected-window
                       frame new-frame-selected-window)))))
         ((and (eq delete-window-choose-selected 'mru)
               ;; Try to use the most recently used window.
               (let ((mru-window (get-mru-window frame nil nil t)))
                 (and mru-window
	              (set-frame-selected-window frame mru-window)))))
         ((and (window-parameter
                (frame-selected-window frame) 'no-other-window)
               ;; If `delete-window-internal' selected a window with a
               ;; non-nil 'no-other-window' parameter as its frame's
               ;; selected window, try to choose another one.
               (catch 'found
                 (walk-window-tree
                  (lambda (other)
                    (unless (window-parameter other 'no-other-window)
                      (set-frame-selected-window frame other)
                      (throw 'found t)))
                  frame))))
         (t
          ;; Record the window chosen by `delete-window-internal'.
          (set-frame-selected-window
           frame (frame-selected-window frame))))

	(window--check frame)
	;; Always return nil.
	nil))))

(defun delete-other-windows (&optional window interactive)
  "Make WINDOW fill its frame.
WINDOW must be a valid window and defaults to the selected one.
Return nil.

If the variable `ignore-window-parameters' is non-nil or the
`delete-other-windows' parameter of WINDOW equals t, do not pay
attention to any other parameters of WINDOW.  Otherwise, if the
`delete-other-windows' parameter of WINDOW specifies a function,
call that function with WINDOW as its sole argument and return
the value returned by that function.

Else, if WINDOW is part of an atomic window, call this function
with the root of the atomic window as its argument.  Signal an
error if that root window is the root window of WINDOW's frame.
Also signal an error if WINDOW is a side window.  Do not delete
any window whose `no-delete-other-windows' parameter is non-nil."
  (interactive "i\np")
  (setq window (window-normalize-window window))
  (let* ((frame (window-frame window))
	 (function (window-parameter window 'delete-other-windows))
	 atom-root main)
    (window--check frame)
    (catch 'done
      (cond
       ;; Ignore window parameters if `ignore-window-parameters' is t or
       ;; `delete-other-windows' is t.
       ((or ignore-window-parameters (eq function t)))
       ((functionp function)
	;; The `delete-other-windows' parameter specifies the function
	;; to call.  If the function is `ignore' no windows are deleted.
	;; It's up to the function called to avoid infinite recursion.
	(throw 'done (funcall function window)))
       ((and (window-parameter window 'window-atom)
	     (setq atom-root (window-atom-root window))
	     (not (eq atom-root window)))
	(if (eq atom-root (frame-root-window frame))
	    (error "Root of atomic window is root window of its frame")
	  (throw 'done (delete-other-windows atom-root))))
       ((window-parameter window 'window-side)
	(error "Cannot make side window the only window"))
       ((and (window-minibuffer-p window)
	     (not (eq window (frame-root-window window))))
	(error "Can't expand minibuffer to full frame")))

      (cond
       ((or ignore-window-parameters
            (not (window-with-parameter 'no-delete-other-windows nil frame)))
        (setq main (frame-root-window frame)))
       ((catch 'tag
          (walk-window-tree
           (lambda (other)
             (when (or (and (window-parameter other 'window-side)
                            (not (window-parameter
                                  other 'no-delete-other-windows)))
                       (and (not (window-parameter other 'window-side))
                            (window-parameter
                             other 'no-delete-other-windows)))
               (throw 'tag nil)))
           nil nil 'nomini)
          t)
        (setq main (window-main-window frame)))
       (t
        ;; Delete windows via `delete-window' because we found either a
        ;; deletable side window or a non-deletable non-side-window.
        (dolist (other (window-list frame))
          (when (and (window-live-p other)
                     (not (eq other window))
                     (not (window-parameter
                           other 'no-delete-other-windows))
                     ;; When WINDOW and the other window are part of the
                     ;; same atomic window, don't delete the other.
                     (or (not atom-root)
                         (not (eq (window-atom-root other) atom-root))))
            (condition-case nil
                (delete-window other)
              (error nil))))
        (throw 'done nil)))

      ;; If WINDOW is the main window of its frame do nothing.
      (if (eq window main)
          ;; Give a message to the user if this has been called as a
          ;; command.
          (when (and interactive
                     (not (or executing-kbd-macro noninteractive)))
            (message "No other windows to delete"))
	(delete-other-windows-internal window main)
	(window--check frame))
      ;; Always return nil.
      nil)))

(defun delete-other-windows-vertically (&optional window)
  "Delete the windows in the same column with WINDOW, but not WINDOW itself.
This may be a useful alternative binding for \\[delete-other-windows]
 if you often split windows horizontally."
  (interactive)
  (let* ((window (or window (selected-window)))
         (edges (window-edges window))
         (w window) delenda)
    (while (not (eq (setq w (next-window w 1)) window))
      (let ((e (window-edges w)))
        (when (and (= (car e) (car edges))
                   (= (nth 2 e) (nth 2 edges)))
          (push w delenda))))
    (mapc 'delete-window delenda)))

;;; Windows and buffers.

;; `prev-buffers' and `next-buffers' are two reserved window slots used
;; for (1) determining which buffer to show in the window when its
;; buffer shall be buried or killed and (2) which buffer to show for
;; `switch-to-prev-buffer' and `switch-to-next-buffer'.

;; `prev-buffers' consists of <buffer, window-start, window-point>
;; triples.  The entries on this list are ordered by the time their
;; buffer has been removed from the window, the most recently removed
;; buffer's entry being first.  The window-start and window-point
;; components are `window-start' and `window-point' at the time the
;; buffer was removed from the window which implies that the entry must
;; be added when `set-window-buffer' removes the buffer from the window.

;; `next-buffers' is the list of buffers that have been replaced
;; recently by `switch-to-prev-buffer'.  These buffers are the least
;; preferred candidates of `switch-to-prev-buffer' and the preferred
;; candidates of `switch-to-next-buffer' to switch to.  This list is
;; reset to nil by any action changing the window's buffer with the
;; exception of `switch-to-prev-buffer' and `switch-to-next-buffer'.
;; `switch-to-prev-buffer' pushes the buffer it just replaced on it,
;; `switch-to-next-buffer' pops the last pushed buffer from it.

;; Both `prev-buffers' and `next-buffers' may reference killed buffers
;; if such a buffer was killed while the window was hidden within a
;; window configuration.  Such killed buffers get removed whenever
;; `switch-to-prev-buffer' or `switch-to-next-buffer' encounter them.

;; The following function is called by `set-window-buffer' _before_ it
;; replaces the buffer of the argument window with the new buffer.
(defun push-window-buffer-onto-prev (&optional window)
  "Push entry for WINDOW's buffer onto WINDOW's prev-buffers list.
WINDOW must be a live window and defaults to the selected one.

Any duplicate entries for the buffer in the list are removed."
  (let* ((window (window-normalize-window window t))
         (buffer (window-buffer window))
         (w-list (window-prev-buffers window))
         (entry (assq buffer w-list)))
    (when entry
      (setq w-list (assq-delete-all buffer w-list)))
    (let ((start (window-start window))
          (point (window-point window)))
      (setq entry
            (cons buffer
                  (with-current-buffer buffer
                    (if entry
                        ;; We have an entry, update marker positions.
                        (list (set-marker (nth 1 entry) start)
                              (set-marker (nth 2 entry) point))
                      (list (copy-marker start)
                            (copy-marker
                             ;; Preserve window-point-insertion-type
                             ;; (Bug#12855)
                             point window-point-insertion-type))))))
      (set-window-prev-buffers window (cons entry w-list)))))

(defun record-window-buffer (&optional window)
  "Record WINDOW's buffer.
WINDOW must be a live window and defaults to the selected one."
  (let* ((window (window-normalize-window window t))
         (buffer (window-buffer window)))
    ;; Reset WINDOW's next buffers.  If needed, they are resurrected by
    ;; `switch-to-prev-buffer' and `switch-to-next-buffer'.
    (set-window-next-buffers window nil)

    ;; Don't record insignificant buffers.
    (when (not (eq (aref (buffer-name buffer) 0) ?\s))
      (push-window-buffer-onto-prev window)
      (run-hooks 'buffer-list-update-hook))))

(defun unrecord-window-buffer (&optional window buffer)
  "Unrecord BUFFER in WINDOW.
WINDOW must be a live window and defaults to the selected one.
BUFFER must be a live buffer and defaults to the buffer of
WINDOW."
  (let* ((window (window-normalize-window window t))
	 (buffer (or buffer (window-buffer window))))
    (set-window-prev-buffers
     window (assq-delete-all buffer (window-prev-buffers window)))
    (set-window-next-buffers
     window (delq buffer (window-next-buffers window)))))

(defun set-window-buffer-start-and-point (window buffer &optional start point)
  "Set WINDOW's buffer to BUFFER.
WINDOW must be a live window and defaults to the selected one.
Optional argument START non-nil means set WINDOW's start position
to START.  Optional argument POINT non-nil means set WINDOW's
point to POINT.  If WINDOW is selected this also sets BUFFER's
`point' to POINT.  If WINDOW is selected and the buffer it showed
before was current this also makes BUFFER the current buffer."
  (setq window (window-normalize-window window t))
  (let ((selected (eq window (selected-window)))
	(current (eq (window-buffer window) (current-buffer)))
        (dedicated-side (eq (window-dedicated-p window) 'side)))
    (set-window-buffer window buffer)
    (and dedicated-side (set-window-dedicated-p window 'side))
    (when (and selected current)
      (set-buffer buffer))
    (when start
      ;; Don't force window-start here (even if POINT is nil).
      (set-window-start window start t))
    (when point
      (set-window-point window point))))

(defcustom switch-to-visible-buffer t
  "If non-nil, allow switching to an already visible buffer.
If this variable is non-nil, `switch-to-prev-buffer' and
`switch-to-next-buffer' may switch to an already visible buffer.
If this variable is nil, `switch-to-prev-buffer' and
`switch-to-next-buffer' always try to avoid switching to a buffer
that is already visible in another window on the same frame."
  :type 'boolean
  :version "24.1"
  :group 'windows)

(make-obsolete-variable 'switch-to-visible-buffer
                        'switch-to-prev-buffer-skip "27.1")

(defcustom switch-to-prev-buffer-skip nil
  "Buffers `switch-to-prev-buffer' should skip.
If this variable is nil, `switch-to-prev-buffer' may switch to
any buffer, including those already shown in other windows.

If this variable is non-nil, `switch-to-prev-buffer' will refrain
from switching to certain buffers according to the value of this
variable:

- `this' means do not switch to a buffer shown on the frame that
  hosts the window `switch-to-prev-buffer' is acting upon.

- `visible' means do not switch to a buffer shown on any visible
  frame.

- 0 (the number zero) means do not switch to a buffer shown on
  any visible or iconified frame.

- t means do not switch to a buffer shown on any live frame.

If this option specifies a function, that function is called with
three arguments - the WINDOW argument of `switch-to-prev-buffer',
a buffer `switch-to-prev-buffer' intends to switch to and the
BURY-OR-KILL argument of `switch-to-prev-buffer'.  If that
function returns non-nil, `switch-to-prev-buffer' will not switch
to that buffer.

Since `switch-to-prev-buffer' is called by `bury-buffer',
`replace-buffer-in-windows' and `quit-restore-window' among
others, customizing this option may also affect the behavior of
Emacs when a window is quit or a buffer gets buried or killed.

The value of this option is consulted by `switch-to-next-buffer'
as well.  In that case, if this option specifies a function, it
will be called with the third argument nil.

Under certain circumstances `switch-to-prev-buffer' may ignore
this option, for example, when there is only one buffer left."
  :type
  '(choice (const :tag "Never" nil)
           (const :tag "This frame" this)
	   (const :tag "Visible frames" visible)
	   (const :tag "Visible and iconified frames" 0)
	   (const :tag "Any frame" t)
           (function :tag "Function"))
  :version "27.1"
  :group 'windows)

(defun switch-to-prev-buffer-skip-p (skip window buffer &optional bury-or-kill)
  "Return non-nil if `switch-to-prev-buffer' should skip BUFFER.
SKIP is a value derived from `switch-to-prev-buffer-skip', WINDOW
the window `switch-to-prev-buffer' acts upon.  Optional argument
BURY-OR-KILL is passed unchanged by `switch-to-prev-buffer' and
omitted in calls from `switch-to-next-buffer'."
  (when skip
    (if (functionp skip)
        (funcall skip window buffer bury-or-kill)
      (get-buffer-window buffer skip))))

(defun switch-to-prev-buffer (&optional window bury-or-kill)
  "In WINDOW switch to previous buffer.
WINDOW must be a live window and defaults to the selected one.
Return the buffer switched to, nil if no suitable buffer could be
found.

Optional argument BURY-OR-KILL non-nil means the buffer currently
shown in WINDOW is about to be buried or killed and consequently
shall not be switched to in future invocations of this command.

As a special case, if BURY-OR-KILL equals `append', this means to
move the buffer to the end of WINDOW's previous buffers list so a
future invocation of `switch-to-prev-buffer' less likely switches
to it.

The option `switch-to-prev-buffer-skip' can be used to not switch
to certain buffers, for example, to those already shown in
another window.  Also, if WINDOW's frame has a `buffer-predicate'
parameter, that predicate may inhibit switching to certain
buffers.

This function is called by `previous-buffer'."
  (interactive)
  (let* ((window (window-normalize-window window t))
	 (frame (window-frame window))
         (window-side (window-parameter window 'window-side))
	 (old-buffer (window-buffer window))
	 ;; Save this since it's destroyed by `set-window-buffer'.
	 (next-buffers (window-next-buffers window))
         (pred (frame-parameter frame 'buffer-predicate))
         (skip
          (cond
           ((or (functionp switch-to-prev-buffer-skip)
                (memq switch-to-prev-buffer-skip '(t visible 0)))
            switch-to-prev-buffer-skip)
           ((or switch-to-prev-buffer-skip
                (not switch-to-visible-buffer))
            frame)))
         entry new-buffer killed-buffers skipped)
    (when (window-minibuffer-p window)
      ;; Don't switch in minibuffer window.
      (unless (setq window (minibuffer-selected-window))
	(error "Window %s is a minibuffer window" window)))

    (unless (memq (window-dedicated-p window) '(nil side))
      ;; Don't switch in dedicated window.
      (error "Window %s is dedicated to buffer %s" window old-buffer))

    (catch 'found
      ;; Scan WINDOW's previous buffers first, skipping entries of next
      ;; buffers.
      (dolist (entry (window-prev-buffers window))
	(when (and (not (eq (car entry) old-buffer))
                   (setq new-buffer (car entry))
		   (or (buffer-live-p new-buffer)
		       (not (setq killed-buffers
				  (cons new-buffer killed-buffers))))
                   (or (null pred) (funcall pred new-buffer))
		   ;; When BURY-OR-KILL is nil, avoid switching to a
		   ;; buffer in WINDOW's next buffers list.
		   (or bury-or-kill (not (memq new-buffer next-buffers))))
          (if (switch-to-prev-buffer-skip-p skip window new-buffer bury-or-kill)
	      (setq skipped new-buffer)
	    (set-window-buffer-start-and-point
	     window new-buffer (nth 1 entry) (nth 2 entry))
	    (throw 'found t))))
      ;; Scan reverted buffer list of WINDOW's frame next, skipping
      ;; entries of next buffers.  Note that when we bury or kill a
      ;; buffer we don't reverse the global buffer list to avoid showing
      ;; a buried buffer instead.  Otherwise, we must reverse the global
      ;; buffer list in order to make sure that switching to the
      ;; previous/next buffer traverse it in opposite directions.  Skip
      ;; this step for side windows.
      (unless window-side
        (dolist (buffer (if bury-or-kill
                            (buffer-list frame)
                          (nreverse (buffer-list frame))))
          (when (and (buffer-live-p buffer)
                     (not (eq buffer old-buffer))
                     (or (null pred) (funcall pred buffer))
                     ;; Skip buffers whose names start with a space.
                     (not (eq (aref (buffer-name buffer) 0) ?\s))
                     ;; Skip buffers shown in a side window before.
                     (not (buffer-local-value 'window--sides-shown buffer))
                     (or bury-or-kill (not (memq buffer next-buffers))))
            (if (switch-to-prev-buffer-skip-p skip window buffer bury-or-kill)
	        (setq skipped (or skipped buffer))
              (setq new-buffer buffer)
              (set-window-buffer-start-and-point window new-buffer)
              (throw 'found t)))))

      (unless bury-or-kill
	;; Scan reverted next buffers last (must not use nreverse
	;; here!).
	(dolist (buffer (reverse next-buffers))
	  ;; Actually, buffer _must_ be live here since otherwise it
	  ;; would have been caught in the scan of previous buffers.
	  (when (and (or (buffer-live-p buffer)
			 (not (setq killed-buffers
				    (cons buffer killed-buffers))))
		     (not (eq buffer old-buffer))
                     (or (null pred) (funcall pred buffer))
		     (setq entry (assq buffer (window-prev-buffers window))))
            (if (switch-to-prev-buffer-skip-p skip window buffer bury-or-kill)
	        (setq skipped (or skipped buffer))
	      (setq new-buffer buffer)
	      (set-window-buffer-start-and-point
	       window new-buffer (nth 1 entry) (nth 2 entry))
	      (throw 'found t)))))

      (when (and skipped (not (functionp switch-to-prev-buffer-skip)))
        ;; Show first skipped buffer, unless skip was a function.
	(setq new-buffer skipped)
	(set-window-buffer-start-and-point window new-buffer)))

    (if bury-or-kill
	(let ((entry (and (eq bury-or-kill 'append)
			  (assq old-buffer (window-prev-buffers window)))))
	  ;; Remove `old-buffer' from WINDOW's previous and (restored list
	  ;; of) next buffers.
	  (set-window-prev-buffers
	   window (assq-delete-all old-buffer (window-prev-buffers window)))
	  (set-window-next-buffers window (delq old-buffer next-buffers))
	  (when entry
	    ;; Append old-buffer's entry to list of WINDOW's previous
	    ;; buffers so it's less likely to get switched to soon but
	    ;; `display-buffer-in-previous-window' can nevertheless find
	    ;; it.
	    (set-window-prev-buffers
	     window (append (window-prev-buffers window) (list entry)))))
      ;; Move `old-buffer' to head of WINDOW's restored list of next
      ;; buffers.
      (set-window-next-buffers
       window (cons old-buffer (delq old-buffer next-buffers))))

    ;; Remove killed buffers from WINDOW's previous and next buffers.
    (when killed-buffers
      (dolist (buffer killed-buffers)
	(set-window-prev-buffers
	 window (assq-delete-all buffer (window-prev-buffers window)))
	(set-window-next-buffers
	 window (delq buffer (window-next-buffers window)))))

    ;; Return new-buffer.
    new-buffer))

(defun switch-to-next-buffer (&optional window)
  "In WINDOW switch to next buffer.
WINDOW must be a live window and defaults to the selected one.
Return the buffer switched to, nil if no suitable buffer could be
found.

The option `switch-to-prev-buffer-skip' can be used to not switch
to certain buffers, for example, to those already shown in
another window.  Also, if WINDOW's frame has a `buffer-predicate'
parameter, that predicate may inhibit switching to certain
buffers.

This function is called by `next-buffer'."
  (interactive)
  (let* ((window (window-normalize-window window t))
	 (frame (window-frame window))
         (window-side (window-parameter window 'window-side))
	 (old-buffer (window-buffer window))
	 (next-buffers (window-next-buffers window))
         (pred (frame-parameter frame 'buffer-predicate))
         (skip
          (cond
           ((or (functionp switch-to-prev-buffer-skip)
                (memq switch-to-prev-buffer-skip '(t visible 0)))
            switch-to-prev-buffer-skip)
           ((or switch-to-prev-buffer-skip
                (not switch-to-visible-buffer))
            frame)))
	 new-buffer entry killed-buffers skipped)
    (when (window-minibuffer-p window)
      ;; Don't switch in minibuffer window.
      (unless (setq window (minibuffer-selected-window))
	(error "Window %s is a minibuffer window" window)))

    (unless (memq (window-dedicated-p window) '(nil side))
      ;; Don't switch in dedicated window.
      (error "Window %s is dedicated to buffer %s" window old-buffer))

    (catch 'found
      ;; Scan WINDOW's next buffers first.
      (dolist (buffer next-buffers)
	(when (and (or (buffer-live-p buffer)
		       (not (setq killed-buffers
				  (cons buffer killed-buffers))))
		   (not (eq buffer old-buffer))
                   (or (null pred) (funcall pred buffer))
		   (setq entry (assq buffer (window-prev-buffers window))))
          (if (switch-to-prev-buffer-skip-p skip window buffer)
	      (setq skipped buffer)
	    (setq new-buffer buffer)
	    (set-window-buffer-start-and-point
	     window new-buffer (nth 1 entry) (nth 2 entry))
	    (throw 'found t))))
      ;; Scan the buffer list of WINDOW's frame next, skipping previous
      ;; buffers entries.  Skip this step for side windows.
      (unless window-side
        (dolist (buffer (buffer-list frame))
          (when (and (buffer-live-p buffer)
                     (not (eq buffer old-buffer))
                     (or (null pred) (funcall pred buffer))
                     ;; Skip buffers whose names start with a space.
                     (not (eq (aref (buffer-name buffer) 0) ?\s))
                     ;; Skip buffers shown in a side window before.
                     (not (buffer-local-value 'window--sides-shown buffer))
                     (not (assq buffer (window-prev-buffers window))))
            (if (switch-to-prev-buffer-skip-p skip window buffer)
	        (setq skipped (or skipped buffer))
              (setq new-buffer buffer)
              (set-window-buffer-start-and-point window new-buffer)
              (throw 'found t)))))
      ;; Scan WINDOW's reverted previous buffers last (must not use
      ;; nreverse here!)
      (dolist (entry (reverse (window-prev-buffers window)))
	(when (and (not (eq new-buffer (car entry)))
                   (not (eq old-buffer (car entry)))
                   (setq new-buffer (car entry))
		   (or (buffer-live-p new-buffer)
		       (not (setq killed-buffers
				  (cons new-buffer killed-buffers))))
                   (or (null pred) (funcall pred new-buffer)))
          (if (switch-to-prev-buffer-skip-p skip window new-buffer)
	      (setq skipped (or skipped new-buffer))
	    (set-window-buffer-start-and-point
	     window new-buffer (nth 1 entry) (nth 2 entry))
	    (throw 'found t))))

      (when (and skipped (not (functionp switch-to-prev-buffer-skip)))
        ;; Show first skipped buffer, unless skip was a function.
	(setq new-buffer skipped)
	(set-window-buffer-start-and-point window new-buffer)))

    ;; Remove `new-buffer' from and restore WINDOW's next buffers.
    (set-window-next-buffers window (delq new-buffer next-buffers))

    ;; Remove killed buffers from WINDOW's previous and next buffers.
    (when killed-buffers
      (dolist (buffer killed-buffers)
	(set-window-prev-buffers
	 window (assq-delete-all buffer (window-prev-buffers window)))
	(set-window-next-buffers
	 window (delq buffer (window-next-buffers window)))))

    ;; Return new-buffer.
    new-buffer))

(defun get-next-valid-buffer (list &optional buffer visible-ok frame)
  "Search LIST for a valid buffer to display in FRAME.
Return nil when all buffers in LIST are undesirable for display,
otherwise return the first suitable buffer in LIST.

Buffers not visible in windows are preferred to visible buffers,
unless VISIBLE-OK is non-nil.
If the optional argument FRAME is nil, it defaults to the selected frame.
If BUFFER is non-nil, ignore occurrences of that buffer in LIST."
  ;; This logic is more or less copied from other-buffer.
  (setq frame (or frame (selected-frame)))
  (let ((pred (frame-parameter frame 'buffer-predicate))
	found buf)
    (while (and (not found) list)
      (setq buf (car list))
      (if (and (not (eq buffer buf))
	       (buffer-live-p buf)
	       (or (null pred) (funcall pred buf))
	       (not (eq (aref (buffer-name buf) 0) ?\s))
	       (or visible-ok (null (get-buffer-window buf 'visible))))
	  (setq found buf)
	(setq list (cdr list))))
    (car list)))

(defun last-buffer (&optional buffer visible-ok frame)
  "Return the last buffer in FRAME's buffer list.
If BUFFER is the last buffer, return the preceding buffer
instead.  Buffers not visible in windows are preferred to visible
buffers, unless optional argument VISIBLE-OK is non-nil.
Optional third argument FRAME nil or omitted means use the
selected frame's buffer list.  If no such buffer exists, return
the buffer `*scratch*', creating it if necessary."
  (setq frame (or frame (selected-frame)))
  (or (get-next-valid-buffer (nreverse (buffer-list frame))
 			     buffer visible-ok frame)
      (get-buffer "*scratch*")
      (let ((scratch (get-buffer-create "*scratch*")))
	(set-buffer-major-mode scratch)
	scratch)))

(defcustom frame-auto-hide-function #'iconify-frame
  "Function called to automatically hide frames.
The function is called with one argument - a frame.

Functions affected by this option are those that bury a buffer
shown in a separate frame like `quit-window' and `bury-buffer'."
  :type '(choice (const :tag "Iconify" iconify-frame)
                 (const :tag "Make invisible" make-frame-invisible)
                 (const :tag "Delete" delete-frame)
                 (const :tag "Do nothing" ignore)
                 function)
  :group 'windows
  :group 'frames
  :version "26.1")

(defun window--delete (&optional window dedicated-only kill)
  "Delete WINDOW if possible.
WINDOW must be a live window and defaults to the selected one.
Optional argument DEDICATED-ONLY non-nil means to delete WINDOW
only if it's dedicated to its buffer.  Optional argument KILL
means the buffer shown in window will be killed.  Return non-nil
if WINDOW gets deleted or its frame is auto-hidden."
  (setq window (window-normalize-window window t))
  (unless (and dedicated-only (not (window-dedicated-p window)))
    (let ((deletable (window-deletable-p window)))
      (cond
       ((eq deletable 'frame)
	(let ((frame (window-frame window)))
	  (cond
	   (kill
	    (delete-frame frame))
           ((functionp (frame-parameter frame 'auto-hide-function))
            (funcall (frame-parameter frame 'auto-hide-function)))
           ((functionp frame-auto-hide-function)
	    (funcall frame-auto-hide-function frame))))
	'frame)
       (deletable
	(delete-window window)
	t)))))

(defun bury-buffer (&optional buffer-or-name)
  "Put BUFFER-OR-NAME at the end of the list of all buffers.
There it is the least likely candidate for `other-buffer' to
return; thus, the least likely buffer for \\[switch-to-buffer] to
select by default.

You can specify a buffer name as BUFFER-OR-NAME, or an actual
buffer object.  If BUFFER-OR-NAME is nil or omitted, bury the
current buffer.  Also, if BUFFER-OR-NAME is nil or omitted,
remove the current buffer from the selected window if it is
displayed there."
  (interactive)
  (let* ((buffer (window-normalize-buffer buffer-or-name)))
    ;; If `buffer-or-name' is not on the selected frame we unrecord it
    ;; although it's not "here" (call it a feature).
    (bury-buffer-internal buffer)
    ;; Handle case where `buffer-or-name' is nil and the current buffer
    ;; is shown in the selected window.
    (cond
     ((or buffer-or-name
          (not (eq buffer (window-buffer)))
          ;; Don't try to delete the minibuffer window, undedicate it
          ;; or switch to a previous buffer in it.
          (window-minibuffer-p)))
     ((window--delete nil t))
     (t
      ;; Switch to another buffer in window.
      (set-window-dedicated-p nil nil)
      (switch-to-prev-buffer nil 'bury)))
    ;; Always return nil.
    nil))

(defun unbury-buffer ()
  "Switch to the last buffer in the buffer list."
  (interactive)
  (switch-to-buffer (last-buffer)))

(defun next-buffer (&optional arg interactive)
  "In selected window switch to ARGth next buffer.
Call `switch-to-next-buffer' unless the selected window is the
minibuffer window or is dedicated to its buffer."
  (interactive "p\np")
  (cond
   ((window-minibuffer-p)
    (user-error "Cannot switch buffers in minibuffer window"))
   ((eq (window-dedicated-p) t)
    (user-error "Window is strongly dedicated to its buffer"))
   (t
    (dotimes (_ (or arg 1))
      (when (and (not (switch-to-next-buffer))
                 interactive
                 (not (or executing-kbd-macro noninteractive)))
        (user-error "No next buffer"))))))

(defun previous-buffer (&optional arg interactive)
  "In selected window switch to ARGth previous buffer.
Call `switch-to-prev-buffer' unless the selected window is the
minibuffer window or is dedicated to its buffer."
  (interactive "p\np")
  (cond
   ((window-minibuffer-p)
    (user-error "Cannot switch buffers in minibuffer window"))
   ((eq (window-dedicated-p) t)
    (user-error "Window is strongly dedicated to its buffer"))
   (t
    (dotimes (_ (or arg 1))
      (when (and (not (switch-to-prev-buffer))
                 interactive
                 (not (or executing-kbd-macro noninteractive)))
        (user-error "No previous buffer"))))))

(defun delete-windows-on (&optional buffer-or-name frame)
  "Delete all windows showing BUFFER-OR-NAME.
BUFFER-OR-NAME may be a buffer or the name of an existing buffer
and defaults to the current buffer.

Interactively, prompt for the buffer.

The following non-nil values of the optional argument FRAME
have special meanings:

- t means consider all windows on the selected frame only.

- `visible' means consider all windows on all visible frames on
  the current terminal.

- 0 (the number zero) means consider all windows on all visible
  and iconified frames on the current terminal.

- A frame means consider all windows on that frame only.

Any other value of FRAME means consider all windows on all
frames.

Interactively, FRAME is the prefix argument, so you can
use \\[universal-argument] 0 to specify all windows only on
the current terminal's frames.

If a frame's root window shows the buffer specified by
BUFFER-OR-NAME and is dedicated to that buffer and that frame
does not host the active minibuffer window and there is at least
one other frame on that frame's terminal, delete that frame.
Otherwise, do not delete a frame's root window if it shows the
buffer specified by BUFFER-OR-NAME and do not delete any frame's
main window showing that buffer either.  Rather, in any such
case, call `switch-to-prev-buffer' to show another buffer in that
window and make sure the window is no more dedicated to its
buffer.

If the buffer specified by BUFFER-OR-NAME is shown in a
minibuffer window, do nothing for that window.  For any window
that does not show that buffer, remove the buffer from that
window's lists of previous and next buffers."
  (interactive "bDelete windows on (buffer):\nP")
  (let ((buffer (window-normalize-buffer buffer-or-name))
	;; Handle the "inverted" meaning of the FRAME argument wrt other
	;; `window-list-1' based function.
	(all-frames (cond ((not frame) t) ((eq frame t) nil) (t frame))))
    (dolist (window (window-list-1 nil nil all-frames))
      (if (eq (window-buffer window) buffer)
	  (let ((deletable (window-deletable-p window))
                (dedicated (window-dedicated-p window)))
	    (cond
	     ((and (eq deletable 'frame) dedicated)
	      ;; Delete frame if and only if window is dedicated.
	      (delete-frame (window-frame window)))
	     ((eq deletable t)
	      ;; Delete window.
	      (delete-window window))
	     (t
	      ;; In window switch to previous buffer.
	      (set-window-dedicated-p window nil)
	      (switch-to-prev-buffer window 'bury)
              ;; Restore the dedicated 'side' flag.
              (when (eq dedicated 'side)
                (set-window-dedicated-p window 'side)))))
	;; If a window doesn't show BUFFER, unrecord BUFFER in it.
	(unrecord-window-buffer window buffer)))))

(defun replace-buffer-in-windows (&optional buffer-or-name)
  "Replace BUFFER-OR-NAME with some other buffer in all windows showing it.
BUFFER-OR-NAME may be a buffer or the name of an existing buffer
and defaults to the current buffer.

With the exception of side windows, when a window showing BUFFER-OR-NAME
is dedicated, that window is deleted.  If that window is the only window
on its frame, the frame is deleted too when there are other frames left.
If there are no other frames left, some other buffer is displayed in that
window.

This function removes the buffer denoted by BUFFER-OR-NAME from
all window-local buffer lists."
  (interactive "bBuffer to replace: ")
  (let ((buffer (window-normalize-buffer buffer-or-name)))
    (dolist (window (window-list-1 nil nil t))
      (if (eq (window-buffer window) buffer)
          ;; Delete a dedicated window unless it is a side window.
          (let ((dedicated-side (eq (window-dedicated-p window) 'side)))
            (when (or dedicated-side (not (window--delete window t t)))
              ;; Switch to another buffer in that window.
              (set-window-dedicated-p window nil)
              (if (switch-to-prev-buffer window 'kill)
                  (and dedicated-side (set-window-dedicated-p window 'side))
                (window--delete window nil 'kill))))
	;; Unrecord BUFFER in WINDOW.
	(unrecord-window-buffer window buffer)))))

(defcustom quit-window-hook nil
  "Hook run before performing any other actions in the `quit-window' command."
  :type 'hook
  :version "27.1"
  :group 'windows)

(defun quit-restore-window (&optional window bury-or-kill)
  "Quit WINDOW and deal with its buffer.
WINDOW must be a live window and defaults to the selected one.

According to information stored in WINDOW's `quit-restore' window
parameter either (1) delete WINDOW and its frame, (2) delete
WINDOW but leave its frame alone, (3) restore the buffer
previously shown in WINDOW, or (4) make WINDOW display some other
buffer.  If WINDOW is not deleted, reset its `quit-restore'
parameter to nil.  See Info node `(elisp) Quitting Windows' for
more details.

If WINDOW's dedicated flag is t, try to delete WINDOW.  If it
equals the value 'side', restore that value when WINDOW is not
deleted.

Optional second argument BURY-OR-KILL tells how to proceed with
the buffer of WINDOW.  The following values are handled:

nil means to not handle the buffer in a particular way.  This
  means that if WINDOW is not deleted by this function, invoking
  `switch-to-prev-buffer' will usually show the buffer again.

`append' means that if WINDOW is not deleted, move its buffer to
  the end of WINDOW's previous buffers so it's less likely that a
  future invocation of `switch-to-prev-buffer' will switch to it.
  Also, move the buffer to the end of the frame's buffer list.

`bury' means that if WINDOW is not deleted, remove its buffer
  from WINDOW'S list of previous buffers.  Also, move the buffer
  to the end of the frame's buffer list.  This value provides the
  most reliable remedy to not have `switch-to-prev-buffer' switch
  to this buffer again without killing the buffer.

`kill' means to kill WINDOW's buffer."
  (setq window (window-normalize-window window t))
  (let* ((buffer (window-buffer window))
	 (quit-restore (window-parameter window 'quit-restore))
         (prev-buffer (catch 'prev-buffer
                        (dolist (buf (window-prev-buffers window))
                          (unless (eq (car buf) buffer)
                            (throw 'prev-buffer (car buf))))))
         (dedicated (window-dedicated-p window))
	 quad entry)
    (cond
     ;; First try to delete dedicated windows that are not side windows.
     ((and dedicated (not (eq dedicated 'side))
           (window--delete window 'dedicated (eq bury-or-kill 'kill)))
      ;; If the previously selected window is still alive, select it.
      (when (window-live-p (nth 2 quit-restore))
        (select-window (nth 2 quit-restore))))
     ((and (not prev-buffer)
	   (eq (nth 1 quit-restore) 'tab)
	   (eq (nth 3 quit-restore) buffer))
      (tab-bar-close-tab)
      ;; If the previously selected window is still alive, select it.
      (when (window-live-p (nth 2 quit-restore))
	(select-window (nth 2 quit-restore))))
     ((and (not prev-buffer)
	   (or (eq (nth 1 quit-restore) 'frame)
	       (and (eq (nth 1 quit-restore) 'window)
		    ;; If the window has been created on an existing
		    ;; frame and ended up as the sole window on that
		    ;; frame, do not delete it (Bug#12764).
		    (not (eq window (frame-root-window window)))))
	   (eq (nth 3 quit-restore) buffer)
	   ;; Delete WINDOW if possible.
	   (window--delete window nil (eq bury-or-kill 'kill)))
      ;; If the previously selected window is still alive, select it.
      (when (window-live-p (nth 2 quit-restore))
	(select-window (nth 2 quit-restore))))
     ((and (listp (setq quad (nth 1 quit-restore)))
	   (buffer-live-p (car quad))
	   (eq (nth 3 quit-restore) buffer))
      ;; Show another buffer stored in quit-restore parameter.
      (when (and (integerp (nth 3 quad))
		 (if (window-combined-p window)
                     (/= (nth 3 quad) (window-total-height window))
                   (/= (nth 3 quad) (window-total-width window))))
	;; Try to resize WINDOW to its old height but don't signal an
	;; error.
	(condition-case nil
	    (window-resize
             window
             (- (nth 3 quad) (if (window-combined-p window)
                                 (window-total-height window)
                               (window-total-width window)))
             (window-combined-p window t))
	  (error nil)))
      (set-window-dedicated-p window nil)
      ;; Restore WINDOW's previous buffer, start and point position.
      (set-window-buffer-start-and-point
       window (nth 0 quad) (nth 1 quad) (nth 2 quad))
      ;; Restore the 'side' dedicated flag as well.
      (when (eq dedicated 'side)
        (set-window-dedicated-p window 'side))
      ;; Deal with the buffer we just removed from WINDOW.
      (setq entry (and (eq bury-or-kill 'append)
		       (assq buffer (window-prev-buffers window))))
      (when bury-or-kill
	;; Remove buffer from WINDOW's previous and next buffers.
	(set-window-prev-buffers
	 window (assq-delete-all buffer (window-prev-buffers window)))
	(set-window-next-buffers
	 window (delq buffer (window-next-buffers window))))
      (when entry
	;; Append old buffer's entry to list of WINDOW's previous
	;; buffers so it's less likely to get switched to soon but
	;; `display-buffer-in-previous-window' can nevertheless find it.
	(set-window-prev-buffers
	 window (append (window-prev-buffers window) (list entry))))
      ;; Reset the quit-restore parameter.
      (set-window-parameter window 'quit-restore nil)
      ;; Select old window.
      (when (window-live-p (nth 2 quit-restore))
	(select-window (nth 2 quit-restore))))
     (t
      ;; Show some other buffer in WINDOW and reset the quit-restore
      ;; parameter.
      (set-window-parameter window 'quit-restore nil)
      ;; Make sure that WINDOW is no more dedicated.
      (set-window-dedicated-p window nil)
      ;; Try to switch to a previous buffer.  Delete the window only if
      ;; that is not possible (Bug#48367).
      (if (switch-to-prev-buffer window bury-or-kill)
          (when (eq dedicated 'side)
            (set-window-dedicated-p window 'side))
        (window--delete window nil (eq bury-or-kill 'kill))
        (when (window-live-p (nth 2 quit-restore))
          (select-window (nth 2 quit-restore))))))

    ;; Deal with the buffer.
    (cond
     ((not (buffer-live-p buffer)))
     ((eq bury-or-kill 'kill)
      (kill-buffer buffer))
     (bury-or-kill
      (bury-buffer-internal buffer)))))

(defun quit-window (&optional kill window)
  "Quit WINDOW and bury its buffer.
WINDOW must be a live window and defaults to the selected one.
With prefix argument KILL non-nil, kill the buffer instead of
burying it.

This calls the function `quit-restore-window' to delete WINDOW or
show some other buffer in it.  See Info node `(elisp) Quitting
Windows' for more details.

The functions in `quit-window-hook' will be run before doing
anything else."
  (interactive "P")
  ;; Run the hook from the buffer implied to get any buffer-local
  ;; values.
  (with-current-buffer (window-buffer (window-normalize-window window))
    (run-hooks 'quit-window-hook))
  (quit-restore-window window (if kill 'kill 'bury)))

(defun quit-windows-on (&optional buffer-or-name kill frame)
  "Quit all windows showing BUFFER-OR-NAME.
BUFFER-OR-NAME may be a buffer or the name of an existing buffer
and defaults to the current buffer.  Optional argument KILL
non-nil means to kill BUFFER-OR-NAME.  KILL nil means to bury
BUFFER-OR-NAME.  Optional argument FRAME is handled as by
`delete-windows-on'.

This function calls `quit-window' on all candidate windows
showing BUFFER-OR-NAME."
  (interactive "bQuit windows on (buffer):\nP")
  (let ((buffer (window-normalize-buffer buffer-or-name))
	;; Handle the "inverted" meaning of the FRAME argument wrt other
	;; `window-list-1' based function.
	(all-frames (cond ((not frame) t) ((eq frame t) nil) (t frame))))
    (dolist (window (window-list-1 nil nil all-frames))
      (if (eq (window-buffer window) buffer)
	  (quit-window kill window)
	;; If a window doesn't show BUFFER, unrecord BUFFER in it.
	(unrecord-window-buffer window buffer)))))

(defun window--combination-resizable (parent &optional horizontal)
  "Return number of pixels recoverable from height of window PARENT.
PARENT must be a vertical (horizontal if HORIZONTAL is non-nil)
window combination.  The return value is the sum of the pixel
heights of all non-fixed height child windows of PARENT divided
by their number plus 1.  If HORIZONTAL is non-nil, return the sum
of the pixel widths of all non-fixed width child windows of
PARENT divided by their number plus 1."
  (let ((sibling (window-child parent))
	(number 0)
	(size 0))
    (while sibling
      (unless (window-size-fixed-p sibling horizontal)
	(setq number (1+ number))
	(setq size (+ (window-size sibling horizontal t) size)))
      (setq sibling (window-next-sibling sibling)))
    (/ size (1+ number))))

(defun split-window (&optional window size side pixelwise)
  "Make a new window adjacent to WINDOW.
WINDOW must be a valid window and defaults to the selected one.
Return the new window which is always a live window.

Optional argument SIZE a positive number means make WINDOW SIZE
lines or columns tall.  If SIZE is negative, make the new window
-SIZE lines or columns tall.  If and only if SIZE is non-nil, its
absolute value can be less than `window-min-height' or
`window-min-width'; so this command can make a new window as
small as one line or two columns.  SIZE defaults to half of
WINDOW's size.

Optional third argument SIDE nil (or `below') specifies that the
new window shall be located below WINDOW.  SIDE `above' means the
new window shall be located above WINDOW.  In both cases SIZE
specifies the new number of lines for WINDOW (or the new window
if SIZE is negative) including space reserved for the mode and/or
header line.

SIDE t (or `right') specifies that the new window shall be
located on the right side of WINDOW.  SIDE `left' means the new
window shall be located on the left of WINDOW.  In both cases
SIZE specifies the new number of columns for WINDOW (or the new
window provided SIZE is negative) including space reserved for
fringes and the scrollbar or a divider column.

For compatibility reasons, SIDE `up' and `down' are interpreted
as `above' and `below'.  Any other non-nil value for SIDE is
currently handled like t (or `right').

PIXELWISE, if non-nil, means to interpret SIZE pixelwise.

If the variable `ignore-window-parameters' is non-nil or the
`split-window' parameter of WINDOW equals t, do not process any
parameters of WINDOW.  Otherwise, if the `split-window' parameter
of WINDOW specifies a function, call that function with all three
arguments and return the value returned by that function.

Otherwise, if WINDOW is part of an atomic window, \"split\" the
root of that atomic window.  The new window does not become a
member of that atomic window.

If WINDOW is live, properties of the new window like margins and
scrollbars are inherited from WINDOW.  If WINDOW is an internal
window, these properties as well as the buffer displayed in the
new window are inherited from the window selected on WINDOW's
frame.  The selected window is not changed by this function."
  (setq window (window-normalize-window window))
  (let* ((side (cond
		((not side) 'below)
                ((eq side 'up) 'above)
                ((eq side 'down) 'below)
		((memq side '(below above right left)) side)
		(t 'right)))
	 (horizontal (not (memq side '(below above))))
	 (frame (window-frame window))
	 (parent (window-parent window))
	 (function (window-parameter window 'split-window))
	 (window-side (window-parameter window 'window-side))
	 ;; Rebind the following two variables since in some cases we
	 ;; have to override their value.
	 (window-combination-limit window-combination-limit)
	 (window-combination-resize window-combination-resize)
	 (char-size (frame-char-size window horizontal))
	 (pixel-size
	  (when (numberp size)
	    (window--size-to-pixel window size horizontal pixelwise t)))
	 (divider-width (if horizontal
			    (frame-right-divider-width frame)
			  (frame-bottom-divider-width frame)))
	 atom-root ignore)
    (window--check frame)
    (catch 'done
      (cond
       ;; Ignore window parameters if either `ignore-window-parameters'
       ;; is t or the 'split-window' parameter equals t.
       ((or ignore-window-parameters (eq function t)))
       ((functionp function)
	;; The 'split-window' parameter specifies the function to call.
	;; If that function is `ignore', do nothing.
	(throw 'done (funcall function window size side)))
       ;; If WINDOW is part of an atomic window, split the root window
       ;; of that atomic window instead.
       ((and (window-parameter window 'window-atom)
	     (setq atom-root (window-atom-root window))
	     (not (eq atom-root window)))
	(throw 'done (split-window atom-root size side pixelwise)))
       ;; If WINDOW is a side window or its first or last child is a
       ;; side window, throw an error unless `window-combination-resize'
       ;; equals 'side.
       ((and (not (eq window-combination-resize 'side))
	     (window-parameter window 'window-side))
	(error "Cannot split side window or parent of side window"))
       ;; If `window-combination-resize' is 'side and window has a side
       ;; window sibling, bind `window-combination-limit' to t.
       ((and (not (eq window-combination-resize 'side))
	     (or (and (window-prev-sibling window)
		      (window-parameter
		       (window-prev-sibling window) 'window-side))
		 (and (window-next-sibling window)
		      (window-parameter
		       (window-next-sibling window) 'window-side))))
	(setq window-combination-limit t)))

      ;; If `window-combination-resize' is t and SIZE is non-negative,
      ;; bind `window-combination-limit' to t.
      (when (and (eq window-combination-resize t)
		 pixel-size (> pixel-size 0))
	(setq window-combination-limit t))

      (let* ((parent-pixel-size
	      ;; 'parent-pixel-size' is the pixel size of WINDOW's
	      ;; parent, provided it has one.
	      (when parent (window-size parent horizontal t)))
	     ;; 'resize' non-nil means we are supposed to resize other
	     ;; windows in WINDOW's combination.
	     (resize
	      (and window-combination-resize
		   (or (window-parameter window 'window-side)
		       (not (eq window-combination-resize 'side)))
		   (not (eq window-combination-limit t))
		   ;; Resize makes sense in iso-combinations only.
		   (window-combined-p window horizontal)))
	     ;; 'old-pixel-size' is the current pixel size of WINDOW.
	     (old-pixel-size (window-size window horizontal t))
	     ;; 'new-size' is the specified or calculated size of the
	     ;; new window.
	     new-pixel-size new-parent new-normal)
	(cond
	 ((not pixel-size)
	  (setq new-pixel-size
		(if resize
		    ;; When resizing try to give the new window the
		    ;; average size of a window in its combination.
		    (max (min (- parent-pixel-size
				 (window-min-size parent horizontal nil t))
			      (window--combination-resizable parent horizontal))
			 (window-min-pixel-size))
		  ;; Else try to give the new window half the size
		  ;; of WINDOW (plus an eventual odd pixel).
		  (/ old-pixel-size 2)))
	  (unless window-resize-pixelwise
	    ;; Round to nearest char-size multiple.
	    (setq new-pixel-size
		  (* char-size (round new-pixel-size char-size)))))
	 ((>= pixel-size 0)
	  ;; SIZE non-negative specifies the new size of WINDOW.

	  ;; Note: Specifying a non-negative SIZE is practically
	  ;; always done as workaround for making the new window
	  ;; appear above or on the left of the new window (the
	  ;; ispell window is a typical example of that).  In all
	  ;; these cases the SIDE argument should be set to 'above
	  ;; or 'left in order to support the 'resize option.
	  ;; Here we have to nest the windows instead, see above.
	  (setq new-pixel-size (- old-pixel-size pixel-size)))
	 (t
	  ;; SIZE negative specifies the size of the new window.
	  (setq new-pixel-size (- pixel-size))))

	;; Check SIZE.
	(cond
	 ((not pixel-size)
	  (cond
	   (resize
	    ;; SIZE unspecified, resizing.
	    (unless (or (window-sizable-p
			 parent (- (+ new-pixel-size divider-width)) horizontal
			 nil t)
			(window-sizable-p
			 parent (- (+ new-pixel-size divider-width)) horizontal
			 (setq ignore 'preserved) t))
	      (error "Window %s too small for splitting" parent)))
	   ((and (> (+ new-pixel-size divider-width
		       (window-min-size window horizontal nil t))
		    old-pixel-size)
		 (> (+ new-pixel-size divider-width
		       (window-min-size
			window horizontal (setq ignore 'preserved) t))
		    old-pixel-size))
	    ;; SIZE unspecified, no resizing.
	    (error "Window %s too small for splitting" window))))
	 ((and (>= pixel-size 0)
	       (or (>= pixel-size old-pixel-size)
		   (< new-pixel-size
		      (window-safe-min-pixel-size window horizontal))))
	  ;; SIZE specified as new size of old window.  If the new size
	  ;; is larger than the old size or the size of the new window
	  ;; would be less than the safe minimum, signal an error.
	  (error "Window %s too small for splitting" window))
	 (resize
	  ;; SIZE specified, resizing.
	  (unless (or (window-sizable-p
		       parent (- (+ new-pixel-size divider-width)) horizontal
		       nil t)
		      (window-sizable-p
		       parent (- (+ new-pixel-size divider-width)) horizontal
		       (setq ignore 'preserved) t))
	    ;; If we cannot resize the parent give up.
	    (error "Window %s too small for splitting" parent)))
	 ((or (< new-pixel-size
		 (window-safe-min-pixel-size window horizontal))
	      (< (- old-pixel-size new-pixel-size)
		 (window-safe-min-pixel-size window horizontal)))
	  ;; SIZE specification violates minimum size restrictions.
	  (error "Window %s too small for splitting" window)))

	(window--resize-reset frame horizontal)

	(setq new-parent
	      ;; Make new-parent non-nil if we need a new parent window;
	      ;; either because we want to nest or because WINDOW is not
	      ;; iso-combined.
	      (or (eq window-combination-limit t)
		  (not (window-combined-p window horizontal))))
	(setq new-normal
	      ;; Make new-normal the normal size of the new window.
	      (cond
	       (pixel-size (/ (float new-pixel-size)
			      (if new-parent old-pixel-size parent-pixel-size)))
	       (new-parent 0.5)
	       (resize (/ 1.0 (1+ (window-combinations parent horizontal t))))
	       (t (/ (window-normal-size window horizontal) 2.0))))

	(if resize
	    ;; Try to get space from OLD's siblings.  We could go "up" and
	    ;; try getting additional space from surrounding windows but
	    ;; we won't be able to return space to those windows when we
	    ;; delete the one we create here.  Hence we do not go up.
	    (progn
	      (window--resize-child-windows
	       parent (- new-pixel-size) horizontal nil ignore)
	      (let* ((normal (- 1.0 new-normal))
		     (sub (window-child parent)))
		(while sub
		  (set-window-new-normal
		   sub (* (window-normal-size sub horizontal) normal))
		  (setq sub (window-right sub)))))
	  ;; Get entire space from WINDOW.
	  (set-window-new-pixel
	   window (- old-pixel-size new-pixel-size))
	  (window--resize-this-window
	   window (- new-pixel-size) horizontal ignore)
	  (set-window-new-normal
	   window (- (if new-parent 1.0 (window-normal-size window horizontal))
		     new-normal)))

	(let* ((new (split-window-internal window new-pixel-size side new-normal)))
	  (window--pixel-to-total frame horizontal)
	  ;; Assign window-side parameters, if any.
	  (cond
	   ((eq window-combination-resize 'side)
	    (let ((window-side
		   (cond
		    (window-side window-side)
		    ((eq side 'above) 'top)
		    ((eq side 'below) 'bottom)
		    (t side))))
	      ;; We made a new side window.
	      (set-window-parameter new 'window-side window-side)
	      (when (and new-parent (window-parameter window 'window-side))
		;; We've been splitting a side root window.  Give the
		;; new parent the same window-side parameter.
		(set-window-parameter
		 (window-parent new) 'window-side window-side))))
	   ((eq window-combination-resize 'atom)
            ;; Make sure `window--check' won't destroy an existing
	    ;; atomic window in case the new window gets nested inside.
	    (unless (window-parameter window 'window-atom)
	      (set-window-parameter window 'window-atom t))
	    (when new-parent
	      (set-window-parameter (window-parent new) 'window-atom t))
	    (set-window-parameter new 'window-atom t)))

          ;; Make the new window inherit the `min-margins' parameter of
          ;; WINDOW (Bug#44483).
          (let ((min-margins (window-parameter window 'min-margins)))
            (when min-margins
              (set-window-parameter new 'min-margins min-margins)))

          ;; Sanitize sizes unless SIZE was specified.
	  (unless size
            (window--sanitize-window-sizes horizontal))

	  (run-window-scroll-functions new)
	  (window--check frame)
	  ;; Always return the new window.
	  new)))))

(defun split-window-no-error (&optional window size side pixelwise)
  "Make a new window adjacent to WINDOW.
This function is like `split-window' but does not signal an error
when WINDOW cannot be split.

For the meaning of all arguments see the documentation of
`split-window'."
  (condition-case nil
      (split-window window size side pixelwise)
    (error nil)))

;; I think this should be the default; I think people will prefer it--rms.
(defcustom split-window-keep-point t
  "If non-nil, \\[split-window-below] preserves point in the new window.
If nil, adjust point in the two windows to minimize redisplay.
This option applies only to `split-window-below' and functions
that call it.  The low-level `split-window' function always keeps
the original point in both windows."
  :type 'boolean
  :group 'windows)

(defun split-window-below (&optional size)
  "Split the selected window into two windows, one above the other.
The selected window is above.  The newly split-off window is
below and displays the same buffer.  Return the new window.

If optional argument SIZE is omitted or nil, both windows get the
same height, or close to it.  If SIZE is positive, the upper
\(selected) window gets SIZE lines.  If SIZE is negative, the
lower (new) window gets -SIZE lines.

If the variable `split-window-keep-point' is non-nil, both
windows get the same value of point as the selected window.
Otherwise, the window starts are chosen so as to minimize the
amount of redisplay; this is convenient on slow terminals."
  (interactive "P")
  (let ((old-window (selected-window))
	(old-point (window-point))
	(size (and size (prefix-numeric-value size)))
        moved-by-window-height moved new-window bottom)
    (when (and size (< size 0) (< (- size) window-min-height))
      ;; `split-window' would not signal an error here.
      (error "Size of new window too small"))
    (setq new-window (split-window nil size))
    (unless split-window-keep-point
      (with-current-buffer (window-buffer)
	;; Use `save-excursion' around vertical movements below
	;; (Bug#10971).  Note: When the selected window's buffer has a
	;; header line, up to two lines of the buffer may not show up
	;; in the resulting configuration.
	(save-excursion
	  (goto-char (window-start))
	  (setq moved (vertical-motion (window-height)))
	  (set-window-start new-window (point))
	  (when (> (point) (window-point new-window))
	    (set-window-point new-window (point)))
	  (when (= moved (window-height))
	    (setq moved-by-window-height t)
	    (vertical-motion -1))
	  (setq bottom (point)))
	(and moved-by-window-height
	     (<= bottom (point))
	     (set-window-point old-window (1- bottom)))
	(and moved-by-window-height
	     (<= (window-start new-window) old-point)
	     (set-window-point new-window old-point)
	     (select-window new-window))))
    ;; Always copy quit-restore parameter in interactive use.
    (let ((quit-restore (window-parameter old-window 'quit-restore)))
      (when quit-restore
	(set-window-parameter new-window 'quit-restore quit-restore)))
    new-window))

(defalias 'split-window-vertically 'split-window-below)

(defun split-window-right (&optional size)
  "Split the selected window into two side-by-side windows.
The selected window is on the left.  The newly split-off window
is on the right and displays the same buffer.  Return the new
window.

If optional argument SIZE is omitted or nil, both windows get the
same width, or close to it.  If SIZE is positive, the left-hand
\(selected) window gets SIZE columns.  If SIZE is negative, the
right-hand (new) window gets -SIZE columns.  Here, SIZE includes
the width of the window's scroll bar; if there are no scroll
bars, it includes the width of the divider column to the window's
right, if any."
  (interactive "P")
  (let ((old-window (selected-window))
	(size (and size (prefix-numeric-value size)))
	new-window)
    (when (and size (< size 0) (< (- size) window-min-width))
      ;; `split-window' would not signal an error here.
      (error "Size of new window too small"))
    (setq new-window (split-window nil size t))
    ;; Always copy quit-restore parameter in interactive use.
    (let ((quit-restore (window-parameter old-window 'quit-restore)))
      (when quit-restore
	(set-window-parameter new-window 'quit-restore quit-restore)))
    new-window))

(defalias 'split-window-horizontally 'split-window-right)

;;; Balancing windows.

;; The following routine uses the recycled code from an old version of
;; `window--resize-child-windows'.  It's not very pretty, but coding it the way the
;; new `window--resize-child-windows' code does would hardly make it any shorter or
;; more readable (FWIW we'd need three loops - one to calculate the
;; minimum sizes per window, one to enlarge or shrink windows until the
;; new parent-size matches, and one where we shrink the largest/enlarge
;; the smallest window).
(defun balance-windows-2 (window horizontal)
  "Subroutine of `balance-windows-1'.
WINDOW must be a vertical combination (horizontal if HORIZONTAL
is non-nil)."
  (let* ((char-size (if window-resize-pixelwise
			1
		      (frame-char-size window horizontal)))
	 (first (window-child window))
	 (sub first)
	 (number-of-children 0)
	 (parent-size (window-new-pixel window))
	 (total-sum parent-size)
	 failed size sub-total sub-delta sub-amount rest)
    (while sub
      (if (window-size-fixed-p sub horizontal)
          (progn
            (setq total-sum
	          (- total-sum (window-size sub horizontal t)))
	    (set-window-new-normal sub 'ignore))
        (setq number-of-children (1+ number-of-children)))
      (setq sub (window-right sub)))

    (setq failed t)
    (while (and failed (> number-of-children 0))
      (setq size (/ total-sum number-of-children))
      (setq failed nil)
      (setq sub first)
      (while (and sub (not failed))
	;; Ignore child windows that should be ignored or are stuck.
	(unless (window--resize-child-windows-skip-p sub)
	  (setq sub-total (window-size sub horizontal t))
	  (setq sub-delta (- size sub-total))
	  (setq sub-amount
		(window-sizable sub sub-delta horizontal nil t))
	  ;; Register the new total size for this child window.
	  (set-window-new-pixel sub (+ sub-total sub-amount))
	  (unless (= sub-amount sub-delta)
	    (setq total-sum (- total-sum sub-total sub-amount))
	    (setq number-of-children (1- number-of-children))
	    ;; We failed and need a new round.
	    (setq failed t)
	    (set-window-new-normal sub 'skip)))
	(setq sub (window-right sub))))

    (when (> number-of-children 0)
      (setq rest (% total-sum number-of-children))
      ;; Fix rounding by trying to enlarge non-stuck windows by one line
      ;; (column) until `rest' is zero.
      (setq sub first)
      (while (and sub (> rest 0))
        (unless (window--resize-child-windows-skip-p window)
	  (set-window-new-pixel sub (min rest char-size) t)
	  (setq rest (- rest char-size)))
        (setq sub (window-right sub))))

    ;; Fix rounding by trying to enlarge stuck windows by one line
    ;; (column) until `rest' equals zero.
    (setq sub first)
    (while (and sub (> rest 0))
      (unless (eq (window-new-normal sub) 'ignore)
	(set-window-new-pixel sub (min rest char-size) t)
	(setq rest (- rest char-size)))
      (setq sub (window-right sub)))

    (setq sub first)
    (while sub
      ;; Record new normal sizes.
      (set-window-new-normal
       sub (/ (if (eq (window-new-normal sub) 'ignore)
		  (window-size sub horizontal t)
		(window-new-pixel sub))
	      (float parent-size)))
      ;; Recursively balance each window's child windows.
      (balance-windows-1 sub horizontal)
      (setq sub (window-right sub)))))

(defun balance-windows-1 (window &optional horizontal)
  "Subroutine of `balance-windows'."
  (if (window-child window)
      (let ((sub (window-child window)))
	(if (window-combined-p sub horizontal)
	    (balance-windows-2 window horizontal)
	  (let ((size (window-new-pixel window)))
	    (while sub
	      (set-window-new-pixel sub size)
	      (balance-windows-1 sub horizontal)
	      (setq sub (window-right sub))))))))

(defun balance-windows (&optional window-or-frame)
  "Balance the sizes of windows of WINDOW-OR-FRAME.
WINDOW-OR-FRAME is optional and defaults to the selected frame.
If WINDOW-OR-FRAME denotes a frame, balance the sizes of all
windows of that frame.  If WINDOW-OR-FRAME denotes a window,
recursively balance the sizes of all child windows of that
window."
  (interactive)
  (let* ((window
	  (cond
	   ((or (not window-or-frame)
		(frame-live-p window-or-frame))
	    (frame-root-window window-or-frame))
	   ((or (window-live-p window-or-frame)
		(window-child window-or-frame))
	    window-or-frame)
	   (t
	    (error "Not a window or frame %s" window-or-frame))))
	 (frame (window-frame window)))
    ;; Balance vertically.
    (window--resize-reset (window-frame window))
    (balance-windows-1 window)
    (when (window--resize-apply-p frame)
      (window-resize-apply frame)
      (window--pixel-to-total frame))
    ;; Balance horizontally.
    (window--resize-reset (window-frame window) t)
    (balance-windows-1 window t)
    (when (window--resize-apply-p frame t)
      (window-resize-apply frame t)
      (window--pixel-to-total frame t))))

(defun window-fixed-size-p (&optional window direction)
  "Return t if WINDOW cannot be resized in DIRECTION.
WINDOW defaults to the selected window.  DIRECTION can be
nil (i.e. any), `height' or `width'."
  (with-current-buffer (window-buffer window)
    (and window-size-fixed
         (not (and direction
		   (member (cons direction window-size-fixed)
			   '((height . width) (width . height))))))))

;;; A different solution to balance-windows.
(defvar-local window-area-factor 1
  "Factor by which the window area should be over-estimated.
This is used by `balance-windows-area'.
Changing this globally has no effect.")

(defun balance-windows-area-adjust (window delta horizontal pixelwise)
  "Wrapper around `window-resize' with error checking.
Arguments WINDOW, DELTA and HORIZONTAL are passed on to that function."
  ;; `window-resize' may fail if delta is too large.
  (while (>= (abs delta) 1)
    (condition-case nil
        (progn
	  ;; It was wrong to use `window-resize' here.  Somehow
	  ;; `balance-windows-area' depends on resizing windows
	  ;; asymmetrically.
	  (adjust-window-trailing-edge window delta horizontal pixelwise)
          (setq delta 0))
      (error
       ;;(message "adjust: %s" (error-message-string err))
       (setq delta (/ delta 2))))))

(defun balance-windows-area ()
  "Make all visible windows the same area (approximately).
See also `window-area-factor' to change the relative size of
specific buffers."
  (interactive)
  (let* ((unchanged 0) (carry 0) (round 0)
         ;; Remove fixed-size windows.
         (wins (delq nil (mapcar (lambda (win)
                                   (if (not (window-fixed-size-p win)) win))
                                 (window-list nil 'nomini))))
         (changelog nil)
	 (pixelwise window-resize-pixelwise)
	 next)
    ;; Resizing a window changes the size of surrounding windows in complex
    ;; ways, so it's difficult to balance them all.  The introduction of
    ;; `adjust-window-trailing-edge' made it a bit easier, but it is still
    ;; very difficult to do.  `balance-window' above takes an off-line
    ;; approach: get the whole window tree, then balance it, then try to
    ;; adjust the windows so they fit the result.
    ;; Here, instead, we take a "local optimization" approach, where we just
    ;; go through all the windows several times until nothing needs to be
    ;; changed.  The main problem with this approach is that it's difficult
    ;; to make sure it terminates, so we use some heuristic to try and break
    ;; off infinite loops.
    ;; After a round without any change, we allow a second, to give a chance
    ;; to the carry to propagate a minor imbalance from the end back to
    ;; the beginning.
    (while (< unchanged 2)
      ;; (message "New round")
      (setq unchanged (1+ unchanged) round (1+ round))
      (dolist (win wins)
        (setq next win)
        (while (progn (setq next (next-window next))
                      (window-fixed-size-p next)))
        ;; (assert (eq next (or (cadr (member win wins)) (car wins))))
        (let* ((horiz
                (< (car (window-pixel-edges win)) (car (window-pixel-edges next))))
               (areadiff (/ (- (* (window-size next nil pixelwise)
				  (window-size next t pixelwise)
                                  (buffer-local-value 'window-area-factor
                                                      (window-buffer next)))
                               (* (window-size win nil pixelwise)
				  (window-size win t pixelwise)
                                  (buffer-local-value 'window-area-factor
                                                      (window-buffer win))))
                            (max (buffer-local-value 'window-area-factor
                                                     (window-buffer win))
                                 (buffer-local-value 'window-area-factor
                                                     (window-buffer next)))))
               (edgesize (if horiz
                             (+ (window-size win nil pixelwise)
				(window-size next nil pixelwise))
                           (+ (window-size win t pixelwise)
			      (window-size next t pixelwise))))
               (diff (/ areadiff edgesize)))
          (when (zerop diff)
            ;; Maybe diff is actually closer to 1 than to 0.
            (setq diff (/ (* 3 areadiff) (* 2 edgesize))))
          (when (and (zerop diff) (not (zerop areadiff)))
            (setq diff (/ (+ areadiff carry) edgesize))
            ;; Change things smoothly.
            (if (or (> diff 1) (< diff -1)) (setq diff (/ diff 2))))
          (if (zerop diff)
              ;; Make sure negligible differences don't accumulate to
              ;; become significant.
              (setq carry (+ carry areadiff))
	    ;; This used `adjust-window-trailing-edge' before and uses
	    ;; `window-resize' now.  Error wrapping is still needed.
	    (balance-windows-area-adjust win diff horiz pixelwise)
            ;; (sit-for 0.5)
            (let ((change (cons win (window-pixel-edges win))))
              ;; If the same change has been seen already for this window,
              ;; we're most likely in an endless loop, so don't count it as
              ;; a change.
              (unless (member change changelog)
                (push change changelog)
                (setq unchanged 0 carry 0)))))))
    ;; We've now basically balanced all the windows.
    ;; But there may be some minor off-by-one imbalance left over,
    ;; so let's do some fine tuning.
    ;; (bw-finetune wins)
    ;; (message "Done in %d rounds" round)
    ))

;;; Window states, how to get them and how to put them in a window.
(defun window--state-get-1 (window &optional writable)
  "Helper function for `window-state-get'."
  (let* ((type
	  (cond
	   ((window-top-child window) 'vc)
	   ((window-left-child window) 'hc)
	   (t 'leaf)))
	 (buffer (window-buffer window))
	 (selected (eq window (selected-window)))
	 (next-buffers (when (window-live-p window)
	                 (delq nil (mapcar (lambda (buffer)
                                             (and (buffer-live-p buffer) buffer))
                                           (window-next-buffers window)))))
	 (prev-buffers (when (window-live-p window)
	                 (delq nil (mapcar (lambda (entry)
                                             (and (buffer-live-p (nth 0 entry))
                                                  entry))
                                           (window-prev-buffers window)))))
	 (head
	  `(,type
            ,@(unless (window-next-sibling window) '((last . t)))
            (pixel-width . ,(window-pixel-width window))
            (pixel-height . ,(window-pixel-height window))
            (total-width . ,(window-total-width window))
            (total-height . ,(window-total-height window))
            (normal-height . ,(window-normal-size window))
            (normal-width . ,(window-normal-size window t))
            ,@(unless (window-live-p window)
                `((combination-limit . ,(window-combination-limit window))))
            ,@(let ((parameters (window-parameters window))
		    list)
		;; Make copies of those window parameters whose
		;; persistence property is `writable' if WRITABLE is
		;; non-nil and non-nil if WRITABLE is nil.
                (dolist (par parameters)
		  (let ((pers (cdr (assq (car par)
					 window-persistent-parameters))))
		    (when (and pers (or (not writable) (eq pers 'writable)))
		      (setq list (cons (cons (car par) (cdr par)) list)))))
		;; Add `clone-of' parameter if necessary.
		(let ((pers (cdr (assq 'clone-of
				       window-persistent-parameters))))
		  (when (and pers (or (not writable) (eq pers 'writable))
			     (not (assq 'clone-of list)))
		    (setq list (cons (cons 'clone-of window) list))))
                (when list
                  `((parameters . ,list))))
            ,@(when buffer
                ;; All buffer related things go in here.
		(let ((point (window-point window))
		      (start (window-start window)))
		  `((buffer
		     ,(if writable (buffer-name buffer) buffer)
		     (selected . ,selected)
		     (hscroll . ,(window-hscroll window))
		     (fringes . ,(window-fringes window))
		     (margins . ,(window-margins window))
		     (scroll-bars . ,(window-scroll-bars window))
		     (vscroll . ,(window-vscroll window))
		     (dedicated . ,(window-dedicated-p window))
		     (point . ,(if writable
                                   point
                                 (with-current-buffer buffer
                                   (copy-marker point
                                                (buffer-local-value
                                                 'window-point-insertion-type
                                                 buffer)))))
		     (start . ,(if writable
                                   start
                                 (with-current-buffer buffer
                                   (copy-marker start))))))))
            ,@(when next-buffers
                `((next-buffers
                   . ,(if writable
                          (mapcar (lambda (buffer) (buffer-name buffer))
                                  next-buffers)
                        next-buffers))))
            ,@(when prev-buffers
                `((prev-buffers
                   . ,(if writable
                          (mapcar (lambda (entry)
                                    (list (buffer-name (nth 0 entry))
                                          (marker-position (nth 1 entry))
                                          (marker-position (nth 2 entry))))
                                  prev-buffers)
                        prev-buffers))))))
	 (tail
	  (when (memq type '(vc hc))
	    (let (list)
	      (setq window (window-child window))
	      (while window
		(setq list (cons (window--state-get-1 window writable) list))
		(setq window (window-right window)))
	      (nreverse list)))))
    (append head tail)))

(defun window-state-get (&optional window writable)
  "Return state of WINDOW as a Lisp object.
WINDOW can be any window and defaults to the root window of the
selected frame.

Optional argument WRITABLE non-nil means do not use markers for
sampling `window-point' and `window-start'.  Together, WRITABLE
and the variable `window-persistent-parameters' specify which
window parameters are saved by this function.  WRITABLE should be
non-nil when the return value shall be written to a file and read
back in another session.  Otherwise, an application may run into
an `invalid-read-syntax' error while attempting to read back the
value from file.

The return value can be used as argument for `window-state-put'
to put the state recorded here into an arbitrary window.  The
value can be also stored on disk and read back in a new session."
  (setq window
	(if window
	    (if (window-valid-p window)
		window
	      (error "%s is not a live or internal window" window))
	  (frame-root-window)))
  ;; The return value is a cons whose car specifies some constraints on
  ;; the size of WINDOW.  The cdr lists the states of the child windows
  ;; of WINDOW.
  (cons
   ;; Frame related things would go into a function, say `frame-state',
   ;; calling `window-state-get' to insert the frame's root window.
   `((min-height        . ,(window-min-size window))
     (min-width         . ,(window-min-size window t))
     (min-height-ignore . ,(window-min-size window nil t))
     (min-width-ignore  . ,(window-min-size window t t))
     (min-height-safe   . ,(window-min-size window nil 'safe))
     (min-width-safe    . ,(window-min-size window t 'safe))
     (min-pixel-height  . ,(window-min-size window nil nil t))
     (min-pixel-width   . ,(window-min-size window t nil t))
     (min-pixel-height-ignore . ,(window-min-size window nil t t))
     (min-pixel-width-ignore  . ,(window-min-size window t t t))
     (min-pixel-height-safe   . ,(window-min-size window nil 'safe t))
     (min-pixel-width-safe    . ,(window-min-size window t 'safe t)))
   (window--state-get-1 window writable)))

(defvar window-state-put-list nil
  "Helper variable for `window-state-put'.")

(defvar window-state-put-stale-windows nil
  "Helper variable for `window-state-put'.")

(defun window--state-put-1 (state &optional window ignore totals pixelwise)
  "Helper function for `window-state-put'."
  (let ((type (car state)))
    (setq state (cdr state))
    (cond
     ((eq type 'leaf)
      ;; For a leaf window just add unprocessed entries to
      ;; `window-state-put-list'.
      (push (cons window state) window-state-put-list))
     ((memq type '(vc hc))
      (let* ((horizontal (eq type 'hc))
	     (total (window-size window horizontal pixelwise))
             (first t)
             (window-combination-limit (cdr (assq 'combination-limit state)))
	     size new)
	(dolist (item state)
	  ;; Find the next child window.  WINDOW always points to the
	  ;; real window that we want to fill with what we find here.
	  (when (memq (car item) '(leaf vc hc))
	    (if (assq 'last item)
		;; The last child window.  Below `window--state-put-1'
		;; will put into it whatever ITEM has in store.
		(setq new nil)
	      ;; Not the last child window, prepare for splitting
	      ;; WINDOW.  SIZE is the new (and final) size of the old
	      ;; window.
	      (setq size
		    (if totals
			;; Use total size.
			(if pixelwise
			    (cdr (assq (if horizontal
					   'pixel-width
					 'pixel-height)
				       item))
			  (cdr (assq (if horizontal
					 'total-width
				       'total-height)
				     item)))
		      ;; Use normalized size and round.
		      (round
		       (* total
			  (cdr (assq (if horizontal 'normal-width 'normal-height)
				     item))))))

	      ;; Use safe sizes, we try to resize later.
	      (setq size (max size
			      (if horizontal
				  (* window-safe-min-width
				     (if pixelwise
					 (frame-char-width (window-frame window))
				       1))
				(* window-safe-min-height
				   (if pixelwise
				       (frame-char-height (window-frame window))
				     1)))))
	      (if (window-sizable-p window (- size) horizontal 'safe pixelwise)
                  (progn
                    (setq new (split-window-no-error
                               window size horizontal pixelwise))
                    (setq window-combination-limit nil))
		;; Give up if we can't resize window down to safe sizes.
		(error "Cannot resize window %s" window))

	      (when first
		(setq first nil)
		;; When creating the first child window add for parent
		;; unprocessed entries to `window-state-put-list'.
		(setq window-state-put-list
		      (cons (cons (window-parent window) state)
			    window-state-put-list))))

	    ;; Now process the current window (either the one we've just
	    ;; split or the last child of its parent).
	    (window--state-put-1 item window ignore totals)
	    ;; Continue with the last window split off.
	    (setq window new))))))))

(defun window--state-put-2 (ignore pixelwise)
  "Helper function for `window-state-put'."
  (dolist (item window-state-put-list)
    (let ((window (car item))
	  (combination-limit (cdr (assq 'combination-limit item)))
	  (parameters (cdr (assq 'parameters item)))
	  (state (cdr (assq 'buffer item)))
	  (next-buffers (cdr (assq 'next-buffers item)))
	  (prev-buffers (cdr (assq 'prev-buffers item))))
      (when combination-limit
	(set-window-combination-limit window combination-limit))
      ;; Reset window's parameters and assign saved ones (we might want
      ;; a `remove-window-parameters' function here).
      (dolist (parameter (window-parameters window))
	(set-window-parameter window (car parameter) nil))
      (when parameters
	(dolist (parameter parameters)
	  (set-window-parameter window (car parameter) (cdr parameter))))
      ;; Process buffer related state.
      (when state
	(let ((buffer (get-buffer (car state)))
	      (state (cdr state)))
	  (if buffer
	      (with-current-buffer buffer
		(set-window-buffer window buffer)
		(set-window-hscroll window (cdr (assq 'hscroll state)))
		(apply 'set-window-fringes
		       (cons window (cdr (assq 'fringes state))))
		(let ((margins (cdr (assq 'margins state))))
		  (set-window-margins window (car margins) (cdr margins)))
		(let ((scroll-bars (cdr (assq 'scroll-bars state))))
		  (set-window-scroll-bars
		   window (car scroll-bars) (nth 2 scroll-bars)
		   (nth 3 scroll-bars) (nth 5 scroll-bars) (nth 6 scroll-bars)))
		(set-window-vscroll window (cdr (assq 'vscroll state)))
		;; Adjust vertically.
		(if (or (memq window-size-fixed '(t height))
                        (window-preserved-size window))
		    ;; A fixed height window, try to restore the
		    ;; original size.
		    (let ((delta
			   (- (cdr (assq
				    (if pixelwise 'pixel-height 'total-height)
				    item))
			      (window-size window nil pixelwise)))
			  window-size-fixed)
		      (when (window--resizable-p
			     window delta nil nil nil nil nil pixelwise)
			(window-resize window delta nil nil pixelwise)))
		  ;; Else check whether the window is not high enough.
		  (let* ((min-size
			  (window-min-size window nil ignore pixelwise))
			 (delta
			  (- min-size (window-size window nil pixelwise))))
		    (when (and (> delta 0)
			       (window--resizable-p
				window delta nil ignore nil nil nil pixelwise))
		      (window-resize window delta nil ignore pixelwise))))
		;; Adjust horizontally.
		(if (or (memq window-size-fixed '(t width))
                        (window-preserved-size window t))
		    ;; A fixed width window, try to restore the original
		    ;; size.
		    (let ((delta
			   (- (cdr (assq
				    (if pixelwise 'pixel-width 'total-width)
				    item))
			      (window-size window t pixelwise)))
			  window-size-fixed)
		      (when (window--resizable-p
			     window delta t nil nil nil nil pixelwise)
			(window-resize window delta t nil pixelwise)))
		  ;; Else check whether the window is not wide enough.
		  (let* ((min-size (window-min-size window t ignore pixelwise))
			 (delta (- min-size (window-size window t pixelwise))))
		    (when (and (> delta 0)
			       (window--resizable-p
				window delta t ignore nil nil nil pixelwise))
		      (window-resize window delta t ignore pixelwise))))
		;; Set dedicated status.
		(set-window-dedicated-p window (cdr (assq 'dedicated state)))
		;; Install positions (maybe we should do this after all
		;; windows have been created and sized).
		(ignore-errors
                  ;; Set 'noforce argument to avoid that window start
                  ;; overrides window point set below (Bug#24240).
		  (set-window-start window (cdr (assq 'start state)) 'noforce)
		  (set-window-point window (cdr (assq 'point state))))
		;; Select window if it's the selected one.
		(when (cdr (assq 'selected state))
		  (select-window window))
                (set-window-next-buffers
                 window
                 (delq nil (mapcar (lambda (buffer)
                                     (setq buffer (get-buffer buffer))
                                     (when (buffer-live-p buffer) buffer))
                                   next-buffers)))
                (set-window-prev-buffers
                 window
                 (delq nil (mapcar (lambda (entry)
                                     (let ((buffer (get-buffer (nth 0 entry)))
                                           (m1 (nth 1 entry))
                                           (m2 (nth 2 entry)))
                                       (when (buffer-live-p buffer)
                                         (list buffer
                                               (if (markerp m1) m1
                                                 (set-marker (make-marker) m1
                                                             buffer))
                                               (if (markerp m2) m2
                                                 (set-marker (make-marker) m2
                                                             buffer))))))
                                   prev-buffers))))
	    ;; We don't want to raise an error in case the buffer does
	    ;; not exist anymore, so we switch to a previous one and
	    ;; save the window with the intention of deleting it later
	    ;; if possible.
	    (switch-to-prev-buffer window)
	    (push window window-state-put-stale-windows)))))))

(defun window-state-put (state &optional window ignore)
  "Put window state STATE into WINDOW.
STATE should be the state of a window returned by an earlier
invocation of `window-state-get'.  Optional argument WINDOW must
specify a valid window.  If WINDOW is not a live window,
replace WINDOW by a new live window created on the same frame.
If WINDOW is nil, create a new window before putting STATE into it.

Optional argument IGNORE non-nil means ignore minimum window
sizes and fixed size restrictions.  IGNORE equal `safe' means
windows can get as small as `window-safe-min-height' and
`window-safe-min-width'."
  (setq window-state-put-stale-windows nil)

  ;; When WINDOW is internal or nil, reduce it to a live one,
  ;; then create a new window on the same frame to put STATE into.
  (unless (window-live-p window)
    (let ((root window))
      (setq window (if root
                       (catch 'live
                         (walk-window-subtree
                          (lambda (window)
                            (when (and (window-live-p window)
                                       (not (window-parameter
                                             window 'window-side)))
                              (throw 'live window)))
                          root))
                     (selected-window)))
      (delete-other-windows-internal window root)
      ;; Create a new window to replace the existing one.
      (setq window (prog1 (split-window window)
                     (delete-window window)))))

  (set-window-dedicated-p window nil)

  (let* ((frame (window-frame window))
	 (head (car state))
	 ;; We check here (1) whether the total sizes of root window of
	 ;; STATE and that of WINDOW are equal so we can avoid
	 ;; calculating new sizes, and (2) if we do have to resize
	 ;; whether we can do so without violating size restrictions.
	 (pixelwise (and (cdr (assq 'pixel-width state))
			 (cdr (assq 'pixel-height state))))
	 (totals (or (and pixelwise
			  (= (window-pixel-width window)
			     (cdr (assq 'pixel-width state)))
			  (= (window-pixel-height window)
			     (cdr (assq 'pixel-height state))))
		     (and (= (window-total-width window)
			     (cdr (assq 'total-width state)))
			  (= (window-total-height window)
			     (cdr (assq 'total-height state))))))
	 (min-height (cdr (assq
			   (if pixelwise 'min-pixel-height 'min-height)
			   head)))
	 (min-width (cdr (assq
			  (if pixelwise 'min-pixel-width 'min-weight)
			  head))))
    (if (and (not totals)
	     (or (> min-height (window-size window nil pixelwise))
		 (> min-width (window-size window t pixelwise)))
	     (or (not ignore)
		 (and (setq min-height
			    (cdr (assq
				  (if pixelwise
				      'min-pixel-height-ignore
				    'min-height-ignore)
				  head)))
		      (setq min-width
			    (cdr (assq
				  (if pixelwise
				      'min-pixel-width-ignore
				    'min-width-ignore)
				  head)))
		      (or (> min-height
			     (window-size window nil pixelwise))
			  (> min-width
			     (window-size window t pixelwise)))
		      (or (not (eq ignore 'safe))
			  (and (setq min-height
				     (cdr (assq
					   (if pixelwise
					       'min-pixel-height-safe
					     'min-height-safe)
					   head)))
			       (setq min-width
				     (cdr (assq
					   (if pixelwise
					       'min-pixel-width-safe
					     'min-width-safe)
					   head)))
			       (or (> min-height
				      (window-size window nil pixelwise))
				   (> min-width
				      (window-size window t pixelwise))))))))
	;; The check above might not catch all errors due to rounding
	;; issues - so IGNORE equal 'safe might not always produce the
	;; minimum possible state.  But such configurations hardly make
	;; sense anyway.
	(error "Window %s too small to accommodate state" window)
      (setq state (cdr state))
      (setq window-state-put-list nil)
      ;; Work on the windows of a temporary buffer to make sure that
      ;; splitting proceeds regardless of any buffer local values of
      ;; `window-size-fixed'.  Release that buffer after the buffers of
      ;; all live windows have been set by `window--state-put-2'.
      (with-temp-buffer
	(set-window-buffer window (current-buffer))
	(window--state-put-1 state window nil totals pixelwise)
	(window--state-put-2 ignore pixelwise))
      (while window-state-put-stale-windows
	(let ((window (pop window-state-put-stale-windows)))
          ;; Avoid that 'window-deletable-p' throws an error if window
          ;; was already deleted when exiting 'with-temp-buffer' above
          ;; (Bug#54028).
	  (when (and (window-valid-p window)
                     (eq (window-deletable-p window) t))
	    (delete-window window))))
      (window--check frame))))

(defun window-state-buffers (state)
  "Return all buffers saved to the given window state STATE."
  (let ((buffer (cadr (assq 'buffer state)))
        (buffers (mapcan (lambda (item)
                           (when (memq (car item) '(leaf vc hc))
                             (window-state-buffers item)))
                         (if (consp (car state)) (list (cdr state)) (cdr state)))))
    (if buffer (cons buffer buffers) buffers)))

(defun window-swap-states (&optional window-1 window-2 size)
  "Swap the states of live windows WINDOW-1 and WINDOW-2.
WINDOW-1 must specify a live window and defaults to the selected
one.  WINDOW-2 must specify a live window and defaults to the
window following WINDOW-1 in the cyclic ordering of windows,
excluding minibuffer windows and including live windows on all
visible frames.

Optional argument SIZE non-nil means to try swapping the sizes of
WINDOW-1 and WINDOW-2 as well.  A value of `height' means to swap
heights only, a value of `width' means to swap widths only, while
t means to swap both widths and heights, if possible.  Frames are
not resized by this function."
  (interactive)
  (setq window-1 (window-normalize-window window-1 t))
  (if window-2
      (unless (window-live-p window-2)
        (error "%s is not a live window" window-2))
    (setq window-2 (next-window window-1 'nomini 'visible)))
  (unless (eq window-1 window-2)
    (let* ((height (memq size '(t height)))
           (width (memq size '(t width)))
           (state-1 (window-state-get window-1))
           (width-1 (and width (window-text-width window-1 t)))
           (height-1 (and height (window-text-height window-1 t)))
           (state-2 (window-state-get window-2))
           (width-2 (and width (window-text-width window-2 t)))
           (height-2 (and height (window-text-height window-2 t)))
           old preserved)
      ;; Swap basic states.
      (window-state-put state-1 window-2 t)
      (window-state-put state-2 window-1 t)
      ;; Swap overlays with `window' property.
      (with-current-buffer (window-buffer window-1)
        (dolist (overlay (overlays-in (point-min) (point-max)))
          (let ((window (overlay-get overlay 'window)))
            (cond
             ((not window))
             ((eq window window-1)
              (overlay-put overlay 'window window-2))
             ((eq window window-2)
              (overlay-put overlay 'window window-1))))))
      (unless (eq (window-buffer window-1) (window-buffer window-2))
        (with-current-buffer (window-buffer window-2)
          (dolist (overlay (overlays-in (point-min) (point-max)))
            (let ((window (overlay-get overlay 'window)))
              (cond
               ((not window))
               ((eq window window-1)
                (overlay-put overlay 'window window-2))
               ((eq window window-2)
                (overlay-put overlay 'window window-1)))))))
      ;; Try to swap window sizes.
      (when size
        (unless (= (setq old (window-text-width window-1 t)) width-2)
          (window-resize-no-error window-1 (- width-2 old) t t t))
        (unless (= (setq old (window-text-width window-2 t)) width-1)
          (setq preserved (window-preserved-size window-1 t))
          (window-preserve-size window-1 t t)
          (window-resize-no-error window-2 (- width-1 old) t t t)
          (window-preserve-size window-1 t preserved))
        (unless (= (setq old (window-text-height window-1 t)) height-2)
          (window-resize-no-error window-1 (- height-2 old) nil t t))
        (unless (= (setq old (window-text-height window-2 t)) height-1)
          (setq preserved (window-preserved-size window-1))
          (window-preserve-size window-1 nil t)
          (window-resize-no-error window-2 (- height-1 old) nil t t)
          (window-preserve-size window-1 nil preserved))))))

(defun display-buffer-record-window (type window buffer)
  "Record information for window used by `display-buffer'.
WINDOW is the window used for or created by a buffer display
action function.  BUFFER is the buffer to display.  Note that
this function must be called before BUFFER is explicitly made
WINDOW's buffer (although WINDOW may show BUFFER already).

TYPE specifies the type of the calling operation and must be one
of the symbols `reuse' (meaning that WINDOW exists already and
will be used for displaying BUFFER), `window' (WINDOW was created
on an already existing frame) or `frame' (WINDOW was created on a
new frame).

This function installs or updates the `quit-restore' parameter of
WINDOW.  The `quit-restore' parameter is a list of four elements:
The first element is one of the symbols `window', `frame', `same'
or `other'.  The second element is either one of the symbols
`window' or `frame' or a list whose elements are the buffer
previously shown in the window, that buffer's window start and
window point, and the window's height.  The third element is the
window selected at the time the parameter was created.  The
fourth element is BUFFER."
  (cond
   ((eq type 'reuse)
    (if (eq (window-buffer window) buffer)
	;; WINDOW shows BUFFER already.  Update WINDOW's quit-restore
	;; parameter, if any.
	(let ((quit-restore (window-parameter window 'quit-restore)))
	  (when (consp quit-restore)
	    (setcar quit-restore 'same)
	    ;; The selected-window might have changed in
	    ;; between (Bug#20353).
	    (unless (or (eq window (selected-window))
                        (eq window (nth 2 quit-restore)))
	      (setcar (cddr quit-restore) (selected-window)))))
      ;; WINDOW shows another buffer.
      (with-current-buffer (window-buffer window)
	(set-window-parameter
	 window 'quit-restore
	 (list 'other
	       ;; A quadruple of WINDOW's buffer, start, point and height.
	       (list (current-buffer) (window-start window)
		     ;; Preserve window-point-insertion-type (Bug#12855).
		     (copy-marker
		      (window-point window) window-point-insertion-type)
		     (if (window-combined-p window)
                         (window-total-height window)
                       (window-total-width window)))
	       (selected-window) buffer)))))
   ((eq type 'window)
    ;; WINDOW has been created on an existing frame.
    (set-window-parameter
     window 'quit-restore
     (list 'window 'window (selected-window) buffer)))
   ((eq type 'frame)
    ;; WINDOW has been created on a new frame.
    (set-window-parameter
     window 'quit-restore
     (list 'frame 'frame (selected-window) buffer)))
   ((eq type 'tab)
    ;; WINDOW has been created on a new tab.
    (set-window-parameter
     window 'quit-restore
     (list 'tab 'tab (selected-window) buffer)))))

(defcustom display-buffer-function nil
  "If non-nil, function to call to handle `display-buffer'.
It will receive two args, the buffer and a flag which if non-nil
means that the currently selected window is not acceptable.  It
should choose or create a window, display the specified buffer in
it, and return the window.

The specified function should call `display-buffer-record-window'
with corresponding arguments to set up the quit-restore parameter
of the window used."
  :type '(choice
	  (const nil)
	  (function :tag "function"))
  :group 'windows)

(make-obsolete-variable 'display-buffer-function
			'display-buffer-alist "24.3")

(defcustom pop-up-frame-alist nil
  "Alist of parameters for automatically generated new frames.
If non-nil, the value you specify here is used by the default
`pop-up-frame-function' for the creation of new frames.

Since `pop-up-frame-function' is used by `display-buffer' for
making new frames, any value specified here by default affects
the automatic generation of new frames via `display-buffer' and
all functions based on it.  The behavior of `make-frame' is not
affected by this variable.

This option is provided for backward compatibility only.  New
code should use a `pop-up-frame-parameters' action alist entry in
`display-buffer-alist' instead.  See Info node `(elisp) Choosing
Window Options' in the Emacs Lisp manual."
  :type '(repeat (cons :format "%v"
		       (symbol :tag "Parameter")
		       (sexp :tag "Value")))
  :group 'frames)

(defcustom pop-up-frame-function
  (lambda () (make-frame pop-up-frame-alist))
  "Function used by `display-buffer' for creating a new frame.
This function is called with no arguments and should return a new
frame.  The default value calls `make-frame' with the argument
`pop-up-frame-alist'."
  :type 'function
  :group 'frames)

(defcustom special-display-buffer-names nil
  "List of names of buffers that should be displayed specially.
Displaying a buffer with `display-buffer' or `pop-to-buffer', if
its name is in this list, displays the buffer in a way specified
by `special-display-function'.  `special-display-popup-frame'
\(the default for `special-display-function') usually displays
the buffer in a separate frame made with the parameters specified
by `special-display-frame-alist'.  If `special-display-function'
has been set to some other function, that function is called with
the buffer as first, and nil as second argument.

Alternatively, an element of this list can be specified as
\(BUFFER-NAME FRAME-PARAMETERS), where BUFFER-NAME is a buffer
name and FRAME-PARAMETERS an alist of (PARAMETER . VALUE) pairs.
`special-display-popup-frame' will interpret such pairs as frame
parameters when it creates a special frame, overriding the
corresponding values from `special-display-frame-alist'.

As a special case, if FRAME-PARAMETERS contains (same-window . t)
`special-display-popup-frame' displays that buffer in the
selected window.  If FRAME-PARAMETERS contains (same-frame . t),
it displays that buffer in a window on the selected frame.

If `special-display-function' specifies some other function than
`special-display-popup-frame', that function is called with the
buffer named BUFFER-NAME as first, and FRAME-PARAMETERS as second
argument.

Finally, an element of this list can be also specified as
\(BUFFER-NAME FUNCTION OTHER-ARGS).  In that case,
`special-display-popup-frame' will call FUNCTION with the buffer
named BUFFER-NAME as first argument, and OTHER-ARGS as the
second.

Any alternative function specified here is responsible for
setting up the quit-restore parameter of the window used.

If this variable appears \"not to work\", because you added a
name to it but the corresponding buffer is displayed in the
selected window, look at the values of `same-window-buffer-names'
and `same-window-regexps'.  Those variables take precedence over
this one.

See also `special-display-regexps'."
  :type '(repeat
	  (choice :tag "Buffer"
		  :value ""
		  (string :format "%v")
		  (cons :tag "With parameters"
			:format "%v"
			:value ("" . nil)
			(string :format "%v")
			(repeat :tag "Parameters"
				(cons :format "%v"
				      (symbol :tag "Parameter")
				      (sexp :tag "Value"))))
		  (list :tag "With function"
			:format "%v"
			:value ("" . nil)
			(string :format "%v")
			(function :tag "Function")
			(repeat :tag "Arguments" (sexp)))))
  :group 'windows
  :group 'frames)
(make-obsolete-variable 'special-display-buffer-names 'display-buffer-alist "24.3")
(put 'special-display-buffer-names 'risky-local-variable t)

(defcustom special-display-regexps nil
  "List of regexps saying which buffers should be displayed specially.
Displaying a buffer with `display-buffer' or `pop-to-buffer', if
any regexp in this list matches its name, displays it specially
using `special-display-function'.  `special-display-popup-frame'
\(the default for `special-display-function') usually displays
the buffer in a separate frame made with the parameters specified
by `special-display-frame-alist'.  If `special-display-function'
has been set to some other function, that function is called with
the buffer as first, and nil as second argument.

Alternatively, an element of this list can be specified as
\(REGEXP FRAME-PARAMETERS), where REGEXP is a regexp as above and
FRAME-PARAMETERS an alist of (PARAMETER . VALUE) pairs.
`special-display-popup-frame' will then interpret these pairs as
frame parameters when creating a special frame for a buffer whose
name matches REGEXP, overriding the corresponding values from
`special-display-frame-alist'.

As a special case, if FRAME-PARAMETERS contains (same-window . t)
`special-display-popup-frame' displays buffers matching REGEXP in
the selected window.  (same-frame . t) in FRAME-PARAMETERS means
to display such buffers in a window on the selected frame.

If `special-display-function' specifies some other function than
`special-display-popup-frame', that function is called with the
buffer whose name matched REGEXP as first, and FRAME-PARAMETERS
as second argument.

Finally, an element of this list can be also specified as
\(REGEXP FUNCTION OTHER-ARGS).  `special-display-popup-frame'
will then call FUNCTION with the buffer whose name matched
REGEXP as first, and OTHER-ARGS as second argument.

Any alternative function specified here is responsible for
setting up the quit-restore parameter of the window used.

If this variable appears \"not to work\", because you added a
name to it but the corresponding buffer is displayed in the
selected window, look at the values of `same-window-buffer-names'
and `same-window-regexps'.  Those variables take precedence over
this one.

See also `special-display-buffer-names'."
  :type '(repeat
	  (choice :tag "Buffer"
		  :value ""
		  (regexp :format "%v")
		  (cons :tag "With parameters"
			:format "%v"
			:value ("" . nil)
			(regexp :format "%v")
			(repeat :tag "Parameters"
				(cons :format "%v"
				      (symbol :tag "Parameter")
				      (sexp :tag "Value"))))
		  (list :tag "With function"
			:format "%v"
			:value ("" . nil)
			(regexp :format "%v")
			(function :tag "Function")
			(repeat :tag "Arguments" (sexp)))))
  :group 'windows
  :group 'frames)
(make-obsolete-variable 'special-display-regexps 'display-buffer-alist "24.3")
(put 'special-display-regexps 'risky-local-variable t)

(defun special-display-p (buffer-name)
  "Return non-nil if a buffer named BUFFER-NAME gets a special frame.
More precisely, return t if `special-display-buffer-names' or
`special-display-regexps' contain a string entry equaling or
matching BUFFER-NAME.  If `special-display-buffer-names' or
`special-display-regexps' contain a list entry whose car equals
or matches BUFFER-NAME, the return value is the cdr of that
entry."
  (let (tmp)
    (cond
     ((member buffer-name special-display-buffer-names)
      t)
     ((setq tmp (assoc buffer-name special-display-buffer-names))
      (cdr tmp))
     ((catch 'found
	(dolist (regexp special-display-regexps)
	  (cond
	   ((stringp regexp)
	    (when (string-match-p regexp buffer-name)
	      (throw 'found t)))
	   ((and (consp regexp) (stringp (car regexp))
		 (string-match-p (car regexp) buffer-name))
	    (throw 'found (cdr regexp))))))))))

(defcustom special-display-frame-alist
  '((height . 14) (width . 80) (unsplittable . t))
  "Alist of parameters for special frames.
Special frames are used for buffers whose names are listed in
`special-display-buffer-names' and for buffers whose names match
one of the regular expressions in `special-display-regexps'.

This variable can be set in your init file, like this:

  (setq special-display-frame-alist \\='((width . 80) (height . 20)))

These supersede the values given in `default-frame-alist'."
  :type '(repeat (cons :format "%v"
			 (symbol :tag "Parameter")
			 (sexp :tag "Value")))
  :group 'frames)
(make-obsolete-variable 'special-display-frame-alist 'display-buffer-alist "24.3")

(defun special-display-popup-frame (buffer &optional args)
  "Pop up a frame displaying BUFFER and return its window.
If BUFFER is already displayed in a visible or iconified frame,
raise that frame.  Otherwise, display BUFFER in a new frame.

Optional argument ARGS is a list specifying additional
information.

If ARGS is an alist, use it as a list of frame parameters.  If
these parameters contain (same-window . t), display BUFFER in
the selected window.  If they contain (same-frame . t), display
BUFFER in a window of the selected frame.

If ARGS is a list whose car is a symbol, use (car ARGS) as a
function to do the work.  Pass it BUFFER as first argument, and
pass the elements of (cdr ARGS) as the remaining arguments."
  (if (and args (symbolp (car args)))
      (apply (car args) buffer (cdr args))
    (let ((window (get-buffer-window buffer 0)))
      (or
       ;; If we have a window already, make it visible.
       (when window
	 (let ((frame (window-frame window)))
	   (make-frame-visible frame)
	   (raise-frame frame)
	   (display-buffer-record-window 'reuse window buffer)
	   window))
       ;; Reuse the current window if the user requested it.
       (when (cdr (assq 'same-window args))
	 (condition-case nil
	     (progn (switch-to-buffer buffer nil t) (selected-window))
	   (error nil)))
       ;; Stay on the same frame if requested.
       (when (or (cdr (assq 'same-frame args)) (cdr (assq 'same-window args)))
	 (let* ((pop-up-windows t)
		pop-up-frames
		special-display-buffer-names special-display-regexps)
	   (display-buffer buffer)))
       ;; If no window yet, make one in a new frame.
       (let* ((frame
	       (with-current-buffer buffer
		 (make-frame (append args special-display-frame-alist))))
	      (window (frame-selected-window frame)))
	 (display-buffer-record-window 'frame window buffer)
	 (unless (eq buffer (window-buffer window))
	   (set-window-buffer window buffer)
	   (set-window-prev-buffers window nil))
	 (set-window-dedicated-p window t)
	 window)))))

(defcustom special-display-function 'special-display-popup-frame
  "Function to call for displaying special buffers.
This function is called with two arguments - the buffer and,
optionally, a list - and should return a window displaying that
buffer.  The default value usually makes a separate frame for the
buffer using `special-display-frame-alist' to specify the frame
parameters.  See the definition of `special-display-popup-frame'
for how to specify such a function.

A buffer is special when its name is either listed in
`special-display-buffer-names' or matches a regexp in
`special-display-regexps'.

The specified function should call `display-buffer-record-window'
with corresponding arguments to set up the quit-restore parameter
of the window used."
  :type 'function
  :group 'frames)
(make-obsolete-variable 'special-display-function 'display-buffer-alist "24.3")

(defcustom same-window-buffer-names nil
  "List of names of buffers that should appear in the \"same\" window.
`display-buffer' and `pop-to-buffer' show a buffer whose name is
on this list in the selected rather than some other window.

An element of this list can be a cons cell instead of just a
string.  In that case, the cell's car must be a string specifying
the buffer name.  This is for compatibility with
`special-display-buffer-names'; the cdr of the cons cell is
ignored.

This variable is provided for backward compatibility only and
should not be used in new code.  Customize `display-buffer-alist'
instead.  See Info node `(elisp) Choosing Window Options' in the
Emacs Lisp manual for an example.

See also `same-window-regexps'."
 :type '(repeat (string :format "%v"))
 :group 'windows)

(defcustom same-window-regexps nil
  "List of regexps saying which buffers should appear in the \"same\" window.
`display-buffer' and `pop-to-buffer' show a buffer whose name
matches a regexp on this list in the selected rather than some
other window.

An element of this list can be a cons cell instead of just a
string.  In that case, the cell's car must be a regexp matching
the buffer name.  This is for compatibility with
`special-display-regexps'; the cdr of the cons cell is ignored.

This variable is provided for backward compatibility only and
should not be used in new code.  Customize `display-buffer-alist'
instead.  See Info node `(elisp) Choosing Window Options' in the
Emacs Lisp manual for an example.

See also `same-window-buffer-names'."
  :type '(repeat (regexp :format "%v"))
  :group 'windows)

(defun same-window-p (buffer-name)
  "Return non-nil if buffer BUFFER-NAME would be shown in the \"same\" window.
This function returns non-nil if `display-buffer' or
`pop-to-buffer' would show a buffer named BUFFER-NAME in the
selected rather than (as usual) some other window.  See
`same-window-buffer-names' and `same-window-regexps'."
  (cond
   ((not (stringp buffer-name)))
   ;; The elements of `same-window-buffer-names' can be buffer
   ;; names or cons cells whose cars are buffer names.
   ((member buffer-name same-window-buffer-names))
   ((assoc buffer-name same-window-buffer-names))
   ((catch 'found
      (dolist (regexp same-window-regexps)
	;; The elements of `same-window-regexps' can be regexps
	;; or cons cells whose cars are regexps.
	(when (or (and (stringp regexp)
		       (string-match-p regexp buffer-name))
		  (and (consp regexp) (stringp (car regexp))
		       (string-match-p (car regexp) buffer-name)))
	  (throw 'found t)))))))

(defcustom pop-up-frames nil
  "Whether `display-buffer' should make a separate frame.
If nil, never make a separate frame.
If the value is `graphic-only', make a separate frame
on graphic displays only.
Any other non-nil value means always make a separate frame.

This variable is provided mainly for backward compatibility and
should not be used in new code.  To make `display-buffer' behave
as if this were t, customize `display-buffer-base-action'
instead.  See Info node `(elisp) Choosing Window Options' in the
Emacs Lisp manual for an example."
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "On graphic displays only" graphic-only)
	  (const :tag "Always" t))
  :group 'windows)

(defcustom display-buffer-reuse-frames nil
  "Non-nil means `display-buffer' should reuse frames.
If the buffer in question is already displayed in a frame, raise
that frame."
  :type 'boolean
  :version "21.1"
  :group 'windows)

(make-obsolete-variable
 'display-buffer-reuse-frames
 "use a `reusable-frames' alist entry in `display-buffer-alist'."
 "24.3")

(defcustom pop-up-windows t
  "Non-nil means `display-buffer' should make a new window.
This variable is provided mainly for backward compatibility and
should not be used in new code.  To make `display-buffer' behave
as if this were t, customize `display-buffer-base-action'
instead.  See Info node `(elisp) Choosing Window Options' in the
Emacs Lisp manual for an example."
  :type 'boolean
  :group 'windows)

(defcustom split-window-preferred-function 'split-window-sensibly
  "Function called by `display-buffer' routines to split a window.
This function is called with a window as single argument and is
supposed to split that window and return the new window.  If the
window can (or shall) not be split, it is supposed to return nil.
The default is to call the function `split-window-sensibly' which
tries to split the window in a way which seems most suitable.
You can customize the options `split-height-threshold' and/or
`split-width-threshold' in order to have `split-window-sensibly'
prefer either vertical or horizontal splitting.

If you set this to any other function, bear in mind that the
`display-buffer' routines may call this function two times.  The
argument of the first call is the largest window on its frame.
If that call fails to return a live window, the function is
called again with the least recently used window as argument.  If
that call fails too, `display-buffer' will use an existing window
to display its buffer.

The window selected at the time `display-buffer' was invoked is
still selected when this function is called.  Hence you can
compare the window argument with the value of `selected-window'
if you intend to split the selected window instead or if you do
not want to split the selected window."
  :type 'function
  :version "23.1"
  :group 'windows)

(defcustom split-height-threshold 80
  "Minimum height for splitting windows sensibly.
If this is an integer, `split-window-sensibly' may split a window
vertically only if it has at least this many lines.  If this is
nil, `split-window-sensibly' is not allowed to split a window
vertically.  If, however, a window is the only window on its
frame, or all the other ones are dedicated,
`split-window-sensibly' may split it vertically disregarding the
value of this variable."
  :type '(choice (const nil) (integer :tag "lines"))
  :version "23.1"
  :group 'windows)

(defcustom split-width-threshold 160
  "Minimum width for splitting windows sensibly.
If this is an integer, `split-window-sensibly' may split a window
horizontally only if it has at least this many columns.  If this
is nil, `split-window-sensibly' is not allowed to split a window
horizontally."
  :type '(choice (const nil) (integer :tag "columns"))
  :version "23.1"
  :group 'windows)

(defun window-splittable-p (window &optional horizontal)
  "Return non-nil if `split-window-sensibly' may split WINDOW.
Optional argument HORIZONTAL nil or omitted means check whether
`split-window-sensibly' may split WINDOW vertically.  HORIZONTAL
non-nil means check whether WINDOW may be split horizontally.

WINDOW may be split vertically when the following conditions
hold:
- `window-size-fixed' is either nil or equals `width' for the
  buffer of WINDOW.
- `split-height-threshold' is an integer and WINDOW is at least as
  high as `split-height-threshold'.
- When WINDOW is split evenly, the emanating windows are at least
  `window-min-height' lines tall and can accommodate at least one
  line plus - if WINDOW has one - a mode line.

WINDOW may be split horizontally when the following conditions
hold:
- `window-size-fixed' is either nil or equals `height' for the
  buffer of WINDOW.
- `split-width-threshold' is an integer and WINDOW is at least as
  wide as `split-width-threshold'.
- When WINDOW is split evenly, the emanating windows are at least
  `window-min-width' or two (whichever is larger) columns wide."
  (when (and (window-live-p window)
             (not (window-parameter window 'window-side)))
    (with-current-buffer (window-buffer window)
      (if horizontal
	  ;; A window can be split horizontally when its width is not
	  ;; fixed, it is at least `split-width-threshold' columns wide
	  ;; and at least twice as wide as `window-min-width' and 2 (the
	  ;; latter value is hardcoded).
	  (and (memq window-size-fixed '(nil height))
	       ;; Testing `window-full-width-p' here hardly makes any
	       ;; sense nowadays.  This can be done more intuitively by
	       ;; setting up `split-width-threshold' appropriately.
	       (numberp split-width-threshold)
	       (>= (window-width window)
		   (max split-width-threshold
			(* 2 (max window-min-width 2)))))
	;; A window can be split vertically when its height is not
	;; fixed, it is at least `split-height-threshold' lines high,
	;; and it is at least twice as high as `window-min-height' and 2
	;; if it has a mode line or 1.
	(and (memq window-size-fixed '(nil width))
	     (numberp split-height-threshold)
	     (>= (window-height window)
		 (max split-height-threshold
		      (* 2 (max window-min-height
				(if mode-line-format 2 1))))))))))

(defun split-window-sensibly (&optional window)
  "Split WINDOW in a way suitable for `display-buffer'.
WINDOW defaults to the currently selected window.
If `split-height-threshold' specifies an integer, WINDOW is at
least `split-height-threshold' lines tall and can be split
vertically, split WINDOW into two windows one above the other and
return the lower window.  Otherwise, if `split-width-threshold'
specifies an integer, WINDOW is at least `split-width-threshold'
columns wide and can be split horizontally, split WINDOW into two
windows side by side and return the window on the right.  If this
can't be done either and WINDOW is the only window on its frame,
try to split WINDOW vertically disregarding any value specified
by `split-height-threshold'.  If that succeeds, return the lower
window.  Return nil otherwise.

By default `display-buffer' routines call this function to split
the largest or least recently used window.  To change the default
customize the option `split-window-preferred-function'.

You can enforce this function to not split WINDOW horizontally,
by setting (or binding) the variable `split-width-threshold' to
nil.  If, in addition, you set `split-height-threshold' to zero,
chances increase that this function does split WINDOW vertically.

In order to not split WINDOW vertically, set (or bind) the
variable `split-height-threshold' to nil.  Additionally, you can
set `split-width-threshold' to zero to make a horizontal split
more likely to occur.

Have a look at the function `window-splittable-p' if you want to
know how `split-window-sensibly' determines whether WINDOW can be
split."
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window)
	     ;; Split window vertically.
	     (with-selected-window window
	       (split-window-below)))
	(and (window-splittable-p window t)
	     ;; Split window horizontally.
	     (with-selected-window window
	       (split-window-right)))
	(and
         ;; If WINDOW is the only usable window on its frame (it is
         ;; the only one or, not being the only one, all the other
         ;; ones are dedicated) and is not the minibuffer window, try
         ;; to split it vertically disregarding the value of
         ;; `split-height-threshold'.
         (let ((frame (window-frame window)))
           (or
            (eq window (frame-root-window frame))
            (catch 'done
              (walk-window-tree (lambda (w)
                                  (unless (or (eq w window)
                                              (window-dedicated-p w))
                                    (throw 'done nil)))
                                frame nil 'nomini)
              t)))
	 (not (window-minibuffer-p window))
	 (let ((split-height-threshold 0))
	   (when (window-splittable-p window)
	     (with-selected-window window
	       (split-window-below))))))))

(defun window--try-to-split-window (window &optional alist)
  "Try to split WINDOW.
Return value returned by `split-window-preferred-function' if it
represents a live window, nil otherwise."
      (and (window-live-p window)
	   (not (frame-parameter (window-frame window) 'unsplittable))
	   (let* ((window-combination-limit
		   ;; When `window-combination-limit' equals
		   ;; `display-buffer' or equals `resize-window' and a
		   ;; `window-height' or `window-width' alist entry are
		   ;; present, bind it to t so resizing steals space
		   ;; preferably from the window that was split.
		   (if (or (eq window-combination-limit 'display-buffer)
			   (and (eq window-combination-limit 'window-size)
				(or (cdr (assq 'window-height alist))
				    (cdr (assq 'window-width alist)))))
		       t
		     window-combination-limit))
		  (new-window
		   ;; Since `split-window-preferred-function' might
		   ;; throw an error use `condition-case'.
		   (condition-case nil
		       (funcall split-window-preferred-function window)
		     (error nil))))
	     (and (window-live-p new-window) new-window))))

(defun window--frame-usable-p (frame)
  "Return FRAME if it can be used to display a buffer."
  (when (frame-live-p frame)
    (let ((window (frame-root-window frame)))
      ;; `frame-root-window' may be an internal window which is considered
      ;; "dead" by `window-live-p'.  Hence if `window' is not live we
      ;; implicitly know that `frame' has a visible window we can use.
      (unless (and (window-live-p window)
                   (or (window-minibuffer-p window)
                       ;; If the window is soft-dedicated, the frame is usable.
                       ;; Actually, even if the window is really dedicated,
                       ;; the frame is still usable by splitting it.
                       ;; At least Emacs-22 allowed it, and it is desirable
                       ;; when displaying same-frame windows.
                       nil ; (eq t (window-dedicated-p window))
                       ))
	frame))))

(defvaralias 'even-window-heights 'even-window-sizes)
(defcustom even-window-sizes t
  "If non-nil `display-buffer' will try to even window sizes.
Otherwise `display-buffer' will leave the window configuration
alone.  Special values are `height-only' to even heights only and
`width-only' to even widths only.  Any other value means to even
any of them."
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "Side-by-side windows only" width-only)
	  (const :tag "Windows above or below only" height-only)
	  (const :tag "Always" t))
  :version "25.1"
  :group 'windows)

(defun window--even-window-sizes (window)
  "Even sizes of WINDOW and selected window.
Even only if these windows are the only children of their parent,
`even-window-sizes' has the appropriate value and the selected
window is larger than WINDOW."
  (when (and (= (window-child-count (window-parent window)) 2)
             (eq (window-parent) (window-parent window)))
    (cond
     ((and (not (memq even-window-sizes '(nil height-only)))
           (window-combined-p window t)
           (> (window-total-width) (window-total-width window)))
      (condition-case nil
          (enlarge-window
           (/ (- (window-total-width window) (window-total-width)) 2) t)
        (error nil)))
     ((and (not (memq even-window-sizes '(nil width-only)))
           (window-combined-p window)
           (> (window-total-height) (window-total-height window)))
      (condition-case nil
          (enlarge-window
           (/ (- (window-total-height window) (window-total-height)) 2))
        (error nil))))))

(defun window--display-buffer (buffer window type &optional alist)
  "Display BUFFER in WINDOW.
WINDOW must be a live window chosen by a buffer display action
function for showing BUFFER.  TYPE tells whether WINDOW existed
already before that action function was called or is a new window
created by that function.  ALIST is a buffer display action alist
as compiled by `display-buffer'.

TYPE must be one of the following symbols: `reuse' (which means
WINDOW existed before the call of `display-buffer' and may
already show BUFFER or not), `window' (WINDOW was created on an
existing frame) or `frame' (WINDOW was created on a new frame).
TYPE is passed unaltered to `display-buffer-record-window'.

Handle WINDOW's dedicated flag as follows: If WINDOW already
shows BUFFER, leave it alone.  Otherwise, if ALIST contains a
`dedicated' entry and WINDOW is either new or that entry's value
equals `side', set WINDOW's dedicated flag to the value of that
entry.  Otherwise, if WINDOW is new and the value of
`display-buffer-mark-dedicated' is non-nil, set WINDOW's
dedicated flag to that value.  In any other case, reset WINDOW's
dedicated flag to nil.

Return WINDOW if BUFFER and WINDOW are live."
  (when (and (buffer-live-p buffer) (window-live-p window))
    (display-buffer-record-window type window buffer)
    (unless (eq buffer (window-buffer window))
      ;; Unless WINDOW already shows BUFFER reset its dedicated flag.
      (set-window-dedicated-p window nil)
      (set-window-buffer window buffer))
    (let ((alist-dedicated (assq 'dedicated alist)))
      ;; Maybe dedicate WINDOW to BUFFER if asked for.
      (cond
       ;; Don't dedicate WINDOW if it is dedicated because it shows
       ;; BUFFER already or it is reused and is not a side window.
       ((or (window-dedicated-p window)
            (and (eq type 'reuse) (not (eq (cdr alist-dedicated) 'side)))))
       ;; Otherwise, if ALIST contains a 'dedicated' entry, use that
       ;; entry's value (which may be nil).
       (alist-dedicated
        (set-window-dedicated-p window (cdr alist-dedicated)))
       ;; Otherwise, if 'display-buffer-mark-dedicated' is non-nil,
       ;; use that.
       (display-buffer-mark-dedicated
        (set-window-dedicated-p window display-buffer-mark-dedicated))))
    (when (memq type '(window frame tab))
      (set-window-prev-buffers window nil))

    (when (functionp (cdr (assq 'body-function alist)))
      (let ((inhibit-read-only t)
            (inhibit-modification-hooks t))
        (funcall (cdr (assq 'body-function alist)) window)))

    (let ((quit-restore (window-parameter window 'quit-restore))
	  (height (cdr (assq 'window-height alist)))
	  (width (cdr (assq 'window-width alist)))
	  (size (cdr (assq 'window-size alist)))
	  (preserve-size (cdr (assq 'preserve-size alist))))
      (cond
       ((or (eq type 'frame)
	    (and (eq (car quit-restore) 'same)
		 (eq (nth 1 quit-restore) 'frame)))
	;; A window that never showed another buffer but BUFFER ever
        ;; since it was created on a new frame.
        ;;
        ;; Adjust size of frame if asked for.  We probably should do
        ;; that only for a single window frame.
	(cond
	 ((not size))
	 ((consp size)
	  (let ((width (car size))
		(height (cdr size))
		(frame (window-frame window)))
	    (when (and (numberp width) (numberp height))
	      (set-frame-height
	       frame (+ (frame-height frame)
			(- height (window-total-height window))))
	      (set-frame-width
	       frame (+ (frame-width frame)
			(- width (window-total-width window)))))))
	 ((functionp size)
	  (ignore-errors (funcall size window)))))
       ((or (eq type 'window)
	    (and (eq (car quit-restore) 'same)
		 (eq (nth 1 quit-restore) 'window)))
	;; A window that never showed another buffer but BUFFER ever
        ;; since it was created on an existing frame.
        ;;
        ;; Adjust width and/or height of window if asked for.
	(cond
	 ((not height))
	 ((numberp height)
	  (let* ((new-height
		  (if (integerp height)
		      height
		    (round
		     (* (window-total-height (frame-root-window window))
			height))))
		 (delta (- new-height (window-total-height window))))
	    (when (and (window--resizable-p window delta nil 'safe)
		       (window-combined-p window))
	      (window-resize window delta nil 'safe))))
	 ((functionp height)
	  (ignore-errors (funcall height window))))
	;; Adjust width of window if asked for.
	(cond
	 ((not width))
	 ((numberp width)
	  (let* ((new-width
		  (if (integerp width)
		      width
		    (round
		     (* (window-total-width (frame-root-window window))
			width))))
		 (delta (- new-width (window-total-width window))))
	    (when (and (window--resizable-p window delta t 'safe)
		       (window-combined-p window t))
	      (window-resize window delta t 'safe))))
	 ((functionp width)
	  (ignore-errors (funcall width window))))
	;; Preserve window size if asked for.
	(when (consp preserve-size)
	  (window-preserve-size window t (car preserve-size))
	  (window-preserve-size window nil (cdr preserve-size)))))
      ;; Assign any window parameters specified.
      (let ((parameters (cdr (assq 'window-parameters alist))))
        (dolist (parameter parameters)
          (set-window-parameter
           window (car parameter) (cdr parameter)))))
    window))

(defun window--maybe-raise-frame (frame)
  (make-frame-visible frame)
  (unless (or (frame-parameter frame 'no-focus-on-map)
              ;; Don't raise frames that should not get focus.
              (frame-parameter frame 'no-accept-focus)
              ;; Assume the selected frame is already visible enough.
	      (eq frame (selected-frame))
	      ;; Assume the frame from which we invoked the
	      ;; minibuffer is visible.
	      (and (minibuffer-window-active-p (selected-window))
		   (eq frame (window-frame (minibuffer-selected-window)))))
    (raise-frame frame)))

;; FIXME: Not implemented.
;; FIXME: By the way, there could be more levels of dedication:
;; - `barely' dedicated doesn't prevent reuse of the window, only records that
;;   the window hasn't been used for something else yet.
;; - `soft' (`softly') dedicated only allows reuse when asked explicitly.
;; - `strongly' never allows reuse.
(defvar display-buffer-mark-dedicated nil
  "If non-nil, `display-buffer' marks the windows it creates as dedicated.
The actual non-nil value of this variable will be copied to the
`window-dedicated-p' flag.")

(defconst display-buffer--action-function-custom-type
  '(choice :tag "Function"
	   (const :tag "--" ignore) ; default for insertion
	   (const display-buffer-reuse-window)
	   (const display-buffer-pop-up-window)
	   (const display-buffer-same-window)
	   (const display-buffer-pop-up-frame)
	   (const display-buffer-in-child-frame)
	   (const display-buffer-below-selected)
	   (const display-buffer-at-bottom)
	   (const display-buffer-in-previous-window)
	   (const display-buffer-use-least-recent-window)
	   (const display-buffer-use-some-window)
	   (const display-buffer-use-some-frame)
	   (function :tag "Other function"))
  "Custom type for `display-buffer' action functions.")

(defconst display-buffer--action-custom-type
  `(cons :tag "Action"
	 (choice :tag "Action functions"
		 ,display-buffer--action-function-custom-type
		 (repeat
		  :tag "List of functions"
		  ,display-buffer--action-function-custom-type))
	 (alist :tag "Action arguments"
		:key-type symbol
		:value-type (sexp :tag "Value")))
  "Custom type for `display-buffer' actions.")

(defvar display-buffer-overriding-action '(nil . nil)
  "Overriding action for buffer display.
This action overrides all the other actions in the action
variables and arguments passed to `display-buffer'.  The value
should be a cons cell (FUNCTIONS . ALIST), where FUNCTIONS is a
function or a list of functions.  Each function should accept two
arguments: a buffer to display and an alist similar to ALIST.
See `display-buffer' for details.

This variable is not intended for user customization.  Lisp
programs should never set this variable permanently but may bind
it around calls of buffer display functions like `display-buffer'
or `pop-to-buffer'.  Since such a binding will affect any nested
buffer display requests, this variable should be used with utmost
care.")
(put 'display-buffer-overriding-action 'risky-local-variable t)

(defcustom display-buffer-alist nil
  "Alist of user-defined conditional actions for `display-buffer'.
Its value takes effect before processing the ACTION argument of
`display-buffer' and before `display-buffer-base-action' and
`display-buffer-fallback-action', but after
`display-buffer-overriding-action', which see.

If non-nil, this is an alist of elements (CONDITION . ACTION),
where:

 CONDITION is either a regexp matching buffer names, or a
  function that takes two arguments - a buffer name and the
  ACTION argument of `display-buffer' - and returns a boolean.

 ACTION is a cons cell (FUNCTIONS . ALIST), where FUNCTIONS is an
  action function or a list of action functions and ALIST is an
  action alist.  Each such action function should accept two
  arguments: a buffer to display and an alist of the same form as
  ALIST.  See `display-buffer' for details.

`display-buffer' scans this alist until it either finds a
matching regular expression or the function specified by a
condition returns non-nil.  In any of these cases, it adds the
associated action to the list of actions it will try."
  :type `(alist :key-type
		(choice :tag "Condition"
			regexp
			(function :tag "Matcher function"))
		:value-type ,display-buffer--action-custom-type)
  :risky t
  :version "24.1"
  :group 'windows)

(defcustom display-buffer-base-action '(nil . nil)
  "User-specified default action for `display-buffer'.
This is the default action used by `display-buffer' if no other
actions are specified or all fail, before falling back on
`display-buffer-fallback-action'.

It should be a cons cell (FUNCTIONS . ALIST), where FUNCTIONS is
an action function or a list of action functions and ALIST is an
action alist.  Each such action function should accept two
arguments: a buffer to display and an alist of the same form as
ALIST.  See `display-buffer' for details."
  :type display-buffer--action-custom-type
  :risky t
  :version "24.1"
  :group 'windows)

(defconst display-buffer-fallback-action
  '((display-buffer--maybe-same-window  ;FIXME: why isn't this redundant?
     display-buffer-reuse-window
     display-buffer--maybe-pop-up-frame-or-window
     display-buffer-in-previous-window
     display-buffer-use-some-window
     ;; If all else fails, pop up a new frame.
     display-buffer-pop-up-frame))
  "Default fallback action for `display-buffer'.
This is the action used by `display-buffer' if no other actions
have been specified, for example, by the user options
`display-buffer-alist' or `display-buffer-base-action', or they
all fail.  It should never be set by programs or users.  See
`display-buffer'.")
(put 'display-buffer-fallback-action 'risky-local-variable t)

(defun display-buffer-assq-regexp (buffer-name alist action)
  "Retrieve ALIST entry corresponding to BUFFER-NAME.
This returns the cdr of the alist entry ALIST if either its key
is a string that matches BUFFER-NAME, as reported by
`string-match-p'; or if the key is a function that returns
non-nil when called with three arguments: the ALIST key,
BUFFER-NAME and ACTION.  ACTION should have the form of the
action argument passed to `display-buffer'."
  (catch 'match
    (dolist (entry alist)
      (let ((key (car entry)))
	(when (or (and (stringp key)
		       (string-match-p key buffer-name))
		  (and (functionp key)
		       (funcall key buffer-name action)))
	  (throw 'match (cdr entry)))))))

(defvar display-buffer--same-window-action
  '(display-buffer-same-window
    (inhibit-same-window . nil))
  "A `display-buffer' action for displaying in the same window.
Specifies to call `display-buffer-same-window'.")
(put 'display-buffer--same-window-action 'risky-local-variable t)

(defvar display-buffer--other-frame-action
  '((display-buffer-reuse-window
     display-buffer-pop-up-frame)
    (reusable-frames . 0)
    (inhibit-same-window . t))
  "A `display-buffer' action for displaying in another frame.
Specifies to call `display-buffer-reuse-window', and if that
fails, call `display-buffer-pop-up-frame'.")
(put 'display-buffer--other-frame-action 'risky-local-variable t)

(defun display-buffer (buffer-or-name &optional action frame)
  "Display BUFFER-OR-NAME in some window, without selecting it.
To change which window is used, set `display-buffer-alist'
to an expression containing one of these \"action\" functions:

 `display-buffer-same-window' -- Use the selected window.
 `display-buffer-reuse-window' -- Use a window already showing
    the buffer.
 `display-buffer-in-previous-window' -- Use a window that did
    show the buffer before.
 `display-buffer-use-some-window' -- Use some existing window.
 `display-buffer-use-least-recent-window' -- Try to avoid re-using
    windows that have recently been switched to.
 `display-buffer-pop-up-window' -- Pop up a new window.
 `display-buffer-below-selected' -- Use or pop up a window below
    the selected one.
 `display-buffer-at-bottom' -- Use or pop up a window at the
    bottom of the selected frame.
 `display-buffer-pop-up-frame' -- Show the buffer on a new frame.
 `display-buffer-in-child-frame' -- Show the buffer in a
    child frame.
 `display-buffer-no-window' -- Do not display the buffer and
    have `display-buffer' return nil immediately.

For instance:

   (setq display-buffer-alist '((\".*\" display-buffer-at-bottom)))

Buffer display can be further customized to a very high degree;
the rest of this docstring explains some of the many
possibilities, and also see Info node `(emacs)Window Choice' for
more information.

BUFFER-OR-NAME must be a buffer or a string naming a live buffer.
Return the window chosen for displaying that buffer, or nil if no
such window is found.

Optional argument ACTION, if non-nil, should specify a buffer
display action of the form (FUNCTIONS . ALIST).  FUNCTIONS is
either an \"action function\" or a possibly empty list of action
functions.  ALIST is a possibly empty \"action alist\".

An action function is a function that accepts two arguments: the
buffer to display and an action alist.  Based on those arguments,
it should try to display the buffer in a window and return that
window.  An action alist is an association list mapping symbols
to values.  Action functions use the action alist passed to them
to fine-tune their behaviors.

`display-buffer' builds a list of action functions and an action
alist by combining any action functions and alists specified by
`display-buffer-overriding-action', `display-buffer-alist', the
ACTION argument, `display-buffer-base-action', and
`display-buffer-fallback-action' (in order).  Then it calls each
function in the combined function list in turn, passing the
buffer as the first argument and the combined action alist as the
second argument, until one of the functions returns non-nil.

See above for the action functions and the action they try to
perform.

Action alist entries are:
 `inhibit-same-window' -- A non-nil value prevents the same
    window from being used for display.
 `inhibit-switch-frame' -- A non-nil value prevents any frame
    used for showing the buffer from being raised or selected.
 `reusable-frames' -- The value specifies the set of frames to
    search for a window that already displays the buffer.
    Possible values are nil (the selected frame), t (any live
    frame), visible (any visible frame), 0 (any visible or
    iconified frame) or an existing live frame.
 `pop-up-frame-parameters' -- The value specifies an alist of
    frame parameters to give a new frame, if one is created.
 `window-height' -- The value specifies the desired height of the
    window chosen and is either an integer (the total height of
    the window), a floating point number (the fraction of its
    total height with respect to the total height of the frame's
    root window) or a function to be called with one argument -
    the chosen window.  The function is supposed to adjust the
    height of the window; its return value is ignored.  Suitable
    functions are `shrink-window-if-larger-than-buffer' and
    `fit-window-to-buffer'.
 `window-width' -- The value specifies the desired width of the
    window chosen and is either an integer (the total width of
    the window), a floating point number (the fraction of its
    total width with respect to the width of the frame's root
    window) or a function to be called with one argument - the
    chosen window.  The function is supposed to adjust the width
    of the window; its return value is ignored.
 `preserve-size' -- The value should be either (t . nil) to
    preserve the width of the chosen window, (nil . t) to
    preserve its height or (t . t) to preserve its height and
    width in future changes of the window configuration.
 `window-parameters' -- The value specifies an alist of window
    parameters to give the chosen window.
 `allow-no-window' -- A non-nil value means that `display-buffer'
    may not display the buffer and return nil immediately.
 `body-function' -- A function called with one argument - the
    displayed window.  It is called after the buffer is
    displayed, and before `window-height', `window-width'
    and `preserve-size' are applied.  The function is supposed
    to fill the window body with some contents that might depend
    on dimensions of the displayed window.

The entries `window-height', `window-width' and `preserve-size'
are applied only when the window used for displaying the buffer
never showed another buffer before.

The ACTION argument can also have a non-nil and non-list value.
This means to display the buffer in a window other than the
selected one, even if it is already displayed in the selected
window.  If called interactively with a prefix argument, ACTION
is t.  Non-interactive calls should always supply a list or nil.

The optional third argument FRAME, if non-nil, acts like a
\(reusable-frames . FRAME) entry appended to the action alist
specified by the ACTION argument."
  (interactive (list (read-buffer "Display buffer: " (other-buffer))
		     (if current-prefix-arg t)))
  (let ((buffer (if (bufferp buffer-or-name)
		    buffer-or-name
		  (get-buffer buffer-or-name)))
	;; Make sure that when we split windows the old window keeps
	;; point, bug#14829.
	(split-window-keep-point t)
	;; Handle the old form of the first argument.
	(inhibit-same-window (and action (not (listp action)))))
    (unless (listp action) (setq action nil))
    (if display-buffer-function
	;; If `display-buffer-function' is defined, let it do the job.
	(funcall display-buffer-function buffer inhibit-same-window)
      ;; Otherwise, use the defined actions.
      (let* ((user-action
	      (display-buffer-assq-regexp
	       (buffer-name buffer) display-buffer-alist action))
             (special-action (display-buffer--special-action buffer))
	     ;; Extra actions from the arguments to this function:
	     (extra-action
	      (cons nil (append (if inhibit-same-window
				    '((inhibit-same-window . t)))
				(if frame
				    `((reusable-frames . ,frame))))))
	     ;; Construct action function list and action alist.
	     (actions (list display-buffer-overriding-action
			    user-action special-action action extra-action
			    display-buffer-base-action
			    display-buffer-fallback-action))
	     (functions (apply 'append
			       (mapcar (lambda (x)
					 (setq x (car x))
					 (if (functionp x) (list x) x))
				       actions)))
	     (alist (apply 'append (mapcar 'cdr actions)))
	     window)
	(unless (buffer-live-p buffer)
	  (error "Invalid buffer"))
	(while (and functions (not window))
	  (setq window (funcall (car functions) buffer alist)
	  	functions (cdr functions)))
	(and (windowp window) window)))))

(defun display-buffer-other-frame (buffer)
  "Display buffer BUFFER preferably in another frame.
This function attempts to look for a window displaying BUFFER,
on all the frames on the current terminal, skipping the selected
window; if that fails, it pops up a new frame.
This uses the function `display-buffer' as a subroutine; see
its documentation for additional customization information."
  (interactive "bDisplay buffer in other frame: ")
  (display-buffer buffer display-buffer--other-frame-action t))

;;; `display-buffer' action functions:

(defun display-buffer-use-some-frame (buffer alist)
  "Display BUFFER in an existing frame that meets a predicate.
The default predicate is to use any frame other than the selected
frame.  If successful, return the window used; otherwise return
nil.

ALIST is an association list of action symbols and values.  See
Info node `(elisp) Buffer Display Action Alists' for details of
such alists.

If ALIST has a non-nil `inhibit-switch-frame' entry, avoid
raising the frame.  If it has a non-nil `frame-predicate' entry,
its value is a function taking one argument (a frame), returning
non-nil if the frame is a candidate; this function replaces the
default predicate.  If ALIST has a non-nil `inhibit-same-window'
entry, avoid using the currently selected window (only useful
with a frame-predicate that allows using the selected frame).

This is an action function for buffer display, see Info
node `(elisp) Buffer Display Action Functions'.  It should be
called only by `display-buffer' or a function directly or
indirectly called by the latter."
  (let* ((predicate
          (or (cdr (assq 'frame-predicate alist))
              (lambda (frame)
                (and (not (eq frame (selected-frame)))
                     (get-lru-window frame)))))
         (frame (car (filtered-frame-list predicate)))
         (window
          (and frame
               (get-lru-window
                frame nil (cdr (assq 'inhibit-same-window alist))))))
    (when window
      (prog1
          (window--display-buffer buffer window 'reuse alist)
        (unless (cdr (assq 'inhibit-switch-frame alist))
          (window--maybe-raise-frame frame))))))

(defun display-buffer-same-window (buffer alist)
  "Display BUFFER in the selected window.
ALIST is an association list of action symbols and values.  See
Info node `(elisp) Buffer Display Action Alists' for details of
such alists.

This function fails if ALIST has an `inhibit-same-window'
element whose value is non-nil, or if the selected window is a
minibuffer window or is dedicated to another buffer; in that case,
return nil.  Otherwise, return the selected window.

This is an action function for buffer display, see Info
node `(elisp) Buffer Display Action Functions'.  It should be
called only by `display-buffer' or a function directly or
indirectly called by the latter."
  (unless (or (cdr (assq 'inhibit-same-window alist))
	      (window-minibuffer-p)
	      (window-dedicated-p))
    (window--display-buffer buffer (selected-window) 'reuse alist)))

(defun display-buffer--maybe-same-window (buffer alist)
  "Conditionally display BUFFER in the selected window.
ALIST is an association list of action symbols and values.  See
Info node `(elisp) Buffer Display Action Alists' for details of
such alists.

If `same-window-p' returns non-nil for BUFFER's name, call
`display-buffer-same-window' and return its value.  Otherwise,
return nil."
  (and (same-window-p (buffer-name buffer))
       (display-buffer-same-window buffer alist)))

(defun display-buffer-reuse-window (buffer alist)
  "Return a window that is already displaying BUFFER.
Preferably use a window on the selected frame if such a window
exists.  Return nil if no usable window is found.

ALIST is an association list of action symbols and values.  See
Info node `(elisp) Buffer Display Action Alists' for details of
such alists.

If ALIST has a non-nil `inhibit-same-window' entry, the selected
window is not eligible for reuse.

If ALIST contains a `reusable-frames' entry, its value determines
which frames to search for a reusable window:
  nil -- the selected frame (actually the last non-minibuffer frame)
  A frame   -- just that frame
  `visible' -- all visible frames
  0   -- all frames on the current terminal
  t   -- all frames.

If ALIST contains no `reusable-frames' entry, search just the
selected frame if `display-buffer-reuse-frames' and
`pop-up-frames' are both nil; search all frames on the current
terminal if either of those variables is non-nil.

If ALIST has a non-nil `inhibit-switch-frame' entry, then in the
event that a window on another frame is chosen, avoid raising
that frame.

This is an action function for buffer display, see Info
node `(elisp) Buffer Display Action Functions'.  It should be
called only by `display-buffer' or a function directly or
indirectly called by the latter."
  (let* ((alist-entry (assq 'reusable-frames alist))
	 (frames (cond (alist-entry (cdr alist-entry))
		       ((if (eq pop-up-frames 'graphic-only)
			    (display-graphic-p)
			  pop-up-frames)
			0)
		       (display-buffer-reuse-frames 0)
		       (t (last-nonminibuffer-frame))))
	 (window (if (and (eq buffer (window-buffer))
			  (not (cdr (assq 'inhibit-same-window alist))))
		     (selected-window)
                   ;; Preferably use a window on the selected frame,
                   ;; if such a window exists (Bug#36680).
                   (let* ((windows (delq (selected-window)
                                         (get-buffer-window-list
                                          buffer 'nomini frames)))
                          (first (car windows))
                          (this-frame (selected-frame)))
                     (cond
                      ((eq (window-frame first) this-frame)
                       first)
                      ((catch 'found
                         (dolist (next (cdr windows))
                           (when (eq (window-frame next) this-frame)
                             (throw 'found next)))))
                      (t first))))))
    (when (window-live-p window)
      (prog1 (window--display-buffer buffer window 'reuse alist)
	(unless (cdr (assq 'inhibit-switch-frame alist))
	  (window--maybe-raise-frame (window-frame window)))))))

(defun display-buffer-reuse-mode-window (buffer alist)
  "Return a window based on the mode of the buffer it displays.
Display BUFFER in the returned window.  Return nil if no usable
window is found.

ALIST is an association list of action symbols and values.  See
Info node `(elisp) Buffer Display Action Alists' for details of
such alists.

If ALIST contains a `mode' entry, its value is a major mode (a
symbol) or a list of modes.  A window is a candidate if it
displays a buffer that derives from one of the given modes.  When
ALIST contains no `mode' entry, the current major mode of BUFFER
is used.

The behavior is also controlled by entries for
`inhibit-same-window', `reusable-frames' and
`inhibit-switch-frame' as is done in the function
`display-buffer-reuse-window'.

This is an action function for buffer display, see Info
node `(elisp) Buffer Display Action Functions'.  It should be
called only by `display-buffer' or a function directly or
indirectly called by the latter."
  (let* ((alist-entry (assq 'reusable-frames alist))
         (alist-mode-entry (assq 'mode alist))
	 (frames (cond (alist-entry (cdr alist-entry))
		       ((if (eq pop-up-frames 'graphic-only)
			    (display-graphic-p)
			  pop-up-frames)
			0)
		       (display-buffer-reuse-frames 0)
		       (t (last-nonminibuffer-frame))))
         (inhibit-same-window-p (cdr (assq 'inhibit-same-window alist)))
	 (windows (window-list-1 nil 'nomini frames))
         (buffer-mode (with-current-buffer buffer major-mode))
         (allowed-modes (if alist-mode-entry
                            (cdr alist-mode-entry)
                          buffer-mode))
         (curwin (selected-window))
         (curframe (selected-frame)))
    (unless (listp allowed-modes)
      (setq allowed-modes (list allowed-modes)))
    (let (same-mode-same-frame
          same-mode-other-frame
          derived-mode-same-frame
          derived-mode-other-frame)
      (dolist (window windows)
        (let ((mode?
               (with-current-buffer (window-buffer window)
                 (cond ((memq major-mode allowed-modes)
                        'same)
                       ((apply #'derived-mode-p allowed-modes)
                        'derived)))))
          (when (and mode?
                     (not (and inhibit-same-window-p
                               (eq window curwin))))
            (push window (if (eq curframe (window-frame window))
                             (if (eq mode? 'same)
                                 same-mode-same-frame
                               derived-mode-same-frame)
                           (if (eq mode? 'same)
                               same-mode-other-frame
                             derived-mode-other-frame))))))
      (let ((window (car (nconc same-mode-same-frame
                                same-mode-other-frame
                                derived-mode-same-frame
                                derived-mode-other-frame))))
        (when (window-live-p window)
          (prog1 (window--display-buffer buffer window 'reuse alist)
            (unless (cdr (assq 'inhibit-switch-frame alist))
              (window--maybe-raise-frame (window-frame window)))))))))

(defun display-buffer--special-action (buffer)
  "Return special display action for BUFFER, if any.
If `special-display-p' returns non-nil for BUFFER, return an
appropriate display action involving `special-display-function'.
See `display-buffer' for the format of display actions."
  (and special-display-function
       ;; `special-display-p' returns either t or a list of frame
       ;; parameters to pass to `special-display-function'.
       (let ((pars (special-display-p (buffer-name buffer))))
	 (when pars
           (list (list #'display-buffer-reuse-window
                       (lambda (buffer _alist)
                         (funcall special-display-function
                                  buffer (if (listp pars) pars)))))))))

(defun display-buffer-pop-up-frame (buffer alist)
  "Display BUFFER in a new frame.
This works by calling `pop-up-frame-function'.  If successful,
return the window used; otherwise return nil.

ALIST is an association list of action symbols and values.  See
Info node `(elisp) Buffer Display Action Alists' for details of
such alists.

If ALIST has a non-nil `inhibit-switch-frame' entry, avoid
raising the new frame.  A non-nil `pop-up-frame-parameters' entry
specifies an alist of frame parameters to give the new frame.

This is an action function for buffer display, see Info
node `(elisp) Buffer Display Action Functions'.  It should be
called only by `display-buffer' or a function directly or
indirectly called by the latter."
  (let* ((params (cdr (assq 'pop-up-frame-parameters alist)))
	 (pop-up-frame-alist (append params pop-up-frame-alist))
	 (fun pop-up-frame-function)
	 frame window)
    (when (and fun
	       ;; Make BUFFER current so `make-frame' will use it as the
	       ;; new frame's buffer (Bug#15133).
	       (with-current-buffer buffer
		 (setq frame (funcall fun)))
	       (setq window (frame-selected-window frame)))
      (prog1 (window--display-buffer buffer window 'frame alist)
	(unless (cdr (assq 'inhibit-switch-frame alist))
	  (window--maybe-raise-frame frame))))))

(defun display-buffer-pop-up-window (buffer alist)
  "Display BUFFER by popping up a new window.
The new window is created on the selected frame, or in
`last-nonminibuffer-frame' if no windows can be created there.
If successful, return the new window; otherwise return nil.

ALIST is an association list of action symbols and values.  See
Info node `(elisp) Buffer Display Action Alists' for details of
such alists.

If ALIST has a non-nil `inhibit-switch-frame' entry, then in the
event that the new window is created on another frame, avoid
raising the frame.

This is an action function for buffer display, see Info
node `(elisp) Buffer Display Action Functions'.  It should be
called only by `display-buffer' or a function directly or
indirectly called by the latter."
  (let ((frame (or (window--frame-usable-p (selected-frame))
		   (window--frame-usable-p (last-nonminibuffer-frame))))
	window)
    (when (and (or (not (frame-parameter frame 'unsplittable))
		   ;; If the selected frame cannot be split, look at
		   ;; `last-nonminibuffer-frame'.
		   (and (eq frame (selected-frame))
			(setq frame (last-nonminibuffer-frame))
			(window--frame-usable-p frame)
			(not (frame-parameter frame 'unsplittable))))
	       ;; Attempt to split largest or least recently used window.
	       (setq window (or (window--try-to-split-window
				 (get-largest-window frame t) alist)
				(window--try-to-split-window
				 (get-lru-window frame t) alist))))

      (prog1 (window--display-buffer buffer window 'window alist)
	(unless (cdr (assq 'inhibit-switch-frame alist))
	  (window--maybe-raise-frame (window-frame window)))))))

(defun display-buffer--maybe-pop-up-frame-or-window (buffer alist)
  "Try displaying BUFFER based on `pop-up-frames' or `pop-up-windows'.
ALIST is an association list of action symbols and values.  See
Info node `(elisp) Buffer Display Action Alists' for details of
such alists.

If `pop-up-frames' is non-nil (and not `graphic-only' on a
text-only terminal), try with `display-buffer-pop-up-frame'.  If
that cannot be done, and `pop-up-windows' is non-nil, try again
with `display-buffer-pop-up-window'."
  (or (display-buffer--maybe-pop-up-frame buffer alist)
      (display-buffer--maybe-pop-up-window buffer alist)))

(defun display-buffer--maybe-pop-up-frame (buffer alist)
  "Try displaying BUFFER based on `pop-up-frames'.
If `pop-up-frames' is non-nil (and not `graphic-only' on a
text-only terminal), try with `display-buffer-pop-up-frame'.

ALIST is an association list of action symbols and values.  See
Info node `(elisp) Buffer Display Action Alists' for details of
such alists."
  (and (if (eq pop-up-frames 'graphic-only)
	   (display-graphic-p)
	 pop-up-frames)
       (display-buffer-pop-up-frame buffer alist)))

(defun display-buffer--maybe-pop-up-window (buffer alist)
  "Try displaying BUFFER based on `pop-up-windows'.
If `pop-up-windows' is non-nil, try with `display-buffer-pop-up-window'."
  (and pop-up-windows
       (display-buffer-pop-up-window buffer alist)))

(defun display-buffer-in-child-frame (buffer alist)
  "Display BUFFER in a child frame.
By default, this either reuses a child frame of the selected
frame or makes a new child frame of the selected frame.  If
successful, return the window used; otherwise return nil.

ALIST is an association list of action symbols and values.  See
Info node `(elisp) Buffer Display Action Alists' for details of
such alists.

If ALIST has a non-nil `child-frame-parameters' entry, the
corresponding value is an alist of frame parameters to give the
new frame.  A `parent-frame' parameter specifying the selected
frame is provided by default.  If the child frame shall be or
become the child of any other frame, a corresponding entry must
be added to ALIST.

This is an action function for buffer display, see Info
node `(elisp) Buffer Display Action Functions'.  It should be
called only by `display-buffer' or a function directly or
indirectly called by the latter."
  (let* ((parameters
          (append
           (cdr (assq 'child-frame-parameters alist))
           `((parent-frame . ,(selected-frame)))))
	 (parent (or (assq 'parent-frame parameters)
                     (selected-frame)))
         (share (assq 'share-child-frame parameters))
         share1 frame window type)
    (with-current-buffer buffer
      (when (frame-live-p parent)
        (catch 'frame
          (dolist (frame1 (frame-list))
            (when (eq (frame-parent frame1) parent)
              (setq share1 (assq 'share-child-frame
                                 (frame-parameters frame1)))
              (when (eq share share1)
                (setq frame frame1)
                (throw 'frame t))))))

      (if frame
          (progn
            (setq window (frame-selected-window frame))
            (setq type 'reuse))
        (setq frame (make-frame parameters))
        (setq window (frame-selected-window frame))
        (setq type 'frame)))

    (prog1 (window--display-buffer buffer window type alist)
      (unless (cdr (assq 'inhibit-switch-frame alist))
	(window--maybe-raise-frame frame)))))

(defun windows-sharing-edge (&optional window edge within)
  "Return list of live windows sharing the same edge with WINDOW.
WINDOW must be a valid window and defaults to the selected one.
EDGE stands for the edge to share and must be either `left',
`above', `right' or `below'.  Omitted or nil, EDGE defaults to
`left'.

WITHIN nil means to find a live window that shares the opposite
EDGE with WINDOW.  For example, if EDGE equals `left', WINDOW has
to share (part of) the right edge of any window returned.  WITHIN
non-nil means to find all live windows that share the same EDGE
with WINDOW (Window must be internal in this case).  So if EDGE
equals `left', WINDOW's left edge has to fully encompass the left
edge of any window returned."
  (setq window (window-normalize-window window))
  (setq edge (or edge 'left))
  (when (and within (window-live-p window))
    (error "Cannot share edge from within live window %s" window))
  (let ((window-edges (window-edges window nil nil t))
	(horizontal (memq edge '(left right)))
	(n (pcase edge
	     ('left 0) ('above 1) ('right 2) ('below 3))))
    (unless (numberp n)
      (error "Invalid EDGE %s" edge))
    (let ((o (mod (+ 2 n) 4))
	  (p (if horizontal 1 0))
	  (q (if horizontal 3 2))
	  windows)
      (walk-window-tree
       (lambda (other)
	 (let ((other-edges (window-edges other nil nil t)))
	   (when (and (not (eq window other))
		      (= (nth n window-edges)
			 (nth (if within n o) other-edges))
		      (cond
		       ((= (nth p window-edges) (nth p other-edges)))
		       ((< (nth p window-edges) (nth p other-edges))
			(< (nth p other-edges) (nth q window-edges)))
		       (t
			(< (nth p window-edges) (nth q other-edges)))))
	     (setq windows (cons other windows)))))
       (window-frame window) nil 'nomini)
      (reverse windows))))

(defun window--try-to-split-window-in-direction (window direction alist)
  "Try to split WINDOW in DIRECTION.
DIRECTION is passed as SIDE argument to `split-window-no-error'.
ALIST is a buffer display alist."
  (and (not (frame-parameter (window-frame window) 'unsplittable))
       (let* ((window-combination-limit
	       ;; When `window-combination-limit' equals
	       ;; `display-buffer' or equals `resize-window' and a
	       ;; `window-height' or `window-width' alist entry are
	       ;; present, bind it to t so resizing steals space
	       ;; preferably from the window that was split.
	       (if (or (eq window-combination-limit 'display-buffer)
		       (and (eq window-combination-limit 'window-size)
			    (or (cdr (assq 'window-height alist))
				(cdr (assq 'window-width alist)))))
		   t
		 window-combination-limit))
	      (new-window (split-window-no-error window nil direction)))
	 (and (window-live-p new-window) new-window))))

(defun display-buffer-in-direction (buffer alist)
  "Try to display BUFFER in a direction specified by ALIST.
ALIST is an association list of action symbols and values.  See
Info node `(elisp) Buffer Display Action Alists' for details of
such alists.

ALIST has to contain a `direction' entry whose value should be
one of `left', `above' (or `up'), `right' and `below' (or `down').
Other values are usually interpreted as `below'.

If ALIST also contains a `window' entry, its value specifies a
reference window.  That value can be a special symbol like
`main' (which stands for the selected frame's main window) or
`root' (standings for the selected frame's root window) or an
arbitrary valid window.  Any other value (or omitting the
`window' entry) means to use the selected window as reference
window.

This function tries to reuse or split a window such that the
window produced this way is on the side of the reference window
specified by the `direction' entry.

Four special values for `direction' entries allow to implicitly
specify the selected frame's main window as reference window:
`leftmost', `top', `rightmost' and `bottom'.  Hence, instead of
`(direction . left) (window . main)' one can simply write
`(direction . leftmost)'.

This is an action function for buffer display, see Info
node `(elisp) Buffer Display Action Functions'.  It should be
called only by `display-buffer' or a function directly or
indirectly called by the latter."
  (let ((direction (cdr (assq 'direction alist))))
    (when direction
      (let ((window (cdr (assq 'window alist)))
	    within windows other-window-shows-buffer other-window)
	;; Sanitize WINDOW.
	(cond
	 ((or (eq window 'main)
              (memq direction '(top bottom leftmost rightmost)))
	  (setq window (window-main-window)))
	 ((eq window 'root)
	  (setq window (frame-root-window)))
	 ((window-valid-p window))
	 (t
	  (setq window (selected-window))))
	(setq within (not (window-live-p window)))
	;; Sanitize DIRECTION
	(cond
	 ((memq direction '(left above right below)))
	 ((eq direction 'leftmost)
	  (setq direction 'left))
	 ((memq direction '(top up))
	  (setq direction 'above))
	 ((eq direction 'rightmost)
	  (setq direction 'right))
	 ((memq direction '(bottom down))
	  (setq direction 'below))
	 (t
	  (setq direction 'below)))

	(setq alist
	      (append alist
		      `(,(if temp-buffer-resize-mode
		             '(window-height . resize-temp-buffer-window)
	                   '(window-height . fit-window-to-buffer))
	                ,(when temp-buffer-resize-mode
	                   '(preserve-size . (nil . t))))))

	(setq windows (windows-sharing-edge window direction within))
	(dolist (other windows)
	  (cond
	   ((and (not other-window-shows-buffer)
		 (eq buffer (window-buffer other)))
	    (setq other-window-shows-buffer t)
	    (setq other-window other))
	   ((not other-window)
	    (setq other-window other))))
	(or (and other-window-shows-buffer
		 (window--display-buffer buffer other-window 'reuse alist))
	    (and (setq other-window
		       (window--try-to-split-window-in-direction
			window direction alist))
		 (window--display-buffer buffer other-window 'window alist))
	    (and (setq window other-window)
		 (not (window-dedicated-p other-window))
		 (not (window-minibuffer-p other-window))
		 (window--display-buffer buffer other-window 'reuse alist)))))))

;; This should be rewritten as
;; (display-buffer-in-direction buffer (cons '(direction . below) alist))
(defun display-buffer-below-selected (buffer alist)
  "Try displaying BUFFER in a window below the selected window.
If there is a window below the selected one and that window
already displays BUFFER, use that window.  Otherwise, try to
create a new window below the selected one and show BUFFER there.
If that attempt fails as well and there is a non-dedicated window
below the selected one, use that window.

ALIST is an association list of action symbols and values.  See
Info node `(elisp) Buffer Display Action Alists' for details of
such alists.

If ALIST contains a `window-min-height' entry, this function
ensures that the window used is or can become at least as high as
specified by that entry's value.  Note that such an entry alone
will not resize the window per se.  In order to do that, ALIST
must also contain a `window-height' entry with the same value.

This is an action function for buffer display, see Info
node `(elisp) Buffer Display Action Functions'.  It should be
called only by `display-buffer' or a function directly or
indirectly called by the latter."
  (let ((min-height (cdr (assq 'window-min-height alist)))
	window)
    (or (and (setq window (window-in-direction 'below))
             (eq buffer (window-buffer window))
	     (or (not (numberp min-height))
		 (>= (window-height window) min-height)
		 ;; 'window--display-buffer' can resize this window if
		 ;; and only if it has a 'quit-restore' parameter
		 ;; certifying that it always showed BUFFER before.
		 (let ((height (window-height window))
		       (quit-restore (window-parameter window 'quit-restore)))
		   (and quit-restore
			(eq (nth 1 quit-restore) 'window)
			(window-resizable-p window (- min-height height)))))
	     (window--display-buffer buffer window 'reuse alist))
	(and (not (frame-parameter nil 'unsplittable))
             (or (not (numberp min-height))
		 (window-sizable-p nil (- min-height)))
             (let ((split-height-threshold 0)
		   split-width-threshold)
               (setq window (window--try-to-split-window
                             (selected-window) alist)))
             (window--display-buffer buffer window 'window alist))
	(and (setq window (window-in-direction 'below))
             (not (window-dedicated-p window))
	     (or (not (numberp min-height))
		 ;; A window that showed another buffer before cannot
		 ;; be resized.
		 (>= (window-height window) min-height))
	     (window--display-buffer buffer window 'reuse alist)))))

(defun display-buffer--maybe-at-bottom (buffer alist)
  (let ((alist (append alist `(,(if temp-buffer-resize-mode
		                    '(window-height . resize-temp-buffer-window)
	                          '(window-height . fit-window-to-buffer))
	                       ,(when temp-buffer-resize-mode
	                          '(preserve-size . (nil . t)))))))
    (or (display-buffer--maybe-same-window buffer alist)
        (display-buffer-reuse-window buffer alist)
        (display-buffer--maybe-pop-up-frame buffer alist)
        (display-buffer-at-bottom buffer alist))))

;; This should be rewritten as
;; (display-buffer-in-direction buffer (cons '(direction . bottom) alist))
(defun display-buffer-at-bottom (buffer alist)
  "Try displaying BUFFER in a window at the bottom of the selected frame.
This either reuses such a window provided it shows BUFFER
already, splits a window at the bottom of the frame or the
frame's root window, or reuses some window at the bottom of the
selected frame.

ALIST is an association list of action symbols and values.  See
Info node `(elisp) Buffer Display Action Alists' for details of
such alists.

This is an action function for buffer display, see Info
node `(elisp) Buffer Display Action Functions'.  It should be
called only by `display-buffer' or a function directly or
indirectly called by the latter."
  (let (bottom-window bottom-window-shows-buffer window)
    (walk-window-tree
     (lambda (window)
       (cond
	((window-in-direction 'below window))
	((and (not bottom-window-shows-buffer)
	      (eq buffer (window-buffer window)))
	 (setq bottom-window-shows-buffer t)
	 (setq bottom-window window))
	((not bottom-window)
	 (setq bottom-window window))))
     nil nil 'nomini)
    (or (and bottom-window-shows-buffer
	     (window--display-buffer buffer bottom-window 'reuse alist))
	(and (not (frame-parameter nil 'unsplittable))
	     (setq window (split-window-no-error (window-main-window)))
	     (window--display-buffer buffer window 'window alist))
	(and (setq window bottom-window)
	     (not (window-dedicated-p window))
	     (window--display-buffer buffer window 'reuse alist)))))

(defun display-buffer-in-previous-window (buffer alist)
  "Display BUFFER in a window previously showing it.
ALIST is an association list of action symbols and values.  See
Info node `(elisp) Buffer Display Action Alists' for details of
such alists.

If ALIST has a non-nil `inhibit-same-window' entry, the selected
window is not usable.  A dedicated window is usable only if it
already shows BUFFER.  If ALIST contains a `previous-window'
entry, the window specified by that entry (either a variable
or a value) is usable even if it never showed BUFFER before.

If ALIST contains a `reusable-frames' entry, its value determines
which frames to search for a usable window:
  nil -- the selected frame (actually the last non-minibuffer frame)
  A frame   -- just that frame
  `visible' -- all visible frames
  0   -- all frames on the current terminal
  t   -- all frames.

If ALIST contains no `reusable-frames' entry, search just the
selected frame if `display-buffer-reuse-frames' and
`pop-up-frames' are both nil; search all frames on the current
terminal if either of those variables is non-nil.

If more than one window is usable according to these rules,
apply the following order of preference:

- Use the window specified by any `previous-window' ALIST entry,
  provided it is not the selected window.

- Use a window that showed BUFFER before, provided it is not the
  selected window.

- Use the selected window if it is either specified by a
  `previous-window' ALIST entry or showed BUFFER before.

This is an action function for buffer display, see Info
node `(elisp) Buffer Display Action Functions'.  It should be
called only by `display-buffer' or a function directly or
indirectly called by the latter."
  (let* ((alist-entry (assq 'reusable-frames alist))
	 (inhibit-same-window
	  (cdr (assq 'inhibit-same-window alist)))
	 (frames (cond
		  (alist-entry (cdr alist-entry))
		  ((if (eq pop-up-frames 'graphic-only)
		       (display-graphic-p)
		     pop-up-frames)
		   0)
		  (display-buffer-reuse-frames 0)
		  (t (last-nonminibuffer-frame))))
         (previous-window (cdr (assq 'previous-window alist)))
	 best-window second-best-window window)
    ;; Scan windows whether they have shown the buffer recently.
    (catch 'best
      (dolist (window (window-list-1 (frame-first-window) 'nomini frames))
	(when (and (assq buffer (window-prev-buffers window))
		   (not (window-dedicated-p window)))
	  (if (eq window (selected-window))
	      (unless inhibit-same-window
		(setq second-best-window window))
	    (setq best-window window)
	    (throw 'best t)))))
    ;; When ALIST has a `previous-window' entry, that entry may override
    ;; anything we found so far.
    (when (and previous-window (symbolp previous-window)
               (boundp previous-window))
      (setq previous-window (symbol-value previous-window)))
    (when (and (setq window previous-window)
	       (window-live-p window)
	       (or (eq buffer (window-buffer window))
                   (not (window-dedicated-p window))))
      (if (eq window (selected-window))
	  (unless inhibit-same-window
	    (setq second-best-window window))
	(setq best-window window)))
    ;; Return best or second best window found.
    (when (setq window (or best-window second-best-window))
      (window--display-buffer buffer window 'reuse alist))))

(defun display-buffer-use-least-recent-window (buffer alist)
  "Display BUFFER in an existing window, but that hasn't been used lately.
This `display-buffer' action function is like
`display-buffer-use-some-window', but will cycle through windows
when displaying buffers repeatedly, and if there's only a single
window, it will split the window."
  (when-let ((window (display-buffer-use-some-window
                      buffer (cons (cons 'inhibit-same-window t) alist))))
    (window-bump-use-time window)))

(defun display-buffer-use-some-window (buffer alist)
  "Display BUFFER in an existing window.
Search for a usable window, set that window to the buffer, and
return the window.  If no suitable window is found, return nil.

ALIST is an association list of action symbols and values.  See
Info node `(elisp) Buffer Display Action Alists' for details of
such alists.

If ALIST has a non-nil `inhibit-switch-frame' entry, then in the
event that a window on another frame is chosen, avoid raising
that frame.

This is an action function for buffer display, see Info
node `(elisp) Buffer Display Action Functions'.  It should be
called only by `display-buffer' or a function directly or
indirectly called by the latter."
  (let* ((not-this-window (cdr (assq 'inhibit-same-window alist)))
	 (frame (or (window--frame-usable-p (selected-frame))
		    (window--frame-usable-p (last-nonminibuffer-frame))))
	 (window
	  ;; Reuse an existing window.
	  (or (get-lru-window frame nil not-this-window)
	      (let ((window (get-buffer-window buffer 'visible)))
		(unless (and not-this-window
			     (eq window (selected-window)))
		  window))
	      (get-largest-window 'visible nil not-this-window)
	      (let ((window (get-buffer-window buffer 0)))
		(unless (and not-this-window
			     (eq window (selected-window)))
		  window))
	      (get-largest-window 0 nil not-this-window)))
	 (quit-restore (and (window-live-p window)
			    (window-parameter window 'quit-restore)))
	 (quad (nth 1 quit-restore)))
    (when (window-live-p window)
      ;; If the window was used by `display-buffer' before, try to
      ;; resize it to its old height but don't signal an error.
      (when (and (listp quad)
		 (integerp (nth 3 quad))
		 (> (nth 3 quad) (window-total-height window)))
	(condition-case nil
	    (window-resize window (- (nth 3 quad) (window-total-height window)))
	  (error nil)))

      (prog1
	  (window--display-buffer buffer window 'reuse alist)
	(window--even-window-sizes window)
	(unless (cdr (assq 'inhibit-switch-frame alist))
	  (window--maybe-raise-frame (window-frame window)))))))

(defun display-buffer-no-window (_buffer alist)
  "Display BUFFER in no window.
ALIST is an association list of action symbols and values.  See
Info node `(elisp) Buffer Display Action Alists' for details of
such alists.

If ALIST contains a non-nil `allow-no-window' entry, do nothing
and return `fail'.  This allows `display-buffer' to override the
default action and avoid displaying the buffer.  It is assumed
that when the caller specifies a non-nil `allow-no-window' ALIST
entry, it can handle a nil value returned by `display-buffer'.

This is an action function for buffer display, see Info
node `(elisp) Buffer Display Action Functions'.  It should be
called only by `display-buffer' or a function directly or
indirectly called by the latter."
  (when (cdr (assq 'allow-no-window alist))
    'fail))

;;; Display + selection commands:
(defun pop-to-buffer (buffer-or-name &optional action norecord)
  "Display buffer specified by BUFFER-OR-NAME and select its window.
BUFFER-OR-NAME may be a buffer, a string (a buffer name), or nil.
If it is a string not naming an existent buffer, create a buffer
with that name.  If BUFFER-OR-NAME is nil, choose some other
buffer.  In either case, make that buffer current and return it.

This uses `display-buffer' as a subroutine.  The optional ACTION
argument is passed to `display-buffer' as its ACTION argument.
See `display-buffer' for more information.  ACTION is t if called
interactively with a prefix argument, which means to pop to a
window other than the selected one even if the buffer is already
displayed in the selected window.

If a suitable window is found, select that window.  If it is not
on the selected frame, raise that window's frame and give it
input focus.

Optional third arg NORECORD non-nil means do not put this buffer
at the front of the list of recently selected ones."
  (interactive (list (read-buffer "Pop to buffer: " (other-buffer))
		     (if current-prefix-arg t)))
  (let* ((buffer (window-normalize-buffer-to-switch-to buffer-or-name))
         (old-frame (selected-frame))
	 (window (display-buffer buffer action)))
    ;; Don't assume that `display-buffer' has supplied us with a window
    ;; (Bug#24332).
    (if window
        (let ((frame (window-frame window)))
          ;; If we chose another frame, make sure it gets input focus.
          (unless (eq frame old-frame)
            (select-frame-set-input-focus frame norecord))
          ;; Make sure the window is selected (Bug#8615), (Bug#6954)
          (select-window window norecord))
      ;; If `display-buffer' failed to supply a window, just make the
      ;; buffer current.
      (set-buffer buffer))
    ;; Return BUFFER even when we got no window.
    buffer))

(defun pop-to-buffer-same-window (buffer &optional norecord)
  "Select buffer BUFFER in some window, preferably the same one.
BUFFER may be a buffer, a string (a buffer name), or nil.  If it
is a string not naming an existent buffer, create a buffer with
that name.  If BUFFER is nil, choose some other buffer.  Return
the buffer.

Optional argument NORECORD, if non-nil means do not put this
buffer at the front of the list of recently selected ones.

Unlike `pop-to-buffer', this function prefers using the selected
window over popping up a new window or frame.  Specifically, if
the selected window is neither a minibuffer window (as reported
by `window-minibuffer-p'), nor is dedicated to another buffer
(see `window-dedicated-p'), BUFFER will be displayed in the
currently selected window; otherwise it will be displayed in
another window."
  (pop-to-buffer buffer display-buffer--same-window-action norecord))

(defun read-buffer-to-switch (prompt)
  "Read the name of a buffer to switch to, prompting with PROMPT.
Return the name of the buffer as a string.

This function is intended for the `switch-to-buffer' family of
commands since these need to omit the name of the current buffer
from the list of completions and default values."
  (let ((rbts-completion-table (internal-complete-buffer-except)))
    (minibuffer-with-setup-hook
        (lambda ()
          (setq minibuffer-completion-table rbts-completion-table)
          ;; Since rbts-completion-table is built dynamically, we
          ;; can't just add it to the default value of
          ;; icomplete-with-completion-tables, so we add it
          ;; here manually.
          (if (and (boundp 'icomplete-with-completion-tables)
                   (listp icomplete-with-completion-tables))
              (setq-local icomplete-with-completion-tables
                          (cons rbts-completion-table
                                icomplete-with-completion-tables))))
      (read-buffer prompt (other-buffer (current-buffer))
                   (confirm-nonexistent-file-or-buffer)))))

(defun window-normalize-buffer-to-switch-to (buffer-or-name)
  "Normalize BUFFER-OR-NAME argument of buffer switching functions.
If BUFFER-OR-NAME is nil, return the buffer returned by
`other-buffer'.  Else, if a buffer specified by BUFFER-OR-NAME
exists, return that buffer.  If no such buffer exists, create a
buffer with the name BUFFER-OR-NAME and return that buffer."
  (if buffer-or-name
      (or (get-buffer buffer-or-name)
	  (let ((buffer (get-buffer-create buffer-or-name)))
	    (set-buffer-major-mode buffer)
	    buffer))
    (other-buffer)))

(defcustom switch-to-buffer-preserve-window-point t
  "If non-nil, `switch-to-buffer' tries to preserve `window-point'.
If this is nil, `switch-to-buffer' displays the buffer at that
buffer's `point'.  If this is `already-displayed', it tries to
display the buffer at its previous position in the selected
window, provided the buffer is currently displayed in some other
window on any visible or iconified frame.  If this is t, it
unconditionally tries to display the buffer at its previous
position in the selected window.

This variable is ignored if the buffer is already displayed in
the selected window or never appeared in it before, or if
`switch-to-buffer' calls `pop-to-buffer' to display the buffer,
or non-nil `switch-to-buffer-obey-display-actions' displays it
in another window."
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "If already displayed elsewhere" already-displayed)
	  (const :tag "Always" t))
  :group 'windows
  :version "26.1")

(defcustom switch-to-buffer-in-dedicated-window nil
  "Allow switching to buffer in strongly dedicated windows.
If non-nil, allow `switch-to-buffer' to proceed when called
interactively and the selected window is strongly dedicated to
its buffer.

The following values are recognized:

nil - disallow switching; signal an error

prompt - prompt user whether to allow switching

pop - perform `pop-to-buffer' instead

t - undedicate selected window and switch

When called non-interactively, `switch-to-buffer' always signals
an error when the selected window is dedicated to its buffer and
FORCE-SAME-WINDOW is non-nil."
  :type '(choice
	  (const :tag "Disallow" nil)
	  (const :tag "Prompt" prompt)
	  (const :tag "Pop" pop)
	  (const :tag "Allow" t))
  :group 'windows
  :version "25.1")

(defcustom switch-to-buffer-obey-display-actions nil
  "If non-nil, `switch-to-buffer' runs `pop-to-buffer-same-window' instead.
This means that when switching the buffer it respects display actions
specified by `display-buffer-overriding-action', `display-buffer-alist'
and other display related variables.  So `switch-to-buffer' will display
the buffer in the window specified by the rules from these variables."
  :type 'boolean
  :group 'windows
  :version "27.1")

(defun switch-to-buffer (buffer-or-name &optional norecord force-same-window)
  "Display buffer BUFFER-OR-NAME in the selected window.

WARNING: This is NOT the way to work on another buffer temporarily
within a Lisp program!  Use `set-buffer' instead.  That avoids
messing with the `window-buffer' correspondences.

If the selected window cannot display the specified buffer
because it is a minibuffer window or strongly dedicated to
another buffer, call `pop-to-buffer' to select the buffer in
another window.  In interactive use, if the selected window is
strongly dedicated to its buffer, the value of the option
`switch-to-buffer-in-dedicated-window' specifies how to proceed.

If called interactively, read the buffer name using `read-buffer'.
The variable `confirm-nonexistent-file-or-buffer' determines
whether to request confirmation before creating a new buffer.
See `read-buffer' for features related to input and completion
of buffer names.

BUFFER-OR-NAME may be a buffer, a string (a buffer name), or nil.
If BUFFER-OR-NAME is a string that does not identify an existing
buffer, create a buffer with that name.  If BUFFER-OR-NAME is
nil, switch to the buffer returned by `other-buffer'.

If optional argument NORECORD is non-nil, do not put the buffer
at the front of the buffer list, and do not make the window
displaying it the most recently selected one.

If optional argument FORCE-SAME-WINDOW is non-nil, the buffer
must be displayed in the selected window when called
non-interactively; if that is impossible, signal an error rather
than calling `pop-to-buffer'.  It has no effect when the option
`switch-to-buffer-obey-display-actions' is non-nil.

The option `switch-to-buffer-preserve-window-point' can be used
to make the buffer appear at its last position in the selected
window.

If the option `switch-to-buffer-obey-display-actions' is non-nil,
run the function `pop-to-buffer-same-window' instead.
This may display the buffer in another window as specified by
`display-buffer-overriding-action', `display-buffer-alist' and
other display related variables.  If this results in displaying
the buffer in the selected window, window start and point are adjusted
as prescribed by the option `switch-to-buffer-preserve-window-point'.
Otherwise, these are left alone.

Return the buffer switched to."
  (interactive
   (let ((force-same-window
          (unless switch-to-buffer-obey-display-actions
            (cond
             ((window-minibuffer-p) nil)
             ((not (eq (window-dedicated-p) t)) 'force-same-window)
             ((pcase switch-to-buffer-in-dedicated-window
                ('nil (user-error
                       "Cannot switch buffers in a dedicated window"))
                ('prompt
                 (if (y-or-n-p
                      (format "Window is dedicated to %s; undedicate it?"
                              (window-buffer)))
                     (progn
                       (set-window-dedicated-p nil nil)
                       'force-same-window)
                   (user-error
                    "Cannot switch buffers in a dedicated window")))
                ('pop nil)
                (_ (set-window-dedicated-p nil nil) 'force-same-window)))))))
     (list (read-buffer-to-switch "Switch to buffer: ") nil force-same-window)))
  (let ((buffer (window-normalize-buffer-to-switch-to buffer-or-name))
        (set-window-start-and-point (not switch-to-buffer-obey-display-actions)))
    (cond
     ;; Don't call set-window-buffer if it's not needed since it
     ;; might signal an error (e.g. if the window is dedicated).
     ((and (eq buffer (window-buffer))
           ;; pop-to-buffer-same-window might decide to display
           ;; the same buffer in another window
           (not switch-to-buffer-obey-display-actions)))
     ((and (window-minibuffer-p)
           (not switch-to-buffer-obey-display-actions))
      (if force-same-window
          (user-error "Cannot switch buffers in minibuffer window")
        (pop-to-buffer buffer norecord)))
     ((and (eq (window-dedicated-p) t)
           (not switch-to-buffer-obey-display-actions))
      (if force-same-window
          (user-error "Cannot switch buffers in a dedicated window")
        (pop-to-buffer buffer norecord)))
     (t
      (when switch-to-buffer-obey-display-actions
        (let ((selected-window (selected-window)))
          (pop-to-buffer-same-window buffer norecord)
          (when (eq (selected-window) selected-window)
            (setq set-window-start-and-point t))))

      (when set-window-start-and-point
        (let* ((entry (assq buffer (window-prev-buffers)))
               (preserve-win-point
                (buffer-local-value 'switch-to-buffer-preserve-window-point
                                    buffer))
	       (displayed (and (eq preserve-win-point 'already-displayed)
			       (get-buffer-window buffer 0))))
	  (set-window-buffer nil buffer)
	  (when (and entry (or (eq preserve-win-point t) displayed))
	    ;; Try to restore start and point of buffer in the selected
	    ;; window (Bug#4041).
	    (set-window-start (selected-window) (nth 1 entry) t)
	    (set-window-point nil (nth 2 entry)))))))

    (unless norecord
      (select-window (selected-window)))
    (set-buffer buffer)))

(defun switch-to-buffer-other-window (buffer-or-name &optional norecord)
  "Select the buffer specified by BUFFER-OR-NAME in another window.
BUFFER-OR-NAME may be a buffer, a string (a buffer name), or
nil.  Return the buffer switched to.

If called interactively, read the buffer name using `read-buffer'.
The variable `confirm-nonexistent-file-or-buffer' determines
whether to request confirmation before creating a new buffer.
See `read-buffer' for features related to input and completion
of buffer names.

If BUFFER-OR-NAME is a string and does not identify an existing
buffer, create a new buffer with that name.  If BUFFER-OR-NAME is
nil, switch to the buffer returned by `other-buffer'.

Optional second argument NORECORD non-nil means do not put this
buffer at the front of the list of recently selected ones.

This uses the function `display-buffer' as a subroutine; see its
documentation for additional customization information."
  (interactive
   (list (read-buffer-to-switch "Switch to buffer in other window: ")))
  (let ((pop-up-windows t))
    (pop-to-buffer buffer-or-name t norecord)))

(defun switch-to-buffer-other-frame (buffer-or-name &optional norecord)
  "Switch to buffer BUFFER-OR-NAME in another frame.
BUFFER-OR-NAME may be a buffer, a string (a buffer name), or
nil.  Return the buffer switched to.

This uses the function `display-buffer' as a subroutine to
display the buffer; see its documentation for additional
customization information.  By default, if the buffer is already
displayed (even in the current frame), that window is selected.
If the buffer isn't displayed in any frame, a new frame is popped
up and the buffer is displayed there.

If called interactively, read the buffer name using `read-buffer'.
The variable `confirm-nonexistent-file-or-buffer' determines
whether to request confirmation before creating a new buffer.
See `read-buffer' for features related to input and completion
of buffer names.

If BUFFER-OR-NAME is a string and does not identify an existing
buffer, create a new buffer with that name.  If BUFFER-OR-NAME is
nil, switch to the buffer returned by `other-buffer'.

Optional second arg NORECORD non-nil means do not put this
buffer at the front of the list of recently selected ones."
  (interactive
   (list (read-buffer-to-switch "Switch to buffer in other frame: ")))
  (pop-to-buffer buffer-or-name display-buffer--other-frame-action norecord))

(defun display-buffer-override-next-command (pre-function &optional post-function echo)
  "Set `display-buffer-overriding-action' for the next command.
`pre-function' is called to prepare the window where the buffer should be
displayed.  This function takes two arguments `buffer' and `alist', and
should return a cons with the displayed window and its type.  See the
meaning of these values in `window--display-buffer'.
Optional `post-function' is called after the buffer is displayed in the
window; the function takes two arguments: an old and new window.
Optional string argument `echo' can be used to add a prefix to the
command echo keystrokes that should describe the current prefix state.
This returns an \"exit function\", which can be called with no argument
to deactivate this overriding action."
  (let* ((old-window (or (minibuffer-selected-window) (selected-window)))
         (new-window nil)
         (minibuffer-depth (minibuffer-depth))
         (clearfun (make-symbol "clear-display-buffer-overriding-action"))
         (postfun (make-symbol "post-display-buffer-override-next-command"))
         (action (lambda (buffer alist)
                   (unless (> (minibuffer-depth) minibuffer-depth)
                     (let* ((ret (funcall pre-function buffer alist))
                            (window (car ret))
                            (type (cdr ret)))
                       (setq new-window (window--display-buffer buffer window
                                                                type alist))
                       ;; Reset display-buffer-overriding-action
                       ;; after the first display-buffer action (bug#39722).
                       (funcall clearfun)
                       new-window))))
         (command this-command)
         (echofun (when echo (lambda () echo)))
         (exitfun
          (lambda ()
            (funcall clearfun)
            (remove-hook 'post-command-hook postfun)
            (remove-hook 'prefix-command-echo-keystrokes-functions echofun)
            (when (functionp post-function)
              (funcall post-function old-window new-window)))))
    (fset clearfun
          (lambda ()
            (setcar display-buffer-overriding-action
                    (delq action (car display-buffer-overriding-action)))))
    (fset postfun
          (lambda ()
            (unless (or
		     ;; Remove the hook immediately
		     ;; after exiting the minibuffer.
		     (> (minibuffer-depth) minibuffer-depth)
		     ;; But don't remove immediately after
		     ;; adding the hook by the same command below.
		     (eq this-command command))
              (funcall exitfun))))
    ;; Call post-function after the next command finishes (bug#49057).
    (add-hook 'post-command-hook postfun)
    (when echofun
      (add-hook 'prefix-command-echo-keystrokes-functions echofun))
    (push action (car display-buffer-overriding-action))
    exitfun))


(defun set-window-text-height (window height)
  "Set the height in lines of the text display area of WINDOW to HEIGHT.
WINDOW must be a live window and defaults to the selected one.
HEIGHT doesn't include the mode line or header line, if any, or
any partial-height lines in the text display area.

Note that the current implementation of this function cannot
always set the height exactly, but attempts to be conservative,
by allocating more lines than are actually needed in the case
where some error may be present."
  (setq window (window-normalize-window window t))
  (let ((delta (- height (window-text-height window))))
    (unless (zerop delta)
      ;; Setting window-min-height to a value like 1 can lead to very
      ;; bizarre displays because it also allows Emacs to make *other*
      ;; windows one line tall, which means that there's no more space
      ;; for the mode line.
      (let ((window-min-height (min 2 height)))
	(window-resize window delta)))))

(defun enlarge-window-horizontally (delta)
  "Make selected window DELTA columns wider.
Interactively, if no argument is given, make selected window one
column wider."
  (interactive "p")
  (enlarge-window delta t))

(defun shrink-window-horizontally (delta)
  "Make selected window DELTA columns narrower.
Interactively, if no argument is given, make selected window one
column narrower."
  (interactive "p")
  (shrink-window delta t))

(defun count-screen-lines (&optional beg end count-final-newline window)
  "Return the number of screen lines in the region.
The number of screen lines may be different from the number of actual lines,
due to line breaking, display table, etc.

Optional arguments BEG and END default to `point-min' and `point-max'
respectively.

If region ends with a newline, ignore it unless optional third argument
COUNT-FINAL-NEWLINE is non-nil.

The optional fourth argument WINDOW specifies the window used for obtaining
parameters such as width, horizontal scrolling, and so on.  The default is
to use the selected window's parameters.

Like `vertical-motion', `count-screen-lines' always uses the current buffer,
regardless of which buffer is displayed in WINDOW.  This makes possible to use
`count-screen-lines' in any buffer, whether or not it is currently displayed
in some window."
  (unless beg
    (setq beg (point-min)))
  (unless end
    (setq end (point-max)))
  (if (= beg end)
      0
    (let ((start (min beg end))
          (finish (max beg end))
          count end-invisible-p)
      ;; When END is invisible because lines are truncated in WINDOW,
      ;; vertical-motion returns a number that is 1 larger than it
      ;; should.  We need to fix that.
      (setq end-invisible-p
            (and (or truncate-lines
                     (and (natnump truncate-partial-width-windows)
                          (< (window-total-width window)
                             truncate-partial-width-windows)))
                 (save-excursion
                   (goto-char finish)
                   (> (- (current-column) (window-hscroll window))
                      (window-body-width window)))))
      (save-excursion
        (save-restriction
          (widen)
          (narrow-to-region start
                            (if (and (not count-final-newline)
                                     (= ?\n (char-before finish)))
                                (1- finish)
                              finish))
          (goto-char start)
          (setq count (vertical-motion (buffer-size) window))
          (if end-invisible-p count (1+ count)))))))

(defun window-buffer-height (window)
  "Return the height (in screen lines) of the buffer that WINDOW is displaying.
WINDOW must be a live window and defaults to the selected one."
  (setq window (window-normalize-window window t))
  (with-current-buffer (window-buffer window)
    (max 1
	 (count-screen-lines (point-min) (point-max)
			     ;; If buffer ends with a newline, ignore it when
			     ;; counting height unless point is after it.
			     (eobp)
			     window))))

(defun window-default-font-height (&optional window)
  "Return height in pixels of WINDOW's default face font.
WINDOW must be a live window and defaults to the selected one.

The return value accounts for any remapping of the default face
font on WINDOW's frame."
  (let* ((window (window-normalize-window window t))
	 (frame (window-frame window))
	 (default-font (face-font 'default frame)))
    ;; Client frames can have the 'display' parameter set like for X
    ;; frames, even though they are TTY frames, so make sure we won't
    ;; be duped by that up front with 'framep'.
    (if (and (not (eq (framep frame) t))
             (display-multi-font-p (frame-parameter frame 'display))
	     (not (string-equal (frame-parameter frame 'font) default-font)))
        (aref (font-info default-font frame) 3)
      (frame-char-height frame))))

(defun window-default-line-height (&optional window)
  "Return height in pixels of a text line in WINDOW.
WINDOW must be a live window and defaults to the selected one.

The return value includes any line spacing defined for WINDOW's
buffer or frame and accounts for any remapping of the default
face on WINDOW's frame."
  (let* ((window (window-normalize-window window t))
	 (font-height (window-default-font-height window))
	 (frame (window-frame window))
	 (buffer (window-buffer window))
	 (space-height
	  (or (and (display-graphic-p frame)
		   (or (buffer-local-value 'line-spacing buffer)
		       (frame-parameter frame 'line-spacing)))
	      0)))
    (+ font-height
       (if (floatp space-height)
           (truncate (* (frame-char-height frame) space-height))
         space-height))))

;;; Resizing windows and frames to fit their contents exactly.
(defcustom fit-window-to-buffer-horizontally nil
  "Non-nil means `fit-window-to-buffer' can resize windows horizontally.
If this is nil, `fit-window-to-buffer' never resizes windows
horizontally.  If this is `only', it can resize windows
horizontally only.  Any other value means `fit-window-to-buffer'
can resize windows in both dimensions."
  :type 'boolean
  :version "24.4"
  :group 'help)

;; `fit-frame-to-buffer' eventually wants to know the real frame sizes
;; counting title bar and outer borders.
(defcustom fit-frame-to-buffer nil
  "Non-nil means `fit-window-to-buffer' can fit a frame to its buffer.
A frame is fit if and only if its root window is a live window
and this option is non-nil.  If this is `horizontally', frames
are resized horizontally only.  If this is `vertically', frames
are resized vertically only.  Any other non-nil value means
frames can be resized in both dimensions."
  :type 'boolean
  :version "24.4"
  :group 'help)

(defcustom fit-frame-to-buffer-margins '(nil nil nil nil)
  "Margins around frame for `fit-frame-to-buffer'.
This specifies the numbers of pixels to be left free on the left,
above, on the right, and below a frame fitted to its buffer.  Set
this to avoid obscuring other desktop objects like the taskbar.
The default is nil for each side, which means to not add margins.

The value specified here can be overridden for a specific frame
by that frame's `fit-frame-to-buffer-margins' parameter, if
present.  See also `fit-frame-to-buffer-sizes'."
  :version "24.4"
  :type '(list
	  (choice
	   :tag "Left"
	   :value nil
	   :format "%[LeftMargin%] %v  "
	   (const :tag "None" :format "%t" nil)
	   (integer :tag "Pixels" :size 5))
	  (choice
	   :tag "Top"
	   :value nil
	   :format "%[TopMargin%] %v  "
	   (const :tag "None" :format "%t" nil)
	   (integer :tag "Pixels" :size 5))
	  (choice
	   :tag "Right"
	   :value nil
	   :format "%[RightMargin%] %v  "
	   (const :tag "None" :format "%t" nil)
	   (integer :tag "Pixels" :size 5))
	  (choice
	   :tag "Bottom"
	   :value nil
	   :format "%[BottomMargin%] %v  "
	   (const :tag "None" :format "%t" nil)
	   (integer :tag "Pixels" :size 5)))
  :group 'help)

(defcustom fit-frame-to-buffer-sizes '(nil nil nil nil)
  "Size boundaries of frame for `fit-frame-to-buffer'.
This list specifies the total maximum and minimum lines and
maximum and minimum columns of the root window of any frame that
shall be fit to its buffer.  If any of these values is non-nil,
it overrides the corresponding argument of `fit-frame-to-buffer'.

On window systems where the menubar can wrap, fitting a frame to
its buffer may swallow the last line(s).  Specifying an
appropriate minimum width value here can avoid such wrapping.

See also `fit-frame-to-buffer-margins'."
  :version "24.4"
  :type '(list
	  (choice
	   :tag "Maximum Height"
	   :value nil
	   :format "%[MaxHeight%] %v  "
	   (const :tag "None" :format "%t" nil)
	   (integer :tag "Lines" :size 5))
	  (choice
	   :tag "Minimum Height"
	   :value nil
	   :format "%[MinHeight%] %v  "
	   (const :tag "None" :format "%t" nil)
	   (integer :tag "Lines" :size 5))
	  (choice
	   :tag "Maximum Width"
	   :value nil
	   :format "%[MaxWidth%] %v  "
	   (const :tag "None" :format "%t" nil)
	   (integer :tag "Columns" :size 5))
	  (choice
	   :tag "Minimum Width"
	   :value nil
	   :format "%[MinWidth%] %v\n"
	   (const :tag "None" :format "%t" nil)
	   (integer :tag "Columns" :size 5)))
  :group 'help)

(declare-function x-display-pixel-height "xfns.c" (&optional terminal))

(defun window--sanitize-margin (margin left right)
  "Return MARGIN if it's a number between LEFT and RIGHT.
Return 0 otherwise."
  (if (and (numberp margin)
           (<= left (- right margin)) (<= margin right))
      margin
    0))

(declare-function tool-bar-height "xdisp.c" (&optional frame pixelwise))

(defun fit-mini-frame-to-buffer (&optional frame)
  "Adjust size of minibuffer FRAME to display its contents.
FRAME should be a minibuffer-only frame and defaults to the
selected one.  Unlike `fit-frame-to-buffer' FRAME will fit to the
contents of its buffer with any leading or trailing empty lines
included."
  (fit-frame-to-buffer-1 frame))

(defun fit-frame-to-buffer (&optional frame max-height min-height max-width min-width only)
  "Adjust size of FRAME to display the contents of its buffer exactly.
FRAME can be any live frame and defaults to the selected one.
Fit only if FRAME's root window is live.

MAX-HEIGHT, MIN-HEIGHT, MAX-WIDTH and MIN-WIDTH specify bounds on
the new total size of FRAME's root window.  MIN-HEIGHT and
MIN-WIDTH default to the values of `window-min-height' and
`window-min-width' respectively.  These arguments are specified
in the canonical character width and height of FRAME.

If the optional argument ONLY is `vertically', resize the frame
vertically only.  If ONLY is `horizontally', resize the frame
horizontally only.

The new position and size of FRAME can be additionally determined
by customizing the options `fit-frame-to-buffer-sizes' and
`fit-frame-to-buffer-margins' or setting the corresponding
parameters of FRAME.

Any leading or trailing empty lines of the buffer content are not
considered."
  (interactive)
  (fit-frame-to-buffer-1 frame max-height min-height max-width min-width only t t))

(defun fit-frame-to-buffer-1 (&optional frame max-height min-height max-width min-width only from to)
  "Helper function for `fit-frame-to-buffer'.
FROM and TO are the buffer positions to determine the size to fit
to, see `window-text-pixel-size'.  The remaining arguments are as
for `fit-frame-to-buffer'."
  (unless (fboundp 'display-monitor-attributes-list)
    (user-error "Cannot resize frame in non-graphic Emacs"))
  (setq frame (window-normalize-frame frame))
  (when (window-live-p (frame-root-window frame))
    (let* ((char-width (frame-char-width frame))
           (char-height (frame-char-height frame))
           ;; WINDOW is FRAME's root window.
           (window (frame-root-window frame))
           (line-height (window-default-line-height window))
           (parent (frame-parent frame))
           (monitor-attributes
            (unless parent
              (frame-monitor-attributes frame)))
           ;; FRAME'S parent or display sizes.  Used in connection
           ;; with margins.
           (geometry
            (unless parent
              (cdr (assq 'geometry monitor-attributes))))
           (parent-or-display-width
            (if parent
                (frame-native-width parent)
              (nth 2 geometry)))
           (parent-or-display-height
            (if parent
                (frame-native-height parent)
              (nth 3 geometry)))
           ;; FRAME's parent or workarea sizes.  Used when no margins
           ;; are specified.
           (parent-or-workarea
            (if parent
                `(0 0 ,parent-or-display-width ,parent-or-display-height)
              (cdr (assq 'workarea monitor-attributes))))
           ;; The outer size of FRAME.  Needed to calculate the
           ;; margins around the root window's body that have to
           ;; remain untouched by fitting.
           (outer-edges (frame-edges frame 'outer-edges))
           (outer-width (if outer-edges
                            (- (nth 2 outer-edges) (nth 0 outer-edges))
                          ;; A poor guess.
                          (frame-pixel-width frame)))
           (outer-height (if outer-edges
                             (- (nth 3 outer-edges) (nth 1 outer-edges))
                           ;; Another poor guess.
                           (frame-pixel-height frame)))
           ;; The text size of FRAME.  Needed to specify FRAME's
           ;; text size after the root window's body's new sizes have
           ;; been calculated.
           (text-width (frame-text-width frame))
           (text-height (frame-text-height frame))
           ;; WINDOW's body size.
           (body-width (window-body-width window t))
           (body-height (window-body-height window t))
           ;; The difference between FRAME's outer size and WINDOW's
           ;; body size.
           (outer-minus-body-width (- outer-width body-width))
           (outer-minus-body-height (- outer-height body-height))
           ;; The difference between FRAME's text size and WINDOW's
           ;; body size (these values "should" be positive).
           (text-minus-body-width (- text-width body-width))
           (text-minus-body-height (- text-height body-height))
           ;; The current position of FRAME.
           (position (frame-position frame))
           (left (car position))
           (top (cdr position))
           ;; The margins specified for FRAME.  These represent pixel
           ;; offsets from the left, top, right and bottom edge of the
           ;; display or FRAME's parent's native rectangle and have to
           ;; take care of the display's taskbar and other obstacles.
           ;; If they are unspecified, constrain the resulting frame
           ;; to its workarea or the parent frame's native rectangle.
           (margins (or (frame-parameter frame 'fit-frame-to-buffer-margins)
                        fit-frame-to-buffer-margins))
           ;; Convert margins into pixel offsets from the left-top
           ;; corner of FRAME's display or parent.
           (left-margin (if (nth 0 margins)
                            (window--sanitize-margin
                             (nth 0 margins) 0 parent-or-display-width)
                          (nth 0 parent-or-workarea)))
           (top-margin (if (nth 1 margins)
                           (window--sanitize-margin
                            (nth 1 margins) 0 parent-or-display-height)
                         (nth 1 parent-or-workarea)))
           (right-margin (if (nth 2 margins)
                             (- parent-or-display-width
                                (window--sanitize-margin
                                 (nth 2 margins) left-margin
                                 parent-or-display-width))
                           (+ (nth 0 parent-or-workarea)
                              (nth 2 parent-or-workarea))))
           (bottom-margin (if (nth 3 margins)
                              (- parent-or-display-height
                                 (window--sanitize-margin
                                  (nth 3 margins) top-margin
                                  parent-or-display-height))
                            (+ (nth 1 parent-or-workarea)
                               (nth 3 parent-or-workarea))))
           ;; Minimum and maximum sizes specified for FRAME.
           (sizes (or (frame-parameter frame 'fit-frame-to-buffer-sizes)
                      fit-frame-to-buffer-sizes))
           ;; Calculate the minimum and maximum pixel sizes of FRAME
           ;; from the values provided by the MAX-HEIGHT, MIN-HEIGHT,
           ;; MAX-WIDTH and MIN-WIDTH arguments or, if these are nil,
           ;; from those provided by `fit-frame-to-buffer-sizes'.
           (max-height
            (min
             (cond
              ((numberp max-height) (* max-height line-height))
              ((numberp (nth 0 sizes)) (* (nth 0 sizes) line-height))
              (t parent-or-display-height))
             ;; The following is the maximum height that fits into the
             ;; top and bottom margins.
             (max (- bottom-margin top-margin outer-minus-body-height))))
           (min-height
            (cond
             ((numberp min-height) (* min-height line-height))
             ((numberp (nth 1 sizes)) (* (nth 1 sizes) line-height))
             (t (window-min-size window nil nil t))))
           (max-width
            (min
             (cond
              ((numberp max-width) (* max-width char-width))
              ((numberp (nth 2 sizes)) (* (nth 2 sizes) char-width))
              (t parent-or-display-width))
             ;; The following is the maximum width that fits into the
             ;; left and right margins.
             (max (- right-margin left-margin outer-minus-body-width))))
           (min-width
            (cond
             ((numberp min-width) (* min-width char-width))
             ((numberp (nth 3 sizes)) (nth 3 sizes))
             (t (window-min-size window t nil t))))
           ;; Note: Currently, for a new frame the sizes of the header
           ;; and mode line may be estimated incorrectly
           (size
            (window-text-pixel-size window from to max-width max-height))
           (width (max (car size) min-width))
           (height (max (cdr size) min-height)))
      ;; Don't change height or width when the window's size is fixed
      ;; in either direction or ONLY forbids it.
      (cond
       ((or (eq window-size-fixed 'width) (eq only 'vertically))
        (setq width nil))
       ((or (eq window-size-fixed 'height) (eq only 'horizontally))
        (setq height nil)))
      ;; Fit width to constraints.
      (when width
        (unless frame-resize-pixelwise
          ;; Round to character sizes.
          (setq width (* (/ (+ width char-width -1) char-width)
                         char-width)))
        ;; The new outer width (in pixels).
        (setq outer-width (+ width outer-minus-body-width))
        ;; Maybe move FRAME to preserve margins.
        (let ((right (+ left outer-width)))
          (cond
           ((> right right-margin)
            ;; Move frame to left.
            (setq left (max left-margin (- left (- right right-margin)))))
           ((< left left-margin)
            ;; Move frame to right.
            (setq left left-margin)))))
      ;; Fit height to constraints.
      (when height
        (unless frame-resize-pixelwise
          (setq height (* (/ (+ height char-height -1) char-height)
                          char-height)))
        ;; The new outer height.
        (setq outer-height (+ height outer-minus-body-height))
        ;; Preserve margins.
        (let ((bottom (+ top outer-height)))
          (cond
           ((> bottom bottom-margin)
            ;; Move frame up.
            (setq top (max top-margin (- top (- bottom bottom-margin)))))
           ((< top top-margin)
            ;; Move frame down.
            (setq top top-margin)))))
      ;; Apply our changes.
      (setq text-width
            (if width
                (+ width text-minus-body-width)
              (frame-text-width frame)))
      (setq text-height
            (if height
                (+ height text-minus-body-height)
              (frame-text-height frame)))
      (modify-frame-parameters
       frame `((left . ,left) (top . ,top)
               (width . (text-pixels . ,text-width))
               (height . (text-pixels . ,text-height)))))))

(defun fit-window-to-buffer (&optional window max-height min-height max-width min-width preserve-size)
  "Adjust size of WINDOW to display its buffer's contents exactly.
WINDOW must be a live window and defaults to the selected one.

If WINDOW is part of a vertical combination, adjust WINDOW's
height.  The new height is calculated from the actual height of
the accessible portion of its buffer.  The optional argument
MAX-HEIGHT specifies a maximum height and defaults to the height
of WINDOW's frame.  The optional argument MIN-HEIGHT specifies a
minimum height and defaults to `window-min-height'.  Both
MAX-HEIGHT and MIN-HEIGHT are specified in lines and include mode
and header line and a bottom divider, if any.

If WINDOW is part of a horizontal combination and the value of
the option `fit-window-to-buffer-horizontally' is non-nil, adjust
WINDOW's width.  The new width of WINDOW is calculated from the
maximum length of its buffer's lines that follow the current
start position of WINDOW.  The optional argument MAX-WIDTH
specifies a maximum width and defaults to the width of WINDOW's
frame.  The optional argument MIN-WIDTH specifies a minimum width
and defaults to `window-min-width'.  Both MAX-WIDTH and MIN-WIDTH
are specified in columns and include fringes, margins, a
scrollbar and a vertical divider, if any.

Optional argument PRESERVE-SIZE non-nil means to preserve the
size of WINDOW (see `window-preserve-size').

Fit pixelwise if the option `window-resize-pixelwise' is non-nil.
If WINDOW is its frame's root window and the option
`fit-frame-to-buffer' is non-nil, call `fit-frame-to-buffer' to
adjust the frame's size.

Note that even if this function makes WINDOW large enough to show
_all_ parts of its buffer you might not see the first part when
WINDOW was scrolled.  If WINDOW is resized horizontally, you will
not see the top of its buffer unless WINDOW starts at its minimum
accessible position."
  (interactive)
  (setq window (window-normalize-window window t))
  (if (eq window (frame-root-window window))
      (when fit-frame-to-buffer
	;; Fit WINDOW's frame to buffer.
	(fit-frame-to-buffer
	 (window-frame window)
	 max-height min-height max-width min-width
	 (and (memq fit-frame-to-buffer '(vertically horizontally))
	      fit-frame-to-buffer)))
    (let* ((pixelwise window-resize-pixelwise)
           (frame (window-frame window))
           (char-height (frame-char-height frame)))
      (cond
       ;; If WINDOW is vertically combined, try to resize it
       ;; vertically.
       ((and (not (eq fit-window-to-buffer-horizontally 'only))
	     (not (window-size-fixed-p window 'preserved))
	     (window-combined-p window))
        (let* ((line-height (window-default-line-height window))
	       (total-height (window-size window nil pixelwise))
               (min-height
	        ;; Sanitize MIN-HEIGHT.
	        (if (numberp min-height)
	            ;; Can't get smaller than `window-safe-min-height'.
	            (max (if pixelwise
			     (* line-height min-height)
		           min-height)
		         (if pixelwise
			     (window-safe-min-pixel-height window)
		           window-safe-min-height))
	          ;; Preserve header and mode line if present.
	          (max (if pixelwise
		           (* line-height window-min-height)
		         window-min-height)
		       (window-min-size window nil window pixelwise))))
	       (max-height
	        ;; Sanitize MAX-HEIGHT.
	        (if (numberp max-height)
	            (min
		     (+ total-height
		        (window-max-delta
		         window nil window nil t nil pixelwise))
		     (if pixelwise
		         (* line-height max-height)
		       (/ (* line-height max-height) line-height)))
	          (+ total-height (window-max-delta
			           window nil window nil t nil pixelwise))))
	       (height (+ (cdr (window-text-pixel-size
			        window nil t nil (frame-pixel-height frame) t))
		          (window-scroll-bar-height window)
		          (window-bottom-divider-width window))))
	  ;; Vertically we always want to fit the entire buffer.
	  ;; WINDOW'S height can't get larger than its frame's pixel
	  ;; height.  Its width remains fixed.
	  ;; Round height.
	  (unless pixelwise
	    (setq height (/ (+ height char-height -1) char-height)))
          (setq height (max min-height (min max-height height)))
	  (unless (= height total-height)
	    (window-preserve-size window)
	    (window-resize-no-error
	     window (- height total-height) nil window pixelwise)
	    (when preserve-size
	      (window-preserve-size window nil t)))))
       ;; If WINDOW is horizontally combined, try to resize it
       ;; horizontally.
       ((and fit-window-to-buffer-horizontally
	     (not (window-size-fixed-p window t 'preserved))
	     (window-combined-p window t))
	(let* ((char-width (frame-char-width frame))
               (total-width (window-size window t pixelwise))
	       (min-width
		;; Sanitize MIN-WIDTH.
		(if (numberp min-width)
		    ;; Can't get smaller than `window-safe-min-width'.
		    (max (if pixelwise
			     (* char-width min-width)
			   min-width)
			 (if pixelwise
			     (window-safe-min-pixel-width window)
			   window-safe-min-width))
		  ;; Preserve fringes, margins, scrollbars if present.
		  (max (if pixelwise
			   (* char-width window-min-width)
			 window-min-width)
		       (window-min-size window nil window pixelwise))))
	       (max-width
		;; Sanitize MAX-WIDTH.
		(if (numberp max-width)
		    (min (+ total-width
			    (window-max-delta
			     window t window nil t nil pixelwise))
			 (if pixelwise
			     (* char-width max-width)
			   max-width))
		  (+ total-width (window-max-delta
				  window t window nil t nil pixelwise))))
	       ;; When fitting horizontally, assume that WINDOW's
	       ;; start position remains unaltered.  WINDOW can't get
	       ;; wider than its frame's pixel width, its height
	       ;; remains unaltered.
	       (width (+ (car (window-text-pixel-size
			       window (window-start window) nil
			       (frame-pixel-width (window-frame window))
			       ;; Add one line-height to assure that
			       ;; we're on the safe side.  This
			       ;; overshoots when the first line below
			       ;; the bottom is wider than the window.
			       (* (window-body-height window pixelwise)
				  (if pixelwise 1 char-height))))
                         (- total-width
                            (window-body-width window pixelwise)))))
	  (unless pixelwise
	    (setq width (/ (+ width char-width -1) char-width)))
          (setq width (max min-width (min max-width width)))
	  (unless (= width total-width)
	    (window-preserve-size window t)
	    (window-resize-no-error
             window (- width total-width) t window pixelwise)
	    (when preserve-size
	      (window-preserve-size window t t)))))))))

(defun window-safely-shrinkable-p (&optional window)
  "Return t if WINDOW can be shrunk without shrinking other windows.
WINDOW defaults to the selected window."
  (with-selected-window (or window (selected-window))
    (let ((edges (window-edges)))
      (or (= (nth 2 edges) (nth 2 (window-edges (previous-window))))
	  (= (nth 0 edges) (nth 0 (window-edges (next-window))))))))

(defun shrink-window-if-larger-than-buffer (&optional window)
  "Shrink height of WINDOW if its buffer doesn't need so many lines.
More precisely, shrink WINDOW vertically to be as small as
possible, while still showing the full contents of its buffer.
WINDOW must be a live window and defaults to the selected one.

Do not shrink WINDOW to less than `window-min-height' lines.  Do
nothing if the buffer contains more lines than the present window
height, or if some of the window's contents are scrolled out of
view, or if shrinking this window would also shrink another
window, or if the window is the only window of its frame.

Return non-nil if the window was shrunk, nil otherwise."
  (interactive)
  (setq window (window-normalize-window window t))
  ;; Make sure that WINDOW is vertically combined and `point-min' is
  ;; visible (for whatever reason that's needed).  The remaining issues
  ;; should be taken care of by `fit-window-to-buffer'.
  (when (and (window-combined-p window)
	     (pos-visible-in-window-p (point-min) window))
    (fit-window-to-buffer window (window-total-height window))))

(defun window-largest-empty-rectangle--maximums-1 (quad maximums)
  "Support function for `window-largest-empty-rectangle'."
  (cond
   ((null maximums)
    (list quad))
   ((> (car quad) (caar maximums))
    (cons quad maximums))
   (t
    (cons (car maximums)
	  (window-largest-empty-rectangle--maximums-1 quad (cdr maximums))))))

(defun window-largest-empty-rectangle--maximums (quad maximums count)
  "Support function for `window-largest-empty-rectangle'."
  (setq maximums (window-largest-empty-rectangle--maximums-1 quad maximums))
  (if (> (length maximums) count)
      (nbutlast maximums)
    maximums))

(defun window-largest-empty-rectangle--disjoint-maximums (maximums count)
  "Support function for `window-largest-empty-rectangle'."
  (setq maximums (sort maximums (lambda (x y) (> (car x) (car y)))))
  (let ((new-length 0)
	new-maximums)
    (while (and maximums (< new-length count))
      (let* ((maximum (car maximums))
	     (at (nth 2 maximum))
	     (to (nth 3 maximum)))
	(catch 'drop
	  (dolist (new-maximum new-maximums)
	    (let ((new-at (nth 2 new-maximum))
		  (new-to (nth 3 new-maximum)))
	      (when (if (< at new-at) (> to new-at) (< at new-to))
		;; Intersection -> drop.
		(throw 'drop nil))))
	  (setq new-maximums (cons maximum new-maximums))
	  (setq new-length (1+ new-length)))
	(setq maximums (cdr maximums))))

    (nreverse new-maximums)))

(defun window-largest-empty-rectangle (&optional window count min-width min-height positions left)
  "Return dimensions of largest empty rectangle in WINDOW.
WINDOW must be a live window and defaults to the selected one.

The return value is a triple of the width and the start and end
Y-coordinates of the largest rectangle that can be inscribed into
the empty space (the space not displaying any text) of WINDOW's
text area.  The return value is nil if the current glyph matrix
of WINDOW is not up-to-date.

Optional argument COUNT, if non-nil, specifies the maximum number
of rectangles to return.  This means that the return value is a
list of triples specifying rectangles with the largest rectangle
first.  COUNT can be also a cons cell whose car specifies the
number of rectangles to return and whose cdr, if non-nil, states
that all rectangles returned must be disjoint.

Note that the right edge of any rectangle returned by this
function is the right edge of WINDOW (the left edge if its buffer
displays RTL text).

Optional arguments MIN-WIDTH and MIN-HEIGHT, if non-nil, specify
the minimum width and height of any rectangle returned.

Optional argument POSITIONS, if non-nil, is a cons cell whose car
specifies the uppermost and whose cdr specifies the lowermost
pixel position that must be covered by any rectangle returned.
Note that positions are counted from the start of the text area
of WINDOW.

Optional argument LEFT, if non-nil, means to return values suitable for
buffers displaying right to left text."
  ;; Process lines as returned by ‘window-lines-pixel-dimensions’.
  ;; STACK is a stack that contains rows that have yet to be processed.
  (let* ((window (window-normalize-window window t))
	 (disjoint (and (consp count) (cdr count)))
	 (count (or (and (numberp count) count)
		    (and (consp count) (numberp (car count)) (car count))))
	 (rows (window-lines-pixel-dimensions window nil nil t t left))
	 (rows-at 0)
	 (max-size 0)
	 row stack stack-at stack-to
	 top top-width top-at top-to top-size
	 max-width max-at max-to maximums)
    ;; ROWS-AT is the position where the first element of ROWS starts.
    ;; STACK-AT is the position where the first element of STACK starts.
    (while rows
      (setq row (car rows))
      (if (or (not stack) (>= (car row) (caar stack)))
	  (progn
	    (unless stack
	      (setq stack-at rows-at))
	    (setq stack (cons row stack))
	    ;; Set ROWS-AT to where the first element of ROWS ends
	    ;; which, after popping ROW, makes it the start position of
	    ;; the next ROW.
	    (setq rows-at (cdr row))
	    (setq rows (cdr rows)))
	(setq top (car stack))
	(setq stack (cdr stack))
	(setq top-width (car top))
	(setq top-at (if stack (cdar stack) stack-at))
	(setq top-to (cdr top))
	(setq top-size (* top-width (- top-to top-at)))
	(unless (or (and min-width (< top-width min-width))
		    (and min-height (< (- top-to top-at) min-height))
		    (and positions
			 (or (> top-at (car positions))
			     (< top-to (cdr positions)))))
	  (if count
	      (if disjoint
		  (setq maximums (cons (list top-size top-width top-at top-to)
				       maximums))
		(setq maximums (window-largest-empty-rectangle--maximums
				(list top-size top-width top-at top-to)
				maximums count)))
	    (when (> top-size max-size)
	      (setq max-size top-size)
	      (setq max-width top-width)
	      (setq max-at top-at)
	      (setq max-to top-to))))
	(if (and stack (> (caar stack) (car row)))
	    ;; Have new top element of stack include old top.
	    (setq stack (cons (cons (caar stack) (cdr top)) (cdr stack)))
	  ;; Move rows-at backwards to top-at.
	  (setq rows-at top-at))))

    (when stack
      ;; STACK-TO is the position where the stack ends.
      (setq stack-to (cdar stack))
      (while stack
	(setq top (car stack))
	(setq stack (cdr stack))
	(setq top-width (car top))
	(setq top-at (if stack (cdar stack) stack-at))
	(setq top-size (* top-width (- stack-to top-at)))
	(unless (or (and min-width (< top-width min-width))
		    (and min-height (< (- stack-to top-at) min-height))
		    (and positions
			 (or (> top-at (car positions))
			     (< stack-to (cdr positions)))))
	  (if count
	      (if disjoint
		  (setq maximums (cons (list top-size top-width top-at stack-to)
				       maximums))
		(setq maximums (window-largest-empty-rectangle--maximums
				(list top-size top-width top-at stack-to)
				maximums count)))
	    (when (> top-size max-size)
	      (setq max-size top-size)
	      (setq max-width top-width)
	      (setq max-at top-at)
	      (setq max-to stack-to))))))

    (cond
     (maximums
      (if disjoint
	  (window-largest-empty-rectangle--disjoint-maximums maximums count)
	maximums))
     ((> max-size 0)
      (list max-width max-at max-to)))))

(defun kill-buffer-and-window ()
  "Kill the current buffer and delete the selected window."
  (interactive)
  (let ((window-to-delete (selected-window))
	(buffer-to-kill (current-buffer))
	(delete-window-hook (lambda () (ignore-errors (delete-window)))))
    (unwind-protect
	(progn
	  (add-hook 'kill-buffer-hook delete-window-hook t t)
	  (if (kill-buffer (current-buffer))
	      ;; If `delete-window' failed before, we rerun it to regenerate
	      ;; the error so it can be seen in the echo area.
	      (when (eq (selected-window) window-to-delete)
		(delete-window))))
      ;; If the buffer is not dead for some reason (probably because
      ;; of a `quit' signal), remove the hook again.
      (ignore-errors
       (with-current-buffer buffer-to-kill
	 (remove-hook 'kill-buffer-hook delete-window-hook t))))))


;;;
;; Groups of windows (Follow Mode).
;;
;; This section of functions extends the functionality of some window
;; manipulating commands to groups of windows cooperatively
;; displaying a buffer, typically with Follow Mode.
;;
;; The xxx-function variables are permanent locals so that their local
;; status is undone only when explicitly programmed, not when a buffer
;; is reverted or a mode function is called.

(defvar-local window-group-start-function nil)
(put 'window-group-start-function 'permanent-local t)
(defun window-group-start (&optional window)
  "Return position at which display currently starts in the group of
windows containing WINDOW.  When a grouping mode (such as Follow Mode)
is not active, this function is identical to `window-start'.

WINDOW must be a live window and defaults to the selected one.
This is updated by redisplay or by calling `set-window*-start'."
  (if (functionp window-group-start-function)
      (funcall window-group-start-function window)
    (window-start window)))

(defvar-local window-group-end-function nil)
(put 'window-group-end-function 'permanent-local t)
(defun window-group-end (&optional window update)
  "Return position at which display currently ends in the group of
windows containing WINDOW.  When a grouping mode (such as Follow Mode)
is not active, this function is identical to `window-end'.

WINDOW must be a live window and defaults to the selected one.
This is updated by redisplay, when it runs to completion.
Simply changing the buffer text or setting `window-group-start'
does not update this value.
Return nil if there is no recorded value.  (This can happen if the
last redisplay of WINDOW was preempted, and did not finish.)
If UPDATE is non-nil, compute the up-to-date position
if it isn't already recorded."
  (if (functionp window-group-end-function)
      (funcall window-group-end-function window update)
    (window-end window update)))

(defvar-local set-window-group-start-function nil)
(put 'set-window-group-start-function 'permanent-local t)
(defun set-window-group-start (window pos &optional noforce)
  "Make display in the group of windows containing WINDOW start at
position POS in WINDOW's buffer.  When a grouping mode (such as Follow
Mode) is not active, this function is identical to `set-window-start'.

WINDOW must be a live window and defaults to the selected one.  Return
POS.  Optional third arg NOFORCE non-nil inhibits next redisplay from
overriding motion of point in order to display at this exact start."
  (if (functionp set-window-group-start-function)
      (funcall set-window-group-start-function window pos noforce)
    (set-window-start window pos noforce)))

(defvar-local recenter-window-group-function nil)
(put 'recenter-window-group-function 'permanent-local t)
(defun recenter-window-group (&optional arg)
  "Center point in the group of windows containing the selected window
and maybe redisplay frame.  When a grouping mode (such as Follow Mode)
is not active, this function is identical to `recenter'.

With a numeric prefix argument ARG, recenter putting point on screen line ARG
relative to the first window in the selected window group.  If ARG is
negative, it counts up from the bottom of the last window in the
group.  (ARG should be less than the total height of the window group.)

If ARG is omitted or nil, then recenter with point on the middle line of
the selected window group; if the variable `recenter-redisplay' is
non-nil, also erase the entire frame and redraw it (when
`auto-resize-tool-bars' is set to `grow-only', this resets the
tool-bar's height to the minimum height needed); if
`recenter-redisplay' has the special value `tty', then only tty frames
are redrawn.

Just \\[universal-argument] as prefix means put point in the center of the window
and redisplay normally--don't erase and redraw the frame."
  (if (functionp recenter-window-group-function)
      (funcall recenter-window-group-function arg)
    (recenter arg)))

(defvar-local pos-visible-in-window-group-p-function nil)
(put 'pos-visible-in-window-group-p-function 'permanent-local t)
(defun pos-visible-in-window-group-p (&optional pos window partially)
  "Return non-nil if position POS is currently on the frame in the
window group containing WINDOW.  When a grouping mode (such as Follow
Mode) is not active, this function is identical to
`pos-visible-in-window-p'.

WINDOW must be a live window and defaults to the selected one.

Return nil if that position is scrolled vertically out of view.  If a
character is only partially visible, nil is returned, unless the
optional argument PARTIALLY is non-nil.  If POS is only out of view
because of horizontal scrolling, return non-nil.  If POS is t, it
specifies the position of the last visible glyph in the window group.
POS defaults to point in WINDOW; WINDOW defaults to the selected
window.

If POS is visible, return t if PARTIALLY is nil; if PARTIALLY is non-nil,
the return value is a list of 2 or 6 elements (X Y [RTOP RBOT ROWH VPOS]),
where X and Y are the pixel coordinates relative to the top left corner
of the window.  The remaining elements are omitted if the character after
POS is fully visible; otherwise, RTOP and RBOT are the number of pixels
off-window at the top and bottom of the screen line (\"row\") containing
POS, ROWH is the visible height of that row, and VPOS is the row number
\(zero-based)."
  (if (functionp pos-visible-in-window-group-p-function)
      (funcall pos-visible-in-window-group-p-function pos window partially)
    (pos-visible-in-window-p pos window partially)))

(defvar-local selected-window-group-function nil)
(put 'selected-window-group-function 'permanent-local t)
(defun selected-window-group ()
  "Return the list of windows in the group containing the selected window.
When a grouping mode (such as Follow Mode) is not active, the
result is a list containing only the selected window."
  (if (functionp selected-window-group-function)
      (funcall selected-window-group-function)
    (list (selected-window))))

(defvar-local move-to-window-group-line-function nil)
(put 'move-to-window-group-line-function 'permanent-local t)
(defun move-to-window-group-line (arg)
  "Position point relative to the current group of windows.
When a grouping mode (such as Follow Mode) is not active, this
function is identical to `move-to-window-line'.

ARG nil means position point at center of the window group.
Else, ARG specifies the vertical position within the window
group; zero means top of first window in the group, negative
means relative to the bottom of the last window in the group."
  (if (functionp move-to-window-group-line-function)
      (funcall move-to-window-group-line-function arg)
    (move-to-window-line arg)))


(defvar recenter-last-op nil
  "Indicates the last recenter operation performed.
Possible values: `top', `middle', `bottom', integer or float numbers.
It can also be nil, which means the first value in `recenter-positions'.")

(defcustom recenter-positions '(middle top bottom)
  "Cycling order for `recenter-top-bottom'.
A list of elements with possible values `top', `middle', `bottom',
integer or float numbers that define the cycling order for
the command `recenter-top-bottom'.

Top and bottom destinations are `scroll-margin' lines from the true
window top and bottom.  Middle redraws the frame and centers point
vertically within the window.  Integer number moves current line to
the specified absolute window-line.  Float number between 0.0 and 1.0
means the percentage of the screen space from the top.  The default
cycling order is middle -> top -> bottom."
  :type '(repeat (choice
		  (const :tag "Top" top)
		  (const :tag "Middle" middle)
		  (const :tag "Bottom" bottom)
		  (integer :tag "Line number")
		  (float :tag "Percentage")))
  :version "23.2"
  :group 'windows)

(defun recenter-top-bottom (&optional arg)
  "Scroll the window so that current line is in the middle of the window.
Successive invocations scroll the window in a cyclical order to put
the current line at certain places within the window, as determined by
`recenter-positions'.  By default, the second invocation puts the
current line at the top-most window line, the third invocation puts it
on the bottom-most window line, and then the order is reused in a
cyclical manner.

With numeric prefix ARG, move current line ARG lines below the window top.
With plain \\[universal-argument], move current line to window center."
  (interactive "P")
  (cond
   (arg (recenter arg t))                 ; Always respect ARG.
   (t
    (setq recenter-last-op
	  (if (eq this-command last-command)
	      (car (or (cdr (member recenter-last-op recenter-positions))
		       recenter-positions))
	    (car recenter-positions)))
    (let ((this-scroll-margin
	   (min (max 0 scroll-margin)
		(truncate (/ (window-body-height) 4.0)))))
      (cond ((eq recenter-last-op 'middle)
	     (recenter nil t))
	    ((eq recenter-last-op 'top)
	     (recenter this-scroll-margin t))
	    ((eq recenter-last-op 'bottom)
	     (recenter (- -1 this-scroll-margin) t))
	    ((integerp recenter-last-op)
	     (recenter recenter-last-op t))
	    ((floatp recenter-last-op)
	     (recenter (round (* recenter-last-op (window-height))) t)))))))

(define-key global-map [?\C-l] 'recenter-top-bottom)

(defun recenter-other-window (&optional arg)
  "Call `recenter-top-bottom' in the other window.

A prefix argument is handled like `recenter':
 With numeric prefix ARG, move current line to window-line ARG.
 With plain \\[universal-argument], move current line to window center."
  (interactive "P")
  (with-selected-window (other-window-for-scrolling)
    (recenter-top-bottom arg)
    (pulse-momentary-highlight-one-line (point))))

(define-key global-map [?\S-\M-\C-l] 'recenter-other-window)

(defun move-to-window-line-top-bottom (&optional arg)
  "Position point relative to window.

With a prefix argument ARG, acts like `move-to-window-line'.

With no argument, positions point at center of window.
Successive calls position point at positions defined
by `recenter-positions'."
  (interactive "P")
  (cond
   (arg (move-to-window-line arg))	; Always respect ARG.
   (t
    (setq recenter-last-op
	  (if (eq this-command last-command)
	      (car (or (cdr (member recenter-last-op recenter-positions))
		       recenter-positions))
	    (car recenter-positions)))
    (let ((this-scroll-margin
	   (min (max 0 scroll-margin)
		(truncate (/ (window-body-height) 4.0)))))
      (cond ((eq recenter-last-op 'middle)
	     (call-interactively 'move-to-window-line))
	    ((eq recenter-last-op 'top)
	     (move-to-window-line this-scroll-margin))
	    ((eq recenter-last-op 'bottom)
	     (move-to-window-line (- -1 this-scroll-margin)))
	    ((integerp recenter-last-op)
	     (move-to-window-line recenter-last-op))
	    ((floatp recenter-last-op)
	     (move-to-window-line (round (* recenter-last-op (window-height))))))))))

(define-key global-map [?\M-r] 'move-to-window-line-top-bottom)

;;; Scrolling commands.

;;; Scrolling commands which do not signal errors at top/bottom
;;; of buffer at first key-press (instead move to top/bottom
;;; of buffer).

(defcustom scroll-error-top-bottom nil
  "Move point to top/bottom of buffer before signaling a scrolling error.
A value of nil means just signal an error if no more scrolling possible.
A value of t means point moves to the beginning or the end of the buffer
\(depending on scrolling direction) when no more scrolling possible.
When point is already on that position, then signal an error."
  :type 'boolean
  :group 'windows
  :version "24.1")

(defun scroll-up-command (&optional arg)
  "Scroll text of selected window upward ARG lines; or near full screen if no ARG.
If `scroll-error-top-bottom' is non-nil and `scroll-up' cannot
scroll window further, move cursor to the bottom line.
When point is already on that position, then signal an error.
A near full screen is `next-screen-context-lines' less than a full screen.
Negative ARG means scroll downward.
If ARG is the atom `-', scroll downward by nearly full screen."
  (interactive "^P")
  (cond
   ((null scroll-error-top-bottom)
    (scroll-up arg))
   ((eq arg '-)
    (scroll-down-command nil))
   ((< (prefix-numeric-value arg) 0)
    (scroll-down-command (- (prefix-numeric-value arg))))
   ((eobp)
    (scroll-up arg))			; signal error
   (t
    (condition-case nil
	(scroll-up arg)
      (end-of-buffer
       (if arg
	   ;; When scrolling by ARG lines can't be done,
	   ;; move by ARG lines instead.
	   (forward-line arg)
	 ;; When ARG is nil for full-screen scrolling,
	 ;; move to the bottom of the buffer.
	 (goto-char (point-max))))))))

(put 'scroll-up-command 'scroll-command t)

(defun scroll-down-command (&optional arg)
  "Scroll text of selected window down ARG lines; or near full screen if no ARG.
If `scroll-error-top-bottom' is non-nil and `scroll-down' cannot
scroll window further, move cursor to the top line.
When point is already on that position, then signal an error.
A near full screen is `next-screen-context-lines' less than a full screen.
Negative ARG means scroll upward.
If ARG is the atom `-', scroll upward by nearly full screen."
  (interactive "^P")
  (cond
   ((null scroll-error-top-bottom)
    (scroll-down arg))
   ((eq arg '-)
    (scroll-up-command nil))
   ((< (prefix-numeric-value arg) 0)
    (scroll-up-command (- (prefix-numeric-value arg))))
   ((bobp)
    (scroll-down arg))			; signal error
   (t
    (condition-case nil
	(scroll-down arg)
      (beginning-of-buffer
       (if arg
	   ;; When scrolling by ARG lines can't be done,
	   ;; move by ARG lines instead.
	   (forward-line (- arg))
	 ;; When ARG is nil for full-screen scrolling,
	 ;; move to the top of the buffer.
	 (goto-char (point-min))))))))

(put 'scroll-down-command 'scroll-command t)

;;; Scrolling commands which scroll a line instead of full screen.

(defun scroll-up-line (&optional arg)
  "Scroll text of selected window upward ARG lines; or one line if no ARG.
If ARG is omitted or nil, scroll upward by one line.
This is different from `scroll-up-command' that scrolls a full screen."
  (interactive "p")
  (scroll-up (or arg 1)))

(put 'scroll-up-line 'scroll-command t)

(defun scroll-down-line (&optional arg)
  "Scroll text of selected window down ARG lines; or one line if no ARG.
If ARG is omitted or nil, scroll down by one line.
This is different from `scroll-down-command' that scrolls a full screen."
  (interactive "p")
  (scroll-down (or arg 1)))

(put 'scroll-down-line 'scroll-command t)


(defun beginning-of-buffer-other-window (arg)
  "Move point to the beginning of the buffer in the other window.
Leave mark at previous position.
With arg N, put point N/10 of the way from the true beginning."
  (interactive "P")
  (with-selected-window (other-window-for-scrolling)
    ;; Set point and mark in that window's buffer.
    (with-no-warnings
      (beginning-of-buffer arg))
    ;; Set point accordingly.
    (recenter '(t))))

(defun end-of-buffer-other-window (arg)
  "Move point to the end of the buffer in the other window.
Leave mark at previous position.
With arg N, put point N/10 of the way from the true end."
  (interactive "P")
  ;; See beginning-of-buffer-other-window for comments.
  (with-selected-window (other-window-for-scrolling)
    (with-no-warnings
      (end-of-buffer arg))
    (recenter '(t))))

(defvar mouse-autoselect-window-timer nil
  "Timer used by delayed window autoselection.")

(defvar mouse-autoselect-window-position-1 nil
  "First mouse position recorded by delayed window autoselection.")

(defvar mouse-autoselect-window-position nil
  "Last mouse position recorded by delayed window autoselection.")

(defvar mouse-autoselect-window-window nil
  "Last window recorded by delayed window autoselection.")

(defvar mouse-autoselect-window-state nil
  "When non-nil, special state of delayed window autoselection.
Possible values are `suspend' (suspend autoselection after a menu or
scrollbar interaction) and `select' (the next invocation of
`handle-select-window' shall select the window immediately).")

(defun mouse-autoselect-window-cancel (&optional force)
  "Cancel delayed window autoselection.
Optional argument FORCE means cancel unconditionally."
  (unless (and (not force)
	       ;; Don't cancel for select-window or select-frame events
	       ;; or when the user drags a scroll bar.
	       (or (memq this-command
			 '(handle-select-window handle-switch-frame))
		   (and (eq this-command 'scroll-bar-toolkit-scroll)
			(memq (nth 4 (event-end last-input-event))
			      '(handle end-scroll)))))
    (setq mouse-autoselect-window-state nil)
    (setq mouse-autoselect-window-position-1 nil)
    (when (timerp mouse-autoselect-window-timer)
      (cancel-timer mouse-autoselect-window-timer))
    (remove-hook 'pre-command-hook 'mouse-autoselect-window-cancel)))

(defun mouse-autoselect-window-start (mouse-position &optional window suspend)
  "Start delayed window autoselection.
MOUSE-POSITION is the last position where the mouse was seen as returned
by `mouse-position'.  Optional argument WINDOW non-nil denotes the
window where the mouse was seen.  Optional argument SUSPEND non-nil
means suspend autoselection."
  ;; Record values for MOUSE-POSITION, WINDOW, and SUSPEND.
  (setq mouse-autoselect-window-position mouse-position)
  (when window (setq mouse-autoselect-window-window window))
  (setq mouse-autoselect-window-state (when suspend 'suspend))
  ;; Install timer which runs `mouse-autoselect-window-select' after
  ;; `mouse-autoselect-window' seconds.
  (setq mouse-autoselect-window-timer
	(run-at-time
	 (abs mouse-autoselect-window) nil 'mouse-autoselect-window-select)))

(defun mouse-autoselect-window-select ()
  "Select window with delayed window autoselection.
If the mouse position has stabilized in a non-selected window, select
that window.  The minibuffer window is selected only if the minibuffer
is active.  This function is run by `mouse-autoselect-window-timer'."
  (let* ((mouse-position (mouse-position))
         (mouse-x (and (numberp (cadr mouse-position))
                       (cadr mouse-position)))
         (mouse-y (and (numberp (cddr mouse-position))
                       (cddr mouse-position)))
         (frame (and mouse-x mouse-y (car mouse-position)))
         (window (and frame (window-at mouse-x mouse-y frame))))
    (cond
      ((or (and (fboundp 'menu-or-popup-active-p) (menu-or-popup-active-p))
	   (and window
		(let ((coords (coordinates-in-window-p
			       (cdr mouse-position) window)))
		  (and (not (consp coords))
		       (not (memq coords '(left-margin right-margin)))))))
       ;; A menu / popup dialog is active or the mouse is not on the
       ;; text region of WINDOW: Suspend autoselection temporarily.
       (mouse-autoselect-window-start mouse-position nil t))
      ((or (eq mouse-autoselect-window-state 'suspend)
          ;; When the mouse is at its first recorded position, restart
          ;; delayed autoselection.  This works around a scenario with
          ;; two two-window frames with identical dimensions: select the
          ;; first window of the first frame, switch to the second
          ;; frame, move the mouse to its second window, minimize the
          ;; second frame.  Now the second window of the first frame
          ;; gets selected although the mouse never really "moved" into
          ;; that window.
          (and (numberp mouse-autoselect-window)
               (equal (mouse-position) mouse-autoselect-window-position-1)))
      ;; Delayed autoselection was temporarily suspended, reenable it.
      (mouse-autoselect-window-start mouse-position))
     ((and window
           (or (not (numberp mouse-autoselect-window))
               (and (>= mouse-autoselect-window 0)
                    ;; If `mouse-autoselect-window' is non-negative,
                    ;; select window if it's the same as before.
                    (eq window mouse-autoselect-window-window))
               ;; Otherwise select window iff the mouse is at the same
               ;; position as before.  Observe that the first test
               ;; after starting autoselection usually fails since the
               ;; value of `mouse-autoselect-window-position' recorded
               ;; there is the position where the mouse has entered the
               ;; new window and not necessarily where the mouse has
               ;; stopped moving.
               (equal mouse-position mouse-autoselect-window-position))
           ;; The minibuffer is a candidate window if it's active.
           (or (not (window-minibuffer-p window))
               (eq window (active-minibuffer-window))))
      ;; Mouse position has stabilized in non-selected window: Cancel
      ;; delayed autoselection and try to select that window.
      (mouse-autoselect-window-cancel t)
      ;; Use `unread-command-events' in order to execute pre- and
      ;; post-command hooks and trigger idle timers.  To avoid delaying
      ;; autoselection again, set `mouse-autoselect-window-state'."
      (setq mouse-autoselect-window-state 'select)
      (setq unread-command-events
            (cons (list 'select-window (list window))
                  unread-command-events)))
     ((or (not (numberp mouse-autoselect-window))
          (equal mouse-position mouse-autoselect-window-position))
      ;; Mouse position has stabilized at
      ;; `mouse-autoselect-window-position': Cancel delayed
      ;; autoselection.
     (mouse-autoselect-window-cancel t))
    (window
     ;; Mouse position has not stabilized yet, resume delayed
     ;; autoselection.
     (mouse-autoselect-window-start mouse-position window)))))

(declare-function display-multi-frame-p "frame" (&optional display))

(defun handle-select-window (event)
  "Handle select-window events."
  (interactive "^e")
  (let* ((window (posn-window (event-start event)))
         (frame (and (window-live-p window) (window-frame window)))
         (old-frame (selected-frame)))
    (unless (or (not (window-live-p window))
		;; Don't switch when autoselection shall be delayed.
		(and (numberp mouse-autoselect-window)
		     (not (eq mouse-autoselect-window-state 'select))
		     (let ((position (mouse-position)))
		       ;; Cancel any delayed autoselection.
		       (mouse-autoselect-window-cancel t)
		       ;; Start delayed autoselection from current mouse
		       ;; position and window.
		       (setq mouse-autoselect-window-position-1 position)
		       (mouse-autoselect-window-start position window)
		       ;; Executing a command cancels delayed autoselection.
		       (add-hook
			'pre-command-hook 'mouse-autoselect-window-cancel)))
                ;; Don't switch to a `no-accept-focus' frame unless it's
                ;; already selected.
                (and (not (eq frame (selected-frame)))
                     (frame-parameter frame 'no-accept-focus))
                ;; Don't switch if window autoselection with mouse is active
                ;; and minibuffer window is selected.
                (and mouse-autoselect-window (window-minibuffer-p))
                ;; Don't switch to minibuffer window unless it's active.
                (and (window-minibuffer-p window)
                     (not (minibuffer-window-active-p window))))
      ;; Reset state of delayed autoselection.
      (setq mouse-autoselect-window-state nil)
      ;; Run `mouse-leave-buffer-hook' when autoselecting window.
      (run-hooks 'mouse-leave-buffer-hook)
      ;; Clear echo area.
      (message nil)
      ;; Select the window before giving the frame focus since otherwise
      ;; we might get two windows with an active cursor.
      (select-window window)
      (cond
       ((or (not (display-multi-frame-p))
            (not focus-follows-mouse)
            ;; Focus FRAME if it's either a child frame or an ancestor
            ;; of the frame switched from.
            (and (not (frame-parameter frame 'parent-frame))
                 (not (frame-ancestor-p frame old-frame)))))
       ((eq focus-follows-mouse 'auto-raise)
        ;; Focus and auto-raise frame.
        (x-focus-frame frame)
        ;; This doesn't seem to work when we move from a normal frame
        ;; right into the child frame of another frame - we should raise
        ;; that child frame's ancestor frame first ...
        (raise-frame frame))
       (t
        ;; Just focus frame.
        (x-focus-frame frame t))))))

(defun truncated-partial-width-window-p (&optional window)
  "Return non-nil if lines in WINDOW are specifically truncated due to its width.
WINDOW must be a live window and defaults to the selected one.
Return nil if WINDOW is not a partial-width window
 (regardless of the value of `truncate-lines').
Otherwise, consult the value of `truncate-partial-width-windows'
 for the buffer shown in WINDOW."
  (setq window (window-normalize-window window t))
  (unless (window-full-width-p window)
    (let ((t-p-w-w (buffer-local-value 'truncate-partial-width-windows
				       (window-buffer window))))
      (if (integerp t-p-w-w)
	  (< (window-width window) t-p-w-w)
        t-p-w-w))))


;; Automatically inform subprocesses of changes to window size.

(defcustom window-adjust-process-window-size-function
  'window-adjust-process-window-size-smallest
  "Control how Emacs chooses inferior process window sizes.
Emacs uses this function to tell processes the space they have
available for displaying their output.  After each window
configuration change, Emacs calls the value of
`window-adjust-process-window-size-function' for each process
with a buffer being displayed in at least one window.
This function is responsible for combining the sizes of the
displayed windows and returning a cons (WIDTH . HEIGHT)
describing the width and height with which Emacs will call
`set-process-window-size' for that process.  If the function
returns nil, Emacs does not call `set-process-window-size'.

This function is called with the process buffer as the current
buffer and with two arguments: the process and a list of windows
displaying process.  Modes can make this variable buffer-local;
additionally, the `adjust-window-size-function' process property
overrides the global or buffer-local value of
`window-adjust-process-window-size-function'."
  :type '(choice
          (const :tag "Minimum area of any window"
           window-adjust-process-window-size-smallest)
          (const :tag "Maximum area of any window"
           window-adjust-process-window-size-largest)
          (const :tag "Do not adjust process window sizes" ignore)
          function)
  :group 'windows
  :version "25.1")

(defun window-adjust-process-window-size (reducer windows)
  "Adjust the window sizes of a process.
WINDOWS is a list of windows associated with that process.  REDUCER is
a two-argument function used to combine the widths and heights of
the given windows."
  (when windows
    (let ((width (window-max-chars-per-line (car windows)))
          (height (with-selected-window (car windows)
                    (floor (window-screen-lines)))))
      (dolist (window (cdr windows))
        (setf width (funcall reducer width (window-max-chars-per-line window)))
        (setf height (funcall reducer height (with-selected-window window
                                               (floor (window-screen-lines))))))
      (cons width height))))

(defun window-adjust-process-window-size-smallest (_process windows)
  "Adjust the process window size of PROCESS.
WINDOWS is a list of windows associated with PROCESS.  Choose the
smallest area available for displaying PROCESS's output."
  (window-adjust-process-window-size #'min windows))

(defun window-adjust-process-window-size-largest (_process windows)
  "Adjust the process window size of PROCESS.
WINDOWS is a list of windows associated with PROCESS.  Choose the
largest area available for displaying PROCESS's output."
  (window-adjust-process-window-size #'max windows))

(defun window--process-window-list ()
  "Return an alist mapping processes to associated windows.
A window is associated with a process if that window is
displaying that processes's buffer."
  (let ((processes (process-list))
        (process-windows nil))
    (if processes
        (walk-windows
         (lambda (window)
           (let ((buffer (window-buffer window))
                 (iter processes))
             (while (let ((process (car iter)))
                      (if (and (process-live-p process)
                               (eq buffer (process-buffer process)))
                          (let ((procwin (assq process process-windows)))
                            ;; Add this window to the list of windows
                            ;; displaying process.
                            (if procwin
                                (push window (cdr procwin))
                              (push (list process window) process-windows))
                            ;; We found our process for this window, so
                            ;; stop iterating over the process list.
                            nil)
                        (setf iter (cdr iter)))))))
         1 t))
    process-windows))

(defun window--adjust-process-windows ()
  "Update process window sizes to match the current window configuration."
  (when (fboundp 'process-list)
    (dolist (procwin (window--process-window-list))
      (let ((process (car procwin)))
        (with-demoted-errors "Error adjusting window size: %S"
          (with-current-buffer (process-buffer process)
            (let ((size (funcall
                         (or (process-get process 'adjust-window-size-function)
                             window-adjust-process-window-size-function)
                         process (cdr procwin))))
              (when size
                (set-process-window-size process (cdr size) (car size))))))))))

(add-hook 'window-configuration-change-hook 'window--adjust-process-windows)

;; Some of these are in tutorial--default-keys, so update that if you
;; change these.
(define-key ctl-x-map "0" 'delete-window)
(define-key ctl-x-map "1" 'delete-other-windows)
(define-key ctl-x-map "2" 'split-window-below)
(define-key ctl-x-map "3" 'split-window-right)
(define-key ctl-x-map "o" 'other-window)
(define-key ctl-x-map "^" 'enlarge-window)
(define-key ctl-x-map "}" 'enlarge-window-horizontally)
(define-key ctl-x-map "{" 'shrink-window-horizontally)
(define-key ctl-x-map "-" 'shrink-window-if-larger-than-buffer)
(define-key ctl-x-map "+" 'balance-windows)
(define-key ctl-x-4-map "0" 'kill-buffer-and-window)
(define-key ctl-x-4-map "1" 'same-window-prefix)
(define-key ctl-x-4-map "4" 'other-window-prefix)

(defvar other-window-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map "o" 'other-window)
    (define-key map "O" (lambda ()
                          (interactive)
                          (setq repeat-map 'other-window-repeat-map)
                          (other-window -1)))
    map)
  "Keymap to repeat `other-window' key sequences.  Used in `repeat-mode'.")
(put 'other-window 'repeat-map 'other-window-repeat-map)

(defvar resize-window-repeat-map
  (let ((map (make-sparse-keymap)))
    ;; Standard keys:
    (define-key map "^" 'enlarge-window)
    (define-key map "}" 'enlarge-window-horizontally)
    (define-key map "{" 'shrink-window-horizontally)
    ;; Additional keys:
    (define-key map "v" 'shrink-window)
    map)
  "Keymap to repeat window resizing commands.  Used in `repeat-mode'.")
(put 'enlarge-window 'repeat-map 'resize-window-repeat-map)
(put 'enlarge-window-horizontally 'repeat-map 'resize-window-repeat-map)
(put 'shrink-window-horizontally 'repeat-map 'resize-window-repeat-map)
(put 'shrink-window 'repeat-map 'resize-window-repeat-map)

(provide 'window)

;;; window.el ends here
