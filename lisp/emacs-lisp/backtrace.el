;;; backtrace.el --- generic major mode for Elisp backtraces -*- lexical-binding: t -*-

;; Copyright (C) 2018-2022 Free Software Foundation, Inc.

;; Author: Gemini Lasswell
;; Keywords: lisp, tools, maint
;; Version: 1.0

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

;; This file defines Backtrace mode, a generic major mode for displaying
;; Elisp stack backtraces, which can be used as is or inherited from
;; by another mode.

;; For usage information, see the documentation of `backtrace-mode'.

;;; Code:

(eval-when-compile (require 'cl-lib))
(eval-when-compile (require 'pcase))
(eval-when-compile (require 'subr-x))        ; if-let
(require 'find-func)
(require 'help-mode)     ; Define `help-function-def' button type.
(require 'lisp-mode)

;;; Options

(defgroup backtrace nil
  "Viewing of Elisp backtraces."
  :group 'lisp)

(defcustom backtrace-fontify t
  "If non-nil, fontify Backtrace buffers.
Set to nil to disable fontification, which may be necessary in
order to debug the code that does fontification."
  :type 'boolean
  :group 'backtrace
  :version "27.1")

(defcustom backtrace-line-length 5000
  "Target length for lines in Backtrace buffers.
Backtrace mode will attempt to abbreviate printing of backtrace
frames to make them shorter than this, but success is not
guaranteed.  If set to nil or zero, Backtrace mode will not
abbreviate the forms it prints."
  :type 'integer
  :group 'backtrace
  :version "27.1")

;;; Backtrace frame data structure

(cl-defstruct
    (backtrace-frame
     (:constructor backtrace-make-frame))
  evald  ; Non-nil if argument evaluation is complete.
  fun    ; The function called/to call in this frame.
  args   ; Either evaluated or unevaluated arguments to the function.
  flags  ; A plist, possible properties are :debug-on-exit and :source-available.
  locals ; An alist containing variable names and values.
  buffer ; If non-nil, the buffer in use by eval-buffer or eval-region.
  pos    ; The position in the buffer.
  )

(cl-defun backtrace-get-frames
    (&optional base &key (constructor #'backtrace-make-frame))
  "Collect all frames of current backtrace into a list.
The list will contain objects made by CONSTRUCTOR, which
defaults to `backtrace-make-frame' and which, if provided, should
be the constructor of a structure which includes
`backtrace-frame'.  If non-nil, BASE should be a function, and
frames before its nearest activation frame are discarded."
  (let ((frames nil)
        (eval-buffers eval-buffer-list))
    (mapbacktrace (lambda (evald fun args flags)
                    (push (funcall constructor
                                   :evald evald :fun fun
                                   :args args :flags flags)
                          frames))
                  (or base 'backtrace-get-frames))
    (setq frames (nreverse frames))
    ;; Add local variables to each frame, and the buffer position
    ;; to frames containing eval-buffer or eval-region.
    (dotimes (idx (length frames))
      (let ((frame (nth idx frames)))
        ;; `backtrace--locals' gives an error when idx is 0.  But the
        ;; locals for frame 0 are not needed, because when we get here
        ;; from debug-on-entry, the locals aren't bound yet, and when
        ;; coming from Edebug or ERT there is an Edebug or ERT
        ;; function at frame 0.
        (when (> idx 0)
          (setf (backtrace-frame-locals frame)
                (backtrace--locals idx (or base 'backtrace-get-frames))))
        (when (and eval-buffers (memq (backtrace-frame-fun frame)
                                      '(eval-buffer eval-region)))
          ;; This will get the wrong result if there are two nested
          ;; eval-region calls for the same buffer.  That's not a very
          ;; useful case.
          (with-current-buffer (pop eval-buffers)
            (setf (backtrace-frame-buffer frame) (current-buffer))
            (setf (backtrace-frame-pos frame) (point))))))
    frames))

;; Button definition for jumping to a buffer position.

(define-button-type 'backtrace-buffer-pos
  'action #'backtrace--pop-to-buffer-pos
  'help-echo "mouse-2, RET: Show reading position")

(defun backtrace--pop-to-buffer-pos (button)
  "Pop to the buffer and position for the BUTTON at point."
  (let* ((buffer (button-get button 'backtrace-buffer))
         (pos (button-get button 'backtrace-pos)))
    (if (buffer-live-p buffer)
        (progn
          (pop-to-buffer buffer)
          (goto-char (max (point-min) (min (point-max) pos))))
      (message "Buffer has been killed"))))

;; Font Locking support

(defconst backtrace--font-lock-keywords
  '((backtrace--match-ellipsis-in-string
     (1 'button prepend)))
  "Expressions to fontify in Backtrace mode.
Fontify these in addition to the expressions Emacs Lisp mode
fontifies.")

(defconst backtrace-font-lock-keywords
  (append lisp-el-font-lock-keywords-for-backtraces
          backtrace--font-lock-keywords)
  "Default expressions to highlight in Backtrace mode.")
(defconst backtrace-font-lock-keywords-1
  (append lisp-el-font-lock-keywords-for-backtraces-1
          backtrace--font-lock-keywords)
  "Subdued level highlighting for Backtrace mode.")
(defconst backtrace-font-lock-keywords-2
  (append lisp-el-font-lock-keywords-for-backtraces-2
          backtrace--font-lock-keywords)
  "Gaudy level highlighting for Backtrace mode.")

(defun backtrace--match-ellipsis-in-string (bound)
  ;; Fontify ellipses within strings as buttons.
  ;; This is necessary because ellipses are text property buttons
  ;; instead of overlay buttons, which is done because there could
  ;; be a large number of them.
  (when (re-search-forward "\\(\\.\\.\\.\\)\"" bound t)
    (and (get-text-property (- (point) 2) 'cl-print-ellipsis)
         (get-text-property (- (point) 3) 'cl-print-ellipsis)
         (get-text-property (- (point) 4) 'cl-print-ellipsis))))

;;; Xref support

(defun backtrace--xref-backend () 'elisp)

;;; Backtrace mode variables

(defvar-local backtrace-frames nil
  "Stack frames displayed in the current Backtrace buffer.
This should be a list of `backtrace-frame' objects.")

(defvar-local backtrace-view nil
  "A plist describing how to render backtrace frames.
Possible entries are :show-flags, :show-locals, :print-circle
and :print-gensym.")

(defvar-local backtrace-insert-header-function nil
  "Function for inserting a header for the current Backtrace buffer.
If nil, no header will be created.  Note that Backtrace buffers
are fontified as in Emacs Lisp Mode, the header text included.")

(defvar backtrace-revert-hook nil
  "Hook run before reverting a Backtrace buffer.
This is commonly used to recompute `backtrace-frames'.")

(defvar-local backtrace-print-function #'cl-prin1
  "Function used to print values in the current Backtrace buffer.")

(defvar backtrace-goto-source-functions nil
  "Abnormal hook used to jump to the source code for the current frame.
Each hook function is called with no argument, and should return
non-nil if it is able to switch to the buffer containing the
source code.  Execution of the hook will stop if one of the
functions returns non-nil.  When adding a function to this hook,
you should also set the :source-available flag for the backtrace
frames where the source code location is known.")

(defvar backtrace-mode-map
  (let ((map (copy-keymap special-mode-map)))
    (set-keymap-parent map button-buffer-map)
    (define-key map "n" 'backtrace-forward-frame)
    (define-key map "p" 'backtrace-backward-frame)
    (define-key map "v" 'backtrace-toggle-locals)
    (define-key map "#" 'backtrace-toggle-print-circle)
    (define-key map ":" 'backtrace-toggle-print-gensym)
    (define-key map "s" 'backtrace-goto-source)
    (define-key map "\C-m" 'backtrace-help-follow-symbol)
    (define-key map "+" 'backtrace-multi-line)
    (define-key map "-" 'backtrace-single-line)
    (define-key map "." 'backtrace-expand-ellipses)
    (define-key map [follow-link] 'mouse-face)
    (define-key map [mouse-2] 'mouse-select-window)
    (easy-menu-define nil map ""
      '("Backtrace"
        ["Next Frame" backtrace-forward-frame
         :help "Move cursor forwards to the start of a backtrace frame"]
        ["Previous Frame" backtrace-backward-frame
         :help "Move cursor backwards to the start of a backtrace frame"]
        "--"
        ["Show Variables" backtrace-toggle-locals
         :style toggle
         :active (backtrace-get-index)
         :selected (plist-get (backtrace-get-view) :show-locals)
         :help "Show or hide the local variables for the frame at point"]
        ["Show Circular Structures" backtrace-toggle-print-circle
         :style toggle
         :active (backtrace-get-index)
         :selected (plist-get (backtrace-get-view) :print-circle)
         :help
         "Condense or expand shared or circular structures in the frame at point"]
        ["Show Uninterned Symbols" backtrace-toggle-print-gensym
         :style toggle
         :active (backtrace-get-index)
         :selected (plist-get (backtrace-get-view) :print-gensym)
         :help
         "Toggle unique printing of uninterned symbols in the frame at point"]
        ["Expand \"...\"s" backtrace-expand-ellipses
         :help "Expand all the abbreviated forms in the current frame"]
        ["Show on Multiple Lines" backtrace-multi-line
         :help "Use line breaks and indentation to make a form more readable"]
        ["Show on Single Line" backtrace-single-line]
        "--"
        ["Go to Source" backtrace-goto-source
         :active (and (backtrace-get-index)
                      (plist-get (backtrace-frame-flags
                                  (nth (backtrace-get-index) backtrace-frames))
                                 :source-available))
         :help "Show the source code for the current frame"]
        ["Help for Symbol" backtrace-help-follow-symbol
         :help "Show help for symbol at point"]
        ["Describe Backtrace Mode" describe-mode
         :help "Display documentation for backtrace-mode"]))
    map)
  "Local keymap for `backtrace-mode' buffers.")

(defconst backtrace--flags-width 2
  "Width in characters of the flags for a backtrace frame.")

;;; Navigation and Text Properties

;; This mode uses the following text properties:
;; backtrace-index: The index into the buffer-local variable
;;   `backtrace-frames' for the frame at point, or nil if outside of a
;;   frame (in the buffer header).
;; backtrace-view: A plist describing how the frame is printed.  See
;;   the docstring for the buffer-local variable `backtrace-view.
;; backtrace-section: The part of a frame which point is in.  Either
;;   `func' or `locals'.  At the moment just used to show and hide the
;;   local variables.  Derived modes which do additional printing
;;   could define their own frame sections.
;; backtrace-form: A value applied to each printed representation of a
;;   top-level s-expression, which needs to be different for sexps
;;   printed adjacent to each other, so the limits can be quickly
;;   found for pretty-printing.

(defsubst backtrace-get-index (&optional pos)
  "Return the index of the backtrace frame at POS.
The value is an index into `backtrace-frames', or nil.
POS, if omitted or nil, defaults to point."
  (get-text-property (or pos (point)) 'backtrace-index))

(defsubst backtrace-get-section (&optional pos)
  "Return the section of a backtrace frame at POS.
POS, if omitted or nil, defaults to point."
  (get-text-property (or pos (point)) 'backtrace-section))

(defsubst backtrace-get-view (&optional pos)
  "Return the view plist of the backtrace frame at POS.
POS, if omitted or nil, defaults to point."
  (get-text-property (or pos (point)) 'backtrace-view))

(defsubst backtrace-get-form (&optional pos)
  "Return the backtrace form data for the form printed at POS.
POS, if omitted or nil, defaults to point."
  (get-text-property (or pos (point)) 'backtrace-form))

(defun backtrace-get-frame-start (&optional pos)
  "Return the beginning position of the frame at POS in the buffer.
POS, if omitted or nil, defaults to point."
  (let ((posn (or pos (point))))
    (if (or (= (point-min) posn)
            (not (eq (backtrace-get-index posn)
                     (backtrace-get-index (1- posn)))))
        posn
      (previous-single-property-change posn 'backtrace-index nil (point-min)))))

(defun backtrace-get-frame-end (&optional pos)
  "Return the position of the end of the frame at POS in the buffer.
POS, if omitted or nil, defaults to point."
  (next-single-property-change (or pos (point))
                                    'backtrace-index nil (point-max)))

(defun backtrace-forward-frame ()
  "Move forward to the beginning of the next frame."
  (interactive)
  (let ((max (backtrace-get-frame-end)))
    (when (= max (point-max))
      (user-error "No next stack frame"))
    (goto-char max)))

(defun backtrace-backward-frame ()
  "Move backward to the start of a stack frame."
  (interactive)
  (let ((current-index (backtrace-get-index))
        (min (backtrace-get-frame-start)))
    (if (or (and (/= (point) (point-max)) (null current-index))
            (= min (point-min))
            (and (= min (point))
                 (null (backtrace-get-index (1- min)))))
        (user-error "No previous stack frame"))
    (if (= min (point))
        (goto-char (backtrace-get-frame-start (1- min)))
      (goto-char min))))

;; Other Backtrace mode commands

(defun backtrace-revert (&rest _ignored)
  "The `revert-buffer-function' for `backtrace-mode'.
It runs `backtrace-revert-hook', then calls `backtrace-print'."
  (interactive)
  (unless (derived-mode-p 'backtrace-mode)
    (error "The current buffer is not in Backtrace mode"))
  (run-hooks 'backtrace-revert-hook)
  (backtrace-print t))

(defmacro backtrace--with-output-variables (view &rest body)
  "Bind output variables according to VIEW and execute BODY."
  (declare (indent 1))
  `(let ((print-escape-control-characters t)
         (print-escape-newlines t)
         (print-circle (plist-get ,view :print-circle))
         (print-gensym (plist-get ,view :print-gensym))
         (standard-output (current-buffer)))
     ,@body))

(defun backtrace-toggle-locals (&optional all)
  "Toggle the display of local variables for the backtrace frame at point.
With prefix argument ALL, toggle the value of :show-locals in
`backtrace-view', which affects all of the backtrace frames in
the buffer."
  (interactive "P")
  (if all
      (let ((pos (make-marker))
            (visible (not (plist-get backtrace-view :show-locals))))
        (setq backtrace-view (plist-put backtrace-view :show-locals visible))
        (set-marker-insertion-type pos t)
        (set-marker pos (point))
        (goto-char (point-min))
        ;; Skip the header.
        (unless (backtrace-get-index)
          (goto-char (backtrace-get-frame-end)))
        (while (< (point) (point-max))
          (backtrace--set-frame-locals-visible visible)
          (goto-char (backtrace-get-frame-end)))
        (goto-char pos)
        (when (invisible-p pos)
          (goto-char (backtrace-get-frame-start))))
    (let ((index (backtrace-get-index)))
      (unless index
        (user-error "Not in a stack frame"))
      (backtrace--set-frame-locals-visible
       (not (plist-get (backtrace-get-view) :show-locals))))))

(defun backtrace--set-frame-locals-visible (visible)
  "Set the visibility of the local vars for the frame at point to VISIBLE."
  (let ((pos (point))
        (index (backtrace-get-index))
        (start (backtrace-get-frame-start))
        (end (backtrace-get-frame-end))
        (view (copy-sequence (backtrace-get-view)))
        (inhibit-read-only t))
    (setq view (plist-put view :show-locals visible))
    (goto-char (backtrace-get-frame-start))
    (while (not (or (= (point) end)
                    (eq (backtrace-get-section) 'locals)))
      (goto-char (next-single-property-change (point)
                                              'backtrace-section nil end)))
    (cond
     ((and (= (point) end) visible)
      ;; The locals section doesn't exist so create it.
      (let ((standard-output (current-buffer)))
        (backtrace--with-output-variables view
          (backtrace--print-locals
           (nth index backtrace-frames) view))
        (add-text-properties end (point) `(backtrace-index ,index))
        (goto-char pos)))
     ((/= (point) end)
      ;; The locals section does exist, so add or remove the overlay.
      (backtrace--set-locals-visible-overlay (point) end visible)
      (goto-char (if (invisible-p pos) start pos))))
    (add-text-properties start (backtrace-get-frame-end)
                         `(backtrace-view ,view))))

(defun backtrace--set-locals-visible-overlay (beg end visible)
  (backtrace--change-button-skip beg end (not visible))
  (if visible
      (remove-overlays beg end 'invisible t)
    (let ((o (make-overlay beg end)))
      (overlay-put o 'invisible t)
      (overlay-put o 'evaporate t))))

(defun backtrace--change-button-skip (beg end value)
  "Change the skip property on all buttons between BEG and END.
Set it to VALUE unless the button is a `backtrace-ellipsis' button."
  (let ((inhibit-read-only t))
    (setq beg (next-button beg))
    (while (and beg (< beg end))
      (unless (eq (button-type beg) 'backtrace-ellipsis)
          (button-put beg 'skip value))
      (setq beg (next-button beg)))))

(defun backtrace-toggle-print-circle (&optional all)
  "Toggle `print-circle' for the backtrace frame at point.
With prefix argument ALL, toggle the default value bound to
`print-circle' for all the frames in the buffer."
  (interactive "P")
  (backtrace--toggle-feature :print-circle all))

(defun backtrace-toggle-print-gensym (&optional all)
  "Toggle `print-gensym' for the backtrace frame at point.
With prefix argument ALL, toggle the default value bound to
`print-gensym' for all the frames in the buffer."
  (interactive "P")
  (backtrace--toggle-feature :print-gensym all))

(defun backtrace--toggle-feature (feature all)
  "Toggle FEATURE for the current backtrace frame or for the buffer.
FEATURE should be one of the options in `backtrace-view'.  If ALL
is non-nil, toggle FEATURE for all frames in the buffer.  After
toggling the feature, reprint the affected frame(s).  Afterwards
position point at the start of the frame it was in before."
  (if all
      (let ((index (backtrace-get-index))
            (pos (point))
            (at-end (= (point) (point-max)))
            (value (not (plist-get backtrace-view feature))))
        (setq backtrace-view (plist-put backtrace-view feature value))
        (goto-char (point-min))
        ;; Skip the header.
        (unless (backtrace-get-index)
          (goto-char (backtrace-get-frame-end)))
        (while (< (point) (point-max))
          (backtrace--set-feature feature value)
          (goto-char (backtrace-get-frame-end)))
        (if (not index)
            (goto-char (if at-end (point-max) pos))
          (goto-char (point-min))
          (while (and (not (eql index (backtrace-get-index)))
                      (< (point) (point-max)))
            (goto-char (backtrace-get-frame-end))))
        (message "%s is now %s for all frames"
                 (substring (symbol-name feature) 1) value))
    (unless (backtrace-get-index)
      (user-error "Not in a stack frame"))
    (let ((value (not (plist-get (backtrace-get-view) feature))))
      (backtrace--set-feature feature value)
      (message "%s is now %s for this frame"
               (substring (symbol-name feature) 1) value))))

(defun backtrace--set-feature (feature value)
  "Set FEATURE in the view plist of the frame at point to VALUE.
Reprint the frame with the new view plist."
  (let ((inhibit-read-only t)
        (view (copy-sequence (backtrace-get-view)))
        (index (backtrace-get-index))
        (min (backtrace-get-frame-start))
        (max (backtrace-get-frame-end)))
    (setq view (plist-put view feature value))
    (delete-region min max)
    (goto-char min)
    (backtrace-print-frame (nth index backtrace-frames) view)
    (add-text-properties min (point)
                         `(backtrace-index ,index backtrace-view ,view))
    (goto-char min)))

(defun backtrace-expand-ellipsis (button)
  "Expand display of the elided form at BUTTON."
  (interactive)
  (goto-char (button-start button))
  (unless (get-text-property (point) 'cl-print-ellipsis)
    (if (and (> (point) (point-min))
             (get-text-property (1- (point)) 'cl-print-ellipsis))
        (backward-char)
      (user-error "No ellipsis to expand here")))
  (let* ((end (next-single-property-change (point) 'cl-print-ellipsis))
         (begin (previous-single-property-change end 'cl-print-ellipsis))
         (value (get-text-property begin 'cl-print-ellipsis))
         (props (backtrace-get-text-properties begin))
         (inhibit-read-only t))
    (backtrace--with-output-variables (backtrace-get-view)
      (delete-region begin end)
      (insert (cl-print-to-string-with-limit #'cl-print-expand-ellipsis value
                                          backtrace-line-length))
      (setq end (point))
      (goto-char begin)
      (while (< (point) end)
        (let ((next (next-single-property-change (point) 'cl-print-ellipsis
                                                 nil end)))
          (when (get-text-property (point) 'cl-print-ellipsis)
            (make-text-button (point) next :type 'backtrace-ellipsis))
          (goto-char next)))
      (goto-char begin)
      (add-text-properties begin end props))))

(defun backtrace-expand-ellipses (&optional no-limit)
  "Expand display of all \"...\"s in the backtrace frame at point.
\\<backtrace-mode-map>
Each ellipsis will be limited to `backtrace-line-length'
characters in its expansion.  With optional prefix argument
NO-LIMIT, do not limit the number of characters.  Note that with
or without the argument, using this command can result in very
long lines and very poor display performance.  If this happens
and is a problem, use `\\[revert-buffer]' to return to the
initial state of the Backtrace buffer."
  (interactive "P")
  (save-excursion
    (let ((start (backtrace-get-frame-start))
          (end (backtrace-get-frame-end))
          (backtrace-line-length (unless no-limit backtrace-line-length)))
      (goto-char end)
      (while (> (point) start)
        (let ((next (previous-single-property-change (point) 'cl-print-ellipsis
                                                     nil start)))
          (when (get-text-property (point) 'cl-print-ellipsis)
            (push-button (point)))
          (goto-char next))))))

(defun backtrace-multi-line ()
  "Show the top level s-expression at point on multiple lines with indentation."
  (interactive)
  (backtrace--reformat-sexp #'backtrace--multi-line))

(defun backtrace--multi-line ()
  "Pretty print the current buffer, then remove the trailing newline."
  (set-syntax-table emacs-lisp-mode-syntax-table)
  (pp-buffer)
  (goto-char (1- (point-max)))
  (delete-char 1))

(defun backtrace-single-line ()
  "Show the top level s-expression at point on one line."
  (interactive)
  (backtrace--reformat-sexp #'backtrace--single-line))

(defun backtrace--single-line ()
  "Replace line breaks and following indentation with spaces.
Works on the current buffer."
  (goto-char (point-min))
  (while (re-search-forward "\n[[:blank:]]*" nil t)
    (replace-match " ")))

(defun backtrace--reformat-sexp (format-function)
  "Reformat the top level sexp at point.
Locate the top level sexp at or following point on the same line,
and reformat it with FORMAT-FUNCTION, preserving the location of
point within the sexp.  If no sexp is found before the end of
the line or buffer, signal an error.

FORMAT-FUNCTION will be called without arguments, with the
current buffer set to a temporary buffer containing only the
content of the sexp."
  (let* ((orig-pos (point))
         (pos (point))
         (tag (backtrace-get-form pos))
         (end (next-single-property-change pos 'backtrace-form))
         (begin (previous-single-property-change end 'backtrace-form
                                                 nil (point-min))))
    (unless tag
      (when (or (= end (point-max)) (> end (point-at-eol)))
        (user-error "No form here to reformat"))
      (goto-char end)
      (setq pos end
            end (next-single-property-change pos 'backtrace-form)
            begin (previous-single-property-change end 'backtrace-form
                                                   nil (point-min))))
    (let* ((offset (when (>= orig-pos begin) (- orig-pos begin)))
           (offset-marker (when offset (make-marker)))
           (content (buffer-substring begin end))
           (props (backtrace-get-text-properties begin))
           (inhibit-read-only t))
      (delete-region begin end)
      (insert (with-temp-buffer
                (insert content)
                (when offset
                  (set-marker-insertion-type offset-marker t)
                  (set-marker offset-marker (+ (point-min) offset)))
                (funcall format-function)
                (when offset
                  (setq offset (- (marker-position offset-marker) (point-min))))
                (buffer-string)))
      (when offset
        (set-marker offset-marker (+ begin offset)))
      (save-excursion
        (goto-char begin)
        (indent-sexp))
      (add-text-properties begin (point) props)
      (if offset
          (goto-char (marker-position offset-marker))
        (goto-char orig-pos)))))

(defun backtrace-get-text-properties (pos)
  "Return a plist of backtrace-mode's text properties at POS."
  (apply #'append
         (mapcar (lambda (prop)
                   (list prop (get-text-property pos prop)))
                 '(backtrace-section backtrace-index backtrace-view
                                     backtrace-form))))

(defun backtrace-goto-source ()
  "If its location is known, jump to the source code for the frame at point."
  (interactive)
  (let* ((index (or (backtrace-get-index) (user-error "Not in a stack frame")))
         (frame (nth index backtrace-frames))
         (source-available (plist-get (backtrace-frame-flags frame)
                                      :source-available)))
    (unless (and source-available
                 (run-hook-with-args-until-success
                  'backtrace-goto-source-functions))
      (user-error "Source code location not known"))))

(defun backtrace-help-follow-symbol (&optional pos)
  "Follow cross-reference at POS, defaulting to point.
For the cross-reference format, see `help-make-xrefs'."
  (interactive "d")
  (unless pos
    (setq pos (point)))
  (unless (push-button pos)
    ;; Check if the symbol under point is a function or variable.
    (let ((sym
	   (intern
	    (save-excursion
	      (goto-char pos) (skip-syntax-backward "w_")
	      (buffer-substring (point)
				(progn (skip-syntax-forward "w_")
				       (point)))))))
      (when (or (boundp sym) (fboundp sym) (facep sym))
        (describe-symbol sym)))))

;; Print backtrace frames

(defun backtrace-print (&optional remember-pos)
  "Populate the current Backtrace mode buffer.
This erases the buffer and inserts printed representations of the
frames.  Optional argument REMEMBER-POS, if non-nil, means to
move point to the entry with the same ID element as the current
line and recenter window line accordingly."
  (let ((inhibit-read-only t)
	entry-index saved-pt window-line)
    (and remember-pos
	 (setq entry-index (backtrace-get-index))
         (when (eq (window-buffer) (current-buffer))
           (setq window-line
                 (count-screen-lines (window-start) (point)))))
    (erase-buffer)
    (when backtrace-insert-header-function
      (funcall backtrace-insert-header-function))
    (dotimes (idx (length backtrace-frames))
      (let ((beg (point))
            (elt (nth idx backtrace-frames)))
        (and entry-index
             (equal entry-index idx)
             (setq entry-index nil
                   saved-pt (point)))
        (backtrace-print-frame elt backtrace-view)
        (add-text-properties
         beg (point)
         `(backtrace-index ,idx backtrace-view ,backtrace-view))))
    (set-buffer-modified-p nil)
    ;; If REMEMBER-POS was specified, move to the "old" location.
    (if saved-pt
	(progn (goto-char saved-pt)
	       (when window-line
                 (recenter window-line)))
      (goto-char (point-min)))))

;; Define button type used for ...'s.
;; Set skip property so you don't have to TAB through 100 of them to
;; get to the next function name.
(define-button-type 'backtrace-ellipsis
  'skip t 'action #'backtrace-expand-ellipsis
  'help-echo "mouse-2, RET: expand this ellipsis")

(defun backtrace-print-to-string (obj &optional limit)
  "Return a printed representation of OBJ formatted for backtraces.
Attempt to get the length of the returned string under LIMIT
characters with appropriate settings of `print-level' and
`print-length.'  LIMIT defaults to `backtrace-line-length'."
  (backtrace--with-output-variables backtrace-view
    (backtrace--print-to-string obj limit)))

(defun backtrace--print-to-string (sexp &optional limit)
  ;; This is for use by callers who wrap the call with
  ;; backtrace--with-output-variables.
  (setq limit (or limit backtrace-line-length))
  (with-temp-buffer
    (insert (cl-print-to-string-with-limit #'backtrace--print sexp limit))
    ;; Add a unique backtrace-form property.
    (put-text-property (point-min) (point) 'backtrace-form (gensym))
    ;; Make buttons from all the "..."s.  Since there might be many of
    ;; them, use text property buttons.
    (goto-char (point-min))
    (while (< (point) (point-max))
      (let ((end (next-single-property-change (point) 'cl-print-ellipsis
                                              nil (point-max))))
        (when (get-text-property (point) 'cl-print-ellipsis)
          (make-text-button (point) end :type 'backtrace-ellipsis))
        (goto-char end)))
    (buffer-string)))

(defun backtrace-print-frame (frame view)
  "Insert a backtrace FRAME at point formatted according to VIEW.
Tag the sections of the frame with the `backtrace-section' text
property for use by navigation."
  (backtrace--with-output-variables view
   (backtrace--print-flags frame view)
   (backtrace--print-func-and-args frame view)
   (backtrace--print-locals frame view)))

(defun backtrace--print-flags (frame view)
  "Print the flags of a backtrace FRAME if enabled in VIEW."
  (let ((beg (point))
        (flag (plist-get (backtrace-frame-flags frame) :debug-on-exit))
        (source (plist-get (backtrace-frame-flags frame) :source-available)))
    (when (plist-get view :show-flags)
      (when source (insert ">"))
      (when flag (insert "*")))
    (insert (make-string (- backtrace--flags-width (- (point) beg)) ?\s))
    (put-text-property beg (point) 'backtrace-section 'func)))

(defun backtrace--print-func-and-args (frame _view)
  "Print the function, arguments and buffer position of a backtrace FRAME.
Format it according to VIEW."
  (let* ((beg (point))
         (evald (backtrace-frame-evald frame))
         (fun   (backtrace-frame-fun frame))
         (args  (backtrace-frame-args frame))
         (def   (find-function-advised-original fun))
         (fun-file (or (symbol-file fun 'defun)
                       (and (subrp def)
                            (not (eq 'unevalled (cdr (subr-arity def))))
                            (find-lisp-object-file-name fun def))))
         (fun-pt (point)))
    (cond
     ((and evald (not debugger-stack-frame-as-list))
      (if (atom fun)
          (funcall backtrace-print-function fun)
        (insert
         (backtrace--print-to-string fun (when args (/ backtrace-line-length 2)))))
      (if args
          (insert (backtrace--print-to-string
                   args (max (truncate (/ backtrace-line-length 5))
                             (- backtrace-line-length (- (point) beg)))))
        ;; The backtrace-form property is so that backtrace-multi-line
        ;; will find it.  backtrace-multi-line doesn't do anything
        ;; useful with it, just being consistent.
        (let ((start (point)))
          (insert "()")
          (put-text-property start (point) 'backtrace-form t))))
     (t
      (let ((fun-and-args (cons fun args)))
        (insert (backtrace--print-to-string fun-and-args)))
      (cl-incf fun-pt)))
    (when fun-file
      (make-text-button fun-pt (+ fun-pt
                                  (length (backtrace--print-to-string fun)))
                        :type 'help-function-def
                        'help-args (list fun fun-file)))
    ;; After any frame that uses eval-buffer, insert a comment that
    ;; states the buffer position it's reading at.
    (when (backtrace-frame-pos frame)
      (insert "  ; Reading at ")
      (let ((pos (point)))
        (insert (format "buffer position %d" (backtrace-frame-pos frame)))
        (make-button pos (point) :type 'backtrace-buffer-pos
                     'backtrace-buffer (backtrace-frame-buffer frame)
                     'backtrace-pos (backtrace-frame-pos frame))))
    (insert "\n")
    (put-text-property beg (point) 'backtrace-section 'func)))

(defun backtrace--print-locals (frame view)
  "Print a backtrace FRAME's local variables according to VIEW.
Print them only if :show-locals is non-nil in the VIEW plist."
  (when (plist-get view :show-locals)
    (let* ((beg (point))
           (locals (backtrace-frame-locals frame)))
      (if (null locals)
	  (insert "    [no locals]\n")
        (pcase-dolist (`(,symbol . ,value) locals)
          (insert "    ")
          (backtrace--print symbol)
	  (insert " = ")
          (insert (backtrace--print-to-string value))
          (insert "\n")))
      (put-text-property beg (point) 'backtrace-section 'locals))))

(defun backtrace--print (obj &optional stream)
  "Attempt to print OBJ to STREAM using `backtrace-print-function'.
Fall back to `prin1' if there is an error."
  (condition-case err
      (funcall backtrace-print-function obj stream)
    (error
     (message "Error in backtrace printer: %S" err)
     (prin1 obj stream))))

(defun backtrace-update-flags ()
  "Update the display of the flags in the backtrace frame at point."
  (let ((view (backtrace-get-view))
        (begin (backtrace-get-frame-start)))
    (when (plist-get view :show-flags)
      (save-excursion
        (goto-char begin)
        (let ((props (backtrace-get-text-properties begin))
              (inhibit-read-only t)
              (standard-output (current-buffer)))
          (delete-char backtrace--flags-width)
          (backtrace--print-flags (nth (backtrace-get-index) backtrace-frames)
                                  view)
          (add-text-properties begin (point) props))))))

(defun backtrace--filter-visible (beg end &optional _delete)
  "Return the visible text between BEG and END."
  (let ((result ""))
    (while (< beg end)
      (let ((next (next-single-char-property-change beg 'invisible)))
        (unless (get-char-property beg 'invisible)
          (setq result (concat result (buffer-substring beg (min end next)))))
        (setq beg next)))
    result))

;;; The mode definition

(define-derived-mode backtrace-mode special-mode "Backtrace"
  "Generic major mode for examining an Elisp stack backtrace.
This mode can be used directly, or other major modes can be
derived from it, using `define-derived-mode'.

In this major mode, the buffer contains some optional lines of
header text followed by backtrace frames, each consisting of one
or more whole lines.

Letters in this mode do not insert themselves; instead they are
commands.
\\<backtrace-mode-map>
\\{backtrace-mode-map}

A mode which inherits from Backtrace mode, or a command which
creates a `backtrace-mode' buffer, should usually do the following:

 - Set `backtrace-revert-hook', if the buffer contents need
   to be specially recomputed prior to `revert-buffer'.
 - Maybe set `backtrace-insert-header-function' to a function to create
   header text for the buffer.
 - Set `backtrace-frames' (see below).
 - Maybe modify `backtrace-view' (see below).
 - Maybe set `backtrace-print-function'.

A command which creates or switches to a Backtrace mode buffer,
such as `ert-results-pop-to-backtrace-for-test-at-point', should
initialize `backtrace-frames' to a list of `backtrace-frame'
objects (`backtrace-get-frames' is provided for that purpose, if
desired), and may optionally modify `backtrace-view', which is a
plist describing the appearance of the backtrace.  Finally, it
should call `backtrace-print'.

`backtrace-print' calls `backtrace-insert-header-function'
followed by `backtrace-print-frame', once for each stack frame."
  :syntax-table emacs-lisp-mode-syntax-table
  (when backtrace-fontify
    (setq font-lock-defaults
          '((backtrace-font-lock-keywords
             backtrace-font-lock-keywords-1
             backtrace-font-lock-keywords-2)
            nil nil nil nil
	    (font-lock-syntactic-face-function
	     . lisp-font-lock-syntactic-face-function))))
  (setq truncate-lines t)
  (buffer-disable-undo)
  ;; In debug.el, from 1998 to 2009 this was set to nil, reason stated
  ;; was because of bytecode. Since 2009 it's been set to t, but the
  ;; default is t so I think this isn't necessary.
  ;; (set-buffer-multibyte t)
  (setq-local revert-buffer-function #'backtrace-revert)
  (setq-local filter-buffer-substring-function #'backtrace--filter-visible)
  (setq-local indent-line-function 'lisp-indent-line)
  (setq-local indent-region-function 'lisp-indent-region)
  (add-hook 'xref-backend-functions #'backtrace--xref-backend nil t))

(put 'backtrace-mode 'mode-class 'special)

;;; Backtrace printing

;;;###autoload
(defun backtrace ()
  "Print a trace of Lisp function calls currently active.
Output stream used is value of `standard-output'."
  (princ (backtrace-to-string (backtrace-get-frames 'backtrace)))
  nil)

(defun backtrace-to-string (&optional frames)
  "Format FRAMES, a list of `backtrace-frame' objects, for output.
Return the result as a string.  If FRAMES is nil, use all
function calls currently active."
  (substring-no-properties
   (backtrace--to-string
    (or frames (backtrace-get-frames 'backtrace-to-string)))))

(defun backtrace--to-string (frames)
  (let ((backtrace-fontify nil))
    (with-temp-buffer
      (backtrace-mode)
      (setq backtrace-view '(:show-flags t)
            backtrace-frames frames
            backtrace-print-function #'cl-prin1)
      (backtrace-print)
      (filter-buffer-substring (point-min) (point-max)))))

(provide 'backtrace)

;;; backtrace.el ends here
