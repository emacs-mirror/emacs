;;; kmacro.el --- enhanced keyboard macros

;; Copyright (C) 2002  Free Software Foundation, Inc.

;; Author: Kim F. Storm <storm@cua.dk>
;; Keywords: keyboard convenience

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; The kmacro package is an alternative user interface to emacs'
;; keyboard macro functionality.  This functionality is normally bound
;; to C-x (, C-x ), and C-x e, but these bindings are too hard to
;; type to be really useful for doing small repeated tasks.

;; With kmacro, two function keys are dedicated to keyboard macros,
;; by default F7 and F8.  Personally, I prefer F1 and F2, but those
;; keys already have default bindings.
;;
;; To start defining a keyboard macro, use F7.  To end the macro,
;; use F8, and to call the macro also use F8.  This makes it very
;; easy to repeat a macro immediately after defining it.
;;
;; You can call the macro repeatedly by pressing F8 multiple times, or
;; you can give a numeric prefix argument specifying the number of
;; times to repeat the macro.  Macro execution automatically
;; terminates when point reaches the end of the buffer or if an error
;; is signalled by ringing the bell.

;; When you define a macro with F7/F8, it is automatically added to
;; the head of the "keyboard macro ring", and F8 actually executes the
;; first element of the macro ring.
;;
;; Note: an empty macro is never added to the macro ring.
;;
;; You can execute the second element on the macro ring with C-u F8 or
;; C-x C-k C-l, you can use C-x C-k C-p and C-x C-k C-n to cycle
;; through the macro ring, and you can swap the first and second
;; elements with C-x C-k C-t.  To delete the first element in the
;; macro ring, use C-x C-k C-d.
;;
;;
;; You can also use C-x C-k C-s to start a macro, and C-x C-k C-k to
;; end it; then use C-k to execute it immediately, or C-x C-k C-k to
;; execute it later.
;;
;; In general, immediately after using C-x C-k followed by one of C-k,
;; C-l, C-p, or C-n, you can further cycle the macro ring using C-p or
;; C-n, execute the first or second macro using C-k or C-l, delete
;; the head macro with C-d, or edit the current macro with C-e without
;; repeating the C-x C-k prefix.

;; If you enter F7 while defining the macro, the numeric value of
;; `kmacro-counter' is inserted using the `kmacro-counter-format', and
;; `kmacro-counter' is incremented by 1 (or the numeric prefix value
;; of F7).
;;
;; The initial value of `kmacro-counter' is 0, or the numeric prefix
;; value given to F7 when starting the macro.
;;
;; Now, each time you call the macro using F8, the current
;; value of `kmacro-counter' is inserted and incremented, making it
;; easy to insert incremental numbers in the buffer.
;;
;; Example:
;;
;; The following sequence: M-5 F7 x M-2 F7 y F8 F8 F8 F8
;; inserts the following string:  x5yx7yx9yx11y

;; A macro can also be called using a mouse click, default S-mouse-3.
;; This calls the macro at the point where you click the mouse.

;; You can edit the last macro using C-x C-k C-e.

;; You can append to the last macro using C-u F7.

;; You can set the macro counter using C-x C-k C-c, add to it using C-x C-k C-a,
;; and you can set the macro counter format with C-x C-k C-f.

;; The following key bindings are performed:
;; 
;;           Normal                         While defining macro
;;           ---------------------------    ------------------------------
;;  f7       Define macro                   Insert current counter value
;;           Prefix arg specifies initial   and increase counter by prefix
;;           counter value (default 0)      (default increment: 1)
;;
;;  C-u f7   APPENDs to last macro
;; 
;;  f8       Call last macro                End macro 
;;           Prefix arg specifies number
;;           of times to execute macro.
;;
;;  C-u f8   Swap last and head of macro ring.
;; 
;;  S-mouse-3  Set point at click and       End macro and execute macro at
;;             execute last macro.          click.

;;; Code:

;; Customization:

(defgroup kmacro nil
  "Simplified keyboard macro user interface."
  :group 'keyboard
  :group 'convenience
  :link '(emacs-commentary-link :tag "Commentary" "kmacro.el")
  :link '(emacs-library-link :tag "Lisp File" "kmacro.el"))

(defcustom kmacro-call-mouse-event 'S-mouse-3
  "The mouse event used by kmacro to call a macro.
Set to nil if no mouse binding is desired."
  :type 'symbol
  :group 'kmacro)

(defcustom kmacro-ring-max 8
  "Maximum number of keyboard macros to save in macro ring."
  :type 'integer
  :group 'kmacro)


(defcustom kmacro-execute-before-append t
  "Controls whether appending to a macro starts by executing the macro.
If non-nil, using a single \\[universal-argument] prefix executes the macro
before appending, while more than one \\[universal-argument] prefix does not
execute the macro.
Otherwise, a single \\[universal-argument] prefix does not execute the
macro, while more than one \\[universal-argument] prefix causes the
macro to be executed before appending to it."
  :type 'boolean
  :group 'kmacro)


(defcustom kmacro-repeat-no-prefix t
  "Allow repeating certain macro commands without entering the C-x C-k prefix."
  :type 'boolean
  :group 'kmacro)


;; Keymap

(defvar kmacro-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-s" 'kmacro-start-macro)
    (define-key map "\C-k" 'kmacro-end-or-call-macro-rep)
    (define-key map "\C-e" 'kmacro-edit-macro)
    (define-key map "\r"   'kmacro-edit-macro-nr)
    (define-key map "l"    'kmacro-edit-lossage)
    (define-key map "\C-i" 'kmacro-insert-counter)
    (define-key map "\C-a" 'kmacro-add-counter)
    (define-key map "\C-v" 'kmacro-view-macro-rep)
    (define-key map "\C-l" 'kmacro-call-ring-2nd-rep)
    (define-key map "\C-r" 'kmacro-view-ring-2nd)
    (define-key map "\C-n" 'kmacro-cycle-ring-next)
    (define-key map "\C-p" 'kmacro-cycle-ring-previous)
    (define-key map "\C-f" 'kmacro-set-format)
    (define-key map "\C-c" 'kmacro-set-counter)
    (define-key map "\C-t" 'kmacro-swap-ring)
    (define-key map "\C-b" 'kmacro-bind-to-key)
    (define-key map "\C-d" 'kmacro-delete-ring-head)
    ;; Compatibility bindings
    (define-key map "q"    'kbd-macro-query)
    (define-key map "n"    'name-last-kbd-macro)
    (define-key map "e"    'edit-kbd-macro)
    (define-key map "r"    'apply-macro-to-region-lines)
    map)
  "Keymap for keyboard macro commands.")
(defalias 'kmacro-keymap kmacro-keymap)

;;; Provide some binding for startup:
;;;###autoload (global-set-key "\C-x(" 'kmacro-start-macro)
;;;###autoload (global-set-key "\C-x)" 'kmacro-end-macro)
;;;###autoload (global-set-key "\C-xe" 'kmacro-call-macro)
;;;###autoload (global-set-key [f7] 'kmacro-start-macro-or-insert-counter)
;;;###autoload (global-set-key [f8] 'kmacro-end-or-call-macro)
;;;###autoload (global-set-key "\C-x\C-k" 'kmacro-keymap)
;;;###autoload (autoload 'kmacro-keymap "kmacro" "Keymap for keyboard macro commands." t 'keymap)

(if kmacro-call-mouse-event
  (global-set-key (vector kmacro-call-mouse-event) 'kmacro-end-call-mouse))



;;; Keyboard macro counter

(defvar kmacro-counter 0
  "*Current keyboard macro counter.")

(defvar kmacro-counter-format "%d"
  "*Current keyboard macro counter format.")

(defvar kmacro-counter-format-start kmacro-counter-format
  "Macro format at start of macro execution.")

(defvar kmacro-counter-value-start kmacro-counter
  "Macro counter at start of macro execution.")

(defvar kmacro-last-counter 0 "Last counter inserted by key macro.")


(defun kmacro-insert-counter (arg)
  "Insert macro counter and increment with ARG or 1 if missing.
With \\[universal-argument], insert previous kmacro-counter (but do not modify counter)."
  (interactive "P")
  (if (and arg (listp arg))
      (insert (format kmacro-counter-format kmacro-last-counter))
    (insert (format kmacro-counter-format kmacro-counter))
    (kmacro-add-counter (prefix-numeric-value arg))))


(defun kmacro-set-format (format)
  "Set macro counter FORMAT."
  (interactive "sMacro Counter Format (printf format): ")
  (setq kmacro-counter-format
	(if (equal format "") "%d" format))
  ;; redefine initial macro counter if we are not executing a macro.
  (if (not (or defining-kbd-macro executing-kbd-macro))
      (setq kmacro-counter-format-start kmacro-counter-format)))


(defun kmacro-display-counter (&optional value)
  "Display current counter value."
  (unless value (setq value kmacro-counter))
  (message "New macro counter value: %s (%d)" (format kmacro-counter-format value) value))


(defun kmacro-set-counter (arg)
  "Set kmacro-counter to ARG or prompt if missing.
With \\[universal-argument], reset counter to its value prior to this iteration of the macro."
  (interactive "NMacro counter value: ")
  (setq kmacro-last-counter kmacro-counter
	kmacro-counter (if (and current-prefix-arg (listp current-prefix-arg))
			   kmacro-counter-value-start
			 arg))
  (unless executing-kbd-macro
    (kmacro-display-counter)))


(defun kmacro-add-counter (arg)
  "Add numeric prefix arg (prompt if missing) to macro counter.
With \\[universal-argument], restore previous counter value."
  (interactive "NAdd to macro counter: ")
  (let ((last kmacro-last-counter))
    (setq kmacro-last-counter kmacro-counter
	  kmacro-counter (if (and current-prefix-arg (listp current-prefix-arg))
			     last
			   kmacro-counter (+ kmacro-counter arg))))
  (unless executing-kbd-macro
    (kmacro-display-counter)))


(defun kmacro-loop-setup-function ()
  "Function called prior to each iteration of macro."
  ;; Restore macro counter format to initial format, so it is ok to change
  ;; counter format in the macro without restoring it.
  (setq kmacro-counter-format kmacro-counter-format-start)
  ;; Save initial counter value so we can restore it with C-u kmacro-set-counter.
  (setq kmacro-counter-value-start kmacro-counter)
  ;; Return non-nil to continue execution.
  t)


;;; Keyboard macro ring

(defvar kmacro-ring nil
  "The keyboard macro ring.
Each element is a list (MACRO COUNTER FORMAT).  Actually, the head of
the macro ring (when defining or executing) is not stored in the ring;
instead it is available in the variables `last-kbd-macro', `kmacro-counter',
and `kmacro-counter-format'.")


(defun kmacro-ring-head ()
  "Return pseudo head element in macro ring."
  (and last-kbd-macro
       (list last-kbd-macro kmacro-counter kmacro-counter-format-start)))


(defun kmacro-push-ring (&optional elt)
  "Push ELT or current macro onto `kmacro-ring'."
  (when (setq elt (or elt (kmacro-ring-head)))
    (let ((len (length kmacro-ring)))
      (setq kmacro-ring (cons elt kmacro-ring))
      (if (>= len kmacro-ring-max)
	  (setcdr (nthcdr len kmacro-ring) nil)))))


(defun kmacro-split-ring-element (elt)
  (setq last-kbd-macro (car elt)
	kmacro-counter (nth 1 elt)
	kmacro-counter-format-start (nth 2 elt)))


(defun kmacro-pop-ring1 (&optional raw)
  "Pop head element off macro ring (no check).
Non-nil arg RAW means just return raw first element."
  (prog1 (car kmacro-ring)
    (unless raw
      (kmacro-split-ring-element (car kmacro-ring)))
    (setq kmacro-ring (cdr kmacro-ring))))


(defun kmacro-pop-ring (&optional raw)
  "Pop head element off macro ring.
Non-nil arg RAW means just return raw first element."
  (unless (kmacro-ring-empty-p)
    (kmacro-pop-ring1 raw)))
      

(defun kmacro-ring-length ()
  "Return length of macro ring, including pseudo head."
  (+ (if last-kbd-macro 1 0) (length kmacro-ring)))


(defun kmacro-ring-empty-p (&optional none)
  "Tell user and return t if `last-kbd-macro' is nil or `kmacro-ring' is empty.
Check only `last-kbd-macro' if optional arg NONE is non-nil."
  (while (and (null last-kbd-macro) kmacro-ring)
    (kmacro-pop-ring1))
  (cond
   ((null last-kbd-macro)
    (message "No keyboard macro defined.")
    t)
   ((and (null none) (null kmacro-ring))
    (message "Only one keyboard macro defined.")
    t)
   (t nil)))


(defun kmacro-display (macro &optional trunc descr empty )
  "Display a keyboard MACRO."
  (if macro
      (let* ((x 60)
	     (m (format-kbd-macro macro))
	     (l (length m))
	     (z (and nil trunc (> l x))))
	(message (format "%s: %s%s" (or descr "Macro") 
			 (if z (substring m 0 (1- x)) m) (if z "..." ""))))
    (message (or empty "No keyboard macros defined"))))


(defun kmacro-call-ring-2nd (arg)
  "Execute second keyboard macro at in macro ring."
  (interactive "P")
  (unless (kmacro-ring-empty-p)
    ;; should use counter format specific to the macro on the ring!
    (let ((kmacro-counter (nth 1 (car kmacro-ring)))
	  (kmacro-counter-format-start (nth 2 (car kmacro-ring))))
      (execute-kbd-macro (car (car kmacro-ring)) arg #'kmacro-loop-setup-function)
      (setcar (cdr (car kmacro-ring)) kmacro-counter))))


(defun kmacro-call-ring-2nd-rep (arg)
  "Like `kmacro-call-ring-2nd', but allow repeat without kmacro prefix."
  (interactive "P")
  (kmacro-call-ring-2nd arg)
  (if kmacro-ring
      (kmacro-repeat-loop)))

(put 'kmacro-call-ring-2nd-rep 'kmacro-repeat 'head)


(defun kmacro-view-ring-2nd ()
  "Display the current head of the keyboard macro ring."
  (interactive)
  (unless (kmacro-ring-empty-p)
    (kmacro-display (car (car kmacro-ring)) "2nd macro")))


(defun kmacro-repeat-loop ()
  "Process kmacro commands keys immidiately after cycling the ring."
  (when kmacro-repeat-no-prefix
    (let (cmd done repeat)
      (while (and last-kbd-macro
		  (not done)
		  (setq cmd (lookup-key kmacro-keymap (vector (read-event))))
		  (setq repeat (get cmd 'kmacro-repeat)))
	(clear-this-command-keys t)
	(cond
	 ((eq repeat 'ring)
	  (if kmacro-ring
	      (let ((kmacro-repeat-no-prefix nil))
		(funcall cmd nil))
	    (kmacro-display last-kbd-macro t)))
	 ((eq repeat 'head)
	  (funcall cmd nil))
	 ((eq repeat 'stop)
	  (funcall cmd nil)
	  (setq done t)))
	(setq last-input-event nil)))
    (when last-input-event
      (clear-this-command-keys t)
      (setq unread-command-events (list last-input-event)))))

  
(defun kmacro-cycle-ring-next (&optional arg)
  "Move to next keyboard macro in keyboard macro ring.
Displays the selected macro in the echo area."
  (interactive)
  (unless (kmacro-ring-empty-p)
    (kmacro-push-ring)
    (let* ((len (length kmacro-ring))
	   (tail (nthcdr (- len 2) kmacro-ring))
	   (elt (car (cdr tail))))
      (setcdr tail nil)
      (kmacro-split-ring-element elt))
    (kmacro-display last-kbd-macro t)
    (kmacro-repeat-loop)))

(put 'kmacro-cycle-ring-next 'kmacro-repeat 'ring)


(defun kmacro-cycle-ring-previous (&optional arg)
  "Move to previous keyboard macro in keyboard macro ring.
Displays the selected macro in the echo area."
  (interactive)
  (unless (kmacro-ring-empty-p)
    (let ((cur (kmacro-ring-head)))
      (kmacro-pop-ring1)
      (if kmacro-ring
	  (nconc kmacro-ring (list cur))
	(setq kmacro-ring (list cur))))
    (kmacro-display last-kbd-macro t)
    (kmacro-repeat-loop)))

(put 'kmacro-cycle-ring-previous 'kmacro-repeat 'ring)


(defun kmacro-swap-ring ()
  "Swap first two elements on keyboard macro ring."
  (interactive)
  (unless (kmacro-ring-empty-p)
    (let ((cur (kmacro-ring-head)))
      (kmacro-pop-ring1)
      (kmacro-push-ring cur))
    (kmacro-display last-kbd-macro t)))


(defun kmacro-delete-ring-head (&optional arg)
  "Delete current macro from keyboard macro ring."
  (interactive)
  (unless (kmacro-ring-empty-p t)
    (if (null kmacro-ring)
	(setq last-kbd-macro nil)
      (kmacro-pop-ring))
    (kmacro-display last-kbd-macro t nil "Keyboard macro ring is now empty.")))

(put 'kmacro-delete-ring-head 'kmacro-repeat 'head)

;;; Traditional bindings:

  
;;;###autoload
(defun kmacro-start-macro (arg)
  "Record subsequent keyboard input, defining a keyboard macro.
The commands are recorded even as they are executed.
Use \\[kmacro-end-macro] to finish recording and make the macro available.
Use \\[call-last-kbd-macro] to execute the macro.
Use \\[name-last-kbd-macro] to give it a permanent name.
Non-nil arg (prefix arg) means append to last macro defined;

With \\[universal-argument] prefix, append to last keyboard macro
defined.  Depending on `kmacro-execute-before-append', this may begin
by re-executing the last macro as if you typed it again.

Otherwise, it sets `kmacro-counter' to ARG or 0 if missing before
defining the macro.

Use \\[kmacro-insert-counter] to insert (and increment) the macro counter.
The counter value can be set or modified via \\[kmacro-set-counter] and \\[kmacro-add-counter].
The format of the counter can be modified via \\[kmacro-set-format]."
  (interactive "P")
  (if (or defining-kbd-macro executing-kbd-macro)
      (message "Already defining keyboard macro.")
    (let ((append (and arg (listp arg))))
      (unless append
	(if last-kbd-macro
	    (let ((len (length kmacro-ring)))
	      (setq kmacro-ring 
		    (cons
		     (list last-kbd-macro kmacro-counter kmacro-counter-format-start)
		     kmacro-ring))
	      (if (>= len kmacro-ring-max)
		  (setcdr (nthcdr len kmacro-ring) nil))))
	(setq kmacro-counter (if arg (prefix-numeric-value arg) 0)
	      kmacro-counter-value-start kmacro-counter
	      kmacro-last-counter kmacro-counter
	      kmacro-counter-format-start kmacro-counter-format))

      (start-kbd-macro append 
		       (and append
			    (if kmacro-execute-before-append
				(> (car arg) 4)
			      (= (car arg) 4)))))))


;;;###autoload
(defun kmacro-end-macro (arg)
  "Finish defining a keyboard macro.
The definition was started by \\[kmacro-start-macro].
The macro is now available for use via \\[kmacro-call-macro],
or it can be given a name with \\[name-last-kbd-macro] and then invoked
under that name.

With numeric arg, repeat macro now that many times,
counting the definition just completed as the first repetition.
An argument of zero means repeat until error."
  (interactive "P")
  (end-kbd-macro arg #'kmacro-loop-setup-function)
  (when (and last-kbd-macro (= (length last-kbd-macro) 0))
    (message "Ignore empty macro")
    (kmacro-pop-ring)))


;;;###autoload
(defun kmacro-call-macro (arg)
  "Call the last keyboard macro that you defined with \\[kmacro-start-macro].

A prefix argument serves as a repeat count.  Zero means repeat until error.

To make a macro permanent so you can call it even after
defining others, use M-x name-last-kbd-macro."
  (interactive "p")
  (call-last-kbd-macro arg #'kmacro-loop-setup-function))



;;; Combined function key bindings:

;;;###autoload
(defun kmacro-start-macro-or-insert-counter (arg)
  "Set `kmacro-counter' to ARG or 0 if missing, and `start-kbd-macro'.
With \\[universal-argument], append to current keyboard macro (keep kmacro-counter).

When defining/executing macro, insert macro counter and increment with 
ARG or 1 if missing.
With \\[universal-argument], insert previous kmacro-counter (but do not modify counter).

The macro counter can be modified via \\[kmacro-set-counter] and \\[kmacro-add-counter].
The format of the counter can be modified via \\[kmacro-set-format]."
  (interactive "P")
  (if (or defining-kbd-macro executing-kbd-macro)
      (kmacro-insert-counter arg)
    (kmacro-start-macro arg)))


;;;###autoload
(defun kmacro-end-or-call-macro (arg)
  "End kbd macro if currently being defined; else call last kbd macro.
With numeric prefix ARG, repeat macro that many times.
With \\[universal-argument], call second macro in macro ring."
  (interactive "P")
  (cond 
   (defining-kbd-macro
     (kmacro-end-macro arg))
   ((and arg (listp arg))
    (kmacro-call-ring-2nd 1))
   (t
    (kmacro-call-macro arg))))


(defun kmacro-end-or-call-macro-rep (arg)
  "As `kmacro-end-or-call-macro' but allows repeat without kmacro prefix."
  (interactive "P")
  (kmacro-end-or-call-macro arg)
  (kmacro-repeat-loop))

(put 'kmacro-end-or-call-macro-rep 'kmacro-repeat 'head)


;;;###autoload
(defun kmacro-end-call-mouse (event)
  "Move point to the position clicked with the mouse and call last kbd macro.
If kbd macro currently being defined end it before activating it."
  (interactive "e")
  (when defining-kbd-macro
    (end-kbd-macro))
  (mouse-set-point event)
  (kmacro-call-macro nil))


;;; Misc. commands

(defun kmacro-bind-to-key (arg)
  "When not defining or executing a macro, offer to bind last macro to a key."
  (interactive "p")
  (if (or defining-kbd-macro executing-kbd-macro)
      (if defining-kbd-macro
	  (message "Cannot save macro while defining it."))
    (unless last-kbd-macro
      (error "No keyboard macro defined"))
    (let ((key-seq (read-key-sequence "Bind last macro to key: ")))
      (unless (equal key-seq "")
	(define-key global-map key-seq last-kbd-macro)))))


(defun kmacro-view-macro (&optional arg)
  "Display the last keyboard macro."
  (interactive)
  (kmacro-display last-kbd-macro))


(defun kmacro-view-macro-rep (&optional arg)
  "Like `kmacro-view-macro', but allow repeat without kmacro prefix."
  (interactive)
  (kmacro-view-macro arg)
  (if last-kbd-macro
      (kmacro-repeat-loop)))

(put 'kmacro-view-macro-rep 'kmacro-repeat 'head)

(defun kmacro-edit-macro (&optional arg)
  "Edit last keyboard macro."
  (interactive "P")
  (edit-kbd-macro "\r" arg))

(put 'kmacro-edit-macro 'kmacro-repeat 'stop)


(defun kmacro-edit-macro-nr (&optional arg)
  "As edit last keyboard macro, but without kmacro-repeat property."
  (interactive "P")
  (kmacro-edit-macro arg))


(defun kmacro-edit-lossage ()
  "Edit most recent 100 keystrokes as a keyboard macro."
  (interactive)
  (kmacro-push-ring)
  (edit-kbd-macro "\C-hl"))


(provide 'kmacro)
;;; kmacro.el ends here
