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

;; When you have defined another macro, which is thus called via F8,
;; the previous macro is pushed onto a keyboard macro ring.  The head
;; macro on the ring can be executed using S-F8.  You can cycle the
;; macro ring using C-F8.  You can also swap the last macro and the
;; head of the macro ring using C-u F8.

;; You can edit the last macro using M-F7.

;; You can append to the last macro using C-u F7.

;; You can set the macro counter using C-F7, and you can set
;; the macro counter format with S-F7..

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
;;  S-f7     Set the format of the macro	  Ditto, but notice that the
;;           counter (default: %d).         format is reset at the next
;;                                          invocation of the macro.
;; 
;;  C-f7     Set the macro counter value    Increase/decrease counter value
;;           to the prefix value.           by the prefix value, or if prefix
;;                                          is C-u, set counter to 0.
;; 
;;  M-f7     Edit the last macro.
;; 
;;  S-f8     Call the previous macro.
;; 
;;  C-f8     Cycle the macro ring.
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

;;;###autoload
(defcustom kmacro-initialize nil
  "Setting this variable turns on the kmacro functionality.
This binds the kmacro function keys in the `global-map', so
unsetting this variable does not have any effect!"
  :set #'(lambda (symbol value)
	   (if value (kmacro-initialize))
	   (set symbol value))
  :initialize 'custom-initialize-default
  :require 'kmacro
  :link '(emacs-commentary-link "kmacro.el")
  :set-after '(kmacro-start-key kmacro-call-key kmacro-mouse-button)
  :version "21.4"
  :type 'boolean
  :group 'kmacro)

(defcustom kmacro-start-key 'f7
  "The function key used by kmacro to start a macro."
  :type 'symbol
  :group 'kmacro)

(defcustom kmacro-call-key 'f8
  "The function key used by kmacro to end and call a macro."
  :type 'symbol
  :group 'kmacro)

(defcustom kmacro-call-mouse-event 'S-mouse-3
  "The mouse event used by kmacro to call a macro."
  :type 'symbol
  :group 'kmacro)

;; State variables

(defvar kmacro-counter 0
  "*Current keyboard macro counter.")

(defvar kmacro-counter-format "%d"
  "*Current keyboard macro counter format.")

(defvar kmacro-counter-format-start kmacro-counter-format
  "Macro format at start of macro execution.")

(defvar kmacro-last-counter 0 "Last counter inserted by key macro.")
(defvar kmacro-append-to nil "Last key macro if appending to macro.")
(defvar kmacro-ring nil "Key macro ring.")

(defvar kmacro-ring-max 4
  "*Maximum number of key macros to save in key macro ring.")

(defun kmacro-display (macro)
  "Display a keyboard MACRO."
  (let (s)
    (if (stringp macro)
	(setq s (if (> (length macro) 50)
		    (concat (substring macro 0 50) "...")
		  macro))
      (if (vectorp macro)
	  (let (v (i 0) (n (length macro)))
	    (setq s "")
	    (while (and (< i n) (< (length s) 50))
	      (setq v (aref macro i))
	      (setq s (cond 
		       ((numberp v) (concat s (char-to-string v)))
		       ((stringp v) (concat s v))
		       ((symbolp v) (concat s "[" (symbol-name v) "]"))
		       (t s)))
	      (setq i (1+ i)))
	    (if (< i n)
		(setq s (concat s "..."))))))
    (message (format "Macro: %s" s))))


(defun kmacro-start-macro (arg)
  "Set `kmacro-counter' to ARG or 0 if missing, and `start-kbd-macro'.
With \\[universal-argument], append to current keyboard macro (keep kmacro-counter).

When defining/executing macro, insert macro counter and increment with 
ARG or 1 if missing.
With \\[universal-argument], insert previous kmacro-counter (but do not modify counter).

The macro counter can be modified via \\[kmacro-set-counter].
The format of the counter can be modified via \\[kmacro-set-format]."
  (interactive "p")
  (if (or defining-kbd-macro executing-kbd-macro)
      (if (and current-prefix-arg (listp current-prefix-arg))
	  (insert (format kmacro-counter-format kmacro-last-counter))
	(insert (format kmacro-counter-format kmacro-counter))
	(setq kmacro-last-counter kmacro-counter
	      kmacro-counter (+ kmacro-counter arg)))
    (if (and current-prefix-arg (listp current-prefix-arg))
	(setq kmacro-append-to last-kbd-macro)
      (setq kmacro-append-to nil
	    kmacro-counter (if current-prefix-arg arg 0)
	    kmacro-last-counter kmacro-counter))
    (if last-kbd-macro
	(let ((len (length kmacro-ring)))
	  (setq kmacro-ring (cons last-kbd-macro kmacro-ring))
	  (if (>= len kmacro-ring-max)
	      (setcdr (nthcdr len kmacro-ring) nil))))
    (setq kmacro-counter-format-start kmacro-counter-format)
    (start-kbd-macro nil)
    (if kmacro-append-to (message "Appending to keyboard macro..."))
))

(defun kmacro-call-macro (arg)
  "End kbd macro if currently being defined; else call last kbd macro.
With numeric prefix ARG, repeat macro that many times.
With \\[universal-argument], swap current macro with head of macro ring."
  (interactive "p")
  (cond 
   (defining-kbd-macro
     (end-kbd-macro)
     (if kmacro-append-to
	 (setq last-kbd-macro (concat kmacro-append-to last-kbd-macro)
	       kmacro-append-to nil)))
   ((and current-prefix-arg (listp current-prefix-arg))
    (when kmacro-ring
      (let ((head (car kmacro-ring)))
	(setq kmacro-ring (cons last-kbd-macro (cdr kmacro-ring)))
	(setq last-kbd-macro head)))
    (kmacro-display last-kbd-macro))
   (t
    (setq kmacro-counter-format kmacro-counter-format-start)
    (call-last-kbd-macro arg))))

(defun kmacro-call-macro-ring (arg)
  "End kbd macro if currently being defined; else call last kbd macro.
With \\[universal-argument], display current macro."
  (interactive "p")
  (if kmacro-ring
      (execute-kbd-macro (car kmacro-ring) arg)))

(defun kmacro-end-call-mouse (event)
  "Move point to the position clicked with the mouse and call last kbd macro.
If kbd macro currently being defined end it before activating it."
  (interactive "e")
  (when defining-kbd-macro
    (end-kbd-macro)
    (if kmacro-append-to
	(setq last-kbd-macro (concat kmacro-append-to last-kbd-macro)
	      kmacro-append-to nil)))
  (mouse-set-point event)
  (call-last-kbd-macro nil))

(defun kmacro-cycle-macro-ring (&optional previous)
  "Cycle the keyboard macro ring on \\[kmacro-call-macro-ring].
Moves to the next element in the keyboard macro ring.
With \\[universal-argument] prefix, move to the previous element in the ring.
Displays the selected macro in the echo area."
  (interactive "p")
  (if (null kmacro-ring)
      (message "No keymacros in ring")
    (cond
     ((not (eq this-command last-command))
      nil)
     ((= (length kmacro-ring) 1)
      nil)
     (previous
      (let* ((len (length kmacro-ring))
	     (tail (nthcdr (- len 2) kmacro-ring))
	     (elt (car (cdr tail))))
	(setcdr tail nil)
	(setq kmacro-ring (cons elt kmacro-ring))))
     (t
      (let ((elt (car kmacro-ring)))
	(setq kmacro-ring (cdr kmacro-ring))
	(nconc kmacro-ring (list elt)))))
    (kmacro-display (car kmacro-ring))))

(defun kmacro-save-macro-on-key (arg)
  "When not defining or executing a macro, offer to save last macro on a key."
  (interactive "p")
  (if (or defining-kbd-macro executing-kbd-macro)
      nil
    (or last-kbd-macro
	(error "No keyboard macro defined"))
    (let ((key-seq (read-key-sequence "Save last macro on key: ")))
      (or (equal key-seq "")
	  (define-key global-map key-seq last-kbd-macro))))
)

(defun kmacro-set-counter (arg)
  "Set kmacro-counter to ARG or 0 if missing.
While defining/executing key macro, increase or decrease counter.
With \\[universal-argument], unconditionally set counter to 0."
  (interactive "p")
  (setq kmacro-counter
	(cond ((and current-prefix-arg (listp current-prefix-arg)) 0)
	      ((or defining-kbd-macro executing-kbd-macro) (+ kmacro-counter arg))
	      (current-prefix-arg arg)
	      (t 0))))

(defun kmacro-set-format (format)
  "Set macro counter FORMAT."
  (interactive "sMacro Counter Format (printf format): ")
  (setq kmacro-counter-format
	(if (equal format "")
	    "%d"
	  format))

  ;; redefine initial macro counter if we are not executing a macro.
  (if (not (or defining-kbd-macro executing-kbd-macro))
      (setq kmacro-counter-format-start kmacro-counter-format))
)

(defun kmacro-edit-macro ()
  "Edit keyboard macro."
  (interactive)
  (edit-kbd-macro "\r"))

;;;###autoload
(defun kmacro-initialize (&optional start-key call-key call-mouse)
  "Setup key bindings for the keyboard macro package.
If specified, use keys START-KEY, CALL-KEY, and CALL-MOUSE.
Don't bind to any mouse event if CALL-MOUSE is t.
Otherwise, use customized keys."

  (setq start-key  (or start-key kmacro-start-key 'f7))
  (setq call-key   (or call-key  kmacro-call-key  'f8))
  (setq call-mouse (or call-mouse kmacro-call-mouse-event 'S-mouse-3))

  (global-set-key (vector start-key)			'kmacro-start-macro)
  (global-set-key (vector (list 'shift start-key))	'kmacro-set-format)
  (global-set-key (vector (list 'control start-key))	'kmacro-set-counter)
  (global-set-key (vector (list 'meta start-key))	'kmacro-edit-macro)

  (global-set-key (vector call-key)			'kmacro-call-macro)
  (global-set-key (vector (list 'shift call-key))	'kmacro-call-macro-ring)
  (global-set-key (vector (list 'control call-key))	'kmacro-cycle-macro-ring)

  (unless (eq call-mouse t)
    (global-set-key (vector call-mouse)		'kmacro-end-call-mouse)))

(provide 'kmacro)
;;; kmacro.el ends here
