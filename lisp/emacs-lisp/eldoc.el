;;; eldoc.el --- Show function arglist or variable docstring in echo area  -*- lexical-binding:t; -*-

;; Copyright (C) 1996-2017 Free Software Foundation, Inc.

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com
;; Keywords: extensions
;; Created: 1995-10-06

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

;; This program was inspired by the behavior of the "mouse documentation
;; window" on many Lisp Machine systems; as you type a function's symbol
;; name as part of a sexp, it will print the argument list for that
;; function.  Behavior is not identical; for example, you need not actually
;; type the function name, you need only move point around in a sexp that
;; calls it.  Also, if point is over a documented variable, it will print
;; the one-line documentation for that variable instead, to remind you of
;; that variable's meaning.

;; One useful way to enable this minor mode is to put the following in your
;; .emacs:
;;
;;      (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
;;      (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
;;      (add-hook 'ielm-mode-hook 'eldoc-mode)
;;      (add-hook 'eval-expression-minibuffer-setup-hook 'eldoc-mode)

;; Major modes for other languages may use ElDoc by defining an
;; appropriate function as the buffer-local value of
;; `eldoc-documentation-function'.

;;; Code:

(defgroup eldoc nil
  "Show function arglist or variable docstring in echo area."
  :group 'lisp
  :group 'extensions)

(defcustom eldoc-idle-delay 0.50
  "Number of seconds of idle time to wait before printing.
If user input arrives before this interval of time has elapsed after the
last input, no documentation will be printed.

If this variable is set to 0, no idle time is required."
  :type 'number
  :group 'eldoc)

(defcustom eldoc-print-after-edit nil
  "If non-nil eldoc info is only shown when editing.
Changing the value requires toggling `eldoc-mode'."
  :type 'boolean
  :group 'eldoc)

;;;###autoload
(defcustom eldoc-minor-mode-string (purecopy " ElDoc")
  "String to display in mode line when ElDoc Mode is enabled; nil for none."
  :type '(choice string (const :tag "None" nil))
  :group 'eldoc)

(defcustom eldoc-argument-case #'identity
  "Case to display argument names of functions, as a symbol.
This has two preferred values: `upcase' or `downcase'.
Actually, any name of a function which takes a string as an argument and
returns another string is acceptable.

Note that this variable has no effect, unless
`eldoc-documentation-function' handles it explicitly."
  :type '(radio (function-item upcase)
		(function-item downcase)
                function)
  :group 'eldoc)
(make-obsolete-variable 'eldoc-argument-case nil "25.1")

(defcustom eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit
  "Allow long ElDoc messages to resize echo area display.
If value is t, never attempt to truncate messages; complete symbol name
and function arglist or 1-line variable documentation will be displayed
even if echo area must be resized to fit.

If value is any non-nil value other than t, symbol name may be truncated
if it will enable the function arglist or documentation string to fit on a
single line without resizing window.  Otherwise, behavior is just like
former case.

If value is nil, messages are always truncated to fit in a single line of
display in the echo area.  Function or variable symbol name may be
truncated to make more of the arglist or documentation string visible.

Note that this variable has no effect, unless
`eldoc-documentation-function' handles it explicitly."
  :type '(radio (const :tag "Always" t)
                (const :tag "Never" nil)
                (const :tag "Yes, but truncate symbol names if it will\
 enable argument list to fit on one line" truncate-sym-name-if-fit))
  :group 'eldoc)

(defface eldoc-highlight-function-argument
  '((t (:inherit bold)))
  "Face used for the argument at point in a function's argument list.
Note that this face has no effect unless the `eldoc-documentation-function'
handles it explicitly."
  :group 'eldoc)

;;; No user options below here.

(defvar eldoc-message-commands-table-size 31
  "Used by `eldoc-add-command' to initialize `eldoc-message-commands' obarray.
It should probably never be necessary to do so, but if you
choose to increase the number of buckets, you must do so before loading
this file since the obarray is initialized at load time.
Remember to keep it a prime number to improve hash performance.")

(defvar eldoc-message-commands
  ;; Don't define as `defconst' since it would then go to (read-only) purespace.
  (make-vector eldoc-message-commands-table-size 0)
  "Commands after which it is appropriate to print in the echo area.
ElDoc does not try to print function arglists, etc., after just any command,
because some commands print their own messages in the echo area and these
functions would instantly overwrite them.  But `self-insert-command' as well
as most motion commands are good candidates.
This variable contains an obarray of symbols; do not manipulate it
directly.  Instead, use `eldoc-add-command' and `eldoc-remove-command'.")

;; Not a constant.
(defvar eldoc-last-data (make-vector 3 nil)
  ;; Don't define as `defconst' since it would then go to (read-only) purespace.
  "Bookkeeping; elements are as follows:
  0 - contains the last symbol read from the buffer.
  1 - contains the string last displayed in the echo area for variables,
      or argument string for functions.
  2 - `function' if function args, `variable' if variable documentation.")
(make-obsolete-variable 'eldoc-last-data "use your own instead" "25.1")

(defvar eldoc-last-message nil)

(defvar eldoc-timer nil "ElDoc's timer object.")

(defvar eldoc-current-idle-delay eldoc-idle-delay
  "Idle time delay currently in use by timer.
This is used to determine if `eldoc-idle-delay' is changed by the user.")

(defvar eldoc-message-function #'eldoc-minibuffer-message
  "The function used by `eldoc-message' to display messages.
It should receive the same arguments as `message'.")

(defun eldoc-edit-message-commands ()
  "Return an obarray containing common editing commands.

When `eldoc-print-after-edit' is non-nil, ElDoc messages are only
printed after commands contained in this obarray."
  (let ((cmds (make-vector 31 0))
	(re (regexp-opt '("delete" "insert" "edit" "electric" "newline"))))
    (mapatoms (lambda (s)
		(and (commandp s)
		     (string-match-p re (symbol-name s))
		     (intern (symbol-name s) cmds)))
	      obarray)
    cmds))


;;;###autoload
(define-minor-mode eldoc-mode
  "Toggle echo area display of Lisp objects at point (ElDoc mode).
With a prefix argument ARG, enable ElDoc mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable ElDoc mode
if ARG is omitted or nil.

ElDoc mode is a buffer-local minor mode.  When enabled, the echo
area displays information about a function or variable in the
text where point is.  If point is on a documented variable, it
displays the first line of that variable's doc string.  Otherwise
it displays the argument list of the function called in the
expression point is on."
  :group 'eldoc :lighter eldoc-minor-mode-string
  (setq eldoc-last-message nil)
  (cond
   ((not (eldoc--supported-p))
    (when (called-interactively-p 'any)
      (message "There is no ElDoc support in this buffer"))
    (setq eldoc-mode nil))
   (eldoc-mode
    (when eldoc-print-after-edit
      (setq-local eldoc-message-commands (eldoc-edit-message-commands)))
    (add-hook 'post-command-hook 'eldoc-schedule-timer nil t)
    (add-hook 'pre-command-hook 'eldoc-pre-command-refresh-echo-area nil t))
   (t
    (kill-local-variable 'eldoc-message-commands)
    (remove-hook 'post-command-hook 'eldoc-schedule-timer t)
    (remove-hook 'pre-command-hook 'eldoc-pre-command-refresh-echo-area t)
    (when eldoc-timer
      (cancel-timer eldoc-timer)
      (setq eldoc-timer nil)))))

;;;###autoload
(define-globalized-minor-mode global-eldoc-mode eldoc-mode turn-on-eldoc-mode
  :group 'eldoc
  :initialize 'custom-initialize-delay
  :init-value t)

;;;###autoload
(defun turn-on-eldoc-mode ()
  "Turn on `eldoc-mode' if the buffer has ElDoc support enabled.
See `eldoc-documentation-function' for more detail."
  (when (eldoc--supported-p)
    (eldoc-mode 1)))

(defun eldoc--supported-p ()
  "Non-nil if an ElDoc function is set for this buffer."
  (not (memq eldoc-documentation-function '(nil ignore))))


(defun eldoc-schedule-timer ()
  "Ensure `eldoc-timer' is running.

If the user has changed `eldoc-idle-delay', update the timer to
reflect the change."
  (or (and eldoc-timer
           (memq eldoc-timer timer-idle-list)) ;FIXME: Why?
      (setq eldoc-timer
            (run-with-idle-timer
	     eldoc-idle-delay nil
	     (lambda ()
               (when (or eldoc-mode
                         (and global-eldoc-mode
                              (eldoc--supported-p)))
                 (eldoc-print-current-symbol-info))))))

  ;; If user has changed the idle delay, update the timer.
  (cond ((not (= eldoc-idle-delay eldoc-current-idle-delay))
         (setq eldoc-current-idle-delay eldoc-idle-delay)
         (timer-set-idle-time eldoc-timer eldoc-idle-delay t))))

(defvar eldoc-mode-line-string nil)
(put 'eldoc-mode-line-string 'risky-local-variable t)

(defun eldoc-minibuffer-message (format-string &rest args)
  "Display messages in the mode-line when in the minibuffer.
Otherwise work like `message'."
  (if (minibufferp)
      (progn
	(add-hook 'minibuffer-exit-hook
		  (lambda () (setq eldoc-mode-line-string nil
			      ;; https://debbugs.gnu.org/16920
			      eldoc-last-message nil))
		  nil t)
	(with-current-buffer
	    (window-buffer
	     (or (window-in-direction 'above (minibuffer-window))
		 (minibuffer-selected-window)
		 (get-largest-window)))
    (when mode-line-format
	  (unless (and (listp mode-line-format)
		       (assq 'eldoc-mode-line-string mode-line-format))
	    (setq mode-line-format
		  (list "" '(eldoc-mode-line-string
			     (" " eldoc-mode-line-string " "))
			mode-line-format))))
          (setq eldoc-mode-line-string
                (when (stringp format-string)
                  (apply #'format-message format-string args)))
          (force-mode-line-update)))
    (apply 'message format-string args)))

(defun eldoc-message (&optional string)
  "Display STRING as an ElDoc message if it's non-nil.

Also store it in `eldoc-last-message' and return that value."
  (let ((omessage eldoc-last-message))
    (setq eldoc-last-message string)
    ;; In emacs 19.29 and later, and XEmacs 19.13 and later, all messages
    ;; are recorded in a log.  Do not put eldoc messages in that log since
    ;; they are Legion.
    ;; Emacs way of preventing log messages.
    (let ((message-log-max nil))
      (cond (eldoc-last-message
	     (funcall eldoc-message-function "%s" eldoc-last-message))
	    (omessage (funcall eldoc-message-function nil)))))
  eldoc-last-message)

(defun eldoc--message-command-p (command)
  "Return non-nil if COMMAND is in `eldoc-message-commands'."
  (and (symbolp command)
       (intern-soft (symbol-name command) eldoc-message-commands)))

;; This function goes on pre-command-hook for XEmacs or when using idle
;; timers in Emacs.  Motion commands clear the echo area for some reason,
;; which make eldoc messages flicker or disappear just before motion
;; begins.  This function reprints the last eldoc message immediately
;; before the next command executes, which does away with the flicker.
;; This doesn't seem to be required for Emacs 19.28 and earlier.
(defun eldoc-pre-command-refresh-echo-area ()
  "Reprint `eldoc-last-message' in the echo area."
  (and eldoc-last-message
       (not (minibufferp))      ;We don't use the echo area when in minibuffer.
       (if (and (eldoc-display-message-no-interference-p)
		(eldoc--message-command-p this-command))
	   (eldoc-message eldoc-last-message)
         ;; No need to call eldoc-message since the echo area will be cleared
         ;; for us, but do note that the last-message will be gone.
         (setq eldoc-last-message nil))))

;; Decide whether now is a good time to display a message.
(defun eldoc-display-message-p ()
  "Return non-nil when it is appropriate to display an ElDoc message."
  (and (eldoc-display-message-no-interference-p)
       ;; If this-command is non-nil while running via an idle
       ;; timer, we're still in the middle of executing a command,
       ;; e.g. a query-replace where it would be annoying to
       ;; overwrite the echo area.
       (not this-command)
       (eldoc--message-command-p last-command)))


;; Check various conditions about the current environment that might make
;; it undesirable to print eldoc messages right this instant.
(defun eldoc-display-message-no-interference-p ()
  "Return nil if displaying a message would cause interference."
  (not (or executing-kbd-macro (bound-and-true-p edebug-active))))


;;;###autoload
(defvar eldoc-documentation-function #'ignore
  "Function to call to return doc string.
The function of no args should return a one-line string for displaying
doc about a function etc. appropriate to the context around point.
It should return nil if there's no doc appropriate for the context.
Typically doc is returned if point is on a function-like name or in its
arg list.

The result is used as is, so the function must explicitly handle
the variables `eldoc-argument-case' and `eldoc-echo-area-use-multiline-p',
and the face `eldoc-highlight-function-argument', if they are to have any
effect.

Major modes should modify this variable using `add-function', for example:
  (add-function :before-until (local \\='eldoc-documentation-function)
                #\\='foo-mode-eldoc-function)
so that the global documentation function (i.e. the default value of the
variable) is taken into account if the major mode specific function does not
return any documentation.")

(defun eldoc-print-current-symbol-info ()
  "Print the text produced by `eldoc-documentation-function'."
  ;; This is run from post-command-hook or some idle timer thing,
  ;; so we need to be careful that errors aren't ignored.
  (with-demoted-errors "eldoc error: %s"
    (and (or (eldoc-display-message-p)
             ;; Erase the last message if we won't display a new one.
             (when eldoc-last-message
               (eldoc-message nil)
               nil))
	 (eldoc-message (funcall eldoc-documentation-function)))))

;; If the entire line cannot fit in the echo area, the symbol name may be
;; truncated or eliminated entirely from the output to make room for the
;; description.
(defun eldoc-docstring-format-sym-doc (prefix doc &optional face)
  "Combine PREFIX and DOC, and shorten the result to fit in the echo area.

When PREFIX is a symbol, propertize its symbol name with FACE
before combining it with DOC.  If FACE is not provided, just
apply the nil face.

See also: `eldoc-echo-area-use-multiline-p'."
  (when (symbolp prefix)
    (setq prefix (concat (propertize (symbol-name prefix) 'face face) ": ")))
  (let* ((ea-multi eldoc-echo-area-use-multiline-p)
         ;; Subtract 1 from window width since emacs will not write
         ;; any chars to the last column, or in later versions, will
         ;; cause a wraparound and resize of the echo area.
         (ea-width (1- (window-width (minibuffer-window))))
         (strip (- (+ (length prefix) (length doc)) ea-width)))
    (cond ((or (<= strip 0)
               (eq ea-multi t)
               (and ea-multi (> (length doc) ea-width)))
           (concat prefix doc))
          ((> (length doc) ea-width)
           (substring (format "%s" doc) 0 ea-width))
          ((>= strip (string-match-p ":? *\\'" prefix))
           doc)
          (t
           ;; Show the end of the partial symbol name, rather
           ;; than the beginning, since the former is more likely
           ;; to be unique given package namespace conventions.
           (concat (substring prefix strip) doc)))))

;; When point is in a sexp, the function args are not reprinted in the echo
;; area after every possible interactive command because some of them print
;; their own messages in the echo area; the eldoc functions would instantly
;; overwrite them unless it is more restrained.
;; These functions do display-command table management.

(defun eldoc-add-command (&rest cmds)
  "Add each of CMDS to the obarray `eldoc-message-commands'."
  (dolist (name cmds)
    (and (symbolp name)
         (setq name (symbol-name name)))
    (set (intern name eldoc-message-commands) t)))

(defun eldoc-add-command-completions (&rest names)
  "Pass every prefix completion of NAMES to `eldoc-add-command'."
  (dolist (name names)
    (apply #'eldoc-add-command (all-completions name obarray 'commandp))))

(defun eldoc-remove-command (&rest cmds)
  "Remove each of CMDS from the obarray `eldoc-message-commands'."
  (dolist (name cmds)
    (and (symbolp name)
         (setq name (symbol-name name)))
    (unintern name eldoc-message-commands)))

(defun eldoc-remove-command-completions (&rest names)
  "Pass every prefix completion of NAMES to `eldoc-remove-command'."
  (dolist (name names)
    (apply #'eldoc-remove-command
           (all-completions name eldoc-message-commands))))


;; Prime the command list.
(eldoc-add-command-completions
 "back-to-indentation"
 "backward-" "beginning-of-" "delete-other-windows" "delete-window"
 "down-list" "end-of-" "exchange-point-and-mark" "forward-" "goto-"
 "handle-select-window" "indent-for-tab-command" "left-" "mark-page"
 "mark-paragraph" "mouse-set-point" "move-" "move-beginning-of-"
 "move-end-of-" "newline" "next-" "other-window" "pop-global-mark"
 "previous-" "recenter" "right-" "scroll-" "self-insert-command"
 "split-window-" "up-list")

(provide 'eldoc)

;;; eldoc.el ends here
