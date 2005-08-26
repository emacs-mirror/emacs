;;; eldoc.el --- show function arglist or variable docstring in echo area

;; Copyright (C) 1996, 1997, 1998, 1999, 2000, 2003, 2005
;;   Free Software Foundation, Inc.

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com
;; Keywords: extensions
;; Created: 1995-10-06

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

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
;;      (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
;;      (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
;;      (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; Major modes for other languages may use Eldoc by defining an
;; appropriate function as the buffer-local value of
;; `eldoc-documentation-function'.

;;; Code:

(require 'help-fns)		       ;For fundoc-usage handling functions.

(defgroup eldoc nil
  "Show function arglist or variable docstring in echo area."
  :group 'lisp
  :group 'extensions)

(defcustom eldoc-idle-delay 0.50
  "*Number of seconds of idle time to wait before printing.
If user input arrives before this interval of time has elapsed after the
last input, no documentation will be printed.

If this variable is set to 0, no idle time is required."
  :type 'number
  :group 'eldoc)

;;;###autoload
(defcustom eldoc-minor-mode-string " ElDoc"
  "*String to display in mode line when Eldoc Mode is enabled; nil for none."
  :type '(choice string (const :tag "None" nil))
  :group 'eldoc)

(defcustom eldoc-argument-case 'upcase
  "Case to display argument names of functions, as a symbol.
This has two preferred values: `upcase' or `downcase'.
Actually, any name of a function which takes a string as an argument and
returns another string is acceptable."
  :type '(radio (function-item upcase)
		(function-item downcase)
                function)
  :group 'eldoc)

(defcustom eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit
  "*Allow long eldoc messages to resize echo area display.
If value is t, never attempt to truncate messages; complete symbol name
and function arglist or 1-line variable documentation will be displayed
even if echo area must be resized to fit.

If value is any non-nil value other than t, symbol name may be truncated
if it will enable the function arglist or documentation string to fit on a
single line without resizing window.  Otherwise, behavior is just like
former case.

If value is nil, messages are always truncated to fit in a single line of
display in the echo area.  Function or variable symbol name may be
truncated to make more of the arglist or documentation string visible."
  :type '(radio (const :tag "Always" t)
                (const :tag "Never" nil)
                (const :tag "Yes, but truncate symbol names if it will\
 enable argument list to fit on one line" truncate-sym-name-if-fit))
  :group 'eldoc)

;;; No user options below here.

;; Commands after which it is appropriate to print in the echo area.
;; Eldoc does not try to print function arglists, etc. after just any command,
;; because some commands print their own messages in the echo area and these
;; functions would instantly overwrite them.  But self-insert-command as well
;; as most motion commands are good candidates.
;; This variable contains an obarray of symbols; do not manipulate it
;; directly.  Instead, use `eldoc-add-command' and `eldoc-remove-command'.
(defvar eldoc-message-commands nil)

;; This is used by eldoc-add-command to initialize eldoc-message-commands
;; as an obarray.
;; It should probably never be necessary to do so, but if you
;; choose to increase the number of buckets, you must do so before loading
;; this file since the obarray is initialized at load time.
;; Remember to keep it a prime number to improve hash performance.
(defvar eldoc-message-commands-table-size 31)

;; Bookkeeping; elements are as follows:
;;   0 - contains the last symbol read from the buffer.
;;   1 - contains the string last displayed in the echo area for that
;;       symbol, so it can be printed again if necessary without reconsing.
;;   2 - 'function if function args, 'variable if variable documentation.
(defvar eldoc-last-data (make-vector 3 nil))
(defvar eldoc-last-message nil)

;; eldoc's timer object.
(defvar eldoc-timer nil)

;; idle time delay currently in use by timer.
;; This is used to determine if eldoc-idle-delay is changed by the user.
(defvar eldoc-current-idle-delay eldoc-idle-delay)


;;;###autoload
(define-minor-mode eldoc-mode
  "Toggle ElDoc mode on or off.
In ElDoc mode, the echo area displays information about a
function or variable in the text where point is.  If point is
on a documented variable, it displays the first line of that
variable's doc string.  Otherwise it displays the argument list
of the function called in the expression point is on.

With prefix ARG, turn ElDoc mode on if and only if ARG is positive."
  :group 'eldoc :lighter eldoc-minor-mode-string
  (setq eldoc-last-message nil)
  (if eldoc-mode
      (progn
	(add-hook 'post-command-hook 'eldoc-schedule-timer nil t)
	(add-hook 'pre-command-hook 'eldoc-pre-command-refresh-echo-area t))
   (remove-hook 'post-command-hook 'eldoc-schedule-timer)
   (remove-hook 'pre-command-hook 'eldoc-pre-command-refresh-echo-area)))

;;;###autoload
(defun turn-on-eldoc-mode ()
  "Unequivocally turn on eldoc-mode (see variable documentation)."
  (interactive)
  (eldoc-mode 1))


(defun eldoc-schedule-timer ()
  (or (and eldoc-timer
           (memq eldoc-timer timer-idle-list))
      (setq eldoc-timer
            (run-with-idle-timer eldoc-idle-delay t
                                 'eldoc-print-current-symbol-info)))

  ;; If user has changed the idle delay, update the timer.
  (cond ((not (= eldoc-idle-delay eldoc-current-idle-delay))
         (setq eldoc-current-idle-delay eldoc-idle-delay)
         (timer-set-idle-time eldoc-timer eldoc-idle-delay t))))

(defun eldoc-message (&rest args)
  (let ((omessage eldoc-last-message))
    (setq eldoc-last-message
	  (cond ((eq (car args) eldoc-last-message) eldoc-last-message)
		((null (car args)) nil)
		;; If only one arg, no formatting to do, so put it in
		;; eldoc-last-message so eq test above might succeed on
		;; subsequent calls.
		((null (cdr args)) (car args))
		(t (apply 'format args))))
    ;; In emacs 19.29 and later, and XEmacs 19.13 and later, all messages
    ;; are recorded in a log.  Do not put eldoc messages in that log since
    ;; they are Legion.
    ;; Emacs way of preventing log messages.
    (let ((message-log-max nil))
      (cond (eldoc-last-message (message "%s" eldoc-last-message))
	    (omessage (message nil)))))
  eldoc-last-message)

;; This function goes on pre-command-hook for XEmacs or when using idle
;; timers in Emacs.  Motion commands clear the echo area for some reason,
;; which make eldoc messages flicker or disappear just before motion
;; begins.  This function reprints the last eldoc message immediately
;; before the next command executes, which does away with the flicker.
;; This doesn't seem to be required for Emacs 19.28 and earlier.
(defun eldoc-pre-command-refresh-echo-area ()
  (and eldoc-last-message
       (if (eldoc-display-message-no-interference-p)
           (eldoc-message eldoc-last-message)
         (setq eldoc-last-message nil))))

;; Decide whether now is a good time to display a message.
(defun eldoc-display-message-p ()
  (and (eldoc-display-message-no-interference-p)
       ;; If this-command is non-nil while running via an idle
       ;; timer, we're still in the middle of executing a command,
       ;; e.g. a query-replace where it would be annoying to
       ;; overwrite the echo area.
       (and (not this-command)
	    (symbolp last-command)
	    (intern-soft (symbol-name last-command)
			 eldoc-message-commands))))

;; Check various conditions about the current environment that might make
;; it undesirable to print eldoc messages right this instant.
(defun eldoc-display-message-no-interference-p ()
  (and eldoc-mode
       (not executing-kbd-macro)
       (not (and (boundp 'edebug-active) edebug-active))
       ;; Having this mode operate in an active minibuffer/echo area causes
       ;; interference with what's going on there.
       (not cursor-in-echo-area)
       (not (eq (selected-window) (minibuffer-window)))))


;;;###autoload
(defvar eldoc-documentation-function nil
  "If non-nil, function to call to return doc string.
The function of no args should return a one-line string for displaying
doc about a function etc. appropriate to the context around point.
It should return nil if there's no doc appropriate for the context.
Typically doc is returned if point is on a function-like name or in its
arg list.

This variable is expected to be made buffer-local by modes (other than
Emacs Lisp mode) that support Eldoc.")

(defun eldoc-print-current-symbol-info ()
  (condition-case err
      (and (eldoc-display-message-p)
	   (if eldoc-documentation-function
	       (eldoc-message (funcall eldoc-documentation-function))
	     (let* ((current-symbol (eldoc-current-symbol))
		    (current-fnsym  (eldoc-fnsym-in-current-sexp))
		    (doc (cond
			  ((eq current-symbol current-fnsym)
			   (or (eldoc-get-fnsym-args-string current-fnsym)
			       (eldoc-get-var-docstring current-symbol)))
			  (t
			   (or (eldoc-get-var-docstring current-symbol)
			       (eldoc-get-fnsym-args-string current-fnsym))))))
	       (eldoc-message doc))))
    ;; This is run from post-command-hook or some idle timer thing,
    ;; so we need to be careful that errors aren't ignored.
    (error (message "eldoc error: %s" err))))

;; Return a string containing the function parameter list, or 1-line
;; docstring if function is a subr and no arglist is obtainable from the
;; docstring or elsewhere.
(defun eldoc-get-fnsym-args-string (sym)
  (let ((args nil)
        (doc nil))
    (cond ((not (and sym (symbolp sym) (fboundp sym))))
          ((and (eq sym (aref eldoc-last-data 0))
                (eq 'function (aref eldoc-last-data 2)))
           (setq doc (aref eldoc-last-data 1)))
	  ((setq doc (help-split-fundoc (documentation sym t) sym))
	   (setq args (car doc))
	   (string-match "\\`[^ )]* ?" args)
	   (setq args (concat "(" (substring args (match-end 0)))))
          (t
           (setq args (eldoc-function-argstring sym))))
    (cond (args
           (setq doc (eldoc-docstring-format-sym-doc sym args))
           (eldoc-last-data-store sym doc 'function)))
    doc))

;; Return a string containing a brief (one-line) documentation string for
;; the variable.
(defun eldoc-get-var-docstring (sym)
  (when sym
    (cond ((and (eq sym (aref eldoc-last-data 0))
		(eq 'variable (aref eldoc-last-data 2)))
	   (aref eldoc-last-data 1))
	  (t
	   (let ((doc (documentation-property sym 'variable-documentation t)))
	     (cond (doc
		    (setq doc (eldoc-docstring-format-sym-doc
			       sym (eldoc-docstring-first-line doc)))
		    (eldoc-last-data-store sym doc 'variable)))
	     doc)))))

(defun eldoc-last-data-store (symbol doc type)
  (aset eldoc-last-data 0 symbol)
  (aset eldoc-last-data 1 doc)
  (aset eldoc-last-data 2 type))

;; Note that any leading `*' in the docstring (which indicates the variable
;; is a user option) is removed.
(defun eldoc-docstring-first-line (doc)
  (and (stringp doc)
       (substitute-command-keys
        (save-match-data
          (let ((start (if (string-match "^\\*" doc) (match-end 0) 0)))
            (cond ((string-match "\n" doc)
                   (substring doc start (match-beginning 0)))
                  ((zerop start) doc)
                  (t (substring doc start))))))))

;; If the entire line cannot fit in the echo area, the symbol name may be
;; truncated or eliminated entirely from the output to make room for the
;; description.
(defun eldoc-docstring-format-sym-doc (sym doc)
  (save-match-data
    (let* ((name (symbol-name sym))
           (ea-multi eldoc-echo-area-use-multiline-p)
           ;; Subtract 1 from window width since emacs will not write
           ;; any chars to the last column, or in later versions, will
           ;; cause a wraparound and resize of the echo area.
           (ea-width (1- (window-width (minibuffer-window))))
           (strip (- (+ (length name) (length ": ") (length doc)) ea-width)))
      (cond ((or (<= strip 0)
                 (eq ea-multi t)
                 (and ea-multi (> (length doc) ea-width)))
             (format "%s: %s" sym doc))
            ((> (length doc) ea-width)
             (substring (format "%s" doc) 0 ea-width))
            ((>= strip (length name))
             (format "%s" doc))
            (t
             ;; Show the end of the partial symbol name, rather
             ;; than the beginning, since the former is more likely
             ;; to be unique given package namespace conventions.
             (setq name (substring name strip))
             (format "%s: %s" name doc))))))


(defun eldoc-fnsym-in-current-sexp ()
  (let ((p (point)))
    (eldoc-beginning-of-sexp)
    (prog1
        ;; Don't do anything if current word is inside a string.
        (if (= (or (char-after (1- (point))) 0) ?\")
            nil
          (eldoc-current-symbol))
      (goto-char p))))

(defun eldoc-beginning-of-sexp ()
  (let ((parse-sexp-ignore-comments t))
    (condition-case err
        (while (progn
                 (forward-sexp -1)
                 (or (= (char-before) ?\")
                     (> (point) (point-min)))))
      (error nil))))

;; returns nil unless current word is an interned symbol.
(defun eldoc-current-symbol ()
  (let ((c (char-after (point))))
    (and c
         (memq (char-syntax c) '(?w ?_))
         (intern-soft (current-word)))))

;; Do indirect function resolution if possible.
(defun eldoc-symbol-function (fsym)
  (let ((defn (and (fboundp fsym)
                   (symbol-function fsym))))
    (and (symbolp defn)
         (condition-case err
             (setq defn (indirect-function fsym))
           (error (setq defn nil))))
    defn))

(defun eldoc-function-argstring (fn)
  (eldoc-function-argstring-format (help-function-arglist fn)))

(defun eldoc-function-argstring-format (arglist)
  (cond ((not (listp arglist))
         (setq arglist nil))
        ((symbolp (car arglist))
         (setq arglist
               (mapcar (function (lambda (s)
                                   (if (memq s '(&optional &rest))
                                       (symbol-name s)
                                     (funcall eldoc-argument-case
                                              (symbol-name s)))))
                       arglist)))
        ((stringp (car arglist))
         (setq arglist
               (mapcar (function (lambda (s)
                                   (if (member s '("&optional" "&rest"))
                                       s
                                     (funcall eldoc-argument-case s))))
                       arglist))))
  (concat "(" (mapconcat 'identity arglist " ") ")"))


;; When point is in a sexp, the function args are not reprinted in the echo
;; area after every possible interactive command because some of them print
;; their own messages in the echo area; the eldoc functions would instantly
;; overwrite them unless it is more restrained.
;; These functions do display-command table management.

(defun eldoc-add-command (&rest cmds)
  (or eldoc-message-commands
      (setq eldoc-message-commands
            (make-vector eldoc-message-commands-table-size 0)))

  (let (name sym)
    (while cmds
      (setq name (car cmds))
      (setq cmds (cdr cmds))

      (cond ((symbolp name)
             (setq sym name)
             (setq name (symbol-name sym)))
            ((stringp name)
             (setq sym (intern-soft name))))

      (and (symbolp sym)
           (fboundp sym)
           (set (intern name eldoc-message-commands) t)))))

(defun eldoc-add-command-completions (&rest names)
  (while names
    (apply 'eldoc-add-command
	   (all-completions (car names) obarray 'fboundp))
    (setq names (cdr names))))

(defun eldoc-remove-command (&rest cmds)
  (let (name)
    (while cmds
      (setq name (car cmds))
      (setq cmds (cdr cmds))

      (and (symbolp name)
           (setq name (symbol-name name)))

      (unintern name eldoc-message-commands))))

(defun eldoc-remove-command-completions (&rest names)
  (while names
    (apply 'eldoc-remove-command
           (all-completions (car names) eldoc-message-commands))
    (setq names (cdr names))))


;; Prime the command list.
(eldoc-add-command-completions
 "backward-" "beginning-of-" "delete-other-windows" "delete-window"
 "end-of-" "forward-" "indent-for-tab-command" "goto-" "mouse-set-point"
 "next-" "other-window" "previous-" "recenter" "scroll-"
 "self-insert-command" "split-window-"
 "up-list" "down-list")

(provide 'eldoc)

;;; arch-tag: c9a58f9d-2055-46c1-9b82-7248b71a8375
;;; eldoc.el ends here
