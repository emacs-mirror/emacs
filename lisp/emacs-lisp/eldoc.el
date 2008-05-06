;;; eldoc.el --- show function arglist or variable docstring in echo area

;; Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008 Free Software Foundation, Inc.

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

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

(defface eldoc-highlight-function-argument
  '((t (:inherit bold)))
  "Face used for the argument at point in a function's argument list."
  :group 'eldoc)

;;; No user options below here.

(defvar eldoc-message-commands-table-size 31
  "This is used by `eldoc-add-command' to initialize `eldoc-message-commands'
as an obarray.
It should probably never be necessary to do so, but if you
choose to increase the number of buckets, you must do so before loading
this file since the obarray is initialized at load time.
Remember to keep it a prime number to improve hash performance.")

(defconst eldoc-message-commands
  (make-vector eldoc-message-commands-table-size 0)
  "Commands after which it is appropriate to print in the echo area.
Eldoc does not try to print function arglists, etc. after just any command,
because some commands print their own messages in the echo area and these
functions would instantly overwrite them.  But `self-insert-command' as well
as most motion commands are good candidates.
This variable contains an obarray of symbols; do not manipulate it
directly.  Instead, use `eldoc-add-command' and `eldoc-remove-command'.")

(defconst eldoc-last-data (make-vector 3 nil)
  "Bookkeeping; elements are as follows:
  0 - contains the last symbol read from the buffer.
  1 - contains the string last displayed in the echo area for variables,
      or argument string for functions.
  2 - 'function if function args, 'variable if variable documentation.")
(defvar eldoc-last-message nil)

(defvar eldoc-timer nil "eldoc's timer object.")

(defvar eldoc-current-idle-delay eldoc-idle-delay
  "Idle time delay currently in use by timer.
This is used to determine if `eldoc-idle-delay' is changed by the user.")


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
  "Unequivocally turn on ElDoc mode (see command `eldoc-mode')."
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
			  ((null current-fnsym)
			   nil)
			  ((eq current-symbol (car current-fnsym))
			   (or (apply 'eldoc-get-fnsym-args-string
				      current-fnsym)
			       (eldoc-get-var-docstring current-symbol)))
			  (t
			   (or (eldoc-get-var-docstring current-symbol)
			       (apply 'eldoc-get-fnsym-args-string
				      current-fnsym))))))
	       (eldoc-message doc))))
    ;; This is run from post-command-hook or some idle timer thing,
    ;; so we need to be careful that errors aren't ignored.
    (error (message "eldoc error: %s" err))))

(defun eldoc-get-fnsym-args-string (sym &optional index)
  "Return a string containing the parameter list of the function SYM.
If SYM is a subr and no arglist is obtainable from the docstring
or elsewhere, return a 1-line docstring.  Calls the functions
`eldoc-function-argstring-format' and
`eldoc-highlight-function-argument' to format the result.  The
former calls `eldoc-argument-case'; the latter gives the
function name `font-lock-function-name-face', and optionally
highlights argument number INDEX. "
  (let (args doc)
    (cond ((not (and sym (symbolp sym) (fboundp sym))))
	  ((and (eq sym (aref eldoc-last-data 0))
		(eq 'function (aref eldoc-last-data 2)))
	   (setq doc (aref eldoc-last-data 1)))
	  ((setq doc (help-split-fundoc (documentation sym t) sym))
	   (setq args (car doc))
	   ;; Remove any enclosing (), since e-function-argstring adds them.
	   (string-match "\\`[^ )]* ?" args)
	   (setq args (substring args (match-end 0)))
	   (if (string-match ")\\'" args)
	       (setq args (substring args 0 -1))))
	  (t
	   (setq args (help-function-arglist sym))))
    (if args
	;; Stringify, and store before highlighting, downcasing, etc.
	;; FIXME should truncate before storing.
	(eldoc-last-data-store sym (setq args (eldoc-function-argstring args))
			       'function)
      (setq args doc))		  ; use stored value
    ;; Change case, highlight, truncate.
    (if args
	(eldoc-highlight-function-argument
	 sym (eldoc-function-argstring-format args) index))))

(defun eldoc-highlight-function-argument (sym args index)
  "Highlight argument INDEX in ARGS list for function SYM.
In the absence of INDEX, just call `eldoc-docstring-format-sym-doc'."
  (let ((start          nil)
	(end            0)
	(argument-face  'eldoc-highlight-function-argument))
    ;; Find the current argument in the argument string.  We need to
    ;; handle `&rest' and informal `...' properly.
    ;;
    ;; FIXME: What to do with optional arguments, like in
    ;;        (defun NAME ARGLIST [DOCSTRING] BODY...) case?
    ;;        The problem is there is no robust way to determine if
    ;;        the current argument is indeed a docstring.
    (while (and index (>= index 1))
      (if (string-match "[^ ()]+" args end)
	  (progn
	    (setq start (match-beginning 0)
		  end   (match-end 0))
	    (let ((argument (match-string 0 args)))
	      (cond ((string= argument "&rest")
		     ;; All the rest arguments are the same.
		     (setq index 1))
		    ((string= argument "&optional"))
		    ((string-match "\\.\\.\\.$" argument)
		     (setq index 0))
		    (t
		     (setq index (1- index))))))
	(setq end           (length args)
	      start         (1- end)
	      argument-face 'font-lock-warning-face
	      index         0)))
    (let ((doc args))
      (when start
	(setq doc (copy-sequence args))
	(add-text-properties start end (list 'face argument-face) doc))
      (setq doc (eldoc-docstring-format-sym-doc
		 sym doc 'font-lock-function-name-face))
      doc)))

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
			       sym (eldoc-docstring-first-line doc)
			       'font-lock-variable-name-face))
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
(defun eldoc-docstring-format-sym-doc (sym doc face)
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
             (format "%s: %s" (propertize name 'face face) doc))
            ((> (length doc) ea-width)
             (substring (format "%s" doc) 0 ea-width))
            ((>= strip (length name))
             (format "%s" doc))
            (t
             ;; Show the end of the partial symbol name, rather
             ;; than the beginning, since the former is more likely
             ;; to be unique given package namespace conventions.
             (setq name (substring name strip))
             (format "%s: %s" (propertize name 'face face) doc))))))


;; Return a list of current function name and argument index.
(defun eldoc-fnsym-in-current-sexp ()
  (save-excursion
    (let ((argument-index (1- (eldoc-beginning-of-sexp))))
      ;; If we are at the beginning of function name, this will be -1.
      (when (< argument-index 0)
	(setq argument-index 0))
      ;; Don't do anything if current word is inside a string.
      (if (= (or (char-after (1- (point))) 0) ?\")
	  nil
	(list (eldoc-current-symbol) argument-index)))))

;; Move to the beginnig of current sexp.  Return the number of nested
;; sexp the point was over or after.
(defun eldoc-beginning-of-sexp ()
  (let ((parse-sexp-ignore-comments t)
	(num-skipped-sexps 0))
    (condition-case err
	(progn
	  ;; First account for the case the point is directly over a
	  ;; beginning of a nested sexp.
	  (condition-case err
	      (let ((p (point)))
		(forward-sexp -1)
		(forward-sexp 1)
		(when (< (point) p)
		  (setq num-skipped-sexps 1)))
	    (error))
	  (while
	      (let ((p (point)))
		(forward-sexp -1)
		(when (< (point) p)
		  (setq num-skipped-sexps (1+ num-skipped-sexps))))))
      (error))
    num-skipped-sexps))

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

(defun eldoc-function-argstring (arglist)
  "Return ARGLIST as a string enclosed by ().
ARGLIST is either a string, or a list of strings or symbols."
  (cond ((stringp arglist))
	((not (listp arglist))
	 (setq arglist nil))
	((symbolp (car arglist))
	 (setq arglist
	       (mapconcat (lambda (s) (symbol-name s))
			  arglist " ")))
	((stringp (car arglist))
	 (setq arglist
	       (mapconcat (lambda (s) s)
			  arglist " "))))
  (if arglist
      (format "(%s)" arglist)))

(defun eldoc-function-argstring-format (argstring)
  "Apply `eldoc-argument-case' to each word in ARGSTRING.
The words \"&rest\", \"&optional\" are returned unchanged."
  (mapconcat (lambda (s)
	       (if (member s '("&optional" "&rest"))
		   s
		 (funcall eldoc-argument-case s)))
	     (split-string argstring "[][ ()]+" t) " "))


;; When point is in a sexp, the function args are not reprinted in the echo
;; area after every possible interactive command because some of them print
;; their own messages in the echo area; the eldoc functions would instantly
;; overwrite them unless it is more restrained.
;; These functions do display-command table management.

(defun eldoc-add-command (&rest cmds)
  (dolist (name cmds)
    (and (symbolp name)
         (setq name (symbol-name name)))
    (set (intern name eldoc-message-commands) t)))

(defun eldoc-add-command-completions (&rest names)
  (dolist (name names)
    (apply 'eldoc-add-command (all-completions name obarray 'commandp))))

(defun eldoc-remove-command (&rest cmds)
  (dolist (name cmds)
    (and (symbolp name)
         (setq name (symbol-name name)))
    (unintern name eldoc-message-commands)))

(defun eldoc-remove-command-completions (&rest names)
  (dolist (name names)
    (apply 'eldoc-remove-command
           (all-completions name eldoc-message-commands))))


;; Prime the command list.
(eldoc-add-command-completions
 "backward-" "beginning-of-" "move-beginning-of-" "delete-other-windows"
 "delete-window" "handle-select-window"
 "end-of-" "move-end-of-" "exchange-point-and-mark" "forward-"
 "indent-for-tab-command" "goto-" "mark-page" "mark-paragraph"
 "mouse-set-point" "move-" "pop-global-mark" "next-" "other-window"
 "previous-" "recenter" "scroll-" "self-insert-command"
 "split-window-" "up-list" "down-list")

(provide 'eldoc)

;; arch-tag: c9a58f9d-2055-46c1-9b82-7248b71a8375
;;; eldoc.el ends here
