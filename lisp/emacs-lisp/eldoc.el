;;; eldoc.el --- show function arglist or variable docstring in echo area

;; Copyright (C) 1996, 1997 Free Software Foundation, Inc.

;; Author: Noah Friedman <friedman@prep.ai.mit.edu>
;; Maintainer: friedman@prep.ai.mit.edu
;; Keywords: extensions
;; Created: 1995-10-06

;; $Id: eldoc.el,v 1.7 1996/10/04 04:43:42 friedman Exp $

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
;;      (autoload 'turn-on-eldoc-mode "eldoc" nil t)
;;      (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
;;      (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)

;;; Code:

;; Use idle timers if available in the version of emacs running.
;; Please don't change this to use `require'; this package works as-is in
;; XEmacs (which doesn't have timer.el as of 19.14), and I would like to
;; maintain compatibility with that since I must use it sometimes.  --Noah
(or (featurep 'timer)
    (load "timer" t))

;;;###autoload
(defvar eldoc-mode nil
  "*If non-nil, show the defined parameters for the elisp function near point.

For the emacs lisp function at the beginning of the sexp which point is
within, show the defined parameters for the function in the echo area.
This information is extracted directly from the function or macro if it is
in pure lisp.  If the emacs function is a subr, the parameters are obtained
from the documentation string if possible.

If point is over a documented variable, print that variable's docstring
instead.

This variable is buffer-local.")
(make-variable-buffer-local 'eldoc-mode)

(defconst eldoc-idle-delay 0.50
  "*Number of seconds of idle time to wait before printing.
If user input arrives before this interval of time has elapsed after the
last input, no documentation will be printed.

If this variable is set to 0, no idle time is required.")

(defconst eldoc-minor-mode-string " ElDoc"
  "*String to display in mode line when Eldoc Mode is enabled.")

;; Put this minor mode on the global minor-mode-alist.
(or (assq 'eldoc-mode (default-value 'minor-mode-alist))
    (setq-default minor-mode-alist
                  (append (default-value 'minor-mode-alist)
                          '((eldoc-mode eldoc-minor-mode-string)))))

(defconst eldoc-argument-case 'upcase
  "Case to display argument names of functions, as a symbol.
This has two preferred values: `upcase' or `downcase'.
Actually, any name of a function which takes a string as an argument and
returns another string is acceptable.")

(defvar eldoc-message-commands nil
  "*Commands after which it is appropriate to print in the echo area.

Eldoc does not try to print function arglists, etc. after just any command,
because some commands print their own messages in the echo area and these
functions would instantly overwrite them.  But self-insert-command as well
as most motion commands are good candidates.

This variable contains an obarray of symbols; it is probably best to
manipulate this data structure with the commands `eldoc-add-command' and
`eldoc-remove-command'.")

(cond ((null eldoc-message-commands)
       ;; If you increase the number of buckets, keep it a prime number.
       (setq eldoc-message-commands (make-vector 31 0))
       (let ((list '("self-insert-command"
                     "next-"         "previous-"
                     "forward-"      "backward-"
                     "beginning-of-" "end-of-"
                     "goto-"
                     "recenter"
                     "scroll-"
                     "mouse-set-point"))
             (syms nil))
         (while list
           (setq syms (all-completions (car list) obarray 'fboundp))
           (setq list (cdr list))
           (while syms
             (set (intern (car syms) eldoc-message-commands) t)
             (setq syms (cdr syms)))))))

;; Bookkeeping; the car contains the last symbol read from the buffer.
;; The cdr contains the string last displayed in the echo area, so it can
;; be printed again if necessary without reconsing.
(defvar eldoc-last-data '(nil . nil))

;; Idle timers are supported in Emacs 19.31 and later.
(defconst eldoc-use-idle-timer-p (fboundp 'run-with-idle-timer))

;; eldoc's timer object, if using idle timers
(defvar eldoc-timer nil)

;; idle time delay currently in use by timer.
;; This is used to determine if eldoc-idle-delay is changed by the user.
(defvar eldoc-current-idle-delay eldoc-idle-delay)

;; In emacs 19.29 and later, and XEmacs 19.13 and later, all messages are
;; recorded in a log.  Do not put eldoc messages in that log since
;; they are Legion.
(defmacro eldoc-message (&rest args)
  (if (fboundp 'display-message)
      ;; XEmacs 19.13 way of preventing log messages.
      (list 'display-message '(quote no-log) (apply 'list 'format args))
    (list 'let (list (list 'message-log-max 'nil))
          (apply 'list 'message args))))


;;;###autoload
(defun eldoc-mode (&optional prefix)
  "*Enable or disable eldoc mode.
See documentation for the variable of the same name for more details.

If called interactively with no prefix argument, toggle current condition
of the mode.
If called with a positive or negative prefix argument, enable or disable
the mode, respectively."
  (interactive "P")

  (cond (eldoc-use-idle-timer-p
         (add-hook 'post-command-hook 'eldoc-schedule-timer))
        (t
         ;; Use post-command-idle-hook if defined, otherwise use
         ;; post-command-hook.  The former is only proper to use in Emacs
         ;; 19.30; that is the first version in which it appeared, but it
         ;; was obsolesced by idle timers in Emacs 19.31.
         (add-hook (if (boundp 'post-command-idle-hook)
                       'post-command-idle-hook
                     'post-command-hook)
                   'eldoc-print-current-symbol-info)))

  (setq eldoc-mode (if prefix
                       (>= (prefix-numeric-value prefix) 0)
                     (not eldoc-mode)))

  (and (interactive-p)
       (if eldoc-mode
           (message "eldoc-mode is enabled")
         (message "eldoc-mode is disabled")))
  eldoc-mode)

;;;###autoload
(defun turn-on-eldoc-mode ()
  "Unequivocally turn on eldoc-mode (see variable documentation)."
  (interactive)
  (eldoc-mode 1))

(defun eldoc-add-command (cmd)
  "Add COMMAND to the list of commands which causes function arg display.
If called interactively, completion on defined commands is available.

When point is in a sexp, the function args are not reprinted in the echo
area after every possible interactive command because some of them print
their own messages in the echo area; the eldoc functions would instantly
overwrite them unless it is more restrained."
  (interactive "CAdd function to eldoc message commands list: ")
  (and (fboundp cmd)
       (set (intern (symbol-name cmd) eldoc-message-commands) t)))

(defun eldoc-remove-command (cmd)
  "Remove COMMAND from the list of commands which causes function arg display.
If called interactively, completion matches only those functions currently
in the list.

When point is in a sexp, the function args are not reprinted in the echo
area after every possible interactive command because some of them print
their own messages in the echo area; the eldoc functions would instantly
overwrite them unless it is more restrained."
  (interactive (list (completing-read
                      "Remove function from eldoc message commands list: "
                      eldoc-message-commands 'boundp t)))
  (and (symbolp cmd)
       (setq cmd (symbol-name cmd)))
  (if (fboundp 'unintern)
      (unintern cmd eldoc-message-commands)
    (let ((s (intern-soft cmd eldoc-message-commands)))
      (and s
           (makunbound s)))))

;; Idle timers are part of Emacs 19.31 and later.
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


(defun eldoc-print-current-symbol-info ()
  (and eldoc-mode
       (not executing-kbd-macro)

       ;; Having this mode operate in an active minibuffer makes it
       ;; impossible to what you're doing.
       (not (eq (selected-window) (minibuffer-window)))

       (cond (eldoc-use-idle-timer-p
              (and (symbolp last-command)
                   (intern-soft (symbol-name last-command)
                                eldoc-message-commands)))
             (t
              ;; If we don't have idle timers, this function is
              ;; running on post-command-hook directly; that means the
              ;; user's last command is still on `this-command', and we
              ;; must wait briefly for input to see whether to do display.
              (and (symbolp this-command)
                   (intern-soft (symbol-name this-command)
                                eldoc-message-commands)
                   (sit-for eldoc-idle-delay))))

       (let ((current-symbol (eldoc-current-symbol))
             (current-fnsym  (eldoc-fnsym-in-current-sexp)))
         (cond ((eq current-symbol current-fnsym)
                (eldoc-print-fnsym-args current-fnsym))
               (t
                (or (eldoc-print-var-docstring current-symbol)
                    (eldoc-print-fnsym-args current-fnsym)))))))

(defun eldoc-print-fnsym-args (&optional symbol)
  (interactive)
  (let ((sym (or symbol (eldoc-fnsym-in-current-sexp)))
        (args nil))
    (cond ((not (and (symbolp sym)
                     (fboundp sym))))
          ((eq sym (car eldoc-last-data))
           (setq args (cdr eldoc-last-data)))
          ((subrp (eldoc-symbol-function sym))
           (setq args (or (eldoc-function-argstring-from-docstring sym)
                          (eldoc-docstring-first-line (documentation sym t))))
           (setcar eldoc-last-data sym)
           (setcdr eldoc-last-data args))
          (t
           (setq args (eldoc-function-argstring sym))
           (setcar eldoc-last-data sym)
           (setcdr eldoc-last-data args)))
    (and args
         (eldoc-message "%s: %s" sym args))))

(defun eldoc-fnsym-in-current-sexp ()
  (let* ((p (point))
         (sym (progn
                (while (and (eldoc-forward-sexp-safe -1)
                            (> (point) (point-min))))
                (cond ((or (= (point) (point-min))
                           (memq (or (char-after (point)) 0)
                                 '(?\( ?\"))
                           ;; If we hit a quotation mark before a paren, we
                           ;; are inside a specific string, not a list of
                           ;; symbols.
                           (eq (or (char-after (1- (point))) 0) ?\"))
                       nil)
                      (t (condition-case nil
                             (read (current-buffer))
                           (error nil)))))))
    (goto-char p)
    (and (symbolp sym)
         sym)))

(defun eldoc-function-argstring (fn)
  (let* ((prelim-def (eldoc-symbol-function fn))
         (def (if (eq (car-safe prelim-def) 'macro)
                  (cdr prelim-def)
                prelim-def))
         (arglist (cond ((null def) nil)
                        ((byte-code-function-p def)
                         (if (fboundp 'compiled-function-arglist)
                             (funcall 'compiled-function-arglist def)
                           (aref def 0)))
                        ((eq (car-safe def) 'lambda)
                         (nth 1 def))
                        (t t))))
    (eldoc-function-argstring-format arglist)))

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


(defun eldoc-print-var-docstring (&optional sym)
  (or sym (setq sym (eldoc-current-symbol)))
  (eldoc-print-docstring sym (documentation-property
                              sym 'variable-documentation t)))

;; Print the brief (one-line) documentation string for the symbol.
(defun eldoc-print-docstring (symbol doc)
  (and doc
       (eldoc-message "%s" (eldoc-docstring-message symbol doc))))

;; If the entire line cannot fit in the echo area, the variable name may be
;; truncated or eliminated entirely from the output to make room.
;; Any leading `*' in the docstring (which indicates the variable is a user
;; option) is not printed."
(defun eldoc-docstring-message (symbol doc)
  (and doc
       (let ((name (symbol-name symbol)))
         (setq doc (eldoc-docstring-first-line doc))
         (save-match-data
           (let* ((doclen (+ (length name) (length ": ") (length doc)))
                  ;; Subtract 1 from window width since emacs seems not to
                  ;; write any chars to the last column, at least for some
                  ;; terminal types.
                  (strip (- doclen (1- (window-width (minibuffer-window))))))
             (cond ((> strip 0)
                    (let* ((len (length name)))
                      (cond ((>= strip len)
                             (format "%s" doc))
                            (t
                             (setq name (substring name 0 (- len strip)))
                             (format "%s: %s" name doc)))))
                   (t
                    (format "%s: %s" symbol doc))))))))

(defun eldoc-docstring-first-line (doc)
  (save-match-data
    (and (string-match "\n" doc)
         (setq doc (substring doc 0 (match-beginning 0))))
    (and (string-match "^\\*" doc)
         (setq doc (substring doc 1))))
  doc)


;; Alist of predicate/action pairs.
;; Each member of the list is a sublist consisting of a predicate function
;; used to determine if the arglist for a function can be found using a
;; certain pattern, and a function which returns the actual arglist from
;; that docstring.
;;
;; The order in this table is significant, since later predicates may be
;; more general than earlier ones.
;;
;; Compiler note for Emacs 19.29 and later: these functions will be
;; compiled to bytecode, but can't be lazy-loaded even if you set
;; byte-compile-dynamic; to do that would require making them named
;; top-level defuns, and that's not particularly desirable either.
(defconst eldoc-function-argstring-from-docstring-method-table
  (list
   ;; Try first searching for args starting with symbol name.
   ;; This is to avoid matching parenthetical remarks in e.g. sit-for.
   (list (function (lambda (doc fn)
                     (string-match (format "^(%s[^\n)]*)$" fn) doc)))
         (function (lambda (doc)
                     ;; end does not include trailing ")" sequence.
                     (let ((end (- (match-end 0) 1)))
                       (if (string-match " +" doc (match-beginning 0))
                           (substring doc (match-end 0) end)
                         "")))))

   ;; Try again not requiring this symbol name in the docstring.
   ;; This will be the case when looking up aliases.
   (list (function (lambda (doc fn)
                     (string-match "^([^\n)]+)$" doc)))
         (function (lambda (doc)
                     ;; end does not include trailing ")" sequence.
                     (let ((end (- (match-end 0) 1)))
                       (and (string-match " +" doc (match-beginning 0))
                            (substring doc (match-end 0) end))))))

   ;; Emacs subr docstring style:
   ;;   (fn arg1 arg2 ...): description...
   (list (function (lambda (doc fn)
                     (string-match "^([^\n)]+):" doc)))
         (function (lambda (doc)
                     ;; end does not include trailing "):" sequence.
                     (let ((end (- (match-end 0) 2)))
                       (and (string-match " +" doc (match-beginning 0))
                            (substring doc (match-end 0) end))))))

   ;; XEmacs subr docstring style:
   ;;   "arguments: (arg1 arg2 ...)
   (list (function (lambda (doc fn)
                     (string-match "^arguments: (\\([^\n)]+\\))" doc)))
         (function (lambda (doc)
                     ;; also skip leading paren, but the first word is
                     ;; actually an argument, not the function name.
                     (substring doc (match-beginning 1) (match-end 1)))))

   ;; This finds the argstring for `condition-case'.  Any others?
   (list (function (lambda (doc fn)
                     (string-match
                      (format "^Usage looks like \\((%s[^\n)]*)\\)\\.$" fn)
                      doc)))
         (function (lambda (doc)
                     ;; end does not include trailing ")" sequence.
                     (let ((end (- (match-end 1) 1)))
                       (and (string-match " +" doc (match-beginning 1))
                            (substring doc (match-end 0) end))))))

   ;; This finds the argstring for `setq-default'.  Any others?
   (list (function (lambda (doc fn)
                     (string-match (format "^[ \t]+\\((%s[^\n)]*)\\)$" fn)
                                   doc)))
         (function (lambda (doc)
                     ;; end does not include trailing ")" sequence.
                     (let ((end (- (match-end 1) 1)))
                       (and (string-match " +" doc (match-beginning 1))
                            (substring doc (match-end 0) end))))))

   ;; This finds the argstring for `start-process'.  Any others?
   (list (function (lambda (doc fn)
                     (string-match "^Args are +\\([^\n]+\\)$" doc)))
         (function (lambda (doc)
                     (substring doc (match-beginning 1) (match-end 1)))))
   ))

(defun eldoc-function-argstring-from-docstring (fn)
  (let ((docstring (documentation fn 'raw))
        (table eldoc-function-argstring-from-docstring-method-table)
        (doc nil)
        (doclist nil))
    (save-match-data
      (while table
        (cond ((funcall (car (car table)) docstring fn)
               (setq doc (funcall (car (cdr (car table))) docstring))
               (setq table nil))
              (t
               (setq table (cdr table)))))

      (cond ((not (stringp doc))
             nil)
            ((string-match "&" doc)
             (let ((p 0)
                   (l (length doc)))
               (while (< p l)
                 (cond ((string-match "[ \t\n]+" doc p)
                        (setq doclist
                              (cons (substring doc p (match-beginning 0))
                                    doclist))
                        (setq p (match-end 0)))
                       (t
                        (setq doclist (cons (substring doc p) doclist))
                        (setq p l))))
               (eldoc-function-argstring-format (nreverse doclist))))
            (t
             (concat "(" (funcall eldoc-argument-case doc) ")"))))))


;; forward-sexp calls scan-sexps, which returns an error if it hits the
;; beginning or end of the sexp.  This returns nil instead.
(defun eldoc-forward-sexp-safe (&optional count)
  "Move forward across one balanced expression (sexp).
With argument, do it that many times.  Negative arg -COUNT means
move backward across COUNT balanced expressions.
Return distance in buffer moved, or nil."
  (or count (setq count 1))
  (condition-case err
      (- (- (point) (progn
                      (let ((parse-sexp-ignore-comments t))
                        (forward-sexp count))
                      (point))))
    (error nil)))

;; Do indirect function resolution if possible.
(defun eldoc-symbol-function (fsym)
  (let ((defn (and (fboundp fsym)
                   (symbol-function fsym))))
    (and (symbolp defn)
         (condition-case err
             (setq defn (indirect-function fsym))
           (error (setq defn nil))))
    defn))

(defun eldoc-current-symbol ()
  (let ((c (char-after (point))))
    (and c
         (memq (char-syntax c) '(?w ?_))
         (intern-soft (current-word)))))

(provide 'eldoc)

;;; eldoc.el ends here
