;;; eldoc.el --- show function arglist or variable docstring in echo area

;; Copyright (C) 1995 Noah S. Friedman

;; Author: Noah Friedman <friedman@prep.ai.mit.edu>
;; Maintainer: friedman@prep.ai.mit.edu
;; Keywords: extensions
;; Status: Works in Emacs 19 and XEmacs.
;; Created: 1995-10-06

;; LCD Archive Entry:
;; eldoc|Noah Friedman|friedman@prep.ai.mit.edu|
;; show function arglist or variable docstring in echo area|
;; $Date: 1995/11/18 22:32:07 $|$Revision: 1.3 $|~/misc/eldoc.el.gz|

;; $Id: eldoc.el,v 1.3 1995/11/18 22:32:07 friedman Exp friedman $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This program was inspired by the behavior of the Lisp Machine "mouse
;; documentation window"; as you type a function's symbol name as part of a
;; sexp, it will print the argument list for that function.  However, this
;; program's behavior is different in a couple of significant ways.  For
;; one, you need not actually type the function name; you need only move
;; point around in a sexp that calls it.  However, if point is over a
;; documented variable, it will print the one-line documentation for that
;; variable instead, to remind you of that variable's purpose.

;; One useful way to enable this minor mode is to put the following in your
;; .emacs:
;;
;;      (autoload 'turn-on-eldoc-mode "eldoc" nil t)
;;      (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
;;      (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)

;;; Code:

;;;###autoload
(defvar eldoc-mode nil
  "*If non-nil, show the defined parameters for the elisp function near point.

For the emacs lisp function at the beginning of the sexp which point is
within, show the defined parameters for the function in the echo area.
This information is extracted directly from the function or macro if it is
in pure lisp.

If the emacs function is a subr, the parameters are obtained from the
documentation string if possible.

If point is over a documented variable, print that variable's docstring
instead; see function `eldoc-print-var-docstring'.

This variable is buffer-local.")
(make-variable-buffer-local 'eldoc-mode)

(defvar eldoc-idle-delay 0.50
  "*Number of seconds of idle time to wait before printing.
If user input arrives before this interval of time has elapsed after the
last input, no documentation will be printed.

If this variable is set to 0, no idle time is required.")

(defvar eldoc-argument-case 'upcase
  "Case to display argument names of functions, as a symbol.
This has two preferred values: `upcase' or `downcase'.
Actually, any name of a function which takes a string as an argument and
returns another string is acceptable.")

(defvar eldoc-mode-message-commands nil
  "*Obarray of command names where it is appropriate to print in the echo area.

This is not done for all commands since some print their own
messages in the echo area, and these functions would instantly overwrite
them.  But self-insert-command as well as most motion commands are good
candidates.

It is probably best to manipulate this data structure with the commands
`eldoc-add-command' and `eldoc-remove-command'.")

(cond ((null eldoc-mode-message-commands)
       ;; If you increase the number of buckets, keep it a prime number.
       (setq eldoc-mode-message-commands (make-vector 31 0))
       (let ((list '("self-insert-command"
                     "next-"         "previous-"
                     "forward-"      "backward-"
                     "beginning-of-" "end-of-"
                     "goto-"
                     "recenter"
                     "scroll-"))
             (syms nil))
         (while list
           (setq syms (all-completions (car list) obarray 'fboundp))
           (setq list (cdr list))
           (while syms
             (set (intern (car syms) eldoc-mode-message-commands) t)
             (setq syms (cdr syms)))))))

;; Bookkeeping; the car contains the last symbol read from the buffer.
;; The cdr contains the string last displayed in the echo area, so it can
;; be printed again if necessary without reconsing.
(defvar eldoc-last-data '(nil . nil))

(defvar eldoc-minor-mode-string " ElDoc"
  "*String to display in mode line when Eldoc Mode is enabled.")

;; Put this minor mode on the global minor-mode-alist.
(or (assq 'eldoc-mode (default-value 'minor-mode-alist))
    (setq-default minor-mode-alist
                  (append (default-value 'minor-mode-alist)
                          '((eldoc-mode eldoc-minor-mode-string)))))


;;;###autoload
(defun eldoc-mode (&optional prefix)
  "*If non-nil, then enable eldoc-mode (see variable docstring)."
  (interactive "P")

  ;; Make sure it's on the post-command-idle-hook if defined, otherwise put
  ;; it on post-command-hook.  The former first appeared in Emacs 19.30.
  (add-hook (if (boundp 'post-command-idle-hook)
                'post-command-idle-hook
              'post-command-hook)
            'eldoc-mode-print-current-symbol-info)

  (setq eldoc-mode
        (>= (prefix-numeric-value prefix) 0))
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
If called interactively, completion matches any bound function.

When point is in a sexp, the function args are not reprinted in the echo
area after every possible interactive command because some of them print
their own messages in the echo area; the eldoc functions would instantly
overwrite them unless it is more restrained."
  (interactive "aAdd function to eldoc message commands list: ")
  (and (fboundp cmd)
       (set (intern (symbol-name cmd) eldoc-mode-message-commands) t)))

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
                      eldoc-mode-message-commands 'boundp t)))
  (and (symbolp cmd)
       (setq cmd (symbol-name cmd)))
  (if (fboundp 'unintern)
      (unintern cmd eldoc-mode-message-commands)
    (let ((s (intern-soft cmd eldoc-mode-message-commands)))
      (and s
           (makunbound s)))))

(defun eldoc-mode-print-current-symbol-info ()
  (and eldoc-mode
       (not executing-macro)
       ;; Having this mode operate in the minibuffer makes it impossible to
       ;; see what you're doing.
       (not (eq (selected-window) (minibuffer-window)))
       (sit-for eldoc-idle-delay)
       (symbolp this-command)
       (intern-soft (symbol-name this-command) eldoc-mode-message-commands)
       (let ((current-symbol (eldoc-current-symbol))
             (current-fnsym  (eldoc-fnsym-in-current-sexp)))
         (cond ((eq current-symbol current-fnsym)
                (eldoc-print-fnsym-args current-fnsym))
               (t
                (or (eldoc-print-var-docstring current-symbol)
                    (eldoc-print-fnsym-args current-fnsym)))))))


(defun eldoc-print-var-docstring (&optional sym)
  "Print the brief (one-line) documentation string for the variable at point.
If called with no argument, print the first line of the variable
documentation string for the symbol at point in the echo area.
If called with a symbol, print the line for that symbol.

If the entire line cannot fit in the echo area, the variable name may be
truncated or eliminated entirely from the output to make room.
Any leading `*' in the docstring (which indicates the variable is a user
option) is not printed."
  (interactive)
  (let* ((s (or sym (eldoc-current-symbol)))
         (name (symbol-name s))
         (doc (and s (documentation-property s 'variable-documentation t))))
    (and doc
         (save-match-data
           (and (string-match "\n" doc)
                (setq doc (substring doc 0 (match-beginning 0))))
           (and (string-match "^\\*" doc)
                (setq doc (substring doc 1)))
           (let* ((doclen (+ (length name) (length ": ") (length doc)))
                  ;; Subtract 1 from window width since emacs seems not to
                  ;; write any chars to the last column, at least for some
                  ;; terminal types.
                  (strip (- doclen (1- (window-width (minibuffer-window))))))
             (cond ((> strip 0)
                    (let* ((len (length name)))
                      (cond ((>= strip len)
                             (message "%s" doc))
                            (t
                             (setq name (substring name 0 (- len strip)))
                             (message "%s: %s" name doc)))))
                   (t
                    (message "%s: %s" s doc))))
           t))))


;;;###autoload
(defun eldoc-print-fnsym-args (&optional symbol)
  "*Show the defined parameters for the function near point.
For the function at the beginning of the sexp which point is within, show
the defined parameters for the function in the echo area.
This information is extracted directly from the function or macro if it is
in pure lisp.
If the emacs function is a subr, the parameters are obtained from the
documentation string if possible."
  (interactive)
  (let ((sym (or symbol (eldoc-fnsym-in-current-sexp)))
        (printit t)
        (args nil))
    (cond ((not (and (symbolp sym)
                     (fboundp sym))))
          ((eq sym (car eldoc-last-data))
           (setq printit nil)
           (setq args (cdr eldoc-last-data)))
          ((subrp (eldoc-symbol-function sym))
           (setq args (eldoc-function-argstring-from-docstring sym))
           (setcdr eldoc-last-data args))
          (t
           (setq args (eldoc-function-argstring sym))
           (setcdr eldoc-last-data args)))
    (and args
         printit
         ;; In emacs 19.29 and later, all messages are recorded in a log.
         ;; Do not put eldoc messages in the log since they are Legion.
         (let ((message-log-max nil))
           (message "%s: %s" sym args)))))

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

(defun eldoc-function-argstring-from-docstring (fn)
  (let ((docstring (documentation fn 'raw))
        (doc nil)
        (doclist nil)
        (end nil))
    (save-match-data
      (cond
       ;; Try first searching for args starting with symbol name.
       ;; This is to avoid matching parenthetical remarks in e.g. sit-for.
       ((string-match (format "^(%s[^\n)]*)$" fn) docstring)
        ;; end does not include trailing ")" sequence.
        (setq end (- (match-end 0) 1))
        (if (string-match " +" docstring (match-beginning 0))
            (setq doc (substring docstring (match-end 0) end))
          (setq doc "")))

       ;; Try again not requiring this symbol name in the docstring.
       ;; This will be the case when looking up aliases.
       ((string-match (format "^([^\n)]+)$" fn) docstring)
        ;; end does not include trailing ")" sequence.
        (setq end (- (match-end 0) 1))
        (if (string-match " +" docstring (match-beginning 0))
            (setq doc (substring docstring (match-end 0) end))
          (setq doc "")))

       ;; Emacs subr docstring style:
       ;;   (fn arg1 arg2 ...): description...
       ((string-match "^([^\n)]+):" docstring)
        ;; end does not include trailing "):" sequence.
        (setq end (- (match-end 0) 2))
        (if (string-match " +" docstring (match-beginning 0))
            (setq doc (substring docstring (match-end 0) end))
          (setq doc "")))

       ;; XEmacs subr docstring style:
       ;;   "arguments: (arg1 arg2 ...)
       ((string-match "^arguments: (\\([^\n)]+\\))" docstring)
        ;; Also, skip leading paren, but the first word is actually an
        ;; argument, not the function name.
        (setq doc (substring docstring
                             (match-beginning 1)
                             (match-end 1))))

       ;; This finds the argstring for `condition-case'.
       ;; I don't know if there are any others with the same pattern.
       ((string-match (format "^Usage looks like \\((%s[^\n)]*)\\)\\.$" fn)
                      docstring)
        ;; end does not include trailing ")" sequence.
        (setq end (- (match-end 1) 1))
        (if (string-match " +" docstring (match-beginning 1))
            (setq doc (substring docstring (match-end 0) end))
          (setq doc "")))

       ;; This finds the argstring for `setq-default'.
       ;; I don't know if there are any others with the same pattern.
       ((string-match (format "^[ \t]+\\((%s[^\n)]*)\\)$" fn) docstring)
        ;; end does not include trailing ")" sequence.
        (setq end (- (match-end 1) 1))
        (if (string-match " +" docstring (match-beginning 1))
            (setq doc (substring docstring (match-end 0) end))
          (setq doc ""))))

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
