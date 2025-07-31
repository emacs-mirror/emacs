;;; eldoc.el --- Show function arglist or variable docstring in echo area  -*- lexical-binding:t; -*-

;; Copyright (C) 1996-2025 Free Software Foundation, Inc.

;; Author: Noah Friedman <friedman@splode.com>
;; Keywords: extensions
;; Created: 1995-10-06
;; Version: 1.16.0
;; Package-Requires: ((emacs "26.3"))

;; This is a GNU ELPA :core package.  Avoid functionality that is not
;; compatible with the version of Emacs recorded above.

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

;; This mode is now enabled by default in all major modes that provide
;; support for it, such as `emacs-lisp-mode'.
;; This is controlled by `global-eldoc-mode'.

;; Major modes for other languages may use ElDoc by adding an
;; appropriate function to the buffer-local value of
;; `eldoc-documentation-functions'.

;;; Code:

(eval-when-compile (require 'cl-lib))

(defgroup eldoc nil
  "Show function arglist or variable docstring in echo area."
  :group 'lisp
  :group 'extensions)

(defcustom eldoc-idle-delay 0.50
  "Number of seconds of idle time to wait before displaying documentation.
If user input arrives before this interval of time has elapsed after the
last input event, no documentation will be displayed.

If this variable is set to 0, display the documentation without any delay."
  :type 'number)

(defcustom eldoc-print-after-edit nil
  "If non-nil, eldoc info is only shown after editing commands.
Changing the value requires toggling `eldoc-mode'."
  :type 'boolean
  :version "24.4")

(defcustom eldoc-echo-area-display-truncation-message t
  "If non-nil, provide verbose help when a message has been truncated.
When this is non-nil, and the documentation string was truncated to
fit in the echo-area, the documentation will be followed by an
explanation of how to display the full documentation text.
If nil, truncated messages will just have \"...\" to indicate truncation."
  :type 'boolean
  :version "28.1")

;;;###autoload
(defcustom eldoc-minor-mode-string " ElDoc"
  "String to display in mode line when ElDoc Mode is enabled; nil for none."
  :type '(choice string (const :tag "None" nil)))

(defcustom eldoc-argument-case #'identity
  "Case to display argument names of functions, as a symbol.
This has two preferred values: `upcase' or `downcase'.
Actually, any name of a function which takes a string as an argument and
returns another string is acceptable.

Note that this variable has no effect, unless
`eldoc-documentation-strategy' handles it explicitly."
  :type '(radio (function-item upcase)
		(function-item downcase)
                function))
(make-obsolete-variable 'eldoc-argument-case nil "25.1")

(defcustom eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit
  "Allow long ElDoc doc strings to resize echo area display.
If the value is t, never attempt to truncate messages, even if the
echo area must be resized to fit.  In that case, Emacs will resize
the mini-window up to the limit set by `max-mini-window-height'.

If the value is a positive number, it is used to calculate a
number of screen lines of documentation that ElDoc is allowed to
put in the echo area.  A positive integer specifies the maximum
number of lines directly, while a floating-point number specifies
the number of screen lines as a fraction of the echo area frame's
height.

If the value is the symbol `truncate-sym-name-if-fit', the part of
the doc string that represents a symbol's name may be truncated
if it will enable the rest of the doc string to fit on a single
line, without resizing the echo area.

If the value is nil, a doc string is always truncated to fit in a
single screen line of echo-area display.

Any resizing of the echo area additionally respects
`max-mini-window-height'."
  :type '(radio (const   :tag "Always" t)
                (float   :tag "Fraction of frame height" 0.25)
                (integer :tag "Number of lines" 5)
                (const   :tag "Never" nil)
                (const   :tag "Yes, but ask major-mode to truncate
 symbol names if it will\ enable argument list to fit on one
 line" truncate-sym-name-if-fit)))

(defcustom eldoc-echo-area-prefer-doc-buffer nil
  "Prefer ElDoc's documentation buffer if it is displayed in some window.
If this variable's value is t, ElDoc will skip showing
documentation in the echo area if the dedicated documentation
buffer (displayed by `eldoc-doc-buffer') is already displayed in
some window.  If the value is the symbol `maybe', then the echo area
is only skipped if the documentation needs to be truncated there."
  :type '(choice (const :tag "Prefer ElDoc's documentation buffer" t)
                 (const :tag "Prefer echo area" nil)
                 (const :tag "Skip echo area if truncating" maybe))
  :version "28.1")

(defcustom eldoc-help-at-pt nil
  "If non-nil, show `help-at-pt-kbd-string' at point via Eldoc.
This setting is an alternative to `help-at-pt-display-when-idle'.  If
the value is non-nil, `eldoc-show-help-at-pt' will show help-at-point
via Eldoc."
  :type 'boolean
  :version "31.1")

(defface eldoc-highlight-function-argument
  '((t (:inherit bold)))
  "Face used for the argument at point in a function's argument list.
Note that this face has no effect unless the `eldoc-documentation-strategy'
handles it explicitly.")

;;; No user options below here.

(defvar eldoc-message-commands-table-size 31
  "Used by `eldoc-add-command' to initialize `eldoc-message-commands' obarray.
It should probably never be necessary to do so, but if you
choose to increase the number of buckets, you must do so before loading
this file since the obarray is initialized at load time.
Remember to keep it a prime number to improve hash performance.")

(defvar eldoc-message-commands
  (obarray-make eldoc-message-commands-table-size)
  "Commands after which it is appropriate to print in the echo area.
ElDoc does not try to print function arglists, etc., after just any command,
because some commands print their own messages in the echo area and these
functions would instantly overwrite them.  But `self-insert-command' as well
as most motion commands are good candidates.
This variable contains an obarray of symbols; do not manipulate it
directly.  Instead, use `eldoc-add-command' and `eldoc-remove-command'.")

;; Not a constant.
(defvar eldoc-last-data (make-vector 3 nil)
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
  "The function used by `eldoc--message' to display messages.
It should receive the same arguments as `message'.")

(defun eldoc-edit-message-commands ()
  "Return an obarray containing common editing commands.

When `eldoc-print-after-edit' is non-nil, ElDoc messages are only
printed after commands contained in this obarray."
  (let ((cmds (obarray-make 31))
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

ElDoc mode is a buffer-local minor mode.  When enabled, the echo
area displays information about a function or variable in the
text where point is.  If point is on a documented variable, it
displays the first line of that variable's doc string.  Otherwise
it displays the argument list of the function called in the
expression point is on." :lighter eldoc-minor-mode-string
  (setq eldoc-last-message nil)
  (cond
   ((not (eldoc--supported-p))
    (when (called-interactively-p 'any)
      (message "There is no ElDoc support in this buffer"))
    (setq eldoc-mode nil))
   (eldoc-mode
    (when eldoc-print-after-edit
      (setq-local eldoc-message-commands (eldoc-edit-message-commands)))
    (add-hook 'post-command-hook #'eldoc-schedule-timer nil t)
    (add-hook 'pre-command-hook #'eldoc-pre-command-refresh-echo-area nil t))
   (t
    (kill-local-variable 'eldoc-message-commands)
    (remove-hook 'post-command-hook #'eldoc-schedule-timer t)
    (remove-hook 'pre-command-hook #'eldoc-pre-command-refresh-echo-area t)
    (when eldoc-timer
      (cancel-timer eldoc-timer)
      (setq eldoc-timer nil)))))

;;;###autoload
(define-globalized-minor-mode global-eldoc-mode eldoc-mode turn-on-eldoc-mode
  :initialize 'custom-initialize-delay
  :init-value t
  ;; For `read--expression', the usual global mode mechanism of
  ;; `change-major-mode-hook' runs in the minibuffer before
  ;; `eldoc-documentation-strategy' is set, so `turn-on-eldoc-mode'
  ;; does nothing.  Configure and enable eldoc from
  ;; `eval-expression-minibuffer-setup-hook' instead.
  (if global-eldoc-mode
      (add-hook 'eval-expression-minibuffer-setup-hook
                #'eldoc--eval-expression-setup)
    (remove-hook 'eval-expression-minibuffer-setup-hook
                 #'eldoc--eval-expression-setup)))

(defun eldoc--eval-expression-setup ()
  ;; Setup `eldoc', similar to `emacs-lisp-mode'.  FIXME: Call
  ;; `emacs-lisp-mode' itself?
  (cond ((<= emacs-major-version 27)
         (declare-function elisp-eldoc-documentation-function "elisp-mode")
         (with-no-warnings
           (add-function :before-until (local 'eldoc-documentation-function)
                         #'elisp-eldoc-documentation-function)))
        (t (add-hook 'eldoc-documentation-functions
                     #'elisp-eldoc-var-docstring nil t)
           (add-hook 'eldoc-documentation-functions
                     #'elisp-eldoc-funcall nil t)
           (setq-local eldoc-documentation-strategy
                       'eldoc-documentation-default)))
  (eldoc-mode +1))

;;;###autoload
(defun turn-on-eldoc-mode ()
  "Turn on `eldoc-mode' if the buffer has ElDoc support enabled.
See `eldoc-documentation-strategy' for more detail."
  (when (eldoc--supported-p)
    (eldoc-mode 1)))


(defun eldoc--update ()
  (when (or eldoc-mode
            (and global-eldoc-mode
                 (eldoc--supported-p)))
    ;; Don't ignore, but also don't full-on signal errors
    (with-demoted-errors "eldoc error: %s"
      (eldoc-print-current-symbol-info)) ))

(defun eldoc-schedule-timer ()
  "Ensure `eldoc-timer' is running.

If the user has changed `eldoc-idle-delay', update the timer to
reflect the change."
  (or (and eldoc-timer
           (memq eldoc-timer timer-idle-list)) ;FIXME: Why?
      (setq eldoc-timer
            (run-with-idle-timer
	     eldoc-idle-delay nil
             #'eldoc--update)))

  ;; If user has changed the idle delay, update the timer.
  (cond ((not (= eldoc-idle-delay eldoc-current-idle-delay))
         (setq eldoc-current-idle-delay eldoc-idle-delay)
         (timer-set-idle-time eldoc-timer eldoc-idle-delay t))))

(defvar eldoc-mode-line-string nil)
(put 'eldoc-mode-line-string 'risky-local-variable t)

(defun eldoc-minibuffer-message (format-string &rest args)
  "Display message specified by FORMAT-STRING and ARGS on the mode-line as needed.
This function displays the message produced by formatting ARGS
with FORMAT-STRING on the mode line when the current buffer is a minibuffer.
Otherwise, it displays the message like `message' would."
  (if (or (bound-and-true-p edebug-mode) (minibufferp))
      (progn
        (add-hook 'post-command-hook #'eldoc-minibuffer--cleanup)
	(with-current-buffer
	    (window-buffer
	     (or (window-in-direction 'above (minibuffer-window))
		 (minibuffer-selected-window)
		 (get-largest-window)))
          (when (and mode-line-format
                     (not (and (listp mode-line-format)
                               (assq 'eldoc-mode-line-string mode-line-format))))
	    (setq mode-line-format
                  (funcall
                   (if (listp mode-line-format) #'append #'list)
                   (list "" '(eldoc-mode-line-string
			      (" " eldoc-mode-line-string " ")))
                   mode-line-format)))
          (setq eldoc-mode-line-string
                (when (stringp format-string)
                  (apply #'format-message format-string args)))
          (force-mode-line-update)))
    (apply #'message format-string args)))

(defun eldoc-minibuffer--cleanup ()
  (unless (or (bound-and-true-p edebug-mode) (minibufferp))
    (setq eldoc-mode-line-string nil
          ;; https://debbugs.gnu.org/16920
          eldoc-last-message nil)
    (remove-hook 'post-command-hook #'eldoc-minibuffer--cleanup)))

(make-obsolete
 'eldoc-message "use `eldoc-documentation-functions' instead." "eldoc-1.1.0")
(defun eldoc-message (&optional string) (eldoc--message string))
(defun eldoc--message (&optional string)
  "Display STRING as an ElDoc message if it's non-nil.

Also store it in `eldoc-last-message' and return that value."
  (let ((omessage eldoc-last-message))
    (setq eldoc-last-message string)
    ;; Do not put eldoc messages in the log since they are Legion.
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

;; This function goes on pre-command-hook.
;; Motion commands clear the echo area for some reason,
;; which make eldoc messages flicker or disappear just before motion
;; begins.  This function reprints the last eldoc message immediately
;; before the next command executes, which does away with the flicker.
;; This doesn't seem to be required for Emacs 19.28 and earlier.
;; FIXME: The above comment suggests we don't really understand why
;; this is needed.  Maybe it's not needed any more, but if it is
;; we should figure out why.
(defun eldoc-pre-command-refresh-echo-area ()
  "Reprint `eldoc-last-message' in the echo area."
  (and eldoc-last-message
       (not (minibufferp))      ;We don't use the echo area when in minibuffer.
       (if (and (eldoc-display-message-no-interference-p)
		(eldoc--message-command-p this-command))
	   (eldoc--message eldoc-last-message)
         ;; No need to call eldoc--message since the echo area will be cleared
         ;; for us, but do note that the last-message will be gone.
         (setq eldoc-last-message nil))))

;; The point of `eldoc--request-state' is not to over-request, which
;; can happen if the idle timer is restarted on execution of command
;; which is guaranteed not to change the conditions that warrant a new
;; request for documentation.
(defvar eldoc--last-request-state nil
  "Tuple containing information about last ElDoc request.")
(defun eldoc--request-state ()
  "Compute information to store in `eldoc--last-request-state'."
  (list (current-buffer) (buffer-modified-tick) (point)))

(defun eldoc-display-message-p ()
  "Tell if ElDoc can use the echo area."
  (and (eldoc-display-message-no-interference-p)
       (not this-command)
       (eldoc--message-command-p last-command)))

(make-obsolete 'eldoc-display-message-p
               "Use `eldoc-documentation-functions' instead."
               "eldoc-1.6.0")

;; Check various conditions about the current environment that might make
;; it undesirable to print eldoc messages right this instant.
(defun eldoc-display-message-no-interference-p ()
  "Return nil if displaying a message would cause interference."
  (not (or executing-kbd-macro
           ;; The following configuration shows "Matches..." in the
           ;; echo area when point is after a closing bracket, which
           ;; conflicts with eldoc.
           (and (boundp 'show-paren-context-when-offscreen)
                show-paren-context-when-offscreen
                ;; There's no conflict with the child-frame and
                ;; overlay versions.
                (not (memq show-paren-context-when-offscreen
                           '(child-frame overlay)))
                (not (pos-visible-in-window-p
                      (overlay-end show-paren--overlay)))))))


(defvar eldoc-documentation-functions (list #'eldoc-show-help-at-pt)
  "Hook of functions that produce doc strings.

A doc string is typically relevant if point is on a function-like
name, inside its arg list, or on any object with some associated
information.

Each hook function is called with at least one argument CALLBACK,
a function, and decides whether to display a short doc string
about the context around point.

- If that decision can be taken quickly, the hook function may
  call CALLBACK immediately, following the protocol described
  below.  Alternatively, it may ignore CALLBACK entirely and
  return either the doc string, or nil if there's no doc
  appropriate for the context.

- If the computation of said doc string (or the decision whether
  there is one at all) is expensive or can't be performed
  directly, the hook function should return a non-nil, non-string
  value and arrange for CALLBACK to be called at a later time,
  using asynchronous processes or other asynchronous mechanisms.

To call the CALLBACK function, the hook function must pass it an
obligatory argument DOCSTRING, a string containing the
documentation, followed by an optional list of arbitrary
keyword-value pairs of the form (:KEY VALUE :KEY2 VALUE2...).
The information contained in these pairs is understood by members
of `eldoc-display-functions', allowing the
documentation-producing backend to cooperate with specific
documentation-displaying frontends.  For example, KEY can be:

* `:thing', VALUE being a short string or symbol designating what
  DOCSTRING reports on.  It can, for example be the name of the
  function whose signature is being documented, or the name of
  the variable whose docstring is being documented.
  `eldoc-display-in-echo-area', a member of
  `eldoc-display-functions', sometimes omits this information
  depending on space constraints;

* `:face', VALUE being a symbol designating a face which both
  `eldoc-display-in-echo-area' and `eldoc-display-in-buffer' will
  use when displaying `:thing''s value.

* `:echo', controlling how `eldoc-display-in-echo-area' should
  present this documentation item in the echo area, to save
  space.  If VALUE is a string, echo it instead of DOCSTRING.  If
  a number, only echo DOCSTRING up to that character position.
  If `skip', don't echo DOCSTRING at all.

The additional KEY `:origin' is always added by ElDoc, its VALUE
being the member of `eldoc-documentation-functions' where
DOCSTRING originated.  `eldoc-display-functions' may use this
information to organize display of multiple docstrings.

Finally, major modes should modify this hook locally, for
example:
  (add-hook \\='eldoc-documentation-functions #\\='foo-mode-eldoc nil t)
so that the global value (i.e. the default value of the hook) is
taken into account if the major mode specific function does not
return any documentation.")

(defvar eldoc-display-functions
  '(eldoc-display-in-echo-area eldoc-display-in-buffer)
  "Hook of functions tasked with displaying ElDoc results.
Each function is passed two arguments: DOCS and INTERACTIVE.  DOCS
is a list (DOC ...) where DOC looks like (STRING :KEY VALUE :KEY2
VALUE2 ...).  STRING is a string containing the documentation's
text and the remainder of DOC is an optional list of
keyword-value pairs denoting additional properties of that
documentation.  For commonly recognized properties, see
`eldoc-documentation-functions'.

INTERACTIVE says if the request to display doc strings came
directly from the user or from ElDoc's automatic mechanisms'.

The display functions always run in the source buffer which initiated
the documentation request.  If the source buffer is killed, the display
functions just won't run.")

(defvar eldoc--doc-buffer nil "Buffer displaying latest ElDoc-produced docs.")

(defun eldoc-doc-buffer (&optional interactive)
  "Get or display ElDoc documentation buffer.

The buffer holds the results of the last documentation request.
If INTERACTIVE, display it.  Else, return said buffer."
  (interactive (list t))
  (unless (buffer-live-p eldoc--doc-buffer)
    (user-error (format
                 "ElDoc buffer doesn't exist, maybe `%s' to produce one."
                 (substitute-command-keys "\\[eldoc]"))))
  (with-current-buffer eldoc--doc-buffer
    (cond (interactive
           (rename-buffer (replace-regexp-in-string "^ *" ""
                                                    (buffer-name)))
           (display-buffer (current-buffer)))
          (t (current-buffer)))))

(defvar eldoc-doc-buffer-separator
  (concat "\n" (propertize "\n" 'face '(:inherit separator-line :extend t)) "\n")
  "String used to separate items in Eldoc documentation buffer.")

(defun eldoc--format-doc-buffer (docs)
  "Ensure DOCS are displayed in an *eldoc* buffer."
  (with-current-buffer (if (buffer-live-p eldoc--doc-buffer)
                           eldoc--doc-buffer
                         (setq eldoc--doc-buffer
                               (get-buffer-create " *eldoc*")))
    (let ((inhibit-read-only t)
          (things-reported-on))
      (special-mode)
      (erase-buffer)
      (setq-local nobreak-char-display nil)
      (cl-loop for (docs . rest) on docs
               for (this-doc . plist) = docs
               for thing = (plist-get plist :thing)
               when thing do
               (cl-pushnew thing things-reported-on)
               (setq this-doc
                     (concat
                      (propertize (format "%s" thing)
                                  'face (plist-get plist :face))
                      ": "
                      this-doc))
               do (insert this-doc)
               when rest do
               (insert eldoc-doc-buffer-separator)
               finally (goto-char (point-min)))
      ;; Rename the buffer, taking into account whether it was
      ;; hidden or not
      (rename-buffer (format "%s*eldoc%s*"
                             (if (string-match "^ " (buffer-name)) " " "")
                             (if things-reported-on
                                 (format " for %s"
                                         (mapconcat
                                          (lambda (s) (format "%s" s))
                                          things-reported-on
                                          ", "))
                               "")))))
  eldoc--doc-buffer)

(defun eldoc--echo-area-render (docs)
  "Similar to `eldoc--format-doc-buffer', but for echo area.
Helper for `eldoc-display-in-echo-area'."
  (cl-loop for (item . rest) on docs
           for (this-doc . plist) = item
           for echo = (plist-get plist :echo)
           for thing = (plist-get plist :thing)
           unless (eq echo 'skip) do
           (setq this-doc
                 (cond ((integerp echo) (substring this-doc 0 echo))
                       ((stringp echo) echo)
                       (t this-doc)))
           (when thing (setq this-doc
                             (concat
                              (propertize (format "%s" thing)
                                          'face (plist-get plist :face))
                              ": "
                              this-doc)))
           (insert this-doc)
           (when rest (insert "\n"))))

(defun eldoc--echo-area-substring (available)
  "Given AVAILABLE lines, get buffer substring to display in echo area.
Helper for `eldoc-display-in-echo-area'."
  (let ((start (prog1 (progn
                        (goto-char (point-min))
                        (skip-chars-forward " \t\n")
                        (point))
                 (forward-visible-line (1- available))
                 (end-of-visible-line)
                 (skip-chars-backward " \t\n")))
        (truncated (save-excursion
                     (skip-chars-forward " \t\n")
                     (not (eobp)))))
    (cond ((eldoc--echo-area-prefer-doc-buffer-p truncated)
           nil)
          ((and truncated
                (> available 1)
                eldoc-echo-area-display-truncation-message)
           (forward-visible-line -1)
           (end-of-visible-line)
           (concat (buffer-substring start (point))
                   (format
                    "\n(Documentation truncated. Use `%s' to see rest)"
                    (substitute-command-keys "\\[eldoc-doc-buffer]"))))
          (t
           (buffer-substring start (point))))))

(defun eldoc--echo-area-prefer-doc-buffer-p (truncatedp)
  "Tell if display in the echo area should be skipped.
Helper for `eldoc-display-in-echo-area'.  If TRUNCATEDP the
documentation to potentially appear in the echo area is
known to be truncated."
  (and (or (eq eldoc-echo-area-prefer-doc-buffer t)
           (and truncatedp
                (eq eldoc-echo-area-prefer-doc-buffer
                    'maybe)))
       (get-buffer-window eldoc--doc-buffer t)))

(defun eldoc-display-in-echo-area (docs interactive)
  "Display DOCS in echo area.
INTERACTIVE is non-nil if user explicitly invoked ElDoc.  Honor
`eldoc-echo-area-use-multiline-p' and
`eldoc-echo-area-prefer-doc-buffer'."
  (cond
   ((and (not interactive)
         ;; When called non-interactively, check if we have permission
         ;; to mess with echo area at all.  For example, if
         ;; this-command is non-nil while running via an idle timer,
         ;; we're still in the middle of executing a command, e.g. a
         ;; query-replace where it would be annoying to overwrite the
         ;; echo area.
         (or
          (not (eldoc-display-message-no-interference-p))
          this-command
          (not (eldoc--message-command-p last-command)))))
   (;; If nothing to report, clear the echo area.
    (null docs)
    (eldoc--message nil))
   (t
    ;; Otherwise, proceed to change the echo area.  Start by
    ;; establishing some parameters.
    (let*
        ((width (1- (window-width (minibuffer-window))))
         (val (if (and (symbolp eldoc-echo-area-use-multiline-p)
                       eldoc-echo-area-use-multiline-p)
                  max-mini-window-height
                eldoc-echo-area-use-multiline-p))
         (available (cl-typecase val
                      (float (truncate (* (frame-height) val)))
                      (integer val)
                      (t 'just-one-line)))
         single-doc single-doc-sym)
      (let ((echo-area-message
             (cond
              (;; To output to the echo area, we handle the
               ;; `truncate-sym-name-if-fit' special case first, by
               ;; checking for a lot of special conditions.
               (and
                (eq 'truncate-sym-name-if-fit eldoc-echo-area-use-multiline-p)
                (null (cdr docs))
                (setq single-doc (caar docs))
                (setq single-doc-sym
                      (format "%s" (plist-get (cdar docs) :thing)))
                (< (length single-doc) width)
                (not (string-match "\n" single-doc))
                (> (+ (length single-doc) (length single-doc-sym) 2) width))
               single-doc)
              ((and (numberp available)
                    (cl-plusp available))
               ;; Else, given a positive number of logical lines, grab
               ;; as many as we can.
               (with-temp-buffer
                 (eldoc--echo-area-render docs)
                 (eldoc--echo-area-substring available)))
              (t ;; this is the "truncate brutally" situation
               (let ((string
                      (with-temp-buffer
                        (eldoc--echo-area-render docs)
                        (buffer-substring (goto-char (point-min))
                                          (progn (end-of-visible-line)
                                                 (point))))))
                 (if (> (length string) width)  ; truncation to happen
                     (unless (eldoc--echo-area-prefer-doc-buffer-p t)
                       (truncate-string-to-width string width))
                   (unless (eldoc--echo-area-prefer-doc-buffer-p nil)
                     string)))))))
        (when echo-area-message
          (eldoc--message echo-area-message)))))))

(defun eldoc-display-in-buffer (docs interactive)
  "Display DOCS in a dedicated buffer.
If INTERACTIVE is t, also display the buffer."
  (eldoc--format-doc-buffer docs)
  (when interactive (eldoc-doc-buffer t)))

(defun eldoc-documentation-default ()
  "Show the first non-nil documentation string for item at point.
This is the default value for `eldoc-documentation-strategy'."
  (run-hook-wrapped 'eldoc-documentation-functions
                    (lambda (f)
                      (funcall f (eldoc--make-callback :eager f)))))

(defun eldoc-documentation-compose ()
  "Show multiple documentation strings together after waiting for all of them.
This is meant to be used as a value for `eldoc-documentation-strategy'."
  (let (fns-and-callbacks)
    ;; Make all the callbacks, setting up state inside
    ;; `eldoc--invoke-strategy' to know how many callbacks to wait for
    ;; before displaying the result (bug#62816).
    (run-hook-wrapped 'eldoc-documentation-functions
                      (lambda (f)
                        (push (cons f (eldoc--make-callback :patient f))
                              fns-and-callbacks)
                        nil))
    ;; Now call them.  The last one will trigger the display.
    (cl-loop for (f . callback) in fns-and-callbacks
             for str = (funcall f callback)
             when (or (null str) (stringp str)) do (funcall callback str)))
  t)

(defun eldoc-documentation-compose-eagerly ()
  "Show multiple documentation strings one by one as soon as possible.
This is meant to be used as a value for `eldoc-documentation-strategy'."
  (run-hook-wrapped 'eldoc-documentation-functions
                    (lambda (f)
                      (let* ((callback (eldoc--make-callback :eager f))
                             (str (funcall f callback)))
                        (if (or (null str) (stringp str)) (funcall callback str))
                        nil)))
  t)

(defun eldoc-documentation-enthusiast ()
  "Show most important documentation string produced so far.
This is meant to be used as a value for `eldoc-documentation-strategy'."
  (run-hook-wrapped 'eldoc-documentation-functions
                    (lambda (f)
                      (let* ((callback (eldoc--make-callback :enthusiast f))
                             (str (funcall f callback)))
                        (if (stringp str) (funcall callback str))
                        nil)))
  t)

;; JT@2020-07-10: ElDoc is pre-loaded, so in Emacs < 28 we can't
;; make the "old" `eldoc-documentation-function' point to the new
;; `eldoc-documentation-strategy', so we do the reverse.  This allows
;; for ElDoc to be loaded in those older Emacs versions and work with
;; whomever (major-modes, extensions, user) sets one or the other
;; variable.
(defmacro eldoc--documentation-strategy-defcustom
    (main secondary value docstring &rest more)
  "Defcustom helper macro for sorting `eldoc-documentation-strategy'."
  (declare (indent 2))
  `(if (< emacs-major-version 28)
       (progn
         (defcustom ,secondary ,value ,docstring ,@more)
         (define-obsolete-variable-alias ',main ',secondary "eldoc-1.1.0"))
       (progn
         (defcustom ,main ,value ,docstring  ,@more)
         (defvaralias ',secondary ',main ,docstring))))

(eldoc--documentation-strategy-defcustom eldoc-documentation-strategy
    eldoc-documentation-function
  #'eldoc-documentation-default
  "How to collect and display results of `eldoc-documentation-functions'.

This variable controls how to call the functions in the special hook
`eldoc-documentation-functions', and how to organize their results
for display to the user.  The functions in `eldoc-documentation-functions'
are the source of documentation, and act as back-end for ElDoc.

The following values are supported:

- `eldoc-documentation-default': Call functions in the special
  hook in order, until one of them returns a non-nil string
  value.  Display only that string.

- `eldoc-documentation-compose': Call all the functions in the
  special hook and display all of the resulting strings together,
  after all of the functions were called, and in the order of the
  functions in the hook.

- `eldoc-documentation-compose-eagerly': Call all the functions in
  the special hook, and display each non-nil string as soon as it
  is returned by a function, before calling the next function.

- `eldoc-documentation-enthusiast': Call all the functions in the
  special hook, and display only the most important resulting
  string at any given time.  A function appearing first in
  the special hook is considered more important than those which
  appear after it.

This variable can also be set to a function of no arguments that
returns something other than a string or nil, and allows for some
or all of the special hook `eldoc-documentation-functions' to be
run.  In that case, the strategy function should follow that
other variable's protocol closely and display the resulting doc
strings itself.

For backward compatibility with the \"old\" protocol, this variable
can also be set to a function that returns nil or a doc string,
depending whether or not there is documentation to display at
all."
  :link '(info-link "(emacs) Lisp Doc")
  :type '(radio (function-item eldoc-documentation-default)
                (function-item eldoc-documentation-compose)
                (function-item eldoc-documentation-compose-eagerly)
                (function-item eldoc-documentation-enthusiast)
                (function :tag "Other function"))
  :version "28.1")

(defun eldoc--supported-p ()
  "Non-nil if an ElDoc function is set for this buffer."
  (and (not (memq eldoc-documentation-strategy '(nil ignore)))
       (or eldoc-documentation-functions
           ;; The old API had major modes set `eldoc-documentation-function'
           ;; to provide eldoc support.  It's impossible now to determine
           ;; reliably whether the `eldoc-documentation-strategy' provides
           ;; eldoc support (as in the old API) or whether it just provides
           ;; a way to combine the results of the
           ;; `eldoc-documentation-functions' (as in the new API).
           ;; But at least if it's set buffer-locally it's a good hint that
           ;; there's some eldoc support in the current buffer.
           (local-variable-p 'eldoc-documentation-strategy))))

(defvar eldoc--enthusiasm-curbing-timer nil
  "Timer used by the `eldoc-documentation-enthusiast' strategy.
When a doc string is encountered, it must endure a certain amount
of time unchallenged until it is displayed to the user.  This
prevents blinking if a lower priority docstring comes in shortly
before a higher priority one.")

(defalias 'eldoc #'eldoc-print-current-symbol-info)

;; This variable should be unbound, but that confuses
;; `describe-symbol' for some reason.
(defvar eldoc--make-callback nil "Helper for function `eldoc--make-callback'.")

;; JT@2020-07-08: the below docstring for the internal function
;; `eldoc--invoke-strategy' could be moved to
;; `eldoc-documentation-strategy' or thereabouts if/when we decide to
;; extend or publish the `make-callback' protocol.
(defun eldoc--make-callback (method origin)
  "Make callback suitable for `eldoc-documentation-functions'.
The return value is a function FN whose lambda list is (STRING
&rest PLIST) and can be called by those functions.  Its
responsibility is always to register the docstring STRING along
with options specified in PLIST as the documentation to display
for each particular situation.

METHOD specifies how the callback behaves relative to other
competing elements in `eldoc-documentation-functions'.  It can
have the following values:

- `:enthusiast' says to display STRING as soon as possible if
  there's no higher priority doc string;

- `:patient' says to display STRING along with all other
   competing strings but only when all of all
   `eldoc-documentation-functions' have been collected;

- `:eager' says to display STRING along with all other competing
  strings so far, as soon as possible.

ORIGIN is the member of `eldoc-documentation-functions' which
will be responsible for eventually calling the FN."
  (funcall eldoc--make-callback method origin))

(defun eldoc--invoke-strategy (interactive)
  "Invoke `eldoc-documentation-strategy' function.

If INTERACTIVE is non-nil, the request came directly from a user
command, otherwise it came from ElDoc's idle
timer, `eldoc-timer'.

That function's job is to run the `eldoc-documentation-functions'
special hook, using the `run-hook' family of functions.  ElDoc's
built-in strategy functions play along with the
`eldoc--make-callback' protocol, using it to produce a callback
argument to feed the functions that the user places in
`eldoc-documentation-functions'.  Whenever the strategy
determines it has information to display to the user, this
function passes responsibility to the functions in
`eldoc-display-functions'.

Other third-party values of `eldoc-documentation-strategy' should
not use `eldoc--make-callback'.  They must find some alternate
way to produce callbacks to feed to
`eldoc-documentation-functions' and should endeavor to display
the docstrings eventually produced, using
`eldoc-display-functions'."
  (let* (;; How many callbacks have been created by the strategy
         ;; function and passed to elements of
         ;; `eldoc-documentation-functions'.
         (howmany 0)
         ;; How many calls to callbacks we're still waiting on.  Used
         ;; by `:patient'.
         (want 0)
         ;; The doc strings and corresponding options registered so
         ;; far.
         (docs-registered '())
         (orig-buffer (current-buffer)))
    (cl-labels
        ((register-doc
          (pos string plist origin)
          (when (and string (> (length string) 0))
            (push (cons pos (cons string `(:origin ,origin ,@plist)))
                  docs-registered)))
         (display-doc
           ()
           (when (buffer-live-p orig-buffer)
             (with-current-buffer orig-buffer
               (run-hook-with-args
                'eldoc-display-functions (mapcar #'cdr
                                                 (setq docs-registered
                                                       (sort docs-registered
                                                             (lambda (a b) (< (car a) (car b))))))
                interactive))))
         (make-callback
          (method origin)
          (let ((pos (prog1 howmany (cl-incf howmany))))
            (cl-ecase method
              (:enthusiast
               (lambda (string &rest plist)
                 (when (and string (cl-loop for (p) in docs-registered
                                            never (< p pos)))
                   (setq docs-registered '())
                   (register-doc pos string plist origin))
                 (when (and (timerp eldoc--enthusiasm-curbing-timer)
                            (memq eldoc--enthusiasm-curbing-timer
                                  timer-list))
                   (cancel-timer eldoc--enthusiasm-curbing-timer))
                 (setq eldoc--enthusiasm-curbing-timer
                       (run-at-time (unless (zerop pos) 0.3)
                                    nil #'display-doc))
                 t))
              (:patient
               (cl-incf want)
               (lambda (string &rest plist)
                 (register-doc pos string plist origin)
                 (when (zerop (cl-decf want)) (display-doc))
                 t))
              (:eager
               (lambda (string &rest plist)
                 (register-doc pos string plist origin)
                 (display-doc)
                 t))))))
      (let* ((eldoc--make-callback #'make-callback)
             (res (funcall eldoc-documentation-strategy)))
        ;; Observe the old and the new protocol:
        (cond (;; Old protocol: got string, e-d-strategy is itself the
               ;; origin function, and we output immediately;
               (stringp res)
               (register-doc 0 res nil eldoc-documentation-strategy)
               (display-doc))
              (;; Old protocol: got nil, clear the echo area;
               (null res) (eldoc--message nil))
              (;; New protocol: trust callback will be called;
               t))))))

(defun eldoc-print-current-symbol-info (&optional interactive)
  "Document thing at point."
  (interactive '(t))
  (let (token)
    (cond (interactive
           (eldoc--invoke-strategy t))
          ((not (equal (setq token (eldoc--request-state))
                       eldoc--last-request-state))
           (let ((non-essential t))
             (setq eldoc--last-request-state token)
             (eldoc--invoke-strategy nil))))))

(defun eldoc-show-help-at-pt (&rest _)
  "Show help at point via Eldoc if `eldoc-help-at-pt' is non-nil.
Intended for `eldoc-documentation-functions' (which see)."
  (when-let* ((help (and eldoc-help-at-pt (help-at-pt-kbd-string))))
    (format "Help: %s" (substitute-command-keys help))))


;; This section only affects ElDoc output to the echo area, as in
;; `eldoc-display-in-echo-area'.
;;
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
 "comment-indent-new-line" "delete-char" "back-to-indentation"
 "backward-" "beginning-of-" "delete-other-windows" "delete-window"
 "down-list" "end-of-" "exchange-point-and-mark" "forward-" "goto-"
 "handle-select-window" "indent-for-tab-command" "left-" "mark-page"
 "mark-paragraph" "mouse-set-point" "move-" "move-beginning-of-"
 "move-end-of-" "newline" "next-" "other-window" "pop-global-mark"
 "previous-" "recenter" "right-" "scroll-" "self-insert-command"
 "split-window-" "up-list" "touch-screen-handle-touch"
 "analyze-text-conversion")

(provide 'eldoc)

;;; eldoc.el ends here
