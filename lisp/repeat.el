;;; repeat.el --- convenient way to repeat the previous command  -*- lexical-binding: t -*-

;; Copyright (C) 1998, 2001-2026 Free Software Foundation, Inc.

;; Author: Will Mengarini <seldon@eskimo.com>
;; Created: Mo 02 Mar 98
;; Old-Version: 0.51
;; Keywords: convenience, vi, repeat

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

;; Sometimes the fastest way to get something done is just to lean on a key;
;; moving forward through a series of words by leaning on M-f is an example.
;; But 'forward-page is orthodoxly bound to C-x ], so moving forward through
;; several pages requires
;;   Loop until desired page is reached:
;;     Hold down control key with left pinkie.
;;     Tap <x>.
;;     Lift left pinkie off control key.
;;     Tap <]>.
;; This is a pain in the ass.

;; This package defines a command that repeats the preceding command,
;; whatever that was, including its arguments, whatever they were.
;; This command is connected to the key C-x z.
;; To repeat the previous command once, type C-x z.
;; To repeat it a second time immediately after, type just z.
;; By typing z again and again, you can repeat the command over and over.

;; This works correctly inside a keyboard macro as far as recording and
;; playback go, but `edit-kbd-macro' gets it wrong.  That shouldn't really
;; matter; if you need to edit something like
;;   C-x ]              ;; forward-page
;;   C-x z              ;; repeat
;;   zz                 ;; self-insert-command * 2
;;   C-x                ;; Control-X-prefix
;; you can just kill the bogus final 2 lines, then duplicate the repeat line
;; as many times as it's really needed.  Also, `edit-kbd-macro' works
;; correctly if `repeat' is invoked through a rebinding to a single keystroke
;; and the global variable repeat-on-final-keystroke is set to a value
;; that doesn't include that keystroke.  For example, the lines
;;   (keymap-global-set "C-z" #'repeat)
;;   (setopt repeat-on-final-keystroke "z")
;; in your .emacs would allow `edit-kbd-macro' to work correctly when C-z was
;; used in a keyboard macro to invoke `repeat', but would still allow C-x z
;; to be used for `repeat' elsewhere.  The real reason for documenting this
;; isn't that anybody would need it for the `edit-kbd-macro' problem, but
;; that there might be other unexpected ramifications of re-executing on
;; repetitions of the final keystroke, and this shows how to do workarounds.

;; If the preceding command had a prefix argument, that argument is applied
;; to the repeat command, unless the repeat command is given a new prefix
;; argument, in which case it applies that new prefix argument to the
;; preceding command.  This means a key sequence like C-u - C-x C-t can be
;; repeated.  (It shoves the preceding line upward in the buffer.)

;; Here are some other key sequences with which repeat might be useful:
;;   C-u - C-t      [shove preceding character backward in line]
;;   C-u - M-t      [shove preceding word backward in sentence]
;;         C-x ^    enlarge-window [one line] (assuming frame has > 1 window)
;;   C-u - C-x ^    [shrink window one line]
;;         C-x `    next-error
;;   C-u - C-x `    [previous error]
;;         C-x DEL  backward-kill-sentence
;;         C-x e    call-last-kbd-macro
;;         C-x r i  insert-register
;;         C-x r t  string-rectangle
;;         C-x TAB  indent-rigidly [one character]
;;   C-u - C-x TAB  [outdent rigidly one character]
;;         C-x {    shrink-window-horizontally
;;         C-x }    enlarge-window-horizontally

;;; Code:

(require 'cl-lib)

;;;;; ************************* USER OPTIONS ************************** ;;;;;

(defgroup repeat nil
  "Convenient way to repeat previous commands."
  :prefix "repeat-"
  :version "29.1"
  :group 'convenience)

(defcustom repeat-too-dangerous '(kill-this-buffer)
  "Commands too dangerous to repeat with \\[repeat]."
  :group 'repeat
  :type '(repeat function))

;; If the last command was self-insert-command, the char to be inserted was
;; obtained by that command from last-command-event, which has now been
;; clobbered by the command sequence that invoked `repeat'.  We could get it
;; from (recent-keys) & set last-command-event to that, "unclobbering" it, but
;; this has the disadvantage that if the user types a sequence of different
;; chars then invokes repeat, only the final char will be inserted.  In vi,
;; the dot command can reinsert the entire most-recently-inserted sequence.

(defvar repeat-message-function nil
  "If non-nil, function used by `repeat' command to say what it's doing.
Message is something like \"Repeating command glorp\".
A value of `ignore' will disable such messages.  To customize
display, assign a function that takes one string as an arg and
displays it however you want.
If this variable is nil, the normal `message' function will be
used to display the messages.")

(defcustom repeat-on-final-keystroke t
  "Allow `repeat' to re-execute for repeating lastchar of a key sequence.
If this variable is t, `repeat' determines what key sequence
it was invoked by, extracts the final character of that sequence, and
re-executes as many times as that final character is hit; so for example
if `repeat' is bound to C-x z, typing C-x z z z repeats the previous command
3 times.  If this variable is a sequence of characters, then re-execution
only occurs if the final character by which `repeat' was invoked is a
member of that sequence.  If this variable is nil, no re-execution occurs."
  :group 'repeat
  :type '(choice (const :tag "Repeat for all keys" t)
		 (const :tag "Don't repeat" nil)
		 (sexp :tag "Repeat for specific keys")))

;;;;; ****************** HACKS TO THE REST OF EMACS ******************* ;;;;;

;; The basic strategy is to use last-command, a variable built in to Emacs.
;; There are 2 issues that complicate this strategy.  The first is that
;; last-command is given a bogus value when any kill command is executed;
;; this is done to make it easy for `yank-pop' to know that it's being invoked
;; after a kill command.  The second is that the meaning of the command is
;; often altered by the prefix arg, but although Emacs (19.34) has a
;; builtin prefix-arg specifying the arg for the next command, as well as a
;; builtin current-prefix-arg, it has no builtin last-prefix-arg.

;; There's a builtin (this-command-keys), the return value of which could be
;; executed with (command-execute), but there's no (last-command-keys).
;; Using (last-command-keys) if it existed wouldn't be optimal, however,
;; since it would complicate checking membership in repeat-too-dangerous.

;; It would of course be trivial to implement last-prefix-arg &
;; true-last-command by putting something in post-command-hook, but that
;; entails a performance hit; the approach taken below avoids that.

;; Coping with strings of self-insert commands gets hairy when they interact
;; with auto-filling.  Most problems are eliminated by remembering what we're
;; self-inserting, so we only need to get it from the undo information once.

;; With Emacs 22.2 the variable `last-repeatable-command' stores the
;; most recently executed command that was not bound to an input event.
;; `repeat' now repeats that command instead of `real-last-command' to
;; avoid a "... must be bound to an event with parameters" error.

;;;;; *************** ANALOGOUS HACKS TO `repeat' ITSELF **************** ;;;;;

;; That mechanism of checking num-input-keys to figure out what's really
;; going on can be useful to other commands that need to fine-tune their
;; interaction with repeat.  Instead of requiring them to advise repeat, we
;; can just defvar the value they need here, & setq it in the repeat command:

(defvar repeat-num-input-keys-at-repeat -1
  "# key sequences read in Emacs session when `repeat' last invoked.")

;; Also, we can assign a name to the test for which that variable is
;; intended, which thereby documents here how to use it, & makes code that
;; uses it self-documenting:

(defsubst repeat-is-really-this-command ()
  "Return t if this command is happening because user invoked `repeat'.
Usually, when a command is executing, the Emacs builtin variable
`this-command' identifies the command the user invoked.  Some commands modify
that variable on the theory they're doing more good than harm; `repeat' does
that, and usually does do more good than harm.  However, like all do-gooders,
sometimes `repeat' gets surprising results from its altruism.  The value of
this function is always whether the value of `this-command' would've been
`repeat' if `repeat' hadn't modified it."
  (= repeat-num-input-keys-at-repeat num-input-keys))

;; An example of the use of (repeat-is-really-this-command) may still be
;; available in <https://www.eskimo.com/~seldon/dotemacs.el>; search for
;; "defun wm-switch-buffer".

;;;;; ******************* THE REPEAT COMMAND ITSELF ******************* ;;;;;

(defvar repeat-previous-repeated-command nil
  "The previous repeated command.")

;;;###autoload
(defun repeat (repeat-arg)
  "Repeat most recently executed command.
If REPEAT-ARG is non-nil (interactively, with a prefix argument),
supply a prefix argument to that command.  Otherwise, give the
command the same prefix argument it was given before, if any.

If this command is invoked by a multi-character key sequence, it
can then be repeated by repeating the final character of that
sequence.  This behavior can be modified by the global variable
`repeat-on-final-keystroke'.

`repeat' ignores commands bound to input events.  Hence the term
\"most recently executed command\" shall be read as \"most
recently executed command not bound to an input event\"."
  ;; The most recently executed command could be anything, so surprises could
  ;; result if it were re-executed in a context where new dynamically
  ;; localized variables were shadowing global variables in a `let' clause in
  ;; here.  (Remember that GNU Emacs 19 is dynamically localized.)
  ;; To avoid that, I tried the `lexical-let' of the Common Lisp extensions,
  ;; but that entails a very noticeable performance hit, so instead I use the
  ;; "repeat-" prefix, reserved by this package, for *local* variables that
  ;; might be visible to re-executed commands, including this function's arg.
  (interactive "P")
  (when (eq last-repeatable-command 'repeat)
    (setq last-repeatable-command repeat-previous-repeated-command))
  (cond
   ((null last-repeatable-command)
    (error "There is nothing to repeat"))
   ((eq last-repeatable-command 'mode-exit)
    (error "`last-repeatable-command' is `mode-exit' and can't be repeated"))
   ((memq last-repeatable-command repeat-too-dangerous)
    (error "Command %S too dangerous to repeat automatically"
	   last-repeatable-command)))
  (setq this-command last-repeatable-command
	repeat-previous-repeated-command last-repeatable-command
        repeat-num-input-keys-at-repeat num-input-keys)
  (when (null repeat-arg)
    (setq repeat-arg last-prefix-arg))
  ;; Now determine whether to loop on repeated taps of the final character
  ;; of the key sequence that invoked repeat.  The Emacs global
  ;; last-command-event contains the final character now, but may not still
  ;; contain it after the previous command is repeated, so the character
  ;; needs to be saved.
  (let ((repeat-repeat-char
         (if (eq repeat-on-final-keystroke t)
	     last-command-event
           ;; Allow only specified final keystrokes.
           (car (memq last-command-event
                      (listify-key-sequence
                       repeat-on-final-keystroke))))))
    (if (eq last-repeatable-command (caar command-history))
        (let ((repeat-command (car command-history)))
          (repeat-message "Repeating %S" repeat-command)
          (eval repeat-command))
      (if (null repeat-arg)
          (repeat-message "Repeating command %S" last-repeatable-command)
        (setq current-prefix-arg repeat-arg)
        (repeat-message
	 "Repeating command %S %S" repeat-arg last-repeatable-command))
      (when (eq last-repeatable-command 'self-insert-command)
        ;; We used to use a much more complex code to try and figure out
        ;; what key was used to run that self-insert-command:
        ;; (if (<= (- num-input-keys
        ;;            repeat-num-input-keys-at-self-insert)
        ;;         1)
        ;;     repeat-last-self-insert
        ;;   (let ((range (nth 1 buffer-undo-list)))
        ;;     (condition-case nil
        ;;         (setq repeat-last-self-insert
        ;;               (buffer-substring (car range)
        ;;                                 (cdr range)))
        ;;       (error (error "%s %s %s"  ;Danger, Will Robinson!
        ;;                     "repeat can't intuit what you"
        ;;                     "inserted before auto-fill"
        ;;                     "clobbered it, sorry")))))
        (setq last-command-event (char-before)))
      (let ((indirect (indirect-function last-repeatable-command)))
        (if (or (stringp indirect)
                (vectorp indirect))
            ;; Bind last-repeatable-command so that executing the macro does
            ;; not alter it.
            (let ((last-repeatable-command last-repeatable-command))
              (execute-kbd-macro last-repeatable-command))
          (call-interactively last-repeatable-command))))
    (when repeat-repeat-char
      (set-transient-map
       (let ((map (make-sparse-keymap)))
         (define-key map (vector repeat-repeat-char)
           (if (null repeat-message-function) 'repeat
             ;; If repeat-message-function is let-bound, preserve it for the
             ;; next "iterations of the loop".
             (let ((fun repeat-message-function))
               (lambda ()
                 (interactive)
                 (let ((repeat-message-function fun))
                   (setq this-command 'repeat)
		   ;; Beware: messing with `real-this-command' is *bad*, but we
		   ;; need it so `last-repeatable-command' can be recognized
		   ;; later (bug#12232).
                   (setq real-this-command 'repeat)
                   (call-interactively 'repeat))))))
         map)))))

(defun repeat-message (format &rest args)
  "Like `message' but displays with `repeat-message-function' if non-nil."
  (let ((message (apply 'format format args)))
    (if repeat-message-function
        (funcall repeat-message-function message)
      (message "%s" message))))

;; OK, there's one situation left where that doesn't work correctly: when the
;; most recent self-insertion provoked an auto-fill.  The problem is that
;; unraveling the undo information after an auto-fill is too hard, since all
;; kinds of stuff can get in there as a result of comment prefixes etc.  It'd
;; be possible to advise do-auto-fill to record the most recent
;; self-insertion before it does its thing, but that's a performance hit on
;; auto-fill, which already has performance problems; so it's better to just
;; leave it like this.  If text didn't provoke an auto-fill when the user
;; typed it, this'll correctly repeat its self-insertion, even if the
;; repetition does cause auto-fill.

;; If you wanted perfection, probably it'd be necessary to hack do-auto-fill
;; into 2 functions, maybe-do-auto-fill & really-do-auto-fill, because only
;; really-do-auto-fill should be advised.  As things are, either the undo
;; information would need to be scanned on every do-auto-fill invocation, or
;; the code at the top of do-auto-fill deciding whether filling is necessary
;; would need to be duplicated in the advice, wasting execution time when
;; filling does turn out to be necessary.

;; I thought maybe this story had a moral, something about functional
;; decomposition; but now I'm not even sure of that, since a function
;; call per se is a performance hit, & even the code that would
;; correspond to really-do-auto-fill has performance problems that
;; can make it necessary to stop typing while Emacs catches up.
;; Maybe the real moral is that perfection is a chimera.

;; Ah, hell, it's all going to fall into a black hole someday anyway.

;;;;; ************************* EMACS CONTROL ************************* ;;;;;


;; And now for something completely different.

;;; repeat-mode

(defcustom repeat-exit-key nil
  "Key that stops the modal repeating of keys in sequence.
For example, you can set it to <return> like `isearch-exit'."
  :type '(choice (const :tag "No special key to exit repeating sequence" nil)
                 (key :tag "Kbd keys that exit repeating sequence"))
  :group 'repeat
  :version "28.1")

(defcustom repeat-exit-timeout nil
  "Break the repetition chain of keys after specified amount of idle time.
When a number, exit the transient repeating mode after idle time
of the specified number of seconds.
You can also set the property `repeat-exit-timeout' on the command symbol.
This property can override the value of this variable."
  :type '(choice (const :tag "No timeout to exit repeating sequence" nil)
                 (number :tag "Timeout in seconds to exit repeating"))
  :group 'repeat
  :version "28.1")

(defvar repeat--transient-exitfun nil
  "Function returned by `set-transient-map'.")

(defvar repeat-exit-timer nil
  "Timer activated after the last key typed in the repeating key sequence.")

(defcustom repeat-keep-prefix nil
  "Whether to keep the prefix arg of the previous command when repeating."
  :type 'boolean
  :initialize #'custom-initialize-default
  :set (lambda (sym val)
         (set-default sym val)
         (when repeat-mode
           (if repeat-keep-prefix
               (add-hook 'pre-command-hook 'repeat-pre-hook)
             (remove-hook 'pre-command-hook 'repeat-pre-hook))))
  :group 'repeat
  :version "28.1")

(defcustom repeat-check-key t
  "Whether to check that the last key exists in the repeat map.
When non-nil, and the last typed key (with or without modifiers)
doesn't exist in the keymap specified by the `repeat-map' property
of the command, don't activate that keymap for the next command.
Thus, when this is non-nil, only the same keys among repeatable
keys are allowed in the repeating sequence.  For example, with a
non-nil value, only \\`C-x u u' repeats undo, whereas \\`C-/ u' doesn't.

You can also set the property `repeat-check-key' on the command symbol.
This property can override the value of this variable.
When the variable value is non-nil, but the property value is `no',
then don't check the last key.  Also when the variable value is nil,
but the property value is t, then check the last key."
  :type 'boolean
  :group 'repeat
  :version "28.1")

(defcustom repeat-echo-function #'repeat-echo-message
  "Function to display a hint about available keys.
The function is called after every repeatable command with one argument:
a repeating map, or nil after deactivating the transient repeating mode.
You can use `add-function' for multiple functions simultaneously."
  :type '(choice (const :tag "Show hints in the echo area"
                        repeat-echo-message)
                 (const :tag "Show indicator in the mode line"
                        repeat-echo-mode-line)
                 (const :tag "No visual feedback" ignore)
                 (function :tag "Function"))
  :group 'repeat
  :version "28.1")

(defvar repeat-in-progress nil
  "Non-nil when the repeating transient map is active.")

;;;###autoload
(defvar repeat-map nil
  "The value of the repeating transient map for the next command.
A command called from the map can set it again to the same map when
the map can't be set on the command symbol property `repeat-map'.")

;;;###autoload
(define-minor-mode repeat-mode
  "Toggle Repeat mode.
When Repeat mode is enabled, certain commands bound to multi-key
sequences can be repeated by typing a single key, after typing the
full key sequence once.

The commands that can be repeated in this way are those whose symbols
have the `repeat-map' property, which specifies a keymap of single keys
for repeating.

Normally, invoking a command outside that keymap terminates the
repeating sequence.  However, if the command's `repeat-continue'
property is non-nil, it may instead continue the current repeating
sequence: if the property is a list of keymaps, then the command
continues when the current repeat map is in the list; if the property is
t, the command always continues the sequence.

See `describe-repeat-maps' for a list of all repeatable commands."
  :global t :group 'repeat
  (if (not repeat-mode)
      (progn
        (remove-hook 'pre-command-hook 'repeat-pre-hook)
        (remove-hook 'post-command-hook 'repeat-post-hook))
    (when repeat-keep-prefix
      (add-hook 'pre-command-hook 'repeat-pre-hook))
    (add-hook 'post-command-hook 'repeat-post-hook)
    (when (called-interactively-p 'any)
      (let* ((keymaps nil)
             (commands (all-completions
                        "" obarray
                        (lambda (s)
                          (and (commandp s)
                               (get s 'repeat-map)
                               (push (get s 'repeat-map) keymaps))))))
        (message "Repeat mode is enabled for %d commands and %d keymaps; \
see `describe-repeat-maps'"
                 (length commands)
                 (length (delete-dups keymaps)))))))

(defun repeat--command-property (property)
  (or (and (symbolp this-command)
           (get this-command property))
      (and (symbolp real-this-command)
           (get real-this-command property))))

(defun repeat-get-map-sym ()
  "Return a transient map possibly as a symbol."
  (when repeat-mode
    (let ((map-sym (or repeat-map (repeat--command-property 'repeat-map)))
          (continue (repeat--command-property 'repeat-continue)))
      (when (and repeat-in-progress
                 (or (eq continue t)
                     (and (consp continue)
                          (memq repeat-in-progress continue))))
        (setq map-sym repeat-in-progress))
      map-sym)))

(defun repeat-get-map (map)
  "Return a transient map for keys repeatable after the current command."
  (when map
    (when (and (symbolp map) (boundp map))
      (setq map (symbol-value map)))
    map))

(defun repeat-check-key (key map)
  "Check if the last KEY is suitable for activating the repeating MAP."
  (let* ((prop (repeat--command-property 'repeat-check-key))
         (check-key (unless (eq prop 'no) (or prop repeat-check-key))))
    (or (not check-key)
        (lookup-key map (vector key))
        ;; Try without modifiers:
        (lookup-key map (vector (event-basic-type key))))))

(defvar repeat--prev-mb '(0)
  "Previous minibuffer state.")

(defun repeat-check-map (map)
  "Decide whether MAP can be used for the next command."
  (and map
       ;; Detect changes in the minibuffer state to allow repetitions
       ;; in the same minibuffer, but not when the minibuffer is activated
       ;; in the middle of repeating sequence (bug#47566).
       (or (< (minibuffer-depth) (car repeat--prev-mb))
           (eq current-minibuffer-command (cdr repeat--prev-mb)))
       (or (eq (repeat--command-property 'repeat-continue) t)
           (repeat-check-key last-command-event map))
       t))

(defun repeat-pre-hook ()
  "Function run before commands to handle repeatable keys."
  (when (and repeat-mode repeat-keep-prefix repeat-in-progress
             (not prefix-arg) current-prefix-arg)
    (let* ((map-sym (repeat-get-map-sym))
           (map (repeat-get-map map-sym)))
      ;; Only when repeat-post-hook will activate the same map
      (when (repeat-check-map map)
        ;; Optimize to use less logic in the function `repeat-get-map'
        ;; for the next call: when called again from `repeat-post-hook'
        ;; it will use the variable `repeat-map'.
        (setq repeat-map map-sym)
        ;; Preserve universal argument
        (setq prefix-arg current-prefix-arg)))))

(defun repeat-post-hook ()
  "Function run after commands to set transient keymap for repeatable keys."
  (let* ((was-in-progress repeat-in-progress)
         (map-sym (repeat-get-map-sym))
         (map (repeat-get-map map-sym)))
    (setq repeat-in-progress nil)
    (when (repeat-check-map map)
      ;; Messaging
      (funcall repeat-echo-function map)

      ;; Adding an exit key
      (when repeat-exit-key
        (setq map (copy-keymap map))
        (define-key map (if (key-valid-p repeat-exit-key)
                            (kbd repeat-exit-key)
                          repeat-exit-key)
                    'ignore))

      (setq repeat-in-progress map-sym)
      (repeat--clear-prev)
      (let ((exitfun (set-transient-map map)))
        (setq repeat--transient-exitfun exitfun)

        (let* ((prop (repeat--command-property 'repeat-exit-timeout))
               (timeout (unless (eq prop 'no) (or prop repeat-exit-timeout))))
          (when timeout
            (setq repeat-exit-timer
                  (run-with-idle-timer timeout nil #'repeat-exit))))))

    (setq repeat-map nil)
    (setq repeat--prev-mb (cons (minibuffer-depth) current-minibuffer-command))
    (when (and was-in-progress (not repeat-in-progress))
      (repeat-exit))))

;;;###autoload
(defun repeat-exit ()
  "Exit the repeating sequence.
This function can be used to force exit of repetition while it's active."
  (interactive)
  (setq repeat-in-progress nil)
  (repeat--clear-prev)
  (funcall repeat-echo-function nil))

(defun repeat--clear-prev ()
  "Internal function to clean up previously set exit function and timer."
  (when repeat-exit-timer
    (cancel-timer repeat-exit-timer)
    (setq repeat-exit-timer nil))
  (when repeat--transient-exitfun
    (funcall repeat--transient-exitfun)
    (setq repeat--transient-exitfun nil)))

(defun repeat-echo-message-string (keymap)
  "Return a string with the list of repeating keys in KEYMAP."
  (let (keys)
    (cl--map-keymap-recursively
     (lambda (key cmd) (and cmd (push (cons (copy-sequence key) cmd) keys)))
     keymap)
    (format-message
     "Repeat with %s%s"
     (mapconcat (lambda (key-cmd)
                  (let ((key (car key-cmd))
                        (cmd (cdr key-cmd)))
                    (if-let* ((hint (and (symbolp cmd)
                                         (get cmd 'repeat-hint)))
                              (last (aref key (1- (length key)))))
                        ;; Possibly reuse `read-multiple-choice' formatting.
                        (if (and (= (length key) 1) (characterp last))
                            (cdr (rmc--add-key-description (list last hint)))
                          (format "%s (%s)"
                                  (propertize (key-description key)
                                              'face 'read-multiple-choice-face)
                                  (if (characterp (event-basic-type last))
                                      (cdr (rmc--add-key-description
                                            (list (event-basic-type last) hint)))
                                    hint)))
                      ;; No hint
                      (propertize (key-description key)
                                  'face 'read-multiple-choice-face))))
                keys ", ")
     (if repeat-exit-key
         (substitute-command-keys
          (format ", or exit with \\`%s'"
                  (if (key-valid-p repeat-exit-key)
                      repeat-exit-key
                    (key-description repeat-exit-key))))
       ""))))

(defun repeat-echo-message (keymap)
  "Display in the echo area the repeating keys defined by KEYMAP.
See `repeat-echo-function' to enable/disable."
  (let ((message-log-max nil))
    (if keymap
        (let ((message (repeat-echo-message-string keymap)))
          (if (current-message)
              (message "%s [%s]" (current-message) message)
            (message "%s" message)))
      (let ((message (current-message)))
        (when message
          (cond
           ((string-prefix-p "Repeat with " message)
            (message nil))
           ((string-search " [Repeat with " message)
            (message "%s" (replace-regexp-in-string
                           " \\[Repeat with .*\\'" "" message)))))))))

(defvar repeat-echo-mode-line-string
  (propertize "[Repeating...] " 'face 'mode-line-emphasis)
  "String displayed in the mode line in repeating mode.")

(defun repeat-echo-mode-line (keymap)
  "Display the repeat indicator in the mode line.
KEYMAP should be non-nil, but is otherwise ignored.
See `repeat-echo-function' to enable/disable."
  (if keymap
      (unless (assq 'repeat-in-progress mode-line-modes)
        (add-to-list 'mode-line-modes (list 'repeat-in-progress
                                            repeat-echo-mode-line-string)))
    (force-mode-line-update t)))

(declare-function help-fns--analyze-function "help-fns" (function))

(defun describe-repeat-maps ()
  "Describe transient keymaps installed for repeating multi-key commands.
These keymaps enable repetition of commands bound to multi-key
sequences by typing just one key, when `repeat-mode' is enabled.
Commands that can be repeated this way must have their symbol
to have the `repeat-map' property whose value specified a keymap."
  (interactive)
  (require 'help-fns)
  (let ((help-buffer-under-preparation t))
    (help-setup-xref (list #'describe-repeat-maps)
                     (called-interactively-p 'interactive))
    (let ((keymaps nil))
      (all-completions
       "" obarray (lambda (s)
                    (and (commandp s)
                         (get s 'repeat-map)
                         (push s (alist-get (get s 'repeat-map) keymaps)))))
      (with-help-window (help-buffer)
        (with-current-buffer standard-output
          (setq-local outline-regexp "[*]+")
          (insert "\
A list of keymaps and their single-key shortcuts for repeating commands.
Click on a keymap to see the commands repeatable by the keymap.\n")

          (dolist (keymap (sort keymaps (lambda (a b)
                                          (when (and (symbolp (car a))
                                                     (symbolp (car b)))
                                            (string< (car a) (car b))))))
            (insert (format-message "\f\n* `%s'\n" (car keymap)))
            (when (symbolp (car keymap))
              (insert (substitute-command-keys (format-message "\\{%s}" (car keymap)))))

            (let* ((map (if (symbolp (car keymap))
                            (symbol-value (car keymap))
                          (car keymap)))
                   (repeat-commands (cdr keymap))
                   map-commands commands-enter commands-exit commands-continue)
              (cl--map-keymap-recursively
               (lambda (_key cmd)
                 (when (symbolp cmd) (push cmd map-commands)))
               map)
              (setq map-commands (seq-uniq map-commands))
              (setq commands-continue
                    (seq-filter (lambda (s) (memq (car keymap)
                                                  (get s 'repeat-continue)))
                                map-commands))
              (setq commands-enter
                    (seq-difference repeat-commands map-commands))
              (setq commands-exit
                    (seq-difference (seq-difference map-commands repeat-commands)
                                    commands-continue))

              (when (or commands-enter commands-exit commands-continue)
                (when commands-enter
                  (insert "\n** Entered with:\n\n")
                  (fill-region-as-paragraph
                   (point)
                   (progn
                     (insert (mapconcat (lambda (cmd)
                                          (format-message "`%s'" cmd))
                                        (sort commands-enter #'string<)
                                        ", "))
                     (point)))
                  (insert "\n"))
                (when commands-continue
                  (insert "\n** Continued only with:\n\n")
                  (fill-region-as-paragraph
                   (point)
                   (progn
                     (insert (mapconcat (lambda (cmd)
                                          (format-message "`%s'" cmd))
                                        (sort commands-continue #'string<)
                                        ", "))
                     (point)))
                  (insert "\n"))
                (when commands-exit
                  (insert "\n** Exited with:\n\n")
                  (fill-region-as-paragraph
                   (point)
                   (progn
                     (insert (mapconcat (lambda (cmd)
                                          (format-message "`%s'" cmd))
                                        (sort commands-exit #'string<)
                                        ", "))
                     (point)))
                  (insert "\n")))))

          ;; Hide ^Ls.
          (goto-char (point-min))
          (while (search-forward "\n\f\n" nil t)
            (put-text-property (1+ (match-beginning 0)) (1- (match-end 0))
                               'invisible t)))))))

(provide 'repeat)

;;; repeat.el ends here
