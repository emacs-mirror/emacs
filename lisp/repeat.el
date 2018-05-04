;;; repeat.el --- convenient way to repeat the previous command  -*- lexical-binding: t -*-

;; Copyright (C) 1998, 2001-2018 Free Software Foundation, Inc.

;; Author: Will Mengarini <seldon@eskimo.com>
;; Created: Mo 02 Mar 98
;; Version: 0.51
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
;;   (global-set-key "\C-z" 'repeat)
;;   (setq repeat-on-final-keystroke "z")
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

;; This command was first called `vi-dot', because
;; it was inspired by the `.' command in the vi editor,
;; but it was renamed to make its name more meaningful.

;;; Code:

;;;;; ************************* USER OPTIONS ************************** ;;;;;

(defcustom repeat-too-dangerous '(kill-this-buffer)
  "Commands too dangerous to repeat with \\[repeat]."
  :group 'convenience
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
  :group 'convenience
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
'repeat if `repeat' hadn't modified it."
  (= repeat-num-input-keys-at-repeat num-input-keys))

;; An example of the use of (repeat-is-really-this-command) may still be
;; available in <http://www.eskimo.com/~seldon/dotemacs.el>; search for
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
    (error "last-repeatable-command is mode-exit & can't be repeated"))
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
    (if (memq last-repeatable-command '(exit-minibuffer
					minibuffer-complete-and-exit
					self-insert-and-exit))
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

(provide 'repeat)

;;; repeat.el ends here
